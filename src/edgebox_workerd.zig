/// EdgeBox workerd Runner — spawns workerd + Zig IO server
///
/// Two modes of operation:
///
/// **HTTP Server Mode** (default):
///   edgebox-workerd <worker-dir>          Run workerd with generated config
///   edgebox-workerd <worker-dir> --port N  Use custom port (default: 8787)
///
/// **CLI Mode** (for tools like TSC):
///   edgebox-workerd <worker-dir> -- <args...>  Run as CLI, pass args to JS app
///
/// Architecture:
///   1. Start Zig IO server on a Unix socket (goroutine runtime for parallel IO)
///   2. Generate runtime entry point (io_bridge.js + globals injection)
///   3. Spawn `workerd serve <config.capnp>` as child process
///   4. workerd runs V8 (JIT + WASM AOT), IO bridge proxies fs ops to Zig
///   5. On exit: detect process.exit(), kill workerd, propagate exit code
///
/// The worker-dir must contain:
///   - *-worker.js or *-module.cjs (the generated worker module)
///   - *-config.capnp (workerd configuration, used in server mode)
///   - standalone.wasm (optional, WASM AOT module)
const std = @import("std");
const builtin = @import("builtin");
const io_server = @import("io_server.zig");

const DEFAULT_PORT: u16 = 8787;

/// io_bridge.js is embedded at comptime — zero file dependency at runtime.
const io_bridge_js = @embedFile("polyfills/io_bridge.js");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var worker_dir: ?[]const u8 = null;
    var port: u16 = DEFAULT_PORT;
    var workerd_bin: []const u8 = "workerd";
    var verbose = false;
    var cli_mode = false;

    // Collect passthrough args (everything after --)
    var passthrough_args: std.ArrayListUnmanaged([]const u8) = .{};
    defer passthrough_args.deinit(allocator);

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--")) {
            // Everything after -- is passthrough for the JS app
            cli_mode = true;
            i += 1;
            while (i < args.len) : (i += 1) {
                passthrough_args.append(allocator, args[i]) catch {};
            }
            break;
        } else if (std.mem.eql(u8, arg, "--port") or std.mem.eql(u8, arg, "-p")) {
            i += 1;
            if (i < args.len) {
                port = std.fmt.parseInt(u16, args[i], 10) catch DEFAULT_PORT;
            }
        } else if (std.mem.eql(u8, arg, "--workerd")) {
            i += 1;
            if (i < args.len) {
                workerd_bin = args[i];
            }
        } else if (std.mem.eql(u8, arg, "--verbose") or std.mem.eql(u8, arg, "-v")) {
            verbose = true;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printUsage();
            return;
        } else if (worker_dir == null and !std.mem.startsWith(u8, arg, "-")) {
            worker_dir = arg;
        }
    }

    const dir = worker_dir orelse {
        printUsage();
        std.process.exit(1);
    };

    // Generate a unique socket path for this instance
    var sock_buf: [256]u8 = undefined;
    const pid = if (@hasDecl(std.posix, "getpid"))
        std.posix.getpid()
    else if (@hasDecl(std.os, "linux") and @hasDecl(std.os.linux, "getpid"))
        std.os.linux.getpid()
    else
        @as(i32, @intCast(std.Thread.getCurrentId() % 65536));
    const socket_path = std.fmt.bufPrint(&sock_buf, "/tmp/edgebox-io-{d}.sock", .{
        pid,
    }) catch "/tmp/edgebox-io.sock";

    // Get current working directory for CWD injection
    var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = std.fs.cwd().realpath(".", &cwd_buf) catch "/";

    if (verbose) {
        std.debug.print("[workerd-runner] Worker dir: {s}\n", .{dir});
        std.debug.print("[workerd-runner] Socket: {s}\n", .{socket_path});
        std.debug.print("[workerd-runner] Mode: {s}\n", .{if (cli_mode) "CLI" else "HTTP server"});
        if (cli_mode) {
            std.debug.print("[workerd-runner] Passthrough args: {d}\n", .{passthrough_args.items.len});
        } else {
            std.debug.print("[workerd-runner] Port: {d}\n", .{port});
        }
    }

    // Find the worker module files in the directory
    const worker_files = try findWorkerFiles(allocator, dir);
    defer {
        allocator.free(worker_files.worker_js);
        if (worker_files.module_cjs) |m| allocator.free(m);
        if (worker_files.wasm_file) |w| allocator.free(w);
    }

    // Generate runtime entry point (io_bridge.js + globals + require module)
    const runtime_entry = try generateRuntimeEntry(
        allocator,
        dir,
        socket_path,
        cwd,
        passthrough_args.items,
        worker_files,
        cli_mode,
    );
    defer allocator.free(runtime_entry);

    // Generate runtime config.capnp
    const runtime_config = try generateRuntimeConfig(
        allocator,
        dir,
        worker_files,
        port,
        cli_mode,
    );
    defer allocator.free(runtime_config);

    if (verbose) {
        std.debug.print("[workerd-runner] Runtime entry: {s}\n", .{runtime_entry});
        std.debug.print("[workerd-runner] Runtime config: {s}\n", .{runtime_config});
    }

    // Start IO server in a background thread
    var server = try io_server.Server.init(allocator, socket_path);

    const io_thread = try std.Thread.spawn(.{}, struct {
        fn run(srv: *io_server.Server) void {
            srv.start() catch |err| {
                std.debug.print("[io-server] Fatal: {}\n", .{err});
            };
        }
    }.run, .{&server});

    if (verbose) {
        std.debug.print("[workerd-runner] IO server started on {s}\n", .{socket_path});
    }

    // Set up signal handling for clean shutdown
    setupSignalHandlers();

    // Spawn workerd
    var workerd_process = spawnWorkerd(allocator, workerd_bin, runtime_config, verbose) catch |err| {
        std.debug.print("[workerd-runner] Failed to start workerd: {}\n", .{err});
        std.debug.print("[workerd-runner] Is workerd installed? Try: npm install -g workerd\n", .{});
        cleanupRuntimeFiles(runtime_entry, runtime_config);
        server.deinit();
        io_thread.join();
        std.process.exit(1);
    };

    if (verbose) {
        std.debug.print("[workerd-runner] workerd started (pid {d})\n", .{workerd_process.id});
    }

    // Wait for workerd to exit, handling CLI mode exit detection.
    //
    // In CLI mode, the JS app calls process.exit(code) which sends an "exit" op
    // to the IO server. We detect should_stop and kill workerd. A separate thread
    // does the blocking wait() so the main thread can poll should_stop.
    var final_exit_code: u8 = 0;

    if (cli_mode) {
        // CLI mode: use a wait thread + poll should_stop
        const WaitState = struct {
            done: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
            exit_code: std.atomic.Value(u8) = std.atomic.Value(u8).init(0),
        };
        var wait_state = WaitState{};

        const wait_thread = try std.Thread.spawn(.{}, struct {
            fn run(child: *std.process.Child, state: *WaitState) void {
                const term = child.wait() catch {
                    state.exit_code.store(1, .release);
                    state.done.store(true, .release);
                    return;
                };
                const code: u8 = switch (term) {
                    .Exited => |c| c,
                    .Signal => 1,
                    else => 1,
                };
                state.exit_code.store(code, .release);
                state.done.store(true, .release);
            }
        }.run, .{ &workerd_process, &wait_state });

        // Poll: wait for either workerd exit or IO server exit signal
        while (!wait_state.done.load(.acquire) and !server.should_stop.load(.acquire)) {
            std.Thread.sleep(10 * std.time.ns_per_ms);
        }

        if (!wait_state.done.load(.acquire)) {
            // IO server received exit before workerd exited — kill workerd
            if (verbose) {
                std.debug.print("[workerd-runner] App called process.exit(), killing workerd\n", .{});
            }
            _ = workerd_process.kill() catch {};
        }

        // Wait for the wait thread to finish (workerd must have exited by now)
        wait_thread.join();

        // Use the JS app's exit code (from IO server), not workerd's
        const app_exit = server.getExitCode();
        final_exit_code = if (app_exit != 0)
            @intCast(@as(u32, @bitCast(app_exit)) & 0xFF)
        else
            wait_state.exit_code.load(.acquire);
    } else {
        // Server mode: just wait for workerd to exit
        const term = workerd_process.wait() catch |err| {
            std.debug.print("[workerd-runner] wait error: {}\n", .{err});
            cleanupRuntimeFiles(runtime_entry, runtime_config);
            server.deinit();
            io_thread.join();
            std.process.exit(1);
        };
        final_exit_code = switch (term) {
            .Exited => |code| code,
            .Signal => |sig| blk: {
                if (verbose) std.debug.print("[workerd-runner] workerd killed by signal {d}\n", .{sig});
                break :blk 1;
            },
            else => 1,
        };
    }

    // Clean shutdown
    server.deinit();
    io_thread.join();

    // Clean up runtime-generated files
    cleanupRuntimeFiles(runtime_entry, runtime_config);

    if (final_exit_code != 0 and verbose) {
        std.debug.print("[workerd-runner] Exiting with code {d}\n", .{final_exit_code});
    }

    if (final_exit_code != 0) std.process.exit(final_exit_code);
}

/// Files found in the worker directory.
const WorkerFiles = struct {
    worker_js: []const u8, // *-worker.js path
    module_cjs: ?[]const u8, // *-module.cjs path (may not exist)
    wasm_file: ?[]const u8, // *.wasm filename (just the name, not path)
};

/// Find worker files in the given directory.
fn findWorkerFiles(allocator: std.mem.Allocator, dir_path: []const u8) !WorkerFiles {
    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
        std.debug.print("[workerd-runner] Cannot open directory: {s} ({})\n", .{ dir_path, err });
        std.process.exit(1);
    };
    defer dir.close();

    var worker_js: ?[]const u8 = null;
    var module_cjs: ?[]const u8 = null;
    var wasm_file: ?[]const u8 = null;

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;
        if (std.mem.endsWith(u8, entry.name, "-worker.js")) {
            worker_js = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, entry.name });
        } else if (std.mem.endsWith(u8, entry.name, "-module.cjs")) {
            module_cjs = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, entry.name });
        } else if (std.mem.endsWith(u8, entry.name, ".wasm")) {
            wasm_file = try allocator.dupe(u8, entry.name);
        }
    }

    if (worker_js == null) {
        std.debug.print("[workerd-runner] No *-worker.js found in {s}\n", .{dir_path});
        std.process.exit(1);
    }

    return WorkerFiles{
        .worker_js = worker_js.?,
        .module_cjs = module_cjs,
        .wasm_file = wasm_file,
    };
}

/// Generate the runtime entry point JavaScript file.
/// This file:
///   1. Sets globalThis.__EDGEBOX_IO_SOCKET, __EDGEBOX_ARGV, __EDGEBOX_CWD
///   2. Contains io_bridge.js (embedded at comptime)
///   3. Requires the original module.cjs (or worker.js)
///   4. Exports a dummy fetch handler for workerd compatibility
fn generateRuntimeEntry(
    allocator: std.mem.Allocator,
    dir: []const u8,
    socket_path: []const u8,
    cwd: []const u8,
    passthrough_args: []const []const u8,
    files: WorkerFiles,
    cli_mode: bool,
) ![]const u8 {
    const entry_path = try std.fmt.allocPrint(allocator, "{s}/__edgebox_runtime_entry.mjs", .{dir});

    const file = std.fs.cwd().createFile(entry_path, .{}) catch |err| {
        std.debug.print("[workerd-runner] Cannot create runtime entry: {}\n", .{err});
        return err;
    };
    defer file.close();

    // Use a large buffer for the writer (io_bridge.js is ~15KB)
    var buf: [131072]u8 = undefined;
    var state = file.writer(&buf);
    const w = &state.interface;

    // 1. Set runtime globals
    w.print(
        \\// EdgeBox Runtime Entry Point (auto-generated, do not edit)
        \\'use strict';
        \\
        \\// Runtime globals for io_bridge.js
        \\globalThis.__EDGEBOX_IO_SOCKET = '{s}';
        \\globalThis.__EDGEBOX_CWD = '{s}';
        \\
    , .{ socket_path, cwd }) catch {};

    // Build EDGEBOX_ARGV as JSON array
    w.writeAll("globalThis.__EDGEBOX_ARGV = ['edgebox'") catch {};
    for (passthrough_args) |arg| {
        w.writeAll(", '") catch {};
        // Escape single quotes and backslashes in args
        for (arg) |c| {
            switch (c) {
                '\'' => w.writeAll("\\'") catch {},
                '\\' => w.writeAll("\\\\") catch {},
                '\n' => w.writeAll("\\n") catch {},
                '\r' => w.writeAll("\\r") catch {},
                else => {
                    const byte = [1]u8{c};
                    w.writeAll(&byte) catch {};
                },
            }
        }
        w.writeAll("'") catch {};
    }
    w.writeAll("];\n\n") catch {};

    // 2. Embed io_bridge.js (sets up fs/process polyfills over Unix socket)
    w.writeAll("// ---- io_bridge.js (embedded) ----\n") catch {};
    w.writeAll(io_bridge_js) catch {};
    w.writeAll("\n// ---- end io_bridge.js ----\n\n") catch {};

    // 3. Load WASM module if available (for workerd, pre-instantiate it as global)
    if (files.wasm_file) |wasm_name| {
        w.print(
            \\// Pre-load WASM module for workerd (avoids readFileSync in sandbox)
            \\try {{
            \\  const __wmod = require('{s}');
            \\  if (__wmod instanceof WebAssembly.Module) {{
            \\    globalThis.__edgebox_wasm_module = __wmod;
            \\  }}
            \\}} catch (e) {{
            \\  // WASM loading via require failed — module.cjs will try readFileSync fallback
            \\}}
            \\
        , .{wasm_name}) catch {};
    }

    // 4. Require the original module
    if (files.module_cjs) |mcjs| {
        const basename = std.fs.path.basename(mcjs);
        w.print("require('./{s}');\n\n", .{basename}) catch {};
    } else {
        const basename = std.fs.path.basename(files.worker_js);
        w.print("require('./{s}');\n\n", .{basename}) catch {};
    }

    // 5. Export fetch handler for workerd (required even in CLI mode)
    if (cli_mode) {
        w.writeAll(
            \\// Dummy fetch handler — workerd requires an exported handler.
            \\// In CLI mode, the app runs at module load time and calls process.exit().
            \\export default {
            \\  fetch() { return new Response("edgebox-cli"); }
            \\};
            \\
        ) catch {};
    } else {
        w.writeAll(
            \\// Server-mode fetch handler
            \\export default {
            \\  fetch(request, env, ctx) {
            \\    return new Response("edgebox-server");
            \\  }
            \\};
            \\
        ) catch {};
    }

    w.flush() catch {};
    return entry_path;
}

/// Generate the runtime config.capnp file.
/// In CLI mode: binds to localhost:0 (random port, never used).
/// In server mode: binds to the specified port.
fn generateRuntimeConfig(
    allocator: std.mem.Allocator,
    dir: []const u8,
    files: WorkerFiles,
    port: u16,
    cli_mode: bool,
) ![]const u8 {
    const config_path = try std.fmt.allocPrint(allocator, "{s}/__edgebox_runtime_config.capnp", .{dir});

    const file = std.fs.cwd().createFile(config_path, .{}) catch |err| {
        std.debug.print("[workerd-runner] Cannot create runtime config: {}\n", .{err});
        return err;
    };
    defer file.close();

    var buf: [8192]u8 = undefined;
    var state = file.writer(&buf);
    const w = &state.interface;

    // Config header + sockets
    if (cli_mode) {
        w.writeAll(
            \\using Workerd = import "/workerd/workerd.capnp";
            \\
            \\const config :Workerd.Config = (
            \\  services = [
            \\    (name = "main", worker = .worker),
            \\  ],
            \\  sockets = [
            \\    (name = "http", address = "127.0.0.1:0", http = (), service = "main"),
            \\  ],
            \\);
            \\
            \\
        ) catch {};
    } else {
        w.print(
            \\using Workerd = import "/workerd/workerd.capnp";
            \\
            \\const config :Workerd.Config = (
            \\  services = [
            \\    (name = "main", worker = .worker),
            \\  ],
            \\  sockets = [
            \\    (name = "http", address = "*:{d}", http = (), service = "main"),
            \\  ],
            \\);
            \\
            \\
        , .{port}) catch {};
    }

    // Worker definition with modules
    w.writeAll(
        \\const worker :Workerd.Worker = (
        \\  modules = [
        \\    (name = "entrypoint", esModule = embed "__edgebox_runtime_entry.mjs"),
        \\
    ) catch {};

    // Add WASM module if present
    if (files.wasm_file) |wasm_name| {
        w.print(
            \\    (name = "{s}", wasm = embed "{s}"),
            \\
        , .{ wasm_name, wasm_name }) catch {};
    }

    // Add the original module.cjs
    if (files.module_cjs) |mcjs| {
        const basename = std.fs.path.basename(mcjs);
        w.print(
            \\    (name = "./{s}", commonJsModule = embed "{s}"),
            \\
        , .{ basename, basename }) catch {};
    }

    // Add the worker.js (may be needed as a require target from module.cjs)
    {
        const basename = std.fs.path.basename(files.worker_js);
        w.print(
            \\    (name = "./{s}", commonJsModule = embed "{s}"),
            \\
        , .{ basename, basename }) catch {};
    }

    // Close modules + add compatibility flags
    w.writeAll(
        \\  ],
        \\  compatibilityDate = "2024-09-23",
        \\  compatibilityFlags = ["nodejs_compat"],
        \\);
        \\
    ) catch {};

    w.flush() catch {};
    return config_path;
}

fn spawnWorkerd(
    allocator: std.mem.Allocator,
    workerd_bin: []const u8,
    config_path: []const u8,
    verbose: bool,
) !std.process.Child {
    var argv: std.ArrayListUnmanaged([]const u8) = .{};
    defer argv.deinit(allocator);

    try argv.append(allocator, workerd_bin);
    try argv.append(allocator, "serve");
    try argv.append(allocator, config_path);
    if (verbose) {
        try argv.append(allocator, "--verbose");
    }

    var child = std.process.Child.init(argv.items, allocator);
    child.stdin_behavior = .Inherit;
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;

    try child.spawn();
    return child;
}

/// Clean up runtime-generated temporary files.
fn cleanupRuntimeFiles(entry_path: []const u8, config_path: []const u8) void {
    std.fs.cwd().deleteFile(entry_path) catch {};
    std.fs.cwd().deleteFile(config_path) catch {};
}

/// Set up signal handlers for graceful shutdown
fn setupSignalHandlers() void {
    const handler = struct {
        fn handle(_: c_int) callconv(.c) void {
            // Will naturally unwind — workerd child process gets the signal too
        }
    };

    if (builtin.os.tag != .windows) {
        const act = std.posix.Sigaction{
            .handler = .{ .handler = handler.handle },
            .mask = std.posix.sigemptyset(),
            .flags = 0,
        };
        std.posix.sigaction(std.posix.SIG.INT, &act, null);
        std.posix.sigaction(std.posix.SIG.TERM, &act, null);
    }
}

fn printUsage() void {
    std.debug.print(
        \\Usage: edgebox-workerd <worker-dir> [options] [-- <app-args...>]
        \\
        \\Runs a compiled EdgeBox worker using workerd (V8) + Zig IO server.
        \\
        \\Arguments:
        \\  <worker-dir>        Directory with *-worker.js and *-config.capnp
        \\
        \\Options:
        \\  --port, -p N        Port to listen on (default: 8787, server mode only)
        \\  --workerd PATH      Path to workerd binary (default: search PATH)
        \\  --verbose, -v       Show debug output
        \\  --help, -h          Show this help
        \\
        \\CLI Mode:
        \\  edgebox-workerd <dir> -- --noEmit tsconfig.json
        \\  Everything after -- is passed to the JS app as process.argv.
        \\  The app runs at module load time and exits via process.exit().
        \\
        \\The worker directory is created by `edgebox build <app.js>`.
        \\
        \\Architecture:
        \\  workerd (V8 JIT + WASM AOT) handles JS execution.
        \\  Zig IO server provides filesystem access over Unix socket.
        \\  IO bridge (io_bridge.js) proxies Node.js fs APIs to Zig.
        \\
    , .{});
}
