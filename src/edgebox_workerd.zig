/// EdgeBox workerd Runner — spawns workerd + Zig IO server
///
/// Replaces the WAMR daemon model. Architecture:
///   1. Start Zig IO server on a Unix socket (thread pool for parallel IO)
///   2. Spawn `workerd serve <config.capnp>` as child process
///   3. workerd runs V8 (JIT + WASM AOT), IO bridge proxies fs ops to Zig
///   4. On exit: kill workerd, stop IO server, clean up socket
///
/// Usage:
///   edgebox-workerd <worker-dir>          Run workerd with generated config
///   edgebox-workerd <worker-dir> --port N  Use custom port (default: 8787)
///
/// The worker-dir must contain:
///   - *-worker.js   (the generated worker module)
///   - *-config.capnp (workerd configuration)
///   - standalone.wasm (optional, WASM AOT module)
const std = @import("std");
const builtin = @import("builtin");
const io_server = @import("io_server.zig");

const DEFAULT_PORT: u16 = 8787;

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

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--port") or std.mem.eql(u8, arg, "-p")) {
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

    // Find config.capnp in the worker directory
    const config_path = try findConfigCapnp(allocator, dir);
    defer allocator.free(config_path);

    if (verbose) {
        std.debug.print("[workerd-runner] Worker dir: {s}\n", .{dir});
        std.debug.print("[workerd-runner] Config: {s}\n", .{config_path});
        std.debug.print("[workerd-runner] Port: {d}\n", .{port});
    }

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
    var workerd_process = spawnWorkerd(allocator, workerd_bin, config_path, verbose) catch |err| {
        std.debug.print("[workerd-runner] Failed to start workerd: {}\n", .{err});
        std.debug.print("[workerd-runner] Is workerd installed? Try: npm install -g workerd\n", .{});
        server.deinit();
        io_thread.join();
        std.process.exit(1);
    };

    if (verbose) {
        std.debug.print("[workerd-runner] workerd started (pid {d})\n", .{workerd_process.id});
    }

    // Wait for workerd to exit
    const term = workerd_process.wait() catch |err| {
        std.debug.print("[workerd-runner] wait error: {}\n", .{err});
        server.deinit();
        io_thread.join();
        std.process.exit(1);
    };
    const exit_code: u8 = switch (term) {
        .Exited => |code| code,
        .Signal => |sig| blk: {
            if (verbose) std.debug.print("[workerd-runner] workerd killed by signal {d}\n", .{sig});
            break :blk 1;
        },
        else => 1,
    };

    // Clean shutdown
    server.deinit();
    io_thread.join();

    if (exit_code != 0 and verbose) {
        std.debug.print("[workerd-runner] workerd exited with code {d}\n", .{exit_code});
    }

    if (exit_code != 0) std.process.exit(exit_code);
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

fn findConfigCapnp(allocator: std.mem.Allocator, dir_path: []const u8) ![]const u8 {
    // Look for *-config.capnp in the directory
    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
        std.debug.print("[workerd-runner] Cannot open directory: {s} ({})\n", .{ dir_path, err });
        std.process.exit(1);
    };
    defer dir.close();

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, "-config.capnp")) {
            return try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, entry.name });
        }
    }

    // Fallback: try config.capnp
    const fallback = try std.fmt.allocPrint(allocator, "{s}/config.capnp", .{dir_path});
    std.fs.cwd().access(fallback, .{}) catch {
        allocator.free(fallback);
        std.debug.print("[workerd-runner] No *-config.capnp found in {s}\n", .{dir_path});
        std.process.exit(1);
    };
    return fallback;
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
        \\Usage: edgebox-workerd <worker-dir> [options]
        \\
        \\Runs a compiled EdgeBox worker using workerd (V8) + Zig IO server.
        \\
        \\Arguments:
        \\  <worker-dir>    Directory with *-worker.js and *-config.capnp
        \\
        \\Options:
        \\  --port, -p N    Port to listen on (default: 8787)
        \\  --workerd PATH  Path to workerd binary (default: search PATH)
        \\  --verbose, -v   Show debug output
        \\  --help, -h      Show this help
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
