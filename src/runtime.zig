/// EdgeBox Native Runtime
/// Embeds WasmEdge C library directly for minimal cold start overhead
///
/// Usage:
///   edgebox build [app-directory]  - Build app (bundle + WASM + AOT)
///   edgebox run <script.js>        - Run JavaScript file
///   edgebox <script.js>            - Run JavaScript file (shorthand)
const std = @import("std");
const builtin = @import("builtin");
const c = @cImport({
    @cInclude("wasmedge/wasmedge.h");
});

const VERSION = "0.1.0";
const SOCKET_PATH = "/tmp/edgebox.sock";

// ============================================================================
// EdgeBox Configuration
// ============================================================================

/// Command permission: binary name -> allowed subcommands (null = all allowed)
const CommandPermission = struct {
    binary: []const u8,
    subcommands: ?[]const []const u8, // null = allow all args, empty = binary only
};

/// EdgeBox app configuration parsed from .edgebox.json
const EdgeBoxConfig = struct {
    name: ?[]const u8 = null,
    npm: ?[]const u8 = null,
    dirs: []const []const u8 = &.{},
    env: []const []const u8 = &.{},
    commands: []const CommandPermission = &.{},

    /// Parse .edgebox.json from a directory
    pub fn load(allocator: std.mem.Allocator, dir_path: []const u8) ?EdgeBoxConfig {
        var path_buf: [4096]u8 = undefined;
        const config_path = std.fmt.bufPrint(&path_buf, "{s}/.edgebox.json", .{dir_path}) catch return null;

        const file = std.fs.cwd().openFile(config_path, .{}) catch return null;
        defer file.close();

        const content = file.readToEndAlloc(allocator, 1024 * 1024) catch return null;
        defer allocator.free(content);

        return parseJson(allocator, content);
    }

    /// Parse .edgebox.json from current directory
    pub fn loadFromCwd(allocator: std.mem.Allocator) ?EdgeBoxConfig {
        const file = std.fs.cwd().openFile(".edgebox.json", .{}) catch return null;
        defer file.close();

        const content = file.readToEndAlloc(allocator, 1024 * 1024) catch return null;
        defer allocator.free(content);

        return parseJson(allocator, content);
    }

    fn parseJson(allocator: std.mem.Allocator, content: []const u8) ?EdgeBoxConfig {
        const parsed = std.json.parseFromSlice(std.json.Value, allocator, content, .{}) catch return null;
        defer parsed.deinit();

        var config = EdgeBoxConfig{};

        if (parsed.value.object.get("name")) |v| {
            if (v == .string) config.name = allocator.dupe(u8, v.string) catch null;
        }

        if (parsed.value.object.get("npm")) |v| {
            if (v == .string) config.npm = allocator.dupe(u8, v.string) catch null;
        }

        // Parse dirs array
        if (parsed.value.object.get("dirs")) |v| {
            if (v == .array) {
                var dirs: std.ArrayListUnmanaged([]const u8) = .{};
                for (v.array.items) |item| {
                    if (item == .string) {
                        dirs.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
                    }
                }
                config.dirs = dirs.toOwnedSlice(allocator) catch &.{};
            }
        }

        // Parse env array
        if (parsed.value.object.get("env")) |v| {
            if (v == .array) {
                var envs: std.ArrayListUnmanaged([]const u8) = .{};
                for (v.array.items) |item| {
                    if (item == .string) {
                        envs.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
                    }
                }
                config.env = envs.toOwnedSlice(allocator) catch &.{};
            }
        }

        // Parse commands object
        if (parsed.value.object.get("commands")) |v| {
            if (v == .object) {
                var cmds: std.ArrayListUnmanaged(CommandPermission) = .{};
                var iter = v.object.iterator();
                while (iter.next()) |entry| {
                    const binary = allocator.dupe(u8, entry.key_ptr.*) catch continue;
                    var perm = CommandPermission{ .binary = binary, .subcommands = null };

                    // Value can be: true (allow all), array of allowed subcommands
                    if (entry.value_ptr.* == .bool and entry.value_ptr.bool) {
                        // true = allow all arguments
                        perm.subcommands = null;
                    } else if (entry.value_ptr.* == .array) {
                        var subs: std.ArrayListUnmanaged([]const u8) = .{};
                        for (entry.value_ptr.array.items) |item| {
                            if (item == .string) {
                                // "*" means allow all
                                if (std.mem.eql(u8, item.string, "*")) {
                                    subs.deinit(allocator);
                                    perm.subcommands = null;
                                    break;
                                }
                                subs.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
                            }
                        } else {
                            perm.subcommands = subs.toOwnedSlice(allocator) catch null;
                        }
                    }

                    cmds.append(allocator, perm) catch continue;
                }
                config.commands = cmds.toOwnedSlice(allocator) catch &.{};
            }
        }

        return config;
    }

    /// Get list of allowed binary names (for WasmEdge --allow-command)
    pub fn getAllowedBinaries(self: EdgeBoxConfig, allocator: std.mem.Allocator) []const []const u8 {
        var binaries: std.ArrayListUnmanaged([]const u8) = .{};
        for (self.commands) |cmd| {
            binaries.append(allocator, cmd.binary) catch continue;
        }
        return binaries.toOwnedSlice(allocator) catch &.{};
    }

    /// Check if a command with given args is allowed
    pub fn isCommandAllowed(self: EdgeBoxConfig, binary: []const u8, args: []const []const u8) bool {
        for (self.commands) |cmd| {
            if (std.mem.eql(u8, cmd.binary, binary)) {
                // Found the binary
                if (cmd.subcommands == null) {
                    // null = all args allowed
                    return true;
                }
                // Check if first arg is in allowed list
                if (args.len == 0) {
                    // No args, check if empty subcommands means binary-only allowed
                    return cmd.subcommands.?.len == 0 or true;
                }
                for (cmd.subcommands.?) |allowed| {
                    if (std.mem.eql(u8, args[0], allowed)) {
                        return true;
                    }
                }
                return false; // First arg not in allowed list
            }
        }
        return false; // Binary not in allowed list
    }

    /// Serialize commands config to JSON for passing to WASM via env var
    pub fn serializeCommands(self: EdgeBoxConfig, allocator: std.mem.Allocator) ?[]const u8 {
        if (self.commands.len == 0) return null;

        var buf: std.ArrayListUnmanaged(u8) = .{};
        const writer = buf.writer(allocator);

        writer.writeAll("{") catch return null;
        var first = true;
        for (self.commands) |cmd| {
            if (!first) writer.writeAll(",") catch return null;
            first = false;

            writer.print("\"{s}\":", .{cmd.binary}) catch return null;
            if (cmd.subcommands) |subs| {
                writer.writeAll("[") catch return null;
                for (subs, 0..) |sub, i| {
                    if (i > 0) writer.writeAll(",") catch return null;
                    writer.print("\"{s}\"", .{sub}) catch return null;
                }
                writer.writeAll("]") catch return null;
            } else {
                writer.writeAll("true") catch return null;
            }
        }
        writer.writeAll("}") catch return null;

        return buf.toOwnedSlice(allocator) catch null;
    }
};

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        printUsage();
        std.process.exit(1);
    }

    const cmd = args[1];

    if (std.mem.eql(u8, cmd, "start")) {
        // Start daemon mode - check for --http flag
        var http_port: ?u16 = null;
        var wasm_args = args[2..];
        for (args[2..], 0..) |arg, i| {
            if (std.mem.startsWith(u8, arg, "--http")) {
                // Parse port: --http=8080 or --http 8080
                if (std.mem.indexOf(u8, arg, "=")) |eq_pos| {
                    http_port = std.fmt.parseInt(u16, arg[eq_pos + 1 ..], 10) catch 8080;
                } else if (i + 1 < args[2..].len) {
                    http_port = std.fmt.parseInt(u16, args[3 + i], 10) catch 8080;
                } else {
                    http_port = 8080;
                }
                wasm_args = args[2..][0..i];
                break;
            }
        }
        if (http_port) |port| {
            try daemonStartHttp(allocator, wasm_args, port);
        } else {
            try daemonStart(allocator, wasm_args);
        }
    } else if (std.mem.eql(u8, cmd, "stop")) {
        // Stop daemon
        try daemonStop();
    } else if (std.mem.eql(u8, cmd, "status")) {
        // Check daemon status
        try daemonStatus();
    } else if (std.mem.eql(u8, cmd, "exec")) {
        // Execute via daemon
        if (args.len < 3) {
            std.debug.print("Usage: edgebox exec <script.js> [args...]\n", .{});
            std.process.exit(1);
        }
        try daemonExec(allocator, args[2..]);
    } else if (std.mem.eql(u8, cmd, "build")) {
        // Static mode is the default (faster), --dynamic for development
        var dynamic_mode = false;
        var force_rebuild = false;
        var app_dir: []const u8 = "examples/hello";
        for (args[2..]) |arg| {
            if (std.mem.eql(u8, arg, "--dynamic")) {
                dynamic_mode = true;
            } else if (std.mem.eql(u8, arg, "--force") or std.mem.eql(u8, arg, "-f")) {
                force_rebuild = true;
            } else if (!std.mem.startsWith(u8, arg, "-")) {
                app_dir = arg;
            }
        }
        if (force_rebuild) {
            cleanBuildOutputs();
        }
        if (dynamic_mode) {
            try runBuild(allocator, app_dir);
        } else {
            try runStaticBuild(allocator, app_dir);
        }
    } else if (std.mem.eql(u8, cmd, "run")) {
        if (args.len < 3) {
            std.debug.print("Usage: edgebox run <file.wasm> [args...]\n", .{});
            std.process.exit(1);
        }
        const file = args[2];
        if (std.mem.endsWith(u8, file, ".wasm") or std.mem.endsWith(u8, file, ".dylib") or std.mem.endsWith(u8, file, ".so")) {
            try runWasm(allocator, args[2..]);
        } else {
            try runScript(allocator, args[2..]);
        }
    } else if (std.mem.eql(u8, cmd, "--help") or std.mem.eql(u8, cmd, "-h")) {
        printUsage();
    } else if (std.mem.eql(u8, cmd, "--version") or std.mem.eql(u8, cmd, "-v")) {
        std.debug.print("edgebox {s}\n", .{VERSION});
    } else if (std.mem.endsWith(u8, cmd, ".wasm") or std.mem.endsWith(u8, cmd, ".dylib") or std.mem.endsWith(u8, cmd, ".so")) {
        // Direct WASM execution: edgebox app.wasm
        try runWasm(allocator, args[1..]);
    } else if (std.mem.endsWith(u8, cmd, ".js")) {
        // Direct script execution (dynamic mode): edgebox script.js
        try runScript(allocator, args[1..]);
    } else {
        std.debug.print("Unknown command: {s}\n", .{cmd});
        printUsage();
        std.process.exit(1);
    }
}

fn printUsage() void {
    std.debug.print(
        \\EdgeBox - QuickJS JavaScript Runtime with WASI + WasmEdge AOT
        \\
        \\Usage:
        \\  edgebox build [app-directory]   Compile JS to WASM with embedded bytecode
        \\  edgebox run <file.wasm>         Run compiled WASM module
        \\  edgebox <file.wasm>             Run WASM (shorthand)
        \\
        \\Daemon Mode (<1ms cold starts):
        \\  edgebox start [file.wasm]       Start daemon with pre-loaded WASM
        \\  edgebox exec <script.js>        Execute script via daemon
        \\  edgebox status                  Check daemon status
        \\  edgebox stop                    Stop daemon
        \\
        \\Options:
        \\  --help, -h     Show this help
        \\  --version, -v  Show version
        \\
        \\Build Options:
        \\  --force, -f    Clean previous build outputs before building
        \\  --dynamic      Use dynamic JS loading (for development)
        \\
        \\Examples:
        \\  edgebox build my-app             Compile app to WASM
        \\  edgebox run edgebox-static.wasm  Run the compiled WASM
        \\  edgebox start                    Start daemon
        \\  edgebox exec script.js           Run via daemon (<1ms)
        \\
    , .{});
}

/// Clean all build outputs
fn cleanBuildOutputs() void {
    const files_to_clean = [_][]const u8{
        "bundle.js",
        "bundle_compiled.c",
        "edgebox-static.wasm",
        "edgebox-static-aot.dylib",
        "edgebox-static-aot.so",
        "edgebox-base.wasm",
        "edgebox-aot.dylib",
        "edgebox-aot.so",
        "bundle.js.cache",
    };
    for (files_to_clean) |file| {
        std.fs.cwd().deleteFile(file) catch {};
    }
}

fn runBuild(allocator: std.mem.Allocator, app_dir: []const u8) !void {
    std.debug.print("[build] App directory: {s}\n", .{app_dir});

    // Step 1: Check if app directory exists
    var dir = std.fs.cwd().openDir(app_dir, .{}) catch {
        std.debug.print("[error] App directory not found: {s}\n", .{app_dir});
        std.process.exit(1);
    };
    dir.close();

    // Step 2: Find entry point
    var entry_path_buf: [4096]u8 = undefined;
    const entry_path = findEntryPoint(app_dir, &entry_path_buf) catch {
        std.debug.print("[error] No entry point found in {s} (index.js, main.js, or app.js)\n", .{app_dir});
        std.process.exit(1);
    };
    std.debug.print("[build] Entry point: {s}\n", .{entry_path});

    // Step 3: Bundle with Bun
    std.debug.print("[build] Bundling with Bun...\n", .{});
    const bun_result = try runCommand(allocator, &.{
        "bun", "build", entry_path, "--outfile=bundle.js", "--target=node", "--format=cjs", "--minify",
    });
    defer {
        if (bun_result.stdout) |s| allocator.free(s);
        if (bun_result.stderr) |s| allocator.free(s);
    }

    if (bun_result.term.Exited != 0) {
        // Try without minify
        std.debug.print("[warn] Bun minify failed, trying without...\n", .{});
        const retry = try runCommand(allocator, &.{
            "bun", "build", entry_path, "--outfile=bundle.js", "--target=node", "--format=cjs",
        });
        defer {
            if (retry.stdout) |s| allocator.free(s);
            if (retry.stderr) |s| allocator.free(s);
        }
        if (retry.term.Exited != 0) {
            std.debug.print("[error] Bun bundling failed\n", .{});
            std.process.exit(1);
        }
    }

    // Step 4: Prepend polyfills
    const polyfills_path = "src/polyfills/runtime.js";
    if (std.fs.cwd().access(polyfills_path, .{})) |_| {
        std.debug.print("[build] Prepending runtime polyfills...\n", .{});
        try prependPolyfills(allocator, polyfills_path, "bundle.js");
    } else |_| {}

    // Print bundle size
    if (std.fs.cwd().statFile("bundle.js")) |stat| {
        const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
        std.debug.print("[build] Bundle: bundle.js ({d:.1}KB)\n", .{size_kb});
    } else |_| {}

    // Step 5: Build WASM if not exists
    const wasm_exists = std.fs.cwd().access("edgebox-base.wasm", .{}) catch null;
    if (wasm_exists == null) {
        std.debug.print("[build] Building QuickJS WASM with Zig...\n", .{});
        const zig_result = try runCommand(allocator, &.{
            "zig", "build", "wasm", "-Doptimize=ReleaseFast",
        });
        defer {
            if (zig_result.stdout) |s| allocator.free(s);
            if (zig_result.stderr) |s| allocator.free(s);
        }

        if (zig_result.term.Exited == 0) {
            // Copy from zig-out
            std.fs.cwd().copyFile("zig-out/bin/edgebox-base.wasm", std.fs.cwd(), "edgebox-base.wasm", .{}) catch {};

            // Run Wizer pre-initialization
            try runWizer(allocator);

            // Run wasm-opt
            try runWasmOpt(allocator);
        } else {
            std.debug.print("[warn] Zig WASM build failed\n", .{});
        }
    } else {
        std.debug.print("[build] Using existing edgebox-base.wasm\n", .{});
    }

    // Step 6: Pre-compile bytecode cache
    const wasm_ok = std.fs.cwd().access("edgebox-base.wasm", .{}) catch null;
    const bundle_ok = std.fs.cwd().access("bundle.js", .{}) catch null;
    if (wasm_ok != null and bundle_ok != null) {
        std.debug.print("[build] Pre-compiling bytecode cache...\n", .{});

        var cwd_buf: [4096]u8 = undefined;
        const cwd = std.process.getCwd(&cwd_buf) catch ".";

        var bundle_abs_buf: [4096]u8 = undefined;
        const bundle_abs = std.fmt.bufPrint(&bundle_abs_buf, "{s}/bundle.js", .{cwd}) catch "bundle.js";

        var dir_arg_buf: [4096]u8 = undefined;
        const dir_arg = std.fmt.bufPrint(&dir_arg_buf, "--dir={s}", .{cwd}) catch "--dir=.";

        const compile_result = try runCommand(allocator, &.{
            "wasmedge", dir_arg, "edgebox-base.wasm", "--compile-only", bundle_abs,
        });
        defer {
            if (compile_result.stdout) |s| allocator.free(s);
            if (compile_result.stderr) |s| allocator.free(s);
        }

        if (std.fs.cwd().statFile("bundle.js.cache")) |stat| {
            const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
            std.debug.print("[build] Bytecode cache: bundle.js.cache ({d:.1}KB)\n", .{size_kb});
        } else |_| {
            std.debug.print("[warn] Bytecode pre-compilation failed (will compile on first run)\n", .{});
        }
    }

    // Step 7: AOT compile
    const aot_ext = if (builtin.os.tag == .macos) "dylib" else "so";
    var aot_path_buf: [256]u8 = undefined;
    const aot_path = std.fmt.bufPrint(&aot_path_buf, "edgebox-aot.{s}", .{aot_ext}) catch "edgebox-aot.so";

    const aot_exists = std.fs.cwd().access(aot_path, .{}) catch null;
    if (aot_exists == null and wasm_ok != null) {
        std.debug.print("[build] AOT compiling with WasmEdge...\n", .{});
        const aot_result = try runCommand(allocator, &.{
            "wasmedge", "compile", "edgebox-base.wasm", aot_path,
        });
        defer {
            if (aot_result.stdout) |s| allocator.free(s);
            if (aot_result.stderr) |s| allocator.free(s);
        }

        if (aot_result.term.Exited == 0) {
            if (std.fs.cwd().statFile(aot_path)) |stat| {
                const size_mb = @as(f64, @floatFromInt(stat.size)) / 1024.0 / 1024.0;
                std.debug.print("[build] AOT compiled: {s} ({d:.1}MB)\n", .{ aot_path, size_mb });
            } else |_| {}
        } else {
            std.debug.print("[warn] AOT compilation failed, WASM module still usable\n", .{});
        }
    } else if (aot_exists != null) {
        std.debug.print("[build] Using existing {s}\n", .{aot_path});
    }

    // Copy .edgebox.json if exists
    var config_path_buf: [4096]u8 = undefined;
    const config_path = std.fmt.bufPrint(&config_path_buf, "{s}/.edgebox.json", .{app_dir}) catch null;
    if (config_path) |cp| {
        if (std.fs.cwd().access(cp, .{})) |_| {
            std.fs.cwd().copyFile(cp, std.fs.cwd(), ".edgebox.json", .{}) catch {};
            std.debug.print("[build] Config copied to .edgebox.json\n", .{});
        } else |_| {}
    }

    // Summary
    std.debug.print("\n[build] === Build Complete ===\n\n", .{});
    std.debug.print("To run:\n", .{});
    std.debug.print("  edgebox run bundle.js\n", .{});
    std.debug.print("  edgebox bundle.js\n\n", .{});
}

/// Static build: compile JS to C bytecode with qjsc, embed in WASM
fn runStaticBuild(allocator: std.mem.Allocator, app_dir: []const u8) !void {
    std.debug.print("[build] Static mode: compiling JS to bytecode\n", .{});
    std.debug.print("[build] App directory: {s}\n", .{app_dir});

    // Step 1: Check app directory
    var dir = std.fs.cwd().openDir(app_dir, .{}) catch {
        std.debug.print("[error] App directory not found: {s}\n", .{app_dir});
        std.process.exit(1);
    };
    dir.close();

    // Step 2: Find entry point
    var entry_path_buf: [4096]u8 = undefined;
    const entry_path = findEntryPoint(app_dir, &entry_path_buf) catch {
        std.debug.print("[error] No entry point found in {s}\n", .{app_dir});
        std.process.exit(1);
    };
    std.debug.print("[build] Entry point: {s}\n", .{entry_path});

    // Step 3: Bundle with Bun
    std.debug.print("[build] Bundling with Bun...\n", .{});
    const bun_result = try runCommand(allocator, &.{
        "bun", "build", entry_path, "--outfile=bundle.js", "--target=node", "--format=cjs", "--minify",
    });
    defer {
        if (bun_result.stdout) |s| allocator.free(s);
        if (bun_result.stderr) |s| allocator.free(s);
    }

    if (bun_result.term.Exited != 0) {
        const retry = try runCommand(allocator, &.{
            "bun", "build", entry_path, "--outfile=bundle.js", "--target=node", "--format=cjs",
        });
        defer {
            if (retry.stdout) |s| allocator.free(s);
            if (retry.stderr) |s| allocator.free(s);
        }
        if (retry.term.Exited != 0) {
            std.debug.print("[error] Bun bundling failed\n", .{});
            std.process.exit(1);
        }
    }

    // Step 4: Prepend polyfills
    const polyfills_path = "src/polyfills/runtime.js";
    if (std.fs.cwd().access(polyfills_path, .{})) |_| {
        std.debug.print("[build] Prepending runtime polyfills...\n", .{});
        try prependPolyfills(allocator, polyfills_path, "bundle.js");
    } else |_| {}

    if (std.fs.cwd().statFile("bundle.js")) |stat| {
        const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
        std.debug.print("[build] Bundle: bundle.js ({d:.1}KB)\n", .{size_kb});
    } else |_| {}

    // Step 5: Build qjsc if not exists
    const qjsc_exists = std.fs.cwd().access("zig-out/bin/qjsc", .{}) catch null;
    if (qjsc_exists == null) {
        std.debug.print("[build] Building qjsc compiler...\n", .{});
        const qjsc_result = try runCommand(allocator, &.{
            "zig", "build", "qjsc", "-Doptimize=ReleaseFast",
        });
        defer {
            if (qjsc_result.stdout) |s| allocator.free(s);
            if (qjsc_result.stderr) |s| allocator.free(s);
        }
        if (qjsc_result.term.Exited != 0) {
            std.debug.print("[error] Failed to build qjsc\n", .{});
            std.process.exit(1);
        }
    }

    // Step 6: Compile JS to C with qjsc
    std.debug.print("[build] Compiling JS to bytecode with qjsc...\n", .{});
    const qjsc_result = try runCommand(allocator, &.{
        "zig-out/bin/qjsc",
        "-s",              // Strip source
        "-N", "bundle",    // Variable name prefix
        "-o", "bundle_compiled.c",
        "bundle.js",
    });
    defer {
        if (qjsc_result.stdout) |s| allocator.free(s);
        if (qjsc_result.stderr) |s| allocator.free(s);
    }

    if (qjsc_result.term.Exited != 0) {
        std.debug.print("[error] qjsc compilation failed\n", .{});
        if (qjsc_result.stderr) |err| {
            std.debug.print("{s}\n", .{err});
        }
        std.process.exit(1);
    }

    // Append C bridge functions for Zig extern access
    const bridge_code =
        \\
        \\// Bridge functions for Zig extern access
        \\const uint8_t* get_bundle_ptr(void) { return bundle; }
        \\uint32_t get_bundle_size(void) { return bundle_size; }
        \\
    ;
    const bridge_file = std.fs.cwd().openFile("bundle_compiled.c", .{ .mode = .read_write }) catch {
        std.debug.print("[error] Failed to open bundle_compiled.c\n", .{});
        std.process.exit(1);
    };
    defer bridge_file.close();
    bridge_file.seekFromEnd(0) catch {};
    bridge_file.writeAll(bridge_code) catch {};

    if (std.fs.cwd().statFile("bundle_compiled.c")) |stat| {
        const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
        std.debug.print("[build] Bytecode: bundle_compiled.c ({d:.1}KB)\n", .{size_kb});
    } else |_| {}

    // Step 7: Build WASM with embedded bytecode
    std.debug.print("[build] Building static WASM with embedded bytecode...\n", .{});
    const wasm_result = try runCommand(allocator, &.{
        "zig", "build", "wasm-static", "-Doptimize=ReleaseFast",
    });
    defer {
        if (wasm_result.stdout) |s| allocator.free(s);
        if (wasm_result.stderr) |s| allocator.free(s);
    }

    if (wasm_result.term.Exited != 0) {
        std.debug.print("[error] WASM build failed\n", .{});
        if (wasm_result.stderr) |err| {
            std.debug.print("{s}\n", .{err});
        }
        std.process.exit(1);
    }

    // Copy from zig-out
    std.fs.cwd().copyFile("zig-out/bin/edgebox-static.wasm", std.fs.cwd(), "edgebox-static.wasm", .{}) catch {
        std.debug.print("[error] Failed to copy WASM\n", .{});
        std.process.exit(1);
    };

    if (std.fs.cwd().statFile("edgebox-static.wasm")) |stat| {
        const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
        std.debug.print("[build] Static WASM: edgebox-static.wasm ({d:.1}KB)\n", .{size_kb});
    } else |_| {}

    // Step 8: Wizer pre-initialization
    try runWizerStatic(allocator);

    // Step 9: wasm-opt
    try runWasmOptStatic(allocator);

    // Step 10: AOT compile
    const aot_ext = if (builtin.os.tag == .macos) "dylib" else "so";
    var aot_path_buf: [256]u8 = undefined;
    const aot_path = std.fmt.bufPrint(&aot_path_buf, "edgebox-static-aot.{s}", .{aot_ext}) catch "edgebox-static-aot.so";

    std.debug.print("[build] AOT compiling with WasmEdge...\n", .{});
    const aot_result = try runCommand(allocator, &.{
        "wasmedge", "compile", "edgebox-static.wasm", aot_path,
    });
    defer {
        if (aot_result.stdout) |s| allocator.free(s);
        if (aot_result.stderr) |s| allocator.free(s);
    }

    if (aot_result.term.Exited == 0) {
        if (std.fs.cwd().statFile(aot_path)) |stat| {
            const size_mb = @as(f64, @floatFromInt(stat.size)) / 1024.0 / 1024.0;
            std.debug.print("[build] AOT: {s} ({d:.1}MB)\n", .{ aot_path, size_mb });
        } else |_| {}
    } else {
        std.debug.print("[warn] AOT compilation failed\n", .{});
    }

    // Summary
    std.debug.print("\n[build] === Static Build Complete ===\n\n", .{});
    std.debug.print("Files created:\n", .{});
    std.debug.print("  bundle_compiled.c     - Compiled bytecode (C source)\n", .{});
    std.debug.print("  edgebox-static.wasm   - WASM with embedded bytecode\n", .{});
    std.debug.print("  {s}  - AOT native module\n\n", .{aot_path});
    std.debug.print("To run:\n", .{});
    std.debug.print("  wasmedge edgebox-static.wasm\n", .{});
    std.debug.print("  wasmedge {s}\n\n", .{aot_path});
}

fn runWizerStatic(allocator: std.mem.Allocator) !void {
    const which_result = try runCommand(allocator, &.{ "which", "wizer" });
    defer {
        if (which_result.stdout) |s| allocator.free(s);
        if (which_result.stderr) |s| allocator.free(s);
    }

    if (which_result.term.Exited != 0) {
        std.debug.print("[build] Wizer not found - skipping pre-initialization\n", .{});
        return;
    }

    std.debug.print("[build] Running Wizer pre-initialization...\n", .{});
    const wizer_result = try runCommand(allocator, &.{
        "wizer",
        "edgebox-static.wasm",
        "-o",
        "edgebox-static-wizer.wasm",
        "--allow-wasi",
        "--wasm-bulk-memory",
        "true",
        "--init-func",
        "wizer_init",
    });
    defer {
        if (wizer_result.stdout) |s| allocator.free(s);
        if (wizer_result.stderr) |s| allocator.free(s);
    }

    if (wizer_result.term.Exited == 0) {
        std.fs.cwd().deleteFile("edgebox-static.wasm") catch {};
        std.fs.cwd().rename("edgebox-static-wizer.wasm", "edgebox-static.wasm") catch {};
        if (std.fs.cwd().statFile("edgebox-static.wasm")) |stat| {
            const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
            std.debug.print("[build] Wizer snapshot: {d:.1}KB\n", .{size_kb});
        } else |_| {}
    } else {
        std.debug.print("[warn] Wizer failed (will use slower init)\n", .{});
    }
}

fn runWasmOptStatic(allocator: std.mem.Allocator) !void {
    const which_result = try runCommand(allocator, &.{ "which", "wasm-opt" });
    defer {
        if (which_result.stdout) |s| allocator.free(s);
        if (which_result.stderr) |s| allocator.free(s);
    }

    if (which_result.term.Exited != 0) return;

    std.debug.print("[build] Optimizing with wasm-opt...\n", .{});
    const opt_result = try runCommand(allocator, &.{
        "wasm-opt", "-Oz", "--enable-simd", "--strip-debug", "edgebox-static.wasm", "-o", "edgebox-static-opt.wasm",
    });
    defer {
        if (opt_result.stdout) |s| allocator.free(s);
        if (opt_result.stderr) |s| allocator.free(s);
    }

    if (opt_result.term.Exited == 0) {
        std.fs.cwd().deleteFile("edgebox-static.wasm") catch {};
        std.fs.cwd().rename("edgebox-static-opt.wasm", "edgebox-static.wasm") catch {};
        if (std.fs.cwd().statFile("edgebox-static.wasm")) |stat| {
            const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
            std.debug.print("[build] Optimized: {d:.1}KB\n", .{size_kb});
        } else |_| {}
    }
}

fn runWizer(allocator: std.mem.Allocator) !void {
    // Check if wizer is available
    const which_result = try runCommand(allocator, &.{ "which", "wizer" });
    defer {
        if (which_result.stdout) |s| allocator.free(s);
        if (which_result.stderr) |s| allocator.free(s);
    }

    if (which_result.term.Exited != 0) {
        std.debug.print("[build] Wizer not found - install with: cargo install wizer --features=\"env_logger structopt\"\n", .{});
        return;
    }

    std.debug.print("[build] Running Wizer pre-initialization...\n", .{});
    const wizer_result = try runCommand(allocator, &.{
        "wizer",
        "edgebox-base.wasm",
        "-o",
        "edgebox-wizer.wasm",
        "--allow-wasi",
        "--wasm-bulk-memory",
        "true",
        "--init-func",
        "wizer_init",
    });
    defer {
        if (wizer_result.stdout) |s| allocator.free(s);
        if (wizer_result.stderr) |s| allocator.free(s);
    }

    if (wizer_result.term.Exited == 0) {
        std.fs.cwd().deleteFile("edgebox-base.wasm") catch {};
        std.fs.cwd().rename("edgebox-wizer.wasm", "edgebox-base.wasm") catch {};
        if (std.fs.cwd().statFile("edgebox-base.wasm")) |stat| {
            const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
            std.debug.print("[build] Wizer snapshot: edgebox-base.wasm ({d:.1}KB)\n", .{size_kb});
        } else |_| {}
    } else {
        std.debug.print("[warn] Wizer pre-initialization failed (will use slower init path)\n", .{});
    }
}

fn runWasmOpt(allocator: std.mem.Allocator) !void {
    // Check if wasm-opt is available
    const which_result = try runCommand(allocator, &.{ "which", "wasm-opt" });
    defer {
        if (which_result.stdout) |s| allocator.free(s);
        if (which_result.stderr) |s| allocator.free(s);
    }

    if (which_result.term.Exited != 0) {
        return;
    }

    std.debug.print("[build] Optimizing WASM with wasm-opt...\n", .{});
    const opt_result = try runCommand(allocator, &.{
        "wasm-opt", "-Oz", "--enable-simd", "--strip-debug", "edgebox-base.wasm", "-o", "edgebox-base-opt.wasm",
    });
    defer {
        if (opt_result.stdout) |s| allocator.free(s);
        if (opt_result.stderr) |s| allocator.free(s);
    }

    if (opt_result.term.Exited == 0) {
        std.fs.cwd().deleteFile("edgebox-base.wasm") catch {};
        std.fs.cwd().rename("edgebox-base-opt.wasm", "edgebox-base.wasm") catch {};
        if (std.fs.cwd().statFile("edgebox-base.wasm")) |stat| {
            const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
            std.debug.print("[build] Optimized WASM: {d:.1}KB\n", .{size_kb});
        } else |_| {}
    }
}

fn findEntryPoint(app_dir: []const u8, buf: *[4096]u8) ![]const u8 {
    const entries = [_][]const u8{ "index.js", "main.js", "app.js" };
    for (entries) |entry| {
        const path = std.fmt.bufPrint(buf, "{s}/{s}", .{ app_dir, entry }) catch continue;
        if (std.fs.cwd().access(path, .{})) |_| {
            return path;
        } else |_| {}
    }
    return error.NotFound;
}

fn prependPolyfills(allocator: std.mem.Allocator, polyfills_path: []const u8, bundle_path: []const u8) !void {
    // Read polyfills
    const polyfills = try std.fs.cwd().readFileAlloc(allocator, polyfills_path, 1024 * 1024);
    defer allocator.free(polyfills);

    // Read bundle
    const bundle = try std.fs.cwd().readFileAlloc(allocator, bundle_path, 10 * 1024 * 1024);
    defer allocator.free(bundle);

    // Write combined
    const file = try std.fs.cwd().createFile(bundle_path, .{});
    defer file.close();

    try file.writeAll(polyfills);
    try file.writeAll(";\n");
    try file.writeAll(bundle);
}

const CommandResult = struct {
    term: std.process.Child.Term,
    stdout: ?[]u8,
    stderr: ?[]u8,
};

fn runCommand(allocator: std.mem.Allocator, argv: []const []const u8) !CommandResult {
    var child = std.process.Child.init(argv, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();

    // Read stdout and stderr in Zig 0.15 style
    const stdout = if (child.stdout) |f| f.readToEndAlloc(allocator, 10 * 1024 * 1024) catch null else null;
    const stderr = if (child.stderr) |f| f.readToEndAlloc(allocator, 10 * 1024 * 1024) catch null else null;

    const term = try child.wait();

    return .{
        .term = term,
        .stdout = stdout,
        .stderr = stderr,
    };
}

fn runScript(allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    _ = allocator;

    const script_path = args[0];

    // Initialize WasmEdge
    c.WasmEdge_LogSetErrorLevel();

    // Create configure
    const conf = c.WasmEdge_ConfigureCreate();
    defer c.WasmEdge_ConfigureDelete(conf);

    // Enable WASI
    c.WasmEdge_ConfigureAddHostRegistration(conf, c.WasmEdge_HostRegistration_Wasi);

    // Create VM
    const vm = c.WasmEdge_VMCreate(conf, null);
    defer c.WasmEdge_VMDelete(vm);

    // Get WASI module and initialize
    const wasi_module = c.WasmEdge_VMGetImportModuleContext(vm, c.WasmEdge_HostRegistration_Wasi);

    // Setup WASI args - just pass script path
    var wasi_args: [64][*c]const u8 = undefined;
    var wasi_argc: usize = 0;
    wasi_args[wasi_argc] = script_path.ptr;
    wasi_argc += 1;
    for (args[1..]) |arg| {
        if (wasi_argc < wasi_args.len) {
            wasi_args[wasi_argc] = arg.ptr;
            wasi_argc += 1;
        }
    }

    // Simple preopens - current dir and common paths
    var preopens: [4][*c]const u8 = undefined;
    preopens[0] = ".:.";
    preopens[1] = "/tmp:/private/tmp";

    var cwd_buf: [1024]u8 = undefined;
    const cwd = std.process.getCwd(&cwd_buf) catch ".";

    var cwd_preopen_buf: [2048]u8 = undefined;
    const cwd_preopen = std.fmt.bufPrintZ(&cwd_preopen_buf, "{s}:{s}", .{ cwd, cwd }) catch ".:.";
    preopens[2] = cwd_preopen.ptr;

    // Get script directory
    const script_dir = std.fs.path.dirname(script_path) orelse ".";
    var script_preopen_buf: [2048]u8 = undefined;
    const script_preopen = std.fmt.bufPrintZ(&script_preopen_buf, "{s}:{s}", .{ script_dir, script_dir }) catch ".:.";
    preopens[3] = script_preopen.ptr;

    c.WasmEdge_ModuleInstanceInitWASI(
        wasi_module,
        &wasi_args,
        @intCast(wasi_argc),
        null, // No env
        0,
        &preopens,
        4,
    );

    // Try AOT first, then WASM
    const aot_ext = if (builtin.os.tag == .macos) "dylib" else "so";
    var aot_path_buf: [256]u8 = undefined;
    const aot_path = std.fmt.bufPrintZ(&aot_path_buf, "edgebox-aot.{s}", .{aot_ext}) catch "edgebox-aot.so";
    const wasm_path = "edgebox-base.wasm";

    var module_path: [*c]const u8 = wasm_path;
    if (std.fs.cwd().access(aot_path, .{})) |_| {
        module_path = aot_path.ptr;
    } else |_| {}

    // Load module
    var result = c.WasmEdge_VMLoadWasmFromFile(vm, module_path);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("Failed to load WASM: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    // Validate
    result = c.WasmEdge_VMValidate(vm);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("Failed to validate: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    // Instantiate
    result = c.WasmEdge_VMInstantiate(vm);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("Failed to instantiate: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    // Run _start
    const func_name = c.WasmEdge_StringCreateByCString("_start");
    defer c.WasmEdge_StringDelete(func_name);

    result = c.WasmEdge_VMExecute(vm, func_name, null, 0, null, 0);
    if (!c.WasmEdge_ResultOK(result)) {
        const msg = c.WasmEdge_ResultGetMessage(result);
        if (msg != null) {
            const msg_str = std.mem.span(msg);
            // Ignore normal exit
            if (std.mem.indexOf(u8, msg_str, "terminated") == null) {
                std.debug.print("Execution failed: {s}\n", .{msg_str});
                std.process.exit(1);
            }
        }
    }
}

/// Run a compiled WASM/AOT module directly (replacement for wasmedge CLI)
/// Automatically prefers AOT (.dylib/.so) over WASM if available
fn runWasm(allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    const input_path = args[0];

    // Load config from .edgebox.json if present
    const config = EdgeBoxConfig.loadFromCwd(allocator);

    // If given a .wasm file, check if AOT version exists and prefer it
    var wasm_path = input_path;
    var aot_path_buf: [4096]u8 = undefined;

    if (std.mem.endsWith(u8, input_path, ".wasm")) {
        // Try to find AOT version: foo.wasm -> foo.dylib or foo.so
        const base = input_path[0 .. input_path.len - 5]; // strip ".wasm"
        const aot_ext = if (builtin.os.tag == .macos) ".dylib" else ".so";

        if (std.fmt.bufPrintZ(&aot_path_buf, "{s}{s}", .{ base, aot_ext })) |aot_path| {
            // Check if AOT file exists
            const file = std.fs.cwd().openFile(aot_path, .{}) catch |err| blk: {
                // If relative fails and path is absolute, try openFileAbsolute
                if (err == error.FileNotFound and aot_path[0] == '/') {
                    break :blk std.fs.openFileAbsolute(aot_path, .{}) catch null;
                }
                break :blk null;
            };
            if (file) |f| {
                f.close();
                wasm_path = aot_path;
            }
        } else |_| {}
    }

    // Initialize WasmEdge
    c.WasmEdge_LogSetErrorLevel();

    // Create configure
    const conf = c.WasmEdge_ConfigureCreate();
    defer c.WasmEdge_ConfigureDelete(conf);

    // Enable WASI
    c.WasmEdge_ConfigureAddHostRegistration(conf, c.WasmEdge_HostRegistration_Wasi);

    // Create VM
    const vm = c.WasmEdge_VMCreate(conf, null);
    defer c.WasmEdge_VMDelete(vm);

    // Get WASI module and initialize
    const wasi_module = c.WasmEdge_VMGetImportModuleContext(vm, c.WasmEdge_HostRegistration_Wasi);

    // Setup WASI args - pass remaining args
    var wasi_args: [64][*c]const u8 = undefined;
    var wasi_argc: usize = 0;
    for (args) |arg| {
        if (wasi_argc < wasi_args.len) {
            wasi_args[wasi_argc] = arg.ptr;
            wasi_argc += 1;
        }
    }

    // Setup environment variables
    var wasi_envs: [128][*c]const u8 = undefined;
    var wasi_env_count: usize = 0;

    // Storage for environment variable strings
    var env_storage: [64][256]u8 = undefined;
    var env_storage_idx: usize = 0;

    // Add __EDGEBOX_COMMANDS if config has commands
    if (config) |cfg| {
        if (cfg.serializeCommands(allocator)) |commands_json| {
            if (env_storage_idx < env_storage.len) {
                const env_str = std.fmt.bufPrintZ(&env_storage[env_storage_idx], "__EDGEBOX_COMMANDS={s}", .{commands_json}) catch null;
                if (env_str) |e| {
                    wasi_envs[wasi_env_count] = e.ptr;
                    wasi_env_count += 1;
                    env_storage_idx += 1;
                }
            }
        }

        // Add allowed environment variables from config
        for (cfg.env) |env_name| {
            if (std.posix.getenv(env_name)) |value| {
                if (env_storage_idx < env_storage.len) {
                    const env_str = std.fmt.bufPrintZ(&env_storage[env_storage_idx], "{s}={s}", .{ env_name, value }) catch null;
                    if (env_str) |e| {
                        wasi_envs[wasi_env_count] = e.ptr;
                        wasi_env_count += 1;
                        env_storage_idx += 1;
                    }
                }
            }
        }
    }

    // Preopens - current dir and common paths
    var preopens: [16][*c]const u8 = undefined;
    var preopen_count: usize = 0;

    preopens[preopen_count] = ".:.";
    preopen_count += 1;
    preopens[preopen_count] = "/tmp:/private/tmp";
    preopen_count += 1;

    var cwd_buf: [1024]u8 = undefined;
    const cwd = std.process.getCwd(&cwd_buf) catch ".";

    var cwd_preopen_buf: [2048]u8 = undefined;
    const cwd_preopen = std.fmt.bufPrintZ(&cwd_preopen_buf, "{s}:{s}", .{ cwd, cwd }) catch ".:.";
    preopens[preopen_count] = cwd_preopen.ptr;
    preopen_count += 1;

    // Get WASM directory
    const wasm_dir = std.fs.path.dirname(wasm_path) orelse ".";
    var wasm_preopen_buf: [2048]u8 = undefined;
    const wasm_preopen = std.fmt.bufPrintZ(&wasm_preopen_buf, "{s}:{s}", .{ wasm_dir, wasm_dir }) catch ".:.";
    preopens[preopen_count] = wasm_preopen.ptr;
    preopen_count += 1;

    // Add dirs from config
    var dir_preopen_bufs: [8][2048]u8 = undefined;
    var dir_buf_idx: usize = 0;
    if (config) |cfg| {
        for (cfg.dirs) |dir| {
            if (dir_buf_idx >= dir_preopen_bufs.len or preopen_count >= preopens.len) break;

            // Expand ~ to home directory
            var expanded_dir: []const u8 = dir;
            var home_buf: [1024]u8 = undefined;
            if (std.mem.startsWith(u8, dir, "~")) {
                if (std.posix.getenv("HOME")) |home| {
                    const suffix = if (dir.len > 1) dir[1..] else "";
                    expanded_dir = std.fmt.bufPrint(&home_buf, "{s}{s}", .{ home, suffix }) catch dir;
                }
            }

            const dir_preopen = std.fmt.bufPrintZ(&dir_preopen_bufs[dir_buf_idx], "{s}:{s}", .{ expanded_dir, expanded_dir }) catch continue;
            preopens[preopen_count] = dir_preopen.ptr;
            preopen_count += 1;
            dir_buf_idx += 1;
        }
    }

    c.WasmEdge_ModuleInstanceInitWASI(
        wasi_module,
        &wasi_args,
        @intCast(wasi_argc),
        if (wasi_env_count > 0) &wasi_envs else null,
        @intCast(wasi_env_count),
        &preopens,
        @intCast(preopen_count),
    );

    // Load module directly from provided path
    var result = c.WasmEdge_VMLoadWasmFromFile(vm, wasm_path.ptr);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("Failed to load WASM: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    // Validate
    result = c.WasmEdge_VMValidate(vm);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("Failed to validate: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    // Instantiate
    result = c.WasmEdge_VMInstantiate(vm);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("Failed to instantiate: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    // Run _start
    const func_name = c.WasmEdge_StringCreateByCString("_start");
    defer c.WasmEdge_StringDelete(func_name);

    result = c.WasmEdge_VMExecute(vm, func_name, null, 0, null, 0);
    if (!c.WasmEdge_ResultOK(result)) {
        const msg = c.WasmEdge_ResultGetMessage(result);
        if (msg != null) {
            const msg_str = std.mem.span(msg);
            // Ignore normal exit
            if (std.mem.indexOf(u8, msg_str, "terminated") == null) {
                std.debug.print("Execution failed: {s}\n", .{msg_str});
                std.process.exit(1);
            }
        }
    }
}

// ============================================================================
// Daemon Mode - Keep WasmEdge VM loaded for <1ms cold starts
// ============================================================================

/// Start HTTP daemon server - fastest mode for benchmarking
fn daemonStartHttp(allocator: std.mem.Allocator, args: []const [:0]const u8, port: u16) !void {
    _ = allocator;

    // Determine WASM path
    var wasm_path_daemon: []const u8 = "edgebox-base.wasm";
    if (args.len > 0 and !std.mem.startsWith(u8, args[0], "-")) {
        wasm_path_daemon = args[0];
    }

    // Check for AOT version
    var aot_path_buf: [4096]u8 = undefined;
    var module_path: []const u8 = wasm_path_daemon;

    if (std.mem.endsWith(u8, wasm_path_daemon, ".wasm")) {
        const base = wasm_path_daemon[0 .. wasm_path_daemon.len - 5];
        const aot_ext = if (builtin.os.tag == .macos) ".dylib" else ".so";
        if (std.fmt.bufPrint(&aot_path_buf, "{s}{s}", .{ base, aot_ext })) |aot_path| {
            if (std.fs.cwd().access(aot_path, .{})) |_| {
                module_path = aot_path;
            } else |_| {}
        } else |_| {}
    }

    std.debug.print("[http] Loading {s}...\n", .{module_path});

    // Initialize WasmEdge
    c.WasmEdge_LogSetErrorLevel();

    const conf = c.WasmEdge_ConfigureCreate();
    defer c.WasmEdge_ConfigureDelete(conf);
    c.WasmEdge_ConfigureAddHostRegistration(conf, c.WasmEdge_HostRegistration_Wasi);

    const vm = c.WasmEdge_VMCreate(conf, null);
    defer c.WasmEdge_VMDelete(vm);

    // Load and validate WASM
    var module_path_z: [4096]u8 = undefined;
    @memcpy(module_path_z[0..module_path.len], module_path);
    module_path_z[module_path.len] = 0;

    var result = c.WasmEdge_VMLoadWasmFromFile(vm, &module_path_z);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("[http] Failed to load: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    result = c.WasmEdge_VMValidate(vm);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("[http] Failed to validate: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    std.debug.print("[http] WASM loaded and validated\n", .{});

    // Create TCP socket
    const server = std.posix.socket(std.posix.AF.INET, std.posix.SOCK.STREAM, 0) catch |err| {
        std.debug.print("[http] Failed to create socket: {}\n", .{err});
        std.process.exit(1);
    };
    defer std.posix.close(server);

    // Set SO_REUSEADDR
    const optval: c_int = 1;
    std.posix.setsockopt(server, std.posix.SOL.SOCKET, std.posix.SO.REUSEADDR, std.mem.asBytes(&optval)) catch {};

    var addr: std.posix.sockaddr.in = .{
        .family = std.posix.AF.INET,
        .port = std.mem.nativeToBig(u16, port),
        .addr = 0, // INADDR_ANY
    };

    std.posix.bind(server, @ptrCast(&addr), @sizeOf(std.posix.sockaddr.in)) catch |err| {
        std.debug.print("[http] Failed to bind port {}: {}\n", .{ port, err });
        std.process.exit(1);
    };

    std.posix.listen(server, 128) catch |err| {
        std.debug.print("[http] Failed to listen: {}\n", .{err});
        std.process.exit(1);
    };

    std.debug.print("[http] Listening on http://localhost:{}\n", .{port});
    std.debug.print("[http] Test: curl http://localhost:{}/bench/hello.js\n", .{port});
    std.debug.print("[http] Ready for requests (Ctrl+C to stop)\n", .{});

    // Accept loop
    while (true) {
        const client = std.posix.accept(server, null, null, 0) catch |err| {
            std.debug.print("[http] Accept error: {}\n", .{err});
            continue;
        };
        defer std.posix.close(client);

        // Read HTTP request
        var buf: [8192]u8 = undefined;
        const n = std.posix.read(client, &buf) catch continue;
        if (n == 0) continue;

        const request = buf[0..n];

        // Parse HTTP request - extract path from "GET /path HTTP/1.1"
        var lines = std.mem.splitScalar(u8, request, '\n');
        const first_line = lines.next() orelse continue;

        var parts = std.mem.splitScalar(u8, first_line, ' ');
        _ = parts.next(); // Skip method (GET)
        const path = parts.next() orelse continue;

        // Remove leading slash
        const script_path = if (path.len > 1 and path[0] == '/') path[1..] else path;

        // Trim any trailing \r
        const clean_path = std.mem.trimRight(u8, script_path, "\r");

        std.debug.print("[http] GET /{s}\n", .{clean_path});

        // Time the execution
        const start = std.time.nanoTimestamp();

        // Get WASI module and configure for this request
        const wasi_module = c.WasmEdge_VMGetImportModuleContext(vm, c.WasmEdge_HostRegistration_Wasi);

        // Setup WASI args
        var wasi_args_http: [64][*c]const u8 = undefined;
        var wasi_argc_http: usize = 0;

        var script_path_z: [4096]u8 = undefined;
        @memcpy(script_path_z[0..clean_path.len], clean_path);
        script_path_z[clean_path.len] = 0;
        wasi_args_http[wasi_argc_http] = &script_path_z;
        wasi_argc_http += 1;

        // Preopens
        var preopens_http: [3][*c]const u8 = undefined;
        preopens_http[0] = ".:.";
        preopens_http[1] = "/tmp:/tmp";

        var cwd_buf_http: [1024]u8 = undefined;
        const cwd_http = std.process.getCwd(&cwd_buf_http) catch ".";
        var cwd_preopen_buf_http: [2048]u8 = undefined;
        const cwd_preopen_http = std.fmt.bufPrintZ(&cwd_preopen_buf_http, "{s}:{s}", .{ cwd_http, cwd_http }) catch ".:.";
        preopens_http[2] = cwd_preopen_http.ptr;

        c.WasmEdge_ModuleInstanceInitWASI(wasi_module, &wasi_args_http, @intCast(wasi_argc_http), null, 0, &preopens_http, 3);

        // Instantiate
        result = c.WasmEdge_VMInstantiate(vm);
        if (!c.WasmEdge_ResultOK(result)) {
            const http_err = "HTTP/1.1 500 Error\r\nContent-Length: 18\r\n\r\nInstantiate failed";
            _ = std.posix.write(client, http_err) catch {};
            continue;
        }

        // Execute _start
        const func_name_http = c.WasmEdge_StringCreateByCString("_start");
        defer c.WasmEdge_StringDelete(func_name_http);

        result = c.WasmEdge_VMExecute(vm, func_name_http, null, 0, null, 0);

        const elapsed = std.time.nanoTimestamp() - start;
        const elapsed_ms = @as(f64, @floatFromInt(elapsed)) / 1_000_000.0;

        // Send HTTP response
        var response_buf: [512]u8 = undefined;
        const body = std.fmt.bufPrint(&response_buf, "OK {d:.2}ms\n", .{elapsed_ms}) catch "OK\n";
        var http_response_buf: [1024]u8 = undefined;
        const http_response = std.fmt.bufPrint(&http_response_buf, "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{s}", .{ body.len, body }) catch "HTTP/1.1 200 OK\r\n\r\nOK\n";
        _ = std.posix.write(client, http_response) catch {};

        std.debug.print("[http] Completed in {d:.2}ms\n", .{elapsed_ms});
    }
}

/// Start daemon server - keeps WasmEdge VM in memory
fn daemonStart(allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    _ = allocator;

    // Remove existing socket
    std.fs.deleteFileAbsolute(SOCKET_PATH) catch {};

    // Determine WASM path
    var wasm_path_daemon: []const u8 = "edgebox-base.wasm";
    if (args.len > 0 and !std.mem.startsWith(u8, args[0], "-")) {
        wasm_path_daemon = args[0];
    }

    // Check for AOT version
    var aot_path_buf: [4096]u8 = undefined;
    var module_path: []const u8 = wasm_path_daemon;

    if (std.mem.endsWith(u8, wasm_path_daemon, ".wasm")) {
        const base = wasm_path_daemon[0 .. wasm_path_daemon.len - 5];
        const aot_ext = if (builtin.os.tag == .macos) ".dylib" else ".so";
        if (std.fmt.bufPrint(&aot_path_buf, "{s}{s}", .{ base, aot_ext })) |aot_path| {
            if (std.fs.cwd().access(aot_path, .{})) |_| {
                module_path = aot_path;
            } else |_| {}
        } else |_| {}
    }

    std.debug.print("[daemon] Loading {s}...\n", .{module_path});

    // Initialize WasmEdge
    c.WasmEdge_LogSetErrorLevel();

    const conf = c.WasmEdge_ConfigureCreate();
    defer c.WasmEdge_ConfigureDelete(conf);
    c.WasmEdge_ConfigureAddHostRegistration(conf, c.WasmEdge_HostRegistration_Wasi);

    const vm = c.WasmEdge_VMCreate(conf, null);
    defer c.WasmEdge_VMDelete(vm);

    // Load and validate WASM
    var module_path_z: [4096]u8 = undefined;
    @memcpy(module_path_z[0..module_path.len], module_path);
    module_path_z[module_path.len] = 0;

    var result = c.WasmEdge_VMLoadWasmFromFile(vm, &module_path_z);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("[daemon] Failed to load: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    result = c.WasmEdge_VMValidate(vm);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("[daemon] Failed to validate: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }

    std.debug.print("[daemon] WASM loaded and validated\n", .{});

    // Create Unix socket server
    const server = std.posix.socket(std.posix.AF.UNIX, std.posix.SOCK.STREAM, 0) catch |err| {
        std.debug.print("[daemon] Failed to create socket: {}\n", .{err});
        std.process.exit(1);
    };
    defer std.posix.close(server);

    var addr: std.posix.sockaddr.un = .{
        .family = std.posix.AF.UNIX,
        .path = undefined,
    };
    @memcpy(addr.path[0..SOCKET_PATH.len], SOCKET_PATH);
    addr.path[SOCKET_PATH.len] = 0;

    std.posix.bind(server, @ptrCast(&addr), @sizeOf(std.posix.sockaddr.un)) catch |err| {
        std.debug.print("[daemon] Failed to bind: {}\n", .{err});
        std.process.exit(1);
    };

    std.posix.listen(server, 5) catch |err| {
        std.debug.print("[daemon] Failed to listen: {}\n", .{err});
        std.process.exit(1);
    };

    std.debug.print("[daemon] Listening on {s}\n", .{SOCKET_PATH});
    std.debug.print("[daemon] Ready for requests (Ctrl+C to stop)\n", .{});

    // Accept loop
    while (true) {
        const client = std.posix.accept(server, null, null, 0) catch |err| {
            std.debug.print("[daemon] Accept error: {}\n", .{err});
            continue;
        };
        defer std.posix.close(client);

        // Read request (script path + args as newline-separated)
        var buf: [8192]u8 = undefined;
        const n = std.posix.read(client, &buf) catch continue;
        if (n == 0) continue;

        const request = buf[0..n];

        // Check for shutdown
        if (std.mem.startsWith(u8, request, "SHUTDOWN")) {
            std.debug.print("[daemon] Shutdown requested\n", .{});
            break;
        }

        // Parse request: first line is script path
        var lines = std.mem.splitScalar(u8, request, '\n');
        const script_path = lines.next() orelse continue;

        std.debug.print("[daemon] Executing: {s}\n", .{script_path});

        // Time the execution
        const start = std.time.nanoTimestamp();

        // Get WASI module and configure for this request
        const wasi_module = c.WasmEdge_VMGetImportModuleContext(vm, c.WasmEdge_HostRegistration_Wasi);

        // Setup WASI args
        var wasi_args_daemon: [64][*c]const u8 = undefined;
        var wasi_argc_daemon: usize = 0;

        // First arg is the script path (null-terminate it)
        var script_path_z: [4096]u8 = undefined;
        @memcpy(script_path_z[0..script_path.len], script_path);
        script_path_z[script_path.len] = 0;
        wasi_args_daemon[wasi_argc_daemon] = &script_path_z;
        wasi_argc_daemon += 1;

        // Preopens
        var preopens_daemon: [3][*c]const u8 = undefined;
        preopens_daemon[0] = ".:.";
        preopens_daemon[1] = "/tmp:/tmp";

        var cwd_buf_daemon: [1024]u8 = undefined;
        const cwd_daemon = std.process.getCwd(&cwd_buf_daemon) catch ".";
        var cwd_preopen_buf_daemon: [2048]u8 = undefined;
        const cwd_preopen_daemon = std.fmt.bufPrintZ(&cwd_preopen_buf_daemon, "{s}:{s}", .{ cwd_daemon, cwd_daemon }) catch ".:.";
        preopens_daemon[2] = cwd_preopen_daemon.ptr;

        c.WasmEdge_ModuleInstanceInitWASI(wasi_module, &wasi_args_daemon, @intCast(wasi_argc_daemon), null, 0, &preopens_daemon, 3);

        // Instantiate (fresh instance each request)
        result = c.WasmEdge_VMInstantiate(vm);
        if (!c.WasmEdge_ResultOK(result)) {
            const err_msg = "Instantiate failed\n";
            _ = std.posix.write(client, err_msg) catch {};
            continue;
        }

        // Execute _start
        const func_name_daemon = c.WasmEdge_StringCreateByCString("_start");
        defer c.WasmEdge_StringDelete(func_name_daemon);

        result = c.WasmEdge_VMExecute(vm, func_name_daemon, null, 0, null, 0);

        const elapsed = std.time.nanoTimestamp() - start;
        const elapsed_ms = @as(f64, @floatFromInt(elapsed)) / 1_000_000.0;

        // Send response
        var response_buf: [256]u8 = undefined;
        const response = std.fmt.bufPrint(&response_buf, "OK {d:.2}ms\n", .{elapsed_ms}) catch "OK\n";
        _ = std.posix.write(client, response) catch {};

        std.debug.print("[daemon] Completed in {d:.2}ms\n", .{elapsed_ms});
    }
}

/// Stop the daemon
fn daemonStop() !void {
    const sock = std.posix.socket(std.posix.AF.UNIX, std.posix.SOCK.STREAM, 0) catch {
        std.debug.print("Daemon not running\n", .{});
        return;
    };
    defer std.posix.close(sock);

    var addr: std.posix.sockaddr.un = .{
        .family = std.posix.AF.UNIX,
        .path = undefined,
    };
    @memcpy(addr.path[0..SOCKET_PATH.len], SOCKET_PATH);
    addr.path[SOCKET_PATH.len] = 0;

    std.posix.connect(sock, @ptrCast(&addr), @sizeOf(std.posix.sockaddr.un)) catch {
        std.debug.print("Daemon not running\n", .{});
        std.fs.deleteFileAbsolute(SOCKET_PATH) catch {};
        return;
    };

    // Send shutdown signal
    _ = std.posix.write(sock, "SHUTDOWN\n") catch {};
    std.debug.print("Daemon stopped\n", .{});
}

/// Check daemon status
fn daemonStatus() !void {
    const sock = std.posix.socket(std.posix.AF.UNIX, std.posix.SOCK.STREAM, 0) catch {
        std.debug.print("Daemon: not running\n", .{});
        return;
    };
    defer std.posix.close(sock);

    var addr: std.posix.sockaddr.un = .{
        .family = std.posix.AF.UNIX,
        .path = undefined,
    };
    @memcpy(addr.path[0..SOCKET_PATH.len], SOCKET_PATH);
    addr.path[SOCKET_PATH.len] = 0;

    std.posix.connect(sock, @ptrCast(&addr), @sizeOf(std.posix.sockaddr.un)) catch {
        std.debug.print("Daemon: not running\n", .{});
        return;
    };

    std.debug.print("Daemon: running (socket: {s})\n", .{SOCKET_PATH});
}

/// Execute script via daemon
fn daemonExec(allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    _ = allocator;

    const sock = std.posix.socket(std.posix.AF.UNIX, std.posix.SOCK.STREAM, 0) catch {
        std.debug.print("Daemon not running. Start with: edgebox start\n", .{});
        std.process.exit(1);
    };
    defer std.posix.close(sock);

    var addr: std.posix.sockaddr.un = .{
        .family = std.posix.AF.UNIX,
        .path = undefined,
    };
    @memcpy(addr.path[0..SOCKET_PATH.len], SOCKET_PATH);
    addr.path[SOCKET_PATH.len] = 0;

    std.posix.connect(sock, @ptrCast(&addr), @sizeOf(std.posix.sockaddr.un)) catch {
        std.debug.print("Daemon not running. Start with: edgebox start\n", .{});
        std.process.exit(1);
    };

    // Send request: script path + args as newlines
    var request_buf: [8192]u8 = undefined;
    var pos: usize = 0;

    for (args) |arg| {
        if (pos + arg.len + 1 < request_buf.len) {
            @memcpy(request_buf[pos .. pos + arg.len], arg);
            pos += arg.len;
            request_buf[pos] = '\n';
            pos += 1;
        }
    }

    _ = std.posix.write(sock, request_buf[0..pos]) catch {
        std.debug.print("Failed to send request\n", .{});
        std.process.exit(1);
    };

    // Read response
    var response_buf: [4096]u8 = undefined;
    const n = std.posix.read(sock, &response_buf) catch {
        std.debug.print("Failed to read response\n", .{});
        std.process.exit(1);
    };

    if (n > 0) {
        std.debug.print("{s}", .{response_buf[0..n]});
    }
}
