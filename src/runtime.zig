/// EdgeBox Native Runtime
/// Embeds WasmEdge C library directly for minimal cold start overhead
///
/// Usage:
///   edgebox build [app-directory]  - Build app (bundle + WASM + AOT)
///   edgebox run <script.js>        - Run JavaScript file
///   edgebox <script.js>            - Run JavaScript file (shorthand)
const std = @import("std");
const builtin = @import("builtin");
const wizer = @import("wizer.zig");
const freeze = @import("freeze/main.zig");
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

    // HTTP security settings
    allowed_urls: []const []const u8 = &.{}, // URL patterns allowed for fetch (glob: https://api.anthropic.com/*)
    blocked_urls: []const []const u8 = &.{}, // URL patterns blocked (takes precedence over allowed)
    rate_limit_rps: u32 = 0, // Max requests per second (0 = unlimited)
    max_connections: u32 = 10, // Max concurrent HTTP connections

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

        // Parse allowedUrls array (HTTP security)
        if (parsed.value.object.get("allowedUrls")) |v| {
            if (v == .array) {
                var urls: std.ArrayListUnmanaged([]const u8) = .{};
                for (v.array.items) |item| {
                    if (item == .string) {
                        urls.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
                    }
                }
                config.allowed_urls = urls.toOwnedSlice(allocator) catch &.{};
            }
        }

        // Parse blockedUrls array (HTTP security)
        if (parsed.value.object.get("blockedUrls")) |v| {
            if (v == .array) {
                var urls: std.ArrayListUnmanaged([]const u8) = .{};
                for (v.array.items) |item| {
                    if (item == .string) {
                        urls.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
                    }
                }
                config.blocked_urls = urls.toOwnedSlice(allocator) catch &.{};
            }
        }

        // Parse rateLimitRps (HTTP security)
        if (parsed.value.object.get("rateLimitRps")) |v| {
            if (v == .integer) {
                config.rate_limit_rps = @intCast(@max(0, v.integer));
            }
        }

        // Parse maxConnections (HTTP security)
        if (parsed.value.object.get("maxConnections")) |v| {
            if (v == .integer) {
                config.max_connections = @intCast(@max(1, v.integer));
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

    /// Serialize dirs config to JSON array for passing to edgebox-sandbox via env var
    pub fn serializeDirs(self: EdgeBoxConfig, allocator: std.mem.Allocator) ?[]const u8 {
        if (self.dirs.len == 0) return null;

        var buf: std.ArrayListUnmanaged(u8) = .{};
        const writer = buf.writer(allocator);

        writer.writeAll("[") catch return null;
        for (self.dirs, 0..) |dir, i| {
            if (i > 0) writer.writeAll(",") catch return null;
            writer.print("\"{s}\"", .{dir}) catch return null;
        }
        writer.writeAll("]") catch return null;

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
        if (std.mem.endsWith(u8, file, ".wasm") or std.mem.endsWith(u8, file, ".aot") or std.mem.endsWith(u8, file, ".dylib") or std.mem.endsWith(u8, file, ".so")) {
            try runWasm(allocator, args[2..]);
        } else {
            try runScript(allocator, args[2..]);
        }
    } else if (std.mem.eql(u8, cmd, "snapshot")) {
        // Wizer-like WASM pre-initialization (pure Zig implementation)
        if (args.len < 4) {
            std.debug.print("Usage: edgeboxc snapshot <input.wasm> <output.wasm> [--init-func=name]\n", .{});
            std.process.exit(1);
        }
        wizer.runSnapshot(allocator, args[2..]) catch |err| {
            std.debug.print("[error] Snapshot failed: {}\n", .{err});
            std.process.exit(1);
        };
    } else if (std.mem.eql(u8, cmd, "optimize")) {
        // WASM optimization using wasm-opt CLI (Binaryen)
        // Use edgebox-wasm-opt if available, otherwise fall back to wasm-opt
        if (args.len < 4) {
            std.debug.print("Usage: edgeboxc optimize <input.wasm> <output.wasm> [-Oz|-Os|-O1|-O2|-O3|-O4]\n", .{});
            std.process.exit(1);
        }
        const opt_level = if (args.len >= 5) args[4] else "-Oz";
        // Try edgebox-wasm-opt first, then fall back to wasm-opt
        const result = std.process.Child.run(.{
            .allocator = allocator,
            .argv = &[_][]const u8{ "wasm-opt", opt_level, args[2], "-o", args[3] },
        }) catch |err| {
            std.debug.print("[error] wasm-opt failed: {}\n", .{err});
            std.process.exit(1);
        };
        if (result.term.Exited != 0) {
            std.debug.print("[error] wasm-opt exited with code {}\n", .{result.term.Exited});
            std.process.exit(1);
        }
        std.debug.print("Optimized: {s} -> {s}\n", .{ args[2], args[3] });
    } else if (std.mem.eql(u8, cmd, "--help") or std.mem.eql(u8, cmd, "-h")) {
        printUsage();
    } else if (std.mem.eql(u8, cmd, "--version") or std.mem.eql(u8, cmd, "-v")) {
        std.debug.print("edgebox {s}\n", .{VERSION});
    } else if (std.mem.endsWith(u8, cmd, ".wasm") or std.mem.endsWith(u8, cmd, ".aot") or std.mem.endsWith(u8, cmd, ".dylib") or std.mem.endsWith(u8, cmd, ".so")) {
        // Direct WASM/AOT execution: edgebox app.wasm or edgebox app.aot
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
        \\  edgeboxc build [app-directory]   Compile JS to WASM with embedded bytecode
        \\  edgeboxc run <file.wasm>         Run compiled WASM module
        \\  edgeboxc <file.wasm>             Run WASM (shorthand)
        \\
        \\Pre-initialization (Wizer):
        \\  edgeboxc snapshot <in.wasm> <out.wasm>  Pre-initialize WASM module
        \\    --init-func=name               Init function name (default: wizer_init)
        \\
        \\WASM Optimization (Binaryen):
        \\  edgeboxc optimize <in.wasm> <out.wasm>  Optimize WASM for size/speed
        \\    -Oz  Aggressive size optimization (default)
        \\    -Os  Size optimization
        \\    -O1/-O2/-O3/-O4  Speed optimization levels
        \\
        \\Daemon Mode (<1ms cold starts):
        \\  edgeboxc start [file.wasm]       Start daemon with pre-loaded WASM
        \\  edgeboxc exec <script.js>        Execute script via daemon
        \\  edgeboxc status                  Check daemon status
        \\  edgeboxc stop                    Stop daemon
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
        \\  edgeboxc build my-app                          Compile app to WASM
        \\  edgeboxc snapshot base.wasm init.wasm          Pre-initialize WASM
        \\  edgeboxc optimize app.wasm app-opt.wasm -Oz    Optimize WASM
        \\  edgeboxc run edgebox-static.wasm               Run the compiled WASM
        \\  edgeboxc start && edgeboxc exec script.js      Run via daemon (<1ms)
        \\
    , .{});
}

/// Clean all build outputs
fn cleanBuildOutputs() void {
    const files_to_clean = [_][]const u8{
        "zig-out/bundle.js",
        "zig-out/bundle_compiled.c",
        "edgebox-static.wasm",
        "edgebox-static.aot",
        "edgebox-base.wasm",
        "edgebox.aot",
        "bundle.js.cache",
        "zig-out/frozen_manifest.json",
    };
    for (files_to_clean) |file| {
        std.fs.cwd().deleteFile(file) catch {};
    }
}

fn runBuild(allocator: std.mem.Allocator, app_dir: []const u8) !void {
    // Check if input is a single JS file or a directory
    const is_js_file = std.mem.endsWith(u8, app_dir, ".js");

    var entry_path_buf: [4096]u8 = undefined;
    var entry_path: []const u8 = undefined;

    if (is_js_file) {
        // Single JS file - use it directly as entry point
        std.debug.print("[build] Entry point: {s}\n", .{app_dir});
        const file = std.fs.cwd().openFile(app_dir, .{}) catch {
            std.debug.print("[error] File not found: {s}\n", .{app_dir});
            std.process.exit(1);
        };
        file.close();
        entry_path = app_dir;
    } else {
        // Directory - find entry point
        std.debug.print("[build] App directory: {s}\n", .{app_dir});

        var dir = std.fs.cwd().openDir(app_dir, .{}) catch {
            std.debug.print("[error] App directory not found: {s}\n", .{app_dir});
            std.process.exit(1);
        };
        dir.close();

        entry_path = findEntryPoint(app_dir, &entry_path_buf) catch {
            std.debug.print("[error] No entry point found in {s} (index.js, main.js, or app.js)\n", .{app_dir});
            std.process.exit(1);
        };
        std.debug.print("[build] Entry point: {s}\n", .{entry_path});
    }

    // Step 3: Bundle with Bun (output to zig-out/)
    std.debug.print("[build] Bundling with Bun...\n", .{});
    std.fs.cwd().makePath("zig-out") catch {};
    const bun_result = try runCommand(allocator, &.{
        "bun", "build", entry_path, "--outfile=zig-out/bundle.js", "--target=node", "--format=cjs", "--minify",
    });
    defer {
        if (bun_result.stdout) |s| allocator.free(s);
        if (bun_result.stderr) |s| allocator.free(s);
    }

    if (bun_result.term.Exited != 0) {
        // Try without minify
        std.debug.print("[warn] Bun minify failed, trying without...\n", .{});
        const retry = try runCommand(allocator, &.{
            "bun", "build", entry_path, "--outfile=zig-out/bundle.js", "--target=node", "--format=cjs",
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
    // Prepend order is reversed - last prepend ends up at top of file
    // We want final order: runtime.js (globals), then node_polyfill.js (modules), then user code
    const node_polyfill_path = "src/polyfills/node_polyfill.js";
    const runtime_path = "src/polyfills/runtime.js";

    // First prepend node_polyfill.js (this will be AFTER runtime.js in final file)
    if (std.fs.cwd().access(node_polyfill_path, .{})) |_| {
        std.debug.print("[build] Prepending Node.js module polyfills...\n", .{});
        try prependPolyfills(allocator, node_polyfill_path, "zig-out/bundle.js");
    } else |_| {}

    // Then prepend runtime.js (this ends up at TOP of file)
    if (std.fs.cwd().access(runtime_path, .{})) |_| {
        std.debug.print("[build] Prepending runtime polyfills...\n", .{});
        try prependPolyfills(allocator, runtime_path, "zig-out/bundle.js");
    } else |_| {}

    // Print bundle size
    if (std.fs.cwd().statFile("zig-out/bundle.js")) |stat| {
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
    const bundle_ok = std.fs.cwd().access("zig-out/bundle.js", .{}) catch null;
    if (wasm_ok != null and bundle_ok != null) {
        std.debug.print("[build] Pre-compiling bytecode cache...\n", .{});

        var cwd_buf: [4096]u8 = undefined;
        const cwd = std.process.getCwd(&cwd_buf) catch ".";

        var bundle_abs_buf: [4096]u8 = undefined;
        const bundle_abs = std.fmt.bufPrint(&bundle_abs_buf, "{s}/bundle.js", .{cwd}) catch "zig-out/bundle.js";

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
        std.debug.print("[build] AOT compiling with WAMR...\n", .{});
        const aot_result = try runCommand(allocator, &.{
            "wamrc", "-o", aot_path, "edgebox-base.wasm",
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
/// All frozen functions stay in WASM/AOT (sandboxed) - no host function exports
fn runStaticBuild(allocator: std.mem.Allocator, app_dir: []const u8) !void {
    // Check if input is a single JS file or a directory
    const is_js_file = std.mem.endsWith(u8, app_dir, ".js");

    // Derive output base name from input
    var output_base_buf: [256]u8 = undefined;
    const output_base = blk: {
        // Get filename from path
        const path_to_use = if (is_js_file) app_dir else app_dir;
        const last_slash = std.mem.lastIndexOf(u8, path_to_use, "/");
        const filename = if (last_slash) |idx| path_to_use[idx + 1 ..] else path_to_use;

        // Remove .js extension if present
        const base = if (std.mem.endsWith(u8, filename, ".js"))
            filename[0 .. filename.len - 3]
        else
            filename;

        // Copy to buffer
        const len = @min(base.len, output_base_buf.len);
        @memcpy(output_base_buf[0..len], base[0..len]);
        break :blk output_base_buf[0..len];
    };

    // Calculate source directory and output directory
    // e.g., bench/hello.js -> source_dir="bench", output_dir="zig-out/bench"
    // All outputs go to zig-out/ but mirror the source folder structure
    var source_dir_buf: [4096]u8 = undefined;
    var output_dir_buf: [4096]u8 = undefined;
    const source_dir: []const u8 = blk: {
        const last_slash = std.mem.lastIndexOf(u8, app_dir, "/");
        if (last_slash) |idx| {
            break :blk app_dir[0..idx];
        }
        break :blk ""; // No parent directory, use root zig-out/
    };
    const output_dir = blk: {
        if (source_dir.len > 0) {
            const len = std.fmt.bufPrint(&output_dir_buf, "zig-out/{s}", .{source_dir}) catch break :blk "zig-out";
            break :blk output_dir_buf[0..len.len];
        }
        break :blk "zig-out";
    };
    // Build -Dsource-dir argument for zig build (tells build.zig where to find bundle_compiled.c)
    const source_dir_arg = if (source_dir.len > 0)
        std.fmt.bufPrint(&source_dir_buf, "-Dsource-dir={s}", .{source_dir}) catch ""
    else
        "";

    // Create output directory
    std.fs.cwd().makePath(output_dir) catch {};

    // Pre-calculate output file paths
    var bundle_js_path_buf: [4096]u8 = undefined;
    const bundle_js_path = std.fmt.bufPrint(&bundle_js_path_buf, "{s}/bundle.js", .{output_dir}) catch "zig-out/bundle.js";

    var bundle_compiled_path_buf: [4096]u8 = undefined;
    const bundle_compiled_path = std.fmt.bufPrint(&bundle_compiled_path_buf, "{s}/bundle_compiled.c", .{output_dir}) catch "zig-out/bundle_compiled.c";

    var bundle_original_path_buf: [4096]u8 = undefined;
    const bundle_original_path = std.fmt.bufPrint(&bundle_original_path_buf, "{s}/bundle_original.c", .{output_dir}) catch "zig-out/bundle_original.c";

    var frozen_functions_path_buf: [4096]u8 = undefined;
    const frozen_functions_path = std.fmt.bufPrint(&frozen_functions_path_buf, "{s}/frozen_functions.c", .{output_dir}) catch "zig-out/frozen_functions.c";

    var frozen_manifest_path_buf: [4096]u8 = undefined;
    const frozen_manifest_path = std.fmt.bufPrint(&frozen_manifest_path_buf, "{s}/frozen_manifest.json", .{output_dir}) catch "zig-out/frozen_manifest.json";

    // Bun --outfile argument
    var bun_outfile_buf: [4096]u8 = undefined;
    const bun_outfile_arg = std.fmt.bufPrint(&bun_outfile_buf, "--outfile={s}/bundle.js", .{output_dir}) catch "--outfile=zig-out/bundle.js";

    var entry_path_buf: [4096]u8 = undefined;
    var entry_path: []const u8 = undefined;

    if (is_js_file) {
        // Single JS file - use it directly as entry point
        std.debug.print("[build] Entry point: {s}\n", .{app_dir});
        const file = std.fs.cwd().openFile(app_dir, .{}) catch {
            std.debug.print("[error] File not found: {s}\n", .{app_dir});
            std.process.exit(1);
        };
        file.close();
        entry_path = app_dir;
    } else {
        // Directory - find entry point
        std.debug.print("[build] App directory: {s}\n", .{app_dir});

        var dir = std.fs.cwd().openDir(app_dir, .{}) catch {
            std.debug.print("[error] App directory not found: {s}\n", .{app_dir});
            std.process.exit(1);
        };
        dir.close();

        entry_path = findEntryPoint(app_dir, &entry_path_buf) catch {
            std.debug.print("[error] No entry point found in {s}\n", .{app_dir});
            std.process.exit(1);
        };
        std.debug.print("[build] Entry point: {s}\n", .{entry_path});
    }

    // Step 3: Bundle with Bun (or skip if file is pre-bundled CommonJS)
    // Detect pre-bundled files by checking size (>1MB typically means pre-bundled)
    // But ESM bundles need conversion to CommonJS for QuickJS compatibility
    const entry_file = std.fs.cwd().openFile(entry_path, .{}) catch {
        std.debug.print("[error] Cannot open entry point: {s}\n", .{entry_path});
        std.process.exit(1);
    };
    defer entry_file.close();
    const entry_stat = entry_file.stat() catch {
        std.debug.print("[error] Cannot stat entry point\n", .{});
        std.process.exit(1);
    };
    const is_large_bundle = entry_stat.size > 1_000_000; // >1MB

    // Check if file is ESM by looking for 'import' statement at start (after shebang)
    var header_buf: [4096]u8 = undefined;
    const header_read = entry_file.read(&header_buf) catch 0;
    entry_file.seekTo(0) catch {};

    // Skip shebang if present to find actual code
    var content_start: usize = 0;
    if (header_read >= 2 and header_buf[0] == '#' and header_buf[1] == '!') {
        var i: usize = 2;
        while (i < header_read and header_buf[i] != '\n') : (i += 1) {}
        if (i < header_read) content_start = i + 1;
    }

    // Skip comments and whitespace to find first real code
    var check_pos = content_start;
    while (check_pos < header_read) {
        // Skip whitespace
        while (check_pos < header_read and (header_buf[check_pos] == ' ' or header_buf[check_pos] == '\t' or header_buf[check_pos] == '\n' or header_buf[check_pos] == '\r')) {
            check_pos += 1;
        }
        // Skip single-line comments
        if (check_pos + 1 < header_read and header_buf[check_pos] == '/' and header_buf[check_pos + 1] == '/') {
            while (check_pos < header_read and header_buf[check_pos] != '\n') : (check_pos += 1) {}
            continue;
        }
        // Skip multi-line comments
        if (check_pos + 1 < header_read and header_buf[check_pos] == '/' and header_buf[check_pos + 1] == '*') {
            check_pos += 2;
            while (check_pos + 1 < header_read and !(header_buf[check_pos] == '*' and header_buf[check_pos + 1] == '/')) : (check_pos += 1) {}
            check_pos += 2;
            continue;
        }
        break;
    }

    // Check if code starts with 'import' (ESM indicator)
    const is_esm = check_pos + 6 < header_read and
        header_buf[check_pos] == 'i' and header_buf[check_pos + 1] == 'm' and
        header_buf[check_pos + 2] == 'p' and header_buf[check_pos + 3] == 'o' and
        header_buf[check_pos + 4] == 'r' and header_buf[check_pos + 5] == 't' and
        (header_buf[check_pos + 6] == ' ' or header_buf[check_pos + 6] == '{');

    if (is_large_bundle and is_esm) {
        // ESM pre-bundled file - need to convert to CommonJS with Bun
        std.debug.print("[build] Detected ESM pre-bundled file ({d}KB), converting to CommonJS...\n", .{entry_stat.size / 1024});
        std.fs.cwd().makePath("zig-out") catch {};
        const bun_result = try runCommand(allocator, &.{
            "bun", "build", entry_path, "--outfile=zig-out/bundle.js", "--target=node", "--format=cjs",
        });
        defer {
            if (bun_result.stdout) |s| allocator.free(s);
            if (bun_result.stderr) |s| allocator.free(s);
        }

        if (bun_result.term.Exited != 0) {
            std.debug.print("[error] Bun ESM->CJS conversion failed\n", .{});
            if (bun_result.stderr) |s| std.debug.print("{s}\n", .{s});
            std.process.exit(1);
        }
    } else if (is_large_bundle) {
        // CommonJS pre-bundled file - just copy with shebang stripped
        std.debug.print("[build] Detected CJS pre-bundled file ({d}KB), skipping Bun...\n", .{entry_stat.size / 1024});

        // Copy entry to bundle.js (skipping shebang)
        entry_file.seekTo(content_start) catch {};
        const out_file = std.fs.cwd().createFile(bundle_js_path, .{}) catch {
            std.debug.print("[error] Cannot create bundle.js\n", .{});
            std.process.exit(1);
        };
        defer out_file.close();

        // Stream copy
        var copy_buf: [65536]u8 = undefined;
        while (true) {
            const n = entry_file.read(&copy_buf) catch break;
            if (n == 0) break;
            out_file.writeAll(copy_buf[0..n]) catch break;
        }
    } else {
        std.debug.print("[build] Bundling with Bun...\n", .{});
        // NOTE: We don't use --minify because it strips function names
        // which prevents frozen function matching by name
        std.fs.cwd().makePath(output_dir) catch {};
        const bun_result = try runCommand(allocator, &.{
            "bun", "build", entry_path, bun_outfile_arg, "--target=node", "--format=cjs",
        });
        defer {
            if (bun_result.stdout) |s| allocator.free(s);
            if (bun_result.stderr) |s| allocator.free(s);
        }

        if (bun_result.term.Exited != 0) {
            std.debug.print("[error] Bun bundling failed\n", .{});
            if (bun_result.stderr) |s| std.debug.print("{s}\n", .{s});
            std.process.exit(1);
        }
    }

    // Step 4: Prepend polyfills
    // Prepend order is reversed - last prepend ends up at top of file
    // We want final order: runtime.js (globals), then node_polyfill.js (modules), then user code
    const node_polyfill_path = "src/polyfills/node_polyfill.js";
    const runtime_path = "src/polyfills/runtime.js";

    // First prepend node_polyfill.js (this will be AFTER runtime.js in final file)
    if (std.fs.cwd().access(node_polyfill_path, .{})) |_| {
        std.debug.print("[build] Prepending Node.js module polyfills...\n", .{});
        try prependPolyfills(allocator, node_polyfill_path, bundle_js_path);
    } else |_| {}

    // Then prepend runtime.js (this ends up at TOP of file)
    if (std.fs.cwd().access(runtime_path, .{})) |_| {
        std.debug.print("[build] Prepending runtime polyfills...\n", .{});
        try prependPolyfills(allocator, runtime_path, bundle_js_path);
    } else |_| {}

    // Check bundle size - skip debug traces for large bundles (>2MB) as they corrupt complex JavaScript
    const skip_traces = if (std.fs.cwd().statFile(bundle_js_path)) |stat| blk: {
        const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
        std.debug.print("[build] Bundle: bundle.js ({d:.1}KB)\n", .{size_kb});
        break :blk stat.size > 2 * 1024 * 1024;
    } else |_| false;

    // Step 5: Patch known issues in bundled code
    // Some bundles set console to a no-op, we replace with a working version
    std.debug.print("[build] Patching bundle for EdgeBox compatibility...\n", .{});
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/console = { log: function() {} };/console = { log: function(a,b,c,d,e) { print(a||'',b||'',c||'',d||'',e||''); } };/g",
        bundle_js_path,
    });

    // Skip all debug trace injection for large bundles (>2MB)
    // These sed patches use hardcoded line numbers specific to old Claude CLI versions
    // and will corrupt newer bundles
    if (skip_traces) {
        std.debug.print("[build] Large bundle - skipping debug trace injection\n", .{});
    }
    if (!skip_traces) {
    // Patch Dp7 call to add debug and await
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^Dp7();$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('before_Dp7'); Dp7().then(function(){ if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Dp7_resolved'); }).catch(function(e){ print('[ENTRY ERROR] ' + e); });/g",
        bundle_js_path,
    });
    // Add trace at first line of user code (var __create = ...)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var __create = Object.create;$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('user_code_start'); var __create = Object.create;/g",
        bundle_js_path,
    });
    // Add trace after first require (node:module)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var import_node_module = require(\"node:module\");$/var import_node_module = require(\"node:module\"); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_node_module_require');/g",
        bundle_js_path,
    });
    // Add trace after fs require
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var import_fs = require(\"fs\");$/var import_fs = require(\"fs\"); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_fs_require');/g",
        bundle_js_path,
    });
    // Add trace after timers/promises require
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var import_promises2 = require(\"node:timers\\/promises\");$/var import_promises2 = require(\"node:timers\\/promises\"); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_timers_require');/g",
        bundle_js_path,
    });
    // Add trace after node:http require
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var import_node_http = require(\"node:http\");$/var import_node_http = require(\"node:http\"); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_http_require');/g",
        bundle_js_path,
    });
    // Add trace around createRequire call (critical point)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var UA = import_node_module.createRequire(\\([^)]*\\));$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('before_createRequire'); var UA = import_node_module.createRequire(\\1); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_createRequire');/g",
        bundle_js_path,
    });
    // Add trace before v9 function definition (around line 4752)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^function v9(A) {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_4752'); function v9(A) {/g",
        bundle_js_path,
    });
    // Add traces at 10K, 25K, 50K, 75K
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var TF1 = z((cR0)/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_10k'); var TF1 = z((cR0)/g",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var EU = z((dN3/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_50k'); var EU = z((dN3/g",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var Dk1 = z((sy3/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_75k'); var Dk1 = z((sy3/g",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var rr1 = z((VoB)/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_150k'); var rr1 = z((VoB)/g",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var XJ0 = z((tl2)/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_250k'); var XJ0 = z((tl2)/g",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var Nz9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_290k'); var Nz9 = O(() => {/g",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var rq9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_300k'); var rq9 = O(() => {/g",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var BN9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_300k_227'); var BN9 = O(() => {/g",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var bN9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_301k'); var bN9 = O(() => {/g",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var WL9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_302k'); var WL9 = O(() => {/g",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var CL9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_303k'); var CL9 = O(() => {/g",
        bundle_js_path,
    });
    // After CL9 IIFE closes, there are module-level calls. Add trace before them.
    // The pattern is "});\nyr();\n" which we'll add trace to
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^yr();$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_CL9_before_yr'); yr();/g",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^dD();$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('dD_start'); dD(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('dD_done');/g",
        bundle_js_path,
    });
    // Add trace to kB function to see what it does
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var kB = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_define'); var kB = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_enter');/g",
        bundle_js_path,
    });
    // Trace at end of kB - the line "  IQ = qq;"
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^  IQ = qq;$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_before_IQ'); IQ = qq; if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_end');/g",
        bundle_js_path,
    });
    // Trace inside kB function calls (line 296501+)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296502s/^  W9();$/  W9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_after_W9');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296503s/i\\$0();/i$0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_after_i$0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296504s/^  m9();$/  m9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_after_m9');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296505s/^  XQ();$/  XQ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_after_XQ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296506s/^  pV();$/  pV(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_after_pV');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296510s/^  ZXA();$/  ZXA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_after_ZXA');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296516s/^  ZHA();$/  ZHA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_after_ZHA');/",
        bundle_js_path,
    });
    // Trace m9 function (line 296311)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296311s/var m9 = O(() => {/var m9 = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296312s/^  m1();$/  m1(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_m1');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296313s/^  W0();$/  W0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_W0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296314s/^  XY();$/  XY(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_XY');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296315s/^  w0();$/  w0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_w0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296316s/r\\$0();/r$0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_r$0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296317s/^  kS();$/  kS(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_kS');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296318s/^  B2();$/  B2(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_B2');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296319s/^  W9();$/  W9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_W9');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296320s/^  XQ();$/  XQ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_XQ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296321s/^  Qu0();$/  Qu0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_Qu0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296322s/^  sX();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_before_sX'); sX(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_sX');/",
        bundle_js_path,
    });
    // Trace sX (line 296077)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296077s/var sX = O(() => {/var sX = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('sX_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296078s/^  f0();$/  f0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('sX_after_f0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296082s/^  s3();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('sX_before_s3'); s3(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('sX_after_s3');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296086s/^  PZ();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('sX_before_PZ'); PZ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('sX_after_PZ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296088s/^  kB();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('sX_before_kB'); kB(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('sX_after_kB');/",
        bundle_js_path,
    });
    // Trace s3 (line 295660)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295660s/var s3 = O(() => {/var s3 = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('s3_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295661s/^  QB();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('s3_before_QB'); QB(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('s3_after_QB');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295665s/^  XQ();$/  XQ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('s3_after_XQ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295668s/^  kB();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('s3_before_kB'); kB(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('s3_after_kB');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295676s/^  tJA();$/  tJA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('s3_after_tJA');/",
        bundle_js_path,
    });
    // Trace QB (line 295050)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295050s/var QB = O(() => {/var QB = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('QB_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295051s/^  XL();$/  XL(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('QB_after_XL');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295054s/^  ax();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('QB_before_ax'); ax(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('QB_after_ax');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295063s/^  HD();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('QB_before_HD'); HD(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('QB_after_HD');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295071s/^  JIA();$/  JIA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('QB_after_JIA');/",
        bundle_js_path,
    });
    // Trace ax (line 293918)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293918s/var ax = O(() => {/var ax = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ax_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293919s/^  p2();$/  p2(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ax_after_p2');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293920s/^  kB();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ax_before_kB'); kB(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ax_after_kB');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293921s/^  Z\\$9();$/  Z$9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ax_after_Z$9');/",
        bundle_js_path,
    });
    // Trace Z$9 (line 293884)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293884s/var Z\\$9 = O(() => {/var Z$9 = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Z$9_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293885s/^  W9();$/  W9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Z$9_after_W9');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293887s/^  Nx();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Z$9_before_Nx'); Nx(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Z$9_after_Nx');/",
        bundle_js_path,
    });
    // Trace Nx (line 293850)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293850s/var Nx = O(() => {/var Nx = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Nx_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293851s/^  kS();$/  kS(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Nx_after_kS');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293852s/^  _WA();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Nx_before__WA'); _WA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Nx_after__WA');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293857s/^  kB();$/  kB(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Nx_after_kB');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293860s/^  W9();$/  W9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Nx_end');/",
        bundle_js_path,
    });
    // Trace _WA (line 293693)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293693s/var _WA = O(() => {/var _WA = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('_WA_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293694s/^  PZ();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('_WA_before_PZ'); PZ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('_WA_after_PZ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293698s/^  kB();$/  kB(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('_WA_after_kB');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293702s/^  d9();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('_WA_before_d9'); d9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('_WA_after_d9');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293705s/^  AN();$/  AN(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('_WA_end');/",
        bundle_js_path,
    });
    // Trace PZ (line 293570)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293570s/var PZ = O(() => {/var PZ = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293571s/^  lV();$/  lV(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_after_lV');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293577s/^  HK();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_before_HK'); HK(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_after_HK');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293580s/^  G\\$();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_before_G$'); G$(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_after_G$');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293582s/^  d9();$/  d9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_end');/",
        bundle_js_path,
    });
    // Trace G$ (line 293244)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293244s/var G\\$ = O(() => {/var G$ = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('G$_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293245s/^  W9();$/  W9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('G$_after_W9');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293246s/^  UY();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('G$_before_UY'); UY(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('G$_after_UY');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293252s/^  f0();$/  f0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('G$_end');/",
        bundle_js_path,
    });
    // Trace UY (line 292763)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "292763s/var UY = O(() => {/var UY = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('UY_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "292764s/^  LC1();$/  LC1(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('UY_after_LC1');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "292765s/^  LzB();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('UY_before_LzB'); LzB(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('UY_after_LzB');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "292799s/^  Ky();$/  Ky(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('UY_end');/",
        bundle_js_path,
    });
    // Trace LzB (line 95212)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95212s/var LzB = O(() => {/var LzB = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95213s/^  oQ();$/  oQ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_after_oQ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95214s/^  pS();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_before_pS'); pS(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_after_pS');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95215s/^  HD();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_before_HD'); HD(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_after_HD');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95216s/^  lS();$/  lS(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_after_lS');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95217s/^  C8A();$/  C8A(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_after_C8A');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95218s/^  d9();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_before_d9'); d9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_end');/",
        bundle_js_path,
    });
    // Trace d9 (line 95128)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95128s/var d9 = O(() => {/var d9 = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95129s/^  W9();$/  W9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_after_W9');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95130s/^  n7();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_before_n7'); n7(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_after_n7');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95131s/^  Yt0();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_before_Yt0'); Yt0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_after_Yt0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95132s/^  Xt0();$/  Xt0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_after_Xt0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95133s/^  yf();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_before_yf'); yf(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_after_yf');/",
        bundle_js_path,
    });
    // Trace yf (line 87524)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87524s/var yf = O(() => {/var yf = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('yf_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87525s/^  bQ();$/  bQ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('yf_after_bQ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87528s/^  o8();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('yf_before_o8'); o8(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('yf_end');/",
        bundle_js_path,
    });
    // Trace bQ (line 87475)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87475s/var bQ = O(() => {/var bQ = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('bQ_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87476s/^  fvA();$/  fvA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('bQ_after_fvA');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87485s/^  m9();$/  m9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('bQ_after_m9');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87486s/^  J2();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('bQ_before_J2'); J2(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('bQ_after_J2');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87488s/^  m1();$/  m1(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('bQ_after_m1');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87489s/^  kB();$/  kB(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('bQ_end');/",
        bundle_js_path,
    });
    // Trace J2 (line 87124) - potential circular dep with bQ
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87124s/var J2 = O(() => {/var J2 = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87125s/^  bQ();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_before_bQ'); bQ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_after_bQ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87126s/^  kB();$/  kB(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_after_kB');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87127s/^  o8();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_before_o8'); o8(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_after_o8');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87130s/^  m1();$/  m1(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_after_m1');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87131s/^  W0();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_before_W0'); W0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_after_W0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87132s/^  L9();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_before_L9'); L9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_after_L9');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87133s/^  AhA();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_before_AhA'); AhA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_after_AhA');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87134s/^  We0();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_before_We0'); We0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_after_We0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87135s/^  OL();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_before_OL'); OL(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_after_OL');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87136s/^  ZhA();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_before_ZhA'); ZhA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_after_ZhA');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87138s/^  XQ();$/  XQ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_after_XQ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87139s/^  oQ();$/  oQ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_after_oQ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87140s/^  KEA();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_before_KEA'); KEA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_after_KEA');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87141s/^  f0();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_before_f0'); f0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_after_f0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87142s/^  Uj1();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_before_Uj1'); Uj1(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_after_Uj1');/",
        bundle_js_path,
    });
    // Trace Uj1 (line 60463)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "60463s/var Uj1 = O(() => {/var Uj1 = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Uj1_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "60464s/^  W0();$/  W0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Uj1_after_W0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "60465s/^  QcA = e(LWQ(), 1), mjQ/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Uj1_before_LWQ'); QcA = e(LWQ(), 1); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Uj1_after_LWQ'); mjQ/",
        bundle_js_path,
    });
    // Trace mEA (AWS STSClient) at line 48788
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "48788s/var mEA = z((VL1) => {/var mEA = z((VL1) => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('mEA_STSClient_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "48790s/VL1.STSClient = VL1.__Client = undefined;/VL1.STSClient = VL1.__Client = undefined; if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('mEA_after_init');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "48791s/var vIQ = FEA(),/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('mEA_before_FEA'); var vIQ = FEA(),/",
        bundle_js_path,
    });
    // Trace FEA (hostHeader) at line 36064
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "36064s/var FEA = z((Tz3, pe0) => {/var FEA = z((Tz3, pe0) => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('FEA_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "36078s/var tF4 = LC();/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('FEA_before_LC'); var tF4 = LC(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('FEA_after_LC');/",
        bundle_js_path,
    });
    // Add trace after each module in mEA's var chain
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "48791s/$m4 = DEA()/$m4 = DEA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('mEA_after_DEA'); var __tmp1/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "48791s/wm4 = HEA()/wm4 = HEA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('mEA_after_HEA'); var __tmp2/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "48791s/bIQ = q6A()/bIQ = q6A(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('mEA_after_q6A'); var __tmp3/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "48791s/gIQ = O6()/gIQ = O6(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('mEA_after_O6')/",
        bundle_js_path,
    });
    // Trace q6A (user-agent middleware) at line 44303
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "44303s/var q6A = z((Kw3, D8Q) => {/var q6A = z((Kw3, D8Q) => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('q6A_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "44317s/var IR4 = X2()/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('q6A_before_X2'); var IR4 = X2(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('q6A_after_X2')/",
        bundle_js_path,
    });
    // Also trace export line (before X2)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "44316s/D8Q.exports = XR4(Y8Q);/D8Q.exports = XR4(Y8Q); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('q6A_after_export');/",
        bundle_js_path,
    });
    // Trace X2 (HTTP Auth Scheme) at line 39644
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "39644s/var X2 = z((eU3, PBQ) => {/var X2 = z((eU3, PBQ) => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('X2_enter');/",
        bundle_js_path,
    });
    // SIMPLIFIED TRACE: Just print BEFORE and AFTER F$1() without breaking var chain
    // The original: var AgA = F$1(), Ww4 = qJ(...), UBQ = g3(), ...
    // We trace by wrapping F$1() in a logging IIFE that preserves the var chain
    // NOTE: Line numbers depend on polyfill size, currently 39657
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "39657s/var AgA = F\\$1(), Ww4 = qJ/var AgA = (print('[TRACE] X2_before_F$1'),F$1()), Ww4 = (print('[TRACE] X2_after_F$1'),qJ/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "39657s/PBQ.exports = Iw4(zBQ);/PBQ.exports = Iw4(zBQ); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('X2_after_export');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "39658s/UBQ = g3()/UBQ = g3(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('X2_after_g3')/",
        bundle_js_path,
    });
    // Trace g3 at line 36255
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "36255s/var g3 = z((yz3, qAQ) => {/var g3 = z((yz3, qAQ) => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('g3_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "36269s/var UAQ = zAQ()/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('g3_before_zAQ'); var UAQ = zAQ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('g3_after_zAQ')/",
        bundle_js_path,
    });
    // Trace e4 at line 38303
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "38303s/var e4 = z((_U3, dQQ) => {/var e4 = z((_U3, dQQ) => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('e4_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "38322s/OU4 = ko()/OU4 = ko(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('e4_after_ko')/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "38322s/RU4 = g3()/RU4 = g3(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('e4_after_g3')/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "44337s/var WR4 = E6A()/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('q6A_before_E6A'); var WR4 = E6A(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('q6A_after_E6A')/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87145s/^  db1();$/  db1(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_after_db1');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "87146s/^  H\\$A();$/  H$A(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('J2_end');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95136s/^  TGA();$/  TGA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_after_TGA');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95137s/^  He();$/  He(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_after_He');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296324s/^  EJ();$/  EJ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_EJ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296325s/^  v\\$9 = /  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_end'); v$9 = /",
        bundle_js_path,
    });
    // Trace kB using sed with address (line 296486)
    // First, add trace after W9() in kB
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296487s/^  W9();$/  W9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_after_W9');/",
        bundle_js_path,
    });
    // Add trace after i$0()
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296488s/i\\$0();/i$0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_after_i$0');/",
        bundle_js_path,
    });
    // Add trace after m9()
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296489s/kB_m9_done/kB_after_m9/",
        bundle_js_path,
    });
    // Add trace after XQ() in kB (line 296490)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296490s/XQ();/XQ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_after_XQ');/",
        bundle_js_path,
    });
    // Add trace after pV()
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296491s/pV();/pV(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_after_pV');/",
        bundle_js_path,
    });
    // Add trace after iK()
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296495s/iK();/iK(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_after_iK');/",
        bundle_js_path,
    });
    // Add trace after ZXA()
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296496s/ZXA();/ZXA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_after_ZXA');/",
        bundle_js_path,
    });
    // Trace inside m9 (line 296296)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296297s/m1();/m1(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_m1');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296298s/W0();/W0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_W0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296299s/XY();/XY(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_XY');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296300s/w0();/w0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_w0');/",
        bundle_js_path,
    });
    // r$0 is at line 296301
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296301s/r\\$0();/r$0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_r$0');/",
        bundle_js_path,
    });
    // kS is at line 296302
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296302s/kS();/kS(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_kS');/",
        bundle_js_path,
    });
    // B2 at 296303
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296303s/B2();/B2(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_B2');/",
        bundle_js_path,
    });
    // W9 at 296304
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296304s/W9();/W9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_W9');/",
        bundle_js_path,
    });
    // Qu0 at 296306
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296306s/Qu0();/Qu0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_Qu0');/",
        bundle_js_path,
    });
    // sX at 296307
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296307s/sX();/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_before_sX'); sX(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_sX');/",
        bundle_js_path,
    });
    // U7 - need to find line
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296308s/U7();/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_before_U7'); U7(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_U7');/",
        bundle_js_path,
    });
    // EJ at 296309
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296309s/EJ();/EJ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_after_EJ');/",
        bundle_js_path,
    });
    // Trace inside sX (line 296062)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296062s/var sX = O(() => {/var sX = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('sX_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296063s/f0();/f0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('sX_after_f0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296064s/B2();/B2(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('sX_after_B2');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296065s/EJ();/EJ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('sX_after_EJ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296066s/g4A();/g4A(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('sX_after_g4A');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296067s/s3();/s3(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('sX_after_s3');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296068s/U7();/U7(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('sX_after_U7');/",
        bundle_js_path,
    });
    // Trace inside s3 (line 295645)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295645s/var s3 = O(() => {/var s3 = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('s3_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295646s/QB();/QB(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('s3_after_QB');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295647s/f0();/f0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('s3_after_f0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295654s/h50();/h50(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('s3_after_h50');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295659s/tJA();/tJA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('s3_after_tJA');/",
        bundle_js_path,
    });
    // Trace inside QB (line 295035)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295035s/var QB = O(() => {/var QB = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('QB_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295036s/XL();/XL(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('QB_after_XL');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295037s/m4A();/m4A(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('QB_after_m4A');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295038s/w0();/w0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('QB_after_w0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295039s/ax();/ax(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('QB_after_ax');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295040s/PO();/PO(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('QB_after_PO');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295041s/FD();/FD(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('QB_after_FD');/",
        bundle_js_path,
    });
    // Trace ax (line 293903)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293903s/var ax = O(() => {/var ax = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ax_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293904s/p2();/p2(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ax_after_p2');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293905s/kB();/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ax_before_kB'); kB(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ax_after_kB');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293906s/Z\\$9();/Z$9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ax_after_Z$9');/",
        bundle_js_path,
    });
    // Trace Z$9 (line 293869)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293869s/var Z\\$9 = O(() => {/var Z$9 = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Z$9_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293870s/W9();/W9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Z$9_after_W9');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293871s/m1();/m1(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Z$9_after_m1');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293872s/Nx();/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Z$9_before_Nx'); Nx(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Z$9_after_Nx');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293873s/uI1();/uI1(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Z$9_after_uI1');/",
        bundle_js_path,
    });
    // Trace Nx (line 293835)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293835s/var Nx = O(() => {/var Nx = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Nx_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293836s/kS();/kS(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Nx_after_kS');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293837s/_WA();/_WA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Nx_after_WA');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293842s/w0();/w0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Nx_after_w0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293843s/iK();/iK(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Nx_after_iK');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293844s/W9();/W9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Nx_after_W9');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293845s/LB9 = /if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Nx_before_LB9'); LB9 = /",
        bundle_js_path,
    });
    // Trace _WA (line 293678)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293678s/var _WA = O(() => {/var _WA = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('_WA_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293679s/PZ();/PZ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('_WA_after_PZ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293680s/rr();/rr(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('_WA_after_rr');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293681s/Ow();/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('_WA_before_Ow'); Ow(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('_WA_after_Ow');/",
        bundle_js_path,
    });
    // Trace PZ (line 293555)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293555s/var PZ = O(() => {/var PZ = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293556s/lV();/lV(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_after_lV');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293557s/IY();/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_before_IY'); IY(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_after_IY');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293558s/m1();/m1(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_after_m1');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293559s/W0();/W0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_after_W0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293560s/rr();/rr(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_after_rr');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293561s/iK();/iK(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_after_iK');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293562s/HK();/HK(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_after_HK');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293563s/tX();/tX(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_after_tX');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293564s/Ow();/Ow(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_after_Ow');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293565s/G\\$();/G$(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_after_G$');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293566s/aU9();/aU9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_after_aU9');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293567s/d9();/d9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_after_d9');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293568s/EH0 = /if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('PZ_end'); EH0 = /",
        bundle_js_path,
    });
    // Trace G$ (line 293229)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293229s/var G\\$ = O(() => {/var G$ = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('G$_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293230s/W9();/W9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('G$_after_W9');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293231s/UY();/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('G$_before_UY'); UY(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('G$_after_UY');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293235s/L9();/L9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('G$_after_L9');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "293236s/f0();/f0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('G$_after_f0');/",
        bundle_js_path,
    });
    // Trace UY (line 292748)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "292748s/var UY = O(() => {/var UY = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('UY_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "292749s/LC1();/LC1(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('UY_after_LC1');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "292750s/LzB();/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('UY_before_LzB'); LzB(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('UY_after_LzB');/",
        bundle_js_path,
    });
    // Trace LzB (line 95197)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95197s/var LzB = O(() => {/var LzB = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95198s/oQ();/oQ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_after_oQ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95199s/pS();/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_before_pS'); pS(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_after_pS');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95200s/HD();/HD(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_after_HD');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95201s/^  lS();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_before_lS'); lS(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_after_lS');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95202s/^  C8A();$/  C8A(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_after_C8A');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95203s/^  d9();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_before_d9'); d9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_after_d9');/",
        bundle_js_path,
    });
    // Trace d9 (line 95113)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95113s/var d9 = O(() => {/var d9 = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95114s/^  W9();$/  W9(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_after_W9');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95115s/^  n7();$/  n7(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_after_n7');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95120s/^  TGA();$/  TGA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_after_TGA');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95121s/^  He();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_before_He'); He(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('d9_after_He');/",
        bundle_js_path,
    });
    // Trace n7 (line 30594)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30594s/var n7 = O(() => {/var n7 = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('n7_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30595s/^  Ns0();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('n7_before_Ns0'); Ns0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('n7_after_Ns0');/",
        bundle_js_path,
    });
    // Trace Ns0 (line 30542)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30542s/var Ns0 = O(() => {/var Ns0 = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Ns0_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30543s/^  SZ();$/  SZ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Ns0_after_SZ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30544s/^  Es0();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Ns0_before_Es0'); Es0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Ns0_after_Es0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30550s/^  aS();$/  aS(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Ns0_after_aS');/",
        bundle_js_path,
    });
    // Trace Es0 (line 30420)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30420s/var Es0 = O(() => {/var Es0 = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Es0_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30421s/^  SZ();$/  SZ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Es0_after_SZ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30422s/^  pbA();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Es0_before_pbA'); pbA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Es0_after_pbA');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30423s/^  Ga0();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Es0_before_Ga0'); Ga0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Es0_after_Ga0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30424s/^  Fs0();$/  Fs0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Es0_after_Fs0');/",
        bundle_js_path,
    });
    // Trace Fs0 (line 30269)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30269s/var Fs0 = O(() => {/var Fs0 = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Fs0_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30270s/^  Ea0();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Fs0_before_Ea0'); Ea0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Fs0_after_Ea0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30271s/^  nbA();$/  nbA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Fs0_after_nbA');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30272s/^  Co();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Fs0_before_Co'); Co(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Fs0_after_Co');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30273s/^  aS();$/  aS(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Fs0_after_aS');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30274s/^  fz1();$/  fz1(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Fs0_end');/",
        bundle_js_path,
    });
    // Trace fz1 (line 30212)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30212s/var fz1 = O(() => {/var fz1 = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('fz1_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30213s/^  SZ();$/  SZ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('fz1_after_SZ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30214s/^  ra0();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('fz1_before_ra0'); ra0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('fz1_after_ra0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30215s/^  Gs0();$/  Gs0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('fz1_after_Gs0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30217s/^  bz1 = /  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('fz1_before_bz1'); bz1 = /",
        bundle_js_path,
    });
    // Trace ra0 (line 29594) - HTTP adapter
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29594s/var ra0 = O(() => {/var ra0 = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29595s/^  SZ();$/  SZ(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_SZ');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29596s/^  sbA();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_before_sbA'); sbA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_sbA');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29597s/^  rbA();$/  rbA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_rbA');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29598s/^  pbA();$/  pbA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_pbA');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29599s/^  lbA();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_before_lbA'); lbA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_lbA');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29600s/^  Pw();$/  Pw(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_Pw');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29601s/^  Co();$/  Co(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_Co');/",
        bundle_js_path,
    });
    // Trace UT (line 28554)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "28554s/var UT = O(() => {/var UT = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('UT_enter');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "28555s/^  Wa0();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('UT_before_Wa0'); Wa0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('UT_after_Wa0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "28556s/^  Ka0();$/  Ka0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('UT_after_Ka0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "28557s/^  E3 = /  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('UT_end'); E3 = /",
        bundle_js_path,
    });
    // Add trace after UT in ra0 - need to update the line after Co which is 29602 (but shifted)
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29602s/^  UT();$/  UT(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_UT');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29603s/^  Oa0();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_before_Oa0'); Oa0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_Oa0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29604s/^  aS();$/  aS(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_aS');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29605s/^  Ta0();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_before_Ta0'); Ta0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_Ta0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29606s/^  _a0();$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_before__a0'); _a0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after__a0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29607s/^  yz1();$/  yz1(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_yz1');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29608s/^  xa0();$/  xa0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_xa0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29609s/^  ba0();$/  ba0(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_ba0');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29610s/^  tbA();$/  tbA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_tbA');/",
        bundle_js_path,
    });
    // Split the complex line to trace each part
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "29611s/^  ia0 = e(za0(), 1), na0 = e(Ma0(), 1), ma0/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_before_za0'); ia0 = e(za0(), 1); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_za0'); na0 = e(Ma0(), 1); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('ra0_after_Ma0'); ma0/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "30427s/^  aS();$/  aS(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Es0_after_aS');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "95204s/^  NW6 = /  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('LzB_end'); NW6 = /",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295044s/HD();/HD(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('QB_after_HD');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "295053s/JIA();/JIA(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('QB_after_JIA');/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296309s/v\\$9 = e/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_before_v$9'); v$9 = e/",
        bundle_js_path,
    });
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "296320s/BW1 = YV1/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('m9_before_BW1'); BW1 = YV1/",
        bundle_js_path,
    });
    // Add trace right before the Commander.js instance creation at line 303469
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/^var vs = new YW1()/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('before_commander'); var vs = new YW1()/g",
        bundle_js_path,
    });
    // Add trace at v9("cli_entry")
    _ = try runCommand(allocator, &.{
        "sed", "-i", "",
        "s/v9(\"cli_entry\")/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('cli_entry_v9'); v9(\"cli_entry\")/g",
        bundle_js_path,
    });
    } // end skip_traces

    // Step 5b: Build qjsc if not exists (need it for freezing step)
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

    // Use same skip_traces flag for freeze (both are skipped for large bundles >2MB)
    const skip_freeze = skip_traces;

    if (skip_freeze) {
        std.debug.print("[build] Large bundle detected - skipping freeze optimization\n", .{});
    }

    // Step 6: Compile ORIGINAL JS to bytecode (BEFORE hooks - for freezing)
    // We freeze the original bytecode because hooks use get_var/get_field which can't be frozen
    if (!skip_freeze) {
        std.debug.print("[build] Compiling original JS to bytecode for freezing...\n", .{});
    }
    const qjsc_orig = if (!skip_freeze) try runCommand(allocator, &.{
        "zig-out/bin/qjsc",
        "-N", "bundle",
        "-o", bundle_original_path,
        bundle_js_path,
    }) else null;
    defer {
        if (qjsc_orig) |result| {
            if (result.stdout) |s| allocator.free(s);
            if (result.stderr) |s| allocator.free(s);
        }
    }
    if (qjsc_orig) |result| {
        if (result.term.Exited != 0) {
            std.debug.print("[error] qjsc compilation failed\n", .{});
            if (result.stderr) |err| std.debug.print("{s}\n", .{err});
            std.process.exit(1);
        }
    }

    // Step 6b: Freeze ORIGINAL bytecode to optimized C (direct API call, no subprocess)
    const freeze_success = if (skip_freeze) false else blk: {
        std.debug.print("[build] Freezing original bytecode to optimized C...\n", .{});
        // Read the bytecode C file
        const bytecode_file = std.fs.cwd().openFile(bundle_original_path, .{}) catch |err| {
            std.debug.print("[warn] Could not open bundle_original.c: {}\n", .{err});
            break :blk false;
        };
        defer bytecode_file.close();

        const bytecode_content = bytecode_file.readToEndAlloc(allocator, 50 * 1024 * 1024) catch |err| {
            std.debug.print("[warn] Could not read bundle_original.c: {}\n", .{err});
            break :blk false;
        };
        defer allocator.free(bytecode_content);

        // Call freeze API directly (no subprocess)
        // All frozen functions stay in WASM/AOT (sandboxed) - no host exports
        const frozen_code = freeze.freezeModule(allocator, bytecode_content, "frozen", false) catch |err| {
            std.debug.print("[warn] Freeze failed: {} (continuing with interpreter)\n", .{err});
            break :blk false;
        };
        defer allocator.free(frozen_code);

        // Write frozen C code
        const frozen_file = std.fs.cwd().createFile(frozen_functions_path, .{}) catch |err| {
            std.debug.print("[warn] Could not create frozen_functions.c: {}\n", .{err});
            break :blk false;
        };
        defer frozen_file.close();

        frozen_file.writeAll(frozen_code) catch |err| {
            std.debug.print("[warn] Could not write frozen_functions.c: {}\n", .{err});
            break :blk false;
        };

        const size_kb = @as(f64, @floatFromInt(frozen_code.len)) / 1024.0;
        std.debug.print("[build] Frozen functions: {s} ({d:.1}KB)\n", .{frozen_functions_path, size_kb});
        break :blk true;
    };

    if (!freeze_success) {
        // Create empty stub so build doesn't break
        const empty_frozen = std.fs.cwd().createFile(frozen_functions_path, .{}) catch null;
        if (empty_frozen) |f| {
            f.writeAll(
                \\// No frozen functions generated
                \\#include "quickjs.h"
                \\int frozen_init(JSContext *ctx) { (void)ctx; return 0; }
                \\
            ) catch {};
            f.close();
        }
    }

    // Step 6c: Inject frozen function hooks into bundle.js (skip for large bundles)
    // This adds: if(globalThis.__frozen_NAME) return globalThis.__frozen_NAME(args);
    if (!skip_freeze and freeze_success) {
        std.debug.print("[build] Injecting frozen function hooks...\n", .{});
        const inject_result = try runCommand(allocator, &.{
            "node", "tools/inject_hooks.js", bundle_js_path, bundle_js_path, frozen_manifest_path,
        });
        defer {
            if (inject_result.stdout) |s| allocator.free(s);
            if (inject_result.stderr) |s| allocator.free(s);
        }
        if (inject_result.term.Exited != 0) {
            std.debug.print("[warn] Hook injection failed (non-fatal)\n", .{});
            if (inject_result.stderr) |s| std.debug.print("{s}\n", .{s});
        }
    }

    // Step 6d: Compile HOOKED JS to bytecode (for runtime)
    std.debug.print("[build] Compiling hooked JS to bytecode for runtime...\n", .{});
    // Ensure output directory exists
    std.fs.cwd().makePath(output_dir) catch {};
    const qjsc_result = try runCommand(allocator, &.{
        "zig-out/bin/qjsc",
        "-N", "bundle",
        "-o", bundle_compiled_path,
        bundle_js_path,
    });
    defer {
        if (qjsc_result.stdout) |s| allocator.free(s);
        if (qjsc_result.stderr) |s| allocator.free(s);
    }
    if (qjsc_result.term.Exited != 0) {
        std.debug.print("[error] qjsc compilation failed\n", .{});
        if (qjsc_result.stderr) |err| std.debug.print("{s}\n", .{err});
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
    const bridge_file = std.fs.cwd().openFile(bundle_compiled_path, .{ .mode = .read_write }) catch {
        std.debug.print("[error] Failed to open {s}\n", .{bundle_compiled_path});
        std.process.exit(1);
    };
    defer bridge_file.close();
    bridge_file.seekFromEnd(0) catch {};
    bridge_file.writeAll(bridge_code) catch {};

    if (std.fs.cwd().statFile(bundle_compiled_path)) |stat| {
        const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
        std.debug.print("[build] Bytecode: {s} ({d:.1}KB)\n", .{ bundle_compiled_path, size_kb });
    } else |_| {}

    // Step 7: Build WASM with embedded bytecode
    // All frozen functions stay in WASM/AOT (sandboxed)
    // build.zig reads from <source-dir>/zig-out/ via -Dsource-dir parameter
    std.debug.print("[build] Building static WASM with embedded bytecode...\n", .{});
    const wasm_result = if (source_dir_arg.len > 0)
        try runCommand(allocator, &.{
            "zig", "build", "wasm-static", "-Doptimize=ReleaseFast", source_dir_arg,
        })
    else
        try runCommand(allocator, &.{
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

    // Generate output filenames based on input base name and output directory
    var wasm_path_buf: [4096]u8 = undefined;
    var aot_path_buf: [4096]u8 = undefined;
    var stripped_path_buf: [4096]u8 = undefined;

    const wasm_path = std.fmt.bufPrint(&wasm_path_buf, "{s}/{s}.wasm", .{ output_dir, output_base }) catch "output.wasm";
    const aot_path = std.fmt.bufPrint(&aot_path_buf, "{s}/{s}.aot", .{ output_dir, output_base }) catch "output.aot";
    const stripped_path = std.fmt.bufPrint(&stripped_path_buf, "{s}/{s}-stripped.wasm", .{ output_dir, output_base }) catch "output-stripped.wasm";

    // Copy from zig-out with output name based on input
    std.fs.cwd().copyFile("zig-out/bin/edgebox-static.wasm", std.fs.cwd(), wasm_path, .{}) catch {
        std.debug.print("[error] Failed to copy WASM\n", .{});
        std.process.exit(1);
    };

    if (std.fs.cwd().statFile(wasm_path)) |stat| {
        const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
        std.debug.print("[build] Static WASM: {s} ({d:.1}KB)\n", .{ wasm_path, size_kb });
    } else |_| {}

    // Step 8: Strip debug sections BEFORE Wizer (reduces size significantly)
    std.debug.print("[build] Stripping debug sections...\n", .{});
    stripWasmDebug(allocator, wasm_path, stripped_path) catch |err| {
        std.debug.print("[warn] Debug strip failed: {}\n", .{err});
    };
    std.fs.cwd().deleteFile(wasm_path) catch {};
    std.fs.cwd().rename(stripped_path, wasm_path) catch {};

    // Step 9: Wizer pre-initialization (snapshot QuickJS runtime state)
    // This eliminates ~10-15ms of JS_NewRuntime/JS_NewContext on every cold start
    // With Wizer: larger binary but instant startup (daemon pool: 64ms → 3ms)
    try runWizerStatic(allocator);

    // Step 10: wasm-opt (optional further optimization)
    try runWasmOptStaticWithPath(allocator, wasm_path);

    // Step 10: AOT compile to .aot (platform-agnostic extension)
    std.debug.print("[build] AOT compiling with WAMR...\n", .{});
    // Use local wamrc from zig-out/bin (built from vendor/wamr/wamr-compiler)
    const aot_result = try runCommand(allocator, &.{
        "zig-out/bin/wamrc", "-o", aot_path, wasm_path,
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
    std.debug.print("  {s}   - WASM with embedded bytecode\n", .{wasm_path});
    std.debug.print("  {s}  - AOT native module\n\n", .{aot_path});
    std.debug.print("To run:\n", .{});
    std.debug.print("  edgebox {s}\n", .{wasm_path});
    std.debug.print("  edgebox {s}\n\n", .{aot_path});
}

fn runWizerStatic(allocator: std.mem.Allocator) !void {
    // Use our own edgebox-wizer (WAMR-based) instead of external wizer CLI
    const wizer_path = "zig-out/bin/edgebox-wizer";

    // Check if edgebox-wizer exists
    std.fs.cwd().access(wizer_path, .{}) catch {
        std.debug.print("[build] edgebox-wizer not found - run 'zig build wizer' first\n", .{});
        return;
    };

    std.debug.print("[build] Running Wizer pre-initialization (using edgebox-wizer)...\n", .{});
    const wizer_result = try runCommand(allocator, &.{
        wizer_path,
        "edgebox-static.wasm",
        "-o",
        "edgebox-static-wizer.wasm",
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

/// Strip debug sections from WASM file (pure Zig, no external tools)
/// Removes all custom sections starting with ".debug" or "name"
fn stripWasmDebug(allocator: std.mem.Allocator, input_path: []const u8, output_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_path, .{});
    defer file.close();

    const data = try file.readToEndAlloc(allocator, 100 * 1024 * 1024); // 100MB max
    defer allocator.free(data);

    const original_size = data.len;

    // WASM magic + version = 8 bytes
    if (data.len < 8 or !std.mem.eql(u8, data[0..4], "\x00asm")) {
        return error.InvalidWasm;
    }

    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(allocator);

    // Copy header
    try output.appendSlice(allocator, data[0..8]);

    var pos: usize = 8;
    while (pos < data.len) {
        const section_id = data[pos];
        pos += 1;

        // Read section size (LEB128)
        var section_size: u32 = 0;
        var shift: u5 = 0;
        while (true) {
            if (pos >= data.len) break;
            const byte = data[pos];
            pos += 1;
            section_size |= @as(u32, byte & 0x7f) << shift;
            if (byte & 0x80 == 0) break;
            shift +|= 7;
        }

        const section_start = pos;
        const section_end = pos + section_size;

        // Custom section (id 0) - check if debug
        if (section_id == 0 and section_size > 0) {
            // Read name length (LEB128)
            var name_len: u32 = 0;
            var name_shift: u5 = 0;
            var name_pos = section_start;
            while (name_pos < section_end) {
                const byte = data[name_pos];
                name_pos += 1;
                name_len |= @as(u32, byte & 0x7f) << name_shift;
                if (byte & 0x80 == 0) break;
                name_shift +|= 7;
            }

            if (name_pos + name_len <= section_end) {
                const name = data[name_pos .. name_pos + name_len];
                // Skip debug sections
                if (std.mem.startsWith(u8, name, ".debug") or
                    std.mem.eql(u8, name, "name") or
                    std.mem.startsWith(u8, name, "sourceMappingURL"))
                {
                    pos = section_end;
                    continue; // Skip this section
                }
            }
        }

        // Keep this section - write section id
        try output.append(allocator, section_id);

        // Write section size (LEB128)
        var size = section_size;
        while (true) {
            const byte: u8 = @truncate(size & 0x7f);
            size >>= 7;
            if (size == 0) {
                try output.append(allocator, byte);
                break;
            } else {
                try output.append(allocator, byte | 0x80);
            }
        }

        // Write section data
        try output.appendSlice(allocator, data[section_start..section_end]);
        pos = section_end;
    }

    // Write output
    const out_file = try std.fs.cwd().createFile(output_path, .{});
    defer out_file.close();
    try out_file.writeAll(output.items);

    const new_size = output.items.len;
    const saved = original_size - new_size;
    const saved_kb = @as(f64, @floatFromInt(saved)) / 1024.0;
    const new_kb = @as(f64, @floatFromInt(new_size)) / 1024.0;
    std.debug.print("[build] Stripped debug: {d:.1}KB -> {d:.1}KB (saved {d:.1}KB)\n", .{
        @as(f64, @floatFromInt(original_size)) / 1024.0,
        new_kb,
        saved_kb,
    });
}

fn runWasmOptStaticWithPath(allocator: std.mem.Allocator, wasm_path: []const u8) !void {
    const which_result = try runCommand(allocator, &.{ "which", "wasm-opt" });
    defer {
        if (which_result.stdout) |s| allocator.free(s);
        if (which_result.stderr) |s| allocator.free(s);
    }

    if (which_result.term.Exited != 0) return;

    // Generate temp output path
    var opt_path_buf: [300]u8 = undefined;
    const opt_path = std.fmt.bufPrint(&opt_path_buf, "{s}-opt.wasm", .{wasm_path[0 .. wasm_path.len - 5]}) catch return;

    std.debug.print("[build] Optimizing with wasm-opt...\n", .{});
    const opt_result = try runCommand(allocator, &.{
        "wasm-opt", "-Oz", "--enable-simd", wasm_path, "-o", opt_path,
    });
    defer {
        if (opt_result.stdout) |s| allocator.free(s);
        if (opt_result.stderr) |s| allocator.free(s);
    }

    if (opt_result.term.Exited == 0) {
        std.fs.cwd().deleteFile(wasm_path) catch {};
        std.fs.cwd().rename(opt_path, wasm_path) catch {};
        if (std.fs.cwd().statFile(wasm_path)) |stat| {
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
    // First, check for .edgebox.json with npm field
    var config_buf: [4096]u8 = undefined;
    const config_path = std.fmt.bufPrint(&config_buf, "{s}/.edgebox.json", .{app_dir}) catch null;
    if (config_path) |cp| {
        if (std.fs.cwd().openFile(cp, .{})) |file| {
            defer file.close();
            var json_buf: [8192]u8 = undefined;
            const json_len = file.readAll(&json_buf) catch 0;
            if (json_len > 0) {
                const json_str = json_buf[0..json_len];
                // Simple JSON parse for "npm" field
                if (std.mem.indexOf(u8, json_str, "\"npm\"")) |npm_idx| {
                    // Find the value after "npm":
                    var i = npm_idx + 5; // skip "npm"
                    while (i < json_len and json_str[i] != '"') : (i += 1) {}
                    if (i < json_len) {
                        i += 1; // skip opening quote
                        const start = i;
                        while (i < json_len and json_str[i] != '"') : (i += 1) {}
                        if (i > start) {
                            const npm_pkg = json_str[start..i];
                            // Resolve npm package entry point
                            if (resolveNpmEntry(app_dir, npm_pkg, buf)) |entry| {
                                return entry;
                            } else |_| {}
                        }
                    }
                }
            }
        } else |_| {}
    }

    // Fall back to standard entry points
    const entries = [_][]const u8{ "index.js", "main.js", "app.js" };
    for (entries) |entry| {
        const path = std.fmt.bufPrint(buf, "{s}/{s}", .{ app_dir, entry }) catch continue;
        if (std.fs.cwd().access(path, .{})) |_| {
            return path;
        } else |_| {}
    }
    return error.NotFound;
}

fn resolveNpmEntry(app_dir: []const u8, npm_pkg: []const u8, buf: *[4096]u8) ![]const u8 {
    // Look for node_modules/{pkg}/package.json
    var pkg_json_buf: [4096]u8 = undefined;
    const pkg_json_path = std.fmt.bufPrint(&pkg_json_buf, "{s}/node_modules/{s}/package.json", .{ app_dir, npm_pkg }) catch return error.NotFound;

    const file = std.fs.cwd().openFile(pkg_json_path, .{}) catch return error.NotFound;
    defer file.close();

    var json_buf: [16384]u8 = undefined;
    const json_len = file.readAll(&json_buf) catch return error.NotFound;
    if (json_len == 0) return error.NotFound;

    const json_str = json_buf[0..json_len];

    // Try to find "bin" field first (for CLI packages)
    if (std.mem.indexOf(u8, json_str, "\"bin\"")) |bin_idx| {
        // Look for the first value in bin object or string
        var i = bin_idx + 5; // skip "bin"
        // Skip whitespace and colon
        while (i < json_len and (json_str[i] == ' ' or json_str[i] == ':' or json_str[i] == '\n' or json_str[i] == '\t')) : (i += 1) {}
        if (i < json_len) {
            if (json_str[i] == '"') {
                // bin is a string directly
                i += 1;
                const start = i;
                while (i < json_len and json_str[i] != '"') : (i += 1) {}
                if (i > start) {
                    const entry_file = json_str[start..i];
                    return std.fmt.bufPrint(buf, "{s}/node_modules/{s}/{s}", .{ app_dir, npm_pkg, entry_file }) catch error.NotFound;
                }
            } else if (json_str[i] == '{') {
                // bin is an object, find first value
                i += 1;
                while (i < json_len and json_str[i] != '"') : (i += 1) {}
                if (i < json_len) {
                    i += 1;
                    // Skip key
                    while (i < json_len and json_str[i] != '"') : (i += 1) {}
                    i += 1; // skip closing quote of key
                    // Skip : and whitespace
                    while (i < json_len and (json_str[i] == ' ' or json_str[i] == ':' or json_str[i] == '\n' or json_str[i] == '\t')) : (i += 1) {}
                    if (i < json_len and json_str[i] == '"') {
                        i += 1;
                        const start = i;
                        while (i < json_len and json_str[i] != '"') : (i += 1) {}
                        if (i > start) {
                            const entry_file = json_str[start..i];
                            return std.fmt.bufPrint(buf, "{s}/node_modules/{s}/{s}", .{ app_dir, npm_pkg, entry_file }) catch error.NotFound;
                        }
                    }
                }
            }
        }
    }

    // Try "main" field
    if (std.mem.indexOf(u8, json_str, "\"main\"")) |main_idx| {
        var i = main_idx + 6; // skip "main"
        while (i < json_len and json_str[i] != '"') : (i += 1) {}
        if (i < json_len) {
            i += 1;
            const start = i;
            while (i < json_len and json_str[i] != '"') : (i += 1) {}
            if (i > start) {
                const entry_file = json_str[start..i];
                return std.fmt.bufPrint(buf, "{s}/node_modules/{s}/{s}", .{ app_dir, npm_pkg, entry_file }) catch error.NotFound;
            }
        }
    }

    // Default to index.js
    const default_path = std.fmt.bufPrint(buf, "{s}/node_modules/{s}/index.js", .{ app_dir, npm_pkg }) catch return error.NotFound;
    if (std.fs.cwd().access(default_path, .{})) |_| {
        return default_path;
    } else |_| {}

    return error.NotFound;
}

fn prependPolyfills(allocator: std.mem.Allocator, polyfills_path: []const u8, bundle_path: []const u8) !void {
    // Read polyfills
    const polyfills = try std.fs.cwd().readFileAlloc(allocator, polyfills_path, 1024 * 1024);
    defer allocator.free(polyfills);

    // Read bundle (50MB max for large npm packages like claude-code)
    const bundle = try std.fs.cwd().readFileAlloc(allocator, bundle_path, 50 * 1024 * 1024);
    defer allocator.free(bundle);

    // Write combined, stripping shebang lines
    const file = try std.fs.cwd().createFile(bundle_path, .{});
    defer file.close();

    try file.writeAll(polyfills);
    try file.writeAll(";\n");

    // Write bundle, skipping shebang lines (e.g., #!/usr/bin/env node)
    // These cause qjsc to fail with "invalid first character of private name"
    try writeBundleWithoutShebangs(file, bundle);
}

fn writeBundleWithoutShebangs(file: std.fs.File, content: []const u8) !void {
    var remaining = content;

    // Skip leading shebang if present
    if (remaining.len >= 2 and remaining[0] == '#' and remaining[1] == '!') {
        if (std.mem.indexOf(u8, remaining, "\n")) |newline| {
            remaining = remaining[newline + 1 ..];
        }
    }

    // Write content in chunks, skipping any embedded shebang lines
    while (remaining.len > 0) {
        // Look for embedded shebang (can occur after minification)
        if (std.mem.indexOf(u8, remaining, "\n#!")) |idx| {
            // Write content before the shebang
            try file.writeAll(remaining[0 .. idx + 1]); // Include the newline

            // Skip the shebang line
            const after_shebang = remaining[idx + 1 ..];
            if (std.mem.indexOf(u8, after_shebang, "\n")) |newline| {
                remaining = after_shebang[newline + 1 ..];
            } else {
                // Shebang at end of file, we're done
                break;
            }
        } else {
            // No more shebangs, write rest of content
            try file.writeAll(remaining);
            break;
        }
    }
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

    // NOTE: We don't load WasmEdge plugins since we provide our own process host module
    // This avoids conflicts with the wasmedge_process plugin

    // Create configure
    const conf = c.WasmEdge_ConfigureCreate();
    defer c.WasmEdge_ConfigureDelete(conf);

    // Enable WASI
    c.WasmEdge_ConfigureAddHostRegistration(conf, c.WasmEdge_HostRegistration_Wasi);

    // Create store for module registration
    const store = c.WasmEdge_StoreCreate();
    defer c.WasmEdge_StoreDelete(store);

    // Create executor for registering import modules
    const executor = c.WasmEdge_ExecutorCreate(conf, null);
    defer c.WasmEdge_ExecutorDelete(executor);

    // Register wasmedge_process host module BEFORE creating VM
    const process_module = createProcessHostModule(allocator);
    if (process_module) |mod| {
        _ = c.WasmEdge_ExecutorRegisterImport(executor, store, mod);
    }

    // Create VM with store that already has our import module
    const vm = c.WasmEdge_VMCreate(conf, store);
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

        // Add __EDGEBOX_DIRS for OS-level sandbox wrapper
        if (cfg.serializeDirs(allocator)) |dirs_json| {
            if (env_storage_idx < env_storage.len) {
                const env_str = std.fmt.bufPrintZ(&env_storage[env_storage_idx], "__EDGEBOX_DIRS={s}", .{dirs_json}) catch null;
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

    // Pre-instantiate VM once (this is the expensive part we want to share)
    result = c.WasmEdge_VMInstantiate(vm);
    if (!c.WasmEdge_ResultOK(result)) {
        std.debug.print("[daemon] Failed to pre-instantiate: {s}\n", .{c.WasmEdge_ResultGetMessage(result)});
        std.process.exit(1);
    }
    std.debug.print("[daemon] VM pre-instantiated (fork mode enabled)\n", .{});

    // Accept loop with fork
    while (true) {
        const client = std.posix.accept(server, null, null, 0) catch |err| {
            std.debug.print("[daemon] Accept error: {}\n", .{err});
            continue;
        };

        // Read request (script path + args as newline-separated)
        var buf: [8192]u8 = undefined;
        const n = std.posix.read(client, &buf) catch {
            std.posix.close(client);
            continue;
        };
        if (n == 0) {
            std.posix.close(client);
            continue;
        }

        const request = buf[0..n];

        // Check for shutdown
        if (std.mem.startsWith(u8, request, "SHUTDOWN")) {
            std.debug.print("[daemon] Shutdown requested\n", .{});
            std.posix.close(client);
            break;
        }

        // Fork for each request - child inherits pre-instantiated VM memory
        const pid = std.posix.fork() catch |err| {
            std.debug.print("[daemon] Fork failed: {}\n", .{err});
            _ = std.posix.write(client, "Fork failed\n") catch {};
            std.posix.close(client);
            continue;
        };

        if (pid == 0) {
            // Child process - execute and exit
            std.posix.close(server); // Child doesn't need server socket

            // Parse request: first line is script path
            var lines = std.mem.splitScalar(u8, request, '\n');
            const script_path = lines.next() orelse {
                _ = std.posix.write(client, "Invalid request\n") catch {};
                std.posix.close(client);
                std.process.exit(1);
            };

            const start = std.time.nanoTimestamp();

            // Get WASI module and configure for this request
            const wasi_module = c.WasmEdge_VMGetImportModuleContext(vm, c.WasmEdge_HostRegistration_Wasi);

            // Setup WASI args
            var wasi_args_daemon: [64][*c]const u8 = undefined;
            var wasi_argc_daemon: usize = 0;

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

            // Execute _start (VM already instantiated, just run)
            const func_name_daemon = c.WasmEdge_StringCreateByCString("_start");
            const exec_result = c.WasmEdge_VMExecute(vm, func_name_daemon, null, 0, null, 0);
            c.WasmEdge_StringDelete(func_name_daemon);

            const elapsed = std.time.nanoTimestamp() - start;
            const elapsed_ms = @as(f64, @floatFromInt(elapsed)) / 1_000_000.0;

            // Send response
            var response_buf: [256]u8 = undefined;
            if (c.WasmEdge_ResultOK(exec_result)) {
                const response = std.fmt.bufPrint(&response_buf, "OK {d:.2}ms\n", .{elapsed_ms}) catch "OK\n";
                _ = std.posix.write(client, response) catch {};
            } else {
                _ = std.posix.write(client, "Execution failed\n") catch {};
            }

            std.posix.close(client);
            std.process.exit(0);
        } else {
            // Parent process - close client socket, reap child
            std.posix.close(client);
            // Don't wait - let child run async, reap with SIGCHLD or waitpid later
            _ = std.posix.waitpid(pid, 0); // Wait for child to finish
        }
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

// ============================================================================
// Process Host Module Implementation
// Provides wasmedge_process functions for child_process support
// ============================================================================

/// Global process state for current command being built
var process_state: struct {
    program: ?[]const u8 = null,
    args: std.ArrayListUnmanaged([]const u8) = .{},
    env: std.ArrayListUnmanaged(struct { key: []const u8, value: []const u8 }) = .{},
    stdin_data: ?[]const u8 = null,
    timeout_ms: u32 = 30000,
    // Results from last run
    exit_code: i32 = 0,
    stdout: []const u8 = "",
    stderr: []const u8 = "",
    allocator: std.mem.Allocator = std.heap.page_allocator,
} = .{};

/// Create the wasmedge_process host module
fn createProcessHostModule(allocator: std.mem.Allocator) ?*c.WasmEdge_ModuleInstanceContext {
    process_state.allocator = allocator;
    process_state.args = .{};

    const mod_name = c.WasmEdge_StringCreateByCString("wasmedge_process");
    defer c.WasmEdge_StringDelete(mod_name);

    const mod = c.WasmEdge_ModuleInstanceCreate(mod_name);
    if (mod == null) return null;

    // Helper to add host functions
    const addFunc = struct {
        fn add(
            module: ?*c.WasmEdge_ModuleInstanceContext,
            name: [*:0]const u8,
            func_type: ?*c.WasmEdge_FunctionTypeContext,
            host_func: c.WasmEdge_HostFunc_t,
        ) void {
            if (module == null or func_type == null) return;
            const func_name = c.WasmEdge_StringCreateByCString(name);
            defer c.WasmEdge_StringDelete(func_name);
            const func_inst = c.WasmEdge_FunctionInstanceCreate(func_type, host_func, null, 0);
            if (func_inst != null) {
                c.WasmEdge_ModuleInstanceAddFunction(module, func_name, func_inst);
            }
        }
    }.add;

    // Create function types
    const i32_type = c.WasmEdge_ValTypeGenI32();

    // set_prog_name(ptr: i32, len: i32) -> void
    var params_2i32 = [_]c.WasmEdge_ValType{ i32_type, i32_type };
    const ft_2i32_void = c.WasmEdge_FunctionTypeCreate(&params_2i32, 2, null, 0);
    defer c.WasmEdge_FunctionTypeDelete(ft_2i32_void);

    // add_env(key_ptr, key_len, val_ptr, val_len) -> void
    var params_4i32 = [_]c.WasmEdge_ValType{ i32_type, i32_type, i32_type, i32_type };
    const ft_4i32_void = c.WasmEdge_FunctionTypeCreate(&params_4i32, 4, null, 0);
    defer c.WasmEdge_FunctionTypeDelete(ft_4i32_void);

    // set_timeout(ms: i32) -> void
    var params_1i32 = [_]c.WasmEdge_ValType{i32_type};
    const ft_1i32_void = c.WasmEdge_FunctionTypeCreate(&params_1i32, 1, null, 0);
    defer c.WasmEdge_FunctionTypeDelete(ft_1i32_void);

    // run() -> i32
    var ret_i32 = [_]c.WasmEdge_ValType{i32_type};
    const ft_void_i32 = c.WasmEdge_FunctionTypeCreate(null, 0, &ret_i32, 1);
    defer c.WasmEdge_FunctionTypeDelete(ft_void_i32);

    // get_stdout(buf: i32) -> void
    const ft_1i32_void_get = c.WasmEdge_FunctionTypeCreate(&params_1i32, 1, null, 0);
    defer c.WasmEdge_FunctionTypeDelete(ft_1i32_void_get);

    // Add host functions
    addFunc(mod, "wasmedge_process_set_prog_name", ft_2i32_void, hostSetProgName);
    addFunc(mod, "wasmedge_process_add_arg", ft_2i32_void, hostAddArg);
    addFunc(mod, "wasmedge_process_add_env", ft_4i32_void, hostAddEnv);
    addFunc(mod, "wasmedge_process_add_stdin", ft_2i32_void, hostAddStdin);
    addFunc(mod, "wasmedge_process_set_timeout", ft_1i32_void, hostSetTimeout);
    addFunc(mod, "wasmedge_process_run", ft_void_i32, hostRun);
    addFunc(mod, "wasmedge_process_get_exit_code", ft_void_i32, hostGetExitCode);
    addFunc(mod, "wasmedge_process_get_stdout_len", ft_void_i32, hostGetStdoutLen);
    addFunc(mod, "wasmedge_process_get_stdout", ft_1i32_void_get, hostGetStdout);
    addFunc(mod, "wasmedge_process_get_stderr_len", ft_void_i32, hostGetStderrLen);
    addFunc(mod, "wasmedge_process_get_stderr", ft_1i32_void_get, hostGetStderr);

    return mod;
}

/// Read string from WASM memory
fn readWasmString(mem_ctx: ?*c.WasmEdge_MemoryInstanceContext, ptr: u32, len: u32) ?[]const u8 {
    if (mem_ctx == null or len == 0) return null;
    const data_ptr = c.WasmEdge_MemoryInstanceGetPointer(mem_ctx, ptr, len);
    if (data_ptr == null) return null;
    return data_ptr[0..len];
}

/// Write data to WASM memory
fn writeWasmMemory(mem_ctx: ?*c.WasmEdge_MemoryInstanceContext, ptr: u32, data: []const u8) bool {
    if (mem_ctx == null or data.len == 0) return false;
    const data_ptr = c.WasmEdge_MemoryInstanceGetPointer(mem_ctx, ptr, @intCast(data.len));
    if (data_ptr == null) return false;
    @memcpy(data_ptr[0..data.len], data);
    return true;
}

/// Get memory instance from calling context
fn getMemory(call_ctx: ?*const c.WasmEdge_CallingFrameContext) ?*c.WasmEdge_MemoryInstanceContext {
    if (call_ctx == null) return null;
    return c.WasmEdge_CallingFrameGetMemoryInstance(call_ctx, 0);
}

// Host function implementations
fn hostSetProgName(data: ?*anyopaque, call_ctx: ?*const c.WasmEdge_CallingFrameContext, params: [*c]const c.WasmEdge_Value, returns: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    _ = data;
    _ = returns;
    const ptr = c.WasmEdge_ValueGetI32(params[0]);
    const len = c.WasmEdge_ValueGetI32(params[1]);
    if (readWasmString(getMemory(call_ctx), @intCast(ptr), @intCast(len))) |str| {
        process_state.program = process_state.allocator.dupe(u8, str) catch null;
    }
    return c.WasmEdge_Result_Success;
}

fn hostAddArg(data: ?*anyopaque, call_ctx: ?*const c.WasmEdge_CallingFrameContext, params: [*c]const c.WasmEdge_Value, returns: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    _ = data;
    _ = returns;
    const ptr = c.WasmEdge_ValueGetI32(params[0]);
    const len = c.WasmEdge_ValueGetI32(params[1]);
    if (readWasmString(getMemory(call_ctx), @intCast(ptr), @intCast(len))) |str| {
        const arg = process_state.allocator.dupe(u8, str) catch return c.WasmEdge_Result_Success;
        process_state.args.append(process_state.allocator, arg) catch {};
    }
    return c.WasmEdge_Result_Success;
}

fn hostAddEnv(data: ?*anyopaque, call_ctx: ?*const c.WasmEdge_CallingFrameContext, params: [*c]const c.WasmEdge_Value, returns: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    _ = data;
    _ = returns;
    const key_ptr = c.WasmEdge_ValueGetI32(params[0]);
    const key_len = c.WasmEdge_ValueGetI32(params[1]);
    const val_ptr = c.WasmEdge_ValueGetI32(params[2]);
    const val_len = c.WasmEdge_ValueGetI32(params[3]);
    const mem = getMemory(call_ctx);
    if (readWasmString(mem, @intCast(key_ptr), @intCast(key_len))) |key| {
        if (readWasmString(mem, @intCast(val_ptr), @intCast(val_len))) |val| {
            const k = process_state.allocator.dupe(u8, key) catch return c.WasmEdge_Result_Success;
            const v = process_state.allocator.dupe(u8, val) catch return c.WasmEdge_Result_Success;
            process_state.env.append(process_state.allocator, .{ .key = k, .value = v }) catch {};
        }
    }
    return c.WasmEdge_Result_Success;
}

fn hostAddStdin(data: ?*anyopaque, call_ctx: ?*const c.WasmEdge_CallingFrameContext, params: [*c]const c.WasmEdge_Value, returns: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    _ = data;
    _ = returns;
    const ptr = c.WasmEdge_ValueGetI32(params[0]);
    const len = c.WasmEdge_ValueGetI32(params[1]);
    if (readWasmString(getMemory(call_ctx), @intCast(ptr), @intCast(len))) |str| {
        process_state.stdin_data = process_state.allocator.dupe(u8, str) catch null;
    }
    return c.WasmEdge_Result_Success;
}

fn hostSetTimeout(data: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, params: [*c]const c.WasmEdge_Value, returns: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    _ = data;
    _ = returns;
    process_state.timeout_ms = @intCast(c.WasmEdge_ValueGetI32(params[0]));
    return c.WasmEdge_Result_Success;
}

fn hostRun(data: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, _: [*c]const c.WasmEdge_Value, returns: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    _ = data;

    const program = process_state.program orelse {
        returns[0] = c.WasmEdge_ValueGenI32(1);
        return c.WasmEdge_Result_Success;
    };

    // Build argv
    var argv: std.ArrayListUnmanaged([]const u8) = .{};
    defer argv.deinit(process_state.allocator);

    // Add program as first arg
    const prog_dup = process_state.allocator.dupe(u8, program) catch {
        returns[0] = c.WasmEdge_ValueGenI32(1);
        return c.WasmEdge_Result_Success;
    };
    argv.append(process_state.allocator, prog_dup) catch {};

    // Add remaining args
    for (process_state.args.items) |arg| {
        const arg_dup = process_state.allocator.dupe(u8, arg) catch continue;
        argv.append(process_state.allocator, arg_dup) catch {};
    }

    // Run the process
    var child = std.process.Child.init(argv.items, process_state.allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    child.spawn() catch {
        returns[0] = c.WasmEdge_ValueGenI32(1);
        return c.WasmEdge_Result_Success;
    };

    // Read stdout and stderr
    const stdout = if (child.stdout) |f| f.readToEndAlloc(process_state.allocator, 10 * 1024 * 1024) catch null else null;
    const stderr = if (child.stderr) |f| f.readToEndAlloc(process_state.allocator, 10 * 1024 * 1024) catch null else null;

    const term = child.wait() catch {
        returns[0] = c.WasmEdge_ValueGenI32(1);
        return c.WasmEdge_Result_Success;
    };

    // Store results
    process_state.stdout = stdout orelse "";
    process_state.stderr = stderr orelse "";
    process_state.exit_code = switch (term) {
        .Exited => |code| @intCast(code),
        .Signal => |sig| @intCast(128 + @as(i32, @intCast(sig))),
        else => 1,
    };

    // Reset for next command
    process_state.program = null;
    process_state.args.clearRetainingCapacity();

    returns[0] = c.WasmEdge_ValueGenI32(0);
    return c.WasmEdge_Result_Success;
}

fn hostGetExitCode(data: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, _: [*c]const c.WasmEdge_Value, returns: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    _ = data;
    returns[0] = c.WasmEdge_ValueGenI32(process_state.exit_code);
    return c.WasmEdge_Result_Success;
}

fn hostGetStdoutLen(data: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, _: [*c]const c.WasmEdge_Value, returns: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    _ = data;
    returns[0] = c.WasmEdge_ValueGenI32(@intCast(process_state.stdout.len));
    return c.WasmEdge_Result_Success;
}

fn hostGetStdout(data: ?*anyopaque, call_ctx: ?*const c.WasmEdge_CallingFrameContext, params: [*c]const c.WasmEdge_Value, returns: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    _ = data;
    _ = returns;
    const ptr = c.WasmEdge_ValueGetI32(params[0]);
    _ = writeWasmMemory(getMemory(call_ctx), @intCast(ptr), process_state.stdout);
    return c.WasmEdge_Result_Success;
}

fn hostGetStderrLen(data: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, _: [*c]const c.WasmEdge_Value, returns: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    _ = data;
    returns[0] = c.WasmEdge_ValueGenI32(@intCast(process_state.stderr.len));
    return c.WasmEdge_Result_Success;
}

fn hostGetStderr(data: ?*anyopaque, call_ctx: ?*const c.WasmEdge_CallingFrameContext, params: [*c]const c.WasmEdge_Value, returns: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    _ = data;
    _ = returns;
    const ptr = c.WasmEdge_ValueGetI32(params[0]);
    _ = writeWasmMemory(getMemory(call_ctx), @intCast(ptr), process_state.stderr);
    return c.WasmEdge_Result_Success;
}
