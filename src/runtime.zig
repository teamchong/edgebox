/// EdgeBox Build Tool
/// Compiles JS to AOT WASM using WAMR with SIMD support
///
/// Usage:
///   edgeboxc build [app-directory]  - Build app (bundle + bytecode + AOT)
const std = @import("std");
const builtin = @import("builtin");
const wizer = @import("wizer_wamr.zig");
const qjsc_wrapper = @import("qjsc_wrapper.zig");
const freeze = @import("freeze/frozen_registry.zig");
const wasm_opt = @import("wasm_opt.zig");

const VERSION = "0.1.0";
const SOCKET_PATH = "/tmp/edgebox.sock";

// ============================================================================
// EdgeBox Configuration
// ============================================================================

/// Output mode for command emulation
pub const OutputMode = enum {
    server, // Standard stdout/stderr (default)
    browser, // HTML output
    viewkit, // ViewKit UI engine
};

/// Command permission: binary name -> allowed subcommands (null = all allowed)
pub const CommandPermission = struct {
    binary: []const u8,
    subcommands: ?[]const []const u8 = null, // null = allow all args, empty = binary only
    deny_subcommands: ?[]const []const u8 = null, // Deny list (takes precedence over allow)

    // Credential proxy: env vars to inject when spawning this command
    credentials: ?std.StringHashMapUnmanaged([]const u8) = null,

    // Output mode for Component Model emulators (optional)
    output_mode: OutputMode = .server,
    emulator_component: ?[]const u8 = null, // WASM Component path
};

/// EdgeBox app configuration parsed from .edgebox.json
pub const EdgeBoxConfig = struct {
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

    // Command security settings
    sensitive_files: []const []const u8 = &.{}, // Glob patterns for sensitive files (.env, ~/.ssh/*, etc.)
    blocked_patterns: []const []const u8 = &.{}, // Patterns for destructive commands (git reset, > .env, etc.)

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
                        const dup = allocator.dupe(u8, item.string) catch |err| {
                            std.debug.print("[warn] Failed to parse dirs entry: {}\n", .{err});
                            continue;
                        };
                        dirs.append(allocator, dup) catch |err| {
                            std.debug.print("[warn] Failed to append dirs entry: {}\n", .{err});
                            continue;
                        };
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
                        const dup = allocator.dupe(u8, item.string) catch |err| {
                            std.debug.print("[warn] Failed to parse env entry: {}\n", .{err});
                            continue;
                        };
                        envs.append(allocator, dup) catch |err| {
                            std.debug.print("[warn] Failed to append env entry: {}\n", .{err});
                            continue;
                        };
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
                        const dup = allocator.dupe(u8, item.string) catch |err| {
                            std.debug.print("[warn] Failed to parse allowedUrls entry: {}\n", .{err});
                            continue;
                        };
                        urls.append(allocator, dup) catch |err| {
                            std.debug.print("[warn] Failed to append allowedUrls entry: {}\n", .{err});
                            continue;
                        };
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
                        const dup = allocator.dupe(u8, item.string) catch |err| {
                            std.debug.print("[warn] Failed to parse blockedUrls entry: {}\n", .{err});
                            continue;
                        };
                        urls.append(allocator, dup) catch |err| {
                            std.debug.print("[warn] Failed to append blockedUrls entry: {}\n", .{err});
                            continue;
                        };
                    }
                }
                config.blocked_urls = urls.toOwnedSlice(allocator) catch &.{};
            }
        }

        // Parse rateLimitRps (HTTP security) - with bounds: 0 = unlimited, max 100k
        if (parsed.value.object.get("rateLimitRps")) |v| {
            if (v == .integer) {
                const val = @max(0, @min(100_000, v.integer));
                config.rate_limit_rps = @intCast(val);
                if (v.integer > 100_000) {
                    std.debug.print("[warn] rateLimitRps clamped to 100000 (was {})\n", .{v.integer});
                }
            }
        }

        // Parse maxConnections (HTTP security) - with bounds: 1-10000
        if (parsed.value.object.get("maxConnections")) |v| {
            if (v == .integer) {
                const val = @max(1, @min(10_000, v.integer));
                config.max_connections = @intCast(val);
                if (v.integer > 10_000) {
                    std.debug.print("[warn] maxConnections clamped to 10000 (was {})\n", .{v.integer});
                }
            }
        }

        // Parse commands object
        if (parsed.value.object.get("commands")) |v| {
            if (v == .object) {
                var cmds: std.ArrayListUnmanaged(CommandPermission) = .{};
                var iter = v.object.iterator();
                while (iter.next()) |entry| {
                    const binary = allocator.dupe(u8, entry.key_ptr.*) catch continue;
                    var perm = CommandPermission{ .binary = binary };

                    // Value can be: true (allow all), array (simple), or object (extended format)
                    if (entry.value_ptr.* == .bool and entry.value_ptr.bool) {
                        // true = allow all arguments
                        perm.subcommands = null;
                    } else if (entry.value_ptr.* == .array) {
                        // Array = list of allowed subcommands
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
                    } else if (entry.value_ptr.* == .object) {
                        // Object = extended format with allow, deny, credentials, etc.
                        const cmd_obj = entry.value_ptr.object;

                        // Parse "allow" field
                        if (cmd_obj.get("allow")) |allow_val| {
                            if (allow_val == .bool and allow_val.bool) {
                                perm.subcommands = null;
                            } else if (allow_val == .array) {
                                var subs: std.ArrayListUnmanaged([]const u8) = .{};
                                for (allow_val.array.items) |item| {
                                    if (item == .string) {
                                        if (std.mem.eql(u8, item.string, "*")) {
                                            subs.deinit(allocator);
                                            perm.subcommands = null;
                                            break;
                                        }
                                        subs.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
                                    }
                                }
                                if (perm.subcommands) |_| {} else {
                                    perm.subcommands = subs.toOwnedSlice(allocator) catch null;
                                }
                            }
                        }

                        // Parse "deny" field
                        if (cmd_obj.get("deny")) |deny_val| {
                            if (deny_val == .array) {
                                var deny_subs: std.ArrayListUnmanaged([]const u8) = .{};
                                for (deny_val.array.items) |item| {
                                    if (item == .string) {
                                        deny_subs.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
                                    }
                                }
                                perm.deny_subcommands = deny_subs.toOwnedSlice(allocator) catch null;
                            }
                        }

                        // Parse "credentials" field
                        if (cmd_obj.get("credentials")) |cred_val| {
                            if (cred_val == .object) {
                                var creds = std.StringHashMapUnmanaged([]const u8){};
                                var cred_iter = cred_val.object.iterator();
                                while (cred_iter.next()) |cred_entry| {
                                    const key = allocator.dupe(u8, cred_entry.key_ptr.*) catch continue;
                                    const value = if (cred_entry.value_ptr.* == .string)
                                        allocator.dupe(u8, cred_entry.value_ptr.string) catch continue
                                    else
                                        continue;
                                    // Expand ${VAR} references from host environment
                                    const expanded = expandEnvVar(allocator, value) catch value;
                                    if (expanded.ptr != value.ptr) {
                                        allocator.free(value);
                                    }
                                    creds.put(allocator, key, expanded) catch continue;
                                }
                                perm.credentials = creds;
                            }
                        }

                        // Parse "outputMode" field
                        if (cmd_obj.get("outputMode")) |mode_val| {
                            if (mode_val == .string) {
                                if (std.mem.eql(u8, mode_val.string, "browser")) {
                                    perm.output_mode = .browser;
                                } else if (std.mem.eql(u8, mode_val.string, "viewkit")) {
                                    perm.output_mode = .viewkit;
                                }
                            }
                        }

                        // Parse "emulator" field
                        if (cmd_obj.get("emulator")) |emul_val| {
                            if (emul_val == .string) {
                                perm.emulator_component = allocator.dupe(u8, emul_val.string) catch null;
                            }
                        }
                    }

                    cmds.append(allocator, perm) catch continue;
                }
                config.commands = cmds.toOwnedSlice(allocator) catch &.{};
            }
        }

        // Parse sensitiveFiles array (command security)
        if (parsed.value.object.get("sensitiveFiles")) |v| {
            if (v == .array) {
                var files: std.ArrayListUnmanaged([]const u8) = .{};
                for (v.array.items) |item| {
                    if (item == .string) {
                        files.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
                    }
                }
                config.sensitive_files = files.toOwnedSlice(allocator) catch &.{};
            }
        }

        // Parse blockedPatterns array (command security)
        if (parsed.value.object.get("blockedPatterns")) |v| {
            if (v == .array) {
                var patterns: std.ArrayListUnmanaged([]const u8) = .{};
                for (v.array.items) |item| {
                    if (item == .string) {
                        patterns.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
                    }
                }
                config.blocked_patterns = patterns.toOwnedSlice(allocator) catch &.{};
            }
        }

        return config;
    }

    /// Get list of allowed binary names
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

/// Expand ${VAR} references in a string with host environment variables
/// Example: "${GH_TOKEN}" -> actual value from host env
fn expandEnvVar(allocator: std.mem.Allocator, value: []const u8) ![]const u8 {
    // Check if value contains ${...} pattern
    if (std.mem.indexOf(u8, value, "${")) |start_idx| {
        if (std.mem.indexOfPos(u8, value, start_idx, "}")) |end_idx| {
            const var_name = value[start_idx + 2 .. end_idx];

            // Get environment variable value from host
            const env_value = std.process.getEnvVarOwned(allocator, var_name) catch {
                // If env var not found, return original value
                return try allocator.dupe(u8, value);
            };
            defer allocator.free(env_value);

            // Build result: prefix + env_value + suffix
            var result: std.ArrayListUnmanaged(u8) = .{};
            defer result.deinit(allocator);

            try result.appendSlice(allocator, value[0..start_idx]);
            try result.appendSlice(allocator, env_value);
            try result.appendSlice(allocator, value[end_idx + 1 ..]);

            return try result.toOwnedSlice(allocator);
        }
    }

    // No ${VAR} pattern found, return as-is
    return try allocator.dupe(u8, value);
}

// ============================================================================
// Tests
// ============================================================================

test "expandEnvVar - no variable" {
    const allocator = std.testing.allocator;
    const result = try expandEnvVar(allocator, "plain text");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("plain text", result);
}

test "expandEnvVar - with HOME variable" {
    const allocator = std.testing.allocator;
    // This test uses HOME which should be set on most systems
    const result = try expandEnvVar(allocator, "${HOME}/test");
    defer allocator.free(result);
    // Just verify it doesn't contain ${HOME} anymore
    try std.testing.expect(std.mem.indexOf(u8, result, "${HOME}") == null);
    try std.testing.expect(std.mem.endsWith(u8, result, "/test"));
}

test "expandEnvVar - nonexistent variable returns original" {
    const allocator = std.testing.allocator;
    const result = try expandEnvVar(allocator, "${NONEXISTENT_VAR_12345}/test");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("${NONEXISTENT_VAR_12345}/test", result);
}

test "expandEnvVar - unclosed brace returns original" {
    const allocator = std.testing.allocator;
    const result = try expandEnvVar(allocator, "${UNCLOSED");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("${UNCLOSED", result);
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        printUsage();
        std.process.exit(1);
    }

    // Parse arguments
    var dynamic_mode = false;
    var force_rebuild = false;
    var no_polyfill = false;
    var no_freeze = false;
    var no_bundle = false;
    var binary_only = false;
    var app_dir: ?[]const u8 = null;

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printUsage();
            return;
        } else if (std.mem.eql(u8, arg, "--version") or std.mem.eql(u8, arg, "-v")) {
            std.debug.print("edgeboxc {s}\n", .{VERSION});
            return;
        } else if (std.mem.eql(u8, arg, "--dynamic")) {
            dynamic_mode = true;
        } else if (std.mem.eql(u8, arg, "--force") or std.mem.eql(u8, arg, "-f")) {
            force_rebuild = true;
        } else if (std.mem.eql(u8, arg, "--no-polyfill")) {
            no_polyfill = true;
        } else if (std.mem.eql(u8, arg, "--no-freeze")) {
            no_freeze = true;
        } else if (std.mem.eql(u8, arg, "--no-bundle")) {
            no_bundle = true;
        } else if (std.mem.eql(u8, arg, "--binary-only")) {
            binary_only = true;
        } else if (std.mem.eql(u8, arg, "--minimal")) {
            // Shortcut for test262: skip polyfills, freeze, bundler, and WASM/AOT
            no_polyfill = true;
            no_freeze = true;
            no_bundle = true;
            binary_only = true;
        } else if (std.mem.eql(u8, arg, "build")) {
            continue; // implicit, ignore for backwards compatibility
        } else if (std.mem.endsWith(u8, arg, ".wasm") or std.mem.endsWith(u8, arg, ".aot") or std.mem.endsWith(u8, arg, ".dylib") or std.mem.endsWith(u8, arg, ".so")) {
            std.debug.print("Use 'edgebox {s}' to run WASM/AOT files\n", .{arg});
            std.process.exit(1);
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            app_dir = arg;
        } else {
            std.debug.print("Unknown option: {s}\n", .{arg});
            printUsage();
            std.process.exit(1);
        }
    }

    const dir = app_dir orelse {
        std.debug.print("Error: No app directory specified\n\n", .{});
        printUsage();
        std.process.exit(1);
    };

    if (force_rebuild) {
        cleanBuildOutputs();
    }

    if (dynamic_mode) {
        try runBuild(allocator, dir);
    } else {
        // ONE code path: JS → Zig QuickJS + Freeze + Polyfill → {Binary (with host), WASM (no host)} → WAMR → AOT
        try runStaticBuild(allocator, dir, .{
            .no_polyfill = no_polyfill,
            .no_freeze = no_freeze,
            .no_bundle = no_bundle,
            .binary_only = binary_only,
        });
    }
}

fn printUsage() void {
    std.debug.print(
        \\EdgeBox Compiler
        \\
        \\Usage:
        \\  edgeboxc <app.js>   Compile JS to Binary + WASM + AOT (all outputs in one build)
        \\
        \\Output (all in zig-out/bin/<app.js>/):
        \\  <app>        Native binary (QuickJS + frozen, includes host)
        \\  <app>.wasm   WASM module (for use with edgebox daemon)
        \\  <app>.aot    AOT compiled (WASM → WAMR, sandboxed)
        \\
        \\Options:
        \\  -f, --force      Clean previous build outputs first
        \\  --no-polyfill    Skip Node.js polyfills (faster builds for simple JS)
        \\  --no-freeze      Skip freeze analysis (faster builds, no optimization)
        \\  --no-bundle      Skip Bun bundler (for simple JS without imports)
        \\  --binary-only    Only build native binary (skip WASM/AOT)
        \\  --minimal        All of the above (fastest, for test262)
        \\  -h, --help       Show this help
        \\  -v, --version    Show version
        \\
        \\Examples:
        \\  edgeboxc app.js              Build Binary + WASM + AOT
        \\  edgeboxc --minimal test.js   Fast build for simple JS
        \\  ./zig-out/bin/app.js/app     Run binary directly
        \\  edgebox zig-out/bin/app.js/app.aot   Run AOT in sandbox
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
            // DISABLED: WAMR SIMDE not working for v128.load/v128.store opcodes in QuickJS
            // TODO: Fix WAMR SIMDE support or use classic interpreter mode for wizer
            // try runWizer(allocator);

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

/// Build options for customizing the compilation pipeline
const BuildOptions = struct {
    no_polyfill: bool = false, // Skip Node.js polyfills
    no_freeze: bool = false, // Skip freeze analysis
    no_bundle: bool = false, // Skip Bun bundler
    binary_only: bool = false, // Only build native binary (skip WASM/AOT)
};

/// Static build: compile JS to C bytecode with qjsc, embed in WASM
/// All frozen functions stay in WASM/AOT (sandboxed) - no host function exports
fn runStaticBuild(allocator: std.mem.Allocator, app_dir: []const u8, options: BuildOptions) !void {
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

    // Calculate source directory, cache directory, and output directory
    // e.g., bench/hello.js ->
    //   source_dir = "bench"
    //   cache_dir  = "zig-out/cache/bench"  (intermediate files: bundle.js, bundle_compiled.c)
    //   output_dir = "zig-out/bin/bench"    (final outputs: hello.wasm, hello.aot)
    var source_dir_buf: [4096]u8 = undefined;
    var cache_dir_buf: [4096]u8 = undefined;
    var output_dir_buf: [4096]u8 = undefined;
    const source_dir: []const u8 = blk: {
        // For per-project cache isolation, use app_dir as source_dir
        // e.g., "bench" -> source_dir = "bench" -> cache_dir = "zig-out/cache/bench"
        // e.g., "examples/app" -> source_dir = "examples/app" -> cache_dir = "zig-out/cache/examples/app"
        var dir = app_dir;
        // Strip trailing slash if present
        if (dir.len > 0 and dir[dir.len - 1] == '/') {
            dir = dir[0 .. dir.len - 1];
        }
        // Strip leading slash for absolute paths (build.zig requires relative paths)
        if (dir.len > 0 and dir[0] == '/') {
            dir = dir[1..];
        }
        break :blk dir;
    };
    const cache_dir = blk: {
        if (source_dir.len > 0) {
            const len = std.fmt.bufPrint(&cache_dir_buf, "zig-out/cache/{s}", .{source_dir}) catch {
                std.debug.print("[error] Source directory path too long: {s}\n", .{source_dir});
                std.process.exit(1);
            };
            break :blk cache_dir_buf[0..len.len];
        }
        break :blk "zig-out/cache";
    };
    const output_dir = blk: {
        if (source_dir.len > 0) {
            const len = std.fmt.bufPrint(&output_dir_buf, "zig-out/bin/{s}", .{source_dir}) catch {
                std.debug.print("[error] Source directory path too long: {s}\n", .{source_dir});
                std.process.exit(1);
            };
            break :blk output_dir_buf[0..len.len];
        }
        break :blk "zig-out/bin";
    };
    // Build -Dsource-dir argument for zig build (tells build.zig where to find bundle_compiled.c)
    const source_dir_arg = if (source_dir.len > 0)
        std.fmt.bufPrint(&source_dir_buf, "-Dsource-dir={s}", .{source_dir}) catch ""
    else
        "";

    // Create directories
    std.fs.cwd().makePath(cache_dir) catch {};
    std.fs.cwd().makePath(output_dir) catch {};

    // Pre-calculate cache file paths (intermediate files)
    var bundle_js_path_buf: [4096]u8 = undefined;
    const bundle_js_path = std.fmt.bufPrint(&bundle_js_path_buf, "{s}/bundle.js", .{cache_dir}) catch "zig-out/cache/bundle.js";

    var bundle_compiled_path_buf: [4096]u8 = undefined;
    const bundle_compiled_path = std.fmt.bufPrint(&bundle_compiled_path_buf, "{s}/bundle_compiled.c", .{cache_dir}) catch "zig-out/cache/bundle_compiled.c";

    var bundle_original_path_buf: [4096]u8 = undefined;
    const bundle_original_path = std.fmt.bufPrint(&bundle_original_path_buf, "{s}/bundle_original.c", .{cache_dir}) catch "zig-out/cache/bundle_original.c";

    // frozen_manifest.json is per-project (each project has its own frozen function manifest)
    var frozen_manifest_buf: [4096]u8 = undefined;
    const frozen_manifest_path = std.fmt.bufPrint(&frozen_manifest_buf, "{s}/frozen_manifest.json", .{cache_dir}) catch "zig-out/cache/frozen_manifest.json";

    // Bun --outfile argument
    var bun_outfile_buf: [4096]u8 = undefined;
    const bun_outfile_arg = std.fmt.bufPrint(&bun_outfile_buf, "--outfile={s}/bundle.js", .{cache_dir}) catch "--outfile=zig-out/cache/bundle.js";

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

    // Step 3: Bundle with Bun (or skip if --no-bundle flag)
    if (options.no_bundle) {
        // Skip bundler - just copy the source file directly
        std.debug.print("[build] Skipping bundler (--no-bundle)\n", .{});
        std.fs.cwd().makePath(cache_dir) catch {};
        const src_file = std.fs.cwd().openFile(entry_path, .{}) catch {
            std.debug.print("[error] Cannot open entry point: {s}\n", .{entry_path});
            std.process.exit(1);
        };
        defer src_file.close();
        const content = src_file.readToEndAlloc(allocator, 50 * 1024 * 1024) catch {
            std.debug.print("[error] Cannot read entry point\n", .{});
            std.process.exit(1);
        };
        defer allocator.free(content);
        const out_file = std.fs.cwd().createFile(bundle_js_path, .{}) catch {
            std.debug.print("[error] Cannot create bundle.js\n", .{});
            std.process.exit(1);
        };
        defer out_file.close();
        out_file.writeAll(content) catch {};
    } else {
        // Detect pre-bundled files by checking size (>1MB typically means pre-bundled)
        // But ESM bundles need conversion to CommonJS for QuickJS compatibility
        const entry_file = std.fs.cwd().openFile(entry_path, .{}) catch {
            std.debug.print("[error] Cannot open entry point: {s}\n", .{entry_path});
            std.process.exit(1);
        };
        defer entry_file.close();
        // Always use Bun for bundling - simpler and more reliable
        // Bun handles ESM/CJS detection and conversion automatically
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

    // Step 4: Prepend polyfills (skip if --no-polyfill flag)
    if (!options.no_polyfill) {
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
    } else {
        std.debug.print("[build] Skipping polyfills (--no-polyfill)\n", .{});
    }

    // Check bundle size - skip debug traces for large bundles (>2MB) as they corrupt complex JavaScript
    const skip_traces = if (std.fs.cwd().statFile(bundle_js_path)) |stat| blk: {
        const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
        std.debug.print("[build] Bundle: bundle.js ({d:.1}KB)\n", .{size_kb});
        break :blk stat.size > 2 * 1024 * 1024;
    } else |_| false;

    // Step 5: Patch known issues in bundled code
    // Some bundles set console to a no-op, we replace with a working version
    std.debug.print("[build] Patching bundle for EdgeBox compatibility...\n", .{});
    // Note: sed -i.bak works on both macOS and Linux (unlike sed -i "" which is macOS-only)
    _ = try runCommand(allocator, &.{
        "sed", "-i.bak",
        "s/console = { log: function() {} };/console = { log: function(a,b,c,d,e) { print(a||'',b||'',c||'',d||'',e||''); } };/g",
        bundle_js_path,
    });
    // Clean up backup file
    const bak_path = try std.fmt.allocPrint(allocator, "{s}.bak", .{bundle_js_path});
    defer allocator.free(bak_path);
    std.fs.cwd().deleteFile(bak_path) catch {};

    // Skip all debug trace injection for large bundles (>2MB)
    // These sed patches use hardcoded line numbers specific to old Claude CLI versions
    // and will corrupt newer bundles
    if (skip_traces) {
        std.debug.print("[build] Large bundle - skipping debug trace injection\n", .{});
    }
    if (!skip_traces) {
        std.debug.print("[build] Injecting trace hooks (optimized: 1 sed call instead of 25)...\n", .{});
        try applyTracePatterns(allocator, bundle_js_path);
    } // end skip_traces

    // Skip freeze if --no-freeze flag is set (faster builds for test262)
    const skip_freeze = options.no_freeze;
    if (skip_freeze) {
        std.debug.print("[build] Skipping freeze analysis (--no-freeze)\n", .{});
    }

    // Step 6: Generate manifest and freeze ORIGINAL bytecode
    // The manifest has names from JS source (e.g., "fib"), which we need for frozen C code
    var manifest_content: ?[]u8 = null;
    defer if (manifest_content) |m| allocator.free(m);

    // Path for hooked bundle (will be created later)
    const bundle_hooked_path = try std.fmt.allocPrint(allocator, "{s}/bundle_hooked.js", .{cache_dir});
    defer allocator.free(bundle_hooked_path);

    if (!skip_freeze) {
        // Step 6a: Compile ORIGINAL JS to bytecode FIRST (before hooks!)
        std.debug.print("[build] Compiling original JS to bytecode for freezing...\n", .{});
        const exit_code = try qjsc_wrapper.compileJsToBytecode(allocator, &.{
            "qjsc",
            "-N", "bundle",
            "-o", bundle_original_path,
            bundle_js_path, // Original bundle without hooks
        });
        if (exit_code != 0) {
            std.debug.print("[error] qjsc compilation failed\n", .{});
            std.process.exit(1);
        }

        // Step 6b: Generate manifest by scanning JS source (inject_hooks extracts function names)
        std.debug.print("[build] Scanning JS for freezable functions (generating manifest)...\n", .{});

        // Copy original bundle to hooked path for processing
        try std.fs.cwd().copyFile(bundle_js_path, std.fs.cwd(), bundle_hooked_path, .{});

        // Run inject_hooks to generate manifest AND create hooked bundle
        const inject_result = try runCommand(allocator, &.{
            "node", "tools/inject_hooks.js", bundle_hooked_path, bundle_hooked_path, frozen_manifest_path,
        });
        defer {
            if (inject_result.stdout) |s| allocator.free(s);
            if (inject_result.stderr) |s| allocator.free(s);
        }
        if (inject_result.term.Exited != 0) {
            std.debug.print("[warn] Hook injection failed - skipping freeze\n", .{});
            if (inject_result.stderr) |s| std.debug.print("{s}\n", .{s});
        } else {
            // Read manifest content for freeze
            const manifest_file = std.fs.cwd().openFile(frozen_manifest_path, .{}) catch null;
            if (manifest_file) |mf| {
                defer mf.close();
                manifest_content = mf.readToEndAlloc(allocator, 1024 * 1024) catch null;
            }
        }
    }

    // Step 6c: Freeze bytecode to optimized C (using manifest for names)
    const freeze_success = if (skip_freeze) false else blk: {
        std.debug.print("[build] Freezing bytecode to optimized C...\n", .{});
        // Read the bytecode C file
        const bytecode_file = std.fs.cwd().openFile(bundle_original_path, .{}) catch |err| {
            std.debug.print("[warn] Could not open bundle_original.c: {}\n", .{err});
            break :blk false;
        };
        defer bytecode_file.close();

        const bytecode_content = bytecode_file.readToEndAlloc(allocator, 500 * 1024 * 1024) catch |err| {
            std.debug.print("[warn] Could not read bundle_original.c: {}\n", .{err});
            break :blk false;
        };
        defer allocator.free(bytecode_content);

        // Pure Zig freeze pipeline - no C code generation
        // Parse bytecode from C array format
        const file_content = freeze.module_parser.parseCArrayBytecode(allocator, bytecode_content) catch |err| {
            std.debug.print("[warn] Could not parse bytecode: {}\n", .{err});
            break :blk false;
        };
        defer allocator.free(file_content);

        // Analyze the module
        var analysis = freeze.analyzeModule(allocator, file_content) catch |err| {
            std.debug.print("[warn] Analysis failed: {}\n", .{err});
            break :blk false;
        };
        defer analysis.deinit();

        // Generate closure manifest JSON directly (no C code)
        var closure_manifest_path_buf: [4096]u8 = undefined;
        const closure_manifest_path = std.fmt.bufPrint(&closure_manifest_path_buf, "{s}/closure_manifest.json", .{cache_dir}) catch "zig-out/cache/closure_manifest.json";
        if (freeze.generateClosureManifest(allocator, &analysis, manifest_content) catch null) |closure_json| {
            defer allocator.free(closure_json);
            const manifest_file = std.fs.cwd().createFile(closure_manifest_path, .{}) catch null;
            if (manifest_file) |mf| {
                mf.writeAll(closure_json) catch {};
                mf.close();
                std.debug.print("[build] Closure manifest: {s}\n", .{closure_manifest_path});
            }
        }

        // Generate Zig frozen module (sharded for parallel compilation)
        zig_gen: {
            // Use size-based sharding for even distribution (200KB per shard)
            // Smaller shards prevent LLVM hang/thrashing on large files
            const BYTES_PER_SHARD = 200 * 1024;
            var sharded = freeze.generateModuleZigSharded(allocator, &analysis, "frozen", manifest_content, BYTES_PER_SHARD) catch |err| {
                std.debug.print("[warn] Zig codegen failed: {}\n", .{err});
                break :zig_gen;
            };
            defer sharded.deinit(allocator);

            // Write each shard file
            var total_size: usize = 0;
            for (sharded.shards, 0..) |shard, i| {
                var shard_path_buf: [4096]u8 = undefined;
                const shard_path = std.fmt.bufPrint(&shard_path_buf, "{s}/frozen_shard_{d}.zig", .{ cache_dir, i }) catch continue;
                const shard_file = std.fs.cwd().createFile(shard_path, .{}) catch continue;
                defer shard_file.close();
                shard_file.writeAll(shard) catch continue;
                total_size += shard.len;
            }

            // Write main file
            var zig_path_buf: [4096]u8 = undefined;
            const zig_path = std.fmt.bufPrint(&zig_path_buf, "{s}/frozen_module.zig", .{cache_dir}) catch "zig-out/cache/frozen_module.zig";
            const zig_file = std.fs.cwd().createFile(zig_path, .{}) catch {
                std.debug.print("[warn] Could not create frozen_module.zig\n", .{});
                break :zig_gen;
            };
            defer zig_file.close();
            zig_file.writeAll(sharded.main) catch {
                std.debug.print("[warn] Could not write frozen_module.zig\n", .{});
                break :zig_gen;
            };
            total_size += sharded.main.len;

            const zig_size_kb = @as(f64, @floatFromInt(total_size)) / 1024.0;
            std.debug.print("[build] Frozen module (Zig): {s} ({d:.1}KB, {d} shards)\n", .{ zig_path, zig_size_kb, sharded.shards.len });
        }

        // Step 6c2: Patch hooks with closure vars (if any)
        // This updates the hooked bundle to pass closure var arrays to frozen functions
        var closure_patch_path_buf: [4096]u8 = undefined;
        const closure_patch_path = std.fmt.bufPrint(&closure_patch_path_buf, "{s}/closure_manifest.json", .{cache_dir}) catch "zig-out/cache/closure_manifest.json";
        const patch_result = try runCommand(allocator, &.{
            "node", "tools/patch_closure_hooks.js", bundle_hooked_path, closure_patch_path,
        });
        defer {
            if (patch_result.stdout) |s| allocator.free(s);
            if (patch_result.stderr) |s| allocator.free(s);
        }
        if (patch_result.term.Exited != 0) {
            std.debug.print("[warn] Closure hook patching failed - closures may not work natively\n", .{});
            if (patch_result.stderr) |s| std.debug.print("{s}\n", .{s});
        }

        break :blk true;
    };

    if (!freeze_success) {
        // Create empty Zig stub so build doesn't break
        var empty_zig_path_buf: [4096]u8 = undefined;
        const empty_zig_path = std.fmt.bufPrint(&empty_zig_path_buf, "{s}/frozen_module.zig", .{cache_dir}) catch "zig-out/cache/frozen_module.zig";
        const empty_frozen = std.fs.cwd().createFile(empty_zig_path, .{}) catch null;
        if (empty_frozen) |f| {
            f.writeAll(
                \\// No frozen functions generated
                \\const zig_runtime = @import("zig_runtime");
                \\pub fn frozen_init_c(_: *zig_runtime.JSContext) c_int { return 0; }
                \\
            ) catch {};
            f.close();
        }
    }

    // Step 6d: Compile HOOKED JS to bytecode (for runtime)
    // Use hooked bundle if freeze succeeded, otherwise use original bundle
    const runtime_bundle_path = if (freeze_success) bundle_hooked_path else bundle_js_path;
    std.debug.print("[build] Compiling JS to bytecode for runtime: {s} (freeze_success={})\n", .{ runtime_bundle_path, freeze_success });
    // Ensure output directory exists
    std.fs.cwd().makePath(output_dir) catch {};
    const exit_code = try qjsc_wrapper.compileJsToBytecode(allocator, &.{
        "qjsc",
        "-e", // Required for executable bytecode (top-level code runs on JS_EvalFunction)
        "-N", "bundle", // Sets array name to "bundle" (required by bridge functions)
        "-o", bundle_compiled_path,
        runtime_bundle_path,
    });
    if (exit_code != 0) {
        std.debug.print("[error] qjsc compilation failed\n", .{});
        std.process.exit(1);
    }

    // Step 6d2: Also generate raw bytecode for native-embed (avoids 321MB C file OOM)
    var bundle_bin_path_buf: [4096]u8 = undefined;
    const bundle_bin_path = std.fmt.bufPrint(&bundle_bin_path_buf, "{s}/bundle.bin", .{cache_dir}) catch "zig-out/cache/bundle.bin";
    std.debug.print("[build] Generating raw bytecode for native-embed...\n", .{});
    const exit_code_bin = try qjsc_wrapper.compileJsToBytecode(allocator, &.{
        "qjsc",
        "-b", // Raw bytecode output (no C wrapper, just bytes)
        "-o", bundle_bin_path,
        runtime_bundle_path,
    });
    if (exit_code_bin != 0) {
        std.debug.print("[warn] Raw bytecode generation failed (native-embed may not work)\n", .{});
    } else {
        if (std.fs.cwd().statFile(bundle_bin_path)) |stat| {
            const size_mb = @as(f64, @floatFromInt(stat.size)) / 1024.0 / 1024.0;
            std.debug.print("[build] Raw bytecode: {s} ({d:.1}MB)\n", .{ bundle_bin_path, size_mb });
        } else |_| {}
    }

    // Keep qjsc-generated code but:
    // 1. Rename main() to qjsc_entry() so our Zig main() can call it
    // 2. Inject frozen_init_c call before js_std_eval_binary
    // 3. Add bridge functions for bytecode access
    const c_content = std.fs.cwd().readFileAlloc(allocator, bundle_compiled_path, 500 * 1024 * 1024) catch {
        std.debug.print("[error] Failed to read {s}\n", .{bundle_compiled_path});
        std.process.exit(1);
    };
    defer allocator.free(c_content);

    // Replace "int main(" with "int qjsc_entry(" so Zig can call it
    var modified = std.ArrayListUnmanaged(u8){};
    defer modified.deinit(allocator);

    // Process the content:
    // 1. Add extern declaration after #include "quickjs-libc.h"
    // 2. Rename "int main" to "int qjsc_entry"
    // 3. Inject frozen_init_c(ctx) before js_std_eval_binary

    // Find the include line and inject extern declaration after it
    const include_marker = "#include \"quickjs-libc.h\"";
    if (std.mem.indexOf(u8, c_content, include_marker)) |include_pos| {
        // Copy up to and including the include line
        var end_of_line = include_pos + include_marker.len;
        while (end_of_line < c_content.len and c_content[end_of_line] != '\n') : (end_of_line += 1) {}
        if (end_of_line < c_content.len) end_of_line += 1; // Include the newline
        modified.appendSlice(allocator, c_content[0..end_of_line]) catch {};
        // Add extern declaration
        modified.appendSlice(allocator, "\nextern int frozen_init_c(JSContext *ctx);\n") catch {};

        // Process the rest of the file
        var i: usize = end_of_line;
        while (i < c_content.len) {
            if (i + 8 <= c_content.len and std.mem.eql(u8, c_content[i .. i + 8], "int main")) {
                // Replace "int main" with "int qjsc_entry"
                modified.appendSlice(allocator, "int qjsc_entry") catch {};
                i += 8;
            } else if (i + 18 <= c_content.len and std.mem.eql(u8, c_content[i .. i + 18], "js_std_eval_binary")) {
                // Inject frozen_init_c call and set __frozen_init_complete before js_std_eval_binary
                // This enables hook redirection to frozen functions during module initialization
                modified.appendSlice(allocator, "{ JSValue _g = JS_GetGlobalObject(ctx); frozen_init_c(ctx); JS_SetPropertyStr(ctx, _g, \"__frozen_init_complete\", JS_TRUE); JS_FreeValue(ctx, _g); } js_std_eval_binary") catch {};
                i += 18;
            } else {
                modified.append(allocator, c_content[i]) catch {};
                i += 1;
            }
        }
    } else {
        // No include found, just do the replacements on the whole file
        var i: usize = 0;
        while (i < c_content.len) {
            if (i + 8 <= c_content.len and std.mem.eql(u8, c_content[i .. i + 8], "int main")) {
                modified.appendSlice(allocator, "int qjsc_entry") catch {};
                i += 8;
            } else if (i + 18 <= c_content.len and std.mem.eql(u8, c_content[i .. i + 18], "js_std_eval_binary")) {
                // Inject frozen_init_c call and set __frozen_init_complete before js_std_eval_binary
                modified.appendSlice(allocator, "{ JSValue _g = JS_GetGlobalObject(ctx); frozen_init_c(ctx); JS_SetPropertyStr(ctx, _g, \"__frozen_init_complete\", JS_TRUE); JS_FreeValue(ctx, _g); } js_std_eval_binary") catch {};
                i += 18;
            } else {
                modified.append(allocator, c_content[i]) catch {};
                i += 1;
            }
        }
    }

    // Add bridge functions
    const bridge_code =
        \\
        \\// Bridge functions for Zig extern access
        \\const uint8_t* get_bundle_ptr(void) { return bundle; }
        \\uint32_t get_bundle_size(void) { return bundle_size; }
        \\
    ;
    modified.appendSlice(allocator, bridge_code) catch {};

    const bridge_file = std.fs.cwd().createFile(bundle_compiled_path, .{}) catch {
        std.debug.print("[error] Failed to create {s}\n", .{bundle_compiled_path});
        std.process.exit(1);
    };
    defer bridge_file.close();
    bridge_file.writeAll(modified.items) catch {};

    if (std.fs.cwd().statFile(bundle_compiled_path)) |stat| {
        const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
        std.debug.print("[build] Bytecode: {s} ({d:.1}KB)\n", .{ bundle_compiled_path, size_kb });
    } else |_| {}

    // Generate output filenames based on input base name and output directory
    var wasm_path_buf: [4096]u8 = undefined;
    var aot_path_buf: [4096]u8 = undefined;
    var stripped_path_buf: [4096]u8 = undefined;

    const wasm_path = std.fmt.bufPrint(&wasm_path_buf, "{s}/{s}.wasm", .{ output_dir, output_base }) catch {
        std.debug.print("[error] Output path too long: {s}/{s}.wasm\n", .{ output_dir, output_base });
        std.process.exit(1);
    };
    const aot_path = std.fmt.bufPrint(&aot_path_buf, "{s}/{s}.aot", .{ output_dir, output_base }) catch {
        std.debug.print("[error] Output path too long: {s}/{s}.aot\n", .{ output_dir, output_base });
        std.process.exit(1);
    };
    const stripped_path = std.fmt.bufPrint(&stripped_path_buf, "{s}/{s}-stripped.wasm", .{ output_dir, output_base }) catch {
        std.debug.print("[error] Output path too long: {s}/{s}-stripped.wasm\n", .{ output_dir, output_base });
        std.process.exit(1);
    };

    // Step 7: Build WASM with embedded bytecode (skip if binary_only)
    // All frozen functions stay in WASM/AOT (sandboxed)
    // build.zig reads from <source-dir>/zig-out/ via -Dsource-dir parameter
    if (!options.binary_only) {
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

        // Verify WASM was actually created (build may succeed with exit 0 but not produce output)
        const static_wasm_path = "zig-out/bin/edgebox-static.wasm";
        _ = std.fs.cwd().statFile(static_wasm_path) catch {
            std.debug.print("[error] WASM build succeeded but {s} not found\n", .{static_wasm_path});
            std.debug.print("[error] Check that bundle_compiled.c exists at: {s}\n", .{bundle_compiled_path});
            if (wasm_result.stderr) |err| {
                std.debug.print("[error] Build stderr: {s}\n", .{err});
            }
            if (wasm_result.stdout) |out| {
                std.debug.print("[error] Build stdout: {s}\n", .{out});
            }
            std.process.exit(1);
        };

        // Copy from zig-out with output name based on input
        std.fs.cwd().copyFile("zig-out/bin/edgebox-static.wasm", std.fs.cwd(), wasm_path, .{}) catch |err| {
            std.debug.print("[error] Failed to copy WASM from {s} to {s}: {}\n", .{ static_wasm_path, wasm_path, err });
            std.process.exit(1);
        };

        if (std.fs.cwd().statFile(wasm_path)) |stat| {
            const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
            std.debug.print("[build] Static WASM: {s} ({d:.1}KB)\n", .{ wasm_path, size_kb });
        } else |_| {}
    } else {
        std.debug.print("[build] Skipping WASM build (--binary-only)\n", .{});
    }

    // Step 7b: Build native binary using native-embed (raw bytecode via @embedFile)
    // This avoids OOM from parsing 321MB C hex arrays by embedding bytecode directly
    // Uses the SAME frozen_module.zig as WASM, but embeds bytecode via linker
    std.debug.print("[build] Building native binary with embedded bytecode (native-embed)...\n", .{});

    // Construct bytecode path argument
    var bytecode_arg_buf: [4096]u8 = undefined;
    const bytecode_arg = std.fmt.bufPrint(&bytecode_arg_buf, "-Dbytecode={s}/bundle.bin", .{cache_dir}) catch {
        std.debug.print("[error] Bytecode path too long\n", .{});
        std.process.exit(1);
    };

    const native_result = if (source_dir_arg.len > 0)
        try runCommand(allocator, &.{
            "zig", "build", "native-embed", "-Doptimize=ReleaseFast", source_dir_arg, bytecode_arg,
        })
    else
        try runCommand(allocator, &.{
            "zig", "build", "native-embed", "-Doptimize=ReleaseFast", bytecode_arg,
        });
    defer {
        if (native_result.stdout) |s| allocator.free(s);
        if (native_result.stderr) |s| allocator.free(s);
    }

    // Generate binary path (no extension)
    var binary_path_buf: [4096]u8 = undefined;
    const binary_path = std.fmt.bufPrint(&binary_path_buf, "{s}/{s}", .{ output_dir, output_base }) catch {
        std.debug.print("[error] Output path too long: {s}/{s}\n", .{ output_dir, output_base });
        std.process.exit(1);
    };

    if (native_result.term.Exited != 0) {
        std.debug.print("[warn] Native-embed build failed (WASM/AOT still usable)\n", .{});
        if (native_result.stderr) |err| {
            std.debug.print("{s}\n", .{err});
        }
    } else {
        // Copy from zig-out with output name based on input
        std.fs.cwd().copyFile("zig-out/bin/edgebox-native-embed", std.fs.cwd(), binary_path, .{}) catch |err| {
            std.debug.print("[warn] Failed to copy binary: {}\n", .{err});
        };
        if (std.fs.cwd().statFile(binary_path)) |stat| {
            const size_mb = @as(f64, @floatFromInt(stat.size)) / 1024.0 / 1024.0;
            std.debug.print("[build] Binary: {s} ({d:.1}MB)\n", .{ binary_path, size_mb });
        } else |_| {}
    }

    // Steps 8-10: Strip, wasm-opt, AOT (skip if binary_only)
    if (!options.binary_only) {
        // Step 8: Strip debug sections (reduces size significantly)
        std.debug.print("[build] Stripping debug sections...\n", .{});
        stripWasmDebug(allocator, wasm_path, stripped_path) catch |err| {
            std.debug.print("[warn] Debug strip failed: {}\n", .{err});
        };
        std.fs.cwd().deleteFile(wasm_path) catch {};
        std.fs.cwd().rename(stripped_path, wasm_path) catch {};

        // Step 9: Optimize WASM with Binaryen (wasm-opt)
        std.debug.print("[build] Running wasm-opt (Binaryen)...\n", .{});
        optimizeWasm(allocator, wasm_path) catch |err| {
            std.debug.print("[warn] wasm-opt failed: {}\n", .{err});
        };

        // Step 10: AOT compile (SIMD enabled to match WASM build)
        // Note: Wizer is skipped for AOT - native code initializes fast enough
        std.debug.print("[build] AOT compiling with WAMR...\n", .{});
        const aot_compiler = @import("aot_compiler.zig");
        var aot_skipped = false;
        aot_compiler.compileWasmToAot(allocator, wasm_path, aot_path, true) catch |err| {
            if (err == error.WasmTooLarge) {
                aot_skipped = true;
                std.debug.print("[build] AOT skipped (WASM too large)\n", .{});
            } else {
                std.debug.print("[warn] AOT compilation failed: {}\n", .{err});
            }
        };

        if (!aot_skipped) {
            if (std.fs.cwd().statFile(aot_path)) |stat| {
                const size_mb = @as(f64, @floatFromInt(stat.size)) / 1024.0 / 1024.0;
                std.debug.print("[build] AOT: {s} ({d:.1}MB)\n", .{ aot_path, size_mb });
            } else |_| {
                std.debug.print("[warn] AOT file not created\n", .{});
            }
        }

        // Summary for full build
        std.debug.print("\n[build] === Static Build Complete ===\n\n", .{});
        std.debug.print("Files created:\n", .{});
        std.debug.print("  {s}     - Native binary (QuickJS + frozen)\n", .{binary_path});
        std.debug.print("  {s}   - WASM with embedded bytecode\n", .{wasm_path});
        std.debug.print("  {s}  - AOT native module\n\n", .{aot_path});
        std.debug.print("To run:\n", .{});
        std.debug.print("  ./{s}            # Binary (fastest startup)\n", .{binary_path});
        std.debug.print("  edgebox {s}   # WASM (sandboxed)\n", .{wasm_path});
        std.debug.print("  edgebox {s}  # AOT (sandboxed, fast)\n\n", .{aot_path});
    } else {
        // Summary for binary-only build
        std.debug.print("\n[build] === Binary Build Complete ===\n\n", .{});
        std.debug.print("File created:\n", .{});
        std.debug.print("  {s}  - Native binary (QuickJS + frozen)\n\n", .{binary_path});
        std.debug.print("To run:\n", .{});
        std.debug.print("  ./{s}\n\n", .{binary_path});
    }
}

fn runWizerStatic(allocator: std.mem.Allocator, wasm_path: []const u8) !void {
    std.debug.print("[build] Running Wizer pre-initialization (built-in WAMR)...\n", .{});

    // Use wizer library directly (no external CLI needed!)
    var wz = wizer.Wizer.init(allocator, .{});

    // Create temp output path: "path/to/file.wasm" -> "path/to/file-wizer.wasm"
    var wizer_path_buf: [4096]u8 = undefined;
    const base = if (wasm_path.len > 5) wasm_path[0 .. wasm_path.len - 5] else wasm_path;
    const wizer_path = std.fmt.bufPrint(&wizer_path_buf, "{s}-wizer.wasm", .{base}) catch |err| {
        std.debug.print("[error] Wizer failed: path too long\n", .{});
        return err;
    };

    wz.run(wasm_path, wizer_path) catch |err| {
        std.debug.print("[error] Wizer pre-initialization FAILED: {}\n", .{err});
        std.debug.print("[error] Wizer is REQUIRED for static builds. Build cannot continue.\n", .{});
        std.debug.print("[error] This may be due to:\n", .{});
        std.debug.print("[error]   - WASM runtime incompatibility (check WAMR build)\n", .{});
        std.debug.print("[error]   - Insufficient memory (wizer needs ~4x WASM file size)\n", .{});
        std.debug.print("[error]   - WASM module using unsupported features\n", .{});
        return err;
    };

    std.fs.cwd().deleteFile(wasm_path) catch {};
    std.fs.cwd().rename(wizer_path, wasm_path) catch |err| {
        std.debug.print("[error] Failed to rename wizer output: {}\n", .{err});
        return err;
    };

    if (std.fs.cwd().statFile(wasm_path)) |stat| {
        const size_kb = @as(f64, @floatFromInt(stat.size)) / 1024.0;
        std.debug.print("[build] Wizer snapshot: {d:.1}KB\n", .{size_kb});
    } else |_| {}
}

/// Optimize WASM file using Binaryen (wasm-opt C API)
fn optimizeWasm(allocator: std.mem.Allocator, wasm_path: []const u8) !void {
    // Read WASM file
    const input_file = try std.fs.cwd().openFile(wasm_path, .{});
    defer input_file.close();
    const input = try input_file.readToEndAlloc(allocator, 100 * 1024 * 1024);
    defer allocator.free(input);

    const original_size = input.len;

    // Optimize with Oz (aggressive size optimization)
    const result = try wasm_opt.optimize(allocator, input, .Oz);
    defer allocator.free(result.binary);

    // Write back
    try std.fs.cwd().writeFile(.{ .sub_path = wasm_path, .data = result.binary });

    const saved = if (original_size > result.optimized_size) original_size - result.optimized_size else 0;
    const percent = if (original_size > 0) @as(f64, @floatFromInt(saved)) / @as(f64, @floatFromInt(original_size)) * 100 else 0;
    std.debug.print("[build] wasm-opt: {d}KB -> {d}KB (saved {d:.1}%)\n", .{
        original_size / 1024,
        result.optimized_size / 1024,
        percent,
    });
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
        // Bounds check to prevent integer overflow and out-of-bounds access
        if (section_size > data.len or pos > data.len - section_size) {
            return error.MalformedWasm;
        }
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
    // Validate wasm path before manipulation
    if (wasm_path.len < 6 or !std.mem.endsWith(u8, wasm_path, ".wasm")) {
        std.debug.print("[warn] Invalid wasm path for optimization: {s}\n", .{wasm_path});
        return;
    }

    // Read input WASM file
    const file = std.fs.cwd().openFile(wasm_path, .{}) catch |err| {
        std.debug.print("[build] wasm-opt: Could not open {s}: {}\n", .{ wasm_path, err });
        return;
    };
    defer file.close();

    const input_data = file.readToEndAlloc(allocator, 100 * 1024 * 1024) catch |err| { // 100MB max
        std.debug.print("[build] wasm-opt: Could not read {s}: {}\n", .{ wasm_path, err });
        return;
    };
    defer allocator.free(input_data);

    const before_kb = @as(f64, @floatFromInt(input_data.len)) / 1024.0;
    std.debug.print("[build] Running wasm-opt (-Oz aggressive size optimization)...\n", .{});
    std.debug.print("[build]   Input: {s} ({d:.1}KB)\n", .{ wasm_path, before_kb });

    // Run optimization using Binaryen library directly (no subprocess needed)
    // Note: wasm-opt may fail on wizered WASM due to bulk memory assertions - skip silently
    const opt_result = wasm_opt.optimize(allocator, input_data, .Oz) catch |err| {
        std.debug.print("[build]   wasm-opt skipped: {}\n", .{err});
        return;
    };
    defer allocator.free(opt_result.binary);

    // Write optimized output back to same path
    const out_file = std.fs.cwd().createFile(wasm_path, .{}) catch |err| {
        std.debug.print("[build]   Could not write optimized output: {}\n", .{err});
        return;
    };
    defer out_file.close();

    out_file.writeAll(opt_result.binary) catch |err| {
        std.debug.print("[build]   Could not write optimized data: {}\n", .{err});
        return;
    };

    // Report results
    const after_kb = @as(f64, @floatFromInt(opt_result.optimized_size)) / 1024.0;
    const reduction = ((before_kb - after_kb) / before_kb) * 100.0;
    std.debug.print("[build]   Output: {d:.1}KB ({d:.1}% reduction)\n", .{ after_kb, reduction });
}

fn runWizer(allocator: std.mem.Allocator) !void {
    std.debug.print("[build] Running edgebox-wizer pre-initialization...\n", .{});
    const wizer_result = try runCommand(allocator, &.{
        "edgebox-wizer",
        "edgebox-base.wasm",
        "edgebox-wizer.wasm",
        "--init-func=wizer_init",
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
    std.debug.print("[build] Optimizing WASM with edgebox-wasm-opt...\n", .{});
    const opt_result = try runCommand(allocator, &.{
        "edgebox-wasm-opt", "edgebox-base.wasm", "edgebox-base-opt.wasm", "-Oz",
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

/// Apply trace injection patterns using native sed with a single script file
/// This reduces 25 subprocess calls to just 1, saving 5-10 seconds
fn applyTracePatterns(allocator: std.mem.Allocator, bundle_path: []const u8) !void {
    // Create temp sed script file
    const script_path = "/tmp/edgebox_trace_patterns.sed";
    const script_file = try std.fs.cwd().createFile(script_path, .{});
    defer std.fs.cwd().deleteFile(script_path) catch {};
    // Note: script_file is closed explicitly before running sed

    // Write all sed patterns to script (one per line)
    const patterns =
        \\s/^Dp7();$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('before_Dp7'); Dp7().then(function(){ if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('Dp7_resolved'); }).catch(function(e){ print('[ENTRY ERROR] ' + e); });/g
        \\s/^var __create = Object.create;$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('user_code_start'); var __create = Object.create;/g
        \\s/^var import_node_module = require("node:module");$/var import_node_module = require("node:module"); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_node_module_require');/g
        \\s/^var import_fs = require("fs");$/var import_fs = require("fs"); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_fs_require');/g
        \\s/^var import_promises2 = require("node:timers\/promises");$/var import_promises2 = require("node:timers\/promises"); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_timers_require');/g
        \\s/^var import_node_http = require("node:http");$/var import_node_http = require("node:http"); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_http_require');/g
        \\s/^var UA = import_node_module.createRequire(\([^)]*\));$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('before_createRequire'); var UA = import_node_module.createRequire(\1); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_createRequire');/g
        \\s/^function v9(A) {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_4752'); function v9(A) {/g
        \\s/^var TF1 = z((cR0)/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_10k'); var TF1 = z((cR0)/g
        \\s/^var EU = z((dN3/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_50k'); var EU = z((dN3/g
        \\s/^var Dk1 = z((sy3/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_75k'); var Dk1 = z((sy3/g
        \\s/^var rr1 = z((VoB)/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_150k'); var rr1 = z((VoB)/g
        \\s/^var XJ0 = z((tl2)/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_250k'); var XJ0 = z((tl2)/g
        \\s/^var Nz9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_290k'); var Nz9 = O(() => {/g
        \\s/^var rq9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_300k'); var rq9 = O(() => {/g
        \\s/^var BN9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_300k_227'); var BN9 = O(() => {/g
        \\s/^var bN9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_301k'); var bN9 = O(() => {/g
        \\s/^var WL9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_302k'); var WL9 = O(() => {/g
        \\s/^var CL9 = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('line_303k'); var CL9 = O(() => {/g
        \\s/^yr();$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('after_CL9_before_yr'); yr();/g
        \\s/^dD();$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('dD_start'); dD(); if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('dD_done');/g
        \\s/^var kB = O(() => {$/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_define'); var kB = O(() => { if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_enter');/g
        \\s/^  IQ = qq;$/  if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_before_IQ'); IQ = qq; if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('kB_end');/g
        \\s/^var vs = new YW1()/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('before_commander'); var vs = new YW1()/g
        \\s/v9("cli_entry")/if(typeof __EDGEBOX_TRACE==='function')__EDGEBOX_TRACE('cli_entry_v9'); v9("cli_entry")/g
        \\
    ;
    try script_file.writeAll(patterns);
    script_file.close();  // Close before running sed so it can read the file

    // Run sed ONCE with the script file
    // Note: macOS sed requires -i '' (empty string), Linux sed requires -i (no argument)
    // Use -i.bak which works on both, then delete the backup
    const result = try runCommand(allocator, &.{
        "sed", "-i.bak", "-f", script_path, bundle_path,
    });
    // Clean up backup file created by sed -i.bak
    const backup_path = try std.fmt.allocPrint(allocator, "{s}.bak", .{bundle_path});
    defer allocator.free(backup_path);
    std.fs.cwd().deleteFile(backup_path) catch {};
    defer {
        if (result.stdout) |s| allocator.free(s);
        if (result.stderr) |s| allocator.free(s);
    }

    if (result.term.Exited != 0) {
        std.debug.print("[error] sed trace injection failed\n", .{});
        if (result.stderr) |s| std.debug.print("{s}\n", .{s});
        return error.SedFailed;
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

