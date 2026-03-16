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

/// Check if a byte is a valid JS identifier character (a-z, A-Z, 0-9, _, $)
fn isIdentChar(ch: u8) bool {
    return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or
        (ch >= '0' and ch <= '9') or ch == '_' or ch == '$';
}

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

    // Parse size values with K/M suffix (e.g., "1500K", "2M", "3145728")
    const parseSizeValue = struct {
        fn parse(s: []const u8) !u32 {
            if (s.len == 0) return error.InvalidValue;
            const last = s[s.len - 1];
            if (last == 'K' or last == 'k') {
                const n = try std.fmt.parseInt(u32, s[0 .. s.len - 1], 10);
                return n * 1024;
            } else if (last == 'M' or last == 'm') {
                const n = try std.fmt.parseInt(u32, s[0 .. s.len - 1], 10);
                return n * 1024 * 1024;
            }
            return std.fmt.parseInt(u32, s, 10);
        }
    }.parse;

    // Parse arguments
    var dynamic_mode = false;
    var force_rebuild = false;
    var no_polyfill = false;
    var no_freeze = false;
    var no_bundle = false;
    var binary_only = false;
    var wasm_only = false;
    var debug_build = false;
    var allocator_type: AllocatorType = .gpa; // Default: GPA for debugging (use --allocator=c for production)
    var allocator_explicitly_set = false;
    var output_prefix: ?[]const u8 = null; // Custom output prefix (default: zig-out)
    var freeze_max_functions: u32 = 0; // 0 = freeze all, N = freeze only top N largest
    var freeze_profile_path: ?[]const u8 = null; // PGO: path to call profile JSON
    var freeze_code_budget: u32 = 0; // 0 = auto-detect L2, N = max frozen code bytes
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
        } else if (std.mem.eql(u8, arg, "--wasm-only")) {
            wasm_only = true;
        } else if (std.mem.eql(u8, arg, "--debug")) {
            debug_build = true;
        } else if (std.mem.startsWith(u8, arg, "--allocator=")) {
            const value = arg[12..];
            allocator_explicitly_set = true;
            if (std.mem.eql(u8, value, "arena")) {
                allocator_type = .arena;
            } else if (std.mem.eql(u8, value, "gpa")) {
                allocator_type = .gpa;
            } else if (std.mem.eql(u8, value, "c")) {
                allocator_type = .c;
            } else {
                std.debug.print("Unknown allocator: {s} (use: arena, c, gpa)\n", .{value});
                std.process.exit(1);
            }
        } else if (std.mem.startsWith(u8, arg, "--output-dir=")) {
            output_prefix = arg[13..];
        } else if (std.mem.startsWith(u8, arg, "--freeze-max-functions=")) {
            const prefix_len = "--freeze-max-functions=".len;
            freeze_max_functions = std.fmt.parseInt(u32, arg[prefix_len..], 10) catch {
                std.debug.print("Invalid --freeze-max-functions value: {s}\n", .{arg[prefix_len..]});
                std.process.exit(1);
            };
        } else if (std.mem.startsWith(u8, arg, "--freeze-code-budget=")) {
            const val_str = arg["--freeze-code-budget=".len..];
            // Parse size with K/M suffix (e.g., "1500K", "2M", "3145728")
            freeze_code_budget = parseSizeValue(val_str) catch {
                std.debug.print("Invalid --freeze-code-budget value: {s} (use e.g., 1500K, 2M, or bytes)\n", .{val_str});
                std.process.exit(1);
            };
        } else if (std.mem.startsWith(u8, arg, "--freeze-profile=")) {
            freeze_profile_path = arg["--freeze-profile=".len..];
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

    // Use libc malloc by default for frozen builds.
    // Arena can't free individual allocations → unbounded memory growth (1.3GB for TSC)
    // which thrashes caches and causes 7.5x slowdown vs libc malloc.
    if (comptime builtin.os.tag == .linux) {
        if (!no_freeze and !allocator_explicitly_set) {
            allocator_type = .c;
        }
    }

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
            .wasm_only = wasm_only,
            .debug_build = debug_build,
            .allocator_type = allocator_type,
            .output_prefix = output_prefix,
            .freeze_max_functions = freeze_max_functions,
            .freeze_profile_path = freeze_profile_path,
            .freeze_code_budget = freeze_code_budget,
        });
    }
}

fn printUsage() void {
    std.debug.print(
        \\EdgeBox Compiler
        \\
        \\Usage:
        \\  edgeboxc <app.js>   AOT-compile numeric JS kernels to WASM for V8/workerd
        \\
        \\Output (all in zig-out/bin/<app.js>/):
        \\  <app>-worker.mjs        Worker module (source-transformed JS + WASM calls)
        \\  <app>-standalone.wasm   Standalone WASM (AOT-compiled numeric kernels)
        \\  <app>-config.capnp      workerd configuration
        \\  <app>                   Native binary (optional, QuickJS + frozen)
        \\
        \\Options:
        \\  -f, --force      Clean previous build outputs first
        \\  --no-polyfill    Skip Node.js polyfills (faster builds for simple JS)
        \\  --no-freeze      Skip freeze analysis (faster builds, no optimization)
        \\  --no-bundle      Skip Bun bundler (for simple JS without imports)
        \\  --binary-only    Only build native binary (skip WASM/AOT)
        \\  --wasm-only      Only build WASM + AOT (skip native binary)
        \\  --debug          Use Debug optimization (faster compile, slower runtime)
        \\  --allocator=X    Allocator for native binary: gpa (default), c, arena
        \\  --output-dir=X   Custom output directory (default: zig-out)
        \\  --freeze-max-functions=N  Freeze only top N functions (0=all, requires --freeze-profile)
        \\  --freeze-code-budget=SIZE  Max frozen code size (e.g., 1500K, 2M; 0=auto-detect L2)
        \\  --freeze-profile=PATH    PGO: sort functions by call profile (from --profile-out)
        \\  --minimal        All of the above (fastest, for test262)
        \\  -h, --help       Show this help
        \\  -v, --version    Show version
        \\
        \\Examples:
        \\  edgeboxc app.js                        Build Worker + WASM + native
        \\  node zig-out/bin/app.js/app-worker.mjs  Run on Node.js (V8 + WASM)
        \\  npx wrangler dev                        Deploy to Cloudflare Workers
        \\
    , .{});
}

/// Clean all build outputs
fn cleanBuildOutputs() void {
    const files_to_clean = [_][]const u8{
        "zig-out/bundle.js",
        "zig-out/bundle.bin",
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
        "bun",           "build",        entry_path,        "--outfile=zig-out/bundle.js",
        "--target=node", "--format=cjs", "--minify",        "--external=fs",
        "--external=path", "--external=os", "--external=crypto", "--external=util",
        "--external=stream", "--external=events", "--external=buffer", "--external=module",
    });
    defer {
        if (bun_result.stdout) |s| allocator.free(s);
        if (bun_result.stderr) |s| allocator.free(s);
    }

    if (bun_result.term.Exited != 0) {
        // Try without minify
        std.debug.print("[warn] Bun minify failed, trying without...\n", .{});
        const retry = try runCommand(allocator, &.{
            "bun",           "build",        entry_path,        "--outfile=zig-out/bundle.js",
            "--target=node", "--format=cjs", "--external=fs",   "--external=path",
            "--external=os", "--external=crypto", "--external=util", "--external=stream",
            "--external=events", "--external=buffer", "--external=module",
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
    // We want final order: runtime.js (globals), then polyfill modules, then user code
    const runtime_path = "src/polyfills/runtime.js";

    // Polyfill modules in dependency order (prepended in reverse order)
    // NOTE: process.js and os.js removed - fully implemented in native Zig
    const polyfill_modules = [_][]const u8{
        "src/polyfills/modules/child_process.js",
        "src/polyfills/modules/readline.js",
        "src/polyfills/modules/dns.js",
        "src/polyfills/modules/cluster.js",
        "src/polyfills/modules/zlib.js",
        "src/polyfills/modules/dgram.js",
        "src/polyfills/modules/tls.js",
        "src/polyfills/modules/net.js",
        "src/polyfills/modules/http2.js",
        "src/polyfills/modules/http.js",
        "src/polyfills/modules/url.js",
        "src/polyfills/modules/crypto.js",
        "src/polyfills/modules/fs.js",
        "src/polyfills/modules/stream.js",
        "src/polyfills/modules/events.js",
        "src/polyfills/modules/encoding.js",
        "src/polyfills/modules/buffer.js",
        "src/polyfills/modules/path.js",
        "src/polyfills/modules/core.js",
    };

    // Prepend each module (in reverse order so core.js ends up first)
    std.debug.print("[build] Prepending polyfill modules...\n", .{});
    for (polyfill_modules) |mod_path| {
        if (std.fs.cwd().access(mod_path, .{})) |_| {
            try prependPolyfills(allocator, mod_path, "zig-out/bundle.js");
        } else |_| {}
    }

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
            "zig", "build", "-j4", "wasm", "-Doptimize=ReleaseFast",
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

/// Allocator types for native binaries
pub const AllocatorType = enum { gpa, arena, c };

/// Build options for customizing the compilation pipeline
const BuildOptions = struct {
    no_polyfill: bool = false, // Skip Node.js polyfills
    no_freeze: bool = false, // Skip freeze analysis
    no_bundle: bool = false, // Skip Bun bundler
    binary_only: bool = false, // Only build native binary (skip WASM/AOT)
    wasm_only: bool = false, // Only build WASM + AOT (skip native binary)
    debug_build: bool = false, // Use Debug optimization (faster compile, slower runtime)
    allocator_type: AllocatorType = .gpa, // Allocator for native binary (gpa=debug, c=fastest, arena=batch)
    output_prefix: ?[]const u8 = null, // Custom output prefix (default: zig-out)
    freeze_max_functions: u32 = 0, // 0 = freeze all, N = freeze only top N largest functions
    freeze_profile_path: ?[]const u8 = null, // PGO: path to call profile JSON from --profile-out
    freeze_code_budget: u32 = 0, // 0 = auto (L2 cache size), N = max frozen code bytes
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
    //   cache_dir  = "zig-out/cache/bench"  (intermediate files: bundle.js, bundle.bin)
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
    const out_prefix = options.output_prefix orelse "zig-out";
    const cache_dir = blk: {
        if (source_dir.len > 0) {
            const len = std.fmt.bufPrint(&cache_dir_buf, "{s}/cache/{s}", .{ out_prefix, source_dir }) catch {
                std.debug.print("[error] Source directory path too long: {s}\n", .{source_dir});
                std.process.exit(1);
            };
            break :blk cache_dir_buf[0..len.len];
        }
        const len = std.fmt.bufPrint(&cache_dir_buf, "{s}/cache", .{out_prefix}) catch {
            break :blk "zig-out/cache";
        };
        break :blk cache_dir_buf[0..len.len];
    };
    const output_dir = blk: {
        if (source_dir.len > 0) {
            const len = std.fmt.bufPrint(&output_dir_buf, "{s}/bin/{s}", .{ out_prefix, source_dir }) catch {
                std.debug.print("[error] Source directory path too long: {s}\n", .{source_dir});
                std.process.exit(1);
            };
            break :blk output_dir_buf[0..len.len];
        }
        const len = std.fmt.bufPrint(&output_dir_buf, "{s}/bin", .{out_prefix}) catch {
            break :blk "zig-out/bin";
        };
        break :blk output_dir_buf[0..len.len];
    };
    // Build -Dsource-dir argument for zig build (tells build.zig where to find bundle.bin)
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

    // bundle.bin path for unified @embedFile flow (used by all targets)

    var bundle_original_path_buf: [4096]u8 = undefined;
    const bundle_original_path = std.fmt.bufPrint(&bundle_original_path_buf, "{s}/bundle_original.c", .{cache_dir}) catch "zig-out/cache/bundle_original.c";

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
            "bun",           "build",        entry_path,        bun_outfile_arg,
            "--target=node", "--format=cjs", "--external=fs",   "--external=path",
            "--external=os", "--external=crypto", "--external=util", "--external=stream",
            "--external=events", "--external=buffer", "--external=string_decoder",
            "--external=module", "--external=tty", "--external=net", "--external=http",
            "--external=https", "--external=url", "--external=querystring", "--external=zlib",
            "--external=child_process", "--external=worker_threads", "--external=perf_hooks",
            "--external=assert", "--external=constants", "--external=vm", "--external=readline",
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

    // Save clean bundle for workerd worker generation (before polyfills/hooks)
    var clean_bundle_path_buf: [4096]u8 = undefined;
    const clean_bundle_path = std.fmt.bufPrint(&clean_bundle_path_buf, "{s}/bundle_clean.js", .{cache_dir}) catch null;
    if (clean_bundle_path) |cbp| {
        std.fs.cwd().copyFile(bundle_js_path, std.fs.cwd(), cbp, .{}) catch {};
    }

    // Step 4: Prepend polyfills (skip if --no-polyfill flag)
    if (!options.no_polyfill) {
        // All polyfill modules (in dependency order)
        const all_polyfill_modules = [_][]const u8{
            "src/polyfills/modules/util.js",
            "src/polyfills/modules/child_process.js",
            "src/polyfills/modules/readline.js",
            "src/polyfills/modules/dns.js",
            "src/polyfills/modules/timers.js",
            "src/polyfills/modules/perf_hooks.js",
            "src/polyfills/modules/cluster.js",
            "src/polyfills/modules/zlib.js",
            "src/polyfills/modules/dgram.js",
            "src/polyfills/modules/tls.js",
            "src/polyfills/modules/net.js",
            "src/polyfills/modules/https.js",
            "src/polyfills/modules/http2.js",
            "src/polyfills/modules/http.js",
            "src/polyfills/modules/url.js",
            "src/polyfills/modules/crypto.js",
            "src/polyfills/modules/fs.js",
            "src/polyfills/modules/stream.js",
            "src/polyfills/modules/events.js",
            "src/polyfills/modules/encoding.js",
            "src/polyfills/modules/buffer.js",
            "src/polyfills/modules/path.js",
            "src/polyfills/modules/core.js",
        };

        // Core modules always included (required for require() to work)
        const core_modules = [_][]const u8{
            "src/polyfills/modules/core.js",
            "src/polyfills/modules/path.js",
            "src/polyfills/modules/buffer.js",
            "src/polyfills/modules/events.js",
        };

        // Tree shaking: analyze bundle for required modules and globals
        std.debug.print("[build] Analyzing bundle for tree shaking...\n", .{});
        const bundle_content = std.fs.cwd().readFileAlloc(allocator, bundle_js_path, 10 * 1024 * 1024) catch |err| {
            std.debug.print("[error] Could not read bundle for tree shaking: {}\n", .{err});
            std.process.exit(1);
        };
        defer allocator.free(bundle_content);

        const tree_shake = @import("polyfills/tree_shake.zig");

        // Analyze require() calls
        const required = tree_shake.analyzeRequires(allocator, bundle_content) catch |err| {
            std.debug.print("[error] Tree shaking analysis failed: {}\n", .{err});
            std.process.exit(1);
        };
        defer {
            for (required) |r| allocator.free(r);
            allocator.free(required);
        }
        std.debug.print("[build] Found {} required modules: ", .{required.len});
        for (required) |r| std.debug.print("{s} ", .{r});
        std.debug.print("\n", .{});

        // Analyze global API usage for runtime module tree shaking
        const required_runtime = tree_shake.analyzeGlobals(allocator, bundle_content) catch |err| {
            std.debug.print("[error] Global analysis failed: {}\n", .{err});
            std.process.exit(1);
        };
        defer allocator.free(required_runtime);
        std.debug.print("[build] Found {} runtime globals: ", .{required_runtime.len});
        for (required_runtime) |r| {
            const basename = std.fs.path.basename(r);
            std.debug.print("{s} ", .{basename});
        }
        std.debug.print("\n", .{});

        // Check if this is a TypeScript bundle (tsc.js runtime module is included)
        var is_typescript_bundle = false;
        for (required_runtime) |r| {
            const basename = std.fs.path.basename(r);
            if (std.mem.eql(u8, basename, "tsc.js")) {
                is_typescript_bundle = true;
                break;
            }
        }

        // Build list of runtime modules to include (sorted in dependency order)
        var runtime_list = try std.ArrayList([]const u8).initCapacity(allocator, 16);
        defer runtime_list.deinit(allocator);

        // Always include core.js (console, timers, error handling)
        try runtime_list.append(allocator, "src/polyfills/runtime/core.js");

        // Add required runtime modules (but not core.js or end.js)
        for (required_runtime) |mod| {
            if (!std.mem.eql(u8, mod, "src/polyfills/runtime/core.js") and
                !std.mem.eql(u8, mod, "src/polyfills/runtime/end.js"))
            {
                var found = false;
                for (runtime_list.items) |existing| {
                    if (std.mem.eql(u8, existing, mod)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    try runtime_list.append(allocator, mod);
                }
            }
        }

        // Always include end.js last (closes the guard block)
        try runtime_list.append(allocator, "src/polyfills/runtime/end.js");

        // Sort runtime modules in dependency order
        const sorted_runtime = tree_shake.sortRuntimeModules(allocator, runtime_list.items) catch runtime_list.items;

        std.debug.print("[build] Runtime tree shaking: {}/15 modules included\n", .{sorted_runtime.len});

        // Check if code uses require() at all - if not, skip polyfill modules entirely
        const uses_require = tree_shake.usesRequire(bundle_content);
        std.debug.print("[build] Uses require(): {}\n", .{uses_require});

        // Build list of polyfill modules to include (only if require() is used)
        var include_list = try std.ArrayList([]const u8).initCapacity(allocator, 32);
        defer include_list.deinit(allocator);

        if (uses_require) {
            // Include core modules (needed for require() to work)
            for (core_modules) |mod| {
                try include_list.append(allocator, mod);
            }

            // Module-to-file mapping for modules defined inside other files
            const module_file_map = [_]struct { module: []const u8, file: []const u8 }{
                .{ .module = "async_hooks", .file = "src/polyfills/modules/zlib.js" },
                .{ .module = "timers/promises", .file = "src/polyfills/modules/zlib.js" },
            };

            // Add any mapped modules first
            for (required) |req| {
                for (module_file_map) |mapping| {
                    if (std.mem.eql(u8, req, mapping.module)) {
                        var already_added = false;
                        for (include_list.items) |item| {
                            if (std.mem.eql(u8, item, mapping.file)) {
                                already_added = true;
                                break;
                            }
                        }
                        if (!already_added) {
                            try include_list.append(allocator, mapping.file);
                        }
                    }
                }
            }

            // Add required modules (with dependency resolution)
            for (all_polyfill_modules) |mod_path| {
                const basename = std.fs.path.basename(mod_path);
                const mod_name = basename[0 .. basename.len - 3]; // strip .js

                var is_core = false;
                for (core_modules) |core| {
                    if (std.mem.eql(u8, mod_path, core)) {
                        is_core = true;
                        break;
                    }
                }
                if (is_core) continue;

                for (required) |req| {
                    const should_add = std.mem.eql(u8, mod_name, req) or
                        (std.mem.startsWith(u8, req, mod_name) and req.len > mod_name.len and req[mod_name.len] == '/');
                    if (should_add) {
                        var already_in_list = false;
                        for (include_list.items) |item| {
                            if (std.mem.eql(u8, item, mod_path)) {
                                already_in_list = true;
                                break;
                            }
                        }
                        if (!already_in_list) {
                            try include_list.append(allocator, mod_path);
                        }
                        break;
                    }
                }
            }

            // Add end.js to close the IIFE
            try include_list.append(allocator, "src/polyfills/modules/end.js");
        }

        const modules_to_include = try include_list.toOwnedSlice(allocator);
        if (uses_require) {
            std.debug.print("[build] Polyfill tree shaking: {}/{} modules included\n", .{ modules_to_include.len - 1, all_polyfill_modules.len });
        } else {
            std.debug.print("[build] Polyfill tree shaking: 0/{} modules (no require() calls)\n", .{all_polyfill_modules.len});
        }

        // Prepend polyfill modules in REVERSE order (last prepended = first in file)
        if (modules_to_include.len > 0) {
            std.debug.print("[build] Prepending {} polyfill modules...\n", .{modules_to_include.len});
            var i: usize = modules_to_include.len;
            while (i > 0) {
                i -= 1;
                const mod_path = modules_to_include[i];
                if (std.fs.cwd().access(mod_path, .{})) |_| {
                    try prependPolyfills(allocator, mod_path, bundle_js_path);
                } else |_| {}
            }
        }

        // Prepend runtime modules in REVERSE order (core.js ends up at TOP)
        // Use prependRuntimeModule (no IIFE wrapping) instead of prependPolyfills
        std.debug.print("[build] Prepending {} runtime modules...\n", .{sorted_runtime.len});
        var j: usize = sorted_runtime.len;
        while (j > 0) {
            j -= 1;
            const mod_path = sorted_runtime[j];
            if (std.fs.cwd().access(mod_path, .{})) |_| {
                try prependRuntimeModule(allocator, mod_path, bundle_js_path);
            } else |_| {}
        }

        // Step 5b: Patch TypeScript bundles to enable Native Shapes factory interception
        // TypeScript bundles have createSourceFile as a local variable, not on globalThis.ts
        // We inject code right before executeCommandLine() to export the necessary functions
        // and call the factory interception hook
        if (is_typescript_bundle) {
            std.debug.print("[build] Patching TypeScript bundle for Native Shapes...\n", .{});
            const bak_path_ts = try std.fmt.allocPrint(allocator, "{s}.bak", .{bundle_js_path});
            defer allocator.free(bak_path_ts);
            _ = try runCommand(allocator, &.{
                "sed", "-i.bak",
                // Export ts namespace to globalThis, call factory interception, hook sys for I/O, then execute
                // The interception wraps globalThis.ts.createSourceFile, so we copy it back to the global variable
                // __edgebox_hook_tsc_sys modifies sys.write/readFile/etc. for proper stdout and file I/O
                "s/executeCommandLine(sys, noop, sys.args);/globalThis.ts = { createSourceFile: createSourceFile, factory: factory, forEachChild: forEachChild }; if (typeof __edgebox_intercept_tsc_factory === \"function\") { __edgebox_intercept_tsc_factory(); createSourceFile = globalThis.ts.createSourceFile; } if (typeof __edgebox_hook_tsc_sys === \"function\") { __edgebox_hook_tsc_sys(sys); } executeCommandLine(sys, noop, sys.args);/",
                bundle_js_path,
            });
            std.fs.cwd().deleteFile(bak_path_ts) catch {};
        }
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

    // Step 6: Freeze ORIGINAL bytecode (no manifest filtering - freeze all functions)
    const manifest_content: ?[]u8 = null;

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

        // Step 6b: Copy bundle unchanged (no JS hooks - native dispatch in Zig)
        try std.fs.cwd().copyFile(bundle_js_path, std.fs.cwd(), bundle_hooked_path, .{});
        // Manifest generated from bytecode analysis, not JS scanning
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

        const bytecode_content = bytecode_file.readToEndAlloc(allocator, 1024 * 1024 * 1024) catch |err| { // 1GB limit
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

        // Generate frozen module via LLVM IR backend
        zig_gen: {
            var sharded = freeze.generateModuleZigShardedWithBackend(allocator, &analysis, "frozen", manifest_content, options.freeze_max_functions, options.freeze_profile_path, cache_dir, options.freeze_code_budget) catch |err| {
                std.debug.print("[warn] LLVM codegen failed: {}\n", .{err});
                break :zig_gen;
            };
            defer sharded.deinit(allocator);

            // Clean up stale Zig shard files from previous runs (no longer generated)
            {
                var stale_buf: [4096]u8 = undefined;
                var stale_idx: usize = 0;
                while (true) : (stale_idx += 1) {
                    const stale_path = std.fmt.bufPrint(&stale_buf, "{s}/frozen_shard_{d}.zig", .{ cache_dir, stale_idx }) catch break;
                    std.fs.cwd().deleteFile(stale_path) catch break;
                }
            }

            // Write main frozen_module.zig (just extern declarations + init)
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

            std.debug.print("[build] Frozen module: {s} ({d} LLVM shards)\n", .{ zig_path, sharded.llvm_shard_count });
        }

        // Note: Closure-based functions fall back to interpreter (not frozen)
        // Native dispatch in Zig handles frozen functions directly

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

    // Step 6d: Generate raw bytecode for unified @embedFile flow
    // Use hooked bundle if freeze succeeded, otherwise use original bundle
    const runtime_bundle_path = if (freeze_success) bundle_hooked_path else bundle_js_path;
    std.debug.print("[build] Compiling JS to bytecode: {s} (freeze_success={})\n", .{ runtime_bundle_path, freeze_success });
    // Ensure output directory exists
    std.fs.cwd().makePath(output_dir) catch {};

    // Generate raw bytecode (unified flow - both WASM and native use @embedFile)
    var bundle_bin_path_buf: [4096]u8 = undefined;
    const bundle_bin_path = std.fmt.bufPrint(&bundle_bin_path_buf, "{s}/bundle.bin", .{cache_dir}) catch "zig-out/cache/bundle.bin";
    std.debug.print("[build] Generating raw bytecode for native...\n", .{});
    const exit_code_bin = try qjsc_wrapper.compileJsToBytecode(allocator, &.{
        "qjsc",
        "-b", // Raw bytecode output (no C wrapper, just bytes)
        "-s", "-s", // Strip source code AND debug info (index-based dispatch doesn't need line numbers)
        "-o", bundle_bin_path,
        runtime_bundle_path,
    });
    if (exit_code_bin != 0) {
        std.debug.print("[warn] Raw bytecode generation failed (native may not work)\n", .{});
    } else {
        if (std.fs.cwd().statFile(bundle_bin_path)) |stat| {
            const size_mb = @as(f64, @floatFromInt(stat.size)) / 1024.0 / 1024.0;
            std.debug.print("[build] Raw bytecode: {s} ({d:.1}MB)\n", .{ bundle_bin_path, size_mb });
        } else |_| {}
    }

    // No C file patching needed - bytecode embedded via @embedFile
    // Both native and wasm-static use the same unified flow

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

    // Construct bytecode path argument
    var bytecode_arg_buf: [4096]u8 = undefined;
    const bytecode_arg = std.fmt.bufPrint(&bytecode_arg_buf, "-Dbytecode={s}/bundle.bin", .{cache_dir}) catch {
        std.debug.print("[error] Bytecode path too long\n", .{});
        std.process.exit(1);
    };

    // Allocator type argument
    const allocator_arg = switch (options.allocator_type) {
        .arena => "-Dallocator=arena",
        .c => "-Dallocator=c",
        .gpa => "-Dallocator=gpa",
    };

    // Build prefix and cache-dir for isolated builds
    var zig_cache_path_buf: [4096]u8 = undefined;
    const zig_cache_path = std.fmt.bufPrint(&zig_cache_path_buf, "{s}/.zig-cache", .{out_prefix}) catch "zig-out/.zig-cache";
    var cache_prefix_arg_buf: [4096]u8 = undefined;
    const cache_prefix_arg = std.fmt.bufPrint(&cache_prefix_arg_buf, "-Dcache-prefix={s}/cache", .{out_prefix}) catch "-Dcache-prefix=zig-out/cache";

    const optimize_arg = if (options.debug_build) "-Doptimize=Debug" else "-Doptimize=ReleaseFast";
    const frozen_optimize_arg = "-Dfrozen-optimize=ReleaseFast";
    const frozen_thin_optimize_arg = "-Dfrozen-thin-optimize=ReleaseSmall";

    // Step 6e: Link standalone WASM (pure int32 functions, no QuickJS runtime)
    // Produces a tiny .wasm file for use in V8/workerd Workers
    var standalone_wasm_path_buf: [4096]u8 = undefined;
    var has_standalone_wasm = false;
    {
        var standalone_o_buf: [4096]u8 = undefined;
        const standalone_o = std.fmt.bufPrint(&standalone_o_buf, "{s}/standalone.o", .{cache_dir}) catch null;
        if (standalone_o) |so| {
            if (std.fs.cwd().access(so, .{})) |_| {
                const standalone_wasm = std.fmt.bufPrint(&standalone_wasm_path_buf, "{s}/{s}-standalone.wasm", .{ output_dir, output_base }) catch null;
                if (standalone_wasm) |sw| {
                    const link_result = runCommand(allocator, &.{
                        "wasm-ld-19", "--no-entry", "--export-all", "--export-memory",
                        "--initial-memory=131072", "-o", sw, so,
                    }) catch null;
                    if (link_result) |lr| {
                        defer {
                            if (lr.stdout) |s| allocator.free(s);
                            if (lr.stderr) |s| allocator.free(s);
                        }
                        const link_ok = switch (lr.term) {
                            .Exited => |code| code == 0,
                            else => false,
                        };
                        if (link_ok) {
                            has_standalone_wasm = true;
                            if (std.fs.cwd().statFile(sw)) |stat| {
                                std.debug.print("[build] Standalone WASM: {s} ({d} bytes)\n", .{ sw, stat.size });
                            } else |_| {}
                        } else {
                            std.debug.print("[warn] Standalone WASM linking failed\n", .{});
                        }
                    }
                }
            } else |_| {} // No standalone.o — no pure int32 functions
        }
    }

    // Step 6f: Generate workerd worker files (worker.mjs + config.capnp)
    // Uses the clean bundle (pre-polyfill) + standalone WASM
    var has_worker_files = false;
    if (has_standalone_wasm) {
        var worker_path_buf: [4096]u8 = undefined;
        var config_path_buf: [4096]u8 = undefined;
        const worker_path = std.fmt.bufPrint(&worker_path_buf, "{s}/{s}-worker.mjs", .{ output_dir, output_base }) catch null;
        const config_path = std.fmt.bufPrint(&config_path_buf, "{s}/{s}-config.capnp", .{ output_dir, output_base }) catch null;

        if (worker_path != null and config_path != null) {
            // Read standalone manifest to know which functions are in WASM
            var manifest_buf: [4096]u8 = undefined;
            const manifest_path = std.fmt.bufPrint(&manifest_buf, "{s}/standalone_manifest.json", .{cache_dir}) catch null;
            var wasm_manifest: ?[]const u8 = null;
            if (manifest_path) |mp| {
                wasm_manifest = std.fs.cwd().readFileAlloc(allocator, mp, 1024 * 1024) catch null;
            }
            defer if (wasm_manifest) |wm| allocator.free(wm);

            // Read clean bundle
            var clean_content: ?[]const u8 = null;
            if (clean_bundle_path) |cbp| {
                clean_content = std.fs.cwd().readFileAlloc(allocator, cbp, 50 * 1024 * 1024) catch null;
            }
            defer if (clean_content) |cc| allocator.free(cc);

            var wasm_fn_buf: [256]u8 = undefined;
            const wasm_filename = std.fmt.bufPrint(&wasm_fn_buf, "{s}-standalone.wasm", .{output_base}) catch "standalone.wasm";

            // Generate worker.mjs — source transform: replace WASM-compiled function
            // bodies with WASM call trampolines. This is a proper source-to-source
            // transform, not a runtime override hack.
            if (worker_path) |wp| {
                if (std.fs.cwd().createFile(wp, .{})) |wf| {
                    defer wf.close();

                    // Collect WASM function metadata from manifest
                    const WasmFunc = struct { name: []const u8, arg_count: u32, instr_count: u32, is_recursive: bool, array_args: u8, mutated_args: u8, length_args: u8, line_num: u32, is_anon: bool, is_f64: bool };
                    var wasm_funcs: [64]WasmFunc = undefined;
                    var wasm_func_count: usize = 0;
                    if (wasm_manifest) |mc| {
                        var pos: usize = 0;
                        while (std.mem.indexOfPos(u8, mc, pos, "\"name\":\"")) |name_start| {
                            const ns = name_start + 8;
                            if (std.mem.indexOfPos(u8, mc, ns, "\"")) |name_end| {
                                var ac: u32 = 1;
                                if (std.mem.indexOfPos(u8, mc, name_end, "\"args\":")) |as| {
                                    const ad = as + 7;
                                    if (ad < mc.len and mc[ad] >= '0' and mc[ad] <= '9') ac = mc[ad] - '0';
                                }
                                // Parse instruction count: "instrs":N
                                var ic: u32 = 0;
                                if (std.mem.indexOfPos(u8, mc, name_end, "\"instrs\":")) |is| {
                                    var id = is + 9;
                                    while (id < mc.len and mc[id] >= '0' and mc[id] <= '9') {
                                        ic = ic * 10 + (mc[id] - '0');
                                        id += 1;
                                    }
                                }
                                // Parse recursive flag: "recursive":1
                                var is_rec = false;
                                if (std.mem.indexOfPos(u8, mc, name_end, "\"recursive\":")) |rs| {
                                    const rd = rs + 12;
                                    if (rd < mc.len and mc[rd] == '1') is_rec = true;
                                }
                                // Parse array_args bitmask: "array_args":N
                                var aa: u8 = 0;
                                if (std.mem.indexOfPos(u8, mc, name_end, "\"array_args\":")) |aas| {
                                    var aad = aas + 13;
                                    var aa_val: u32 = 0;
                                    while (aad < mc.len and mc[aad] >= '0' and mc[aad] <= '9') {
                                        aa_val = aa_val * 10 + (mc[aad] - '0');
                                        aad += 1;
                                    }
                                    aa = @truncate(aa_val);
                                }
                                // Parse mutated_args bitmask: "mutated_args":N
                                var ma: u8 = aa; // Default: assume all array args mutated
                                if (std.mem.indexOfPos(u8, mc, name_end, "\"mutated_args\":")) |mas| {
                                    var mad = mas + 15;
                                    var ma_val: u32 = 0;
                                    while (mad < mc.len and mc[mad] >= '0' and mc[mad] <= '9') {
                                        ma_val = ma_val * 10 + (mc[mad] - '0');
                                        mad += 1;
                                    }
                                    ma = @truncate(ma_val);
                                }
                                // Parse length_args bitmask: "length_args":N
                                var la: u8 = 0;
                                if (std.mem.indexOfPos(u8, mc, name_end, "\"length_args\":")) |las| {
                                    var lad = las + 14;
                                    var la_val: u32 = 0;
                                    while (lad < mc.len and mc[lad] >= '0' and mc[lad] <= '9') {
                                        la_val = la_val * 10 + (mc[lad] - '0');
                                        lad += 1;
                                    }
                                    la = @truncate(la_val);
                                }
                                // Parse line number: "line":N
                                var ln: u32 = 0;
                                if (std.mem.indexOfPos(u8, mc, name_end, "\"line\":")) |ls| {
                                    var ld = ls + 7;
                                    while (ld < mc.len and mc[ld] >= '0' and mc[ld] <= '9') {
                                        ln = ln * 10 + (mc[ld] - '0');
                                        ld += 1;
                                    }
                                }
                                // Parse type: "type":"f64" or "type":"i32"
                                var is_f64 = false;
                                if (std.mem.indexOfPos(u8, mc, name_end, "\"type\":\"f64\"")) |ts| {
                                    // Only match if within this entry (before next "name")
                                    const next_name = std.mem.indexOfPos(u8, mc, name_end + 1, "\"name\":\"") orelse mc.len;
                                    if (ts < next_name) is_f64 = true;
                                }
                                // Detect anonymous functions (synthetic name starts with __anon_)
                                const is_anon = std.mem.startsWith(u8, mc[ns..name_end], "__anon_");
                                if (wasm_func_count < 64) {
                                    wasm_funcs[wasm_func_count] = .{ .name = mc[ns..name_end], .arg_count = ac, .instr_count = ic, .is_recursive = is_rec, .array_args = aa, .mutated_args = ma, .length_args = la, .line_num = ln, .is_anon = is_anon, .is_f64 = is_f64 };
                                    wasm_func_count += 1;
                                }
                                pos = name_end + 1;
                            } else break;
                        }
                    }

                    // Preamble: load WASM module (compatible with Node.js + workerd)
                    // Node.js: readFileSync + sync instantiation
                    // workerd: WASM binding from capnp config — import resolves to WebAssembly.Module
                    wf.writeAll("// EdgeBox AOT+JIT (auto-generated)\n") catch {};
                    wf.writeAll("// Source transform: function bodies replaced with WASM calls\n") catch {};
                    wf.writeAll("// V8 inlines these WASM calls into JS (zero overhead)\n\n") catch {};
                    wf.writeAll("import { readFileSync } from 'fs';\n") catch {};
                    wf.writeAll("import { fileURLToPath } from 'url';\n") catch {};
                    wf.writeAll("import { dirname, join } from 'path';\n") catch {};
                    wf.writeAll("import { createRequire } from 'module';\n") catch {};
                    wf.writeAll("const require = createRequire(import.meta.url);\n\n") catch {};
                    wf.writeAll("let __wasm;\n") catch {};
                    wf.writeAll("if (typeof process !== 'undefined' && process.versions?.node) {\n") catch {};
                    wf.writeAll("  const __dir = dirname(fileURLToPath(import.meta.url));\n") catch {};
                    wf.writeAll("  const buf = readFileSync(join(__dir, '") catch {};
                    wf.writeAll(wasm_filename) catch {};
                    wf.writeAll("'));\n") catch {};
                    wf.writeAll("  __wasm = new WebAssembly.Instance(new WebAssembly.Module(buf));\n") catch {};
                    wf.writeAll("  __wasm.exports.memory.grow(254);\n") catch {}; // 256 pages = 16MB
                    wf.writeAll("} else {\n") catch {};
                    // workerd: WASM module bound via capnp config as ESM module
                    wf.writeAll("  const { default: __wasmModule } = await import('") catch {};
                    wf.writeAll(wasm_filename) catch {};
                    wf.writeAll("');\n") catch {};
                    wf.writeAll("  __wasm = new WebAssembly.Instance(__wasmModule);\n") catch {};
                    wf.writeAll("  __wasm.exports.memory.grow(254);\n") catch {};
                    wf.writeAll("}\n") catch {};
                    // Hoist Int32Array view creation to module level (avoids per-call allocation).
                    // Array elements are always i32 in WASM linear memory regardless of function tier.
                    {
                        var has_array_funcs = false;
                        for (wasm_funcs[0..wasm_func_count]) |wfe| {
                            if (wfe.array_args != 0) { has_array_funcs = true; break; }
                        }
                        if (has_array_funcs) {
                            wf.writeAll("const __m = __wasm ? new Int32Array(__wasm.exports.memory.buffer) : null;\n") catch {};
                            // Per-arg identity cache: skip copy when same array reference is passed.
                            // Cache variables: __cN = last array ref for arg N.
                            {
                                var any_cacheable = false;
                                for (wasm_funcs[0..wasm_func_count]) |wfe| {
                                    const read_only = wfe.array_args & ~wfe.mutated_args;
                                    if (read_only != 0) { any_cacheable = true; break; }
                                }
                                if (any_cacheable) {
                                    for (wasm_funcs[0..wasm_func_count], 0..) |wfe, wfi| {
                                        const read_only = wfe.array_args & ~wfe.mutated_args;
                                        if (read_only == 0) continue;
                                        var ai: u32 = 0;
                                        while (ai < 8) : (ai += 1) {
                                            if (read_only & (@as(u8, 1) << @intCast(ai)) != 0) {
                                                var decl_buf: [80]u8 = undefined;
                                                const decl = std.fmt.bufPrint(&decl_buf, "let __c{d}_{d}=null;\n", .{ wfi, ai }) catch continue;
                                                wf.writeAll(decl) catch {};
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    wf.writeAll("\n") catch {};

                    // Source transform: copy original source, but replace function bodies
                    // for WASM-compiled functions with WASM call trampolines.
                    // e.g. `function fib(n) { ... }` → `function fib(n) { return __wasm.exports.fib(n); }`
                    // Also handles arrow functions: `var adler32 = (a, b) => { ... }` → trampoline
                    if (clean_content) |cc| {
                        // Compute preamble line offset: hooked bundle has polyfills prepended.
                        // Module parser line numbers are based on hooked bundle.
                        // Find the first line of clean content in hooked bundle to get offset.
                        var preamble_lines: u32 = 0;
                        {
                            var hooked_path_buf2: [4096]u8 = undefined;
                            const hooked_path2 = std.fmt.bufPrint(&hooked_path_buf2, "{s}/bundle_hooked.js", .{cache_dir}) catch null;
                            if (hooked_path2) |hp| {
                                if (std.fs.cwd().readFileAlloc(allocator, hp, 50 * 1024 * 1024)) |hc| {
                                    defer allocator.free(hc);
                                    // Find where clean content starts in hooked (match first 80 chars)
                                    const match_len = @min(cc.len, 80);
                                    if (match_len > 0) {
                                        if (std.mem.indexOf(u8, hc, cc[0..match_len])) |start_offset| {
                                            // Count newlines before start_offset
                                            for (hc[0..start_offset]) |ch| {
                                                if (ch == '\n') preamble_lines += 1;
                                            }
                                        }
                                    }
                                } else |_| {}
                            }
                        }

                        // Build line→offset table for anonymous function matching
                        var line_offsets: [8192]u32 = undefined;
                        var line_count: u32 = 1;
                        line_offsets[0] = 0; // line 1 starts at offset 0
                        for (cc, 0..) |ch, ci| {
                            if (ch == '\n' and line_count < line_offsets.len) {
                                line_offsets[line_count] = @intCast(ci + 1);
                                line_count += 1;
                            }
                        }

                        // Pre-compute arrow function replacement positions
                        // For each anonymous WASM function, find `=> {` on its line
                        // Adjust line_num by preamble offset (hooked → clean)
                        const ArrowReplace = struct { brace_pos: u32, func_idx: u16 };
                        var arrow_replacements: [64]ArrowReplace = undefined;
                        var arrow_count: usize = 0;
                        for (wasm_funcs[0..wasm_func_count], 0..) |wf_entry, wfi| {
                            if (!wf_entry.is_anon) continue;
                            const adjusted_line = if (wf_entry.line_num > preamble_lines) wf_entry.line_num - preamble_lines else wf_entry.line_num;
                            if (adjusted_line == 0 or adjusted_line >= line_count) continue;
                            const line_start = line_offsets[adjusted_line - 1]; // 1-based
                            const line_end = if (adjusted_line < line_count) line_offsets[adjusted_line] else @as(u32, @intCast(cc.len));
                            // Find `=> {` on this line
                            if (std.mem.indexOf(u8, cc[line_start..line_end], "=> {")) |arrow_off| {
                                const brace_pos = line_start + @as(u32, @intCast(arrow_off)) + 3; // position of `{`
                                if (arrow_count < arrow_replacements.len) {
                                    arrow_replacements[arrow_count] = .{ .brace_pos = brace_pos, .func_idx = @intCast(wfi) };
                                    arrow_count += 1;
                                }
                            }
                        }

                        var src_pos: usize = 0;
                        while (src_pos < cc.len) {
                            // Try to match `function NAME(` for each WASM function
                            var matched_func: ?WasmFunc = null;
                            var matched_func_idx: usize = 0;
                            var match_end: usize = 0;
                            var is_arrow_match = false;
                            for (wasm_funcs[0..wasm_func_count], 0..) |wf_entry, wfi| {
                                if (wf_entry.is_anon) continue; // handled separately
                                // Build search pattern: "function NAME("
                                if (src_pos + 9 + wf_entry.name.len + 1 > cc.len) continue;
                                if (!std.mem.startsWith(u8, cc[src_pos..], "function ")) continue;
                                const after_kw = src_pos + 9; // len("function ")
                                if (!std.mem.startsWith(u8, cc[after_kw..], wf_entry.name)) continue;
                                const after_name = after_kw + wf_entry.name.len;
                                if (after_name >= cc.len or cc[after_name] != '(') continue;
                                // Verify it's a declaration (not part of a longer identifier)
                                if (src_pos > 0 and isIdentChar(cc[src_pos - 1])) continue;
                                matched_func = wf_entry;
                                matched_func_idx = wfi;
                                match_end = after_name;
                                break;
                            }

                            // Check for arrow function replacement at this position
                            if (matched_func == null) {
                                for (arrow_replacements[0..arrow_count]) |ar| {
                                    if (ar.brace_pos == @as(u32, @intCast(src_pos))) {
                                        matched_func = wasm_funcs[ar.func_idx];
                                        matched_func_idx = ar.func_idx;
                                        is_arrow_match = true;
                                        // For arrows, match_end points to just before '(' in the params
                                        // We need to find the '(' before ') => {'
                                        // Scan backwards from brace_pos to find params
                                        var scan_back = src_pos; // at '{'
                                        if (scan_back >= 4) scan_back -= 4; // skip ' => '
                                        // Now find the matching '('
                                        while (scan_back > 0 and cc[scan_back] != '(') scan_back -= 1;
                                        match_end = scan_back; // points to '('
                                        break;
                                    }
                                }
                            }

                            if (matched_func) |mf| {
                                // Smart trampoline heuristic:
                                // - Recursive functions: ALWAYS trampoline (deep stacks stay in WASM)
                                // - Cross-callers: ALWAYS trampoline (calls stay in WASM)
                                // - Array functions with heavy computation (>200 instrs): trampoline
                                //   (WASM compute amortizes copy cost)
                                // - Small array/scalar functions (<200 instrs): keep as JS
                                //   (V8 JIT inlines these perfectly; copy overhead dominates)
                                if (!mf.is_recursive) {
                                    // Check if body calls another WASM function (cross-call)
                                    var calls_wasm = false;
                                    if (std.mem.indexOfPos(u8, cc, match_end, "{")) |bp| {
                                        var depth_scan: i32 = 1;
                                        var sp = bp + 1;
                                        while (sp < cc.len and depth_scan > 0) : (sp += 1) {
                                            if (cc[sp] == '{') depth_scan += 1;
                                            if (cc[sp] == '}') depth_scan -= 1;
                                        }
                                        const body = cc[bp + 1 .. @min(sp, cc.len)];
                                        for (wasm_funcs[0..wasm_func_count]) |other| {
                                            if (std.mem.eql(u8, other.name, mf.name)) continue;
                                            if (std.mem.indexOf(u8, body, other.name)) |_| {
                                                calls_wasm = true;
                                                break;
                                            }
                                        }
                                    }
                                    // Keep as JS when trampoline overhead exceeds WASM benefit:
                                    // 1. Unrolled fixed-size array ops (no .length, >= 100 instrs):
                                    //    .set() copy cost dominates. Many instructions = unrolled
                                    //    (vec_add16: 338, salsa20_qr: 803, core_salsa20: 2386).
                                    // 2. Small scalar functions (<150 instrs): V8 JIT already optimal.
                                    // Functions with .length OR compact array loops (<100 instrs) are
                                    // trampolined: computation scales with array size.
                                    // adler32 (68 instrs, no .length) has a loop bounded by scalar len.
                                    const is_unrolled_array = mf.array_args != 0 and mf.length_args == 0 and mf.instr_count >= 100;
                                    if (!calls_wasm and (is_unrolled_array or (mf.array_args == 0 and mf.instr_count < 150))) {
                                        wf.writeAll(cc[src_pos .. src_pos + 1]) catch {};
                                        src_pos += 1;
                                        continue;
                                    }
                                }

                                // Found a WASM function declaration. Find opening brace.
                                const brace_pos = if (is_arrow_match)
                                    src_pos // for arrow functions, src_pos IS the brace
                                else
                                    std.mem.indexOfPos(u8, cc, match_end, "{") orelse {
                                        // No brace found — write char and advance
                                        wf.writeAll(cc[src_pos .. src_pos + 1]) catch {};
                                        src_pos += 1;
                                        continue;
                                    };

                                // Write everything up to and including the opening brace
                                wf.writeAll(cc[src_pos .. brace_pos + 1]) catch {};

                                // Skip the original function body by brace-matching
                                var depth: i32 = 1;
                                var scan = brace_pos + 1;
                                while (scan < cc.len and depth > 0) {
                                    const ch = cc[scan];
                                    if (ch == '\'' or ch == '"' or ch == '`') {
                                        // Skip string literals
                                        scan += 1;
                                        while (scan < cc.len) {
                                            if (cc[scan] == '\\') {
                                                scan += 2; // skip escape
                                                continue;
                                            }
                                            if (cc[scan] == ch) {
                                                scan += 1;
                                                break;
                                            }
                                            scan += 1;
                                        }
                                        continue;
                                    }
                                    if (ch == '/' and scan + 1 < cc.len) {
                                        if (cc[scan + 1] == '/') {
                                            // Skip line comment
                                            while (scan < cc.len and cc[scan] != '\n') scan += 1;
                                            continue;
                                        }
                                        if (cc[scan + 1] == '*') {
                                            // Skip block comment
                                            scan += 2;
                                            while (scan + 1 < cc.len) {
                                                if (cc[scan] == '*' and cc[scan + 1] == '/') {
                                                    scan += 2;
                                                    break;
                                                }
                                                scan += 1;
                                            }
                                            continue;
                                        }
                                    }
                                    if (ch == '{') depth += 1;
                                    if (ch == '}') depth -= 1;
                                    scan += 1;
                                }

                                // Parse param names from original source
                                const params_start = match_end + 1; // after '('
                                const params_end = std.mem.indexOfPos(u8, cc, params_start, ")") orelse params_start;
                                const params_str = cc[params_start..params_end];

                                // Split param names for individual access
                                var param_names: [8][]const u8 = undefined;
                                var param_count: u32 = 0;
                                {
                                    var ppos: usize = 0;
                                    while (ppos < params_str.len and param_count < 8) {
                                        // Skip whitespace
                                        while (ppos < params_str.len and (params_str[ppos] == ' ' or params_str[ppos] == '\t')) ppos += 1;
                                        const pstart = ppos;
                                        while (ppos < params_str.len and params_str[ppos] != ',' and params_str[ppos] != ' ') ppos += 1;
                                        if (ppos > pstart) {
                                            param_names[param_count] = params_str[pstart..ppos];
                                            param_count += 1;
                                        }
                                        // Skip comma
                                        if (ppos < params_str.len and params_str[ppos] == ',') ppos += 1;
                                    }
                                }

                                if (mf.array_args == 0) {
                                    // No array args — simple scalar trampoline
                                    wf.writeAll(" return __wasm.exports.") catch {};
                                    wf.writeAll(mf.name) catch {};
                                    wf.writeAll("(") catch {};
                                    wf.writeAll(params_str) catch {};
                                    wf.writeAll("); }") catch {};
                                } else {
                                    // Array args — copy to/from WASM linear memory
                                    wf.writeAll("\n") catch {};
                                    // Array elements are always i32 in WASM linear memory
                                    // (array_get/array_put use i32.load/i32.store regardless of tier).
                                    // The f64 tier only affects scalar locals/args/return, not array elements.
                                    const mem_view: []const u8 = "__m";
                                    const byte_shift: []const u8 = " << 2";

                                    // Dynamic offsets: each array arg gets space = arr.length elements.
                                    // Uses bulk .set() for fast TypedArray transfer (memcpy under the hood).
                                    // Read-only args cached by reference identity (skip copy when same array).
                                    const read_only_args = mf.array_args & ~mf.mutated_args;
                                    wf.writeAll("  let __off = 0;\n") catch {};
                                    var i: u32 = 0;
                                    while (i < param_count) : (i += 1) {
                                        if (mf.array_args & (@as(u8, 1) << @intCast(i)) != 0) {
                                            var idx_buf: [8]u8 = undefined;
                                            const idx_str = std.fmt.bufPrint(&idx_buf, "{d}", .{i}) catch "0";
                                            // Emit: const __bN = __off;
                                            wf.writeAll("  const __b") catch {};
                                            wf.writeAll(idx_str) catch {};
                                            wf.writeAll(" = __off;\n") catch {};

                                            if (read_only_args & (@as(u8, 1) << @intCast(i)) != 0) {
                                                // Read-only: cache by reference identity, but ONLY for
                                                // large arrays (>1024 elements). Small arrays always copy
                                                // because .set() is cheap (<30ns for 32 elem) and caching
                                                // small arrays risks stale data when JS mutates elements
                                                // between calls (same ref, different data — e.g. tweetnacl vn).
                                                var cid_buf: [32]u8 = undefined;
                                                const cid = std.fmt.bufPrint(&cid_buf, "{d}_{d}", .{ matched_func_idx, i }) catch "0_0";
                                                const pn = param_names[i];
                                                // if (arr.length > 1024 && arr === __cN) skip; else copy
                                                wf.writeAll("  if (") catch {};
                                                wf.writeAll(pn) catch {};
                                                wf.writeAll(".length <= 1024 || ") catch {};
                                                wf.writeAll(pn) catch {};
                                                wf.writeAll(" !== __c") catch {};
                                                wf.writeAll(cid) catch {};
                                                wf.writeAll(") { ") catch {};
                                                wf.writeAll(mem_view) catch {};
                                                wf.writeAll(".set(") catch {};
                                                wf.writeAll(pn) catch {};
                                                wf.writeAll(", __off); __c") catch {};
                                                wf.writeAll(cid) catch {};
                                                wf.writeAll(" = ") catch {};
                                                wf.writeAll(pn) catch {};
                                                wf.writeAll("; }\n") catch {};
                                            } else {
                                                // Mutated: always copy
                                                wf.writeAll("  ") catch {};
                                                wf.writeAll(mem_view) catch {};
                                                wf.writeAll(".set(") catch {};
                                                wf.writeAll(param_names[i]) catch {};
                                                wf.writeAll(", __off);\n") catch {};
                                            }

                                            // Advance offset past this array
                                            wf.writeAll("  __off += ") catch {};
                                            wf.writeAll(param_names[i]) catch {};
                                            wf.writeAll(".length;\n") catch {};
                                        }
                                    }
                                    // Call WASM with byte offsets (__bN << 2) for array args, scalars as-is
                                    wf.writeAll("  const __r = __wasm.exports.") catch {};
                                    wf.writeAll(mf.name) catch {};
                                    wf.writeAll("(") catch {};
                                    i = 0;
                                    while (i < param_count) : (i += 1) {
                                        if (i > 0) wf.writeAll(", ") catch {};
                                        if (mf.array_args & (@as(u8, 1) << @intCast(i)) != 0) {
                                            var idx_buf2: [8]u8 = undefined;
                                            const idx_str2 = std.fmt.bufPrint(&idx_buf2, "{d}", .{i}) catch "0";
                                            wf.writeAll("__b") catch {};
                                            wf.writeAll(idx_str2) catch {};
                                            wf.writeAll(byte_shift) catch {};
                                        } else {
                                            wf.writeAll(param_names[i]) catch {};
                                        }
                                    }
                                    // Append .length for array args that use get_length
                                    i = 0;
                                    while (i < param_count) : (i += 1) {
                                        if (mf.length_args & (@as(u8, 1) << @intCast(i)) != 0) {
                                            wf.writeAll(", ") catch {};
                                            wf.writeAll(param_names[i]) catch {};
                                            wf.writeAll(".length") catch {};
                                        }
                                    }
                                    wf.writeAll(");\n") catch {};
                                    // Copy modified arrays back from WASM memory (only mutated ones)
                                    i = 0;
                                    while (i < param_count) : (i += 1) {
                                        if (mf.mutated_args & (@as(u8, 1) << @intCast(i)) != 0) {
                                            var idx_buf3: [8]u8 = undefined;
                                            const idx_str3 = std.fmt.bufPrint(&idx_buf3, "{d}", .{i}) catch "0";
                                            wf.writeAll("  for (let __i = 0; __i < ") catch {};
                                            wf.writeAll(param_names[i]) catch {};
                                            wf.writeAll(".length; __i++) ") catch {};
                                            wf.writeAll(param_names[i]) catch {};
                                            wf.writeAll("[__i] = ") catch {};
                                            wf.writeAll(mem_view) catch {};
                                            wf.writeAll("[__b") catch {};
                                            wf.writeAll(idx_str3) catch {};
                                            wf.writeAll(" + __i];\n") catch {};
                                        }
                                    }
                                    // Invalidate all caches when mutation happens (cross-function safety)
                                    if (mf.mutated_args != 0 and read_only_args != 0) {
                                        // This function has both mutated and cached args — invalidate caches
                                        var ai: u32 = 0;
                                        while (ai < param_count) : (ai += 1) {
                                            if (read_only_args & (@as(u8, 1) << @intCast(ai)) != 0) {
                                                var cid_buf2: [32]u8 = undefined;
                                                const cid2 = std.fmt.bufPrint(&cid_buf2, "{d}_{d}", .{ matched_func_idx, ai }) catch "0_0";
                                                wf.writeAll("  __c") catch {};
                                                wf.writeAll(cid2) catch {};
                                                wf.writeAll(" = null;\n") catch {};
                                            }
                                        }
                                    }
                                    wf.writeAll("  return __r;\n}") catch {};
                                }

                                // src_pos = scan (past the closing brace we already consumed)
                                src_pos = scan;
                            } else {
                                // No match — copy one character
                                wf.writeAll(cc[src_pos .. src_pos + 1]) catch {};
                                src_pos += 1;
                            }
                        }
                    } else {
                        wf.writeAll("// ERROR: Original source not available\n") catch {};
                    }

                    has_worker_files = true;
                    std.debug.print("[build] Worker: {s}\n", .{wp});
                } else |_| {}
            }

            // Generate config.capnp
            if (config_path) |cp| {
                if (std.fs.cwd().createFile(cp, .{})) |cf| {
                    defer cf.close();
                    var wn_buf: [256]u8 = undefined;
                    const wn = std.fmt.bufPrint(&wn_buf, "{s}-worker.mjs", .{output_base}) catch "worker.mjs";
                    cf.writeAll("using Workerd = import \"/workerd/workerd.capnp\";\n\n") catch {};
                    cf.writeAll("const config :Workerd.Config = (\n") catch {};
                    cf.writeAll("  services = [\n") catch {};
                    cf.writeAll("    (name = \"main\", worker = .worker),\n") catch {};
                    cf.writeAll("  ],\n") catch {};
                    cf.writeAll("  sockets = [\n") catch {};
                    cf.writeAll("    (name = \"http\", address = \"*:8787\", http = (), service = \"main\"),\n") catch {};
                    cf.writeAll("  ],\n") catch {};
                    cf.writeAll(");\n\n") catch {};
                    cf.writeAll("const worker :Workerd.Worker = (\n") catch {};
                    cf.writeAll("  modules = [\n") catch {};
                    cf.writeAll("    (name = \"entrypoint\", esModule = embed \"") catch {};
                    cf.writeAll(wn) catch {};
                    cf.writeAll("\"),\n") catch {};
                    cf.writeAll("    (name = \"") catch {};
                    cf.writeAll(wasm_filename) catch {};
                    cf.writeAll("\", wasm = embed \"") catch {};
                    cf.writeAll(wasm_filename) catch {};
                    cf.writeAll("\"),\n") catch {};
                    cf.writeAll("  ],\n") catch {};
                    cf.writeAll("  compatibilityDate = \"2024-09-23\",\n") catch {};
                    cf.writeAll("  compatibilityFlags = [\"nodejs_compat\"],\n") catch {};
                    cf.writeAll(");\n") catch {};

                    std.debug.print("[build] Config: {s}\n", .{cp});
                } else |_| {}
            }
        }
    }

    // Step 7: Build WASM static (with host imports for edgebox daemon AOT)
    if (!options.binary_only) {
        std.debug.print("[build] Building WASM static with embedded bytecode...\n", .{});
        const wasm_result = if (source_dir_arg.len > 0)
            try runCommand(allocator, &.{
                "zig", "build", "-j4", "--prefix", out_prefix, "--cache-dir", zig_cache_path, "wasm-static", "-Doptimize=ReleaseSmall", source_dir_arg, bytecode_arg, cache_prefix_arg,
            })
        else
            try runCommand(allocator, &.{
                "zig", "build", "-j4", "--prefix", out_prefix, "--cache-dir", zig_cache_path, "wasm-static", "-Doptimize=ReleaseSmall", bytecode_arg, cache_prefix_arg,
            });
        defer {
            if (wasm_result.stdout) |s| allocator.free(s);
            if (wasm_result.stderr) |s| allocator.free(s);
        }

        const wasm_failed = switch (wasm_result.term) {
            .Exited => |code| code != 0,
            .Signal => true,
            .Stopped, .Unknown => true,
        };
        if (wasm_failed) {
            std.debug.print("[warn] WASM static build failed\n", .{});
            if (wasm_result.stderr) |err| {
                std.debug.print("{s}\n", .{err});
            }
        } else {
            // Copy from build output (wasm-static produces edgebox-static.wasm)
            var wasm_static_path_buf: [4096]u8 = undefined;
            const wasm_static_path = std.fmt.bufPrint(&wasm_static_path_buf, "{s}/bin/edgebox-static.wasm", .{out_prefix}) catch "zig-out/bin/edgebox-static.wasm";
            std.fs.cwd().copyFile(wasm_static_path, std.fs.cwd(), wasm_path, .{}) catch |err| {
                std.debug.print("[warn] Failed to copy WASM: {}\n", .{err});
            };
            if (std.fs.cwd().statFile(wasm_path)) |stat| {
                const size_mb = @as(f64, @floatFromInt(stat.size)) / 1024.0 / 1024.0;
                std.debug.print("[build] WASM: {s} ({d:.1}MB)\n", .{ wasm_path, size_mb });
            } else |_| {}
        }
    }

    // Generate binary path (no extension) — needed for summary even if skipped
    var binary_path_buf: [4096]u8 = undefined;
    const binary_path = std.fmt.bufPrint(&binary_path_buf, "{s}/{s}", .{ output_dir, output_base }) catch {
        std.debug.print("[error] Output path too long: {s}/{s}\n", .{ output_dir, output_base });
        std.process.exit(1);
    };

    // Step 7b: Build native binary using native (raw bytecode via @embedFile)
    // This avoids OOM from parsing 321MB C hex arrays by embedding bytecode directly
    // Uses the SAME frozen_module.zig as WASM, but embeds bytecode via linker
    if (!options.wasm_only) {
        std.debug.print("[build] Building native binary with embedded bytecode (native)...\n", .{});
        const native_result = if (source_dir_arg.len > 0)
            try runCommand(allocator, &.{
                "zig", "build", "-j4", "--prefix", out_prefix, "--cache-dir", zig_cache_path, "native", optimize_arg, frozen_optimize_arg, frozen_thin_optimize_arg, source_dir_arg, bytecode_arg, allocator_arg, cache_prefix_arg,
            })
        else
            try runCommand(allocator, &.{
                "zig", "build", "-j4", "--prefix", out_prefix, "--cache-dir", zig_cache_path, "native", optimize_arg, frozen_optimize_arg, frozen_thin_optimize_arg, bytecode_arg, allocator_arg, cache_prefix_arg,
            });
        defer {
            if (native_result.stdout) |s| allocator.free(s);
            if (native_result.stderr) |s| allocator.free(s);
        }

        const native_failed = switch (native_result.term) {
            .Exited => |code| code != 0,
            .Signal => true,
            .Stopped, .Unknown => true,
        };
        if (native_failed) {
            std.debug.print("[warn] Native-embed build failed (WASM/AOT still usable)\n", .{});
            if (native_result.stderr) |err| {
                std.debug.print("{s}\n", .{err});
            }
        } else {
            // Copy from build output with output name based on input
            // build.zig derives output name from source_dir basename (e.g., _tsc.js -> _tsc)
            var native_path_buf: [4096]u8 = undefined;
            const native_path = std.fmt.bufPrint(&native_path_buf, "{s}/bin/{s}", .{ out_prefix, output_base }) catch "zig-out/bin/edgebox-native";
            std.fs.cwd().copyFile(native_path, std.fs.cwd(), binary_path, .{}) catch |err| {
                std.debug.print("[warn] Failed to copy binary: {}\n", .{err});
            };
            if (std.fs.cwd().statFile(binary_path)) |stat| {
                const size_mb = @as(f64, @floatFromInt(stat.size)) / 1024.0 / 1024.0;
                std.debug.print("[build] Binary: {s} ({d:.1}MB)\n", .{ binary_path, size_mb });
            } else |_| {}
        }
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

        if (options.wasm_only) {
            // Summary for wasm-only build
            std.debug.print("\n[build] === WASM Build Complete ===\n\n", .{});
            std.debug.print("Files created:\n", .{});
            std.debug.print("  {s}   - WASM with embedded bytecode\n", .{wasm_path});
            std.debug.print("  {s}  - AOT native module\n", .{aot_path});
            if (has_standalone_wasm) {
                const sw_path = std.fmt.bufPrint(&standalone_wasm_path_buf, "{s}/{s}-standalone.wasm", .{ output_dir, output_base }) catch "";
                std.debug.print("  {s}  - Standalone WASM (pure functions, for workerd/V8)\n", .{sw_path});
            }
            if (has_worker_files) {
                var wp_buf: [4096]u8 = undefined;
                var cp_buf2: [4096]u8 = undefined;
                const wp = std.fmt.bufPrint(&wp_buf, "{s}/{s}-worker.mjs", .{ output_dir, output_base }) catch "";
                const cp = std.fmt.bufPrint(&cp_buf2, "{s}/{s}-config.capnp", .{ output_dir, output_base }) catch "";
                std.debug.print("  {s}  - workerd Worker module\n", .{wp});
                std.debug.print("  {s}  - workerd config\n", .{cp});
            }
            std.debug.print("\n", .{});
            std.debug.print("To run:\n", .{});
            std.debug.print("  edgebox {s}   # WASM (sandboxed)\n", .{wasm_path});
            std.debug.print("  edgebox {s}  # AOT (sandboxed, fast)\n", .{aot_path});
            if (has_worker_files) {
                var cp_buf3: [4096]u8 = undefined;
                const cp2 = std.fmt.bufPrint(&cp_buf3, "{s}/{s}-config.capnp", .{ output_dir, output_base }) catch "";
                std.debug.print("  npx workerd serve {s}  # workerd (V8 JIT)\n", .{cp2});
            }
            std.debug.print("\n", .{});
        } else {
            // Summary for full build
            std.debug.print("\n[build] === Static Build Complete ===\n\n", .{});
            std.debug.print("Files created:\n", .{});
            std.debug.print("  {s}     - Native binary (QuickJS + frozen)\n", .{binary_path});
            std.debug.print("  {s}   - WASM with embedded bytecode\n", .{wasm_path});
            std.debug.print("  {s}  - AOT native module\n", .{aot_path});
            if (has_standalone_wasm) {
                const sw_path = std.fmt.bufPrint(&standalone_wasm_path_buf, "{s}/{s}-standalone.wasm", .{ output_dir, output_base }) catch "";
                std.debug.print("  {s}  - Standalone WASM (pure functions, for workerd/V8)\n", .{sw_path});
            }
            std.debug.print("\n", .{});
            std.debug.print("To run:\n", .{});
            std.debug.print("  ./{s}            # Binary (fastest startup)\n", .{binary_path});
            std.debug.print("  edgebox {s}   # WASM (sandboxed)\n", .{wasm_path});
            std.debug.print("  edgebox {s}  # AOT (sandboxed, fast)\n\n", .{aot_path});
        }
    } else {
        // Summary for binary-only build
        std.debug.print("\n[build] === Build Complete ===\n\n", .{});
        std.debug.print("Files created:\n", .{});
        if (has_worker_files) {
            var wp_buf4: [4096]u8 = undefined;
            const wp2 = std.fmt.bufPrint(&wp_buf4, "{s}/{s}-worker.mjs", .{ output_dir, output_base }) catch "";
            std.debug.print("  {s}  - Worker module (V8 + WASM)\n", .{wp2});
        }
        if (has_standalone_wasm) {
            const sw_path = std.fmt.bufPrint(&standalone_wasm_path_buf, "{s}/{s}-standalone.wasm", .{ output_dir, output_base }) catch "";
            std.debug.print("  {s}  - Standalone WASM (AOT numeric kernels)\n", .{sw_path});
        }
        std.debug.print("  {s}  - Native binary (QuickJS + frozen)\n", .{binary_path});
        std.debug.print("\nTo run:\n", .{});
        if (has_worker_files) {
            var wp_buf5: [4096]u8 = undefined;
            const wp3 = std.fmt.bufPrint(&wp_buf5, "{s}/{s}-worker.mjs", .{ output_dir, output_base }) catch "";
            std.debug.print("  node {s}  # V8 + WASM (fastest)\n", .{wp3});
        }
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

    // Wrap user code in IIFE so functions become closure functions and get frozen.
    // Without this, top-level functions run through the QuickJS interpreter (slow).
    try file.writeAll("(function() {\n");

    // Write bundle, skipping shebang lines (e.g., #!/usr/bin/env node)
    // These cause qjsc to fail with "invalid first character of private name"
    try writeBundleWithoutShebangs(file, bundle);

    // Close the IIFE
    try file.writeAll("\n})();\n");
}

/// Prepend runtime module without IIFE wrapping
/// Used for runtime/ modules that set up globals at top level
fn prependRuntimeModule(allocator: std.mem.Allocator, module_path: []const u8, bundle_path: []const u8) !void {
    // Read module
    const module_content = try std.fs.cwd().readFileAlloc(allocator, module_path, 1024 * 1024);
    defer allocator.free(module_content);

    // Read existing bundle
    const bundle = try std.fs.cwd().readFileAlloc(allocator, bundle_path, 50 * 1024 * 1024);
    defer allocator.free(bundle);

    // Write combined (no IIFE wrapping for runtime modules)
    const file = try std.fs.cwd().createFile(bundle_path, .{});
    defer file.close();

    try file.writeAll(module_content);
    try file.writeAll("\n");
    try file.writeAll(bundle);
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

