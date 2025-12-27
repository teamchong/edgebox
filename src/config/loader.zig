/// Configuration loader for EdgeBox
///
/// Loads configuration from:
/// 1. CLI flags (highest priority)
/// 2. Environment variables (EDGEBOX_*)
/// 3. .env file
/// 4. .edgebox.json
/// 5. Defaults (lowest priority)
const std = @import("std");
const types = @import("types.zig");
const path_utils = @import("path.zig");
const env_utils = @import("env.zig");

pub const Config = types.Config;
pub const DirPerms = types.DirPerms;
pub const Mount = types.Mount;
pub const RuntimeConfig = types.RuntimeConfig;

/// Load options for configuration
pub const LoadOptions = struct {
    /// Search paths for .edgebox.json (in order)
    search_paths: []const []const u8 = &.{ ".", ".." },
    /// Also check directory of wasm_path
    wasm_path: ?[]const u8 = null,
    /// Load .env file
    load_env: bool = true,
    /// Which config sections to parse
    sections: ConfigSections = .all,
};

pub const ConfigSections = enum {
    all,
    runtime_only, // For edgebox
    daemon_only, // For edgeboxd
    build_only, // For edgeboxc
};

/// Load configuration with precedence handling
pub fn load(alloc: std.mem.Allocator, options: LoadOptions) !Config {
    var config = Config{};
    errdefer config.deinit(alloc);

    // Load .env file first
    if (options.load_env) {
        env_utils.loadEnvFile(alloc, &config.env_vars);
    }

    // Try to find and load .edgebox.json
    const config_path = findConfigFile(options) orelse return config;

    const config_file = std.fs.cwd().openFile(config_path, .{}) catch return config;
    defer config_file.close();

    const config_size = config_file.getEndPos() catch return config;
    if (config_size > 1024 * 1024) return config; // Max 1MB config

    const config_buf = try alloc.alloc(u8, config_size);
    defer alloc.free(config_buf);

    _ = config_file.readAll(config_buf) catch return config;

    // Parse JSON
    const parsed = std.json.parseFromSlice(std.json.Value, alloc, config_buf, .{}) catch return config;
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return config;

    // Parse sections based on options
    try parseConfig(alloc, &config, root.object, options.sections);

    return config;
}

/// Find .edgebox.json in search paths
fn findConfigFile(options: LoadOptions) ?[]const u8 {
    // Check wasm_path directory first
    if (options.wasm_path) |wasm_path| {
        if (std.fs.path.dirname(wasm_path)) |dir| {
            var buf: [4096]u8 = undefined;
            const config_path = std.fmt.bufPrint(&buf, "{s}/.edgebox.json", .{dir}) catch null;
            if (config_path) |p| {
                if (std.fs.cwd().access(p, .{})) |_| {
                    return ".edgebox.json"; // Fall back to cwd
                } else |_| {}
            }
        }
    }

    // Check search paths
    for (options.search_paths) |search_path| {
        var buf: [4096]u8 = undefined;
        const config_path = std.fmt.bufPrint(&buf, "{s}/.edgebox.json", .{search_path}) catch continue;
        if (std.fs.cwd().access(config_path, .{})) |_| {
            if (std.mem.eql(u8, search_path, ".")) {
                return ".edgebox.json";
            }
            // For other paths, we'd need to allocate - just use cwd for now
            return ".edgebox.json";
        } else |_| {}
    }

    // Default to current directory
    if (std.fs.cwd().access(".edgebox.json", .{})) |_| {
        return ".edgebox.json";
    } else |_| {}

    return null;
}

/// Parse configuration from JSON object
fn parseConfig(
    alloc: std.mem.Allocator,
    config: *Config,
    root: std.json.ObjectMap,
    sections: ConfigSections,
) !void {
    // Name
    if (root.get("name")) |name_val| {
        if (name_val == .string) {
            config.name = try alloc.dupe(u8, name_val.string);
        }
    }

    // Directory permissions
    if (root.get("dirs")) |dirs_val| {
        try parseDirs(alloc, config, dirs_val);
    }

    // Mounts
    if (root.get("mounts")) |mounts_val| {
        try parseMounts(alloc, config, mounts_val);
    }

    // Environment variables
    if (root.get("env")) |env_val| {
        try parseEnv(alloc, config, env_val);
    }

    // Runtime config
    if (sections == .all or sections == .runtime_only) {
        if (root.get("runtime")) |runtime_val| {
            if (runtime_val == .object) {
                parseRuntimeConfig(&config.runtime, runtime_val.object);
            }
        }

        // Legacy top-level runtime fields
        parseRuntimeConfigLegacy(&config.runtime, root);
    }

    // HTTP security
    if (sections == .all or sections == .runtime_only) {
        try parseHttpSecurity(alloc, config, root);
    }

    // Command security
    if (sections == .all or sections == .runtime_only) {
        try parseCommandSecurity(alloc, config, root);
    }

    // Daemon config
    if (sections == .all or sections == .daemon_only) {
        if (root.get("daemon")) |daemon_val| {
            if (daemon_val == .object) {
                config.daemon = parseDaemonConfig(daemon_val.object);
            }
        }
    }
}

/// Parse dirs section (object or array format)
fn parseDirs(alloc: std.mem.Allocator, config: *Config, dirs_val: std.json.Value) !void {
    if (dirs_val == .object) {
        // Object format: { "/path": "rwx", ... }
        var it = dirs_val.object.iterator();
        while (it.next()) |entry| {
            const dir_path = entry.key_ptr.*;
            const perms_str = if (entry.value_ptr.* == .string) entry.value_ptr.string else "r";

            const expanded = try path_utils.expandPath(alloc, dir_path);
            const dir_perms = DirPerms.fromString(expanded, perms_str);
            try config.dirs.append(alloc, dir_perms);
        }
    } else if (dirs_val == .array) {
        // Array format
        for (dirs_val.array.items) |item| {
            if (item == .string) {
                // Legacy: string = read-only
                const expanded = try path_utils.expandPath(alloc, item.string);
                try config.dirs.append(alloc, DirPerms{
                    .path = expanded,
                    .read = true,
                    .write = false,
                    .execute = false,
                });
            } else if (item == .object) {
                // Object format: {"path": "/tmp", "read": true, ...}
                const path_val = item.object.get("path") orelse continue;
                if (path_val != .string) continue;

                const expanded = try path_utils.expandPath(alloc, path_val.string);
                try config.dirs.append(alloc, DirPerms{
                    .path = expanded,
                    .read = if (item.object.get("read")) |v| v == .bool and v.bool else false,
                    .write = if (item.object.get("write")) |v| v == .bool and v.bool else false,
                    .execute = if (item.object.get("execute")) |v| v == .bool and v.bool else false,
                });
            }
        }
    }
}

/// Parse mounts section
fn parseMounts(alloc: std.mem.Allocator, config: *Config, mounts_val: std.json.Value) !void {
    if (mounts_val != .array) return;

    for (mounts_val.array.items) |item| {
        if (item != .object) continue;

        const host_val = item.object.get("host") orelse continue;
        const guest_val = item.object.get("guest") orelse continue;

        if (host_val != .string or guest_val != .string) continue;

        const host = try path_utils.expandPath(alloc, host_val.string);
        const guest = try path_utils.expandPath(alloc, guest_val.string);

        try config.mounts.append(alloc, Mount{
            .host = host,
            .guest = guest,
        });
    }
}

/// Parse env section
fn parseEnv(alloc: std.mem.Allocator, config: *Config, env_val: std.json.Value) !void {
    if (env_val != .array) return;

    for (env_val.array.items) |item| {
        if (item != .string) continue;

        // Get value from environment
        if (std.posix.getenv(item.string)) |val| {
            const env_str = try std.fmt.allocPrint(alloc, "{s}={s}", .{ item.string, val });
            try config.env_vars.append(alloc, env_str);
        }
    }
}

/// Parse runtime config object
fn parseRuntimeConfig(runtime: *types.RuntimeConfig, obj: std.json.ObjectMap) void {
    if (obj.get("stack_size")) |v| {
        if (v == .integer) runtime.stack_size = @intCast(@max(0, v.integer));
    }
    if (obj.get("heap_size")) |v| {
        if (v == .integer) runtime.heap_size = @intCast(@max(0, v.integer));
    }
    if (obj.get("max_memory_pages")) |v| {
        if (v == .integer) runtime.max_memory_pages = @intCast(@min(65536, @max(0, v.integer)));
    }
    if (obj.get("max_instructions")) |v| {
        if (v == .integer) runtime.max_instructions = @intCast(v.integer);
    }
    if (obj.get("exec_timeout_ms")) |v| {
        if (v == .integer) runtime.exec_timeout_ms = @intCast(@max(0, v.integer));
    }
    if (obj.get("cpu_limit_seconds")) |v| {
        if (v == .integer) runtime.cpu_limit_seconds = @intCast(@max(0, v.integer));
    }
    // Note: gas_metering removed - use cpu_limit_seconds or exec_timeout_ms instead
    if (obj.get("allocator")) |v| {
        if (v == .string) {
            runtime.use_bump_allocator = std.mem.eql(u8, v.string, "bump");
        }
    }
}

/// Parse legacy top-level runtime fields
fn parseRuntimeConfigLegacy(runtime: *types.RuntimeConfig, root: std.json.ObjectMap) void {
    // Support legacy top-level fields
    if (root.get("stackSize")) |v| {
        if (v == .integer) runtime.stack_size = @intCast(@max(0, v.integer));
    }
    if (root.get("heapSize")) |v| {
        if (v == .integer) runtime.heap_size = @intCast(@max(0, v.integer));
    }
    if (root.get("maxMemoryPages")) |v| {
        if (v == .integer) runtime.max_memory_pages = @intCast(@min(65536, @max(0, v.integer)));
    }
    if (root.get("execTimeoutMs")) |v| {
        if (v == .integer) runtime.exec_timeout_ms = @intCast(@max(0, v.integer));
    }
    // Note: gasLimit removed - use cpu_limit_seconds or exec_timeout_ms instead
}

/// Parse HTTP security settings
fn parseHttpSecurity(alloc: std.mem.Allocator, config: *Config, root: std.json.ObjectMap) !void {
    if (root.get("allowedUrls")) |urls_val| {
        if (urls_val == .array) {
            for (urls_val.array.items) |item| {
                if (item == .string) {
                    try config.http.allowed_urls.append(alloc, try alloc.dupe(u8, item.string));
                }
            }
        }
    }
    if (root.get("blockedUrls")) |urls_val| {
        if (urls_val == .array) {
            for (urls_val.array.items) |item| {
                if (item == .string) {
                    try config.http.blocked_urls.append(alloc, try alloc.dupe(u8, item.string));
                }
            }
        }
    }
    if (root.get("rateLimitRps")) |v| {
        if (v == .integer) config.http.rate_limit_rps = @intCast(@min(100000, @max(0, v.integer)));
    }
    if (root.get("maxConnections")) |v| {
        if (v == .integer) config.http.max_connections = @intCast(@min(10000, @max(1, v.integer)));
    }
}

/// Parse command security settings
fn parseCommandSecurity(alloc: std.mem.Allocator, config: *Config, root: std.json.ObjectMap) !void {
    if (root.get("allowCommands")) |cmds_val| {
        if (cmds_val == .array) {
            for (cmds_val.array.items) |item| {
                if (item == .string) {
                    try config.commands.allow_commands.append(alloc, try alloc.dupe(u8, item.string));
                }
            }
        }
    }
    if (root.get("denyCommands")) |cmds_val| {
        if (cmds_val == .array) {
            for (cmds_val.array.items) |item| {
                if (item == .string) {
                    try config.commands.deny_commands.append(alloc, try alloc.dupe(u8, item.string));
                }
            }
        }
    }
    if (root.get("useKeychain")) |v| {
        if (v == .bool) config.commands.use_keychain = v.bool;
    }
}

/// Parse daemon configuration
fn parseDaemonConfig(obj: std.json.ObjectMap) types.DaemonConfig {
    var daemon = types.DaemonConfig{};

    if (obj.get("pool_size")) |v| {
        if (v == .integer) daemon.pool_size = @intCast(@max(1, v.integer));
    }
    if (obj.get("port")) |v| {
        if (v == .integer) daemon.port = @intCast(@max(1, @min(65535, v.integer)));
    }
    if (obj.get("reuse_instances")) |v| {
        if (v == .bool) daemon.reuse_instances = v.bool;
    }
    if (obj.get("enable_cow")) |v| {
        if (v == .bool) daemon.enable_cow = v.bool;
    }
    if (obj.get("heap_size_mb")) |v| {
        if (v == .integer) daemon.heap_size_mb = @intCast(@max(1, v.integer));
    }

    return daemon;
}

// ============================================================================
// Tests
// ============================================================================

test "load returns config without error" {
    const alloc = std.testing.allocator;
    // This test just verifies loading doesn't crash - there may or may not be a config file
    var config = try load(alloc, .{ .search_paths = &.{"/nonexistent_path_12345"} });
    defer config.deinit(alloc);

    // With nonexistent search path and no wasm_path, should get empty config
    try std.testing.expect(config.name == null);
}
