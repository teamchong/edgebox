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

// ============================================================================
// JSON Helper Functions (DRY consolidation)
// ============================================================================

/// Extract string value from JSON object
fn getJsonString(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    if (obj.get(key)) |val| {
        if (val == .string) return val.string;
    }
    return null;
}

/// Extract integer value from JSON object with bounds clamping
fn getJsonInt(comptime T: type, obj: std.json.ObjectMap, key: []const u8, min_val: i64, max_val: i64) ?T {
    if (obj.get(key)) |val| {
        if (val == .integer) {
            const clamped = @min(max_val, @max(min_val, val.integer));
            return @intCast(clamped);
        }
    }
    return null;
}

/// Extract boolean value from JSON object
fn getJsonBool(obj: std.json.ObjectMap, key: []const u8) ?bool {
    if (obj.get(key)) |val| {
        if (val == .bool) return val.bool;
    }
    return null;
}

/// Append string array values to a list (with allocation/duplication)
fn appendJsonStrings(
    alloc: std.mem.Allocator,
    list: anytype,
    obj: std.json.ObjectMap,
    key: []const u8,
) !void {
    if (obj.get(key)) |arr_val| {
        if (arr_val == .array) {
            for (arr_val.array.items) |item| {
                if (item == .string) {
                    try list.append(alloc, try alloc.dupe(u8, item.string));
                }
            }
        }
    }
}

/// Append integer array values to a list with bounds clamping
fn appendJsonInts(
    comptime T: type,
    alloc: std.mem.Allocator,
    list: anytype,
    obj: std.json.ObjectMap,
    key: []const u8,
    min_val: i64,
    max_val: i64,
) !void {
    if (obj.get(key)) |arr_val| {
        if (arr_val == .array) {
            for (arr_val.array.items) |item| {
                if (item == .integer) {
                    const clamped = @min(max_val, @max(min_val, item.integer));
                    try list.append(alloc, @as(T, @intCast(clamped)));
                }
            }
        }
    }
}
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
/// Returns a static path buffer that's valid until the next call
var g_config_path_buf: [4096]u8 = undefined;

fn findConfigFile(options: LoadOptions) ?[]const u8 {
    // Check wasm_path directory first (highest priority)
    if (options.wasm_path) |wasm_path| {
        if (std.fs.path.dirname(wasm_path)) |dir| {
            const config_path = std.fmt.bufPrint(&g_config_path_buf, "{s}/.edgebox.json", .{dir}) catch null;
            if (config_path) |p| {
                if (std.fs.cwd().access(p, .{})) |_| {
                    return p;
                } else |_| {}
            }
        }
    }

    // Check search paths
    for (options.search_paths) |search_path| {
        const config_path = std.fmt.bufPrint(&g_config_path_buf, "{s}/.edgebox.json", .{search_path}) catch continue;
        if (std.fs.cwd().access(config_path, .{})) |_| {
            return config_path;
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
    if (getJsonString(root, "name")) |name| {
        config.name = try alloc.dupe(u8, name);
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

    // Server permissions (HTTP listen ports)
    if (sections == .all or sections == .runtime_only) {
        try parseServerConfig(alloc, config, root);
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
                const path_str = getJsonString(item.object, "path") orelse continue;
                const expanded = try path_utils.expandPath(alloc, path_str);
                try config.dirs.append(alloc, DirPerms{
                    .path = expanded,
                    .read = getJsonBool(item.object, "read") orelse false,
                    .write = getJsonBool(item.object, "write") orelse false,
                    .execute = getJsonBool(item.object, "execute") orelse false,
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

        const host_str = getJsonString(item.object, "host") orelse continue;
        const guest_str = getJsonString(item.object, "guest") orelse continue;

        const host = try path_utils.expandPath(alloc, host_str);
        const guest = try path_utils.expandPath(alloc, guest_str);

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
    if (getJsonInt(u32, obj, "stack_size", 0, std.math.maxInt(i64))) |v| runtime.stack_size = v;
    if (getJsonInt(u32, obj, "heap_size", 0, std.math.maxInt(i64))) |v| runtime.heap_size = v;
    if (getJsonInt(u32, obj, "max_memory_pages", 0, 65536)) |v| runtime.max_memory_pages = v;
    if (getJsonInt(i32, obj, "max_instructions", -1, std.math.maxInt(i32))) |v| runtime.max_instructions = v;
    if (getJsonInt(u32, obj, "exec_timeout_ms", 0, std.math.maxInt(i64))) |v| runtime.exec_timeout_ms = v;
    if (getJsonInt(u32, obj, "cpu_limit_seconds", 0, std.math.maxInt(i64))) |v| runtime.cpu_limit_seconds = v;
    // Note: gas_metering removed - use cpu_limit_seconds or exec_timeout_ms instead
    if (getJsonString(obj, "allocator")) |v| {
        runtime.use_bump_allocator = std.mem.eql(u8, v, "bump");
    }
}

/// Parse legacy top-level runtime fields
fn parseRuntimeConfigLegacy(runtime: *types.RuntimeConfig, root: std.json.ObjectMap) void {
    // Support legacy top-level fields (camelCase)
    if (getJsonInt(u32, root, "stackSize", 0, std.math.maxInt(i64))) |v| runtime.stack_size = v;
    if (getJsonInt(u32, root, "heapSize", 0, std.math.maxInt(i64))) |v| runtime.heap_size = v;
    if (getJsonInt(u32, root, "maxMemoryPages", 0, 65536)) |v| runtime.max_memory_pages = v;
    if (getJsonInt(u32, root, "execTimeoutMs", 0, std.math.maxInt(i64))) |v| runtime.exec_timeout_ms = v;
    // Note: gasLimit removed - use cpu_limit_seconds or exec_timeout_ms instead
}

/// Parse HTTP security settings
fn parseHttpSecurity(alloc: std.mem.Allocator, config: *Config, root: std.json.ObjectMap) !void {
    try appendJsonStrings(alloc, &config.http.allowed_urls, root, "allowedUrls");
    try appendJsonStrings(alloc, &config.http.blocked_urls, root, "blockedUrls");
    if (getJsonInt(u32, root, "rateLimitRps", 0, 100000)) |v| config.http.rate_limit_rps = v;
    if (getJsonInt(u32, root, "maxConnections", 1, 10000)) |v| config.http.max_connections = v;
}

/// Parse command security settings
fn parseCommandSecurity(alloc: std.mem.Allocator, config: *Config, root: std.json.ObjectMap) !void {
    try appendJsonStrings(alloc, &config.commands.allow_commands, root, "allowCommands");
    try appendJsonStrings(alloc, &config.commands.deny_commands, root, "denyCommands");
    if (getJsonBool(root, "useKeychain")) |v| config.commands.use_keychain = v;
}

/// Parse server permissions (for http.createServer, net.createServer)
fn parseServerConfig(alloc: std.mem.Allocator, config: *Config, root: std.json.ObjectMap) !void {
    // Support "server" section: { "ports": [3000, 8080], "listenAny": false }
    if (root.get("server")) |server_val| {
        if (server_val == .object) {
            try appendJsonInts(u16, alloc, &config.server.listen_ports, server_val.object, "ports", 1, 65535);
            if (getJsonBool(server_val.object, "listenAny")) |v| config.server.listen_any = v;
        }
    }

    // Also support legacy top-level "listenPorts" array
    try appendJsonInts(u16, alloc, &config.server.listen_ports, root, "listenPorts", 1, 65535);
}

/// Parse daemon configuration
fn parseDaemonConfig(obj: std.json.ObjectMap) types.DaemonConfig {
    var daemon = types.DaemonConfig{};

    if (getJsonInt(u32, obj, "pool_size", 1, std.math.maxInt(i64))) |v| daemon.pool_size = v;
    if (getJsonInt(u16, obj, "port", 1, 65535)) |v| daemon.port = v;
    if (getJsonBool(obj, "reuse_instances")) |v| daemon.reuse_instances = v;
    if (getJsonBool(obj, "enable_cow")) |v| daemon.enable_cow = v;
    if (getJsonInt(u32, obj, "heap_size_mb", 1, std.math.maxInt(i64))) |v| daemon.heap_size_mb = v;

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
