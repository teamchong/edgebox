/// EdgeBox WAMR Runtime - Fast cold start using WAMR (WebAssembly Micro Runtime)
///
/// WAMR is a lightweight WASM runtime from Bytecode Alliance.
/// - 4ms cold start vs 11ms with WasmEdge
/// - 400KB binary vs 2.6MB
/// - Pure C, compiled with Zig
///
/// Implements edgebox_process.* API for process spawning
/// with edgebox-sandbox integration for OS-level isolation.
const std = @import("std");
const builtin = @import("builtin");
const safe_fetch = @import("safe_fetch.zig");
const stdlib = @import("host/stdlib.zig");
const h2 = @import("h2");
const runtime = @import("runtime.zig");
const shell_parser = @import("shell_parser.zig");
const emulators = @import("component/emulators/mod.zig");

// Component Model support
const NativeRegistry = @import("component/native_registry.zig").NativeRegistry;
const import_resolver = @import("component/import_resolver.zig");

// Import WAMR C API
const c = @cImport({
    @cInclude("wasm_export.h");
    @cInclude("lib_export.h");
});

// Host function signatures for EdgeBox extensions
const NativeSymbol = c.NativeSymbol;

// =============================================================================
// Process State - edgebox_process implementation
// =============================================================================

/// Global process builder state (edgebox_process uses global state)
var process_state: ProcessState = .{};
const allocator = std.heap.page_allocator;

/// Global security policy for HTTP requests (loaded from .edgebox.json)
var g_security_policy: safe_fetch.SecurityPolicy = safe_fetch.SecurityPolicy.permissive;

/// Global config pointer for permission checks (set by run())
var g_config: ?*const Config = null;

/// Component Model registry and initialization state
var g_component_registry: ?NativeRegistry = null;
var g_component_initialized: bool = false;

const ProcessState = struct {
    program: ?[]const u8 = null,
    args: std.ArrayListUnmanaged([]const u8) = .{},
    env_vars: std.ArrayListUnmanaged(EnvVar) = .{},
    stdin_data: ?[]const u8 = null,
    timeout_ms: u32 = 30000,

    // Result state
    exit_code: i32 = 0,
    stdout_buf: ?[]u8 = null,
    stderr_buf: ?[]u8 = null,

    const EnvVar = struct { key: []const u8, value: []const u8 };

    fn reset(self: *ProcessState) void {
        self.program = null;
        self.args.clearRetainingCapacity(allocator);
        self.env_vars.clearRetainingCapacity(allocator);
        self.stdin_data = null;
        self.timeout_ms = 30000;
        self.exit_code = 0;
        if (self.stdout_buf) |buf| allocator.free(buf);
        if (self.stderr_buf) |buf| allocator.free(buf);
        self.stdout_buf = null;
        self.stderr_buf = null;
    }
};

// =============================================================================
// Configuration from .edgebox.json
// =============================================================================

/// Directory permissions (Unix-style rwx)
const DirPerms = struct {
    path: []const u8,
    read: bool = false,
    write: bool = false,
    execute: bool = false, // Shell/spawn access to files in this dir

    /// Parse "rwx" string into permissions
    fn fromString(path: []const u8, perms: []const u8) DirPerms {
        return DirPerms{
            .path = path,
            .read = std.mem.indexOfScalar(u8, perms, 'r') != null,
            .write = std.mem.indexOfScalar(u8, perms, 'w') != null,
            .execute = std.mem.indexOfScalar(u8, perms, 'x') != null,
        };
    }
};

/// Mount point for path remapping (Docker-style volumes)
const Mount = struct {
    host: []const u8, // Actual path on host filesystem
    guest: []const u8, // Path the app sees (e.g., $HOME)
};

const Config = struct {
    dirs: std.ArrayListUnmanaged(DirPerms) = .{},
    mounts: std.ArrayListUnmanaged(Mount) = .{}, // Path remapping
    env_vars: std.ArrayListUnmanaged([]const u8) = .{}, // Format: "KEY=value"
    stack_size: u32 = 128 * 1024 * 1024, // 128MB stack (needed for frozen recursive functions with AOT+SIMD)
    heap_size: u32 = 256 * 1024 * 1024, // 256MB heap for host-managed allocations
    max_memory_pages: u32 = 32768, // 2GB max linear memory (32768 * 64KB pages) - WASM32 limit
    // Note: WASM can dynamically grow memory via memory.grow, but cannot exceed max_memory_pages
    max_instructions: i32 = -1, // CPU limit: max WASM instructions per execution (-1 = unlimited)
    // Note: Only works in interpreter mode (.wasm), not AOT mode (.aot)

    // Execution limits for AOT mode (setrlimit-based)
    exec_timeout_ms: u64 = 0, // Wall-clock timeout in ms (0 = unlimited, uses watchdog thread)
    cpu_limit_seconds: u32 = 0, // CPU time limit in seconds (0 = unlimited, uses setrlimit)

    // HTTP security settings (default: off / permissive)
    allowed_urls: std.ArrayListUnmanaged([]const u8) = .{}, // Empty = allow all
    blocked_urls: std.ArrayListUnmanaged([]const u8) = .{},
    rate_limit_rps: u32 = 0, // 0 = unlimited
    max_connections: u32 = 100, // Default high for permissive mode

    // Command allow/deny lists for spawn (enforced at sandbox level)
    allow_commands: std.ArrayListUnmanaged([]const u8) = .{}, // Empty = allow all
    deny_commands: std.ArrayListUnmanaged([]const u8) = .{}, // Takes precedence

    // Command security settings
    commands: []const runtime.CommandPermission = &.{}, // Extended command permissions with credentials
    sensitive_files: []const []const u8 = &.{}, // Glob patterns for files WASM cannot access directly
    blocked_patterns: []const []const u8 = &.{}, // Patterns for destructive commands to block

    // Keychain access (default: off for security)
    use_keychain: bool = false, // Must opt-in via "useKeychain": true

    fn deinit(self: *Config) void {
        for (self.dirs.items) |dir| {
            allocator.free(dir.path);
        }
        self.dirs.deinit(allocator);
        for (self.env_vars.items) |env| {
            allocator.free(env);
        }
        self.env_vars.deinit(allocator);
        for (self.allowed_urls.items) |url| {
            allocator.free(url);
        }
        self.allowed_urls.deinit(allocator);
        for (self.blocked_urls.items) |url| {
            allocator.free(url);
        }
        self.blocked_urls.deinit(allocator);
        for (self.allow_commands.items) |cmd| {
            allocator.free(cmd);
        }
        self.allow_commands.deinit(allocator);
        for (self.deny_commands.items) |cmd| {
            allocator.free(cmd);
        }
        self.deny_commands.deinit(allocator);
        for (self.mounts.items) |mount| {
            allocator.free(mount.host);
            allocator.free(mount.guest);
        }
        self.mounts.deinit(allocator);
    }

    /// Remap a guest path to host path using mounts (longest match first)
    fn remapPath(self: *const Config, alloc: std.mem.Allocator, path: []const u8) ?[]const u8 {
        // Sort by guest path length (longest first for specificity)
        // We check all mounts and find the longest matching guest prefix
        var best_match: ?Mount = null;
        var best_len: usize = 0;

        for (self.mounts.items) |mount| {
            // Check exact match or path starts with mount.guest/
            if (std.mem.eql(u8, path, mount.guest)) {
                if (mount.guest.len > best_len) {
                    best_match = mount;
                    best_len = mount.guest.len;
                }
            } else if (path.len > mount.guest.len and
                std.mem.startsWith(u8, path, mount.guest) and
                path[mount.guest.len] == '/')
            {
                if (mount.guest.len > best_len) {
                    best_match = mount;
                    best_len = mount.guest.len;
                }
            }
        }

        if (best_match) |mount| {
            // Remap: replace guest prefix with host prefix
            const rest = path[mount.guest.len..];
            return std.fmt.allocPrint(alloc, "{s}{s}", .{ mount.host, rest }) catch null;
        }

        return null; // No remapping needed
    }

    /// Check if path has read permission
    fn canRead(self: *const Config, path: []const u8) bool {
        for (self.dirs.items) |dir| {
            if (dir.read and pathStartsWith(path, dir.path)) return true;
        }
        return false;
    }

    /// Check if path has write permission
    fn canWrite(self: *const Config, path: []const u8) bool {
        for (self.dirs.items) |dir| {
            if (dir.write and pathStartsWith(path, dir.path)) return true;
        }
        return false;
    }

    /// Check if path has execute permission (for spawn/shell)
    fn canExecute(self: *const Config, path: []const u8) bool {
        for (self.dirs.items) |dir| {
            if (dir.execute and pathStartsWith(path, dir.path)) return true;
        }
        return false;
    }

    /// Check if any execute permission exists (for general spawn access)
    fn hasAnyExecute(self: *const Config) bool {
        for (self.dirs.items) |dir| {
            if (dir.execute) return true;
        }
        return false;
    }
};

/// Check if path starts with prefix (handles trailing slashes)
fn pathStartsWith(path: []const u8, prefix: []const u8) bool {
    if (prefix.len == 0) return false;
    // Normalize: remove trailing slash from prefix for comparison
    const clean_prefix = if (prefix.len > 1 and prefix[prefix.len - 1] == '/')
        prefix[0 .. prefix.len - 1]
    else
        prefix;
    // Exact match or path starts with prefix/
    if (std.mem.eql(u8, path, clean_prefix)) return true;
    if (path.len > clean_prefix.len and
        std.mem.startsWith(u8, path, clean_prefix) and
        path[clean_prefix.len] == '/')
    {
        return true;
    }
    // Handle "." prefix for current directory
    if (std.mem.eql(u8, clean_prefix, ".")) return true;
    return false;
}

fn expandPath(path: []const u8) ![]const u8 {
    // Handle ~ expansion
    if (path.len > 0 and path[0] == '~') {
        const home = std.process.getEnvVarOwned(allocator, "HOME") catch return try allocator.dupe(u8, path);
        defer allocator.free(home);
        const rest = if (path.len > 1) path[1..] else "";
        return try std.fmt.allocPrint(allocator, "{s}{s}", .{ home, rest });
    }
    // Handle $HOME expansion
    if (std.mem.eql(u8, path, "$HOME")) {
        const home = std.process.getEnvVarOwned(allocator, "HOME") catch return try allocator.dupe(u8, path);
        return home;
    }
    if (std.mem.startsWith(u8, path, "$HOME/")) {
        const home = std.process.getEnvVarOwned(allocator, "HOME") catch return try allocator.dupe(u8, path);
        defer allocator.free(home);
        const rest = path[5..]; // "$HOME" is 5 chars, keep the "/"
        return try std.fmt.allocPrint(allocator, "{s}{s}", .{ home, rest });
    }
    // Handle $PWD expansion
    if (std.mem.eql(u8, path, "$PWD")) {
        var buf: [4096]u8 = undefined;
        const cwd = std.process.getCwd(&buf) catch return try allocator.dupe(u8, path);
        return try allocator.dupe(u8, cwd);
    }
    if (std.mem.startsWith(u8, path, "$PWD/")) {
        var buf: [4096]u8 = undefined;
        const cwd = std.process.getCwd(&buf) catch return try allocator.dupe(u8, path);
        const rest = path[4..]; // "$PWD" is 4 chars, keep the "/"
        return try std.fmt.allocPrint(allocator, "{s}{s}", .{ cwd, rest });
    }
    return try allocator.dupe(u8, path);
}

/// Load .env file and add EDGEBOX_ prefixed vars to config
/// EDGEBOX_ANTHROPIC_API_KEY -> ANTHROPIC_API_KEY
/// EDGEBOX_CLAUDE_CONFIG_DIR -> CLAUDE_CONFIG_DIR
fn loadEnvFile(config: *Config) void {
    const env_file = std.fs.cwd().openFile(".env", .{}) catch return;
    defer env_file.close();

    const env_size = env_file.getEndPos() catch return;
    if (env_size > 64 * 1024) return; // Max 64KB .env file

    const env_buf = allocator.alloc(u8, env_size) catch return;
    defer allocator.free(env_buf);

    const bytes_read = env_file.readAll(env_buf) catch return;

    // Parse line by line
    var lines = std.mem.splitScalar(u8, env_buf[0..bytes_read], '\n');
    while (lines.next()) |line| {
        // Skip comments and empty lines
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len == 0 or trimmed[0] == '#') continue;

        // Find = separator
        const eq_pos = std.mem.indexOfScalar(u8, trimmed, '=') orelse continue;
        const key = trimmed[0..eq_pos];
        var value = trimmed[eq_pos + 1 ..];

        // Strip inline comments (# not inside quotes)
        // Simple approach: if value doesn't start with quote, strip everything after #
        if (value.len > 0 and value[0] != '"' and value[0] != '\'') {
            if (std.mem.indexOfScalar(u8, value, '#')) |hash_pos| {
                value = std.mem.trim(u8, value[0..hash_pos], " \t");
            }
        }

        // Remove surrounding quotes if present
        if (value.len >= 2) {
            if ((value[0] == '"' and value[value.len - 1] == '"') or
                (value[0] == '\'' and value[value.len - 1] == '\''))
            {
                value = value[1 .. value.len - 1];
            }
        }

        // Expand $HOME in value
        const expanded_value = if (std.mem.indexOf(u8, value, "$HOME")) |_| blk: {
            const home = std.process.getEnvVarOwned(allocator, "HOME") catch break :blk allocator.dupe(u8, value) catch continue;
            defer allocator.free(home);
            // Simple replacement of $HOME
            var result = std.ArrayListUnmanaged(u8){};
            var i: usize = 0;
            while (i < value.len) {
                if (i + 5 <= value.len and std.mem.eql(u8, value[i .. i + 5], "$HOME")) {
                    result.appendSlice(allocator, home) catch break :blk allocator.dupe(u8, value) catch continue;
                    i += 5;
                } else {
                    result.append(allocator, value[i]) catch break :blk allocator.dupe(u8, value) catch continue;
                    i += 1;
                }
            }
            break :blk result.toOwnedSlice(allocator) catch continue;
        } else allocator.dupe(u8, value) catch continue;

        // Map EDGEBOX_ prefixed vars to unprefixed
        const mapped_key = if (std.mem.startsWith(u8, key, "EDGEBOX_"))
            key[8..] // Strip EDGEBOX_ prefix
        else
            key;

        // Format as KEY=value
        const env_str = std.fmt.allocPrint(allocator, "{s}={s}", .{ mapped_key, expanded_value }) catch {
            allocator.free(expanded_value);
            continue;
        };
        allocator.free(expanded_value);

        config.env_vars.append(allocator, env_str) catch {
            allocator.free(env_str);
        };
    }
}

fn loadConfig() Config {
    var config = Config{};

    // By default, NO directory access. User must explicitly grant in .edgebox.json
    // Load .env file first (EDGEBOX_ prefix vars mapped to unprefixed)
    loadEnvFile(&config);

    // Try to load .edgebox.json
    const config_file = std.fs.cwd().openFile(".edgebox.json", .{}) catch return config;
    defer config_file.close();

    const config_size = config_file.getEndPos() catch return config;
    if (config_size > 1024 * 1024) return config; // Max 1MB config

    const config_buf = allocator.alloc(u8, config_size) catch return config;
    defer allocator.free(config_buf);

    _ = config_file.readAll(config_buf) catch return config;

    // Parse JSON
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, config_buf, .{}) catch return config;
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return config;

    // Parse dirs - object format: { "/path": "rwx", ... }
    if (root.object.get("dirs")) |dirs_val| {
        if (dirs_val == .object) {
            var it = dirs_val.object.iterator();
            while (it.next()) |entry| {
                const path = entry.key_ptr.*;
                const perms_str = if (entry.value_ptr.* == .string) entry.value_ptr.string else "r"; // default read-only

                const expanded = expandPath(path) catch continue;
                const dir_perms = DirPerms.fromString(expanded, perms_str);
                config.dirs.append(allocator, dir_perms) catch {
                    allocator.free(expanded);
                };
            }
        } else if (dirs_val == .array) {
            // Array format - can be strings (legacy) or objects (new)
            for (dirs_val.array.items) |item| {
                if (item == .string) {
                    // Legacy: array of strings - treat as read-only
                    const expanded = expandPath(item.string) catch continue;
                    const dir_perms = DirPerms{
                        .path = expanded,
                        .read = true,
                        .write = false,
                        .execute = false,
                    };
                    config.dirs.append(allocator, dir_perms) catch {
                        allocator.free(expanded);
                    };
                } else if (item == .object) {
                    // New format: {"path": "/tmp", "read": true, "write": true, "execute": true}
                    const path_val = item.object.get("path") orelse continue;
                    if (path_val != .string) continue;
                    const expanded = expandPath(path_val.string) catch continue;
                    const dir_perms = DirPerms{
                        .path = expanded,
                        .read = if (item.object.get("read")) |v| v == .bool and v.bool else false,
                        .write = if (item.object.get("write")) |v| v == .bool and v.bool else false,
                        .execute = if (item.object.get("execute")) |v| v == .bool and v.bool else false,
                    };
                    config.dirs.append(allocator, dir_perms) catch {
                        allocator.free(expanded);
                    };
                }
            }
        }
    }

    // Parse mounts - Docker-style host:guest path mapping
    // Format: [{"host": "/tmp/edgebox-home", "guest": "$HOME"}]
    if (root.object.get("mounts")) |mounts_val| {
        if (mounts_val == .array) {
            for (mounts_val.array.items) |item| {
                if (item != .object) continue;
                const host_val = item.object.get("host") orelse continue;
                const guest_val = item.object.get("guest") orelse continue;
                if (host_val != .string or guest_val != .string) continue;

                const expanded_host = expandPath(host_val.string) catch continue;
                const expanded_guest = expandPath(guest_val.string) catch continue;

                config.mounts.append(allocator, Mount{
                    .host = expanded_host,
                    .guest = expanded_guest,
                }) catch {
                    allocator.free(expanded_host);
                    allocator.free(expanded_guest);
                };
            }
        }
    }

    // Parse runtime config
    if (root.object.get("runtime")) |runtime_val| {
        if (runtime_val == .object) {
            if (runtime_val.object.get("stack_size")) |stack_val| {
                if (stack_val == .integer) {
                    const val = @max(0, @min(std.math.maxInt(u32), stack_val.integer));
                    config.stack_size = @intCast(val);
                }
            }
            if (runtime_val.object.get("heap_size")) |heap_val| {
                if (heap_val == .integer) {
                    const val = @max(0, @min(std.math.maxInt(u32), heap_val.integer));
                    config.heap_size = @intCast(val);
                }
            }
            // max_memory_pages: Maximum WASM linear memory pages (each page = 64KB)
            // Default 32768 = 2GB (WASM32 max). WASM can grow memory dynamically
            // via memory.grow but cannot exceed this limit.
            if (runtime_val.object.get("max_memory_pages")) |pages_val| {
                if (pages_val == .integer) {
                    config.max_memory_pages = @intCast(@max(0, @min(65536, pages_val.integer))); // Cap at 4GB
                }
            }
            // max_instructions: CPU limit for interpreter mode (not AOT)
            // -1 = unlimited (default), positive value = instruction limit
            if (runtime_val.object.get("max_instructions")) |inst_val| {
                if (inst_val == .integer) {
                    config.max_instructions = @intCast(inst_val.integer);
                }
            }
            // exec_timeout_ms: Wall-clock timeout in milliseconds (uses watchdog thread)
            // 0 = unlimited (default), positive value = timeout
            // Works for both interpreter and AOT mode
            if (runtime_val.object.get("exec_timeout_ms")) |timeout_val| {
                if (timeout_val == .integer) {
                    const val = @max(0, @min(std.math.maxInt(u32), timeout_val.integer));
                    config.exec_timeout_ms = @intCast(val);
                }
            }
            // cpu_limit_seconds: CPU time limit in seconds (uses setrlimit, Unix only)
            // 0 = unlimited (default), positive value = limit
            // Works for both interpreter and AOT mode
            if (runtime_val.object.get("cpu_limit_seconds")) |limit_val| {
                if (limit_val == .integer) {
                    config.cpu_limit_seconds = @intCast(@max(0, limit_val.integer));
                }
            }
        }
    }

    // Parse env array - pass through specified env vars from host
    if (root.object.get("env")) |env_val| {
        if (env_val == .array) {
            for (env_val.array.items) |item| {
                if (item == .string) {
                    // Get env var value from host
                    const val = std.process.getEnvVarOwned(allocator, item.string) catch continue;
                    defer allocator.free(val);
                    // Format as KEY=value
                    const env_str = std.fmt.allocPrint(allocator, "{s}={s}", .{ item.string, val }) catch continue;
                    config.env_vars.append(allocator, env_str) catch {
                        allocator.free(env_str);
                    };
                }
            }
        }
    }

    // Parse HTTP security settings (all optional, default = permissive / no restrictions)
    if (root.object.get("allowedUrls")) |urls_val| {
        if (urls_val == .array) {
            for (urls_val.array.items) |item| {
                if (item == .string) {
                    const url = allocator.dupe(u8, item.string) catch continue;
                    config.allowed_urls.append(allocator, url) catch {
                        allocator.free(url);
                    };
                }
            }
        }
    }

    if (root.object.get("blockedUrls")) |urls_val| {
        if (urls_val == .array) {
            for (urls_val.array.items) |item| {
                if (item == .string) {
                    const url = allocator.dupe(u8, item.string) catch continue;
                    config.blocked_urls.append(allocator, url) catch {
                        allocator.free(url);
                    };
                }
            }
        }
    }

    if (root.object.get("rateLimitRps")) |rps_val| {
        if (rps_val == .integer) {
            config.rate_limit_rps = @intCast(@max(0, rps_val.integer));
        }
    }

    if (root.object.get("maxConnections")) |conn_val| {
        if (conn_val == .integer) {
            config.max_connections = @intCast(@max(1, conn_val.integer));
        }
    }

    // Parse useKeychain (default: false for security)
    if (root.object.get("useKeychain")) |keychain_val| {
        if (keychain_val == .bool) {
            config.use_keychain = keychain_val.bool;
        }
    }

    // Parse command allow/deny lists (enforced at sandbox level)
    if (root.object.get("allowCommands")) |cmds_val| {
        if (cmds_val == .array) {
            for (cmds_val.array.items) |item| {
                if (item == .string) {
                    const cmd = allocator.dupe(u8, item.string) catch continue;
                    config.allow_commands.append(allocator, cmd) catch {
                        allocator.free(cmd);
                    };
                }
            }
        }
    }

    if (root.object.get("denyCommands")) |cmds_val| {
        if (cmds_val == .array) {
            for (cmds_val.array.items) |item| {
                if (item == .string) {
                    const cmd = allocator.dupe(u8, item.string) catch continue;
                    config.deny_commands.append(allocator, cmd) catch {
                        allocator.free(cmd);
                    };
                }
            }
        }
    }

    // Load extended command security from runtime.EdgeBoxConfig (handles credentials, deny subcommands, etc.)
    const edge_config = runtime.EdgeBoxConfig.loadFromCwd(allocator);
    if (edge_config) |ec| {
        config.commands = ec.commands;
        config.sensitive_files = ec.sensitive_files;
        config.blocked_patterns = ec.blocked_patterns;
    }

    // Try to get Claude API key from macOS keychain if enabled and not already set
    if (config.use_keychain) {
        const has_api_key = blk: {
            for (config.env_vars.items) |env| {
                if (std.mem.startsWith(u8, env, "ANTHROPIC_API_KEY=")) break :blk true;
            }
            break :blk false;
        };

        if (!has_api_key) {
            if (getClaudeApiKeyFromKeychain(allocator)) |api_key| {
                const env_str = std.fmt.allocPrint(allocator, "ANTHROPIC_API_KEY={s}", .{api_key}) catch {
                    allocator.free(api_key);
                    return config;
                };
                allocator.free(api_key);
                config.env_vars.append(allocator, env_str) catch {
                    allocator.free(env_str);
                };
            }
        }
    }

    // Pass mounts to WASM as __EDGEBOX_MOUNTS JSON env var for JS-side path remapping
    // Also determine if HOME should be remapped
    var home_override: ?[]const u8 = null;
    if (config.mounts.items.len > 0) {
        const real_home = std.process.getEnvVarOwned(allocator, "HOME") catch null;
        defer if (real_home) |h| allocator.free(h);

        // Check if any mount has guest == real HOME (meaning we should remap HOME env)
        if (real_home) |rh| {
            for (config.mounts.items) |mount| {
                if (std.mem.eql(u8, mount.guest, rh)) {
                    home_override = mount.host;
                    break;
                }
            }
        }

        // Build JSON array for __EDGEBOX_MOUNTS
        var mounts_json = std.ArrayListUnmanaged(u8){};
        defer mounts_json.deinit(allocator);
        mounts_json.appendSlice(allocator, "[") catch {};
        for (config.mounts.items, 0..) |mount, i| {
            if (i > 0) mounts_json.appendSlice(allocator, ",") catch {};
            const entry = std.fmt.allocPrint(allocator, "{{\"host\":\"{s}\",\"guest\":\"{s}\"}}", .{ mount.host, mount.guest }) catch continue;
            defer allocator.free(entry);
            mounts_json.appendSlice(allocator, entry) catch {};
        }
        mounts_json.appendSlice(allocator, "]") catch {};

        const mounts_env = std.fmt.allocPrint(allocator, "__EDGEBOX_MOUNTS={s}", .{mounts_json.items}) catch null;
        if (mounts_env) |env_str| {
            config.env_vars.append(allocator, env_str) catch {
                allocator.free(env_str);
            };
        }
    }

    // Auto-add essential env vars (HOME, USER, etc.) for Node.js-style apps
    const essential_vars = [_][]const u8{ "HOME", "USER", "PWD", "PATH", "TMPDIR", "TERM", "SHELL" };
    for (essential_vars) |var_name| {
        // Check if already set
        const already_set = blk: {
            const prefix = std.fmt.allocPrint(allocator, "{s}=", .{var_name}) catch continue;
            defer allocator.free(prefix);
            for (config.env_vars.items) |env| {
                if (std.mem.startsWith(u8, env, prefix)) break :blk true;
            }
            break :blk false;
        };
        if (already_set) continue;

        // Handle HOME specially - use mount override if available
        if (std.mem.eql(u8, var_name, "HOME") and home_override != null) {
            const env_str = std.fmt.allocPrint(allocator, "HOME={s}", .{home_override.?}) catch continue;
            config.env_vars.append(allocator, env_str) catch {
                allocator.free(env_str);
            };
            continue;
        }

        // Get from host and add
        const val = std.process.getEnvVarOwned(allocator, var_name) catch continue;
        defer allocator.free(val);
        const env_str = std.fmt.allocPrint(allocator, "{s}={s}", .{ var_name, val }) catch continue;
        config.env_vars.append(allocator, env_str) catch {
            allocator.free(env_str);
        };
    }

    return config;
}

/// Read Claude API key from macOS keychain using security command
fn getClaudeApiKeyFromKeychain(alloc: std.mem.Allocator) ?[]u8 {
    // Run: security find-generic-password -s "Claude Code-credentials" -w
    var child = std.process.Child.init(&.{ "security", "find-generic-password", "-s", "Claude Code-credentials", "-w" }, alloc);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Ignore;

    child.spawn() catch return null;

    const stdout = child.stdout orelse return null;
    const creds_json = stdout.readToEndAlloc(alloc, 10 * 1024) catch return null;
    defer alloc.free(creds_json);

    _ = child.wait() catch return null;

    // Parse JSON to extract accessToken
    // Format: {"claudeAiOauth":{"accessToken":"sk-ant-...","refreshToken":"...","expiresAt":...}}
    const parsed = std.json.parseFromSlice(std.json.Value, alloc, creds_json, .{}) catch return null;
    defer parsed.deinit();

    if (parsed.value != .object) return null;
    const oauth = parsed.value.object.get("claudeAiOauth") orelse return null;
    if (oauth != .object) return null;
    const token = oauth.object.get("accessToken") orelse return null;
    if (token != .string) return null;

    return alloc.dupe(u8, token.string) catch null;
}

// =============================================================================
// CPU/Execution Limit Helpers (for AOT mode where instruction metering doesn't work)
// =============================================================================

/// Global flag set by signal handlers to indicate timeout
var g_cpu_limit_exceeded: std.atomic.Value(bool) = std.atomic.Value(bool).init(false);

/// Set CPU time limit using setrlimit (Unix only)
/// Returns true if limit was set successfully
fn setCpuTimeLimit(seconds: u32) bool {
    if (builtin.os.tag == .windows) return false;
    if (seconds == 0) return false;

    const rlim = std.posix.rlimit{
        .cur = seconds,
        .max = seconds,
    };
    std.posix.setrlimit(.CPU, rlim) catch return false;
    return true;
}

/// Restore unlimited CPU time (for daemon mode reuse)
fn restoreCpuTimeLimit() void {
    if (builtin.os.tag == .windows) return;

    const unlimited = std.posix.rlimit{
        .cur = std.posix.RLIM.INFINITY,
        .max = std.posix.RLIM.INFINITY,
    };
    std.posix.setrlimit(.CPU, unlimited) catch {};
}

/// Setup signal handlers for CPU limit exceeded (SIGXCPU) and termination (SIGTERM)
fn setupCpuLimitSignalHandlers() void {
    if (builtin.os.tag == .windows) return;

    // SIGXCPU handler - sent when CPU limit is exceeded
    const xcpu_handler = std.posix.Sigaction{
        .handler = .{ .handler = cpuLimitExceededHandler },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    };
    _ = std.posix.sigaction(std.posix.SIG.XCPU, &xcpu_handler, null);
}

fn cpuLimitExceededHandler(sig: i32) callconv(.c) void {
    _ = sig;
    g_cpu_limit_exceeded.store(true, .release);
    // Exit with standard timeout exit code (124 matches GNU timeout)
    std.debug.print("[EDGEBOX] CPU time limit exceeded\n", .{});
    std.process.exit(124);
}

/// Watchdog context for wall-clock timeout enforcement
const WatchdogContext = struct {
    deadline_ns: i128,
    cancelled: std.atomic.Value(bool),
    main_pid: if (builtin.os.tag != .windows) std.posix.pid_t else u32,
};

/// Get current process ID (Unix only)
fn getPid() if (builtin.os.tag != .windows) std.posix.pid_t else u32 {
    if (builtin.os.tag == .windows) return 0;
    // Use the C library getpid
    return @import("std").c.getpid();
}

/// Watchdog thread function - monitors wall-clock time and kills process on timeout
fn watchdogThread(ctx: *WatchdogContext) void {
    while (std.time.nanoTimestamp() < ctx.deadline_ns) {
        if (ctx.cancelled.load(.acquire)) return;
        std.Thread.sleep(50 * std.time.ns_per_ms); // Check every 50ms
    }

    // Timeout reached - atomically check and set to prevent race with cancellation
    // Use cmpxchgStrong: if cancelled is false (0), set it to true (1) and proceed with kill
    // If it was already true, someone else cancelled - don't kill
    if (ctx.cancelled.cmpxchgStrong(false, true, .acq_rel, .acquire) == null) {
        // We won the race - we set cancelled=true, so we should send the signal
        std.debug.print("[EDGEBOX] Execution timeout exceeded (wall-clock)\n", .{});
        // Send SIGTERM to self
        if (builtin.os.tag != .windows) {
            std.posix.kill(ctx.main_pid, std.posix.SIG.TERM) catch {};
        }
    }
    // else: cmpxchg returned the old value (true), meaning it was already cancelled
}

// =============================================================================
// Main Entry Point
// =============================================================================

pub fn main() !void {
    // Increase stack size for deep recursion in frozen functions with AOT+SIMD
    // Default macOS stack limit is ~8MB, which is too small for fib(40+)
    if (builtin.os.tag == .macos or builtin.os.tag == .linux) {
        var limit = std.posix.getrlimit(.STACK) catch std.posix.rlimit{ .cur = 0, .max = 0 };

        // macOS hard limit is usually 64-67MB, request that maximum
        const desired_stack: u64 = if (builtin.os.tag == .macos) 64 * 1024 * 1024 else 512 * 1024 * 1024;
        if (limit.cur < desired_stack) {
            // On macOS, can't exceed hard limit (usually ~67MB)
            const actual_desired = if (limit.max > 0 and desired_stack > limit.max) limit.max else desired_stack;
            limit.cur = actual_desired;
            _ = std.posix.setrlimit(.STACK, limit) catch {};
        }
    }

    // Load config first
    var config = loadConfig();
    defer config.deinit();

    // Initialize emulator system
    emulators.init(allocator);
    defer emulators.deinit();

    // Set global config pointer for permission checks
    g_config = &config;
    defer {
        g_config = null;
    }

    // Apply HTTP security policy from config (default: permissive / no restrictions)
    if (config.allowed_urls.items.len > 0 or config.blocked_urls.items.len > 0) {
        g_security_policy = .{
            .allowed_urls = config.allowed_urls.items,
            .blocked_urls = config.blocked_urls.items,
            .rate_limit_rps = config.rate_limit_rps,
            .max_connections = config.max_connections,
            .timeout_ms = 30000,
            .log_requests = true,
        };
    }

    // Collect all args
    var args_list = std.ArrayListUnmanaged([*:0]const u8){};
    defer args_list.deinit(allocator);

    var args_iter = std.process.args();
    _ = args_iter.next(); // skip program name

    const wasm_path = args_iter.next() orelse {
        std.debug.print("Usage: edgebox <file.wasm|file.aot> [args...]\n", .{});
        std.debug.print("\nedgebox is a WASM/AOT runner. To run JS, compile first:\n", .{});
        std.debug.print("  edgeboxc build <app_dir>   # Compile JS to WASM\n", .{});
        std.debug.print("  edgebox <file.wasm>        # Run compiled WASM\n", .{});
        return;
    };

    // Collect remaining args for WASI
    // First arg to WASM should be the wasm filename itself
    const wasm_basename = std.fs.path.basename(wasm_path);
    const basename_z = try allocator.dupeZ(u8, wasm_basename);
    try args_list.append(allocator, basename_z);

    while (args_iter.next()) |arg| {
        try args_list.append(allocator, arg);
    }

    const debug = std.process.getEnvVarOwned(allocator, "EDGEBOX_DEBUG") catch null;
    const show_debug = debug != null;
    if (debug) |d| allocator.free(d);

    // EDGEBOX_QUIET=1 suppresses informational messages (useful for benchmarks)
    const quiet = std.process.getEnvVarOwned(allocator, "EDGEBOX_QUIET") catch null;
    _ = quiet != null; // Reserved for future use
    if (quiet) |q| allocator.free(q);

    const start = std.time.nanoTimestamp();

    // Initialize WAMR runtime - use system allocator for simplicity
    var init_args = std.mem.zeroes(c.RuntimeInitArgs);
    init_args.mem_alloc_type = c.Alloc_With_System_Allocator;

    // Use interpreter mode - Fast JIT is disabled in our WAMR build for consistency
    // For best performance, use AOT-compiled .aot files instead of .wasm
    init_args.running_mode = c.Mode_Interp;
    if (show_debug and std.mem.endsWith(u8, wasm_path, ".wasm")) {
        std.debug.print("Note: Running in interpreter mode. Use .aot for best performance.\n", .{});
    }

    if (!c.wasm_runtime_full_init(&init_args)) {
        std.debug.print("Failed to initialize WAMR runtime\n", .{});
        return;
    }
    defer {
        deinitComponentModel();
        c.wasm_runtime_destroy();
    }

    // Register host functions (WASI extensions)
    registerEdgeboxProcess();
    registerHostFunctions();

    // Load WASM file
    var error_buf: [256]u8 = undefined;
    const wasm_file = std.fs.cwd().openFile(wasm_path, .{}) catch |err| {
        std.debug.print("Failed to open {s}: {}\n", .{ wasm_path, err });
        return;
    };
    defer wasm_file.close();

    const wasm_size = wasm_file.getEndPos() catch 0;
    const wasm_buf = std.heap.page_allocator.alloc(u8, wasm_size) catch {
        std.debug.print("Failed to allocate memory for WASM\n", .{});
        return;
    };
    defer std.heap.page_allocator.free(wasm_buf);

    _ = wasm_file.readAll(wasm_buf) catch {
        std.debug.print("Failed to read WASM file\n", .{});
        return;
    };

    const load_time = std.time.nanoTimestamp();

    // Load module
    const module = c.wasm_runtime_load(wasm_buf.ptr, @intCast(wasm_size), &error_buf, error_buf.len);
    if (module == null) {
        std.debug.print("Failed to load module: {s}\n", .{&error_buf});
        return;
    }
    defer c.wasm_runtime_unload(module);

    const parse_time = std.time.nanoTimestamp();

    // Set WASI args before instantiation
    // dir_list: preopened directories from .edgebox.json
    var dir_list_z = std.ArrayListUnmanaged([*:0]const u8){};
    defer dir_list_z.deinit(allocator);

    for (config.dirs.items) |dir| {
        // Only preopen dirs with at least read permission
        if (!dir.read) continue;
        const dir_z = allocator.dupeZ(u8, dir.path) catch continue;
        dir_list_z.append(allocator, dir_z) catch {
            allocator.free(dir_z);
        };
    }

    if (show_debug) {
        std.debug.print("[DEBUG] Preopened dirs ({}):\n", .{dir_list_z.items.len});
        for (config.dirs.items) |dir| {
            const perms = [3]u8{
                if (dir.read) 'r' else '-',
                if (dir.write) 'w' else '-',
                if (dir.execute) 'x' else '-',
            };
            std.debug.print("  - {s} ({s})\n", .{ dir.path, &perms });
        }
    }

    // Build map_dir_list for WASI (guest_path::host_path mappings)
    // This maps virtual paths in WASM to real paths on host
    var map_dir_list_z = std.ArrayListUnmanaged([*:0]const u8){};
    defer map_dir_list_z.deinit(allocator);

    // Map /home/user to actual home directory (for Node.js-style apps)
    // WAMR requires: dir_list contains HOST paths, map_dir_list maps GUEST -> HOST
    if (std.posix.getenv("HOME")) |home| {
        // Add actual home to preopened dirs (the host path that exists)
        const home_z = allocator.dupeZ(u8, home) catch @panic("OOM");
        dir_list_z.append(allocator, home_z) catch {};

        // Map /home/user (guest) -> $HOME (host)
        const home_guest = "/home/user";
        const mapping_len = home_guest.len + 2 + home.len;
        const mapping_buf = allocator.allocSentinel(u8, mapping_len, 0) catch @panic("OOM");
        @memcpy(mapping_buf[0..home_guest.len], home_guest);
        mapping_buf[home_guest.len] = ':';
        mapping_buf[home_guest.len + 1] = ':';
        @memcpy(mapping_buf[home_guest.len + 2 ..][0..home.len], home);
        map_dir_list_z.append(allocator, mapping_buf.ptr) catch {};

        if (show_debug) {
            std.debug.print("[DEBUG] Dir mapping: {s} -> {s}\n", .{ home_guest, home });
        }
    }

    // Build env var list for WASI
    var env_list_z = std.ArrayListUnmanaged([*:0]const u8){};
    defer env_list_z.deinit(allocator);

    for (config.env_vars.items) |env| {
        const env_z = allocator.dupeZ(u8, env) catch continue;
        env_list_z.append(allocator, env_z) catch {
            allocator.free(env_z);
        };
    }

    if (show_debug) {
        std.debug.print("[DEBUG] Env vars ({}):\n", .{env_list_z.items.len});
        for (env_list_z.items) |env| {
            // Only show key, not value for security
            const env_str = std.mem.span(env);
            if (std.mem.indexOf(u8, env_str, "=")) |eq| {
                std.debug.print("  - {s}=...\n", .{env_str[0..eq]});
            }
        }
    }

    if (show_debug) {
        std.debug.print("[DEBUG] WASI args ({}):\n", .{args_list.items.len});
        for (args_list.items) |arg| {
            std.debug.print("  - {s}\n", .{std.mem.span(arg)});
        }
    }

    c.wasm_runtime_set_wasi_args(
        module,
        @ptrCast(dir_list_z.items.ptr), // dir_list
        @intCast(dir_list_z.items.len), // dir_count
        @ptrCast(map_dir_list_z.items.ptr), // map_dir_list
        @intCast(map_dir_list_z.items.len), // map_dir_count
        @ptrCast(env_list_z.items.ptr), // env
        @intCast(env_list_z.items.len), // env_count
        @ptrCast(args_list.items.ptr), // argv
        @intCast(args_list.items.len), // argc
    );

    // Instantiate module with configurable stack/heap/memory from .edgebox.json
    const stack_size = config.stack_size;
    const heap_size = config.heap_size;
    const max_memory_pages = config.max_memory_pages;
    const max_memory_mb = (max_memory_pages * 64) / 1024; // Each page = 64KB

    if (show_debug) std.debug.print("[DEBUG] Instantiating module (stack={d}MB, heap={d}MB, max_memory={d}MB)...\n", .{ stack_size / 1024 / 1024, heap_size / 1024 / 1024, max_memory_mb });

    // Use wasm_runtime_instantiate_ex to set max_memory_pages (WASM linear memory limit)
    const inst_args = c.InstantiationArgs{
        .default_stack_size = stack_size,
        .host_managed_heap_size = heap_size,
        .max_memory_pages = max_memory_pages,
    };
    const module_inst = c.wasm_runtime_instantiate_ex(module, &inst_args, &error_buf, error_buf.len);
    if (module_inst == null) {
        std.debug.print("Failed to instantiate: {s}\n", .{&error_buf});
        return;
    }
    defer c.wasm_runtime_deinstantiate(module_inst);
    if (show_debug) std.debug.print("[DEBUG] Module instantiated OK\n", .{});

    const inst_time = std.time.nanoTimestamp();

    // Create execution environment
    const exec_env = c.wasm_runtime_create_exec_env(module_inst, stack_size);
    if (exec_env == null) {
        std.debug.print("Failed to create exec env\n", .{});
        return;
    }
    defer c.wasm_runtime_destroy_exec_env(exec_env);

    // Set CPU instruction limit for interpreter mode (ignored in AOT mode)
    // -1 = unlimited, positive value = max instructions before termination
    if (config.max_instructions > 0) {
        c.wasm_runtime_set_instruction_count_limit(exec_env, config.max_instructions);
        if (show_debug) std.debug.print("[DEBUG] Instruction limit set to {d}\n", .{config.max_instructions});
    }

    // Find and call _start
    const start_func = c.wasm_runtime_lookup_function(module_inst, "_start");
    if (start_func == null) {
        std.debug.print("_start function not found\n", .{});
        return;
    }
    if (show_debug) std.debug.print("[DEBUG] Found _start, calling...\n", .{});

    // Setup CPU limits for AOT mode (instruction metering only works in interpreter)
    setupCpuLimitSignalHandlers();

    // Apply CPU time limit if configured (setrlimit - Unix only)
    const cpu_limit_set = setCpuTimeLimit(config.cpu_limit_seconds);
    if (cpu_limit_set and show_debug) {
        std.debug.print("[DEBUG] CPU time limit set to {d} seconds\n", .{config.cpu_limit_seconds});
    }

    // Setup watchdog thread for wall-clock timeout if configured
    var watchdog_ctx: WatchdogContext = undefined;
    var watchdog_thread: ?std.Thread = null;
    if (config.exec_timeout_ms > 0) {
        const timeout_ns: i128 = @as(i128, @intCast(config.exec_timeout_ms)) * std.time.ns_per_ms;
        watchdog_ctx = .{
            .deadline_ns = std.time.nanoTimestamp() + timeout_ns,
            .cancelled = std.atomic.Value(bool).init(false),
            .main_pid = getPid(),
        };
        watchdog_thread = std.Thread.spawn(.{}, watchdogThread, .{&watchdog_ctx}) catch null;
        if (watchdog_thread != null and show_debug) {
            std.debug.print("[DEBUG] Watchdog thread started, timeout {d}ms\n", .{config.exec_timeout_ms});
        }
    }

    const exec_start = std.time.nanoTimestamp();

    const call_ok = c.wasm_runtime_call_wasm(exec_env, start_func, 0, null);
    const exec_time = std.time.nanoTimestamp();

    // Cancel watchdog thread (execution completed)
    if (watchdog_thread) |thread| {
        watchdog_ctx.cancelled.store(true, .release);
        thread.join();
    }

    // Restore unlimited CPU time (for daemon mode reuse)
    if (cpu_limit_set) {
        restoreCpuTimeLimit();
    }

    if (!call_ok) {
        const exception = c.wasm_runtime_get_exception(module_inst);
        if (exception != null) {
            // proc_exit is normal, not an error
            const exc_str = std.mem.span(exception);
            if (std.mem.indexOf(u8, exc_str, "proc exit") == null) {
                std.debug.print("Exception: {s}\n", .{exception});
            }
        }
    }

    if (show_debug) {
        const load_ms = @as(f64, @floatFromInt(load_time - start)) / 1_000_000.0;
        const parse_ms = @as(f64, @floatFromInt(parse_time - load_time)) / 1_000_000.0;
        const inst_ms = @as(f64, @floatFromInt(inst_time - parse_time)) / 1_000_000.0;
        const exec_ms = @as(f64, @floatFromInt(exec_time - exec_start)) / 1_000_000.0;
        const total_ms = @as(f64, @floatFromInt(exec_time - start)) / 1_000_000.0;
        std.debug.print("\n[WAMR Debug] load: {d:.2}ms, parse: {d:.2}ms, instantiate: {d:.2}ms, exec: {d:.2}ms, total: {d:.2}ms\n", .{ load_ms, parse_ms, inst_ms, exec_ms, total_ms });
    }
}

// =============================================================================
// edgebox_process.* Host Functions
// EdgeBox process spawning API
// =============================================================================

fn getWasmMemory(exec_env: c.wasm_exec_env_t) ?[*]u8 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return null;
    // Get linear memory base address
    const mem = c.wasm_runtime_addr_app_to_native(module_inst, 0);
    return @ptrCast(mem);
}

fn readWasmString(exec_env: c.wasm_exec_env_t, ptr: u32, len: u32) ?[]const u8 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return null;
    // Validate bounds before accessing WASM memory
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, len)) return null;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return null;
    const slice: [*]const u8 = @ptrCast(native_ptr);
    return slice[0..len];
}

fn writeWasmBuffer(exec_env: c.wasm_exec_env_t, ptr: u32, data: []const u8) void {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return;
    // Security: Reject data larger than u32 max to prevent truncation in WAMR API
    if (data.len > 0xFFFFFFFF) return;
    // Validate bounds before writing to WASM memory
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, @intCast(data.len))) return;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return;
    const slice: [*]u8 = @ptrCast(native_ptr);
    @memcpy(slice[0..data.len], data);
}

/// edgebox_process_set_prog_name(name_ptr, name_len)
fn processSetProgName(exec_env: c.wasm_exec_env_t, name_ptr: u32, name_len: u32) void {
    const name = readWasmString(exec_env, name_ptr, name_len) orelse return;
    // Duplicate to ensure we own the memory
    process_state.program = allocator.dupe(u8, name) catch return;
}

/// edgebox_process_add_arg(arg_ptr, arg_len)
fn processAddArg(exec_env: c.wasm_exec_env_t, arg_ptr: u32, arg_len: u32) void {
    const arg = readWasmString(exec_env, arg_ptr, arg_len) orelse return;
    const duped = allocator.dupe(u8, arg) catch return;
    process_state.args.append(allocator, duped) catch return;
}

/// edgebox_process_add_env(key_ptr, key_len, val_ptr, val_len)
fn processAddEnv(exec_env: c.wasm_exec_env_t, key_ptr: u32, key_len: u32, val_ptr: u32, val_len: u32) void {
    const key = readWasmString(exec_env, key_ptr, key_len) orelse return;
    const val = readWasmString(exec_env, val_ptr, val_len) orelse return;
    const key_duped = allocator.dupe(u8, key) catch return;
    const val_duped = allocator.dupe(u8, val) catch return;
    process_state.env_vars.append(allocator, .{ .key = key_duped, .value = val_duped }) catch return;
}

/// edgebox_process_add_stdin(buf_ptr, buf_len)
fn processAddStdin(exec_env: c.wasm_exec_env_t, buf_ptr: u32, buf_len: u32) void {
    const data = readWasmString(exec_env, buf_ptr, buf_len) orelse return;
    process_state.stdin_data = allocator.dupe(u8, data) catch return;
}

/// edgebox_process_set_timeout(time_ms)
fn processSetTimeout(_: c.wasm_exec_env_t, time_ms: u32) void {
    process_state.timeout_ms = time_ms;
}

/// edgebox_process_run() -> i32 (0 = success, -1 = error)
fn processRun(_: c.wasm_exec_env_t) i32 {
    const program = process_state.program orelse return -1;

    // Check if OS-level sandbox is enabled
    const use_sandbox = std.posix.getenv("__EDGEBOX_DIRS") != null;

    // Build argv
    var argv = std.ArrayListUnmanaged([]const u8){};
    defer argv.deinit(allocator);

    if (use_sandbox) {
        // Wrap with edgebox-sandbox
        argv.append(allocator, "edgebox-sandbox") catch return -1;
    }

    // Add program
    argv.append(allocator, program) catch return -1;

    // Add args
    for (process_state.args.items) |arg| {
        argv.append(allocator, arg) catch return -1;
    }

    // Spawn child process using std.process.Child
    var child = std.process.Child.init(argv.items, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    // Note: Environment inheritance happens automatically
    // __EDGEBOX_DIRS and __EDGEBOX_COMMANDS are passed through

    child.spawn() catch |err| {
        std.debug.print("Process spawn failed: {}\n", .{err});
        return -1;
    };

    // Read stdout/stderr before wait (pipes must be drained first)
    var stdout_data: ?[]u8 = null;
    var stderr_data: ?[]u8 = null;
    var read_buf: [4096]u8 = undefined;

    if (child.stdout) |*stdout_file| {
        var stdout_list = std.ArrayListUnmanaged(u8){};
        while (true) {
            const n = stdout_file.read(&read_buf) catch break;
            if (n == 0) break;
            stdout_list.appendSlice(allocator, read_buf[0..n]) catch break;
        }
        if (stdout_list.items.len > 0) {
            // Return empty slice on allocation failure instead of null to prevent use-after-free
            stdout_data = stdout_list.toOwnedSlice(allocator) catch &.{};
        }
    }
    if (child.stderr) |*stderr_file| {
        var stderr_list = std.ArrayListUnmanaged(u8){};
        while (true) {
            const n = stderr_file.read(&read_buf) catch break;
            if (n == 0) break;
            stderr_list.appendSlice(allocator, read_buf[0..n]) catch break;
        }
        if (stderr_list.items.len > 0) {
            // Return empty slice on allocation failure instead of null to prevent use-after-free
            stderr_data = stderr_list.toOwnedSlice(allocator) catch &.{};
        }
    }

    // Wait for process to terminate
    const term = child.wait() catch |err| {
        std.debug.print("Process wait failed: {}\n", .{err});
        return -1;
    };

    // Store results
    process_state.exit_code = switch (term) {
        .Exited => |code| @intCast(code),
        .Signal => -1,
        .Stopped => -1,
        .Unknown => -1,
    };
    process_state.stdout_buf = stdout_data;
    process_state.stderr_buf = stderr_data;

    return 0;
}

/// edgebox_process_get_exit_code() -> i32
fn processGetExitCode(_: c.wasm_exec_env_t) i32 {
    return process_state.exit_code;
}

/// edgebox_process_get_stdout_len() -> u32
fn processGetStdoutLen(_: c.wasm_exec_env_t) u32 {
    return if (process_state.stdout_buf) |buf| @intCast(buf.len) else 0;
}

/// edgebox_process_get_stdout(buf_ptr)
fn processGetStdout(exec_env: c.wasm_exec_env_t, buf_ptr: u32) void {
    if (process_state.stdout_buf) |buf| {
        writeWasmBuffer(exec_env, buf_ptr, buf);
    }
}

/// edgebox_process_get_stderr_len() -> u32
fn processGetStderrLen(_: c.wasm_exec_env_t) u32 {
    return if (process_state.stderr_buf) |buf| @intCast(buf.len) else 0;
}

/// edgebox_process_get_stderr(buf_ptr)
fn processGetStderr(exec_env: c.wasm_exec_env_t, buf_ptr: u32) void {
    if (process_state.stderr_buf) |buf| {
        writeWasmBuffer(exec_env, buf_ptr, buf);
    }
}

// IMPORTANT: These must be global/static because WAMR retains references to them
var g_edgebox_process_symbols = [_]NativeSymbol{
    .{ .symbol = "edgebox_process_set_prog_name", .func_ptr = @ptrCast(@constCast(&processSetProgName)), .signature = "(ii)", .attachment = null },
    .{ .symbol = "edgebox_process_add_arg", .func_ptr = @ptrCast(@constCast(&processAddArg)), .signature = "(ii)", .attachment = null },
    .{ .symbol = "edgebox_process_add_env", .func_ptr = @ptrCast(@constCast(&processAddEnv)), .signature = "(iiii)", .attachment = null },
    .{ .symbol = "edgebox_process_add_stdin", .func_ptr = @ptrCast(@constCast(&processAddStdin)), .signature = "(ii)", .attachment = null },
    .{ .symbol = "edgebox_process_set_timeout", .func_ptr = @ptrCast(@constCast(&processSetTimeout)), .signature = "(i)", .attachment = null },
    .{ .symbol = "edgebox_process_run", .func_ptr = @ptrCast(@constCast(&processRun)), .signature = "()i", .attachment = null },
    .{ .symbol = "edgebox_process_get_exit_code", .func_ptr = @ptrCast(@constCast(&processGetExitCode)), .signature = "()i", .attachment = null },
    .{ .symbol = "edgebox_process_get_stdout_len", .func_ptr = @ptrCast(@constCast(&processGetStdoutLen)), .signature = "()i", .attachment = null },
    .{ .symbol = "edgebox_process_get_stdout", .func_ptr = @ptrCast(@constCast(&processGetStdout)), .signature = "(i)", .attachment = null },
    .{ .symbol = "edgebox_process_get_stderr_len", .func_ptr = @ptrCast(@constCast(&processGetStderrLen)), .signature = "()i", .attachment = null },
    .{ .symbol = "edgebox_process_get_stderr", .func_ptr = @ptrCast(@constCast(&processGetStderr)), .signature = "(i)", .attachment = null },
};

fn registerEdgeboxProcess() void {
    _ = c.wasm_runtime_register_natives("edgebox_process", &g_edgebox_process_symbols, g_edgebox_process_symbols.len);
}

// =============================================================================
// EdgeBox host functions - dispatch pattern
// =============================================================================

// Opcodes for dispatch functions
const FILE_OP_READ_START: i32 = 0;
const FILE_OP_WRITE_START: i32 = 1;
const FILE_OP_POLL: i32 = 2;
const FILE_OP_RESULT_LEN: i32 = 3;
const FILE_OP_RESULT: i32 = 4;
const FILE_OP_FREE: i32 = 5;

const SPAWN_OP_START: i32 = 0;
const SPAWN_OP_POLL: i32 = 1;
const SPAWN_OP_OUTPUT_LEN: i32 = 2;
const SPAWN_OP_OUTPUT: i32 = 3;
const SPAWN_OP_FREE: i32 = 4;

const HTTP_OP_REQUEST: i32 = 0;
const HTTP_OP_GET_RESPONSE_LEN: i32 = 1;
const HTTP_OP_GET_RESPONSE: i32 = 2;
const HTTP_OP_START_ASYNC: i32 = 3;
const HTTP_OP_POLL: i32 = 4;
const HTTP_OP_RESPONSE_LEN: i32 = 5;
const HTTP_OP_RESPONSE: i32 = 6;
const HTTP_OP_FREE: i32 = 7;

// Max concurrent async operations
const MAX_ASYNC_OPS: usize = 64;

// Async file operation state
const FileOpStatus = enum { pending, complete, error_state };
const FileOp = enum { read, write };

const AsyncFileRequest = struct {
    id: u32,
    op: FileOp,
    status: FileOpStatus,
    data: ?[]u8,
    error_msg: ?[]const u8,
    bytes_written: usize,
};

var g_file_ops: [MAX_ASYNC_OPS]?AsyncFileRequest = [_]?AsyncFileRequest{null} ** MAX_ASYNC_OPS;
var g_next_file_id: u32 = 1;

// Async spawn operation state
const SpawnOpStatus = enum { pending, complete, error_state };

const AsyncSpawnRequest = struct {
    id: u32,
    status: SpawnOpStatus,
    exit_code: i32,
    stdout_data: ?[]u8,
    stderr_data: ?[]u8,
};

var g_spawn_ops: [MAX_ASYNC_OPS]?AsyncSpawnRequest = [_]?AsyncSpawnRequest{null} ** MAX_ASYNC_OPS;
var g_next_spawn_id: u32 = 1;

// HTTP response state (sync) - protected by mutex for concurrent safety
var g_http_response: ?[]u8 = null;
var g_http_status: i32 = 0;
var g_http_mutex: std.Thread.Mutex = .{};

// Async HTTP operation state
const HttpOpStatus = enum { pending, complete, error_state };

const AsyncHttpRequest = struct {
    id: u32,
    status: HttpOpStatus,
    response: ?[]u8,
    http_status: u16,
    child: ?std.process.Child,
};

var g_http_ops: [MAX_ASYNC_OPS]?AsyncHttpRequest = [_]?AsyncHttpRequest{null} ** MAX_ASYNC_OPS;
var g_next_http_id: u32 = 1;

// Helper to read string from WASM memory
fn readWasmMemory(exec_env: c.wasm_exec_env_t, ptr: u32, len: u32) ?[]const u8 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return null;

    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, len)) return null;

    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return null;

    const bytes: [*]const u8 = @ptrCast(native_ptr);
    return bytes[0..len];
}

// Helper to write to WASM memory
fn writeWasmMemory(exec_env: c.wasm_exec_env_t, ptr: u32, data: []const u8) bool {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return false;

    // Security: Reject data larger than u32 max to prevent truncation in WAMR API
    if (data.len > 0xFFFFFFFF) return false;

    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, @intCast(data.len))) return false;

    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return false;

    const dest: [*]u8 = @ptrCast(native_ptr);
    @memcpy(dest[0..data.len], data);
    return true;
}

// =============================================================================
// File Dispatch Implementation
// =============================================================================

fn fileDispatch(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32, a4: i32) i32 {
    return switch (opcode) {
        // Legacy async file operations (0-5)
        FILE_OP_READ_START => fileReadStart(exec_env, @bitCast(a1), @bitCast(a2)),
        FILE_OP_WRITE_START => fileWriteStart(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4)),
        FILE_OP_POLL => filePoll(@bitCast(a1)),
        FILE_OP_RESULT_LEN => fileResultLen(@bitCast(a1)),
        FILE_OP_RESULT => fileResult(exec_env, @bitCast(a1), @bitCast(a2)),
        FILE_OP_FREE => fileFree(@bitCast(a1)),

        // Component Model sync file operations (100+)
        FILE_CM_READ => fileCmRead(exec_env, @bitCast(a1), @bitCast(a2)),
        FILE_CM_WRITE => fileCmWrite(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4)),
        FILE_CM_EXISTS => fileCmExists(exec_env, @bitCast(a1), @bitCast(a2)),
        FILE_CM_STAT => fileCmStat(exec_env, @bitCast(a1), @bitCast(a2)),
        FILE_CM_READDIR => fileCmReaddir(exec_env, @bitCast(a1), @bitCast(a2)),
        FILE_CM_MKDIR => fileCmMkdir(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3)),
        FILE_CM_UNLINK => fileCmUnlink(exec_env, @bitCast(a1), @bitCast(a2)),
        FILE_CM_RMDIR => fileCmRmdir(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3)),
        FILE_CM_RENAME => fileCmRename(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4)),
        FILE_CM_COPY => fileCmCopy(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4)),
        FILE_CM_GET_RESULT_LEN => fileCmGetResultLen(),
        FILE_CM_GET_RESULT => fileCmGetResult(exec_env, @bitCast(a1), @bitCast(a2)),

        else => -1,
    };
}

fn fileReadStart(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32) i32 {
    const raw_path = readWasmMemory(exec_env, path_ptr, path_len) orelse return -1;

    // Apply mount remapping (guest path -> host path)
    var owned_path: ?[]const u8 = null;
    defer if (owned_path) |p| allocator.free(p);
    const path = if (g_config) |config| blk: {
        if (config.remapPath(allocator, raw_path)) |remapped| {
            owned_path = remapped;
            break :blk remapped;
        }
        break :blk raw_path;
    } else raw_path;

    // Find free slot
    var slot_idx: ?usize = null;
    for (&g_file_ops, 0..) |*slot, i| {
        if (slot.* == null) {
            slot_idx = i;
            break;
        }
    }
    if (slot_idx == null) return -4; // Too many pending operations

    const request_id = g_next_file_id;
    g_next_file_id +%= 1;

    // Read file synchronously (async can be added later with threads)
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        g_file_ops[slot_idx.?] = AsyncFileRequest{
            .id = request_id,
            .op = .read,
            .status = .error_state,
            .data = null,
            .error_msg = switch (err) {
                error.FileNotFound => "ENOENT",
                error.AccessDenied => "EACCES",
                else => "EIO",
            },
            .bytes_written = 0,
        };
        return @intCast(request_id);
    };
    defer file.close();

    const content = file.readToEndAlloc(allocator, 100 * 1024 * 1024) catch {
        g_file_ops[slot_idx.?] = AsyncFileRequest{
            .id = request_id,
            .op = .read,
            .status = .error_state,
            .data = null,
            .error_msg = "EIO",
            .bytes_written = 0,
        };
        return @intCast(request_id);
    };

    g_file_ops[slot_idx.?] = AsyncFileRequest{
        .id = request_id,
        .op = .read,
        .status = .complete,
        .data = content,
        .error_msg = null,
        .bytes_written = content.len,
    };

    return @intCast(request_id);
}

fn fileWriteStart(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32, data_ptr: u32, data_len: u32) i32 {
    const raw_path = readWasmMemory(exec_env, path_ptr, path_len) orelse return -1;
    const data = readWasmMemory(exec_env, data_ptr, data_len) orelse return -2;

    // Apply mount remapping (guest path -> host path)
    var owned_path: ?[]const u8 = null;
    defer if (owned_path) |p| allocator.free(p);
    const path = if (g_config) |config| blk: {
        if (config.remapPath(allocator, raw_path)) |remapped| {
            owned_path = remapped;
            break :blk remapped;
        }
        break :blk raw_path;
    } else raw_path;

    // Find free slot
    var slot_idx: ?usize = null;
    for (&g_file_ops, 0..) |*slot, i| {
        if (slot.* == null) {
            slot_idx = i;
            break;
        }
    }
    if (slot_idx == null) return -4;

    const request_id = g_next_file_id;
    g_next_file_id +%= 1;

    // Create parent directories if needed
    if (std.mem.lastIndexOf(u8, path, "/")) |last_slash| {
        if (last_slash > 0) {
            const parent = path[0..last_slash];
            if (path[0] == '/') {
                std.fs.makeDirAbsolute(parent) catch {};
            } else {
                std.fs.cwd().makePath(parent) catch {};
            }
        }
    }

    // Write file
    const abs_file = if (path[0] == '/')
        std.fs.openFileAbsolute(path, .{ .mode = .write_only })
    else
        std.fs.cwd().openFile(path, .{ .mode = .write_only });

    const file = abs_file catch |err| {
        // Try to create the file
        const create_result = if (path[0] == '/')
            std.fs.createFileAbsolute(path, .{})
        else
            std.fs.cwd().createFile(path, .{});

        const created_file = create_result catch {
            g_file_ops[slot_idx.?] = AsyncFileRequest{
                .id = request_id,
                .op = .write,
                .status = .error_state,
                .data = null,
                .error_msg = switch (err) {
                    error.FileNotFound => "ENOENT",
                    error.AccessDenied => "EACCES",
                    else => "EIO",
                },
                .bytes_written = 0,
            };
            return @intCast(request_id);
        };

        created_file.writeAll(data) catch {
            created_file.close();
            g_file_ops[slot_idx.?] = AsyncFileRequest{
                .id = request_id,
                .op = .write,
                .status = .error_state,
                .data = null,
                .error_msg = "EIO",
                .bytes_written = 0,
            };
            return @intCast(request_id);
        };
        created_file.close();

        g_file_ops[slot_idx.?] = AsyncFileRequest{
            .id = request_id,
            .op = .write,
            .status = .complete,
            .data = null,
            .error_msg = null,
            .bytes_written = data.len,
        };
        return @intCast(request_id);
    };

    file.writeAll(data) catch {
        file.close();
        g_file_ops[slot_idx.?] = AsyncFileRequest{
            .id = request_id,
            .op = .write,
            .status = .error_state,
            .data = null,
            .error_msg = "EIO",
            .bytes_written = 0,
        };
        return @intCast(request_id);
    };
    file.close();

    g_file_ops[slot_idx.?] = AsyncFileRequest{
        .id = request_id,
        .op = .write,
        .status = .complete,
        .data = null,
        .error_msg = null,
        .bytes_written = data.len,
    };

    return @intCast(request_id);
}

fn filePoll(request_id: u32) i32 {
    for (&g_file_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                return switch (req.status) {
                    .pending => 0,
                    .complete => 1,
                    .error_state => -1,
                };
            }
        }
    }
    return -2; // Not found
}

fn fileResultLen(request_id: u32) i32 {
    for (&g_file_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.data) |data| {
                    return @intCast(data.len);
                }
                return 0;
            }
        }
    }
    return -1;
}

fn fileResult(exec_env: c.wasm_exec_env_t, request_id: u32, dest_ptr: u32) i32 {
    for (&g_file_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.data) |data| {
                    if (!writeWasmMemory(exec_env, dest_ptr, data)) {
                        return -1;
                    }
                    return @intCast(data.len);
                }
                return 0;
            }
        }
    }
    return -1;
}

fn fileFree(request_id: u32) i32 {
    for (&g_file_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.data) |data| {
                    allocator.free(data);
                }
                slot.* = null;
                return 0;
            }
        }
    }
    return -1;
}

// =============================================================================
// Spawn Dispatch Implementation
// =============================================================================

export fn __edgebox_spawn_dispatch(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32, a4: i32) i32 {
    const result = switch (opcode) {
        SPAWN_OP_START => spawnStart(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4)),
        SPAWN_OP_POLL => spawnPoll(@bitCast(a1)),
        SPAWN_OP_OUTPUT_LEN => spawnOutputLen(@bitCast(a1)),
        SPAWN_OP_OUTPUT => spawnOutput(exec_env, @bitCast(a1), @bitCast(a2)),
        SPAWN_OP_FREE => spawnFree(@bitCast(a1)),
        else => -1,
    };

    return result;
}

/// Extract command name from command string (e.g., "git status" -> "git")
fn extractCmdName(cmd: []const u8) []const u8 {
    // Skip leading whitespace
    var start: usize = 0;
    while (start < cmd.len and (cmd[start] == ' ' or cmd[start] == '\t')) : (start += 1) {}

    // Find end of command (space or end of string)
    var end = start;
    while (end < cmd.len and cmd[end] != ' ' and cmd[end] != '\t') : (end += 1) {}

    const full_cmd = cmd[start..end];

    // Extract basename (handle paths like /usr/bin/git)
    if (std.mem.lastIndexOfScalar(u8, full_cmd, '/')) |slash| {
        return full_cmd[slash + 1 ..];
    }
    return full_cmd;
}

/// Check if command is in list
fn cmdInList(cmd_name: []const u8, list: []const []const u8) bool {
    for (list) |allowed| {
        if (std.mem.eql(u8, cmd_name, allowed)) return true;
    }
    return false;
}

fn spawnStart(exec_env: c.wasm_exec_env_t, cmd_ptr: u32, cmd_len: u32, args_ptr: u32, args_len: u32) i32 {
    // Cleanup any stale JSON buffer from previous spawn (in case JS crashed without calling spawnFree)
    if (g_spawn_json_buf) |buf| {
        allocator.free(buf);
        g_spawn_json_buf = null;
    }

    // Security check: require execute permission on at least one directory
    const cfg = g_config orelse {
        std.debug.print("[SPAWN DENIED] No config loaded - shell access denied by default\n", .{});
        return -10; // Permission denied
    };
    if (!cfg.hasAnyExecute()) {
        std.debug.print("[SPAWN DENIED] No execute permission granted in .edgebox.json dirs\n", .{});
        return -10; // Permission denied
    }

    const cmd = readWasmMemory(exec_env, cmd_ptr, cmd_len) orelse return -1;
    const args_json = if (args_len > 0) readWasmMemory(exec_env, args_ptr, args_len) else null;

    // Command allow/deny filtering
    const cmd_name = extractCmdName(cmd);

    // Deny list takes precedence
    if (cfg.deny_commands.items.len > 0 and cmdInList(cmd_name, cfg.deny_commands.items)) {
        std.debug.print("[SPAWN DENIED] Command '{s}' is in deny list\n", .{cmd_name});
        return -11; // Command denied
    }

    // If allow list is set, command must be in it
    if (cfg.allow_commands.items.len > 0 and !cmdInList(cmd_name, cfg.allow_commands.items)) {
        std.debug.print("[SPAWN DENIED] Command '{s}' is not in allow list\n", .{cmd_name});
        return -12; // Command not allowed
    }

    // SECURITY LAYER 2: Command AST Parsing - Check blocked patterns
    if (cfg.blocked_patterns.len > 0 and isCommandDestructive(cmd, cfg.blocked_patterns)) {
        std.debug.print("[SPAWN DENIED] Command matches blocked pattern: {s}\n", .{cmd});
        return -20; // Destructive command blocked
    }

    // SECURITY LAYER 3: Sensitive File Access Check
    if (cfg.sensitive_files.len > 0 and accessesSensitiveFile(cmd, cfg.sensitive_files)) {
        std.debug.print("[SPAWN DENIED] Command accesses sensitive file: {s}\n", .{cmd});
        return -21; // Sensitive file access blocked
    }

    // EMULATOR LAYER: Check if command should be emulated
    const binary_name = extractBinaryName(cmd);
    const cmd_perm = findCommandPermission(cfg.commands, binary_name);
    if (cmd_perm) |perm| {
        if (perm.emulator_component) |emulator_name| {
            // Parse command arguments for emulator
            var emulator_args = std.ArrayListUnmanaged([]const u8){};
            defer emulator_args.deinit(allocator);

            // Parse command to extract args (skip the binary name)
            var tokenizer = std.mem.tokenizeScalar(u8, cmd, ' ');
            _ = tokenizer.next(); // Skip the command itself
            while (tokenizer.next()) |arg| {
                emulator_args.append(allocator, arg) catch continue;
            }

            // Try the emulator
            if (emulators.tryEmulate(emulator_name, emulator_args.items, perm.output_mode)) |result| {
                // Find free slot for emulated result
                var emulator_slot_idx: ?usize = null;
                for (&g_spawn_ops, 0..) |*slot, i| {
                    if (slot.* == null) {
                        emulator_slot_idx = i;
                        break;
                    }
                }
                if (emulator_slot_idx == null) return -4;

                const emulator_request_id = g_next_spawn_id;
                g_next_spawn_id +%= 1;

                // Store emulated result (need to dupe strings since they're comptime)
                const stdout_copy = allocator.dupe(u8, result.stdout) catch null;
                const stderr_copy = if (result.stderr.len > 0) allocator.dupe(u8, result.stderr) catch null else null;

                g_spawn_ops[emulator_slot_idx.?] = AsyncSpawnRequest{
                    .id = emulator_request_id,
                    .status = .complete,
                    .exit_code = result.exit_code,
                    .stdout_data = stdout_copy,
                    .stderr_data = stderr_copy,
                };

                std.debug.print("[SPAWN EMULATED] {s} -> {s} (exit={d})\n", .{ binary_name, emulator_name, result.exit_code });
                return @intCast(emulator_request_id);
            }
            // Emulator returned null - fall through to real execution
        }
    }

    // Find free slot
    var slot_idx: ?usize = null;
    for (&g_spawn_ops, 0..) |*slot, i| {
        if (slot.* == null) {
            slot_idx = i;
            break;
        }
    }
    if (slot_idx == null) return -4;

    const request_id = g_next_spawn_id;
    g_next_spawn_id +%= 1;

    // Debug: std.debug.print("[SPAWN START] id={d} cmd={s}\n", .{ request_id, cmd });

    // Build argument list - execute through shell for proper command parsing
    var argv = std.ArrayListUnmanaged([]const u8){};
    defer argv.deinit(allocator);

    // Always run through shell to handle command strings like "git remote get-url origin"
    argv.append(allocator, "/bin/sh") catch return -1;
    argv.append(allocator, "-c") catch return -1;
    argv.append(allocator, cmd) catch return -1;

    // Note: args_json is ignored for now since we're using shell execution
    _ = args_json;

    // Execute the process

    var child = std.process.Child.init(argv.items, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;
    child.stdin_behavior = .Close; // Close stdin immediately - hooks use `cat > /dev/null` which blocks on stdin

    // SECURITY LAYER 4: Credential Proxy - Inject credentials from config
    // (binary_name and cmd_perm already computed above for emulator check)
    if (cmd_perm) |perm| {
        if (perm.credentials) |creds| {
            // Copy current environment and add credentials
            var env_map = std.process.getEnvMap(allocator) catch null;
            if (env_map) |*em| {
                var cred_iter = creds.iterator();
                while (cred_iter.next()) |entry| {
                    em.put(entry.key_ptr.*, entry.value_ptr.*) catch {};
                }
                child.env_map = em;
                std.debug.print("[SPAWN] Injecting {d} credentials for '{s}'\n", .{ creds.count(), binary_name });
            }
        }
    }

    child.spawn() catch {
        g_spawn_ops[slot_idx.?] = AsyncSpawnRequest{
            .id = request_id,
            .status = .error_state,
            .exit_code = -1,
            .stdout_data = null,
            .stderr_data = null,
        };
        return @intCast(request_id);
    };

    // Read stdout/stderr and wait for completion (use if-unwrap instead of .? to avoid crash)
    const stdout = if (child.stdout) |stdout_file|
        stdout_file.readToEndAlloc(allocator, 10 * 1024 * 1024) catch null
    else
        null;
    const stderr = if (child.stderr) |stderr_file|
        stderr_file.readToEndAlloc(allocator, 10 * 1024 * 1024) catch null
    else
        null;

    const result = child.wait() catch {
        g_spawn_ops[slot_idx.?] = AsyncSpawnRequest{
            .id = request_id,
            .status = .error_state,
            .exit_code = -1,
            .stdout_data = stdout,
            .stderr_data = stderr,
        };
        return @intCast(request_id);
    };

    g_spawn_ops[slot_idx.?] = AsyncSpawnRequest{
        .id = request_id,
        .status = .complete,
        .exit_code = @intCast(result.Exited),
        .stdout_data = stdout,
        .stderr_data = stderr,
    };

    return @intCast(request_id);
}

fn spawnPoll(request_id: u32) i32 {
    for (&g_spawn_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                const result = switch (req.status) {
                    .pending => @as(i32, 0),
                    .complete => @as(i32, 1),
                    .error_state => @as(i32, -1),
                };

                return result;
            }
        }
    }

    return -2;
}

// Global buffer for spawn JSON output - needed because we return length first then data
var g_spawn_json_buf: ?[]u8 = null;
var g_spawn_json_id: u32 = 0;

fn spawnOutputLen(request_id: u32) i32 {
    // Debug: std.debug.print("[spawnOutputLen] request_id={}\n", .{request_id});

    // If we already have JSON for this request, return its length
    if (g_spawn_json_buf) |buf| {
        if (g_spawn_json_id == request_id) {
            // Debug: std.debug.print("[spawnOutputLen] cached len={}\n", .{buf.len});
            return @intCast(buf.len);
        }
        // Free previous buffer if different request
        allocator.free(buf);
        g_spawn_json_buf = null;
    }

    for (&g_spawn_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                // Build JSON response
                var json_buf = std.ArrayListUnmanaged(u8){};
                const writer = json_buf.writer(allocator);

                writer.print("{{\"exitCode\":{d},\"stdout\":\"", .{req.exit_code}) catch return -1;

                // Escape stdout
                if (req.stdout_data) |data| {
                    for (data) |ch| {
                        switch (ch) {
                            '"' => writer.writeAll("\\\"") catch {},
                            '\\' => writer.writeAll("\\\\") catch {},
                            '\n' => writer.writeAll("\\n") catch {},
                            '\r' => writer.writeAll("\\r") catch {},
                            '\t' => writer.writeAll("\\t") catch {},
                            else => {
                                if (ch < 0x20) {
                                    writer.print("\\u{x:0>4}", .{ch}) catch {};
                                } else {
                                    writer.writeByte(ch) catch {};
                                }
                            },
                        }
                    }
                }

                writer.writeAll("\",\"stderr\":\"") catch return -1;

                // Escape stderr
                if (req.stderr_data) |data| {
                    for (data) |ch| {
                        switch (ch) {
                            '"' => writer.writeAll("\\\"") catch {},
                            '\\' => writer.writeAll("\\\\") catch {},
                            '\n' => writer.writeAll("\\n") catch {},
                            '\r' => writer.writeAll("\\r") catch {},
                            '\t' => writer.writeAll("\\t") catch {},
                            else => {
                                if (ch < 0x20) {
                                    writer.print("\\u{x:0>4}", .{ch}) catch {};
                                } else {
                                    writer.writeByte(ch) catch {};
                                }
                            },
                        }
                    }
                }

                writer.writeAll("\"}") catch return -1;

                g_spawn_json_buf = json_buf.toOwnedSlice(allocator) catch return -1;
                g_spawn_json_id = request_id;

                return @intCast(g_spawn_json_buf.?.len);
            }
        }
    }
    return -1;
}

fn spawnOutput(exec_env: c.wasm_exec_env_t, request_id: u32, dest_ptr: u32) i32 {
    // Debug prints removed for clean output
    if (g_spawn_json_buf) |buf| {
        if (g_spawn_json_id == request_id) {
            if (!writeWasmMemory(exec_env, dest_ptr, buf)) {
                return -1;
            }
            return @intCast(buf.len);
        }
    }
    return -1;
}

fn spawnFree(request_id: u32) i32 {
    // Free JSON buffer if it was for this request
    if (g_spawn_json_buf) |buf| {
        if (g_spawn_json_id == request_id) {
            allocator.free(buf);
            g_spawn_json_buf = null;
        }
    }

    for (&g_spawn_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.stdout_data) |data| allocator.free(data);
                if (req.stderr_data) |data| allocator.free(data);
                slot.* = null;
                return 0;
            }
        }
    }
    return -1;
}

// =============================================================================
// Command Security Functions
// =============================================================================

// Dangerous commands that should be blocked when receiving piped input
const dangerous_pipe_targets = [_][]const u8{
    "tee", // Can write to files
    "xargs", // Can execute arbitrary commands
    "sh", // Shell execution
    "bash", // Shell execution
    "eval", // Arbitrary evaluation
    "exec", // Replace process
    "source", // Execute file
    ".", // Execute file (POSIX)
};

// Sensitive environment variables that should not be exposed
const sensitive_env_vars = [_][]const u8{
    "API_KEY",
    "ANTHROPIC_API_KEY",
    "AWS_SECRET_ACCESS_KEY",
    "AWS_SECRET",
    "GH_TOKEN",
    "GITHUB_TOKEN",
    "NPM_TOKEN",
    "DATABASE_URL",
    "DB_PASSWORD",
    "SECRET_KEY",
    "PRIVATE_KEY",
};

/// Check if command matches destructive patterns using AST-based parsing
/// Returns true if command should be blocked
fn isCommandDestructive(cmd: []const u8, blocked_patterns: []const []const u8) bool {
    // LAYER 1: Simple pattern matching (fast path for obvious cases)
    for (blocked_patterns) |pattern| {
        if (std.mem.indexOf(u8, cmd, pattern)) |_| {
            return true;
        }
    }

    // LAYER 2: AST-based analysis for complex cases
    var analysis = shell_parser.analyzeForSecurity(allocator, cmd) catch {
        // If parsing fails, block for safety
        return true;
    };
    defer analysis.deinit();

    // Block subshells by default (security risk - arbitrary code execution)
    if (analysis.has_subshell) {
        std.debug.print("[SECURITY] Blocking subshell execution in: {s}\n", .{cmd});
        return true;
    }

    // Check for dangerous pipe targets
    if (shell_parser.hasDangerousPipeTarget(&analysis, &dangerous_pipe_targets)) {
        std.debug.print("[SECURITY] Blocking dangerous pipe target in: {s}\n", .{cmd});
        return true;
    }

    // Check for sensitive variable usage (potential credential leak)
    if (shell_parser.usesSensitiveVariable(&analysis, &sensitive_env_vars)) {
        std.debug.print("[SECURITY] Blocking sensitive variable usage in: {s}\n", .{cmd});
        return true;
    }

    return false;
}

/// Check if command tries to access/write sensitive files using AST-based parsing
/// Returns true if command should be blocked
fn accessesSensitiveFile(cmd: []const u8, sensitive_files: []const []const u8) bool {
    // LAYER 1: Simple pattern matching for read access
    for (sensitive_files) |pattern| {
        // Remove glob wildcards for simple matching
        var clean_pattern = pattern;
        if (std.mem.startsWith(u8, pattern, "**/")) {
            clean_pattern = pattern[3..];
        }
        if (std.mem.endsWith(u8, clean_pattern, "/*")) {
            clean_pattern = clean_pattern[0 .. clean_pattern.len - 2];
        }

        if (std.mem.indexOf(u8, cmd, clean_pattern)) |_| {
            return true;
        }
    }

    // LAYER 2: AST-based analysis for redirects (write access)
    var analysis = shell_parser.analyzeForSecurity(allocator, cmd) catch {
        // If parsing fails, allow (already checked patterns above)
        return false;
    };
    defer analysis.deinit();

    // Check if command redirects output to sensitive files
    if (shell_parser.hasRedirectToSensitiveFile(&analysis, sensitive_files)) {
        std.debug.print("[SECURITY] Blocking redirect to sensitive file in: {s}\n", .{cmd});
        return true;
    }

    return false;
}

/// Find command permission by binary name
/// Returns null if not found
fn findCommandPermission(commands: []const runtime.CommandPermission, binary: []const u8) ?*const runtime.CommandPermission {
    for (commands) |*cmd| {
        if (std.mem.eql(u8, cmd.binary, binary)) {
            return cmd;
        }
    }
    return null;
}

/// Extract binary name from command string
/// Example: "gh pr list" -> "gh"
fn extractBinaryName(cmd: []const u8) []const u8 {
    // Find first space or end of string
    if (std.mem.indexOf(u8, cmd, " ")) |space_idx| {
        return cmd[0..space_idx];
    }
    return cmd;
}

// =============================================================================
// HTTP Dispatch Implementation
// =============================================================================

export fn __edgebox_http_dispatch(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32, a4: i32, a5: i32, a6: i32, a7: i32, a8: i32) i32 {
    const debug = std.process.getEnvVarOwned(allocator, "EDGEBOX_DEBUG") catch null;
    if (debug) |d| {
        std.debug.print("[httpDispatch] opcode={d}\n", .{opcode});
        if (opcode == HTTP_OP_REQUEST or opcode == HTTP_OP_START_ASYNC) {
            std.debug.print("  url_ptr(a1)={d} url_len(a2)={d}\n", .{ a1, a2 });
            std.debug.print("  method_ptr(a3)={d} method_len(a4)={d}\n", .{ a3, a4 });
            std.debug.print("  headers_ptr(a5)={d} headers_len(a6)={d}\n", .{ a5, a6 });
            std.debug.print("  body_ptr(a7)={d} body_len(a8)={d}\n", .{ a7, a8 });
        }
        allocator.free(d);
    }
    return switch (opcode) {
        HTTP_OP_REQUEST => httpRequest(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4), @bitCast(a5), @bitCast(a6), @bitCast(a7), @bitCast(a8)),
        HTTP_OP_GET_RESPONSE_LEN => httpGetResponseLen(),
        HTTP_OP_GET_RESPONSE => httpGetResponse(exec_env, @bitCast(a1)),
        HTTP_OP_START_ASYNC => httpStartAsync(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4), @bitCast(a5), @bitCast(a6), @bitCast(a7), @bitCast(a8)),
        HTTP_OP_POLL => httpPoll(@bitCast(a1)),
        HTTP_OP_RESPONSE_LEN => httpResponseLen(@bitCast(a1)),
        HTTP_OP_RESPONSE => httpGetAsyncResponse(exec_env, @bitCast(a1), @bitCast(a2)),
        HTTP_OP_FREE => httpFree(@bitCast(a1)),
        else => -1,
    };
}

// Wrapper that calculates body_len using strlen
fn httpRequestWithStrlen(exec_env: c.wasm_exec_env_t, url_ptr: u32, url_len: u32, method_ptr: u32, method_len: u32, headers_ptr: u32, headers_len: u32, body_ptr: u32) i32 {
    // Calculate body_len using strlen if body_ptr is non-zero
    var body_len: u32 = 0;
    if (body_ptr != 0) {
        const module_inst = c.wasm_runtime_get_module_inst(exec_env);
        if (module_inst != null) {
            // Read body as null-terminated string and get length
            const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, body_ptr);
            if (native_ptr != null) {
                const body_bytes: [*:0]const u8 = @ptrCast(native_ptr);
                body_len = @intCast(std.mem.len(body_bytes));
            }
        }
    }
    return httpRequest(exec_env, url_ptr, url_len, method_ptr, method_len, headers_ptr, headers_len, body_ptr, body_len);
}

fn httpRequest(exec_env: c.wasm_exec_env_t, url_ptr: u32, url_len: u32, method_ptr: u32, method_len: u32, headers_ptr: u32, headers_len: u32, body_ptr: u32, body_len: u32) i32 {
    const url = readWasmMemory(exec_env, url_ptr, url_len) orelse return -1;
    const method = if (method_len > 0) readWasmMemory(exec_env, method_ptr, method_len) else null;
    const headers_str = if (headers_len > 0) readWasmMemory(exec_env, headers_ptr, headers_len) else null;
    const body = if (body_len > 0) readWasmMemory(exec_env, body_ptr, body_len) else null;

    const debug = std.process.getEnvVarOwned(allocator, "EDGEBOX_DEBUG") catch null;
    const show_debug = debug != null;
    if (debug) |d| allocator.free(d);

    // Security check: URL allowlist (uses safe_fetch policy)
    if (!safe_fetch.isUrlAllowed(url, g_security_policy)) {
        if (show_debug) std.debug.print("[HTTP] URL blocked by security policy: {s}\n", .{url});
        return -403; // Forbidden
    }

    if (show_debug) {
        std.debug.print("[HTTP] Request to: {s}\n", .{url});
        std.debug.print("[HTTP] Body ptr={d} len={d}\n", .{ body_ptr, body_len });
        if (body) |b| {
            std.debug.print("[HTTP] Body content: {s}\n", .{b[0..@min(b.len, 200)]});
        } else {
            std.debug.print("[HTTP] Body is null\n", .{});
        }
    }

    // Free previous response (protected by mutex)
    g_http_mutex.lock();
    if (g_http_response) |resp| {
        allocator.free(resp);
        g_http_response = null;
    }
    g_http_mutex.unlock();

    // Determine HTTP method string
    const method_str = method orelse "GET";

    if (show_debug) std.debug.print("[HTTP] Method: {s}, URL: {s}\n", .{ method_str, url });

    // Build extra headers if provided (using h2.ExtraHeader format)
    var extra_headers = std.ArrayListUnmanaged(h2.ExtraHeader){};
    defer extra_headers.deinit(allocator);

    if (headers_str) |hdr| {
        if (show_debug) std.debug.print("[HTTP] Parsing headers from: {s}\n", .{hdr});
        // Parse headers from "Key: Value\r\nKey2: Value2" format
        var lines = std.mem.splitSequence(u8, hdr, "\r\n");
        while (lines.next()) |line| {
            if (std.mem.indexOf(u8, line, ": ")) |colon_pos| {
                const key = line[0..colon_pos];
                const value = line[colon_pos + 2 ..];
                if (show_debug) std.debug.print("[HTTP] Header: {s} = {s}\n", .{ key, value });
                extra_headers.append(allocator, .{ .name = key, .value = value }) catch {};
            }
        }
        if (show_debug) std.debug.print("[HTTP] Total headers parsed: {d}\n", .{extra_headers.items.len});
    }

    // Use metal0's h2 client for HTTP/1.1 and HTTP/2 with TLS support
    var client = h2.Client.init(allocator);
    defer client.deinit();

    if (show_debug) std.debug.print("[HTTP] Sending request via h2.Client...\n", .{});

    // Make request using h2 client (handles HTTP/1.1 for http://, HTTP/2 for https://)
    var response = client.request(method_str, url, extra_headers.items, body) catch |err| {
        if (show_debug) std.debug.print("[HTTP] Request error: {}\n", .{err});
        return -1;
    };
    defer response.deinit();

    // Copy response body (h2.Response owns memory, we need to dupe before deinit)
    const body_data = allocator.dupe(u8, response.body) catch |err| {
        if (show_debug) std.debug.print("[HTTP] Body copy error: {}\n", .{err});
        return -1;
    };

    // Store status and response (protected by mutex)
    g_http_mutex.lock();
    g_http_status = @intCast(response.status);
    g_http_response = body_data;
    g_http_mutex.unlock();

    if (show_debug) std.debug.print("[HTTP] Status: {d}, Response body length: {d}\n", .{ g_http_status, body_data.len });

    return @intCast(response.status);
}

fn httpGetResponseLen() i32 {
    g_http_mutex.lock();
    defer g_http_mutex.unlock();
    if (g_http_response) |resp| {
        return @intCast(resp.len);
    }
    return 0;
}

fn httpGetResponse(exec_env: c.wasm_exec_env_t, dest_ptr: u32) i32 {
    g_http_mutex.lock();
    defer g_http_mutex.unlock();
    if (g_http_response) |resp| {
        if (!writeWasmMemory(exec_env, dest_ptr, resp)) {
            return -1;
        }
        return @intCast(resp.len);
    }
    return 0;
}

// =============================================================================
// Async HTTP Functions
// =============================================================================

fn httpStartAsync(exec_env: c.wasm_exec_env_t, url_ptr: u32, url_len: u32, method_ptr: u32, method_len: u32, headers_ptr: u32, headers_len: u32, body_ptr: u32, body_len: u32) i32 {
    _ = headers_ptr;
    _ = headers_len;

    // Read URL from WASM memory
    const url = readWasmMemory(exec_env, url_ptr, url_len) orelse return -1;
    const method = readWasmMemory(exec_env, method_ptr, method_len) orelse "GET";
    const body = if (body_len > 0) readWasmMemory(exec_env, body_ptr, body_len) else null;

    // Find free slot
    var slot_idx: ?usize = null;
    for (g_http_ops, 0..) |op, i| {
        if (op == null) {
            slot_idx = i;
            break;
        }
    }
    if (slot_idx == null) return -4;

    // Allocate URL copy
    const url_copy = allocator.dupe(u8, url) catch return -5;

    // Build curl command
    var argv = std.ArrayListUnmanaged([]const u8){};
    argv.append(allocator, "curl") catch {
        allocator.free(url_copy);
        return -6;
    };
    argv.append(allocator, "-s") catch return -6;
    argv.append(allocator, "-S") catch return -6;
    argv.append(allocator, "-w") catch return -6;
    argv.append(allocator, "\n%{http_code}") catch return -6;
    argv.append(allocator, "-X") catch return -6;
    argv.append(allocator, method) catch return -6;

    if (body) |b| {
        argv.append(allocator, "-d") catch return -6;
        const body_copy = allocator.dupe(u8, b) catch return -6;
        argv.append(allocator, body_copy) catch return -6;
        argv.append(allocator, "-H") catch return -6;
        argv.append(allocator, "Content-Type: application/json") catch return -6;
    }

    argv.append(allocator, url_copy) catch return -6;

    // Spawn curl
    var child = std.process.Child.init(argv.items, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    child.spawn() catch {
        allocator.free(url_copy);
        return -7;
    };

    const request_id = g_next_http_id;
    g_next_http_id += 1;

    g_http_ops[slot_idx.?] = AsyncHttpRequest{
        .id = request_id,
        .status = .pending,
        .response = null,
        .http_status = 0,
        .child = child,
    };

    return @intCast(request_id);
}

fn httpPoll(request_id: u32) i32 {
    for (&g_http_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.status == .complete) return 1;
                if (req.status == .error_state) return -2;

                // Check if child process is done
                if (req.child) |*child| {
                    const result = child.wait() catch {
                        req.status = .error_state;
                        return -3;
                    };

                    // Read stdout
                    if (child.stdout) |stdout_file| {
                        const stdout = stdout_file.readToEndAlloc(allocator, 10 * 1024 * 1024) catch {
                            req.status = .error_state;
                            return -4;
                        };
                        defer allocator.free(stdout);

                        if (result.Exited != 0) {
                            req.status = .error_state;
                            return -5;
                        }

                        // Parse status code from curl output
                        var status_code: u16 = 0;
                        var response_body: []const u8 = stdout;

                        if (std.mem.lastIndexOf(u8, stdout, "\n")) |last_newline| {
                            response_body = stdout[0..last_newline];
                            const status_str = stdout[last_newline + 1 ..];
                            status_code = std.fmt.parseInt(u16, std.mem.trim(u8, status_str, " \n\r"), 10) catch 0;
                        }

                        // Build JSON response
                        var json_buf = std.ArrayListUnmanaged(u8){};
                        const writer = json_buf.writer(allocator);
                        writer.print("{{\"status\":{d},\"ok\":{s},\"body\":", .{
                            status_code,
                            if (status_code >= 200 and status_code < 300) "true" else "false",
                        }) catch {
                            req.status = .error_state;
                            return -6;
                        };

                        writer.writeByte('"') catch {};
                        for (response_body) |ch| {
                            switch (ch) {
                                '"' => writer.writeAll("\\\"") catch {},
                                '\\' => writer.writeAll("\\\\") catch {},
                                '\n' => writer.writeAll("\\n") catch {},
                                '\r' => writer.writeAll("\\r") catch {},
                                '\t' => writer.writeAll("\\t") catch {},
                                else => {
                                    if (ch < 0x20) {
                                        writer.print("\\u{x:0>4}", .{ch}) catch {};
                                    } else {
                                        writer.writeByte(ch) catch {};
                                    }
                                },
                            }
                        }
                        writer.writeByte('"') catch {};
                        writer.writeAll(",\"headers\":{}}") catch {};

                        req.response = json_buf.toOwnedSlice(allocator) catch null;
                        req.http_status = status_code;
                        req.status = .complete;
                        req.child = null;

                        return 1;
                    }
                }
                return 0; // Still pending
            }
        }
    }
    return -1; // Not found
}

fn httpResponseLen(request_id: u32) i32 {
    for (&g_http_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.response) |resp| {
                    return @intCast(resp.len);
                }
                return 0;
            }
        }
    }
    return -1;
}

fn httpGetAsyncResponse(exec_env: c.wasm_exec_env_t, request_id: u32, dest_ptr: u32) i32 {
    for (&g_http_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.response) |resp| {
                    if (writeWasmMemory(exec_env, dest_ptr, resp)) {
                        return @intCast(resp.len);
                    }
                }
                return 0;
            }
        }
    }
    return -1;
}

fn httpFree(request_id: u32) i32 {
    for (&g_http_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.response) |resp| {
                    allocator.free(resp);
                }
                slot.* = null;
                return 0;
            }
        }
    }
    return -1;
}

// =============================================================================
// Zlib Dispatch - Basic implementation
// =============================================================================

fn zlibDispatch(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32) i32 {
    _ = exec_env;
    _ = opcode;
    _ = a1;
    _ = a2;
    // Zlib compression not critical for Claude CLI - return error to use JS fallback
    return -1;
}

// =============================================================================
// File Dispatch - Component Model Integration (Phase 9b)
// =============================================================================

// Component Model file dispatch opcodes (100+ to avoid collision with async ops 0-5)
const FILE_CM_READ: i32 = 100;
const FILE_CM_WRITE: i32 = 101;
const FILE_CM_EXISTS: i32 = 102;
const FILE_CM_STAT: i32 = 103;
const FILE_CM_READDIR: i32 = 104;
const FILE_CM_MKDIR: i32 = 105;
const FILE_CM_UNLINK: i32 = 106;
const FILE_CM_RMDIR: i32 = 107;
const FILE_CM_RENAME: i32 = 108;
const FILE_CM_COPY: i32 = 109;
const FILE_CM_GET_RESULT_LEN: i32 = 110;
const FILE_CM_GET_RESULT: i32 = 111;

// File result buffer (single-threaded, reused between calls)
var g_file_cm_result: ?[]const u8 = null;
var g_file_cm_result_allocator: ?std.mem.Allocator = null;

// FS error codes (match Node.js conventions)
const FS_ERR_ENOENT: i32 = -2; // No such file or directory
const FS_ERR_EACCES: i32 = -3; // Permission denied
const FS_ERR_EEXIST: i32 = -4; // File already exists
const FS_ERR_ENOTDIR: i32 = -5; // Not a directory
const FS_ERR_EISDIR: i32 = -6; // Is a directory
const FS_ERR_ENOTEMPTY: i32 = -7; // Directory not empty
const FS_ERR_EIO: i32 = -8; // I/O error
const FS_ERR_EINVAL: i32 = -9; // Invalid argument

// Map Component Model fs-error enum to error codes
fn mapFsErrorToCode(err_val: u32) i32 {
    return switch (err_val) {
        0 => FS_ERR_ENOENT, // not_found
        1 => FS_ERR_EACCES, // permission_denied
        2 => FS_ERR_EEXIST, // already_exists
        3 => FS_ERR_EINVAL, // invalid_path
        4 => FS_ERR_ENOTDIR, // not_a_directory
        5 => FS_ERR_EISDIR, // not_a_file
        6 => FS_ERR_ENOTEMPTY, // directory_not_empty
        7 => FS_ERR_EIO, // io_error
        8 => FS_ERR_EINVAL, // invalid_encoding
        else => -1,
    };
}

// Component Model file operations - call registry.call("filesystem", ...)
fn fileCmRead(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32) i32 {
    const Value = @import("component/native_registry.zig").Value;
    const fs_alloc = g_file_cm_result_allocator orelse std.heap.page_allocator;
    const registry = &(g_component_registry orelse return -1);

    const path = readStringFromWasm(exec_env, path_ptr, path_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = path },
        Value{ .u32 = 0 }, // encoding = utf8
    };

    const result = registry.call("filesystem", "read-file", &args) catch return -1;

    if (result.isOk()) {
        const content = result.asOkString() catch return -1;
        if (g_file_cm_result) |old| {
            fs_alloc.free(old);
        }
        g_file_cm_result = fs_alloc.dupe(u8, content) catch return -1;
        return 0;
    } else {
        const err_val = result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn fileCmWrite(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32, data_ptr: u32, data_len: u32) i32 {
    const Value = @import("component/native_registry.zig").Value;
    const registry = &(g_component_registry orelse return -1);

    const path = readStringFromWasm(exec_env, path_ptr, path_len) orelse return -1;
    const data = readStringFromWasm(exec_env, data_ptr, data_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = path },
        Value{ .string = data },
    };

    const result = registry.call("filesystem", "write-file", &args) catch return -1;

    if (result.isOk()) {
        return 0;
    } else {
        const err_val = result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn fileCmExists(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32) i32 {
    const Value = @import("component/native_registry.zig").Value;
    const registry = &(g_component_registry orelse return -1);

    const path = readStringFromWasm(exec_env, path_ptr, path_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = path },
    };

    const result = registry.call("filesystem", "exists", &args) catch return 0;

    // exists returns a bool directly (not a result type)
    if (result == .bool) {
        return if (result.bool) 1 else 0;
    }
    return 0;
}

fn fileCmStat(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32) i32 {
    const Value = @import("component/native_registry.zig").Value;
    const fs_alloc = g_file_cm_result_allocator orelse std.heap.page_allocator;
    const registry = &(g_component_registry orelse return -1);

    const path = readStringFromWasm(exec_env, path_ptr, path_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = path },
    };

    const result = registry.call("filesystem", "stat", &args) catch return -1;

    if (result.isOk()) {
        // Serialize file-stat as binary: size(8) | mode(4) | is_file(1) | is_dir(1) | mtime(8) | ctime(8) | atime(8)
        const stat = result.asOkFileStat() catch return -1;
        var buf: [38]u8 = undefined;

        // size (8 bytes, little endian)
        std.mem.writeInt(i64, buf[0..8], @intCast(stat.size), .little);

        // mode (4 bytes, little endian) - compute from is_file/is_directory
        const mode: i32 = if (stat.is_directory) 0o40755 else 0o100644;
        std.mem.writeInt(i32, buf[8..12], mode, .little);

        // is_file (1 byte)
        buf[12] = if (stat.is_file) 1 else 0;

        // is_directory (1 byte)
        buf[13] = if (stat.is_directory) 1 else 0;

        // mtime (8 bytes, little endian)
        std.mem.writeInt(i64, buf[14..22], stat.modified_time, .little);

        // ctime (8 bytes, little endian)
        std.mem.writeInt(i64, buf[22..30], stat.created_time, .little);

        // atime (8 bytes, little endian)
        std.mem.writeInt(i64, buf[30..38], stat.accessed_time, .little);

        if (g_file_cm_result) |old| {
            fs_alloc.free(old);
        }
        g_file_cm_result = fs_alloc.dupe(u8, &buf) catch return -1;
        return 0;
    } else {
        const err_val = result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn fileCmReaddir(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32) i32 {
    const Value = @import("component/native_registry.zig").Value;
    const fs_alloc = g_file_cm_result_allocator orelse std.heap.page_allocator;
    const registry = &(g_component_registry orelse return -1);

    const path = readStringFromWasm(exec_env, path_ptr, path_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = path },
    };

    const result = registry.call("filesystem", "read-dir", &args) catch return -1;

    if (result.isOk()) {
        // Serialize directory entries as binary: count(4) | [len(4) | name(len)]...
        const entries = result.asOkDirEntries() catch return -1;
        defer fs_alloc.free(entries);

        var bin_list = std.ArrayListUnmanaged(u8){};
        defer bin_list.deinit(fs_alloc);

        // Write count (4 bytes)
        var count_buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &count_buf, @intCast(entries.len), .little);
        bin_list.appendSlice(fs_alloc, &count_buf) catch return -1;

        for (entries) |entry| {
            // Write name length (4 bytes)
            var len_buf: [4]u8 = undefined;
            std.mem.writeInt(u32, &len_buf, @intCast(entry.name.len), .little);
            bin_list.appendSlice(fs_alloc, &len_buf) catch return -1;

            // Write name
            bin_list.appendSlice(fs_alloc, entry.name) catch return -1;
            fs_alloc.free(entry.name);
        }

        if (g_file_cm_result) |old| {
            fs_alloc.free(old);
        }
        g_file_cm_result = bin_list.toOwnedSlice(fs_alloc) catch return -1;
        return 0;
    } else {
        const err_val = result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn fileCmMkdir(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32, recursive: u32) i32 {
    const Value = @import("component/native_registry.zig").Value;
    const registry = &(g_component_registry orelse return -1);

    const path = readStringFromWasm(exec_env, path_ptr, path_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = path },
        Value{ .bool = recursive != 0 },
    };

    const result = registry.call("filesystem", "mkdir", &args) catch return -1;

    if (result.isOk()) {
        return 0;
    } else {
        const err_val = result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn fileCmUnlink(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32) i32 {
    const Value = @import("component/native_registry.zig").Value;
    const registry = &(g_component_registry orelse return -1);

    const path = readStringFromWasm(exec_env, path_ptr, path_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = path },
    };

    const result = registry.call("filesystem", "remove-file", &args) catch return -1;

    if (result.isOk()) {
        return 0;
    } else {
        const err_val = result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn fileCmRmdir(exec_env: c.wasm_exec_env_t, path_ptr: u32, path_len: u32, recursive: u32) i32 {
    const Value = @import("component/native_registry.zig").Value;
    const registry = &(g_component_registry orelse return -1);

    const path = readStringFromWasm(exec_env, path_ptr, path_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = path },
        Value{ .bool = recursive != 0 },
    };

    const result = registry.call("filesystem", "remove-dir", &args) catch return -1;

    if (result.isOk()) {
        return 0;
    } else {
        const err_val = result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn fileCmRename(exec_env: c.wasm_exec_env_t, old_ptr: u32, old_len: u32, new_ptr: u32, new_len: u32) i32 {
    const Value = @import("component/native_registry.zig").Value;
    const registry = &(g_component_registry orelse return -1);

    const old_path = readStringFromWasm(exec_env, old_ptr, old_len) orelse return -1;
    const new_path = readStringFromWasm(exec_env, new_ptr, new_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = old_path },
        Value{ .string = new_path },
    };

    const result = registry.call("filesystem", "rename", &args) catch return -1;

    if (result.isOk()) {
        return 0;
    } else {
        const err_val = result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn fileCmCopy(exec_env: c.wasm_exec_env_t, src_ptr: u32, src_len: u32, dest_ptr: u32, dest_len: u32) i32 {
    const Value = @import("component/native_registry.zig").Value;
    const registry = &(g_component_registry orelse return -1);

    const src_path = readStringFromWasm(exec_env, src_ptr, src_len) orelse return -1;
    const dest_path = readStringFromWasm(exec_env, dest_ptr, dest_len) orelse return -1;

    const args = [_]Value{
        Value{ .string = src_path },
        Value{ .string = dest_path },
    };

    const result = registry.call("filesystem", "copy-file", &args) catch return -1;

    if (result.isOk()) {
        return 0;
    } else {
        const err_val = result.asErr() catch return -1;
        return mapFsErrorToCode(err_val);
    }
}

fn fileCmGetResultLen() i32 {
    if (g_file_cm_result) |file_result| {
        return @intCast(file_result.len);
    }
    return -1;
}

fn fileCmGetResult(exec_env: c.wasm_exec_env_t, out_ptr: u32, max_len: u32) i32 {
    _ = max_len;
    if (g_file_cm_result) |file_result| {
        if (writeStringToWasm(exec_env, out_ptr, file_result)) {
            return @intCast(file_result.len);
        }
    }
    return -1;
}

// =============================================================================
// Crypto Dispatch - Component Model Integration (Phase 9b)
// =============================================================================

// Crypto dispatch opcodes
const CRYPTO_OP_HASH: i32 = 0;
const CRYPTO_OP_HMAC: i32 = 1;
const CRYPTO_OP_RANDOM_BYTES: i32 = 2;
const CRYPTO_OP_GET_RESULT_LEN: i32 = 3;
const CRYPTO_OP_GET_RESULT: i32 = 4;

// Crypto result buffer (single-threaded, reused between calls)
var g_crypto_result: ?[]const u8 = null;
var g_crypto_result_allocator: ?std.mem.Allocator = null;

fn cryptoDispatch(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32, a4: i32, a5: i32, a6: i32) i32 {
    // Unused parameters in some branches - Zig requires explicit handling
    _ = a6;

    const crypto_alloc = g_crypto_result_allocator orelse std.heap.page_allocator;
    const Value = @import("component/native_registry.zig").Value;

    switch (opcode) {
        CRYPTO_OP_HASH => {
            // a1=algorithm (0=sha256, 1=sha384, 2=sha512, 3=sha1, 4=md5)
            // a2=data_ptr, a3=data_len
            // Returns: 0=success (result in buffer), -1=error
            const registry = &(g_component_registry orelse return -1);

            const data = readStringFromWasm(exec_env, @bitCast(a2), @bitCast(a3)) orelse return -1;

            const args = [_]Value{
                Value{ .u32 = @bitCast(a1) },
                Value{ .string = data },
            };

            const result = registry.call("crypto", "hash", &args) catch return -1;

            if (result.isOk()) {
                const hex_string = result.asOkString() catch return -1;
                // Store result for later retrieval
                if (g_crypto_result) |old| {
                    crypto_alloc.free(old);
                }
                g_crypto_result = crypto_alloc.dupe(u8, hex_string) catch return -1;
                return 0;
            } else {
                return -1;
            }
        },
        CRYPTO_OP_HMAC => {
            // a1=algorithm (0=sha256, 1=sha384, 2=sha512)
            // a2=key_ptr, a3=key_len, a4=data_ptr, a5=data_len
            // Returns: 0=success (result in buffer), -1=error
            const registry = &(g_component_registry orelse return -1);

            const key = readStringFromWasm(exec_env, @bitCast(a2), @bitCast(a3)) orelse return -1;
            const data = readStringFromWasm(exec_env, @bitCast(a4), @bitCast(a5)) orelse return -1;

            const args = [_]Value{
                Value{ .u32 = @bitCast(a1) },
                Value{ .string = key },
                Value{ .string = data },
            };

            const result = registry.call("crypto", "hmac", &args) catch return -1;

            if (result.isOk()) {
                const hex_string = result.asOkString() catch return -1;
                if (g_crypto_result) |old| {
                    crypto_alloc.free(old);
                }
                g_crypto_result = crypto_alloc.dupe(u8, hex_string) catch return -1;
                return 0;
            } else {
                return -1;
            }
        },
        CRYPTO_OP_RANDOM_BYTES => {
            // a1=size
            // Returns: 0=success (result in buffer), -1=error
            const registry = &(g_component_registry orelse return -1);

            const args = [_]Value{
                Value{ .u32 = @bitCast(a1) },
            };

            const result = registry.call("crypto", "random-bytes", &args) catch return -1;

            if (result.isOk()) {
                const bytes = result.asOkListU8() catch return -1;
                if (g_crypto_result) |old| {
                    crypto_alloc.free(old);
                }
                g_crypto_result = crypto_alloc.dupe(u8, bytes) catch return -1;
                return 0;
            } else {
                return -1;
            }
        },
        CRYPTO_OP_GET_RESULT_LEN => {
            // Returns: length of last result, or -1 if no result
            if (g_crypto_result) |crypto_result| {
                return @intCast(crypto_result.len);
            }
            return -1;
        },
        CRYPTO_OP_GET_RESULT => {
            // a1=out_ptr (WASM memory address to write result)
            // Returns: bytes written, or -1 if error
            if (g_crypto_result) |crypto_result| {
                if (writeStringToWasm(exec_env, @bitCast(a1), crypto_result)) {
                    return @intCast(crypto_result.len);
                }
            }
            return -1;
        },
        else => return -1,
    }
}

// Helper: Read string from WASM memory
fn readStringFromWasm(exec_env: c.wasm_exec_env_t, ptr: u32, len: u32) ?[]const u8 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return null;
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, len)) return null;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return null;
    const bytes: [*]const u8 = @ptrCast(native_ptr);
    return bytes[0..len];
}

// Helper: Write string to WASM memory
fn writeStringToWasm(exec_env: c.wasm_exec_env_t, ptr: u32, data: []const u8) bool {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return false;
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, @intCast(data.len))) return false;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return false;
    const dest: [*]u8 = @ptrCast(native_ptr);
    @memcpy(dest[0..data.len], data);
    return true;
}

// =============================================================================
// Process Dispatch - Component Model Integration (Phase 9b)
// =============================================================================

// Process Component Model dispatch opcodes (200+ to avoid collision)
const PROCESS_CM_SPAWN_SYNC: i32 = 200;
const PROCESS_CM_GET_RESULT_LEN: i32 = 201;
const PROCESS_CM_GET_RESULT: i32 = 202;

// Process result buffer - static to avoid allocation issues
// Max 64KB for JSON result (stdout + stderr + metadata)
var g_process_cm_result_buf: [65536]u8 = undefined;
var g_process_cm_result_len: usize = 0;

/// Map process error codes from Component Model to integer codes
fn mapProcessErrorToCode(err_code: u32) i32 {
    return switch (err_code) {
        0 => -10, // permission_denied
        1 => -2, // command_not_found
        2 => -3, // timeout
        3 => -4, // invalid_input
        4 => -5, // spawn_failed
        5 => -6, // operation_failed
        else => -1,
    };
}

/// Process Component Model dispatch function
fn processCmDispatch(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32, a4: i32, a5: i32, a6: i32, a7: i32) i32 {
    switch (opcode) {
        PROCESS_CM_SPAWN_SYNC => {
            // a1=cmd_ptr, a2=cmd_len, a3=args_json_ptr, a4=args_json_len,
            // a5=stdin_ptr, a6=stdin_len, a7=timeout_ms
            // Returns: 0=success (result in buffer), <0=error code
            return processCmSpawnSync(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4), @bitCast(a5), @bitCast(a6), @bitCast(a7));
        },
        PROCESS_CM_GET_RESULT_LEN => {
            if (g_process_cm_result_len > 0) {
                return @intCast(g_process_cm_result_len);
            }
            return -1;
        },
        PROCESS_CM_GET_RESULT => {
            // a1=out_ptr (WASM memory address to write result)
            if (g_process_cm_result_len > 0) {
                const result_slice = g_process_cm_result_buf[0..g_process_cm_result_len];
                if (writeStringToWasm(exec_env, @bitCast(a1), result_slice)) {
                    // Don't clear - static buffer stays valid until next spawn
                    return @intCast(g_process_cm_result_len);
                }
            }
            return -1;
        },
        else => return -1,
    }
}

/// Spawn sync via std.process.Child
/// Phase 9b: Direct process spawning without going through registry.call
/// Uses static buffer to avoid memory corruption between sequential calls
fn processCmSpawnSync(exec_env: c.wasm_exec_env_t, cmd_ptr: u32, cmd_len: u32, args_json_ptr: u32, args_json_len: u32, stdin_ptr: u32, stdin_len: u32, timeout_ms: u32) i32 {
    _ = stdin_ptr;
    _ = stdin_len;
    _ = timeout_ms;

    // Use page_allocator for temporary allocations (argv, stdout/stderr collection)
    const tmp_alloc = std.heap.page_allocator;

    const command = readStringFromWasm(exec_env, cmd_ptr, cmd_len) orelse return -1;

    // Parse args from JSON array format: ["arg1","arg2",...]
    var args_list = std.ArrayListUnmanaged([]const u8){};
    defer args_list.deinit(tmp_alloc);

    if (args_json_len > 2) {
        const args_json = readStringFromWasm(exec_env, args_json_ptr, args_json_len) orelse return -1;
        // Simple JSON array parsing - find strings between quotes
        var i: usize = 0;
        while (i < args_json.len) {
            // Find opening quote
            if (args_json[i] == '"') {
                const start = i + 1;
                i += 1;
                // Find closing quote (handle escaped quotes)
                while (i < args_json.len) {
                    if (args_json[i] == '"' and (i == start or args_json[i - 1] != '\\')) {
                        const arg = args_json[start..i];
                        args_list.append(tmp_alloc, arg) catch return -1;
                        break;
                    }
                    i += 1;
                }
            }
            i += 1;
        }
    }

    // Build argv: command + args
    var argv = std.ArrayListUnmanaged([]const u8){};
    defer argv.deinit(tmp_alloc);
    argv.append(tmp_alloc, command) catch return -1;
    for (args_list.items) |arg| {
        argv.append(tmp_alloc, arg) catch return -1;
    }

    // Spawn child process using std.process.Child
    var child = std.process.Child.init(argv.items, tmp_alloc);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    child.spawn() catch {
        return mapProcessErrorToCode(4); // spawn_failed
    };

    // Read stdout/stderr into temporary buffers
    var stdout_data: ?[]u8 = null;
    var stderr_data: ?[]u8 = null;
    defer if (stdout_data) |sd| tmp_alloc.free(sd);
    defer if (stderr_data) |sd| tmp_alloc.free(sd);
    var read_buf: [4096]u8 = undefined;

    if (child.stdout) |*stdout_file| {
        var stdout_list = std.ArrayListUnmanaged(u8){};
        defer stdout_list.deinit(tmp_alloc);
        while (true) {
            const n = stdout_file.read(&read_buf) catch break;
            if (n == 0) break;
            stdout_list.appendSlice(tmp_alloc, read_buf[0..n]) catch break;
        }
        if (stdout_list.items.len > 0) {
            stdout_data = stdout_list.toOwnedSlice(tmp_alloc) catch null;
        }
    }
    if (child.stderr) |*stderr_file| {
        var stderr_list = std.ArrayListUnmanaged(u8){};
        defer stderr_list.deinit(tmp_alloc);
        while (true) {
            const n = stderr_file.read(&read_buf) catch break;
            if (n == 0) break;
            stderr_list.appendSlice(tmp_alloc, read_buf[0..n]) catch break;
        }
        if (stderr_list.items.len > 0) {
            stderr_data = stderr_list.toOwnedSlice(tmp_alloc) catch null;
        }
    }

    // Wait for process to terminate
    const term = child.wait() catch {
        return mapProcessErrorToCode(5); // operation_failed
    };

    const exit_code: i32 = switch (term) {
        .Exited => |code| @intCast(code),
        .Signal => -1,
        .Stopped => -1,
        .Unknown => -1,
    };

    // Serialize JSON directly into static buffer
    // Format: {"exitCode":N,"stdout":"...","stderr":"..."}
    var fbs = std.io.fixedBufferStream(&g_process_cm_result_buf);
    const writer = fbs.writer();

    writer.writeAll("{\"exitCode\":") catch return -1;

    var exit_code_buf: [16]u8 = undefined;
    const exit_code_str = std.fmt.bufPrint(&exit_code_buf, "{d}", .{exit_code}) catch return -1;
    writer.writeAll(exit_code_str) catch return -1;

    writer.writeAll(",\"stdout\":\"") catch return -1;
    escapeJsonStringToWriter(writer, stdout_data orelse "") catch return -1;

    writer.writeAll("\",\"stderr\":\"") catch return -1;
    escapeJsonStringToWriter(writer, stderr_data orelse "") catch return -1;

    writer.writeAll("\"}") catch return -1;

    // Store length in global for GET_RESULT_LEN opcode
    g_process_cm_result_len = fbs.pos;

    return 0;
}

/// Escape special characters for JSON string (writes to a std.io.Writer)
fn escapeJsonStringToWriter(writer: anytype, str: []const u8) !void {
    for (str) |ch| {
        switch (ch) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => {
                if (ch < 0x20) {
                    // Control character - escape as \uXXXX
                    var hex_buf: [6]u8 = undefined;
                    const hex = std.fmt.bufPrint(&hex_buf, "\\u{x:0>4}", .{ch}) catch continue;
                    try writer.writeAll(hex);
                } else {
                    try writer.writeByte(ch);
                }
            },
        }
    }
}

// =============================================================================
// Socket Dispatch - Not needed for Claude CLI
// =============================================================================

fn socketDispatch(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32) i32 {
    _ = exec_env;
    _ = opcode;
    _ = a1;
    _ = a2;
    _ = a3;
    // Socket not needed for Claude CLI
    return -1;
}

// Global symbol arrays for EdgeBox host functions (WAMR retains references)
var g_http_symbols = [_]NativeSymbol{
    .{ .symbol = "http_dispatch", .func_ptr = @ptrCast(@constCast(&__edgebox_http_dispatch)), .signature = "(iiiiiiiii)i", .attachment = null },
};
var g_spawn_symbols = [_]NativeSymbol{
    .{ .symbol = "spawn_dispatch", .func_ptr = @ptrCast(@constCast(&__edgebox_spawn_dispatch)), .signature = "(iiiii)i", .attachment = null },
};
var g_file_symbols = [_]NativeSymbol{
    .{ .symbol = "file_dispatch", .func_ptr = @ptrCast(@constCast(&fileDispatch)), .signature = "(iiiii)i", .attachment = null },
};
var g_zlib_symbols = [_]NativeSymbol{
    .{ .symbol = "zlib_dispatch", .func_ptr = @ptrCast(@constCast(&zlibDispatch)), .signature = "(iii)i", .attachment = null },
};
var g_crypto_symbols = [_]NativeSymbol{
    .{ .symbol = "crypto_dispatch", .func_ptr = @ptrCast(@constCast(&cryptoDispatch)), .signature = "(iiiiiii)i", .attachment = null },
};
var g_socket_symbols = [_]NativeSymbol{
    .{ .symbol = "socket_dispatch", .func_ptr = @ptrCast(@constCast(&socketDispatch)), .signature = "(iiii)i", .attachment = null },
};
var g_process_cm_symbols = [_]NativeSymbol{
    .{ .symbol = "process_cm_dispatch", .func_ptr = @ptrCast(@constCast(&processCmDispatch)), .signature = "(iiiiiiii)i", .attachment = null },
};

/// Initialize Component Model registry and implementations
/// Called once during WAMR initialization to register Component Model interfaces
fn initComponentModel() void {
    if (g_component_initialized) {
        return; // Already initialized
    }

    // Initialize async runtime for true async HTTP/process operations
    const async_runtime = @import("component/async_runtime.zig");
    async_runtime.init(allocator);

    // Initialize Component Model registry
    g_component_registry = NativeRegistry.init(allocator);

    // Register timer implementation
    const timer_impl = @import("component/impls/timer_impl.zig");
    timer_impl.init(allocator);
    timer_impl.registerTimerImpl(&g_component_registry.?) catch |err| {
        std.debug.print("[Component Model] Failed to register timer implementation: {}\n", .{err});
        return;
    };

    // Register timer imports with WAMR (allows WASM to call timer::set-timeout, etc.)
    import_resolver.registerTimerImports(&g_component_registry.?);

    // Register filesystem implementation (Phase 8b)
    const filesystem_impl = @import("component/impls/filesystem_impl.zig");
    filesystem_impl.init(allocator);
    filesystem_impl.registerFilesystemImpl(&g_component_registry.?) catch |err| {
        std.debug.print("[Component Model] Failed to register filesystem implementation: {}\n", .{err});
        return;
    };

    // Register filesystem imports with WAMR (allows WASM to call filesystem::read-file, etc.)
    import_resolver.registerFilesystemImports(&g_component_registry.?);

    // Register crypto implementation (Phase 8b)
    const crypto_impl = @import("component/impls/crypto_impl.zig");
    crypto_impl.init(allocator);
    crypto_impl.registerCryptoImpl(&g_component_registry.?) catch |err| {
        std.debug.print("[Component Model] Failed to register crypto implementation: {}\n", .{err});
        return;
    };

    // Register crypto imports with WAMR (allows WASM to call crypto::hash, etc.)
    import_resolver.registerCryptoImports(&g_component_registry.?);

    // Register HTTP implementation (Phase 8b)
    const http_impl = @import("component/impls/http_impl.zig");
    http_impl.init(allocator);
    http_impl.registerHttpImpl(&g_component_registry.?) catch |err| {
        std.debug.print("[Component Model] Failed to register http implementation: {}\n", .{err});
        return;
    };

    // Register HTTP imports with WAMR (allows WASM to call http::fetch, etc.)
    import_resolver.registerHttpImports(&g_component_registry.?);

    // Register process imports with WAMR (Phase 8c)
    // Note: Process implementation runs directly in processCmSpawnSync using std.process.Child,
    // rather than going through registry.call (because process_impl.zig uses WASM-only externs).
    import_resolver.registerProcessImports(&g_component_registry.?);

    g_component_initialized = true;
}

/// Cleanup Component Model resources
fn deinitComponentModel() void {
    if (!g_component_initialized) {
        return;
    }

    // Deinit timer implementation
    const timer_impl = @import("component/impls/timer_impl.zig");
    timer_impl.deinit();

    // Deinit filesystem implementation (Phase 8b)
    const filesystem_impl = @import("component/impls/filesystem_impl.zig");
    filesystem_impl.deinit();

    // Deinit crypto implementation (Phase 8b)
    const crypto_impl = @import("component/impls/crypto_impl.zig");
    crypto_impl.deinit();

    // Deinit HTTP implementation (Phase 8b)
    const http_impl = @import("component/impls/http_impl.zig");
    http_impl.deinit();

    // Deinit process implementation
    const process_impl = @import("component/impls/process_impl.zig");
    process_impl.deinit();

    // Deinit async runtime (must be after HTTP/process impls which use it)
    const async_runtime = @import("component/async_runtime.zig");
    async_runtime.deinit();

    // Deinit registry
    if (g_component_registry) |*registry| {
        registry.deinit();
    }

    g_component_initialized = false;
}

fn registerHostFunctions() void {
    _ = c.wasm_runtime_register_natives("edgebox_http", &g_http_symbols, g_http_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_spawn", &g_spawn_symbols, g_spawn_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_file", &g_file_symbols, g_file_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_zlib", &g_zlib_symbols, g_zlib_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_crypto", &g_crypto_symbols, g_crypto_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_socket", &g_socket_symbols, g_socket_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_process_cm", &g_process_cm_symbols, g_process_cm_symbols.len);

    // WASI-style stdlib (Map, Array) - trusted host functions for high-performance data structures
    stdlib.registerStdlib();

    // Component Model interfaces (WASI Component Model)
    initComponentModel();

    // Note: WASI socket imports (sock_open, sock_connect, sock_getaddrinfo) will show
    // warnings because WAMR's WASI doesn't implement them. These are benign for apps
    // that don't use sockets.
}

// ============================================================================
// Security Helper Tests
// ============================================================================

test "isCommandDestructive - blocks git reset --hard" {
    const blocked = &[_][]const u8{
        "git reset --hard",
        "rm -rf",
        "> .env",
    };
    try std.testing.expect(isCommandDestructive("git reset --hard HEAD", blocked));
    try std.testing.expect(isCommandDestructive("git reset --hard", blocked));
    try std.testing.expect(!isCommandDestructive("git reset --soft", blocked));
    try std.testing.expect(!isCommandDestructive("git status", blocked));
}

test "isCommandDestructive - blocks rm -rf" {
    const blocked = &[_][]const u8{
        "rm -rf",
        "git reset --hard",
    };
    try std.testing.expect(isCommandDestructive("rm -rf /", blocked));
    try std.testing.expect(isCommandDestructive("rm -rf .", blocked));
    try std.testing.expect(!isCommandDestructive("rm file.txt", blocked));
    try std.testing.expect(!isCommandDestructive("rm -r dir", blocked));
}

test "isCommandDestructive - blocks shell redirects to .env" {
    const blocked = &[_][]const u8{
        "> .env",
        ">> .env",
    };
    try std.testing.expect(isCommandDestructive("echo foo > .env", blocked));
    try std.testing.expect(isCommandDestructive("echo bar >> .env", blocked));
    try std.testing.expect(!isCommandDestructive("cat .env", blocked));
    try std.testing.expect(!isCommandDestructive("echo foo > output.txt", blocked));
}

test "accessesSensitiveFile - blocks .env" {
    const sensitive = &[_][]const u8{
        ".env",
        "*.pem",
        "credentials.json",
    };
    try std.testing.expect(accessesSensitiveFile("cat .env", sensitive));
    try std.testing.expect(accessesSensitiveFile("less .env", sensitive));
    try std.testing.expect(!accessesSensitiveFile("cat README.md", sensitive));
}

test "accessesSensitiveFile - blocks wildcard patterns" {
    const sensitive = &[_][]const u8{
        "*.pem",
        "*.key",
    };
    try std.testing.expect(accessesSensitiveFile("cat server.pem", sensitive));
    try std.testing.expect(accessesSensitiveFile("cat private.key", sensitive));
    try std.testing.expect(!accessesSensitiveFile("cat config.json", sensitive));
}

test "accessesSensitiveFile - blocks credentials.json" {
    const sensitive = &[_][]const u8{
        "credentials.json",
        ".env",
    };
    try std.testing.expect(accessesSensitiveFile("cat credentials.json", sensitive));
    try std.testing.expect(!accessesSensitiveFile("cat package.json", sensitive));
}

test "extractBinaryName - extracts first word" {
    try std.testing.expectEqualStrings("git", extractBinaryName("git status"));
    try std.testing.expectEqualStrings("echo", extractBinaryName("echo hello world"));
    try std.testing.expectEqualStrings("ls", extractBinaryName("ls -la /tmp"));
    try std.testing.expectEqualStrings("cat", extractBinaryName("cat"));
}

test "extractBinaryName - handles quoted paths" {
    // Note: Current implementation is simple, doesn't handle quotes
    try std.testing.expectEqualStrings("/usr/bin/git", extractBinaryName("/usr/bin/git status"));
}
