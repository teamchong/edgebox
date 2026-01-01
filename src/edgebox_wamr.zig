/// EdgeBox WAMR Runtime - WASM runtime with daemon mode
///
/// Architecture (Cloudflare Workers style):
/// - Daemon auto-starts on first request, keeps WASM warm
/// - CoW memory reset between requests for fast execution
///
/// Usage:
///   edgebox <file.wasm|aot>   # Run via daemon (auto-starts if needed)
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

// Debug flag for daemon verbose logging (set to false for production)
const DAEMON_DEBUG = true;

fn daemonLog(comptime fmt: []const u8, args: anytype) void {
    if (DAEMON_DEBUG) {
        std.debug.print(fmt, args);
    }
}
const safe_fetch = @import("safe_fetch.zig");
const stdlib = @import("host/stdlib.zig");
const h2 = @import("h2");
const runtime = @import("runtime.zig");
const shell_parser = @import("shell_parser.zig");
const emulators = @import("component/emulators/mod.zig");
const async_loader = @import("async_loader.zig");
const module_cache = @import("module_cache.zig");
const cow_allocator = @import("cow_allocator.zig");
const errors = @import("errors.zig");
const config_mod = @import("config/mod.zig");
const wasm_helpers = @import("wasm_helpers.zig");
const json = @import("json.zig");

// Component Model support
const NativeRegistry = @import("component/native_registry.zig").NativeRegistry;
const import_resolver = @import("component/import_resolver.zig");
const wasm_component = @import("component/wasm_component.zig");

// GPU sandbox support
const gpu_sandbox = @import("gpu_sandbox.zig");

// Native HTTP server with kqueue/epoll
const http = @import("http/mod.zig");

// Extracted modules (Phase 4 integration)
const daemon = @import("daemon/mod.zig");
const dispatch = @import("dispatch/mod.zig");
const limits = @import("limits/mod.zig");
const http_bench = @import("tools/http_bench.zig");

// Use WAMR C API from wasm_helpers (shared types)
const c = wasm_helpers.c;

// Host function signatures for EdgeBox extensions
const NativeSymbol = c.NativeSymbol;

// =============================================================================
// Daemon Mode Configuration
// =============================================================================

const DEFAULT_HEAP_SIZE_MB: u32 = 64;

/// SECURITY: Get socket path using secure directory (XDG_RUNTIME_DIR preferred)
/// Falls back to /tmp with UID suffix to reduce collision risk
fn getSocketPath(alloc: std.mem.Allocator) ![]const u8 {
    // Prefer XDG_RUNTIME_DIR (per-user, tmpfs, auto-cleaned, mode 0700)
    if (std.posix.getenv("XDG_RUNTIME_DIR")) |xdg| {
        return std.fmt.allocPrint(alloc, "{s}/edgebox.sock", .{xdg});
    }
    // Fallback: /tmp with UID to reduce collision/attack risk
    const uid = std.c.getuid();
    return std.fmt.allocPrint(alloc, "/tmp/edgebox-{d}.sock", .{uid});
}

/// SECURITY: Create daemon socket with ownership verification
/// Checks socket ownership before deletion to detect TOCTOU attacks
fn createDaemonSocket(alloc: std.mem.Allocator) !struct { fd: std.posix.socket_t, path: []const u8 } {
    const socket_path = try getSocketPath(alloc);
    errdefer alloc.free(socket_path);

    const server = try std.posix.socket(std.posix.AF.UNIX, std.posix.SOCK.STREAM, 0);
    errdefer std.posix.close(server);

    // Convert to null-terminated path for C APIs
    const path_z = std.posix.toPosixPath(socket_path) catch return error.NameTooLong;

    // SECURITY: Check ownership before deleting existing socket using POSIX stat
    const my_uid = std.c.getuid();
    var stat_buf: std.c.Stat = undefined;
    if (std.c.stat(&path_z, &stat_buf) == 0) {
        if (stat_buf.uid != my_uid) {
            std.debug.print("[SECURITY] Socket owned by different user (uid={d}), refusing to use\n", .{stat_buf.uid});
            return error.SocketOwnedByOther;
        }
    }

    // Delete existing socket (now safe - we verified ownership)
    std.fs.cwd().deleteFile(socket_path) catch {};

    var addr: std.posix.sockaddr.un = std.mem.zeroes(std.posix.sockaddr.un);
    addr.family = std.posix.AF.UNIX;
    @memcpy(addr.path[0..socket_path.len], socket_path);

    try std.posix.bind(server, @ptrCast(&addr), @sizeOf(std.posix.sockaddr.un));

    // SECURITY: Set restrictive permissions on socket file (owner only)
    _ = std.c.chmod(&path_z, 0o600);

    return .{ .fd = server, .path = socket_path };
}

// sanitizePath moved to daemon/client.zig

/// Cached module entry - one per registered WASM/AOT file
const CachedModule = struct {
    module: c.wasm_module_t,
    wasm_buf: []const u8,
    memimg_path: []const u8,
    is_aot: bool,
    cow_initialized: bool,
};

/// Global module cache - maps absolute path to cached module
var g_module_cache: std.StringHashMap(CachedModule) = undefined;
var g_module_cache_initialized: bool = false;

/// Currently active module (for CoW allocator callbacks)
var g_active_module_path: ?[]const u8 = null;
var g_daemon_module: c.wasm_module_t = null;

/// Get memimg path from wasm/aot path (replace extension with .memimg)
fn getMemimgPath(wasm_path: []const u8, buf: []u8) ![]const u8 {
    const dot_idx = std.mem.lastIndexOf(u8, wasm_path, ".") orelse wasm_path.len;
    return std.fmt.bufPrint(buf, "{s}.memimg", .{wasm_path[0..dot_idx]}) catch error.BufferTooSmall;
}

// =============================================================================
// Custom Memory Allocator for WAMR
// NOTE: WASM_MEM_ALLOC_WITH_USAGE=0 by default, so signatures have NO usage param
// =============================================================================

/// Custom malloc for WAMR - wraps libc malloc
/// Signature: fn(size) -> ?*anyopaque
fn wamrMalloc(size: c_uint) callconv(.c) ?*anyopaque {
    const c_stdlib = @cImport(@cInclude("stdlib.h"));
    return c_stdlib.malloc(size);
}

/// Custom realloc for WAMR - wraps libc realloc
/// Signature: fn(ptr, size) -> ?*anyopaque
fn wamrRealloc(ptr: ?*anyopaque, size: c_uint) callconv(.c) ?*anyopaque {
    const c_stdlib = @cImport(@cInclude("stdlib.h"));
    return c_stdlib.realloc(ptr, size);
}

/// Custom free for WAMR - wraps libc free
/// Signature: fn(ptr) -> void
fn wamrFree(ptr: ?*anyopaque) callconv(.c) void {
    const c_stdlib = @cImport(@cInclude("stdlib.h"));
    c_stdlib.free(ptr);
}

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

// GPU sandbox instance (initialized if GPU is enabled in config)
var g_gpu_sandbox: ?*gpu_sandbox.GpuSandbox = null;
var g_gpu_allocator: ?std.mem.Allocator = null;


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
    stack_size: u32 = 2 * 1024 * 1024, // 2MB stack (WASM linear memory handles deep recursion)
    heap_size: u32 = 16 * 1024 * 1024, // 16MB host heap (WASM linear memory handles most allocations)
    max_memory_pages: u32 = 32768, // 2GB max linear memory (32768 * 64KB pages) - WASM32 limit
    // Note: WASM can dynamically grow memory via memory.grow, but cannot exceed max_memory_pages
    max_instructions: i32 = -1, // CPU limit: max WASM instructions per execution (-1 = unlimited)
    // Note: Instruction metering only works in interpreter mode (edgebox <file.wasm>)

    // Execution limits (setrlimit-based, works in both interpreter and AOT)
    exec_timeout_ms: u64 = 0, // Wall-clock timeout in ms (0 = unlimited, uses watchdog thread)
    cpu_limit_seconds: u32 = 0, // CPU time limit in seconds (0 = unlimited, uses setrlimit SIGXCPU)

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

    // Allocator selection: "system" (default) or "bump" (serverless)
    // - system: libc malloc, properly reclaims freed memory, good for long-running/sandbox
    // - bump: O(1) alloc, no-op free, memory reclaimed at process exit, good for <=15min serverless
    use_bump_allocator: bool = false,

    // Server listen permissions (for http.createServer, net.createServer)
    // Default: empty = no listen permission (off by default for security)
    listen_ports: std.ArrayListUnmanaged(u16) = .{}, // Allowed ports
    listen_any: bool = false, // Allow binding to any port (dangerous)

    fn deinit(self: *Config) void {
        self.listen_ports.deinit(allocator);
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

    /// Check if a port is allowed for listening
    fn canListenPort(self: *const Config, port: u16) bool {
        if (self.listen_any) return true;
        for (self.listen_ports.items) |allowed| {
            if (allowed == port) return true;
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

/// Load configuration using the config module, then apply edgebox-specific post-processing
/// wasm_path: Optional path to WASM file - config will be loaded from its directory
fn loadConfig(wasm_path: ?[]const u8) Config {
    var config = Config{};

    // Load config using the centralized config module
    // If wasm_path provided, search in its directory first
    var mod_config = config_mod.load(allocator, .{
        .sections = .runtime_only,
        .wasm_path = wasm_path,
    }) catch return config;

    // Copy from module's nested Config to our flat Config
    // Dirs
    for (mod_config.dirs.items) |dir| {
        config.dirs.append(allocator, DirPerms{
            .path = allocator.dupe(u8, dir.path) catch continue,
            .read = dir.read,
            .write = dir.write,
            .execute = dir.execute,
        }) catch {};
    }

    // Mounts
    for (mod_config.mounts.items) |mount| {
        config.mounts.append(allocator, Mount{
            .host = allocator.dupe(u8, mount.host) catch continue,
            .guest = allocator.dupe(u8, mount.guest) catch continue,
        }) catch {};
    }

    // Environment variables (from .env file)
    for (mod_config.env_vars.items) |env| {
        const env_copy = allocator.dupe(u8, env) catch continue;
        config.env_vars.append(allocator, env_copy) catch {
            allocator.free(env_copy);
        };
    }

    // Runtime config
    config.stack_size = mod_config.runtime.stack_size;
    config.heap_size = mod_config.runtime.heap_size;
    config.max_memory_pages = mod_config.runtime.max_memory_pages;
    config.max_instructions = mod_config.runtime.max_instructions;
    config.exec_timeout_ms = mod_config.runtime.exec_timeout_ms;
    config.cpu_limit_seconds = mod_config.runtime.cpu_limit_seconds;
    config.use_bump_allocator = mod_config.runtime.use_bump_allocator;

    // HTTP security
    for (mod_config.http.allowed_urls.items) |url| {
        const url_copy = allocator.dupe(u8, url) catch continue;
        config.allowed_urls.append(allocator, url_copy) catch {
            allocator.free(url_copy);
        };
    }
    for (mod_config.http.blocked_urls.items) |url| {
        const url_copy = allocator.dupe(u8, url) catch continue;
        config.blocked_urls.append(allocator, url_copy) catch {
            allocator.free(url_copy);
        };
    }
    config.rate_limit_rps = mod_config.http.rate_limit_rps;
    config.max_connections = mod_config.http.max_connections;

    // Command security
    for (mod_config.commands.allow_commands.items) |cmd| {
        const cmd_copy = allocator.dupe(u8, cmd) catch continue;
        config.allow_commands.append(allocator, cmd_copy) catch {
            allocator.free(cmd_copy);
        };
    }
    for (mod_config.commands.deny_commands.items) |cmd| {
        const cmd_copy = allocator.dupe(u8, cmd) catch continue;
        config.deny_commands.append(allocator, cmd_copy) catch {
            allocator.free(cmd_copy);
        };
    }
    config.use_keychain = mod_config.commands.use_keychain;

    // Server listen permissions
    for (mod_config.server.listen_ports.items) |port| {
        config.listen_ports.append(allocator, port) catch {};
    }
    config.listen_any = mod_config.server.listen_any;

    // Free the module config (we've copied what we need)
    mod_config.deinit(allocator);

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

    // Export allocator selection to WASM for early detection
    if (config.use_bump_allocator) {
        const alloc_env = allocator.dupe(u8, "__EDGEBOX_ALLOCATOR=bump") catch null;
        if (alloc_env) |env_str| {
            config.env_vars.append(allocator, env_str) catch {
                allocator.free(env_str);
            };
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
// Main Entry Point
// =============================================================================

pub fn main() !void {
    // Increase stack size for deep recursion in frozen functions
    if (builtin.os.tag == .macos or builtin.os.tag == .linux) {
        var limit = std.posix.getrlimit(.STACK) catch std.posix.rlimit{ .cur = 0, .max = 0 };
        const desired_stack: u64 = if (builtin.os.tag == .macos) 64 * 1024 * 1024 else 512 * 1024 * 1024;
        if (limit.cur < desired_stack) {
            const actual_desired = if (limit.max > 0 and desired_stack > limit.max) limit.max else desired_stack;
            limit.cur = actual_desired;
            _ = std.posix.setrlimit(.STACK, limit) catch {};
        }
    }

    // Parse args
    var args_iter = std.process.args();
    _ = args_iter.next(); // skip program name

    var is_daemon_server = false; // Internal: forked daemon process
    var is_up = false; // Pre-warm a module
    var is_down = false; // Unregister a module
    var is_exit = false; // Stop daemon
    var native_http_bench: bool = false; // Native HTTP benchmark mode
    var native_http_port: u16 = 8888; // Port for native HTTP benchmark
    var wasm_path: ?[]const u8 = null;
    var remaining_args = std.ArrayListUnmanaged([]const u8){};
    defer remaining_args.deinit(allocator);

    while (args_iter.next()) |arg| {
        if (std.mem.eql(u8, arg, "--daemon-server")) {
            // Internal flag: this process is the daemon server (forked)
            is_daemon_server = true;
        } else if (std.mem.eql(u8, arg, "up")) {
            // Pre-warm a module (load + create CoW snapshot)
            is_up = true;
        } else if (std.mem.eql(u8, arg, "down")) {
            // Unregister a module from daemon cache
            is_down = true;
        } else if (std.mem.eql(u8, arg, "exit")) {
            // Stop daemon gracefully
            is_exit = true;
        } else if (std.mem.eql(u8, arg, "run")) {
            // Explicit run command - just skip it, next arg is the file
            continue;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printUsage();
            return;
        } else if (std.mem.eql(u8, arg, "--native-http-bench")) {
            native_http_bench = true;
        } else if (std.mem.startsWith(u8, arg, "--port=")) {
            native_http_port = std.fmt.parseInt(u16, arg[7..], 10) catch 8888;
        } else if (wasm_path == null and !std.mem.startsWith(u8, arg, "-")) {
            wasm_path = arg;
        } else {
            remaining_args.append(allocator, arg) catch {};
        }
    }

    // --daemon-server runs the global daemon (no wasm path needed)
    if (is_daemon_server) {
        try runDaemonServer();
        return;
    }

    // --native-http-bench runs a native HTTP benchmark
    if (native_http_bench) {
        try http_bench.run(native_http_port);
        return;
    }

    // exit command doesn't need a path
    if (is_exit) {
        try daemon.exitDaemon(allocator);
        return;
    }

    const path = wasm_path orelse {
        printUsage();
        return;
    };

    if (is_up) {
        // Pre-warm: connect to daemon and ask it to load the module
        try daemon.warmupModule(allocator, path);
    } else if (is_down) {
        // Unregister: connect to daemon and remove from cache
        try daemon.downModule(allocator, path);
    } else {
        // Run: connect to daemon and execute the module
        try daemon.runDaemon(allocator, path, remaining_args.items);
    }
}

fn printUsage() void {
    std.debug.print(
        \\Usage: edgebox <file.wasm|aot> [args...]
        \\       edgebox run <file>        Run module (explicit)
        \\       edgebox up <file>         Warmup/register module
        \\       edgebox down <file>       Unregister module
        \\       edgebox exit              Stop daemon
        \\
        \\Runs WASM/AOT via daemon (auto-starts if needed).
        \\
        \\Commands:
        \\  <file>        Run the module (loads on first use)
        \\  run <file>    Same as above, explicit
        \\  up <file>     Pre-load module and create CoW snapshot
        \\  down <file>   Unregister module from daemon cache
        \\  exit          Stop daemon gracefully
        \\
        \\The daemon caches modules - first run loads, subsequent runs are instant.
        \\
        \\To compile JS to WASM:
        \\  edgeboxc build <app_dir>
        \\
    , .{});
}


// Client functions moved to daemon/client.zig
// Server uses: daemon.warmupModule, daemon.downModule, daemon.exitDaemon, daemon.runDaemon

/// Global daemon server - handles multiple WASM/AOT modules
/// Like Docker daemon: one process, multiple images cached
fn runDaemonServer() !void {
    // Load default config (from daemon's cwd)
    var config = loadConfig(null);
    defer config.deinit();
    g_config = &config;
    defer g_config = null;

    // Initialize emulator system
    emulators.init(allocator);
    defer emulators.deinit();

    // Initialize WAMR with custom allocator
    var init_args = std.mem.zeroes(c.RuntimeInitArgs);
    init_args.mem_alloc_type = c.Alloc_With_Allocator;
    init_args.mem_alloc_option.allocator.malloc_func = @constCast(@ptrCast(&wamrMalloc));
    init_args.mem_alloc_option.allocator.realloc_func = @constCast(@ptrCast(&wamrRealloc));
    init_args.mem_alloc_option.allocator.free_func = @constCast(@ptrCast(&wamrFree));
    if (!c.wasm_runtime_full_init(&init_args)) {
        daemonLog("[daemon] Failed to initialize WAMR runtime\n", .{});
        return;
    }
    defer c.wasm_runtime_destroy();

    // Ensure thread signal handlers are initialized (required for AOT)
    if (!c.wasm_runtime_init_thread_env()) {
        daemonLog("[daemon] Failed to initialize thread env\n", .{});
        return;
    }
    defer c.wasm_runtime_destroy_thread_env();

    // Register host functions
    registerEdgeboxProcess();
    registerHostFunctions();

    // Initialize component registry
    try wasm_component.initGlobalRegistry(allocator);
    defer {
        deinitComponentModel();
        wasm_component.deinitGlobalRegistry(allocator);
    }

    // Initialize module cache
    g_module_cache = std.StringHashMap(CachedModule).init(allocator);
    g_module_cache_initialized = true;
    defer {
        // Clean up all cached modules
        var it = g_module_cache.iterator();
        while (it.next()) |entry| {
            c.wasm_runtime_unload(entry.value_ptr.module);
            allocator.free(entry.key_ptr.*);
            allocator.free(entry.value_ptr.memimg_path);
        }
        g_module_cache.deinit();
        g_module_cache_initialized = false;
    }

    // SECURITY: Create socket with ownership verification and secure path
    const socket_info = try createDaemonSocket(allocator);
    defer std.posix.close(socket_info.fd);
    defer allocator.free(socket_info.path);

    try std.posix.listen(socket_info.fd, 128);

    daemonLog("[daemon] EdgeBox daemon started\n", .{});
    daemonLog("[daemon] Listening on {s}\n", .{socket_info.path});

    // Main accept loop
    while (true) {
        const client = std.posix.accept(socket_info.fd, null, null, 0) catch continue;
        const should_exit = handleClientRequest(client) catch |err| blk: {
            daemonLog("[daemon] Request error: {}\n", .{err});
            // Write error to client so benchmark can see it
            var err_buf: [256]u8 = undefined;
            const err_msg = std.fmt.bufPrint(&err_buf, "[daemon error] {}\n", .{err}) catch "[daemon error] unknown\n";
            _ = std.posix.write(client, err_msg) catch {};
            break :blk false;
        };
        std.posix.close(client);
        if (should_exit) {
            daemonLog("[daemon] Exiting\n", .{});
            break;
        }
    }
}

/// Read exactly n bytes from fd, looping until complete or error
fn readExact(fd: std.posix.fd_t, buf: []u8) !void {
    var total: usize = 0;
    while (total < buf.len) {
        const n = std.posix.read(fd, buf[total..]) catch |err| {
            if (err == error.WouldBlock) continue;
            return err;
        };
        if (n == 0) return error.EndOfStream;
        total += n;
    }
}

/// Handle a single client request. Returns true if daemon should exit.
fn handleClientRequest(client: std.posix.fd_t) !bool {
    // Read command prefix (4 bytes): "WARM", "DOWN", "EXIT", or path length for run
    var cmd_buf: [4]u8 = undefined;
    readExact(client, &cmd_buf) catch |err| {
        daemonLog("[daemon] Failed to read command: {}\n", .{err});
        return error.InvalidRequest;
    };
    daemonLog("[daemon] Read 4 bytes: 0x{x:0>2}{x:0>2}{x:0>2}{x:0>2}\n", .{
        cmd_buf[0], cmd_buf[1], cmd_buf[2], cmd_buf[3],
    });

    // EXIT command - stop daemon
    if (std.mem.eql(u8, &cmd_buf, "EXIT")) {
        const msg = "Daemon stopped\n";
        _ = std.posix.write(client, msg) catch {};
        return true; // Signal to exit
    }

    const is_warmup = std.mem.eql(u8, &cmd_buf, "WARM");
    const is_down = std.mem.eql(u8, &cmd_buf, "DOWN");

    var path_len: u32 = undefined;
    if (is_warmup or is_down) {
        // WARM/DOWN: read path length next
        var len_buf: [4]u8 = undefined;
        try readExact(client, &len_buf);
        path_len = std.mem.readInt(u32, &len_buf, .little);
    } else {
        // Run: cmd_buf already contains path length
        path_len = std.mem.readInt(u32, &cmd_buf, .little);
    }

    if (path_len > 4096) return error.PathTooLong;

    var path_buf: [4097]u8 = undefined; // +1 for newline
    try readExact(client, path_buf[0 .. path_len + 1]);

    const wasm_path = path_buf[0..path_len];

    // DOWN command - unregister module from cache
    if (is_down) {
        if (g_module_cache.fetchRemove(wasm_path)) |_| {
            daemonLog("[daemon] Unregistered module: {s}\n", .{wasm_path});
            const msg = "Module unregistered\n";
            _ = std.posix.write(client, msg) catch {};
        } else {
            const msg = "Module not found in cache\n";
            _ = std.posix.write(client, msg) catch {};
        }
        return false;
    }

    // Read args (protocol v2)
    var args_list = std.ArrayListUnmanaged([:0]const u8){};
    defer {
        for (args_list.items) |arg| allocator.free(arg);
        args_list.deinit(allocator);
    }

    if (!is_warmup) {
        var args_count_buf: [4]u8 = undefined;
        readExact(client, &args_count_buf) catch |err| {
            // Older clients may not send args - treat as 0 args
            daemonLog("[daemon] No args sent (older client?): {}\n", .{err});
            args_count_buf = std.mem.zeroes([4]u8);
        };
        const args_count = std.mem.readInt(u32, &args_count_buf, .little);

        if (args_count > 0 and args_count <= 256) {
            for (0..args_count) |_| {
                var arg_len_buf: [4]u8 = undefined;
                try readExact(client, &arg_len_buf);
                const arg_len = std.mem.readInt(u32, &arg_len_buf, .little);

                if (arg_len > 4096) return error.ArgTooLong;

                const arg_buf = try allocator.alloc(u8, arg_len + 1);
                errdefer allocator.free(arg_buf);

                try readExact(client, arg_buf[0..arg_len]);
                arg_buf[arg_len] = 0; // null terminate

                try args_list.append(allocator, arg_buf[0..arg_len :0]);
            }
        }
    }

    // Get or load cached module
    const cached = try getOrLoadModule(wasm_path);

    if (is_warmup) {
        // Warmup: just load the module, don't run
        const msg = "Module loaded and cached\n";
        _ = std.posix.write(client, msg) catch {};
    } else {
        // Execute directly without fork (Cloudflare Workers style)
        runModuleInstance(cached, wasm_path, args_list.items, client) catch |err| {
            daemonLog("[daemon] Execution error: {}\n", .{err});
        };
    }
    return false;
}

/// Get cached module or load and cache it
fn getOrLoadModule(wasm_path: []const u8) !*CachedModule {
    // Check cache first
    if (g_module_cache.getPtr(wasm_path)) |cached| {
        return cached;
    }

    // Not cached - load the module
    daemonLog("[daemon] Loading module: {s}\n", .{wasm_path});

    var error_buf: [256]u8 = undefined;

    // Load file - keep the loader alive so mmap stays valid
    const loader_ptr = try allocator.create(async_loader.AsyncLoader);
    loader_ptr.* = try async_loader.AsyncLoader.init(allocator, wasm_path);
    const wasm_buf = loader_ptr.loadSync() catch |err| {
        loader_ptr.deinit();
        allocator.destroy(loader_ptr);
        return err;
    };
    // Note: loader stays allocated to keep mmap valid. Freed when daemon exits.

    daemonLog("[daemon] Loaded {s} ({d:.1} MB)\n", .{ wasm_path, @as(f64, @floatFromInt(wasm_buf.len)) / (1024 * 1024) });

    // Detect AOT vs WASM
    const is_aot = wasm_buf.len >= 4 and wasm_buf[0] == 0 and wasm_buf[1] == 'a' and wasm_buf[2] == 'o' and wasm_buf[3] == 't';

    // For WASM (interpreter mode), don't pre-load the module - we'll load fresh each time
    // because QuickJS caches module state in linear memory.
    // For AOT, load once and use CoW to reset memory between runs.
    var module: c.wasm_module_t = null;
    if (is_aot) {
        module = c.wasm_runtime_load(@constCast(wasm_buf.ptr), @intCast(wasm_buf.len), &error_buf, error_buf.len);
        if (module == null) {
            const err_msg = std.mem.sliceTo(&error_buf, 0);
            daemonLog("[daemon] Failed to load AOT module: {s}\n", .{err_msg});
            return error.ModuleLoadFailed;
        }
    }

    // Create memimg path
    var memimg_buf: [4096]u8 = undefined;
    const memimg_path_slice = try getMemimgPath(wasm_path, &memimg_buf);
    const memimg_path = try allocator.dupe(u8, memimg_path_slice);

    // Store path key (needs to be owned by cache)
    const path_key = try allocator.dupe(u8, wasm_path);

    // Create cache entry
    const entry = CachedModule{
        .module = module, // null for WASM, valid for AOT
        .wasm_buf = wasm_buf,
        .memimg_path = memimg_path,
        .is_aot = is_aot,
        .cow_initialized = false,
    };

    try g_module_cache.put(path_key, entry);

    // Initialize CoW for both WASM and AOT (patch 003-cow-aot.patch enables AOT support)
    const cached_ptr = g_module_cache.getPtr(path_key).?;
    try initializeModuleCow(cached_ptr);

    return cached_ptr;
}

/// Initialize CoW snapshot for a module
fn initializeModuleCow(cached: *CachedModule) !void {
    if (cached.cow_initialized) return;

    daemonLog("[daemon] Initializing CoW for module...\n", .{});

    // Check if memimg already exists (from previous daemon run) - reuse if valid
    // Also check if AOT is newer than memimg (memimg needs regeneration after recompile)
    const memimg_exists = blk: {
        const memimg_file = std.fs.cwd().openFile(cached.memimg_path, .{}) catch break :blk false;
        defer memimg_file.close();
        const memimg_stat = memimg_file.stat() catch break :blk false;

        // Memimg must have reasonable size (> 1MB, our heap is 576MB)
        if (memimg_stat.size <= 1024 * 1024) break :blk false;

        // Derive AOT path from memimg path (replace .memimg with .aot)
        const memimg_path = cached.memimg_path;
        if (memimg_path.len < 7) break :blk false; // ".memimg" is 7 chars
        const base_path = memimg_path[0 .. memimg_path.len - 7];
        var aot_path_buf: [4096]u8 = undefined;
        const aot_path = std.fmt.bufPrint(&aot_path_buf, "{s}.aot", .{base_path}) catch break :blk false;

        // Check if AOT file is newer than memimg (needs regeneration)
        const aot_file = std.fs.cwd().openFile(aot_path, .{}) catch break :blk false;
        defer aot_file.close();
        const aot_stat = aot_file.stat() catch break :blk false;

        // If AOT is newer than memimg, memimg is stale
        if (aot_stat.mtime > memimg_stat.mtime) {
            daemonLog("[daemon] AOT is newer than memimg, regenerating snapshot\n", .{});
            break :blk false;
        }

        break :blk true;
    };

    if (memimg_exists) {
        daemonLog("[daemon] Reusing existing CoW snapshot: {s}\n", .{cached.memimg_path});
        cow_allocator.init(allocator, cached.memimg_path) catch |err| {
            daemonLog("[daemon] Failed to init CoW from existing file: {}\n", .{err});
            // Fall through to create new snapshot
        };
        if (cow_allocator.isAvailable()) {
            cached.cow_initialized = true;
            daemonLog("[daemon] Module ready (CoW mode, reused)\n", .{});
            return;
        }
    }

    // Close old CoW fd before regenerating memimg
    // This is critical: if memimg was regenerated (new inode), the old fd still points
    // to the deleted inode. mmap would use stale content, causing SIGSEGV.
    if (cow_allocator.isAvailable()) {
        daemonLog("[daemon] Closing stale CoW mapping before regeneration\n", .{});
        cow_allocator.deinit();
    }

    const stack_size: u32 = 64 * 1024;
    const heap_size: u32 = DEFAULT_HEAP_SIZE_MB * 1024 * 1024;
    var error_buf: [256]u8 = undefined;

    // NOTE: WASI args are NOT set here anymore to avoid conflict with runModuleInstance
    // which sets WASI args with the client socket fd. Setting args twice on the same
    // module caused SIGABRT during _start execution.

    // Create template instance
    const template_instance = c.wasm_runtime_instantiate(cached.module, stack_size, heap_size, &error_buf, error_buf.len);
    if (template_instance == null) {
        daemonLog("[daemon] Failed to create template instance: {s}\n", .{&error_buf});
        return error.InstanceCreationFailed;
    }

    const template_exec_env = c.wasm_runtime_create_exec_env(template_instance, stack_size);
    if (template_exec_env == null) {
        c.wasm_runtime_deinstantiate(template_instance);
        return error.ExecEnvCreationFailed;
    }

    // NOTE: We do NOT call _start during template init because:
    // 1. WAMR reapplies the data section during instantiation, resetting all globals
    // 2. Any state set here would be lost when the actual instance is created
    // 3. With -e flag, _start runs the FULL user program - we'd waste time running twice
    // CoW only provides fast memory allocation (mmap vs malloc), not state preservation.

    // PRE-GROW MEMORY: Enlarge memory NOW so CoW snapshot includes grown memory.
    // This avoids slow runtime memory.grow calls during actual execution.
    // QuickJS typically needs ~576MB for full initialization, so we pre-grow to heap_size.
    const memory_inst = c.wasm_runtime_get_memory(template_instance, 0);
    if (memory_inst != null) {
        const WASM_PAGE_SIZE: u64 = 65536; // 64KB per page
        const target_pages: u64 = (heap_size + WASM_PAGE_SIZE - 1) / WASM_PAGE_SIZE;
        const current_pages: u64 = c.wasm_memory_get_cur_page_count(memory_inst);

        if (target_pages > current_pages) {
            const pages_to_add: u32 = @intCast(target_pages - current_pages);
            daemonLog("[daemon] Pre-growing memory: {d} -> {d} pages ({d} MB -> {d} MB)\n", .{
                current_pages,
                target_pages,
                current_pages * WASM_PAGE_SIZE / (1024 * 1024),
                target_pages * WASM_PAGE_SIZE / (1024 * 1024),
            });

            // Use enlarge_memory to grow linearly (faster than runtime memory.grow)
            const grow_success = c.wasm_runtime_enlarge_memory(template_instance, pages_to_add);
            if (grow_success) {
                daemonLog("[daemon] Pre-grow succeeded\n", .{});
            } else {
                daemonLog("[daemon] Pre-grow failed (will use smaller snapshot)\n", .{});
            }
        }
    }

    // Capture CoW memory image
    daemonLog("[daemon] Capturing CoW snapshot to {s}\n", .{cached.memimg_path});
    cow_allocator.createMemoryImage(allocator, @ptrCast(template_instance), cached.memimg_path) catch |err| {
        daemonLog("[daemon] Failed to create memory image: {}\n", .{err});
        c.wasm_runtime_destroy_exec_env(template_exec_env);
        c.wasm_runtime_deinstantiate(template_instance);
        return err;
    };

    // Clean up template
    c.wasm_runtime_destroy_exec_env(template_exec_env);
    c.wasm_runtime_deinstantiate(template_instance);

    // Initialize CoW allocator for this module
    cow_allocator.init(allocator, cached.memimg_path) catch |err| {
        daemonLog("[daemon] Failed to init CoW allocator: {}\n", .{err});
        return err;
    };

    // Warmup phase: Do multiple instantiation cycles to stabilize WAMR's internal state.
    // Testing shows that the first instantiation has higher crash rate (~30%), but
    // subsequent ones are more stable. By doing 3 warmup cycles, we "prime" the runtime.
    // If any warmup fails, the module is likely unstable and we should fail early.
    const WARMUP_CYCLES: u32 = 3;
    daemonLog("[daemon] Running {} warmup instantiations...\n", .{WARMUP_CYCLES});
    for (0..WARMUP_CYCLES) |i| {
        const warmup_instance = c.wasm_runtime_instantiate(cached.module, stack_size, heap_size, &error_buf, error_buf.len);
        if (warmup_instance == null) {
            daemonLog("[daemon] Warmup {} failed: {s}\n", .{ i + 1, &error_buf });
            cow_allocator.deinit();
            return error.WarmupFailed;
        }
        c.wasm_runtime_deinstantiate(warmup_instance);
        daemonLog("[daemon] Warmup {}/{} OK\n", .{ i + 1, WARMUP_CYCLES });
    }

    cached.cow_initialized = true;
    daemonLog("[daemon] Module ready (CoW mode, {} warmup cycles passed)\n", .{WARMUP_CYCLES});
}

/// Run a module instance for a request
fn runModuleInstance(cached: *CachedModule, wasm_path: []const u8, args: []const [:0]const u8, client: std.posix.fd_t) !void {
    // Reload config from the wasm file's directory (this runs in forked child)
    var module_config = loadConfig(wasm_path);
    defer module_config.deinit();
    g_config = &module_config;
    defer g_config = null;

    // Reset socket state (inherited from parent daemon)
    dispatch.reset();

    const stack_size: u32 = 64 * 1024;
    const heap_size: u32 = DEFAULT_HEAP_SIZE_MB * 1024 * 1024;
    var error_buf: [256]u8 = undefined;

    // Both WASM and AOT use cached modules. Each instantiation creates fresh
    // linear memory from the data section, resetting QuickJS state.
    // For AOT, CoW provides additional optimization for memory reset.
    var module_to_use: c.wasm_module_t = undefined;

    if (cached.module != null) {
        module_to_use = cached.module;
    } else {
        // WASM: load module on first use (AOT is pre-loaded in getOrLoadModule)
        cached.module = c.wasm_runtime_load(@constCast(cached.wasm_buf.ptr), @intCast(cached.wasm_buf.len), &error_buf, error_buf.len);
        if (cached.module == null) {
            daemonLog("[daemon] Failed to load WASM: {s}\n", .{std.mem.sliceTo(&error_buf, 0)});
            return error.ModuleLoadFailed;
        }
        module_to_use = cached.module;
    }

    // Enable CoW allocator for fast memory allocation (both WASM and AOT)
    // WAMR patch 003-cow-aot.patch adds CoW support for AOT mode
    if (cached.cow_initialized) {
        cow_allocator.init(allocator, cached.memimg_path) catch |err| {
            daemonLog("[daemon] Failed to enable CoW: {}\n", .{err});
        };
    }

    // Set global module for CoW allocator callbacks
    g_daemon_module = module_to_use;
    g_active_module_path = wasm_path;
    defer {
        g_daemon_module = null;
        g_active_module_path = null;
    }

    // For AOT modules, make a fresh copy of the binary and load from it.
    // WAMR AOT loader may modify the buffer during relocation/patching,
    // which corrupts the mmap'd source for subsequent loads.
    var aot_copy: ?[]u8 = null;
    var fresh_module: c.wasm_module_t = null;
    defer {
        if (fresh_module != null) c.wasm_runtime_unload(fresh_module);
        if (aot_copy) |copy| allocator.free(copy);
    }

    if (cached.is_aot) {
        // Allocate and copy the AOT binary
        aot_copy = allocator.alloc(u8, cached.wasm_buf.len) catch {
            daemonLog("[daemon] Failed to allocate AOT copy\n", .{});
            return error.OutOfMemory;
        };
        @memcpy(aot_copy.?, cached.wasm_buf);

        fresh_module = c.wasm_runtime_load(aot_copy.?.ptr, @intCast(aot_copy.?.len), &error_buf, error_buf.len);
        if (fresh_module == null) {
            daemonLog("[daemon] Failed to load fresh AOT: {s}\n", .{std.mem.sliceTo(&error_buf, 0)});
            return error.ModuleLoadFailed;
        }
        module_to_use = fresh_module;
    }

    // Set WASI args for this execution
    const S = struct {
        var dir_list = [_][*:0]const u8{ ".", "/tmp" };
        var empty_env = [_][*:0]const u8{};
    };

    // Build WASI argv from args (max 64 args to keep stack bounded)
    var wasi_argv: [65][*:0]const u8 = undefined;
    wasi_argv[0] = "edgebox";
    const arg_count = @min(args.len, 64);
    for (0..arg_count) |i| {
        wasi_argv[i + 1] = args[i].ptr;
    }

    const client_fd: i64 = @intCast(client);
    c.wasm_runtime_set_wasi_args_ex(
        module_to_use,
        @ptrCast(&S.dir_list),
        S.dir_list.len,
        null,
        0,
        @ptrCast(&S.empty_env),
        S.empty_env.len,
        @ptrCast(&wasi_argv),
        arg_count + 1, // +1 for "edgebox"
        -1,
        client_fd,
        -1,
    );

    // Redirect OS stdout to client
    const saved_stdout = std.posix.dup(1) catch return error.DupFailed;
    std.posix.dup2(client, 1) catch {
        std.posix.close(saved_stdout);
        return error.Dup2Failed;
    };
    defer {
        std.posix.dup2(saved_stdout, 1) catch {};
        std.posix.close(saved_stdout);
    }

    // Create instance (uses CoW for AOT)
    const instance = c.wasm_runtime_instantiate(module_to_use, stack_size, heap_size, &error_buf, error_buf.len);
    if (instance == null) {
        daemonLog("[daemon] Failed to create instance: {s}\n", .{&error_buf});
        return error.InstanceCreationFailed;
    }
    defer c.wasm_runtime_deinstantiate(instance);

    const exec_env = c.wasm_runtime_create_exec_env(instance, stack_size);
    if (exec_env == null) {
        return error.ExecEnvCreationFailed;
    }
    defer c.wasm_runtime_destroy_exec_env(exec_env);

    // Call _start to run the WASM module
    // NOTE: Even with CoW, we call _start because WAMR reapplies the data section
    // during instantiation, resetting any global state set during template init.
    // CoW still helps by providing pre-allocated memory (fast mmap instead of allocation).
    const start_func = c.wasm_runtime_lookup_function(instance, "_start");
    if (start_func == null) {
        const msg = "[daemon error] _start function not found\n";
        _ = std.posix.write(client, msg) catch {};
        return;
    }

    daemonLog("[daemon] Calling _start...\n", .{});
    const success = c.wasm_runtime_call_wasm(exec_env, start_func, 0, null);
    daemonLog("[daemon] _start returned: success={}\n", .{success});

    // Check for errors (time-based limits use signals, no explicit check needed)
    if (!success) {
        const exception = c.wasm_runtime_get_exception(instance);
        if (exception != null) {
            const exc_str = std.mem.span(exception);
            // "wasi proc exit" is a normal exit, not an error
            if (std.mem.indexOf(u8, exc_str, "proc exit") == null) {
                var err_msg: [512]u8 = undefined;
                const err_str = std.fmt.bufPrint(&err_msg, "[daemon error] {s}\n", .{exception}) catch "[daemon error] unknown exception\n";
                _ = std.posix.write(client, err_str) catch {};
                // Check if this is a memory access error (likely WAMR bug)
                // These should trigger retry in the fork wrapper
                if (std.mem.indexOf(u8, exc_str, "out of bounds memory access") != null) {
                    return error.OutOfBoundsMemoryAccess;
                }
            }
        }
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

// Use shared WASM memory helpers
const safeWasmSlice = wasm_helpers.safeWasmSlice;
const readWasmString = wasm_helpers.readWasmMemory;
const writeWasmBuffer = wasm_helpers.writeWasmBuffer;
const readStringFromWasm = wasm_helpers.readWasmMemory;
const writeStringToWasm = wasm_helpers.writeWasmMemory;

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

    // Apply explicit environment variables from process_state
    if (process_state.env_vars.items.len > 0) {
        var env_map = std.process.getEnvMap(allocator) catch null;
        if (env_map) |*em| {
            for (process_state.env_vars.items) |env| {
                em.put(env.key, env.value) catch {};
            }
            child.env_map = em;
        }
    }

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

// Use shared JSON escape utility
const escapeJsonStringToWriter = json.escapeJsonStringToWriter;

// =============================================================================
// WASM Component Host Functions
// =============================================================================

/// Host function: Load WASM component and return component ID
/// Args: path (offset, len in WASM memory)
/// Returns: component ID (>0) or 0 on error
fn __edgebox_wasm_component_load(
    exec_env: c.wasm_exec_env_t,
    path_offset: i32,
    path_len: i32,
) callconv(.c) i32 {
    // SECURITY: Use safe bounds-checked memory access
    const path_slice = safeWasmSlice(exec_env, path_offset, path_len) orelse return 0;

    const registry = wasm_component.getGlobalRegistry() orelse {
        std.debug.print("[WASM Component] Registry not initialized\n", .{});
        return 0;
    };

    const component = registry.loadComponent(path_slice) catch |err| {
        // SECURITY: Sanitize path in error message
        std.debug.print("[WASM Component] Failed to load module: {}\n", .{err});
        return 0;
    };

    // Return component pointer as ID (validated via registry lookup on use)
    return @intCast(@intFromPtr(component));
}

/// Host function: Get export count from loaded component
/// Args: component_id
/// Returns: export count
fn __edgebox_wasm_component_export_count(
    _: c.wasm_exec_env_t,
    component_id: i32,
) callconv(.c) i32 {
    if (component_id <= 0) return 0;

    // SECURITY: Validate component_id is a registered component pointer
    const registry = wasm_component.getGlobalRegistry() orelse return 0;
    const component = registry.getComponentById(@intCast(component_id)) orelse return 0;

    return @intCast(component.exports.count());
}

/// Host function: Get export name by index
/// Args: component_id, index, name_buf_offset, name_buf_len
/// Returns: actual name length (0 if out of bounds)
fn __edgebox_wasm_component_export_name(
    exec_env: c.wasm_exec_env_t,
    component_id: i32,
    index: i32,
    name_buf_offset: i32,
    name_buf_len: i32,
) callconv(.c) i32 {
    if (component_id <= 0 or index < 0) return 0;

    // SECURITY: Use safe bounds-checked memory access
    const name_buf = safeWasmSlice(exec_env, name_buf_offset, name_buf_len) orelse return 0;

    // SECURITY: Validate component_id is a registered component pointer
    const registry = wasm_component.getGlobalRegistry() orelse return 0;
    const component = registry.getComponentById(@intCast(component_id)) orelse return 0;

    // Iterate to find the export at this index
    var iter = component.exports.iterator();
    var i: usize = 0;
    while (iter.next()) |entry| : (i += 1) {
        if (i == @as(usize, @intCast(index))) {
            const name = entry.key_ptr.*;
            const copy_len = @min(name.len, name_buf.len);
            @memcpy(name_buf[0..copy_len], name[0..copy_len]);
            return @intCast(name.len);
        }
    }

    return 0;
}

/// Host function: Call WASM component export
/// Args: component_id, func_name (offset, len), args (offset, count)
/// Returns: i32 result
fn __edgebox_wasm_component_call(
    exec_env: c.wasm_exec_env_t,
    component_id: i32,
    func_name_offset: i32,
    func_name_len: i32,
    args_offset: i32,
    args_count: i32,
) callconv(.c) i32 {
    if (component_id <= 0 or args_count < 0) return 0;

    // SECURITY: Use safe bounds-checked memory access for function name
    const func_name = safeWasmSlice(exec_env, func_name_offset, func_name_len) orelse return 0;

    // SECURITY: Validate component_id is a registered component pointer
    const registry = wasm_component.getGlobalRegistry() orelse return 0;
    const component = registry.getComponentById(@intCast(component_id)) orelse return 0;

    // Get args array with safe memory access
    var args: [16]i32 = undefined;
    const arg_count = @min(@as(usize, @intCast(args_count)), 16);
    if (arg_count > 0) {
        // SECURITY: Safe bounds-checked memory access for args (i32 array = 4 bytes per element)
        const args_bytes: i32 = @intCast(arg_count * 4);
        const args_mem = safeWasmSlice(exec_env, args_offset, args_bytes) orelse return 0;
        // Read i32 values safely (handles unaligned access)
        for (0..arg_count) |i| {
            const offset = i * 4;
            args[i] = std.mem.readInt(i32, args_mem[offset..][0..4], .little);
        }
    }

    const result = registry.callExport(component, func_name, args[0..arg_count]) catch |err| {
        std.debug.print("[WASM Component] Call failed: {}\n", .{err});
        return 0;
    };

    return result;
}

// =============================================================================
// Global symbol arrays for EdgeBox host functions (WAMR retains references)
// Note: http, spawn, file, crypto, socket symbols are registered via dispatch.registerAll()
var g_zlib_symbols = [_]NativeSymbol{
    .{ .symbol = "zlib_dispatch", .func_ptr = @ptrCast(@constCast(&zlibDispatch)), .signature = "(iii)i", .attachment = null },
};
var g_process_cm_symbols = [_]NativeSymbol{
    .{ .symbol = "process_cm_dispatch", .func_ptr = @ptrCast(@constCast(&processCmDispatch)), .signature = "(iiiiiiii)i", .attachment = null },
};
var g_wasm_component_symbols = [_]NativeSymbol{
    .{ .symbol = "wasm_component_load", .func_ptr = @ptrCast(@constCast(&__edgebox_wasm_component_load)), .signature = "(ii)i", .attachment = null },
    .{ .symbol = "wasm_component_export_count", .func_ptr = @ptrCast(@constCast(&__edgebox_wasm_component_export_count)), .signature = "(i)i", .attachment = null },
    .{ .symbol = "wasm_component_export_name", .func_ptr = @ptrCast(@constCast(&__edgebox_wasm_component_export_name)), .signature = "(iiii)i", .attachment = null },
    .{ .symbol = "wasm_component_call", .func_ptr = @ptrCast(@constCast(&__edgebox_wasm_component_call)), .signature = "(iiiii)i", .attachment = null },
};

// =============================================================================
// WebGPU host functions - GPU sandbox dispatch
// =============================================================================

/// Initialize GPU sandbox with given config
/// Called from daemon startup if GPU is enabled
pub fn initGpuSandbox(alloc: std.mem.Allocator, config: gpu_sandbox.GpuConfig) !void {
    if (g_gpu_sandbox != null) {
        return; // Already initialized
    }

    g_gpu_allocator = alloc;
    const sandbox = try alloc.create(gpu_sandbox.GpuSandbox);
    sandbox.* = try gpu_sandbox.GpuSandbox.init(alloc, config);
    g_gpu_sandbox = sandbox;

    daemonLog("[gpu] GPU sandbox initialized (enabled={})\n", .{config.enabled});
}

/// Deinitialize GPU sandbox
pub fn deinitGpuSandbox() void {
    if (g_gpu_sandbox) |sandbox| {
        sandbox.deinit();
        if (g_gpu_allocator) |alloc| {
            alloc.destroy(sandbox);
        }
        g_gpu_sandbox = null;
        g_gpu_allocator = null;
        daemonLog("[gpu] GPU sandbox deinitialized\n", .{});
    }
}

var g_gpu_symbols = [_]NativeSymbol{
    // GPU dispatch: (op, arg1, arg2, arg3, arg4, result_ptr, result_len) -> status
    // op: operation code (see GpuOp enum)
    // args: operation-specific parameters
    // result_ptr/len: where to write result data
    .{ .symbol = "gpu_dispatch", .func_ptr = @ptrCast(@constCast(&gpuDispatch)), .signature = "(iiiiiii)i", .attachment = null },
};

const GpuOp = enum(i32) {
    // Status
    is_available = 0,
    get_error = 1,

    // Device
    request_adapter = 10,
    request_device = 11,

    // Buffers
    create_buffer = 20,
    destroy_buffer = 21,
    write_buffer = 22,
    read_buffer = 23,

    // Shaders
    create_shader_module = 30,
    destroy_shader_module = 31,

    // Pipelines
    create_compute_pipeline = 40,
    create_bind_group = 41,

    // Execution
    dispatch_workgroups = 50,
    queue_submit = 51,
};

// GPU error codes (matches gpu_sandbox.ResponseCode)
const GPU_ERROR_NOT_AVAILABLE: i32 = -1;
const GPU_ERROR_INVALID_ARGS: i32 = -2;
const GPU_ERROR_DISPATCH_LIMIT: i32 = -4;
const GPU_ERROR_MEMORY_LIMIT: i32 = -6;
const GPU_ERROR_SHADER_INVALID: i32 = -8;

/// Read slice from WASM memory for GPU operations
fn gpuReadWasmSlice(exec_env: c.wasm_exec_env_t, ptr: u32, len: u32) ?[]const u8 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return null;
    if (len == 0) return &[_]u8{};
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, len)) return null;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return null;
    const bytes: [*]const u8 = @ptrCast(native_ptr);
    return bytes[0..len];
}

/// Write slice to WASM memory for GPU operations
fn gpuWriteWasmSlice(exec_env: c.wasm_exec_env_t, ptr: u32, data: []const u8) bool {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return false;
    if (data.len == 0) return true;
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, @intCast(data.len))) return false;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return false;
    const dest: [*]u8 = @ptrCast(native_ptr);
    @memcpy(dest[0..data.len], data);
    return true;
}

/// GPU dispatch - routes WebGPU calls to GPU sandbox
/// Returns: handle/status on success (>=0), negative error code on failure
fn gpuDispatch(
    exec_env: c.wasm_exec_env_t,
    op: i32,
    arg1: i32,
    arg2: i32,
    arg3: i32,
    arg4: i32,
    result_ptr: i32,
    result_len: i32,
) i32 {
    const operation: GpuOp = @enumFromInt(op);

    // Get sandbox (may be null if GPU disabled)
    const sandbox = g_gpu_sandbox orelse {
        // GPU not available - return 0 for is_available, -1 for others
        return if (operation == .is_available) 0 else GPU_ERROR_NOT_AVAILABLE;
    };

    switch (operation) {
        .is_available => {
            return if (sandbox.isAvailable()) 1 else 0;
        },

        .get_error => {
            // Error message retrieval (not implemented yet - would need error buffer)
            return 0;
        },

        .request_adapter, .request_device => {
            // GPU initialization is done automatically by sandbox.init()
            // These are no-ops if already initialized
            return if (sandbox.device_ready) 0 else GPU_ERROR_NOT_AVAILABLE;
        },

        .create_buffer => {
            // arg1 = size (low 32 bits), arg2 = size (high 32 bits for u64)
            // arg3 = usage flags
            const size: u64 = @as(u64, @intCast(@as(u32, @bitCast(arg1)))) |
                (@as(u64, @intCast(@as(u32, @bitCast(arg2)))) << 32);
            const usage: gpu_sandbox.GpuSandbox.BufferUsage = @enumFromInt(@as(u32, @bitCast(arg3)));

            const handle = sandbox.createBuffer(size, usage) catch |err| {
                daemonLog("[gpu] createBuffer failed: {}\n", .{err});
                return GPU_ERROR_MEMORY_LIMIT;
            };
            return @intCast(handle);
        },

        .destroy_buffer => {
            const handle: u32 = @intCast(@as(u32, @bitCast(arg1)));
            sandbox.destroyBuffer(handle) catch {};
            return 0;
        },

        .write_buffer => {
            // arg1 = buffer handle, arg2 = wasm_ptr, arg3 = len, arg4 = offset (low bits)
            const handle: u32 = @intCast(@as(u32, @bitCast(arg1)));
            const wasm_ptr: u32 = @intCast(@as(u32, @bitCast(arg2)));
            const len: u32 = @intCast(@as(u32, @bitCast(arg3)));
            const offset: u64 = @intCast(@as(u32, @bitCast(arg4)));

            const data = gpuReadWasmSlice(exec_env, wasm_ptr, len) orelse {
                return GPU_ERROR_INVALID_ARGS;
            };

            sandbox.writeBuffer(handle, offset, data) catch |err| {
                daemonLog("[gpu] writeBuffer failed: {}\n", .{err});
                return GPU_ERROR_MEMORY_LIMIT;
            };
            return 0;
        },

        .read_buffer => {
            // arg1 = buffer handle, arg2 = offset (low), arg3 = size, result_ptr = destination
            const handle: u32 = @intCast(@as(u32, @bitCast(arg1)));
            const offset: u64 = @intCast(@as(u32, @bitCast(arg2)));
            const size: u64 = @intCast(@as(u32, @bitCast(arg3)));

            const data = sandbox.readBuffer(handle, offset, size) catch |err| {
                daemonLog("[gpu] readBuffer failed: {}\n", .{err});
                return GPU_ERROR_MEMORY_LIMIT;
            };
            defer if (g_gpu_allocator) |alloc| alloc.free(data);

            const dest_ptr: u32 = @intCast(@as(u32, @bitCast(result_ptr)));
            if (!gpuWriteWasmSlice(exec_env, dest_ptr, data)) {
                return GPU_ERROR_INVALID_ARGS;
            }
            return @intCast(data.len);
        },

        .create_shader_module => {
            // arg1 = wgsl_ptr, arg2 = wgsl_len
            const wgsl_ptr: u32 = @intCast(@as(u32, @bitCast(arg1)));
            const wgsl_len: u32 = @intCast(@as(u32, @bitCast(arg2)));

            const wgsl = gpuReadWasmSlice(exec_env, wgsl_ptr, wgsl_len) orelse {
                return GPU_ERROR_INVALID_ARGS;
            };

            const handle = sandbox.createShaderModule(wgsl) catch |err| {
                daemonLog("[gpu] createShaderModule failed: {}\n", .{err});
                return GPU_ERROR_SHADER_INVALID;
            };
            return @intCast(handle);
        },

        .destroy_shader_module => {
            // Not exposed in current gpu_sandbox API - no-op
            return 0;
        },

        .create_compute_pipeline => {
            // arg1 = shader_handle, arg2 = entry_ptr, arg3 = entry_len
            const shader_handle: u32 = @intCast(@as(u32, @bitCast(arg1)));
            const entry_ptr: u32 = @intCast(@as(u32, @bitCast(arg2)));
            const entry_len: u32 = @intCast(@as(u32, @bitCast(arg3)));

            const entry_point = gpuReadWasmSlice(exec_env, entry_ptr, entry_len) orelse {
                return GPU_ERROR_INVALID_ARGS;
            };

            const handle = sandbox.createComputePipeline(shader_handle, entry_point) catch |err| {
                daemonLog("[gpu] createComputePipeline failed: {}\n", .{err});
                return GPU_ERROR_SHADER_INVALID;
            };
            return @intCast(handle);
        },

        .create_bind_group => {
            // Bind group creation not directly exposed - handled internally
            return GPU_ERROR_NOT_AVAILABLE;
        },

        .dispatch_workgroups => {
            // arg1 = x, arg2 = y, arg3 = z workgroups
            const x: u32 = @intCast(@as(u32, @bitCast(arg1)));
            const y: u32 = @intCast(@as(u32, @bitCast(arg2)));
            const z: u32 = @intCast(@as(u32, @bitCast(arg3)));

            sandbox.dispatch(x, y, z) catch |err| {
                daemonLog("[gpu] dispatch failed: {}\n", .{err});
                return GPU_ERROR_DISPATCH_LIMIT;
            };
            return 0;
        },

        .queue_submit => {
            // Queue submit is implicit in dispatch - no-op
            return 0;
        },
    }

    _ = result_len; // Unused in most operations
}

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
    // Use dispatch module for http, spawn, file, crypto, socket, gpu
    dispatch.registerAll();

    // Keep inline registrations for modules not yet in dispatch/
    _ = c.wasm_runtime_register_natives("edgebox_zlib", &g_zlib_symbols, g_zlib_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_process_cm", &g_process_cm_symbols, g_process_cm_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_wasm_component", &g_wasm_component_symbols, g_wasm_component_symbols.len);

    // WASI-style stdlib (Map, Array) - trusted host functions for high-performance data structures
    stdlib.registerStdlib();

    // Component Model interfaces (WASI Component Model)
    initComponentModel();

    // Initialize dispatch module with registry (needed for crypto/file component calls)
    if (g_component_registry) |*registry| {
        dispatch.init(allocator, registry);
    }

    // Note: WASI socket imports (sock_open, sock_connect, sock_getaddrinfo) will show
    // warnings because WAMR's WASI doesn't implement them. These are benign for apps
    // that don't use sockets.
}

// Note: Security helper tests moved to dispatch/spawn.zig
