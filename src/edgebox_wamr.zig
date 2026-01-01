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
const Config = config_mod.Config;
const DirPerms = config_mod.DirPerms;
const Mount = config_mod.Mount;
const wasm_helpers = @import("wasm_helpers.zig");
const json = @import("json.zig");

// Component Model support
const NativeRegistry = @import("component/native_registry.zig").NativeRegistry;
const import_resolver = @import("component/import_resolver.zig");
const wasm_component = @import("component/wasm_component.zig");
const component_host_functions = @import("component/host_functions.zig");

// Native HTTP server with kqueue/epoll
const http = @import("http/mod.zig");

// Extracted modules (Phase 4 integration)
const daemon = @import("daemon/mod.zig");
const dispatch = @import("dispatch/mod.zig");
const limits = @import("limits/mod.zig");
const http_bench = @import("tools/http_bench.zig");

// Process state from dispatch module (used by edgebox_process.* host functions)
const process_state = &dispatch.process.process_state;

// Use WAMR C API from wasm_helpers (shared types)
const c = wasm_helpers.c;

// Host function signatures for EdgeBox extensions
const NativeSymbol = c.NativeSymbol;

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
// Global State
// =============================================================================

const allocator = std.heap.page_allocator;

/// Global security policy for HTTP requests (loaded from .edgebox.json)
var g_security_policy: safe_fetch.SecurityPolicy = safe_fetch.SecurityPolicy.permissive;

/// Global config pointer for permission checks (set by run())
var g_config: ?*const Config = null;

/// Component Model registry and initialization state
var g_component_registry: ?NativeRegistry = null;
var g_component_initialized: bool = false;

// =============================================================================
// Configuration from .edgebox.json
// =============================================================================

// Config, DirPerms, Mount types are imported from config_mod (see imports above)

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

    // Runtime config (nested structure)
    config.runtime = mod_config.runtime;

    // HTTP security (nested structure)
    config.http = mod_config.http;

    // Command security (nested structure)
    config.commands = mod_config.commands;

    // Server listen permissions (nested structure)
    config.server = mod_config.server;

    // Free the module config (we've copied what we need)
    mod_config.deinit(allocator);

    // Load extended command security from runtime.EdgeBoxConfig (handles credentials, deny subcommands, etc.)
    // Note: runtime.CommandPermission and config.types.CommandPermission are different types
    // For now, just load sensitive_files and blocked_patterns
    const edge_config = runtime.EdgeBoxConfig.loadFromCwd(allocator);
    if (edge_config) |ec| {
        config.commands.sensitive_files = ec.sensitive_files;
        config.commands.blocked_patterns = ec.blocked_patterns;
    }

    // Try to get Claude API key from macOS keychain if enabled and not already set
    if (config.commands.use_keychain) {
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
    if (config.runtime.use_bump_allocator) {
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
        daemon.server.init(allocator);
        try daemon.server.runDaemonServer(
            loadConfig,
            dispatch.reset,
            registerHostFunctions,
            deinitComponentModel,
        );
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

// Daemon server implementation moved to daemon/server.zig
// edgebox_process.* host functions moved to dispatch/process.zig

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
// Global symbol arrays for EdgeBox host functions (WAMR retains references)
// Note: http, spawn, file, crypto, socket symbols are registered via dispatch.registerAll()
// Note: WASM Component symbols are registered via component/host_functions.zig
var g_zlib_symbols = [_]NativeSymbol{
    .{ .symbol = "zlib_dispatch", .func_ptr = @ptrCast(@constCast(&zlibDispatch)), .signature = "(iii)i", .attachment = null },
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
    // Use dispatch module for http, spawn, file, crypto, socket, gpu
    dispatch.registerAll();

    // Keep inline registrations for modules not yet in dispatch/
    _ = c.wasm_runtime_register_natives("edgebox_zlib", &g_zlib_symbols, g_zlib_symbols.len);
    component_host_functions.registerAll();

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
