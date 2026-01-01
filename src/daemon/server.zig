//! Daemon Server - Module caching and execution engine
//!
//! Architecture (Cloudflare Workers style):
//! - Daemon auto-starts on first request, keeps WASM warm
//! - CoW memory reset between requests for fast execution
//!
//! Handles:
//! - Module loading and caching
//! - CoW memory initialization
//! - Request handling (WARM, DOWN, EXIT, RUN)
//! - WASM execution with isolation

const std = @import("std");
const builtin = @import("builtin");
const client = @import("client.zig");

// Core runtime dependencies
const wasm_helpers = @import("../wasm_helpers.zig");
const c = wasm_helpers.c;
const async_loader = @import("../async_loader.zig");
const cow_allocator = @import("../cow_allocator.zig");
const emulators = @import("../component/emulators/mod.zig");
const wasm_component = @import("../component/wasm_component.zig");
const NativeRegistry = @import("../component/native_registry.zig").NativeRegistry;
const Config = @import("../config/mod.zig").Config;
const dispatch = @import("../dispatch/mod.zig");

// Debug flag for daemon verbose logging
const DAEMON_DEBUG = true;

fn daemonLog(comptime fmt: []const u8, args: anytype) void {
    if (DAEMON_DEBUG) {
        std.debug.print(fmt, args);
    }
}

// =============================================================================
// Configuration
// =============================================================================

pub const DEFAULT_HEAP_SIZE_MB: u32 = 64;

/// Cached module entry - one per registered WASM/AOT file
pub const CachedModule = struct {
    module: c.wasm_module_t,
    wasm_buf: []const u8,
    memimg_path: []const u8,
    is_aot: bool,
    cow_initialized: bool,
    /// Heap copy of AOT buffer for WAMR relocation patching (first load only)
    aot_heap_copy: ?[]u8 = null,
};

// =============================================================================
// Global State
// =============================================================================

var allocator: std.mem.Allocator = std.heap.page_allocator;

/// Global module cache - maps absolute path to cached module
var g_module_cache: std.StringHashMap(CachedModule) = undefined;
var g_module_cache_initialized: bool = false;

/// Currently active module (for CoW allocator callbacks)
var g_active_module_path: ?[]const u8 = null;
var g_daemon_module: c.wasm_module_t = null;

/// Config pointer (set before handling requests)
var g_config: ?*const Config = null;

/// Component registry reference
var g_component_registry: ?*NativeRegistry = null;

// =============================================================================
// Initialization
// =============================================================================

/// Initialize daemon server with allocator
pub fn init(alloc: std.mem.Allocator) void {
    allocator = alloc;
}

/// Set config for module execution
pub fn setConfig(config: *const Config) void {
    g_config = config;
}

/// Set component registry
pub fn setRegistry(registry: *NativeRegistry) void {
    g_component_registry = registry;
}

// =============================================================================
// Socket Helpers
// =============================================================================

/// SECURITY: Create daemon socket with ownership verification
pub fn createDaemonSocket() !struct { fd: std.posix.socket_t, path: []const u8 } {
    const socket_path = try client.getSocketPath(allocator);
    errdefer allocator.free(socket_path);

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

/// Read exactly n bytes from fd
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

// =============================================================================
// Module Cache
// =============================================================================

/// Get memimg path from wasm/aot path (replace extension with .memimg)
fn getMemimgPath(wasm_path: []const u8, buf: []u8) ![]const u8 {
    const dot_idx = std.mem.lastIndexOf(u8, wasm_path, ".") orelse wasm_path.len;
    return std.fmt.bufPrint(buf, "{s}.memimg", .{wasm_path[0..dot_idx]}) catch error.BufferTooSmall;
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

    daemonLog("[daemon] Loaded {s} ({d:.1} MB)\n", .{ wasm_path, @as(f64, @floatFromInt(wasm_buf.len)) / (1024 * 1024) });

    // Detect AOT vs WASM
    const is_aot = wasm_buf.len >= 4 and wasm_buf[0] == 0 and wasm_buf[1] == 'a' and wasm_buf[2] == 'o' and wasm_buf[3] == 't';

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
        .module = module,
        .wasm_buf = wasm_buf,
        .memimg_path = memimg_path,
        .is_aot = is_aot,
        .cow_initialized = false,
    };

    try g_module_cache.put(path_key, entry);

    // Initialize CoW for both WASM and AOT
    const cached_ptr = g_module_cache.getPtr(path_key).?;
    try initializeModuleCow(cached_ptr);

    return cached_ptr;
}

/// Initialize CoW snapshot for a module
fn initializeModuleCow(cached: *CachedModule) !void {
    if (cached.cow_initialized) return;

    daemonLog("[daemon] Initializing CoW for module...\n", .{});

    // Check if memimg already exists and is valid
    const memimg_exists = blk: {
        const memimg_file = std.fs.cwd().openFile(cached.memimg_path, .{}) catch break :blk false;
        defer memimg_file.close();
        const memimg_stat = memimg_file.stat() catch break :blk false;

        if (memimg_stat.size <= 1024 * 1024) break :blk false;

        // Check if AOT is newer than memimg
        const memimg_path = cached.memimg_path;
        if (memimg_path.len < 7) break :blk false;
        const base_path = memimg_path[0 .. memimg_path.len - 7];
        var aot_path_buf: [4096]u8 = undefined;
        const aot_path = std.fmt.bufPrint(&aot_path_buf, "{s}.aot", .{base_path}) catch break :blk false;

        const aot_file = std.fs.cwd().openFile(aot_path, .{}) catch break :blk false;
        defer aot_file.close();
        const aot_stat = aot_file.stat() catch break :blk false;

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
        };
        if (cow_allocator.isAvailable()) {
            cached.cow_initialized = true;
            daemonLog("[daemon] Module ready (CoW mode, reused)\n", .{});
            return;
        }
    }

    // Close old CoW fd before regenerating memimg
    if (cow_allocator.isAvailable()) {
        daemonLog("[daemon] Closing stale CoW mapping before regeneration\n", .{});
        cow_allocator.deinit();
    }

    const stack_size: u32 = 64 * 1024;
    const heap_size: u32 = DEFAULT_HEAP_SIZE_MB * 1024 * 1024;
    var error_buf: [256]u8 = undefined;

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

    // Pre-grow memory for CoW snapshot
    const memory_inst = c.wasm_runtime_get_memory(template_instance, 0);
    if (memory_inst != null) {
        const WASM_PAGE_SIZE: u64 = 65536;
        const target_pages: u64 = (heap_size + WASM_PAGE_SIZE - 1) / WASM_PAGE_SIZE;
        const current_pages: u64 = c.wasm_memory_get_cur_page_count(memory_inst);

        if (target_pages > current_pages) {
            const pages_to_add: u32 = @intCast(target_pages - current_pages);
            daemonLog("[daemon] Pre-growing memory: {d} -> {d} pages\n", .{ current_pages, target_pages });
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

    // Warmup cycles
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

// =============================================================================
// Request Handling
// =============================================================================

/// Handle a single client request. Returns true if daemon should exit.
pub fn handleClientRequest(client_fd: std.posix.fd_t, loadConfigFn: anytype, resetSocketStateFn: anytype) !bool {
    // Read command prefix (4 bytes): "WARM", "DOWN", "EXIT", or path length for run
    var cmd_buf: [4]u8 = undefined;
    readExact(client_fd, &cmd_buf) catch |err| {
        daemonLog("[daemon] Failed to read command: {}\n", .{err});
        return error.InvalidRequest;
    };

    // EXIT command
    if (std.mem.eql(u8, &cmd_buf, "EXIT")) {
        const msg = "Daemon stopped\n";
        _ = std.posix.write(client_fd, msg) catch {};
        return true;
    }

    const is_warmup = std.mem.eql(u8, &cmd_buf, "WARM");
    const is_down = std.mem.eql(u8, &cmd_buf, "DOWN");

    var path_len: u32 = undefined;
    if (is_warmup or is_down) {
        var len_buf: [4]u8 = undefined;
        try readExact(client_fd, &len_buf);
        path_len = std.mem.readInt(u32, &len_buf, .little);
    } else {
        path_len = std.mem.readInt(u32, &cmd_buf, .little);
    }

    if (path_len > 4096) return error.PathTooLong;

    var path_buf: [4097]u8 = undefined;
    try readExact(client_fd, path_buf[0 .. path_len + 1]);
    const wasm_path = path_buf[0..path_len];

    // DOWN command - unregister module from cache
    if (is_down) {
        if (g_module_cache.fetchRemove(wasm_path)) |_| {
            daemonLog("[daemon] Unregistered module: {s}\n", .{wasm_path});
            const msg = "Module unregistered\n";
            _ = std.posix.write(client_fd, msg) catch {};
        } else {
            const msg = "Module not found in cache\n";
            _ = std.posix.write(client_fd, msg) catch {};
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
        readExact(client_fd, &args_count_buf) catch {
            args_count_buf = std.mem.zeroes([4]u8);
        };
        const args_count = std.mem.readInt(u32, &args_count_buf, .little);

        if (args_count > 0 and args_count <= 256) {
            for (0..args_count) |_| {
                var arg_len_buf: [4]u8 = undefined;
                try readExact(client_fd, &arg_len_buf);
                const arg_len = std.mem.readInt(u32, &arg_len_buf, .little);

                if (arg_len > 4096) return error.ArgTooLong;

                const arg_buf = try allocator.alloc(u8, arg_len + 1);
                errdefer allocator.free(arg_buf);

                try readExact(client_fd, arg_buf[0..arg_len]);
                arg_buf[arg_len] = 0;

                try args_list.append(allocator, arg_buf[0..arg_len :0]);
            }
        }
    }

    // Get or load cached module
    const cached = try getOrLoadModule(wasm_path);

    if (is_warmup) {
        const msg = "Module loaded and cached\n";
        _ = std.posix.write(client_fd, msg) catch {};
    } else {
        runModuleInstance(cached, wasm_path, args_list.items, client_fd, loadConfigFn, resetSocketStateFn) catch |err| {
            daemonLog("[daemon] Execution error: {}\n", .{err});
        };
    }
    return false;
}

/// Run a module instance for a request
fn runModuleInstance(
    cached: *CachedModule,
    wasm_path: []const u8,
    args: []const [:0]const u8,
    client_fd: std.posix.fd_t,
    loadConfigFn: anytype,
    resetSocketStateFn: anytype,
) !void {
    // Load config from the wasm file's directory
    var module_config = loadConfigFn(wasm_path);
    defer module_config.deinit(allocator);
    g_config = &module_config;
    defer g_config = null;

    // Reset socket state
    resetSocketStateFn();

    const stack_size: u32 = 64 * 1024;
    const heap_size: u32 = DEFAULT_HEAP_SIZE_MB * 1024 * 1024;
    var error_buf: [256]u8 = undefined;

    var module_to_use: c.wasm_module_t = undefined;

    if (cached.module != null) {
        module_to_use = cached.module;
    } else {
        // For AOT modules, WAMR modifies the buffer during load (relocation patching).
        // Make a durable heap copy on first load so it can be modified safely.
        // The module object is then cached and reused for all subsequent requests.
        if (cached.is_aot and cached.aot_heap_copy == null) {
            cached.aot_heap_copy = allocator.dupe(u8, cached.wasm_buf) catch {
                daemonLog("[daemon] Failed to allocate AOT heap copy\n", .{});
                return error.OutOfMemory;
            };
        }

        const load_buf = if (cached.aot_heap_copy) |copy| copy else cached.wasm_buf;
        cached.module = c.wasm_runtime_load(@constCast(load_buf.ptr), @intCast(load_buf.len), &error_buf, error_buf.len);
        if (cached.module == null) {
            daemonLog("[daemon] Failed to load WASM: {s}\n", .{std.mem.sliceTo(&error_buf, 0)});
            return error.ModuleLoadFailed;
        }
        module_to_use = cached.module;
    }

    // Enable CoW allocator
    if (cached.cow_initialized) {
        cow_allocator.init(allocator, cached.memimg_path) catch |err| {
            daemonLog("[daemon] Failed to enable CoW: {}\n", .{err});
        };
    }

    g_daemon_module = module_to_use;
    g_active_module_path = wasm_path;
    defer {
        g_daemon_module = null;
        g_active_module_path = null;
    }

    // Set WASI args for this execution
    const S = struct {
        var dir_list = [_][*:0]const u8{ ".", "/tmp" };
        var empty_env = [_][*:0]const u8{};
    };

    var wasi_argv: [65][*:0]const u8 = undefined;
    wasi_argv[0] = "edgebox";
    const arg_count = @min(args.len, 64);
    for (0..arg_count) |i| {
        wasi_argv[i + 1] = args[i].ptr;
    }

    const client_fd_i64: i64 = @intCast(client_fd);
    c.wasm_runtime_set_wasi_args_ex(
        module_to_use,
        @ptrCast(&S.dir_list),
        S.dir_list.len,
        null,
        0,
        @ptrCast(&S.empty_env),
        S.empty_env.len,
        @ptrCast(&wasi_argv),
        arg_count + 1,
        -1,
        client_fd_i64,
        -1,
    );

    // Redirect OS stdout to client
    const saved_stdout = std.posix.dup(1) catch return error.DupFailed;
    std.posix.dup2(client_fd, 1) catch {
        std.posix.close(saved_stdout);
        return error.Dup2Failed;
    };
    defer {
        std.posix.dup2(saved_stdout, 1) catch {};
        std.posix.close(saved_stdout);
    }

    // Create instance
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

    // Call _start
    const start_func = c.wasm_runtime_lookup_function(instance, "_start");
    if (start_func == null) {
        const msg = "[daemon error] _start function not found\n";
        _ = std.posix.write(client_fd, msg) catch {};
        return;
    }

    daemonLog("[daemon] Calling _start...\n", .{});
    const success = c.wasm_runtime_call_wasm(exec_env, start_func, 0, null);
    daemonLog("[daemon] _start returned: success={}\n", .{success});

    if (!success) {
        const exception = c.wasm_runtime_get_exception(instance);
        if (exception != null) {
            const exc_str = std.mem.span(exception);
            if (std.mem.indexOf(u8, exc_str, "proc exit") == null) {
                var err_msg: [512]u8 = undefined;
                const err_str = std.fmt.bufPrint(&err_msg, "[daemon error] {s}\n", .{exception}) catch "[daemon error] unknown exception\n";
                _ = std.posix.write(client_fd, err_str) catch {};
                if (std.mem.indexOf(u8, exc_str, "out of bounds memory access") != null) {
                    return error.OutOfBoundsMemoryAccess;
                }
            }
        }
    }
}

// =============================================================================
// Main Server Loop
// =============================================================================

/// Run the daemon server - main entry point
/// loadConfigFn: fn(?[]const u8) Config - loads config from wasm directory
/// resetSocketStateFn: fn() void - resets socket dispatch state
/// registerFn: fn() void - registers host functions
/// deinitComponentModelFn: fn() void - cleanup component model
pub fn runDaemonServer(
    loadConfigFn: anytype,
    resetSocketStateFn: anytype,
    registerFn: anytype,
    deinitComponentModelFn: anytype,
) !void {
    // Load default config
    var config = loadConfigFn(null);
    defer config.deinit(allocator);
    g_config = &config;
    defer g_config = null;

    // Initialize emulator system
    emulators.init(allocator);
    defer emulators.deinit();

    // Initialize WAMR
    var init_args = std.mem.zeroes(c.RuntimeInitArgs);
    init_args.mem_alloc_type = c.Alloc_With_Allocator;
    init_args.mem_alloc_option.allocator.malloc_func = @ptrCast(@constCast(&wamrMalloc));
    init_args.mem_alloc_option.allocator.realloc_func = @ptrCast(@constCast(&wamrRealloc));
    init_args.mem_alloc_option.allocator.free_func = @ptrCast(@constCast(&wamrFree));
    if (!c.wasm_runtime_full_init(&init_args)) {
        daemonLog("[daemon] Failed to initialize WAMR runtime\n", .{});
        return;
    }
    defer c.wasm_runtime_destroy();

    if (!c.wasm_runtime_init_thread_env()) {
        daemonLog("[daemon] Failed to initialize thread env\n", .{});
        return;
    }
    defer c.wasm_runtime_destroy_thread_env();

    // Register host functions
    registerFn();

    // Initialize component registry
    try wasm_component.initGlobalRegistry(allocator);
    defer {
        deinitComponentModelFn();
        wasm_component.deinitGlobalRegistry(allocator);
    }

    // Initialize module cache
    g_module_cache = std.StringHashMap(CachedModule).init(allocator);
    g_module_cache_initialized = true;
    defer {
        var it = g_module_cache.iterator();
        while (it.next()) |entry| {
            c.wasm_runtime_unload(entry.value_ptr.module);
            allocator.free(entry.key_ptr.*);
            allocator.free(entry.value_ptr.memimg_path);
        }
        g_module_cache.deinit();
        g_module_cache_initialized = false;
    }

    // Create socket
    const socket_info = try createDaemonSocket();
    defer std.posix.close(socket_info.fd);
    defer allocator.free(socket_info.path);

    try std.posix.listen(socket_info.fd, 128);

    daemonLog("[daemon] EdgeBox daemon started\n", .{});
    daemonLog("[daemon] Listening on {s}\n", .{socket_info.path});

    // Main accept loop
    while (true) {
        const client_fd = std.posix.accept(socket_info.fd, null, null, 0) catch continue;
        const should_exit = handleClientRequest(client_fd, loadConfigFn, resetSocketStateFn) catch |err| blk: {
            daemonLog("[daemon] Request error: {}\n", .{err});
            var err_buf: [256]u8 = undefined;
            const err_msg = std.fmt.bufPrint(&err_buf, "[daemon error] {}\n", .{err}) catch "[daemon error] unknown\n";
            _ = std.posix.write(client_fd, err_msg) catch {};
            break :blk false;
        };
        std.posix.close(client_fd);
        if (should_exit) {
            daemonLog("[daemon] Exiting\n", .{});
            break;
        }
    }
}

// Custom WAMR allocator functions
fn wamrMalloc(size: c_uint) callconv(.c) ?*anyopaque {
    const c_stdlib = @cImport(@cInclude("stdlib.h"));
    return c_stdlib.malloc(size);
}

fn wamrRealloc(ptr: ?*anyopaque, size: c_uint) callconv(.c) ?*anyopaque {
    const c_stdlib = @cImport(@cInclude("stdlib.h"));
    return c_stdlib.realloc(ptr, size);
}

fn wamrFree(ptr: ?*anyopaque) callconv(.c) void {
    const c_stdlib = @cImport(@cInclude("stdlib.h"));
    c_stdlib.free(ptr);
}
