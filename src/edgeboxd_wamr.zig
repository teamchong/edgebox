/// EdgeBox Daemon (edgeboxd) - HTTP server with BATCH INSTANCE POOL using WAMR
///
/// Architecture (inspired by Cloudflare Workers warm pool):
/// - Master process: accepts connections, grabs pre-instantiated WASM from pool
/// - Pool manager thread: continuously pre-instantiates WASM instances in background
/// - Instance pool: ring buffer of READY instances waiting for requests
///
/// Key insight:
/// - Current approach: request → instantiate (~2ms) → execute → destroy
/// - Batch pool: request → grab ready instance (0ms!) → execute → destroy
///
/// The pool manager thread runs in background, filling the pool ahead of demand.
/// When a request comes in, we grab an already-instantiated WASM - no wait!
/// If pool is empty (burst traffic), we fall back to on-demand instantiation.
///
/// Why this is the fastest possible:
/// - Zero instantiation latency on hot path (already done in background)
/// - Continuous pool replenishment handles sustained load
/// - Graceful degradation: empty pool → on-demand (still works, just slower)
///
/// Usage:
///   edgeboxd <file.wasm|aot> [--port=8080] [--pool-size=32]
///
/// Can also be built with embedded WASM (no file required):
///   zig build embedded-daemon -Daot-path=app.wasm -Dname=myapp-daemon
const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const async_loader = @import("async_loader.zig");
const cow_allocator = @import("cow_allocator.zig");
const config_mod = @import("config/mod.zig");

// Embedded mode: WASM is compiled into the binary
const embedded_mode = build_options.embedded_mode;
const aot_data = if (embedded_mode) @import("aot_data") else struct {
    pub const data: []const u8 = &.{};
};

// Import WAMR C API
const c = @cImport({
    @cInclude("wasm_export.h");
    @cInclude("lib_export.h");
});

const NativeSymbol = c.NativeSymbol;

const VERSION = "0.5.0";
const DEFAULT_PORT: u16 = 8080;
const DEFAULT_POOL_SIZE: usize = 32; // Pre-instantiated instances per batch
const MAX_POOL_SIZE: usize = 128;
const MAX_CLEANUP_QUEUE: usize = 256; // Max pending cleanups
const DEFAULT_EXEC_TIMEOUT_MS: u64 = 30000; // 30 seconds default execution timeout
const DEFAULT_SOCKET_PATH = "/tmp/edgebox.sock"; // Docker-style Unix socket default

// Global state
var g_allocator: std.mem.Allocator = undefined;

// Socket mode: Unix socket (default, like Docker) or TCP
const SocketMode = enum {
    unix, // /tmp/edgebox.sock (default)
    tcp, // localhost:8080
};

// Config from .edgebox.json
const DaemonConfig = struct {
    pool_size: usize = DEFAULT_POOL_SIZE,
    exec_timeout_ms: u64 = DEFAULT_EXEC_TIMEOUT_MS,
    port: u16 = DEFAULT_PORT,
    socket_path: []const u8 = DEFAULT_SOCKET_PATH,
    socket_mode: SocketMode = .unix, // Default to Unix socket like Docker
    reuse_instances: bool = false, // If true, instances reused (fast but state persists); if false, destroyed (clean state)
    heap_size_mb: u32 = 64, // WASM heap size in MB (default 64MB, sufficient for most scripts)
    enable_cow: bool = true, // Enable copy-on-write memory for fast instantiation
};

var g_config: DaemonConfig = .{};
var g_memory_image_path: ?[]const u8 = null; // Path to .memimg file for CoW

/// Load config from .edgebox.json using the config module
fn loadConfig(wasm_path: []const u8) void {
    var mod_config = config_mod.load(g_allocator, .{
        .sections = .daemon_only,
        .wasm_path = wasm_path,
    }) catch {
        std.debug.print("[edgeboxd] No .edgebox.json found, using defaults\n", .{});
        return;
    };
    defer mod_config.deinit(g_allocator);

    // Copy daemon config from module
    if (mod_config.daemon) |daemon| {
        if (daemon.pool_size > 0 and daemon.pool_size <= MAX_POOL_SIZE) {
            g_config.pool_size = daemon.pool_size;
        }
        if (daemon.port > 0) {
            g_config.port = daemon.port;
        }
        g_config.reuse_instances = daemon.reuse_instances;
        g_config.enable_cow = daemon.enable_cow;
        if (daemon.heap_size_mb > 0 and daemon.heap_size_mb <= 4096) {
            g_config.heap_size_mb = daemon.heap_size_mb;
        }
    }

    // Also check runtime config for exec_timeout_ms
    if (mod_config.runtime.exec_timeout_ms > 0) {
        g_config.exec_timeout_ms = mod_config.runtime.exec_timeout_ms;
    }

    std.debug.print("[edgeboxd] Config: pool_size={}, exec_timeout={}ms, port={}, reuse={}, heap={}MB\n", .{ g_config.pool_size, g_config.exec_timeout_ms, g_config.port, g_config.reuse_instances, g_config.heap_size_mb });
}

// Pre-loaded WASM module (shared)
var g_module: c.wasm_module_t = null;

// Instance pool - ring buffer of pre-instantiated WASM
const PooledInstance = struct {
    module_inst: c.wasm_module_inst_t,
    exec_env: c.wasm_exec_env_t,
};

var g_pool: [MAX_POOL_SIZE]?PooledInstance = [_]?PooledInstance{null} ** MAX_POOL_SIZE;
var g_pool_head: usize = 0; // Next instance to grab
var g_pool_tail: usize = 0; // Next slot to fill
var g_pool_count: usize = 0; // Current instances in pool
var g_target_pool_size: usize = DEFAULT_POOL_SIZE;
var g_pool_mutex: std.Thread.Mutex = .{};
var g_pool_not_empty: std.Thread.Condition = .{};
var g_pool_not_full: std.Thread.Condition = .{};
var g_shutdown: std.atomic.Value(bool) = std.atomic.Value(bool).init(false);

// Cleanup queue - instances waiting to be destroyed (async cleanup)
var g_cleanup_queue: [MAX_CLEANUP_QUEUE]?PooledInstance = [_]?PooledInstance{null} ** MAX_CLEANUP_QUEUE;
var g_cleanup_head: usize = 0; // Next instance to destroy
var g_cleanup_tail: usize = 0; // Next slot to queue
var g_cleanup_count: usize = 0; // Pending cleanups
var g_cleanup_mutex: std.Thread.Mutex = .{};
var g_cleanup_not_empty: std.Thread.Condition = .{};

// Stats - use atomics to prevent race conditions (these are updated without mutex)
var g_stats_hits: std.atomic.Value(usize) = std.atomic.Value(usize).init(0);
var g_stats_misses: std.atomic.Value(usize) = std.atomic.Value(usize).init(0);
var g_stats_total_ns: std.atomic.Value(i64) = std.atomic.Value(i64).init(0);

// Process state for edgebox_process.* API
var process_state: ProcessState = .{};

const ProcessState = struct {
    program: ?[]const u8 = null,
    args: std.ArrayListUnmanaged([]const u8) = .{},
    env_vars: std.ArrayListUnmanaged(EnvVar) = .{},
    stdin_data: ?[]const u8 = null,
    timeout_ms: u32 = 30000,
    exit_code: i32 = 0,
    stdout_buf: ?[]u8 = null,
    stderr_buf: ?[]u8 = null,

    const EnvVar = struct { key: []const u8, value: []const u8 };

    fn reset(self: *ProcessState) void {
        self.program = null;
        self.args.clearRetainingCapacity(g_allocator);
        self.env_vars.clearRetainingCapacity(g_allocator);
        self.stdin_data = null;
        self.timeout_ms = 30000;
        self.exit_code = 0;
        if (self.stdout_buf) |buf| g_allocator.free(buf);
        if (self.stderr_buf) |buf| g_allocator.free(buf);
        self.stdout_buf = null;
        self.stderr_buf = null;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    g_allocator = gpa.allocator();

    var args_iter = std.process.args();
    _ = args_iter.next();

    var wasm_path: ?[]const u8 = null;
    var cli_port: ?u16 = null;
    var cli_socket_path: ?[]const u8 = null;
    var cli_pool_size: ?usize = null;
    var cli_timeout: ?u64 = null;
    var force_tcp = false;

    while (args_iter.next()) |arg| {
        if (std.mem.startsWith(u8, arg, "--port=")) {
            cli_port = std.fmt.parseInt(u16, arg[7..], 10) catch null;
            force_tcp = true; // --port implies TCP mode
        } else if (std.mem.startsWith(u8, arg, "-p=")) {
            cli_port = std.fmt.parseInt(u16, arg[3..], 10) catch null;
            force_tcp = true;
        } else if (std.mem.startsWith(u8, arg, "--sock=")) {
            cli_socket_path = arg[7..];
        } else if (std.mem.startsWith(u8, arg, "--socket=")) {
            cli_socket_path = arg[9..];
        } else if (std.mem.eql(u8, arg, "--tcp")) {
            force_tcp = true; // Explicit TCP mode
        } else if (std.mem.startsWith(u8, arg, "--pool-size=")) {
            cli_pool_size = std.fmt.parseInt(usize, arg[12..], 10) catch null;
        } else if (std.mem.startsWith(u8, arg, "--pool=")) {
            cli_pool_size = std.fmt.parseInt(usize, arg[7..], 10) catch null;
        } else if (std.mem.startsWith(u8, arg, "--timeout=")) {
            cli_timeout = std.fmt.parseInt(u64, arg[10..], 10) catch null;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printUsage();
            return;
        } else if (std.mem.eql(u8, arg, "--version") or std.mem.eql(u8, arg, "-v")) {
            std.debug.print("edgeboxd {s} (WAMR, batch instance pool)\n", .{VERSION});
            return;
        } else if (std.mem.eql(u8, arg, "--stats")) {
            // Will print stats on shutdown
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            wasm_path = arg;
        }
    }

    // In embedded mode, WASM is built into the binary
    if (!embedded_mode and wasm_path == null) {
        std.debug.print("Error: WASM file required\n\n", .{});
        printUsage();
        std.process.exit(1);
    }

    // Load config from .edgebox.json (sets defaults in g_config)
    if (wasm_path) |path| {
        loadConfig(path);
    } else {
        loadConfig(".");
    }

    // CLI args override config file
    if (force_tcp) g_config.socket_mode = .tcp;
    if (cli_port) |p| g_config.port = p;
    if (cli_socket_path) |sp| g_config.socket_path = sp;
    if (cli_pool_size) |ps| g_config.pool_size = @min(ps, MAX_POOL_SIZE);
    if (cli_timeout) |t| g_config.exec_timeout_ms = t;

    g_target_pool_size = g_config.pool_size;

    try startServer(wasm_path);
}

fn printUsage() void {
    std.debug.print(
        \\EdgeBox Daemon (WAMR) - HTTP server with batch instance pool
        \\
        \\Usage:
        \\  edgeboxd <file.wasm> [options]
        \\
        \\Socket Options (default: Unix socket like Docker):
        \\  --sock=PATH       Unix socket path (default: /tmp/edgebox.sock)
        \\  --tcp             Use TCP instead of Unix socket
        \\  --port=PORT       TCP port (default: 8080, implies --tcp)
        \\
        \\Pool Options:
        \\  --pool-size=N     Pre-instantiated pool size (default: 32, max: 128)
        \\  --timeout=MS      Execution timeout in ms (default: 30000)
        \\  --help            Show this help
        \\
        \\Examples:
        \\  edgeboxd app.wasm                    # Unix socket at /tmp/edgebox.sock
        \\  edgeboxd app.wasm --sock=/run/eb.sock  # Custom socket path
        \\  edgeboxd app.wasm --port=3000        # TCP mode on port 3000
        \\
        \\Config file (.edgebox.json):
        \\  {{
        \\    "daemon": {{
        \\      "pool_size": 32,
        \\      "socket_path": "/tmp/edgebox.sock",
        \\      "socket_mode": "unix",         // "unix" or "tcp"
        \\      "port": 8080,                  // for TCP mode
        \\      "reuse_instances": false
        \\    }}
        \\  }}
        \\
    , .{});
}

fn startServer(wasm_path: ?[]const u8) !void {
    const startup_begin = std.time.nanoTimestamp();

    // Initialize WAMR runtime
    const init_start = std.time.nanoTimestamp();
    var init_args = std.mem.zeroes(c.RuntimeInitArgs);
    init_args.mem_alloc_type = c.Alloc_With_System_Allocator;

    if (!c.wasm_runtime_full_init(&init_args)) {
        std.debug.print("[edgeboxd] Failed to initialize WAMR runtime\n", .{});
        std.process.exit(1);
    }
    const init_ms = @as(f64, @floatFromInt(std.time.nanoTimestamp() - init_start)) / 1_000_000.0;

    // Set up CoW memory image path (e.g., app.wasm -> app.memimg)
    if (g_config.enable_cow and wasm_path != null) {
        var memimg_path_buf: [4096]u8 = undefined;
        const wasm_path_str = wasm_path.?;

        // Replace extension with .memimg
        if (std.mem.lastIndexOf(u8, wasm_path_str, ".")) |dot_idx| {
            if (std.fmt.bufPrint(&memimg_path_buf, "{s}.memimg", .{wasm_path_str[0..dot_idx]})) |path| {
                g_memory_image_path = g_allocator.dupe(u8, path) catch null;
            } else |_| {}
        } else {
            if (std.fmt.bufPrint(&memimg_path_buf, "{s}.memimg", .{wasm_path_str})) |path| {
                g_memory_image_path = g_allocator.dupe(u8, path) catch null;
            } else |_| {}
        }

        // Initialize CoW allocator in capture mode (will capture on first instance)
        if (g_memory_image_path) |memimg_path| {
            // Check if memory image already exists
            if (std.fs.cwd().access(memimg_path, .{})) |_| {
                // Image exists - use it directly
                cow_allocator.init(g_allocator, memimg_path) catch |err| {
                    std.debug.print("[edgeboxd] CoW init failed: {}, continuing without CoW\n", .{err});
                    g_memory_image_path = null;
                };
            } else |_| {
                // Image doesn't exist - will capture after first instantiation
                cow_allocator.initWithCapture(g_allocator, memimg_path) catch |err| {
                    std.debug.print("[edgeboxd] CoW capture init failed: {}, continuing without CoW\n", .{err});
                    g_memory_image_path = null;
                };
            }
        }
    }

    // Register host functions BEFORE loading module
    const register_start = std.time.nanoTimestamp();
    registerHostFunctions();
    const register_ms = @as(f64, @floatFromInt(std.time.nanoTimestamp() - register_start)) / 1_000_000.0;

    var error_buf: [256]u8 = undefined;

    // Load module - either from embedded data or file
    const load_start = std.time.nanoTimestamp();
    if (embedded_mode) {
        std.debug.print("[edgeboxd] Loading embedded module ({} bytes)...\n", .{aot_data.data.len});

        // Copy embedded data to heap (WAMR may modify during parsing)
        const wasm_buf = std.heap.page_allocator.alloc(u8, aot_data.data.len) catch {
            std.debug.print("[edgeboxd] Failed to allocate buffer for module\n", .{});
            std.process.exit(1);
        };
        @memcpy(wasm_buf, aot_data.data);

        g_module = c.wasm_runtime_load(wasm_buf.ptr, @intCast(wasm_buf.len), &error_buf, error_buf.len);
        if (g_module == null) {
            std.debug.print("[edgeboxd] Failed to load embedded module: {s}\n", .{&error_buf});
            std.process.exit(1);
        }
    } else {
        // File-based loading using async loader (platform-optimized: mmap + madvise on macOS/Linux)
        const wasm_path_str = wasm_path.?;

        std.debug.print("[edgeboxd] Loading {s}...\n", .{wasm_path_str});

        // Use async loader for high-throughput SSD loading
        var loader = async_loader.AsyncLoader.init(g_allocator, wasm_path_str) catch |err| {
            std.debug.print("[edgeboxd] Failed to init loader for {s}: {}\n", .{ wasm_path_str, err });
            std.process.exit(1);
        };

        const wasm_buf = loader.loadSync() catch |err| {
            std.debug.print("[edgeboxd] Failed to load {s}: {}\n", .{ wasm_path_str, err });
            loader.deinit();
            std.process.exit(1);
        };

        std.debug.print("[edgeboxd] Loaded {d:.1} MB in {d:.1}ms ({d:.0} MB/s)\n", .{
            @as(f64, @floatFromInt(wasm_buf.len)) / (1024 * 1024),
            loader.getLoadTimeMs(),
            loader.getThroughputMBs(),
        });

        // Note: Don't deinit loader - we need the mmap'd buffer to stay valid!
        // WAMR will use this buffer for the lifetime of the module.

        g_module = c.wasm_runtime_load(@constCast(wasm_buf.ptr), @intCast(wasm_buf.len), &error_buf, error_buf.len);
        if (g_module == null) {
            std.debug.print("[edgeboxd] Failed to load module: {s}\n", .{&error_buf});
            std.process.exit(1);
        }
    }
    const load_ms = @as(f64, @floatFromInt(std.time.nanoTimestamp() - load_start)) / 1_000_000.0;

    // Set WASI args
    var dir_list = [_][*:0]const u8{ ".", "/tmp" };
    var wasi_args = [_][*:0]const u8{"edgeboxd"};
    c.wasm_runtime_set_wasi_args(
        g_module,
        @ptrCast(&dir_list),
        dir_list.len,
        null,
        0,
        null,
        0,
        @ptrCast(&wasi_args),
        wasi_args.len,
    );

    // INSTANT STARTUP: Create just 1 instance, fill pool in background
    std.debug.print("[edgeboxd] Creating initial instance (instant startup mode)...\n", .{});

    const prefill_start = std.time.nanoTimestamp();

    // Create single instance for immediate availability
    var error_buf_init: [256]u8 = undefined;
    const stack_size: u32 = 64 * 1024;
    const heap_size: u32 = g_config.heap_size_mb * 1024 * 1024;

    const initial_inst = c.wasm_runtime_instantiate(g_module, stack_size, heap_size, &error_buf_init, error_buf_init.len);
    if (initial_inst == null) {
        std.debug.print("[edgeboxd] Failed to create initial instance: {s}\n", .{&error_buf_init});
        std.process.exit(1);
    }

    const initial_exec = c.wasm_runtime_create_exec_env(initial_inst, stack_size);
    if (initial_exec == null) {
        c.wasm_runtime_deinstantiate(initial_inst);
        std.debug.print("[edgeboxd] Failed to create exec env\n", .{});
        std.process.exit(1);
    }

    // Add to pool
    g_pool[0] = .{ .module_inst = initial_inst, .exec_env = initial_exec };
    g_pool_tail = 1;
    g_pool_count = 1;

    // Initialize CoW memory after first instance is created
    var cow_ms: f64 = 0;
    if (g_config.enable_cow and g_memory_image_path != null) {
        const cow_start = std.time.nanoTimestamp();
        cow_allocator.captureAndEnable(initial_inst) catch |err| {
            std.debug.print("[edgeboxd] CoW capture failed: {}, continuing without CoW\n", .{err});
        };
        cow_ms = @as(f64, @floatFromInt(std.time.nanoTimestamp() - cow_start)) / 1_000_000.0;
        if (cow_allocator.isAvailable()) {
            std.debug.print("[edgeboxd] CoW enabled: {s}\n", .{g_memory_image_path.?});
        }
    }

    const prefill_ms = @as(f64, @floatFromInt(std.time.nanoTimestamp() - prefill_start)) / 1_000_000.0;
    const total_startup_ms = @as(f64, @floatFromInt(std.time.nanoTimestamp() - startup_begin)) / 1_000_000.0;

    std.debug.print("[edgeboxd] Initial instance ready in {d:.1}ms\n", .{prefill_ms});
    if (cow_ms > 0) {
        std.debug.print(
            \\
            \\[edgeboxd] INSTANT STARTUP:
            \\  WAMR init:        {d:>6.1}ms
            \\  Host functions:   {d:>6.1}ms
            \\  Module load:      {d:>6.1}ms
            \\  Initial instance: {d:>6.1}ms
            \\  CoW capture:      {d:>6.1}ms
            \\  ────────────────────────
            \\  TOTAL:            {d:>6.1}ms
            \\
            \\  Pool: 1/{} ready (background filling to {})
            \\  CoW: enabled (future instantiations will be ~5μs)
            \\
        , .{ init_ms, register_ms, load_ms, prefill_ms - cow_ms, cow_ms, total_startup_ms, g_pool_count, g_target_pool_size });
    } else {
        std.debug.print(
            \\
            \\[edgeboxd] INSTANT STARTUP:
            \\  WAMR init:        {d:>6.1}ms
            \\  Host functions:   {d:>6.1}ms
            \\  Module load:      {d:>6.1}ms
            \\  Initial instance: {d:>6.1}ms
            \\  ────────────────────────
            \\  TOTAL:            {d:>6.1}ms
            \\
            \\  Pool: 1/{} ready (background filling to {})
            \\
        , .{ init_ms, register_ms, load_ms, prefill_ms, total_startup_ms, g_pool_count, g_target_pool_size });
    }

    // Start pool manager thread (background replenishment)
    const pool_thread = std.Thread.spawn(.{}, poolManagerThread, .{}) catch |err| {
        std.debug.print("[edgeboxd] Failed to start pool manager: {}\n", .{err});
        std.process.exit(1);
    };
    _ = pool_thread; // Thread runs until shutdown

    // Start cleanup thread (async instance destruction)
    const cleanup_thread = std.Thread.spawn(.{}, cleanupThread, .{}) catch |err| {
        std.debug.print("[edgeboxd] Failed to start cleanup thread: {}\n", .{err});
        std.process.exit(1);
    };
    _ = cleanup_thread; // Thread runs until shutdown

    // Create server socket based on mode
    const server = switch (g_config.socket_mode) {
        .unix => try std.posix.socket(std.posix.AF.UNIX, std.posix.SOCK.STREAM, 0),
        .tcp => try std.posix.socket(std.posix.AF.INET, std.posix.SOCK.STREAM, 0),
    };
    defer std.posix.close(server);

    // Cleanup on exit for Unix socket
    if (g_config.socket_mode == .unix) {
        // Remove existing socket file if present
        std.fs.cwd().deleteFile(g_config.socket_path) catch {};
    }

    const optval: c_int = 1;
    try std.posix.setsockopt(server, std.posix.SOL.SOCKET, std.posix.SO.REUSEADDR, std.mem.asBytes(&optval));

    switch (g_config.socket_mode) {
        .unix => {
            // Unix socket binding
            var addr: std.posix.sockaddr.un = std.mem.zeroes(std.posix.sockaddr.un);
            addr.family = std.posix.AF.UNIX;
            const path_bytes = g_config.socket_path;
            if (path_bytes.len >= addr.path.len) {
                std.debug.print("[edgeboxd] Socket path too long: {s}\n", .{g_config.socket_path});
                std.process.exit(1);
            }
            @memcpy(addr.path[0..path_bytes.len], path_bytes);

            try std.posix.bind(server, @ptrCast(&addr), @sizeOf(std.posix.sockaddr.un));
            std.debug.print("[edgeboxd] Listening on unix://{s}\n", .{g_config.socket_path});
        },
        .tcp => {
            // TCP socket binding
            var addr: std.posix.sockaddr.in = .{
                .family = std.posix.AF.INET,
                .port = std.mem.nativeToBig(u16, g_config.port),
                .addr = 0,
            };
            try std.posix.bind(server, @ptrCast(&addr), @sizeOf(std.posix.sockaddr.in));
            std.debug.print("[edgeboxd] Listening on http://localhost:{}\n", .{g_config.port});
        },
    }

    try std.posix.listen(server, 128);
    std.debug.print("[edgeboxd] Ready - batch instance pool (zero-wait requests)\n", .{});

    // Main accept loop
    while (true) {
        const client = std.posix.accept(server, null, null, 0) catch continue;
        handleRequest(client);
        std.posix.close(client);
    }
}

/// Pre-fill pool synchronously (used at startup for cold start)
fn prefillPool() void {
    var error_buf: [256]u8 = undefined;
    const stack_size: u32 = 64 * 1024;
    const heap_size: u32 = g_config.heap_size_mb * 1024 * 1024;

    var total_inst_ns: i128 = 0;
    var total_exec_ns: i128 = 0;

    while (g_pool_count < g_target_pool_size) {
        const inst_start = std.time.nanoTimestamp();
        const module_inst = c.wasm_runtime_instantiate(g_module, stack_size, heap_size, &error_buf, error_buf.len);
        total_inst_ns += std.time.nanoTimestamp() - inst_start;

        if (module_inst == null) {
            std.debug.print("[edgeboxd] Pre-fill failed: {s}\n", .{&error_buf});
            break;
        }

        const exec_start = std.time.nanoTimestamp();
        const exec_env = c.wasm_runtime_create_exec_env(module_inst, stack_size);
        total_exec_ns += std.time.nanoTimestamp() - exec_start;

        if (exec_env == null) {
            c.wasm_runtime_deinstantiate(module_inst);
            break;
        }

        g_pool[g_pool_tail] = .{
            .module_inst = module_inst,
            .exec_env = exec_env,
        };
        g_pool_tail = (g_pool_tail + 1) % MAX_POOL_SIZE;
        g_pool_count += 1;
    }

    // Print per-instance breakdown
    if (g_pool_count > 0) {
        const avg_inst_ms = @as(f64, @floatFromInt(total_inst_ns)) / @as(f64, @floatFromInt(g_pool_count)) / 1_000_000.0;
        const avg_exec_ms = @as(f64, @floatFromInt(total_exec_ns)) / @as(f64, @floatFromInt(g_pool_count)) / 1_000_000.0;
        std.debug.print("[edgeboxd] Per-instance avg: instantiate={d:.2}ms, exec_env={d:.2}ms\n", .{ avg_inst_ms, avg_exec_ms });
    }
}

/// Pool manager thread - continuously pre-instantiates in background
fn poolManagerThread() void {
    var error_buf: [256]u8 = undefined;
    const stack_size: u32 = 64 * 1024;
    const heap_size: u32 = g_config.heap_size_mb * 1024 * 1024;

    var last_count: usize = 0;
    var pool_filled_logged = false;

    while (!g_shutdown.load(.acquire)) {
        // Wait until pool needs refilling
        g_pool_mutex.lock();
        while (g_pool_count >= g_target_pool_size and !g_shutdown.load(.acquire)) {
            g_pool_not_full.wait(&g_pool_mutex);
        }

        if (g_shutdown.load(.acquire)) {
            g_pool_mutex.unlock();
            break;
        }

        // Check if we can add (pool not at max)
        if (g_pool_count >= MAX_POOL_SIZE) {
            g_pool_mutex.unlock();
            continue;
        }

        const current_count = g_pool_count;
        g_pool_mutex.unlock();

        // Log progress every 4 instances
        if (!pool_filled_logged and current_count > last_count and current_count % 4 == 0) {
            std.debug.print("[edgeboxd] Background: {}/{} instances ready\n", .{ current_count, g_target_pool_size });
            last_count = current_count;
        }

        // Create instance (outside lock - slow operation)
        const module_inst = c.wasm_runtime_instantiate(g_module, stack_size, heap_size, &error_buf, error_buf.len);
        if (module_inst == null) {
            std.Thread.sleep(10 * std.time.ns_per_ms); // Back off on error
            continue;
        }

        const exec_env = c.wasm_runtime_create_exec_env(module_inst, stack_size);
        if (exec_env == null) {
            c.wasm_runtime_deinstantiate(module_inst);
            std.Thread.sleep(10 * std.time.ns_per_ms);
            continue;
        }

        // Add to pool
        g_pool_mutex.lock();
        if (g_pool_count < MAX_POOL_SIZE) {
            g_pool[g_pool_tail] = .{
                .module_inst = module_inst,
                .exec_env = exec_env,
            };
            g_pool_tail = (g_pool_tail + 1) % MAX_POOL_SIZE;
            g_pool_count += 1;
            g_pool_not_empty.signal();

            // Log when pool is fully filled
            if (g_pool_count >= g_target_pool_size and !pool_filled_logged) {
                pool_filled_logged = true;
                g_pool_mutex.unlock();
                std.debug.print("[edgeboxd] Background: Pool fully ready ({} instances)\n", .{g_pool_count});
                g_pool_mutex.lock();
            }
        } else {
            // Pool filled while we were creating - destroy this instance
            c.wasm_runtime_destroy_exec_env(exec_env);
            c.wasm_runtime_deinstantiate(module_inst);
        }
        g_pool_mutex.unlock();
    }
}

/// Grab a pre-instantiated instance from pool (non-blocking)
fn grabInstance() ?PooledInstance {
    g_pool_mutex.lock();
    defer g_pool_mutex.unlock();

    // Security: Check for shutdown while holding mutex to prevent race
    if (g_shutdown.load(.acquire)) {
        return null;
    }

    if (g_pool_count == 0) {
        return null;
    }

    // Bounds check for safety (g_pool_head should always be < MAX_POOL_SIZE due to modulo)
    if (g_pool_head >= MAX_POOL_SIZE) {
        g_pool_head = 0; // Reset to safe state
        return null;
    }

    // Use if-unwrap instead of .? to avoid crash if slot is unexpectedly null
    const instance = g_pool[g_pool_head] orelse {
        // Pool corruption: slot is null but count says it shouldn't be
        // Fix count to maintain consistency, advance head, and return null
        std.debug.print("[edgeboxd] WARNING: pool slot {d} unexpectedly null, fixing count\n", .{g_pool_head});
        g_pool_head = (g_pool_head + 1) % MAX_POOL_SIZE;
        g_pool_count -= 1;
        g_pool_not_full.signal();
        return null;
    };
    g_pool[g_pool_head] = null;
    g_pool_head = (g_pool_head + 1) % MAX_POOL_SIZE;
    g_pool_count -= 1;

    // Signal pool manager to refill
    g_pool_not_full.signal();

    return instance;
}

/// Create instance on-demand (fallback when pool empty)
fn createInstanceOnDemand() ?PooledInstance {
    var error_buf: [256]u8 = undefined;
    const stack_size: u32 = 64 * 1024;
    const heap_size: u32 = g_config.heap_size_mb * 1024 * 1024;

    const module_inst = c.wasm_runtime_instantiate(g_module, stack_size, heap_size, &error_buf, error_buf.len);
    if (module_inst == null) return null;

    const exec_env = c.wasm_runtime_create_exec_env(module_inst, stack_size);
    if (exec_env == null) {
        c.wasm_runtime_deinstantiate(module_inst);
        return null;
    }

    return .{
        .module_inst = module_inst,
        .exec_env = exec_env,
    };
}

/// Destroy instance after use (synchronous)
fn destroyInstance(instance: PooledInstance) void {
    c.wasm_runtime_destroy_exec_env(instance.exec_env);
    c.wasm_runtime_deinstantiate(instance.module_inst);
}

/// Queue instance for async destruction (non-blocking)
/// Returns true if queued, false if queue full (falls back to sync destroy)
fn queueForCleanup(instance: PooledInstance) bool {
    g_cleanup_mutex.lock();
    defer g_cleanup_mutex.unlock();

    if (g_cleanup_count >= MAX_CLEANUP_QUEUE) {
        // Queue full - caller should destroy synchronously
        return false;
    }

    g_cleanup_queue[g_cleanup_tail] = instance;
    g_cleanup_tail = (g_cleanup_tail + 1) % MAX_CLEANUP_QUEUE;
    g_cleanup_count += 1;
    g_cleanup_not_empty.signal();
    return true;
}

/// Cleanup thread - destroys instances in background
fn cleanupThread() void {
    while (!g_shutdown.load(.acquire)) {
        g_cleanup_mutex.lock();

        // Wait for work
        while (g_cleanup_count == 0 and !g_shutdown.load(.acquire)) {
            g_cleanup_not_empty.wait(&g_cleanup_mutex);
        }

        if (g_shutdown.load(.acquire) and g_cleanup_count == 0) {
            g_cleanup_mutex.unlock();
            break;
        }

        // Grab instance to destroy
        const instance = g_cleanup_queue[g_cleanup_head] orelse {
            g_cleanup_head = (g_cleanup_head + 1) % MAX_CLEANUP_QUEUE;
            g_cleanup_count -= 1;
            g_cleanup_mutex.unlock();
            continue;
        };
        g_cleanup_queue[g_cleanup_head] = null;
        g_cleanup_head = (g_cleanup_head + 1) % MAX_CLEANUP_QUEUE;
        g_cleanup_count -= 1;
        g_cleanup_mutex.unlock();

        // Destroy outside lock (slow operation)
        destroyInstance(instance);
    }

    // Drain remaining cleanup queue on shutdown
    g_cleanup_mutex.lock();
    while (g_cleanup_count > 0) {
        if (g_cleanup_queue[g_cleanup_head]) |instance| {
            g_cleanup_queue[g_cleanup_head] = null;
            g_cleanup_head = (g_cleanup_head + 1) % MAX_CLEANUP_QUEUE;
            g_cleanup_count -= 1;
            g_cleanup_mutex.unlock();
            destroyInstance(instance);
            g_cleanup_mutex.lock();
        } else {
            g_cleanup_head = (g_cleanup_head + 1) % MAX_CLEANUP_QUEUE;
            g_cleanup_count -= 1;
        }
    }
    g_cleanup_mutex.unlock();
}

/// Return instance to pool for reuse (instead of destroying)
fn returnInstance(instance: PooledInstance) void {
    g_pool_mutex.lock();

    // If pool is at target size, destroy the instance (shouldn't happen in normal operation)
    if (g_pool_count >= g_target_pool_size) {
        g_pool_mutex.unlock();
        destroyInstance(instance);
        return;
    }

    // Add back to pool
    g_pool[g_pool_tail] = instance;
    g_pool_tail = (g_pool_tail + 1) % MAX_POOL_SIZE;
    g_pool_count += 1;
    g_pool_not_empty.signal();
    g_pool_mutex.unlock();
}

fn handleRequest(client: std.posix.fd_t) void {
    var buf: [4096]u8 = undefined;
    _ = std.posix.read(client, &buf) catch return;

    const start = std.time.nanoTimestamp();

    // Try to grab from pool first (fast path)
    var instance: PooledInstance = undefined;
    var pool_hit = false;

    if (grabInstance()) |pooled| {
        instance = pooled;
        pool_hit = true;
        _ = g_stats_hits.fetchAdd(1, .monotonic);
    } else {
        // Pool empty - fall back to on-demand (slow path)
        instance = createInstanceOnDemand() orelse {
            sendError(client, "Failed to create WASM instance");
            return;
        };
        _ = g_stats_misses.fetchAdd(1, .monotonic);
    }

    // Execute WASM _start
    const exec_start = std.time.nanoTimestamp();
    const start_func = c.wasm_runtime_lookup_function(instance.module_inst, "_start");
    if (start_func != null) {
        _ = c.wasm_runtime_call_wasm(instance.exec_env, start_func, 0, null);
        // Ignore WASI exit exceptions - they're normal
    }
    const exec_ns = std.time.nanoTimestamp() - exec_start;
    const exec_ms = @as(f64, @floatFromInt(exec_ns)) / 1_000_000.0;

    // Calculate response time BEFORE cleanup (user-facing latency)
    const elapsed_ns = std.time.nanoTimestamp() - start;
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
    _ = g_stats_total_ns.fetchAdd(@intCast(elapsed_ns), .monotonic);

    // Send response BEFORE cleanup (async cleanup - don't block user)
    var response: [512]u8 = undefined;
    const pool_status = if (pool_hit) "hit" else "miss";
    // X-Exec-Time: pure WASM execution time (for benchmarks)
    // X-Total-Time: includes pool grab overhead (user-facing latency)
    const http = std.fmt.bufPrint(&response, "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 3\r\nX-Exec-Time: {d:.2}ms\r\nX-Total-Time: {d:.2}ms\r\nX-Pool: {s}\r\nX-Pool-Size: {}\r\nConnection: close\r\n\r\nOK\n", .{ exec_ms, elapsed_ms, pool_status, g_pool_count }) catch {
        sendError(client, "Response failed");
        // Still need to cleanup even on response failure
        if (!g_config.reuse_instances) {
            if (!queueForCleanup(instance)) destroyInstance(instance);
        } else {
            returnInstance(instance);
        }
        return;
    };
    _ = std.posix.write(client, http) catch {};

    // Now cleanup instance (async if destroying, sync if reusing)
    const cleanup_start = std.time.nanoTimestamp();
    var cleanup_async = false;
    if (g_config.reuse_instances) {
        // TODO: Reset WASM linear memory to clean state here
        // For now, state persists between requests when reusing
        returnInstance(instance);
    } else {
        // Queue for async destruction (doesn't block response)
        if (queueForCleanup(instance)) {
            cleanup_async = true;
        } else {
            // Queue full - fall back to sync (rare under normal load)
            destroyInstance(instance);
        }
    }
    const cleanup_ns = std.time.nanoTimestamp() - cleanup_start;
    const cleanup_ms = @as(f64, @floatFromInt(cleanup_ns)) / 1_000_000.0;

    const cleanup_type: []const u8 = if (cleanup_async) "async" else if (g_config.reuse_instances) "reuse" else "sync";
    std.debug.print("[edgeboxd] {d:.2}ms (exec={d:.2}ms, cleanup={s} {d:.2}ms, pool {s}, {}/{} ready)\n", .{ elapsed_ms, exec_ms, cleanup_type, cleanup_ms, pool_status, g_pool_count, g_target_pool_size });
}

fn sendError(client: std.posix.fd_t, msg: []const u8) void {
    var response: [256]u8 = undefined;
    const http = std.fmt.bufPrint(&response, "HTTP/1.1 500 Error\r\nContent-Type: text/plain\r\nContent-Length: {}\r\n\r\n{s}", .{ msg.len, msg }) catch return;
    _ = std.posix.write(client, http) catch {};
}

// =============================================================================
// Host Functions
// =============================================================================

fn readWasmString(exec_env: c.wasm_exec_env_t, ptr: u32, len: u32) ?[]const u8 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return null;
    // Validate that ptr+len is within WASM linear memory bounds
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, len)) return null;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return null;
    const slice: [*]const u8 = @ptrCast(native_ptr);
    return slice[0..len];
}

fn writeWasmBuffer(exec_env: c.wasm_exec_env_t, ptr: u32, data: []const u8) void {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return;
    // Validate that ptr+len is within WASM linear memory bounds
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, @intCast(data.len))) return;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return;
    const slice: [*]u8 = @ptrCast(native_ptr);
    @memcpy(slice[0..data.len], data);
}

fn processSetProgName(exec_env: c.wasm_exec_env_t, name_ptr: u32, name_len: u32) void {
    const name = readWasmString(exec_env, name_ptr, name_len) orelse return;
    process_state.program = g_allocator.dupe(u8, name) catch return;
}

fn processAddArg(exec_env: c.wasm_exec_env_t, arg_ptr: u32, arg_len: u32) void {
    const arg = readWasmString(exec_env, arg_ptr, arg_len) orelse return;
    const duped = g_allocator.dupe(u8, arg) catch return;
    process_state.args.append(g_allocator, duped) catch return;
}

fn processAddEnv(exec_env: c.wasm_exec_env_t, key_ptr: u32, key_len: u32, val_ptr: u32, val_len: u32) void {
    const key = readWasmString(exec_env, key_ptr, key_len) orelse return;
    const val = readWasmString(exec_env, val_ptr, val_len) orelse return;
    const key_duped = g_allocator.dupe(u8, key) catch return;
    const val_duped = g_allocator.dupe(u8, val) catch return;
    process_state.env_vars.append(g_allocator, .{ .key = key_duped, .value = val_duped }) catch return;
}

fn processAddStdin(exec_env: c.wasm_exec_env_t, buf_ptr: u32, buf_len: u32) void {
    const data = readWasmString(exec_env, buf_ptr, buf_len) orelse return;
    process_state.stdin_data = g_allocator.dupe(u8, data) catch return;
}

fn processSetTimeout(_: c.wasm_exec_env_t, time_ms: u32) void {
    process_state.timeout_ms = time_ms;
}

fn processRun(_: c.wasm_exec_env_t) i32 {
    const program = process_state.program orelse return -1;

    var argv = std.ArrayListUnmanaged([]const u8){};
    defer argv.deinit(g_allocator);

    argv.append(g_allocator, program) catch return -1;
    for (process_state.args.items) |arg| {
        argv.append(g_allocator, arg) catch return -1;
    }

    var child = std.process.Child.init(argv.items, g_allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    child.spawn() catch return -1;

    var stdout_data: ?[]u8 = null;
    var stderr_data: ?[]u8 = null;
    var read_buf: [4096]u8 = undefined;

    if (child.stdout) |*stdout_file| {
        var stdout_list = std.ArrayListUnmanaged(u8){};
        defer if (stdout_data == null) stdout_list.deinit(g_allocator); // Free if not transferred
        while (true) {
            const n = stdout_file.read(&read_buf) catch break;
            if (n == 0) break;
            stdout_list.appendSlice(g_allocator, read_buf[0..n]) catch break;
        }
        if (stdout_list.items.len > 0) {
            stdout_data = stdout_list.toOwnedSlice(g_allocator) catch null;
        }
    }
    if (child.stderr) |*stderr_file| {
        var stderr_list = std.ArrayListUnmanaged(u8){};
        defer if (stderr_data == null) stderr_list.deinit(g_allocator); // Free if not transferred
        while (true) {
            const n = stderr_file.read(&read_buf) catch break;
            if (n == 0) break;
            stderr_list.appendSlice(g_allocator, read_buf[0..n]) catch break;
        }
        if (stderr_list.items.len > 0) {
            stderr_data = stderr_list.toOwnedSlice(g_allocator) catch null;
        }
    }

    const term = child.wait() catch return -1;

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

fn processGetExitCode(_: c.wasm_exec_env_t) i32 {
    return process_state.exit_code;
}

fn processGetStdoutLen(_: c.wasm_exec_env_t) u32 {
    return if (process_state.stdout_buf) |buf| @intCast(buf.len) else 0;
}

fn processGetStdout(exec_env: c.wasm_exec_env_t, buf_ptr: u32) void {
    if (process_state.stdout_buf) |buf| {
        writeWasmBuffer(exec_env, buf_ptr, buf);
    }
}

fn processGetStderrLen(_: c.wasm_exec_env_t) u32 {
    return if (process_state.stderr_buf) |buf| @intCast(buf.len) else 0;
}

fn processGetStderr(exec_env: c.wasm_exec_env_t, buf_ptr: u32) void {
    if (process_state.stderr_buf) |buf| {
        writeWasmBuffer(exec_env, buf_ptr, buf);
    }
}

// Stub dispatch functions
fn httpDispatch(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32) i32 {
    return -1;
}
fn spawnDispatch(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32, _: i32, _: i32) i32 {
    return -1;
}
fn fileDispatch(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32, _: i32, _: i32) i32 {
    return -1;
}
fn zlibDispatch(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32) i32 {
    return -1;
}
fn cryptoDispatch(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32) i32 {
    return -1;
}
fn socketDispatch(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32, _: i32) i32 {
    return -1;
}

// Global symbol arrays (WAMR retains references)
var g_edgebox_process_symbols = [_]NativeSymbol{
    .{ .symbol = "edgebox_process_set_prog_name", .func_ptr = @constCast(@ptrCast(&processSetProgName)), .signature = "(ii)", .attachment = null },
    .{ .symbol = "edgebox_process_add_arg", .func_ptr = @constCast(@ptrCast(&processAddArg)), .signature = "(ii)", .attachment = null },
    .{ .symbol = "edgebox_process_add_env", .func_ptr = @constCast(@ptrCast(&processAddEnv)), .signature = "(iiii)", .attachment = null },
    .{ .symbol = "edgebox_process_add_stdin", .func_ptr = @constCast(@ptrCast(&processAddStdin)), .signature = "(ii)", .attachment = null },
    .{ .symbol = "edgebox_process_set_timeout", .func_ptr = @constCast(@ptrCast(&processSetTimeout)), .signature = "(i)", .attachment = null },
    .{ .symbol = "edgebox_process_run", .func_ptr = @constCast(@ptrCast(&processRun)), .signature = "()i", .attachment = null },
    .{ .symbol = "edgebox_process_get_exit_code", .func_ptr = @constCast(@ptrCast(&processGetExitCode)), .signature = "()i", .attachment = null },
    .{ .symbol = "edgebox_process_get_stdout_len", .func_ptr = @constCast(@ptrCast(&processGetStdoutLen)), .signature = "()i", .attachment = null },
    .{ .symbol = "edgebox_process_get_stdout", .func_ptr = @constCast(@ptrCast(&processGetStdout)), .signature = "(i)", .attachment = null },
    .{ .symbol = "edgebox_process_get_stderr_len", .func_ptr = @constCast(@ptrCast(&processGetStderrLen)), .signature = "()i", .attachment = null },
    .{ .symbol = "edgebox_process_get_stderr", .func_ptr = @constCast(@ptrCast(&processGetStderr)), .signature = "(i)", .attachment = null },
};

var g_http_symbols = [_]NativeSymbol{
    .{ .symbol = "http_dispatch", .func_ptr = @constCast(@ptrCast(&httpDispatch)), .signature = "(iiiiiiiii)i", .attachment = null },
};
var g_spawn_symbols = [_]NativeSymbol{
    .{ .symbol = "spawn_dispatch", .func_ptr = @constCast(@ptrCast(&spawnDispatch)), .signature = "(iiiii)i", .attachment = null },
};
var g_file_symbols = [_]NativeSymbol{
    .{ .symbol = "file_dispatch", .func_ptr = @constCast(@ptrCast(&fileDispatch)), .signature = "(iiiii)i", .attachment = null },
};
var g_zlib_symbols = [_]NativeSymbol{
    .{ .symbol = "zlib_dispatch", .func_ptr = @constCast(@ptrCast(&zlibDispatch)), .signature = "(iii)i", .attachment = null },
};
var g_crypto_symbols = [_]NativeSymbol{
    .{ .symbol = "crypto_dispatch", .func_ptr = @constCast(@ptrCast(&cryptoDispatch)), .signature = "(iiiiiii)i", .attachment = null },
};
var g_socket_symbols = [_]NativeSymbol{
    .{ .symbol = "socket_dispatch", .func_ptr = @constCast(@ptrCast(&socketDispatch)), .signature = "(iiii)i", .attachment = null },
};

fn registerHostFunctions() void {
    _ = c.wasm_runtime_register_natives("edgebox_process", &g_edgebox_process_symbols, g_edgebox_process_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_http", &g_http_symbols, g_http_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_spawn", &g_spawn_symbols, g_spawn_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_file", &g_file_symbols, g_file_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_zlib", &g_zlib_symbols, g_zlib_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_crypto", &g_crypto_symbols, g_crypto_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_socket", &g_socket_symbols, g_socket_symbols.len);
}
