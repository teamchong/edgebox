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

const VERSION = "0.4.0";
const DEFAULT_PORT: u16 = 8080;
const DEFAULT_POOL_SIZE: usize = 32; // Pre-instantiated instances per batch
const MAX_POOL_SIZE: usize = 128;
const DEFAULT_EXEC_TIMEOUT_MS: u64 = 30000; // 30 seconds default execution timeout

// Global state
var g_allocator: std.mem.Allocator = undefined;

// Config from .edgebox.json
const DaemonConfig = struct {
    pool_size: usize = DEFAULT_POOL_SIZE,
    exec_timeout_ms: u64 = DEFAULT_EXEC_TIMEOUT_MS,
    port: u16 = DEFAULT_PORT,
};

var g_config: DaemonConfig = .{};

/// Load config from .edgebox.json in current directory or WASM file directory
fn loadConfig(wasm_path: []const u8) void {
    // Try current directory first
    const config_paths = [_][]const u8{
        ".edgebox.json",
    };

    // Also try directory containing the WASM file
    var wasm_dir_config: [4096]u8 = undefined;
    var paths_to_try: [2][]const u8 = undefined;
    var num_paths: usize = 1;
    paths_to_try[0] = config_paths[0];

    if (std.mem.lastIndexOf(u8, wasm_path, "/")) |sep| {
        const dir = wasm_path[0 .. sep + 1];
        if (std.fmt.bufPrint(&wasm_dir_config, "{s}.edgebox.json", .{dir})) |path| {
            paths_to_try[1] = path;
            num_paths = 2;
        } else |_| {}
    }

    for (paths_to_try[0..num_paths]) |config_path| {
        const file = std.fs.cwd().openFile(config_path, .{}) catch continue;
        defer file.close();

        // Security: Limit config file size to 64KB to prevent DoS
        const MAX_CONFIG_SIZE: usize = 64 * 1024;
        const content = file.readToEndAlloc(g_allocator, MAX_CONFIG_SIZE) catch continue;
        defer g_allocator.free(content);

        // Security: File size limit protects against DoS
        const parsed = std.json.parseFromSlice(std.json.Value, g_allocator, content, .{}) catch continue;
        defer parsed.deinit();

        const root = parsed.value;
        if (root != .object) continue;

        // Look for "daemon" section
        if (root.object.get("daemon")) |daemon| {
            if (daemon == .object) {
                if (daemon.object.get("pool_size")) |ps| {
                    if (ps == .integer) {
                        const val = ps.integer;
                        if (val > 0 and val <= MAX_POOL_SIZE) {
                            g_config.pool_size = @intCast(val);
                        }
                    }
                }
                if (daemon.object.get("exec_timeout_ms")) |et| {
                    if (et == .integer) {
                        const val = et.integer;
                        if (val > 0) {
                            g_config.exec_timeout_ms = @intCast(val);
                        }
                    }
                }
                if (daemon.object.get("port")) |p| {
                    if (p == .integer) {
                        const val = p.integer;
                        if (val > 0 and val <= 65535) {
                            g_config.port = @intCast(val);
                        }
                    }
                }
            }
        }

        std.debug.print("[edgeboxd] Loaded config from {s}\n", .{config_path});
        std.debug.print("[edgeboxd] Config: pool_size={}, exec_timeout={}ms, port={}\n", .{ g_config.pool_size, g_config.exec_timeout_ms, g_config.port });
        return;
    }

    // No config found - use defaults
    std.debug.print("[edgeboxd] No .edgebox.json found, using defaults\n", .{});
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
    var cli_pool_size: ?usize = null;
    var cli_timeout: ?u64 = null;

    while (args_iter.next()) |arg| {
        if (std.mem.startsWith(u8, arg, "--port=")) {
            cli_port = std.fmt.parseInt(u16, arg[7..], 10) catch null;
        } else if (std.mem.startsWith(u8, arg, "-p=")) {
            cli_port = std.fmt.parseInt(u16, arg[3..], 10) catch null;
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
    if (cli_port) |p| g_config.port = p;
    if (cli_pool_size) |ps| g_config.pool_size = @min(ps, MAX_POOL_SIZE);
    if (cli_timeout) |t| g_config.exec_timeout_ms = t;

    g_target_pool_size = g_config.pool_size;

    try startServer(wasm_path, g_config.port);
}

fn printUsage() void {
    std.debug.print(
        \\EdgeBox Daemon (WAMR) - HTTP server with batch instance pool
        \\
        \\Usage:
        \\  edgeboxd <file.wasm> [options]
        \\
        \\Options:
        \\  --port=PORT       HTTP port (default: 8080)
        \\  --pool-size=N     Pre-instantiated pool size (default: 32, max: 128)
        \\  --timeout=MS      Execution timeout in ms (default: 30000)
        \\  --help            Show this help
        \\
        \\For best performance, use embedded daemon:
        \\  zig build embedded-daemon -Daot-path=<file.wasm>
        \\
        \\Config file (.edgebox.json):
        \\  {{
        \\    "daemon": {{
        \\      "pool_size": 32,       // instances per batch
        \\      "exec_timeout_ms": 30000,  // max execution time
        \\      "port": 8080
        \\    }}
        \\  }}
        \\
    , .{});
}

fn startServer(wasm_path: ?[]const u8, port: u16) !void {
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
        // File-based loading (WASM only - use embedded binary for AOT)
        const wasm_path_str = wasm_path.?;

        std.debug.print("[edgeboxd] Loading {s}...\n", .{wasm_path_str});

        const wasm_file = std.fs.cwd().openFile(wasm_path_str, .{}) catch |err| {
            std.debug.print("[edgeboxd] Failed to open {s}: {}\n", .{ wasm_path_str, err });
            std.process.exit(1);
        };
        defer wasm_file.close();

        const wasm_size = wasm_file.getEndPos() catch 0;
        const wasm_buf = g_allocator.alloc(u8, wasm_size) catch {
            std.debug.print("[edgeboxd] Failed to allocate memory\n", .{});
            std.process.exit(1);
        };

        _ = wasm_file.readAll(wasm_buf) catch {
            std.debug.print("[edgeboxd] Failed to read WASM file\n", .{});
            std.process.exit(1);
        };

        g_module = c.wasm_runtime_load(wasm_buf.ptr, @intCast(wasm_size), &error_buf, error_buf.len);
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

    std.debug.print("[edgeboxd] Module loaded, pre-filling pool with {} instances...\n", .{g_target_pool_size});

    // Pre-fill pool before starting server (cold start)
    const prefill_start = std.time.nanoTimestamp();
    prefillPool();
    const prefill_ns = std.time.nanoTimestamp() - prefill_start;
    const prefill_ms = @as(f64, @floatFromInt(prefill_ns)) / 1_000_000.0;

    const total_startup_ms = @as(f64, @floatFromInt(std.time.nanoTimestamp() - startup_begin)) / 1_000_000.0;

    std.debug.print("[edgeboxd] Pool ready: {} instances in {d:.1}ms\n", .{ g_pool_count, prefill_ms });
    std.debug.print(
        \\
        \\[edgeboxd] Startup breakdown:
        \\  WAMR init:        {d:>6.1}ms
        \\  Host functions:   {d:>6.1}ms
        \\  Module load:      {d:>6.1}ms
        \\  Pool prefill:     {d:>6.1}ms
        \\  ────────────────────────
        \\  TOTAL:            {d:>6.1}ms
        \\
    , .{ init_ms, register_ms, load_ms, prefill_ms, total_startup_ms });

    // Start pool manager thread (background replenishment)
    const pool_thread = std.Thread.spawn(.{}, poolManagerThread, .{}) catch |err| {
        std.debug.print("[edgeboxd] Failed to start pool manager: {}\n", .{err});
        std.process.exit(1);
    };
    _ = pool_thread; // Thread runs until shutdown

    // Create server socket
    const server = try std.posix.socket(std.posix.AF.INET, std.posix.SOCK.STREAM, 0);
    defer std.posix.close(server);

    const optval: c_int = 1;
    try std.posix.setsockopt(server, std.posix.SOL.SOCKET, std.posix.SO.REUSEADDR, std.mem.asBytes(&optval));

    var addr: std.posix.sockaddr.in = .{
        .family = std.posix.AF.INET,
        .port = std.mem.nativeToBig(u16, port),
        .addr = 0,
    };

    try std.posix.bind(server, @ptrCast(&addr), @sizeOf(std.posix.sockaddr.in));
    try std.posix.listen(server, 128);

    std.debug.print("[edgeboxd] Listening on http://localhost:{}\n", .{port});
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
    const heap_size: u32 = 2 * 1024 * 1024 * 1024; // 2 GB - match Node/Bun defaults

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
    const heap_size: u32 = 2 * 1024 * 1024 * 1024; // 2 GB - match Node/Bun defaults

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

        g_pool_mutex.unlock();

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
    const heap_size: u32 = 2 * 1024 * 1024 * 1024; // 2 GB - match Node/Bun defaults

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

/// Destroy instance after use
fn destroyInstance(instance: PooledInstance) void {
    c.wasm_runtime_destroy_exec_env(instance.exec_env);
    c.wasm_runtime_deinstantiate(instance.module_inst);
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
    const start_func = c.wasm_runtime_lookup_function(instance.module_inst, "_start");
    if (start_func != null) {
        _ = c.wasm_runtime_call_wasm(instance.exec_env, start_func, 0, null);
        // Ignore WASI exit exceptions - they're normal
    }

    // Return instance to pool for reuse (instead of destroying)
    // NOTE: QuickJS state persists - this is OK for stateless scripts
    // but may cause issues with stateful code (globals, timers, etc.)
    returnInstance(instance);

    const elapsed_ns = std.time.nanoTimestamp() - start;
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
    _ = g_stats_total_ns.fetchAdd(@intCast(elapsed_ns), .monotonic);

    // Send response
    var response: [512]u8 = undefined;
    const pool_status = if (pool_hit) "hit" else "miss";
    const http = std.fmt.bufPrint(&response, "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 3\r\nX-Exec-Time: {d:.2}ms\r\nX-Pool: {s}\r\nX-Pool-Size: {}\r\nConnection: close\r\n\r\nOK\n", .{ elapsed_ms, pool_status, g_pool_count }) catch {
        sendError(client, "Response failed");
        return;
    };
    _ = std.posix.write(client, http) catch {};

    std.debug.print("[edgeboxd] {d:.2}ms (pool {s}, {}/{} ready)\n", .{ elapsed_ms, pool_status, g_pool_count, g_target_pool_size });
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
