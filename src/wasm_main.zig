/// EdgeBox WASM Entry Point
/// QuickJS runtime for WASI with networking support
///
/// Supports Wizer pre-initialization for instant startup:
/// - If wizer_init was called at build time, uses pre-initialized runtime
/// - Otherwise falls back to traditional runtime creation
const std = @import("std");
const quickjs = @import("quickjs_core.zig");
const qjs = quickjs.c;
const wasm_fetch = @import("wasm_fetch.zig");
const wasi_tty = @import("wasi_tty.zig");
const wasi_process = @import("wasi_process.zig");
const snapshot = @import("snapshot.zig");
const pool_alloc = @import("wasm_pool_alloc.zig");
const wizer_mod = @import("wizer_init.zig");
const wasm_zlib = @import("wasm_zlib.zig");
const build_options = @import("build_options");
const wasi_nn = if (build_options.enable_wasi_nn) @import("wasi_nn.zig") else struct {
    pub fn chat(_: []const u8, _: []u8) ![]u8 {
        return error.NotEnabled;
    }
    pub fn isAvailable() bool {
        return false;
    }
};

// Split modules
const dispatch = @import("wasm/dispatch.zig");
const native_bindings = @import("wasm/native_bindings.zig");

// Re-export dispatch items for local use
const socket_dispatch = dispatch.socket_dispatch;
const spawn_dispatch = dispatch.spawn_dispatch;
const http_dispatch = dispatch.http_dispatch;
const file_dispatch = dispatch.file_dispatch;
const zlib_dispatch = dispatch.zlib_dispatch;
const crypto_dispatch = dispatch.crypto_dispatch;
const stdlib_dispatch = dispatch.stdlib_dispatch;
const process_cm_dispatch = dispatch.process_cm_dispatch;
const socket_host = dispatch.socket_host;

// Re-export all opcodes
const SOCKET_OP_CREATE = dispatch.SOCKET_OP_CREATE;
const SOCKET_OP_BIND = dispatch.SOCKET_OP_BIND;
const SOCKET_OP_LISTEN = dispatch.SOCKET_OP_LISTEN;
const SOCKET_OP_ACCEPT = dispatch.SOCKET_OP_ACCEPT;
const SOCKET_OP_CONNECT = dispatch.SOCKET_OP_CONNECT;
const SOCKET_OP_WRITE = dispatch.SOCKET_OP_WRITE;
const SOCKET_OP_READ = dispatch.SOCKET_OP_READ;
const SOCKET_OP_GET_READ_DATA = dispatch.SOCKET_OP_GET_READ_DATA;
const SOCKET_OP_CLOSE = dispatch.SOCKET_OP_CLOSE;
const SOCKET_OP_STATE = dispatch.SOCKET_OP_STATE;
const SPAWN_OP_START = dispatch.SPAWN_OP_START;
const SPAWN_OP_POLL = dispatch.SPAWN_OP_POLL;
const SPAWN_OP_OUTPUT_LEN = dispatch.SPAWN_OP_OUTPUT_LEN;
const SPAWN_OP_OUTPUT = dispatch.SPAWN_OP_OUTPUT;
const SPAWN_OP_FREE = dispatch.SPAWN_OP_FREE;
const HTTP_OP_REQUEST = dispatch.HTTP_OP_REQUEST;
const HTTP_OP_GET_RESPONSE_LEN = dispatch.HTTP_OP_GET_RESPONSE_LEN;
const HTTP_OP_GET_RESPONSE = dispatch.HTTP_OP_GET_RESPONSE;
const HTTP_OP_START_ASYNC = dispatch.HTTP_OP_START_ASYNC;
const HTTP_OP_POLL = dispatch.HTTP_OP_POLL;
const HTTP_OP_RESPONSE_LEN = dispatch.HTTP_OP_RESPONSE_LEN;
const HTTP_OP_RESPONSE = dispatch.HTTP_OP_RESPONSE;
const HTTP_OP_FREE = dispatch.HTTP_OP_FREE;
const FILE_OP_READ_START = dispatch.FILE_OP_READ_START;
const FILE_OP_WRITE_START = dispatch.FILE_OP_WRITE_START;
const FILE_OP_POLL = dispatch.FILE_OP_POLL;
const FILE_OP_RESULT_LEN = dispatch.FILE_OP_RESULT_LEN;
const FILE_OP_RESULT = dispatch.FILE_OP_RESULT;
const FILE_OP_FREE = dispatch.FILE_OP_FREE;
const ZLIB_OP_GZIP = dispatch.ZLIB_OP_GZIP;
const ZLIB_OP_GUNZIP = dispatch.ZLIB_OP_GUNZIP;
const ZLIB_OP_DEFLATE = dispatch.ZLIB_OP_DEFLATE;
const ZLIB_OP_INFLATE = dispatch.ZLIB_OP_INFLATE;
const ZLIB_OP_GET_RESULT = dispatch.ZLIB_OP_GET_RESULT;
const CRYPTO_OP_HASH = dispatch.CRYPTO_OP_HASH;
const CRYPTO_OP_HMAC = dispatch.CRYPTO_OP_HMAC;
const CRYPTO_OP_RANDOM_BYTES = dispatch.CRYPTO_OP_RANDOM_BYTES;
const CRYPTO_OP_GET_RESULT_LEN = dispatch.CRYPTO_OP_GET_RESULT_LEN;
const CRYPTO_OP_GET_RESULT = dispatch.CRYPTO_OP_GET_RESULT;
const USE_COMPONENT_MODEL_CRYPTO = dispatch.USE_COMPONENT_MODEL_CRYPTO;
const USE_COMPONENT_MODEL_FS = dispatch.USE_COMPONENT_MODEL_FS;
const USE_COMPONENT_MODEL_PROCESS = dispatch.USE_COMPONENT_MODEL_PROCESS;
const PROCESS_CM_SPAWN_SYNC = dispatch.PROCESS_CM_SPAWN_SYNC;
const PROCESS_CM_GET_RESULT_LEN = dispatch.PROCESS_CM_GET_RESULT_LEN;
const PROCESS_CM_GET_RESULT = dispatch.PROCESS_CM_GET_RESULT;
const FILE_CM_READ = dispatch.FILE_CM_READ;
const FILE_CM_WRITE = dispatch.FILE_CM_WRITE;
const FILE_CM_EXISTS = dispatch.FILE_CM_EXISTS;
const FILE_CM_STAT = dispatch.FILE_CM_STAT;
const FILE_CM_READDIR = dispatch.FILE_CM_READDIR;
const FILE_CM_MKDIR = dispatch.FILE_CM_MKDIR;
const FILE_CM_UNLINK = dispatch.FILE_CM_UNLINK;
const FILE_CM_RMDIR = dispatch.FILE_CM_RMDIR;
const FILE_CM_RENAME = dispatch.FILE_CM_RENAME;
const FILE_CM_COPY = dispatch.FILE_CM_COPY;
const FILE_CM_GET_RESULT_LEN = dispatch.FILE_CM_GET_RESULT_LEN;
const FILE_CM_GET_RESULT = dispatch.FILE_CM_GET_RESULT;
const STDLIB_OP_ARRAY_NEW = dispatch.STDLIB_OP_ARRAY_NEW;
const STDLIB_OP_ARRAY_PUSH = dispatch.STDLIB_OP_ARRAY_PUSH;
const STDLIB_OP_ARRAY_POP = dispatch.STDLIB_OP_ARRAY_POP;
const STDLIB_OP_ARRAY_GET = dispatch.STDLIB_OP_ARRAY_GET;
const STDLIB_OP_ARRAY_SET = dispatch.STDLIB_OP_ARRAY_SET;
const STDLIB_OP_ARRAY_LEN = dispatch.STDLIB_OP_ARRAY_LEN;
const STDLIB_OP_ARRAY_SORT = dispatch.STDLIB_OP_ARRAY_SORT;
const STDLIB_OP_ARRAY_FREE = dispatch.STDLIB_OP_ARRAY_FREE;
const STDLIB_OP_ARRAY_SORT_DESC = dispatch.STDLIB_OP_ARRAY_SORT_DESC;
const STDLIB_OP_ARRAY_REVERSE = dispatch.STDLIB_OP_ARRAY_REVERSE;
const STDLIB_OP_MAP_NEW = dispatch.STDLIB_OP_MAP_NEW;
const STDLIB_OP_MAP_SET = dispatch.STDLIB_OP_MAP_SET;
const STDLIB_OP_MAP_GET = dispatch.STDLIB_OP_MAP_GET;
const STDLIB_OP_MAP_HAS = dispatch.STDLIB_OP_MAP_HAS;
const STDLIB_OP_MAP_DELETE = dispatch.STDLIB_OP_MAP_DELETE;
const STDLIB_OP_MAP_LEN = dispatch.STDLIB_OP_MAP_LEN;
const STDLIB_OP_MAP_FREE = dispatch.STDLIB_OP_MAP_FREE;
const STDLIB_OP_MAP_CLEAR = dispatch.STDLIB_OP_MAP_CLEAR;
const STDLIB_OP_ARRAY_CLEAR = dispatch.STDLIB_OP_ARRAY_CLEAR;
const STDLIB_OP_ARRAY_INDEX_OF = dispatch.STDLIB_OP_ARRAY_INDEX_OF;

// Re-export native binding functions
const nativeFetch = native_bindings.nativeFetch;
const nativeIsatty = native_bindings.nativeIsatty;
const nativeGetTerminalSize = native_bindings.nativeGetTerminalSize;
const nativeReadStdin = native_bindings.nativeReadStdin;
const nativeStdinReady = native_bindings.nativeStdinReady;
const nativeSpawn = native_bindings.nativeSpawn;
const nativeFsRead = native_bindings.nativeFsRead;
const nativeFsWrite = native_bindings.nativeFsWrite;
const nativeFsExists = native_bindings.nativeFsExists;
const nativeFsStat = native_bindings.nativeFsStat;
const nativeFsReaddir = native_bindings.nativeFsReaddir;
const nativeFsMkdir = native_bindings.nativeFsMkdir;
const nativeFsUnlink = native_bindings.nativeFsUnlink;
const nativeFsRmdir = native_bindings.nativeFsRmdir;
const nativeFsRename = native_bindings.nativeFsRename;
const nativeFsCopy = native_bindings.nativeFsCopy;
const nativeCwd = native_bindings.nativeCwd;
const nativeHomedir = native_bindings.nativeHomedir;
const nativeGunzip = native_bindings.nativeGunzip;
const nativeInflate = native_bindings.nativeInflate;
const nativeInflateZlib = native_bindings.nativeInflateZlib;
const nativeGzip = native_bindings.nativeGzip;
const nativeDeflate = native_bindings.nativeDeflate;
const nativeDeflateZlib = native_bindings.nativeDeflateZlib;
const nativeHash = native_bindings.nativeHash;
const nativeHmac = native_bindings.nativeHmac;
const nativeAIChat = native_bindings.nativeAIChat;
const nativeAIAvailable = native_bindings.nativeAIAvailable;
const nativeArrayNew = native_bindings.nativeArrayNew;
const nativeArrayPush = native_bindings.nativeArrayPush;
const nativeArrayPop = native_bindings.nativeArrayPop;
const nativeArrayGet = native_bindings.nativeArrayGet;
const nativeArraySet = native_bindings.nativeArraySet;
const nativeArrayLen = native_bindings.nativeArrayLen;
const nativeArraySort = native_bindings.nativeArraySort;
const nativeArraySortDesc = native_bindings.nativeArraySortDesc;
const nativeArrayReverse = native_bindings.nativeArrayReverse;
const nativeArrayClear = native_bindings.nativeArrayClear;
const nativeArrayIndexOf = native_bindings.nativeArrayIndexOf;
const nativeArrayFree = native_bindings.nativeArrayFree;
const nativeMapNew = native_bindings.nativeMapNew;
const nativeMapSet = native_bindings.nativeMapSet;
const nativeMapGet = native_bindings.nativeMapGet;
const nativeMapHas = native_bindings.nativeMapHas;
const nativeMapDelete = native_bindings.nativeMapDelete;
const nativeMapLen = native_bindings.nativeMapLen;
const nativeMapClear = native_bindings.nativeMapClear;
const nativeMapFree = native_bindings.nativeMapFree;
const nativeSocketCreate = native_bindings.nativeSocketCreate;
const nativeSocketBind = native_bindings.nativeSocketBind;
const nativeSocketListen = native_bindings.nativeSocketListen;
const nativeSocketAccept = native_bindings.nativeSocketAccept;
const nativeSocketConnect = native_bindings.nativeSocketConnect;
const nativeSocketWrite = native_bindings.nativeSocketWrite;
const nativeSocketRead = native_bindings.nativeSocketRead;
const nativeSocketClose = native_bindings.nativeSocketClose;
const nativeSocketState = native_bindings.nativeSocketState;
const nativeJsonParse = native_bindings.nativeJsonParse;
const nativeJsonStringify = native_bindings.nativeJsonStringify;
const nativeBigIntFromString = native_bindings.nativeBigIntFromString;
const nativeBigIntFromNumber = native_bindings.nativeBigIntFromNumber;
const nativeBigIntToString = native_bindings.nativeBigIntToString;
const nativeBigIntAdd = native_bindings.nativeBigIntAdd;
const nativeBigIntSub = native_bindings.nativeBigIntSub;
const nativeBigIntMul = native_bindings.nativeBigIntMul;
const nativeBigIntDiv = native_bindings.nativeBigIntDiv;
const nativeBigIntMod = native_bindings.nativeBigIntMod;
const nativeBigIntCmp = native_bindings.nativeBigIntCmp;
const nativeBigIntFree = native_bindings.nativeBigIntFree;
const nativeWsConnect = native_bindings.nativeWsConnect;
const nativeWsSend = native_bindings.nativeWsSend;
const nativeWsSendBinary = native_bindings.nativeWsSendBinary;
const nativeWsRecv = native_bindings.nativeWsRecv;
const nativeWsClose = native_bindings.nativeWsClose;
const nativeWsState = native_bindings.nativeWsState;
const nativeWsPing = native_bindings.nativeWsPing;
const jsBool = native_bindings.jsBool;
const computePolyfillsHash = native_bindings.computePolyfillsHash;

// Export wizer_init for Wizer to call at build time
export fn wizer_init() void {
    wizer_mod.wizer_init();
}

// Static buffer for receiving JS code from daemon (avoids malloc after CoW restore)
var serve_code_buffer: [64 * 1024]u8 = undefined; // 64KB should be enough for most code
var serve_code_len: usize = 0;

/// Get pointer to the code receive buffer (for host to write to)
export fn edgebox_get_code_buffer() [*]u8 {
    return &serve_code_buffer;
}

/// Get the size of the code receive buffer
export fn edgebox_get_code_buffer_size() usize {
    return serve_code_buffer.len;
}

/// Execute JS code in serve mode using pre-initialized runtime.
/// Called by the daemon for fast execution after CoW memory restore.
/// The daemon should write code to the buffer returned by edgebox_get_code_buffer first.
/// Returns: 0 on success, non-zero on error
export fn edgebox_serve_exec(code_ptr: [*]const u8, code_len: usize) i32 {
    const code = code_ptr[0..code_len];

    // Debug: Check wizer_context address and value
    std.debug.print("[serve_exec] wizer_context ptr: {*}, value: {?}\n", .{ &wizer_mod.wizer_context, wizer_mod.wizer_context });

    // Get the pre-initialized context
    const ctx = wizer_mod.getContext() orelse {
        std.debug.print("[serve_exec] Error: context not initialized\n", .{});
        return -1;
    };

    // Evaluate the JS code using the CoW-restored context
    const result = qjs.JS_Eval(ctx, code.ptr, code.len, "<serve>", qjs.JS_EVAL_TYPE_GLOBAL);

    if (qjs.JS_IsException(result)) {
        const exception = qjs.JS_GetException(ctx);
        defer qjs.JS_FreeValue(ctx, exception);
        const str = qjs.JS_ToCString(ctx, exception);
        if (str != null) {
            std.debug.print("{s}\n", .{str});
            qjs.JS_FreeCString(ctx, str);
        }
        return -1;
    }

    // If result is a promise, run the event loop
    if (qjs.JS_IsPromise(result)) {
        qjs.JS_FreeValue(ctx, result);
        _ = qjs.js_std_loop(ctx);
    } else {
        qjs.JS_FreeValue(ctx, result);
    }

    std.debug.print("[serve_exec] Done\n", .{});
    return 0;
}

// Frozen functions - pre-compiled hot paths for 18x speedup
extern fn frozen_fib_init(ctx: ?*anyopaque) c_int;

/// Check if running in serve mode (daemon will capture memory after _start)
/// The daemon passes EDGEBOX_SERVE_MODE=1 via WASI env to signal this
fn isServeMode() bool {
    const result = std.posix.getenv("EDGEBOX_SERVE_MODE") != null;
    std.debug.print("[wasm] isServeMode check: {}\n", .{result});
    return result;
}

// Global allocator for native bindings - shared with native_bindings module
var global_allocator: ?std.mem.Allocator = null;

// Startup timing for cold start measurement
var startup_time_ns: i128 = 0;

/// Read file and add null terminator (QuickJS requires null-terminated source for some operations)
/// Returns slice excluding the null terminator, but the buffer has it appended
fn readFileNullTerminated(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const max_size = 50 * 1024 * 1024; // 50MB max
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        return err;
    };
    defer file.close();

    const stat = try file.stat();
    const file_size: usize = @intCast(stat.size);
    if (file_size > max_size) return error.FileTooBig;

    // Allocate with +1 for null terminator
    const buf = try allocator.alloc(u8, file_size + 1);
    const bytes_read = try file.readAll(buf[0..file_size]);
    if (bytes_read != file_size) return error.UnexpectedEof;

    // Add null terminator
    buf[file_size] = 0;

    // Return slice without the null terminator (but buffer has it)
    return buf[0..file_size];
}

/// Get current time in nanoseconds (WASI clock)
fn getTimeNs() i128 {
    if (@import("builtin").target.os.tag == .wasi) {
        var ts: u64 = undefined;
        const rc = std.os.wasi.clock_time_get(.MONOTONIC, 1, &ts);
        if (rc == .SUCCESS) {
            return @as(i128, ts);
        }
    }
    return std.time.nanoTimestamp();
}

/// Check if bump allocator is requested via __EDGEBOX_ALLOCATOR env var
/// This is called early before QuickJS init to choose the right allocator
fn shouldUseBumpAllocator() bool {
    if (@import("builtin").target.cpu.arch != .wasm32) {
        return false; // Non-WASM always uses system allocator
    }

    // Read environ via WASI early (before QuickJS)
    var environ_count: usize = 0;
    var environ_buf_size: usize = 0;
    _ = std.os.wasi.environ_sizes_get(&environ_count, &environ_buf_size);

    if (environ_count == 0 or environ_buf_size == 0) return false;

    // Use page allocator for this early check
    const page_alloc = std.heap.page_allocator;
    const environ_ptrs = page_alloc.alloc([*:0]u8, environ_count) catch return false;
    defer page_alloc.free(environ_ptrs);

    const environ_buf = page_alloc.alloc(u8, environ_buf_size) catch return false;
    defer page_alloc.free(environ_buf);

    _ = std.os.wasi.environ_get(environ_ptrs.ptr, environ_buf.ptr);

    for (environ_ptrs) |env_ptr| {
        const env = std.mem.span(env_ptr);
        if (std.mem.startsWith(u8, env, "__EDGEBOX_ALLOCATOR=")) {
            const value = env["__EDGEBOX_ALLOCATOR=".len..];
            return std.mem.eql(u8, value, "bump");
        }
    }

    return false;
}

pub fn main() !void {
    // Record startup time for cold start measurement
    startup_time_ns = getTimeNs();

    // On WASM, use page allocator instead of GPA to avoid "Invalid free" panic
    // GPA tracks allocations and panics when freed memory wasn't allocated by it
    const allocator = if (@import("builtin").target.cpu.arch == .wasm32)
        std.heap.page_allocator
    else blk: {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        break :blk gpa.allocator();
    };
    global_allocator = allocator;
    native_bindings.global_allocator = allocator;

    const args = try std.process.argsAlloc(allocator);
    // On WASM, skip argsFree to avoid potential "Invalid free" panic
    if (@import("builtin").target.cpu.arch != .wasm32) {
        defer std.process.argsFree(allocator, args);
    }

    // Check serve mode FIRST - daemon passes EDGEBOX_SERVE_MODE=1
    // In serve mode, we initialize the runtime and return (daemon captures state via CoW)
    if (isServeMode()) {
        try initializeServeMode(allocator);
        return;
    }

    if (args.len < 2) {
        std.debug.print(
            \\EdgeBox - QuickJS WASM Runtime (WAMR)
            \\
            \\Usage:
            \\  edgebox <script.js>     Run JavaScript file
            \\  edgebox -e "<code>"     Evaluate JavaScript code
            \\
            \\Features:
            \\  - QuickJS JavaScript engine
            \\  - WASI filesystem access
            \\  - Network sockets
            \\  - Automatic bytecode caching for fast restarts
            \\
        , .{});
        return;
    }

    const cmd = args[1];
    const is_eval = std.mem.eql(u8, cmd, "-e") and args.len > 2;
    const is_compile_only = std.mem.eql(u8, cmd, "--compile-only") and args.len > 2;
    const is_benchmark = std.mem.eql(u8, cmd, "--benchmark");
    const is_cold_start_test = std.mem.eql(u8, cmd, "--cold-start");

    // Handle benchmark mode
    if (is_benchmark or is_cold_start_test) {
        const end_time = getTimeNs();
        const startup_ms = @as(f64, @floatFromInt(end_time - startup_time_ns)) / 1_000_000.0;
        const wizer_status = if (wizer_mod.isWizerInitialized()) " (Wizer)" else "";
        std.debug.print("Cold start{s}: {d:.2}ms\n", .{ wizer_status, startup_ms });

        if (is_cold_start_test) {
            // Just measure cold start, don't run anything else
            return;
        }
    }

    // WIZER FAST PATH: Use pre-initialized runtime if available
    if (wizer_mod.isWizerInitialized()) {
        // Must use process.exit to avoid GPA deinit - Wizer context uses bump allocator
        // which was snapshotted at build time. GPA.deinit would try to free memory
        // that wasn't allocated by GPA, causing "Invalid free" panic.
        runWithWizerRuntime(allocator, args, cmd, is_eval, is_compile_only) catch |err| {
            std.debug.print("Wizer runtime error: {}\n", .{err});
            std.process.exit(1);
        };
        std.process.exit(0); // Exit without running defers to avoid GPA deinit
    }

    // SLOW PATH: Traditional initialization (fallback if Wizer wasn't used)
    // Use system allocator (libc malloc) which properly reclaims freed memory
    // This is essential for sandbox environments with fixed memory limits

    // Determine script args to pass to JS
    // For eval mode: args after -e "<code>"
    // For file mode: args after <script.js>
    const script_args_start: usize = if (is_eval) 3 else 2;
    const script_args = if (script_args_start < args.len) args[script_args_start..] else &[_][:0]const u8{};

    // Convert Zig args to C-style argv for js_std_add_helpers
    // scriptArgs in QuickJS = argv[1:] when using js_std_add_helpers
    // So we prepend the script name to make scriptArgs work correctly
    var c_argv = try allocator.alloc([*c]u8, script_args.len + 1);
    defer allocator.free(c_argv);

    // First element is the script name (or "-e" for eval mode)
    const script_name = if (is_eval) "-e" else cmd;
    c_argv[0] = @constCast(@ptrCast(script_name.ptr));

    // Rest are the script arguments
    for (script_args, 0..) |arg, i| {
        c_argv[i + 1] = @constCast(@ptrCast(arg.ptr));
    }

    // Check if we're in serve mode FIRST (before any allocations)
    // In serve mode, we keep the runtime/context alive for serve_exec calls
    const serve_mode = isServeMode();

    // Create QuickJS runtime
    // Choose allocator based on config: bump for serverless, system for sandbox
    const use_bump = shouldUseBumpAllocator();
    var runtime = if (use_bump)
        try quickjs.Runtime.initWithBumpAllocator(allocator)
    else
        try quickjs.Runtime.init(allocator);
    defer if (!serve_mode) runtime.deinit();

    // Update global_allocator to match what QuickJS uses
    // This ensures polyfill loading uses the same allocator
    global_allocator = runtime.allocator;
    native_bindings.global_allocator = runtime.allocator;

    // CRITICAL: Initialize std handlers FIRST, before any JS code runs
    // This sets up the event loop handlers needed for timers and promises
    qjs.js_std_init_handlers(runtime.inner);

    var context = try runtime.newStdContextWithArgs(@intCast(c_argv.len), c_argv.ptr);
    defer if (!serve_mode) context.deinit();

    // Initialize frozen functions (pre-compiled hot paths for 18x speedup)
    // This registers frozen_fib() as a global JS function
    _ = frozen_fib_init(context.inner);

    // Inject minimal bootstrap only - polyfills are lazy-loaded on first use
    // This dramatically improves cold start time
    injectMinimalBootstrap(&context) catch |err| {
        std.debug.print("Warning: Failed to inject bootstrap: {}\n", .{err});
    };

    // In serve mode, save context for later serve_exec calls and return early
    if (serve_mode) {
        wizer_mod.wizer_context = context.inner;
        return; // Let daemon capture memory state
    }

    if (is_compile_only) {
        // Compile-only mode: just generate bytecode cache, don't execute
        // Used by build.sh to pre-compile at build time
        try compileOnly(allocator, &context, args[2]);
    } else if (is_eval) {
        // Eval mode
        const result = context.eval(args[2]) catch |err| {
            std.debug.print("Error: {}\n", .{err});
            // Try to get exception details
            if (context.getException()) |exc| {
                defer exc.free();
                if (exc.toStringSlice()) |msg| {
                    std.debug.print("Exception: {s}\n", .{msg});
                }
            }
            std.process.exit(1);
        };
        defer result.free();

        if (!result.isUndefined()) {
            if (result.toStringSlice()) |str| {
                std.debug.print("{s}\n", .{str});
            }
        }
    } else {
        // Run file with automatic bytecode caching
        try runFileWithCache(allocator, &context, cmd);
    }

    // Run pending Promise jobs (microtasks)
    // This is critical because js_std_loop may return early if there are no timers/I/O
    {
        const ctx = context.getRaw();
        const rt = qjs.JS_GetRuntime(ctx);
        var pending_ctx: ?*qjs.JSContext = null;
        while (qjs.JS_ExecutePendingJob(rt, &pending_ctx) > 0) {}
    }

    // Run the event loop for async operations (timers, promises, I/O)
    // Note: js_std_loop may internally try to free memory that was allocated differently
    // which can cause "Invalid free" on WASM. We exit after to prevent defer cleanup issues.
    _ = qjs.js_std_loop(context.getRaw());

    // On WASM, exit immediately to skip defer cleanup (context.deinit, runtime.deinit)
    // Memory is reclaimed when the WASM instance exits anyway.
    if (@import("builtin").target.cpu.arch == .wasm32) {
        std.process.exit(0);
    }
}

// ============================================================================
// SERVE MODE INITIALIZATION (for CoW daemon)
// ============================================================================

/// Initialize runtime for serve mode - called when EDGEBOX_SERVE_MODE=1
/// This creates the QuickJS runtime/context and saves it for serve_exec calls.
/// The daemon will capture this memory state via CoW after _start returns.
fn initializeServeMode(allocator: std.mem.Allocator) !void {
    std.debug.print("[wasm] Initializing serve mode...\n", .{});

    // Create QuickJS runtime with BUMP allocator for CoW compatibility
    // The bump allocator's state is simple (just a pointer) and survives CoW restore
    // Unlike dlmalloc which has complex internal heap structures that break after CoW
    var runtime = try quickjs.Runtime.initWithBumpAllocator(allocator);
    // DON'T defer runtime.deinit() - we keep it alive for serve_exec

    // Initialize std handlers for event loop
    qjs.js_std_init_handlers(runtime.inner);

    // Create dummy argv for context init
    var c_argv = [_][*c]u8{@constCast(@ptrCast("edgebox"))};
    var context = try runtime.newStdContextWithArgs(1, &c_argv);
    // DON'T defer context.deinit() - we keep it alive for serve_exec

    // Initialize frozen functions
    _ = frozen_fib_init(context.inner);

    // Inject minimal bootstrap
    injectMinimalBootstrap(&context) catch |err| {
        std.debug.print("[wasm] Warning: Failed to inject bootstrap: {}\n", .{err});
    };

    // Save context for serve_exec calls
    wizer_mod.wizer_context = context.inner;

    std.debug.print("[wasm] Serve mode initialized, context saved at {*} = {?}\n", .{ &wizer_mod.wizer_context, wizer_mod.wizer_context });
}

// ============================================================================
// WIZER FAST PATH
// ============================================================================

/// Run with Wizer pre-initialized runtime (instant startup)
fn runWithWizerRuntime(
    allocator: std.mem.Allocator,
    args: []const [:0]u8,
    cmd: [:0]const u8,
    is_eval: bool,
    is_compile_only: bool,
) !void {
    const ctx = wizer_mod.getContext() orelse return error.WizerNotInitialized;

    // Initialize std helpers (print, console, etc.) - must be done at runtime
    // because they depend on stdout/stderr file descriptors
    // Note: Converting args to C-style argv for js_std_add_helpers
    var c_argv: [128][*c]u8 = undefined;
    var argc: c_int = 0;
    for (args) |arg| {
        if (argc >= 128) break;
        c_argv[@intCast(argc)] = @ptrCast(@constCast(arg.ptr));
        argc += 1;
    }
    qjs.js_std_add_helpers(ctx, argc, &c_argv);

    // Bind dynamic state (process.argv, process.env) - these change per request
    bindDynamicState(ctx, args);

    // Register native bindings (fetch, isatty, spawn, etc.)
    registerWizerNativeBindings(ctx);

    if (is_compile_only) {
        // Compile-only not supported with Wizer (use slow path)
        std.debug.print("Note: --compile-only uses slow path\n", .{});
        return;
    }

    if (is_eval) {
        // Eval mode
        const code = args[2];
        const val = qjs.JS_Eval(ctx, code.ptr, code.len, "<eval>", qjs.JS_EVAL_TYPE_GLOBAL);
        if (qjs.JS_IsException(val)) {
            printWizerException(ctx);
            std.process.exit(1);
        }
        qjs.JS_FreeValue(ctx, val);
    } else {
        // Run file
        try runFileWithWizer(allocator, ctx, cmd);
    }

    // Run the event loop for async operations (timers, promises, I/O)
    _ = qjs.js_std_loop(ctx);
}

/// Bind dynamic state that changes per request (process.argv, process.env)
/// Note: std/os modules are already imported at Wizer build time
fn bindDynamicState(ctx: *quickjs.c.JSContext, args: []const [:0]u8) void {
    // Minimal process setup - std is already available from Wizer
    const dynamic_init =
        \\globalThis.process = globalThis.process || {};
        \\globalThis.process.version = 'v20.0.0';
        \\globalThis.process.versions = { node: '20.0.0' };
        \\globalThis.process.platform = 'wasi';
        \\globalThis.process.arch = 'wasm32';
        \\globalThis.process.exit = (code) => std.exit(code || 0);
        \\globalThis.process.cwd = () => std.getenv('PWD') || '/';
        \\globalThis.process.env = new Proxy({}, {
        \\    get(t, n) { return typeof n === 'symbol' ? undefined : std.getenv(String(n)); },
        \\    has(t, n) { return typeof n !== 'symbol' && std.getenv(String(n)) !== undefined; }
        \\});
    ;

    const val = qjs.JS_Eval(ctx, dynamic_init.ptr, dynamic_init.len, "<dynamic>", qjs.JS_EVAL_TYPE_GLOBAL);
    if (qjs.JS_IsException(val)) {
        std.debug.print("Dynamic init failed\n", .{});
        printWizerException(ctx);
    }
    qjs.JS_FreeValue(ctx, val);

    // Set process.argv from actual command line
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const process = qjs.JS_GetPropertyStr(ctx, global, "process");
    defer qjs.JS_FreeValue(ctx, process);

    const argv_arr = qjs.JS_NewArray(ctx);
    var idx: u32 = 0;

    // argv[0] = 'node' (for compatibility)
    const node_str = qjs.JS_NewString(ctx, "node");
    _ = qjs.JS_SetPropertyUint32(ctx, argv_arr, idx, node_str);
    idx += 1;

    // argv[1..] = actual args
    for (args[1..]) |arg| {
        const str = qjs.JS_NewStringLen(ctx, arg.ptr, arg.len);
        _ = qjs.JS_SetPropertyUint32(ctx, argv_arr, idx, str);
        idx += 1;
    }

    _ = qjs.JS_SetPropertyStr(ctx, process, "argv", argv_arr);
}

/// Register native bindings for Wizer context (uses raw JSContext)
fn registerWizerNativeBindings(ctx: *quickjs.c.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Register native functions
    inline for (.{
        .{ "__edgebox_fetch", nativeFetch, 4 },
        .{ "__edgebox_isatty", nativeIsatty, 1 },
        .{ "__edgebox_get_terminal_size", nativeGetTerminalSize, 0 },
        .{ "__edgebox_read_stdin", nativeReadStdin, 1 },
        .{ "__edgebox_stdin_ready", nativeStdinReady, 0 },
        .{ "__edgebox_spawn", nativeSpawn, 4 },
        .{ "__edgebox_load_polyfills", nativeLoadPolyfills, 0 },
        // fs bindings
        .{ "__edgebox_fs_read", nativeFsRead, 1 },
        .{ "__edgebox_fs_write", nativeFsWrite, 2 },
        .{ "__edgebox_fs_exists", nativeFsExists, 1 },
        .{ "__edgebox_fs_stat", nativeFsStat, 1 },
        .{ "__edgebox_fs_readdir", nativeFsReaddir, 1 },
        .{ "__edgebox_fs_mkdir", nativeFsMkdir, 2 },
        .{ "__edgebox_fs_unlink", nativeFsUnlink, 1 },
        .{ "__edgebox_fs_rmdir", nativeFsRmdir, 2 },
        .{ "__edgebox_fs_rename", nativeFsRename, 2 },
        .{ "__edgebox_fs_copy", nativeFsCopy, 2 },
        .{ "__edgebox_cwd", nativeCwd, 0 },
        .{ "__edgebox_homedir", nativeHomedir, 0 },
        // zlib bindings
        .{ "__edgebox_gunzip", nativeGunzip, 1 },
        .{ "__edgebox_inflate", nativeInflate, 1 },
        .{ "__edgebox_inflate_zlib", nativeInflateZlib, 1 },
        .{ "__edgebox_gzip", nativeGzip, 1 },
        .{ "__edgebox_deflate", nativeDeflate, 1 },
        .{ "__edgebox_deflate_zlib", nativeDeflateZlib, 1 },
        // crypto bindings
        .{ "__edgebox_hash", nativeHash, 2 },
        .{ "__edgebox_hmac", nativeHmac, 3 },
        // WASI-NN AI bindings
        .{ "__edgebox_ai_chat", nativeAIChat, 1 },
        .{ "__edgebox_ai_available", nativeAIAvailable, 0 },
        // Stdlib bindings (HostArray, HostMap)
        .{ "__edgebox_array_new", nativeArrayNew, 0 },
        .{ "__edgebox_array_push", nativeArrayPush, 2 },
        .{ "__edgebox_array_pop", nativeArrayPop, 1 },
        .{ "__edgebox_array_get", nativeArrayGet, 2 },
        .{ "__edgebox_array_set", nativeArraySet, 3 },
        .{ "__edgebox_array_len", nativeArrayLen, 1 },
        .{ "__edgebox_array_sort", nativeArraySort, 1 },
        .{ "__edgebox_array_sort_desc", nativeArraySortDesc, 1 },
        .{ "__edgebox_array_reverse", nativeArrayReverse, 1 },
        .{ "__edgebox_array_clear", nativeArrayClear, 1 },
        .{ "__edgebox_array_index_of", nativeArrayIndexOf, 2 },
        .{ "__edgebox_array_free", nativeArrayFree, 1 },
        .{ "__edgebox_map_new", nativeMapNew, 0 },
        .{ "__edgebox_map_set", nativeMapSet, 3 },
        .{ "__edgebox_map_get", nativeMapGet, 2 },
        .{ "__edgebox_map_has", nativeMapHas, 2 },
        .{ "__edgebox_map_delete", nativeMapDelete, 2 },
        .{ "__edgebox_map_len", nativeMapLen, 1 },
        .{ "__edgebox_map_clear", nativeMapClear, 1 },
        .{ "__edgebox_map_free", nativeMapFree, 1 },
        // SIMD JSON
        .{ "__edgebox_json_parse", nativeJsonParse, 1 },
        .{ "__edgebox_json_stringify", nativeJsonStringify, 1 },
        // BigInt
        .{ "__edgebox_bigint_from_string", nativeBigIntFromString, 2 },
        .{ "__edgebox_bigint_from_number", nativeBigIntFromNumber, 1 },
        .{ "__edgebox_bigint_to_string", nativeBigIntToString, 2 },
        .{ "__edgebox_bigint_add", nativeBigIntAdd, 2 },
        .{ "__edgebox_bigint_sub", nativeBigIntSub, 2 },
        .{ "__edgebox_bigint_mul", nativeBigIntMul, 2 },
        .{ "__edgebox_bigint_div", nativeBigIntDiv, 2 },
        .{ "__edgebox_bigint_mod", nativeBigIntMod, 2 },
        .{ "__edgebox_bigint_cmp", nativeBigIntCmp, 2 },
        .{ "__edgebox_bigint_free", nativeBigIntFree, 1 },
        // WebSocket
        .{ "__edgebox_ws_connect", nativeWsConnect, 1 },
        .{ "__edgebox_ws_send", nativeWsSend, 2 },
        .{ "__edgebox_ws_send_binary", nativeWsSendBinary, 2 },
        .{ "__edgebox_ws_recv", nativeWsRecv, 1 },
        .{ "__edgebox_ws_close", nativeWsClose, 1 },
        .{ "__edgebox_ws_state", nativeWsState, 1 },
        .{ "__edgebox_ws_ping", nativeWsPing, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, global, binding[0], func);
    }
}

/// Run file with Wizer context
/// NOTE: This function intentionally does NOT free memory - it relies on process exit
/// to reclaim all memory. This avoids conflicts between GPA and bump allocator.
fn runFileWithWizer(allocator: std.mem.Allocator, ctx: *quickjs.c.JSContext, script_path: [:0]const u8) !void {

    // Resolve symlinks
    var resolved_buf: [512]u8 = undefined;
    const resolved_path = resolvePath(script_path, &resolved_buf);

    // Try bytecode cache first
    var cache_path_buf: [512]u8 = undefined;
    const cache_path = std.fmt.bufPrint(&cache_path_buf, "{s}.cache", .{resolved_path}) catch {
        return runFileDirectWizer(allocator, ctx, resolved_path);
    };

    const polyfills_hash = computePolyfillsHash();
    if (snapshot.validateSnapshot(cache_path, polyfills_hash)) {
        const load_result = snapshot.loadSnapshot(allocator, cache_path, polyfills_hash) catch {
            return runFileDirectWizer(allocator, ctx, resolved_path);
        };

        if (load_result.bytecode) |bc| {
            // Don't free bc - process exit will reclaim all memory
            const func = qjs.JS_ReadObject(ctx, bc.ptr, bc.len, qjs.JS_READ_OBJ_BYTECODE);
            if (qjs.JS_IsException(func)) {
                printWizerException(ctx);
                std.process.exit(1);
            }
            const result = qjs.JS_EvalFunction(ctx, func);
            if (qjs.JS_IsException(result)) {
                printWizerException(ctx);
                std.process.exit(1);
            }
            qjs.JS_FreeValue(ctx, result);
            return;
        }
    }

    return runFileDirectWizer(allocator, ctx, resolved_path);
}

/// Run file directly without cache (Wizer path)
/// NOTE: This function intentionally does NOT free memory - it relies on process exit
/// to reclaim all memory. This avoids conflicts between GPA and bump allocator.
fn runFileDirectWizer(allocator: std.mem.Allocator, ctx: *quickjs.c.JSContext, script_path: []const u8) !void {

    // Read file and add null terminator (QuickJS requires null-terminated source)
    const code = readFileNullTerminated(allocator, script_path) catch |err| {
        std.debug.print("Error reading {s}: {}\n", .{ script_path, err });
        std.process.exit(1);
    };
    // Don't free code - process exit will reclaim all memory

    // Create null-terminated filename for JS_Eval
    var filename_buf: [512]u8 = undefined;
    const filename: [:0]const u8 = if (script_path.len < filename_buf.len) blk: {
        @memcpy(filename_buf[0..script_path.len], script_path);
        filename_buf[script_path.len] = 0;
        break :blk filename_buf[0..script_path.len :0];
    } else "<script>";

    // Note: code.len excludes the null terminator, which is correct for JS_Eval
    const val = qjs.JS_Eval(ctx, code.ptr, code.len, filename.ptr, qjs.JS_EVAL_TYPE_GLOBAL);
    if (qjs.JS_IsException(val)) {
        printWizerException(ctx);
        std.process.exit(1);
    }
    qjs.JS_FreeValue(ctx, val);
}

/// Print QuickJS exception details (Wizer path)
fn printWizerException(ctx: *quickjs.c.JSContext) void {
    const exc = qjs.JS_GetException(ctx);
    defer qjs.JS_FreeValue(ctx, exc);

    var len: usize = undefined;
    const cstr = qjs.JS_ToCStringLen(ctx, &len, exc);
    if (cstr != null) {
        std.debug.print("Exception: {s}\n", .{cstr[0..len]});
        qjs.JS_FreeCString(ctx, cstr);
    }
}

/// Compile a JS file to bytecode cache without executing (build-time)
fn compileOnly(allocator: std.mem.Allocator, context: *quickjs.Context, script_path: [:0]const u8) !void {
    // Resolve symlinks
    var resolved_buf: [512]u8 = undefined;
    const resolved_path = resolvePath(script_path, &resolved_buf);

    // Read source with null terminator (QuickJS requires this)
    const code = readFileNullTerminated(allocator, resolved_path) catch |err| {
        std.debug.print("Error reading {s}: {}\n", .{ resolved_path, err });
        std.process.exit(1);
    };
    defer allocator.free(code.ptr[0 .. code.len + 1]); // Free includes null terminator

    // Compile to bytecode
    const bytecode = context.compile(code, script_path, allocator) catch |err| {
        std.debug.print("Compilation error: {}\n", .{err});
        if (context.getException()) |exc| {
            defer exc.free();
            if (exc.toStringSlice()) |msg| {
                std.debug.print("Exception: {s}\n", .{msg});
            }
        }
        std.process.exit(1);
    };
    defer allocator.free(bytecode);

    // Generate cache path
    var cache_path_buf: [512]u8 = undefined;
    const cache_path = std.fmt.bufPrint(&cache_path_buf, "{s}.cache", .{resolved_path}) catch {
        std.debug.print("Path too long\n", .{});
        std.process.exit(1);
    };

    // Save to cache
    const polyfills_hash = computePolyfillsHash();
    snapshot.createSnapshotBytecodeOnly(bytecode, cache_path, polyfills_hash) catch |err| {
        std.debug.print("Failed to write cache: {}\n", .{err});
        std.process.exit(1);
    };
}

/// Resolve symlinks in path (e.g., /tmp -> /private/tmp on macOS)
/// Returns resolved path in buffer, or original if resolution fails
fn resolvePath(path: []const u8, buf: []u8) []const u8 {
    // Try to open the file/dir and get its real path
    if (std.fs.cwd().openFile(path, .{})) |file| {
        defer file.close();
        // File exists, return original path (WASI handles the mapping)
        if (path.len <= buf.len) {
            @memcpy(buf[0..path.len], path);
            return buf[0..path.len];
        }
        return path;
    } else |_| {
        // File doesn't exist - try resolving parent directory
        // For /tmp/foo.js, check if /private/tmp/foo.js exists
        const common_symlinks = [_]struct { from: []const u8, to: []const u8 }{
            .{ .from = "/tmp/", .to = "/private/tmp/" },
            .{ .from = "/var/", .to = "/private/var/" },
        };

        for (common_symlinks) |mapping| {
            if (std.mem.startsWith(u8, path, mapping.from)) {
                const suffix = path[mapping.from.len..];
                const resolved_len = mapping.to.len + suffix.len;
                if (resolved_len <= buf.len) {
                    @memcpy(buf[0..mapping.to.len], mapping.to);
                    @memcpy(buf[mapping.to.len..][0..suffix.len], suffix);
                    // Verify the resolved path exists
                    if (std.fs.cwd().openFile(buf[0..resolved_len], .{})) |file| {
                        file.close();
                        return buf[0..resolved_len];
                    } else |_| {}
                }
            }
        }
        return path;
    }
}

/// Run a JavaScript file with automatic bytecode caching
/// On first run: parse JS → compile to bytecode → save cache → execute
/// On subsequent runs: load bytecode from cache → execute (faster)
fn runFileWithCache(allocator: std.mem.Allocator, context: *quickjs.Context, script_path: [:0]const u8) !void {
    // Resolve symlinks (e.g., /tmp -> /private/tmp on macOS)
    var resolved_buf: [512]u8 = undefined;
    const resolved_path = resolvePath(script_path, &resolved_buf);

    // Generate cache path: script.js -> script.js.cache
    var cache_path_buf: [512]u8 = undefined;
    const cache_path = std.fmt.bufPrint(&cache_path_buf, "{s}.cache", .{resolved_path}) catch {
        // Path too long, just run without cache
        return runFileDirectly(allocator, context, script_path);
    };

    // Compute hash of current polyfills for cache invalidation
    const polyfills_hash = computePolyfillsHash();

    // Try to load from cache
    if (snapshot.validateSnapshot(cache_path, polyfills_hash)) {
        // Cache is valid - fast path
        // Polyfills are already bundled in the bytecode, but we still need:
        // 1. Native bindings (can't be serialized)
        // 2. QuickJS std/os module imports (must be done fresh each time)
        registerNativeBindings(context);
        importStdModules(context) catch {};

        const load_result = snapshot.loadSnapshot(allocator, cache_path, polyfills_hash) catch {
            // Cache corrupted, fall back to direct execution
            return runFileDirectly(allocator, context, script_path);
        };

        if (load_result.bytecode) |bc| {
            defer allocator.free(bc);
            // Execute cached bytecode (includes bundled polyfills + user code)
            const result = context.loadBytecode(bc) catch |err| {
                // Cache bytecode invalid - fall back to direct execution
                std.debug.print("Cache bytecode invalid ({s}), falling back to eval...\n", .{@errorName(err)});
                if (context.getException()) |exc| {
                    defer exc.free();
                    if (exc.toStringSlice()) |msg| {
                        std.debug.print("  {s}\n", .{msg});
                    }
                }
                return runFileDirectly(allocator, context, script_path);
            };
            result.free();
        }
        return;
    }

    // No valid cache - slow path: read, compile, cache, execute
    // Read with null terminator (QuickJS requires this for parsing)
    const code = readFileNullTerminated(allocator, resolved_path) catch |err| {
        std.debug.print("Error reading {s}: {}\n", .{ resolved_path, err });
        std.process.exit(1);
    };
    defer allocator.free(code.ptr[0 .. code.len + 1]); // Free includes null terminator

    // Compile to bytecode
    const bytecode = context.compile(code, script_path, allocator) catch {
        // Compilation failed, fall back to eval
        // Must register bindings and import modules first
        registerNativeBindings(context);
        importStdModules(context) catch {};
        const result = context.eval(code) catch |err| {
            std.debug.print("Error: {}\n", .{err});
            if (context.getException()) |exc| {
                defer exc.free();
                if (exc.toStringSlice()) |msg| {
                    std.debug.print("Exception: {s}\n", .{msg});
                }
            }
            std.process.exit(1);
        };
        result.free();
        return;
    };
    defer allocator.free(bytecode);

    // Save to cache (ignore errors - caching is optional)
    snapshot.createSnapshotBytecodeOnly(bytecode, cache_path, polyfills_hash) catch {};

    // Execute bytecode
    const result = context.loadBytecode(bytecode) catch |err| {
        // Bytecode loading failed (QuickJS serialization bug with large files)
        // Fall back to direct eval which bypasses bytecode serialization
        std.debug.print("Bytecode load failed ({s}), falling back to eval...\n", .{@errorName(err)});
        if (context.getException()) |exc| {
            defer exc.free();
            if (exc.toStringSlice()) |msg| {
                std.debug.print("  {s}\n", .{msg});
            }
        }
        // Must register bindings and import modules first
        registerNativeBindings(context);
        importStdModules(context) catch {};
        // Direct eval - slower but works for all valid JS
        const eval_result = context.eval(code) catch |eval_err| {
            std.debug.print("Eval error: {}\n", .{eval_err});
            if (context.getException()) |exc2| {
                defer exc2.free();
                if (exc2.toStringSlice()) |msg| {
                    std.debug.print("Exception: {s}\n", .{msg});
                }
            }
            std.process.exit(1);
        };
        eval_result.free();
        return;
    };
    result.free();
}

/// Run file without caching (fallback)
fn runFileDirectly(allocator: std.mem.Allocator, context: *quickjs.Context, script_path: [:0]const u8) !void {
    // Read with null terminator (QuickJS requires this for parsing)
    const code = readFileNullTerminated(allocator, script_path) catch |err| {
        std.debug.print("Error reading {s}: {}\n", .{ script_path, err });
        std.process.exit(1);
    };
    defer allocator.free(code.ptr[0 .. code.len + 1]); // Free includes null terminator

    const result = context.eval(code) catch |err| {
        std.debug.print("Error: {}\n", .{err});
        if (context.getException()) |exc| {
            defer exc.free();
            if (exc.toStringSlice()) |msg| {
                std.debug.print("Exception: {s}\n", .{msg});
            }
        }
        std.process.exit(1);
    };
    result.free();
}

/// Register native bindings (must run every time, even with bytecode cache)
fn registerNativeBindings(context: *quickjs.Context) void {
    // Register native fetch function
    context.registerGlobalFunction("__edgebox_fetch", nativeFetch, 4);

    // Register TTY functions
    context.registerGlobalFunction("__edgebox_isatty", nativeIsatty, 1);
    context.registerGlobalFunction("__edgebox_get_terminal_size", nativeGetTerminalSize, 0);
    context.registerGlobalFunction("__edgebox_read_stdin", nativeReadStdin, 1);
    context.registerGlobalFunction("__edgebox_stdin_ready", nativeStdinReady, 0);

    // Register child_process functions
    context.registerGlobalFunction("__edgebox_spawn", nativeSpawn, 4);

    // Register lazy polyfill loader
    context.registerGlobalFunction("__edgebox_load_polyfills", nativeLoadPolyfills, 0);

    // Register fs bindings
    context.registerGlobalFunction("__edgebox_fs_read", nativeFsRead, 1);
    context.registerGlobalFunction("__edgebox_fs_write", nativeFsWrite, 2);
    context.registerGlobalFunction("__edgebox_fs_exists", nativeFsExists, 1);
    context.registerGlobalFunction("__edgebox_fs_stat", nativeFsStat, 1);
    context.registerGlobalFunction("__edgebox_fs_readdir", nativeFsReaddir, 1);
    context.registerGlobalFunction("__edgebox_fs_mkdir", nativeFsMkdir, 2);
    context.registerGlobalFunction("__edgebox_fs_unlink", nativeFsUnlink, 1);
    context.registerGlobalFunction("__edgebox_fs_rmdir", nativeFsRmdir, 2);
    context.registerGlobalFunction("__edgebox_fs_rename", nativeFsRename, 2);
    context.registerGlobalFunction("__edgebox_fs_copy", nativeFsCopy, 2);
    context.registerGlobalFunction("__edgebox_cwd", nativeCwd, 0);
    context.registerGlobalFunction("__edgebox_homedir", nativeHomedir, 0);

    // Register zlib bindings
    context.registerGlobalFunction("__edgebox_gunzip", nativeGunzip, 1);
    context.registerGlobalFunction("__edgebox_inflate", nativeInflate, 1);
    context.registerGlobalFunction("__edgebox_inflate_zlib", nativeInflateZlib, 1);

    // Register crypto bindings
    context.registerGlobalFunction("__edgebox_hash", nativeHash, 2);
    context.registerGlobalFunction("__edgebox_hmac", nativeHmac, 3);

    // WASI-NN AI bindings
    context.registerGlobalFunction("__edgebox_ai_chat", nativeAIChat, 1);
    context.registerGlobalFunction("__edgebox_ai_available", nativeAIAvailable, 0);

    // Stdlib bindings (HostArray, HostMap) - native data structures for ~10x speedup
    context.registerGlobalFunction("__edgebox_array_new", nativeArrayNew, 0);
    context.registerGlobalFunction("__edgebox_array_push", nativeArrayPush, 2);
    context.registerGlobalFunction("__edgebox_array_pop", nativeArrayPop, 1);
    context.registerGlobalFunction("__edgebox_array_get", nativeArrayGet, 2);
    context.registerGlobalFunction("__edgebox_array_set", nativeArraySet, 3);
    context.registerGlobalFunction("__edgebox_array_len", nativeArrayLen, 1);
    context.registerGlobalFunction("__edgebox_array_sort", nativeArraySort, 1);
    context.registerGlobalFunction("__edgebox_array_sort_desc", nativeArraySortDesc, 1);
    context.registerGlobalFunction("__edgebox_array_reverse", nativeArrayReverse, 1);
    context.registerGlobalFunction("__edgebox_array_clear", nativeArrayClear, 1);
    context.registerGlobalFunction("__edgebox_array_index_of", nativeArrayIndexOf, 2);
    context.registerGlobalFunction("__edgebox_array_free", nativeArrayFree, 1);
    context.registerGlobalFunction("__edgebox_map_new", nativeMapNew, 0);
    context.registerGlobalFunction("__edgebox_map_set", nativeMapSet, 3);
    context.registerGlobalFunction("__edgebox_map_get", nativeMapGet, 2);
    context.registerGlobalFunction("__edgebox_map_has", nativeMapHas, 2);
    context.registerGlobalFunction("__edgebox_map_delete", nativeMapDelete, 2);
    context.registerGlobalFunction("__edgebox_map_len", nativeMapLen, 1);
    context.registerGlobalFunction("__edgebox_map_clear", nativeMapClear, 1);
    context.registerGlobalFunction("__edgebox_map_free", nativeMapFree, 1);

    // Socket bindings (sandboxed via Unix domain sockets)
    context.registerGlobalFunction("__edgebox_socket_create", nativeSocketCreate, 0);
    context.registerGlobalFunction("__edgebox_socket_bind", nativeSocketBind, 2);
    context.registerGlobalFunction("__edgebox_socket_listen", nativeSocketListen, 2);
    context.registerGlobalFunction("__edgebox_socket_accept", nativeSocketAccept, 1);
    context.registerGlobalFunction("__edgebox_socket_connect", nativeSocketConnect, 2);
    context.registerGlobalFunction("__edgebox_socket_write", nativeSocketWrite, 2);
    context.registerGlobalFunction("__edgebox_socket_read", nativeSocketRead, 2);
    context.registerGlobalFunction("__edgebox_socket_close", nativeSocketClose, 1);
    context.registerGlobalFunction("__edgebox_socket_state", nativeSocketState, 1);
}

/// Track if full polyfills have been loaded
var polyfills_loaded: bool = false;

/// Native function to load full polyfills on-demand
fn nativeLoadPolyfills(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (polyfills_loaded) return jsBool(true);

    // Create a wrapper context to call injectFullPolyfills
    var wrapper = quickjs.Context.fromRaw(ctx, global_allocator.?);
    injectFullPolyfills(&wrapper) catch |err| {
        std.debug.print("Failed to load polyfills: {}\n", .{err});
        return jsBool(false);
    };

    polyfills_loaded = true;
    return jsBool(true);
}

/// Import QuickJS std/os modules (must run every time)
fn importStdModules(context: *quickjs.Context) !void {
    const module_imports =
        \\import * as std from 'std';
        \\import * as os from 'os';
        \\globalThis.std = std;
        \\globalThis._os = os;
    ;

    _ = context.evalModule(module_imports, "<polyfills-module>") catch |err| {
        std.debug.print("Failed to import std/os modules: {}\n", .{err});
        if (context.getException()) |exc| {
            defer exc.free();
            if (exc.toStringSlice()) |msg| {
                std.debug.print("Module import exception: {s}\n", .{msg});
            }
        }
        return err;
    };
}

/// Minimal bootstrap for fast cold start - just console and require stub
/// Full polyfills are lazy-loaded on first use of Node.js APIs
fn injectMinimalBootstrap(context: *quickjs.Context) !void {
    registerNativeBindings(context);
    try importStdModules(context);

    // Minimal bootstrap - just what's needed for basic scripts
    const minimal_bootstrap =
        \\// Minimal bootstrap for fast cold start
        \\globalThis.global = globalThis;
        \\globalThis.self = globalThis;
        \\
        \\// Basic console (QuickJS has print)
        \\if (typeof console === 'undefined') {
        \\    globalThis.console = {
        \\        log: (...args) => print(...args),
        \\        error: (...args) => print('ERROR:', ...args),
        \\        warn: (...args) => print('WARN:', ...args),
        \\        info: (...args) => print(...args),
        \\        debug: (...args) => {},
        \\        trace: (...args) => {},
        \\    };
        \\}
        \\
        \\// Module registry for lazy loading
        \\globalThis._modules = globalThis._modules || {};
        \\globalThis._polyfillsLoaded = false;
        \\
        \\// Lazy require - loads full polyfills on first Node.js module access
        \\globalThis.require = function(name) {
        \\    // Strip node: prefix for lookup
        \\    const lookupName = name.startsWith('node:') ? name.slice(5) : name;
        \\    // Load polyfills on first require of a Node.js module
        \\    if (!globalThis._polyfillsLoaded) {
        \\        if (name.startsWith('node:') || ['fs', 'path', 'os', 'buffer', 'events', 'stream', 'util', 'http', 'https', 'http2', 'crypto', 'child_process', 'tty', 'net', 'dns', 'url', 'querystring', 'zlib', 'assert', 'timers', 'readline', 'module', 'process', 'dgram', 'worker_threads', 'cluster', 'tls'].includes(lookupName)) {
        \\            globalThis.__edgebox_load_polyfills();
        \\            globalThis._polyfillsLoaded = true;
        \\        }
        \\    }
        \\    // Try with stripped name first, then original name
        \\    const mod = globalThis._modules[lookupName] || globalThis._modules[name];
        \\    if (mod !== undefined) return mod;
        \\    throw new Error('Module not found: ' + name);
        \\};
        \\
        \\// Basic process object (minimal) - uses std.getenv for real values
        \\globalThis.process = globalThis.process || {
        \\    version: 'v20.0.0',
        \\    versions: { node: '20.0.0' },
        \\    platform: 'wasi',
        \\    arch: 'wasm32',
        \\    env: new Proxy({}, {
        \\        get: (t, n) => typeof n === 'symbol' ? undefined : std.getenv(String(n)),
        \\        has: (t, n) => typeof n !== 'symbol' && std.getenv(String(n)) !== undefined
        \\    }),
        \\    argv: typeof scriptArgs !== 'undefined' ? ['node', ...scriptArgs] : ['node'],
        \\    cwd: () => std.getenv('PWD') || '/',
        \\    exit: (code) => { if (typeof std !== 'undefined') std.exit(code || 0); },
        \\    nextTick: (fn, ...args) => queueMicrotask(() => fn(...args)),
        \\    stdout: { write: (s) => { print(String(s).replace(/\n$/, '')); return true; }, isTTY: false },
        \\    stderr: { write: (s) => { print(String(s).replace(/\n$/, '')); return true; }, isTTY: false },
        \\    stdin: { isTTY: false, on: () => {}, once: () => {}, setEncoding: () => {} },
        \\};
        \\
        \\// Timer polyfills using QuickJS native _os.setTimeout for proper event loop integration
        \\(function() {
        \\    const _intervalTimers = new Map();
        \\    // Use QuickJS native _os.setTimeout which integrates with js_std_loop
        \\    globalThis.setTimeout = function(callback, delay = 0, ...args) {
        \\        if (typeof _os !== 'undefined' && typeof _os.setTimeout === 'function') {
        \\            return _os.setTimeout(() => callback(...args), delay);
        \\        }
        \\        // Fallback for zero-delay (microtask)
        \\        const id = Math.floor(Math.random() * 1000000);
        \\        if (delay === 0) { queueMicrotask(() => callback(...args)); }
        \\        return id;
        \\    };
        \\    globalThis.clearTimeout = function(id) {
        \\        if (typeof _os !== 'undefined' && typeof _os.clearTimeout === 'function') {
        \\            _os.clearTimeout(id);
        \\        }
        \\    };
        \\    globalThis.setInterval = function(callback, delay = 0, ...args) {
        \\        if (typeof _os !== 'undefined' && typeof _os.setTimeout === 'function') {
        \\            let id;
        \\            const run = () => {
        \\                callback(...args);
        \\                if (_intervalTimers.has(id)) {
        \\                    _intervalTimers.set(id, _os.setTimeout(run, delay));
        \\                }
        \\            };
        \\            id = _os.setTimeout(run, delay);
        \\            _intervalTimers.set(id, id);
        \\            return id;
        \\        }
        \\        return Math.floor(Math.random() * 1000000);
        \\    };
        \\    globalThis.clearInterval = function(id) {
        \\        if (_intervalTimers.has(id)) {
        \\            const timerId = _intervalTimers.get(id);
        \\            _intervalTimers.delete(id);
        \\            if (typeof _os !== 'undefined' && typeof _os.clearTimeout === 'function') {
        \\                _os.clearTimeout(timerId);
        \\            }
        \\        }
        \\    };
        \\    globalThis.setImmediate = function(callback, ...args) { return setTimeout(callback, 0, ...args); };
        \\    globalThis.clearImmediate = function(id) { clearTimeout(id); };
        \\})();
        \\
        \\// TextEncoder/TextDecoder polyfills (essential web globals)
        \\if (typeof TextEncoder === 'undefined') {
        \\    globalThis.TextEncoder = class TextEncoder {
        \\        constructor() { this.encoding = 'utf-8'; }
        \\        encode(str) {
        \\            const bytes = [];
        \\            for (let i = 0; i < str.length; i++) {
        \\                let c = str.charCodeAt(i);
        \\                if (c < 0x80) bytes.push(c);
        \\                else if (c < 0x800) { bytes.push(0xC0 | (c >> 6)); bytes.push(0x80 | (c & 0x3F)); }
        \\                else { bytes.push(0xE0 | (c >> 12)); bytes.push(0x80 | ((c >> 6) & 0x3F)); bytes.push(0x80 | (c & 0x3F)); }
        \\            }
        \\            return new Uint8Array(bytes);
        \\        }
        \\    };
        \\}
        \\if (typeof TextDecoder === 'undefined') {
        \\    globalThis.TextDecoder = class TextDecoder {
        \\        constructor() { this.encoding = 'utf-8'; }
        \\        decode(bytes) {
        \\            if (!bytes) return '';
        \\            let str = '';
        \\            for (let i = 0; i < bytes.length; ) {
        \\                const b = bytes[i++];
        \\                if (b < 0x80) str += String.fromCharCode(b);
        \\                else if (b < 0xE0) str += String.fromCharCode(((b & 0x1F) << 6) | (bytes[i++] & 0x3F));
        \\                else str += String.fromCharCode(((b & 0x0F) << 12) | ((bytes[i++] & 0x3F) << 6) | (bytes[i++] & 0x3F));
        \\            }
        \\            return str;
        \\        }
        \\    };
        \\}
        \\
        \\// URL/URLSearchParams polyfills (essential web globals)
        \\if (typeof URLSearchParams === 'undefined') {
        \\    globalThis.URLSearchParams = class URLSearchParams {
        \\        constructor(init = '') {
        \\            this._params = new Map();
        \\            if (typeof init === 'string') {
        \\                init = init.startsWith('?') ? init.slice(1) : init;
        \\                for (const pair of init.split('&')) {
        \\                    const [k, v = ''] = pair.split('=').map(decodeURIComponent);
        \\                    if (k) { if (!this._params.has(k)) this._params.set(k, []); this._params.get(k).push(v); }
        \\                }
        \\            }
        \\        }
        \\        get(n) { const v = this._params.get(n); return v ? v[0] : null; }
        \\        set(n, v) { this._params.set(n, [String(v)]); }
        \\        append(n, v) { if (!this._params.has(n)) this._params.set(n, []); this._params.get(n).push(String(v)); }
        \\        has(n) { return this._params.has(n); }
        \\        delete(n) { this._params.delete(n); }
        \\        toString() { const p = []; this._params.forEach((v, k) => v.forEach(x => p.push(encodeURIComponent(k) + '=' + encodeURIComponent(x)))); return p.join('&'); }
        \\        forEach(cb) { this._params.forEach((v, k) => v.forEach(x => cb(x, k, this))); }
        \\    };
        \\}
        \\if (typeof URL === 'undefined') {
        \\    globalThis.URL = class URL {
        \\        constructor(url, base) {
        \\            if (base) { const b = typeof base === 'string' ? base : base.href; if (!url.match(/^[a-z]+:/i)) url = b.replace(/[^/]*$/, '') + url; }
        \\            const m = url.match(/^([a-z]+):\/\/([^/:]+)?(?::(\d+))?(\/[^?#]*)?(\?[^#]*)?(#.*)?$/i);
        \\            if (!m) throw new TypeError('Invalid URL: ' + url);
        \\            this.protocol = m[1] + ':'; this.hostname = m[2] || ''; this.port = m[3] || ''; this.pathname = m[4] || '/';
        \\            this.search = m[5] || ''; this.hash = m[6] || ''; this.searchParams = new URLSearchParams(this.search);
        \\        }
        \\        get host() { return this.port ? this.hostname + ':' + this.port : this.hostname; }
        \\        get origin() { return this.protocol + '//' + this.host; }
        \\        get href() { return this.origin + this.pathname + this.search + this.hash; }
        \\        toString() { return this.href; }
        \\    };
        \\}
        \\
        \\// AbortController/AbortSignal polyfills (essential web globals)
        \\if (typeof AbortController === 'undefined') {
        \\    globalThis.AbortSignal = class AbortSignal { constructor() { this.aborted = false; this.reason = undefined; this._listeners = []; }
        \\        addEventListener(t, fn) { if (t === 'abort') this._listeners.push(fn); }
        \\        removeEventListener(t, fn) { if (t === 'abort') this._listeners = this._listeners.filter(f => f !== fn); }
        \\    };
        \\    globalThis.AbortController = class AbortController { constructor() { this.signal = new AbortSignal(); }
        \\        abort(reason) { if (!this.signal.aborted) { this.signal.aborted = true; this.signal.reason = reason || new Error('Aborted');
        \\            this.signal._listeners.forEach(fn => fn({ type: 'abort' })); } }
        \\    };
        \\}
        \\
        \\// crypto polyfill (essential web global)
        \\if (typeof crypto === 'undefined') {
        \\    globalThis.crypto = {
        \\        randomUUID: () => { const h = '0123456789abcdef'; let u = ''; for (let i = 0; i < 36; i++) {
        \\            if (i === 8 || i === 13 || i === 18 || i === 23) u += '-';
        \\            else if (i === 14) u += '4'; else if (i === 19) u += h[(Math.random() * 4 | 0) + 8];
        \\            else u += h[Math.random() * 16 | 0]; } return u; },
        \\        getRandomValues: (arr) => { for (let i = 0; i < arr.length; i++) arr[i] = Math.floor(Math.random() * 256); return arr; }
        \\    };
        \\}
        \\
        \\// fetch polyfill using native binding (essential web global)
        \\if (typeof fetch === 'undefined' && typeof globalThis.__edgebox_fetch === 'function') {
        \\    globalThis.fetch = async function(url, options = {}) {
        \\        const method = options.method || 'GET';
        \\        const body = options.body || null;
        \\        const result = globalThis.__edgebox_fetch(url, method, null, body);
        \\        return {
        \\            ok: result.ok,
        \\            status: result.status,
        \\            headers: result.headers || {},
        \\            text: async () => result.body,
        \\            json: async () => JSON.parse(result.body),
        \\        };
        \\    };
        \\}
        \\
        \\// Minimal Buffer for common use cases (full Buffer in polyfills)
        \\// Uses manual UTF-8 encoding since TextEncoder may not be available yet
        \\if (typeof Buffer === 'undefined') {
        \\    const _encodeUTF8 = (str) => {
        \\        const bytes = [];
        \\        for (let i = 0; i < str.length; i++) {
        \\            let c = str.charCodeAt(i);
        \\            if (c < 0x80) bytes.push(c);
        \\            else if (c < 0x800) { bytes.push(0xC0 | (c >> 6)); bytes.push(0x80 | (c & 0x3F)); }
        \\            else if (c >= 0xD800 && c <= 0xDBFF && i + 1 < str.length) {
        \\                const next = str.charCodeAt(++i);
        \\                if (next >= 0xDC00 && next <= 0xDFFF) {
        \\                    c = ((c - 0xD800) << 10) + (next - 0xDC00) + 0x10000;
        \\                    bytes.push(0xF0 | (c >> 18)); bytes.push(0x80 | ((c >> 12) & 0x3F));
        \\                    bytes.push(0x80 | ((c >> 6) & 0x3F)); bytes.push(0x80 | (c & 0x3F));
        \\                }
        \\            } else { bytes.push(0xE0 | (c >> 12)); bytes.push(0x80 | ((c >> 6) & 0x3F)); bytes.push(0x80 | (c & 0x3F)); }
        \\        }
        \\        return new Uint8Array(bytes);
        \\    };
        \\    const _decodeUTF8 = (bytes) => {
        \\        let str = '';
        \\        for (let i = 0; i < bytes.length; ) {
        \\            const b = bytes[i++];
        \\            if (b < 0x80) str += String.fromCharCode(b);
        \\            else if (b < 0xE0) str += String.fromCharCode(((b & 0x1F) << 6) | (bytes[i++] & 0x3F));
        \\            else if (b < 0xF0) str += String.fromCharCode(((b & 0x0F) << 12) | ((bytes[i++] & 0x3F) << 6) | (bytes[i++] & 0x3F));
        \\            else {
        \\                const c = ((b & 0x07) << 18) | ((bytes[i++] & 0x3F) << 12) | ((bytes[i++] & 0x3F) << 6) | (bytes[i++] & 0x3F);
        \\                if (c > 0xFFFF) { const o = c - 0x10000; str += String.fromCharCode(0xD800 + (o >> 10), 0xDC00 + (o & 0x3FF)); }
        \\                else str += String.fromCharCode(c);
        \\            }
        \\        }
        \\        return str;
        \\    };
        \\    globalThis.Buffer = {
        \\        from: (data, encoding) => {
        \\            if (typeof data === 'string') {
        \\                const bytes = _encodeUTF8(data);
        \\                bytes.toString = function(enc) { return _decodeUTF8(this); };
        \\                return bytes;
        \\            }
        \\            const arr = new Uint8Array(data);
        \\            arr.toString = function(enc) { return _decodeUTF8(this); };
        \\            return arr;
        \\        },
        \\        alloc: (size) => { const arr = new Uint8Array(size); arr.toString = function(enc) { return _decodeUTF8(this); }; return arr; },
        \\        allocUnsafe: (size) => { const arr = new Uint8Array(size); arr.toString = function(enc) { return _decodeUTF8(this); }; return arr; },
        \\        isBuffer: (obj) => obj instanceof Uint8Array,
        \\        concat: (list) => {
        \\            const len = list.reduce((a, b) => a + b.length, 0);
        \\            const result = new Uint8Array(len);
        \\            let offset = 0;
        \\            for (const buf of list) { result.set(buf, offset); offset += buf.length; }
        \\            result.toString = function(enc) { return _decodeUTF8(this); };
        \\            return result;
        \\        },
        \\    };
        \\}
    ;

    _ = context.eval(minimal_bootstrap) catch |err| {
        std.debug.print("Failed to inject minimal bootstrap: {}\n", .{err});
        return err;
    };
}

/// Full polyfills - loaded lazily on first Node.js module require
fn injectFullPolyfills(context: *quickjs.Context) !void {
    // JS polyfills - these are now bundled into bundle.js at build time
    // But we still need them for -e eval mode and scripts without .cache
    const base_polyfills =
        \\// Node.js global alias
        \\globalThis.global = globalThis;
        \\globalThis.self = globalThis;
        \\
        \\// Basic console polyfill
        \\if (typeof console === 'undefined') {
        \\    globalThis.console = {
        \\        log: (...args) => print(...args),
        \\        error: (...args) => print('ERROR:', ...args),
        \\        warn: (...args) => print('WARN:', ...args),
        \\        info: (...args) => print('INFO:', ...args),
        \\        debug: (...args) => print('DEBUG:', ...args),
        \\        trace: (...args) => print('TRACE:', ...args),
        \\    };
        \\}
        \\
        \\// Timer polyfills
        \\(function() {
        \\    let _timerId = 1;
        \\    const _timers = new Map();
        \\
        \\    globalThis.setTimeout = function(callback, delay = 0, ...args) {
        \\        const id = _timerId++;
        \\        _timers.set(id, { callback, args, delay });
        \\        if (delay === 0) {
        \\            queueMicrotask(() => {
        \\                if (_timers.has(id)) {
        \\                    _timers.delete(id);
        \\                    callback(...args);
        \\                }
        \\            });
        \\        }
        \\        return id;
        \\    };
        \\
        \\    globalThis.clearTimeout = function(id) { _timers.delete(id); };
        \\    globalThis.setInterval = function(callback, delay = 0, ...args) { return _timerId++; };
        \\    globalThis.clearInterval = function(id) {};
        \\    globalThis.setImmediate = function(callback, ...args) { return setTimeout(callback, 0, ...args); };
        \\    globalThis.clearImmediate = function(id) { clearTimeout(id); };
        \\})();
        \\
        \\// TextEncoder/TextDecoder polyfills
        \\if (typeof TextEncoder === 'undefined') {
        \\    globalThis.TextEncoder = class TextEncoder {
        \\        constructor(encoding = 'utf-8') { this.encoding = encoding; }
        \\        encode(str) {
        \\            const bytes = [];
        \\            for (let i = 0; i < str.length; i++) {
        \\                let code = str.charCodeAt(i);
        \\                if (code < 0x80) bytes.push(code);
        \\                else if (code < 0x800) { bytes.push(0xC0 | (code >> 6)); bytes.push(0x80 | (code & 0x3F)); }
        \\                else if (code >= 0xD800 && code <= 0xDBFF && i + 1 < str.length) {
        \\                    const next = str.charCodeAt(++i);
        \\                    if (next >= 0xDC00 && next <= 0xDFFF) {
        \\                        code = ((code - 0xD800) << 10) + (next - 0xDC00) + 0x10000;
        \\                        bytes.push(0xF0 | (code >> 18)); bytes.push(0x80 | ((code >> 12) & 0x3F));
        \\                        bytes.push(0x80 | ((code >> 6) & 0x3F)); bytes.push(0x80 | (code & 0x3F));
        \\                    }
        \\                } else { bytes.push(0xE0 | (code >> 12)); bytes.push(0x80 | ((code >> 6) & 0x3F)); bytes.push(0x80 | (code & 0x3F)); }
        \\            }
        \\            return new Uint8Array(bytes);
        \\        }
        \\    };
        \\}
        \\
        \\if (typeof TextDecoder === 'undefined') {
        \\    globalThis.TextDecoder = class TextDecoder {
        \\        constructor(encoding = 'utf-8') { this.encoding = encoding; }
        \\        decode(input) {
        \\            const bytes = input instanceof Uint8Array ? input : new Uint8Array(input);
        \\            let str = '';
        \\            for (let i = 0; i < bytes.length; ) {
        \\                const b = bytes[i++];
        \\                if (b < 0x80) str += String.fromCharCode(b);
        \\                else if (b < 0xE0) str += String.fromCharCode(((b & 0x1F) << 6) | (bytes[i++] & 0x3F));
        \\                else if (b < 0xF0) str += String.fromCharCode(((b & 0x0F) << 12) | ((bytes[i++] & 0x3F) << 6) | (bytes[i++] & 0x3F));
        \\                else {
        \\                    const code = ((b & 0x07) << 18) | ((bytes[i++] & 0x3F) << 12) | ((bytes[i++] & 0x3F) << 6) | (bytes[i++] & 0x3F);
        \\                    if (code > 0xFFFF) { const offset = code - 0x10000; str += String.fromCharCode(0xD800 + (offset >> 10), 0xDC00 + (offset & 0x3FF)); }
        \\                    else str += String.fromCharCode(code);
        \\                }
        \\            }
        \\            return str;
        \\        }
        \\    };
        \\}
        \\
        \\// DOMException polyfill
        \\if (typeof DOMException === 'undefined') {
        \\    globalThis.DOMException = class DOMException extends Error {
        \\        constructor(message, name) { super(message); this.name = name || 'Error'; }
        \\    };
        \\}
        \\
        \\// Event polyfill
        \\if (typeof Event === 'undefined') {
        \\    globalThis.Event = class Event {
        \\        constructor(type, options = {}) {
        \\            this.type = type;
        \\            this.bubbles = options.bubbles || false;
        \\            this.cancelable = options.cancelable || false;
        \\            this.composed = options.composed || false;
        \\            this.defaultPrevented = false;
        \\            this.target = null;
        \\            this.currentTarget = null;
        \\            this.timeStamp = Date.now();
        \\        }
        \\        preventDefault() { this.defaultPrevented = true; }
        \\        stopPropagation() {}
        \\        stopImmediatePropagation() {}
        \\    };
        \\}
        \\if (typeof CustomEvent === 'undefined') {
        \\    globalThis.CustomEvent = class CustomEvent extends Event {
        \\        constructor(type, options = {}) { super(type, options); this.detail = options.detail || null; }
        \\    };
        \\}
        \\if (typeof EventTarget === 'undefined') {
        \\    globalThis.EventTarget = class EventTarget {
        \\        constructor() { this._listeners = {}; }
        \\        addEventListener(type, listener) { (this._listeners[type] = this._listeners[type] || []).push(listener); }
        \\        removeEventListener(type, listener) { if (this._listeners[type]) this._listeners[type] = this._listeners[type].filter(l => l !== listener); }
        \\        dispatchEvent(event) { event.target = this; (this._listeners[event.type] || []).forEach(l => l(event)); return !event.defaultPrevented; }
        \\    };
        \\}
        \\
        \\// Crypto polyfill
        \\globalThis.crypto = globalThis.crypto || {
        \\    getRandomValues(array) {
        \\        for (let i = 0; i < array.length; i++) {
        \\            if (array instanceof Uint8Array) array[i] = Math.floor(Math.random() * 256);
        \\            else if (array instanceof Uint16Array) array[i] = Math.floor(Math.random() * 65536);
        \\            else if (array instanceof Uint32Array) array[i] = Math.floor(Math.random() * 4294967296);
        \\            else array[i] = Math.floor(Math.random() * 256);
        \\        }
        \\        return array;
        \\    },
        \\    randomUUID() {
        \\        const bytes = new Uint8Array(16);
        \\        crypto.getRandomValues(bytes);
        \\        bytes[6] = (bytes[6] & 0x0f) | 0x40;
        \\        bytes[8] = (bytes[8] & 0x3f) | 0x80;
        \\        const hex = Array.from(bytes, b => b.toString(16).padStart(2, '0')).join('');
        \\        return hex.slice(0,8) + '-' + hex.slice(8,12) + '-' + hex.slice(12,16) + '-' + hex.slice(16,20) + '-' + hex.slice(20);
        \\    },
        \\};
        \\
        \\// Performance polyfill
        \\globalThis.performance = globalThis.performance || { now: () => Date.now(), mark: () => {}, measure: () => {}, getEntriesByType: () => [] };
        \\
        \\// Intl polyfill (minimal)
        \\if (typeof Intl === 'undefined') {
        \\    globalThis.Intl = {
        \\        DateTimeFormat: class DateTimeFormat { constructor(locale, opts) { this.locale = locale; this.opts = opts; } format(date) { return date.toISOString(); } formatToParts(date) { return [{ type: 'literal', value: date.toISOString() }]; } resolvedOptions() { return this.opts || {}; } },
        \\        NumberFormat: class NumberFormat { constructor(locale, opts) { this.locale = locale; this.opts = opts; } format(num) { return String(num); } formatToParts(num) { return [{ type: 'integer', value: String(num) }]; } resolvedOptions() { return this.opts || {}; } },
        \\        Collator: class Collator { constructor(locale, opts) { this.locale = locale; } compare(a, b) { return a < b ? -1 : a > b ? 1 : 0; } resolvedOptions() { return {}; } },
        \\        PluralRules: class PluralRules { constructor(locale, opts) { this.locale = locale; } select(n) { return n === 1 ? 'one' : 'other'; } resolvedOptions() { return {}; } },
        \\        RelativeTimeFormat: class RelativeTimeFormat { constructor(locale, opts) { this.locale = locale; } format(value, unit) { return value + ' ' + unit + (Math.abs(value) !== 1 ? 's' : '') + (value < 0 ? ' ago' : ''); } formatToParts(value, unit) { return [{ type: 'literal', value: this.format(value, unit) }]; } resolvedOptions() { return {}; } },
        \\        ListFormat: class ListFormat { constructor(locale, opts) { this.locale = locale; } format(list) { return list.join(', '); } formatToParts(list) { return list.map(v => ({ type: 'element', value: v })); } resolvedOptions() { return {}; } },
        \\        Segmenter: class Segmenter { constructor(locale, opts) { this.locale = locale; this.granularity = opts?.granularity || 'grapheme'; } segment(str) { return { [Symbol.iterator]: function*() { for (let i = 0; i < str.length; i++) yield { segment: str[i], index: i }; } }; } resolvedOptions() { return { granularity: this.granularity }; } },
        \\        getCanonicalLocales: (locales) => Array.isArray(locales) ? locales : [locales],
        \\        supportedValuesOf: (key) => []
        \\    };
        \\}
        \\
        \\// URLSearchParams polyfill
        \\if (typeof URLSearchParams === 'undefined') {
        \\    globalThis.URLSearchParams = class URLSearchParams {
        \\        constructor(init) {
        \\            this._params = [];
        \\            if (typeof init === 'string') {
        \\                const query = init.startsWith('?') ? init.slice(1) : init;
        \\                query.split('&').forEach(pair => {
        \\                    const [k, v] = pair.split('=').map(decodeURIComponent);
        \\                    if (k) this._params.push([k, v || '']);
        \\                });
        \\            } else if (init && typeof init === 'object') {
        \\                Object.entries(init).forEach(([k, v]) => this._params.push([k, String(v)]));
        \\            }
        \\        }
        \\        get(name) { const p = this._params.find(([k]) => k === name); return p ? p[1] : null; }
        \\        getAll(name) { return this._params.filter(([k]) => k === name).map(([, v]) => v); }
        \\        has(name) { return this._params.some(([k]) => k === name); }
        \\        set(name, value) { this.delete(name); this._params.push([name, String(value)]); }
        \\        append(name, value) { this._params.push([name, String(value)]); }
        \\        delete(name) { this._params = this._params.filter(([k]) => k !== name); }
        \\        entries() { return this._params[Symbol.iterator](); }
        \\        keys() { return this._params.map(([k]) => k)[Symbol.iterator](); }
        \\        values() { return this._params.map(([, v]) => v)[Symbol.iterator](); }
        \\        forEach(cb, thisArg) { this._params.forEach(([k, v]) => cb.call(thisArg, v, k, this)); }
        \\        toString() { return this._params.map(([k, v]) => encodeURIComponent(k) + '=' + encodeURIComponent(v)).join('&'); }
        \\        [Symbol.iterator]() { return this.entries(); }
        \\    };
        \\}
        \\
        \\// URL polyfill
        \\if (typeof URL === 'undefined') {
        \\    globalThis.URL = class URL {
        \\        constructor(url, base) {
        \\            let full = url;
        \\            if (base) {
        \\                if (typeof base === 'string') base = new URL(base);
        \\                if (url.startsWith('/')) full = base.origin + url;
        \\                else if (!url.includes('://')) full = base.href.replace(/[^/]*$/, '') + url;
        \\            }
        \\            const match = full.match(/^(([^:/?#]+):)?(\/\/([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?/);
        \\            this.protocol = (match[2] || '') + ':';
        \\            const hostPart = match[4] || '';
        \\            const userMatch = hostPart.match(/^([^@]+)@(.*)$/);
        \\            if (userMatch) { const [, userInfo, host] = userMatch; this.username = userInfo.split(':')[0] || ''; this.password = userInfo.split(':')[1] || ''; this.host = host; }
        \\            else { this.username = ''; this.password = ''; this.host = hostPart; }
        \\            const portMatch = this.host.match(/^(.+):(\d+)$/);
        \\            if (portMatch) { this.hostname = portMatch[1]; this.port = portMatch[2]; }
        \\            else { this.hostname = this.host; this.port = ''; }
        \\            this.pathname = match[5] || '/';
        \\            this.search = match[6] || '';
        \\            this.hash = match[8] || '';
        \\            this.searchParams = new URLSearchParams(this.search);
        \\        }
        \\        get origin() { return this.protocol + '//' + this.host; }
        \\        get href() { return this.protocol + '//' + (this.username ? this.username + (this.password ? ':' + this.password : '') + '@' : '') + this.host + this.pathname + this.search + this.hash; }
        \\        set href(v) { const u = new URL(v); Object.assign(this, u); }
        \\        toString() { return this.href; }
        \\        toJSON() { return this.href; }
        \\    };
        \\}
    ;

    _ = context.eval(base_polyfills) catch |err| {
        std.debug.print("Failed to inject base polyfills: {}\n", .{err});
        return err;
    };

    // Step 3a: Load EdgeBox runtime polyfills (HostArray, HostMap, etc.)
    const polyfills = @import("polyfills/polyfills.zig");
    _ = context.eval(polyfills.runtime_js) catch |err| {
        std.debug.print("Failed to load runtime polyfills: {}\n", .{err});
        return err;
    };

    // Step 3b: Load Node.js polyfills (from single source of truth)
    _ = context.eval(polyfills.node_polyfill_js) catch |err| {
        std.debug.print("Failed to load Node.js polyfills: {}\n", .{err});
        return err;
    };

    // Step 4: Platform-specific bindings (TTY, child_process, fs using native QuickJS)
    const platform_polyfills =
        \\// TTY module using native bindings
        \\globalThis._modules['tty'] = {
        \\    isatty: (fd) => globalThis.__edgebox_isatty(fd),
        \\    ReadStream: class ReadStream {
        \\        constructor(fd) { this.fd = fd; this.isTTY = globalThis.__edgebox_isatty(fd); }
        \\    },
        \\    WriteStream: class WriteStream {
        \\        constructor(fd) {
        \\            this.fd = fd;
        \\            this.isTTY = globalThis.__edgebox_isatty(fd);
        \\            const size = globalThis.__edgebox_get_terminal_size();
        \\            this.rows = size.rows;
        \\            this.columns = size.cols;
        \\        }
        \\        getWindowSize() { return [this.columns, this.rows]; }
        \\    },
        \\};
        \\globalThis._modules['node:tty'] = globalThis._modules['tty'];
        \\
        \\// child_process module using native bindings
        \\(function() {
        \\    const _spawnSync = (command, args = [], options = {}) => {
        \\        const argsArray = Array.isArray(args) ? args : [];
        \\        const stdinData = options.input || null;
        \\        const timeout = options.timeout || 30000;
        \\        try {
        \\            const result = globalThis.__edgebox_spawn(command, argsArray, stdinData, timeout);
        \\            return {
        \\                status: result.exitCode,
        \\                stdout: result.stdout || '',
        \\                stderr: result.stderr || '',
        \\                error: result.exitCode !== 0 ? new Error('Process exited with code ' + result.exitCode) : null,
        \\                signal: null,
        \\            };
        \\        } catch (e) {
        \\            return { status: null, stdout: '', stderr: '', error: e, signal: null };
        \\        }
        \\    };
        \\    globalThis._modules['child_process'] = {
        \\        spawnSync: _spawnSync,
        \\        execSync: (command, options = {}) => {
        \\            const parts = command.trim().split(/\s+/);
        \\            const result = _spawnSync(parts[0], parts.slice(1), options);
        \\            if (result.error && result.status !== 0) {
        \\                const err = new Error('Command failed: ' + command);
        \\                err.status = result.status;
        \\                err.stderr = result.stderr;
        \\                throw err;
        \\            }
        \\            return result.stdout;
        \\        },
        \\        execFileSync: (file, args = [], options = {}) => {
        \\            const result = _spawnSync(file, args, options);
        \\            if (result.error && result.status !== 0) {
        \\                const err = new Error('Command failed: ' + file);
        \\                err.status = result.status;
        \\                err.stderr = result.stderr;
        \\                throw err;
        \\            }
        \\            return result.stdout;
        \\        },
        \\        spawn: (command, args = [], options = {}) => {
        \\            return {
        \\                stdout: { on: () => {} },
        \\                stderr: { on: () => {} },
        \\                on: (event, callback) => {
        \\                    if (event === 'close') {
        \\                        const result = _spawnSync(command, args, options);
        \\                        setTimeout(() => callback(result.status), 0);
        \\                    }
        \\                },
        \\            };
        \\        },
        \\    };
        \\})();
        \\globalThis._modules['node:child_process'] = globalThis._modules['child_process'];
        \\
        \\// fs module using QuickJS std/os
        \\globalThis._modules['fs'] = {
        \\    existsSync(path) {
        \\        try { const [stat, err] = _os.stat(path); return err === 0; } catch { return false; }
        \\    },
        \\    readFileSync(path, options) {
        \\        const encoding = typeof options === 'string' ? options : options?.encoding;
        \\        const content = std.loadFile(path);
        \\        if (content === null) { const err = new Error('ENOENT: ' + path); err.code = 'ENOENT'; throw err; }
        \\        if (encoding === 'utf8' || encoding === 'utf-8') return content;
        \\        return Buffer.from(content);
        \\    },
        \\    writeFileSync(path, data, options) {
        \\        const content = typeof data === 'string' ? data : data.toString();
        \\        const file = std.open(path, 'w');
        \\        if (!file) throw new Error('ENOENT: cannot write: ' + path);
        \\        file.puts(content);
        \\        file.close();
        \\    },
        \\    appendFileSync(path, data) {
        \\        const file = std.open(path, 'a');
        \\        if (!file) throw new Error('ENOENT: cannot append: ' + path);
        \\        file.puts(typeof data === 'string' ? data : data.toString());
        \\        file.close();
        \\    },
        \\    unlinkSync(path) { try { _os.remove(path); } catch (e) { const err = new Error('ENOENT: ' + path); err.code = 'ENOENT'; throw err; } },
        \\    mkdirSync(path, options) { try { _os.mkdir(path); } catch (e) { if (!options?.recursive) throw e; } },
        \\    rmdirSync(path) { try { _os.remove(path); } catch (e) { throw new Error('ENOENT: ' + path); } },
        \\    readdirSync(path) {
        \\        try {
        \\            const [entries, err] = _os.readdir(path);
        \\            if (err) throw new Error('ENOENT: ' + path);
        \\            return entries.filter(e => e !== '.' && e !== '..');
        \\        } catch (e) { const err = new Error('ENOENT: ' + path); err.code = 'ENOENT'; throw err; }
        \\    },
        \\    statSync(path) {
        \\        try {
        \\            const [stat, err] = _os.stat(path);
        \\            if (err) throw new Error('ENOENT: ' + path);
        \\            return {
        \\                isFile: () => (stat.mode & _os.S_IFMT) === _os.S_IFREG,
        \\                isDirectory: () => (stat.mode & _os.S_IFMT) === _os.S_IFDIR,
        \\                isSymbolicLink: () => (stat.mode & _os.S_IFMT) === _os.S_IFLNK,
        \\                size: stat.size, mtime: new Date(stat.mtime * 1000),
        \\                atime: new Date(stat.atime * 1000), ctime: new Date(stat.ctime * 1000), mode: stat.mode,
        \\            };
        \\        } catch (e) { const err = new Error('ENOENT: ' + path); err.code = 'ENOENT'; throw err; }
        \\    },
        \\    lstatSync(path) { return this.statSync(path); },
        \\    realpathSync: Object.assign(function(path) { return path; }, { native: function(path) { return path; } }),
        \\    realpath: Object.assign(function(path, opts, cb) { if (typeof opts === 'function') { cb = opts; opts = {}; } cb(null, path); }, { native: function(path, opts, cb) { if (typeof opts === 'function') { cb = opts; opts = {}; } cb(null, path); } }),
        \\    copyFileSync(src, dest) { this.writeFileSync(dest, this.readFileSync(src)); },
        \\    renameSync(oldPath, newPath) { try { _os.rename(oldPath, newPath); } catch (e) { throw new Error('ENOENT: ' + oldPath); } },
        \\    chmodSync(path, mode) {},
        \\    promises: {
        \\        async readFile(path, options) { return globalThis._modules.fs.readFileSync(path, options); },
        \\        async writeFile(path, data, options) { return globalThis._modules.fs.writeFileSync(path, data, options); },
        \\        async unlink(path) { return globalThis._modules.fs.unlinkSync(path); },
        \\        async mkdir(path, options) { return globalThis._modules.fs.mkdirSync(path, options); },
        \\        async rmdir(path) { return globalThis._modules.fs.rmdirSync(path); },
        \\        async readdir(path) { return globalThis._modules.fs.readdirSync(path); },
        \\        async stat(path) { return globalThis._modules.fs.statSync(path); },
        \\        async lstat(path) { return globalThis._modules.fs.lstatSync(path); },
        \\        async realpath(path) { return globalThis._modules.fs.realpathSync(path); },
        \\        async copyFile(src, dest) { return globalThis._modules.fs.copyFileSync(src, dest); },
        \\        async rename(oldPath, newPath) { return globalThis._modules.fs.renameSync(oldPath, newPath); },
        \\    },
        \\};
        \\globalThis._modules['node:fs'] = globalThis._modules['fs'];
        \\globalThis._modules['fs/promises'] = globalThis._modules['fs'].promises;
        \\globalThis._modules['node:fs/promises'] = globalThis._modules['fs'].promises;
        \\
        \\// fetch polyfill using native binding
        \\globalThis.fetch = async function(url, options = {}) {
        \\    const method = options.method || 'GET';
        \\    const body = options.body || null;
        \\    const result = globalThis.__edgebox_fetch(url, method, null, body);
        \\    return {
        \\        ok: result.ok,
        \\        status: result.status,
        \\        headers: result.headers,
        \\        text: async () => result.body,
        \\        json: async () => JSON.parse(result.body),
        \\    };
        \\};
        \\
        \\// Wire SIMD JSON parser to JSON.parse/stringify
        \\if (typeof globalThis.__edgebox_json_parse === 'function') {
        \\    const _originalParse = JSON.parse;
        \\    const _originalStringify = JSON.stringify;
        \\    JSON.parse = function(text, reviver) {
        \\        if (reviver) return _originalParse(text, reviver);
        \\        return globalThis.__edgebox_json_parse(text);
        \\    };
        \\    JSON.stringify = function(value, replacer, space) {
        \\        if (replacer || space) return _originalStringify(value, replacer, space);
        \\        return globalThis.__edgebox_json_stringify(value);
        \\    };
        \\}
        \\
        \\// WebSocket polyfill using native bindings
        \\if (typeof WebSocket === 'undefined' && typeof __edgebox_ws_connect === 'function') {
        \\    globalThis.WebSocket = class WebSocket extends EventTarget {
        \\        constructor(url) {
        \\            super();
        \\            this.url = url;
        \\            this.readyState = 0; // CONNECTING
        \\            this._id = __edgebox_ws_connect(url);
        \\            if (this._id < 0) {
        \\                setTimeout(() => {
        \\                    this.readyState = 3; // CLOSED
        \\                    this.dispatchEvent(new Event('error'));
        \\                }, 0);
        \\                return;
        \\            }
        \\            // Polling for state/messages (simple implementation)
        \\            this._pollInterval = setInterval(() => {
        \\                const state = __edgebox_ws_state(this._id);
        \\                if (this.readyState === 0 && state === 1) {
        \\                    this.readyState = 1;
        \\                    this.dispatchEvent(new Event('open'));
        \\                }
        \\                if (state === 3 && this.readyState !== 3) {
        \\                    this.readyState = 3;
        \\                    clearInterval(this._pollInterval);
        \\                    this.dispatchEvent(new Event('close'));
        \\                }
        \\                if (this.readyState === 1) {
        \\                    const msg = __edgebox_ws_recv(this._id);
        \\                    if (msg) {
        \\                        this.dispatchEvent(new CustomEvent('message', { detail: msg.data }));
        \\                        if (this.onmessage) this.onmessage({ data: msg.data });
        \\                    }
        \\                }
        \\            }, 50);
        \\        }
        \\        send(data) {
        \\            if (this.readyState !== 1) throw new Error('WebSocket is not open');
        \\            if (typeof data === 'string') __edgebox_ws_send(this._id, data);
        \\            else __edgebox_ws_send_binary(this._id, data);
        \\        }
        \\        close() {
        \\            if (this.readyState === 3) return;
        \\            this.readyState = 2; // CLOSING
        \\            __edgebox_ws_close(this._id);
        \\        }
        \\    };
        \\}
        \\
        \\// http, https, http2, net, tls, dns, module are already defined in polyfill modules
        \\globalThis._modules['assert'] = function(condition, message) { if (!condition) throw new Error(message || 'Assertion failed'); };
        \\globalThis._modules['node:assert'] = globalThis._modules['assert'];
        \\globalThis._modules['async_hooks'] = {
        \\    AsyncLocalStorage: class { run(store, fn) { return fn(); } getStore() { return undefined; } enter() {} exit() {} disable() {} },
        \\    AsyncResource: class AsyncResource { constructor(type, opts) { this.type = type; } runInAsyncScope(fn, thisArg, ...args) { return fn.apply(thisArg, args); } emitDestroy() { return this; } asyncId() { return 1; } triggerAsyncId() { return 0; } bind(fn) { return fn; } static bind(fn) { return fn; } },
        \\    executionAsyncId: () => 1,
        \\    triggerAsyncId: () => 0,
        \\    createHook: () => ({ enable: () => {}, disable: () => {} }),
        \\    executionAsyncResource: () => ({})
        \\};
        \\globalThis._modules['node:async_hooks'] = globalThis._modules['async_hooks'];
        \\globalThis._modules['zlib'] = {
        \\    constants: { Z_NO_FLUSH: 0, Z_PARTIAL_FLUSH: 1, Z_SYNC_FLUSH: 2, Z_FULL_FLUSH: 3, Z_FINISH: 4, Z_BLOCK: 5, Z_OK: 0, Z_STREAM_END: 1, Z_NEED_DICT: 2, Z_ERRNO: -1, Z_STREAM_ERROR: -2, Z_DATA_ERROR: -3, Z_MEM_ERROR: -4, Z_BUF_ERROR: -5, Z_VERSION_ERROR: -6, Z_DEFAULT_COMPRESSION: -1, Z_BEST_SPEED: 1, Z_BEST_COMPRESSION: 9, Z_NO_COMPRESSION: 0, Z_FILTERED: 1, Z_HUFFMAN_ONLY: 2, Z_RLE: 3, Z_FIXED: 4, Z_DEFAULT_STRATEGY: 0, BROTLI_DECODE: 0, BROTLI_ENCODE: 1 },
        \\    Z_NO_FLUSH: 0, Z_PARTIAL_FLUSH: 1, Z_SYNC_FLUSH: 2, Z_FULL_FLUSH: 3, Z_FINISH: 4, Z_BLOCK: 5,
        \\    Z_OK: 0, Z_STREAM_END: 1, Z_NEED_DICT: 2, Z_ERRNO: -1, Z_STREAM_ERROR: -2, Z_DATA_ERROR: -3, Z_MEM_ERROR: -4, Z_BUF_ERROR: -5, Z_VERSION_ERROR: -6,
        \\    Z_DEFAULT_COMPRESSION: -1, Z_BEST_SPEED: 1, Z_BEST_COMPRESSION: 9, Z_NO_COMPRESSION: 0,
        \\    createGzip: () => { throw new Error('zlib compression not implemented in WASM'); },
        \\    createGunzip: () => { throw new Error('zlib decompression not implemented in WASM'); },
        \\    createDeflate: () => { throw new Error('zlib deflate not implemented in WASM'); },
        \\    createInflate: () => { throw new Error('zlib inflate not implemented in WASM'); },
        \\    gzip: (buf, cb) => { cb(new Error('zlib not implemented')); },
        \\    gunzip: (buf, cb) => { cb(new Error('zlib not implemented')); },
        \\    deflate: (buf, cb) => { cb(new Error('zlib not implemented')); },
        \\    inflate: (buf, cb) => { cb(new Error('zlib not implemented')); },
        \\    gzipSync: () => { throw new Error('zlib not implemented'); },
        \\    gunzipSync: () => { throw new Error('zlib not implemented'); },
        \\    deflateSync: () => { throw new Error('zlib not implemented'); },
        \\    inflateSync: () => { throw new Error('zlib not implemented'); }
        \\};
        \\globalThis._modules['node:zlib'] = globalThis._modules['zlib'];
        \\globalThis._modules['timers'] = { setTimeout: globalThis.setTimeout, setInterval: globalThis.setInterval, clearTimeout: globalThis.clearTimeout, clearInterval: globalThis.clearInterval };
        \\globalThis._modules['node:timers'] = globalThis._modules['timers'];
        \\globalThis._modules['readline'] = { createInterface: () => ({ question: () => {}, close: () => {}, on: () => {} }) };
        \\globalThis._modules['node:readline'] = globalThis._modules['readline'];
        \\globalThis._modules['constants'] = { fs: {}, os: {} };
        \\globalThis._modules['node:constants'] = globalThis._modules['constants'];
        \\globalThis._modules['perf_hooks'] = { performance: globalThis.performance };
        \\globalThis._modules['node:perf_hooks'] = globalThis._modules['perf_hooks'];
        \\globalThis._modules['v8'] = {};
        \\globalThis._modules['node:v8'] = globalThis._modules['v8'];
        \\globalThis._modules['vm'] = { runInNewContext: () => { throw new Error('vm not implemented'); } };
        \\globalThis._modules['node:vm'] = globalThis._modules['vm'];
        \\globalThis._modules['punycode'] = {};
        \\globalThis._modules['node:punycode'] = globalThis._modules['punycode'];
        \\globalThis._modules['querystring'] = { parse: (s) => Object.fromEntries(new URLSearchParams(s)), stringify: (o) => new URLSearchParams(o).toString() };
        \\globalThis._modules['node:querystring'] = globalThis._modules['querystring'];
        \\// worker_threads and cluster are defined in polyfill modules
        \\globalThis._modules['crypto'] = {
        \\    randomBytes: function(size) {
        \\        var buf = new Uint8Array(size);
        \\        for (var i = 0; i < size; i++) buf[i] = Math.floor(Math.random() * 256);
        \\        return globalThis.Buffer ? globalThis.Buffer.from(buf) : buf;
        \\    },
        \\    randomUUID: function() {
        \\        return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
        \\            var r = Math.random() * 16 | 0;
        \\            return (c === 'x' ? r : (r & 0x3 | 0x8)).toString(16);
        \\        });
        \\    },
        \\    createHash: function(algorithm) {
        \\        var algo = algorithm.toLowerCase();
        \\        return {
        \\            _algorithm: algo,
        \\            _data: '',
        \\            update: function(data) {
        \\                this._data += (typeof data === 'string' ? data : String(data));
        \\                return this;
        \\            },
        \\            digest: function(encoding) {
        \\                var hex = __edgebox_hash(this._algorithm, this._data);
        \\                if (encoding === 'hex') return hex;
        \\                if (encoding === 'base64') {
        \\                    var bytes = [];
        \\                    for (var i = 0; i < hex.length; i += 2) {
        \\                        bytes.push(parseInt(hex.substr(i, 2), 16));
        \\                    }
        \\                    return btoa(String.fromCharCode.apply(null, bytes));
        \\                }
        \\                return hex;
        \\            }
        \\        };
        \\    },
        \\    createHmac: function(algorithm, key) {
        \\        var algo = algorithm.toLowerCase();
        \\        var keyStr = typeof key === 'string' ? key : String(key);
        \\        return {
        \\            _algorithm: algo,
        \\            _key: keyStr,
        \\            _data: '',
        \\            update: function(data) {
        \\                this._data += (typeof data === 'string' ? data : String(data));
        \\                return this;
        \\            },
        \\            digest: function(encoding) {
        \\                var hex = __edgebox_hmac(this._algorithm, this._key, this._data);
        \\                if (encoding === 'hex') return hex;
        \\                if (encoding === 'base64') {
        \\                    var bytes = [];
        \\                    for (var i = 0; i < hex.length; i += 2) {
        \\                        bytes.push(parseInt(hex.substr(i, 2), 16));
        \\                    }
        \\                    return btoa(String.fromCharCode.apply(null, bytes));
        \\                }
        \\                return hex;
        \\            }
        \\        };
        \\    },
        \\    getHashes: function() { return ['sha256', 'sha384', 'sha512', 'sha1', 'md5']; },
        \\    getRandomValues: function(buf) { for (var i = 0; i < buf.length; i++) buf[i] = Math.floor(Math.random() * 256); return buf; }
        \\};
        \\globalThis._modules['node:crypto'] = globalThis._modules['crypto'];
        \\
        \\// WASI-NN AI module for LLM inference
        \\globalThis._modules['ai'] = {
        \\    chat: function(prompt) {
        \\        if (typeof __edgebox_ai_chat === 'function') {
        \\            return __edgebox_ai_chat(prompt);
        \\        }
        \\        throw new Error('WASI-NN not available. Build with: zig build wasm -Denable-wasi-nn=true');
        \\    },
        \\    isAvailable: function() {
        \\        return typeof __edgebox_ai_chat === 'function';
        \\    }
        \\};
        \\globalThis.ai = globalThis._modules['ai'];
        \\
        \\globalThis._modules['diagnostics_channel'] = { channel: () => ({ subscribe: () => {}, unsubscribe: () => {}, publish: () => {} }), hasSubscribers: () => false, subscribe: () => {}, unsubscribe: () => {} };
        \\globalThis._modules['node:diagnostics_channel'] = globalThis._modules['diagnostics_channel'];
        \\globalThis._modules['inspector'] = { open: () => {}, close: () => {}, url: () => undefined, waitForDebugger: () => {} };
        \\globalThis._modules['node:inspector'] = globalThis._modules['inspector'];
        \\// cluster is defined in polyfill modules with full fork() support
        \\// dgram is defined in polyfill modules with full UDP socket support
        \\globalThis._modules['domain'] = { create: () => ({ run: fn => fn(), on: () => {} }) };
        \\globalThis._modules['node:domain'] = globalThis._modules['domain'];
        \\globalThis._modules['trace_events'] = { createTracing: () => ({ enable: () => {}, disable: () => {} }) };
        \\globalThis._modules['node:trace_events'] = globalThis._modules['trace_events'];
        \\globalThis._modules['wasi'] = {};
        \\globalThis._modules['node:wasi'] = globalThis._modules['wasi'];
        \\globalThis._modules['repl'] = { start: () => { throw new Error('repl not implemented'); } };
        \\globalThis._modules['node:repl'] = globalThis._modules['repl'];
        \\globalThis._modules['console'] = globalThis.console;
        \\globalThis._modules['node:console'] = globalThis.console;
        \\globalThis._modules['sys'] = globalThis._modules['util'];
        \\globalThis._modules['node:sys'] = globalThis._modules['util'];
        \\globalThis._modules['util/types'] = globalThis._modules['util']?.types || { isRegExp: (o) => o instanceof RegExp, isDate: (o) => o instanceof Date, isNativeError: (o) => o instanceof Error };
        \\globalThis._modules['node:util/types'] = globalThis._modules['util/types'];
        \\
        \\// Web Streams API (minimal polyfill)
        \\if (!globalThis.ReadableStream) {
        \\    globalThis.ReadableStream = class ReadableStream {
        \\        constructor(underlyingSource, strategy) {
        \\            this._source = underlyingSource;
        \\            this._reader = null;
        \\            this._locked = false;
        \\            this._controller = { enqueue: (chunk) => {}, close: () => {}, error: (e) => {} };
        \\            if (underlyingSource?.start) underlyingSource.start(this._controller);
        \\        }
        \\        get locked() { return this._locked; }
        \\        getReader() { this._locked = true; return { read: async () => ({ done: true, value: undefined }), releaseLock: () => { this._locked = false; }, cancel: async () => {} }; }
        \\        cancel() { return Promise.resolve(); }
        \\        tee() { return [new ReadableStream(), new ReadableStream()]; }
        \\        pipeTo(dest) { return Promise.resolve(); }
        \\        pipeThrough(transform) { return transform.readable; }
        \\        async *[Symbol.asyncIterator]() {}
        \\    };
        \\}
        \\if (!globalThis.WritableStream) {
        \\    globalThis.WritableStream = class WritableStream {
        \\        constructor(underlyingSink, strategy) {
        \\            this._sink = underlyingSink;
        \\            this._writer = null;
        \\            this._locked = false;
        \\        }
        \\        get locked() { return this._locked; }
        \\        getWriter() { this._locked = true; return { write: async (chunk) => {}, close: async () => {}, abort: async () => {}, releaseLock: () => { this._locked = false; }, ready: Promise.resolve(), closed: Promise.resolve() }; }
        \\        abort() { return Promise.resolve(); }
        \\        close() { return Promise.resolve(); }
        \\    };
        \\}
        \\if (!globalThis.TransformStream) {
        \\    globalThis.TransformStream = class TransformStream {
        \\        constructor(transformer) { this.readable = new ReadableStream(); this.writable = new WritableStream(); }
        \\    };
        \\}
        \\globalThis._modules['stream/web'] = { ReadableStream: globalThis.ReadableStream, WritableStream: globalThis.WritableStream, TransformStream: globalThis.TransformStream };
        \\globalThis._modules['node:stream/web'] = globalThis._modules['stream/web'];
        \\
        \\// AbortController/AbortSignal
        \\const EventEmitter = globalThis._modules.events;
        \\globalThis.AbortSignal = class AbortSignal extends EventEmitter {
        \\    constructor() { super(); this.aborted = false; this.reason = undefined; }
        \\    throwIfAborted() { if (this.aborted) throw this.reason; }
        \\    static abort(reason) { const s = new AbortSignal(); s.aborted = true; s.reason = reason || new DOMException('Aborted', 'AbortError'); return s; }
        \\    static timeout(ms) { const s = new AbortSignal(); setTimeout(() => { s.aborted = true; s.reason = new DOMException('Timeout', 'TimeoutError'); s.emit('abort', s.reason); }, ms); return s; }
        \\};
        \\globalThis.AbortController = class AbortController {
        \\    constructor() { this.signal = new AbortSignal(); }
        \\    abort(reason) { if (!this.signal.aborted) { this.signal.aborted = true; this.signal.reason = reason || new DOMException('Aborted', 'AbortError'); this.signal.emit('abort', this.signal.reason); } }
        \\};
        \\
        \\// Update process with native TTY info
        \\if (globalThis.process) {
        \\    globalThis.process.stdin = globalThis.process.stdin || { isTTY: globalThis.__edgebox_isatty(0), fd: 0, read: (size) => globalThis.__edgebox_read_stdin(size || 1024) };
        \\    globalThis.process.stdout = globalThis.process.stdout || { isTTY: globalThis.__edgebox_isatty(1), fd: 1, write: (data) => print(data) };
        \\    globalThis.process.stderr = globalThis.process.stderr || { isTTY: globalThis.__edgebox_isatty(2), fd: 2, write: (data) => print(data) };
        \\    globalThis.process.argv = scriptArgs || [];
        \\    globalThis.process.cwd = () => std.getenv('PWD') || '/';
        \\    globalThis.process.exit = (code) => std.exit(code || 0);
        \\    // Create process.env as a Proxy to access environment variables via std.getenv
        \\    globalThis.process.env = new Proxy({}, {
        \\        get(target, name) {
        \\            if (typeof name === 'symbol') return undefined;
        \\            if (name === 'toJSON') return () => target;
        \\            // Check cache first
        \\            if (name in target) return target[name];
        \\            // Get from environment
        \\            var val = std.getenv(String(name));
        \\            if (val !== undefined) target[name] = val;
        \\            return val;
        \\        },
        \\        has(target, name) {
        \\            if (typeof name === 'symbol') return false;
        \\            return std.getenv(String(name)) !== undefined;
        \\        },
        \\        ownKeys(target) {
        \\            // Can't enumerate env vars in QuickJS, return cached keys
        \\            return Object.keys(target);
        \\        },
        \\        getOwnPropertyDescriptor(target, name) {
        \\            var val = std.getenv(String(name));
        \\            if (val !== undefined) {
        \\                return { value: val, writable: true, enumerable: true, configurable: true };
        \\            }
        \\            return undefined;
        \\        }
        \\    });
        \\}
    ;

    _ = context.eval(platform_polyfills) catch |err| {
        std.debug.print("Failed to inject platform polyfills: {}\n", .{err});
        if (context.getException()) |exc| {
            defer exc.free();
            if (exc.toStringSlice()) |msg| {
                std.debug.print("Exception: {s}\n", .{msg});
            }
        }
    };
}

