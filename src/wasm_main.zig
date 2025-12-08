/// EdgeBox WASM Entry Point
/// QuickJS runtime for WASI with networking support
///
/// Supports Wizer pre-initialization for instant startup:
/// - If wizer_init was called at build time, uses pre-initialized runtime
/// - Otherwise falls back to traditional runtime creation
const std = @import("std");
const quickjs = @import("quickjs_core.zig");
const wasm_fetch = @import("wasm_fetch.zig");
const wasi_tty = @import("wasi_tty.zig");
const wasi_process = @import("wasi_process.zig");
const node_polyfills = @import("node_polyfills.zig");
const snapshot = @import("snapshot.zig");
const pool_alloc = @import("wasm_pool_alloc.zig");
const wizer_mod = @import("wizer_init.zig");

// Export wizer_init for Wizer to call at build time
// Using 'export fn' directly to ensure it appears in the WASM exports
export fn wizer_init() void {
    wizer_mod.wizer_init();
}

// Global allocator for native bindings
var global_allocator: ?std.mem.Allocator = null;

// Startup timing for cold start measurement
var startup_time_ns: i128 = 0;

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

pub fn main() !void {
    // Record startup time for cold start measurement
    startup_time_ns = getTimeNs();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    global_allocator = allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print(
            \\EdgeBox - QuickJS WASM Runtime (WasmEdge)
            \\
            \\Usage:
            \\  edgebox <script.js>     Run JavaScript file
            \\  edgebox -e "<code>"     Evaluate JavaScript code
            \\
            \\Features:
            \\  - QuickJS JavaScript engine
            \\  - WASI filesystem access
            \\  - Network sockets (WasmEdge)
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
    // Bump allocator - fast O(1) malloc, NO-OP free
    // Memory is reclaimed when WASM instance exits (perfect for serverless)
    var use_pool_allocator = @import("builtin").target.cpu.arch == .wasm32;
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--no-pool-allocator")) {
            use_pool_allocator = false;
            break;
        }
    }

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

    // Create QuickJS runtime with std module (print, console, etc.)
    // Use pool allocator for O(1) malloc/free if requested
    var runtime = if (use_pool_allocator)
        try quickjs.Runtime.initWithPoolAllocator(allocator)
    else
        try quickjs.Runtime.init(allocator);
    defer runtime.deinit();

    var context = try runtime.newStdContextWithArgs(@intCast(c_argv.len), c_argv.ptr);
    defer context.deinit();

    // Inject minimal bootstrap only - polyfills are lazy-loaded on first use
    // This dramatically improves cold start time
    injectMinimalBootstrap(&context) catch |err| {
        std.debug.print("Warning: Failed to inject bootstrap: {}\n", .{err});
    };

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
}

/// Bind dynamic state that changes per request (process.argv, process.env)
fn bindDynamicState(ctx: *quickjs.c.JSContext, args: []const [:0]u8) void {

    // Dynamic polyfills - process object with argv/env
    const dynamic_init =
        \\globalThis.process = globalThis.process || {};
        \\globalThis.process.version = 'v20.0.0';
        \\globalThis.process.versions = { node: '20.0.0' };
        \\globalThis.process.platform = 'wasi';
        \\globalThis.process.arch = 'wasm32';
        \\globalThis.process.exit = (code) => { if (typeof std !== 'undefined') std.exit(code || 0); };
        \\globalThis.process.cwd = () => std.getenv('PWD') || '/';
        \\globalThis.process.env = new Proxy({}, {
        \\    get(t, n) { return typeof n === 'symbol' ? undefined : std.getenv(String(n)); },
        \\    has(t, n) { return typeof n !== 'symbol' && std.getenv(String(n)) !== undefined; }
        \\});
        \\import * as std from 'std';
        \\globalThis.std = std;
    ;

    const val = qjs.JS_Eval(ctx, dynamic_init.ptr, dynamic_init.len, "<dynamic>", qjs.JS_EVAL_TYPE_MODULE);
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
        .{ "__edgebox_spawn", nativeSpawn, 4 },
        .{ "__edgebox_load_polyfills", nativeLoadPolyfills, 0 },
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

    const code = std.fs.cwd().readFileAlloc(allocator, script_path, 50 * 1024 * 1024) catch |err| {
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

    // Read source
    const code = std.fs.cwd().readFileAlloc(allocator, resolved_path, 50 * 1024 * 1024) catch |err| {
        std.debug.print("Error reading {s}: {}\n", .{ resolved_path, err });
        std.process.exit(1);
    };
    defer allocator.free(code);

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
            // Execute cached bytecode (includes bundled polyfills + user code)
            const result = context.loadBytecode(bc) catch |err| {
                std.debug.print("Cache execution error: {}\n", .{err});
                if (context.getException()) |exc| {
                    defer exc.free();
                    if (exc.toStringSlice()) |msg| {
                        std.debug.print("Exception: {s}\n", .{msg});
                    }
                }
                std.process.exit(1);
            };
            result.free();
            allocator.free(bc);
        }
        return;
    }

    // No valid cache - slow path: read, compile, cache, execute
    const code = std.fs.cwd().readFileAlloc(allocator, resolved_path, 50 * 1024 * 1024) catch |err| {
        std.debug.print("Error reading {s}: {}\n", .{ resolved_path, err });
        std.process.exit(1);
    };
    defer allocator.free(code);

    // Compile to bytecode
    const bytecode = context.compile(code, script_path, allocator) catch {
        // Compilation failed, fall back to eval
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

/// Run file without caching (fallback)
fn runFileDirectly(allocator: std.mem.Allocator, context: *quickjs.Context, script_path: [:0]const u8) !void {
    const code = std.fs.cwd().readFileAlloc(allocator, script_path, 50 * 1024 * 1024) catch |err| {
        std.debug.print("Error reading {s}: {}\n", .{ script_path, err });
        std.process.exit(1);
    };
    defer allocator.free(code);

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

    // Register child_process functions
    context.registerGlobalFunction("__edgebox_spawn", nativeSpawn, 4);

    // Register lazy polyfill loader
    context.registerGlobalFunction("__edgebox_load_polyfills", nativeLoadPolyfills, 0);
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
        \\    // Load polyfills on first require of a Node.js module
        \\    if (!globalThis._polyfillsLoaded) {
        \\        if (name.startsWith('node:') || ['fs', 'path', 'os', 'buffer', 'events', 'stream', 'util', 'http', 'https', 'crypto', 'child_process', 'tty', 'net', 'dns', 'url', 'querystring', 'zlib', 'assert', 'timers', 'readline', 'module', 'process'].includes(name)) {
        \\            globalThis.__edgebox_load_polyfills();
        \\            globalThis._polyfillsLoaded = true;
        \\        }
        \\    }
        \\    const mod = globalThis._modules[name];
        \\    if (mod !== undefined) return mod;
        \\    throw new Error('Module not found: ' + name);
        \\};
        \\
        \\// Basic process object (minimal)
        \\globalThis.process = globalThis.process || {
        \\    version: 'v20.0.0',
        \\    versions: { node: '20.0.0' },
        \\    platform: 'wasi',
        \\    arch: 'wasm32',
        \\    env: {},
        \\    argv: typeof scriptArgs !== 'undefined' ? ['node', ...scriptArgs] : ['node'],
        \\    cwd: () => '/',
        \\    exit: (code) => { if (typeof std !== 'undefined') std.exit(code || 0); },
        \\    nextTick: (fn, ...args) => queueMicrotask(() => fn(...args)),
        \\};
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

    // Step 3: Load Node.js polyfills from external JS files (embedded at compile time)
    try node_polyfills.init(context);

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
        \\globalThis._modules['child_process'] = {
        \\    spawnSync: function(command, args = [], options = {}) {
        \\        const argsArray = Array.isArray(args) ? args : [];
        \\        const stdinData = options.input || null;
        \\        const timeout = options.timeout || 30000;
        \\        try {
        \\            const result = globalThis.__edgebox_spawn(command, argsArray, stdinData, timeout);
        \\            return {
        \\                status: result.exitCode,
        \\                stdout: result.stdout,
        \\                stderr: result.stderr,
        \\                error: result.exitCode !== 0 ? new Error('Process exited with code ' + result.exitCode) : null,
        \\                signal: null,
        \\            };
        \\        } catch (e) {
        \\            return { status: null, stdout: '', stderr: '', error: e, signal: null };
        \\        }
        \\    },
        \\    execSync: function(command, options = {}) {
        \\        const parts = command.trim().split(/\s+/);
        \\        const result = this.spawnSync(parts[0], parts.slice(1), options);
        \\        if (result.error && result.status !== 0) {
        \\            const err = new Error('Command failed: ' + command);
        \\            err.status = result.status;
        \\            err.stderr = result.stderr;
        \\            throw err;
        \\        }
        \\        return result.stdout;
        \\    },
        \\    spawn: function(command, args = [], options = {}) {
        \\        const self = this;
        \\        return {
        \\            stdout: { on: () => {} },
        \\            stderr: { on: () => {} },
        \\            on: (event, callback) => {
        \\                if (event === 'close') {
        \\                    const result = self.spawnSync(command, args, options);
        \\                    setTimeout(() => callback(result.status), 0);
        \\                }
        \\            },
        \\        };
        \\    },
        \\};
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
        \\// Additional module stubs
        \\globalThis._modules['http'] = {
        \\    Agent: class Agent { constructor(opts) { this.options = opts || {}; this.requests = {}; this.sockets = {}; this.freeSockets = {}; this.maxSockets = 256; this.maxFreeSockets = 256; } createConnection() { throw new Error('http Agent not implemented'); } destroy() {} },
        \\    request: () => { throw new Error('http.request not implemented'); },
        \\    get: () => { throw new Error('http.get not implemented'); },
        \\    createServer: () => { throw new Error('http.createServer not implemented'); },
        \\    globalAgent: null,
        \\    METHODS: ['GET','POST','PUT','DELETE','PATCH','HEAD','OPTIONS'],
        \\    STATUS_CODES: { 200: 'OK', 201: 'Created', 204: 'No Content', 301: 'Moved Permanently', 302: 'Found', 304: 'Not Modified', 400: 'Bad Request', 401: 'Unauthorized', 403: 'Forbidden', 404: 'Not Found', 500: 'Internal Server Error' }
        \\};
        \\globalThis._modules['http'].globalAgent = new globalThis._modules['http'].Agent();
        \\globalThis._modules['node:http'] = globalThis._modules['http'];
        \\globalThis._modules['https'] = {
        \\    Agent: class Agent extends globalThis._modules['http'].Agent { constructor(opts) { super(opts); } },
        \\    request: () => { throw new Error('https.request not implemented'); },
        \\    get: () => { throw new Error('https.get not implemented'); },
        \\    createServer: () => { throw new Error('https.createServer not implemented'); },
        \\    globalAgent: null
        \\};
        \\globalThis._modules['https'].globalAgent = new globalThis._modules['https'].Agent();
        \\globalThis._modules['node:https'] = globalThis._modules['https'];
        \\globalThis._modules['http2'] = {
        \\    constants: { HTTP2_HEADER_PATH: ':path', HTTP2_HEADER_STATUS: ':status', HTTP2_HEADER_METHOD: ':method', HTTP2_HEADER_AUTHORITY: ':authority', HTTP2_HEADER_SCHEME: ':scheme' },
        \\    connect: () => { throw new Error('http2 not implemented'); },
        \\    createServer: () => { throw new Error('http2 not implemented'); },
        \\    createSecureServer: () => { throw new Error('http2 not implemented'); }
        \\};
        \\globalThis._modules['node:http2'] = globalThis._modules['http2'];
        \\globalThis._modules['tls'] = { connect: () => { throw new Error('tls not implemented'); }, createServer: () => { throw new Error('tls not implemented'); }, createSecureContext: () => ({}) };
        \\globalThis._modules['node:tls'] = globalThis._modules['tls'];
        \\globalThis._modules['net'] = { createConnection: () => { throw new Error('net.createConnection not implemented'); } };
        \\globalThis._modules['node:net'] = globalThis._modules['net'];
        \\globalThis._modules['dns'] = { lookup: () => { throw new Error('dns.lookup not implemented'); } };
        \\globalThis._modules['node:dns'] = globalThis._modules['dns'];
        \\globalThis._modules['module'] = { createRequire: (url) => globalThis.require };
        \\globalThis._modules['node:module'] = globalThis._modules['module'];
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
        \\globalThis._modules['worker_threads'] = { isMainThread: true, parentPort: null };
        \\globalThis._modules['node:worker_threads'] = globalThis._modules['worker_threads'];
        \\globalThis._modules['crypto'] = globalThis.crypto;
        \\globalThis._modules['node:crypto'] = globalThis.crypto;
        \\globalThis._modules['diagnostics_channel'] = { channel: () => ({ subscribe: () => {}, unsubscribe: () => {}, publish: () => {} }), hasSubscribers: () => false, subscribe: () => {}, unsubscribe: () => {} };
        \\globalThis._modules['node:diagnostics_channel'] = globalThis._modules['diagnostics_channel'];
        \\globalThis._modules['inspector'] = { open: () => {}, close: () => {}, url: () => undefined, waitForDebugger: () => {} };
        \\globalThis._modules['node:inspector'] = globalThis._modules['inspector'];
        \\globalThis._modules['cluster'] = { isMaster: true, isPrimary: true, isWorker: false, workers: {} };
        \\globalThis._modules['node:cluster'] = globalThis._modules['cluster'];
        \\globalThis._modules['dgram'] = { createSocket: () => { throw new Error('dgram not implemented'); } };
        \\globalThis._modules['node:dgram'] = globalThis._modules['dgram'];
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

// ============================================================================
// Native Bindings for WASM
// ============================================================================

const qjs = quickjs.c;

/// Get JS undefined value
inline fn jsUndefined() qjs.JSValue {
    // JS_UNDEFINED in QuickJS - use the C macro via eval
    return qjs.JS_UNDEFINED;
}

/// Get JS bool value
inline fn jsBool(val: bool) qjs.JSValue {
    return if (val) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// Get string argument from JS value
fn getStringArg(ctx: ?*qjs.JSContext, val: qjs.JSValue) ?[]const u8 {
    var len: usize = undefined;
    const cstr = qjs.JS_ToCStringLen(ctx, &len, val);
    if (cstr == null) return null;
    return cstr[0..len];
}

/// Free string argument
fn freeStringArg(ctx: ?*qjs.JSContext, str: []const u8) void {
    qjs.JS_FreeCString(ctx, str.ptr);
}

/// Native fetch implementation for WASM
fn nativeFetch(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fetch requires url argument");

    const url = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "url must be a string");
    defer freeStringArg(ctx, url);

    // Get method (default GET)
    const method = if (argc >= 2) getStringArg(ctx, argv[1]) orelse "GET" else "GET";
    const method_owned = argc >= 2 and getStringArg(ctx, argv[1]) != null;
    defer if (method_owned) freeStringArg(ctx, method);

    // Get body (optional - arg 3)
    const body = if (argc >= 4 and !qjs.JS_IsUndefined(argv[3]) and !qjs.JS_IsNull(argv[3]))
        getStringArg(ctx, argv[3])
    else
        null;
    defer if (body) |b| freeStringArg(ctx, b);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Perform fetch using WASI sockets
    var response = wasm_fetch.jsFetch(allocator, url, method, null, body) catch |err| {
        return switch (err) {
            wasm_fetch.FetchError.InvalidUrl => qjs.JS_ThrowTypeError(ctx, "Invalid URL"),
            wasm_fetch.FetchError.ConnectionFailed => qjs.JS_ThrowInternalError(ctx, "Connection failed"),
            wasm_fetch.FetchError.HostNotFound => qjs.JS_ThrowInternalError(ctx, "Host not found"),
            wasm_fetch.FetchError.Timeout => qjs.JS_ThrowInternalError(ctx, "Request timed out"),
            wasm_fetch.FetchError.InvalidResponse => qjs.JS_ThrowInternalError(ctx, "Invalid HTTP response"),
            wasm_fetch.FetchError.OutOfMemory => qjs.JS_ThrowInternalError(ctx, "Out of memory"),
            wasm_fetch.FetchError.TlsNotSupported => qjs.JS_ThrowInternalError(ctx, "HTTPS not supported yet (use HTTP)"),
        };
    };
    defer response.deinit();

    // Create response object
    const obj = qjs.JS_NewObject(ctx);

    // Set status
    _ = qjs.JS_SetPropertyStr(ctx, obj, "status", qjs.JS_NewInt32(ctx, @intCast(response.status)));

    // Set ok (status 200-299)
    _ = qjs.JS_SetPropertyStr(ctx, obj, "ok", jsBool(response.status >= 200 and response.status < 300));

    // Set body as string
    _ = qjs.JS_SetPropertyStr(ctx, obj, "body", qjs.JS_NewStringLen(ctx, response.body.ptr, response.body.len));

    // Set headers as object
    const headers_obj = qjs.JS_NewObject(ctx);
    for (response.headers.items) |h| {
        // Need null-terminated key for JS_SetPropertyStr
        var key_buf: [256]u8 = undefined;
        if (h.name.len < key_buf.len) {
            @memcpy(key_buf[0..h.name.len], h.name);
            key_buf[h.name.len] = 0;
            _ = qjs.JS_SetPropertyStr(
                ctx,
                headers_obj,
                &key_buf,
                qjs.JS_NewStringLen(ctx, h.value.ptr, h.value.len),
            );
        }
    }
    _ = qjs.JS_SetPropertyStr(ctx, obj, "headers", headers_obj);

    return obj;
}

/// Native isatty implementation
fn nativeIsatty(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return jsBool(false);

    var fd: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &fd, argv[0]) < 0) {
        return jsBool(false);
    }

    const is_tty = wasi_tty.isatty(fd);
    return jsBool(is_tty);
}

/// Native get terminal size implementation
fn nativeGetTerminalSize(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const size = wasi_tty.getTerminalSize() orelse {
        // Return default size
        const obj = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, obj, "rows", qjs.JS_NewInt32(ctx, 24));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "cols", qjs.JS_NewInt32(ctx, 80));
        return obj;
    };

    const obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, obj, "rows", qjs.JS_NewInt32(ctx, size.rows));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "cols", qjs.JS_NewInt32(ctx, size.cols));
    return obj;
}

/// Native read stdin implementation
fn nativeReadStdin(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    var max_size: i32 = 1024;
    if (argc >= 1) {
        _ = qjs.JS_ToInt32(ctx, &max_size, argv[0]);
    }
    if (max_size <= 0) max_size = 1024;

    const line = wasi_tty.readLine(allocator, @intCast(max_size)) catch |err| {
        return qjs.JS_ThrowInternalError(ctx, "read error: %d", @intFromError(err));
    } orelse {
        return qjs.JS_NULL;
    };
    defer allocator.free(line);

    return qjs.JS_NewStringLen(ctx, line.ptr, line.len);
}

/// Native spawn implementation for child_process
/// Args: command (string), args (array), stdin (string|null), timeout (number)
fn nativeSpawn(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "spawn requires command argument");

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Get command
    const command = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "command must be a string");
    defer freeStringArg(ctx, command);

    // Build command
    var cmd = wasi_process.Command.init(allocator, command);
    defer cmd.deinit();

    // Parse args array (arg 1)
    if (argc >= 2 and qjs.JS_IsArray(argv[1])) {
        const arr_len_val = qjs.JS_GetPropertyStr(ctx, argv[1], "length");
        var arr_len: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &arr_len, arr_len_val);
        qjs.JS_FreeValue(ctx, arr_len_val);

        var i: u32 = 0;
        while (i < @as(u32, @intCast(arr_len))) : (i += 1) {
            const elem = qjs.JS_GetPropertyUint32(ctx, argv[1], i);
            defer qjs.JS_FreeValue(ctx, elem);

            if (getStringArg(ctx, elem)) |arg_str| {
                // Need to copy because arg_str is freed when elem is freed
                const arg_copy = allocator.dupe(u8, arg_str) catch {
                    return qjs.JS_ThrowInternalError(ctx, "out of memory");
                };
                freeStringArg(ctx, arg_str);
                _ = cmd.arg(arg_copy) catch {
                    allocator.free(arg_copy);
                    return qjs.JS_ThrowInternalError(ctx, "out of memory");
                };
            }
        }
    }

    // Set stdin data (arg 2)
    if (argc >= 3 and !qjs.JS_IsNull(argv[2]) and !qjs.JS_IsUndefined(argv[2])) {
        if (getStringArg(ctx, argv[2])) |stdin_data| {
            _ = cmd.setStdin(stdin_data);
            // Note: don't free stdin_data until after command runs
        }
    }

    // Set timeout (arg 3)
    if (argc >= 4) {
        var timeout: i32 = 30000;
        _ = qjs.JS_ToInt32(ctx, &timeout, argv[3]);
        if (timeout > 0) {
            _ = cmd.setTimeout(@intCast(timeout));
        }
    }

    // Run the command
    var result = cmd.output() catch |err| {
        return switch (err) {
            wasi_process.ProcessError.CommandFailed => qjs.JS_ThrowInternalError(ctx, "Command failed to execute (WasmEdge process plugin not enabled?)"),
            wasi_process.ProcessError.TimedOut => qjs.JS_ThrowInternalError(ctx, "Command timed out"),
            wasi_process.ProcessError.OutOfMemory => qjs.JS_ThrowInternalError(ctx, "Out of memory"),
            wasi_process.ProcessError.InvalidCommand => qjs.JS_ThrowTypeError(ctx, "Invalid command"),
        };
    };
    defer result.deinit();

    // Create result object
    const obj = qjs.JS_NewObject(ctx);

    // Set exitCode
    _ = qjs.JS_SetPropertyStr(ctx, obj, "exitCode", qjs.JS_NewInt32(ctx, result.exit_code));

    // Set stdout
    if (result.stdout.len > 0) {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "stdout", qjs.JS_NewStringLen(ctx, result.stdout.ptr, result.stdout.len));
    } else {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "stdout", qjs.JS_NewString(ctx, ""));
    }

    // Set stderr
    if (result.stderr.len > 0) {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "stderr", qjs.JS_NewStringLen(ctx, result.stderr.ptr, result.stderr.len));
    } else {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "stderr", qjs.JS_NewString(ctx, ""));
    }

    return obj;
}

// ============================================================================
// Bytecode Cache Helpers
// ============================================================================

/// Hash all polyfill sources for cache invalidation
fn computePolyfillsHash() u64 {
    // Include the platform polyfills source (a representative sample)
    // In a full implementation, we'd hash all polyfill source files
    const sources = [_][]const u8{
        "EdgeBox-Polyfills-v1", // Version marker
        @embedFile("polyfills/node/buffer.js"),
        @embedFile("polyfills/node/events.js"),
    };
    return snapshot.hashPolyfills(&sources);
}
