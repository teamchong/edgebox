/// Wizer Pre-initialization Support for EdgeBox
///
/// This module provides "instant startup" by pre-initializing the QuickJS
/// runtime at build time. Wizer snapshots the memory state after wizer_init()
/// runs, embedding it into the WASM binary.
///
/// Build pipeline:
/// 1. zig build wasm -> edgebox-base.wasm (with wizer_init export)
/// 2. wizer edgebox-base.wasm --init-func=wizer_init -> edgebox-wizer.wasm
/// 3. wasmedge compile edgebox-wizer.wasm -> edgebox-aot.dylib
///
/// At runtime, main() detects wizer_initialized=true and uses the
/// pre-initialized runtime, skipping JS_NewRuntime/JS_NewContext.
const std = @import("std");
const quickjs = @import("quickjs_core.zig");
const wasm_bump = @import("wasm_bump.zig");

const qjs = quickjs.c;

// ============================================================================
// GLOBAL STATE (Persists across Wizer snapshot)
// ============================================================================

/// Wizer-initialized QuickJS runtime (populated at build time)
pub var wizer_runtime: ?*qjs.JSRuntime = null;

/// Wizer-initialized QuickJS context (populated at build time)
pub var wizer_context: ?*qjs.JSContext = null;

/// Flag indicating Wizer initialization completed successfully
pub var wizer_initialized: bool = false;

// ============================================================================
// NATIVE BINDING STUBS (For Wizer snapshot - replaced at runtime)
// ============================================================================

/// Stub function for Wizer build-time placeholders (returns undefined)
/// These stubs ensure polyfills see functions (not undefined) during Wizer init.
/// At runtime, registerWizerNativeBindings() in wasm_main_static.zig overwrites
/// these with real implementations.
fn nativeStub(_: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return qjs.JS_UNDEFINED;
}

/// Register native binding stubs during Wizer init
/// These placeholders ensure polyfills see functions, not undefined
fn registerNativeBindingStubs(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // All native function names from wasm_main_static.zig registerWizerNativeBindings()
    const native_names = [_][*:0]const u8{
        // Async HTTP
        "__edgebox_fetch",
        "__edgebox_fetch_start",
        "__edgebox_fetch_poll",
        "__edgebox_fetch_response",
        // Async Spawn
        "__edgebox_spawn_start",
        "__edgebox_spawn_poll",
        "__edgebox_spawn_output",
        // Async File I/O
        "__edgebox_file_read_start",
        "__edgebox_file_write_start",
        "__edgebox_file_poll",
        "__edgebox_file_result",
        // Sync bindings
        "__edgebox_isatty",
        "__edgebox_get_terminal_size",
        "__edgebox_read_stdin",
        "__edgebox_stdin_ready",
        "__edgebox_spawn",
        // fs bindings
        "__edgebox_fs_read",
        "__edgebox_fs_write",
        "__edgebox_fs_append",
        "__edgebox_fs_exists",
        "__edgebox_fs_stat",
        "__edgebox_fs_readdir",
        "__edgebox_fs_mkdir",
        "__edgebox_fs_unlink",
        "__edgebox_fs_rmdir",
        "__edgebox_fs_rename",
        "__edgebox_fs_copy",
        "__edgebox_cwd",
        "__edgebox_homedir",
        // crypto bindings
        "__edgebox_hash",
        "__edgebox_hmac",
        // zlib bindings
        "__edgebox_gzip",
        "__edgebox_gunzip",
        "__edgebox_deflate",
        "__edgebox_inflate",
        // crypto AES bindings
        "__edgebox_aes_gcm_encrypt",
        "__edgebox_aes_gcm_decrypt",
        "__edgebox_random_bytes",
        // socket bindings
        "__edgebox_socket_create",
        "__edgebox_socket_bind",
        "__edgebox_socket_listen",
        "__edgebox_socket_accept",
        "__edgebox_socket_connect",
        "__edgebox_socket_write",
        "__edgebox_socket_read",
        "__edgebox_socket_close",
        "__edgebox_socket_state",
    };

    for (native_names) |name| {
        const stub = qjs.JS_NewCFunction(ctx, nativeStub, name, 0);
        _ = qjs.JS_SetPropertyStr(ctx, global, name, stub);
    }
}

// ============================================================================
// WIZER INITIALIZATION (Runs at BUILD TIME)
// ============================================================================

/// Wizer initialization function - exported and called at build time.
/// Creates the QuickJS runtime/context and pre-compiles static polyfills.
///
/// This function MUST NOT:
/// - Open files or sockets (WASI state is not snapshotted)
/// - Call Date.now() or Math.random() (seeds would be fixed)
/// - Read environment variables (dynamic)
/// - Read command line args (dynamic)
pub fn wizer_init() void {
    // Initialize bump allocator for QuickJS memory
    wasm_bump.init();

    // Create malloc functions using our bump allocator
    const malloc_funcs = qjs.JSMallocFunctions{
        .js_calloc = wasm_bump.js_calloc,
        .js_malloc = wasm_bump.js_malloc,
        .js_free = wasm_bump.js_free,
        .js_realloc = wasm_bump.js_realloc,
        .js_malloc_usable_size = wasm_bump.js_malloc_usable_size,
    };

    // Create QuickJS runtime with bump allocator
    wizer_runtime = qjs.JS_NewRuntime2(&malloc_funcs, null);
    if (wizer_runtime == null) return;

    // Set module loader (required for ES6 imports like 'std' and 'os')
    qjs.JS_SetModuleLoaderFunc(wizer_runtime, null, qjs.js_module_loader, null);

    // Create context
    wizer_context = qjs.JS_NewContext(wizer_runtime);
    if (wizer_context == null) return;

    // Initialize std thread state (timer list, signal handlers, etc.)
    // This MUST be called before js_init_module_os for timers to work
    qjs.js_std_init_handlers(wizer_runtime);

    // Initialize std/os modules (module loaders, not fd bindings)
    _ = qjs.js_init_module_std(wizer_context, "std");
    _ = qjs.js_init_module_os(wizer_context, "os");

    // Register native binding stubs BEFORE polyfills
    // This ensures polyfills see functions (not undefined) when they capture globals
    if (wizer_context) |ctx| {
        registerNativeBindingStubs(ctx);
    }

    // Pre-compile static polyfills (class definitions, pure functions)
    initStaticPolyfills();

    // Mark arena position after init (for request-scoped reset)
    wasm_bump.mark();

    wizer_initialized = true;
}

/// Initialize polyfills at Wizer build time.
/// Loads the complete node_polyfill.js which contains all Node.js module implementations.
/// This is the single source of truth for all polyfills.
fn initStaticPolyfills() void {
    const ctx = wizer_context orelse return;

    // Load runtime.js first (console helpers, error handlers)
    const runtime_js = @embedFile("polyfills/runtime.js");
    var val = qjs.JS_Eval(ctx, runtime_js.ptr, runtime_js.len, "<runtime>", qjs.JS_EVAL_TYPE_GLOBAL);
    qjs.JS_FreeValue(ctx, val);

    // Load main polyfills (all Node.js modules: fs, path, net, http, etc.)
    const node_polyfill_js = @embedFile("polyfills/node_polyfill.js");
    val = qjs.JS_Eval(ctx, node_polyfill_js.ptr, node_polyfill_js.len, "<polyfills>", qjs.JS_EVAL_TYPE_GLOBAL);
    if (qjs.JS_IsException(val)) {
        // Print exception for debugging
        const exc = qjs.JS_GetException(ctx);
        const str = qjs.JS_ToCString(ctx, exc);
        if (str != null) {
            std.debug.print("Polyfill error: {s}\n", .{str});
            qjs.JS_FreeCString(ctx, str);
        }
        qjs.JS_FreeValue(ctx, exc);
    }
    qjs.JS_FreeValue(ctx, val);

    // Mark as initialized - set all guard flags so bundled polyfills skip re-init
    const init_marker =
        \\globalThis._wizerInitialized = true;
        \\globalThis._runtimePolyfillsInitialized = true;
        \\globalThis._polyfillsInitialized = true;
    ;
    val = qjs.JS_Eval(ctx, init_marker.ptr, init_marker.len, "<init>", qjs.JS_EVAL_TYPE_GLOBAL);
    qjs.JS_FreeValue(ctx, val);
}
// ============================================================================
// RUNTIME HELPERS (Called at runtime to bind dynamic state)
// ============================================================================

/// Check if we're running with Wizer pre-initialization
pub fn isWizerInitialized() bool {
    return wizer_initialized and wizer_runtime != null and wizer_context != null;
}

/// Get the pre-initialized context (for use in main())
pub fn getContext() ?*qjs.JSContext {
    return wizer_context;
}

/// Get the pre-initialized runtime
pub fn getRuntime() ?*qjs.JSRuntime {
    return wizer_runtime;
}
