/// Unified Static Entry Point
/// Executes pre-compiled bytecode with freeze optimizations
/// Compiles for both native and wasm32 targets via comptime
///
/// Build pipeline:
///   1. edgebox-compile compiles JS → bundle.bin
///   2. edgebox-compile generates frozen_module.zig
///   3. zig build native-static OR wasm-static embeds bytecode and compiles
///
const std = @import("std");
const builtin = @import("builtin");

// Comptime target detection
const is_wasm32 = builtin.cpu.arch == .wasm32;

// QuickJS C API - use shared import for type compatibility with polyfills
const quickjs = @import("quickjs_core.zig");
const qjs = quickjs.c;

// Universal polyfills (work on both native and wasm32)
const path_polyfill = @import("polyfills/path.zig");
const process_polyfill = @import("polyfills/process.zig");
const console_polyfill = @import("polyfills/console.zig");
const buffer_polyfill = @import("polyfills/buffer.zig");
const util_polyfill = @import("polyfills/util.zig");
const encoding_polyfill = @import("polyfills/encoding.zig");
const crypto_polyfill = @import("polyfills/crypto.zig");
const require_polyfill = @import("polyfills/require.zig");
const querystring_polyfill = @import("polyfills/querystring.zig");
const globals_polyfill = @import("polyfills/globals.zig");
const string_decoder_polyfill = @import("polyfills/string_decoder.zig");
const assert_polyfill = @import("polyfills/assert.zig");

// Native-only polyfills (require OS syscalls)
const os_polyfill = if (is_wasm32) null else @import("polyfills/os.zig");
const fs_polyfill = if (is_wasm32) null else @import("polyfills/fs.zig");
const tty_polyfill = if (is_wasm32) null else @import("polyfills/tty.zig");
const dns_polyfill = if (is_wasm32) null else @import("polyfills/dns.zig");
const child_process_polyfill = if (is_wasm32) null else @import("polyfills/child_process.zig");
const net_polyfill = if (is_wasm32) null else @import("polyfills/net.zig");
const tls_polyfill = if (is_wasm32) null else @import("polyfills/tls.zig");
const compression_polyfill = if (is_wasm32) null else @import("polyfills/compression.zig");

// Zig native registry (native-only, wasm32 gets stubs)
const native_shapes_registry = if (is_wasm32) null else @import("freeze/native_shapes.zig");
comptime {
    if (!is_wasm32) {
        _ = native_shapes_registry.?.native_registry_init;
        _ = native_shapes_registry.?.native_node_register;
        _ = native_shapes_registry.?.native_node_lookup;
    }
}

// WASM stubs for native registry functions (called by frozen_module's extern declarations)
// On native, these are provided by native_shapes_registry (via comptime reference above)
// On wasm32, we provide stub implementations
comptime {
    // Force export of stub functions for wasm32 linker
    if (is_wasm32) {
        @export(&wasm32_native_registry_init, .{ .name = "native_registry_init" });
        @export(&wasm32_native_registry_count, .{ .name = "native_registry_count" });
        @export(&wasm32_native_node_register32, .{ .name = "native_node_register32" });
    }
}

fn wasm32_native_registry_init() callconv(.c) void {}
fn wasm32_native_registry_count() callconv(.c) c_int {
    return 0;
}
fn wasm32_native_node_register32(_: u32, _: i32, _: i32, _: i32, _: i32) callconv(.c) ?*anyopaque {
    return null;
}

// Native bindings for fs, crypto, etc. (native-only)
const native_bindings = if (is_wasm32) null else @import("native_bindings.zig");

// Frozen module (generated Zig frozen functions)
const frozen_module = @import("frozen_module");
comptime {
    _ = &frozen_module.frozen_init_c;
}

// Zig hot paths (optional)
const zig_hotpaths = @import("zig_hotpaths");
comptime {
    _ = &zig_hotpaths;
}

// Embedded bytecode via @embedFile
const bytecode_module = @import("bytecode");
const bytecode: []const u8 = bytecode_module.data;

// Global allocator for native operations
var gpa = std.heap.GeneralPurposeAllocator(.{}){};

// Zig 0.15 compatible JS_TRUE (works for both native struct and wasm32 NaN-boxed)
const js_is_nan_boxed = @sizeOf(qjs.JSValue) == 8;
inline fn jsTrue() qjs.JSValue {
    if (comptime js_is_nan_boxed) {
        return @bitCast(@as(u64, 0x0000000100000001));
    } else {
        var v: qjs.JSValue = undefined;
        v.u.int32 = 1;
        v.tag = 1; // JS_TAG_BOOL
        return v;
    }
}

fn printException(ctx: *qjs.JSContext) void {
    const exc = qjs.JS_GetException(ctx);
    defer qjs.JS_FreeValue(ctx, exc);

    const str = qjs.JS_ToCString(ctx, exc);
    if (str != null) {
        std.debug.print("Exception: {s}\n", .{str});
        qjs.JS_FreeCString(ctx, str);
    }
}

fn registerPolyfills(ctx: *qjs.JSContext) void {
    // Register require FIRST - it creates _modules object that others depend on
    require_polyfill.register(ctx);
    console_polyfill.register(ctx);
    process_polyfill.register(ctx);
    buffer_polyfill.register(ctx);
    path_polyfill.register(ctx);
    util_polyfill.register(ctx);
    encoding_polyfill.register(ctx);
    querystring_polyfill.register(ctx);
    globals_polyfill.register(ctx);
    crypto_polyfill.register(ctx);
    assert_polyfill.register(ctx);
    string_decoder_polyfill.register(ctx);

    // Native-only polyfills (skipped on wasm32)
    if (dns_polyfill) |p| p.register(ctx);
    if (child_process_polyfill) |p| p.register(ctx);
    if (net_polyfill) |p| p.register(ctx);
    if (tls_polyfill) |p| p.register(ctx);
    if (compression_polyfill) |p| p.register(ctx);
    if (os_polyfill) |p| p.register(ctx);
    if (fs_polyfill) |p| p.register(ctx);
    if (tty_polyfill) |p| p.register(ctx);
}

pub fn main() !void {
    // Use page_allocator for WASM (GPA can have issues), GPA for native
    const allocator = if (is_wasm32) std.heap.page_allocator else gpa.allocator();

    // Initialize native bindings (native-only)
    if (native_bindings) |nb| nb.init(allocator);

    // Create QuickJS runtime
    const rt = qjs.JS_NewRuntime() orelse {
        std.debug.print("Failed to create QuickJS runtime\n", .{});
        return error.RuntimeCreationFailed;
    };
    defer qjs.JS_FreeRuntime(rt);

    // Set memory limit (4GB for native, 2GB for wasm32)
    const mem_limit: usize = if (is_wasm32) 2 * 1024 * 1024 * 1024 else 4 * 1024 * 1024 * 1024;
    qjs.JS_SetMemoryLimit(rt, mem_limit);

    // Create context
    const ctx = qjs.JS_NewContext(rt) orelse {
        std.debug.print("Failed to create QuickJS context\n", .{});
        return error.ContextCreationFailed;
    };
    defer {
        // Clear global module references to break reference cycles
        // Objects created via JS_Eval may hold globalThis references
        {
            const global = qjs.JS_GetGlobalObject(ctx);

            // List of all known globals that might hold circular references
            // This includes all globalThis properties set by polyfills
            const globals_to_clear = [_][*:0]const u8{
                // Core module system
                "_modules",
                "require",
                "module",
                "exports",
                // QuickJS std/os modules
                "std",
                "os",
                "_os",
                "scriptArgs",
                // Core globals
                "process",
                "console",
                "Buffer",
                "crypto",
                // Stream classes
                "Readable",
                "Writable",
                "Duplex",
                "Transform",
                "PassThrough",
                // URL/Fetch API
                "URL",
                "URLSearchParams",
                "fetch",
                "Headers",
                "Request",
                "Response",
                // Encoding
                "TextEncoder",
                "TextDecoder",
                // Timers
                "setTimeout",
                "setInterval",
                "setImmediate",
                "clearTimeout",
                "clearInterval",
                "clearImmediate",
                // TTY helpers
                "_tty_isatty",
                "_tty_getWindowSize",
                "__edgebox_isatty",
                "__edgebox_get_terminal_size",
                // Spawn helpers
                "__edgebox_spawn",
                "__edgebox_spawn_start",
                "__edgebox_spawn_poll",
                "__edgebox_spawn_output",
                // FS helpers
                "__edgebox_fetch",
                "__edgebox_fs_read",
                "__edgebox_fs_write",
                "__edgebox_fs_stat",
                "__edgebox_fs_lstat",
                "__edgebox_fs_mkdir",
                "__edgebox_fs_rmdir",
                "__edgebox_fs_readdir",
                "__edgebox_fs_unlink",
                "__edgebox_fs_rename",
                "__edgebox_fs_copy",
                "__edgebox_fs_exists",
                "__edgebox_fs_chmod",
                "__edgebox_fs_chown",
                "__edgebox_fs_link",
                "__edgebox_fs_symlink",
                "__edgebox_fs_readlink",
                "__edgebox_fs_truncate",
                "__edgebox_fs_utimes",
                "__edgebox_fs_append",
                "__edgebox_file_read_start",
                "__edgebox_file_write_start",
                "__edgebox_file_poll",
                "__edgebox_file_result",
                // Crypto helpers
                "__edgebox_hash",
                "__edgebox_hmac",
                // Stdin helper
                "__edgebox_read_stdin",
                // Worker helpers
                "__edgebox_worker_id",
                "__edgebox_worker_data",
                // Internal state
                "_dns",
                "_fdBuffers",
                "_fdFlags",
                "_fdPaths",
                "_nextPseudoFd",
                "_polyfillDebug",
                "_polyfillsInitialized",
                "_runtimePolyfillsInitialized",
                "__frozen_init_complete",
                // Runtime polyfill globals
                "__cliTrace",
                "__edgebox_intercept_tsc_factory",
                "__edgebox_tsc_intercepted",
                "_edgebox_debug",
                "_edgebox_errors",
                "_edgebox_reportError",
                "_edgeboxKeepalive",
                "_edgeboxStartKeepalive",
                "abort",
                "AbortController",
                "AbortSignal",
                "atob",
                "btoa",
                "ByteLengthQueuingStrategy",
                "CountQueuingStrategy",
                "CustomEvent",
                "DOMException",
                "Event",
                "EventTarget",
                "FormData",
                "HostArray",
                "HostMap",
                "Intl",
                "localStorage",
                "onunhandledrejection",
                "performance",
                "ReadableStream",
                "TransformStream",
                "WritableStream",
                "ts",
                "v9",
                "WebAssembly",
                // Math and path (core Node.js compatibility)
                "Math",
                "path",
                "print",
                // Microtask/clone APIs
                "queueMicrotask",
                "structuredClone",
                // Native shapes (optional debug)
                "__edgebox_register_node",
                "__edgebox_registry_count",
                "__edgebox_get_addr",
                "__edgebox_debug_last_lookup",
                // FS watch helpers
                "__edgebox_fs_watch",
                "__edgebox_fs_poll_watch",
                "__edgebox_fs_unwatch",
                // String decoder helpers
                "_string_decoder_write",
                "_string_decoder_end",
                // Native bindings (from native_bindings.zig and native_bindings.c)
                "__edgebox_cwd",
                "__edgebox_homedir",
                "__edgebox_random_bytes",
                "__edgebox_test42",
                // Child process spawn helpers (additional)
                "__edgebox_spawn_async",
                "__edgebox_poll_process",
                "__edgebox_kill_process",
                "__edgebox_cleanup_process",
                "__edgebox_socketpair",
                "__edgebox_ipc_write",
                "__edgebox_ipc_read",
                "__edgebox_close_fd",
                // Net socket helpers
                "__edgebox_socket_create",
                "__edgebox_socket_create_unix",
                "__edgebox_socket_connect",
                "__edgebox_socket_connect_unix",
                "__edgebox_socket_bind",
                "__edgebox_socket_bind_unix",
                "__edgebox_socket_listen",
                "__edgebox_socket_accept",
                "__edgebox_socket_read",
                "__edgebox_socket_write",
                "__edgebox_socket_close",
                "__edgebox_socket_state",
                "__edgebox_socket_set_nodelay",
                "__edgebox_socket_set_keepalive",
                "__edgebox_socket_pending_bytes",
                "__edgebox_socket_set_timeout",
                "__edgebox_socket_read_with_timeout",
                "__edgebox_socket_poll_writable",
                "__edgebox_socket_get_recv_buffer_size",
                "__edgebox_socket_get_send_buffer_size",
                "__edgebox_socket_set_recv_buffer_size",
                "__edgebox_socket_set_send_buffer_size",
                "__edgebox_socket_set_linger",
                // UDP socket helpers
                "__edgebox_udp_socket_create",
                "__edgebox_udp_socket_bind",
                "__edgebox_udp_socket_send",
                "__edgebox_udp_socket_recv",
                "__edgebox_udp_socket_close",
                "__edgebox_udp_set_broadcast",
                "__edgebox_udp_set_ttl",
                "__edgebox_udp_set_multicast_ttl",
                "__edgebox_udp_set_multicast_loopback",
                "__edgebox_udp_add_membership",
                "__edgebox_udp_drop_membership",
                "__edgebox_udp_set_multicast_interface",
                "__edgebox_udp_get_recv_buffer_size",
                "__edgebox_udp_get_send_buffer_size",
                "__edgebox_udp_set_recv_buffer_size",
                "__edgebox_udp_set_send_buffer_size",
                // TLS helpers
                "__edgebox_tls_connect",
                "__edgebox_tls_read",
                "__edgebox_tls_write",
                "__edgebox_tls_close",
                "__edgebox_tls_state",
                "__edgebox_tls_create_server",
                "__edgebox_tls_accept",
                "__edgebox_tls_destroy_server",
                "__edgebox_tls_get_cipher",
                "__edgebox_tls_get_protocol",
                "__edgebox_tls_get_peer_certificate",
                "__edgebox_tls_is_session_reused",
                "__edgebox_tls_get_session",
                // Node.js compatibility aliases
                "global",
                "sys",
                // Host stdlib classes
                "HostArray",
                "HostMap",
                "__edgebox_array_new",
                "__edgebox_array_push",
                "__edgebox_array_pop",
                "__edgebox_array_get",
                "__edgebox_array_set",
                "__edgebox_array_len",
                "__edgebox_array_sort",
                "__edgebox_array_sort_desc",
                "__edgebox_array_reverse",
                "__edgebox_array_clear",
                "__edgebox_array_index_of",
                "__edgebox_array_free",
                "__edgebox_map_new",
                "__edgebox_map_set",
                "__edgebox_map_get",
                "__edgebox_map_has",
                "__edgebox_map_delete",
                "__edgebox_map_len",
                "__edgebox_map_clear",
                "__edgebox_map_free",
            };

            // First, set _modules to empty object to break internal circular references
            // This is important because modules reference each other
            {
                const modules_atom = qjs.JS_NewAtom(ctx, "_modules");
                const empty_obj = qjs.JS_NewObject(ctx);
                _ = qjs.JS_SetProperty(ctx, global, modules_atom, empty_obj);
                qjs.JS_FreeAtom(ctx, modules_atom);
            }

            // Run GC to collect orphaned module objects
            qjs.JS_RunGC(rt);

            // Free std handlers BEFORE deleting globals - handlers may hold timer callbacks
            // that reference global functions. Freeing handlers first breaks these chains.
            qjs.js_std_free_handlers(rt);

            // Run GC again after freeing handlers
            qjs.JS_RunGC(rt);

            // Now delete all global properties
            for (globals_to_clear) |name| {
                const atom = qjs.JS_NewAtom(ctx, name);
                _ = qjs.JS_DeleteProperty(ctx, global, atom, 0);
                qjs.JS_FreeAtom(ctx, atom);
            }

            qjs.JS_FreeValue(ctx, global);
        }

        // Cleanup cached polyfill references before GC
        buffer_polyfill.cleanup();

        // Run GC multiple times BEFORE freeing context to collect cyclic references
        // and all context-owned objects properly
        qjs.JS_RunGC(rt);
        qjs.JS_RunGC(rt);
        qjs.JS_RunGC(rt);
        qjs.JS_RunGC(rt);
        qjs.JS_RunGC(rt);
        qjs.JS_FreeContext(ctx);
    }

    // Initialize std module
    qjs.js_std_init_handlers(rt);
    qjs.JS_SetModuleLoaderFunc(rt, null, qjs.js_module_loader, null);
    _ = qjs.js_init_module_std(ctx, "std");
    _ = qjs.js_init_module_os(ctx, "os");

    // Expose QuickJS std and os modules as global objects for polyfills
    // This is needed because the bundled code can't use ES module imports
    {
        const global = qjs.JS_GetGlobalObject(ctx);
        defer qjs.JS_FreeValue(ctx, global);

        // Execute code to import modules and expose them globally
        const init_code =
            \\import * as std from 'std';
            \\import * as os from 'os';
            \\globalThis.std = std;
            \\globalThis.os = os;
            \\globalThis._os = os;
        ;
        const init_result = qjs.JS_Eval(ctx, init_code, init_code.len, "<init>", qjs.JS_EVAL_TYPE_MODULE);
        if (qjs.JS_IsException(init_result)) {
            // Print but don't fail - some builds may not have these modules
            printException(ctx);
        }
        qjs.JS_FreeValue(ctx, init_result);
    }

    // Register native polyfills
    registerPolyfills(ctx);

    // Set process.argv from command-line arguments
    {
        const args = std.process.argsAlloc(allocator) catch &[_][:0]const u8{};
        defer if (args.len > 0) allocator.free(args);
        // Skip first arg (executable path), and skip "--" separator if present
        var script_args_start: usize = 1;
        if (args.len > 1 and std.mem.eql(u8, args[1], "--")) {
            script_args_start = 2; // Skip the "--"
        }
        if (args.len > script_args_start) {
            process_polyfill.setArgv(ctx, args[script_args_start..]);
        } else {
            process_polyfill.setArgv(ctx, &[_][:0]const u8{});
        }

        // Set scriptArgs for QuickJS std module compatibility
        const global = qjs.JS_GetGlobalObject(ctx);
        defer qjs.JS_FreeValue(ctx, global);

        const script_args_array = qjs.JS_NewArray(ctx);
        const script_args_slice = if (args.len > script_args_start) args[script_args_start..] else &[_][:0]const u8{};
        for (script_args_slice, 0..) |arg, i| {
            _ = qjs.JS_SetPropertyUint32(ctx, script_args_array, @intCast(i), qjs.JS_NewString(ctx, arg.ptr));
        }
        _ = qjs.JS_SetPropertyStr(ctx, global, "scriptArgs", script_args_array);
    }

    // Set cwd to actual current working directory (native-only)
    if (native_bindings) |nb| {
        if (std.fs.cwd().realpathAlloc(allocator, ".")) |cwd_path| {
            nb.setCwd(cwd_path);
        } else |_| {
            // Fallback to "/" if cwd() fails
        }
        // Register native bindings (fs, crypto, fast_transpile, etc.)
        nb.registerAll(ctx);
    }

    // Register frozen functions BEFORE executing bytecode
    _ = frozen_module.frozen_init_c(@ptrCast(ctx));

    // Set __frozen_init_complete flag
    {
        const global = qjs.JS_GetGlobalObject(ctx);
        defer qjs.JS_FreeValue(ctx, global);
        _ = qjs.JS_SetPropertyStr(ctx, global, "__frozen_init_complete", jsTrue());
    }

    // Load bytecode
    const obj = qjs.JS_ReadObject(ctx, bytecode.ptr, bytecode.len, qjs.JS_READ_OBJ_BYTECODE);
    if (qjs.JS_IsException(obj)) {
        printException(ctx);
        return error.BytecodeLoadFailed;
    }

    // Execute
    const tag = qjs.JS_VALUE_GET_TAG(obj);
    var result: qjs.JSValue = undefined;

    if (tag == -3) { // JS_TAG_MODULE
        if (qjs.JS_ResolveModule(ctx, obj) < 0) {
            qjs.JS_FreeValue(ctx, obj);
            printException(ctx);
            return error.ModuleResolveFailed;
        }
        if (qjs.js_module_set_import_meta(ctx, obj, false, true) < 0) {
            printException(ctx);
            return error.ModuleMetaFailed;
        }
        result = qjs.JS_EvalFunction(ctx, obj);
        _ = qjs.js_std_loop(ctx);
    } else {
        result = qjs.JS_EvalFunction(ctx, obj);
        _ = qjs.js_std_loop(ctx);
    }

    if (qjs.JS_IsException(result)) {
        printException(ctx);
        qjs.JS_FreeValue(ctx, result);
        return error.ExecutionFailed;
    }

    qjs.JS_FreeValue(ctx, result);
}
