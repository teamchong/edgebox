/// Native Static Entry Point
/// Executes pre-compiled bytecode with freeze optimizations
/// For pure native binaries (no WAMR)
///
/// Build pipeline:
///   1. edgebox-compile compiles JS → bundle.bin
///   2. edgebox-compile generates frozen_module.zig
///   3. zig build native-static embeds bytecode and compiles to native binary
///
const std = @import("std");
const builtin = @import("builtin");

// QuickJS C API - use shared import for type compatibility with polyfills
const quickjs = @import("quickjs_core.zig");
const qjs = quickjs.c;

// Native polyfills (for console, process, etc.)
const path_polyfill = @import("polyfills/path.zig");
const process_polyfill = @import("polyfills/process.zig");
const console_polyfill = @import("polyfills/console.zig");
const buffer_polyfill = @import("polyfills/buffer.zig");
const util_polyfill = @import("polyfills/util.zig");
const encoding_polyfill = @import("polyfills/encoding.zig");
const os_polyfill = @import("polyfills/os.zig");
const fs_polyfill = @import("polyfills/fs.zig");
const crypto_polyfill = @import("polyfills/crypto.zig");
const tty_polyfill = @import("polyfills/tty.zig");
const assert_polyfill = @import("polyfills/assert.zig");
const dns_polyfill = @import("polyfills/dns.zig");
const child_process_polyfill = @import("polyfills/child_process.zig");
const net_polyfill = @import("polyfills/net.zig");
const tls_polyfill = @import("polyfills/tls.zig");
const compression_polyfill = @import("polyfills/compression.zig");
const require_polyfill = @import("polyfills/require.zig");
const querystring_polyfill = @import("polyfills/querystring.zig");
const globals_polyfill = @import("polyfills/globals.zig");
const string_decoder_polyfill = @import("polyfills/string_decoder.zig");

// Zig native registry (replaces C frozen_runtime.c registry)
const native_shapes_registry = @import("freeze/native_shapes.zig");
comptime {
    _ = native_shapes_registry.native_registry_init;
    _ = native_shapes_registry.native_node_register;
    _ = native_shapes_registry.native_node_lookup;
}

// Native bindings for fs, crypto, etc.
const native_bindings = @import("native_bindings.zig");

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

// Zig 0.15 compatible JS_TRUE
inline fn jsTrue() qjs.JSValue {
    var v: qjs.JSValue = undefined;
    v.u.int32 = 1;
    v.tag = 1; // JS_TAG_BOOL
    return v;
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
    dns_polyfill.register(ctx);
    child_process_polyfill.register(ctx);
    net_polyfill.register(ctx);
    tls_polyfill.register(ctx);
    querystring_polyfill.register(ctx);
    compression_polyfill.register(ctx);
    globals_polyfill.register(ctx);
    os_polyfill.register(ctx);
    fs_polyfill.register(ctx);
    crypto_polyfill.register(ctx);
    tty_polyfill.register(ctx);
    assert_polyfill.register(ctx);
    string_decoder_polyfill.register(ctx);
}

pub fn main() !void {
    const allocator = gpa.allocator();

    // Initialize native bindings
    native_bindings.init(allocator);

    // Create QuickJS runtime
    const rt = qjs.JS_NewRuntime() orelse {
        std.debug.print("Failed to create QuickJS runtime\n", .{});
        return error.RuntimeCreationFailed;
    };
    defer qjs.JS_FreeRuntime(rt);

    // Set memory limit (4GB)
    qjs.JS_SetMemoryLimit(rt, 4 * 1024 * 1024 * 1024);

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

        // Free std handlers before context (releases module-level references)
        qjs.js_std_free_handlers(rt);
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

    // Set cwd to actual current working directory
    if (std.fs.cwd().realpathAlloc(allocator, ".")) |cwd_path| {
        native_bindings.setCwd(cwd_path);
    } else |_| {
        // Fallback to "/" if cwd() fails
    }

    // Register native bindings (fs, crypto, fast_transpile, etc.)
    native_bindings.registerAll(ctx);

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
