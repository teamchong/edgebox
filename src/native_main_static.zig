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
    defer qjs.JS_FreeContext(ctx);

    // Initialize std module
    qjs.js_std_init_handlers(rt);
    qjs.JS_SetModuleLoaderFunc(rt, null, qjs.js_module_loader, null);
    _ = qjs.js_init_module_std(ctx, "std");
    _ = qjs.js_init_module_os(ctx, "os");

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
