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

// QuickJS C API
const qjs = @cImport({
    @cDefine("CONFIG_VERSION", "\"2024-02-14\"");
    @cDefine("CONFIG_BIGNUM", "1");
    @cInclude("quickjs.h");
    @cInclude("quickjs-libc.h");
});

// Native polyfills (for console, process, etc.)
const path_polyfill = @import("polyfills/path.zig");
const process_polyfill = @import("polyfills/process.zig");
const console_polyfill = @import("polyfills/console.zig");
const buffer_polyfill = @import("polyfills/buffer.zig");
const util_polyfill = @import("polyfills/util.zig");
const encoding_polyfill = @import("polyfills/encoding.zig");

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

fn registerPolyfills(ctx: *qjs.JSContext, allocator: std.mem.Allocator) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    path_polyfill.register(ctx, global, allocator);
    process_polyfill.register(ctx, global, allocator);
    console_polyfill.register(ctx, global);
    buffer_polyfill.register(ctx, global, allocator);
    util_polyfill.register(ctx, global, allocator);
    encoding_polyfill.register(ctx, global, allocator);
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
    registerPolyfills(ctx, allocator);

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
