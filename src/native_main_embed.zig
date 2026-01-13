/// Native Entry Point with Embedded Bytecode
/// Uses @embedFile for raw bytecode - no C file generation
/// Solves OOM from parsing 321MB C hex arrays
const std = @import("std");
const builtin = @import("builtin");

// QuickJS C API
const qjs = @cImport({
    @cDefine("CONFIG_VERSION", "\"2024-02-14\"");
    @cDefine("CONFIG_BIGNUM", "1");
    @cInclude("quickjs.h");
    @cInclude("quickjs-libc.h");
});

// Zig 0.15 compatible JS_TRUE (cImport version uses comptime union init which doesn't work)
// Native build always uses 128-bit struct format
inline fn jsTrue() qjs.JSValue {
    var v: qjs.JSValue = undefined;
    v.u.int32 = 1;
    v.tag = 1; // JS_TAG_BOOL
    return v;
}

// Native polyfills
const path_polyfill = @import("polyfills/path.zig");
const process_polyfill = @import("polyfills/process.zig");
const console_polyfill = @import("polyfills/console.zig");
const buffer_polyfill = @import("polyfills/buffer.zig");
const util_polyfill = @import("polyfills/util.zig");
const encoding_polyfill = @import("polyfills/encoding.zig");
const native_bindings = @import("native_bindings.zig");

// Frozen module (generated Zig frozen functions)
const frozen_module = @import("frozen_module");
comptime {
    _ = &frozen_module.frozen_init_c;
}

// Zig hot paths
const zig_hotpaths = @import("zig_hotpaths");
comptime {
    _ = &zig_hotpaths;
}

// Embed raw bytecode - provided via anonymous import from build.zig
const bytecode_module = @import("bytecode");
const bytecode: []const u8 = bytecode_module.data;

// Global allocator
var gpa = std.heap.GeneralPurposeAllocator(.{}){};

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

    // Initialize std module (for console, etc)
    qjs.js_std_init_handlers(rt);
    qjs.JS_SetModuleLoaderFunc(rt, null, qjs.js_module_loader, null);
    _ = qjs.js_init_module_std(ctx, "std");
    _ = qjs.js_init_module_os(ctx, "os");

    // Register native polyfills
    registerPolyfills(ctx, allocator);

    // Register native bindings (fs, crypto, fast_transpile, etc.)
    native_bindings.registerAll(ctx);

    // Register frozen functions BEFORE executing bytecode
    // Cast needed because frozen_module uses zig_runtime.JSContext opaque type
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

fn registerPolyfills(ctx: *qjs.JSContext, allocator: std.mem.Allocator) void {
    _ = allocator; // Polyfills no longer need allocator

    // All polyfills now take only ctx and get global internally
    // Cast needed because polyfills use different cImport (distinct opaque types)
    console_polyfill.register(@ptrCast(ctx));
    process_polyfill.register(@ptrCast(ctx));
    buffer_polyfill.register(@ptrCast(ctx));
    path_polyfill.register(@ptrCast(ctx));
    util_polyfill.register(@ptrCast(ctx));
    encoding_polyfill.register(@ptrCast(ctx));
}

fn printException(ctx: *qjs.JSContext) void {
    const exc = qjs.JS_GetException(ctx);
    defer qjs.JS_FreeValue(ctx, exc);

    const str = qjs.JS_ToCString(ctx, exc);
    if (str != null) {
        std.debug.print("Exception: {s}\n", .{str});
        qjs.JS_FreeCString(ctx, str);
    }

    // Print stack trace if available
    const stack = qjs.JS_GetPropertyStr(ctx, exc, "stack");
    if (!qjs.JS_IsUndefined(stack)) {
        const stack_str = qjs.JS_ToCString(ctx, stack);
        if (stack_str != null) {
            std.debug.print("Stack: {s}\n", .{stack_str});
            qjs.JS_FreeCString(ctx, stack_str);
        }
    }
    qjs.JS_FreeValue(ctx, stack);
}
