/// Native compression module - QuickJS C functions
/// Redirects to host-implemented compression functions via __edgebox_* globals.
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

/// Helper to call a global function by name with the given arguments
fn callGlobalFunction(ctx: ?*qjs.JSContext, func_name: [*:0]const u8, argc: c_int, argv: [*c]qjs.JSValue) qjs.JSValue {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const func = qjs.JS_GetPropertyStr(ctx, global, func_name);
    defer qjs.JS_FreeValue(ctx, func);

    if (qjs.JS_IsUndefined(func) or !qjs.JS_IsFunction(ctx, func)) {
        return qjs.JS_ThrowTypeError(ctx, "compression host function not available");
    }

    return qjs.JS_Call(ctx, func, global, argc, argv);
}

/// gzip(data) - Compress data using gzip
fn gzipFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return callGlobalFunction(ctx, "__edgebox_gzip", argc, argv);
}

/// gunzip(data) - Decompress gzip data
fn gunzipFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return callGlobalFunction(ctx, "__edgebox_gunzip", argc, argv);
}

/// deflate(data) - Compress data using deflate
fn deflateFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return callGlobalFunction(ctx, "__edgebox_deflate", argc, argv);
}

/// inflate(data) - Decompress deflate data
fn inflateFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return callGlobalFunction(ctx, "__edgebox_inflate", argc, argv);
}

/// inflateZlib(data) - Decompress zlib data
/// Uses inflate since zlib format is handled by the host function
fn inflateZlibFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return callGlobalFunction(ctx, "__edgebox_inflate", argc, argv);
}

/// Register compression module
pub fn register(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Create compression module
    const comp_obj = qjs.JS_NewObject(ctx);

    // Register functions
    inline for (.{
        .{ "gzip", gzipFunc, 1 },
        .{ "gunzip", gunzipFunc, 1 },
        .{ "deflate", deflateFunc, 1 },
        .{ "inflate", inflateFunc, 1 },
        .{ "inflateZlib", inflateZlibFunc, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, comp_obj, binding[0], func);
    }

    // Set in _modules for require('compression')
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "compression", comp_obj);
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, comp_obj);
    }
}
