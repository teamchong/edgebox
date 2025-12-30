/// Native compression module - QuickJS C functions
/// Redirects to host-implemented compression functions via __edgebox_* globals.
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// Cached function references (avoids JS_GetGlobalObject + JS_GetPropertyStr per call)
var cached_gzip: qjs.JSValue = qjs.JS_UNDEFINED;
var cached_gunzip: qjs.JSValue = qjs.JS_UNDEFINED;
var cached_deflate: qjs.JSValue = qjs.JS_UNDEFINED;
var cached_inflate: qjs.JSValue = qjs.JS_UNDEFINED;
var cached_global: qjs.JSValue = qjs.JS_UNDEFINED;

/// Call cached function (fast path - no property lookup)
fn callCachedFunction(ctx: ?*qjs.JSContext, func: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) qjs.JSValue {
    if (qjs.JS_IsUndefined(func)) {
        return qjs.JS_ThrowTypeError(ctx, "compression host function not available");
    }
    return qjs.JS_Call(ctx, func, cached_global, argc, argv);
}

/// gzip(data) - Compress data using gzip
fn gzipFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return callCachedFunction(ctx, cached_gzip, argc, argv);
}

/// gunzip(data) - Decompress gzip data
fn gunzipFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return callCachedFunction(ctx, cached_gunzip, argc, argv);
}

/// deflate(data) - Compress data using deflate
fn deflateFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return callCachedFunction(ctx, cached_deflate, argc, argv);
}

/// inflate(data) - Decompress deflate data
fn inflateFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return callCachedFunction(ctx, cached_inflate, argc, argv);
}

/// inflateZlib(data) - Decompress zlib data (uses inflate - same format handled by host)
fn inflateZlibFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return callCachedFunction(ctx, cached_inflate, argc, argv);
}

/// Register compression module
pub fn register(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);

    // Cache global and host functions for fast access (no per-call property lookup)
    cached_global = qjs.JS_DupValue(ctx, global);
    cached_gzip = qjs.JS_GetPropertyStr(ctx, global, "__edgebox_gzip");
    cached_gunzip = qjs.JS_GetPropertyStr(ctx, global, "__edgebox_gunzip");
    cached_deflate = qjs.JS_GetPropertyStr(ctx, global, "__edgebox_deflate");
    cached_inflate = qjs.JS_GetPropertyStr(ctx, global, "__edgebox_inflate");

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

    qjs.JS_FreeValue(ctx, global);
}
