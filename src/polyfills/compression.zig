/// Native compression module - QuickJS C functions
/// Gzip, Deflate, Inflate stubs (TODO: implement with Zig std.compress or external library)
///
/// NOTE: Zig 0.15.2's std.compress API is incomplete for wasm32-wasi target.
/// For now, these functions throw "not implemented" errors.
/// Compression should remain as host functions until we can implement them properly.
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

/// gzip(data) - Compress data using gzip (stub)
fn gzipFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return qjs.JS_ThrowTypeError(ctx, "gzip() not yet implemented in sandbox - use host function");
}

/// gunzip(data) - Decompress gzip data (stub)
fn gunzipFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return qjs.JS_ThrowTypeError(ctx, "gunzip() not yet implemented in sandbox - use host function");
}

/// deflate(data) - Compress data using deflate (stub)
fn deflateFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return qjs.JS_ThrowTypeError(ctx, "deflate() not yet implemented in sandbox - use host function");
}

/// inflate(data) - Decompress deflate data (stub)
fn inflateFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return qjs.JS_ThrowTypeError(ctx, "inflate() not yet implemented in sandbox - use host function");
}

/// inflateZlib(data) - Decompress zlib data (stub)
fn inflateZlibFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return qjs.JS_ThrowTypeError(ctx, "inflateZlib() not yet implemented in sandbox - use host function");
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
