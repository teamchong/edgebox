/// Native util module - QuickJS C functions
/// Utility functions for Node.js compatibility
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

/// util.format(format, ...args) - Simple string formatting
fn utilFormat(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    var buffer: [4096]u8 = undefined;
    var pos: usize = 0;

    // For now, just concatenate all arguments with spaces
    for (0..@intCast(argc)) |i| {
        if (i > 0 and pos < buffer.len) {
            buffer[pos] = ' ';
            pos += 1;
        }

        const str = qjs.JS_ToCString(ctx, argv[i]);
        if (str != null) {
            defer qjs.JS_FreeCString(ctx, str);
            const text = std.mem.span(str);
            const len = @min(text.len, buffer.len - pos);
            @memcpy(buffer[pos..][0..len], text[0..len]);
            pos += len;
        }
    }

    return qjs.JS_NewStringLen(ctx, &buffer, @intCast(pos));
}

/// util.inspect(obj) - Return string representation
fn utilInspect(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "undefined");

    // Use JSON.stringify as a simple inspection method
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const json_obj = qjs.JS_GetPropertyStr(ctx, global, "JSON");
    defer qjs.JS_FreeValue(ctx, json_obj);

    const stringify_func = qjs.JS_GetPropertyStr(ctx, json_obj, "stringify");
    defer qjs.JS_FreeValue(ctx, stringify_func);

    var stringify_args = [1]qjs.JSValue{argv[0]};
    const result = qjs.JS_Call(ctx, stringify_func, json_obj, 1, &stringify_args);

    // If stringify fails, fall back to toString
    if (qjs.JS_IsException(result)) {
        qjs.JS_FreeValue(ctx, result);
        return qjs.JS_ToString(ctx, argv[0]);
    }

    return result;
}

/// util.deprecate(fn, msg) - Return function as-is (no-op for now)
fn utilDeprecate(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_UNDEFINED;
    return qjs.JS_DupValue(ctx, argv[0]);
}

/// util.inherits(ctor, superCtor) - Set up prototype chain
fn utilInherits(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_UNDEFINED;

    // Get Object.setPrototypeOf
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const object_ctor = qjs.JS_GetPropertyStr(ctx, global, "Object");
    defer qjs.JS_FreeValue(ctx, object_ctor);

    const set_proto_func = qjs.JS_GetPropertyStr(ctx, object_ctor, "setPrototypeOf");
    defer qjs.JS_FreeValue(ctx, set_proto_func);

    // Get ctor.prototype
    const ctor_proto = qjs.JS_GetPropertyStr(ctx, argv[0], "prototype");
    defer qjs.JS_FreeValue(ctx, ctor_proto);

    // Get superCtor.prototype
    const super_proto = qjs.JS_GetPropertyStr(ctx, argv[1], "prototype");
    defer qjs.JS_FreeValue(ctx, super_proto);

    // Object.setPrototypeOf(ctor.prototype, superCtor.prototype)
    var set_proto_args = [2]qjs.JSValue{ ctor_proto, super_proto };
    const result = qjs.JS_Call(ctx, set_proto_func, object_ctor, 2, &set_proto_args);
    qjs.JS_FreeValue(ctx, result);

    return qjs.JS_UNDEFINED;
}

/// util.isArray(obj) - Check if object is an array
fn utilIsArray(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    const is_array = qjs.JS_IsArray(argv[0]);
    if (is_array) {
        return qjs.JS_TRUE;
    }
    return qjs.JS_FALSE;
}

/// util.isBuffer(obj) - Check if object is a Buffer
fn utilIsBuffer(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;

    // Check if it's a Uint8Array
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], uint8array_ctor);
    if (result == 1) {
        return qjs.JS_TRUE;
    }
    return qjs.JS_FALSE;
}

/// Register util module
pub fn register(ctx: *qjs.JSContext) void {
    const util_obj = qjs.JS_NewObject(ctx);

    // Register functions
    inline for (.{
        .{ "format", utilFormat, -1 },
        .{ "inspect", utilInspect, 1 },
        .{ "deprecate", utilDeprecate, 2 },
        .{ "inherits", utilInherits, 2 },
        .{ "isArray", utilIsArray, 1 },
        .{ "isBuffer", utilIsBuffer, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, util_obj, binding[0], func);
    }

    // Add TextDecoder and TextEncoder references
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const text_decoder = qjs.JS_GetPropertyStr(ctx, global, "TextDecoder");
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "TextDecoder", text_decoder);

    const text_encoder = qjs.JS_GetPropertyStr(ctx, global, "TextEncoder");
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "TextEncoder", text_encoder);

    // Set in _modules for require('util')
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "util", util_obj);
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, util_obj);
    }
}
