/// Native Buffer module - QuickJS C functions
/// Implements Node.js Buffer API on top of Uint8Array
/// Zero allocation for most operations
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

/// Buffer.from(array) - Create buffer from array or string
fn bufferFrom(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "Buffer.from requires at least 1 argument");

    // Check if it's a string
    if (qjs.JS_IsString(argv[0])) {
        var len: usize = undefined;
        const str = qjs.JS_ToCStringLen(ctx, &len, argv[0]);
        if (str == null) return qjs.JS_EXCEPTION;
        defer qjs.JS_FreeCString(ctx, str);

        // Create Uint8Array from string bytes
        const global = qjs.JS_GetGlobalObject(ctx);
        defer qjs.JS_FreeValue(ctx, global);

        const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
        defer qjs.JS_FreeValue(ctx, uint8array_ctor);

        // Create new Uint8Array(length)
        var ctor_args = [1]qjs.JSValue{qjs.JS_NewInt32(ctx, @intCast(len))};
        const arr = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
        if (qjs.JS_IsException(arr)) return arr;

        // Copy string bytes to array
        for (0..len) |i| {
            const byte_val = qjs.JS_NewInt32(ctx, @intCast(str[i]));
            _ = qjs.JS_SetPropertyUint32(ctx, arr, @intCast(i), byte_val);
        }

        return arr;
    }

    // For arrays/typed arrays, just wrap in Uint8Array
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    var ctor_args = [1]qjs.JSValue{argv[0]};
    return qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
}

/// Buffer.alloc(size) - Create zero-filled buffer
fn bufferAlloc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "Buffer.alloc requires size argument");

    var size: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &size, argv[0]) != 0) return qjs.JS_EXCEPTION;
    if (size < 0) return qjs.JS_ThrowRangeError(ctx, "size must be non-negative");

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    var ctor_args = [1]qjs.JSValue{qjs.JS_NewInt32(ctx, size)};
    return qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
}

/// Buffer.allocUnsafe(size) - Create uninitialized buffer (faster)
fn bufferAllocUnsafe(ctx: ?*qjs.JSContext, this: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    // Same as alloc for now (QuickJS zeros memory anyway)
    return bufferAlloc(ctx, this, argc, argv);
}

/// Buffer.concat(list, totalLength) - Concatenate buffers
fn bufferConcat(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "Buffer.concat requires list argument");

    // Get array length
    const len_val = qjs.JS_GetPropertyStr(ctx, argv[0], "length");
    defer qjs.JS_FreeValue(ctx, len_val);

    var arr_len: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &arr_len, len_val) != 0) return qjs.JS_EXCEPTION;

    // Calculate total length
    var total_len: usize = 0;
    for (0..@intCast(arr_len)) |i| {
        const buf = qjs.JS_GetPropertyUint32(ctx, argv[0], @intCast(i));
        defer qjs.JS_FreeValue(ctx, buf);

        const buf_len_val = qjs.JS_GetPropertyStr(ctx, buf, "length");
        defer qjs.JS_FreeValue(ctx, buf_len_val);

        var buf_len: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &buf_len, buf_len_val);
        total_len += @intCast(buf_len);
    }

    // Create result buffer
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    var ctor_args = [1]qjs.JSValue{qjs.JS_NewInt32(ctx, @intCast(total_len))};
    const result = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    if (qjs.JS_IsException(result)) return result;

    // Copy all buffers into result
    var offset: usize = 0;
    for (0..@intCast(arr_len)) |i| {
        const buf = qjs.JS_GetPropertyUint32(ctx, argv[0], @intCast(i));
        defer qjs.JS_FreeValue(ctx, buf);

        const buf_len_val = qjs.JS_GetPropertyStr(ctx, buf, "length");
        defer qjs.JS_FreeValue(ctx, buf_len_val);

        var buf_len: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &buf_len, buf_len_val);

        // Copy bytes
        for (0..@intCast(buf_len)) |j| {
            const byte = qjs.JS_GetPropertyUint32(ctx, buf, @intCast(j));
            _ = qjs.JS_SetPropertyUint32(ctx, result, @intCast(offset + j), byte);
        }
        offset += @intCast(buf_len);
    }

    return result;
}

/// Buffer.isBuffer(obj) - Check if object is a buffer
fn bufferIsBuffer(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewBool(ctx, false);

    // Check if it's a Uint8Array
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], uint8array_ctor);
    return qjs.JS_NewBool(ctx, result == 1);
}

/// Register Buffer as a global
pub fn register(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Get Uint8Array constructor to extend
    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    // Create Buffer object (will be a constructor function)
    // For now, just create an object with static methods
    const buffer_obj = qjs.JS_NewObject(ctx);

    // Register static methods
    inline for (.{
        .{ "from", bufferFrom, 2 },
        .{ "alloc", bufferAlloc, 1 },
        .{ "allocUnsafe", bufferAllocUnsafe, 1 },
        .{ "concat", bufferConcat, 2 },
        .{ "isBuffer", bufferIsBuffer, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, buffer_obj, binding[0], func);
    }

    // Set as global.Buffer
    _ = qjs.JS_SetPropertyStr(ctx, global, "Buffer", buffer_obj);
}
