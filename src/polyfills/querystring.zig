/// Native querystring module - QuickJS C functions
/// Parse and stringify URL query strings
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

var parse_buffer: [4096]u8 = undefined;

/// querystring.parse(str) - Parse query string into object
fn qsParse(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewObject(ctx);

    const str = qjs.JS_ToCString(ctx, argv[0]);
    if (str == null) return qjs.JS_NewObject(ctx);
    defer qjs.JS_FreeCString(ctx, str);

    const query = std.mem.span(str);
    const result = qjs.JS_NewObject(ctx);

    var it = std.mem.splitScalar(u8, query, '&');
    while (it.next()) |pair| {
        if (pair.len == 0) continue;

        const eq_idx = std.mem.indexOfScalar(u8, pair, '=') orelse {
            // No =, treat as key with empty value
            const empty_val = qjs.JS_NewString(ctx, "");
            const key_cstr = qjs.JS_NewStringLen(ctx, pair.ptr, @intCast(pair.len));
            const key_str = qjs.JS_ToCString(ctx, key_cstr);
            if (key_str != null) {
                defer qjs.JS_FreeCString(ctx, key_str);
                _ = qjs.JS_SetPropertyStr(ctx, result, key_str, empty_val);
            }
            qjs.JS_FreeValue(ctx, key_cstr);
            continue;
        };

        const key = pair[0..eq_idx];
        const value = pair[eq_idx + 1 ..];

        const key_jsval = qjs.JS_NewStringLen(ctx, key.ptr, @intCast(key.len));
        const value_jsval = qjs.JS_NewStringLen(ctx, value.ptr, @intCast(value.len));

        // Decode key and value using decodeURIComponent
        const global = qjs.JS_GetGlobalObject(ctx);
        const decode_func = qjs.JS_GetPropertyStr(ctx, global, "decodeURIComponent");

        var decode_args = [1]qjs.JSValue{value_jsval};
        const decoded_val = qjs.JS_Call(ctx, decode_func, global, 1, &decode_args);
        qjs.JS_FreeValue(ctx, value_jsval);
        qjs.JS_FreeValue(ctx, decode_func);
        qjs.JS_FreeValue(ctx, global);

        const key_str = qjs.JS_ToCString(ctx, key_jsval);
        if (key_str != null) {
            defer qjs.JS_FreeCString(ctx, key_str);
            // Use decoded value (or original if decode failed)
            if (qjs.JS_IsException(decoded_val)) {
                qjs.JS_FreeValue(ctx, decoded_val);
                _ = qjs.JS_SetPropertyStr(ctx, result, key_str, qjs.JS_NewStringLen(ctx, value.ptr, @intCast(value.len)));
            } else {
                _ = qjs.JS_SetPropertyStr(ctx, result, key_str, decoded_val);
            }
        } else {
            qjs.JS_FreeValue(ctx, decoded_val);
        }
        qjs.JS_FreeValue(ctx, key_jsval);
    }

    return result;
}

/// querystring.stringify(obj) - Stringify object into query string
fn qsStringify(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    var pos: usize = 0;
    var first = true;

    // Use JS Object.keys to iterate
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const object_ctor = qjs.JS_GetPropertyStr(ctx, global, "Object");
    defer qjs.JS_FreeValue(ctx, object_ctor);

    const keys_func = qjs.JS_GetPropertyStr(ctx, object_ctor, "keys");
    defer qjs.JS_FreeValue(ctx, keys_func);

    var keys_args = [1]qjs.JSValue{argv[0]};
    const keys_array = qjs.JS_Call(ctx, keys_func, object_ctor, 1, &keys_args);
    if (qjs.JS_IsException(keys_array)) return qjs.JS_NewString(ctx, "");
    defer qjs.JS_FreeValue(ctx, keys_array);

    const len_val = qjs.JS_GetPropertyStr(ctx, keys_array, "length");
    defer qjs.JS_FreeValue(ctx, len_val);

    var len: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &len, len_val);

    for (0..@intCast(len)) |i| {
        const key_val = qjs.JS_GetPropertyUint32(ctx, keys_array, @intCast(i));
        defer qjs.JS_FreeValue(ctx, key_val);

        const key_str = qjs.JS_ToCString(ctx, key_val);
        if (key_str == null) continue;
        defer qjs.JS_FreeCString(ctx, key_str);

        const value = qjs.JS_GetPropertyStr(ctx, argv[0], key_str);
        defer qjs.JS_FreeValue(ctx, value);

        const key_slice = std.mem.span(key_str);

        // Check if value is an array
        if (qjs.JS_IsArray(value)) {
            const arr_len_val = qjs.JS_GetPropertyStr(ctx, value, "length");
            defer qjs.JS_FreeValue(ctx, arr_len_val);
            var arr_len: i32 = 0;
            _ = qjs.JS_ToInt32(ctx, &arr_len, arr_len_val);

            for (0..@intCast(arr_len)) |j| {
                const arr_item = qjs.JS_GetPropertyUint32(ctx, value, @intCast(j));
                defer qjs.JS_FreeValue(ctx, arr_item);

                // Add separator
                if (!first and pos < parse_buffer.len) {
                    parse_buffer[pos] = '&';
                    pos += 1;
                }
                first = false;

                // Add key
                const key_len = @min(key_slice.len, parse_buffer.len - pos);
                @memcpy(parse_buffer[pos..][0..key_len], key_slice[0..key_len]);
                pos += key_len;

                // Add =
                if (pos < parse_buffer.len) {
                    parse_buffer[pos] = '=';
                    pos += 1;
                }

                // Add array item value
                const item_str = qjs.JS_ToCString(ctx, arr_item);
                if (item_str != null) {
                    defer qjs.JS_FreeCString(ctx, item_str);
                    const item_slice = std.mem.span(item_str);
                    const item_len = @min(item_slice.len, parse_buffer.len - pos);
                    @memcpy(parse_buffer[pos..][0..item_len], item_slice[0..item_len]);
                    pos += item_len;
                }
            }
        } else {
            // Non-array value
            // Add separator
            if (!first and pos < parse_buffer.len) {
                parse_buffer[pos] = '&';
                pos += 1;
            }
            first = false;

            // Add key
            const key_len = @min(key_slice.len, parse_buffer.len - pos);
            @memcpy(parse_buffer[pos..][0..key_len], key_slice[0..key_len]);
            pos += key_len;

            // Add =
            if (pos < parse_buffer.len) {
                parse_buffer[pos] = '=';
                pos += 1;
            }

            // Add value
            const value_str = qjs.JS_ToCString(ctx, value);
            if (value_str != null) {
                defer qjs.JS_FreeCString(ctx, value_str);
                const value_slice = std.mem.span(value_str);
                const value_len = @min(value_slice.len, parse_buffer.len - pos);
                @memcpy(parse_buffer[pos..][0..value_len], value_slice[0..value_len]);
                pos += value_len;
            }
        }
    }

    return qjs.JS_NewStringLen(ctx, &parse_buffer, @intCast(pos));
}

/// querystring.escape(str) - Percent-encode string
fn qsEscape(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    // Use encodeURIComponent
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const encode_func = qjs.JS_GetPropertyStr(ctx, global, "encodeURIComponent");
    defer qjs.JS_FreeValue(ctx, encode_func);

    var encode_args = [1]qjs.JSValue{argv[0]};
    return qjs.JS_Call(ctx, encode_func, global, 1, &encode_args);
}

/// querystring.unescape(str) - Decode percent-encoded string
fn qsUnescape(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    // Use decodeURIComponent
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const decode_func = qjs.JS_GetPropertyStr(ctx, global, "decodeURIComponent");
    defer qjs.JS_FreeValue(ctx, decode_func);

    var decode_args = [1]qjs.JSValue{argv[0]};
    return qjs.JS_Call(ctx, decode_func, global, 1, &decode_args);
}

/// Register querystring module
pub fn register(ctx: *qjs.JSContext) void {
    const qs_obj = qjs.JS_NewObject(ctx);

    // Register functions
    inline for (.{
        .{ "parse", qsParse, 1 },
        .{ "stringify", qsStringify, 1 },
        .{ "escape", qsEscape, 1 },
        .{ "unescape", qsUnescape, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, qs_obj, binding[0], func);
    }

    // Aliases
    const parse_func = qjs.JS_GetPropertyStr(ctx, qs_obj, "parse");
    _ = qjs.JS_SetPropertyStr(ctx, qs_obj, "decode", qjs.JS_DupValue(ctx, parse_func));
    qjs.JS_FreeValue(ctx, parse_func);

    const stringify_func = qjs.JS_GetPropertyStr(ctx, qs_obj, "stringify");
    _ = qjs.JS_SetPropertyStr(ctx, qs_obj, "encode", qjs.JS_DupValue(ctx, stringify_func));
    qjs.JS_FreeValue(ctx, stringify_func);

    // Set in _modules for require('querystring')
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "querystring", qs_obj);
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, qs_obj);
    }
}
