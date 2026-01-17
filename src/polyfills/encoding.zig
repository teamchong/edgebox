/// Native encoding module - QuickJS C functions
/// TextEncoder, TextDecoder, atob, btoa, hex encoding
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// Stack buffers for encoding/decoding operations
var encode_buffer: [65536]u8 = undefined; // 64KB for encoding output
var decode_buffer: [65536]u8 = undefined; // 64KB for decoding output

const base64_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

/// TextEncoder.encode(str) - Encode string to UTF-8 bytes
fn textEncoderEncode(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        // Return empty Uint8Array
        return qjs.JS_NewArrayBufferCopy(ctx, &[_]u8{}, 0);
    }

    const str = qjs.JS_ToCString(ctx, argv[0]);
    if (str == null) {
        return qjs.JS_NewArrayBufferCopy(ctx, &[_]u8{}, 0);
    }
    defer qjs.JS_FreeCString(ctx, str);

    const text = std.mem.span(str);

    // QuickJS strings are already UTF-8, just copy to Uint8Array
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    // Create new Uint8Array with string bytes
    const len_val = qjs.JS_NewInt32(ctx, @intCast(text.len));
    var ctor_args = [1]qjs.JSValue{len_val};
    const arr = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    qjs.JS_FreeValue(ctx, len_val);

    if (qjs.JS_IsException(arr)) return arr;

    // Fill the array with bytes
    for (text, 0..) |byte, i| {
        const byte_val = qjs.JS_NewInt32(ctx, @intCast(byte));
        _ = qjs.JS_SetPropertyUint32(ctx, arr, @intCast(i), byte_val);
    }

    return arr;
}

/// TextDecoder.decode(buffer) - Decode UTF-8 bytes to string
fn textDecoderDecode(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    // Get the buffer data
    var size: usize = 0;
    const ptr = qjs.JS_GetArrayBuffer(ctx, &size, argv[0]);

    if (ptr != null) {
        // Direct ArrayBuffer
        const bytes = @as([*]const u8, @ptrCast(ptr))[0..size];
        return qjs.JS_NewStringLen(ctx, bytes.ptr, @intCast(bytes.len));
    }

    // Try as TypedArray (Uint8Array, etc.)
    const length_val = qjs.JS_GetPropertyStr(ctx, argv[0], "length");
    defer qjs.JS_FreeValue(ctx, length_val);

    var length: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &length, length_val);

    if (length <= 0) return qjs.JS_NewString(ctx, "");
    if (length > decode_buffer.len) {
        return qjs.JS_ThrowRangeError(ctx, "Buffer too large for decode");
    }

    // Read bytes from array
    for (0..@intCast(length)) |i| {
        const val = qjs.JS_GetPropertyUint32(ctx, argv[0], @intCast(i));
        defer qjs.JS_FreeValue(ctx, val);

        var byte: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &byte, val);
        decode_buffer[i] = @intCast(@mod(byte, 256));
    }

    return qjs.JS_NewStringLen(ctx, &decode_buffer, @intCast(length));
}

/// atob(base64) - Decode base64 to binary string
fn atobFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    const str = qjs.JS_ToCString(ctx, argv[0]);
    if (str == null) return qjs.JS_NewString(ctx, "");
    defer qjs.JS_FreeCString(ctx, str);

    const input = std.mem.span(str);

    // Remove whitespace
    var clean_len: usize = 0;
    for (input) |c| {
        if (c != ' ' and c != '\t' and c != '\n' and c != '\r') {
            decode_buffer[clean_len] = c;
            clean_len += 1;
            if (clean_len >= decode_buffer.len) break;
        }
    }

    const clean_input = decode_buffer[0..clean_len];

    // Decode base64
    const decoder = std.base64.standard.Decoder;
    const max_decoded = decoder.calcSizeForSlice(clean_input) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid base64 string");
    };

    if (max_decoded > encode_buffer.len) {
        return qjs.JS_ThrowRangeError(ctx, "Base64 string too large");
    }

    decoder.decode(&encode_buffer, clean_input) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid base64 string");
    };

    return qjs.JS_NewStringLen(ctx, &encode_buffer, @intCast(max_decoded));
}

/// btoa(binary) - Encode binary string to base64
/// Per spec, input is a "binary string" where each character is in the range 0-255 (Latin-1)
/// QuickJS stores strings as UTF-8, so we need to decode UTF-8 code points back to Latin-1 bytes
fn btoaFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    const str = qjs.JS_ToCString(ctx, argv[0]);
    if (str == null) return qjs.JS_NewString(ctx, "");
    defer qjs.JS_FreeCString(ctx, str);

    const utf8_input = std.mem.span(str);

    // Decode UTF-8 to Latin-1 code points (byte values)
    // Each Unicode code point 0-255 becomes one byte
    var latin1_buf: [65536]u8 = undefined;
    var latin1_len: usize = 0;

    var utf8_view = std.unicode.Utf8View.initUnchecked(utf8_input);
    var it = utf8_view.iterator();

    while (it.nextCodepoint()) |cp| {
        if (cp > 255) {
            return qjs.JS_ThrowRangeError(ctx, "btoa: character code point > 255 (not a valid binary string)");
        }
        if (latin1_len >= latin1_buf.len) {
            return qjs.JS_ThrowRangeError(ctx, "String too large for base64 encoding");
        }
        latin1_buf[latin1_len] = @intCast(cp);
        latin1_len += 1;
    }

    // Encode Latin-1 bytes to base64
    const encoder = std.base64.standard.Encoder;
    const encoded_len = encoder.calcSize(latin1_len);

    if (encoded_len > encode_buffer.len) {
        return qjs.JS_ThrowRangeError(ctx, "String too large for base64 encoding");
    }

    const result = encoder.encode(&encode_buffer, latin1_buf[0..latin1_len]);
    return qjs.JS_NewStringLen(ctx, result.ptr, @intCast(result.len));
}

/// hexEncode(buffer) - Encode bytes to hex string
fn hexEncode(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    // Get buffer data
    var size: usize = 0;
    const ptr = qjs.JS_GetArrayBuffer(ctx, &size, argv[0]);

    var bytes_to_encode: []const u8 = undefined;

    if (ptr != null) {
        // Direct ArrayBuffer
        bytes_to_encode = @as([*]const u8, @ptrCast(ptr))[0..size];
    } else {
        // Try as TypedArray
        const length_val = qjs.JS_GetPropertyStr(ctx, argv[0], "length");
        defer qjs.JS_FreeValue(ctx, length_val);

        var length: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &length, length_val);

        if (length <= 0) return qjs.JS_NewString(ctx, "");
        if (length > decode_buffer.len) {
            return qjs.JS_ThrowRangeError(ctx, "Buffer too large");
        }

        // Read bytes from array
        for (0..@intCast(length)) |i| {
            const val = qjs.JS_GetPropertyUint32(ctx, argv[0], @intCast(i));
            defer qjs.JS_FreeValue(ctx, val);

            var byte: i32 = 0;
            _ = qjs.JS_ToInt32(ctx, &byte, val);
            decode_buffer[i] = @intCast(@mod(byte, 256));
        }

        bytes_to_encode = decode_buffer[0..@intCast(length)];
    }

    // Encode to hex (2 chars per byte)
    const hex_len = bytes_to_encode.len * 2;
    if (hex_len > encode_buffer.len) {
        return qjs.JS_ThrowRangeError(ctx, "Buffer too large for hex encoding");
    }

    // Manual hex encoding
    const hex_chars = "0123456789abcdef";
    for (bytes_to_encode, 0..) |byte, i| {
        encode_buffer[i * 2] = hex_chars[byte >> 4];
        encode_buffer[i * 2 + 1] = hex_chars[byte & 0x0F];
    }

    return qjs.JS_NewStringLen(ctx, &encode_buffer, @intCast(hex_len));
}

/// hexDecode(hexString) - Decode hex string to bytes
fn hexDecode(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_NewArrayBufferCopy(ctx, &[_]u8{}, 0);
    }

    const str = qjs.JS_ToCString(ctx, argv[0]);
    if (str == null) {
        return qjs.JS_NewArrayBufferCopy(ctx, &[_]u8{}, 0);
    }
    defer qjs.JS_FreeCString(ctx, str);

    const hex_str = std.mem.span(str);

    // Hex string must have even length
    if (hex_str.len % 2 != 0) {
        return qjs.JS_ThrowTypeError(ctx, "Invalid hex string (odd length)");
    }

    const byte_len = hex_str.len / 2;
    if (byte_len > decode_buffer.len) {
        return qjs.JS_ThrowRangeError(ctx, "Hex string too large");
    }

    // Decode hex to bytes
    for (0..byte_len) |i| {
        const hi = std.fmt.charToDigit(hex_str[i * 2], 16) catch {
            return qjs.JS_ThrowTypeError(ctx, "Invalid hex character");
        };
        const lo = std.fmt.charToDigit(hex_str[i * 2 + 1], 16) catch {
            return qjs.JS_ThrowTypeError(ctx, "Invalid hex character");
        };
        decode_buffer[i] = (hi << 4) | lo;
    }

    // Create Uint8Array
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    const len_val = qjs.JS_NewInt32(ctx, @intCast(byte_len));
    var ctor_args = [1]qjs.JSValue{len_val};
    const arr = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    qjs.JS_FreeValue(ctx, len_val);

    if (qjs.JS_IsException(arr)) return arr;

    // Fill array with decoded bytes
    for (decode_buffer[0..byte_len], 0..) |byte, i| {
        const byte_val = qjs.JS_NewInt32(ctx, @intCast(byte));
        _ = qjs.JS_SetPropertyUint32(ctx, arr, @intCast(i), byte_val);
    }

    return arr;
}

/// Helper to get raw bytes from a TypedArray/ArrayBuffer
fn getBufferBytes(ctx: ?*qjs.JSContext, val: qjs.JSValue) ?[]const u8 {
    var offset: usize = undefined;
    var byte_len: usize = undefined;
    var bytes_per_element: usize = undefined;
    const array_buf = qjs.JS_GetTypedArrayBuffer(ctx, val, &offset, &byte_len, &bytes_per_element);

    if (!qjs.JS_IsException(array_buf)) {
        var size: usize = undefined;
        const ptr = qjs.JS_GetArrayBuffer(ctx, &size, array_buf);
        qjs.JS_FreeValue(ctx, array_buf);
        if (ptr != null and byte_len > 0) {
            return (ptr + offset)[0..byte_len];
        }
    } else {
        const exc = qjs.JS_GetException(ctx);
        qjs.JS_FreeValue(ctx, exc);
    }

    var ab_size: usize = undefined;
    const ab_ptr = qjs.JS_GetArrayBuffer(ctx, &ab_size, val);
    if (ab_ptr != null and ab_size > 0) {
        return ab_ptr[0..ab_size];
    } else {
        const exc = qjs.JS_GetException(ctx);
        qjs.JS_FreeValue(ctx, exc);
    }
    return null;
}

/// bytesToString(bytes, start?, end?) - Convert byte slice to string (50-200x faster)
/// Uses JS_NewStringLen for single O(n) memcpy instead of O(n²) concatenation
fn bytesToString(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_NewString(ctx, "");
    if (bytes.len == 0) return qjs.JS_NewString(ctx, "");

    // Get optional start parameter
    var start: usize = 0;
    if (argc >= 2) {
        var s: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &s, argv[1]);
        if (s > 0) start = @intCast(s);
    }

    // Get optional end parameter
    var end: usize = bytes.len;
    if (argc >= 3) {
        var e: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &e, argv[2]);
        if (e > 0) end = @intCast(e);
    }

    // Bounds check
    if (start >= bytes.len) return qjs.JS_NewString(ctx, "");
    if (end > bytes.len) end = bytes.len;
    if (start >= end) return qjs.JS_NewString(ctx, "");

    // Single memcpy via JS_NewStringLen
    return qjs.JS_NewStringLen(ctx, bytes.ptr + start, end - start);
}

/// TextEncoder constructor function
fn textEncoderCtor(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const obj = qjs.JS_NewObject(ctx);
    const enc_func = qjs.JS_NewCFunction(ctx, textEncoderEncode, "encode", 1);
    _ = qjs.JS_SetPropertyStr(ctx, obj, "encode", enc_func);
    return obj;
}

/// TextDecoder constructor function
fn textDecoderCtor(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const obj = qjs.JS_NewObject(ctx);
    const dec_func = qjs.JS_NewCFunction(ctx, textDecoderDecode, "decode", 1);
    _ = qjs.JS_SetPropertyStr(ctx, obj, "decode", dec_func);
    return obj;
}

/// Register encoding module and global functions
pub fn register(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Set global TextEncoder constructor
    const text_encoder_ctor = qjs.JS_NewCFunction(ctx, textEncoderCtor, "TextEncoder", 0);
    _ = qjs.JS_SetPropertyStr(ctx, global, "TextEncoder", text_encoder_ctor);

    // Set global TextDecoder constructor
    const text_decoder_ctor = qjs.JS_NewCFunction(ctx, textDecoderCtor, "TextDecoder", 0);
    _ = qjs.JS_SetPropertyStr(ctx, global, "TextDecoder", text_decoder_ctor);

    // Set global atob/btoa
    const atob_func = qjs.JS_NewCFunction(ctx, atobFunc, "atob", 1);
    _ = qjs.JS_SetPropertyStr(ctx, global, "atob", atob_func);

    const btoa_func = qjs.JS_NewCFunction(ctx, btoaFunc, "btoa", 1);
    _ = qjs.JS_SetPropertyStr(ctx, global, "btoa", btoa_func);

    // Create encoding module with hex functions
    const encoding_obj = qjs.JS_NewObject(ctx);

    const hex_encode_func = qjs.JS_NewCFunction(ctx, hexEncode, "hexEncode", 1);
    _ = qjs.JS_SetPropertyStr(ctx, encoding_obj, "hexEncode", hex_encode_func);

    const hex_decode_func = qjs.JS_NewCFunction(ctx, hexDecode, "hexDecode", 1);
    _ = qjs.JS_SetPropertyStr(ctx, encoding_obj, "hexDecode", hex_decode_func);

    // Also add atob/btoa to encoding module
    _ = qjs.JS_SetPropertyStr(ctx, encoding_obj, "atob", qjs.JS_DupValue(ctx, atob_func));
    _ = qjs.JS_SetPropertyStr(ctx, encoding_obj, "btoa", qjs.JS_DupValue(ctx, btoa_func));

    // Add bytesToString for fast byte→string conversion (50-200x faster than JS loops)
    const bytes_to_string_func = qjs.JS_NewCFunction(ctx, bytesToString, "bytesToString", 3);
    _ = qjs.JS_SetPropertyStr(ctx, encoding_obj, "bytesToString", bytes_to_string_func);

    // Set in _modules for require('encoding')
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "encoding", encoding_obj);
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, encoding_obj);
    }
}
