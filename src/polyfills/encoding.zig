/// Native encoding module - QuickJS C functions
/// TextEncoder, TextDecoder, atob, btoa, hex encoding
/// OPTIMIZED: Uses zero-copy bulk operations
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;
const shared_encoding = @import("../encoding.zig");

// Stack buffers for encoding/decoding operations
var encode_buffer: [65536]u8 = undefined; // 64KB for encoding output
var decode_buffer: [65536]u8 = undefined; // 64KB for decoding output

const base64_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

// Hex decode lookup table (0xFF = invalid)
const hex_decode_table: [256]u8 = blk: {
    var table: [256]u8 = [_]u8{0xFF} ** 256;
    for ('0'..'9' + 1) |c| table[c] = @intCast(c - '0');
    for ('A'..'F' + 1) |c| table[c] = @intCast(c - 'A' + 10);
    for ('a'..'f' + 1) |c| table[c] = @intCast(c - 'a' + 10);
    break :blk table;
};

/// Fast hex character to value (returns null for invalid)
inline fn hexCharValue(c: u8) ?u4 {
    const v = hex_decode_table[c];
    return if (v == 0xFF) null else @intCast(v);
}

/// Helper to get raw bytes from a TypedArray/ArrayBuffer (zero-copy)
fn getBufferBytes(ctx: ?*qjs.JSContext, val: qjs.JSValue) ?[]const u8 {
    // Try as TypedArray first (Uint8Array, etc.)
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

    // Try raw ArrayBuffer
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

/// TextEncoder.encode(str) - Encode string to UTF-8 bytes
/// OPTIMIZED: Uses JS_NewArrayBufferCopy for bulk memcpy
fn textEncoderEncode(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        // Return empty Uint8Array
        return qjs.JS_NewArrayBufferCopy(ctx, &[_]u8{}, 0);
    }

    var len: usize = undefined;
    const str = qjs.JS_ToCStringLen(ctx, &len, argv[0]);
    if (str == null) {
        return qjs.JS_NewArrayBufferCopy(ctx, &[_]u8{}, 0);
    }
    defer qjs.JS_FreeCString(ctx, str);

    // ZERO-COPY: Create ArrayBuffer with bulk memcpy
    const array_buf = qjs.JS_NewArrayBufferCopy(ctx, @ptrCast(str), len);
    if (qjs.JS_IsException(array_buf)) return array_buf;

    // Wrap ArrayBuffer in Uint8Array
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    var ctor_args = [1]qjs.JSValue{array_buf};
    return qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
}

/// TextDecoder.decode(buffer) - Decode UTF-8 bytes to string
/// OPTIMIZED: Uses zero-copy buffer access
fn textDecoderDecode(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    // ZERO-COPY: Try to get direct pointer to buffer data
    if (getBufferBytes(ctx, argv[0])) |bytes| {
        return qjs.JS_NewStringLen(ctx, bytes.ptr, @intCast(bytes.len));
    }

    // Fallback: Try as plain array (rare case)
    const length_val = qjs.JS_GetPropertyStr(ctx, argv[0], "length");
    defer qjs.JS_FreeValue(ctx, length_val);

    var length: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &length, length_val);

    if (length <= 0) return qjs.JS_NewString(ctx, "");
    if (length > decode_buffer.len) {
        return qjs.JS_ThrowRangeError(ctx, "Buffer too large for decode");
    }

    // Read bytes from array (fallback for non-TypedArray)
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

    // Fast path: check if any whitespace exists
    const has_whitespace = blk: {
        for (input) |c| {
            if (c == ' ' or c == '\t' or c == '\n' or c == '\r') break :blk true;
        }
        break :blk false;
    };

    // Use input directly if no whitespace, otherwise filter
    const clean_input = if (has_whitespace) blk: {
        var clean_len: usize = 0;
        for (input) |c| {
            if (c != ' ' and c != '\t' and c != '\n' and c != '\r') {
                decode_buffer[clean_len] = c;
                clean_len += 1;
                if (clean_len >= decode_buffer.len) break;
            }
        }
        break :blk decode_buffer[0..clean_len];
    } else input;

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
fn btoaFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    const str = qjs.JS_ToCString(ctx, argv[0]);
    if (str == null) return qjs.JS_NewString(ctx, "");
    defer qjs.JS_FreeCString(ctx, str);

    const input = std.mem.span(str);

    // Encode to base64
    const encoder = std.base64.standard.Encoder;
    const encoded_len = encoder.calcSize(input.len);

    if (encoded_len > encode_buffer.len) {
        return qjs.JS_ThrowRangeError(ctx, "String too large for base64 encoding");
    }

    const result = encoder.encode(&encode_buffer, input);
    return qjs.JS_NewStringLen(ctx, result.ptr, @intCast(result.len));
}

/// hexEncode(buffer) - Encode bytes to hex string
/// OPTIMIZED: Uses zero-copy buffer access
fn hexEncode(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    // ZERO-COPY: Try to get direct pointer to buffer data
    var bytes_to_encode: []const u8 = undefined;

    if (getBufferBytes(ctx, argv[0])) |bytes| {
        bytes_to_encode = bytes;
    } else {
        // Fallback: Try as plain array
        const length_val = qjs.JS_GetPropertyStr(ctx, argv[0], "length");
        defer qjs.JS_FreeValue(ctx, length_val);

        var length: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &length, length_val);

        if (length <= 0) return qjs.JS_NewString(ctx, "");
        if (length > decode_buffer.len) {
            return qjs.JS_ThrowRangeError(ctx, "Buffer too large");
        }

        // Read bytes from array (fallback for non-TypedArray)
        for (0..@intCast(length)) |i| {
            const val = qjs.JS_GetPropertyUint32(ctx, argv[0], @intCast(i));
            defer qjs.JS_FreeValue(ctx, val);

            var byte: i32 = 0;
            _ = qjs.JS_ToInt32(ctx, &byte, val);
            decode_buffer[i] = @intCast(@mod(byte, 256));
        }

        bytes_to_encode = decode_buffer[0..@intCast(length)];
    }

    // Encode to hex (2 chars per byte) using shared encoding module
    const hex_len = bytes_to_encode.len * 2;
    if (hex_len > encode_buffer.len) {
        return qjs.JS_ThrowRangeError(ctx, "Buffer too large for hex encoding");
    }

    shared_encoding.hexEncodeToSlice(bytes_to_encode, encode_buffer[0..hex_len]);
    return qjs.JS_NewStringLen(ctx, &encode_buffer, @intCast(hex_len));
}

/// hexDecode(hexString) - Decode hex string to bytes
/// OPTIMIZED: Uses JS_NewArrayBufferCopy for bulk output
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

    // Decode hex to bytes using lookup table (fast path)
    for (0..byte_len) |i| {
        const hi = hexCharValue(hex_str[i * 2]) orelse {
            return qjs.JS_ThrowTypeError(ctx, "Invalid hex character");
        };
        const lo = hexCharValue(hex_str[i * 2 + 1]) orelse {
            return qjs.JS_ThrowTypeError(ctx, "Invalid hex character");
        };
        decode_buffer[i] = (@as(u8, hi) << 4) | lo;
    }

    // ZERO-COPY: Create ArrayBuffer with bulk memcpy
    const array_buf = qjs.JS_NewArrayBufferCopy(ctx, &decode_buffer, byte_len);
    if (qjs.JS_IsException(array_buf)) return array_buf;

    // Wrap ArrayBuffer in Uint8Array
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    var ctor_args = [1]qjs.JSValue{array_buf};
    return qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
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

    // Set in _modules for require('encoding')
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "encoding", encoding_obj);
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, encoding_obj);
    }
}
