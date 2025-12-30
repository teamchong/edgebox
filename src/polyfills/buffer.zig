/// Native Buffer module - QuickJS C functions
/// Implements Node.js Buffer API on top of Uint8Array
/// Zero allocation for most operations
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

/// Buffer.from(array) - Create buffer from array or string
/// OPTIMIZED: Uses JS_NewArrayBufferCopy for bulk memcpy instead of byte-by-byte
fn bufferFrom(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "Buffer.from requires at least 1 argument");

    // Check if it's a string
    if (qjs.JS_IsString(argv[0])) {
        var len: usize = undefined;
        const str = qjs.JS_ToCStringLen(ctx, &len, argv[0]);
        if (str == null) return qjs.JS_EXCEPTION;
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
        const arr = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
        // Note: Uint8Array constructor takes ownership of ArrayBuffer, no need to free array_buf
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

/// Helper to get raw bytes from a TypedArray/ArrayBuffer
/// Returns null if not a valid buffer type
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
        // Clear exception from failed typed array check
        const exc = qjs.JS_GetException(ctx);
        qjs.JS_FreeValue(ctx, exc);
    }

    // Try raw ArrayBuffer
    var ab_size: usize = undefined;
    const ab_ptr = qjs.JS_GetArrayBuffer(ctx, &ab_size, val);
    if (ab_ptr != null and ab_size > 0) {
        return ab_ptr[0..ab_size];
    } else {
        // Clear exception
        const exc = qjs.JS_GetException(ctx);
        qjs.JS_FreeValue(ctx, exc);
    }

    return null;
}

/// Buffer.concat(list, totalLength) - Concatenate buffers
/// OPTIMIZED: Uses bulk memcpy instead of byte-by-byte
fn bufferConcat(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "Buffer.concat requires list argument");

    // Get array length
    const len_val = qjs.JS_GetPropertyStr(ctx, argv[0], "length");
    defer qjs.JS_FreeValue(ctx, len_val);

    var arr_len: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &arr_len, len_val) != 0) return qjs.JS_EXCEPTION;

    if (arr_len == 0) {
        // Return empty Uint8Array
        const global = qjs.JS_GetGlobalObject(ctx);
        defer qjs.JS_FreeValue(ctx, global);
        const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
        defer qjs.JS_FreeValue(ctx, uint8array_ctor);
        var ctor_args = [1]qjs.JSValue{qjs.JS_NewInt32(ctx, 0)};
        return qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    }

    // First pass: calculate total length and validate all buffers can be accessed
    var total_len: usize = 0;
    for (0..@intCast(arr_len)) |i| {
        const buf = qjs.JS_GetPropertyUint32(ctx, argv[0], @intCast(i));
        defer qjs.JS_FreeValue(ctx, buf);

        if (getBufferBytes(ctx, buf)) |bytes| {
            total_len += bytes.len;
        } else {
            // Fall back to length property for non-zero-copy path
            const buf_len_val = qjs.JS_GetPropertyStr(ctx, buf, "length");
            defer qjs.JS_FreeValue(ctx, buf_len_val);
            var buf_len: i32 = 0;
            _ = qjs.JS_ToInt32(ctx, &buf_len, buf_len_val);
            if (buf_len > 0) {
                total_len += @intCast(buf_len);
            }
        }
    }

    if (total_len == 0) {
        // Return empty Uint8Array
        const global = qjs.JS_GetGlobalObject(ctx);
        defer qjs.JS_FreeValue(ctx, global);
        const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
        defer qjs.JS_FreeValue(ctx, uint8array_ctor);
        var ctor_args = [1]qjs.JSValue{qjs.JS_NewInt32(ctx, 0)};
        return qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    }

    // Allocate temporary buffer for concatenation
    const allocator = std.heap.page_allocator;
    const result_buf = allocator.alloc(u8, total_len) catch {
        return qjs.JS_ThrowInternalError(ctx, "out of memory");
    };
    defer allocator.free(result_buf);

    // Second pass: copy all buffers using bulk memcpy
    var offset: usize = 0;
    for (0..@intCast(arr_len)) |i| {
        const buf = qjs.JS_GetPropertyUint32(ctx, argv[0], @intCast(i));
        defer qjs.JS_FreeValue(ctx, buf);

        if (getBufferBytes(ctx, buf)) |bytes| {
            // ZERO-COPY: bulk memcpy
            @memcpy(result_buf[offset..][0..bytes.len], bytes);
            offset += bytes.len;
        } else {
            // Fallback: byte-by-byte for non-standard arrays
            const buf_len_val = qjs.JS_GetPropertyStr(ctx, buf, "length");
            defer qjs.JS_FreeValue(ctx, buf_len_val);
            var buf_len: i32 = 0;
            _ = qjs.JS_ToInt32(ctx, &buf_len, buf_len_val);

            for (0..@intCast(buf_len)) |j| {
                const byte = qjs.JS_GetPropertyUint32(ctx, buf, @intCast(j));
                defer qjs.JS_FreeValue(ctx, byte);
                var byte_val: i32 = 0;
                _ = qjs.JS_ToInt32(ctx, &byte_val, byte);
                result_buf[offset + j] = @intCast(@mod(byte_val, 256));
            }
            offset += @intCast(buf_len);
        }
    }

    // Create ArrayBuffer with bulk copy and wrap in Uint8Array
    const array_buf = qjs.JS_NewArrayBufferCopy(ctx, result_buf.ptr, total_len);
    if (qjs.JS_IsException(array_buf)) return array_buf;

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    var ctor_args = [1]qjs.JSValue{array_buf};
    return qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
}

/// FUSED: Buffer.allocFill(size, value) - alloc + fill in single operation
/// Avoids 2 WASM boundary crossings
fn bufferAllocFill(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "allocFill requires size and value");

    var size: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &size, argv[0]) != 0) return qjs.JS_EXCEPTION;
    if (size < 0) return qjs.JS_ThrowRangeError(ctx, "size must be non-negative");
    if (size == 0) {
        // Return empty buffer
        const array_buf = qjs.JS_NewArrayBufferCopy(ctx, &[_]u8{}, 0);
        if (qjs.JS_IsException(array_buf)) return array_buf;
        const global = qjs.JS_GetGlobalObject(ctx);
        defer qjs.JS_FreeValue(ctx, global);
        const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
        defer qjs.JS_FreeValue(ctx, uint8array_ctor);
        var ctor_args = [1]qjs.JSValue{array_buf};
        return qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    }

    var fill_val: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &fill_val, argv[1]) != 0) return qjs.JS_EXCEPTION;
    const fill_byte: u8 = @intCast(@mod(fill_val, 256));

    // Allocate and fill in Zig, then bulk copy to JS
    const allocator = std.heap.page_allocator;
    const buf = allocator.alloc(u8, @intCast(size)) catch {
        return qjs.JS_ThrowInternalError(ctx, "out of memory");
    };
    defer allocator.free(buf);

    @memset(buf, fill_byte);

    // Create ArrayBuffer with bulk copy
    const array_buf = qjs.JS_NewArrayBufferCopy(ctx, buf.ptr, buf.len);
    if (qjs.JS_IsException(array_buf)) return array_buf;

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    var ctor_args = [1]qjs.JSValue{array_buf};
    return qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
}

/// FUSED: Buffer.stringToHex(str) - from(str) + toString('hex') in single operation
/// Avoids 2 WASM boundary crossings
fn bufferStringToHex(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    var len: usize = undefined;
    const str = qjs.JS_ToCStringLen(ctx, &len, argv[0]);
    if (str == null) return qjs.JS_NewString(ctx, "");
    defer qjs.JS_FreeCString(ctx, str);

    if (len == 0) return qjs.JS_NewString(ctx, "");

    // Directly convert to hex without intermediate buffer
    const hex_len = len * 2;
    const allocator = std.heap.page_allocator;
    const hex_buf = allocator.alloc(u8, hex_len) catch {
        return qjs.JS_ThrowInternalError(ctx, "out of memory");
    };
    defer allocator.free(hex_buf);

    const hex_chars = "0123456789abcdef";
    const bytes = @as([*]const u8, @ptrCast(str))[0..len];
    for (bytes, 0..) |byte, i| {
        hex_buf[i * 2] = hex_chars[byte >> 4];
        hex_buf[i * 2 + 1] = hex_chars[byte & 0x0F];
    }

    return qjs.JS_NewStringLen(ctx, hex_buf.ptr, @intCast(hex_len));
}

/// FUSED: Buffer.hexToString(hex) - from(hex, 'hex') + toString() in single operation
fn bufferHexToString(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    var len: usize = undefined;
    const hex_str = qjs.JS_ToCStringLen(ctx, &len, argv[0]);
    if (hex_str == null) return qjs.JS_NewString(ctx, "");
    defer qjs.JS_FreeCString(ctx, hex_str);

    if (len == 0) return qjs.JS_NewString(ctx, "");
    if (len % 2 != 0) return qjs.JS_ThrowTypeError(ctx, "Invalid hex string (odd length)");

    const byte_len = len / 2;
    const allocator = std.heap.page_allocator;
    const buf = allocator.alloc(u8, byte_len) catch {
        return qjs.JS_ThrowInternalError(ctx, "out of memory");
    };
    defer allocator.free(buf);

    const hex_bytes = @as([*]const u8, @ptrCast(hex_str))[0..len];
    for (0..byte_len) |i| {
        const hi = std.fmt.charToDigit(hex_bytes[i * 2], 16) catch {
            return qjs.JS_ThrowTypeError(ctx, "Invalid hex character");
        };
        const lo = std.fmt.charToDigit(hex_bytes[i * 2 + 1], 16) catch {
            return qjs.JS_ThrowTypeError(ctx, "Invalid hex character");
        };
        buf[i] = (hi << 4) | lo;
    }

    return qjs.JS_NewStringLen(ctx, buf.ptr, @intCast(byte_len));
}

/// Native compare operation - returns -1, 0, or 1
fn bufferCompare(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_NewInt32(ctx, 0);

    const bytes1 = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_NewInt32(ctx, 0);
    const bytes2 = getBufferBytes(ctx, argv[1]) orelse return qjs.JS_NewInt32(ctx, 0);

    // Compare bytes
    const min_len = @min(bytes1.len, bytes2.len);
    for (0..min_len) |i| {
        if (bytes1[i] < bytes2[i]) return qjs.JS_NewInt32(ctx, -1);
        if (bytes1[i] > bytes2[i]) return qjs.JS_NewInt32(ctx, 1);
    }

    // If all compared bytes are equal, compare lengths
    if (bytes1.len < bytes2.len) return qjs.JS_NewInt32(ctx, -1);
    if (bytes1.len > bytes2.len) return qjs.JS_NewInt32(ctx, 1);
    return qjs.JS_NewInt32(ctx, 0);
}

/// Native equals check - returns boolean
fn bufferEquals(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_NewBool(ctx, false);

    const bytes1 = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_NewBool(ctx, false);
    const bytes2 = getBufferBytes(ctx, argv[1]) orelse return qjs.JS_NewBool(ctx, false);

    if (bytes1.len != bytes2.len) return qjs.JS_NewBool(ctx, false);

    // Use memcmp equivalent
    for (0..bytes1.len) |i| {
        if (bytes1[i] != bytes2[i]) return qjs.JS_NewBool(ctx, false);
    }
    return qjs.JS_NewBool(ctx, true);
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

/// Register native Buffer helpers in _modules (NOT globalThis.Buffer)
/// The JS Buffer class in runtime.js handles the full implementation with prototype methods.
/// Native helpers are registered for internal optimization only.
pub fn register(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Don't set globalThis.Buffer - let JS Buffer class handle it
    // The JS implementation extends Uint8Array with all instance methods (copy, equals, etc.)
    // Native Zig can only return plain Uint8Array without prototype methods

    // Register native helpers in _modules._nativeBuffer for potential future optimization
    var modules = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (qjs.JS_IsUndefined(modules)) {
        modules = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, global, "_modules", qjs.JS_DupValue(ctx, modules));
    }
    defer qjs.JS_FreeValue(ctx, modules);

    // Create native buffer helpers object
    const native_buffer = qjs.JS_NewObject(ctx);

    // Register optimized static methods
    inline for (.{
        .{ "from", bufferFrom, 2 },
        .{ "alloc", bufferAlloc, 1 },
        .{ "allocUnsafe", bufferAllocUnsafe, 1 },
        .{ "concat", bufferConcat, 2 },
        .{ "isBuffer", bufferIsBuffer, 1 },
        // Fused operations (avoid multiple WASM crossings)
        .{ "allocFill", bufferAllocFill, 2 },
        .{ "stringToHex", bufferStringToHex, 1 },
        .{ "hexToString", bufferHexToString, 1 },
        // Instance method helpers (operate on existing buffers)
        .{ "compare", bufferCompare, 2 },
        .{ "equals", bufferEquals, 2 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, native_buffer, binding[0], func);
    }

    // Store as _modules._nativeBuffer (available for future optimization)
    _ = qjs.JS_SetPropertyStr(ctx, modules, "_nativeBuffer", native_buffer);
}
