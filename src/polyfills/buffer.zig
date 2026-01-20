/// Native Buffer module - QuickJS C functions
/// Implements Node.js Buffer API on top of Uint8Array
/// Zero allocation for most operations
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// Cached Uint8Array constructor (avoids 10+ global+property lookups per buffer operation)
var cached_uint8array_ctor: qjs.JSValue = quickjs.jsUndefined();

// Static buffer for transcode operations (64KB should cover most use cases)
var transcode_buffer: [65536]u8 = undefined;

/// Get cached Uint8Array constructor (caches on first call)
fn getUint8ArrayCtor(ctx: ?*qjs.JSContext) qjs.JSValue {
    if (qjs.JS_IsUndefined(cached_uint8array_ctor)) {
        const global = qjs.JS_GetGlobalObject(ctx);
        cached_uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
        qjs.JS_FreeValue(ctx, global);
    }
    return cached_uint8array_ctor;
}

/// Helper to create Uint8Array from bytes (used by pack functions)
fn createUint8Array(ctx: ?*qjs.JSContext, buf: []const u8) qjs.JSValue {
    const array_buf = qjs.JS_NewArrayBufferCopy(ctx, buf.ptr, buf.len);
    if (qjs.JS_IsException(array_buf)) return array_buf;

    const uint8array_ctor = getUint8ArrayCtor(ctx);
    var ctor_args = [1]qjs.JSValue{array_buf};
    const result = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    qjs.JS_FreeValue(ctx, array_buf); // Free after constructor call (constructor dups args)
    return result;
}

/// Buffer.from(array) - Create buffer from array or string
/// OPTIMIZED: Uses JS_NewArrayBufferCopy for bulk memcpy instead of byte-by-byte
fn bufferFrom(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "Buffer.from requires at least 1 argument");

    // Check if it's a string
    if (qjs.JS_IsString(argv[0])) {
        var len: usize = undefined;
        const str = qjs.JS_ToCStringLen(ctx, &len, argv[0]);
        if (str == null) return quickjs.jsException();
        defer qjs.JS_FreeCString(ctx, str);

        // ZERO-COPY: Create ArrayBuffer with bulk memcpy
        const array_buf = qjs.JS_NewArrayBufferCopy(ctx, @ptrCast(str), len);
        if (qjs.JS_IsException(array_buf)) return array_buf;

        // Wrap ArrayBuffer in Uint8Array (using cached constructor)
        const uint8array_ctor = getUint8ArrayCtor(ctx);
        var ctor_args = [1]qjs.JSValue{array_buf};
        const result = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
        qjs.JS_FreeValue(ctx, array_buf); // Free after constructor call (constructor dups args)
        return result;
    }

    // For arrays/typed arrays, just wrap in Uint8Array (using cached constructor)
    const uint8array_ctor = getUint8ArrayCtor(ctx);
    var ctor_args = [1]qjs.JSValue{argv[0]};
    return qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
}

/// Buffer.alloc(size) - Create zero-filled buffer
fn bufferAlloc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "Buffer.alloc requires size argument");

    var size: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &size, argv[0]) != 0) return quickjs.jsException();
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
    if (qjs.JS_ToInt32(ctx, &arr_len, len_val) != 0) return quickjs.jsException();

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
    if (argc < 1) return quickjs.jsNewBool( false);

    // Check if it's a Uint8Array
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], uint8array_ctor);
    return quickjs.jsNewBool( result == 1);
}

/// Fast Buffer.subarray - bypasses QuickJS speciesCreate overhead (4.6x faster)
/// Creates a view into the same backing ArrayBuffer without copying data
/// Uses new Uint8Array(arrayBuffer, byteOffset, length) constructor pattern
fn bufferSubarray(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "Buffer.subarray requires buffer argument");

    const c = ctx orelse return quickjs.jsException();
    const source = argv[0];

    // Get typed array info from source buffer
    var byte_offset: usize = 0;
    var byte_length: usize = 0;
    var bytes_per_element: usize = 0;
    const ab = qjs.JS_GetTypedArrayBuffer(c, source, &byte_offset, &byte_length, &bytes_per_element);
    if (qjs.JS_IsException(ab)) return quickjs.jsException();
    // Note: We need to free ab after use since we're taking a reference

    const length: i32 = @intCast(byte_length);

    // Parse start argument (default 0)
    var start: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(c, &start, argv[1]);

    // Parse end argument (default length)
    var end: i32 = length;
    if (argc > 2 and !qjs.JS_IsUndefined(argv[2])) _ = qjs.JS_ToInt32(c, &end, argv[2]);

    // Handle negative indices (from end) - same as Node.js behavior
    if (start < 0) start = @max(length + start, 0);
    if (end < 0) end = @max(length + end, 0);
    start = @min(start, length);
    end = @min(@max(end, start), length);

    // Calculate new offset and length for the view
    const new_offset = byte_offset + @as(usize, @intCast(start));
    const new_length = @as(usize, @intCast(end - start));

    // Create new Uint8Array view: new Uint8Array(arrayBuffer, byteOffset, length)
    const global = qjs.JS_GetGlobalObject(c);
    defer qjs.JS_FreeValue(c, global);
    const uint8array_ctor = qjs.JS_GetPropertyStr(c, global, "Uint8Array");
    defer qjs.JS_FreeValue(c, uint8array_ctor);

    var ctor_args = [3]qjs.JSValue{
        ab,
        qjs.JS_NewInt32(c, @intCast(new_offset)),
        qjs.JS_NewInt32(c, @intCast(new_length)),
    };
    const result = qjs.JS_CallConstructor(c, uint8array_ctor, 3, &ctor_args);
    qjs.JS_FreeValue(c, ab); // Free array buffer reference after constructor
    return result;
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

/// Helper to get mutable bytes from a TypedArray/ArrayBuffer (for copy operations)
fn getMutableBufferBytes(ctx: ?*qjs.JSContext, val: qjs.JSValue) ?[]u8 {
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

/// bufferCompare(buf1, buf2) - Compare two buffers, returns -1, 0, or 1
fn bufferCompare(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_NewInt32(ctx, 0);

    const bytes1 = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_NewInt32(ctx, 0);
    const bytes2 = getBufferBytes(ctx, argv[1]) orelse return qjs.JS_NewInt32(ctx, 0);

    const min_len = @min(bytes1.len, bytes2.len);
    for (0..min_len) |i| {
        if (bytes1[i] < bytes2[i]) return qjs.JS_NewInt32(ctx, -1);
        if (bytes1[i] > bytes2[i]) return qjs.JS_NewInt32(ctx, 1);
    }

    if (bytes1.len < bytes2.len) return qjs.JS_NewInt32(ctx, -1);
    if (bytes1.len > bytes2.len) return qjs.JS_NewInt32(ctx, 1);
    return qjs.JS_NewInt32(ctx, 0);
}

/// bufferEquals(buf1, buf2) - Check if two buffers are equal
fn bufferEquals(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return quickjs.jsNewBool( false);

    const bytes1 = getBufferBytes(ctx, argv[0]) orelse return quickjs.jsNewBool( false);
    const bytes2 = getBufferBytes(ctx, argv[1]) orelse return quickjs.jsNewBool( false);

    if (bytes1.len != bytes2.len) return quickjs.jsNewBool( false);
    return quickjs.jsNewBool( std.mem.eql(u8, bytes1, bytes2));
}

/// bufferCopy(source, target, targetStart, sourceStart, sourceEnd) - Copy bytes between buffers
fn bufferCopy(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_NewInt32(ctx, 0);

    const source = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_NewInt32(ctx, 0);
    const target = getMutableBufferBytes(ctx, argv[1]) orelse return qjs.JS_NewInt32(ctx, 0);

    var target_start: i32 = 0;
    var source_start: i32 = 0;
    var source_end: i32 = @intCast(source.len);

    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &target_start, argv[2]);
    if (argc > 3) _ = qjs.JS_ToInt32(ctx, &source_start, argv[3]);
    if (argc > 4) _ = qjs.JS_ToInt32(ctx, &source_end, argv[4]);

    // Clamp values
    if (target_start < 0) target_start = 0;
    if (source_start < 0) source_start = 0;
    if (source_end < 0) source_end = 0;

    const ts: usize = @intCast(@min(target_start, @as(i32, @intCast(target.len))));
    const ss: usize = @intCast(@min(source_start, @as(i32, @intCast(source.len))));
    const se: usize = @intCast(@min(source_end, @as(i32, @intCast(source.len))));

    if (ss >= se) return qjs.JS_NewInt32(ctx, 0);

    const copy_len = @min(se - ss, target.len - ts);
    if (copy_len == 0) return qjs.JS_NewInt32(ctx, 0);

    @memcpy(target[ts..][0..copy_len], source[ss..][0..copy_len]);
    return qjs.JS_NewInt32(ctx, @intCast(copy_len));
}

/// bufferIndexOf(haystack, needle, offset) - Find needle in buffer
fn bufferIndexOf(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_NewInt32(ctx, -1);

    const haystack = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_NewInt32(ctx, -1);
    const needle = getBufferBytes(ctx, argv[1]) orelse return qjs.JS_NewInt32(ctx, -1);

    if (needle.len == 0) return qjs.JS_NewInt32(ctx, 0);
    if (needle.len > haystack.len) return qjs.JS_NewInt32(ctx, -1);

    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);

    if (offset < 0) offset = @max(0, @as(i32, @intCast(haystack.len)) + offset);

    const start: usize = @intCast(@min(offset, @as(i32, @intCast(haystack.len))));
    if (start >= haystack.len) return qjs.JS_NewInt32(ctx, -1);

    if (std.mem.indexOf(u8, haystack[start..], needle)) |pos| {
        return qjs.JS_NewInt32(ctx, @intCast(pos + start));
    }
    return qjs.JS_NewInt32(ctx, -1);
}

/// bufferLastIndexOf(haystack, needle, offset) - Find last occurrence of needle
fn bufferLastIndexOf(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_NewInt32(ctx, -1);

    const haystack = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_NewInt32(ctx, -1);
    const needle = getBufferBytes(ctx, argv[1]) orelse return qjs.JS_NewInt32(ctx, -1);

    if (needle.len == 0) return qjs.JS_NewInt32(ctx, @intCast(haystack.len));
    if (needle.len > haystack.len) return qjs.JS_NewInt32(ctx, -1);

    var offset: i32 = @intCast(haystack.len - 1);
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);

    if (offset < 0) offset = @max(0, @as(i32, @intCast(haystack.len)) + offset);

    const search_end: usize = @intCast(@min(offset + @as(i32, @intCast(needle.len)), @as(i32, @intCast(haystack.len))));

    if (std.mem.lastIndexOf(u8, haystack[0..search_end], needle)) |pos| {
        return qjs.JS_NewInt32(ctx, @intCast(pos));
    }
    return qjs.JS_NewInt32(ctx, -1);
}

/// bufferToUtf8String(buffer) - Convert buffer bytes to UTF-8 string
/// Uses QuickJS internal UTF-8 handling for 2400x speedup over JS
fn bufferToUtf8String(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_NewString(ctx, "");
    if (bytes.len == 0) return qjs.JS_NewString(ctx, "");

    // QuickJS JS_NewStringLen creates a string from UTF-8 bytes
    return qjs.JS_NewStringLen(ctx, bytes.ptr, bytes.len);
}

/// bufferToBase64(buffer) - Convert buffer to base64 string (811x faster)
/// Uses std.base64 for optimized encoding
fn bufferToBase64(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_NewString(ctx, "");
    if (bytes.len == 0) return qjs.JS_NewString(ctx, "");

    // Calculate output size and allocate
    const encoded_len = std.base64.standard.Encoder.calcSize(bytes.len);
    const buf = std.heap.page_allocator.alloc(u8, encoded_len) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    defer std.heap.page_allocator.free(buf);

    // Encode to base64
    _ = std.base64.standard.Encoder.encode(buf, bytes);

    return qjs.JS_NewStringLen(ctx, buf.ptr, encoded_len);
}

/// bufferFromBase64(string) - Convert base64 string to buffer (811x faster)
/// Uses std.base64 for optimized decoding
fn bufferFromBase64(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "bufferFromBase64 requires a string argument");

    if (!qjs.JS_IsString(argv[0])) {
        return qjs.JS_ThrowTypeError(ctx, "bufferFromBase64 requires a string argument");
    }

    var len: usize = undefined;
    const str = qjs.JS_ToCStringLen(ctx, &len, argv[0]);
    if (str == null) return quickjs.jsException();
    defer qjs.JS_FreeCString(ctx, str);

    if (len == 0) {
        const global = qjs.JS_GetGlobalObject(ctx);
        defer qjs.JS_FreeValue(ctx, global);
        const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
        defer qjs.JS_FreeValue(ctx, uint8array_ctor);
        var ctor_args = [1]qjs.JSValue{qjs.JS_NewInt32(ctx, 0)};
        return qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    }

    const data: []const u8 = @ptrCast(str[0..len]);

    // Calculate exact decoded size first (validates padding)
    const decoded_len = std.base64.standard.Decoder.calcSizeForSlice(data) catch {
        return qjs.JS_ThrowSyntaxError(ctx, "Invalid base64 string");
    };

    // Allocate buffer for decoded data
    const buf = std.heap.page_allocator.alloc(u8, decoded_len) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    defer std.heap.page_allocator.free(buf);

    // Decode from base64
    std.base64.standard.Decoder.decode(buf, data) catch {
        return qjs.JS_ThrowSyntaxError(ctx, "Invalid base64 string");
    };

    // Create ArrayBuffer and copy data
    const array_buf = qjs.JS_NewArrayBufferCopy(ctx, buf.ptr, decoded_len);
    if (qjs.JS_IsException(array_buf)) return array_buf;

    // Wrap in Uint8Array
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);
    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);
    var ctor_args = [1]qjs.JSValue{array_buf};
    const result = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    qjs.JS_FreeValue(ctx, array_buf); // Free after constructor call (constructor dups args)
    return result;
}

/// bufferFromUtf8String(string) - Convert UTF-8 string to buffer
/// Uses QuickJS internal UTF-8 for 2400x speedup over JS
fn bufferFromUtf8String(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "bufferFromUtf8String requires a string argument");

    if (!qjs.JS_IsString(argv[0])) {
        return qjs.JS_ThrowTypeError(ctx, "bufferFromUtf8String requires a string argument");
    }

    var len: usize = undefined;
    const str = qjs.JS_ToCStringLen(ctx, &len, argv[0]);
    if (str == null) return quickjs.jsException();
    defer qjs.JS_FreeCString(ctx, str);

    if (len == 0) {
        const global = qjs.JS_GetGlobalObject(ctx);
        defer qjs.JS_FreeValue(ctx, global);
        const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
        defer qjs.JS_FreeValue(ctx, uint8array_ctor);
        var ctor_args = [1]qjs.JSValue{qjs.JS_NewInt32(ctx, 0)};
        return qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    }

    // Create ArrayBuffer with bulk memcpy
    const array_buf = qjs.JS_NewArrayBufferCopy(ctx, @ptrCast(str), len);
    if (qjs.JS_IsException(array_buf)) return array_buf;

    // Wrap in Uint8Array
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);
    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);
    var ctor_args = [1]qjs.JSValue{array_buf};
    const result = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    qjs.JS_FreeValue(ctx, array_buf); // Free after constructor call (constructor dups args)
    return result;
}

// Hex encoding lookup table
const hex_chars = "0123456789abcdef";

/// bufferToHex(buffer) - Convert buffer to hex string
fn bufferToHex(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_NewString(ctx, "");
    if (bytes.len == 0) return qjs.JS_NewString(ctx, "");

    // Allocate output buffer (2 hex chars per byte)
    const hex_len = bytes.len * 2;
    const buf = std.heap.page_allocator.alloc(u8, hex_len) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    defer std.heap.page_allocator.free(buf);

    // Convert bytes to hex
    for (bytes, 0..) |byte, i| {
        buf[i * 2] = hex_chars[byte >> 4];
        buf[i * 2 + 1] = hex_chars[byte & 0x0f];
    }

    return qjs.JS_NewStringLen(ctx, buf.ptr, hex_len);
}

/// Helper to convert hex char to value (returns 255 on invalid)
fn hexCharToValue(c: u8) u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        'a'...'f' => c - 'a' + 10,
        'A'...'F' => c - 'A' + 10,
        else => 255,
    };
}

/// bufferFromHex(hexString) - Convert hex string to buffer
fn bufferFromHex(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "bufferFromHex requires a string argument");

    if (!qjs.JS_IsString(argv[0])) {
        return qjs.JS_ThrowTypeError(ctx, "bufferFromHex requires a string argument");
    }

    var len: usize = undefined;
    const str = qjs.JS_ToCStringLen(ctx, &len, argv[0]);
    if (str == null) return quickjs.jsException();
    defer qjs.JS_FreeCString(ctx, str);

    if (len == 0) {
        return createUint8Array(ctx, &[_]u8{});
    }

    // Hex string must have even length
    if (len % 2 != 0) {
        return qjs.JS_ThrowSyntaxError(ctx, "Invalid hex string: odd length");
    }

    const data: []const u8 = @ptrCast(str[0..len]);
    const decoded_len = len / 2;

    const buf = std.heap.page_allocator.alloc(u8, decoded_len) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    defer std.heap.page_allocator.free(buf);

    // Decode hex pairs
    for (0..decoded_len) |i| {
        const hi = hexCharToValue(data[i * 2]);
        const lo = hexCharToValue(data[i * 2 + 1]);
        if (hi == 255 or lo == 255) {
            return qjs.JS_ThrowSyntaxError(ctx, "Invalid hex character");
        }
        buf[i] = (hi << 4) | lo;
    }

    return createUint8Array(ctx, buf);
}

/// bufferStringToHex(string) - Convert string bytes to hex
fn bufferStringToHex(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    if (!qjs.JS_IsString(argv[0])) {
        return qjs.JS_NewString(ctx, "");
    }

    var len: usize = undefined;
    const str = qjs.JS_ToCStringLen(ctx, &len, argv[0]);
    if (str == null) return qjs.JS_NewString(ctx, "");
    defer qjs.JS_FreeCString(ctx, str);

    if (len == 0) return qjs.JS_NewString(ctx, "");

    const bytes: []const u8 = @ptrCast(str[0..len]);

    // Allocate output buffer (2 hex chars per byte)
    const hex_len = len * 2;
    const buf = std.heap.page_allocator.alloc(u8, hex_len) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    defer std.heap.page_allocator.free(buf);

    // Convert bytes to hex
    for (bytes, 0..) |byte, i| {
        buf[i * 2] = hex_chars[byte >> 4];
        buf[i * 2 + 1] = hex_chars[byte & 0x0f];
    }

    return qjs.JS_NewStringLen(ctx, buf.ptr, hex_len);
}

/// bufferHexToString(hexString) - Convert hex string to UTF-8 string
fn bufferHexToString(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    if (!qjs.JS_IsString(argv[0])) {
        return qjs.JS_NewString(ctx, "");
    }

    var len: usize = undefined;
    const str = qjs.JS_ToCStringLen(ctx, &len, argv[0]);
    if (str == null) return qjs.JS_NewString(ctx, "");
    defer qjs.JS_FreeCString(ctx, str);

    if (len == 0) return qjs.JS_NewString(ctx, "");

    // Hex string must have even length
    if (len % 2 != 0) {
        return qjs.JS_ThrowSyntaxError(ctx, "Invalid hex string: odd length");
    }

    const data: []const u8 = @ptrCast(str[0..len]);
    const decoded_len = len / 2;

    const buf = std.heap.page_allocator.alloc(u8, decoded_len) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    defer std.heap.page_allocator.free(buf);

    // Decode hex pairs
    for (0..decoded_len) |i| {
        const hi = hexCharToValue(data[i * 2]);
        const lo = hexCharToValue(data[i * 2 + 1]);
        if (hi == 255 or lo == 255) {
            return qjs.JS_ThrowSyntaxError(ctx, "Invalid hex character");
        }
        buf[i] = (hi << 4) | lo;
    }

    return qjs.JS_NewStringLen(ctx, buf.ptr, decoded_len);
}

/// bufferAllocFill(size, fillByte) - Allocate buffer and fill with byte value
fn bufferAllocFill(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "bufferAllocFill requires size and fillByte arguments");

    var size: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &size, argv[0]) != 0) return quickjs.jsException();
    if (size < 0) return qjs.JS_ThrowRangeError(ctx, "size must be non-negative");

    var fill_byte: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &fill_byte, argv[1]) != 0) return quickjs.jsException();

    if (size == 0) {
        return createUint8Array(ctx, &[_]u8{});
    }

    const usize_size: usize = @intCast(size);
    const buf = std.heap.page_allocator.alloc(u8, usize_size) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    defer std.heap.page_allocator.free(buf);

    // Fill with byte value
    @memset(buf, @truncate(@as(u32, @bitCast(fill_byte))));

    return createUint8Array(ctx, buf);
}

/// bufferPackUInt32LE(array) - Pack uint32 array to buffer (little-endian)
fn bufferPackUInt32LE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "bufferPackUInt32LE requires an array argument");

    // Get array length
    const len_val = qjs.JS_GetPropertyStr(ctx, argv[0], "length");
    defer qjs.JS_FreeValue(ctx, len_val);

    var arr_len: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &arr_len, len_val) != 0) return quickjs.jsException();
    if (arr_len <= 0) return createUint8Array(ctx, &[_]u8{});

    const usize_len: usize = @intCast(arr_len);
    const buf = std.heap.page_allocator.alloc(u8, usize_len * 4) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    defer std.heap.page_allocator.free(buf);

    // Pack each uint32 in little-endian
    for (0..usize_len) |i| {
        const val = qjs.JS_GetPropertyUint32(ctx, argv[0], @intCast(i));
        defer qjs.JS_FreeValue(ctx, val);

        var num: u32 = 0;
        if (qjs.JS_ToUint32(ctx, &num, val) != 0) return quickjs.jsException();

        const le_bytes = std.mem.toBytes(std.mem.nativeToLittle(u32, num));
        @memcpy(buf[i * 4 ..][0..4], &le_bytes);
    }

    return createUint8Array(ctx, buf);
}

/// bufferPackUInt32BE(array) - Pack uint32 array to buffer (big-endian)
fn bufferPackUInt32BE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "bufferPackUInt32BE requires an array argument");

    const len_val = qjs.JS_GetPropertyStr(ctx, argv[0], "length");
    defer qjs.JS_FreeValue(ctx, len_val);

    var arr_len: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &arr_len, len_val) != 0) return quickjs.jsException();
    if (arr_len <= 0) return createUint8Array(ctx, &[_]u8{});

    const usize_len: usize = @intCast(arr_len);
    const buf = std.heap.page_allocator.alloc(u8, usize_len * 4) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    defer std.heap.page_allocator.free(buf);

    // Pack each uint32 in big-endian
    for (0..usize_len) |i| {
        const val = qjs.JS_GetPropertyUint32(ctx, argv[0], @intCast(i));
        defer qjs.JS_FreeValue(ctx, val);

        var num: u32 = 0;
        if (qjs.JS_ToUint32(ctx, &num, val) != 0) return quickjs.jsException();

        const be_bytes = std.mem.toBytes(std.mem.nativeToBig(u32, num));
        @memcpy(buf[i * 4 ..][0..4], &be_bytes);
    }

    return createUint8Array(ctx, buf);
}

/// bufferPackInt32LE(array) - Pack int32 array to buffer (little-endian)
fn bufferPackInt32LE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "bufferPackInt32LE requires an array argument");

    const len_val = qjs.JS_GetPropertyStr(ctx, argv[0], "length");
    defer qjs.JS_FreeValue(ctx, len_val);

    var arr_len: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &arr_len, len_val) != 0) return quickjs.jsException();
    if (arr_len <= 0) return createUint8Array(ctx, &[_]u8{});

    const usize_len: usize = @intCast(arr_len);
    const buf = std.heap.page_allocator.alloc(u8, usize_len * 4) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    defer std.heap.page_allocator.free(buf);

    // Pack each int32 in little-endian
    for (0..usize_len) |i| {
        const val = qjs.JS_GetPropertyUint32(ctx, argv[0], @intCast(i));
        defer qjs.JS_FreeValue(ctx, val);

        var num: i32 = 0;
        if (qjs.JS_ToInt32(ctx, &num, val) != 0) return quickjs.jsException();

        const le_bytes = std.mem.toBytes(std.mem.nativeToLittle(i32, num));
        @memcpy(buf[i * 4 ..][0..4], &le_bytes);
    }

    return createUint8Array(ctx, buf);
}

/// bufferPackInt32BE(array) - Pack int32 array to buffer (big-endian)
fn bufferPackInt32BE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "bufferPackInt32BE requires an array argument");

    const len_val = qjs.JS_GetPropertyStr(ctx, argv[0], "length");
    defer qjs.JS_FreeValue(ctx, len_val);

    var arr_len: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &arr_len, len_val) != 0) return quickjs.jsException();
    if (arr_len <= 0) return createUint8Array(ctx, &[_]u8{});

    const usize_len: usize = @intCast(arr_len);
    const buf = std.heap.page_allocator.alloc(u8, usize_len * 4) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    defer std.heap.page_allocator.free(buf);

    // Pack each int32 in big-endian
    for (0..usize_len) |i| {
        const val = qjs.JS_GetPropertyUint32(ctx, argv[0], @intCast(i));
        defer qjs.JS_FreeValue(ctx, val);

        var num: i32 = 0;
        if (qjs.JS_ToInt32(ctx, &num, val) != 0) return quickjs.jsException();

        const be_bytes = std.mem.toBytes(std.mem.nativeToBig(i32, num));
        @memcpy(buf[i * 4 ..][0..4], &be_bytes);
    }

    return createUint8Array(ctx, buf);
}

/// bufferUnpackUInt32LE(buffer) - Unpack buffer to uint32 array (little-endian)
fn bufferUnpackUInt32LE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "bufferUnpackUInt32LE requires a buffer argument");

    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_NewArray(ctx);
    if (bytes.len < 4) return qjs.JS_NewArray(ctx);

    const count = bytes.len / 4;
    const result = qjs.JS_NewArray(ctx);
    if (qjs.JS_IsException(result)) return result;

    for (0..count) |i| {
        const le_val = std.mem.bytesToValue(u32, bytes[i * 4 ..][0..4]);
        const native_val = std.mem.littleToNative(u32, le_val);
        _ = qjs.JS_SetPropertyUint32(ctx, result, @intCast(i), qjs.JS_NewUint32(ctx, native_val));
    }

    return result;
}

/// bufferUnpackUInt32BE(buffer) - Unpack buffer to uint32 array (big-endian)
fn bufferUnpackUInt32BE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "bufferUnpackUInt32BE requires a buffer argument");

    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_NewArray(ctx);
    if (bytes.len < 4) return qjs.JS_NewArray(ctx);

    const count = bytes.len / 4;
    const result = qjs.JS_NewArray(ctx);
    if (qjs.JS_IsException(result)) return result;

    for (0..count) |i| {
        const be_val = std.mem.bytesToValue(u32, bytes[i * 4 ..][0..4]);
        const native_val = std.mem.bigToNative(u32, be_val);
        _ = qjs.JS_SetPropertyUint32(ctx, result, @intCast(i), qjs.JS_NewUint32(ctx, native_val));
    }

    return result;
}

// =============================================================================
// BigInt read/write methods (64-bit integer support)
// =============================================================================

/// bufferReadBigInt64BE(buffer, offset) - Read signed 64-bit integer (big-endian)
fn bufferReadBigInt64BE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readBigInt64BE requires buffer argument");

    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");

    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0) return qjs.JS_ThrowRangeError(ctx, "offset must be non-negative");

    const uoffset: usize = @intCast(offset);
    if (uoffset + 8 > bytes.len) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");

    const be_val = std.mem.bytesToValue(u64, bytes[uoffset..][0..8]);
    const value: i64 = @bitCast(std.mem.bigToNative(u64, be_val));
    return qjs.JS_NewBigInt64(ctx, value);
}

/// bufferReadBigInt64LE(buffer, offset) - Read signed 64-bit integer (little-endian)
fn bufferReadBigInt64LE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readBigInt64LE requires buffer argument");

    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");

    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0) return qjs.JS_ThrowRangeError(ctx, "offset must be non-negative");

    const uoffset: usize = @intCast(offset);
    if (uoffset + 8 > bytes.len) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");

    const le_val = std.mem.bytesToValue(u64, bytes[uoffset..][0..8]);
    const value: i64 = @bitCast(std.mem.littleToNative(u64, le_val));
    return qjs.JS_NewBigInt64(ctx, value);
}

/// bufferReadBigUInt64BE(buffer, offset) - Read unsigned 64-bit integer (big-endian)
fn bufferReadBigUInt64BE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readBigUInt64BE requires buffer argument");

    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");

    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0) return qjs.JS_ThrowRangeError(ctx, "offset must be non-negative");

    const uoffset: usize = @intCast(offset);
    if (uoffset + 8 > bytes.len) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");

    const be_val = std.mem.bytesToValue(u64, bytes[uoffset..][0..8]);
    const value: u64 = std.mem.bigToNative(u64, be_val);
    return qjs.JS_NewBigUint64(ctx, value);
}

/// bufferReadBigUInt64LE(buffer, offset) - Read unsigned 64-bit integer (little-endian)
fn bufferReadBigUInt64LE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readBigUInt64LE requires buffer argument");

    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");

    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0) return qjs.JS_ThrowRangeError(ctx, "offset must be non-negative");

    const uoffset: usize = @intCast(offset);
    if (uoffset + 8 > bytes.len) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");

    const le_val = std.mem.bytesToValue(u64, bytes[uoffset..][0..8]);
    const value: u64 = std.mem.littleToNative(u64, le_val);
    return qjs.JS_NewBigUint64(ctx, value);
}

/// bufferWriteBigInt64BE(buffer, value, offset) - Write signed 64-bit integer (big-endian)
fn bufferWriteBigInt64BE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeBigInt64BE requires buffer and value arguments");

    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");

    var value: i64 = 0;
    if (qjs.JS_ToBigInt64(ctx, &value, argv[1]) != 0) return quickjs.jsException();

    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0) return qjs.JS_ThrowRangeError(ctx, "offset must be non-negative");

    const uoffset: usize = @intCast(offset);
    if (uoffset + 8 > bytes.len) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");

    const be_bytes = std.mem.toBytes(std.mem.nativeToBig(u64, @bitCast(value)));
    @memcpy(bytes[uoffset..][0..8], &be_bytes);
    return qjs.JS_NewInt32(ctx, @intCast(uoffset + 8));
}

/// bufferWriteBigInt64LE(buffer, value, offset) - Write signed 64-bit integer (little-endian)
fn bufferWriteBigInt64LE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeBigInt64LE requires buffer and value arguments");

    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");

    var value: i64 = 0;
    if (qjs.JS_ToBigInt64(ctx, &value, argv[1]) != 0) return quickjs.jsException();

    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0) return qjs.JS_ThrowRangeError(ctx, "offset must be non-negative");

    const uoffset: usize = @intCast(offset);
    if (uoffset + 8 > bytes.len) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");

    const le_bytes = std.mem.toBytes(std.mem.nativeToLittle(u64, @bitCast(value)));
    @memcpy(bytes[uoffset..][0..8], &le_bytes);
    return qjs.JS_NewInt32(ctx, @intCast(uoffset + 8));
}

/// bufferWriteBigUInt64BE(buffer, value, offset) - Write unsigned 64-bit integer (big-endian)
fn bufferWriteBigUInt64BE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeBigUInt64BE requires buffer and value arguments");

    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");

    var value: u64 = 0;
    if (qjs.JS_ToBigUint64(ctx, &value, argv[1]) != 0) return quickjs.jsException();

    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0) return qjs.JS_ThrowRangeError(ctx, "offset must be non-negative");

    const uoffset: usize = @intCast(offset);
    if (uoffset + 8 > bytes.len) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");

    const be_bytes = std.mem.toBytes(std.mem.nativeToBig(u64, value));
    @memcpy(bytes[uoffset..][0..8], &be_bytes);
    return qjs.JS_NewInt32(ctx, @intCast(uoffset + 8));
}

/// bufferWriteBigUInt64LE(buffer, value, offset) - Write unsigned 64-bit integer (little-endian)
fn bufferWriteBigUInt64LE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeBigUInt64LE requires buffer and value arguments");

    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");

    var value: u64 = 0;
    if (qjs.JS_ToBigUint64(ctx, &value, argv[1]) != 0) return quickjs.jsException();

    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0) return qjs.JS_ThrowRangeError(ctx, "offset must be non-negative");

    const uoffset: usize = @intCast(offset);
    if (uoffset + 8 > bytes.len) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");

    const le_bytes = std.mem.toBytes(std.mem.nativeToLittle(u64, value));
    @memcpy(bytes[uoffset..][0..8], &le_bytes);
    return qjs.JS_NewInt32(ctx, @intCast(uoffset + 8));
}

// ============ Buffer.fill ============

/// Buffer.fill(buffer, value, offset, end) - Fill buffer with value
fn bufferFill(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "fill requires buffer and value");

    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");

    // Get offset and end
    var offset: i32 = 0;
    var end: i32 = @intCast(bytes.len);
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (argc > 3) _ = qjs.JS_ToInt32(ctx, &end, argv[3]);

    if (offset < 0) offset = 0;
    if (end > @as(i32, @intCast(bytes.len))) end = @intCast(bytes.len);
    if (offset >= end) return argv[0];

    const uoffset: usize = @intCast(offset);
    const uend: usize = @intCast(end);

    // Check if value is a number
    if (qjs.JS_IsNumber(argv[1])) {
        var fill_val: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &fill_val, argv[1]);
        @memset(bytes[uoffset..uend], @truncate(@as(u32, @bitCast(fill_val))));
        return argv[0];
    }

    // Check if value is a string
    const str = qjs.JS_ToCString(ctx, argv[1]);
    if (str != null) {
        defer qjs.JS_FreeCString(ctx, str);
        const fill_data = std.mem.span(str);
        if (fill_data.len == 0) return argv[0];

        // Repeat fill pattern
        var i = uoffset;
        while (i < uend) : (i += 1) {
            bytes[i] = fill_data[(i - uoffset) % fill_data.len];
        }
        return argv[0];
    }

    // Check if value is a buffer/Uint8Array
    var fill_size: usize = 0;
    const fill_ptr = qjs.JS_GetArrayBuffer(ctx, &fill_size, argv[1]);
    if (fill_ptr != null and fill_size > 0) {
        const fill_data = @as([*]const u8, @ptrCast(fill_ptr))[0..fill_size];
        var i = uoffset;
        while (i < uend) : (i += 1) {
            bytes[i] = fill_data[(i - uoffset) % fill_data.len];
        }
        return argv[0];
    }

    return argv[0];
}

// ============ Buffer.swap16/32/64 ============

/// Buffer.swap16(buffer) - Swap byte order for 16-bit values
fn bufferSwap16(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "swap16 requires buffer");

    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    if (bytes.len % 2 != 0) return qjs.JS_ThrowRangeError(ctx, "Buffer size must be multiple of 2");

    var i: usize = 0;
    while (i < bytes.len) : (i += 2) {
        const tmp = bytes[i];
        bytes[i] = bytes[i + 1];
        bytes[i + 1] = tmp;
    }
    return argv[0];
}

/// Buffer.swap32(buffer) - Swap byte order for 32-bit values
fn bufferSwap32(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "swap32 requires buffer");

    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    if (bytes.len % 4 != 0) return qjs.JS_ThrowRangeError(ctx, "Buffer size must be multiple of 4");

    var i: usize = 0;
    while (i < bytes.len) : (i += 4) {
        const t0 = bytes[i];
        const t1 = bytes[i + 1];
        bytes[i] = bytes[i + 3];
        bytes[i + 1] = bytes[i + 2];
        bytes[i + 2] = t1;
        bytes[i + 3] = t0;
    }
    return argv[0];
}

/// Buffer.swap64(buffer) - Swap byte order for 64-bit values
fn bufferSwap64(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "swap64 requires buffer");

    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    if (bytes.len % 8 != 0) return qjs.JS_ThrowRangeError(ctx, "Buffer size must be multiple of 8");

    var i: usize = 0;
    while (i < bytes.len) : (i += 8) {
        var j: usize = 0;
        while (j < 4) : (j += 1) {
            const tmp = bytes[i + j];
            bytes[i + j] = bytes[i + 7 - j];
            bytes[i + 7 - j] = tmp;
        }
    }
    return argv[0];
}

// ============ UTF-8 validation methods ============

/// Buffer.isUtf8(buffer) - Validate UTF-8 encoding
/// Uses optimized byte-by-byte validation with proper multi-byte sequence checking
fn bufferIsUtf8(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "isUtf8 requires buffer");

    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");

    // Validate UTF-8 encoding
    var i: usize = 0;
    while (i < bytes.len) {
        const b = bytes[i];

        // ASCII (0x00-0x7F): single byte
        if (b <= 0x7F) {
            i += 1;
            continue;
        }

        // Determine sequence length and validate leading byte
        const seq_len: usize = blk: {
            if ((b & 0xE0) == 0xC0) break :blk 2; // 110xxxxx - 2 bytes
            if ((b & 0xF0) == 0xE0) break :blk 3; // 1110xxxx - 3 bytes
            if ((b & 0xF8) == 0xF0) break :blk 4; // 11110xxx - 4 bytes
            return quickjs.jsFalse(); // Invalid leading byte
        };

        // Check if we have enough bytes
        if (i + seq_len > bytes.len) return quickjs.jsFalse();

        // Validate continuation bytes (must be 10xxxxxx)
        for (1..seq_len) |j| {
            if ((bytes[i + j] & 0xC0) != 0x80) return quickjs.jsFalse();
        }

        // Check for overlong encodings and invalid ranges
        switch (seq_len) {
            2 => {
                // 2-byte: first byte must be >= 0xC2 (avoids overlong ASCII)
                if (b < 0xC2) return quickjs.jsFalse();
            },
            3 => {
                // 3-byte: check for overlong encoding (< U+0800)
                if (b == 0xE0 and bytes[i + 1] < 0xA0) return quickjs.jsFalse();
                // Check for surrogate halves (U+D800-U+DFFF)
                if (b == 0xED and bytes[i + 1] >= 0xA0) return quickjs.jsFalse();
            },
            4 => {
                // 4-byte: check for overlong encoding (< U+10000) and beyond Unicode (> U+10FFFF)
                if (b == 0xF0 and bytes[i + 1] < 0x90) return quickjs.jsFalse();
                if (b > 0xF4) return quickjs.jsFalse();
                if (b == 0xF4 and bytes[i + 1] > 0x8F) return quickjs.jsFalse();
            },
            else => return quickjs.jsFalse(),
        }

        i += seq_len;
    }

    return quickjs.jsTrue();
}

/// Buffer.isAscii(buffer) - Check if all bytes are ASCII (< 128)
fn bufferIsAscii(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "isAscii requires buffer");

    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");

    // Check all bytes are < 128
    for (bytes) |b| {
        if (b > 127) return quickjs.jsFalse();
    }

    return quickjs.jsTrue();
}

// ============ Buffer read/write integer methods ============

/// readInt8(buffer, offset)
fn bufferReadInt8(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readInt8 requires buffer");
    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0 or offset >= @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const val: i8 = @bitCast(bytes[@intCast(offset)]);
    return qjs.JS_NewInt32(ctx, val);
}

/// readUInt8(buffer, offset)
fn bufferReadUInt8(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readUInt8 requires buffer");
    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0 or offset >= @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    return qjs.JS_NewInt32(ctx, bytes[@intCast(offset)]);
}

/// readInt16LE(buffer, offset)
fn bufferReadInt16LE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readInt16LE requires buffer");
    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0 or offset + 2 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    const val = std.mem.readInt(i16, bytes[uoff..][0..2], .little);
    return qjs.JS_NewInt32(ctx, val);
}

/// readInt16BE(buffer, offset)
fn bufferReadInt16BE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readInt16BE requires buffer");
    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0 or offset + 2 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    const val = std.mem.readInt(i16, bytes[uoff..][0..2], .big);
    return qjs.JS_NewInt32(ctx, val);
}

/// readUInt16LE(buffer, offset)
fn bufferReadUInt16LE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readUInt16LE requires buffer");
    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0 or offset + 2 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    const val = std.mem.readInt(u16, bytes[uoff..][0..2], .little);
    return qjs.JS_NewInt32(ctx, val);
}

/// readUInt16BE(buffer, offset)
fn bufferReadUInt16BE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readUInt16BE requires buffer");
    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0 or offset + 2 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    const val = std.mem.readInt(u16, bytes[uoff..][0..2], .big);
    return qjs.JS_NewInt32(ctx, val);
}

/// readInt32LE(buffer, offset)
fn bufferReadInt32LE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readInt32LE requires buffer");
    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0 or offset + 4 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    const val = std.mem.readInt(i32, bytes[uoff..][0..4], .little);
    return qjs.JS_NewInt32(ctx, val);
}

/// readInt32BE(buffer, offset)
fn bufferReadInt32BE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readInt32BE requires buffer");
    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0 or offset + 4 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    const val = std.mem.readInt(i32, bytes[uoff..][0..4], .big);
    return qjs.JS_NewInt32(ctx, val);
}

/// readUInt32LE(buffer, offset)
fn bufferReadUInt32LE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readUInt32LE requires buffer");
    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0 or offset + 4 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    const val = std.mem.readInt(u32, bytes[uoff..][0..4], .little);
    return qjs.JS_NewFloat64(ctx, @floatFromInt(val)); // Use float for u32 to avoid sign issues
}

/// readUInt32BE(buffer, offset)
fn bufferReadUInt32BE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readUInt32BE requires buffer");
    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0 or offset + 4 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    const val = std.mem.readInt(u32, bytes[uoff..][0..4], .big);
    return qjs.JS_NewFloat64(ctx, @floatFromInt(val));
}

/// readFloatLE(buffer, offset)
fn bufferReadFloatLE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readFloatLE requires buffer");
    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0 or offset + 4 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    const val: f32 = @bitCast(std.mem.readInt(u32, bytes[uoff..][0..4], .little));
    return qjs.JS_NewFloat64(ctx, val);
}

/// readFloatBE(buffer, offset)
fn bufferReadFloatBE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readFloatBE requires buffer");
    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0 or offset + 4 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    const val: f32 = @bitCast(std.mem.readInt(u32, bytes[uoff..][0..4], .big));
    return qjs.JS_NewFloat64(ctx, val);
}

/// readDoubleLE(buffer, offset)
fn bufferReadDoubleLE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readDoubleLE requires buffer");
    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0 or offset + 8 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    const val: f64 = @bitCast(std.mem.readInt(u64, bytes[uoff..][0..8], .little));
    return qjs.JS_NewFloat64(ctx, val);
}

/// readDoubleBE(buffer, offset)
fn bufferReadDoubleBE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "readDoubleBE requires buffer");
    const bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var offset: i32 = 0;
    if (argc > 1) _ = qjs.JS_ToInt32(ctx, &offset, argv[1]);
    if (offset < 0 or offset + 8 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    const val: f64 = @bitCast(std.mem.readInt(u64, bytes[uoff..][0..8], .big));
    return qjs.JS_NewFloat64(ctx, val);
}

/// writeInt8(buffer, value, offset)
fn bufferWriteInt8(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeInt8 requires buffer and value");
    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var value: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &value, argv[1]);
    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0 or offset >= @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    bytes[@intCast(offset)] = @bitCast(@as(i8, @truncate(value)));
    return qjs.JS_NewInt32(ctx, offset + 1);
}

/// writeUInt8(buffer, value, offset)
fn bufferWriteUInt8(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeUInt8 requires buffer and value");
    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var value: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &value, argv[1]);
    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0 or offset >= @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    bytes[@intCast(offset)] = @truncate(@as(u32, @bitCast(value)));
    return qjs.JS_NewInt32(ctx, offset + 1);
}

/// writeInt16LE(buffer, value, offset)
fn bufferWriteInt16LE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeInt16LE requires buffer and value");
    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var value: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &value, argv[1]);
    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0 or offset + 2 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    std.mem.writeInt(i16, bytes[uoff..][0..2], @truncate(value), .little);
    return qjs.JS_NewInt32(ctx, offset + 2);
}

/// writeInt16BE(buffer, value, offset)
fn bufferWriteInt16BE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeInt16BE requires buffer and value");
    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var value: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &value, argv[1]);
    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0 or offset + 2 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    std.mem.writeInt(i16, bytes[uoff..][0..2], @truncate(value), .big);
    return qjs.JS_NewInt32(ctx, offset + 2);
}

/// writeUInt16LE(buffer, value, offset)
fn bufferWriteUInt16LE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeUInt16LE requires buffer and value");
    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var value: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &value, argv[1]);
    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0 or offset + 2 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    std.mem.writeInt(u16, bytes[uoff..][0..2], @truncate(@as(u32, @bitCast(value))), .little);
    return qjs.JS_NewInt32(ctx, offset + 2);
}

/// writeUInt16BE(buffer, value, offset)
fn bufferWriteUInt16BE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeUInt16BE requires buffer and value");
    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var value: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &value, argv[1]);
    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0 or offset + 2 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    std.mem.writeInt(u16, bytes[uoff..][0..2], @truncate(@as(u32, @bitCast(value))), .big);
    return qjs.JS_NewInt32(ctx, offset + 2);
}

/// writeInt32LE(buffer, value, offset)
fn bufferWriteInt32LE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeInt32LE requires buffer and value");
    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var value: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &value, argv[1]);
    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0 or offset + 4 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    std.mem.writeInt(i32, bytes[uoff..][0..4], value, .little);
    return qjs.JS_NewInt32(ctx, offset + 4);
}

/// writeInt32BE(buffer, value, offset)
fn bufferWriteInt32BE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeInt32BE requires buffer and value");
    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var value: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &value, argv[1]);
    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0 or offset + 4 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    std.mem.writeInt(i32, bytes[uoff..][0..4], value, .big);
    return qjs.JS_NewInt32(ctx, offset + 4);
}

/// writeUInt32LE(buffer, value, offset)
fn bufferWriteUInt32LE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeUInt32LE requires buffer and value");
    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var value: f64 = 0;
    _ = qjs.JS_ToFloat64(ctx, &value, argv[1]);
    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0 or offset + 4 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    std.mem.writeInt(u32, bytes[uoff..][0..4], @intFromFloat(value), .little);
    return qjs.JS_NewInt32(ctx, offset + 4);
}

/// writeUInt32BE(buffer, value, offset)
fn bufferWriteUInt32BE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeUInt32BE requires buffer and value");
    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var value: f64 = 0;
    _ = qjs.JS_ToFloat64(ctx, &value, argv[1]);
    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0 or offset + 4 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    std.mem.writeInt(u32, bytes[uoff..][0..4], @intFromFloat(value), .big);
    return qjs.JS_NewInt32(ctx, offset + 4);
}

/// writeFloatLE(buffer, value, offset)
fn bufferWriteFloatLE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeFloatLE requires buffer and value");
    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var value: f64 = 0;
    _ = qjs.JS_ToFloat64(ctx, &value, argv[1]);
    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0 or offset + 4 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    const f32_val: f32 = @floatCast(value);
    std.mem.writeInt(u32, bytes[uoff..][0..4], @bitCast(f32_val), .little);
    return qjs.JS_NewInt32(ctx, offset + 4);
}

/// writeFloatBE(buffer, value, offset)
fn bufferWriteFloatBE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeFloatBE requires buffer and value");
    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var value: f64 = 0;
    _ = qjs.JS_ToFloat64(ctx, &value, argv[1]);
    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0 or offset + 4 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    const f32_val: f32 = @floatCast(value);
    std.mem.writeInt(u32, bytes[uoff..][0..4], @bitCast(f32_val), .big);
    return qjs.JS_NewInt32(ctx, offset + 4);
}

/// writeDoubleLE(buffer, value, offset)
fn bufferWriteDoubleLE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeDoubleLE requires buffer and value");
    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var value: f64 = 0;
    _ = qjs.JS_ToFloat64(ctx, &value, argv[1]);
    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0 or offset + 8 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    std.mem.writeInt(u64, bytes[uoff..][0..8], @bitCast(value), .little);
    return qjs.JS_NewInt32(ctx, offset + 8);
}

/// writeDoubleBE(buffer, value, offset)
fn bufferWriteDoubleBE(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "writeDoubleBE requires buffer and value");
    const bytes = getMutableBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid buffer");
    var value: f64 = 0;
    _ = qjs.JS_ToFloat64(ctx, &value, argv[1]);
    var offset: i32 = 0;
    if (argc > 2) _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    if (offset < 0 or offset + 8 > @as(i32, @intCast(bytes.len))) return qjs.JS_ThrowRangeError(ctx, "offset out of bounds");
    const uoff: usize = @intCast(offset);
    std.mem.writeInt(u64, bytes[uoff..][0..8], @bitCast(value), .big);
    return qjs.JS_NewInt32(ctx, offset + 8);
}

/// bufferTranscode(source, fromEncoding, toEncoding) - Transcode buffer between encodings
/// Supports: utf8, utf16le, latin1, ascii
fn bufferTranscode(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) return qjs.JS_ThrowTypeError(ctx, "transcode requires source, fromEncoding, toEncoding");

    const source_bytes = getBufferBytes(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "Invalid source buffer");

    const from_enc = qjs.JS_ToCString(ctx, argv[1]);
    if (from_enc == null) return qjs.JS_ThrowTypeError(ctx, "fromEncoding must be a string");
    defer qjs.JS_FreeCString(ctx, from_enc);

    const to_enc = qjs.JS_ToCString(ctx, argv[2]);
    if (to_enc == null) return qjs.JS_ThrowTypeError(ctx, "toEncoding must be a string");
    defer qjs.JS_FreeCString(ctx, to_enc);

    const from_slice = std.mem.span(from_enc);
    const to_slice = std.mem.span(to_enc);

    // Helper to match encoding names (case-insensitive)
    const Encoding = enum { utf8, utf16le, latin1, ascii, unknown };
    const parseEnc = struct {
        fn parse(enc: []const u8) Encoding {
            if (std.ascii.eqlIgnoreCase(enc, "utf8") or std.ascii.eqlIgnoreCase(enc, "utf-8")) return .utf8;
            if (std.ascii.eqlIgnoreCase(enc, "utf16le") or std.ascii.eqlIgnoreCase(enc, "ucs2") or std.ascii.eqlIgnoreCase(enc, "ucs-2")) return .utf16le;
            if (std.ascii.eqlIgnoreCase(enc, "latin1") or std.ascii.eqlIgnoreCase(enc, "binary")) return .latin1;
            if (std.ascii.eqlIgnoreCase(enc, "ascii")) return .ascii;
            return .unknown;
        }
    }.parse;

    const from = parseEnc(from_slice);
    const to = parseEnc(to_slice);

    if (from == .unknown or to == .unknown) {
        return qjs.JS_ThrowTypeError(ctx, "Unsupported encoding");
    }

    // Same encoding - just copy
    if (from == to) {
        return qjs.JS_NewArrayBufferCopy(ctx, source_bytes.ptr, source_bytes.len);
    }

    // Use static buffer for transcoding
    const result_slice = &transcode_buffer;

    // utf8 -> latin1: truncate codepoints to single bytes (0-255)
    if (from == .utf8 and (to == .latin1 or to == .ascii)) {
        var i: usize = 0;
        var out_idx: usize = 0;
        while (i < source_bytes.len and out_idx < transcode_buffer.len) {
            const byte = source_bytes[i];
            if (byte < 0x80) {
                result_slice[out_idx] = byte;
                i += 1;
            } else if (byte < 0xE0 and i + 1 < source_bytes.len) {
                // 2-byte sequence: 110xxxxx 10xxxxxx
                const cp = (@as(u16, byte & 0x1F) << 6) | @as(u16, source_bytes[i + 1] & 0x3F);
                result_slice[out_idx] = @truncate(cp);
                i += 2;
            } else if (byte < 0xF0 and i + 2 < source_bytes.len) {
                // 3-byte sequence
                const cp = (@as(u16, byte & 0x0F) << 12) |
                    (@as(u16, source_bytes[i + 1] & 0x3F) << 6) |
                    @as(u16, source_bytes[i + 2] & 0x3F);
                result_slice[out_idx] = @truncate(cp);
                i += 3;
            } else if (i + 3 < source_bytes.len) {
                // 4-byte sequence - truncate to replacement char
                result_slice[out_idx] = '?';
                i += 4;
            } else {
                break;
            }
            out_idx += 1;
        }
        return qjs.JS_NewArrayBufferCopy(ctx, result_slice, out_idx);
    }

    // latin1/ascii -> utf8: expand bytes to utf8
    if ((from == .latin1 or from == .ascii) and to == .utf8) {
        var out_idx: usize = 0;
        for (source_bytes) |byte| {
            if (out_idx + 2 > transcode_buffer.len) break;
            if (byte < 0x80) {
                result_slice[out_idx] = byte;
                out_idx += 1;
            } else {
                // 2-byte UTF-8: 110xxxxx 10xxxxxx
                result_slice[out_idx] = 0xC0 | (byte >> 6);
                result_slice[out_idx + 1] = 0x80 | (byte & 0x3F);
                out_idx += 2;
            }
        }
        return qjs.JS_NewArrayBufferCopy(ctx, result_slice, out_idx);
    }

    // utf16le -> utf8
    if (from == .utf16le and to == .utf8) {
        if (source_bytes.len % 2 != 0) {
            return qjs.JS_ThrowTypeError(ctx, "Invalid UTF-16LE buffer length");
        }

        var out_idx: usize = 0;
        var i: usize = 0;
        while (i < source_bytes.len and out_idx + 3 <= transcode_buffer.len) : (i += 2) {
            const cp: u16 = @as(u16, source_bytes[i]) | (@as(u16, source_bytes[i + 1]) << 8);

            if (cp < 0x80) {
                result_slice[out_idx] = @truncate(cp);
                out_idx += 1;
            } else if (cp < 0x800) {
                result_slice[out_idx] = @truncate(0xC0 | (cp >> 6));
                result_slice[out_idx + 1] = @truncate(0x80 | (cp & 0x3F));
                out_idx += 2;
            } else {
                result_slice[out_idx] = @truncate(0xE0 | (cp >> 12));
                result_slice[out_idx + 1] = @truncate(0x80 | ((cp >> 6) & 0x3F));
                result_slice[out_idx + 2] = @truncate(0x80 | (cp & 0x3F));
                out_idx += 3;
            }
        }
        return qjs.JS_NewArrayBufferCopy(ctx, result_slice, out_idx);
    }

    // utf8 -> utf16le
    if (from == .utf8 and to == .utf16le) {
        var i: usize = 0;
        var out_idx: usize = 0;
        while (i < source_bytes.len and out_idx + 2 <= transcode_buffer.len) {
            const byte = source_bytes[i];
            var cp: u16 = 0;

            if (byte < 0x80) {
                cp = byte;
                i += 1;
            } else if (byte < 0xE0 and i + 1 < source_bytes.len) {
                cp = (@as(u16, byte & 0x1F) << 6) | @as(u16, source_bytes[i + 1] & 0x3F);
                i += 2;
            } else if (byte < 0xF0 and i + 2 < source_bytes.len) {
                cp = (@as(u16, byte & 0x0F) << 12) |
                    (@as(u16, source_bytes[i + 1] & 0x3F) << 6) |
                    @as(u16, source_bytes[i + 2] & 0x3F);
                i += 3;
            } else if (i + 3 < source_bytes.len) {
                cp = 0xFFFD; // Replacement character for 4-byte sequences
                i += 4;
            } else {
                break;
            }

            // Write as little-endian
            result_slice[out_idx] = @truncate(cp);
            result_slice[out_idx + 1] = @truncate(cp >> 8);
            out_idx += 2;
        }
        return qjs.JS_NewArrayBufferCopy(ctx, result_slice, out_idx);
    }

    return qjs.JS_ThrowTypeError(ctx, "Unsupported encoding combination");
}

/// Buffer.byteLength(string, encoding) - Calculate byte length of string in encoding
/// Supports: utf8, utf-8, utf16le, ucs2, base64, base64url, hex, latin1, binary, ascii
fn bufferByteLength(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewInt32(ctx, 0);

    // If it's a buffer/TypedArray, return its byteLength
    if (!qjs.JS_IsString(argv[0])) {
        const bytes = getBufferBytes(ctx, argv[0]);
        if (bytes) |b| {
            return qjs.JS_NewInt32(ctx, @intCast(b.len));
        }
        return qjs.JS_NewInt32(ctx, 0);
    }

    // Get string and encoding
    var str_len: usize = undefined;
    const str = qjs.JS_ToCStringLen(ctx, &str_len, argv[0]);
    if (str == null) return qjs.JS_NewInt32(ctx, 0);
    defer qjs.JS_FreeCString(ctx, str);

    // Default encoding is UTF-8
    var encoding: []const u8 = "utf8";
    if (argc >= 2 and qjs.JS_IsString(argv[1])) {
        const enc_str = qjs.JS_ToCString(ctx, argv[1]);
        if (enc_str != null) {
            encoding = std.mem.span(enc_str);
            qjs.JS_FreeCString(ctx, enc_str);
        }
    }

    // Calculate byte length based on encoding
    if (std.ascii.eqlIgnoreCase(encoding, "utf8") or std.ascii.eqlIgnoreCase(encoding, "utf-8")) {
        // UTF-8: string bytes are already UTF-8 in QuickJS
        return qjs.JS_NewInt32(ctx, @intCast(str_len));
    } else if (std.ascii.eqlIgnoreCase(encoding, "utf16le") or std.ascii.eqlIgnoreCase(encoding, "ucs2") or std.ascii.eqlIgnoreCase(encoding, "ucs-2")) {
        // UTF-16LE: 2 bytes per BMP codepoint, 4 bytes for supplementary planes (surrogate pairs)
        var byte_count: usize = 0;
        const data: []const u8 = @ptrCast(str[0..str_len]);
        var i: usize = 0;
        while (i < str_len) {
            const byte = data[i];
            if (byte < 0x80) {
                i += 1;
                byte_count += 2; // BMP: 2 bytes
            } else if (byte < 0xE0) {
                i += 2;
                byte_count += 2; // BMP: 2 bytes
            } else if (byte < 0xF0) {
                i += 3;
                byte_count += 2; // BMP: 2 bytes
            } else {
                i += 4;
                byte_count += 4; // Supplementary plane: surrogate pair (4 bytes)
            }
        }
        return qjs.JS_NewInt32(ctx, @intCast(byte_count));
    } else if (std.ascii.eqlIgnoreCase(encoding, "base64") or std.ascii.eqlIgnoreCase(encoding, "base64url")) {
        // Base64: calculate decoded size from base64 string
        // Strip padding and calculate
        var padded_len = str_len;
        const data: []const u8 = @ptrCast(str[0..str_len]);
        while (padded_len > 0 and data[padded_len - 1] == '=') {
            padded_len -= 1;
        }
        const decoded_len = (padded_len * 3) / 4;
        return qjs.JS_NewInt32(ctx, @intCast(decoded_len));
    } else if (std.ascii.eqlIgnoreCase(encoding, "hex")) {
        // Hex: 2 characters per byte
        return qjs.JS_NewInt32(ctx, @intCast(str_len / 2));
    } else if (std.ascii.eqlIgnoreCase(encoding, "latin1") or std.ascii.eqlIgnoreCase(encoding, "binary") or std.ascii.eqlIgnoreCase(encoding, "ascii")) {
        // Latin1/Binary/ASCII: 1 byte per character (codepoint count)
        var codepoints: usize = 0;
        const data: []const u8 = @ptrCast(str[0..str_len]);
        var i: usize = 0;
        while (i < str_len) {
            const byte = data[i];
            if (byte < 0x80) {
                i += 1;
            } else if (byte < 0xE0) {
                i += 2;
            } else if (byte < 0xF0) {
                i += 3;
            } else {
                i += 4;
            }
            codepoints += 1;
        }
        return qjs.JS_NewInt32(ctx, @intCast(codepoints));
    }

    // Unknown encoding - return UTF-8 length as default
    return qjs.JS_NewInt32(ctx, @intCast(str_len));
}

/// buffer.includes(value, byteOffset, encoding) - Check if buffer includes value
/// Returns true if the value is found in the buffer
fn bufferIncludes(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return quickjs.jsFalse();

    const haystack = getBufferBytes(ctx, argv[0]) orelse return quickjs.jsFalse();

    // Get byte offset (default 0)
    var byte_offset: i32 = 0;
    if (argc > 2 and qjs.JS_IsNumber(argv[2])) {
        _ = qjs.JS_ToInt32(ctx, &byte_offset, argv[2]);
    }
    if (byte_offset < 0) byte_offset = @max(0, @as(i32, @intCast(haystack.len)) + byte_offset);
    const start: usize = @intCast(@min(byte_offset, @as(i32, @intCast(haystack.len))));

    if (start >= haystack.len) return quickjs.jsFalse();

    // If value is a number (single byte)
    if (qjs.JS_IsNumber(argv[1])) {
        var num: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &num, argv[1]);
        const byte: u8 = @truncate(@as(u32, @bitCast(num)));
        for (haystack[start..]) |b| {
            if (b == byte) return quickjs.jsTrue();
        }
        return quickjs.jsFalse();
    }

    // If value is a string
    if (qjs.JS_IsString(argv[1])) {
        var len: usize = undefined;
        const str = qjs.JS_ToCStringLen(ctx, &len, argv[1]);
        if (str == null or len == 0) return quickjs.jsFalse();
        defer qjs.JS_FreeCString(ctx, str);

        const needle: []const u8 = @ptrCast(str[0..len]);
        if (std.mem.indexOf(u8, haystack[start..], needle) != null) {
            return quickjs.jsTrue();
        }
        return quickjs.jsFalse();
    }

    // If value is a buffer
    const needle = getBufferBytes(ctx, argv[1]) orelse return quickjs.jsFalse();
    if (needle.len == 0) return quickjs.jsTrue();

    if (std.mem.indexOf(u8, haystack[start..], needle) != null) {
        return quickjs.jsTrue();
    }
    return quickjs.jsFalse();
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
        .{ "allocFill", bufferAllocFill, 2 },
        .{ "concat", bufferConcat, 2 },
        .{ "isBuffer", bufferIsBuffer, 1 },
        // Fast subarray - bypasses speciesCreate overhead - 4.6x faster
        .{ "subarray", bufferSubarray, 3 },
        // Buffer comparison and equality
        .{ "compare", bufferCompare, 2 },
        .{ "equals", bufferEquals, 2 },
        // Buffer copy - uses @memcpy for bulk operations
        .{ "copy", bufferCopy, 5 },
        // Buffer search - uses std.mem.indexOf/lastIndexOf
        .{ "indexOf", bufferIndexOf, 3 },
        .{ "lastIndexOf", bufferLastIndexOf, 3 },
        // UTF-8 string conversion (uses QuickJS internal UTF-8) - 2400x faster
        .{ "toUtf8String", bufferToUtf8String, 1 },
        .{ "fromUtf8String", bufferFromUtf8String, 1 },
        // Base64 encoding/decoding (uses std.base64) - 811x faster
        .{ "toBase64", bufferToBase64, 1 },
        .{ "fromBase64", bufferFromBase64, 1 },
        // Hex encoding/decoding
        .{ "toHex", bufferToHex, 1 },
        .{ "fromHex", bufferFromHex, 1 },
        .{ "stringToHex", bufferStringToHex, 1 },
        .{ "hexToString", bufferHexToString, 1 },
        // Binary pack/unpack for Int32 (little-endian and big-endian)
        .{ "packUInt32LE", bufferPackUInt32LE, 1 },
        .{ "packUInt32BE", bufferPackUInt32BE, 1 },
        .{ "packInt32LE", bufferPackInt32LE, 1 },
        .{ "packInt32BE", bufferPackInt32BE, 1 },
        .{ "unpackUInt32LE", bufferUnpackUInt32LE, 1 },
        .{ "unpackUInt32BE", bufferUnpackUInt32BE, 1 },
        // BigInt 64-bit read/write methods
        .{ "readBigInt64BE", bufferReadBigInt64BE, 2 },
        .{ "readBigInt64LE", bufferReadBigInt64LE, 2 },
        .{ "readBigUInt64BE", bufferReadBigUInt64BE, 2 },
        .{ "readBigUInt64LE", bufferReadBigUInt64LE, 2 },
        .{ "writeBigInt64BE", bufferWriteBigInt64BE, 3 },
        .{ "writeBigInt64LE", bufferWriteBigInt64LE, 3 },
        .{ "writeBigUInt64BE", bufferWriteBigUInt64BE, 3 },
        .{ "writeBigUInt64LE", bufferWriteBigUInt64LE, 3 },
        // Buffer fill and swap
        .{ "fill", bufferFill, 4 },
        .{ "swap16", bufferSwap16, 1 },
        .{ "swap32", bufferSwap32, 1 },
        .{ "swap64", bufferSwap64, 1 },
        // UTF-8/ASCII validation
        .{ "isUtf8", bufferIsUtf8, 1 },
        .{ "isAscii", bufferIsAscii, 1 },
        // Buffer read integer methods
        .{ "readInt8", bufferReadInt8, 2 },
        .{ "readUInt8", bufferReadUInt8, 2 },
        .{ "readInt16LE", bufferReadInt16LE, 2 },
        .{ "readInt16BE", bufferReadInt16BE, 2 },
        .{ "readUInt16LE", bufferReadUInt16LE, 2 },
        .{ "readUInt16BE", bufferReadUInt16BE, 2 },
        .{ "readInt32LE", bufferReadInt32LE, 2 },
        .{ "readInt32BE", bufferReadInt32BE, 2 },
        .{ "readUInt32LE", bufferReadUInt32LE, 2 },
        .{ "readUInt32BE", bufferReadUInt32BE, 2 },
        .{ "readFloatLE", bufferReadFloatLE, 2 },
        .{ "readFloatBE", bufferReadFloatBE, 2 },
        .{ "readDoubleLE", bufferReadDoubleLE, 2 },
        .{ "readDoubleBE", bufferReadDoubleBE, 2 },
        // Buffer write integer methods
        .{ "writeInt8", bufferWriteInt8, 3 },
        .{ "writeUInt8", bufferWriteUInt8, 3 },
        .{ "writeInt16LE", bufferWriteInt16LE, 3 },
        .{ "writeInt16BE", bufferWriteInt16BE, 3 },
        .{ "writeUInt16LE", bufferWriteUInt16LE, 3 },
        .{ "writeUInt16BE", bufferWriteUInt16BE, 3 },
        .{ "writeInt32LE", bufferWriteInt32LE, 3 },
        .{ "writeInt32BE", bufferWriteInt32BE, 3 },
        .{ "writeUInt32LE", bufferWriteUInt32LE, 3 },
        .{ "writeUInt32BE", bufferWriteUInt32BE, 3 },
        .{ "writeFloatLE", bufferWriteFloatLE, 3 },
        .{ "writeFloatBE", bufferWriteFloatBE, 3 },
        .{ "writeDoubleLE", bufferWriteDoubleLE, 3 },
        .{ "writeDoubleBE", bufferWriteDoubleBE, 3 },
        // Buffer transcode
        .{ "transcode", bufferTranscode, 3 },
        // Buffer.byteLength and includes
        .{ "byteLength", bufferByteLength, 2 },
        .{ "includes", bufferIncludes, 4 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, native_buffer, binding[0], func);
    }

    // Store as _modules._nativeBuffer (available for future optimization)
    _ = qjs.JS_SetPropertyStr(ctx, modules, "_nativeBuffer", native_buffer);
}
