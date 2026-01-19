/// Native compression module - QuickJS C functions
/// Uses libdeflate for real gzip/deflate compression (native builds only)
/// Brotli uses stored blocks (no brotli library linked)
const std = @import("std");
const builtin = @import("builtin");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;
const flate = std.compress.flate;
const Io = std.Io;

// libdeflate C bindings
const libdeflate = @cImport({
    @cInclude("libdeflate.h");
});

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

/// Create Uint8Array from bytes
fn createUint8Array(ctx: ?*qjs.JSContext, data: []const u8) qjs.JSValue {
    const array_buf = qjs.JS_NewArrayBufferCopy(ctx, data.ptr, data.len);
    if (qjs.JS_IsException(array_buf)) return array_buf;

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    var ctor_args = [1]qjs.JSValue{array_buf};
    return qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
}

/// GZIP constants
const GZIP_MAGIC: [2]u8 = .{ 0x1f, 0x8b };

/// Read all data from a Reader into a dynamically allocated buffer
fn readAllFromReader(reader: *Io.Reader, allocator: std.mem.Allocator, max_size: usize) ![]u8 {
    return reader.allocRemaining(allocator, .limited(max_size)) catch |err| switch (err) {
        error.StreamTooLong => return error.OutOfMemory,
        error.OutOfMemory => return error.OutOfMemory,
        error.ReadFailed => return error.InvalidData,
    };
}

/// gzip(data) - Compress data using gzip format with libdeflate
fn gzipFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "gzip requires input data");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    // Allocate compressor (level 6 = default, good balance)
    const compressor = libdeflate.libdeflate_alloc_compressor(6);
    if (compressor == null) {
        return qjs.JS_ThrowOutOfMemory(ctx);
    }
    defer libdeflate.libdeflate_free_compressor(compressor);

    // Get worst-case output size
    const max_size = libdeflate.libdeflate_gzip_compress_bound(compressor, input.len);

    // Allocate output buffer
    const output = std.heap.page_allocator.alloc(u8, max_size) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    defer std.heap.page_allocator.free(output);

    // Compress
    const compressed_size = libdeflate.libdeflate_gzip_compress(
        compressor,
        input.ptr,
        input.len,
        output.ptr,
        output.len,
    );

    if (compressed_size == 0) {
        return qjs.JS_ThrowInternalError(ctx, "gzip compression failed");
    }

    return createUint8Array(ctx, output[0..compressed_size]);
}

/// gunzip(data) - Decompress gzip data using Zig std.compress (works with any gzip)
fn gunzipFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "gunzip requires input data");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    // Verify gzip header
    if (input.len < 18) {
        return qjs.JS_ThrowSyntaxError(ctx, "Invalid gzip data - too short");
    }
    if (input[0] != GZIP_MAGIC[0] or input[1] != GZIP_MAGIC[1]) {
        return qjs.JS_ThrowSyntaxError(ctx, "Invalid gzip magic bytes");
    }

    // Use Zig's decompression (handles any valid gzip)
    var reader = Io.Reader.fixed(input);
    var window_buffer: [flate.max_window_len]u8 = undefined;
    var decompress: flate.Decompress = .init(&reader, .gzip, &window_buffer);

    const decompressed = readAllFromReader(&decompress.reader, std.heap.page_allocator, 100 * 1024 * 1024) catch |err| {
        return switch (err) {
            error.OutOfMemory => qjs.JS_ThrowOutOfMemory(ctx),
            else => qjs.JS_ThrowSyntaxError(ctx, "Invalid gzip data"),
        };
    };
    defer std.heap.page_allocator.free(decompressed);

    return createUint8Array(ctx, decompressed);
}

/// deflate(data) - Compress data using raw deflate with libdeflate
fn deflateFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "deflate requires input data");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    const compressor = libdeflate.libdeflate_alloc_compressor(6);
    if (compressor == null) {
        return qjs.JS_ThrowOutOfMemory(ctx);
    }
    defer libdeflate.libdeflate_free_compressor(compressor);

    const max_size = libdeflate.libdeflate_deflate_compress_bound(compressor, input.len);

    const output = std.heap.page_allocator.alloc(u8, max_size) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    defer std.heap.page_allocator.free(output);

    const compressed_size = libdeflate.libdeflate_deflate_compress(
        compressor,
        input.ptr,
        input.len,
        output.ptr,
        output.len,
    );

    if (compressed_size == 0) {
        return qjs.JS_ThrowInternalError(ctx, "deflate compression failed");
    }

    return createUint8Array(ctx, output[0..compressed_size]);
}

/// inflate(data) - Decompress raw deflate data
fn inflateFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "inflate requires input data");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    var reader = Io.Reader.fixed(input);
    var window_buffer: [flate.max_window_len]u8 = undefined;
    var decompress: flate.Decompress = .init(&reader, .raw, &window_buffer);

    const decompressed = readAllFromReader(&decompress.reader, std.heap.page_allocator, 100 * 1024 * 1024) catch |err| {
        return switch (err) {
            error.OutOfMemory => qjs.JS_ThrowOutOfMemory(ctx),
            else => qjs.JS_ThrowSyntaxError(ctx, "Invalid deflate data"),
        };
    };
    defer std.heap.page_allocator.free(decompressed);

    return createUint8Array(ctx, decompressed);
}

/// deflateZlib(data) - Compress data using zlib wrapper with libdeflate
fn deflateZlibFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "deflateZlib requires input data");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    const compressor = libdeflate.libdeflate_alloc_compressor(6);
    if (compressor == null) {
        return qjs.JS_ThrowOutOfMemory(ctx);
    }
    defer libdeflate.libdeflate_free_compressor(compressor);

    const max_size = libdeflate.libdeflate_zlib_compress_bound(compressor, input.len);

    const output = std.heap.page_allocator.alloc(u8, max_size) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    defer std.heap.page_allocator.free(output);

    const compressed_size = libdeflate.libdeflate_zlib_compress(
        compressor,
        input.ptr,
        input.len,
        output.ptr,
        output.len,
    );

    if (compressed_size == 0) {
        return qjs.JS_ThrowInternalError(ctx, "zlib compression failed");
    }

    return createUint8Array(ctx, output[0..compressed_size]);
}

/// inflateZlib(data) - Decompress zlib-wrapped data
fn inflateZlibFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "inflateZlib requires input data");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    if (input.len < 6) {
        return qjs.JS_ThrowSyntaxError(ctx, "Invalid zlib data - too short");
    }

    var reader = Io.Reader.fixed(input);
    var window_buffer: [flate.max_window_len]u8 = undefined;
    var decompress: flate.Decompress = .init(&reader, .zlib, &window_buffer);

    const decompressed = readAllFromReader(&decompress.reader, std.heap.page_allocator, 100 * 1024 * 1024) catch |err| {
        return switch (err) {
            error.OutOfMemory => qjs.JS_ThrowOutOfMemory(ctx),
            else => qjs.JS_ThrowSyntaxError(ctx, "Invalid zlib data"),
        };
    };
    defer std.heap.page_allocator.free(decompressed);

    return createUint8Array(ctx, decompressed);
}

// =============================================================================
// Brotli - stored blocks only (no brotli library)
// =============================================================================

/// Write brotli uncompressed format (stored blocks)
fn writeBrotliUncompressed(result: *std.ArrayList(u8), input: []const u8) !void {
    if (input.len == 0) {
        try result.append(std.heap.page_allocator, 0x06); // Empty stream
        return;
    }

    const len = input.len;
    if (len <= 65535) {
        try result.append(std.heap.page_allocator, 0x21); // WBITS=10, ISLAST=1
        const len_minus_1: u16 = @intCast(len - 1);
        try result.append(std.heap.page_allocator, @intCast(len_minus_1 & 0xFF));
        try result.append(std.heap.page_allocator, @intCast((len_minus_1 >> 8) & 0xFF));
        try result.appendSlice(std.heap.page_allocator, input);
    } else {
        var offset: usize = 0;
        while (offset < len) {
            const remaining = len - offset;
            const block_len = @min(remaining, 65535);
            const is_last: u8 = if (offset + block_len >= len) 1 else 0;

            if (offset == 0) {
                try result.append(std.heap.page_allocator, 0x20 | is_last);
            } else {
                try result.append(std.heap.page_allocator, is_last);
            }

            const len_minus_1: u16 = @intCast(block_len - 1);
            try result.append(std.heap.page_allocator, @intCast(len_minus_1 & 0xFF));
            try result.append(std.heap.page_allocator, @intCast((len_minus_1 >> 8) & 0xFF));
            try result.appendSlice(std.heap.page_allocator, input[offset..][0..block_len]);
            offset += block_len;
        }
    }
}

/// brotliCompress(data) - Compress using brotli stored blocks (no actual compression)
fn brotliCompressFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "brotliCompress requires input data");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    var result: std.ArrayList(u8) = .{};
    defer result.deinit(std.heap.page_allocator);

    writeBrotliUncompressed(&result, input) catch {
        return qjs.JS_ThrowInternalError(ctx, "Brotli compression failed");
    };

    return createUint8Array(ctx, result.items);
}

/// Read brotli stored/uncompressed format
fn readBrotliUncompressed(input: []const u8, allocator: std.mem.Allocator) ![]u8 {
    if (input.len == 0) {
        return allocator.alloc(u8, 0);
    }

    var result: std.ArrayList(u8) = .{};
    errdefer result.deinit(allocator);

    var pos: usize = 0;
    if (pos >= input.len) return error.InvalidData;
    const first_byte = input[pos];
    pos += 1;

    if (first_byte == 0x06) {
        return result.toOwnedSlice(allocator);
    }

    if (first_byte == 0x21 or first_byte == 0x20) {
        const is_last = (first_byte & 0x01) != 0;

        if (pos + 2 > input.len) return error.InvalidData;
        const len_minus_1 = @as(u16, input[pos]) | (@as(u16, input[pos + 1]) << 8);
        pos += 2;
        const data_len: usize = @as(usize, len_minus_1) + 1;

        if (pos + data_len > input.len) return error.InvalidData;
        try result.appendSlice(allocator, input[pos..][0..data_len]);
        pos += data_len;

        if (!is_last) {
            while (pos < input.len) {
                const block_header = input[pos];
                pos += 1;
                const block_is_last = (block_header & 0x01) != 0;

                if (pos + 2 > input.len) break;
                const block_len_minus_1 = @as(u16, input[pos]) | (@as(u16, input[pos + 1]) << 8);
                pos += 2;
                const block_data_len: usize = @as(usize, block_len_minus_1) + 1;

                if (pos + block_data_len > input.len) return error.InvalidData;
                try result.appendSlice(allocator, input[pos..][0..block_data_len]);
                pos += block_data_len;

                if (block_is_last) break;
            }
        }

        return result.toOwnedSlice(allocator);
    }

    return error.UnsupportedFormat;
}

/// brotliDecompress(data) - Decompress brotli stored blocks only
fn brotliDecompressFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "brotliDecompress requires input data");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    const decompressed = readBrotliUncompressed(input, std.heap.page_allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => qjs.JS_ThrowOutOfMemory(ctx),
            error.UnsupportedFormat => qjs.JS_ThrowInternalError(ctx, "Brotli decompression failed - only stored-block format supported"),
            else => qjs.JS_ThrowSyntaxError(ctx, "Invalid brotli data"),
        };
    };
    defer std.heap.page_allocator.free(decompressed);

    return createUint8Array(ctx, decompressed);
}

/// Register compression module
pub fn register(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const comp_obj = qjs.JS_NewObject(ctx);

    // Register functions
    inline for (.{
        .{ "gzip", gzipFunc, 1 },
        .{ "gunzip", gunzipFunc, 1 },
        .{ "deflate", deflateFunc, 1 },
        .{ "inflate", inflateFunc, 1 },
        .{ "deflateZlib", deflateZlibFunc, 1 },
        .{ "inflateZlib", inflateZlibFunc, 1 },
        .{ "brotliCompress", brotliCompressFunc, 1 },
        .{ "brotliDecompress", brotliDecompressFunc, 1 },
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
