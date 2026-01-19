/// Native compression module - QuickJS C functions
/// Gzip, Deflate, Inflate, and Brotli using Zig std.compress (pure Zig, works on WASM)
///
/// Note: Compression uses "stored" (uncompressed) blocks for all formats since
/// Zig 0.15.2's std.compress.flate.Compress API is incomplete. Decompression
/// fully works for deflate/gzip. Brotli decompression only supports stored blocks.
const std = @import("std");
const builtin = @import("builtin");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;
const flate = std.compress.flate;
const Io = std.Io;


// Simple brotli implementation using stored/raw format
// This creates valid brotli streams that can be decompressed, but with no actual compression
// (similar to the gzip/deflate stored block approach)
// Full brotli compression requires linking to system libbrotli which adds ~300KB

/// Brotli stream header for simple uncompressed data
/// Format: Uses the simplest possible brotli stream structure
fn writeBrotliUncompressed(result: *std.ArrayList(u8), input: []const u8) !void {
    // Brotli uncompressed format:
    // - Uses metablock with uncompressed data type
    // - ISLAST = 1, ISUNCOMPRESSED = 1, followed by length and raw data

    if (input.len == 0) {
        // Empty stream: final meta-block with MLEN=0
        // Wbits = 10 (minimum), final empty metablock
        try result.append(std.heap.page_allocator, 0x06); // WBITS=10, ISLAST=1, ISEMPTY=1
        return;
    }

    // For simplicity, encode as a single large uncompressed block
    // Header byte: ISLAST=1, MNIBBLES=0 (16 bits length), ISUNCOMPRESSED=1
    const len = input.len;

    if (len <= 65535) {
        // Single block, length fits in 16 bits
        // WBITS nibble (10 = minimum = 0x0A → encoded as 0x0)
        // Then ISLAST=1, MNIBBLES=4 (16 bits), ISUNCOMPRESSED=1
        try result.append(std.heap.page_allocator, 0x21); // WBITS=10, ISLAST=1
        // Length - 1 in 16 bits
        const len_minus_1: u16 = @intCast(len - 1);
        try result.append(std.heap.page_allocator, @intCast(len_minus_1 & 0xFF));
        try result.append(std.heap.page_allocator, @intCast((len_minus_1 >> 8) & 0xFF));
        // Uncompressed data
        try result.appendSlice(std.heap.page_allocator, input);
    } else {
        // Multiple blocks needed for large data
        var offset: usize = 0;
        while (offset < len) {
            const remaining = len - offset;
            const block_len = @min(remaining, 65535);
            const is_last: u8 = if (offset + block_len >= len) 1 else 0;

            if (offset == 0) {
                // First block has WBITS header
                try result.append(std.heap.page_allocator, 0x20 | is_last); // WBITS=10, ISLAST
            } else {
                try result.append(std.heap.page_allocator, is_last); // ISLAST only
            }

            // MNIBBLES=4, MLEN (16 bits), ISUNCOMPRESSED=1
            const len_minus_1: u16 = @intCast(block_len - 1);
            try result.append(std.heap.page_allocator, @intCast(len_minus_1 & 0xFF));
            try result.append(std.heap.page_allocator, @intCast((len_minus_1 >> 8) & 0xFF));

            // Uncompressed data
            try result.appendSlice(std.heap.page_allocator, input[offset..][0..block_len]);
            offset += block_len;
        }
    }
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
const DEFLATE_CM: u8 = 8;
const FTEXT: u8 = 1;
const FHCRC: u8 = 2;
const FEXTRA: u8 = 4;
const FNAME: u8 = 8;
const FCOMMENT: u8 = 16;

/// Read all data from a Reader into a dynamically allocated buffer
fn readAllFromReader(reader: *Io.Reader, allocator: std.mem.Allocator, max_size: usize) ![]u8 {
    // Use the Zig 0.15.2 Reader API - allocRemaining reads all data up to limit
    return reader.allocRemaining(allocator, .limited(max_size)) catch |err| switch (err) {
        error.StreamTooLong => return error.OutOfMemory,
        error.OutOfMemory => return error.OutOfMemory,
        error.ReadFailed => return error.InvalidData,
    };
}

/// Write a deflate "stored" block (no compression, just raw data with headers)
/// This creates valid deflate format that can be decompressed
fn writeStoredBlocks(result: *std.ArrayList(u8), input: []const u8) !void {
    const MAX_BLOCK_SIZE: usize = 65535; // Max stored block size
    var offset: usize = 0;

    while (offset < input.len) {
        const remaining = input.len - offset;
        const block_size: u16 = @intCast(@min(remaining, MAX_BLOCK_SIZE));
        const is_final: u8 = if (offset + block_size >= input.len) 1 else 0;

        // Block header: BFINAL (1 bit) + BTYPE (2 bits) = 0b00 for stored
        // Byte-aligned, so: is_final | (0b00 << 1) = is_final
        try result.append(std.heap.page_allocator, is_final);

        // LEN (2 bytes, little-endian)
        try result.append(std.heap.page_allocator, @intCast(block_size & 0xFF));
        try result.append(std.heap.page_allocator, @intCast((block_size >> 8) & 0xFF));

        // NLEN (2 bytes, one's complement of LEN)
        const nlen: u16 = ~block_size;
        try result.append(std.heap.page_allocator, @intCast(nlen & 0xFF));
        try result.append(std.heap.page_allocator, @intCast((nlen >> 8) & 0xFF));

        // Raw data
        try result.appendSlice(std.heap.page_allocator, input[offset .. offset + block_size]);
        offset += block_size;
    }

    // Handle empty input
    if (input.len == 0) {
        // Final empty stored block
        try result.append(std.heap.page_allocator, 1); // BFINAL=1, BTYPE=00
        try result.append(std.heap.page_allocator, 0); // LEN low
        try result.append(std.heap.page_allocator, 0); // LEN high
        try result.append(std.heap.page_allocator, 0xFF); // NLEN low (~0)
        try result.append(std.heap.page_allocator, 0xFF); // NLEN high
    }
}

/// gzip(data) - Compress data using gzip format (stored blocks, no actual compression)
fn gzipFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "gzip requires input data");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    var result: std.ArrayList(u8) = .{};
    defer result.deinit(std.heap.page_allocator);

    // Write gzip header (10 bytes)
    result.appendSlice(std.heap.page_allocator, &GZIP_MAGIC) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    result.append(std.heap.page_allocator, DEFLATE_CM) catch return qjs.JS_ThrowOutOfMemory(ctx); // compression method
    result.append(std.heap.page_allocator, 0) catch return qjs.JS_ThrowOutOfMemory(ctx); // flags
    result.appendSlice(std.heap.page_allocator, &[_]u8{ 0, 0, 0, 0 }) catch return qjs.JS_ThrowOutOfMemory(ctx); // mtime
    result.append(std.heap.page_allocator, 0) catch return qjs.JS_ThrowOutOfMemory(ctx); // extra flags
    result.append(std.heap.page_allocator, 255) catch return qjs.JS_ThrowOutOfMemory(ctx); // OS (unknown)

    // Write deflate stored blocks
    writeStoredBlocks(&result, input) catch {
        return qjs.JS_ThrowInternalError(ctx, "Compression failed");
    };

    // Write trailer (CRC32 + uncompressed size)
    const crc = std.hash.crc.Crc32.hash(input);
    result.append(std.heap.page_allocator, @intCast(crc & 0xFF)) catch return qjs.JS_ThrowOutOfMemory(ctx);
    result.append(std.heap.page_allocator, @intCast((crc >> 8) & 0xFF)) catch return qjs.JS_ThrowOutOfMemory(ctx);
    result.append(std.heap.page_allocator, @intCast((crc >> 16) & 0xFF)) catch return qjs.JS_ThrowOutOfMemory(ctx);
    result.append(std.heap.page_allocator, @intCast((crc >> 24) & 0xFF)) catch return qjs.JS_ThrowOutOfMemory(ctx);

    const size32: u32 = @intCast(input.len & 0xFFFFFFFF);
    result.append(std.heap.page_allocator, @intCast(size32 & 0xFF)) catch return qjs.JS_ThrowOutOfMemory(ctx);
    result.append(std.heap.page_allocator, @intCast((size32 >> 8) & 0xFF)) catch return qjs.JS_ThrowOutOfMemory(ctx);
    result.append(std.heap.page_allocator, @intCast((size32 >> 16) & 0xFF)) catch return qjs.JS_ThrowOutOfMemory(ctx);
    result.append(std.heap.page_allocator, @intCast((size32 >> 24) & 0xFF)) catch return qjs.JS_ThrowOutOfMemory(ctx);

    return createUint8Array(ctx, result.items);
}

/// gunzip(data) - Decompress gzip data using new Zig 0.15.2 API
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

    // Use new Zig 0.15.2 Decompress API
    var reader = Io.Reader.fixed(input);
    var window_buffer: [flate.max_window_len]u8 = undefined;
    var decompress: flate.Decompress = .init(&reader, .gzip, &window_buffer);

    // Read decompressed data
    const decompressed = readAllFromReader(&decompress.reader, std.heap.page_allocator, 100 * 1024 * 1024) catch |err| {
        return switch (err) {
            error.OutOfMemory => qjs.JS_ThrowOutOfMemory(ctx),
            else => qjs.JS_ThrowSyntaxError(ctx, "Invalid gzip data"),
        };
    };
    defer std.heap.page_allocator.free(decompressed);

    return createUint8Array(ctx, decompressed);
}

/// deflate(data) - Compress data using raw deflate (stored blocks)
fn deflateFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "deflate requires input data");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    var result: std.ArrayList(u8) = .{};
    defer result.deinit(std.heap.page_allocator);

    writeStoredBlocks(&result, input) catch {
        return qjs.JS_ThrowInternalError(ctx, "Compression failed");
    };

    return createUint8Array(ctx, result.items);
}

/// inflate(data) - Decompress raw deflate data using new API
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

/// inflateZlib(data) - Decompress zlib-wrapped data using new API
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

/// brotliCompress(data) - Compress data using brotli (stored blocks, no actual compression)
/// Creates valid brotli format that can be decompressed by any brotli decoder
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

/// Decompress brotli stored/uncompressed format (the format writeBrotliUncompressed produces)
/// Note: This only handles stored-block format, not full brotli compression
fn readBrotliUncompressed(input: []const u8, allocator: std.mem.Allocator) ![]u8 {
    if (input.len == 0) {
        return allocator.alloc(u8, 0);
    }

    var result: std.ArrayList(u8) = .{};
    errdefer result.deinit(allocator);

    var pos: usize = 0;

    // Read first byte (contains WBITS)
    if (pos >= input.len) return error.InvalidData;
    const first_byte = input[pos];
    pos += 1;

    // Check for empty stream marker
    if (first_byte == 0x06) {
        // Empty brotli stream
        return result.toOwnedSlice(allocator);
    }

    // Check for stored block with WBITS header (0x21 = WBITS + ISLAST + ISUNCOMPRESSED marker)
    if (first_byte == 0x21 or first_byte == 0x20) {
        const is_last = (first_byte & 0x01) != 0;

        // Read length (2 bytes)
        if (pos + 2 > input.len) return error.InvalidData;
        const len_minus_1 = @as(u16, input[pos]) | (@as(u16, input[pos + 1]) << 8);
        pos += 2;
        const data_len: usize = @as(usize, len_minus_1) + 1;

        // Read uncompressed data
        if (pos + data_len > input.len) return error.InvalidData;
        try result.appendSlice(allocator, input[pos..][0..data_len]);
        pos += data_len;

        // If not last block, continue reading
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

    // Unknown format - may be actual compressed brotli
    return error.UnsupportedFormat;
}

/// brotliDecompress(data) - Decompress brotli data
/// Note: Only supports stored-block format (no actual compression). For fully compressed
/// brotli streams, the native brotli library would be required.
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

    // Create compression module
    const comp_obj = qjs.JS_NewObject(ctx);

    // Register functions
    inline for (.{
        .{ "gzip", gzipFunc, 1 },
        .{ "gunzip", gunzipFunc, 1 },
        .{ "deflate", deflateFunc, 1 },
        .{ "inflate", inflateFunc, 1 },
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
