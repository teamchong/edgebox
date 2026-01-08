/// Native compression module - QuickJS C functions
/// Gzip, Deflate, Inflate using Zig std.compress (pure Zig, works on WASM)
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

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

/// gzip(data) - Compress data using gzip format
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

    // Compress data using deflate
    var comp = std.compress.flate.compressor(result.writer(std.heap.page_allocator), .{}) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to init compressor");
    };
    comp.write(input) catch {
        return qjs.JS_ThrowInternalError(ctx, "Compression failed");
    };
    comp.finish() catch {
        return qjs.JS_ThrowInternalError(ctx, "Compression finish failed");
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

/// gunzip(data) - Decompress gzip data
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

    // Parse header to find data start
    var header_end: usize = 10;
    const flags = input[3];

    // Skip EXTRA field
    if (flags & FEXTRA != 0 and input.len > header_end + 2) {
        const xlen = @as(u16, input[header_end]) | (@as(u16, input[header_end + 1]) << 8);
        header_end += 2 + xlen;
    }
    // Skip FNAME
    if (flags & FNAME != 0) {
        while (header_end < input.len and input[header_end] != 0) header_end += 1;
        header_end += 1;
    }
    // Skip FCOMMENT
    if (flags & FCOMMENT != 0) {
        while (header_end < input.len and input[header_end] != 0) header_end += 1;
        header_end += 1;
    }
    // Skip FHCRC
    if (flags & FHCRC != 0) header_end += 2;

    if (input.len < header_end + 8) {
        return qjs.JS_ThrowSyntaxError(ctx, "Invalid gzip format");
    }

    // Decompress the deflate data (excluding 8-byte trailer)
    const deflate_data = input[header_end .. input.len - 8];
    var fbs = std.io.fixedBufferStream(deflate_data);
    var decomp = std.compress.flate.decompressor(fbs.reader());

    const decompressed = decomp.reader().readAllAlloc(std.heap.page_allocator, 100 * 1024 * 1024) catch |err| {
        return switch (err) {
            error.OutOfMemory => qjs.JS_ThrowOutOfMemory(ctx),
            else => qjs.JS_ThrowSyntaxError(ctx, "Invalid deflate data"),
        };
    };
    defer std.heap.page_allocator.free(decompressed);

    // Verify CRC32
    const expected_crc = @as(u32, input[input.len - 8]) |
        (@as(u32, input[input.len - 7]) << 8) |
        (@as(u32, input[input.len - 6]) << 16) |
        (@as(u32, input[input.len - 5]) << 24);
    const actual_crc = std.hash.crc.Crc32.hash(decompressed);
    if (expected_crc != actual_crc) {
        return qjs.JS_ThrowSyntaxError(ctx, "Gzip CRC mismatch");
    }

    return createUint8Array(ctx, decompressed);
}

/// deflate(data) - Compress data using raw deflate
fn deflateFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "deflate requires input data");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    var result: std.ArrayList(u8) = .{};
    defer result.deinit(std.heap.page_allocator);

    var comp = std.compress.flate.compressor(result.writer(std.heap.page_allocator), .{}) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to init compressor");
    };
    comp.write(input) catch {
        return qjs.JS_ThrowInternalError(ctx, "Compression failed");
    };
    comp.finish() catch {
        return qjs.JS_ThrowInternalError(ctx, "Compression finish failed");
    };

    return createUint8Array(ctx, result.items);
}

/// inflate(data) - Decompress raw deflate data
fn inflateFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "inflate requires input data");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    var fbs = std.io.fixedBufferStream(input);
    var decomp = std.compress.flate.decompressor(fbs.reader());

    const decompressed = decomp.reader().readAllAlloc(std.heap.page_allocator, 100 * 1024 * 1024) catch |err| {
        return switch (err) {
            error.OutOfMemory => qjs.JS_ThrowOutOfMemory(ctx),
            else => qjs.JS_ThrowSyntaxError(ctx, "Invalid deflate data"),
        };
    };
    defer std.heap.page_allocator.free(decompressed);

    return createUint8Array(ctx, decompressed);
}

/// inflateZlib(data) - Decompress zlib-wrapped data (2-byte header + deflate + 4-byte checksum)
fn inflateZlibFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "inflateZlib requires input data");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    // Zlib format: 2-byte header + deflate data + 4-byte Adler32 checksum
    if (input.len < 6) {
        return qjs.JS_ThrowSyntaxError(ctx, "Invalid zlib data - too short");
    }

    // Verify zlib header (CMF + FLG)
    const cmf = input[0];
    const flg = input[1];
    if ((cmf & 0x0F) != 8) { // compression method must be deflate
        return qjs.JS_ThrowSyntaxError(ctx, "Invalid zlib compression method");
    }
    if ((@as(u16, cmf) * 256 + flg) % 31 != 0) {
        return qjs.JS_ThrowSyntaxError(ctx, "Invalid zlib header checksum");
    }

    // Skip 2-byte header, exclude 4-byte Adler32 trailer
    const deflate_data = input[2 .. input.len - 4];

    var fbs = std.io.fixedBufferStream(deflate_data);
    var decomp = std.compress.flate.decompressor(fbs.reader());

    const decompressed = decomp.reader().readAllAlloc(std.heap.page_allocator, 100 * 1024 * 1024) catch |err| {
        return switch (err) {
            error.OutOfMemory => qjs.JS_ThrowOutOfMemory(ctx),
            else => qjs.JS_ThrowSyntaxError(ctx, "Invalid deflate data in zlib stream"),
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
