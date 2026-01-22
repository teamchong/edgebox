/// Native compression module - QuickJS C functions
/// Uses libdeflate for real gzip/deflate compression (native builds only)
/// Uses libbrotli for actual Brotli compression
/// WASM builds use stub implementations (compression via host functions)
const std = @import("std");
const builtin = @import("builtin");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;
const flate = std.compress.flate;
const Io = std.Io;

// Check if building for WASM - use stub implementations
const is_wasm = builtin.target.cpu.arch == .wasm32 or builtin.target.cpu.arch == .wasm64;

// libdeflate C bindings (native only)
// WASM builds use host dispatch functions, but still need type definitions
const libdeflate = if (is_wasm) struct {
    // Stub types for WASM - compression done via host functions
    pub const struct_libdeflate_compressor = opaque {};
    pub const struct_libdeflate_decompressor = opaque {};
    pub fn libdeflate_alloc_compressor(_: c_int) ?*struct_libdeflate_compressor {
        return null;
    }
    pub fn libdeflate_free_compressor(_: ?*struct_libdeflate_compressor) void {}
    // Compress functions return 0 (failure) since WASM uses host dispatch
    pub fn libdeflate_gzip_compress(_: ?*struct_libdeflate_compressor, _: ?*const anyopaque, _: usize, _: ?*anyopaque, _: usize) usize {
        return 0;
    }
    pub fn libdeflate_deflate_compress(_: ?*struct_libdeflate_compressor, _: ?*const anyopaque, _: usize, _: ?*anyopaque, _: usize) usize {
        return 0;
    }
    pub fn libdeflate_zlib_compress(_: ?*struct_libdeflate_compressor, _: ?*const anyopaque, _: usize, _: ?*anyopaque, _: usize) usize {
        return 0;
    }
    pub fn libdeflate_gzip_compress_bound(_: ?*struct_libdeflate_compressor, _: usize) usize {
        return 0;
    }
    pub fn libdeflate_deflate_compress_bound(_: ?*struct_libdeflate_compressor, _: usize) usize {
        return 0;
    }
    pub fn libdeflate_zlib_compress_bound(_: ?*struct_libdeflate_compressor, _: usize) usize {
        return 0;
    }
    pub fn libdeflate_alloc_decompressor() ?*struct_libdeflate_decompressor {
        return null;
    }
    pub fn libdeflate_free_decompressor(_: ?*struct_libdeflate_decompressor) void {}
    pub fn libdeflate_gzip_decompress(_: ?*struct_libdeflate_decompressor, _: ?*const anyopaque, _: usize, _: ?*anyopaque, _: usize, _: *usize) c_uint {
        return 1;
    }
    pub fn libdeflate_deflate_decompress(_: ?*struct_libdeflate_decompressor, _: ?*const anyopaque, _: usize, _: ?*anyopaque, _: usize, _: *usize) c_uint {
        return 1;
    }
    pub fn libdeflate_zlib_decompress(_: ?*struct_libdeflate_decompressor, _: ?*const anyopaque, _: usize, _: ?*anyopaque, _: usize, _: *usize) c_uint {
        return 1;
    }
    pub const LIBDEFLATE_SUCCESS: c_uint = 0;
} else @cImport({
    @cInclude("libdeflate.h");
});

// libbrotli C bindings (native only)
const brotli = if (is_wasm) struct {
    // Stub types for WASM - brotli done via host functions
    pub const BROTLI_MODE_TEXT: c_int = 0;
    pub const BROTLI_MODE_GENERIC: c_int = 0;
    pub const BROTLI_DEFAULT_QUALITY: c_int = 11;
    pub const BROTLI_DEFAULT_WINDOW: c_int = 22;
    pub const BROTLI_TRUE: c_int = 1;
    pub const BROTLI_FALSE: c_int = 0;
    pub const BrotliDecoderResult = enum(c_int) {
        BROTLI_DECODER_RESULT_ERROR = 0,
        BROTLI_DECODER_RESULT_SUCCESS = 1,
        BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT = 2,
        BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT = 3,
    };
    pub const BROTLI_DECODER_RESULT_ERROR = BrotliDecoderResult.BROTLI_DECODER_RESULT_ERROR;
    pub const BROTLI_DECODER_RESULT_SUCCESS = BrotliDecoderResult.BROTLI_DECODER_RESULT_SUCCESS;
    pub const BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT = BrotliDecoderResult.BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT;
    pub const BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT = BrotliDecoderResult.BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT;
    pub fn BrotliEncoderMaxCompressedSize(_: usize) usize {
        return 0;
    }
    pub fn BrotliEncoderCompress(_: c_int, _: c_int, _: c_int, _: usize, _: [*]const u8, _: *usize, _: [*]u8) c_int {
        return BROTLI_FALSE;
    }
    pub fn BrotliDecoderDecompress(_: usize, _: [*]const u8, _: *usize, _: [*]u8) BrotliDecoderResult {
        return .BROTLI_DECODER_RESULT_ERROR;
    }
} else @cImport({
    @cInclude("brotli/encode.h");
    @cInclude("brotli/decode.h");
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
    const result = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    qjs.JS_FreeValue(ctx, array_buf); // Free after constructor call (constructor dups args)
    return result;
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
// Brotli - using libbrotli for actual compression
// =============================================================================

/// brotliCompress(data) - Compress using libbrotli
fn brotliCompressFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "brotliCompress requires input data");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    // Handle empty input
    if (input.len == 0) {
        // Empty Brotli stream
        const empty_stream = [_]u8{0x06};
        return createUint8Array(ctx, &empty_stream);
    }

    // Calculate max compressed size (Brotli worst case is input + overhead)
    const max_compressed_size = brotli.BrotliEncoderMaxCompressedSize(input.len);
    if (max_compressed_size == 0) {
        return qjs.JS_ThrowInternalError(ctx, "Input too large for Brotli compression");
    }

    // Allocate output buffer
    const output_buf = std.heap.page_allocator.alloc(u8, max_compressed_size) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    defer std.heap.page_allocator.free(output_buf);

    var encoded_size: usize = max_compressed_size;

    // Compress using default quality (11) and window size (22)
    const result = brotli.BrotliEncoderCompress(
        brotli.BROTLI_DEFAULT_QUALITY, // quality = 11
        brotli.BROTLI_DEFAULT_WINDOW, // lgwin = 22
        brotli.BROTLI_MODE_GENERIC, // generic mode
        input.len,
        input.ptr,
        &encoded_size,
        output_buf.ptr,
    );

    if (result != brotli.BROTLI_TRUE) {
        return qjs.JS_ThrowInternalError(ctx, "Brotli compression failed");
    }

    return createUint8Array(ctx, output_buf[0..encoded_size]);
}

/// brotliDecompress(data) - Decompress using libbrotli
fn brotliDecompressFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "brotliDecompress requires input data");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    // Handle empty input
    if (input.len == 0) {
        return createUint8Array(ctx, &[_]u8{});
    }

    // Start with a reasonable initial output size guess (4x input)
    var output_capacity: usize = @max(input.len * 4, 1024);
    var output_buf = std.heap.page_allocator.alloc(u8, output_capacity) catch {
        return qjs.JS_ThrowOutOfMemory(ctx);
    };
    defer std.heap.page_allocator.free(output_buf);

    var decoded_size: usize = output_capacity;

    // Try decompression - may need to grow buffer
    var result = brotli.BrotliDecoderDecompress(
        input.len,
        input.ptr,
        &decoded_size,
        output_buf.ptr,
    );

    // If buffer too small, retry with larger buffer
    var attempts: u32 = 0;
    while (result == brotli.BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT and attempts < 5) {
        std.heap.page_allocator.free(output_buf);
        output_capacity *= 4;
        output_buf = std.heap.page_allocator.alloc(u8, output_capacity) catch {
            return qjs.JS_ThrowOutOfMemory(ctx);
        };
        decoded_size = output_capacity;

        result = brotli.BrotliDecoderDecompress(
            input.len,
            input.ptr,
            &decoded_size,
            output_buf.ptr,
        );
        attempts += 1;
    }

    if (result != brotli.BROTLI_DECODER_RESULT_SUCCESS) {
        return qjs.JS_ThrowSyntaxError(ctx, "Invalid Brotli data");
    }

    return createUint8Array(ctx, output_buf[0..decoded_size]);
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

    // Set in _modules for require('compression') and require('zlib')
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        // Register as both 'compression' and 'zlib' for Node.js compatibility
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "compression", qjs.JS_DupValue(ctx, comp_obj));
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "zlib", comp_obj);
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, comp_obj);
    }
}
