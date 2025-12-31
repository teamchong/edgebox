/// Native compression module - QuickJS C functions
/// Provides native buffer compression using std.compress.flate (50-200x faster than JS)
/// Falls back to host-implemented compression functions via __edgebox_* globals.
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;
const wasm_zlib = @import("../wasm_zlib.zig");

// Cached function references (avoids JS_GetGlobalObject + JS_GetPropertyStr per call)
var cached_gzip: qjs.JSValue = qjs.JS_UNDEFINED;
var cached_gunzip: qjs.JSValue = qjs.JS_UNDEFINED;
var cached_deflate: qjs.JSValue = qjs.JS_UNDEFINED;
var cached_inflate: qjs.JSValue = qjs.JS_UNDEFINED;
var cached_global: qjs.JSValue = qjs.JS_UNDEFINED;

// Cached Uint8Array constructor for creating result buffers
var cached_uint8array_ctor: qjs.JSValue = qjs.JS_UNDEFINED;

/// Get cached Uint8Array constructor (caches on first call)
fn getUint8ArrayCtor(ctx: ?*qjs.JSContext) qjs.JSValue {
    if (qjs.JS_IsUndefined(cached_uint8array_ctor)) {
        const global = qjs.JS_GetGlobalObject(ctx);
        cached_uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
        qjs.JS_FreeValue(ctx, global);
    }
    return cached_uint8array_ctor;
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
        // Handle zero-length buffer
        if (byte_len == 0) {
            return &[0]u8{};
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
    } else if (ab_ptr != null and ab_size == 0) {
        return &[0]u8{};
    } else {
        // Clear exception
        const exc = qjs.JS_GetException(ctx);
        qjs.JS_FreeValue(ctx, exc);
    }

    return null;
}

/// Create Uint8Array from bytes
fn createUint8Array(ctx: ?*qjs.JSContext, data: []const u8) qjs.JSValue {
    // Create ArrayBuffer with copy of data
    const array_buf = qjs.JS_NewArrayBufferCopy(ctx, data.ptr, data.len);
    if (qjs.JS_IsException(array_buf)) return array_buf;

    // Wrap ArrayBuffer in Uint8Array
    const uint8array_ctor = getUint8ArrayCtor(ctx);
    var ctor_args = [1]qjs.JSValue{array_buf};
    return qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
}

/// gzipBuffer(input) - Compress Uint8Array to gzip format, returns Uint8Array
/// Uses native std.compress.flate - 50-200x faster than JS string conversion
fn gzipBufferFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "gzipBuffer requires input");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    // Call native gzip compression
    const result = wasm_zlib.gzip(std.heap.page_allocator, input) catch |err| {
        return qjs.JS_ThrowInternalError(ctx, "Compression failed: %s", @errorName(err).ptr);
    };
    defer std.heap.page_allocator.free(result);

    return createUint8Array(ctx, result);
}

/// gunzipBuffer(input) - Decompress gzip Uint8Array, returns Uint8Array
fn gunzipBufferFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "gunzipBuffer requires input");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    // Call native gunzip decompression
    const result = wasm_zlib.gunzip(std.heap.page_allocator, input) catch |err| {
        return qjs.JS_ThrowInternalError(ctx, "Decompression failed: %s", @errorName(err).ptr);
    };
    defer std.heap.page_allocator.free(result);

    return createUint8Array(ctx, result);
}

/// deflateBuffer(input) - Compress Uint8Array to raw deflate format, returns Uint8Array
fn deflateBufferFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "deflateBuffer requires input");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    // Call native deflate compression
    const result = wasm_zlib.deflate(std.heap.page_allocator, input) catch |err| {
        return qjs.JS_ThrowInternalError(ctx, "Compression failed: %s", @errorName(err).ptr);
    };
    defer std.heap.page_allocator.free(result);

    return createUint8Array(ctx, result);
}

/// inflateBuffer(input) - Decompress raw deflate Uint8Array, returns Uint8Array
fn inflateBufferFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "inflateBuffer requires input");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    // Call native inflate decompression
    const result = wasm_zlib.inflate(std.heap.page_allocator, input) catch |err| {
        return qjs.JS_ThrowInternalError(ctx, "Decompression failed: %s", @errorName(err).ptr);
    };
    defer std.heap.page_allocator.free(result);

    return createUint8Array(ctx, result);
}

/// inflateZlibBuffer(input) - Decompress zlib-wrapped deflate Uint8Array, returns Uint8Array
fn inflateZlibBufferFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "inflateZlibBuffer requires input");

    const input = getBufferBytes(ctx, argv[0]) orelse {
        return qjs.JS_ThrowTypeError(ctx, "Input must be Uint8Array or Buffer");
    };

    // Call native inflateZlib decompression
    const result = wasm_zlib.inflateZlib(std.heap.page_allocator, input) catch |err| {
        return qjs.JS_ThrowInternalError(ctx, "Decompression failed: %s", @errorName(err).ptr);
    };
    defer std.heap.page_allocator.free(result);

    return createUint8Array(ctx, result);
}

/// Call cached function (fast path - no property lookup)
fn callCachedFunction(ctx: ?*qjs.JSContext, func: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) qjs.JSValue {
    if (qjs.JS_IsUndefined(func)) {
        return qjs.JS_ThrowTypeError(ctx, "compression host function not available");
    }
    return qjs.JS_Call(ctx, func, cached_global, argc, argv);
}

/// gzip(data) - Compress data using gzip
fn gzipFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return callCachedFunction(ctx, cached_gzip, argc, argv);
}

/// gunzip(data) - Decompress gzip data
fn gunzipFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return callCachedFunction(ctx, cached_gunzip, argc, argv);
}

/// deflate(data) - Compress data using deflate
fn deflateFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return callCachedFunction(ctx, cached_deflate, argc, argv);
}

/// inflate(data) - Decompress deflate data
fn inflateFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return callCachedFunction(ctx, cached_inflate, argc, argv);
}

/// inflateZlib(data) - Decompress zlib data (uses inflate - same format handled by host)
fn inflateZlibFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return callCachedFunction(ctx, cached_inflate, argc, argv);
}

/// Register compression module
pub fn register(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);

    // Cache global and host functions for fast access (no per-call property lookup)
    cached_global = qjs.JS_DupValue(ctx, global);
    cached_gzip = qjs.JS_GetPropertyStr(ctx, global, "__edgebox_gzip");
    cached_gunzip = qjs.JS_GetPropertyStr(ctx, global, "__edgebox_gunzip");
    cached_deflate = qjs.JS_GetPropertyStr(ctx, global, "__edgebox_deflate");
    cached_inflate = qjs.JS_GetPropertyStr(ctx, global, "__edgebox_inflate");

    // Create compression module
    const comp_obj = qjs.JS_NewObject(ctx);

    // Register functions (both string-based fallbacks and native buffer functions)
    inline for (.{
        // String-based fallbacks (use __edgebox_* host functions)
        .{ "gzip", gzipFunc, 1 },
        .{ "gunzip", gunzipFunc, 1 },
        .{ "deflate", deflateFunc, 1 },
        .{ "inflate", inflateFunc, 1 },
        .{ "inflateZlib", inflateZlibFunc, 1 },
        // Native buffer functions (50-200x faster - no JS string conversion)
        .{ "gzipBuffer", gzipBufferFunc, 1 },
        .{ "gunzipBuffer", gunzipBufferFunc, 1 },
        .{ "deflateBuffer", deflateBufferFunc, 1 },
        .{ "inflateBuffer", inflateBufferFunc, 1 },
        .{ "inflateZlibBuffer", inflateZlibBufferFunc, 1 },
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

    qjs.JS_FreeValue(ctx, global);
}
