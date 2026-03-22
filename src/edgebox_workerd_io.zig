// edgebox_workerd_io.zig — C ABI exports for workerd integration
//
// These functions are called by workerd's jsg API binding (global-scope.h patch).
// They bridge workerd's V8 isolates to Zig's IO layer (file cache, mmap, SIMD).
//
// C ABI:
//   edgebox_io_sync(request, request_len, *response_len) → response_ptr
//   edgebox_io_free(ptr, len) → void

const std = @import("std");
const v8_io = @import("v8_io.zig");

const alloc = std.heap.page_allocator;

/// Synchronous IO: takes JSON request, returns JSON response.
/// Called from workerd's ServiceWorkerGlobalScope.__edgebox_io_sync()
export fn edgebox_io_sync(
    request_ptr: [*]const u8,
    request_len: c_int,
    response_len: *c_int,
) ?[*]const u8 {
    if (request_len <= 0) {
        response_len.* = 0;
        return null;
    }

    const request = request_ptr[0..@intCast(request_len)];

    // Use our existing IO dispatcher
    const response = v8_io.handleRequestForWorkerd(request) catch {
        const err = "{\"ok\":false,\"error\":\"io_error\"}";
        const buf = alloc.alloc(u8, err.len) catch return null;
        @memcpy(buf, err);
        response_len.* = @intCast(err.len);
        return buf.ptr;
    };

    response_len.* = @intCast(response.len);
    return response.ptr;
}

/// Batch IO: takes JSON array of requests, returns JSON array of responses.
export fn edgebox_io_batch(
    request_ptr: [*]const u8,
    request_len: c_int,
    response_len: *c_int,
) ?[*]const u8 {
    if (request_len <= 0) {
        response_len.* = 0;
        return null;
    }

    const request = request_ptr[0..@intCast(request_len)];
    const response = v8_io.handleBatchRequestForWorkerd(request) catch {
        const err = "[]";
        const buf = alloc.alloc(u8, err.len) catch return null;
        @memcpy(buf, err);
        response_len.* = @intCast(err.len);
        return buf.ptr;
    };

    response_len.* = @intCast(response.len);
    return response.ptr;
}

/// Free response buffer allocated by Zig.
export fn edgebox_io_free(ptr: ?[*]const u8, len: c_int) void {
    if (ptr) |p| {
        if (len > 0) {
            const slice: []const u8 = p[0..@intCast(len)];
            alloc.free(@constCast(slice));
        }
    }
}
