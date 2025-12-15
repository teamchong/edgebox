//! EdgeBox ABI - Exported Functions for JavaScript
//!
//! This module provides the public ABI that JavaScript calls.
//! All complex logic runs here in WASM sandbox.
//!
//! Design principles:
//! 1. Minimal host dependencies (only raw I/O)
//! 2. All parsing/validation in WASM
//! 3. Simple pointer-based interface
//! 4. Length-prefixed results for easy JS consumption
//!
//! Result format: [len:u32][data...]
//! Error: returns 0

const std = @import("std");
pub const crypto = @import("crypto.zig");
pub const host = @import("host.zig");
pub const http = @import("http.zig");

// Forward declarations for modules to be implemented
// pub const fs = @import("fs.zig");
// pub const process = @import("process.zig");

const wasm_allocator = std.heap.wasm_allocator;

// ============================================================================
// Memory Management
// ============================================================================

/// Allocate memory in WASM linear memory.
/// JS uses this to allocate space for writing strings/data.
export fn zig_alloc(len: u32) u32 {
    const slice = wasm_allocator.alloc(u8, len) catch return 0;
    return @intFromPtr(slice.ptr);
}

/// Free memory allocated by zig_alloc or returned by other functions.
export fn zig_free(ptr: u32, len: u32) void {
    if (ptr == 0) return;
    const slice = @as([*]u8, @ptrFromInt(ptr))[0..len];
    wasm_allocator.free(slice);
}

/// Free a result buffer (reads length from prefix).
export fn zig_free_result(ptr: u32) void {
    crypto.freeResult(ptr);
}

/// Get length of a result buffer.
export fn zig_result_len(ptr: u32) u32 {
    return crypto.resultLen(ptr);
}

/// Get data pointer of a result buffer (skips 4-byte length prefix).
export fn zig_result_data(ptr: u32) u32 {
    return crypto.resultData(ptr);
}

// ============================================================================
// Crypto ABI
// ============================================================================

/// Hash data with algorithm.
/// algo: 0=sha256, 1=sha384, 2=sha512, 3=md5, 4=sha1
/// Returns pointer to result: [len:u32][hex_string...]
/// Returns 0 on error.
export fn zig_hash(algo: u32, data_ptr: u32, data_len: u32) u32 {
    return crypto.hash(algo, data_ptr, data_len);
}

/// HMAC with algorithm.
/// algo: 0=sha256, 1=sha384, 2=sha512, 3=md5, 4=sha1
/// Returns pointer to result: [len:u32][hex_string...]
/// Returns 0 on error.
export fn zig_hmac(algo: u32, key_ptr: u32, key_len: u32, data_ptr: u32, data_len: u32) u32 {
    return crypto.hmac(algo, key_ptr, key_len, data_ptr, data_len);
}

/// Generate random bytes.
/// Returns pointer to result: [len:u32][random_bytes...]
/// Returns 0 on error.
export fn zig_random_bytes(len: u32) u32 {
    return crypto.randomBytes(len);
}

/// Generate random UUID v4.
/// Returns pointer to result: [len:u32][uuid_string...]
/// Returns 0 on error.
export fn zig_random_uuid() u32 {
    return crypto.randomUuid();
}

// ============================================================================
// HTTP ABI
// ============================================================================

// Response handle storage (simple array for now)
var response_handles: [64]?*http.Response = [_]?*http.Response{null} ** 64;
var next_handle: u32 = 0;

fn storeResponse(response: *http.Response) u32 {
    const handle = next_handle;
    next_handle = (next_handle + 1) % 64;
    if (response_handles[handle]) |old| {
        old.deinit();
        wasm_allocator.destroy(old);
    }
    response_handles[handle] = response;
    return handle + 1; // 0 is reserved for error
}

fn getResponse(handle: u32) ?*http.Response {
    if (handle == 0 or handle > 64) return null;
    return response_handles[handle - 1];
}

/// Fetch URL with options.
/// method: 0=GET, 1=POST, 2=PUT, 3=DELETE, 4=PATCH, 5=HEAD
/// Returns handle to response, or 0 on error.
export fn zig_fetch(
    url_ptr: u32,
    url_len: u32,
    method: u32,
    headers_ptr: u32,
    headers_len: u32,
    body_ptr: u32,
    body_len: u32,
) u32 {
    const url = ptrToSlice(url_ptr, url_len) orelse return 0;
    const headers_json = if (headers_len > 0) ptrToSlice(headers_ptr, headers_len) else null;
    const body = if (body_len > 0) ptrToSlice(body_ptr, body_len) else null;

    const response = http.fetch(url, method, headers_json, body) catch return 0;
    return storeResponse(response);
}

/// Get response status code.
export fn zig_fetch_status(handle: u32) u32 {
    const response = getResponse(handle) orelse return 0;
    return response.status;
}

/// Get response body pointer.
/// Returns pointer to result: [len:u32][body...]
export fn zig_fetch_body(handle: u32) u32 {
    const response = getResponse(handle) orelse return 0;
    return crypto.allocResult(response.body);
}

/// Get response headers as JSON.
/// Returns pointer to result: [len:u32][json...]
export fn zig_fetch_headers(handle: u32) u32 {
    const response = getResponse(handle) orelse return 0;

    // Build JSON object from headers
    var json = std.ArrayListUnmanaged(u8){};
    defer json.deinit(wasm_allocator);

    json.appendSlice(wasm_allocator, "{") catch return 0;
    var first = true;

    var iter = response.headers.iterator();
    while (iter.next()) |entry| {
        if (!first) json.appendSlice(wasm_allocator, ",") catch return 0;
        first = false;

        json.appendSlice(wasm_allocator, "\"") catch return 0;
        json.appendSlice(wasm_allocator, entry.key_ptr.*) catch return 0;
        json.appendSlice(wasm_allocator, "\":\"") catch return 0;
        json.appendSlice(wasm_allocator, entry.value_ptr.*) catch return 0;
        json.appendSlice(wasm_allocator, "\"") catch return 0;
    }

    json.appendSlice(wasm_allocator, "}") catch return 0;

    return crypto.allocResult(json.items);
}

/// Free response.
export fn zig_fetch_free(handle: u32) void {
    if (handle == 0 or handle > 64) return;
    if (response_handles[handle - 1]) |response| {
        response.deinit();
        wasm_allocator.destroy(response);
        response_handles[handle - 1] = null;
    }
}

// ============================================================================
// Filesystem ABI (to be implemented)
// ============================================================================

/// Read file contents.
/// Returns pointer to result: [len:u32][file_contents...]
/// Returns 0 on error.
export fn zig_fs_read(path_ptr: u32, path_len: u32) u32 {
    const path = ptrToSlice(path_ptr, path_len) orelse return 0;

    // Open file via host
    const fd = host.fsOpen(path, host.O_RDONLY);
    if (fd < 0) return 0;
    defer host.fsClose(fd);

    // Get file size via stat
    const stat = host.fsStat(path) orelse return 0;
    if (stat.is_dir) return 0;

    const size: u32 = @intCast(@min(stat.size, 100 * 1024 * 1024)); // 100MB limit

    // Allocate result buffer
    const buf = wasm_allocator.alloc(u8, 4 + size) catch return 0;
    errdefer wasm_allocator.free(buf);

    // Read file
    var total_read: u32 = 0;
    while (total_read < size) {
        const n = host.fsRead(fd, buf[4 + total_read ..]);
        if (n <= 0) break;
        total_read += @intCast(n);
    }

    // Write length prefix
    std.mem.writeInt(u32, buf[0..4], total_read, .little);

    return @intFromPtr(buf.ptr);
}

/// Write file contents.
/// Returns 0 on success, -1 on error.
export fn zig_fs_write(path_ptr: u32, path_len: u32, data_ptr: u32, data_len: u32) i32 {
    const path = ptrToSlice(path_ptr, path_len) orelse return -1;
    const data = ptrToSlice(data_ptr, data_len) orelse return -1;

    // Open file for writing (create/truncate)
    const fd = host.fsOpen(path, host.O_WRONLY | host.O_CREAT | host.O_TRUNC);
    if (fd < 0) return -1;
    defer host.fsClose(fd);

    // Write data
    var total_written: u32 = 0;
    while (total_written < data_len) {
        const n = host.fsWrite(fd, data[total_written..]);
        if (n <= 0) return -1;
        total_written += @intCast(n);
    }

    return 0;
}

/// Check if file exists.
/// Returns 1 if exists, 0 if not.
export fn zig_fs_exists(path_ptr: u32, path_len: u32) i32 {
    const path = ptrToSlice(path_ptr, path_len) orelse return 0;
    const stat = host.fsStat(path);
    return if (stat != null) 1 else 0;
}

/// Get file stat.
/// Returns pointer to stat struct, or 0 on error.
export fn zig_fs_stat(path_ptr: u32, path_len: u32) u32 {
    const path = ptrToSlice(path_ptr, path_len) orelse return 0;
    const stat = host.fsStat(path) orelse return 0;

    // Allocate result: JSON format for easy JS consumption
    var json_buf: [256]u8 = undefined;
    const json = std.fmt.bufPrint(&json_buf, "{{\"size\":{d},\"mtime\":{d},\"mode\":{d},\"isDirectory\":{s}}}", .{
        stat.size,
        stat.mtime,
        stat.mode,
        if (stat.is_dir) "true" else "false",
    }) catch return 0;

    return crypto.allocResult(json);
}

/// Read directory entries.
/// Returns pointer to result: [len:u32][json_array...]
export fn zig_fs_readdir(path_ptr: u32, path_len: u32) u32 {
    const path = ptrToSlice(path_ptr, path_len) orelse return 0;

    // Buffer for directory entries (null-separated names)
    var buf: [65536]u8 = undefined;
    const count = host.fsReaddir(path, &buf);
    if (count < 0) return 0;

    // Convert to JSON array
    var json = std.ArrayListUnmanaged(u8){};
    defer json.deinit(wasm_allocator);

    json.appendSlice(wasm_allocator, "[") catch return 0;

    var i: usize = 0;
    var first = true;
    while (i < buf.len and buf[i] != 0) {
        const start = i;
        while (i < buf.len and buf[i] != 0) : (i += 1) {}
        const name = buf[start..i];
        i += 1; // Skip null

        if (!first) json.appendSlice(wasm_allocator, ",") catch return 0;
        first = false;

        json.appendSlice(wasm_allocator, "\"") catch return 0;
        json.appendSlice(wasm_allocator, name) catch return 0;
        json.appendSlice(wasm_allocator, "\"") catch return 0;
    }

    json.appendSlice(wasm_allocator, "]") catch return 0;

    return crypto.allocResult(json.items);
}

// ============================================================================
// Process ABI (to be implemented)
// ============================================================================

/// Spawn process synchronously.
/// Returns pointer to result struct, or 0 on error.
export fn zig_spawn_sync(
    cmd_ptr: u32,
    cmd_len: u32,
    timeout_ms: u32,
) u32 {
    const cmd = ptrToSlice(cmd_ptr, cmd_len) orelse return 0;

    // Spawn via host
    const spawn_result = host.procSpawn(cmd) orelse return 0;

    // Read stdout
    var stdout_buf = std.ArrayListUnmanaged(u8){};
    defer stdout_buf.deinit(wasm_allocator);

    var read_buf: [4096]u8 = undefined;
    while (true) {
        const n = host.fsRead(spawn_result.stdout_fd, &read_buf);
        if (n <= 0) break;
        stdout_buf.appendSlice(wasm_allocator, read_buf[0..@intCast(n)]) catch break;
    }
    host.fsClose(spawn_result.stdout_fd);

    // Read stderr
    var stderr_buf = std.ArrayListUnmanaged(u8){};
    defer stderr_buf.deinit(wasm_allocator);

    while (true) {
        const n = host.fsRead(spawn_result.stderr_fd, &read_buf);
        if (n <= 0) break;
        stderr_buf.appendSlice(wasm_allocator, read_buf[0..@intCast(n)]) catch break;
    }
    host.fsClose(spawn_result.stderr_fd);
    host.fsClose(spawn_result.stdin_fd);

    // Wait for process
    _ = timeout_ms; // TODO: implement timeout
    const exit_code = host.procWait(spawn_result.pid);

    // Build JSON result
    var json = std.ArrayListUnmanaged(u8){};
    json.appendSlice(wasm_allocator, "{\"status\":") catch return 0;

    var num_buf: [32]u8 = undefined;
    const status_str = std.fmt.bufPrint(&num_buf, "{d}", .{exit_code}) catch return 0;
    json.appendSlice(wasm_allocator, status_str) catch return 0;

    json.appendSlice(wasm_allocator, ",\"stdout\":\"") catch return 0;
    // TODO: escape JSON string properly
    json.appendSlice(wasm_allocator, stdout_buf.items) catch return 0;

    json.appendSlice(wasm_allocator, "\",\"stderr\":\"") catch return 0;
    json.appendSlice(wasm_allocator, stderr_buf.items) catch return 0;

    json.appendSlice(wasm_allocator, "\"}") catch return 0;

    return crypto.allocResult(json.items);
}

/// Get spawn result exit code.
export fn zig_spawn_exit_code(result_ptr: u32) i32 {
    _ = result_ptr;
    return 0; // TODO: parse from JSON result
}

/// Get spawn result stdout pointer.
export fn zig_spawn_stdout(result_ptr: u32) u32 {
    _ = result_ptr;
    return 0;
}

/// Get spawn result stderr pointer.
export fn zig_spawn_stderr(result_ptr: u32) u32 {
    _ = result_ptr;
    return 0;
}

/// Free spawn result.
export fn zig_spawn_free(result_ptr: u32) void {
    crypto.freeResult(result_ptr);
}

// ============================================================================
// Utility ABI
// ============================================================================

/// Get current time in milliseconds since epoch.
export fn zig_time_now() i64 {
    return host.timeNow();
}

/// Encode string to base64.
/// Returns pointer to result: [len:u32][base64_string...]
export fn zig_base64_encode(data_ptr: u32, data_len: u32) u32 {
    const data = ptrToSlice(data_ptr, data_len) orelse return 0;

    const encoded_len = std.base64.standard.Encoder.calcSize(data.len);
    const buf = wasm_allocator.alloc(u8, 4 + encoded_len) catch return 0;

    std.mem.writeInt(u32, buf[0..4], @intCast(encoded_len), .little);
    _ = std.base64.standard.Encoder.encode(buf[4..], data);

    return @intFromPtr(buf.ptr);
}

/// Decode base64 string.
/// Returns pointer to result: [len:u32][decoded_data...]
export fn zig_base64_decode(data_ptr: u32, data_len: u32) u32 {
    const data = ptrToSlice(data_ptr, data_len) orelse return 0;

    const max_decoded_len = std.base64.standard.Decoder.calcSizeUpperBound(data.len);
    const buf = wasm_allocator.alloc(u8, 4 + max_decoded_len) catch return 0;

    const decoded_len = std.base64.standard.Decoder.decode(buf[4..], data) catch {
        wasm_allocator.free(buf);
        return 0;
    };

    std.mem.writeInt(u32, buf[0..4], @intCast(decoded_len), .little);

    return @intFromPtr(buf.ptr);
}

// ============================================================================
// Memory Helpers
// ============================================================================

fn ptrToSlice(ptr: u32, len: u32) ?[]const u8 {
    if (ptr == 0 and len > 0) return null;
    if (len == 0) return &[_]u8{};
    return @as([*]const u8, @ptrFromInt(ptr))[0..len];
}
