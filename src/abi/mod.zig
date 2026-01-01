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
const json = @import("../json.zig");
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

// Response handle storage with generation counter to prevent use-after-free
// Each slot tracks its generation; handles encode both slot index and generation
const ResponseEntry = struct {
    response: ?*http.Response,
    generation: u16, // Incremented on each reuse
};

const MAX_RESPONSE_HANDLES: usize = 256; // Increased from 64
var response_handles: [MAX_RESPONSE_HANDLES]ResponseEntry = [_]ResponseEntry{.{ .response = null, .generation = 0 }} ** MAX_RESPONSE_HANDLES;
var next_slot: u32 = 0;

// Handle format: (generation << 16) | (slot + 1)
// This allows detecting stale handles even after slot reuse
fn storeResponse(response: *http.Response) u32 {
    const slot = next_slot;
    next_slot = (next_slot + 1) % MAX_RESPONSE_HANDLES;

    // Free old response if slot was occupied
    if (response_handles[slot].response) |old| {
        old.deinit();
        wasm_allocator.destroy(old);
    }

    // Increment generation for this slot
    response_handles[slot].generation +%= 1;
    response_handles[slot].response = response;

    // Encode handle: (generation << 16) | (slot + 1)
    const gen: u32 = response_handles[slot].generation;
    return (gen << 16) | (slot + 1);
}

fn getResponse(handle: u32) ?*http.Response {
    if (handle == 0) return null;

    // Decode handle
    const slot = (handle & 0xFFFF) - 1;
    const expected_gen: u16 = @truncate(handle >> 16);

    if (slot >= MAX_RESPONSE_HANDLES) return null;

    // Check generation matches (detects use-after-free)
    if (response_handles[slot].generation != expected_gen) return null;

    return response_handles[slot].response;
}

fn getSlotFromHandle(handle: u32) ?u32 {
    if (handle == 0) return null;
    const slot = (handle & 0xFFFF) - 1;
    if (slot >= MAX_RESPONSE_HANDLES) return null;
    const expected_gen: u16 = @truncate(handle >> 16);
    if (response_handles[slot].generation != expected_gen) return null;
    return slot;
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

        // Escape header keys and values for valid JSON
        const escaped_key = escapeJsonString(wasm_allocator, entry.key_ptr.*) catch return 0;
        defer wasm_allocator.free(escaped_key);
        const escaped_value = escapeJsonString(wasm_allocator, entry.value_ptr.*) catch return 0;
        defer wasm_allocator.free(escaped_value);

        json.appendSlice(wasm_allocator, "\"") catch return 0;
        json.appendSlice(wasm_allocator, escaped_key) catch return 0;
        json.appendSlice(wasm_allocator, "\":\"") catch return 0;
        json.appendSlice(wasm_allocator, escaped_value) catch return 0;
        json.appendSlice(wasm_allocator, "\"") catch return 0;
    }

    json.appendSlice(wasm_allocator, "}") catch return 0;

    return crypto.allocResult(json.items);
}

/// Free response.
export fn zig_fetch_free(handle: u32) void {
    const slot = getSlotFromHandle(handle) orelse return;
    if (response_handles[slot].response) |response| {
        response.deinit();
        wasm_allocator.destroy(response);
        response_handles[slot].response = null;
        // Note: generation stays the same - will be incremented on next store
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
    // Zero-initialize to prevent reading garbage if host doesn't fill buffer
    var buf: [65536]u8 = [_]u8{0} ** 65536;
    const count = host.fsReaddir(path, &buf);
    if (count < 0) return 0;

    // Use count as upper bound for iteration (bytes written by host)
    const data_len: usize = @intCast(count);

    // Convert to JSON array
    var json_out = std.ArrayListUnmanaged(u8){};
    defer json_out.deinit(wasm_allocator);

    json_out.appendSlice(wasm_allocator, "[") catch return 0;

    var i: usize = 0;
    var first = true;
    // Only iterate through bytes that were actually written by host
    while (i < data_len) {
        const start = i;
        // Find end of current null-terminated string
        while (i < data_len and buf[i] != 0) : (i += 1) {}
        const name = buf[start..i];
        i += 1; // Skip null terminator

        // Skip empty names (consecutive nulls)
        if (name.len == 0) continue;

        if (!first) json_out.appendSlice(wasm_allocator, ",") catch return 0;
        first = false;

        // Escape filename for JSON (handles special chars in filenames)
        const escaped_name = escapeJsonString(wasm_allocator, name) catch return 0;
        defer wasm_allocator.free(escaped_name);

        json_out.appendSlice(wasm_allocator, "\"") catch return 0;
        json_out.appendSlice(wasm_allocator, escaped_name) catch return 0;
        json_out.appendSlice(wasm_allocator, "\"") catch return 0;
    }

    json_out.appendSlice(wasm_allocator, "]") catch return 0;

    return crypto.allocResult(json_out.items);
}

// ============================================================================
// Process ABI (to be implemented)
// ============================================================================

// Global to store last spawn exit code (for zig_spawn_exit_code)
var g_last_spawn_exit_code: i32 = 0;

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

    // Close stdin first - we're not writing to it
    host.fsClose(spawn_result.stdin_fd);

    // Wait for process FIRST with optional timeout
    // This ensures we don't block indefinitely on read loops
    const exit_code = if (timeout_ms > 0)
        host.procWaitTimeout(spawn_result.pid, timeout_ms)
    else
        host.procWait(spawn_result.pid);

    // Now read stdout (process has exited, so reads will complete)
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

    // Store exit code for zig_spawn_exit_code
    g_last_spawn_exit_code = exit_code;

    // Escape stdout/stderr for JSON embedding
    const escaped_stdout = escapeJsonString(wasm_allocator, stdout_buf.items) catch return 0;
    defer wasm_allocator.free(escaped_stdout);
    const escaped_stderr = escapeJsonString(wasm_allocator, stderr_buf.items) catch return 0;
    defer wasm_allocator.free(escaped_stderr);

    // Build JSON result
    var json = std.ArrayListUnmanaged(u8){};
    json.appendSlice(wasm_allocator, "{\"status\":") catch return 0;

    var num_buf: [32]u8 = undefined;
    const status_str = std.fmt.bufPrint(&num_buf, "{d}", .{exit_code}) catch return 0;
    json.appendSlice(wasm_allocator, status_str) catch return 0;

    json.appendSlice(wasm_allocator, ",\"stdout\":\"") catch return 0;
    json.appendSlice(wasm_allocator, escaped_stdout) catch return 0;

    json.appendSlice(wasm_allocator, "\",\"stderr\":\"") catch return 0;
    json.appendSlice(wasm_allocator, escaped_stderr) catch return 0;

    json.appendSlice(wasm_allocator, "\"}") catch return 0;

    return crypto.allocResult(json.items);
}

/// Get spawn result exit code.
/// Returns the exit code from the last zig_spawn_sync call.
export fn zig_spawn_exit_code(result_ptr: u32) i32 {
    _ = result_ptr; // result_ptr not needed since we store in global
    return g_last_spawn_exit_code;
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

// Use shared JSON escape utility
const escapeJsonString = json.escapeJsonString;
