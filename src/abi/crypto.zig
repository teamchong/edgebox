//! Crypto Implementation for EdgeBox ABI
//!
//! All crypto operations run in WASM sandbox using Zig std.crypto.
//! Host only provides random bytes via host_random_get().
//!
//! This is a KEY SECURITY FEATURE:
//! - Crypto bugs crash WASM instance, not host
//! - Side-channel attacks limited to WASM sandbox
//! - Same timing guarantees as native (Zig std.crypto is constant-time)

const std = @import("std");
const host = @import("host.zig");

// Use WASM allocator for dynamic allocations
const wasm_allocator = std.heap.wasm_allocator;

// ============================================================================
// Hash Algorithms
// ============================================================================

pub const HashAlgorithm = enum(u32) {
    sha256 = 0,
    sha384 = 1,
    sha512 = 2,
    md5 = 3,
    sha1 = 4,
};

/// Hash data with specified algorithm.
/// Returns pointer to result buffer: [len:u32][hex_string...]
/// Returns 0 on error.
pub fn hash(algo: u32, data_ptr: u32, data_len: u32) u32 {
    const algorithm = std.meta.intToEnum(HashAlgorithm, algo) catch return 0;
    const data = ptrToSlice(data_ptr, data_len) orelse return 0;

    return switch (algorithm) {
        .sha256 => hashWith(std.crypto.hash.sha2.Sha256, data),
        .sha384 => hashWith(std.crypto.hash.sha2.Sha384, data),
        .sha512 => hashWith(std.crypto.hash.sha2.Sha512, data),
        .md5 => hashWith(std.crypto.hash.Md5, data),
        .sha1 => hashWith(std.crypto.hash.Sha1, data),
    };
}

fn hashWith(comptime H: type, data: []const u8) u32 {
    var hasher = H.init(.{});
    hasher.update(data);
    const digest = hasher.finalResult();

    return allocResultHex(&digest);
}

// ============================================================================
// HMAC
// ============================================================================

/// HMAC with specified algorithm.
/// Returns pointer to result buffer: [len:u32][hex_string...]
/// Returns 0 on error.
pub fn hmac(algo: u32, key_ptr: u32, key_len: u32, data_ptr: u32, data_len: u32) u32 {
    const algorithm = std.meta.intToEnum(HashAlgorithm, algo) catch return 0;
    const key = ptrToSlice(key_ptr, key_len) orelse return 0;
    const data = ptrToSlice(data_ptr, data_len) orelse return 0;

    return switch (algorithm) {
        .sha256 => hmacWith(std.crypto.auth.hmac.sha2.HmacSha256, key, data),
        .sha384 => hmacWith(std.crypto.auth.hmac.sha2.HmacSha384, key, data),
        .sha512 => hmacWith(std.crypto.auth.hmac.sha2.HmacSha512, key, data),
        .md5 => hmacWith(std.crypto.auth.hmac.HmacMd5, key, data),
        .sha1 => hmacWith(std.crypto.auth.hmac.HmacSha1, key, data),
    };
}

fn hmacWith(comptime H: type, key: []const u8, data: []const u8) u32 {
    var h = H.init(key);
    h.update(data);
    const mac = h.finalResult();

    return allocResultHex(&mac);
}

// ============================================================================
// Random Bytes
// ============================================================================

/// Generate cryptographically secure random bytes.
/// Uses host_random_get() - the ONLY host crypto dependency.
/// Returns pointer to result buffer: [len:u32][random_bytes...]
/// Returns 0 on error.
pub fn randomBytes(len: u32) u32 {
    if (len == 0 or len > 65536) return 0; // Sanity limit

    // Allocate: 4 bytes for length + len bytes for data
    const buf = wasm_allocator.alloc(u8, 4 + len) catch return 0;

    // Write length prefix
    std.mem.writeInt(u32, buf[0..4], len, .little);

    // Fill with random bytes from host
    host.randomGet(buf[4..]);

    return @intFromPtr(buf.ptr);
}

/// Generate random UUID v4.
/// Returns pointer to result buffer: [len:u32][uuid_string...]
pub fn randomUuid() u32 {
    var uuid_bytes: [16]u8 = undefined;
    host.randomGet(&uuid_bytes);

    // Set version (4) and variant (RFC 4122)
    uuid_bytes[6] = (uuid_bytes[6] & 0x0f) | 0x40;
    uuid_bytes[8] = (uuid_bytes[8] & 0x3f) | 0x80;

    // Format as string: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx (36 chars)
    const buf = wasm_allocator.alloc(u8, 4 + 36) catch return 0;
    std.mem.writeInt(u32, buf[0..4], 36, .little);

    const hex_chars = "0123456789abcdef";
    var out_idx: usize = 4;
    for (uuid_bytes, 0..) |byte, i| {
        if (i == 4 or i == 6 or i == 8 or i == 10) {
            buf[out_idx] = '-';
            out_idx += 1;
        }
        buf[out_idx] = hex_chars[byte >> 4];
        buf[out_idx + 1] = hex_chars[byte & 0x0f];
        out_idx += 2;
    }

    return @intFromPtr(buf.ptr);
}

// ============================================================================
// Result Helpers
// ============================================================================

/// Allocate result buffer with hex-encoded data.
/// Format: [len:u32][hex_string...]
fn allocResultHex(data: []const u8) u32 {
    const hex_len = data.len * 2;
    const buf = wasm_allocator.alloc(u8, 4 + hex_len) catch return 0;

    // Write length prefix
    std.mem.writeInt(u32, buf[0..4], @intCast(hex_len), .little);

    // Write hex string
    const hex_chars = "0123456789abcdef";
    for (data, 0..) |byte, i| {
        buf[4 + i * 2] = hex_chars[byte >> 4];
        buf[4 + i * 2 + 1] = hex_chars[byte & 0x0f];
    }

    return @intFromPtr(buf.ptr);
}

/// Allocate result buffer with raw data.
/// Format: [len:u32][data...]
pub fn allocResult(data: []const u8) u32 {
    const buf = wasm_allocator.alloc(u8, 4 + data.len) catch return 0;
    std.mem.writeInt(u32, buf[0..4], @intCast(data.len), .little);
    @memcpy(buf[4..][0..data.len], data);
    return @intFromPtr(buf.ptr);
}

/// Free a result buffer allocated by this module.
pub fn freeResult(ptr: u32) void {
    if (ptr == 0) return;
    const buf_ptr: [*]u8 = @ptrFromInt(ptr);
    const len = std.mem.readInt(u32, buf_ptr[0..4], .little);
    wasm_allocator.free(buf_ptr[0 .. 4 + len]);
}

/// Get length of a result buffer.
pub fn resultLen(ptr: u32) u32 {
    if (ptr == 0) return 0;
    const buf_ptr: [*]const u8 = @ptrFromInt(ptr);
    return std.mem.readInt(u32, buf_ptr[0..4], .little);
}

/// Get data pointer of a result buffer (skips length prefix).
pub fn resultData(ptr: u32) u32 {
    if (ptr == 0) return 0;
    return ptr + 4;
}

// ============================================================================
// Memory Helpers
// ============================================================================

/// Convert WASM pointer to slice.
fn ptrToSlice(ptr: u32, len: u32) ?[]const u8 {
    if (ptr == 0 and len > 0) return null;
    if (len == 0) return &[_]u8{};
    return @as([*]const u8, @ptrFromInt(ptr))[0..len];
}

/// Convert WASM pointer to mutable slice.
fn ptrToSliceMut(ptr: u32, len: u32) ?[]u8 {
    if (ptr == 0 and len > 0) return null;
    if (len == 0) return &[_]u8{};
    return @as([*]u8, @ptrFromInt(ptr))[0..len];
}

// ============================================================================
// Tests (run on host, not in WASM)
// ============================================================================

test "sha256 hash" {
    // This test would run on host with mock allocator
    // In WASM, uses wasm_allocator
}

test "hmac sha256" {
    // Test HMAC generation
}
