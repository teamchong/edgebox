//! Encoding Utilities for EdgeBox
//!
//! Provides common encoding constants and functions consolidated from:
//! - abi/crypto.zig
//! - component/impls/crypto_impl.zig
//! - polyfills/buffer.zig
//! - polyfills/crypto.zig
//! - polyfills/encoding.zig
//! - wasm/native_bindings.zig
//! - wasm_main_static.zig

const std = @import("std");

/// Lowercase hex character set for encoding
pub const hex_chars: *const [16]u8 = "0123456789abcdef";

/// Uppercase hex character set for encoding
pub const hex_chars_upper: *const [16]u8 = "0123456789ABCDEF";

/// Encode bytes to hex string (allocating version)
/// Caller owns returned memory.
pub fn hexEncode(allocator: std.mem.Allocator, bytes: []const u8) ![]u8 {
    const hex = try allocator.alloc(u8, bytes.len * 2);
    hexEncodeToSlice(bytes, hex);
    return hex;
}

/// Encode bytes to hex string (non-allocating version)
/// Output buffer must be at least bytes.len * 2 in size.
pub fn hexEncodeToSlice(bytes: []const u8, output: []u8) void {
    std.debug.assert(output.len >= bytes.len * 2);
    for (bytes, 0..) |byte, i| {
        output[i * 2] = hex_chars[byte >> 4];
        output[i * 2 + 1] = hex_chars[byte & 0x0F];
    }
}

/// Decode hex string to bytes (allocating version)
/// Returns null if input has odd length or contains non-hex characters.
pub fn hexDecode(allocator: std.mem.Allocator, hex: []const u8) !?[]u8 {
    if (hex.len % 2 != 0) return null;

    const output = try allocator.alloc(u8, hex.len / 2);
    errdefer allocator.free(output);

    for (0..output.len) |i| {
        const high = hexCharToValue(hex[i * 2]) orelse {
            allocator.free(output);
            return null;
        };
        const low = hexCharToValue(hex[i * 2 + 1]) orelse {
            allocator.free(output);
            return null;
        };
        output[i] = (high << 4) | low;
    }
    return output;
}

/// Convert a single hex character to its numeric value
fn hexCharToValue(c: u8) ?u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        'a'...'f' => c - 'a' + 10,
        'A'...'F' => c - 'A' + 10,
        else => null,
    };
}

// ============================================================================
// Tests
// ============================================================================

test "hexEncode basic" {
    const allocator = std.testing.allocator;

    const input = "\x00\x01\x0f\xff";
    const encoded = try hexEncode(allocator, input);
    defer allocator.free(encoded);

    try std.testing.expectEqualStrings("00010fff", encoded);
}

test "hexEncodeToSlice" {
    var output: [8]u8 = undefined;
    hexEncodeToSlice("\xde\xad\xbe\xef", &output);
    try std.testing.expectEqualStrings("deadbeef", &output);
}

test "hexDecode basic" {
    const allocator = std.testing.allocator;

    const decoded = (try hexDecode(allocator, "deadbeef")).?;
    defer allocator.free(decoded);

    try std.testing.expectEqualSlices(u8, "\xde\xad\xbe\xef", decoded);
}

test "hexDecode invalid" {
    const allocator = std.testing.allocator;

    // Odd length
    try std.testing.expect((try hexDecode(allocator, "abc")) == null);

    // Invalid character
    try std.testing.expect((try hexDecode(allocator, "ghij")) == null);
}
