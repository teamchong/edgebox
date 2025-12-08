/// WASM SIMD Utilities for EdgeBox
///
/// Requires: -mcpu=generic+simd128 build flag
/// These functions use 128-bit SIMD vectors for 16x throughput on string operations.
const std = @import("std");

/// Check if all bytes in a slice are ASCII (< 128)
/// Uses SIMD to process 16 bytes at a time
pub fn isAscii(data: []const u8) bool {
    const chunk_size = 16;
    var i: usize = 0;

    // Process 16 bytes at a time with SIMD
    while (i + chunk_size <= data.len) : (i += chunk_size) {
        const chunk: @Vector(16, u8) = data[i..][0..16].*;
        // Check if any byte has high bit set (>= 128)
        const high_bits = chunk & @as(@Vector(16, u8), @splat(0x80));
        if (@reduce(.Or, high_bits) != 0) {
            return false;
        }
    }

    // Handle remaining bytes
    while (i < data.len) : (i += 1) {
        if (data[i] >= 128) return false;
    }

    return true;
}

/// Check if all bytes are digits (0-9)
/// Uses SIMD comparison
pub fn isAllDigits(data: []const u8) bool {
    if (data.len == 0) return false;

    const chunk_size = 16;
    const zero: @Vector(16, u8) = @splat('0');
    const nine: @Vector(16, u8) = @splat('9');
    var i: usize = 0;

    while (i + chunk_size <= data.len) : (i += chunk_size) {
        const chunk: @Vector(16, u8) = data[i..][0..16].*;
        const ge_zero = chunk >= zero;
        const le_nine = chunk <= nine;
        const is_digit = @select(bool, ge_zero, le_nine, @as(@Vector(16, bool), @splat(false)));
        if (!@reduce(.And, is_digit)) return false;
    }

    while (i < data.len) : (i += 1) {
        if (data[i] < '0' or data[i] > '9') return false;
    }

    return true;
}

/// Check if all bytes are whitespace (space, tab, newline, etc.)
pub fn isAllWhitespace(data: []const u8) bool {
    if (data.len == 0) return false;

    for (data) |c| {
        if (c != ' ' and c != '\t' and c != '\n' and c != '\r' and c != 0x0B and c != 0x0C) {
            return false;
        }
    }
    return true;
}

/// Find first occurrence of a byte in a slice using SIMD
/// Returns index or null if not found
pub fn findByte(data: []const u8, needle: u8) ?usize {
    const chunk_size = 16;
    const needle_vec: @Vector(16, u8) = @splat(needle);
    var i: usize = 0;

    while (i + chunk_size <= data.len) : (i += chunk_size) {
        const chunk: @Vector(16, u8) = data[i..][0..16].*;
        const matches = chunk == needle_vec;
        // Check if any match
        if (@reduce(.Or, matches)) {
            // Find first match in this chunk
            inline for (0..16) |j| {
                if (matches[j]) return i + j;
            }
        }
    }

    // Handle remaining bytes
    while (i < data.len) : (i += 1) {
        if (data[i] == needle) return i;
    }

    return null;
}

/// Count occurrences of a byte using SIMD
pub fn countByte(data: []const u8, needle: u8) usize {
    const chunk_size = 16;
    const needle_vec: @Vector(16, u8) = @splat(needle);
    var count: usize = 0;
    var i: usize = 0;

    while (i + chunk_size <= data.len) : (i += chunk_size) {
        const chunk: @Vector(16, u8) = data[i..][0..16].*;
        const matches = chunk == needle_vec;
        // Count matches using popcount
        count += @popCount(@as(u16, @bitCast(matches)));
    }

    // Handle remaining bytes
    while (i < data.len) : (i += 1) {
        if (data[i] == needle) count += 1;
    }

    return count;
}

/// Check if string contains any JSON special characters that need escaping
/// (quote, backslash, control chars)
pub fn hasJsonSpecialChars(data: []const u8) bool {
    const chunk_size = 16;
    const quote: @Vector(16, u8) = @splat('"');
    const backslash: @Vector(16, u8) = @splat('\\');
    const control_max: @Vector(16, u8) = @splat(0x1F);
    var i: usize = 0;

    while (i + chunk_size <= data.len) : (i += chunk_size) {
        const chunk: @Vector(16, u8) = data[i..][0..16].*;

        // Check for quotes or backslashes
        const is_quote = chunk == quote;
        const is_backslash = chunk == backslash;
        const is_control = chunk <= control_max;

        const has_special = @reduce(.Or, is_quote) or @reduce(.Or, is_backslash) or @reduce(.Or, is_control);
        if (has_special) return true;
    }

    // Handle remaining bytes
    while (i < data.len) : (i += 1) {
        const c = data[i];
        if (c == '"' or c == '\\' or c <= 0x1F) return true;
    }

    return false;
}

/// Compare two byte slices for equality using SIMD
pub fn eqlSimd(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;

    const chunk_size = 16;
    var i: usize = 0;

    while (i + chunk_size <= a.len) : (i += chunk_size) {
        const chunk_a: @Vector(16, u8) = a[i..][0..16].*;
        const chunk_b: @Vector(16, u8) = b[i..][0..16].*;
        if (!@reduce(.And, chunk_a == chunk_b)) return false;
    }

    // Handle remaining bytes
    while (i < a.len) : (i += 1) {
        if (a[i] != b[i]) return false;
    }

    return true;
}

// ============================================================================
// Tests
// ============================================================================

test "isAscii" {
    const testing = std.testing;

    try testing.expect(isAscii("Hello, World!"));
    try testing.expect(isAscii("0123456789"));
    try testing.expect(!isAscii("Hello, 世界!"));
    try testing.expect(!isAscii("\x80"));
    try testing.expect(isAscii(""));
}

test "isAllDigits" {
    const testing = std.testing;

    try testing.expect(isAllDigits("0123456789"));
    try testing.expect(isAllDigits("42"));
    try testing.expect(!isAllDigits("12.34"));
    try testing.expect(!isAllDigits("abc"));
    try testing.expect(!isAllDigits(""));
}

test "findByte" {
    const testing = std.testing;

    try testing.expectEqual(findByte("Hello, World!", ','), 5);
    try testing.expectEqual(findByte("Hello, World!", 'x'), null);
    try testing.expectEqual(findByte("abcdefghijklmnopqrstuvwxyz", 'z'), 25);
}

test "countByte" {
    const testing = std.testing;

    try testing.expectEqual(countByte("Hello, World!", 'o'), 2);
    try testing.expectEqual(countByte("aaaaaaaaaaaaaaaa", 'a'), 16); // Exactly one SIMD chunk
    try testing.expectEqual(countByte("aaaaaaaaaaaaaaaaa", 'a'), 17); // One chunk + 1
}

test "hasJsonSpecialChars" {
    const testing = std.testing;

    try testing.expect(hasJsonSpecialChars("Hello\"World"));
    try testing.expect(hasJsonSpecialChars("Hello\\World"));
    try testing.expect(hasJsonSpecialChars("Hello\nWorld"));
    try testing.expect(!hasJsonSpecialChars("Hello World"));
}
