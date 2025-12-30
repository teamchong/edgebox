//! JSON Utilities for EdgeBox
//!
//! Provides JSON escaping functions consolidated from:
//! - abi/mod.zig
//! - edgebox_wamr.zig

const std = @import("std");

/// Escape a string for safe JSON embedding (allocating version).
/// Handles: " \ \n \r \t and control characters (0x00-0x1F)
pub fn escapeJsonString(allocator: std.mem.Allocator, s: []const u8) ![]u8 {
    var result = std.ArrayListUnmanaged(u8){};
    errdefer result.deinit(allocator);

    for (s) |ch| {
        switch (ch) {
            '"' => try result.appendSlice(allocator, "\\\""),
            '\\' => try result.appendSlice(allocator, "\\\\"),
            '\n' => try result.appendSlice(allocator, "\\n"),
            '\r' => try result.appendSlice(allocator, "\\r"),
            '\t' => try result.appendSlice(allocator, "\\t"),
            0x00...0x08, 0x0B, 0x0C, 0x0E...0x1F => {
                // Control characters as \uXXXX
                var buf: [6]u8 = undefined;
                _ = std.fmt.bufPrint(&buf, "\\u{x:0>4}", .{ch}) catch continue;
                try result.appendSlice(allocator, &buf);
            },
            else => try result.append(allocator, ch),
        }
    }
    return result.toOwnedSlice(allocator);
}

/// Escape special characters for JSON string (writes to std.io.Writer).
/// More efficient when building large responses - no allocations.
pub fn escapeJsonStringToWriter(writer: anytype, str: []const u8) !void {
    for (str) |ch| {
        switch (ch) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => {
                if (ch < 0x20) {
                    // Control character - escape as \uXXXX
                    var hex_buf: [6]u8 = undefined;
                    const hex = std.fmt.bufPrint(&hex_buf, "\\u{x:0>4}", .{ch}) catch continue;
                    try writer.writeAll(hex);
                } else {
                    try writer.writeByte(ch);
                }
            },
        }
    }
}

test "escapeJsonString basic" {
    const allocator = std.testing.allocator;

    // Basic escaping
    const escaped = try escapeJsonString(allocator, "hello\nworld");
    defer allocator.free(escaped);
    try std.testing.expectEqualStrings("hello\\nworld", escaped);

    // Quotes
    const escaped2 = try escapeJsonString(allocator, "say \"hello\"");
    defer allocator.free(escaped2);
    try std.testing.expectEqualStrings("say \\\"hello\\\"", escaped2);
}

test "escapeJsonStringToWriter basic" {
    var buf: [64]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);

    try escapeJsonStringToWriter(stream.writer(), "hello\nworld");
    try std.testing.expectEqualStrings("hello\\nworld", stream.getWritten());
}
