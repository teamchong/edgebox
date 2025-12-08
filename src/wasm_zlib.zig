/// WASM Zlib Implementation
/// Provides gzip/gunzip/deflate/inflate using Zig's std.compress.flate
///
/// Note: Compression (gzip/deflate/deflateZlib) currently not implemented
/// due to Zig 0.15 std.compress.flate.Compress API issues.
/// Decompression is fully supported.
const std = @import("std");
const Allocator = std.mem.Allocator;
const flate = std.compress.flate;

pub const ZlibError = error{
    InvalidInput,
    DecompressionFailed,
    CompressionFailed,
    OutOfMemory,
    NotImplemented,
};

/// Decompress gzip data
pub fn gunzip(allocator: Allocator, compressed: []const u8) ZlibError![]u8 {
    if (compressed.len < 10) return ZlibError.InvalidInput;

    // Create input reader from slice
    var input: std.Io.Reader = .fixed(compressed);

    // Create allocating writer for output
    var output: std.Io.Writer.Allocating = .init(allocator);
    errdefer output.deinit();

    // Create gzip decompressor
    var decompress: flate.Decompress = .init(&input, .gzip, &.{});

    // Stream all decompressed data to output
    _ = decompress.reader.streamRemaining(&output.writer) catch return ZlibError.DecompressionFailed;

    // Get the result slice
    return output.toOwnedSlice() catch return ZlibError.OutOfMemory;
}

/// Compress data to gzip format
/// TODO: Implement when Zig flate.Compress API is stable
pub fn gzip(_: Allocator, _: []const u8) ZlibError![]u8 {
    return ZlibError.NotImplemented;
}

/// Decompress raw deflate data
pub fn inflate(allocator: Allocator, compressed: []const u8) ZlibError![]u8 {
    if (compressed.len == 0) return ZlibError.InvalidInput;

    var input: std.Io.Reader = .fixed(compressed);
    var output: std.Io.Writer.Allocating = .init(allocator);
    errdefer output.deinit();

    // Raw deflate (no header)
    var decompress: flate.Decompress = .init(&input, .raw, &.{});
    _ = decompress.reader.streamRemaining(&output.writer) catch return ZlibError.DecompressionFailed;

    return output.toOwnedSlice() catch return ZlibError.OutOfMemory;
}

/// Compress data using raw deflate
/// TODO: Implement when Zig flate.Compress API is stable
pub fn deflate(_: Allocator, _: []const u8) ZlibError![]u8 {
    return ZlibError.NotImplemented;
}

/// Decompress zlib-wrapped deflate data (deflate with zlib header)
pub fn inflateZlib(allocator: Allocator, compressed: []const u8) ZlibError![]u8 {
    if (compressed.len < 2) return ZlibError.InvalidInput;

    var input: std.Io.Reader = .fixed(compressed);
    var output: std.Io.Writer.Allocating = .init(allocator);
    errdefer output.deinit();

    // Zlib header format
    var decompress: flate.Decompress = .init(&input, .zlib, &.{});
    _ = decompress.reader.streamRemaining(&output.writer) catch return ZlibError.DecompressionFailed;

    return output.toOwnedSlice() catch return ZlibError.OutOfMemory;
}

/// Compress data using deflate with zlib header
/// TODO: Implement when Zig flate.Compress API is stable
pub fn deflateZlib(_: Allocator, _: []const u8) ZlibError![]u8 {
    return ZlibError.NotImplemented;
}

test "gunzip decompresses gzip data" {
    const allocator = std.testing.allocator;

    // Pre-compressed gzip data for "Hello, World!"
    // Generated with: echo -n "Hello, World!" | gzip | xxd -i
    const compressed = [_]u8{
        0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03,
        0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0xd7, 0x51, 0x08, 0xcf, 0x2f,
        0xca, 0x49, 0x51, 0x04, 0x00, 0xd0, 0xc3, 0x4a, 0xd9, 0x0d,
        0x00, 0x00, 0x00,
    };

    const decompressed = try gunzip(allocator, &compressed);
    defer allocator.free(decompressed);

    try std.testing.expectEqualStrings("Hello, World!", decompressed);
}

test "inflateZlib decompresses zlib data" {
    const allocator = std.testing.allocator;

    // Pre-compressed zlib data for "Hello, World!"
    // Generated with: echo -n "Hello, World!" | python3 -c "import sys,zlib;sys.stdout.buffer.write(zlib.compress(sys.stdin.buffer.read()))" | xxd -i
    const compressed = [_]u8{
        0x78, 0x9c, 0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0xd7, 0x51, 0x08,
        0xcf, 0x2f, 0xca, 0x49, 0x51, 0x04, 0x00, 0x1f, 0x9e, 0x04,
        0x6a,
    };

    const decompressed = try inflateZlib(allocator, &compressed);
    defer allocator.free(decompressed);

    try std.testing.expectEqualStrings("Hello, World!", decompressed);
}

test "inflate decompresses raw deflate data" {
    const allocator = std.testing.allocator;

    // Raw deflate data (no zlib/gzip wrapper)
    // This is the deflate payload from the zlib stream above (skip 2-byte header, omit 4-byte trailer)
    const compressed = [_]u8{
        0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0xd7, 0x51, 0x08,
        0xcf, 0x2f, 0xca, 0x49, 0x51, 0x04, 0x00,
    };

    const decompressed = try inflate(allocator, &compressed);
    defer allocator.free(decompressed);

    try std.testing.expectEqualStrings("Hello, World!", decompressed);
}
