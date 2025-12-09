/// WASM Zlib Implementation
/// Provides gzip/gunzip/deflate/inflate using Zig's std.compress.flate for decompression
/// and custom implementation for compression (stored blocks format).
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

/// Compress data to gzip format using stored blocks (no compression, but valid gzip)
pub fn gzip(allocator: Allocator, data: []const u8) ZlibError![]u8 {
    // GZIP format:
    // - 10 byte header
    // - deflate payload (stored blocks)
    // - 4 byte CRC32
    // - 4 byte original size (mod 2^32)

    const deflate_data = try deflate(allocator, data);
    defer allocator.free(deflate_data);

    // Calculate output size: header(10) + deflate + crc32(4) + size(4)
    const total_size = 10 + deflate_data.len + 8;
    const result = allocator.alloc(u8, total_size) catch return ZlibError.OutOfMemory;
    errdefer allocator.free(result);

    // GZIP header (10 bytes)
    // ID1=0x1f, ID2=0x8b, CM=8 (deflate), FLG=0, MTIME=0, XFL=0, OS=3 (Unix)
    result[0] = 0x1f; // ID1
    result[1] = 0x8b; // ID2
    result[2] = 0x08; // CM (deflate)
    result[3] = 0x00; // FLG
    result[4] = 0x00; // MTIME[0]
    result[5] = 0x00; // MTIME[1]
    result[6] = 0x00; // MTIME[2]
    result[7] = 0x00; // MTIME[3]
    result[8] = 0x00; // XFL
    result[9] = 0x03; // OS (Unix)

    // Copy deflate payload
    @memcpy(result[10 .. 10 + deflate_data.len], deflate_data);

    // CRC32 of original data
    const crc = std.hash.Crc32.hash(data);
    std.mem.writeInt(u32, result[10 + deflate_data.len ..][0..4], crc, .little);

    // Original size (mod 2^32)
    std.mem.writeInt(u32, result[10 + deflate_data.len + 4 ..][0..4], @truncate(data.len), .little);

    return result;
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

/// Compress data using raw deflate (stored blocks - no actual compression)
/// Uses deflate "stored" block format which is valid deflate but uncompressed.
pub fn deflate(allocator: Allocator, data: []const u8) ZlibError![]u8 {
    // Deflate stored block format:
    // Each block: 1 byte header + 2 bytes LEN + 2 bytes NLEN + data
    // Max block size is 65535 bytes

    const max_block_size: usize = 65535;
    const num_blocks = if (data.len == 0) 1 else (data.len + max_block_size - 1) / max_block_size;

    // Each block needs 5 bytes header
    const output_size = num_blocks * 5 + data.len;
    const result = allocator.alloc(u8, output_size) catch return ZlibError.OutOfMemory;
    errdefer allocator.free(result);

    var out_pos: usize = 0;
    var in_pos: usize = 0;

    while (in_pos < data.len or (data.len == 0 and in_pos == 0)) {
        const remaining = data.len - in_pos;
        const block_size: u16 = @intCast(@min(remaining, max_block_size));
        const is_final = (in_pos + block_size >= data.len);

        // Block header byte: BFINAL (1 bit) + BTYPE (2 bits)
        // BTYPE=00 means stored block
        result[out_pos] = if (is_final) 0x01 else 0x00;
        out_pos += 1;

        // LEN (2 bytes, little-endian)
        std.mem.writeInt(u16, result[out_pos..][0..2], block_size, .little);
        out_pos += 2;

        // NLEN (one's complement of LEN)
        std.mem.writeInt(u16, result[out_pos..][0..2], ~block_size, .little);
        out_pos += 2;

        // Data
        if (block_size > 0) {
            @memcpy(result[out_pos .. out_pos + block_size], data[in_pos .. in_pos + block_size]);
            out_pos += block_size;
            in_pos += block_size;
        }

        // Handle empty data case
        if (data.len == 0) break;
    }

    // Shrink to actual size (should already be correct)
    return allocator.realloc(result, out_pos) catch result[0..out_pos];
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
pub fn deflateZlib(allocator: Allocator, data: []const u8) ZlibError![]u8 {
    // Zlib format:
    // - 2 byte header (CMF + FLG)
    // - deflate payload
    // - 4 byte Adler-32 checksum (big-endian)

    const deflate_data = try deflate(allocator, data);
    defer allocator.free(deflate_data);

    // Calculate output size: header(2) + deflate + adler32(4)
    const total_size = 2 + deflate_data.len + 4;
    const result = allocator.alloc(u8, total_size) catch return ZlibError.OutOfMemory;
    errdefer allocator.free(result);

    // Zlib header (2 bytes)
    // CMF: CINFO=7 (32K window), CM=8 (deflate) => 0x78
    // FLG: FLEVEL=2 (default), FDICT=0, FCHECK must make (CMF*256+FLG) % 31 == 0
    // 0x78 * 256 + FLG = 0x7800 + FLG, need FLG such that 0x7800 + FLG ≡ 0 (mod 31)
    // 0x7800 = 30720 = 991*31 + 9, so we need FLG ≡ -9 ≡ 22 (mod 31)
    // With FLEVEL=2 (bits 6-7 = 10), FLG = 0b10_0_10110 = 0x9C
    result[0] = 0x78;
    result[1] = 0x9c;

    // Copy deflate payload
    @memcpy(result[2 .. 2 + deflate_data.len], deflate_data);

    // Adler-32 checksum (big-endian)
    const adler = std.hash.Adler32.hash(data);
    std.mem.writeInt(u32, result[2 + deflate_data.len ..][0..4], adler, .big);

    return result;
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

test "gzip roundtrip compression" {
    const allocator = std.testing.allocator;
    const original = "Hello, World! This is a test of gzip compression roundtrip.";

    // Compress
    const compressed = try gzip(allocator, original);
    defer allocator.free(compressed);

    // Verify gzip header (0x1f, 0x8b)
    try std.testing.expect(compressed.len >= 18); // min: 10 header + 0 data + 8 footer
    try std.testing.expectEqual(@as(u8, 0x1f), compressed[0]);
    try std.testing.expectEqual(@as(u8, 0x8b), compressed[1]);

    // Decompress
    const decompressed = try gunzip(allocator, compressed);
    defer allocator.free(decompressed);

    // Verify roundtrip
    try std.testing.expectEqualStrings(original, decompressed);
}

test "deflate roundtrip compression" {
    const allocator = std.testing.allocator;
    const original = "Hello, World! This is a test of raw deflate compression roundtrip.";

    // Compress
    const compressed = try deflate(allocator, original);
    defer allocator.free(compressed);

    // Decompress
    const decompressed = try inflate(allocator, compressed);
    defer allocator.free(decompressed);

    // Verify roundtrip
    try std.testing.expectEqualStrings(original, decompressed);
}

test "deflateZlib roundtrip compression" {
    const allocator = std.testing.allocator;
    const original = "Hello, World! This is a test of zlib compression roundtrip.";

    // Compress
    const compressed = try deflateZlib(allocator, original);
    defer allocator.free(compressed);

    // Verify zlib header (0x78)
    try std.testing.expect(compressed.len >= 6); // min: 2 header + 0 data + 4 footer
    try std.testing.expectEqual(@as(u8, 0x78), compressed[0]);

    // Decompress
    const decompressed = try inflateZlib(allocator, compressed);
    defer allocator.free(decompressed);

    // Verify roundtrip
    try std.testing.expectEqualStrings(original, decompressed);
}

test "deflate handles empty data" {
    const allocator = std.testing.allocator;

    const compressed = try deflate(allocator, "");
    defer allocator.free(compressed);

    const decompressed = try inflate(allocator, compressed);
    defer allocator.free(decompressed);

    try std.testing.expectEqualStrings("", decompressed);
}

test "gzip handles empty data" {
    const allocator = std.testing.allocator;

    const compressed = try gzip(allocator, "");
    defer allocator.free(compressed);

    const decompressed = try gunzip(allocator, compressed);
    defer allocator.free(decompressed);

    try std.testing.expectEqualStrings("", decompressed);
}
