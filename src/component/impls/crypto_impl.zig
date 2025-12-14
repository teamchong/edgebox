/// Crypto Implementation for Component Model
/// Wraps std.crypto operations as Component Model functions

const std = @import("std");
const crypto = std.crypto;
const native_registry = @import("../native_registry.zig");
const Value = native_registry.Value;
const NativeRegistry = native_registry.NativeRegistry;

/// Crypto error enum (matches WIT crypto-error)
pub const CryptoError = enum(u32) {
    invalid_algorithm = 0,
    invalid_input = 1,
    invalid_key = 2,
    operation_failed = 3,
};

/// Hash algorithm enum (matches WIT hash-algorithm)
pub const HashAlgorithm = enum(u32) {
    sha256 = 0,
    sha384 = 1,
    sha512 = 2,
    sha1 = 3,
    md5 = 4,
};

/// Global allocator for crypto operations
var crypto_allocator: ?std.mem.Allocator = null;

/// Initialize crypto implementation
pub fn init(allocator: std.mem.Allocator) void {
    crypto_allocator = allocator;
}

/// Clean up crypto implementation
pub fn deinit() void {
    crypto_allocator = null;
}

/// Register crypto implementations with native registry
pub fn registerCryptoImpl(registry: *NativeRegistry) !void {
    try registry.register("crypto", "hash", hashImpl);
    try registry.register("crypto", "hmac", hmacImpl);
    try registry.register("crypto", "random-bytes", randomBytesImpl);
    try registry.register("crypto", "get-hash-algorithms", getHashAlgorithmsImpl);
}

/// Helper to convert bytes to hex string
fn hexEncode(allocator: std.mem.Allocator, bytes: []const u8) ![]const u8 {
    const hex_chars = "0123456789abcdef";
    const hex = try allocator.alloc(u8, bytes.len * 2);

    for (bytes, 0..) |b, i| {
        hex[i * 2] = hex_chars[b >> 4];
        hex[i * 2 + 1] = hex_chars[b & 0x0F];
    }

    return hex;
}

/// hash: func(algorithm: hash-algorithm, data: string) -> result<string, crypto-error>
fn hashImpl(args: []const Value) !Value {
    const allocator = crypto_allocator orelse return error.NotInitialized;
    const algo_u32 = try args[0].asU32();
    const data = try args[1].asString();

    // Map enum to algorithm
    const algorithm = std.meta.intToEnum(HashAlgorithm, algo_u32) catch {
        return Value{ .err = @intFromEnum(CryptoError.invalid_algorithm) };
    };

    // Compute hash based on algorithm
    const hash_bytes = switch (algorithm) {
        .sha256 => blk: {
            var hash: [32]u8 = undefined;
            crypto.hash.sha2.Sha256.hash(data, &hash, .{});
            break :blk try allocator.dupe(u8, &hash);
        },
        .sha384 => blk: {
            var hash: [48]u8 = undefined;
            crypto.hash.sha2.Sha384.hash(data, &hash, .{});
            break :blk try allocator.dupe(u8, &hash);
        },
        .sha512 => blk: {
            var hash: [64]u8 = undefined;
            crypto.hash.sha2.Sha512.hash(data, &hash, .{});
            break :blk try allocator.dupe(u8, &hash);
        },
        .sha1 => blk: {
            var hash: [20]u8 = undefined;
            crypto.hash.Sha1.hash(data, &hash, .{});
            break :blk try allocator.dupe(u8, &hash);
        },
        .md5 => blk: {
            var hash: [16]u8 = undefined;
            crypto.hash.Md5.hash(data, &hash, .{});
            break :blk try allocator.dupe(u8, &hash);
        },
    };

    defer allocator.free(hash_bytes);

    // Convert to hex string
    const hex_string = try hexEncode(allocator, hash_bytes);
    return Value{ .ok_string = hex_string };
}

/// hmac: func(algorithm: hash-algorithm, key: string, data: string) -> result<string, crypto-error>
fn hmacImpl(args: []const Value) !Value {
    const allocator = crypto_allocator orelse return error.NotInitialized;
    const algo_u32 = try args[0].asU32();
    const key = try args[1].asString();
    const data = try args[2].asString();

    // Map enum to algorithm
    const algorithm = std.meta.intToEnum(HashAlgorithm, algo_u32) catch {
        return Value{ .err = @intFromEnum(CryptoError.invalid_algorithm) };
    };

    // HMAC only supports SHA2 variants
    if (algorithm == .sha1 or algorithm == .md5) {
        return Value{ .err = @intFromEnum(CryptoError.invalid_algorithm) };
    }

    // Compute HMAC based on algorithm
    const hmac_bytes = switch (algorithm) {
        .sha256 => blk: {
            var out: [32]u8 = undefined;
            // Pad or truncate key to match algorithm output size
            var key_buf: [32]u8 = undefined;
            @memset(&key_buf, 0);
            @memcpy(key_buf[0..@min(key.len, 32)], key[0..@min(key.len, 32)]);
            crypto.auth.hmac.sha2.HmacSha256.create(&out, data, &key_buf);
            break :blk try allocator.dupe(u8, &out);
        },
        .sha384 => blk: {
            var out: [48]u8 = undefined;
            var key_buf: [48]u8 = undefined;
            @memset(&key_buf, 0);
            @memcpy(key_buf[0..@min(key.len, 48)], key[0..@min(key.len, 48)]);
            crypto.auth.hmac.sha2.HmacSha384.create(&out, data, &key_buf);
            break :blk try allocator.dupe(u8, &out);
        },
        .sha512 => blk: {
            var out: [64]u8 = undefined;
            var key_buf: [64]u8 = undefined;
            @memset(&key_buf, 0);
            @memcpy(key_buf[0..@min(key.len, 64)], key[0..@min(key.len, 64)]);
            crypto.auth.hmac.sha2.HmacSha512.create(&out, data, &key_buf);
            break :blk try allocator.dupe(u8, &out);
        },
        .sha1, .md5 => unreachable, // Already checked above
    };

    defer allocator.free(hmac_bytes);

    // Convert to hex string
    const hex_string = try hexEncode(allocator, hmac_bytes);
    return Value{ .ok_string = hex_string };
}

/// random-bytes: func(size: u32) -> result<list<u8>, crypto-error>
fn randomBytesImpl(args: []const Value) !Value {
    const allocator = crypto_allocator orelse return error.NotInitialized;
    const size = try args[0].asU32();

    // Limit to 100MB (safety check)
    if (size > 100 * 1024 * 1024) {
        return Value{ .err = @intFromEnum(CryptoError.invalid_input) };
    }

    // Generate random bytes
    const bytes = try allocator.alloc(u8, size);
    crypto.random.bytes(bytes);

    // Return result<list<u8>, crypto-error>
    return Value{ .ok_list_u8 = bytes };
}

/// get-hash-algorithms: func() -> list<string>
fn getHashAlgorithmsImpl(_: []const Value) !Value {
    const allocator = crypto_allocator orelse return error.NotInitialized;

    // Return static list of algorithm names
    const algorithms = [_][]const u8{
        "sha256",
        "sha384",
        "sha512",
        "sha1",
        "md5",
    };

    // Allocate and copy algorithm names
    const result = try allocator.alloc([]const u8, algorithms.len);
    for (algorithms, 0..) |algo, i| {
        result[i] = try allocator.dupe(u8, algo);
    }

    return Value{ .list_string = result };
}
