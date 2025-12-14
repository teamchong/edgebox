/// Crypto Implementation Unit Tests

const std = @import("std");
const crypto_impl = @import("crypto_impl.zig");
const native_registry = @import("../native_registry.zig");
const Value = native_registry.Value;
const NativeRegistry = native_registry.NativeRegistry;

test "Crypto - hash sha256" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    crypto_impl.init(std.testing.allocator);
    defer crypto_impl.deinit();

    try crypto_impl.registerCryptoImpl(&registry);

    const result = try registry.call("crypto", "hash", &[_]Value{
        Value{ .u32 = 0 }, // sha256
        Value{ .string = "hello" },
    });

    try std.testing.expect(result.isOk());
    const hash = try result.asOkString();
    defer std.testing.allocator.free(hash);

    // Known sha256("hello") = 2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824
    try std.testing.expectEqualStrings(
        "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824",
        hash,
    );
}

test "Crypto - hash all algorithms" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    crypto_impl.init(std.testing.allocator);
    defer crypto_impl.deinit();

    try crypto_impl.registerCryptoImpl(&registry);

    // Test SHA-256
    {
        const result = try registry.call("crypto", "hash", &[_]Value{
            Value{ .u32 = 0 }, // sha256
            Value{ .string = "test" },
        });
        try std.testing.expect(result.isOk());
        const hash = try result.asOkString();
        defer std.testing.allocator.free(hash);
        try std.testing.expectEqual(@as(usize, 64), hash.len); // 32 bytes * 2 hex chars
    }

    // Test SHA-384
    {
        const result = try registry.call("crypto", "hash", &[_]Value{
            Value{ .u32 = 1 }, // sha384
            Value{ .string = "test" },
        });
        try std.testing.expect(result.isOk());
        const hash = try result.asOkString();
        defer std.testing.allocator.free(hash);
        try std.testing.expectEqual(@as(usize, 96), hash.len); // 48 bytes * 2 hex chars
    }

    // Test SHA-512
    {
        const result = try registry.call("crypto", "hash", &[_]Value{
            Value{ .u32 = 2 }, // sha512
            Value{ .string = "test" },
        });
        try std.testing.expect(result.isOk());
        const hash = try result.asOkString();
        defer std.testing.allocator.free(hash);
        try std.testing.expectEqual(@as(usize, 128), hash.len); // 64 bytes * 2 hex chars
    }

    // Test SHA-1
    {
        const result = try registry.call("crypto", "hash", &[_]Value{
            Value{ .u32 = 3 }, // sha1
            Value{ .string = "test" },
        });
        try std.testing.expect(result.isOk());
        const hash = try result.asOkString();
        defer std.testing.allocator.free(hash);
        try std.testing.expectEqual(@as(usize, 40), hash.len); // 20 bytes * 2 hex chars
    }

    // Test MD5
    {
        const result = try registry.call("crypto", "hash", &[_]Value{
            Value{ .u32 = 4 }, // md5
            Value{ .string = "test" },
        });
        try std.testing.expect(result.isOk());
        const hash = try result.asOkString();
        defer std.testing.allocator.free(hash);
        try std.testing.expectEqual(@as(usize, 32), hash.len); // 16 bytes * 2 hex chars
    }
}

test "Crypto - hash invalid algorithm" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    crypto_impl.init(std.testing.allocator);
    defer crypto_impl.deinit();

    try crypto_impl.registerCryptoImpl(&registry);

    const result = try registry.call("crypto", "hash", &[_]Value{
        Value{ .u32 = 999 }, // invalid algorithm
        Value{ .string = "test" },
    });

    try std.testing.expect(result.isErr());
    const err_code = try result.asErr();
    try std.testing.expectEqual(@as(u32, 0), err_code); // invalid-algorithm
}

test "Crypto - hmac sha256" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    crypto_impl.init(std.testing.allocator);
    defer crypto_impl.deinit();

    try crypto_impl.registerCryptoImpl(&registry);

    const result = try registry.call("crypto", "hmac", &[_]Value{
        Value{ .u32 = 0 }, // sha256
        Value{ .string = "secret" },
        Value{ .string = "message" },
    });

    try std.testing.expect(result.isOk());
    const hmac = try result.asOkString();
    defer std.testing.allocator.free(hmac);

    // Should be 64 hex chars (32 bytes)
    try std.testing.expectEqual(@as(usize, 64), hmac.len);
}

test "Crypto - hmac all supported algorithms" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    crypto_impl.init(std.testing.allocator);
    defer crypto_impl.deinit();

    try crypto_impl.registerCryptoImpl(&registry);

    // Test SHA-256 HMAC
    {
        const result = try registry.call("crypto", "hmac", &[_]Value{
            Value{ .u32 = 0 },
            Value{ .string = "key" },
            Value{ .string = "data" },
        });
        try std.testing.expect(result.isOk());
        const hmac = try result.asOkString();
        defer std.testing.allocator.free(hmac);
        try std.testing.expectEqual(@as(usize, 64), hmac.len);
    }

    // Test SHA-384 HMAC
    {
        const result = try registry.call("crypto", "hmac", &[_]Value{
            Value{ .u32 = 1 },
            Value{ .string = "key" },
            Value{ .string = "data" },
        });
        try std.testing.expect(result.isOk());
        const hmac = try result.asOkString();
        defer std.testing.allocator.free(hmac);
        try std.testing.expectEqual(@as(usize, 96), hmac.len);
    }

    // Test SHA-512 HMAC
    {
        const result = try registry.call("crypto", "hmac", &[_]Value{
            Value{ .u32 = 2 },
            Value{ .string = "key" },
            Value{ .string = "data" },
        });
        try std.testing.expect(result.isOk());
        const hmac = try result.asOkString();
        defer std.testing.allocator.free(hmac);
        try std.testing.expectEqual(@as(usize, 128), hmac.len);
    }
}

test "Crypto - hmac invalid algorithm (md5/sha1)" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    crypto_impl.init(std.testing.allocator);
    defer crypto_impl.deinit();

    try crypto_impl.registerCryptoImpl(&registry);

    // MD5 should be rejected
    {
        const result = try registry.call("crypto", "hmac", &[_]Value{
            Value{ .u32 = 4 }, // md5
            Value{ .string = "key" },
            Value{ .string = "data" },
        });
        try std.testing.expect(result.isErr());
        const err_code = try result.asErr();
        try std.testing.expectEqual(@as(u32, 0), err_code); // invalid-algorithm
    }

    // SHA-1 should be rejected
    {
        const result = try registry.call("crypto", "hmac", &[_]Value{
            Value{ .u32 = 3 }, // sha1
            Value{ .string = "key" },
            Value{ .string = "data" },
        });
        try std.testing.expect(result.isErr());
    }
}

test "Crypto - random-bytes" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    crypto_impl.init(std.testing.allocator);
    defer crypto_impl.deinit();

    try crypto_impl.registerCryptoImpl(&registry);

    const result = try registry.call("crypto", "random-bytes", &[_]Value{
        Value{ .u32 = 32 },
    });

    try std.testing.expect(result.isOk());
    const bytes = try result.asOkListU8();
    defer std.testing.allocator.free(bytes);

    // Check length
    try std.testing.expectEqual(@as(usize, 32), bytes.len);

    // Check that bytes are not all zeros (extremely unlikely with crypto.random)
    var all_zeros = true;
    for (bytes) |b| {
        if (b != 0) {
            all_zeros = false;
            break;
        }
    }
    try std.testing.expect(!all_zeros);
}

test "Crypto - random-bytes large size limit" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    crypto_impl.init(std.testing.allocator);
    defer crypto_impl.deinit();

    try crypto_impl.registerCryptoImpl(&registry);

    // Try to request more than 100MB
    const result = try registry.call("crypto", "random-bytes", &[_]Value{
        Value{ .u32 = 101 * 1024 * 1024 },
    });

    try std.testing.expect(result.isErr());
    const err_code = try result.asErr();
    try std.testing.expectEqual(@as(u32, 1), err_code); // invalid-input
}

test "Crypto - get-hash-algorithms" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    crypto_impl.init(std.testing.allocator);
    defer crypto_impl.deinit();

    try crypto_impl.registerCryptoImpl(&registry);

    const result = try registry.call("crypto", "get-hash-algorithms", &[_]Value{});

    const algos = try result.asListString();
    defer {
        for (algos) |algo| {
            std.testing.allocator.free(algo);
        }
        std.testing.allocator.free(algos);
    }

    // Should have 5 algorithms
    try std.testing.expectEqual(@as(usize, 5), algos.len);

    // Check that all expected algorithms are present
    var has_sha256 = false;
    var has_sha384 = false;
    var has_sha512 = false;
    var has_sha1 = false;
    var has_md5 = false;

    for (algos) |algo| {
        if (std.mem.eql(u8, algo, "sha256")) has_sha256 = true;
        if (std.mem.eql(u8, algo, "sha384")) has_sha384 = true;
        if (std.mem.eql(u8, algo, "sha512")) has_sha512 = true;
        if (std.mem.eql(u8, algo, "sha1")) has_sha1 = true;
        if (std.mem.eql(u8, algo, "md5")) has_md5 = true;
    }

    try std.testing.expect(has_sha256);
    try std.testing.expect(has_sha384);
    try std.testing.expect(has_sha512);
    try std.testing.expect(has_sha1);
    try std.testing.expect(has_md5);
}
