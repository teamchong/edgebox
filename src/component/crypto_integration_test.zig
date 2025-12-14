/// Crypto Component Integration Test
/// Tests crypto through the native registry
///
/// Architecture Note (Phase 5):
/// The crypto interface uses package-level enums (hash-algorithm, crypto-error)
/// which are compatible with ComponentAdapter's resolveType(). This test uses
/// NativeRegistry directly to verify the implementation, similar to filesystem tests.

const std = @import("std");
const NativeRegistry = @import("native_registry.zig").NativeRegistry;
const crypto = @import("impls/crypto_impl.zig");
const Value = @import("native_registry.zig").Value;

test "Crypto Component - integration test" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    crypto.init(std.testing.allocator);
    defer crypto.deinit();

    try crypto.registerCryptoImpl(&registry);

    // Test hash
    const hash_result = try registry.call("crypto", "hash", &[_]Value{
        Value{ .u32 = 0 }, // sha256
        Value{ .string = "test" },
    });
    try std.testing.expect(hash_result.isOk());
    const hash_str = try hash_result.asOkString();
    defer std.testing.allocator.free(hash_str);
    try std.testing.expectEqual(@as(usize, 64), hash_str.len);

    // Test HMAC
    const hmac_result = try registry.call("crypto", "hmac", &[_]Value{
        Value{ .u32 = 0 }, // sha256
        Value{ .string = "secret" },
        Value{ .string = "message" },
    });
    try std.testing.expect(hmac_result.isOk());
    const hmac_str = try hmac_result.asOkString();
    defer std.testing.allocator.free(hmac_str);
    try std.testing.expectEqual(@as(usize, 64), hmac_str.len);

    // Test random bytes
    const random_result = try registry.call("crypto", "random-bytes", &[_]Value{
        Value{ .u32 = 32 },
    });
    try std.testing.expect(random_result.isOk());
    const bytes = try random_result.asOkListU8();
    defer std.testing.allocator.free(bytes);
    try std.testing.expectEqual(@as(usize, 32), bytes.len);

    // Test get-hash-algorithms
    const algos_result = try registry.call("crypto", "get-hash-algorithms", &[_]Value{});
    const algos = try algos_result.asListString();
    defer {
        for (algos) |algo| {
            std.testing.allocator.free(algo);
        }
        std.testing.allocator.free(algos);
    }
    try std.testing.expectEqual(@as(usize, 5), algos.len);
}

test "Crypto Component - error handling" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    crypto.init(std.testing.allocator);
    defer crypto.deinit();

    try crypto.registerCryptoImpl(&registry);

    // Invalid hash algorithm
    const invalid_hash = try registry.call("crypto", "hash", &[_]Value{
        Value{ .u32 = 999 },
        Value{ .string = "test" },
    });
    try std.testing.expect(invalid_hash.isErr());

    // Invalid HMAC algorithm (md5 not supported for HMAC)
    const invalid_hmac = try registry.call("crypto", "hmac", &[_]Value{
        Value{ .u32 = 4 }, // md5
        Value{ .string = "key" },
        Value{ .string = "data" },
    });
    try std.testing.expect(invalid_hmac.isErr());

    // Random bytes size too large
    const too_large = try registry.call("crypto", "random-bytes", &[_]Value{
        Value{ .u32 = 101 * 1024 * 1024 },
    });
    try std.testing.expect(too_large.isErr());
}
