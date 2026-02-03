/// Optimized HashMap utilities for EdgeBox
/// ALWAYS use these instead of std.StringHashMap for performance!
///
/// Performance gains:
/// - wyhash: faster than default hash (same as Bun)
/// - ArrayHashMap: O(n) iteration, better cache locality
/// Used by: PyDict, tokenizer vocab lookups, any string-keyed hashmaps
const std = @import("std");
const wyhash = @import("wyhash.zig");

/// Fast string hash context using wyhash (same as Bun)
/// Returns u32 for ArrayHashMap compatibility
pub const WyhashStringContext = struct {
    pub fn hash(_: @This(), key: []const u8) u32 {
        return @truncate(wyhash.WyhashStateless.hash(0, key));
    }

    pub fn eql(_: @This(), a: []const u8, b: []const u8, b_index: usize) bool {
        _ = b_index;
        return std.mem.eql(u8, a, b);
    }
};

/// Fast string-keyed ArrayHashMap with wyhash
/// Use this instead of std.StringHashMap!
///
/// API changes from HashMap:
/// - .keyIterator() → .keys() (returns slice)
/// - .valueIterator() → .values() (returns slice)
/// - .iterator() → iterate over .keys() and .values() together
///
/// Example:
///   var map = hashmap_helper.StringHashMap(ValueType).init(allocator);
///   for (map.keys()) |key| { ... }
pub fn StringHashMap(comptime V: type) type {
    return std.ArrayHashMap([]const u8, V, WyhashStringContext, true);
}

/// Fast string hash set with wyhash (stores keys only, no values)
/// Use this for tracking sets of string names.
pub const StringHashSet = std.ArrayHashMap([]const u8, void, WyhashStringContext, true);

/// Fast pointer hash context using wyhash on pointer address
/// Used for bytecode pointer → frozen function lookups
pub fn WyhashPointerContext(comptime T: type) type {
    return struct {
        pub fn hash(_: @This(), key: T) u32 {
            const addr = @intFromPtr(key);
            const bytes = std.mem.asBytes(&addr);
            return @truncate(wyhash.WyhashStateless.hash(0, bytes));
        }

        pub fn eql(_: @This(), a: T, b: T, b_index: usize) bool {
            _ = b_index;
            return a == b;
        }
    };
}

/// Fast pointer-keyed ArrayHashMap with wyhash
/// Use this for bytecode pointer → function pointer lookups
pub fn PointerHashMap(comptime K: type, comptime V: type) type {
    return std.ArrayHashMap(K, V, WyhashPointerContext(K), true);
}
