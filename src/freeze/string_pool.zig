//! String Intern Pool - Cache frequently accessed strings
//!
//! Uses FNV-1a hash for O(1) lookup. Useful for property names, identifiers.

const std = @import("std");

/// String intern pool for caching frequently accessed strings.
pub const StringPool = struct {
    const POOL_SIZE = 256; // Must be power of 2
    const Entry = struct {
        hash: u32,
        str: []const u8,
        valid: bool,
    };

    entries: [POOL_SIZE]Entry,
    data: std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) StringPool {
        return StringPool{
            .entries = [_]Entry{.{ .hash = 0, .str = &.{}, .valid = false }} ** POOL_SIZE,
            .data = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *StringPool) void {
        self.data.deinit(self.allocator);
    }

    /// FNV-1a hash
    fn hash(str: []const u8) u32 {
        var h: u32 = 2166136261;
        for (str) |c| {
            h ^= c;
            h *%= 16777619;
        }
        return h;
    }

    /// Intern a string - returns pointer to pooled copy
    pub fn intern(self: *StringPool, str: []const u8) ![]const u8 {
        const h = hash(str);
        const idx = h & (POOL_SIZE - 1);

        // Check if already interned
        if (self.entries[idx].valid and self.entries[idx].hash == h) {
            if (std.mem.eql(u8, self.entries[idx].str, str)) {
                return self.entries[idx].str;
            }
        }

        // Add to pool
        const start = self.data.items.len;
        try self.data.appendSlice(self.allocator, str);
        const interned = self.data.items[start..];

        self.entries[idx] = .{
            .hash = h,
            .str = interned,
            .valid = true,
        };

        return interned;
    }

    /// Check if string is in pool without adding
    pub fn lookup(self: *const StringPool, str: []const u8) ?[]const u8 {
        const h = hash(str);
        const idx = h & (POOL_SIZE - 1);

        if (self.entries[idx].valid and self.entries[idx].hash == h) {
            if (std.mem.eql(u8, self.entries[idx].str, str)) {
                return self.entries[idx].str;
            }
        }
        return null;
    }
};

/// Global string pool for runtime use (initialized on first use)
var global_string_pool: ?StringPool = null;
var global_pool_allocator: ?std.mem.Allocator = null;

pub fn getGlobalStringPool(allocator: std.mem.Allocator) *StringPool {
    if (global_string_pool == null) {
        global_string_pool = StringPool.init(allocator);
        global_pool_allocator = allocator;
    }
    return &global_string_pool.?;
}

pub fn deinitGlobalStringPool() void {
    if (global_string_pool) |*pool| {
        pool.deinit();
        global_string_pool = null;
    }
}

// ============================================================================
// Tests
// ============================================================================

test "StringPool intern" {
    var pool = StringPool.init(std.testing.allocator);
    defer pool.deinit();

    const s1 = try pool.intern("hello");
    const s2 = try pool.intern("hello");
    const s3 = try pool.intern("world");

    // Same string returns same pointer
    try std.testing.expectEqual(s1.ptr, s2.ptr);
    // Different strings are different
    try std.testing.expect(s1.ptr != s3.ptr);
    // Content is correct
    try std.testing.expectEqualStrings("hello", s1);
    try std.testing.expectEqualStrings("world", s3);
}
