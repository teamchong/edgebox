/// BigInt for EdgeBox WASM
/// Provides JavaScript BigInt support using Zig's std.math.big.int.Managed
///
/// This is a simpler implementation that doesn't require metal0's full BigInt module.
/// It provides the essential operations needed for JavaScript BigInt semantics.
///
const std = @import("std");
const Managed = std.math.big.int.Managed;

/// BigInt handle for native bindings
/// Stores the managed big int and its allocator for cleanup
pub const BigIntHandle = struct {
    managed: Managed,
    allocator: std.mem.Allocator,

    const Self = @This();

    /// Create from i64 value
    pub fn fromInt(allocator: std.mem.Allocator, value: i64) !Self {
        var m = try Managed.init(allocator);
        errdefer m.deinit();
        try m.set(value);
        return Self{ .managed = m, .allocator = allocator };
    }

    /// Create from string in given base
    pub fn fromString(allocator: std.mem.Allocator, str: []const u8, base: u8) !Self {
        var m = try Managed.init(allocator);
        errdefer m.deinit();
        try m.setString(base, str);
        return Self{ .managed = m, .allocator = allocator };
    }

    /// Create from f64 (truncates towards zero like JavaScript BigInt)
    pub fn fromFloat(allocator: std.mem.Allocator, value: f64) !Self {
        var m = try Managed.init(allocator);
        errdefer m.deinit();

        // Handle special cases
        if (std.math.isNan(value) or std.math.isInf(value)) {
            return error.InvalidFloat;
        }
        if (value == 0.0) {
            try m.set(@as(i64, 0));
            return Self{ .managed = m, .allocator = allocator };
        }

        // Use std.math.trunc to get integer part
        const truncated = @trunc(value);
        // Max safe integer for f64->i64: 2^53 - 1 = 9007199254740991
        const MAX_SAFE: f64 = 9007199254740991.0;
        const MIN_SAFE: f64 = -9007199254740991.0;
        if (truncated >= MIN_SAFE and truncated <= MAX_SAFE) {
            try m.set(@as(i64, @intFromFloat(truncated)));
        } else {
            // For very large floats, convert via string
            var buf: [32]u8 = undefined;
            const str = std.fmt.bufPrint(&buf, "{d:.0}", .{truncated}) catch return error.Overflow;
            try m.setString(10, str);
        }
        return Self{ .managed = m, .allocator = allocator };
    }

    pub fn deinit(self: *Self) void {
        self.managed.deinit();
    }

    /// Convert to string in given base
    pub fn toString(self: *const Self, base: u8) ![]u8 {
        return self.managed.toString(self.allocator, base, .lower);
    }

    /// Convert to decimal string
    pub fn toDecimalString(self: *const Self) ![]u8 {
        return self.toString(10);
    }

    /// Try to convert to i64 (returns error if too large)
    pub fn toInt64(self: *const Self) !i64 {
        return self.managed.toConst().toInt(i64);
    }

    /// Convert to f64 (may lose precision)
    pub fn toFloat(self: *const Self) f64 {
        const result = self.managed.toConst().toFloat(f64, .trunc);
        return result[0];
    }

    /// Add two BigInts
    pub fn add(self: *const Self, other: *const Self) !Self {
        var result = try Managed.init(self.allocator);
        errdefer result.deinit();
        try result.add(&self.managed, &other.managed);
        return Self{ .managed = result, .allocator = self.allocator };
    }

    /// Subtract two BigInts
    pub fn sub(self: *const Self, other: *const Self) !Self {
        var result = try Managed.init(self.allocator);
        errdefer result.deinit();
        try result.sub(&self.managed, &other.managed);
        return Self{ .managed = result, .allocator = self.allocator };
    }

    /// Multiply two BigInts
    pub fn mul(self: *const Self, other: *const Self) !Self {
        var result = try Managed.init(self.allocator);
        errdefer result.deinit();
        try result.mul(&self.managed, &other.managed);
        return Self{ .managed = result, .allocator = self.allocator };
    }

    /// Floor division (JavaScript BigInt /)
    pub fn div(self: *const Self, other: *const Self) !Self {
        if (other.managed.eqlZero()) return error.DivisionByZero;
        var q = try Managed.init(self.allocator);
        errdefer q.deinit();
        var r = try Managed.init(self.allocator);
        defer r.deinit();
        try q.divTrunc(&r, &self.managed, &other.managed);
        return Self{ .managed = q, .allocator = self.allocator };
    }

    /// Modulo (JavaScript BigInt %)
    pub fn mod(self: *const Self, other: *const Self) !Self {
        if (other.managed.eqlZero()) return error.DivisionByZero;
        var q = try Managed.init(self.allocator);
        defer q.deinit();
        var r = try Managed.init(self.allocator);
        errdefer r.deinit();
        try q.divTrunc(&r, &self.managed, &other.managed);
        return Self{ .managed = r, .allocator = self.allocator };
    }

    /// Exponentiation (JavaScript BigInt **)
    pub fn pow(self: *const Self, exp: u32) !Self {
        var result = try Managed.init(self.allocator);
        errdefer result.deinit();
        try result.pow(&self.managed, exp);
        return Self{ .managed = result, .allocator = self.allocator };
    }

    /// Negate
    pub fn negate(self: *const Self) !Self {
        var result = try self.managed.cloneWithDifferentAllocator(self.allocator);
        result.negate();
        return Self{ .managed = result, .allocator = self.allocator };
    }

    /// Compare: returns -1, 0, or 1
    pub fn compare(self: *const Self, other: *const Self) i32 {
        const order = self.managed.order(other.managed);
        return switch (order) {
            .lt => -1,
            .eq => 0,
            .gt => 1,
        };
    }

    /// Check if zero
    pub fn isZero(self: *const Self) bool {
        return self.managed.eqlZero();
    }

    /// Check if negative
    pub fn isNegative(self: *const Self) bool {
        return !self.managed.isPositive() and !self.managed.eqlZero();
    }

    /// Bitwise AND
    pub fn bitAnd(self: *const Self, other: *const Self) !Self {
        var result = try Managed.init(self.allocator);
        errdefer result.deinit();
        try result.setAnd(&self.managed, &other.managed);
        return Self{ .managed = result, .allocator = self.allocator };
    }

    /// Bitwise OR
    pub fn bitOr(self: *const Self, other: *const Self) !Self {
        var result = try Managed.init(self.allocator);
        errdefer result.deinit();
        try result.setOr(&self.managed, &other.managed);
        return Self{ .managed = result, .allocator = self.allocator };
    }

    /// Bitwise XOR
    pub fn bitXor(self: *const Self, other: *const Self) !Self {
        var result = try Managed.init(self.allocator);
        errdefer result.deinit();
        try result.setXor(&self.managed, &other.managed);
        return Self{ .managed = result, .allocator = self.allocator };
    }

    /// Left shift
    pub fn shiftLeft(self: *const Self, bits: usize) !Self {
        var result = try Managed.init(self.allocator);
        errdefer result.deinit();
        try result.shiftLeft(&self.managed, bits);
        return Self{ .managed = result, .allocator = self.allocator };
    }

    /// Right shift (arithmetic)
    pub fn shiftRight(self: *const Self, bits: usize) !Self {
        var result = try Managed.init(self.allocator);
        errdefer result.deinit();
        try result.shiftRight(&self.managed, bits);
        return Self{ .managed = result, .allocator = self.allocator };
    }
};

// ============================================================================
// Tests
// ============================================================================

test "BigIntHandle fromInt and toString" {
    const allocator = std.testing.allocator;

    var a = try BigIntHandle.fromInt(allocator, 12345);
    defer a.deinit();

    const str = try a.toDecimalString();
    defer allocator.free(str);

    try std.testing.expectEqualStrings("12345", str);
}

test "BigIntHandle add" {
    const allocator = std.testing.allocator;

    var a = try BigIntHandle.fromInt(allocator, 100);
    defer a.deinit();
    var b = try BigIntHandle.fromInt(allocator, 200);
    defer b.deinit();

    var c = try a.add(&b);
    defer c.deinit();

    const result = try c.toInt64();
    try std.testing.expectEqual(@as(i64, 300), result);
}

test "BigIntHandle fromString" {
    const allocator = std.testing.allocator;

    var a = try BigIntHandle.fromString(allocator, "999999999999999999999", 10);
    defer a.deinit();

    const str = try a.toDecimalString();
    defer allocator.free(str);

    try std.testing.expectEqualStrings("999999999999999999999", str);
}
