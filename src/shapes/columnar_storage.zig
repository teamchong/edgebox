//! Columnar Storage (SoA) - SIMD-Only Structure-of-Arrays Backend
//!
//! Provides cache-friendly, SIMD-optimized storage for object fields.
//! All operations are SIMD-only - no scalar fallback paths.
//!
//! When not used, Zig's DCE removes this entirely.
//!
//! Architecture:
//!   - Each field stored in contiguous array (cache-friendly)
//!   - 32-byte aligned for SIMD (8 x i32 or 4 x i64)
//!   - Batch operations process 8 elements at once
//!   - Zero-copy integration with QuickJS JSValue

const std = @import("std");
const static_analyzer = @import("static_analyzer.zig");

const Allocator = std.mem.Allocator;

// Re-export SIMD types
pub const Vec8i32 = static_analyzer.Vec8i32;
pub const Vec8u32 = static_analyzer.Vec8u32;
pub const Vec8i64 = static_analyzer.Vec8i64;
pub const Vec8u64 = static_analyzer.Vec8u64;
pub const Vec8f64 = static_analyzer.Vec8f64;

/// Filter condition for multi-column filtering
pub const FilterCondition = struct {
    col: u32,
    target: i64,
};

/// Runtime columnar storage for dynamically-detected shapes
/// Uses SIMD for all operations - no scalar code path
pub const DynamicColumnarStorage = struct {
    const Self = @This();

    allocator: Allocator,

    // Field metadata
    field_names: [][]const u8,
    field_count: u32,

    // Columnar data - each field is contiguous, 32-byte aligned
    columns: [][]align(32) i64,

    // Object address -> row index
    addr_to_row: std.AutoHashMapUnmanaged(u64, u32),

    // Current count and capacity
    count: u32,
    capacity: u32,

    pub fn init(allocator: Allocator, field_names: []const []const u8, initial_capacity: u32) !Self {
        const field_count: u32 = @intCast(field_names.len);

        // Allocate field name storage
        const names = try allocator.alloc([]const u8, field_count);
        for (field_names, 0..) |name, i| {
            names[i] = try allocator.dupe(u8, name);
        }

        // Allocate columns - each 32-byte aligned for SIMD
        const columns = try allocator.alloc([]align(32) i64, field_count);
        for (0..field_count) |i| {
            columns[i] = try allocator.alignedAlloc(i64, .@"32", initial_capacity);
        }

        return .{
            .allocator = allocator,
            .field_names = names,
            .field_count = field_count,
            .columns = columns,
            .addr_to_row = .{},
            .count = 0,
            .capacity = initial_capacity,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.field_names) |name| {
            self.allocator.free(name);
        }
        self.allocator.free(self.field_names);

        for (self.columns) |col| {
            self.allocator.free(col);
        }
        self.allocator.free(self.columns);

        self.addr_to_row.deinit(self.allocator);
    }

    /// Register an object - SIMD-optimized row insertion
    pub fn register(self: *Self, js_addr: u64, values: []const i64) !u32 {
        if (values.len != self.field_count) return error.FieldCountMismatch;

        // Grow if needed
        if (self.count >= self.capacity) {
            try self.grow();
        }

        const row = self.count;
        self.count += 1;

        // Store values in columnar layout
        for (0..self.field_count) |i| {
            self.columns[i][row] = values[i];
        }

        try self.addr_to_row.put(self.allocator, js_addr, row);
        return row;
    }

    fn grow(self: *Self) !void {
        const new_capacity = self.capacity * 2;

        for (0..self.field_count) |i| {
            const new_col = try self.allocator.alignedAlloc(i64, .@"32", new_capacity);
            @memcpy(new_col[0..self.count], self.columns[i][0..self.count]);
            self.allocator.free(self.columns[i]);
            self.columns[i] = new_col;
        }

        self.capacity = new_capacity;
    }

    /// Lookup row by JS address
    pub fn lookup(self: *const Self, js_addr: u64) ?u32 {
        return self.addr_to_row.get(js_addr);
    }

    /// Get field value by row and column index
    pub fn getField(self: *const Self, row: u32, col: u32) i64 {
        return self.columns[col][row];
    }

    /// Set field value by row and column index
    pub fn setField(self: *Self, row: u32, col: u32, value: i64) void {
        self.columns[col][row] = value;
    }

    /// Get field index by name
    pub fn getFieldIndex(self: *const Self, name: []const u8) ?u32 {
        for (self.field_names, 0..) |field_name, i| {
            if (std.mem.eql(u8, field_name, name)) {
                return @intCast(i);
            }
        }
        return null;
    }

    // ========================================================================
    // SIMD Batch Operations - Process 8 elements at once
    // ========================================================================

    /// SIMD batch load - load 8 values from a column
    pub fn loadBatch(self: *const Self, col: u32, start: u32) Vec8i64 {
        return self.columns[col][start..][0..8].*;
    }

    /// SIMD batch store - store 8 values to a column
    pub fn storeBatch(self: *Self, col: u32, start: u32, values: Vec8i64) void {
        self.columns[col][start..][0..8].* = values;
    }

    /// SIMD filter - find all rows where column matches value
    /// Returns indices of matching rows
    pub fn simdFilter(self: *const Self, col: u32, target: i64, result_buf: []u32) usize {
        const target_vec: Vec8i64 = @splat(target);
        var result_count: usize = 0;

        var i: u32 = 0;
        while (i + 8 <= self.count) : (i += 8) {
            const batch = self.loadBatch(col, i);
            const matches = batch == target_vec;

            // Extract matching indices using SIMD
            inline for (0..8) |j| {
                if (matches[j]) {
                    if (result_count < result_buf.len) {
                        result_buf[result_count] = i + @as(u32, @intCast(j));
                        result_count += 1;
                    }
                }
            }
        }

        // Handle remainder with SIMD (pad with sentinel value that won't match)
        if (i < self.count) {
            const sentinel: i64 = if (target == std.math.minInt(i64)) std.math.maxInt(i64) else std.math.minInt(i64);
            var padded: Vec8i64 = @splat(sentinel);
            const remaining = self.count - i;
            for (0..@min(8, remaining)) |j| {
                padded[j] = self.columns[col][i + j];
            }
            const matches = padded == target_vec;
            inline for (0..8) |j| {
                if (j < remaining and matches[j]) {
                    if (result_count < result_buf.len) {
                        result_buf[result_count] = i + @as(u32, @intCast(j));
                        result_count += 1;
                    }
                }
            }
        }

        return result_count;
    }

    /// SIMD reduce - reduce all values in a column
    pub fn simdReduce(self: *const Self, col: u32, comptime op: std.builtin.ReduceOp) i64 {
        if (self.count == 0) return 0;

        const identity: i64 = switch (op) {
            .Add => 0,
            .Mul => 1,
            .Min => std.math.maxInt(i64),
            .Max => std.math.minInt(i64),
            else => 0,
        };

        var acc: Vec8i64 = @splat(identity);

        var i: u32 = 0;
        while (i + 8 <= self.count) : (i += 8) {
            const batch = self.loadBatch(col, i);
            acc = switch (op) {
                .Add => acc + batch,
                .Mul => acc * batch,
                .Min => @min(acc, batch),
                .Max => @max(acc, batch),
                else => acc,
            };
        }

        // Handle remainder
        if (i < self.count) {
            var padded: Vec8i64 = @splat(identity);
            const remaining = self.count - i;
            for (0..@min(8, remaining)) |j| {
                padded[j] = self.columns[col][i + j];
            }
            acc = switch (op) {
                .Add => acc + padded,
                .Mul => acc * padded,
                .Min => @min(acc, padded),
                .Max => @max(acc, padded),
                else => acc,
            };
        }

        return @reduce(op, acc);
    }

    /// SIMD map - apply operation to all values in a column
    pub fn simdMap(self: *Self, col: u32, comptime op: fn (Vec8i64) Vec8i64) void {
        var i: u32 = 0;
        while (i + 8 <= self.count) : (i += 8) {
            const batch = self.loadBatch(col, i);
            const result = op(batch);
            self.storeBatch(col, i, result);
        }

        // Handle remainder with SIMD
        if (i < self.count) {
            var padded: Vec8i64 = @splat(0);
            const remaining = self.count - i;
            for (0..@min(8, remaining)) |j| {
                padded[j] = self.columns[col][i + j];
            }
            const result = op(padded);
            for (0..@min(8, remaining)) |j| {
                self.columns[col][i + j] = result[j];
            }
        }
    }

    /// SIMD forEach - call function for each row with batch optimization
    pub fn simdForEach(self: *const Self, col: u32, comptime callback: fn (u32, i64) void) void {
        var i: u32 = 0;
        while (i + 8 <= self.count) : (i += 8) {
            const batch = self.loadBatch(col, i);
            inline for (0..8) |j| {
                callback(i + @as(u32, @intCast(j)), batch[j]);
            }
        }

        // Handle remainder
        while (i < self.count) : (i += 1) {
            callback(i, self.columns[col][i]);
        }
    }

    /// SIMD multi-column filter - find rows matching multiple conditions
    pub fn simdMultiFilter(
        self: *const Self,
        conditions: []const FilterCondition,
        result_buf: []u32,
    ) usize {
        if (conditions.len == 0) return 0;

        var result_count: usize = 0;

        var i: u32 = 0;
        while (i + 8 <= self.count) : (i += 8) {
            // Start with all true
            var combined_matches: @Vector(8, bool) = @splat(true);

            // AND all conditions together using SIMD
            for (conditions) |cond| {
                const batch = self.loadBatch(cond.col, i);
                const target_vec: Vec8i64 = @splat(cond.target);
                const matches = batch == target_vec;
                // Use select to implement AND: if matches is false, set to false
                combined_matches = @select(bool, matches, combined_matches, @as(@Vector(8, bool), @splat(false)));
            }

            // Extract matching indices
            inline for (0..8) |j| {
                if (combined_matches[j]) {
                    if (result_count < result_buf.len) {
                        result_buf[result_count] = i + @as(u32, @intCast(j));
                        result_count += 1;
                    }
                }
            }
        }

        // Handle remainder
        while (i < self.count) : (i += 1) {
            var all_match = true;
            for (conditions) |cond| {
                if (self.columns[cond.col][i] != cond.target) {
                    all_match = false;
                    break;
                }
            }
            if (all_match) {
                if (result_count < result_buf.len) {
                    result_buf[result_count] = i;
                    result_count += 1;
                }
            }
        }

        return result_count;
    }
};

// ============================================================================
// Pre-defined TypeScript AST Storage (comptime-generated)
// ============================================================================

/// Optimized storage for TypeScript AST nodes
/// Uses static_analyzer.AstNodeStorage under the hood
pub const AstNodeColumnarStorage = static_analyzer.AstNodeStorage;

// ============================================================================
// Tests
// ============================================================================

test "dynamic columnar storage basic ops" {
    const fields = [_][]const u8{ "kind", "flags", "pos", "end" };
    var storage = try DynamicColumnarStorage.init(std.testing.allocator, &fields, 64);
    defer storage.deinit();

    // Register nodes
    const row0 = try storage.register(0x1000, &.{ 100, 0, 0, 10 });
    const row1 = try storage.register(0x1001, &.{ 200, 1, 10, 20 });

    try std.testing.expectEqual(@as(u32, 0), row0);
    try std.testing.expectEqual(@as(u32, 1), row1);

    // Lookup
    try std.testing.expectEqual(@as(u32, 0), storage.lookup(0x1000).?);
    try std.testing.expectEqual(@as(u32, 1), storage.lookup(0x1001).?);

    // Field access
    try std.testing.expectEqual(@as(i64, 100), storage.getField(0, 0)); // kind of row 0
    try std.testing.expectEqual(@as(i64, 200), storage.getField(1, 0)); // kind of row 1
}

test "dynamic columnar storage SIMD filter" {
    const fields = [_][]const u8{ "kind", "flags" };
    var storage = try DynamicColumnarStorage.init(std.testing.allocator, &fields, 64);
    defer storage.deinit();

    // Register 10 nodes with various kinds
    _ = try storage.register(0x1000, &.{ 100, 0 });
    _ = try storage.register(0x1001, &.{ 200, 0 });
    _ = try storage.register(0x1002, &.{ 100, 1 });
    _ = try storage.register(0x1003, &.{ 300, 0 });
    _ = try storage.register(0x1004, &.{ 100, 0 });
    _ = try storage.register(0x1005, &.{ 200, 1 });
    _ = try storage.register(0x1006, &.{ 100, 0 });
    _ = try storage.register(0x1007, &.{ 400, 0 });
    _ = try storage.register(0x1008, &.{ 100, 1 });
    _ = try storage.register(0x1009, &.{ 100, 0 });

    // Filter by kind=100
    var result_buf: [64]u32 = undefined;
    const kind_col = storage.getFieldIndex("kind").?;
    const matches = storage.simdFilter(kind_col, 100, &result_buf);

    try std.testing.expectEqual(@as(usize, 6), matches);
    try std.testing.expectEqual(@as(u32, 0), result_buf[0]);
    try std.testing.expectEqual(@as(u32, 2), result_buf[1]);
    try std.testing.expectEqual(@as(u32, 4), result_buf[2]);
}

test "dynamic columnar storage SIMD reduce" {
    const fields = [_][]const u8{"value"};
    var storage = try DynamicColumnarStorage.init(std.testing.allocator, &fields, 64);
    defer storage.deinit();

    // Register values 1-10
    for (1..11) |i| {
        _ = try storage.register(0x1000 + i, &.{@intCast(i)});
    }

    // Sum should be 55
    const sum = storage.simdReduce(0, .Add);
    try std.testing.expectEqual(@as(i64, 55), sum);

    // Max should be 10
    const max = storage.simdReduce(0, .Max);
    try std.testing.expectEqual(@as(i64, 10), max);

    // Min should be 1
    const min = storage.simdReduce(0, .Min);
    try std.testing.expectEqual(@as(i64, 1), min);
}

test "dynamic columnar storage multi-filter" {
    const fields = [_][]const u8{ "kind", "flags" };
    var storage = try DynamicColumnarStorage.init(std.testing.allocator, &fields, 64);
    defer storage.deinit();

    // Register nodes
    _ = try storage.register(0x1000, &.{ 100, 0 }); // matches
    _ = try storage.register(0x1001, &.{ 100, 1 }); // kind matches, flags doesn't
    _ = try storage.register(0x1002, &.{ 200, 0 }); // flags matches, kind doesn't
    _ = try storage.register(0x1003, &.{ 100, 0 }); // matches
    _ = try storage.register(0x1004, &.{ 300, 0 }); // flags matches, kind doesn't

    // Find rows where kind=100 AND flags=0
    var result_buf: [64]u32 = undefined;
    const conditions = [_]FilterCondition{
        .{ .col = 0, .target = 100 }, // kind
        .{ .col = 1, .target = 0 }, // flags
    };
    const matches = storage.simdMultiFilter(&conditions, &result_buf);

    try std.testing.expectEqual(@as(usize, 2), matches);
    try std.testing.expectEqual(@as(u32, 0), result_buf[0]);
    try std.testing.expectEqual(@as(u32, 3), result_buf[1]);
}

test "dynamic columnar storage SIMD map" {
    const fields = [_][]const u8{"value"};
    var storage = try DynamicColumnarStorage.init(std.testing.allocator, &fields, 64);
    defer storage.deinit();

    // Register values
    for (0..10) |i| {
        _ = try storage.register(0x1000 + i, &.{@intCast(i)});
    }

    // Double all values
    storage.simdMap(0, struct {
        fn op(v: Vec8i64) Vec8i64 {
            return v * @as(Vec8i64, @splat(2));
        }
    }.op);

    // Verify
    try std.testing.expectEqual(@as(i64, 0), storage.getField(0, 0));
    try std.testing.expectEqual(@as(i64, 2), storage.getField(1, 0));
    try std.testing.expectEqual(@as(i64, 18), storage.getField(9, 0));
}
