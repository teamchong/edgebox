//! Static Shape Analyzer - Comptime SIMD-Only Shape Detection
//!
//! All analysis happens at compile time. When shapes are not used,
//! Zig's dead code elimination removes the entire analyzer.
//!
//! Architecture:
//!   1. Comptime bytecode analysis for property patterns
//!   2. Comptime type generation for detected shapes
//!   3. SIMD-only accessors (no scalar fallback)
//!   4. Zero runtime cost when unused (DCE removes all)

const std = @import("std");

// SIMD vector types - 256-bit (8 x i32) for maximum throughput
pub const Vec8i32 = @Vector(8, i32);
pub const Vec8u32 = @Vector(8, u32);
pub const Vec8i64 = @Vector(8, i64);
pub const Vec8u64 = @Vector(8, u64);
pub const Vec8f64 = @Vector(8, f64);

/// Comptime shape definition
pub const ShapeField = struct {
    name: []const u8,
    field_type: type,
};

/// Generate a SIMD-optimized columnar storage type at comptime
/// When not instantiated, DCE removes this entirely
pub fn ColumnarStorage(comptime fields: []const ShapeField) type {
    return struct {
        const Self = @This();
        const field_count = fields.len;

        // Columnar arrays - each field stored contiguously for SIMD
        arrays: [field_count][]align(32) i64,
        count: u32,
        capacity: u32,

        // JS object address -> index mapping
        index_map: std.AutoHashMapUnmanaged(u64, u32),

        pub fn init(allocator: std.mem.Allocator, initial_capacity: u32) !Self {
            var self: Self = undefined;
            self.count = 0;
            self.capacity = initial_capacity;
            self.index_map = .{};

            inline for (0..field_count) |i| {
                self.arrays[i] = try allocator.alignedAlloc(i64, .@"32", initial_capacity);
            }

            return self;
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            inline for (0..field_count) |i| {
                allocator.free(self.arrays[i]);
            }
            self.index_map.deinit(allocator);
        }

        /// Register object - returns index
        pub fn register(self: *Self, allocator: std.mem.Allocator, js_addr: u64, values: [field_count]i64) !u32 {
            if (self.count >= self.capacity) {
                // Grow arrays
                const new_cap = self.capacity * 2;
                inline for (0..field_count) |i| {
                    const new_arr = try allocator.alignedAlloc(i64, .@"32", new_cap);
                    @memcpy(new_arr[0..self.count], self.arrays[i][0..self.count]);
                    allocator.free(self.arrays[i]);
                    self.arrays[i] = new_arr;
                }
                self.capacity = new_cap;
            }

            const idx = self.count;
            self.count += 1;

            // Store values in columnar layout
            inline for (0..field_count) |i| {
                self.arrays[i][idx] = values[i];
            }

            try self.index_map.put(allocator, js_addr, idx);
            return idx;
        }

        /// SIMD batch load - load 8 values from a field
        pub fn loadBatch(self: *const Self, comptime field_idx: comptime_int, start: u32) Vec8i64 {
            return self.arrays[field_idx][start..][0..8].*;
        }

        /// SIMD batch store - store 8 values to a field
        pub fn storeBatch(self: *Self, comptime field_idx: comptime_int, start: u32, values: Vec8i64) void {
            self.arrays[field_idx][start..][0..8].* = values;
        }

        /// Get single field value by index
        pub fn getField(self: *const Self, comptime field_idx: comptime_int, idx: u32) i64 {
            return self.arrays[field_idx][idx];
        }

        /// Set single field value by index
        pub fn setField(self: *Self, comptime field_idx: comptime_int, idx: u32, value: i64) void {
            self.arrays[field_idx][idx] = value;
        }

        /// Lookup index by JS address
        pub fn lookup(self: *const Self, js_addr: u64) ?u32 {
            return self.index_map.get(js_addr);
        }

        /// SIMD filter - find all indices where field matches value
        pub fn simdFilter(self: *const Self, comptime field_idx: comptime_int, target: i64) []u32 {
            const target_vec: Vec8i64 = @splat(target);
            var results: [8192]u32 = undefined; // Static buffer
            var result_count: usize = 0;

            var i: u32 = 0;
            while (i + 8 <= self.count) : (i += 8) {
                const batch = self.loadBatch(field_idx, i);
                const matches = batch == target_vec;

                // Extract matching indices
                inline for (0..8) |j| {
                    if (matches[j]) {
                        if (result_count < 8192) {
                            results[result_count] = i + @as(u32, @intCast(j));
                            result_count += 1;
                        }
                    }
                }
            }

            // Handle remainder with SIMD (pad with zeros)
            if (i < self.count) {
                var padded: Vec8i64 = @splat(0);
                const remaining = self.count - i;
                for (0..@min(8, remaining)) |j| {
                    padded[j] = self.arrays[field_idx][i + j];
                }
                const matches = padded == target_vec;
                inline for (0..8) |j| {
                    if (j < remaining and matches[j]) {
                        if (result_count < 8192) {
                            results[result_count] = i + @as(u32, @intCast(j));
                            result_count += 1;
                        }
                    }
                }
            }

            return results[0..result_count];
        }

        /// SIMD map - apply function to all values of a field
        pub fn simdMap(self: *Self, comptime field_idx: comptime_int, comptime op: fn (Vec8i64) Vec8i64) void {
            var i: u32 = 0;
            while (i + 8 <= self.count) : (i += 8) {
                const batch = self.loadBatch(field_idx, i);
                const result = op(batch);
                self.storeBatch(field_idx, i, result);
            }

            // Handle remainder with SIMD
            if (i < self.count) {
                var padded: Vec8i64 = @splat(0);
                const remaining = self.count - i;
                for (0..@min(8, remaining)) |j| {
                    padded[j] = self.arrays[field_idx][i + j];
                }
                const result = op(padded);
                for (0..@min(8, remaining)) |j| {
                    self.arrays[field_idx][i + j] = result[j];
                }
            }
        }

        /// SIMD reduce - reduce all values of a field
        pub fn simdReduce(self: *const Self, comptime field_idx: comptime_int, comptime op: std.builtin.ReduceOp) i64 {
            if (self.count == 0) return 0;

            var acc: Vec8i64 = @splat(switch (op) {
                .Add => 0,
                .Mul => 1,
                .Min => std.math.maxInt(i64),
                .Max => std.math.minInt(i64),
                else => 0,
            });

            var i: u32 = 0;
            while (i + 8 <= self.count) : (i += 8) {
                const batch = self.loadBatch(field_idx, i);
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
                var padded: Vec8i64 = switch (op) {
                    .Add => @splat(0),
                    .Mul => @splat(1),
                    .Min => @splat(std.math.maxInt(i64)),
                    .Max => @splat(std.math.minInt(i64)),
                    else => @splat(0),
                };
                const remaining = self.count - i;
                for (0..@min(8, remaining)) |j| {
                    padded[j] = self.arrays[field_idx][i + j];
                }
                acc = switch (op) {
                    .Add => acc + padded,
                    .Mul => acc * padded,
                    .Min => @min(acc, padded),
                    .Max => @max(acc, padded),
                    else => acc,
                };
            }

            // Horizontal reduce
            return @reduce(op, acc);
        }
    };
}

// ============================================================================
// Predefined Shapes (comptime-generated, DCE removes if unused)
// ============================================================================

/// TypeScript AST Node shape - most common in tsc
pub const AstNodeFields = [_]ShapeField{
    .{ .name = "kind", .field_type = i32 },
    .{ .name = "flags", .field_type = i32 },
    .{ .name = "pos", .field_type = i32 },
    .{ .name = "end", .field_type = i32 },
};

/// AstNode columnar storage - SIMD-only
/// If not used, this entire type is removed by DCE
pub const AstNodeStorage = ColumnarStorage(&AstNodeFields);

/// Vector3 shape for 3D/game workloads
pub const Vector3Fields = [_]ShapeField{
    .{ .name = "x", .field_type = f64 },
    .{ .name = "y", .field_type = f64 },
    .{ .name = "z", .field_type = f64 },
};

pub const Vector3Storage = ColumnarStorage(&Vector3Fields);

/// Generic key-value shape
pub const KeyValueFields = [_]ShapeField{
    .{ .name = "key", .field_type = i64 },
    .{ .name = "value", .field_type = i64 },
};

pub const KeyValueStorage = ColumnarStorage(&KeyValueFields);

// ============================================================================
// Comptime Shape Detector (analysis at compile time)
// ============================================================================

/// Comptime property statistics
pub const ComptimePropertyStats = struct {
    name: []const u8,
    get_count: u32 = 0,
    set_count: u32 = 0,
    define_count: u32 = 0,

    pub fn totalAccesses(self: ComptimePropertyStats) u32 {
        return self.get_count + self.set_count + self.define_count;
    }
};

/// Property access record for comptime analysis
pub const PropertyAccess = struct {
    name: []const u8,
    access_type: AccessType,
};

pub const AccessType = enum { get, set, define };

/// Analyze property access patterns at comptime
/// Returns the number of unique properties found
pub fn comptimeAnalyzeCount(comptime property_accesses: []const PropertyAccess) comptime_int {
    comptime {
        var unique_names: [256][]const u8 = undefined;
        var unique_count: usize = 0;

        for (property_accesses) |access| {
            var found = false;
            for (unique_names[0..unique_count]) |name| {
                if (std.mem.eql(u8, name, access.name)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                unique_names[unique_count] = access.name;
                unique_count += 1;
            }
        }
        return unique_count;
    }
}

/// Generate comptime analysis results
pub fn ComptimeAnalysis(comptime property_accesses: []const PropertyAccess) type {
    const count = comptimeAnalyzeCount(property_accesses);

    return struct {
        pub const stats: [count]ComptimePropertyStats = blk: {
            var result: [count]ComptimePropertyStats = undefined;
            var idx: usize = 0;

            // Collect unique properties with counts
            outer: for (property_accesses) |access| {
                // Check if already in result
                for (result[0..idx]) |*s| {
                    if (std.mem.eql(u8, s.name, access.name)) {
                        switch (access.access_type) {
                            .get => s.get_count += 1,
                            .set => s.set_count += 1,
                            .define => s.define_count += 1,
                        }
                        continue :outer;
                    }
                }

                // New property
                result[idx] = .{ .name = access.name };
                switch (access.access_type) {
                    .get => result[idx].get_count = 1,
                    .set => result[idx].set_count = 1,
                    .define => result[idx].define_count = 1,
                }
                idx += 1;
            }

            // Sort by total accesses (descending) using bubble sort
            for (0..count) |i| {
                for (i + 1..count) |j| {
                    if (result[i].totalAccesses() < result[j].totalAccesses()) {
                        const tmp = result[i];
                        result[i] = result[j];
                        result[j] = tmp;
                    }
                }
            }

            break :blk result;
        };

        pub const len = count;
    };
}

/// Generate optimal shape type at comptime based on hot properties
pub fn generateShapeType(comptime hot_properties: []const ComptimePropertyStats, comptime threshold: u32) type {
    comptime {
        var fields: [hot_properties.len]ShapeField = undefined;
        var field_count: usize = 0;

        for (hot_properties) |prop| {
            if (prop.totalAccesses() >= threshold) {
                fields[field_count] = .{
                    .name = prop.name,
                    .field_type = i64, // Default type
                };
                field_count += 1;
            }
        }

        if (field_count == 0) {
            // No hot properties - return minimal storage
            return ColumnarStorage(&[_]ShapeField{.{ .name = "_dummy", .field_type = i64 }});
        }

        return ColumnarStorage(fields[0..field_count]);
    }
}

// ============================================================================
// SIMD Utility Functions (always available, DCE removes if unused)
// ============================================================================

/// SIMD horizontal sum for i32
pub inline fn simdSum8i32(vec: Vec8i32) i32 {
    return @reduce(.Add, vec);
}

/// SIMD horizontal sum for i64
pub inline fn simdSum8i64(vec: Vec8i64) i64 {
    return @reduce(.Add, vec);
}

/// SIMD horizontal min for i32
pub inline fn simdMin8i32(vec: Vec8i32) i32 {
    return @reduce(.Min, vec);
}

/// SIMD horizontal max for i32
pub inline fn simdMax8i32(vec: Vec8i32) i32 {
    return @reduce(.Max, vec);
}

/// SIMD compare and mask - returns matching indices
pub inline fn simdCompareEq8i64(vec: Vec8i64, target: i64) @Vector(8, bool) {
    const target_vec: Vec8i64 = @splat(target);
    return vec == target_vec;
}

/// SIMD gather - load non-contiguous elements
pub inline fn simdGather8i64(arr: []const i64, indices: Vec8u32) Vec8i64 {
    var result: Vec8i64 = undefined;
    inline for (0..8) |i| {
        result[i] = arr[indices[i]];
    }
    return result;
}

/// SIMD scatter - store to non-contiguous locations
pub inline fn simdScatter8i64(arr: []i64, indices: Vec8u32, values: Vec8i64) void {
    inline for (0..8) |i| {
        arr[indices[i]] = values[i];
    }
}

// ============================================================================
// Tests (comptime verification)
// ============================================================================

test "comptime shape generation" {
    const Storage = ColumnarStorage(&AstNodeFields);

    var storage = try Storage.init(std.testing.allocator, 64);
    defer storage.deinit(std.testing.allocator);

    // Register a node
    const idx = try storage.register(std.testing.allocator, 0x1234, .{ 100, 0, 0, 10 });
    try std.testing.expectEqual(@as(u32, 0), idx);

    // Verify field access
    try std.testing.expectEqual(@as(i64, 100), storage.getField(0, 0)); // kind
    try std.testing.expectEqual(@as(i64, 10), storage.getField(3, 0)); // end
}

test "SIMD batch operations" {
    const Storage = ColumnarStorage(&AstNodeFields);

    var storage = try Storage.init(std.testing.allocator, 64);
    defer storage.deinit(std.testing.allocator);

    // Register 8 nodes for SIMD batch
    for (0..8) |i| {
        _ = try storage.register(std.testing.allocator, 0x1000 + i, .{
            @as(i64, @intCast(i * 10)), // kind
            0,
            @as(i64, @intCast(i)),
            @as(i64, @intCast(i + 10)),
        });
    }

    // SIMD batch load
    const kinds = storage.loadBatch(0, 0);
    try std.testing.expectEqual(@as(i64, 0), kinds[0]);
    try std.testing.expectEqual(@as(i64, 70), kinds[7]);

    // SIMD reduce
    const sum = storage.simdReduce(0, .Add);
    try std.testing.expectEqual(@as(i64, 0 + 10 + 20 + 30 + 40 + 50 + 60 + 70), sum);
}

test "SIMD filter" {
    const Storage = ColumnarStorage(&AstNodeFields);

    var storage = try Storage.init(std.testing.allocator, 64);
    defer storage.deinit(std.testing.allocator);

    // Register nodes with various kinds
    _ = try storage.register(std.testing.allocator, 0x1000, .{ 100, 0, 0, 10 });
    _ = try storage.register(std.testing.allocator, 0x1001, .{ 200, 0, 0, 10 });
    _ = try storage.register(std.testing.allocator, 0x1002, .{ 100, 0, 0, 10 });
    _ = try storage.register(std.testing.allocator, 0x1003, .{ 300, 0, 0, 10 });
    _ = try storage.register(std.testing.allocator, 0x1004, .{ 100, 0, 0, 10 });

    // Find all nodes with kind=100
    const matches = storage.simdFilter(0, 100);
    try std.testing.expectEqual(@as(usize, 3), matches.len);
    try std.testing.expectEqual(@as(u32, 0), matches[0]);
    try std.testing.expectEqual(@as(u32, 2), matches[1]);
    try std.testing.expectEqual(@as(u32, 4), matches[2]);
}

test "comptime analysis" {
    const accesses = [_]PropertyAccess{
        .{ .name = "kind", .access_type = .get },
        .{ .name = "kind", .access_type = .get },
        .{ .name = "kind", .access_type = .get },
        .{ .name = "flags", .access_type = .get },
        .{ .name = "pos", .access_type = .get },
        .{ .name = "pos", .access_type = .get },
    };

    const Analysis = ComptimeAnalysis(&accesses);
    const stats = Analysis.stats;

    try std.testing.expectEqual(@as(usize, 3), Analysis.len);
    try std.testing.expectEqualStrings("kind", stats[0].name);
    try std.testing.expectEqual(@as(u32, 3), stats[0].get_count);
}
