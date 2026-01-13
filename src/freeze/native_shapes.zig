//! Native Shape Definitions for Zero-Overhead Polyfills
//!
//! This module defines native Zig structs that mirror common JavaScript object shapes.
//! For hot paths like TypeScript AST traversal, we can operate on native memory
//! instead of going through QuickJS JSValue operations.
//!
//! Architecture:
//!   1. Shape Detection: At compile time, detect objects with known shapes
//!   2. Native Registry: Map JSValue addresses to native struct pointers
//!   3. Fast Access: Property access on known shapes → direct memory load
//!   4. Fallback: Unknown shapes → standard QuickJS path
//!
//! Performance:
//!   - QuickJS property access: ~35 cycles (hash lookup + boxing + exception check)
//!   - Native field access: ~1 cycle (direct memory load)
//!   - Potential 35x speedup on property access
//!
//! SIMD Columnar Storage:
//!   - Nodes stored in Structure-of-Arrays (SoA) layout
//!   - 32-byte aligned columns for SIMD (8 x i32 per batch)
//!   - simdFilter: Find all nodes by kind in parallel
//!   - simdReduce: Aggregate operations (sum, min, max)
//!   - Batch processing: 8 nodes at once

const std = @import("std");
const columnar = @import("../shapes/columnar_storage.zig");

// ============================================================================
// TypeScript AST Node Shape
// ============================================================================

/// Native representation of a TypeScript AST Node
/// Mirrors the shape detected from TypeScript's createNode() pattern
pub const AstNode = extern struct {
    /// SyntaxKind enum value
    kind: i32,
    /// NodeFlags bit flags
    flags: i32,
    /// Start position in source
    pos: i32,
    /// End position in source
    end: i32,
    /// Pointer to parent node (or null)
    parent: ?*AstNode,
    /// Original JSValue for fallback (stored as raw bits)
    js_value: u64,
    /// Modifier flags (optional, may be 0)
    modifier_flags_cache: i32,
    /// Transform flags (optional, may be 0)
    transform_flags: i32,
};

/// Extended AST node with text content (for identifiers, literals)
pub const AstNodeWithText = extern struct {
    base: AstNode,
    /// Text content (for identifiers, string literals)
    text_ptr: ?[*]const u8,
    text_len: u32,
    /// Original text (for template literals, etc.)
    original_text_ptr: ?[*]const u8,
    original_text_len: u32,
};

// ============================================================================
// Native Node Registry with SIMD Columnar Storage
// ============================================================================

/// Column indices for AST node fields
pub const NodeColumn = enum(u32) {
    kind = 0,
    flags = 1,
    pos = 2,
    end = 3,
    parent_row = 4, // Row index of parent node (-1 if none)
    js_addr_lo = 5, // Lower 32 bits of js_addr
    js_addr_hi = 6, // Upper 32 bits of js_addr
};

/// Field names for columnar storage
const node_field_names = [_][]const u8{
    "kind",
    "flags",
    "pos",
    "end",
    "parent_row",
    "js_addr_lo",
    "js_addr_hi",
};

/// Registry mapping JSValue addresses to native AstNode pointers
/// Uses SIMD columnar storage for cache-friendly, vectorized operations
pub const NodeRegistry = struct {
    const Self = @This();
    const Pool = std.ArrayListUnmanaged(AstNode);

    allocator: std.mem.Allocator,

    /// Columnar storage for SIMD operations on node fields
    columnar_storage: ?columnar.DynamicColumnarStorage = null,

    /// AstNode pool for pointer-based API compatibility
    /// Row index in columnar_storage matches index in node_pool
    node_pool: Pool = .{},

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Self) void {
        if (self.columnar_storage) |*cs| {
            cs.deinit();
        }
        self.node_pool.deinit(self.allocator);
    }

    /// Lazily initialize columnar storage
    fn ensureColumnarStorage(self: *Self) !*columnar.DynamicColumnarStorage {
        if (self.columnar_storage == null) {
            self.columnar_storage = try columnar.DynamicColumnarStorage.init(
                self.allocator,
                &node_field_names,
                4096, // Initial capacity for 4K nodes
            );
        }
        return &self.columnar_storage.?;
    }

    /// Register a new native node for a JSValue
    /// Returns pointer to the native node
    pub fn register(self: *Self, js_addr: u64, kind: i32, flags: i32, pos: i32, end: i32) !*AstNode {
        const cs = try self.ensureColumnarStorage();

        // Store in columnar format for SIMD operations
        const values = [_]i64{
            @intCast(kind),
            @intCast(flags),
            @intCast(pos),
            @intCast(end),
            -1, // parent_row (none initially)
            @intCast(js_addr & 0xFFFFFFFF), // js_addr_lo
            @intCast(js_addr >> 32), // js_addr_hi
        };
        const row = try cs.register(js_addr, &values);

        // Also maintain AstNode pool for pointer-based access
        try self.node_pool.append(self.allocator, .{
            .kind = kind,
            .flags = flags,
            .pos = pos,
            .end = end,
            .parent = null,
            .js_value = js_addr,
            .modifier_flags_cache = 0,
            .transform_flags = 0,
        });

        // Sanity check: row index should match pool index
        std.debug.assert(row == self.node_pool.items.len - 1);

        return &self.node_pool.items[row];
    }

    /// Look up native node for a JSValue address
    /// Returns null if not found (fallback to QuickJS)
    pub fn lookup(self: *Self, js_addr: u64) ?*AstNode {
        if (self.columnar_storage) |*cs| {
            if (cs.lookup(js_addr)) |row| {
                return &self.node_pool.items[row];
            }
        }
        return null;
    }

    /// Set parent relationship
    pub fn setParent(self: *Self, child_addr: u64, parent_addr: u64) void {
        if (self.columnar_storage) |*cs| {
            if (cs.lookup(child_addr)) |child_row| {
                if (cs.lookup(parent_addr)) |parent_row| {
                    // Update columnar storage
                    cs.setField(child_row, @intFromEnum(NodeColumn.parent_row), @intCast(parent_row));
                    // Update AstNode pointer
                    self.node_pool.items[child_row].parent = &self.node_pool.items[parent_row];
                }
            }
        }
    }

    // ========================================================================
    // SIMD Batch Operations - exposed for high-performance queries
    // ========================================================================

    /// SIMD filter - find all nodes with a specific kind
    /// Returns count of matching nodes, indices stored in result_buf
    pub fn simdFilterByKind(self: *Self, target_kind: i32, result_buf: []u32) usize {
        if (self.columnar_storage) |*cs| {
            return cs.simdFilter(@intFromEnum(NodeColumn.kind), @intCast(target_kind), result_buf);
        }
        return 0;
    }

    /// SIMD filter - find all nodes with specific flags
    pub fn simdFilterByFlags(self: *Self, target_flags: i32, result_buf: []u32) usize {
        if (self.columnar_storage) |*cs| {
            return cs.simdFilter(@intFromEnum(NodeColumn.flags), @intCast(target_flags), result_buf);
        }
        return 0;
    }

    /// SIMD multi-filter - find nodes matching multiple conditions
    pub fn simdMultiFilter(self: *Self, conditions: []const columnar.FilterCondition, result_buf: []u32) usize {
        if (self.columnar_storage) |*cs| {
            return cs.simdMultiFilter(conditions, result_buf);
        }
        return 0;
    }

    /// Get node by row index (for use after SIMD filter)
    pub fn getNodeByRow(self: *Self, row: u32) ?*AstNode {
        if (row < self.node_pool.items.len) {
            return &self.node_pool.items[row];
        }
        return null;
    }

    /// SIMD batch load - get 8 kinds at once
    pub fn loadKindBatch(self: *Self, start_row: u32) columnar.Vec8i64 {
        if (self.columnar_storage) |*cs| {
            if (start_row + 8 <= cs.count) {
                return cs.loadBatch(@intFromEnum(NodeColumn.kind), start_row);
            }
        }
        return @splat(0);
    }

    /// Get total node count
    pub fn count(self: *Self) u32 {
        if (self.columnar_storage) |cs| {
            return cs.count;
        }
        return @intCast(self.node_pool.items.len);
    }
};

// ============================================================================
// Global Registry Instance and Exported Functions
// ============================================================================
// Pure Zig implementation - replaces frozen_runtime.c native registry

var global_registry: ?NodeRegistry = null;
var debug_last_lookup_addr: u64 = 0;
var debug_lookup_found: bool = false;
var debug_lookup_call_count: i32 = 0;
var debug_last_registered_addr: u64 = 0;
var debug_register_success: bool = false;
var debug_register32_addr: u32 = 0;
var debug_register32_called: i32 = 0;

/// Page allocator for WASM - works in both native and WASM builds
const registry_allocator = std.heap.page_allocator;

/// Initialize the native registry
pub export fn native_registry_init() void {
    if (global_registry == null) {
        global_registry = NodeRegistry.init(registry_allocator);
    }
}

/// Clean up the native registry
pub export fn native_registry_deinit() void {
    if (global_registry) |*reg| {
        reg.deinit();
        global_registry = null;
    }
}

/// Get count of registered nodes
pub export fn native_registry_count() c_int {
    if (global_registry) |reg| {
        return @intCast(reg.node_pool.items.len);
    }
    return 0;
}

/// Register a native node (64-bit address)
pub export fn native_node_register(js_addr: u64, kind: i32, flags: i32, pos: i32, end: i32) ?*AstNode {
    debug_last_registered_addr = js_addr;
    debug_register_success = false;

    if (global_registry) |*reg| {
        const node = reg.register(js_addr, kind, flags, pos, end) catch return null;
        debug_register_success = true;
        return node;
    }
    return null;
}

/// Register a native node (32-bit address for WASM ABI)
pub export fn native_node_register32(js_addr32: u32, kind: i32, flags: i32, pos: i32, end: i32) ?*AstNode {
    debug_register32_called += 1;
    debug_register32_addr = js_addr32;
    return native_node_register(@as(u64, js_addr32), kind, flags, pos, end);
}

/// Fast lookup for native node
pub export fn native_node_lookup(js_addr: u64) ?*AstNode {
    debug_last_lookup_addr = js_addr;
    debug_lookup_call_count += 1;

    if (global_registry) |*reg| {
        const node = reg.lookup(js_addr);
        debug_lookup_found = (node != null);
        return node;
    }
    debug_lookup_found = false;
    return null;
}

/// Alias for native_node_lookup - used by zig_runtime.zig
pub export fn native_shapes_lookup(js_addr: u64) ?*AstNode {
    return native_node_lookup(js_addr);
}

/// Fast property accessors
pub export fn native_get_kind(node: ?*AstNode) i32 {
    return if (node) |n| n.kind else 0;
}

pub export fn native_get_flags(node: ?*AstNode) i32 {
    return if (node) |n| n.flags else 0;
}

pub export fn native_get_pos(node: ?*AstNode) i32 {
    return if (node) |n| n.pos else 0;
}

pub export fn native_get_end(node: ?*AstNode) i32 {
    return if (node) |n| n.end else 0;
}

pub export fn native_get_parent(node: ?*AstNode) ?*AstNode {
    return if (node) |n| n.parent else null;
}

/// Set parent relationship
pub export fn native_node_set_parent(child: ?*AstNode, parent: ?*AstNode) void {
    if (child) |c| {
        c.parent = parent;
    }
}

/// Debug accessors
pub export fn native_debug_get_last_lookup() u64 {
    return debug_last_lookup_addr;
}

pub export fn native_debug_get_found() c_int {
    return if (debug_lookup_found) 1 else 0;
}

pub export fn native_debug_get_call_count() c_int {
    return debug_lookup_call_count;
}

pub export fn native_debug_get_registered_addr() u64 {
    return debug_last_registered_addr;
}

pub export fn native_debug_get_register_success() c_int {
    return if (debug_register_success) 1 else 0;
}

pub export fn native_debug_get_register32_addr() u32 {
    return debug_register32_addr;
}

pub export fn native_debug_get_register32_called() c_int {
    return debug_register32_called;
}

pub export fn native_debug_log_lookup(addr: u64, found: c_int) void {
    debug_last_lookup_addr = addr;
    debug_lookup_found = (found != 0);
    debug_lookup_call_count += 1;
}

// ============================================================================
// SIMD Batch Operations - Exported for High-Performance Node Processing
// ============================================================================

/// Thread-local result buffer for SIMD filter operations
var simd_result_buffer: [4096]u32 = undefined;

/// SIMD filter by kind - returns count of matching nodes
/// Results stored in internal buffer, use native_simd_get_result() to retrieve
pub export fn native_simd_filter_by_kind(target_kind: i32) u32 {
    if (global_registry) |*reg| {
        return @intCast(reg.simdFilterByKind(target_kind, &simd_result_buffer));
    }
    return 0;
}

/// SIMD filter by flags - returns count of matching nodes
pub export fn native_simd_filter_by_flags(target_flags: i32) u32 {
    if (global_registry) |*reg| {
        return @intCast(reg.simdFilterByFlags(target_flags, &simd_result_buffer));
    }
    return 0;
}

/// Get row index from SIMD filter result buffer
pub export fn native_simd_get_result(index: u32) u32 {
    if (index < simd_result_buffer.len) {
        return simd_result_buffer[index];
    }
    return 0;
}

/// Get node pointer by row index (use after SIMD filter)
pub export fn native_get_node_by_row(row: u32) ?*AstNode {
    if (global_registry) |*reg| {
        return reg.getNodeByRow(row);
    }
    return null;
}

/// Get total count of registered nodes
pub export fn native_node_count() u32 {
    if (global_registry) |*reg| {
        return reg.count();
    }
    return 0;
}

/// SIMD batch get kind - returns kind at row index
/// For batch processing: caller iterates rows 0..count and calls this
pub export fn native_batch_get_kind(row: u32) i32 {
    if (global_registry) |reg| {
        if (row < reg.node_pool.items.len) {
            return reg.node_pool.items[row].kind;
        }
    }
    return 0;
}

/// SIMD batch get field from columnar storage
/// col: 0=kind, 1=flags, 2=pos, 3=end
pub export fn native_columnar_get(row: u32, col: u32) i64 {
    if (global_registry) |*reg| {
        if (reg.columnar_storage) |*cs| {
            if (row < cs.count and col < cs.field_count) {
                return cs.getField(row, col);
            }
        }
    }
    return 0;
}

// ============================================================================
// Shape Detection Helpers
// ============================================================================

/// Known shape IDs for optimization
pub const ShapeId = enum(u8) {
    unknown = 0,
    ast_node = 1,
    ast_node_with_text = 2,
    source_file = 3,
    symbol = 4,
    type = 5,
};

/// Properties that define the AST Node shape
pub const ast_node_properties = [_][]const u8{
    "kind",
    "flags",
    "pos",
    "end",
    "parent",
};

/// Check if a set of property names matches the AST Node shape
pub fn matchesAstNodeShape(properties: []const []const u8) bool {
    var matches: u32 = 0;
    for (properties) |prop| {
        for (ast_node_properties) |required| {
            if (std.mem.eql(u8, prop, required)) {
                matches += 1;
                break;
            }
        }
    }
    // Must have at least kind, pos, end to be an AST node
    return matches >= 3;
}

// ============================================================================
// Tests
// ============================================================================

test "AstNode size" {
    // Ensure struct is reasonably sized
    try std.testing.expect(@sizeOf(AstNode) <= 64);
}

test "NodeRegistry basic operations" {
    var reg = NodeRegistry.init(std.testing.allocator);
    defer reg.deinit();

    // Register a node
    const node = try reg.register(0x1234, 100, 0, 0, 10);
    try std.testing.expectEqual(@as(i32, 100), node.kind);

    // Lookup should find it
    const found = reg.lookup(0x1234);
    try std.testing.expect(found != null);
    try std.testing.expectEqual(@as(i32, 100), found.?.kind);

    // Unknown address returns null
    try std.testing.expect(reg.lookup(0x9999) == null);
}

test "NodeRegistry SIMD filter by kind" {
    var reg = NodeRegistry.init(std.testing.allocator);
    defer reg.deinit();

    // Register nodes with various kinds
    _ = try reg.register(0x1000, 100, 0, 0, 10); // kind=100
    _ = try reg.register(0x1001, 200, 0, 10, 20); // kind=200
    _ = try reg.register(0x1002, 100, 1, 20, 30); // kind=100
    _ = try reg.register(0x1003, 300, 0, 30, 40); // kind=300
    _ = try reg.register(0x1004, 100, 0, 40, 50); // kind=100

    // SIMD filter by kind=100 should find 3 nodes
    var result_buf: [64]u32 = undefined;
    const count = reg.simdFilterByKind(100, &result_buf);

    try std.testing.expectEqual(@as(usize, 3), count);

    // Verify row indices
    try std.testing.expectEqual(@as(u32, 0), result_buf[0]);
    try std.testing.expectEqual(@as(u32, 2), result_buf[1]);
    try std.testing.expectEqual(@as(u32, 4), result_buf[2]);

    // Get nodes by row
    const node0 = reg.getNodeByRow(result_buf[0]);
    try std.testing.expect(node0 != null);
    try std.testing.expectEqual(@as(i32, 100), node0.?.kind);
}

test "NodeRegistry columnar storage integration" {
    var reg = NodeRegistry.init(std.testing.allocator);
    defer reg.deinit();

    // Register a node
    _ = try reg.register(0x5000, 42, 7, 100, 200);

    // Verify columnar storage has the data
    try std.testing.expect(reg.columnar_storage != null);

    const cs = &reg.columnar_storage.?;
    try std.testing.expectEqual(@as(u32, 1), cs.count);

    // Check columnar values
    try std.testing.expectEqual(@as(i64, 42), cs.getField(0, @intFromEnum(NodeColumn.kind)));
    try std.testing.expectEqual(@as(i64, 7), cs.getField(0, @intFromEnum(NodeColumn.flags)));
    try std.testing.expectEqual(@as(i64, 100), cs.getField(0, @intFromEnum(NodeColumn.pos)));
    try std.testing.expectEqual(@as(i64, 200), cs.getField(0, @intFromEnum(NodeColumn.end)));
}
