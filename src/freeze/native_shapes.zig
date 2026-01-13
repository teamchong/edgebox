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

const std = @import("std");

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
// Native Node Registry
// ============================================================================

/// Registry mapping JSValue addresses to native AstNode pointers
/// Uses a simple hash map for O(1) lookup
pub const NodeRegistry = struct {
    const Self = @This();
    const Map = std.AutoHashMapUnmanaged(u64, *AstNode);
    const Pool = std.ArrayListUnmanaged(AstNode);

    allocator: std.mem.Allocator,
    nodes: Map = .{},
    node_pool: Pool = .{},

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Self) void {
        self.nodes.deinit(self.allocator);
        self.node_pool.deinit(self.allocator);
    }

    /// Register a new native node for a JSValue
    /// Returns pointer to the native node
    pub fn register(self: *Self, js_addr: u64, kind: i32, flags: i32, pos: i32, end: i32) !*AstNode {
        // Allocate from pool
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

        const node = &self.node_pool.items[self.node_pool.items.len - 1];
        self.nodes.put(self.allocator, js_addr, node) catch return error.OutOfMemory;
        return node;
    }

    /// Look up native node for a JSValue address
    /// Returns null if not found (fallback to QuickJS)
    pub fn lookup(self: *Self, js_addr: u64) ?*AstNode {
        return self.nodes.get(js_addr);
    }

    /// Set parent relationship
    pub fn setParent(self: *Self, child_addr: u64, parent_addr: u64) void {
        if (self.nodes.get(child_addr)) |child| {
            if (self.nodes.get(parent_addr)) |parent| {
                child.parent = parent;
            }
        }
    }
};

// ============================================================================
// C Interface for Native Access
// ============================================================================
// NOTE: The actual native registry implementation is in frozen_runtime.c
// These exports are NOT needed here since the C code provides them.
// The polyfill (src/polyfills/native_shapes.zig) uses extern declarations
// to call the C implementations directly.
//
// The C implementation in frozen_runtime.c provides:
//   - native_registry_init()
//   - native_registry_deinit()
//   - native_registry_count()
//   - native_node_register()
//   - native_node_lookup()
//   - native_get_kind/flags/pos/end/parent()
//   - native_debug_get_last_lookup()
//   - native_debug_get_found()
// ============================================================================

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
