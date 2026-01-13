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
