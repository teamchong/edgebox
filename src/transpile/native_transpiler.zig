//! Native TypeScript Transpiler - Fast Path for Simple Cases
//!
//! This module provides a native Zig implementation of simple TypeScript
//! transformations, bypassing the full TypeScript compiler for common patterns.
//!
//! Supported transformations:
//!   - const -> var
//!   - Type annotations removal (simple and generic types)
//!   - e.g., `: number`, `: string`, `: Array<number>`, `: Map<K, V>`
//!
//! For complex cases, this returns null and the caller should fall back
//! to the full TypeScript compiler.

const std = @import("std");

/// Result of transpilation
pub const TranspileResult = struct {
    output: []const u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *TranspileResult) void {
        self.allocator.free(self.output);
    }
};

/// Check if source can be handled by fast path
/// Returns true for simple variable declarations without complex types
pub fn canFastTranspile(source: []const u8) bool {
    // Quick heuristics for simple cases:
    // 1. No class/interface/type/enum declarations
    // 2. No imports/exports with type modifiers
    // 3. No arrow functions or async/await

    // Check for unsupported constructs
    if (std.mem.indexOf(u8, source, "class ") != null) return false;
    if (std.mem.indexOf(u8, source, "interface ") != null) return false;
    if (std.mem.indexOf(u8, source, "type ") != null) return false;
    if (std.mem.indexOf(u8, source, "enum ") != null) return false;
    if (std.mem.indexOf(u8, source, "import ") != null) return false;
    if (std.mem.indexOf(u8, source, "export ") != null) return false;
    if (std.mem.indexOf(u8, source, "function ") != null) return false;
    if (std.mem.indexOf(u8, source, "async ") != null) return false;
    if (std.mem.indexOf(u8, source, "await ") != null) return false;
    if (std.mem.indexOf(u8, source, "=>")) |_| return false;

    return true;
}

/// Fast transpile a simple TypeScript statement
/// Returns null if the source cannot be handled by fast path
pub fn fastTranspile(allocator: std.mem.Allocator, source: []const u8) !?TranspileResult {
    if (!canFastTranspile(source)) return null;

    // For simple cases, just replace "const " with "var "
    // and remove type annotations (including generics)

    var output = std.ArrayListUnmanaged(u8){};
    errdefer output.deinit(allocator);

    var i: usize = 0;
    while (i < source.len) {
        // Check for "const " keyword
        if (i + 6 <= source.len and std.mem.eql(u8, source[i .. i + 6], "const ")) {
            try output.appendSlice(allocator, "var ");
            i += 6;
            continue;
        }

        // Check for type annotation ": type" pattern
        // Handles simple types and generics like Array<number>, Map<K, V>
        if (source[i] == ':' and i + 1 < source.len) {
            // Look ahead to see if this is a type annotation
            var j = i + 1;
            // Skip whitespace after colon
            while (j < source.len and (source[j] == ' ' or source[j] == '\t')) {
                j += 1;
            }
            // Check if followed by identifier (type name)
            const type_start = j;
            while (j < source.len and (std.ascii.isAlphanumeric(source[j]) or source[j] == '_')) {
                j += 1;
            }

            // Check for generic type parameters <...>
            if (j < source.len and source[j] == '<') {
                // Parse balanced angle brackets
                var angle_depth: u32 = 1;
                j += 1;
                while (j < source.len and angle_depth > 0) {
                    switch (source[j]) {
                        '<' => angle_depth += 1,
                        '>' => angle_depth -= 1,
                        else => {},
                    }
                    j += 1;
                }
                // If brackets didn't balance, fall back
                if (angle_depth != 0) {
                    try output.append(allocator, source[i]);
                    i += 1;
                    continue;
                }
            }

            // Record position after type (before trailing whitespace)
            const after_type = j;
            // Skip trailing whitespace
            while (j < source.len and (source[j] == ' ' or source[j] == '\t')) {
                j += 1;
            }
            // If we hit = or ; or , or end, this was a type annotation - skip it
            if (j < source.len and (source[j] == '=' or source[j] == ';' or source[j] == ',')) {
                if (after_type > type_start) {
                    // Skip the type annotation (colon + whitespace + type + generics)
                    // but add a space before = if there was whitespace
                    if (j > after_type and source[j] == '=') {
                        try output.append(allocator, ' ');
                    }
                    i = j;
                    continue;
                }
            }
        }

        // Copy character as-is
        try output.append(allocator, source[i]);
        i += 1;
    }

    // Add newline if not present
    if (output.items.len > 0 and output.items[output.items.len - 1] != '\n') {
        try output.append(allocator, '\n');
    }

    return TranspileResult{
        .output = try output.toOwnedSlice(allocator),
        .allocator = allocator,
    };
}

// ============================================================================
// Tests
// ============================================================================

test "simple const to var" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "const x = 1;")).?;
    defer result.deinit();
    try std.testing.expectEqualStrings("var x = 1;\n", result.output);
}

test "const with type annotation" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "const x: number = 1;")).?;
    defer result.deinit();
    try std.testing.expectEqualStrings("var x = 1;\n", result.output);
}

test "multiple const declarations" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "const a = 1; const b = 2;")).?;
    defer result.deinit();
    try std.testing.expectEqualStrings("var a = 1; var b = 2;\n", result.output);
}

test "complex case returns null" {
    const allocator = std.testing.allocator;
    const result = try fastTranspile(allocator, "class Foo {}");
    try std.testing.expect(result == null);
}

test "arrow function returns null" {
    const allocator = std.testing.allocator;
    const result = try fastTranspile(allocator, "const f = () => 1;");
    try std.testing.expect(result == null);
}

test "generic type Array<number>" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "const arr: Array<number> = [];")).?;
    defer result.deinit();
    try std.testing.expectEqualStrings("var arr = [];\n", result.output);
}

test "generic type Map<string, number>" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "const m: Map<string, number> = new Map();")).?;
    defer result.deinit();
    try std.testing.expectEqualStrings("var m = new Map();\n", result.output);
}

test "nested generic type" {
    const allocator = std.testing.allocator;
    var result = (try fastTranspile(allocator, "const x: Array<Map<string, number>> = [];")).?;
    defer result.deinit();
    try std.testing.expectEqualStrings("var x = [];\n", result.output);
}
