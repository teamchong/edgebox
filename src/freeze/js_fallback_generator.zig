//! Generate JavaScript block fallback functions for partial freeze
//!
//! These functions are called when frozen code hits contaminated blocks.
//! They receive (args, locals, block_id, stack) and return updated state.
//!
//! Block-level fallback enables resumption from specific contaminated blocks
//! rather than re-executing the entire function.

const std = @import("std");
const Allocator = std.mem.Allocator;
const cfg_builder = @import("cfg_builder.zig");

/// Generate JS fallback function for a partially frozen function
pub fn generateBlockFallback(
    allocator: Allocator,
    func_name: []const u8,
    cfg: *const cfg_builder.CFG,
    arg_count: u32,
    var_count: u32,
) ![]const u8 {
    var output = std.ArrayListUnmanaged(u8){};
    errdefer output.deinit(allocator);
    const writer = output.writer(allocator);

    // Collect contaminated blocks
    var contaminated_blocks = std.ArrayListUnmanaged(ContaminatedBlock){};
    defer contaminated_blocks.deinit(allocator);

    for (cfg.blocks.items) |block| {
        if (block.is_contaminated) {
            try contaminated_blocks.append(allocator, .{
                .id = block.id,
                .reason = block.contamination_reason orelse "unknown",
                .has_unfreezable_opcode = block.has_unfreezable_opcode,
            });
        }
    }

    if (contaminated_blocks.items.len == 0) {
        return output.toOwnedSlice(allocator);
    }

    // Generate function header
    try writer.print(
        \\
        \\// Block fallback for partially frozen function: {s}
        \\// Contaminated blocks: {d}
        \\function __block_fallback_{s}(
    , .{ func_name, contaminated_blocks.items.len, func_name });

    // Generate parameter list: (arg0, arg1, ..., locals, block_id, stack)
    for (0..arg_count) |i| {
        try writer.print("arg{d}, ", .{i});
    }
    try writer.writeAll("locals, block_id, stack) {\n");

    // Get the original function reference
    try writer.print(
        \\    // Get the original bytecode function (saved before frozen replacement)
        \\    const original = __original_{s} || globalThis.{s};
        \\    if (!original) {{
        \\        throw new Error("Original function '{s}' not found for block fallback");
        \\    }}
        \\
        \\    // Block-level dispatch based on which contaminated block was hit
        \\    switch (block_id) {{
        \\
    , .{ func_name, func_name, func_name });

    // Generate case for each contaminated block
    for (contaminated_blocks.items) |block| {
        try writer.print(
            \\        case {d}: // {s}
            \\
        , .{ block.id, block.reason });

        // Check if this block type can be optimized
        if (canOptimizeBlock(block.reason)) {
            try emitOptimizedBlockHandler(writer, block, arg_count, var_count);
        } else {
            // Default: fall through to full function re-execution
            try writer.writeAll("            break; // Fall through to full re-execution\n");
        }
    }

    // Default case and function re-execution fallback
    try writer.print(
        \\        default:
        \\            break;
        \\    }}
        \\
        \\    // Full function re-execution fallback
        \\    // Used when block-level optimization is not available
        \\    try {{
        \\        const result = original(
    , .{});

    // Pass original args
    for (0..arg_count) |i| {
        if (i > 0) try writer.writeAll(", ");
        try writer.print("arg{d}", .{i});
    }

    try writer.print(
        \\);
        \\        return {{ return_value: result }};
        \\    }} catch (e) {{
        \\        throw e;
        \\    }}
        \\}}
        \\
    , .{});

    _ = var_count; // Reserved for future block-level local state restoration

    return output.toOwnedSlice(allocator);
}

const ContaminatedBlock = struct {
    id: u32,
    reason: []const u8,
    has_unfreezable_opcode: bool,
};

/// Check if a contaminated block type can be optimized
fn canOptimizeBlock(reason: []const u8) bool {
    // Currently no block types are optimized - all fall back to full re-execution
    // Future optimizations can be added here for specific contamination reasons:
    // - "eval": Could potentially be handled with direct eval() call
    // - "await": Could be handled with async wrapper
    // - Closures: May need special handling for captured variables
    _ = reason;
    return false;
}

/// Emit optimized handler for a specific block type (future use)
fn emitOptimizedBlockHandler(
    writer: anytype,
    block: ContaminatedBlock,
    arg_count: u32,
    var_count: u32,
) !void {
    _ = block;
    _ = arg_count;
    _ = var_count;
    // Placeholder for future optimized block handlers
    try writer.writeAll("            // Optimized handler (not yet implemented)\n");
    try writer.writeAll("            break;\n");
}

/// Generate all block fallback functions for a module
pub fn generateAllFallbacks(
    allocator: Allocator,
    functions: []const FunctionInfo,
) ![]const u8 {
    var output = std.ArrayListUnmanaged(u8){};
    errdefer output.deinit(allocator);

    try output.appendSlice(allocator,
        \\// Auto-generated block fallback functions for partial freeze
        \\// These are called when frozen C code hits contaminated blocks
        \\
        \\
    );

    for (functions) |func| {
        if (func.has_partial_freeze) {
            const fallback = try generateBlockFallback(
                allocator,
                func.name,
                func.cfg,
                func.arg_count,
                func.var_count,
            );
            defer allocator.free(fallback);
            try output.appendSlice(allocator, fallback);
        }
    }

    return output.toOwnedSlice(allocator);
}

pub const FunctionInfo = struct {
    name: []const u8,
    cfg: *const cfg_builder.CFG,
    arg_count: u32,
    var_count: u32,
    has_partial_freeze: bool,
};
