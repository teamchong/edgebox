//! Generate JavaScript block fallback functions for partial freeze
//!
//! These functions are called when frozen code hits contaminated blocks.
//! They receive (args, locals, block_id, stack) and return updated state.

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

    // Find all contaminated blocks
    var has_contaminated = false;
    for (cfg.blocks.items) |block| {
        if (block.is_contaminated) {
            has_contaminated = true;
            break;
        }
    }

    if (!has_contaminated) {
        return output.toOwnedSlice(allocator);
    }

    // Generate function header
    try writer.print(
        \\
        \\// Block fallback for partially frozen function: {s}
        \\function __block_fallback_{s}(
    , .{ func_name, func_name });

    // Generate parameter list: (arg0, arg1, ..., locals, block_id, stack)
    for (0..arg_count) |i| {
        try writer.print("arg{d}, ", .{i});
    }
    try writer.writeAll("locals, block_id, stack) {\n");

    // Get the original function to re-execute
    try writer.print(
        \\    // Get the original bytecode function (saved before frozen replacement)
        \\    const original = __original_{s} || globalThis.{s};
        \\    if (!original) {{
        \\        throw new Error("Original function '{s}' not found for block fallback");
        \\    }}
        \\
        \\
    , .{ func_name, func_name, func_name });

    // For now, just re-execute the entire function
    // This is not optimal but it's correct and simple
    try writer.print(
        \\    // Re-execute entire function (function-level fallback)
        \\    // TODO: Implement true block-level execution when JS has block PC info
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

    return output.toOwnedSlice(allocator);
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
