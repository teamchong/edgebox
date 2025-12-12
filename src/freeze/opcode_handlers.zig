//! Comptime Opcode Handler Generator
//!
//! Generates opcode handlers at comptime from pattern definitions.
//! When QuickJS-NG updates, only the opcode enum needs to change -
//! handlers are auto-generated from patterns.

const std = @import("std");
const opcodes = @import("opcodes.zig");
const Opcode = opcodes.Opcode;

/// Handler pattern types
pub const HandlerPattern = enum {
    /// push_X: Push constant value
    push_const,
    /// Binary arithmetic: pop 2, apply op, push result
    binary_arith,
    /// Binary comparison: pop 2, compare, push bool
    binary_cmp,
    /// Unary op: pop 1, apply op, push result
    unary_op,
    /// Local variable access: get/set by index
    local_access,
    /// Argument access: get by index
    arg_access,
    /// Control flow: jumps, returns
    control_flow,
    /// Complex: requires runtime call
    complex,
};

/// Comptime handler definition
pub const Handler = struct {
    pattern: HandlerPattern,
    c_func: ?[]const u8 = null, // C helper function name (e.g., "frozen_add")
    c_template: ?[]const u8 = null, // Custom C code template
    value: ?i32 = null, // For push_const: the value to push
};

/// Map opcodes to handlers at comptime
pub fn getHandler(op: Opcode) Handler {
    return switch (op) {
        // Push constants - auto-generated pattern
        .push_minus1 => .{ .pattern = .push_const, .value = -1 },
        .push_0 => .{ .pattern = .push_const, .value = 0 },
        .push_1 => .{ .pattern = .push_const, .value = 1 },
        .push_2 => .{ .pattern = .push_const, .value = 2 },
        .push_3 => .{ .pattern = .push_const, .value = 3 },
        .push_4 => .{ .pattern = .push_const, .value = 4 },
        .push_5 => .{ .pattern = .push_const, .value = 5 },
        .push_6 => .{ .pattern = .push_const, .value = 6 },
        .push_7 => .{ .pattern = .push_const, .value = 7 },

        // Binary arithmetic - all use same pattern
        .add => .{ .pattern = .binary_arith, .c_func = "frozen_add" },
        .sub => .{ .pattern = .binary_arith, .c_func = "frozen_sub" },
        .mul => .{ .pattern = .binary_arith, .c_func = "frozen_mul" },
        .div => .{ .pattern = .binary_arith, .c_func = "frozen_div" },
        .mod => .{ .pattern = .binary_arith, .c_func = "frozen_mod" },

        // Binary comparison - all use same pattern
        .lt => .{ .pattern = .binary_cmp, .c_func = "frozen_lt" },
        .lte => .{ .pattern = .binary_cmp, .c_func = "frozen_lte" },
        .gt => .{ .pattern = .binary_cmp, .c_func = "frozen_gt" },
        .gte => .{ .pattern = .binary_cmp, .c_func = "frozen_gte" },
        .eq => .{ .pattern = .binary_cmp, .c_func = "frozen_eq" },
        .neq => .{ .pattern = .binary_cmp, .c_func = "frozen_neq" },

        // Default: complex handler needed
        else => .{ .pattern = .complex },
    };
}

/// Generate C code for a handler pattern at comptime
pub fn generateCode(comptime handler: Handler, comptime op_name: []const u8) []const u8 {
    return switch (handler.pattern) {
        .push_const => std.fmt.comptimePrint(
            "    /* {s} */\n    PUSH(JS_MKVAL(JS_TAG_INT, {d}));\n",
            .{ op_name, handler.value.? },
        ),

        .binary_arith => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue b = POP(), a = POP(); JSValue r = {s}(ctx, a, b); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); if (JS_IsException(r)) return r; PUSH(r); }}\n",
            .{ op_name, handler.c_func.? },
        ),

        .binary_cmp => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, {s}(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }}\n",
            .{ op_name, handler.c_func.? },
        ),

        else => "    /* complex handler - not auto-generated */\n",
    };
}

/// List of binary arithmetic opcodes for iteration
pub const binary_arith_ops = [_]struct { op: Opcode, name: []const u8, func: []const u8 }{
    .{ .op = .add, .name = "add", .func = "frozen_add" },
    .{ .op = .sub, .name = "sub", .func = "frozen_sub" },
    .{ .op = .mul, .name = "mul", .func = "frozen_mul" },
    .{ .op = .div, .name = "div", .func = "frozen_div" },
    .{ .op = .mod, .name = "mod", .func = "frozen_mod" },
};

/// List of binary comparison opcodes for iteration
pub const binary_cmp_ops = [_]struct { op: Opcode, name: []const u8, func: []const u8 }{
    .{ .op = .lt, .name = "lt", .func = "frozen_lt" },
    .{ .op = .lte, .name = "lte", .func = "frozen_lte" },
    .{ .op = .gt, .name = "gt", .func = "frozen_gt" },
    .{ .op = .gte, .name = "gte", .func = "frozen_gte" },
    .{ .op = .eq, .name = "eq", .func = "frozen_eq" },
    .{ .op = .neq, .name = "neq", .func = "frozen_neq" },
};

/// Check if opcode is handled by comptime patterns
pub fn isComptimeHandled(op: Opcode) bool {
    const handler = getHandler(op);
    return handler.pattern != .complex;
}

// Test comptime code generation
test "comptime handler generation" {
    const add_code = comptime generateCode(getHandler(.add), "add");
    try std.testing.expect(std.mem.indexOf(u8, add_code, "frozen_add") != null);

    const push_code = comptime generateCode(getHandler(.push_1), "push_1");
    try std.testing.expect(std.mem.indexOf(u8, push_code, "JS_TAG_INT, 1") != null);
}
