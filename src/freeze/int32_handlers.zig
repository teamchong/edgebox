//! Comptime Int32 Opcode Handler Generator
//!
//! Generates int32 opcode handlers at comptime from pattern definitions.
//! This generates pure int32 C code (no JSValue boxing).

const std = @import("std");
const opcodes = @import("opcodes.zig");
const Opcode = opcodes.Opcode;

/// Int32 handler pattern types
pub const Int32Pattern = enum {
    /// Push constant int32
    push_const_i32,
    /// Get argument (n0, n1, n2, n3)
    get_arg_i32,
    /// Put (store) argument (n0, n1, n2, n3) - pops value and stores to arg slot
    put_arg_i32,
    /// Binary arithmetic: pop 2, compute, push result
    binary_arith_i32,
    /// Binary comparison: pop 2, compare, push bool (0 or 1)
    binary_cmp_i32,
    /// Bitwise binary: pop 2, compute, push result
    bitwise_binary_i32,
    /// Unary: pop 1, compute, push result
    unary_i32,
    /// Increment/decrement: pop 1, +/- 1, push result
    inc_dec_i32,
    /// Stack operation
    stack_op_i32,
    /// Self-ref (for recursive calls)
    self_ref_i32,
    /// Recursive call
    call_self_i32,
    /// Return int32 value
    return_i32,
    /// Conditional jump
    if_false_i32,
    /// Unconditional jump
    goto_i32,
    /// Unsupported in int32 mode
    unsupported,
};

/// Int32 handler definition
pub const Int32Handler = struct {
    pattern: Int32Pattern,
    op: ?[]const u8 = null, // C operator like "+", "-", "&"
    value: ?i32 = null, // Constant value
    index: ?u8 = null, // Argument index
    is_inc: ?bool = null, // For inc/dec: true=inc, false=dec
};

/// Get int32 handler for an opcode
pub fn getInt32Handler(opcode: Opcode) Int32Handler {
    return switch (opcode) {
        // Push constants
        .push_0 => .{ .pattern = .push_const_i32, .value = 0 },
        .push_1 => .{ .pattern = .push_const_i32, .value = 1 },
        .push_2 => .{ .pattern = .push_const_i32, .value = 2 },
        .push_3 => .{ .pattern = .push_const_i32, .value = 3 },
        .push_i8, .push_i16, .push_i32 => .{ .pattern = .push_const_i32 }, // Value from operand

        // Get arguments
        .get_arg0 => .{ .pattern = .get_arg_i32, .index = 0 },
        .get_arg1 => .{ .pattern = .get_arg_i32, .index = 1 },
        .get_arg2 => .{ .pattern = .get_arg_i32, .index = 2 },
        .get_arg3 => .{ .pattern = .get_arg_i32, .index = 3 },

        // Put (store) arguments - used for tail recursion optimization
        .put_arg0 => .{ .pattern = .put_arg_i32, .index = 0 },
        .put_arg1 => .{ .pattern = .put_arg_i32, .index = 1 },
        .put_arg2 => .{ .pattern = .put_arg_i32, .index = 2 },
        .put_arg3 => .{ .pattern = .put_arg_i32, .index = 3 },

        // Binary arithmetic
        .add => .{ .pattern = .binary_arith_i32, .op = "+" },
        .sub => .{ .pattern = .binary_arith_i32, .op = "-" },
        .mul => .{ .pattern = .binary_arith_i32, .op = "*" },
        .div => .{ .pattern = .binary_arith_i32, .op = "/" },
        .mod => .{ .pattern = .binary_arith_i32, .op = "%" },

        // Bitwise
        .shl => .{ .pattern = .bitwise_binary_i32, .op = "<<" },
        .shr => .{ .pattern = .bitwise_binary_i32, .op = ">>" },
        .@"and" => .{ .pattern = .bitwise_binary_i32, .op = "&" },
        .@"or" => .{ .pattern = .bitwise_binary_i32, .op = "|" },
        .@"xor" => .{ .pattern = .bitwise_binary_i32, .op = "^" },

        // Comparisons
        .lt => .{ .pattern = .binary_cmp_i32, .op = "<" },
        .lte => .{ .pattern = .binary_cmp_i32, .op = "<=" },
        .gt => .{ .pattern = .binary_cmp_i32, .op = ">" },
        .gte => .{ .pattern = .binary_cmp_i32, .op = ">=" },
        .eq, .strict_eq => .{ .pattern = .binary_cmp_i32, .op = "==" },
        .neq, .strict_neq => .{ .pattern = .binary_cmp_i32, .op = "!=" },

        // Unary
        .neg => .{ .pattern = .unary_i32, .op = "-" },
        .not => .{ .pattern = .unary_i32, .op = "~" },

        // Inc/Dec
        .inc => .{ .pattern = .inc_dec_i32, .is_inc = true },
        .dec => .{ .pattern = .inc_dec_i32, .is_inc = false },

        // Stack ops
        .dup => .{ .pattern = .stack_op_i32, .op = "dup" },
        .drop => .{ .pattern = .stack_op_i32, .op = "drop" },

        // Self-ref (for recursive calls)
        .get_var_ref0, .get_var, .get_var_undef => .{ .pattern = .self_ref_i32 },

        // Calls
        .call1 => .{ .pattern = .call_self_i32 },

        // Return
        .@"return" => .{ .pattern = .return_i32 },
        .return_undef => .{ .pattern = .return_i32, .value = 0 },

        // Control flow
        .if_false, .if_false8 => .{ .pattern = .if_false_i32 },
        .goto, .goto8, .goto16 => .{ .pattern = .goto_i32 },

        // Everything else is unsupported in int32 mode
        else => .{ .pattern = .unsupported },
    };
}
