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
    /// Tail recursive call (TCO via goto)
    tail_call_self_i32,
    /// Return int32 value
    return_i32,
    /// Conditional jump (jump if false)
    if_false_i32,
    /// Conditional jump (jump if true)
    if_true_i32,
    /// Unconditional jump
    goto_i32,
    /// Get local variable (loc0..loc3, loc, loc8)
    get_loc_i32,
    /// Put (store) local variable - pops value, stores to local slot
    put_loc_i32,
    /// Set local variable - stores to local slot, keeps value on stack
    set_loc_i32,
    /// Push boolean as i32 (true=1, false=0)
    push_bool_i32,
    /// Logical NOT: pop 1, push !val as i32 (0→1, nonzero→0)
    lnot_i32,
    /// Get local with TDZ check (same as get_loc in int32 context)
    get_loc_check_i32,
    /// Put local with const check (same as put_loc in int32 context)
    put_loc_check_i32,
    /// Set local uninitialized (set to 0 in int32 context)
    set_loc_uninitialized_i32,
    /// Swap top two stack values
    swap_i32,
    /// No-op
    nop_i32,
    /// Set argument (store to arg, keeps value on stack)
    set_arg_i32,
    /// Add to local: local[N] += pop()
    add_loc_i32,
    /// Increment local: local[N]++
    inc_loc_i32,
    /// Push constant from cpool (resolved at compile time)
    push_cpool_i32,
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
        .push_const8, .push_const => .{ .pattern = .push_cpool_i32 }, // Value from constant pool
        .push_minus1 => .{ .pattern = .push_const_i32, .value = -1 },
        .push_4 => .{ .pattern = .push_const_i32, .value = 4 },
        .push_5 => .{ .pattern = .push_const_i32, .value = 5 },
        .push_6 => .{ .pattern = .push_const_i32, .value = 6 },
        .push_7 => .{ .pattern = .push_const_i32, .value = 7 },

        // Push booleans as i32
        .push_true => .{ .pattern = .push_bool_i32, .value = 1 },
        .push_false => .{ .pattern = .push_bool_i32, .value = 0 },

        // Get arguments
        .get_arg0 => .{ .pattern = .get_arg_i32, .index = 0 },
        .get_arg1 => .{ .pattern = .get_arg_i32, .index = 1 },
        .get_arg2 => .{ .pattern = .get_arg_i32, .index = 2 },
        .get_arg3 => .{ .pattern = .get_arg_i32, .index = 3 },
        .get_arg => .{ .pattern = .get_arg_i32 }, // Index from operand

        // Put (store) arguments - used for tail recursion optimization
        .put_arg0 => .{ .pattern = .put_arg_i32, .index = 0 },
        .put_arg1 => .{ .pattern = .put_arg_i32, .index = 1 },
        .put_arg2 => .{ .pattern = .put_arg_i32, .index = 2 },
        .put_arg3 => .{ .pattern = .put_arg_i32, .index = 3 },

        // Binary arithmetic
        .add => .{ .pattern = .binary_arith_i32, .op = "+" },
        .sub => .{ .pattern = .binary_arith_i32, .op = "-" },
        .mul => .{ .pattern = .binary_arith_i32, .op = "*" },
        // div excluded from int32 tier: JS division can produce float (9/2 = 4.5)
        .div => .{ .pattern = .unsupported },
        .mod => .{ .pattern = .binary_arith_i32, .op = "%" },

        // Bitwise
        .shl => .{ .pattern = .bitwise_binary_i32, .op = "<<" },
        .sar => .{ .pattern = .bitwise_binary_i32, .op = ">>" },   // JS >> (arithmetic/signed shift right)
        .shr => .{ .pattern = .bitwise_binary_i32, .op = ">>>" },  // JS >>> (logical/unsigned shift right)
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

        // Logical NOT
        .lnot => .{ .pattern = .lnot_i32 },

        // Get local variables
        .get_loc0 => .{ .pattern = .get_loc_i32, .index = 0 },
        .get_loc1 => .{ .pattern = .get_loc_i32, .index = 1 },
        .get_loc2 => .{ .pattern = .get_loc_i32, .index = 2 },
        .get_loc3 => .{ .pattern = .get_loc_i32, .index = 3 },
        .get_loc, .get_loc8 => .{ .pattern = .get_loc_i32 }, // Index from operand

        // Put (store) local variables - pops value
        .put_loc0 => .{ .pattern = .put_loc_i32, .index = 0 },
        .put_loc1 => .{ .pattern = .put_loc_i32, .index = 1 },
        .put_loc2 => .{ .pattern = .put_loc_i32, .index = 2 },
        .put_loc3 => .{ .pattern = .put_loc_i32, .index = 3 },
        .put_loc, .put_loc8 => .{ .pattern = .put_loc_i32 }, // Index from operand

        // Set local variables - stores but keeps value on stack
        .set_loc0 => .{ .pattern = .set_loc_i32, .index = 0 },
        .set_loc1 => .{ .pattern = .set_loc_i32, .index = 1 },
        .set_loc2 => .{ .pattern = .set_loc_i32, .index = 2 },
        .set_loc3 => .{ .pattern = .set_loc_i32, .index = 3 },
        .set_loc, .set_loc8 => .{ .pattern = .set_loc_i32 }, // Index from operand

        // Inc/Dec
        .inc => .{ .pattern = .inc_dec_i32, .is_inc = true },
        .dec => .{ .pattern = .inc_dec_i32, .is_inc = false },

        // Stack ops
        .dup => .{ .pattern = .stack_op_i32, .op = "dup" },
        .drop => .{ .pattern = .stack_op_i32, .op = "drop" },

        // Self-ref (for recursive calls)
        .get_var_ref0, .get_var, .get_var_undef => .{ .pattern = .self_ref_i32 },

        // Calls (multi-arg support for gcd, ackermann, etc.)
        .call1 => .{ .pattern = .call_self_i32 },
        .call2 => .{ .pattern = .call_self_i32 },
        .call3 => .{ .pattern = .call_self_i32 },
        .call => .{ .pattern = .call_self_i32 }, // Generic call with argc from operand

        // Tail calls (TCO via goto)
        .tail_call => .{ .pattern = .tail_call_self_i32 },

        // Return
        .@"return" => .{ .pattern = .return_i32 },
        .return_undef => .{ .pattern = .return_i32, .value = 0 },

        // Get/put local with TDZ/const checks (same as regular get/put in int32 context)
        .get_loc_check => .{ .pattern = .get_loc_check_i32 },
        .put_loc_check => .{ .pattern = .put_loc_check_i32 },
        .put_loc_check_init => .{ .pattern = .put_loc_check_i32 },
        .set_loc_uninitialized => .{ .pattern = .set_loc_uninitialized_i32 },

        // Set argument (store to arg slot, keeps value on stack)
        .set_arg0 => .{ .pattern = .set_arg_i32, .index = 0 },
        .set_arg1 => .{ .pattern = .set_arg_i32, .index = 1 },
        .set_arg2 => .{ .pattern = .set_arg_i32, .index = 2 },
        .set_arg3 => .{ .pattern = .set_arg_i32, .index = 3 },
        .set_arg => .{ .pattern = .set_arg_i32 },

        // Add to local / increment local
        .add_loc => .{ .pattern = .add_loc_i32 },
        .inc_loc => .{ .pattern = .inc_loc_i32 },

        // Stack ops
        .swap => .{ .pattern = .swap_i32 },
        .nop => .{ .pattern = .nop_i32 },

        // Control flow
        .if_false, .if_false8 => .{ .pattern = .if_false_i32 },
        .if_true, .if_true8 => .{ .pattern = .if_true_i32 },
        .goto, .goto8, .goto16 => .{ .pattern = .goto_i32 },

        // Everything else is unsupported in int32 mode
        else => .{ .pattern = .unsupported },
    };
}
