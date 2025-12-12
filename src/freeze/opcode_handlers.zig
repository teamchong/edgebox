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
    /// push_X: Push constant int value
    push_const,
    /// push_true/false/null/undefined: Push JS constant
    push_js_const,
    /// Binary arithmetic: pop 2, apply op, push result
    binary_arith,
    /// Binary comparison: pop 2, compare, push bool
    binary_cmp,
    /// Bitwise binary: pop 2, apply bitwise op, push result
    bitwise_binary,
    /// Unary op: pop 1, apply op, push result
    unary_op,
    /// Get local by implicit index
    get_local_implicit,
    /// Put local by implicit index
    put_local_implicit,
    /// Get argument by implicit index
    get_arg_implicit,
    /// Put argument by implicit index
    put_arg_implicit,
    /// Stack operations
    stack_op,
    /// Tail call: return result of call directly (enables TCO)
    tail_call,
    /// Property get: pop object, push obj[atom] (atom from operand)
    prop_get,
    /// Property get2: pop object, push obj + obj[atom]
    prop_get2,
    /// Property put: pop obj + value, set obj[atom] = value
    prop_put,
    /// Unary bitwise: pop 1, apply bitwise op, push result
    bitwise_unary,
    /// Unary increment: pop, add 1, push
    inc_op,
    /// Unary decrement: pop, sub 1, push
    dec_op,
    /// Unary negate: pop, negate, push
    neg_op,
    /// Unary plus: no-op (ToNumber)
    plus_op,
    /// Return: pop value and return it (or return undefined)
    return_op,
    /// Complex: requires runtime-specific handling
    complex,
};

/// Comptime handler definition
pub const Handler = struct {
    pattern: HandlerPattern,
    c_func: ?[]const u8 = null,
    value: ?i32 = null,
    index: ?u8 = null,
};

/// Map opcodes to handlers at comptime
pub fn getHandler(op: Opcode) Handler {
    return switch (op) {
        // ==================== PUSH CONSTANTS ====================
        .push_minus1 => .{ .pattern = .push_const, .value = -1 },
        .push_0 => .{ .pattern = .push_const, .value = 0 },
        .push_1 => .{ .pattern = .push_const, .value = 1 },
        .push_2 => .{ .pattern = .push_const, .value = 2 },
        .push_3 => .{ .pattern = .push_const, .value = 3 },
        .push_4 => .{ .pattern = .push_const, .value = 4 },
        .push_5 => .{ .pattern = .push_const, .value = 5 },
        .push_6 => .{ .pattern = .push_const, .value = 6 },
        .push_7 => .{ .pattern = .push_const, .value = 7 },

        // ==================== PUSH JS CONSTANTS ====================
        .push_true => .{ .pattern = .push_js_const, .c_func = "JS_TRUE" },
        .push_false => .{ .pattern = .push_js_const, .c_func = "JS_FALSE" },
        .null => .{ .pattern = .push_js_const, .c_func = "JS_NULL" },
        .undefined => .{ .pattern = .push_js_const, .c_func = "JS_UNDEFINED" },

        // ==================== BINARY ARITHMETIC ====================
        .add => .{ .pattern = .binary_arith, .c_func = "frozen_add" },
        .sub => .{ .pattern = .binary_arith, .c_func = "frozen_sub" },
        .mul => .{ .pattern = .binary_arith, .c_func = "frozen_mul" },
        .div => .{ .pattern = .binary_arith, .c_func = "frozen_div" },
        .mod => .{ .pattern = .binary_arith, .c_func = "frozen_mod" },

        // ==================== BINARY COMPARISON ====================
        .lt => .{ .pattern = .binary_cmp, .c_func = "frozen_lt" },
        .lte => .{ .pattern = .binary_cmp, .c_func = "frozen_lte" },
        .gt => .{ .pattern = .binary_cmp, .c_func = "frozen_gt" },
        .gte => .{ .pattern = .binary_cmp, .c_func = "frozen_gte" },
        .eq => .{ .pattern = .binary_cmp, .c_func = "frozen_eq" },
        .neq => .{ .pattern = .binary_cmp, .c_func = "frozen_neq" },
        .strict_eq => .{ .pattern = .binary_cmp, .c_func = "frozen_eq" },
        .strict_neq => .{ .pattern = .binary_cmp, .c_func = "frozen_neq" },

        // ==================== BITWISE BINARY ====================
        .@"and" => .{ .pattern = .bitwise_binary, .c_func = "frozen_and" },
        .@"or" => .{ .pattern = .bitwise_binary, .c_func = "frozen_or" },
        .xor => .{ .pattern = .bitwise_binary, .c_func = "frozen_xor" },
        .shl => .{ .pattern = .bitwise_binary, .c_func = "frozen_shl" },
        .sar => .{ .pattern = .bitwise_binary, .c_func = "frozen_sar" },
        .shr => .{ .pattern = .bitwise_binary, .c_func = "frozen_shr" },

        // ==================== GET LOCAL (implicit index) ====================
        .get_loc0 => .{ .pattern = .get_local_implicit, .index = 0 },
        .get_loc1 => .{ .pattern = .get_local_implicit, .index = 1 },
        .get_loc2 => .{ .pattern = .get_local_implicit, .index = 2 },
        .get_loc3 => .{ .pattern = .get_local_implicit, .index = 3 },

        // ==================== PUT LOCAL (implicit index) ====================
        .put_loc0 => .{ .pattern = .put_local_implicit, .index = 0 },
        .put_loc1 => .{ .pattern = .put_local_implicit, .index = 1 },
        .put_loc2 => .{ .pattern = .put_local_implicit, .index = 2 },
        .put_loc3 => .{ .pattern = .put_local_implicit, .index = 3 },

        // ==================== GET ARG (implicit index) ====================
        .get_arg0 => .{ .pattern = .get_arg_implicit, .index = 0 },
        .get_arg1 => .{ .pattern = .get_arg_implicit, .index = 1 },
        .get_arg2 => .{ .pattern = .get_arg_implicit, .index = 2 },
        .get_arg3 => .{ .pattern = .get_arg_implicit, .index = 3 },

        // ==================== PUT ARG (implicit index) ====================
        .put_arg0 => .{ .pattern = .put_arg_implicit, .index = 0 },
        .put_arg1 => .{ .pattern = .put_arg_implicit, .index = 1 },

        // ==================== STACK OPS ====================
        .drop => .{ .pattern = .stack_op, .c_func = "drop" },
        .dup => .{ .pattern = .stack_op, .c_func = "dup" },
        .dup2 => .{ .pattern = .stack_op, .c_func = "dup2" },
        .dup3 => .{ .pattern = .stack_op, .c_func = "dup3" },
        .nip => .{ .pattern = .stack_op, .c_func = "nip" },
        .nip1 => .{ .pattern = .stack_op, .c_func = "nip1" },
        .swap => .{ .pattern = .stack_op, .c_func = "swap" },
        .swap2 => .{ .pattern = .stack_op, .c_func = "swap2" },
        .rot3l => .{ .pattern = .stack_op, .c_func = "rot3l" },
        .rot3r => .{ .pattern = .stack_op, .c_func = "rot3r" },

        // ==================== TAIL CALL ====================
        .tail_call => .{ .pattern = .tail_call, .index = 1 }, // npop from operand
        .tail_call_method => .{ .pattern = .tail_call, .index = 2 }, // npop from operand

        // ==================== PROPERTY ACCESS ====================
        .get_field => .{ .pattern = .prop_get },
        .get_field2 => .{ .pattern = .prop_get2 },
        .put_field => .{ .pattern = .prop_put },

        // ==================== BITWISE UNARY ====================
        .not => .{ .pattern = .bitwise_unary, .c_func = "frozen_not" },

        // ==================== UNARY INCREMENT/DECREMENT ====================
        .inc => .{ .pattern = .inc_op },
        .dec => .{ .pattern = .dec_op },

        // ==================== UNARY NEG/PLUS ====================
        .neg => .{ .pattern = .neg_op },
        .plus => .{ .pattern = .plus_op },

        // ==================== RETURN ====================
        .@"return" => .{ .pattern = .return_op, .index = 1 }, // return value
        .return_undef => .{ .pattern = .return_op, .index = 0 }, // return undefined

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

        .push_js_const => std.fmt.comptimePrint(
            "    /* {s} */\n    PUSH({s});\n",
            .{ op_name, handler.c_func.? },
        ),

        .binary_arith => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue b = POP(), a = POP(); JSValue r = {s}(ctx, a, b); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); if (JS_IsException(r)) return r; PUSH(r); }}\n",
            .{ op_name, handler.c_func.? },
        ),

        .binary_cmp => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, {s}(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }}\n",
            .{ op_name, handler.c_func.? },
        ),

        .bitwise_binary => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue b = POP(), a = POP(); PUSH({s}(ctx, a, b)); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }}\n",
            .{ op_name, handler.c_func.? },
        ),

        .get_local_implicit => std.fmt.comptimePrint(
            "    /* {s} */\n    PUSH(FROZEN_DUP(ctx, locals[{d}]));\n",
            .{ op_name, handler.index.? },
        ),

        .put_local_implicit => std.fmt.comptimePrint(
            "    /* {s} */\n    FROZEN_FREE(ctx, locals[{d}]); locals[{d}] = POP();\n",
            .{ op_name, handler.index.?, handler.index.? },
        ),

        .get_arg_implicit => std.fmt.comptimePrint(
            "    /* {s} */\n    PUSH(argc > {d} ? FROZEN_DUP(ctx, argv[{d}]) : JS_UNDEFINED);\n",
            .{ op_name, handler.index.?, handler.index.? },
        ),

        .put_arg_implicit => std.fmt.comptimePrint(
            "    /* {s} */\n    if (argc > {d}) {{ JS_FreeValue(ctx, argv[{d}]); argv[{d}] = POP(); }}\n",
            .{ op_name, handler.index.?, handler.index.?, handler.index.? },
        ),

        .stack_op => blk: {
            const func = handler.c_func.?;
            break :blk if (std.mem.eql(u8, func, "drop"))
                std.fmt.comptimePrint("    /* {s} */\n    FROZEN_FREE(ctx, POP());\n", .{op_name})
            else if (std.mem.eql(u8, func, "dup"))
                std.fmt.comptimePrint("    /* {s} */\n    PUSH(FROZEN_DUP(ctx, TOP()));\n", .{op_name})
            else if (std.mem.eql(u8, func, "dup2"))
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue a = stack[sp-2], b = stack[sp-1]; PUSH(FROZEN_DUP(ctx, a)); PUSH(FROZEN_DUP(ctx, b)); }}\n", .{op_name})
            else if (std.mem.eql(u8, func, "dup3"))
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue a = stack[sp-3], b = stack[sp-2], c = stack[sp-1]; PUSH(FROZEN_DUP(ctx, a)); PUSH(FROZEN_DUP(ctx, b)); PUSH(FROZEN_DUP(ctx, c)); }}\n", .{op_name})
            else if (std.mem.eql(u8, func, "nip"))
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue top = POP(); FROZEN_FREE(ctx, POP()); PUSH(top); }}\n", .{op_name})
            else if (std.mem.eql(u8, func, "nip1"))
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue top = POP(); JSValue s1 = POP(); FROZEN_FREE(ctx, POP()); PUSH(s1); PUSH(top); }}\n", .{op_name})
            else if (std.mem.eql(u8, func, "swap"))
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue tmp = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = tmp; }}\n", .{op_name})
            else if (std.mem.eql(u8, func, "swap2"))
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue t1 = stack[sp-1], t2 = stack[sp-2]; stack[sp-1] = stack[sp-3]; stack[sp-2] = stack[sp-4]; stack[sp-3] = t1; stack[sp-4] = t2; }}\n", .{op_name})
            else if (std.mem.eql(u8, func, "rot3l"))
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue tmp = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = tmp; }}\n", .{op_name})
            else if (std.mem.eql(u8, func, "rot3r"))
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue tmp = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = stack[sp-3]; stack[sp-3] = tmp; }}\n", .{op_name})
            else
                "    /* unknown stack op */\n";
        },

        // Tail call: for self-recursive, codegen will convert to goto
        // This is the fallback for non-self-recursive
        .tail_call => std.fmt.comptimePrint(
            "    /* {s} - tail call (TCO) */\n    {{ JSValue arg = POP(); JSValue func = POP(); return JS_Call(ctx, func, JS_UNDEFINED, 1, &arg); }}\n",
            .{op_name},
        ),

        .bitwise_unary => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue a = POP(); PUSH({s}(ctx, a)); }}\n",
            .{ op_name, handler.c_func.? },
        ),

        .inc_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue a = POP(); PUSH(frozen_add(ctx, a, JS_MKVAL(JS_TAG_INT, 1))); }}\n",
            .{op_name},
        ),

        .dec_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue a = POP(); PUSH(frozen_sub(ctx, a, JS_MKVAL(JS_TAG_INT, 1))); }}\n",
            .{op_name},
        ),

        .neg_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue a = POP(); PUSH(frozen_neg(ctx, a)); }}\n",
            .{op_name},
        ),

        .plus_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* unary plus - ToNumber, keep value */\n",
            .{op_name},
        ),

        .return_op => if (handler.index.? == 1)
            std.fmt.comptimePrint("    /* {s} */\n    return POP();\n", .{op_name})
        else
            std.fmt.comptimePrint("    /* {s} */\n    return JS_UNDEFINED;\n", .{op_name}),

        .complex => "    /* complex handler - not auto-generated */\n",
        else => "    /* unhandled pattern */\n",
    };
}

/// Check if opcode is handled by comptime patterns
pub fn isComptimeHandled(op: Opcode) bool {
    const handler = getHandler(op);
    return handler.pattern != .complex;
}

/// Count of comptime-handled opcodes
pub fn countComptimeHandled() comptime_int {
    var count: comptime_int = 0;
    for (std.enums.values(Opcode)) |op| {
        if (isComptimeHandled(op)) count += 1;
    }
    return count;
}

// Test comptime code generation
test "comptime handler generation" {
    const add_code = comptime generateCode(getHandler(.add), "add");
    try std.testing.expect(std.mem.indexOf(u8, add_code, "frozen_add") != null);

    const push_code = comptime generateCode(getHandler(.push_1), "push_1");
    try std.testing.expect(std.mem.indexOf(u8, push_code, "JS_TAG_INT, 1") != null);

    const get_loc_code = comptime generateCode(getHandler(.get_loc0), "get_loc0");
    try std.testing.expect(std.mem.indexOf(u8, get_loc_code, "locals[0]") != null);

    const drop_code = comptime generateCode(getHandler(.drop), "drop");
    try std.testing.expect(std.mem.indexOf(u8, drop_code, "POP()") != null);
}

test "comptime coverage count" {
    const count = comptime countComptimeHandled();
    // Should have at least 40 comptime-handled opcodes
    try std.testing.expect(count >= 40);
}
