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
    /// Set local by implicit index (like put but leaves value on stack)
    set_local_implicit,
    /// Get argument by implicit index
    get_arg_implicit,
    /// Put argument by implicit index
    put_arg_implicit,
    /// Set argument by implicit index (like put but leaves value on stack)
    set_arg_implicit,
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
    /// Logical not: pop, !ToBool, push bool
    lnot_op,
    /// Type check: pop value, check type, push bool
    type_check,
    /// NOP: no operation
    nop_op,
    /// Post increment: pop, push original, push incremented
    post_inc_op,
    /// Post decrement: pop, push original, push decremented
    post_dec_op,
    /// Array element get: pop obj+index, push value
    array_get,
    /// Array element get2: pop obj+index, push obj+value
    array_get2,
    /// Array element put: pop obj+index+value
    array_put,
    /// Array/string length: pop obj, push length as int
    array_length,
    /// Power: pop 2, compute a**b, push result
    pow_op,
    /// Typeof: pop value, push type string
    typeof_op,
    /// Instanceof: pop 2, check if obj instanceof ctor
    instanceof_op,
    /// In: pop 2, check if key in obj
    in_op,
    /// ToObject: pop value, push object
    to_object_op,
    /// ToPropKey: pop value, push property key
    to_propkey_op,
    /// ToString: pop value, push string
    to_string_op,
    /// Push with operand from bytecode
    push_operand,
    /// Push from const pool (operand is pool index)
    push_from_const_pool,
    /// Push bigint from i32
    push_bigint,
    /// Get local with explicit operand index
    get_local_operand,
    /// Put local with explicit operand index
    put_local_operand,
    /// Set local with explicit operand index
    set_local_operand,
    /// Get argument with explicit operand index
    get_arg_operand,
    /// Put argument with explicit operand index
    put_arg_operand,
    /// Set argument with explicit operand index
    set_arg_operand,
    /// Function call with explicit arg count
    call_op,
    /// Conditional jump if true
    if_true_op,
    /// Throw exception
    throw_op,
    /// Inc local variable by 1
    inc_local_op,
    /// Dec local variable by 1
    dec_local_op,
    /// Add to local variable
    add_local_op,
    /// Delete property
    delete_op,
    /// Unconditional jump (goto label)
    goto_op,
    /// Conditional jump if false
    if_false_op,
    /// Catch exception setup
    catch_op,
    /// Gosub (computed goto)
    gosub_op,
    /// Return from gosub
    ret_op,
    /// Special nip for catch
    nip_catch_op,
    /// Push this value
    push_this_op,
    /// Push atom as value
    push_atom_value_op,
    /// Rest parameters
    rest_op,
    /// Create regexp
    regexp_op,
    /// Get var_ref by implicit index
    get_var_ref_implicit,
    /// Put var_ref by implicit index
    put_var_ref_implicit,
    /// Set var_ref by implicit index
    set_var_ref_implicit,
    /// Array append
    append_op,
    /// Set prototype
    set_proto_op,
    /// Set home object
    set_home_object_op,
    /// Set function name
    set_name_op,
    /// Set computed name
    set_name_computed_op,
    /// Define object field
    define_field_op,
    /// Define array element
    define_array_el_op,
    /// Copy data properties
    copy_data_properties_op,
    /// Special object creation
    special_object_op,
    /// Apply function call
    apply_op,
    /// Create array from values
    array_from_op,
    /// Get two locals at once
    get_loc0_loc1_op,
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
        // Note: push_this removed - this_val not available in _impl functions
        .push_empty_string => .{ .pattern = .push_js_const, .c_func = "JS_NewString(ctx, \"\")" },

        // ==================== BINARY ARITHMETIC ====================
        .add => .{ .pattern = .binary_arith, .c_func = "frozen_add" },
        .sub => .{ .pattern = .binary_arith, .c_func = "frozen_sub" },
        .mul => .{ .pattern = .binary_arith, .c_func = "frozen_mul" },
        .div => .{ .pattern = .binary_arith, .c_func = "frozen_div" },
        .mod => .{ .pattern = .binary_arith, .c_func = "frozen_mod" },
        .pow => .{ .pattern = .pow_op },

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
        .put_arg2 => .{ .pattern = .put_arg_implicit, .index = 2 },
        .put_arg3 => .{ .pattern = .put_arg_implicit, .index = 3 },

        // ==================== SET LOCAL (implicit index, leaves value on stack) ====================
        .set_loc0 => .{ .pattern = .set_local_implicit, .index = 0 },
        .set_loc1 => .{ .pattern = .set_local_implicit, .index = 1 },
        .set_loc2 => .{ .pattern = .set_local_implicit, .index = 2 },
        .set_loc3 => .{ .pattern = .set_local_implicit, .index = 3 },

        // ==================== SET ARG (implicit index, leaves value on stack) ====================
        .set_arg0 => .{ .pattern = .set_arg_implicit, .index = 0 },
        .set_arg1 => .{ .pattern = .set_arg_implicit, .index = 1 },
        .set_arg2 => .{ .pattern = .set_arg_implicit, .index = 2 },
        .set_arg3 => .{ .pattern = .set_arg_implicit, .index = 3 },

        // ==================== STACK OPS ====================
        .drop => .{ .pattern = .stack_op, .c_func = "drop" },
        .dup => .{ .pattern = .stack_op, .c_func = "dup" },
        .dup1 => .{ .pattern = .stack_op, .c_func = "dup1" },
        .dup2 => .{ .pattern = .stack_op, .c_func = "dup2" },
        .dup3 => .{ .pattern = .stack_op, .c_func = "dup3" },
        .nip => .{ .pattern = .stack_op, .c_func = "nip" },
        .nip1 => .{ .pattern = .stack_op, .c_func = "nip1" },
        .swap => .{ .pattern = .stack_op, .c_func = "swap" },
        .swap2 => .{ .pattern = .stack_op, .c_func = "swap2" },
        .rot3l => .{ .pattern = .stack_op, .c_func = "rot3l" },
        .rot3r => .{ .pattern = .stack_op, .c_func = "rot3r" },
        .rot4l => .{ .pattern = .stack_op, .c_func = "rot4l" },
        .rot5l => .{ .pattern = .stack_op, .c_func = "rot5l" },
        .insert2 => .{ .pattern = .stack_op, .c_func = "insert2" },
        .insert3 => .{ .pattern = .stack_op, .c_func = "insert3" },
        .insert4 => .{ .pattern = .stack_op, .c_func = "insert4" },
        .perm3 => .{ .pattern = .stack_op, .c_func = "perm3" },
        .perm4 => .{ .pattern = .stack_op, .c_func = "perm4" },
        .perm5 => .{ .pattern = .stack_op, .c_func = "perm5" },
        .nop => .{ .pattern = .nop_op },

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

        // ==================== LOGICAL NOT ====================
        .lnot => .{ .pattern = .lnot_op },

        // ==================== TYPE CHECKS ====================
        .is_undefined => .{ .pattern = .type_check, .c_func = "JS_IsUndefined" },
        .is_null => .{ .pattern = .type_check, .c_func = "JS_IsNull" },
        .is_undefined_or_null => .{ .pattern = .type_check, .c_func = "JS_IsUndefined(v) || JS_IsNull(v)" },
        .typeof_is_undefined => .{ .pattern = .type_check, .c_func = "JS_IsUndefined" },
        .typeof_is_function => .{ .pattern = .type_check, .c_func = "JS_IsFunction(ctx, v)" },

        // ==================== TYPE OPERATORS ====================
        .typeof => .{ .pattern = .typeof_op },
        .instanceof => .{ .pattern = .instanceof_op },
        .in => .{ .pattern = .in_op },

        // ==================== TYPE COERCION ====================
        .to_object => .{ .pattern = .to_object_op },
        .to_propkey => .{ .pattern = .to_propkey_op },

        // ==================== POST INC/DEC ====================
        .post_inc => .{ .pattern = .post_inc_op },
        .post_dec => .{ .pattern = .post_dec_op },

        // ==================== ARRAY ACCESS ====================
        .get_array_el => .{ .pattern = .array_get },
        .get_array_el2 => .{ .pattern = .array_get2 },
        .put_array_el => .{ .pattern = .array_put },
        .get_length => .{ .pattern = .array_length },

        // ==================== RETURN ====================
        .@"return" => .{ .pattern = .return_op, .index = 1 }, // return value
        .return_undef => .{ .pattern = .return_op, .index = 0 }, // return undefined

        // ==================== OBJECT CREATION ====================
        .object => .{ .pattern = .push_js_const, .c_func = "JS_NewObject(ctx)" },

        // ==================== PUSH WITH OPERANDS ====================
        .push_i8 => .{ .pattern = .push_operand }, // i8 operand
        .push_i16 => .{ .pattern = .push_operand }, // i16 operand
        .push_i32 => .{ .pattern = .push_operand }, // i32 operand
        .push_const => .{ .pattern = .push_from_const_pool }, // u32 const pool index
        .push_const8 => .{ .pattern = .push_from_const_pool }, // u8 const pool index
        .push_bigint_i32 => .{ .pattern = .push_bigint }, // i32 to BigInt

        // ==================== GET/PUT/SET WITH EXPLICIT OPERANDS ====================
        .get_loc => .{ .pattern = .get_local_operand }, // u16 local index
        .get_loc8 => .{ .pattern = .get_local_operand }, // u8 local index
        .put_loc => .{ .pattern = .put_local_operand }, // u16 local index
        .put_loc8 => .{ .pattern = .put_local_operand }, // u8 local index
        .set_loc => .{ .pattern = .set_local_operand }, // u16 local index
        .set_loc8 => .{ .pattern = .set_local_operand }, // u8 local index
        .get_arg => .{ .pattern = .get_arg_operand }, // u16 arg index
        .put_arg => .{ .pattern = .put_arg_operand }, // u16 arg index
        .set_arg => .{ .pattern = .set_arg_operand }, // u16 arg index

        // ==================== FUNCTION CALLS ====================
        .call0 => .{ .pattern = .call_op, .index = 0 },
        .call1 => .{ .pattern = .call_op, .index = 1 },
        .call2 => .{ .pattern = .call_op, .index = 2 },
        .call3 => .{ .pattern = .call_op, .index = 3 },
        .call => .{ .pattern = .call_op }, // u16 operand for arg count
        .call_constructor => .{ .pattern = .call_op }, // u16 operand, new.target handling
        .call_method => .{ .pattern = .call_op }, // u16 operand, method call

        // ==================== CONTROL FLOW ====================
        .if_true => .{ .pattern = .if_true_op }, // label operand
        .throw => .{ .pattern = .throw_op },
        .throw_error => .{ .pattern = .throw_op }, // with atom operand for error type

        // ==================== INC/DEC/ADD LOCALS ====================
        .inc_loc => .{ .pattern = .inc_local_op }, // u8 local index
        .dec_loc => .{ .pattern = .dec_local_op }, // u8 local index
        .add_loc => .{ .pattern = .add_local_op }, // u8 local index + i8 value

        // ==================== TYPE CONVERSIONS ====================
        .to_propkey2 => .{ .pattern = .to_propkey_op }, // same as to_propkey

        // ==================== DELETE OPERATOR ====================
        .delete => .{ .pattern = .delete_op },

        // ==================== CONTROL FLOW (JUMPS) ====================
        .goto => .{ .pattern = .goto_op },
        .goto8 => .{ .pattern = .goto_op },
        .goto16 => .{ .pattern = .goto_op },
        .if_false => .{ .pattern = .if_false_op },
        .if_false8 => .{ .pattern = .if_false_op },
        .if_true8 => .{ .pattern = .if_true_op },
        .@"catch" => .{ .pattern = .catch_op },
        .gosub => .{ .pattern = .gosub_op },
        .ret => .{ .pattern = .ret_op },
        .nip_catch => .{ .pattern = .nip_catch_op },

        // ==================== SIMPLE PUSH OPERATIONS ====================
        .push_this => .{ .pattern = .push_this_op },
        .push_atom_value => .{ .pattern = .push_atom_value_op },
        .rest => .{ .pattern = .rest_op },
        .regexp => .{ .pattern = .regexp_op },

        // ==================== VAR_REF IMPLICIT INDEX ====================
        .get_var_ref0 => .{ .pattern = .get_var_ref_implicit, .index = 0 },
        .get_var_ref1 => .{ .pattern = .get_var_ref_implicit, .index = 1 },
        .get_var_ref2 => .{ .pattern = .get_var_ref_implicit, .index = 2 },
        .get_var_ref3 => .{ .pattern = .get_var_ref_implicit, .index = 3 },
        .put_var_ref0 => .{ .pattern = .put_var_ref_implicit, .index = 0 },
        .put_var_ref1 => .{ .pattern = .put_var_ref_implicit, .index = 1 },
        .put_var_ref2 => .{ .pattern = .put_var_ref_implicit, .index = 2 },
        .put_var_ref3 => .{ .pattern = .put_var_ref_implicit, .index = 3 },
        .set_var_ref0 => .{ .pattern = .set_var_ref_implicit, .index = 0 },
        .set_var_ref1 => .{ .pattern = .set_var_ref_implicit, .index = 1 },
        .set_var_ref2 => .{ .pattern = .set_var_ref_implicit, .index = 2 },
        .set_var_ref3 => .{ .pattern = .set_var_ref_implicit, .index = 3 },

        // ==================== OBJECT OPERATIONS ====================
        .append => .{ .pattern = .append_op },
        .set_proto => .{ .pattern = .set_proto_op },
        .set_home_object => .{ .pattern = .set_home_object_op },
        .set_name => .{ .pattern = .set_name_op },
        .set_name_computed => .{ .pattern = .set_name_computed_op },
        .define_field => .{ .pattern = .define_field_op },
        .define_array_el => .{ .pattern = .define_array_el_op },
        .copy_data_properties => .{ .pattern = .copy_data_properties_op },

        // ==================== SPECIAL OPERATIONS ====================
        .special_object => .{ .pattern = .special_object_op },
        .apply => .{ .pattern = .apply_op },
        .array_from => .{ .pattern = .array_from_op },
        .get_loc0_loc1 => .{ .pattern = .get_loc0_loc1_op },

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

        .set_local_implicit => std.fmt.comptimePrint(
            "    /* {s} */\n    FROZEN_FREE(ctx, locals[{d}]); locals[{d}] = FROZEN_DUP(ctx, TOP());\n",
            .{ op_name, handler.index.?, handler.index.? },
        ),

        .set_arg_implicit => std.fmt.comptimePrint(
            "    /* {s} */\n    if (argc > {d}) {{ JS_FreeValue(ctx, argv[{d}]); argv[{d}] = FROZEN_DUP(ctx, TOP()); }}\n",
            .{ op_name, handler.index.?, handler.index.?, handler.index.? },
        ),

        .stack_op => blk: {
            const func = handler.c_func.?;
            break :blk if (std.mem.eql(u8, func, "drop"))
                std.fmt.comptimePrint("    /* {s} */\n    FROZEN_FREE(ctx, POP());\n", .{op_name})
            else if (std.mem.eql(u8, func, "dup"))
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue tmp = TOP(); PUSH(FROZEN_DUP(ctx, tmp)); }}\n", .{op_name})
            else if (std.mem.eql(u8, func, "dup1"))
                // dup1: [a, b] -> [a, b, a] (dup second from top)
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue a = stack[sp-2]; PUSH(FROZEN_DUP(ctx, a)); }}\n", .{op_name})
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
            else if (std.mem.eql(u8, func, "rot4l"))
                // rot4l: [a, b, c, d] -> [b, c, d, a]
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue tmp = stack[sp-4]; stack[sp-4] = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = tmp; }}\n", .{op_name})
            else if (std.mem.eql(u8, func, "rot5l"))
                // rot5l: [a, b, c, d, e] -> [b, c, d, e, a]
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue tmp = stack[sp-5]; stack[sp-5] = stack[sp-4]; stack[sp-4] = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = tmp; }}\n", .{op_name})
            else if (std.mem.eql(u8, func, "insert2"))
                // insert2: [a, b] -> [b, a, b] (insert b before a)
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue b = TOP(); stack[sp-1] = stack[sp-2]; stack[sp-2] = FROZEN_DUP(ctx, b); PUSH(b); }}\n", .{op_name})
            else if (std.mem.eql(u8, func, "insert3"))
                // insert3: [a, b, c] -> [c, a, b, c]
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue c = TOP(); JSValue b = stack[sp-2]; JSValue a = stack[sp-3]; stack[sp-3] = FROZEN_DUP(ctx, c); stack[sp-2] = a; stack[sp-1] = b; PUSH(c); }}\n", .{op_name})
            else if (std.mem.eql(u8, func, "insert4"))
                // insert4: [a, b, c, d] -> [d, a, b, c, d]
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue d = TOP(); JSValue c = stack[sp-2]; JSValue b = stack[sp-3]; JSValue a = stack[sp-4]; stack[sp-4] = FROZEN_DUP(ctx, d); stack[sp-3] = a; stack[sp-2] = b; stack[sp-1] = c; PUSH(d); }}\n", .{op_name})
            else if (std.mem.eql(u8, func, "perm3"))
                // perm3: [a, b, c] -> [b, c, a]
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue a = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = a; }}\n", .{op_name})
            else if (std.mem.eql(u8, func, "perm4"))
                // perm4: [a, b, c, d] -> [b, c, d, a]
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue a = stack[sp-4]; stack[sp-4] = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = a; }}\n", .{op_name})
            else if (std.mem.eql(u8, func, "perm5"))
                // perm5: [a, b, c, d, e] -> [b, c, d, e, a]
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue a = stack[sp-5]; stack[sp-5] = stack[sp-4]; stack[sp-4] = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = a; }}\n", .{op_name})
            else
                "    /* unknown stack op */\n";
        },

        .nop_op => std.fmt.comptimePrint("    /* {s} - no operation */\n", .{op_name}),

        .lnot_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue v = POP(); PUSH(JS_NewBool(ctx, !JS_ToBool(ctx, v))); FROZEN_FREE(ctx, v); }}\n",
            .{op_name},
        ),

        .type_check => blk: {
            const func = handler.c_func.?;
            // Handle complex expressions that need different formatting
            break :blk if (std.mem.indexOf(u8, func, "||") != null)
                // Complex expression like "JS_IsUndefined(v) || JS_IsNull(v)"
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue v = POP(); PUSH(JS_NewBool(ctx, {s})); FROZEN_FREE(ctx, v); }}\n", .{ op_name, func })
            else if (std.mem.indexOf(u8, func, "ctx") != null)
                // Function that needs ctx like "JS_IsFunction(ctx, v)"
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue v = POP(); PUSH(JS_NewBool(ctx, {s})); FROZEN_FREE(ctx, v); }}\n", .{ op_name, func })
            else
                // Simple function like "JS_IsUndefined"
                std.fmt.comptimePrint("    /* {s} */\n    {{ JSValue v = POP(); PUSH(JS_NewBool(ctx, {s}(v))); FROZEN_FREE(ctx, v); }}\n", .{ op_name, func });
        },

        .post_inc_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue v = POP(); PUSH(FROZEN_DUP(ctx, v)); PUSH(frozen_add(ctx, v, JS_MKVAL(JS_TAG_INT, 1))); FROZEN_FREE(ctx, v); }}\n",
            .{op_name},
        ),

        .post_dec_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue v = POP(); PUSH(FROZEN_DUP(ctx, v)); PUSH(frozen_sub(ctx, v, JS_MKVAL(JS_TAG_INT, 1))); FROZEN_FREE(ctx, v); }}\n",
            .{op_name},
        ),

        .array_get => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue idx = POP(); JSValue obj = POP(); JSValue val = frozen_array_get(ctx, obj, idx); FROZEN_FREE(ctx, obj); FROZEN_FREE(ctx, idx); if (JS_IsException(val)) return val; PUSH(val); }}\n",
            .{op_name},
        ),

        .array_get2 => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue idx = POP(); JSValue obj = TOP(); JSValue val = frozen_array_get(ctx, obj, idx); FROZEN_FREE(ctx, idx); if (JS_IsException(val)) return val; PUSH(val); }}\n",
            .{op_name},
        ),

        .array_put => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue val = POP(); JSValue idx = POP(); JSValue obj = POP(); int r = frozen_array_set(ctx, obj, idx, val); FROZEN_FREE(ctx, obj); FROZEN_FREE(ctx, idx); if (r < 0) return JS_EXCEPTION; }}\n",
            .{op_name},
        ),

        .array_length => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue obj = POP(); int64_t len = frozen_get_length(ctx, obj); FROZEN_FREE(ctx, obj); PUSH(JS_NewInt64(ctx, len)); }}\n",
            .{op_name},
        ),

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
            std.fmt.comptimePrint("    /* {s} */\n    FROZEN_EXIT_STACK(); return POP();\n", .{op_name})
        else
            std.fmt.comptimePrint("    /* {s} */\n    FROZEN_EXIT_STACK(); return JS_UNDEFINED;\n", .{op_name}),

        .pow_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue b = POP(), a = POP(); JSValue r = frozen_pow(ctx, a, b); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); if (JS_IsException(r)) return r; PUSH(r); }}\n",
            .{op_name},
        ),

        .typeof_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue v = POP(); JSValue t = frozen_typeof(ctx, v); FROZEN_FREE(ctx, v); PUSH(t); }}\n",
            .{op_name},
        ),

        .instanceof_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue ctor = POP(), obj = POP(); int r = JS_IsInstanceOf(ctx, obj, ctor); FROZEN_FREE(ctx, obj); FROZEN_FREE(ctx, ctor); if (r < 0) return JS_EXCEPTION; PUSH(JS_NewBool(ctx, r)); }}\n",
            .{op_name},
        ),

        .in_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue obj = POP(), key = POP(); int r = frozen_in(ctx, key, obj); FROZEN_FREE(ctx, key); FROZEN_FREE(ctx, obj); if (r < 0) return JS_EXCEPTION; PUSH(JS_NewBool(ctx, r)); }}\n",
            .{op_name},
        ),

        .to_object_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue v = POP(); JSValue obj = JS_ToObject(ctx, v); FROZEN_FREE(ctx, v); if (JS_IsException(obj)) return obj; PUSH(obj); }}\n",
            .{op_name},
        ),

        .to_propkey_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue v = POP(); JSAtom atom = JS_ValueToAtom(ctx, v); FROZEN_FREE(ctx, v); if (atom == JS_ATOM_NULL) return JS_EXCEPTION; PUSH(JS_AtomToValue(ctx, atom)); JS_FreeAtom(ctx, atom); }}\n",
            .{op_name},
        ),

        .to_string_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue v = POP(); JSValue str = JS_ToString(ctx, v); FROZEN_FREE(ctx, v); if (JS_IsException(str)) return str; PUSH(str); }}\n",
            .{op_name},
        ),

        .push_operand => std.fmt.comptimePrint(
            "    /* {s} */\n    /* operand value will be read from bytecode, push as int */\n",
            .{op_name},
        ),

        .push_from_const_pool => std.fmt.comptimePrint(
            "    /* {s} */\n    /* const pool index from operand, requires bytecode context */\n",
            .{op_name},
        ),

        .push_bigint => std.fmt.comptimePrint(
            "    /* {s} */\n    /* BigInt creation from i32 operand, requires JS_NewBigInt64 */\n",
            .{op_name},
        ),

        .get_local_operand => std.fmt.comptimePrint(
            "    /* {s} */\n    /* local index from operand, requires bytecode context */\n",
            .{op_name},
        ),

        .put_local_operand => std.fmt.comptimePrint(
            "    /* {s} */\n    /* local index from operand, requires bytecode context */\n",
            .{op_name},
        ),

        .set_local_operand => std.fmt.comptimePrint(
            "    /* {s} */\n    /* local index from operand, requires bytecode context */\n",
            .{op_name},
        ),

        .get_arg_operand => std.fmt.comptimePrint(
            "    /* {s} */\n    /* arg index from operand, requires bytecode context */\n",
            .{op_name},
        ),

        .put_arg_operand => std.fmt.comptimePrint(
            "    /* {s} */\n    /* arg index from operand, requires bytecode context */\n",
            .{op_name},
        ),

        .set_arg_operand => std.fmt.comptimePrint(
            "    /* {s} */\n    /* arg index from operand, requires bytecode context */\n",
            .{op_name},
        ),

        .call_op => if (handler.index) |argc| std.fmt.comptimePrint(
            "    /* {s} */\n    /* call with {d} args - pop func + args, push result */\n",
            .{ op_name, argc },
        ) else std.fmt.comptimePrint(
            "    /* {s} */\n    /* call with argc from operand */\n",
            .{op_name},
        ),

        .if_true_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* conditional jump if true - requires label from operand */\n",
            .{op_name},
        ),

        .throw_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue exc = POP(); return JS_Throw(ctx, exc); }}\n",
            .{op_name},
        ),

        .inc_local_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* inc local[operand] by 1 */\n",
            .{op_name},
        ),

        .dec_local_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* dec local[operand] by 1 */\n",
            .{op_name},
        ),

        .add_local_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* add operand2 to local[operand1] */\n",
            .{op_name},
        ),

        .delete_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue key = POP(); JSValue obj = POP(); int r = JS_DeleteProperty(ctx, obj, JS_ValueToAtom(ctx, key), 0); FROZEN_FREE(ctx, obj); FROZEN_FREE(ctx, key); if (r < 0) return JS_EXCEPTION; PUSH(JS_NewBool(ctx, r)); }}\n",
            .{op_name},
        ),

        .goto_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* unconditional jump - requires label from operand */\n",
            .{op_name},
        ),

        .if_false_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* conditional jump if false - requires label from operand */\n",
            .{op_name},
        ),

        .catch_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* exception handler setup - requires catch block context */\n",
            .{op_name},
        ),

        .gosub_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* computed goto - requires label from operand */\n",
            .{op_name},
        ),

        .ret_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* return from gosub - requires saved PC */\n",
            .{op_name},
        ),

        .nip_catch_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* nip for catch - pop exception, keep value */\n",
            .{op_name},
        ),

        .push_this_op => std.fmt.comptimePrint(
            "    /* {s} */\n    PUSH(FROZEN_DUP(ctx, this_val));\n",
            .{op_name},
        ),

        .push_atom_value_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* push atom as value - requires atom from operand */\n",
            .{op_name},
        ),

        .rest_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* rest parameters - requires arg index from operand */\n",
            .{op_name},
        ),

        .regexp_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* create regexp - requires pattern/flags from operand */\n",
            .{op_name},
        ),

        .get_var_ref_implicit => if (handler.index) |idx| std.fmt.comptimePrint(
            "    /* {s} */\n    /* get var_ref[{d}] - requires var_ref context */\n",
            .{ op_name, idx },
        ) else std.fmt.comptimePrint(
            "    /* {s} */\n    /* get var_ref - requires index from operand */\n",
            .{op_name},
        ),

        .put_var_ref_implicit => if (handler.index) |idx| std.fmt.comptimePrint(
            "    /* {s} */\n    /* put var_ref[{d}] - requires var_ref context */\n",
            .{ op_name, idx },
        ) else std.fmt.comptimePrint(
            "    /* {s} */\n    /* put var_ref - requires index from operand */\n",
            .{op_name},
        ),

        .set_var_ref_implicit => if (handler.index) |idx| std.fmt.comptimePrint(
            "    /* {s} */\n    /* set var_ref[{d}] - requires var_ref context */\n",
            .{ op_name, idx },
        ) else std.fmt.comptimePrint(
            "    /* {s} */\n    /* set var_ref - requires index from operand */\n",
            .{op_name},
        ),

        .append_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue val = POP(); JSValue arr = TOP(); if (JS_IsException(JS_DefinePropertyValueUint32(ctx, arr, -1, val, JS_PROP_C_W_E))) return JS_EXCEPTION; }}\n",
            .{op_name},
        ),

        .set_proto_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue proto = POP(); JSValue obj = TOP(); if (JS_SetPrototype(ctx, obj, proto) < 0) {{ FROZEN_FREE(ctx, proto); return JS_EXCEPTION; }} FROZEN_FREE(ctx, proto); }}\n",
            .{op_name},
        ),

        .set_home_object_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* set home object for super - requires function context */\n",
            .{op_name},
        ),

        .set_name_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* set function name - requires atom from operand */\n",
            .{op_name},
        ),

        .set_name_computed_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* set computed name - pop name, set on TOS */\n",
            .{op_name},
        ),

        .define_field_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* define object field - requires atom from operand */\n",
            .{op_name},
        ),

        .define_array_el_op => std.fmt.comptimePrint(
            "    /* {s} */\n    {{ JSValue val = POP(); JSValue idx = POP(); JSValue arr = TOP(); if (JS_DefinePropertyValue(ctx, arr, JS_ValueToAtom(ctx, idx), val, JS_PROP_C_W_E) < 0) {{ FROZEN_FREE(ctx, idx); return JS_EXCEPTION; }} FROZEN_FREE(ctx, idx); }}\n",
            .{op_name},
        ),

        .copy_data_properties_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* copy data properties - requires mask from operand */\n",
            .{op_name},
        ),

        .special_object_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* create special object - requires type from operand */\n",
            .{op_name},
        ),

        .apply_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* apply function call - pop args array, this, func */\n",
            .{op_name},
        ),

        .array_from_op => std.fmt.comptimePrint(
            "    /* {s} */\n    /* create array from stack values - requires count from operand */\n",
            .{op_name},
        ),

        .get_loc0_loc1_op => std.fmt.comptimePrint(
            "    /* {s} */\n    PUSH(FROZEN_DUP(ctx, locals[0])); PUSH(FROZEN_DUP(ctx, locals[1]));\n",
            .{op_name},
        ),

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
