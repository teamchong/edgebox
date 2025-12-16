//! Zig Opcode Handlers
//!
//! Generates Zig code for opcode execution at comptime.
//! Each handler emits inline Zig code for a specific opcode pattern.
//!
//! Key differences from C handlers:
//!   - Uses zig_jsvalue.zig types (JSValue, JSContext)
//!   - SINT fast paths via inline methods (addSint, subSint, etc.)
//!   - No FROZEN_DUP/FROZEN_FREE macros - use dupValue/freeValue helpers
//!   - Type-safe at compile time

const std = @import("std");
const opcodes = @import("opcodes.zig");
const jsvalue = @import("zig_jsvalue.zig");

const Opcode = opcodes.Opcode;
const JSValue = jsvalue.JSValue;

/// Handler pattern types (same as C version)
pub const HandlerPattern = enum {
    push_const, // Push constant int value
    push_js_const, // Push JS constant (true/false/null/undefined)
    binary_arith, // Binary arithmetic with SINT fast path
    binary_cmp, // Binary comparison with SINT fast path
    bitwise_binary, // Bitwise binary ops
    unary_op, // Unary operations
    get_local_implicit, // Get local by implicit index
    put_local_implicit, // Put local by implicit index
    set_local_implicit, // Set local (leave value on stack)
    get_arg_implicit, // Get argument by implicit index
    put_arg_implicit, // Put argument by implicit index
    set_arg_implicit, // Set argument (leave value on stack)
    stack_op, // Stack manipulation
    tail_call, // Tail call optimization
    prop_get, // Property get
    prop_get2, // Property get (keep object)
    prop_put, // Property put
    bitwise_unary, // Bitwise unary (NOT)
    inc_op, // Increment
    dec_op, // Decrement
    neg_op, // Negate
    plus_op, // Unary plus (no-op)
    return_op, // Return
    lnot_op, // Logical NOT
    type_check, // Type checks
    nop_op, // No operation
    post_inc_op, // Post increment
    post_dec_op, // Post decrement
    array_get, // Array element get
    array_get2, // Array element get (keep object)
    array_put, // Array element put
    array_length, // Array/string length
    pow_op, // Power
    typeof_op, // Typeof
    instanceof_op, // Instanceof
    in_op, // In operator
    to_object_op, // ToObject
    to_propkey_op, // ToPropKey
    complex, // Requires special handling
};

/// SINT arithmetic function names
pub const SintOp = enum {
    add,
    sub,
    mul,
    div,
    mod,
    @"and",
    @"or",
    xor,
    shl,
    shr,
    ushr,

    pub fn zigMethod(self: SintOp) []const u8 {
        return switch (self) {
            .add => "addSint",
            .sub => "subSint",
            .mul => "mulSint",
            .div => "divSint",
            .mod => "modSint",
            .@"and" => "andSint",
            .@"or" => "orSint",
            .xor => "xorSint",
            .shl => "shlSint",
            .shr => "shrSint",
            .ushr => "ushrSint",
        };
    }

    pub fn fallbackFn(self: SintOp) []const u8 {
        return switch (self) {
            .add => "JS_Add",
            .sub => "JS_Sub",
            .mul => "JS_Mul",
            .div => "JS_Div",
            else => "JS_UNDEFINED", // TODO: Add more fallbacks
        };
    }
};

/// SINT comparison function names
pub const SintCmp = enum {
    lt,
    le,
    gt,
    ge,
    eq,
    ne,

    pub fn zigMethod(self: SintCmp) []const u8 {
        return switch (self) {
            .lt => "ltSint",
            .le => "leSint",
            .gt => "gtSint",
            .ge => "geSint",
            .eq => "eqSint",
            .ne => "neSint",
        };
    }
};

/// Handler definition
pub const Handler = struct {
    pattern: HandlerPattern,
    sint_op: ?SintOp = null, // For binary arithmetic
    sint_cmp: ?SintCmp = null, // For comparisons
    value: ?i32 = null, // For push_const
    index: ?u8 = null, // For get_loc, etc.
    js_const: ?[]const u8 = null, // For push_js_const
    stack_op_name: ?[]const u8 = null, // For stack operations
    returns_value: bool = true, // For return ops
};

/// Get handler for opcode
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
        .push_true => .{ .pattern = .push_js_const, .js_const = "initBool(true)" },
        .push_false => .{ .pattern = .push_js_const, .js_const = "initBool(false)" },
        .null => .{ .pattern = .push_js_const, .js_const = "initNull()" },
        .undefined => .{ .pattern = .push_js_const, .js_const = "initUndefined()" },

        // ==================== BINARY ARITHMETIC ====================
        .add => .{ .pattern = .binary_arith, .sint_op = .add },
        .sub => .{ .pattern = .binary_arith, .sint_op = .sub },
        .mul => .{ .pattern = .binary_arith, .sint_op = .mul },
        .div => .{ .pattern = .binary_arith, .sint_op = .div },
        .mod => .{ .pattern = .binary_arith, .sint_op = .mod },
        .pow => .{ .pattern = .pow_op },

        // ==================== BINARY COMPARISON ====================
        .lt => .{ .pattern = .binary_cmp, .sint_cmp = .lt },
        .lte => .{ .pattern = .binary_cmp, .sint_cmp = .le },
        .gt => .{ .pattern = .binary_cmp, .sint_cmp = .gt },
        .gte => .{ .pattern = .binary_cmp, .sint_cmp = .ge },
        .eq => .{ .pattern = .binary_cmp, .sint_cmp = .eq },
        .neq => .{ .pattern = .binary_cmp, .sint_cmp = .ne },
        .strict_eq => .{ .pattern = .binary_cmp, .sint_cmp = .eq },
        .strict_neq => .{ .pattern = .binary_cmp, .sint_cmp = .ne },

        // ==================== BITWISE BINARY ====================
        .@"and" => .{ .pattern = .bitwise_binary, .sint_op = .@"and" },
        .@"or" => .{ .pattern = .bitwise_binary, .sint_op = .@"or" },
        .xor => .{ .pattern = .bitwise_binary, .sint_op = .xor },
        .shl => .{ .pattern = .bitwise_binary, .sint_op = .shl },
        .sar => .{ .pattern = .bitwise_binary, .sint_op = .shr },
        .shr => .{ .pattern = .bitwise_binary, .sint_op = .ushr },

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

        // ==================== SET LOCAL (implicit index) ====================
        .set_loc0 => .{ .pattern = .set_local_implicit, .index = 0 },
        .set_loc1 => .{ .pattern = .set_local_implicit, .index = 1 },
        .set_loc2 => .{ .pattern = .set_local_implicit, .index = 2 },
        .set_loc3 => .{ .pattern = .set_local_implicit, .index = 3 },

        // ==================== SET ARG (implicit index) ====================
        .set_arg0 => .{ .pattern = .set_arg_implicit, .index = 0 },
        .set_arg1 => .{ .pattern = .set_arg_implicit, .index = 1 },
        .set_arg2 => .{ .pattern = .set_arg_implicit, .index = 2 },
        .set_arg3 => .{ .pattern = .set_arg_implicit, .index = 3 },

        // ==================== STACK OPS ====================
        .drop => .{ .pattern = .stack_op, .stack_op_name = "drop" },
        .dup => .{ .pattern = .stack_op, .stack_op_name = "dup" },
        .dup1 => .{ .pattern = .stack_op, .stack_op_name = "dup1" },
        .dup2 => .{ .pattern = .stack_op, .stack_op_name = "dup2" },
        .nip => .{ .pattern = .stack_op, .stack_op_name = "nip" },
        .nip1 => .{ .pattern = .stack_op, .stack_op_name = "nip1" },
        .swap => .{ .pattern = .stack_op, .stack_op_name = "swap" },
        .rot3l => .{ .pattern = .stack_op, .stack_op_name = "rot3l" },
        .rot3r => .{ .pattern = .stack_op, .stack_op_name = "rot3r" },
        .nop => .{ .pattern = .nop_op },

        // ==================== TAIL CALL ====================
        .tail_call => .{ .pattern = .tail_call },

        // ==================== PROPERTY ACCESS ====================
        .get_field => .{ .pattern = .prop_get },
        .get_field2 => .{ .pattern = .prop_get2 },
        .put_field => .{ .pattern = .prop_put },

        // ==================== BITWISE UNARY ====================
        .not => .{ .pattern = .bitwise_unary },

        // ==================== UNARY INC/DEC ====================
        .inc => .{ .pattern = .inc_op },
        .dec => .{ .pattern = .dec_op },

        // ==================== UNARY NEG/PLUS ====================
        .neg => .{ .pattern = .neg_op },
        .plus => .{ .pattern = .plus_op },

        // ==================== LOGICAL NOT ====================
        .lnot => .{ .pattern = .lnot_op },

        // ==================== TYPE CHECKS ====================
        .is_undefined => .{ .pattern = .type_check, .js_const = "isUndefined" },
        .is_null => .{ .pattern = .type_check, .js_const = "isNull" },

        // ==================== POST INC/DEC ====================
        .post_inc => .{ .pattern = .post_inc_op },
        .post_dec => .{ .pattern = .post_dec_op },

        // ==================== ARRAY ACCESS ====================
        .get_array_el => .{ .pattern = .array_get },
        .get_array_el2 => .{ .pattern = .array_get2 },
        .put_array_el => .{ .pattern = .array_put },
        .get_length => .{ .pattern = .array_length },

        // ==================== RETURN ====================
        .@"return" => .{ .pattern = .return_op, .returns_value = true },
        .return_undef => .{ .pattern = .return_op, .returns_value = false },

        // Default: complex handler needed
        else => .{ .pattern = .complex },
    };
}

/// Generate Zig code for a handler pattern
pub fn generateZigCode(comptime handler: Handler, comptime op_name: []const u8) []const u8 {
    return switch (handler.pattern) {
        .push_const => std.fmt.comptimePrint(
            \\// {s}
            \\stack[sp] = JSValue.initInt({d});
            \\sp += 1;
            \\
        , .{ op_name, handler.value.? }),

        .push_js_const => std.fmt.comptimePrint(
            \\// {s}
            \\stack[sp] = JSValue.{s};
            \\sp += 1;
            \\
        , .{ op_name, handler.js_const.? }),

        .binary_arith => blk: {
            const sint_method = handler.sint_op.?.zigMethod();
            const fallback = handler.sint_op.?.fallbackFn();
            break :blk std.fmt.comptimePrint(
                \\// {s} (SINT fast path)
                \\{{
                \\    sp -= 1;
                \\    const b = stack[sp];
                \\    const a = stack[sp - 1];
                \\    if (JSValue.{s}(a, b)) |result| {{
                \\        stack[sp - 1] = result;
                \\    }} else {{
                \\        // Fallback to runtime
                \\        stack[sp - 1] = {s}(ctx, a, b);
                \\        freeValue(ctx, a);
                \\        freeValue(ctx, b);
                \\    }}
                \\}}
                \\
            , .{ op_name, sint_method, fallback });
        },

        .binary_cmp => blk: {
            const sint_method = handler.sint_cmp.?.zigMethod();
            break :blk std.fmt.comptimePrint(
                \\// {s} (SINT fast path)
                \\{{
                \\    sp -= 1;
                \\    const b = stack[sp];
                \\    const a = stack[sp - 1];
                \\    if (JSValue.{s}(a, b)) |result| {{
                \\        stack[sp - 1] = result;
                \\    }} else {{
                \\        // Fallback to runtime comparison
                \\        const cmp = JS_LT(ctx, a, b);
                \\        stack[sp - 1] = JSValue.initBool(cmp != 0);
                \\        freeValue(ctx, a);
                \\        freeValue(ctx, b);
                \\    }}
                \\}}
                \\
            , .{ op_name, sint_method });
        },

        .bitwise_binary => blk: {
            const sint_method = handler.sint_op.?.zigMethod();
            break :blk std.fmt.comptimePrint(
                \\// {s} (SINT fast path)
                \\{{
                \\    sp -= 1;
                \\    const b = stack[sp];
                \\    const a = stack[sp - 1];
                \\    if (JSValue.{s}(a, b)) |result| {{
                \\        stack[sp - 1] = result;
                \\    }} else {{
                \\        stack[sp - 1] = JSValue.initUndefined();
                \\    }}
                \\}}
                \\
            , .{ op_name, sint_method });
        },

        .get_local_implicit => std.fmt.comptimePrint(
            \\// {s}
            \\stack[sp] = dupValue(ctx, locals[{d}]);
            \\sp += 1;
            \\
        , .{ op_name, handler.index.? }),

        .put_local_implicit => std.fmt.comptimePrint(
            \\// {s}
            \\sp -= 1;
            \\freeValue(ctx, locals[{d}]);
            \\locals[{d}] = stack[sp];
            \\
        , .{ op_name, handler.index.?, handler.index.? }),

        .set_local_implicit => std.fmt.comptimePrint(
            \\// {s}
            \\freeValue(ctx, locals[{d}]);
            \\locals[{d}] = dupValue(ctx, stack[sp - 1]);
            \\
        , .{ op_name, handler.index.?, handler.index.? }),

        .get_arg_implicit => std.fmt.comptimePrint(
            \\// {s}
            \\stack[sp] = if ({d} < @as(usize, @intCast(argc))) dupValue(ctx, argv[{d}]) else JSValue.initUndefined();
            \\sp += 1;
            \\
        , .{ op_name, handler.index.?, handler.index.? }),

        .put_arg_implicit => std.fmt.comptimePrint(
            \\// {s}
            \\if ({d} < @as(usize, @intCast(argc))) {{
            \\    sp -= 1;
            \\    freeValue(ctx, argv[{d}]);
            \\    argv[{d}] = stack[sp];
            \\}}
            \\
        , .{ op_name, handler.index.?, handler.index.?, handler.index.? }),

        .set_arg_implicit => std.fmt.comptimePrint(
            \\// {s}
            \\if ({d} < @as(usize, @intCast(argc))) {{
            \\    freeValue(ctx, argv[{d}]);
            \\    argv[{d}] = dupValue(ctx, stack[sp - 1]);
            \\}}
            \\
        , .{ op_name, handler.index.?, handler.index.?, handler.index.? }),

        .stack_op => blk: {
            const name = handler.stack_op_name.?;
            break :blk if (std.mem.eql(u8, name, "drop"))
                std.fmt.comptimePrint(
                    \\// {s}
                    \\sp -= 1;
                    \\freeValue(ctx, stack[sp]);
                    \\
                , .{op_name})
            else if (std.mem.eql(u8, name, "dup"))
                std.fmt.comptimePrint(
                    \\// {s}
                    \\stack[sp] = dupValue(ctx, stack[sp - 1]);
                    \\sp += 1;
                    \\
                , .{op_name})
            else if (std.mem.eql(u8, name, "dup1"))
                std.fmt.comptimePrint(
                    \\// {s}
                    \\stack[sp] = dupValue(ctx, stack[sp - 2]);
                    \\sp += 1;
                    \\
                , .{op_name})
            else if (std.mem.eql(u8, name, "dup2"))
                std.fmt.comptimePrint(
                    \\// {s}
                    \\stack[sp] = dupValue(ctx, stack[sp - 2]);
                    \\stack[sp + 1] = dupValue(ctx, stack[sp - 1]);
                    \\sp += 2;
                    \\
                , .{op_name})
            else if (std.mem.eql(u8, name, "nip"))
                std.fmt.comptimePrint(
                    \\// {s}
                    \\{{
                    \\    const top = stack[sp - 1];
                    \\    sp -= 1;
                    \\    freeValue(ctx, stack[sp - 1]);
                    \\    stack[sp - 1] = top;
                    \\}}
                    \\
                , .{op_name})
            else if (std.mem.eql(u8, name, "nip1"))
                std.fmt.comptimePrint(
                    \\// {s}
                    \\{{
                    \\    const top = stack[sp - 1];
                    \\    const s1 = stack[sp - 2];
                    \\    sp -= 1;
                    \\    freeValue(ctx, stack[sp - 2]);
                    \\    stack[sp - 2] = s1;
                    \\    stack[sp - 1] = top;
                    \\}}
                    \\
                , .{op_name})
            else if (std.mem.eql(u8, name, "swap"))
                std.fmt.comptimePrint(
                    \\// {s}
                    \\{{
                    \\    const tmp = stack[sp - 1];
                    \\    stack[sp - 1] = stack[sp - 2];
                    \\    stack[sp - 2] = tmp;
                    \\}}
                    \\
                , .{op_name})
            else if (std.mem.eql(u8, name, "rot3l"))
                std.fmt.comptimePrint(
                    \\// {s}
                    \\{{
                    \\    const tmp = stack[sp - 3];
                    \\    stack[sp - 3] = stack[sp - 2];
                    \\    stack[sp - 2] = stack[sp - 1];
                    \\    stack[sp - 1] = tmp;
                    \\}}
                    \\
                , .{op_name})
            else if (std.mem.eql(u8, name, "rot3r"))
                std.fmt.comptimePrint(
                    \\// {s}
                    \\{{
                    \\    const tmp = stack[sp - 1];
                    \\    stack[sp - 1] = stack[sp - 2];
                    \\    stack[sp - 2] = stack[sp - 3];
                    \\    stack[sp - 3] = tmp;
                    \\}}
                    \\
                , .{op_name})
            else
                std.fmt.comptimePrint("// {s} - TODO\n", .{op_name});
        },

        .nop_op => std.fmt.comptimePrint("// {s} - no operation\n", .{op_name}),

        .lnot_op => std.fmt.comptimePrint(
            \\// {s}
            \\{{
            \\    const v = stack[sp - 1];
            \\    stack[sp - 1] = JSValue.initBool(v.isFalsy());
            \\    freeValue(ctx, v);
            \\}}
            \\
        , .{op_name}),

        .type_check => std.fmt.comptimePrint(
            \\// {s}
            \\{{
            \\    const v = stack[sp - 1];
            \\    stack[sp - 1] = JSValue.initBool(v.{s}());
            \\    freeValue(ctx, v);
            \\}}
            \\
        , .{ op_name, handler.js_const.? }),

        .inc_op => std.fmt.comptimePrint(
            \\// {s}
            \\{{
            \\    const a = stack[sp - 1];
            \\    if (JSValue.incSint(a)) |result| {{
            \\        stack[sp - 1] = result;
            \\    }} else {{
            \\        stack[sp - 1] = JS_Add(ctx, a, JSValue.initInt(1));
            \\        freeValue(ctx, a);
            \\    }}
            \\}}
            \\
        , .{op_name}),

        .dec_op => std.fmt.comptimePrint(
            \\// {s}
            \\{{
            \\    const a = stack[sp - 1];
            \\    if (JSValue.decSint(a)) |result| {{
            \\        stack[sp - 1] = result;
            \\    }} else {{
            \\        stack[sp - 1] = JS_Sub(ctx, a, JSValue.initInt(1));
            \\        freeValue(ctx, a);
            \\    }}
            \\}}
            \\
        , .{op_name}),

        .neg_op => std.fmt.comptimePrint(
            \\// {s}
            \\{{
            \\    const a = stack[sp - 1];
            \\    if (JSValue.negSint(a)) |result| {{
            \\        stack[sp - 1] = result;
            \\    }} else {{
            \\        stack[sp - 1] = JSValue.initUndefined(); // TODO: runtime neg
            \\    }}
            \\}}
            \\
        , .{op_name}),

        .plus_op => std.fmt.comptimePrint("// {s} - unary plus (no-op for numbers)\n", .{op_name}),

        .return_op => if (handler.returns_value)
            std.fmt.comptimePrint(
                \\// {s}
                \\sp -= 1;
                \\return stack[sp];
                \\
            , .{op_name})
        else
            std.fmt.comptimePrint(
                \\// {s}
                \\return JSValue.initUndefined();
                \\
            , .{op_name}),

        .post_inc_op => std.fmt.comptimePrint(
            \\// {s}
            \\{{
            \\    const v = stack[sp - 1];
            \\    stack[sp] = if (JSValue.incSint(v)) |r| r else JS_Add(ctx, v, JSValue.initInt(1));
            \\    sp += 1;
            \\}}
            \\
        , .{op_name}),

        .post_dec_op => std.fmt.comptimePrint(
            \\// {s}
            \\{{
            \\    const v = stack[sp - 1];
            \\    stack[sp] = if (JSValue.decSint(v)) |r| r else JS_Sub(ctx, v, JSValue.initInt(1));
            \\    sp += 1;
            \\}}
            \\
        , .{op_name}),

        .bitwise_unary => std.fmt.comptimePrint(
            \\// {s}
            \\{{
            \\    const a = stack[sp - 1];
            \\    if (JSValue.notSint(a)) |result| {{
            \\        stack[sp - 1] = result;
            \\    }} else {{
            \\        stack[sp - 1] = JSValue.initUndefined();
            \\    }}
            \\}}
            \\
        , .{op_name}),

        .tail_call => std.fmt.comptimePrint(
            \\// {s} - tail call (TCO)
            \\// For self-recursive: converted to goto by codegen
            \\// For non-recursive: call and return
            \\{{
            \\    sp -= 2;
            \\    const arg = stack[sp + 1];
            \\    const func = stack[sp];
            \\    return JS_Call(ctx, func, JSValue.initUndefined(), 1, @ptrCast(&arg));
            \\}}
            \\
        , .{op_name}),

        // Complex ops - need special handling in codegen
        .array_get,
        .array_get2,
        .array_put,
        .array_length,
        .prop_get,
        .prop_get2,
        .prop_put,
        .pow_op,
        .typeof_op,
        .instanceof_op,
        .in_op,
        .to_object_op,
        .to_propkey_op,
        .unary_op,
        .complex,
        => std.fmt.comptimePrint("// {s} - complex (handled by codegen)\n", .{op_name}),
    };
}

/// Check if opcode can be handled by simple comptime patterns
pub fn isSimpleHandler(op: Opcode) bool {
    const handler = getHandler(op);
    return handler.pattern != .complex and
        handler.pattern != .array_get and
        handler.pattern != .array_get2 and
        handler.pattern != .array_put and
        handler.pattern != .array_length and
        handler.pattern != .prop_get and
        handler.pattern != .prop_get2 and
        handler.pattern != .prop_put;
}

/// Count comptime-handled opcodes
pub fn countHandled() comptime_int {
    var count: comptime_int = 0;
    for (std.enums.values(Opcode)) |op| {
        if (isSimpleHandler(op)) count += 1;
    }
    return count;
}

// ============================================================================
// Tests
// ============================================================================

test "handler generation" {
    const add_code = comptime generateZigCode(getHandler(.add), "add");
    try std.testing.expect(std.mem.indexOf(u8, add_code, "addSint") != null);

    const push_code = comptime generateZigCode(getHandler(.push_1), "push_1");
    try std.testing.expect(std.mem.indexOf(u8, push_code, "initInt(1)") != null);

    const get_loc_code = comptime generateZigCode(getHandler(.get_loc0), "get_loc0");
    try std.testing.expect(std.mem.indexOf(u8, get_loc_code, "locals[0]") != null);

    const drop_code = comptime generateZigCode(getHandler(.drop), "drop");
    try std.testing.expect(std.mem.indexOf(u8, drop_code, "freeValue") != null);
}

test "handler coverage" {
    const count = comptime countHandled();
    // Should have at least 50 simple handlers
    try std.testing.expect(count >= 50);
}
