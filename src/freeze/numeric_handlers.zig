//! Comptime Numeric Opcode Handler Generator
//!
//! Generalizes int32_handlers.zig to support both i32 and f64 function tiers.
//! At comptime, analyzes opcodes to determine which numeric tier a function
//! qualifies for:
//!   - `i32` tier: pure integer arithmetic (fib, gcd, isPrime)
//!   - `f64` tier: float-capable arithmetic (distance, lerp, mandelbrot)
//!
//! The LLVM codegen uses comptime-known ValueKind to emit the right
//! instructions (buildAdd vs buildFAdd, buildICmp vs buildFCmp).

const std = @import("std");
const opcodes = @import("opcodes.zig");
const Opcode = opcodes.Opcode;

/// The numeric type a standalone WASM function operates on.
/// Determines LLVM types, instructions, and WASM export signature.
pub const ValueKind = enum {
    /// Pure i32 — args are i32, locals are i32, return is i32
    i32,
    /// Pure f64 — args are f64, locals are f64, return is f64
    f64,
};

/// Opcode pattern categories for numeric codegen
pub const NumericPattern = enum {
    /// Push constant (i32 literal or f64 literal)
    push_const,
    /// Get function argument by index
    get_arg,
    /// Put (store) argument — pops value, stores to arg slot
    put_arg,
    /// Set argument — stores to arg slot, keeps value on stack
    set_arg,
    /// Binary arithmetic: +, -, *, /, %
    binary_arith,
    /// Binary comparison: <, <=, >, >=, ==, !=
    binary_cmp,
    /// Bitwise binary (i32 only): &, |, ^, <<, >>, >>>
    bitwise_binary,
    /// Unary: negation, bitwise not
    unary,
    /// Logical NOT: !val → 0 or 1
    lnot,
    /// Increment/decrement: val +/- 1
    inc_dec,
    /// Post-increment/decrement: pop val, push (val+/-1), push val (old value)
    post_inc_dec,
    /// Get local variable
    get_loc,
    /// Put local variable (pop + store)
    put_loc,
    /// Set local variable (store + keep on stack)
    set_loc,
    /// Add to local: local[N] += pop()
    add_loc,
    /// Increment local: local[N]++
    inc_loc,
    /// Decrement local: local[N]--
    dec_loc,
    /// Get two locals at once (fused: push loc0, push loc1)
    get_loc_pair,
    /// Get local with TDZ check
    get_loc_check,
    /// Put local with const check
    put_loc_check,
    /// Set local uninitialized
    set_loc_uninitialized,
    /// Push boolean as numeric (true=1/1.0, false=0/0.0)
    push_bool,
    /// Stack: dup
    stack_dup,
    /// Stack: dup2 (duplicate top 2 values)
    stack_dup2,
    /// Stack: drop
    stack_drop,
    /// Stack: swap
    stack_swap,
    /// No-op
    nop,
    /// Self-reference for recursive calls
    self_ref,
    /// Recursive call
    call_self,
    /// Tail recursive call (TCO)
    tail_call_self,
    /// Return value
    ret,
    /// Conditional branch (if false)
    if_false,
    /// Conditional branch (if true)
    if_true,
    /// Unconditional branch
    goto_br,
    /// Push constant from constant pool (resolved at compile time)
    push_cpool,
    /// Array element read: arr[idx] (pops array+index, pushes element)
    array_get,
    /// Array element read keeping array: arr[idx] (pops index, keeps array, pushes element)
    array_get2,
    /// Array element write: arr[idx] = val (pops array+index+value)
    array_put,
    /// Array length: pops array base, pushes its length (from extra WASM param)
    array_length,
    /// Always-false check (pops value, pushes 0). Used for is_undefined/is_null
    /// in numeric context where values are never undefined or null.
    always_false,
    /// Object field read: obj.prop (pops object, pushes i32 field value)
    /// Only valid when the arg is a known struct shape (detected by shape analysis)
    field_get,
    /// Unsupported in numeric mode
    unsupported,
};

/// Comptime operator kind — replaces runtime string comparisons with enum dispatch.
pub const OpKind = enum {
    add, // +
    sub, // -
    mul, // *
    div, // /
    mod, // %
    shl, // <<
    sar, // >> (arithmetic)
    shr, // >>> (logical)
    band, // &
    bor, // |
    bxor, // ^
    lt, // <
    lte, // <=
    gt, // >
    gte, // >=
    eq, // ==
    neq, // !=
    neg, // unary -
    bnot, // ~
    dup, // stack dup
    drop, // stack drop
};

/// Numeric handler definition — comptime-evaluated per opcode
pub const NumericHandler = struct {
    pattern: NumericPattern,
    /// Operator kind for arithmetic/comparison/bitwise dispatch
    op: ?OpKind = null,
    /// Constant value (for push_const with known value)
    value: ?i32 = null,
    /// Argument/local index (for get_arg0..3, get_loc0..3, etc.)
    index: ?u8 = null,
    /// For inc/dec: true=increment, false=decrement
    is_inc: ?bool = null,
    /// Whether this opcode requires i32 (bitwise ops, shifts)
    /// When true, the opcode is rejected for f64-tier functions
    requires_i32: bool = false,
    /// Whether this opcode introduces float (div produces float in JS)
    introduces_float: bool = false,
};

/// Get numeric handler for an opcode. This is evaluated at comptime
/// by isPureNumericFunction and the LLVM codegen.
pub fn getHandler(opcode: Opcode) NumericHandler {
    return switch (opcode) {
        // ── Push constants ──────────────────────────────────────
        .push_0 => .{ .pattern = .push_const, .value = 0 },
        .push_1 => .{ .pattern = .push_const, .value = 1 },
        .push_2 => .{ .pattern = .push_const, .value = 2 },
        .push_3 => .{ .pattern = .push_const, .value = 3 },
        .push_4 => .{ .pattern = .push_const, .value = 4 },
        .push_5 => .{ .pattern = .push_const, .value = 5 },
        .push_6 => .{ .pattern = .push_const, .value = 6 },
        .push_7 => .{ .pattern = .push_const, .value = 7 },
        .push_minus1 => .{ .pattern = .push_const, .value = -1 },
        .push_i8, .push_i16, .push_i32 => .{ .pattern = .push_const }, // value from operand
        .push_const8, .push_const => .{ .pattern = .push_cpool }, // value from constant pool

        // ── Push booleans ───────────────────────────────────────
        .push_true => .{ .pattern = .push_bool, .value = 1 },
        .push_false => .{ .pattern = .push_bool, .value = 0 },

        // ── Null/undefined as numeric constants ─────────────────
        // In JS: null + 1 === 1 (null coerces to 0), undefined + 1 === NaN
        // In i32 context with bitwise: both coerce to 0 (NaN | 0 === 0)
        .null => .{ .pattern = .unsupported },
        .undefined => .{ .pattern = .unsupported },

        // ── Null/undefined checks (always false in numeric context) ──
        .is_undefined, .is_null, .is_undefined_or_null => .{ .pattern = .always_false },

        // ── Property key conversion (no-op for numeric values) ──────
        // to_propkey: replaces TOS (n_pop=1, n_push=1) → nop in numeric context
        .to_propkey => .{ .pattern = .nop },
        // to_propkey2: keeps original, pushes key copy (n_pop=1, n_push=2) → dup in numeric context
        .to_propkey2 => .{ .pattern = .stack_dup },

        // ── Get arguments ───────────────────────────────────────
        .get_arg0 => .{ .pattern = .get_arg, .index = 0 },
        .get_arg1 => .{ .pattern = .get_arg, .index = 1 },
        .get_arg2 => .{ .pattern = .get_arg, .index = 2 },
        .get_arg3 => .{ .pattern = .get_arg, .index = 3 },
        .get_arg => .{ .pattern = .get_arg },

        // ── Put arguments ───────────────────────────────────────
        .put_arg0 => .{ .pattern = .put_arg, .index = 0 },
        .put_arg1 => .{ .pattern = .put_arg, .index = 1 },
        .put_arg2 => .{ .pattern = .put_arg, .index = 2 },
        .put_arg3 => .{ .pattern = .put_arg, .index = 3 },

        // ── Set arguments ───────────────────────────────────────
        .set_arg0 => .{ .pattern = .set_arg, .index = 0 },
        .set_arg1 => .{ .pattern = .set_arg, .index = 1 },
        .set_arg2 => .{ .pattern = .set_arg, .index = 2 },
        .set_arg3 => .{ .pattern = .set_arg, .index = 3 },
        .set_arg => .{ .pattern = .set_arg },

        // ── Binary arithmetic ───────────────────────────────────
        .add => .{ .pattern = .binary_arith, .op = .add },
        .sub => .{ .pattern = .binary_arith, .op = .sub },
        // mul: JS mul is f64, products can exceed i32 range affecting subsequent ops
        .mul => .{ .pattern = .binary_arith, .op = .mul, .introduces_float = true },
        // div: produces float in JS (9/2=4.5), so marks introduces_float
        .div => .{ .pattern = .binary_arith, .op = .div, .introduces_float = true },
        .mod => .{ .pattern = .binary_arith, .op = .mod },

        // ── Bitwise (i32 only) ──────────────────────────────────
        .shl => .{ .pattern = .bitwise_binary, .op = .shl, .requires_i32 = true },
        .sar => .{ .pattern = .bitwise_binary, .op = .sar, .requires_i32 = true },
        .shr => .{ .pattern = .bitwise_binary, .op = .shr, .requires_i32 = true },
        .@"and" => .{ .pattern = .bitwise_binary, .op = .band, .requires_i32 = true },
        .@"or" => .{ .pattern = .bitwise_binary, .op = .bor, .requires_i32 = true },
        .@"xor" => .{ .pattern = .bitwise_binary, .op = .bxor, .requires_i32 = true },

        // ── Comparisons ─────────────────────────────────────────
        .lt => .{ .pattern = .binary_cmp, .op = .lt },
        .lte => .{ .pattern = .binary_cmp, .op = .lte },
        .gt => .{ .pattern = .binary_cmp, .op = .gt },
        .gte => .{ .pattern = .binary_cmp, .op = .gte },
        .eq, .strict_eq => .{ .pattern = .binary_cmp, .op = .eq },
        .neq, .strict_neq => .{ .pattern = .binary_cmp, .op = .neq },

        // ── Unary ───────────────────────────────────────────────
        .neg => .{ .pattern = .unary, .op = .neg },
        .not => .{ .pattern = .unary, .op = .bnot, .requires_i32 = true },

        // ── Logical NOT ─────────────────────────────────────────
        .lnot => .{ .pattern = .lnot },

        // ── Get locals ──────────────────────────────────────────
        .get_loc0 => .{ .pattern = .get_loc, .index = 0 },
        .get_loc1 => .{ .pattern = .get_loc, .index = 1 },
        .get_loc2 => .{ .pattern = .get_loc, .index = 2 },
        .get_loc3 => .{ .pattern = .get_loc, .index = 3 },
        .get_loc, .get_loc8 => .{ .pattern = .get_loc },
        .get_loc0_loc1 => .{ .pattern = .get_loc_pair },

        // ── Put locals ──────────────────────────────────────────
        .put_loc0 => .{ .pattern = .put_loc, .index = 0 },
        .put_loc1 => .{ .pattern = .put_loc, .index = 1 },
        .put_loc2 => .{ .pattern = .put_loc, .index = 2 },
        .put_loc3 => .{ .pattern = .put_loc, .index = 3 },
        .put_loc, .put_loc8 => .{ .pattern = .put_loc },

        // ── Set locals ──────────────────────────────────────────
        .set_loc0 => .{ .pattern = .set_loc, .index = 0 },
        .set_loc1 => .{ .pattern = .set_loc, .index = 1 },
        .set_loc2 => .{ .pattern = .set_loc, .index = 2 },
        .set_loc3 => .{ .pattern = .set_loc, .index = 3 },
        .set_loc, .set_loc8 => .{ .pattern = .set_loc },

        // ── Local arithmetic shortcuts ──────────────────────────
        .add_loc => .{ .pattern = .add_loc },
        .inc_loc => .{ .pattern = .inc_loc },
        .dec_loc => .{ .pattern = .dec_loc },

        // ── Inc/Dec ─────────────────────────────────────────────
        .inc => .{ .pattern = .inc_dec, .is_inc = true },
        .dec => .{ .pattern = .inc_dec, .is_inc = false },
        .post_inc => .{ .pattern = .post_inc_dec, .is_inc = true },
        .post_dec => .{ .pattern = .post_inc_dec, .is_inc = false },

        // ── Stack ops ───────────────────────────────────────────
        .dup => .{ .pattern = .stack_dup },
        .dup2 => .{ .pattern = .stack_dup2 },
        .drop => .{ .pattern = .stack_drop },
        .swap => .{ .pattern = .stack_swap },
        .nop => .{ .pattern = .nop },

        // ── Self-reference (recursive calls / destructured Math) ────────────────────
        .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3, .get_var_ref, .get_var, .get_var_undef => .{ .pattern = .self_ref },

        // ── Calls ───────────────────────────────────────────────
        .call0, .call1, .call2, .call3, .call => .{ .pattern = .call_self },
        .tail_call => .{ .pattern = .tail_call_self },

        // ── Return ──────────────────────────────────────────────
        .@"return" => .{ .pattern = .ret },
        .return_undef => .{ .pattern = .ret, .value = 0 },

        // ── TDZ/const checks (same as regular in numeric context) ─
        .get_loc_check => .{ .pattern = .get_loc_check },
        .put_loc_check, .put_loc_check_init => .{ .pattern = .put_loc_check },
        .set_loc_uninitialized => .{ .pattern = .set_loc_uninitialized },

        // ── Control flow ────────────────────────────────────────
        .if_false, .if_false8 => .{ .pattern = .if_false },
        .if_true, .if_true8 => .{ .pattern = .if_true },
        .goto, .goto8, .goto16 => .{ .pattern = .goto_br },

        // ── Array access (via WASM linear memory) ──────────────
        .get_array_el => .{ .pattern = .array_get },
        .get_array_el2 => .{ .pattern = .array_get2 },
        .put_array_el => .{ .pattern = .array_put },
        .get_length => .{ .pattern = .array_length },

        // ── Object field access (struct mode only) ─────────────
        .get_field => .{ .pattern = .field_get },

        // ── Everything else ─────────────────────────────────────
        else => .{ .pattern = .unsupported },
    };
}

/// Result of function analysis — includes tier and array usage info.
pub const AnalysisResult = struct {
    kind: ValueKind,
    uses_arrays: bool,
};

/// Analyze a function's opcodes at comptime to determine which numeric
/// tier it qualifies for. Returns null if unsupported.
///
/// Decision logic:
///   1. If ANY opcode is `.unsupported` → null (can't compile standalone)
///   2. If ANY opcode has `introduces_float` (div) → f64 tier
///   3. If ALL opcodes are numeric-compatible and none introduce float → i32 tier
///   4. Must have at least one "computing" opcode to avoid pass-through functions
pub fn analyzeFunction(instructions: anytype) ?ValueKind {
    const result = analyzeFunctionFull(instructions) orelse return null;
    return result.kind;
}

/// Full analysis returning both numeric tier and array usage info.
/// By default, field_get (get_field) is rejected — use analyzeFunctionWithStructs
/// for functions with known struct args.
pub fn analyzeFunctionFull(instructions: anytype) ?AnalysisResult {
    return analyzeFunctionFullImpl(instructions, false);
}

/// Variant that allows field_get (get_field) on struct-typed arguments.
/// Called after struct arg detection determines which args have known shapes.
pub fn analyzeFunctionWithStructs(instructions: anytype) ?AnalysisResult {
    return analyzeFunctionFullImpl(instructions, true);
}

fn analyzeFunctionFullImpl(instructions: anytype, allow_field_get: bool) ?AnalysisResult {
    var has_computing_op = false;
    var needs_float = false;
    var has_i32_only = false;
    var uses_arrays = false;
    var uses_structs = false;

    for (instructions) |instr| {
        const handler = getHandler(instr.opcode);
        if (handler.pattern == .field_get) {
            if (!allow_field_get) return null;
            has_computing_op = true;
            uses_structs = true;
            continue;
        }
        if (handler.pattern == .unsupported) return null;

        if (handler.introduces_float) needs_float = true;
        if (handler.requires_i32) has_i32_only = true;

        switch (handler.pattern) {
            .binary_arith, .binary_cmp, .bitwise_binary,
            .unary, .inc_dec, .post_inc_dec, .lnot, .push_const, .push_cpool,
            .push_bool, .call_self, .tail_call_self,
            .add_loc, .inc_loc, .dec_loc,
            => has_computing_op = true,
            .array_get, .array_get2, .array_put, .array_length => {
                has_computing_op = true;
                uses_arrays = true;
            },
            else => {},
        }
    }

    if (!has_computing_op) return null;

    // Tier selection:
    // - Pure integer (no float-introducing ops): i32 (optimal for bitwise, loops)
    // - Any float-introducing ops (division): f64 — matches JS semantics where
    //   arithmetic always uses double-precision float. The f64 tier handles
    //   bitwise ops via fptosi→bitwise→sitofp (correct for JS `| 0` idiom).
    //   Using i32 for `(x * y) | 0` gives WRONG results because i32.mul wraps
    //   at 32 bits while JS multiplies in float64 then truncates.
    const kind: ValueKind = if (needs_float) .f64 else .i32;

    return .{ .kind = kind, .uses_arrays = uses_arrays };
}

/// Check if a pattern proves the function does computation (not just pass-through)
pub fn isComputingPattern(pattern: NumericPattern) bool {
    return switch (pattern) {
        .binary_arith, .binary_cmp, .bitwise_binary,
        .unary, .inc_dec, .post_inc_dec, .lnot, .push_const, .push_cpool,
        .push_bool, .call_self, .tail_call_self,
        .add_loc, .inc_loc, .dec_loc,
        .array_get, .array_get2, .array_put, .array_length,
        .field_get,
        => true,
        else => false,
    };
}

/// Apply stack effects for opcodes that map to .unsupported in the numeric handler table
/// but have well-defined stack semantics (Math.* pattern opcodes: get_field2, call_method,
/// tail_call_method). Without this, stack simulation drifts and subsequent put_array_el
/// checks fail to detect mutations.
fn applyUnsupportedStackEffect(instr: anytype, stack: *[128]i8, sp: *usize) void {
    switch (instr.opcode) {
        // get_field2: pop obj, push (obj, method) → net +1
        .get_field2 => {
            // obj stays, method pushed on top
            if (sp.* < stack.len) {
                stack[sp.*] = -1;
                sp.* += 1;
            }
        },
        // call_method(N): pop (N args + method + receiver), push result → net -(N+1)
        .call_method, .tail_call_method => {
            const argc: u32 = switch (instr.operand) {
                .u16 => |v| v,
                else => 1,
            };
            // pop N args
            if (sp.* >= argc) sp.* -= argc;
            // pop method
            if (sp.* >= 1) sp.* -= 1;
            // pop receiver
            if (sp.* >= 1) sp.* -= 1;
            // push result
            if (sp.* < stack.len) {
                stack[sp.*] = -1;
                sp.* += 1;
            }
        },
        else => {},
    }
}

/// Detect which function arguments are used as arrays (vs scalars).
/// Uses lightweight stack simulation to trace argument flow into
/// get_array_el/put_array_el opcodes.
/// Returns a bitmask: bit N set = arg N is an array.
pub fn detectArrayArgs(instructions: anytype, arg_count: u32) u8 {
    // Stack entries: -1 = not an arg, 0..7 = arg index
    var stack: [128]i8 = undefined;
    var sp: usize = 0;
    var result: u8 = 0;

    for (instructions) |instr| {
        const handler = getHandler(instr.opcode);
        switch (handler.pattern) {
            .get_arg => {
                const idx: u8 = handler.index orelse switch (instr.operand) {
                    .arg => |a| @intCast(a),
                    .u8 => |a| a,
                    else => 255,
                };
                if (sp < stack.len) {
                    stack[sp] = if (idx < arg_count) @intCast(idx) else -1;
                    sp += 1;
                }
            },
            .array_get => {
                // Pop index, pop base. Base might be an arg.
                if (sp >= 2) {
                    sp -= 1; // pop index
                    sp -= 1; // pop base
                    const base_arg = stack[sp];
                    if (base_arg >= 0 and base_arg < 8) result |= @as(u8, 1) << @intCast(base_arg);
                    stack[sp] = -1; // result is not an arg
                    sp += 1;
                }
            },
            .array_get2 => {
                // Pop index, keep base. Push result on top.
                if (sp >= 2) {
                    sp -= 1; // pop index
                    // base stays at sp-1
                    if (sp >= 1) {
                        const base_arg = stack[sp - 1];
                        if (base_arg >= 0 and base_arg < 8) result |= @as(u8, 1) << @intCast(base_arg);
                    }
                    // push result
                    if (sp < stack.len) {
                        stack[sp] = -1;
                        sp += 1;
                    }
                }
            },
            .array_put => {
                // Pop value, pop index, pop base.
                if (sp >= 3) {
                    sp -= 1; // pop value
                    sp -= 1; // pop index
                    sp -= 1; // pop base
                    const base_arg = stack[sp];
                    if (base_arg >= 0 and base_arg < 8) result |= @as(u8, 1) << @intCast(base_arg);
                }
            },
            // For all other patterns, use generic stack effect
            .push_const, .push_cpool, .push_bool => {
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .binary_arith, .binary_cmp, .bitwise_binary => {
                if (sp >= 2) { sp -= 1; stack[sp - 1] = -1; } // pop 2, push 1
            },
            .unary, .lnot, .inc_dec => {
                if (sp >= 1) { stack[sp - 1] = -1; } // pop 1, push 1
            },
            .post_inc_dec => {
                // Pop 1, push 2 (incremented value + old value)
                if (sp >= 1) {
                    stack[sp - 1] = -1; // overwrite with new value
                    if (sp < stack.len) { stack[sp] = -1; sp += 1; } // push old value
                }
            },
            .get_loc, .get_loc_check => {
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .get_loc_pair => {
                // Push 2 locals (loc0 and loc1)
                if (sp + 1 < stack.len) { stack[sp] = -1; stack[sp + 1] = -1; sp += 2; }
            },
            .put_loc, .put_loc_check, .put_arg => {
                if (sp >= 1) sp -= 1; // pop 1, push 0
            },
            .set_loc, .set_arg => {}, // peek, no pop
            .stack_dup => {
                if (sp >= 1 and sp < stack.len) { stack[sp] = stack[sp - 1]; sp += 1; }
            },
            .stack_dup2 => {
                if (sp >= 2 and sp + 1 < stack.len) { stack[sp] = stack[sp - 2]; stack[sp + 1] = stack[sp - 1]; sp += 2; }
            },
            .stack_drop => {
                if (sp >= 1) sp -= 1;
            },
            .stack_swap => {
                if (sp >= 2) {
                    const tmp = stack[sp - 1];
                    stack[sp - 1] = stack[sp - 2];
                    stack[sp - 2] = tmp;
                }
            },
            .self_ref => {
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .call_self => {
                // Pop function ref + args, push result
                const argc: u32 = switch (instr.opcode) {
                    .call1 => 1,
                    .call2 => 2,
                    .call3 => 3,
                    .call => instr.operand.u16,
                    else => 1,
                };
                if (sp >= argc) sp -= argc;
                // pop the function ref too (from self_ref)
                if (sp >= 1) sp -= 1;
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .ret, .if_false, .if_true => {
                if (sp >= 1) sp -= 1;
            },
            .add_loc => {
                if (sp >= 1) sp -= 1; // add_loc pops from stack
            },
            .inc_loc, .dec_loc => {}, // in-place local modify, no stack effect
            .array_length => {
                // Pop array base, push length (not an arg)
                if (sp >= 1) {
                    const base_arg = stack[sp - 1];
                    if (base_arg >= 0 and base_arg < 8) result |= @as(u8, 1) << @intCast(base_arg);
                    stack[sp - 1] = -1;
                }
            },
            .always_false => {
                // Pop 1, push 1 (result is not an arg)
                if (sp >= 1) { stack[sp - 1] = -1; }
            },
            .field_get => {
                // Pop object, push field value. Object might be an arg (struct arg detection).
                // Don't set array_args bit — struct args are tracked separately.
                if (sp >= 1) { stack[sp - 1] = -1; }
            },
            .nop, .goto_br, .set_loc_uninitialized, .tail_call_self => {},
            .unsupported => {
                // Math.* opcodes (get_field2, call_method, tail_call_method) fall through
                // to .unsupported but have well-defined stack effects that must be tracked.
                applyUnsupportedStackEffect(instr, &stack, &sp);
            },
        }
    }
    return result;
}

/// Detect which function args are WRITTEN via put_array_el.
/// Returns a bitmask: bit N set = arg N is mutated (needs copy-back).
/// Read-only array args (only used via get_array_el) won't have their bit set.
pub fn detectMutatedArgs(instructions: anytype, arg_count: u32) u8 {
    var stack: [128]i8 = undefined;
    var sp: usize = 0;
    var result: u8 = 0;

    for (instructions) |instr| {
        const handler = getHandler(instr.opcode);
        switch (handler.pattern) {
            .get_arg => {
                const idx: u8 = handler.index orelse switch (instr.operand) {
                    .arg => |a| @intCast(a),
                    .u8 => |a| a,
                    else => 255,
                };
                if (sp < stack.len) {
                    stack[sp] = if (idx < arg_count) @intCast(idx) else -1;
                    sp += 1;
                }
            },
            .array_get => {
                if (sp >= 2) { sp -= 2; stack[sp] = -1; sp += 1; }
            },
            .array_get2 => {
                if (sp >= 2) { sp -= 1; if (sp < stack.len) { stack[sp] = -1; sp += 1; } }
            },
            .array_put => {
                // Only put_array_el marks args as mutated
                if (sp >= 3) {
                    sp -= 1; // pop value
                    sp -= 1; // pop index
                    sp -= 1; // pop base
                    const base_arg = stack[sp];
                    if (base_arg >= 0 and base_arg < 8) result |= @as(u8, 1) << @intCast(base_arg);
                }
            },
            .push_const, .push_cpool, .push_bool => {
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .binary_arith, .binary_cmp, .bitwise_binary => {
                if (sp >= 2) { sp -= 1; stack[sp - 1] = -1; }
            },
            .unary, .lnot, .inc_dec => {
                if (sp >= 1) { stack[sp - 1] = -1; }
            },
            .post_inc_dec => {
                if (sp >= 1) { stack[sp - 1] = -1; if (sp < stack.len) { stack[sp] = -1; sp += 1; } }
            },
            .get_loc, .get_loc_check => {
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .get_loc_pair => {
                if (sp + 1 < stack.len) { stack[sp] = -1; stack[sp + 1] = -1; sp += 2; }
            },
            .put_loc, .put_loc_check, .put_arg => {
                if (sp >= 1) sp -= 1;
            },
            .set_loc, .set_arg => {},
            .stack_dup => {
                if (sp >= 1 and sp < stack.len) { stack[sp] = stack[sp - 1]; sp += 1; }
            },
            .stack_dup2 => {
                if (sp >= 2 and sp + 1 < stack.len) { stack[sp] = stack[sp - 2]; stack[sp + 1] = stack[sp - 1]; sp += 2; }
            },
            .stack_drop => {
                if (sp >= 1) sp -= 1;
            },
            .stack_swap => {
                if (sp >= 2) {
                    const tmp = stack[sp - 1];
                    stack[sp - 1] = stack[sp - 2];
                    stack[sp - 2] = tmp;
                }
            },
            .self_ref => {
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .call_self => {
                const argc: u32 = switch (instr.opcode) {
                    .call1 => 1, .call2 => 2, .call3 => 3, .call => instr.operand.u16, else => 1,
                };
                if (sp >= argc) sp -= argc;
                if (sp >= 1) sp -= 1;
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .ret, .if_false, .if_true => {
                if (sp >= 1) sp -= 1;
            },
            .add_loc => {
                if (sp >= 1) sp -= 1;
            },
            .inc_loc, .dec_loc => {},
            .array_length => {
                // Pop array base, push length (not an arg). No mutation.
                if (sp >= 1) { stack[sp - 1] = -1; }
            },
            .always_false, .field_get => {
                if (sp >= 1) { stack[sp - 1] = -1; }
            },
            .nop, .goto_br, .set_loc_uninitialized, .tail_call_self => {},
            .unsupported => {
                applyUnsupportedStackEffect(instr, &stack, &sp);
            },
        }
    }
    return result;
}

/// Detect which function args are READ via get_array_el (element access).
/// Returns a bitmask: bit N set = arg N has its elements read.
/// Unlike detectArrayArgs, this does NOT count put_array_el or get_length —
/// only actual element reads. Used to identify write-only array params where
/// the copy-in can be skipped (the data is overwritten, not read).
pub fn detectReadArrayArgs(instructions: anytype, arg_count: u32) u8 {
    var stack: [128]i8 = undefined;
    var sp: usize = 0;
    var result: u8 = 0;

    for (instructions) |instr| {
        const handler = getHandler(instr.opcode);
        switch (handler.pattern) {
            .get_arg => {
                const idx: u8 = handler.index orelse switch (instr.operand) {
                    .arg => |a| @intCast(a),
                    .u8 => |a| a,
                    else => 255,
                };
                if (sp < stack.len) {
                    stack[sp] = if (idx < arg_count) @intCast(idx) else -1;
                    sp += 1;
                }
            },
            .array_get => {
                // Only reads set the result bit
                if (sp >= 2) {
                    sp -= 1; // pop index
                    sp -= 1; // pop base
                    const base_arg = stack[sp];
                    if (base_arg >= 0 and base_arg < 8) result |= @as(u8, 1) << @intCast(base_arg);
                    stack[sp] = -1;
                    sp += 1;
                }
            },
            .array_get2 => {
                if (sp >= 2) {
                    sp -= 1; // pop index
                    if (sp >= 1) {
                        const base_arg = stack[sp - 1];
                        if (base_arg >= 0 and base_arg < 8) result |= @as(u8, 1) << @intCast(base_arg);
                    }
                    if (sp < stack.len) { stack[sp] = -1; sp += 1; }
                }
            },
            .array_put => {
                // Writes do NOT set the result bit — just update stack
                if (sp >= 3) { sp -= 3; }
            },
            .push_const, .push_cpool, .push_bool => {
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .binary_arith, .binary_cmp, .bitwise_binary => {
                if (sp >= 2) { sp -= 1; stack[sp - 1] = -1; }
            },
            .unary, .lnot, .inc_dec => {
                if (sp >= 1) { stack[sp - 1] = -1; }
            },
            .post_inc_dec => {
                if (sp >= 1) { stack[sp - 1] = -1; if (sp < stack.len) { stack[sp] = -1; sp += 1; } }
            },
            .get_loc, .get_loc_check => {
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .get_loc_pair => {
                if (sp + 1 < stack.len) { stack[sp] = -1; stack[sp + 1] = -1; sp += 2; }
            },
            .put_loc, .put_loc_check, .put_arg => {
                if (sp >= 1) sp -= 1;
            },
            .set_loc, .set_arg => {},
            .stack_dup => {
                if (sp >= 1 and sp < stack.len) { stack[sp] = stack[sp - 1]; sp += 1; }
            },
            .stack_dup2 => {
                if (sp >= 2 and sp + 1 < stack.len) { stack[sp] = stack[sp - 2]; stack[sp + 1] = stack[sp - 1]; sp += 2; }
            },
            .stack_drop => {
                if (sp >= 1) sp -= 1;
            },
            .stack_swap => {
                if (sp >= 2) {
                    const tmp = stack[sp - 1];
                    stack[sp - 1] = stack[sp - 2];
                    stack[sp - 2] = tmp;
                }
            },
            .self_ref => {
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .call_self => {
                const argc: u32 = switch (instr.opcode) {
                    .call1 => 1, .call2 => 2, .call3 => 3, .call => instr.operand.u16, else => 1,
                };
                if (sp >= argc) sp -= argc;
                if (sp >= 1) sp -= 1;
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .ret, .if_false, .if_true => {
                if (sp >= 1) sp -= 1;
            },
            .add_loc => {
                if (sp >= 1) sp -= 1;
            },
            .inc_loc, .dec_loc => {},
            .array_length => {
                // Length access does NOT count as element read — just update stack
                if (sp >= 1) { stack[sp - 1] = -1; }
            },
            .always_false, .field_get => {
                if (sp >= 1) { stack[sp - 1] = -1; }
            },
            .nop, .goto_br, .set_loc_uninitialized, .tail_call_self => {},
            .unsupported => {
                applyUnsupportedStackEffect(instr, &stack, &sp);
            },
        }
    }
    return result;
}

/// Detect which array args have `.length` accessed via `get_length`.
/// Returns a bitmask: bit N set = arg N's length is read.
/// Used to add extra length parameters to the WASM function signature.
pub fn detectLengthArgs(instructions: anytype, arg_count: u32) u8 {
    // Stack entries: -1 = not an arg, 0..7 = arg index
    var stack: [128]i8 = undefined;
    var sp: usize = 0;
    var result: u8 = 0;

    for (instructions) |instr| {
        const handler = getHandler(instr.opcode);
        switch (handler.pattern) {
            .get_arg => {
                const idx: u8 = handler.index orelse switch (instr.operand) {
                    .arg => |a| @intCast(a),
                    .u8 => |a| a,
                    else => 255,
                };
                if (sp < stack.len) {
                    stack[sp] = if (idx < arg_count) @intCast(idx) else -1;
                    sp += 1;
                }
            },
            .array_length => {
                // Pop array base — if it was an arg, mark its length as used
                if (sp >= 1) {
                    const base_arg = stack[sp - 1];
                    if (base_arg >= 0 and base_arg < 8) result |= @as(u8, 1) << @intCast(base_arg);
                    stack[sp - 1] = -1; // length is not an arg
                }
            },
            // All other patterns: generic stack simulation
            .array_get => {
                if (sp >= 2) { sp -= 2; if (sp < stack.len) { stack[sp] = -1; sp += 1; } }
            },
            .array_get2 => {
                if (sp >= 2) { sp -= 1; if (sp < stack.len) { stack[sp] = -1; sp += 1; } }
            },
            .array_put => {
                if (sp >= 3) { sp -= 3; }
            },
            .push_const, .push_cpool, .push_bool => {
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .binary_arith, .binary_cmp, .bitwise_binary => {
                if (sp >= 2) { sp -= 1; stack[sp - 1] = -1; }
            },
            .unary, .lnot, .inc_dec, .always_false, .field_get => {
                if (sp >= 1) { stack[sp - 1] = -1; }
            },
            .post_inc_dec => {
                if (sp >= 1) { stack[sp - 1] = -1; if (sp < stack.len) { stack[sp] = -1; sp += 1; } }
            },
            .get_loc, .get_loc_check => {
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .get_loc_pair => {
                if (sp + 1 < stack.len) { stack[sp] = -1; stack[sp + 1] = -1; sp += 2; }
            },
            .put_loc, .put_loc_check, .put_arg => {
                if (sp >= 1) sp -= 1;
            },
            .set_loc, .set_arg => {},
            .stack_dup => {
                if (sp >= 1 and sp < stack.len) { stack[sp] = stack[sp - 1]; sp += 1; }
            },
            .stack_dup2 => {
                if (sp >= 2 and sp + 1 < stack.len) { stack[sp] = stack[sp - 2]; stack[sp + 1] = stack[sp - 1]; sp += 2; }
            },
            .stack_drop => {
                if (sp >= 1) sp -= 1;
            },
            .stack_swap => {
                if (sp >= 2) {
                    const tmp = stack[sp - 1];
                    stack[sp - 1] = stack[sp - 2];
                    stack[sp - 2] = tmp;
                }
            },
            .self_ref => {
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .call_self => {
                const argc: u32 = switch (instr.opcode) {
                    .call1 => 1, .call2 => 2, .call3 => 3, .call => instr.operand.u16, else => 1,
                };
                if (sp >= argc) sp -= argc;
                if (sp >= 1) sp -= 1;
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .ret, .if_false, .if_true => {
                if (sp >= 1) sp -= 1;
            },
            .add_loc => {
                if (sp >= 1) sp -= 1;
            },
            .inc_loc, .dec_loc => {},
            .nop, .goto_br, .set_loc_uninitialized, .tail_call_self => {},
            .unsupported => {
                applyUnsupportedStackEffect(instr, &stack, &sp);
            },
        }
    }
    return result;
}

/// Detect whether a function contains a loop (backward jump in bytecode).
/// Functions with loops iterate over data — their computation scales with
/// input size, making WASM trampolines worthwhile even for small instruction counts.
pub fn detectHasLoop(instructions: anytype) bool {
    for (instructions) |instr| {
        if (instr.getJumpTarget()) |target| {
            if (target <= instr.pc) return true; // backward jump = loop
        }
    }
    return false;
}

/// Detect whether a function contains bitwise operations (and, or, xor, shl, sar, shr).
/// Functions with bitwise ops in f64 tier pay heavy fptosi/sitofp conversion overhead.
/// V8 JIT handles these natively as i32 via runtime type specialization.
pub fn detectHasBitwise(instructions: anytype) bool {
    for (instructions) |instr| {
        const handler = getHandler(instr.opcode);
        if (handler.requires_i32) return true;
    }
    return false;
}

/// Max fields per struct arg (bail out for wide objects — copy overhead eats the gain)
pub const MAX_STRUCT_FIELDS = 16;

/// Struct arg info: which args are struct-typed and what field atoms they access
pub const StructArgInfo = struct {
    /// Bitmask: bit N set = arg N is a struct-typed object
    struct_args: u8,
    /// Bitmask: bit N set = arg N is an array of struct-typed objects
    /// (accessed via get_array_el → get_field pattern)
    array_of_struct_args: u8,
    /// Per-arg field atoms (up to MAX_STRUCT_FIELDS per arg, up to 8 args)
    /// Stored as atom indices. field_counts[i] = number of fields for arg i.
    field_atoms: [8][MAX_STRUCT_FIELDS]u32,
    field_counts: [8]u8,

    pub fn fieldCount(self: *const StructArgInfo, arg_idx: u3) u8 {
        return self.field_counts[arg_idx];
    }
};

/// Detect which function arguments are struct-typed objects (accessed via get_field).
/// Uses stack simulation to trace get_arg{N} → get_field(atom) patterns.
/// Returns null if no struct args found, or if any get_field is on a non-arg value
/// (e.g., on a local variable or computed value — not safe to convert).
pub fn detectStructArgs(instructions: anytype, arg_count: u32) ?StructArgInfo {
    var stack: [128]i8 = undefined;
    var sp: usize = 0;
    var info = StructArgInfo{
        .struct_args = 0,
        .array_of_struct_args = 0,
        .field_atoms = undefined,
        .field_counts = [_]u8{0} ** 8,
    };
    var has_field_get = false;
    var has_unsafe_field_get = false;
    // Track which locals hold arg values: local_args[i] = arg index or -1
    // Enables detection of `var x = node; x.kind` as struct field read on arg
    var local_args: [64]i8 = .{-1} ** 64;

    for (instructions) |instr| {
        const handler = getHandler(instr.opcode);
        switch (handler.pattern) {
            .get_arg => {
                const idx: u8 = handler.index orelse switch (instr.operand) {
                    .arg => |a| @intCast(a),
                    .u8 => |a| a,
                    else => 255,
                };
                if (sp < stack.len) {
                    stack[sp] = if (idx < arg_count) @intCast(idx) else -1;
                    sp += 1;
                }
            },
            .get_loc, .get_loc_check => {
                const loc_idx: u8 = handler.index orelse switch (instr.operand) {
                    .loc => |a| @intCast(a),
                    .u8 => |a| a,
                    else => 255,
                };
                if (sp < stack.len) {
                    stack[sp] = if (loc_idx < local_args.len) local_args[loc_idx] else -1;
                    sp += 1;
                }
                continue; // skip default handler below
            },
            .put_loc, .put_loc_check => {
                if (sp >= 1) {
                    const val = stack[sp - 1];
                    sp -= 1;
                    const loc_idx: u8 = handler.index orelse switch (instr.operand) {
                        .loc => |a| @intCast(a),
                        .u8 => |a| a,
                        else => 255,
                    };
                    if (loc_idx < local_args.len) local_args[loc_idx] = val;
                }
                continue;
            },
            .set_loc => {
                // set_loc keeps value on stack but also stores to local
                if (sp >= 1) {
                    const loc_idx: u8 = handler.index orelse switch (instr.operand) {
                        .loc => |a| @intCast(a),
                        .u8 => |a| a,
                        else => 255,
                    };
                    if (loc_idx < local_args.len) local_args[loc_idx] = stack[sp - 1];
                }
                continue;
            },
            .field_get => {
                has_field_get = true;
                if (sp >= 1) {
                    const obj_arg = stack[sp - 1];
                    // Stack encoding: 0..7 = direct arg N, 8..15 = element of arg N-8
                    const is_direct = obj_arg >= 0 and obj_arg < 8;
                    const is_element = obj_arg >= 8 and obj_arg < 16;
                    if (is_direct or is_element) {
                        const ai: u3 = if (is_element) @intCast(obj_arg - 8) else @intCast(obj_arg);
                        const atom: u32 = switch (instr.operand) {
                            .atom => |a| a,
                            else => {
                                has_unsafe_field_get = true;
                                stack[sp - 1] = -1;
                                continue;
                            },
                        };
                        // Add atom if not already tracked for this arg
                        const fc = info.field_counts[ai];
                        if (fc < MAX_STRUCT_FIELDS) {
                            var found = false;
                            for (info.field_atoms[ai][0..fc]) |existing| {
                                if (existing == atom) {
                                    found = true;
                                    break;
                                }
                            }
                            if (!found) {
                                info.field_atoms[ai][fc] = atom;
                                info.field_counts[ai] = fc + 1;
                            }
                        } else {
                            has_unsafe_field_get = true;
                        }
                        if (is_element) {
                            info.array_of_struct_args |= @as(u8, 1) << ai;
                        } else {
                            info.struct_args |= @as(u8, 1) << ai;
                        }
                    } else {
                        // get_field on a non-arg value (local, computed) — not safe
                        has_unsafe_field_get = true;
                    }
                    stack[sp - 1] = -1; // result is not an arg
                }
            },
            // Array element access: if array is arg N, result is "element of arg N"
            // (stack encoding: 8 + N), enabling get_array_el → get_field detection
            .array_get => {
                if (sp >= 2) {
                    const arr_val = stack[sp - 2];
                    sp -= 2;
                    if (sp < stack.len) {
                        // If array was a direct arg (0..7), result is element-of-arg (8..15)
                        stack[sp] = if (arr_val >= 0 and arr_val < 8) (arr_val + 8) else -1;
                        sp += 1;
                    }
                }
            },
            .array_get2 => {
                if (sp >= 2) { sp -= 1; if (sp < stack.len) { stack[sp] = -1; sp += 1; } }
            },
            .array_put => {
                if (sp >= 3) sp -= 3;
            },
            .push_const, .push_cpool, .push_bool => {
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .binary_arith, .binary_cmp, .bitwise_binary => {
                if (sp >= 2) { sp -= 1; stack[sp - 1] = -1; }
            },
            .unary, .lnot, .inc_dec, .always_false => {
                if (sp >= 1) { stack[sp - 1] = -1; }
            },
            .post_inc_dec => {
                if (sp >= 1) { stack[sp - 1] = -1; if (sp < stack.len) { stack[sp] = -1; sp += 1; } }
            },
            // get_loc/put_loc/set_loc handled above (with local-to-arg tracking)
            .get_loc_pair => {
                if (sp + 1 < stack.len) { stack[sp] = -1; stack[sp + 1] = -1; sp += 2; }
            },
            .put_arg => {
                if (sp >= 1) sp -= 1;
            },
            .set_arg => {},
            .stack_dup => {
                if (sp >= 1 and sp < stack.len) { stack[sp] = stack[sp - 1]; sp += 1; }
            },
            .stack_dup2 => {
                if (sp >= 2 and sp + 1 < stack.len) { stack[sp] = stack[sp - 2]; stack[sp + 1] = stack[sp - 1]; sp += 2; }
            },
            .stack_drop => {
                if (sp >= 1) sp -= 1;
            },
            .stack_swap => {
                if (sp >= 2) {
                    const tmp = stack[sp - 1];
                    stack[sp - 1] = stack[sp - 2];
                    stack[sp - 2] = tmp;
                }
            },
            .self_ref => {
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .call_self => {
                const argc: u32 = switch (instr.opcode) {
                    .call1 => 1, .call2 => 2, .call3 => 3, .call => instr.operand.u16, else => 1,
                };
                if (sp >= argc) sp -= argc;
                if (sp >= 1) sp -= 1;
                if (sp < stack.len) { stack[sp] = -1; sp += 1; }
            },
            .ret, .if_false, .if_true => {
                if (sp >= 1) sp -= 1;
            },
            .add_loc => {
                if (sp >= 1) sp -= 1;
            },
            .inc_loc, .dec_loc => {},
            .array_length => {
                if (sp >= 1) { stack[sp - 1] = -1; }
            },
            .nop, .goto_br, .set_loc_uninitialized, .tail_call_self => {},
            .unsupported => {
                applyUnsupportedStackEffect(instr, &stack, &sp);
            },
        }
    }

    // Must have at least one field_get on an arg, and NO unsafe field access
    if (!has_field_get or has_unsafe_field_get) return null;
    if (info.struct_args == 0 and info.array_of_struct_args == 0) return null;

    return info;
}

/// Allocation site info: a function creates objects with a known shape.
/// Detected from `object → define_field(atom)` sequences in bytecodes.
pub const AllocSiteInfo = struct {
    /// Field atoms in definition order (matches constructor argument order)
    field_atoms: [MAX_STRUCT_FIELDS]u32,
    field_count: u8,
    /// True when every field value comes directly from get_arg (pure pass-through).
    /// Only pass-through factories are safe for SOA (Int32Array can't store strings/objects).
    pass_through: bool = false,
    /// For pass-through: which get_arg index maps to each field.
    /// Allows correct mapping when field order ≠ param order.
    arg_indices: [MAX_STRUCT_FIELDS]u8 = .{0} ** MAX_STRUCT_FIELDS,
};

/// Detect object literal allocation sites in a function's bytecodes.
/// Looks for: `object → (value) → define_field(atom)` repeated sequences.
/// Returns the shape (field names in order) or null if no clean pattern found.
///
/// Only matches functions that create a SINGLE object literal with a fixed shape.
/// Multiple object literals or dynamic shapes return null.
pub fn detectAllocSites(instructions: anytype) ?AllocSiteInfo {
    var info = AllocSiteInfo{
        .field_atoms = undefined,
        .field_count = 0,
    };
    var in_object = false;
    var object_count: u32 = 0;
    var last_was_get_arg = false;
    var last_arg_idx: u8 = 0;
    var all_get_arg = true;

    for (instructions) |instr| {
        if (instr.opcode == .object) {
            object_count += 1;
            if (object_count > 1) return null; // multiple object literals — bail
            in_object = true;
            last_was_get_arg = false;
            continue;
        }
        if (in_object and instr.opcode == .define_field) {
            const atom: u32 = switch (instr.operand) {
                .atom => |a| a,
                else => return null, // dynamic field — bail
            };
            if (info.field_count >= MAX_STRUCT_FIELDS) return null; // too many fields
            info.field_atoms[info.field_count] = atom;
            if (!last_was_get_arg) all_get_arg = false;
            info.arg_indices[info.field_count] = if (last_was_get_arg) last_arg_idx else 0;
            info.field_count += 1;
            last_was_get_arg = false;
            continue;
        }
        // `return` after define_field sequence = factory function
        if (in_object and (instr.opcode == .@"return" or instr.opcode == .return_undef)) {
            break;
        }
        // Track get_arg and its index
        if (in_object) {
            switch (instr.opcode) {
                .get_arg0 => { last_was_get_arg = true; last_arg_idx = 0; },
                .get_arg1 => { last_was_get_arg = true; last_arg_idx = 1; },
                .get_arg2 => { last_was_get_arg = true; last_arg_idx = 2; },
                .get_arg3 => { last_was_get_arg = true; last_arg_idx = 3; },
                .get_arg => {
                    last_was_get_arg = true;
                    last_arg_idx = switch (instr.operand) {
                        .arg => |v| @intCast(v),
                        .u16 => |v| @intCast(v),
                        else => 0,
                    };
                },
                // Call after object means object is consumed by another function — not a simple factory
                .call0, .call1, .call2, .call3, .call, .call_method,
                .tail_call, .tail_call_method, .call_constructor => return null,
                else => { last_was_get_arg = false; },
            }
        }
    }

    // Require at least 3 fields — V8 already optimizes 1-2 field objects via
    // monomorphic hidden classes, making SOA getter/setter overhead a net loss.
    // SOA is also provenance-gated at source transform time (only factories with
    // `arr.push(factory(...))` get transformed). This threshold catches the rest.
    if (info.field_count < 3) return null;
    info.pass_through = all_get_arg;
    return info;
}

