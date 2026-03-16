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
    /// Unsupported in numeric mode
    unsupported,
};

/// Numeric handler definition — comptime-evaluated per opcode
pub const NumericHandler = struct {
    pattern: NumericPattern,
    /// C/LLVM operator string: "+", "-", "&", "<<", etc.
    op: ?[]const u8 = null,
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

        // ── Push booleans ───────────────────────────────────────
        .push_true => .{ .pattern = .push_bool, .value = 1 },
        .push_false => .{ .pattern = .push_bool, .value = 0 },

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
        .add => .{ .pattern = .binary_arith, .op = "+" },
        .sub => .{ .pattern = .binary_arith, .op = "-" },
        .mul => .{ .pattern = .binary_arith, .op = "*" },
        // div: produces float in JS (9/2=4.5), so marks introduces_float
        .div => .{ .pattern = .binary_arith, .op = "/", .introduces_float = true },
        .mod => .{ .pattern = .binary_arith, .op = "%" },

        // ── Bitwise (i32 only) ──────────────────────────────────
        .shl => .{ .pattern = .bitwise_binary, .op = "<<", .requires_i32 = true },
        .sar => .{ .pattern = .bitwise_binary, .op = ">>", .requires_i32 = true },
        .shr => .{ .pattern = .bitwise_binary, .op = ">>>", .requires_i32 = true },
        .@"and" => .{ .pattern = .bitwise_binary, .op = "&", .requires_i32 = true },
        .@"or" => .{ .pattern = .bitwise_binary, .op = "|", .requires_i32 = true },
        .@"xor" => .{ .pattern = .bitwise_binary, .op = "^", .requires_i32 = true },

        // ── Comparisons ─────────────────────────────────────────
        .lt => .{ .pattern = .binary_cmp, .op = "<" },
        .lte => .{ .pattern = .binary_cmp, .op = "<=" },
        .gt => .{ .pattern = .binary_cmp, .op = ">" },
        .gte => .{ .pattern = .binary_cmp, .op = ">=" },
        .eq, .strict_eq => .{ .pattern = .binary_cmp, .op = "==" },
        .neq, .strict_neq => .{ .pattern = .binary_cmp, .op = "!=" },

        // ── Unary ───────────────────────────────────────────────
        .neg => .{ .pattern = .unary, .op = "-" },
        .not => .{ .pattern = .unary, .op = "~", .requires_i32 = true },

        // ── Logical NOT ─────────────────────────────────────────
        .lnot => .{ .pattern = .lnot },

        // ── Get locals ──────────────────────────────────────────
        .get_loc0 => .{ .pattern = .get_loc, .index = 0 },
        .get_loc1 => .{ .pattern = .get_loc, .index = 1 },
        .get_loc2 => .{ .pattern = .get_loc, .index = 2 },
        .get_loc3 => .{ .pattern = .get_loc, .index = 3 },
        .get_loc, .get_loc8 => .{ .pattern = .get_loc },

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

        // ── Inc/Dec ─────────────────────────────────────────────
        .inc => .{ .pattern = .inc_dec, .is_inc = true },
        .dec => .{ .pattern = .inc_dec, .is_inc = false },

        // ── Stack ops ───────────────────────────────────────────
        .dup => .{ .pattern = .stack_dup },
        .drop => .{ .pattern = .stack_drop },
        .swap => .{ .pattern = .stack_swap },
        .nop => .{ .pattern = .nop },

        // ── Self-reference (recursive calls) ────────────────────
        .get_var_ref0, .get_var, .get_var_undef => .{ .pattern = .self_ref },

        // ── Calls ───────────────────────────────────────────────
        .call1, .call2, .call3, .call => .{ .pattern = .call_self },
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

        // ── Everything else ─────────────────────────────────────
        else => .{ .pattern = .unsupported },
    };
}

/// Analyze a function's opcodes at comptime to determine which numeric
/// tier it qualifies for. Returns null if unsupported.
///
/// Decision logic:
///   1. If ANY opcode is `.unsupported` → null (can't compile standalone)
///   2. If ANY opcode has `introduces_float` (div) → f64 tier
///   3. If ALL opcodes are numeric-compatible and none introduce float → i32 tier
///   4. Must have at least one "computing" opcode to avoid pass-through functions
pub fn analyzeFunction(instructions: anytype) ?ValueKind {
    var has_computing_op = false;
    var needs_float = false;
    var has_i32_only = false;

    for (instructions) |instr| {
        const handler = getHandler(instr.opcode);
        if (handler.pattern == .unsupported) return null;

        if (handler.introduces_float) needs_float = true;
        if (handler.requires_i32) has_i32_only = true;

        switch (handler.pattern) {
            .binary_arith, .binary_cmp, .bitwise_binary,
            .unary, .inc_dec, .lnot, .push_const,
            .push_bool, .call_self, .tail_call_self,
            .add_loc, .inc_loc,
            => has_computing_op = true,
            else => {},
        }
    }

    if (!has_computing_op) return null;

    // If function uses div (introduces float) AND bitwise ops (requires i32),
    // we can't cleanly compile it in either tier → reject
    if (needs_float and has_i32_only) return null;

    if (needs_float) return .f64;
    return .i32;
}

/// Check if a pattern proves the function does computation (not just pass-through)
pub fn isComputingPattern(pattern: NumericPattern) bool {
    return switch (pattern) {
        .binary_arith, .binary_cmp, .bitwise_binary,
        .unary, .inc_dec, .lnot, .push_const,
        .push_bool, .call_self, .tail_call_self,
        .add_loc, .inc_loc,
        => true,
        else => false,
    };
}

// ============================================================================
// Comptime LLVM instruction selection
// ============================================================================

/// Select the right LLVM binary arithmetic instruction based on ValueKind.
/// This is evaluated at comptime when the codegen is monomorphized.
pub fn selectArithOp(comptime kind: ValueKind, comptime op: []const u8) ArithOp {
    return switch (kind) {
        .i32 => switch (op[0]) {
            '+' => .{ .i32_op = .add },
            '-' => .{ .i32_op = .sub },
            '*' => .{ .i32_op = .mul },
            '%' => .{ .i32_op = .srem },
            '/' => .{ .i32_op = .sdiv }, // unreachable for i32 tier (div rejected)
            else => @compileError("unknown arith op: " ++ op),
        },
        .f64 => switch (op[0]) {
            '+' => .{ .f64_op = .fadd },
            '-' => .{ .f64_op = .fsub },
            '*' => .{ .f64_op = .fmul },
            '/' => .{ .f64_op = .fdiv },
            '%' => .{ .f64_op = .frem },
            else => @compileError("unknown arith op: " ++ op),
        },
    };
}

pub const I32ArithKind = enum { add, sub, mul, sdiv, srem };
pub const F64ArithKind = enum { fadd, fsub, fmul, fdiv, frem };

pub const ArithOp = union {
    i32_op: I32ArithKind,
    f64_op: F64ArithKind,
};

/// Comparison predicate mapping for ICmp (i32) vs FCmp (f64)
pub fn selectCmpOp(comptime kind: ValueKind, comptime op: []const u8) CmpOp {
    _ = kind; // both tiers use same op strings
    if (std.mem.eql(u8, op, "<")) return .lt;
    if (std.mem.eql(u8, op, "<=")) return .le;
    if (std.mem.eql(u8, op, ">")) return .gt;
    if (std.mem.eql(u8, op, ">=")) return .ge;
    if (std.mem.eql(u8, op, "==")) return .eq;
    if (std.mem.eql(u8, op, "!=")) return .ne;
    @compileError("unknown cmp op: " ++ op);
}

pub const CmpOp = enum { lt, le, gt, ge, eq, ne };
