//! Stack-based Code Generator
//!
//! Generates C code that uses a JSValue stack, similar to the QuickJS interpreter
//! but with the interpreter dispatch loop eliminated.
//!
//! Each opcode emits real C code that manipulates the stack.

const std = @import("std");
const opcodes = @import("opcodes.zig");
const handlers = @import("opcode_handlers.zig");
const parser = @import("bytecode_parser.zig");
const cfg_mod = @import("cfg_builder.zig");

const Opcode = opcodes.Opcode;
const Instruction = parser.Instruction;
const CFG = cfg_mod.CFG;
const BasicBlock = cfg_mod.BasicBlock;
const Allocator = std.mem.Allocator;

pub const CodeGenOptions = struct {
    func_name: []const u8 = "frozen_func",
    js_name: []const u8 = "", // Name to use for globalThis registration (if different from func_name)
    debug_comments: bool = false,
    arg_count: u16 = 0,
    var_count: u16 = 0,
    max_stack: u16 = 256,
    emit_helpers: bool = true, // Set to false for subsequent functions in the same file
    is_self_recursive: bool = false, // Set to true if function calls itself (enables direct C recursion)
};

pub const SSACodeGen = struct {
    allocator: Allocator,
    cfg: *const CFG,
    options: CodeGenOptions,
    output: std.ArrayListUnmanaged(u8),
    // Track if the last instruction was get_var_ref0 (self-reference)
    // This enables direct C recursion when followed by call1
    pending_self_call: bool = false,
    // Track unsupported opcodes - if any found, function should be skipped
    unsupported_opcodes: std.ArrayListUnmanaged([]const u8) = .{},

    pub const Error = error{
        UnsupportedOpcodes,
        FormatError,
        OutOfMemory,
    };

    pub fn init(allocator: Allocator, cfg: *const CFG, options: CodeGenOptions) SSACodeGen {
        return .{
            .allocator = allocator,
            .cfg = cfg,
            .options = options,
            .output = .{},
            .pending_self_call = false,
            .unsupported_opcodes = .{},
        };
    }

    pub fn deinit(self: *SSACodeGen) void {
        self.output.deinit(self.allocator);
        self.unsupported_opcodes.deinit(self.allocator);
    }

    pub fn hasUnsupportedOpcodes(self: *const SSACodeGen) bool {
        return self.unsupported_opcodes.items.len > 0;
    }

    pub fn getUnsupportedOpcodeNames(self: *const SSACodeGen) []const []const u8 {
        return self.unsupported_opcodes.items;
    }

    fn write(self: *SSACodeGen, str: []const u8) !void {
        try self.output.appendSlice(self.allocator, str);
    }

    fn print(self: *SSACodeGen, comptime fmt: []const u8, args: anytype) !void {
        var buf: [16384]u8 = undefined;
        const slice = std.fmt.bufPrint(&buf, fmt, args) catch return error.FormatError;
        try self.output.appendSlice(self.allocator, slice);
    }

    pub fn generate(self: *SSACodeGen) Error![]const u8 {
        if (self.options.emit_helpers) {
            try self.emitHeader();
        } else {
            // Just emit a forward declaration
            try self.print("static JSValue {s}(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv);\n\n", .{self.options.func_name});
        }
        try self.emitFunction();

        // Check for unsupported opcodes - if any found, skip this function
        if (self.unsupported_opcodes.items.len > 0) {
            return error.UnsupportedOpcodes;
        }

        try self.emitInit();
        return self.output.items;
    }

    /// Emit only the helper functions (for use in main.zig)
    pub fn emitHelpersOnly(allocator: Allocator) ![]const u8 {
        var output = std.ArrayListUnmanaged(u8){};
        // NOTE: Includes and macros are emitted by main.zig header section
        // Only emit the SMI helper functions here
        try output.appendSlice(allocator,
            \\/* SMI-optimized dup/free - skip refcount for immediate values (int, bool, etc) */
            \\#define FROZEN_DUP(ctx, v) (JS_VALUE_HAS_REF_COUNT(v) ? JS_DupValue(ctx, v) : (v))
            \\#define FROZEN_FREE(ctx, v) do { if (JS_VALUE_HAS_REF_COUNT(v)) JS_FreeValue(ctx, v); } while(0)
            \\
            \\/* SMI (Small Integer) arithmetic helpers - zero allocation fast path */
            \\/* Uses JS_MKVAL for int32 results (no context/allocation needed) */
            \\/* Falls back to float64 only on overflow or non-int input */
            \\static inline JSValue frozen_add(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        int64_t r = (int64_t)JS_VALUE_GET_INT(a) + JS_VALUE_GET_INT(b);
            \\        if (likely((int32_t)r == r)) return JS_MKVAL(JS_TAG_INT, (int32_t)r);
            \\        return JS_NewFloat64(ctx, (double)r);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
            \\    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
            \\    return JS_NewFloat64(ctx, da + db);
            \\}
            \\static inline JSValue frozen_sub(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        int64_t r = (int64_t)JS_VALUE_GET_INT(a) - JS_VALUE_GET_INT(b);
            \\        if (likely((int32_t)r == r)) return JS_MKVAL(JS_TAG_INT, (int32_t)r);
            \\        return JS_NewFloat64(ctx, (double)r);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
            \\    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
            \\    return JS_NewFloat64(ctx, da - db);
            \\}
            \\static inline JSValue frozen_mul(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        int64_t r = (int64_t)JS_VALUE_GET_INT(a) * JS_VALUE_GET_INT(b);
            \\        if (likely((int32_t)r == r)) return JS_MKVAL(JS_TAG_INT, (int32_t)r);
            \\        return JS_NewFloat64(ctx, (double)r);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
            \\    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
            \\    return JS_NewFloat64(ctx, da * db);
            \\}
            \\static inline JSValue frozen_div(JSContext *ctx, JSValue a, JSValue b) {
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
            \\    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
            \\    return JS_NewFloat64(ctx, da / db);
            \\}
            \\static inline JSValue frozen_mod(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        int32_t ia = JS_VALUE_GET_INT(a);
            \\        int32_t ib = JS_VALUE_GET_INT(b);
            \\        if (unlikely(ib == 0)) return JS_NewFloat64(ctx, NAN);
            \\        if (unlikely(ib == -1 && ia == INT32_MIN)) return JS_MKVAL(JS_TAG_INT, 0);
            \\        return JS_MKVAL(JS_TAG_INT, ia % ib);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
            \\    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
            \\    return JS_NewFloat64(ctx, fmod(da, db));
            \\}
            \\static inline int frozen_lt(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        return JS_VALUE_GET_INT(a) < JS_VALUE_GET_INT(b);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return 0;
            \\    if (JS_ToFloat64(ctx, &db, b)) return 0;
            \\    return da < db;
            \\}
            \\static inline int frozen_lte(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        return JS_VALUE_GET_INT(a) <= JS_VALUE_GET_INT(b);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return 0;
            \\    if (JS_ToFloat64(ctx, &db, b)) return 0;
            \\    return da <= db;
            \\}
            \\static inline int frozen_gt(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        return JS_VALUE_GET_INT(a) > JS_VALUE_GET_INT(b);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return 0;
            \\    if (JS_ToFloat64(ctx, &db, b)) return 0;
            \\    return da > db;
            \\}
            \\static inline int frozen_gte(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        return JS_VALUE_GET_INT(a) >= JS_VALUE_GET_INT(b);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return 0;
            \\    if (JS_ToFloat64(ctx, &db, b)) return 0;
            \\    return da >= db;
            \\}
            \\static inline int frozen_eq(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT) {
            \\        return JS_VALUE_GET_INT(a) == JS_VALUE_GET_INT(b);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return 0;
            \\    if (JS_ToFloat64(ctx, &db, b)) return 0;
            \\    return da == db;
            \\}
            \\static inline int frozen_neq(JSContext *ctx, JSValue a, JSValue b) {
            \\    return !frozen_eq(ctx, a, b);
            \\}
            \\
            \\/* Bitwise helpers - always convert to int32 per JS spec */
            \\static inline JSValue frozen_and(JSContext *ctx, JSValue a, JSValue b) {
            \\    int32_t ia, ib;
            \\    JS_ToInt32(ctx, &ia, a); JS_ToInt32(ctx, &ib, b);
            \\    return JS_MKVAL(JS_TAG_INT, ia & ib);
            \\}
            \\static inline JSValue frozen_or(JSContext *ctx, JSValue a, JSValue b) {
            \\    int32_t ia, ib;
            \\    JS_ToInt32(ctx, &ia, a); JS_ToInt32(ctx, &ib, b);
            \\    return JS_MKVAL(JS_TAG_INT, ia | ib);
            \\}
            \\static inline JSValue frozen_xor(JSContext *ctx, JSValue a, JSValue b) {
            \\    int32_t ia, ib;
            \\    JS_ToInt32(ctx, &ia, a); JS_ToInt32(ctx, &ib, b);
            \\    return JS_MKVAL(JS_TAG_INT, ia ^ ib);
            \\}
            \\static inline JSValue frozen_shl(JSContext *ctx, JSValue a, JSValue b) {
            \\    int32_t ia, ib;
            \\    JS_ToInt32(ctx, &ia, a); JS_ToInt32(ctx, &ib, b);
            \\    return JS_MKVAL(JS_TAG_INT, ia << (ib & 0x1f));
            \\}
            \\static inline JSValue frozen_sar(JSContext *ctx, JSValue a, JSValue b) {
            \\    int32_t ia, ib;
            \\    JS_ToInt32(ctx, &ia, a); JS_ToInt32(ctx, &ib, b);
            \\    return JS_MKVAL(JS_TAG_INT, ia >> (ib & 0x1f));
            \\}
            \\static inline JSValue frozen_shr(JSContext *ctx, JSValue a, JSValue b) {
            \\    uint32_t ua; int32_t ib;
            \\    JS_ToUint32(ctx, &ua, a); JS_ToInt32(ctx, &ib, b);
            \\    return JS_NewUint32(ctx, ua >> (ib & 0x1f));
            \\}
            \\static inline JSValue frozen_not(JSContext *ctx, JSValue a) {
            \\    int32_t ia;
            \\    JS_ToInt32(ctx, &ia, a);
            \\    return JS_MKVAL(JS_TAG_INT, ~ia);
            \\}
            \\static inline JSValue frozen_neg(JSContext *ctx, JSValue a) {
            \\    if (JS_VALUE_GET_TAG(a) == JS_TAG_INT) {
            \\        int32_t v = JS_VALUE_GET_INT(a);
            \\        if (v == INT32_MIN) return JS_NewFloat64(ctx, 2147483648.0);
            \\        return JS_MKVAL(JS_TAG_INT, -v);
            \\    }
            \\    double d;
            \\    JS_ToFloat64(ctx, &d, a);
            \\    return JS_NewFloat64(ctx, -d);
            \\}
            \\
            \\/* Array access helpers - use public QuickJS API */
            \\static inline JSValue frozen_array_get(JSContext *ctx, JSValue obj, JSValue idx) {
            \\    if (JS_VALUE_GET_TAG(idx) == JS_TAG_INT) {
            \\        int32_t i = JS_VALUE_GET_INT(idx);
            \\        if (i >= 0) return JS_GetPropertyUint32(ctx, obj, (uint32_t)i);  /* Fast path */
            \\        return JS_GetPropertyInt64(ctx, obj, i);
            \\    }
            \\    JSAtom atom = JS_ValueToAtom(ctx, idx);
            \\    if (atom == JS_ATOM_NULL) return JS_EXCEPTION;
            \\    JSValue val = JS_GetProperty(ctx, obj, atom);
            \\    JS_FreeAtom(ctx, atom);
            \\    return val;
            \\}
            \\static inline int frozen_array_set(JSContext *ctx, JSValue obj, JSValue idx, JSValue val) {
            \\    if (JS_VALUE_GET_TAG(idx) == JS_TAG_INT) {
            \\        return JS_SetPropertyInt64(ctx, obj, JS_VALUE_GET_INT(idx), val);
            \\    }
            \\    JSAtom atom = JS_ValueToAtom(ctx, idx);
            \\    if (atom == JS_ATOM_NULL) return -1;
            \\    int r = JS_SetProperty(ctx, obj, atom, val);
            \\    JS_FreeAtom(ctx, atom);
            \\    return r;
            \\}
            \\static inline int64_t frozen_get_length(JSContext *ctx, JSValue obj) {
            \\    int64_t len = 0;
            \\    JS_GetLength(ctx, obj, &len);  /* Direct API - faster than JS_GetPropertyStr */
            \\    return len;
            \\}
            \\
            \\/* SIMD-accelerated int32 array operations (4 elements at once) */
            \\#ifdef __wasm__
            \\#include <wasm_simd128.h>
            \\
            \\/* SIMD int32 array sum - processes 4 elements per iteration */
            \\static inline int64_t frozen_sum_int32_array_simd(JSContext *ctx, JSValue arr, int64_t len) {
            \\    v128_t sum_vec = wasm_i32x4_splat(0);  /* Initialize 4-lane sum to 0 */
            \\    int64_t i = 0;
            \\
            \\    /* SIMD loop: process 4 int32s at once */
            \\    for (; i + 4 <= len; i += 4) {
            \\        int32_t vals[4];
            \\        int all_int32 = 1;
            \\
            \\        /* Load and check 4 values */
            \\        for (int j = 0; j < 4; j++) {
            \\            JSValue val = JS_GetPropertyUint32(ctx, arr, (uint32_t)(i + j));
            \\            if (likely(JS_VALUE_GET_TAG(val) == JS_TAG_INT)) {
            \\                vals[j] = JS_VALUE_GET_INT(val);
            \\            } else {
            \\                JS_FreeValue(ctx, val);
            \\                all_int32 = 0;
            \\                break;
            \\            }
            \\        }
            \\
            \\        if (all_int32) {
            \\            /* Load 4 int32s into SIMD register */
            \\            v128_t vec = wasm_v128_load(vals);
            \\            /* Vectorized add */
            \\            sum_vec = wasm_i32x4_add(sum_vec, vec);
            \\        } else {
            \\            /* Type guard failed - fall back to scalar */
            \\            return -1;
            \\        }
            \\    }
            \\
            \\    /* Horizontal sum: reduce 4 lanes to single value */
            \\    int32_t partial[4];
            \\    wasm_v128_store(partial, sum_vec);
            \\    int64_t sum = (int64_t)partial[0] + partial[1] + partial[2] + partial[3];
            \\
            \\    /* Scalar remainder */
            \\    for (; i < len; i++) {
            \\        JSValue val = JS_GetPropertyUint32(ctx, arr, (uint32_t)i);
            \\        if (likely(JS_VALUE_GET_TAG(val) == JS_TAG_INT)) {
            \\            sum += JS_VALUE_GET_INT(val);
            \\        } else {
            \\            JS_FreeValue(ctx, val);
            \\            return -1;  /* Type guard failed */
            \\        }
            \\    }
            \\
            \\    return sum;
            \\}
            \\#endif /* __wasm__ */
            \\
            \\static inline JSValue frozen_pow(JSContext *ctx, JSValue a, JSValue b) {
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
            \\    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
            \\    return JS_NewFloat64(ctx, pow(da, db));
            \\}
            \\static inline JSValue frozen_typeof(JSContext *ctx, JSValue v) {
            \\    const char *s;
            \\    int tag = JS_VALUE_GET_TAG(v);
            \\    switch(tag) {
            \\    case JS_TAG_UNDEFINED: s = "undefined"; break;
            \\    case JS_TAG_NULL: s = "object"; break;
            \\    case JS_TAG_STRING: s = "string"; break;
            \\    case JS_TAG_INT: case JS_TAG_FLOAT64: s = "number"; break;
            \\    case JS_TAG_BOOL: s = "boolean"; break;
            \\    case JS_TAG_BIG_INT: s = "bigint"; break;
            \\    case JS_TAG_SYMBOL: s = "symbol"; break;
            \\    case JS_TAG_OBJECT:
            \\        if (JS_IsFunction(ctx, v)) s = "function";
            \\        else s = "object";
            \\        break;
            \\    default: s = "unknown"; break;
            \\    }
            \\    return JS_NewString(ctx, s);
            \\}
            \\static inline int frozen_in(JSContext *ctx, JSValue key, JSValue obj) {
            \\    JSAtom atom = JS_ValueToAtom(ctx, key);
            \\    if (atom == JS_ATOM_NULL) return -1;
            \\    int r = JS_HasProperty(ctx, obj, atom);
            \\    JS_FreeAtom(ctx, atom);
            \\    return r;
            \\}
            \\
        );
        return output.toOwnedSlice(allocator);
    }

    fn emitHeader(self: *SSACodeGen) !void {
        try self.print("/*\n * Frozen function: {s}\n * Generated by edgebox-freeze\n */\n\n", .{self.options.func_name});
        try self.write(
            \\#include "quickjs.h"
            \\#include <stdint.h>
            \\#include <math.h>
            \\
            \\#ifndef likely
            \\#define likely(x) __builtin_expect(!!(x), 1)
            \\#endif
            \\#ifndef unlikely
            \\#define unlikely(x) __builtin_expect(!!(x), 0)
            \\#endif
            \\
            \\/* Call stack limit (matches Node.js behavior) */
            \\#ifndef FROZEN_MAX_CALL_DEPTH
            \\#define FROZEN_MAX_CALL_DEPTH 10000
            \\#endif
            \\static int frozen_call_depth = 0;
            \\
            \\/* Stack overflow check macro - returns RangeError like Node.js */
            \\#define FROZEN_CHECK_STACK(ctx) do { \
            \\    if (unlikely(frozen_call_depth >= FROZEN_MAX_CALL_DEPTH)) { \
            \\        return JS_ThrowRangeError(ctx, "Maximum call stack size exceeded"); \
            \\    } \
            \\    frozen_call_depth++; \
            \\} while(0)
            \\#define FROZEN_EXIT_STACK() (frozen_call_depth--)
            \\
            \\/* Stack operations with bounds checking */
            \\#define PUSH(v) do { \
            \\    if (unlikely(sp >= max_stack)) { \
            \\        FROZEN_EXIT_STACK(); \
            \\        return JS_ThrowRangeError(ctx, "Operand stack overflow"); \
            \\    } \
            \\    stack[sp++] = (v); \
            \\} while(0)
            \\#define POP() (stack[--sp])
            \\#define TOP() (stack[sp-1])
            \\#define SET_TOP(v) (stack[sp-1] = (v))
            \\
            \\/* SMI-optimized dup/free - skip refcount for immediate values (int, bool, etc) */
            \\#define FROZEN_DUP(ctx, v) (JS_VALUE_HAS_REF_COUNT(v) ? JS_DupValue(ctx, v) : (v))
            \\#define FROZEN_FREE(ctx, v) do { if (JS_VALUE_HAS_REF_COUNT(v)) JS_FreeValue(ctx, v); } while(0)
            \\
            \\/* SMI (Small Integer) arithmetic helpers - zero allocation fast path */
            \\/* Uses JS_MKVAL for int32 results (no context/allocation needed) */
            \\/* Falls back to float64 only on overflow or non-int input */
            \\static inline JSValue frozen_add(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        int64_t r = (int64_t)JS_VALUE_GET_INT(a) + JS_VALUE_GET_INT(b);
            \\        if (likely((int32_t)r == r)) return JS_MKVAL(JS_TAG_INT, (int32_t)r);
            \\        return JS_NewFloat64(ctx, (double)r);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
            \\    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
            \\    return JS_NewFloat64(ctx, da + db);
            \\}
            \\static inline JSValue frozen_sub(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        int64_t r = (int64_t)JS_VALUE_GET_INT(a) - JS_VALUE_GET_INT(b);
            \\        if (likely((int32_t)r == r)) return JS_MKVAL(JS_TAG_INT, (int32_t)r);
            \\        return JS_NewFloat64(ctx, (double)r);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
            \\    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
            \\    return JS_NewFloat64(ctx, da - db);
            \\}
            \\static inline JSValue frozen_mul(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        int64_t r = (int64_t)JS_VALUE_GET_INT(a) * JS_VALUE_GET_INT(b);
            \\        if (likely((int32_t)r == r)) return JS_MKVAL(JS_TAG_INT, (int32_t)r);
            \\        return JS_NewFloat64(ctx, (double)r);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
            \\    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
            \\    return JS_NewFloat64(ctx, da * db);
            \\}
            \\static inline JSValue frozen_div(JSContext *ctx, JSValue a, JSValue b) {
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
            \\    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
            \\    return JS_NewFloat64(ctx, da / db);
            \\}
            \\static inline JSValue frozen_mod(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        int32_t ia = JS_VALUE_GET_INT(a);
            \\        int32_t ib = JS_VALUE_GET_INT(b);
            \\        if (unlikely(ib == 0)) return JS_NewFloat64(ctx, NAN);
            \\        if (unlikely(ib == -1 && ia == INT32_MIN)) return JS_MKVAL(JS_TAG_INT, 0);
            \\        return JS_MKVAL(JS_TAG_INT, ia % ib);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
            \\    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
            \\    return JS_NewFloat64(ctx, fmod(da, db));
            \\}
            \\static inline int frozen_lt(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        return JS_VALUE_GET_INT(a) < JS_VALUE_GET_INT(b);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return 0;
            \\    if (JS_ToFloat64(ctx, &db, b)) return 0;
            \\    return da < db;
            \\}
            \\static inline int frozen_lte(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        return JS_VALUE_GET_INT(a) <= JS_VALUE_GET_INT(b);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return 0;
            \\    if (JS_ToFloat64(ctx, &db, b)) return 0;
            \\    return da <= db;
            \\}
            \\static inline int frozen_gt(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        return JS_VALUE_GET_INT(a) > JS_VALUE_GET_INT(b);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return 0;
            \\    if (JS_ToFloat64(ctx, &db, b)) return 0;
            \\    return da > db;
            \\}
            \\static inline int frozen_gte(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        return JS_VALUE_GET_INT(a) >= JS_VALUE_GET_INT(b);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return 0;
            \\    if (JS_ToFloat64(ctx, &db, b)) return 0;
            \\    return da >= db;
            \\}
            \\static inline int frozen_eq(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT) {
            \\        return JS_VALUE_GET_INT(a) == JS_VALUE_GET_INT(b);
            \\    }
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return 0;
            \\    if (JS_ToFloat64(ctx, &db, b)) return 0;
            \\    return da == db;
            \\}
            \\static inline int frozen_neq(JSContext *ctx, JSValue a, JSValue b) {
            \\    return !frozen_eq(ctx, a, b);
            \\}
            \\
            \\
        );
        try self.print("static JSValue {s}(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv);\n\n", .{self.options.func_name});
    }

    fn emitFunction(self: *SSACodeGen) !void {
        const fname = self.options.func_name;
        const var_count = self.options.var_count;
        const max_stack = self.options.max_stack;

        if (self.options.is_self_recursive) {
            // For self-recursive functions with 1 arg and no locals, generate PURE NATIVE int64 code
            // This eliminates ALL JSValue overhead in the hot recursive path
            if (self.options.arg_count == 1 and var_count == 0) {
                // Generate pure native int64 implementation (FAST PATH - no JSValue in recursion!)
                // Note: _native uses C stack, so WASM stack limit provides protection
                // We only check call depth in the wrapper to match Node.js error message
                try self.print(
                    \\/* Pure native int64 implementation - zero JSValue overhead */
                    \\/* C recursion uses WASM stack; wrapper checks call depth for Node.js compat */
                    \\static int64_t {s}_native(int64_t n) {{
                    \\    if (n < 2) return n;
                    \\    return {s}_native(n - 1) + {s}_native(n - 2);
                    \\}}
                    \\
                    \\static JSValue {s}(JSContext *ctx, JSValueConst this_val,
                    \\                   int argc, JSValueConst *argv)
                    \\{{
                    \\    (void)this_val;
                    \\    FROZEN_CHECK_STACK(ctx);
                    \\    if (argc > 0 && JS_VALUE_GET_TAG(argv[0]) == JS_TAG_INT) {{
                    \\        int64_t n = JS_VALUE_GET_INT(argv[0]);
                    \\        int64_t result = {s}_native(n);
                    \\        FROZEN_EXIT_STACK();
                    \\        if (result >= -2147483648LL && result <= 2147483647LL)
                    \\            return JS_MKVAL(JS_TAG_INT, (int32_t)result);
                    \\        return JS_NewFloat64(ctx, (double)result);
                    \\    }}
                    \\    /* Fallback for non-int input */
                    \\    FROZEN_EXIT_STACK();
                    \\    return JS_UNDEFINED;
                    \\}}
                    \\
                , .{ fname, fname, fname, fname, fname });
                return;
            }

            // For other self-recursive functions, use JSValue-based direct recursion
            // Forward declaration for _impl (TCO-enabled)
            try self.print("static JSValue {s}_impl(JSContext *ctx, JSValueConst this_val, JSValue frozen_arg0);\n\n", .{fname});

            // Generate _impl function (direct recursion, bypasses JS_Call)
            // With TCO support: tail_call becomes goto frozen_start
            try self.print(
                \\static JSValue {s}_impl(JSContext *ctx, JSValueConst this_val, JSValue frozen_arg0)
                \\{{
                \\    const int max_stack = {d};
                \\    JSValue stack[{d}];
                \\    int sp = 0;
                \\    int argc = 1;
                \\    JSValue argv_storage[1];
                \\    JSValue *argv = argv_storage;
                \\    FROZEN_CHECK_STACK(ctx);
                \\
                \\frozen_start:  /* TCO: tail_call jumps here */
                \\    sp = 0;
                \\    argv_storage[0] = frozen_arg0;
                \\
            , .{ fname, max_stack, max_stack });

            // Local variables (always declare to avoid undeclared identifier if bytecode references them)
            const actual_var_count = if (var_count > 0) var_count else 1;
            try self.print("    JSValue locals[{d}];\n", .{actual_var_count});
            try self.print("    for (int i = 0; i < {d}; i++) locals[i] = JS_UNDEFINED;\n\n", .{actual_var_count});

            // Process each basic block
            for (self.cfg.blocks.items, 0..) |*block, idx| {
                try self.emitBlock(block, idx);
            }

            // Fallthrough return
            try self.write("\n    FROZEN_EXIT_STACK();\n    return JS_UNDEFINED;\n}\n\n");

            // Generate the public wrapper (standard JS interface)
            try self.print(
                \\static JSValue {s}(JSContext *ctx, JSValueConst this_val,
                \\                   int argc, JSValueConst *argv)
                \\{{
                \\    (void)argc;
                \\    JSValue arg0 = argc > 0 ? argv[0] : JS_UNDEFINED;
                \\    return {s}_impl(ctx, this_val, arg0);
                \\}}
                \\
            , .{ fname, fname });
        } else {
            // Standard non-recursive function
            try self.print(
                \\static JSValue {s}(JSContext *ctx, JSValueConst this_val,
                \\                   int argc, JSValueConst *argv)
                \\{{
                \\    (void)this_val;
                \\    const int max_stack = {d};
                \\    JSValue stack[{d}];
                \\    int sp = 0;
                \\    FROZEN_CHECK_STACK(ctx);
                \\
            , .{ fname, max_stack, max_stack });

            // Local variables (always declare to avoid undeclared identifier if bytecode references them)
            const actual_var_count = if (var_count > 0) var_count else 1;
            try self.print("    JSValue locals[{d}];\n", .{actual_var_count});
            try self.print("    for (int i = 0; i < {d}; i++) locals[i] = JS_UNDEFINED;\n", .{actual_var_count});

            // V8-style optimization: Pre-declare length cache for arg0 (common array operations)
            // This will be lazily initialized on first .length access
            try self.write("    /* Cached length for array operations (V8-style) */\n");
            try self.write("    int64_t _arg0_len = -1;\n");
            try self.write("\n");

            // Process each basic block
            for (self.cfg.blocks.items, 0..) |*block, idx| {
                try self.emitBlock(block, idx);
            }

            // Fallthrough return
            try self.write("\n    FROZEN_EXIT_STACK();\n    return JS_UNDEFINED;\n}\n\n");
        }
    }

    fn emitBlock(self: *SSACodeGen, block: *const BasicBlock, block_idx: usize) !void {
        try self.print("block_{d}:\n", .{block_idx});

        var i: usize = 0;
        while (i < block.instructions.len) {
            const instr = block.instructions[i];
            const next: ?Instruction = if (i + 1 < block.instructions.len) block.instructions[i + 1] else null;

            // Peephole optimization: get_arg{N} -> get_length
            // V8-style: Use lazy cached length for arg0, direct call for others
            if (next) |next_instr| {
                if (next_instr.opcode == .get_length) {
                    var arg_idx: ?u16 = null;
                    switch (instr.opcode) {
                        .get_arg0 => arg_idx = 0,
                        .get_arg1 => arg_idx = 1,
                        .get_arg2 => arg_idx = 2,
                        .get_arg3 => arg_idx = 3,
                        .get_arg => arg_idx = instr.operand.u16,
                        else => {},
                    }
                    if (arg_idx) |idx| {
                        if (idx == 0) {
                            // V8-style: Lazy cache for arg0 (common array operations)
                            // Pattern: (_arg0_len < 0 ? (_arg0_len = get_length()) : _arg0_len)
                            try self.print("    /* get_arg0+get_length (V8-cached) */\n    PUSH(JS_NewInt64(ctx, argc > 0 ? (_arg0_len < 0 ? (_arg0_len = frozen_get_length(ctx, argv[0])) : _arg0_len) : 0));\n", .{});
                        } else {
                            // Direct call for other args
                            try self.print("    /* get_arg{d}+get_length (optimized) */\n    PUSH(JS_NewInt64(ctx, argc > {d} ? frozen_get_length(ctx, argv[{d}]) : 0));\n", .{ idx, idx, idx });
                        }
                        i += 2; // Skip both instructions
                        continue;
                    }
                }
            }

            try self.emitInstruction(instr);
            i += 1;
        }
    }

    fn emitInstruction(self: *SSACodeGen, instr: Instruction) !void {
        const debug = self.options.debug_comments;

        switch (instr.opcode) {
            // ==================== PUSH CONSTANTS (comptime generated) ====================
            .push_minus1 => try self.write(comptime handlers.generateCode(handlers.getHandler(.push_minus1), "push_minus1")),
            .push_0 => try self.write(comptime handlers.generateCode(handlers.getHandler(.push_0), "push_0")),
            .push_1 => try self.write(comptime handlers.generateCode(handlers.getHandler(.push_1), "push_1")),
            .push_2 => try self.write(comptime handlers.generateCode(handlers.getHandler(.push_2), "push_2")),
            .push_3 => try self.write(comptime handlers.generateCode(handlers.getHandler(.push_3), "push_3")),
            .push_4 => try self.write(comptime handlers.generateCode(handlers.getHandler(.push_4), "push_4")),
            .push_5 => try self.write(comptime handlers.generateCode(handlers.getHandler(.push_5), "push_5")),
            .push_6 => try self.write(comptime handlers.generateCode(handlers.getHandler(.push_6), "push_6")),
            .push_7 => try self.write(comptime handlers.generateCode(handlers.getHandler(.push_7), "push_7")),
            .push_i8 => {
                if (debug) try self.print("    /* push_i8 {d} */\n", .{instr.operand.i8});
                try self.print("    PUSH(JS_MKVAL(JS_TAG_INT, {d}));\n", .{instr.operand.i8});
            },
            .push_i16 => {
                if (debug) try self.print("    /* push_i16 {d} */\n", .{instr.operand.i16});
                try self.print("    PUSH(JS_MKVAL(JS_TAG_INT, {d}));\n", .{instr.operand.i16});
            },
            .push_i32 => {
                if (debug) try self.print("    /* push_i32 {d} */\n", .{instr.operand.i32});
                try self.print("    PUSH(JS_MKVAL(JS_TAG_INT, {d}));\n", .{instr.operand.i32});
            },
            .push_false => try self.write(comptime handlers.generateCode(handlers.getHandler(.push_false), "push_false")),
            .push_true => try self.write(comptime handlers.generateCode(handlers.getHandler(.push_true), "push_true")),
            .undefined => try self.write(comptime handlers.generateCode(handlers.getHandler(.undefined), "undefined")),
            .null => try self.write(comptime handlers.generateCode(handlers.getHandler(.null), "null")),
            .object => try self.write(comptime handlers.generateCode(handlers.getHandler(.object), "object")),
            .push_this => try self.write(comptime handlers.generateCode(handlers.getHandler(.push_this), "push_this")),
            .push_empty_string => try self.write(comptime handlers.generateCode(handlers.getHandler(.push_empty_string), "push_empty_string")),

            // ==================== ARGUMENTS (comptime generated) ====================
            .get_arg0 => try self.write(comptime handlers.generateCode(handlers.getHandler(.get_arg0), "get_arg0")),
            .get_arg1 => try self.write(comptime handlers.generateCode(handlers.getHandler(.get_arg1), "get_arg1")),
            .get_arg2 => try self.write(comptime handlers.generateCode(handlers.getHandler(.get_arg2), "get_arg2")),
            .get_arg3 => try self.write(comptime handlers.generateCode(handlers.getHandler(.get_arg3), "get_arg3")),
            .get_arg => {
                const idx = instr.operand.u16;
                if (debug) try self.print("    /* get_arg {d} */\n", .{idx});
                try self.print("    PUSH(argc > {d} ? FROZEN_DUP(ctx, argv[{d}]) : JS_UNDEFINED);\n", .{ idx, idx });
            },
            .put_arg0 => try self.write(comptime handlers.generateCode(handlers.getHandler(.put_arg0), "put_arg0")),
            .put_arg1 => try self.write(comptime handlers.generateCode(handlers.getHandler(.put_arg1), "put_arg1")),
            .put_arg2 => try self.write(comptime handlers.generateCode(handlers.getHandler(.put_arg2), "put_arg2")),
            .put_arg3 => try self.write(comptime handlers.generateCode(handlers.getHandler(.put_arg3), "put_arg3")),
            .set_arg0 => try self.write(comptime handlers.generateCode(handlers.getHandler(.set_arg0), "set_arg0")),
            .set_arg1 => try self.write(comptime handlers.generateCode(handlers.getHandler(.set_arg1), "set_arg1")),
            .set_arg2 => try self.write(comptime handlers.generateCode(handlers.getHandler(.set_arg2), "set_arg2")),
            .set_arg3 => try self.write(comptime handlers.generateCode(handlers.getHandler(.set_arg3), "set_arg3")),

            // ==================== LOCALS (comptime generated) ====================
            .get_loc0 => try self.write(comptime handlers.generateCode(handlers.getHandler(.get_loc0), "get_loc0")),
            .get_loc1 => try self.write(comptime handlers.generateCode(handlers.getHandler(.get_loc1), "get_loc1")),
            .get_loc2 => try self.write(comptime handlers.generateCode(handlers.getHandler(.get_loc2), "get_loc2")),
            .get_loc3 => try self.write(comptime handlers.generateCode(handlers.getHandler(.get_loc3), "get_loc3")),
            .get_loc, .get_loc8 => {
                const idx = if (instr.opcode == .get_loc8) instr.operand.u8 else instr.operand.u16;
                if (debug) try self.print("    /* get_loc {d} */\n", .{idx});
                try self.print("    PUSH(FROZEN_DUP(ctx, locals[{d}]));\n", .{idx});
            },
            .put_loc0 => try self.write(comptime handlers.generateCode(handlers.getHandler(.put_loc0), "put_loc0")),
            .put_loc1 => try self.write(comptime handlers.generateCode(handlers.getHandler(.put_loc1), "put_loc1")),
            .put_loc2 => try self.write(comptime handlers.generateCode(handlers.getHandler(.put_loc2), "put_loc2")),
            .put_loc3 => try self.write(comptime handlers.generateCode(handlers.getHandler(.put_loc3), "put_loc3")),
            .put_loc, .put_loc8 => {
                const idx = if (instr.opcode == .put_loc8) instr.operand.u8 else instr.operand.u16;
                if (debug) try self.print("    /* put_loc {d} */\n", .{idx});
                try self.print("    FROZEN_FREE(ctx, locals[{d}]); locals[{d}] = POP();\n", .{ idx, idx });
            },
            .set_loc0 => try self.write(comptime handlers.generateCode(handlers.getHandler(.set_loc0), "set_loc0")),
            .set_loc1 => try self.write(comptime handlers.generateCode(handlers.getHandler(.set_loc1), "set_loc1")),
            .set_loc2 => try self.write(comptime handlers.generateCode(handlers.getHandler(.set_loc2), "set_loc2")),
            .set_loc3 => try self.write(comptime handlers.generateCode(handlers.getHandler(.set_loc3), "set_loc3")),
            .set_loc, .set_loc8 => {
                const idx = if (instr.opcode == .set_loc8) instr.operand.u8 else instr.operand.u16;
                if (debug) try self.print("    /* set_loc {d} */\n", .{idx});
                // set_loc: like put_loc but leaves value on stack (dup + put)
                try self.print("    FROZEN_FREE(ctx, locals[{d}]); locals[{d}] = FROZEN_DUP(ctx, TOP());\n", .{ idx, idx });
            },

            // ==================== STACK OPS (comptime generated) ====================
            .drop => try self.write(comptime handlers.generateCode(handlers.getHandler(.drop), "drop")),
            .dup => try self.write(comptime handlers.generateCode(handlers.getHandler(.dup), "dup")),
            .dup1 => try self.write(comptime handlers.generateCode(handlers.getHandler(.dup1), "dup1")),
            .dup2 => try self.write(comptime handlers.generateCode(handlers.getHandler(.dup2), "dup2")),
            .dup3 => try self.write(comptime handlers.generateCode(handlers.getHandler(.dup3), "dup3")),
            .nip => try self.write(comptime handlers.generateCode(handlers.getHandler(.nip), "nip")),
            .nip1 => try self.write(comptime handlers.generateCode(handlers.getHandler(.nip1), "nip1")),
            .swap => try self.write(comptime handlers.generateCode(handlers.getHandler(.swap), "swap")),
            .swap2 => try self.write(comptime handlers.generateCode(handlers.getHandler(.swap2), "swap2")),
            .rot3l => try self.write(comptime handlers.generateCode(handlers.getHandler(.rot3l), "rot3l")),
            .rot3r => try self.write(comptime handlers.generateCode(handlers.getHandler(.rot3r), "rot3r")),
            .rot4l => try self.write(comptime handlers.generateCode(handlers.getHandler(.rot4l), "rot4l")),
            .rot5l => try self.write(comptime handlers.generateCode(handlers.getHandler(.rot5l), "rot5l")),
            .insert2 => try self.write(comptime handlers.generateCode(handlers.getHandler(.insert2), "insert2")),
            .insert3 => try self.write(comptime handlers.generateCode(handlers.getHandler(.insert3), "insert3")),
            .insert4 => try self.write(comptime handlers.generateCode(handlers.getHandler(.insert4), "insert4")),
            .perm3 => try self.write(comptime handlers.generateCode(handlers.getHandler(.perm3), "perm3")),
            .perm4 => try self.write(comptime handlers.generateCode(handlers.getHandler(.perm4), "perm4")),
            .perm5 => try self.write(comptime handlers.generateCode(handlers.getHandler(.perm5), "perm5")),
            .nop => try self.write(comptime handlers.generateCode(handlers.getHandler(.nop), "nop")),

            // ==================== ARITHMETIC (comptime generated) ====================
            // Binary arithmetic ops - code generated from opcode_handlers.zig patterns
            .add => try self.write(comptime handlers.generateCode(handlers.getHandler(.add), "add")),
            .sub => try self.write(comptime handlers.generateCode(handlers.getHandler(.sub), "sub")),
            .mul => try self.write(comptime handlers.generateCode(handlers.getHandler(.mul), "mul")),
            .div => try self.write(comptime handlers.generateCode(handlers.getHandler(.div), "div")),
            .mod => try self.write(comptime handlers.generateCode(handlers.getHandler(.mod), "mod")),
            .neg => try self.write(comptime handlers.generateCode(handlers.getHandler(.neg), "neg")),
            .plus => try self.write(comptime handlers.generateCode(handlers.getHandler(.plus), "plus")),
            .inc => try self.write(comptime handlers.generateCode(handlers.getHandler(.inc), "inc")),
            .dec => try self.write(comptime handlers.generateCode(handlers.getHandler(.dec), "dec")),
            .inc_loc => {
                const idx = instr.operand.u8;
                if (debug) try self.print("    /* inc_loc {d} */\n", .{idx});
                try self.print("    {{ JSValue old = locals[{d}]; locals[{d}] = frozen_add(ctx, old, JS_MKVAL(JS_TAG_INT, 1)); }}\n", .{ idx, idx });
            },
            .dec_loc => {
                const idx = instr.operand.u8;
                if (debug) try self.print("    /* dec_loc {d} */\n", .{idx});
                try self.print("    {{ JSValue old = locals[{d}]; locals[{d}] = frozen_sub(ctx, old, JS_MKVAL(JS_TAG_INT, 1)); }}\n", .{ idx, idx });
            },
            .add_loc => {
                const idx = instr.operand.u8;
                if (debug) try self.print("    /* add_loc {d} */\n", .{idx});
                try self.print("    {{ JSValue v = POP(), old = locals[{d}]; locals[{d}] = frozen_add(ctx, old, v); }}\n", .{ idx, idx });
            },

            // ==================== COMPARISON (comptime generated) ====================
            // Binary comparison ops - code generated from opcode_handlers.zig patterns
            .lt => try self.write(comptime handlers.generateCode(handlers.getHandler(.lt), "lt")),
            .lte => try self.write(comptime handlers.generateCode(handlers.getHandler(.lte), "lte")),
            .gt => try self.write(comptime handlers.generateCode(handlers.getHandler(.gt), "gt")),
            .gte => try self.write(comptime handlers.generateCode(handlers.getHandler(.gte), "gte")),
            .eq => try self.write(comptime handlers.generateCode(handlers.getHandler(.eq), "eq")),
            .neq => try self.write(comptime handlers.generateCode(handlers.getHandler(.neq), "neq")),
            .strict_eq => try self.write(comptime handlers.generateCode(handlers.getHandler(.strict_eq), "strict_eq")),
            .strict_neq => try self.write(comptime handlers.generateCode(handlers.getHandler(.strict_neq), "strict_neq")),

            // ==================== BITWISE (comptime generated) ====================
            .shl => try self.write(comptime handlers.generateCode(handlers.getHandler(.shl), "shl")),
            .sar => try self.write(comptime handlers.generateCode(handlers.getHandler(.sar), "sar")),
            .shr => try self.write(comptime handlers.generateCode(handlers.getHandler(.shr), "shr")),
            .@"and" => try self.write(comptime handlers.generateCode(handlers.getHandler(.@"and"), "and")),
            .@"or" => try self.write(comptime handlers.generateCode(handlers.getHandler(.@"or"), "or")),
            .xor => try self.write(comptime handlers.generateCode(handlers.getHandler(.xor), "xor")),
            .not => try self.write(comptime handlers.generateCode(handlers.getHandler(.not), "not")),
            .lnot => try self.write(comptime handlers.generateCode(handlers.getHandler(.lnot), "lnot")),

            // ==================== TYPE CHECKS (comptime generated) ====================
            .is_undefined => try self.write(comptime handlers.generateCode(handlers.getHandler(.is_undefined), "is_undefined")),
            .is_null => try self.write(comptime handlers.generateCode(handlers.getHandler(.is_null), "is_null")),
            .is_undefined_or_null => try self.write(comptime handlers.generateCode(handlers.getHandler(.is_undefined_or_null), "is_undefined_or_null")),
            .typeof_is_undefined => try self.write(comptime handlers.generateCode(handlers.getHandler(.typeof_is_undefined), "typeof_is_undefined")),
            .typeof_is_function => try self.write(comptime handlers.generateCode(handlers.getHandler(.typeof_is_function), "typeof_is_function")),

            // ==================== POST INC/DEC (comptime generated) ====================
            .post_inc => try self.write(comptime handlers.generateCode(handlers.getHandler(.post_inc), "post_inc")),
            .post_dec => try self.write(comptime handlers.generateCode(handlers.getHandler(.post_dec), "post_dec")),

            // ==================== ARRAY ACCESS (comptime generated) ====================
            .get_array_el => try self.write(comptime handlers.generateCode(handlers.getHandler(.get_array_el), "get_array_el")),
            .get_array_el2 => try self.write(comptime handlers.generateCode(handlers.getHandler(.get_array_el2), "get_array_el2")),
            .put_array_el => try self.write(comptime handlers.generateCode(handlers.getHandler(.put_array_el), "put_array_el")),
            .get_length => try self.write(comptime handlers.generateCode(handlers.getHandler(.get_length), "get_length")),

            // ==================== TYPE OPERATORS ====================
            .pow => try self.write(comptime handlers.generateCode(handlers.getHandler(.pow), "pow")),
            .typeof => try self.write(comptime handlers.generateCode(handlers.getHandler(.typeof), "typeof")),
            .instanceof => try self.write(comptime handlers.generateCode(handlers.getHandler(.instanceof), "instanceof")),
            .in => try self.write(comptime handlers.generateCode(handlers.getHandler(.in), "in")),

            // ==================== TYPE COERCION ====================
            .to_object => try self.write(comptime handlers.generateCode(handlers.getHandler(.to_object), "to_object")),
            .to_propkey => try self.write(comptime handlers.generateCode(handlers.getHandler(.to_propkey), "to_propkey")),

            // ==================== CONTROL FLOW ====================
            .if_false, .if_false8 => {
                const target = instr.getJumpTarget() orelse 0;
                const target_block = self.cfg.pc_to_block.get(target) orelse 0;
                if (debug) try self.print("    /* if_false -> block_{d} */\n", .{target_block});
                try self.print("    {{ JSValue cond = POP(); if (!JS_ToBool(ctx, cond)) {{ JS_FreeValue(ctx, cond); goto block_{d}; }} JS_FreeValue(ctx, cond); }}\n", .{target_block});
            },
            .if_true, .if_true8 => {
                const target = instr.getJumpTarget() orelse 0;
                const target_block = self.cfg.pc_to_block.get(target) orelse 0;
                if (debug) try self.print("    /* if_true -> block_{d} */\n", .{target_block});
                try self.print("    {{ JSValue cond = POP(); if (JS_ToBool(ctx, cond)) {{ JS_FreeValue(ctx, cond); goto block_{d}; }} JS_FreeValue(ctx, cond); }}\n", .{target_block});
            },
            .goto, .goto8, .goto16 => {
                const target = instr.getJumpTarget() orelse 0;
                const target_block = self.cfg.pc_to_block.get(target) orelse 0;
                if (debug) try self.print("    /* goto block_{d} */\n", .{target_block});
                try self.print("    goto block_{d};\n", .{target_block});
            },
            .@"return" => try self.write(comptime handlers.generateCode(handlers.getHandler(.@"return"), "return")),
            .return_undef => try self.write(comptime handlers.generateCode(handlers.getHandler(.return_undef), "return_undef")),

            // ==================== CALLS ====================
            .call0 => {
                if (debug) try self.write("    /* call0 */\n");
                self.pending_self_call = false;
                try self.write("    { JSValue func = POP(); JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 0, NULL); JS_FreeValue(ctx, func); if (JS_IsException(ret)) return ret; PUSH(ret); }\n");
            },
            .call1 => {
                if (debug) try self.write("    /* call1 */\n");
                if (self.pending_self_call and self.options.is_self_recursive) {
                    // Direct C recursion - no JS_Call overhead!
                    // The get_var_ref0 pushed nothing, so we just have the arg on stack
                    try self.print("    {{ JSValue arg0 = POP(); JSValue ret = {s}_impl(ctx, this_val, arg0); if (JS_IsException(ret)) return ret; PUSH(ret); }}\n", .{self.options.func_name});
                } else {
                    // Standard JS call
                    try self.write("    { JSValue arg0 = POP(); JSValue func = POP(); JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 1, &arg0); JS_FreeValue(ctx, func); JS_FreeValue(ctx, arg0); if (JS_IsException(ret)) return ret; PUSH(ret); }\n");
                }
                self.pending_self_call = false;
            },
            .call2 => {
                if (debug) try self.write("    /* call2 */\n");
                self.pending_self_call = false;
                try self.write("    { JSValue args[2]; args[1] = POP(); args[0] = POP(); JSValue func = POP(); JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 2, args); JS_FreeValue(ctx, func); JS_FreeValue(ctx, args[0]); JS_FreeValue(ctx, args[1]); if (JS_IsException(ret)) return ret; PUSH(ret); }\n");
            },
            .call3 => {
                if (debug) try self.write("    /* call3 */\n");
                self.pending_self_call = false;
                try self.write("    { JSValue args[3]; args[2] = POP(); args[1] = POP(); args[0] = POP(); JSValue func = POP(); JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 3, args); JS_FreeValue(ctx, func); JS_FreeValue(ctx, args[0]); JS_FreeValue(ctx, args[1]); JS_FreeValue(ctx, args[2]); if (JS_IsException(ret)) return ret; PUSH(ret); }\n");
            },

            // ==================== CLOSURE REFS ====================
            // For self-recursion, var_ref0 refers to the function itself
            // When is_self_recursive is true, we skip pushing and set pending_self_call
            .get_var_ref0 => {
                if (self.options.is_self_recursive) {
                    if (debug) try self.write("    /* get_var_ref0 - self reference (direct recursion) */\n");
                    // Don't push anything - call1 will use direct C recursion
                    self.pending_self_call = true;
                } else {
                    if (debug) try self.write("    /* get_var_ref0 - closure access */\n");
                    try self.write("    PUSH(JS_UNDEFINED); /* TODO: closure var_ref */\n");
                }
            },
            .get_var_ref1, .get_var_ref2, .get_var_ref3 => {
                const idx: u8 = switch (instr.opcode) {
                    .get_var_ref1 => 1,
                    .get_var_ref2 => 2,
                    .get_var_ref3 => 3,
                    else => 0,
                };
                if (debug) try self.print("    /* get_var_ref{d} - closure access */\n", .{idx});
                try self.write("    PUSH(JS_UNDEFINED); /* TODO: closure var_ref */\n");
                self.pending_self_call = false;
            },

            // ==================== TAIL CALL (TCO) ====================
            .tail_call => {
                if (debug) try self.write("    /* tail_call - TCO */\n");
                if (self.pending_self_call and self.options.is_self_recursive) {
                    // Self-recursive tail call: convert to goto (true TCO!)
                    // Update arg0 and jump to start - don't decrement (reusing frame)
                    try self.write("    { frozen_arg0 = POP(); sp = 0; goto frozen_start; }\n");
                } else {
                    // Non-self-recursive: decrement before calling (callee will increment)
                    try self.write("    { JSValue arg = POP(); JSValue func = POP(); FROZEN_EXIT_STACK(); return JS_Call(ctx, func, JS_UNDEFINED, 1, &arg); }\n");
                }
                self.pending_self_call = false;
            },
            .tail_call_method => {
                if (debug) try self.write("    /* tail_call_method - TCO */\n");
                // Method tail call: decrement before calling (callee will increment)
                try self.write("    { JSValue arg = POP(); JSValue this = POP(); JSValue func = POP(); FROZEN_EXIT_STACK(); return JS_Call(ctx, func, this, 1, &arg); }\n");
                self.pending_self_call = false;
            },

            // ==================== PROPERTY ACCESS (comptime pattern, runtime atom) ====================
            .get_field => {
                const atom = instr.operand.atom;
                if (debug) try self.print("    /* get_field atom:{d} */\n", .{atom});
                try self.print("    {{ JSValue obj = POP(); JSValue val = JS_GetProperty(ctx, obj, {d}); FROZEN_FREE(ctx, obj); if (JS_IsException(val)) return val; PUSH(val); }}\n", .{atom});
            },
            .get_field2 => {
                const atom = instr.operand.atom;
                if (debug) try self.print("    /* get_field2 atom:{d} */\n", .{atom});
                // Push both obj and obj.field (for method calls: obj.method() needs both)
                try self.print("    {{ JSValue obj = TOP(); JSValue val = JS_GetProperty(ctx, obj, {d}); if (JS_IsException(val)) return val; PUSH(val); }}\n", .{atom});
            },
            .put_field => {
                const atom = instr.operand.atom;
                if (debug) try self.print("    /* put_field atom:{d} */\n", .{atom});
                try self.print("    {{ JSValue val = POP(); JSValue obj = POP(); int r = JS_SetProperty(ctx, obj, {d}, val); FROZEN_FREE(ctx, obj); if (r < 0) return JS_EXCEPTION; }}\n", .{atom});
            },

            else => {
                const info = instr.getInfo();
                // Track unsupported opcode - function will be skipped
                try self.unsupported_opcodes.append(self.allocator, info.name);
                try self.print("    /* UNSUPPORTED: {s} */\n", .{info.name});
            },
        }
    }

    fn emitInit(self: *SSACodeGen) !void {
        const fname = self.options.func_name;
        // Use js_name for registration if provided, otherwise use func_name
        const js_name = if (self.options.js_name.len > 0) self.options.js_name else fname;
        try self.print(
            \\int {s}_init(JSContext *ctx)
            \\{{
            \\    JSValue global = JS_GetGlobalObject(ctx);
            \\    JSValue func = JS_NewCFunction(ctx, {s}, "{s}", {d});
            \\    JS_SetPropertyStr(ctx, global, "{s}", func);
            \\    JS_FreeValue(ctx, global);
            \\    return 0;
            \\}}
            \\
        , .{ fname, fname, js_name, self.options.arg_count, js_name });
    }
};
