//! Stack-based Code Generator
//!
//! Generates C code that uses a JSValue stack, similar to the QuickJS interpreter
//! but with the interpreter dispatch loop eliminated.
//!
//! Each opcode emits real C code that manipulates the stack.

const std = @import("std");
const opcodes = @import("opcodes.zig");
const parser = @import("bytecode_parser.zig");
const cfg_mod = @import("cfg_builder.zig");

const Opcode = opcodes.Opcode;
const Instruction = parser.Instruction;
const CFG = cfg_mod.CFG;
const BasicBlock = cfg_mod.BasicBlock;
const Allocator = std.mem.Allocator;

pub const CodeGenOptions = struct {
    func_name: []const u8 = "frozen_func",
    debug_comments: bool = false,
    arg_count: u16 = 0,
    var_count: u16 = 0,
    max_stack: u16 = 256,
    emit_helpers: bool = true, // Set to false for subsequent functions in the same file
};

pub const SSACodeGen = struct {
    allocator: Allocator,
    cfg: *const CFG,
    options: CodeGenOptions,
    output: std.ArrayListUnmanaged(u8),

    pub fn init(allocator: Allocator, cfg: *const CFG, options: CodeGenOptions) SSACodeGen {
        return .{
            .allocator = allocator,
            .cfg = cfg,
            .options = options,
            .output = .{},
        };
    }

    pub fn deinit(self: *SSACodeGen) void {
        self.output.deinit(self.allocator);
    }

    fn write(self: *SSACodeGen, str: []const u8) !void {
        try self.output.appendSlice(self.allocator, str);
    }

    fn print(self: *SSACodeGen, comptime fmt: []const u8, args: anytype) !void {
        var buf: [16384]u8 = undefined;
        const slice = std.fmt.bufPrint(&buf, fmt, args) catch return error.FormatError;
        try self.output.appendSlice(self.allocator, slice);
    }

    pub fn generate(self: *SSACodeGen) ![]const u8 {
        if (self.options.emit_helpers) {
            try self.emitHeader();
        } else {
            // Just emit a forward declaration
            try self.print("static JSValue {s}(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv);\n\n", .{self.options.func_name});
        }
        try self.emitFunction();
        try self.emitInit();
        return self.output.items;
    }

    /// Emit only the helper functions (for use in main.zig)
    pub fn emitHelpersOnly(allocator: Allocator) ![]const u8 {
        var output = std.ArrayListUnmanaged(u8){};
        try output.appendSlice(allocator,
            \\/* Integer arithmetic helpers with proper fallback */
            \\/* Fast path: int32 arithmetic. Slow path: convert to float64 */
            \\static inline JSValue frozen_add(JSContext *ctx, JSValue a, JSValue b) {
            \\    /* Fast path: both are int32 */
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        int64_t r = (int64_t)JS_VALUE_GET_INT(a) + JS_VALUE_GET_INT(b);
            \\        if (likely((int32_t)r == r)) return JS_NewInt32(ctx, (int32_t)r);
            \\        /* Overflow: promote to float64 */
            \\        return JS_NewFloat64(ctx, (double)r);
            \\    }
            \\    /* Slow path: convert both to float64 and add */
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
            \\    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
            \\    return JS_NewFloat64(ctx, da + db);
            \\}
            \\static inline JSValue frozen_sub(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        int64_t r = (int64_t)JS_VALUE_GET_INT(a) - JS_VALUE_GET_INT(b);
            \\        if (likely((int32_t)r == r)) return JS_NewInt32(ctx, (int32_t)r);
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
            \\        if (likely((int32_t)r == r)) return JS_NewInt32(ctx, (int32_t)r);
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
            \\        if (unlikely(ib == -1 && ia == INT32_MIN)) return JS_NewInt32(ctx, 0);
            \\        return JS_NewInt32(ctx, ia % ib);
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
            \\/* Stack operations */
            \\#define PUSH(v) (stack[sp++] = (v))
            \\#define POP() (stack[--sp])
            \\#define TOP() (stack[sp-1])
            \\#define SET_TOP(v) (stack[sp-1] = (v))
            \\
            \\/* Integer arithmetic helpers with proper fallback */
            \\/* Fast path: int32 arithmetic. Slow path: convert to float64 */
            \\static inline JSValue frozen_add(JSContext *ctx, JSValue a, JSValue b) {
            \\    /* Fast path: both are int32 */
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        int64_t r = (int64_t)JS_VALUE_GET_INT(a) + JS_VALUE_GET_INT(b);
            \\        if (likely((int32_t)r == r)) return JS_NewInt32(ctx, (int32_t)r);
            \\        /* Overflow: promote to float64 */
            \\        return JS_NewFloat64(ctx, (double)r);
            \\    }
            \\    /* Slow path: convert both to float64 and add */
            \\    double da, db;
            \\    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
            \\    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
            \\    return JS_NewFloat64(ctx, da + db);
            \\}
            \\static inline JSValue frozen_sub(JSContext *ctx, JSValue a, JSValue b) {
            \\    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
            \\        int64_t r = (int64_t)JS_VALUE_GET_INT(a) - JS_VALUE_GET_INT(b);
            \\        if (likely((int32_t)r == r)) return JS_NewInt32(ctx, (int32_t)r);
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
            \\        if (likely((int32_t)r == r)) return JS_NewInt32(ctx, (int32_t)r);
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
            \\        if (unlikely(ib == -1 && ia == INT32_MIN)) return JS_NewInt32(ctx, 0);
            \\        return JS_NewInt32(ctx, ia % ib);
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

        try self.print(
            \\static JSValue {s}(JSContext *ctx, JSValueConst this_val,
            \\                   int argc, JSValueConst *argv)
            \\{{
            \\    (void)this_val;
            \\    JSValue stack[{d}];
            \\    int sp = 0;
            \\
        , .{ fname, max_stack });

        // Local variables
        if (var_count > 0) {
            try self.print("    JSValue locals[{d}];\n", .{var_count});
            try self.print("    for (int i = 0; i < {d}; i++) locals[i] = JS_UNDEFINED;\n\n", .{var_count});
        }

        // Process each basic block
        for (self.cfg.blocks.items, 0..) |*block, idx| {
            try self.emitBlock(block, idx);
        }

        // Fallthrough return
        try self.write("\n    return JS_UNDEFINED;\n}\n\n");
    }

    fn emitBlock(self: *SSACodeGen, block: *const BasicBlock, block_idx: usize) !void {
        try self.print("block_{d}:\n", .{block_idx});

        for (block.instructions) |instr| {
            try self.emitInstruction(instr);
        }
    }

    fn emitInstruction(self: *SSACodeGen, instr: Instruction) !void {
        const debug = self.options.debug_comments;

        switch (instr.opcode) {
            // ==================== PUSH CONSTANTS ====================
            .push_minus1 => {
                if (debug) try self.write("    /* push_minus1 */\n");
                try self.write("    PUSH(JS_NewInt32(ctx, -1));\n");
            },
            .push_0 => {
                if (debug) try self.write("    /* push_0 */\n");
                try self.write("    PUSH(JS_NewInt32(ctx, 0));\n");
            },
            .push_1 => {
                if (debug) try self.write("    /* push_1 */\n");
                try self.write("    PUSH(JS_NewInt32(ctx, 1));\n");
            },
            .push_2 => {
                if (debug) try self.write("    /* push_2 */\n");
                try self.write("    PUSH(JS_NewInt32(ctx, 2));\n");
            },
            .push_3 => {
                if (debug) try self.write("    /* push_3 */\n");
                try self.write("    PUSH(JS_NewInt32(ctx, 3));\n");
            },
            .push_4 => {
                if (debug) try self.write("    /* push_4 */\n");
                try self.write("    PUSH(JS_NewInt32(ctx, 4));\n");
            },
            .push_5 => {
                if (debug) try self.write("    /* push_5 */\n");
                try self.write("    PUSH(JS_NewInt32(ctx, 5));\n");
            },
            .push_6 => {
                if (debug) try self.write("    /* push_6 */\n");
                try self.write("    PUSH(JS_NewInt32(ctx, 6));\n");
            },
            .push_7 => {
                if (debug) try self.write("    /* push_7 */\n");
                try self.write("    PUSH(JS_NewInt32(ctx, 7));\n");
            },
            .push_i8 => {
                if (debug) try self.print("    /* push_i8 {d} */\n", .{instr.operand.i8});
                try self.print("    PUSH(JS_NewInt32(ctx, {d}));\n", .{instr.operand.i8});
            },
            .push_i16 => {
                if (debug) try self.print("    /* push_i16 {d} */\n", .{instr.operand.i16});
                try self.print("    PUSH(JS_NewInt32(ctx, {d}));\n", .{instr.operand.i16});
            },
            .push_i32 => {
                if (debug) try self.print("    /* push_i32 {d} */\n", .{instr.operand.i32});
                try self.print("    PUSH(JS_NewInt32(ctx, {d}));\n", .{instr.operand.i32});
            },
            .push_false => {
                if (debug) try self.write("    /* push_false */\n");
                try self.write("    PUSH(JS_FALSE);\n");
            },
            .push_true => {
                if (debug) try self.write("    /* push_true */\n");
                try self.write("    PUSH(JS_TRUE);\n");
            },
            .undefined => {
                if (debug) try self.write("    /* undefined */\n");
                try self.write("    PUSH(JS_UNDEFINED);\n");
            },
            .null => {
                if (debug) try self.write("    /* null */\n");
                try self.write("    PUSH(JS_NULL);\n");
            },

            // ==================== ARGUMENTS ====================
            .get_arg0 => {
                if (debug) try self.write("    /* get_arg0 */\n");
                try self.write("    PUSH(argc > 0 ? JS_DupValue(ctx, argv[0]) : JS_UNDEFINED);\n");
            },
            .get_arg1 => {
                if (debug) try self.write("    /* get_arg1 */\n");
                try self.write("    PUSH(argc > 1 ? JS_DupValue(ctx, argv[1]) : JS_UNDEFINED);\n");
            },
            .get_arg2 => {
                if (debug) try self.write("    /* get_arg2 */\n");
                try self.write("    PUSH(argc > 2 ? JS_DupValue(ctx, argv[2]) : JS_UNDEFINED);\n");
            },
            .get_arg3 => {
                if (debug) try self.write("    /* get_arg3 */\n");
                try self.write("    PUSH(argc > 3 ? JS_DupValue(ctx, argv[3]) : JS_UNDEFINED);\n");
            },
            .get_arg => {
                const idx = instr.operand.u16;
                if (debug) try self.print("    /* get_arg {d} */\n", .{idx});
                try self.print("    PUSH(argc > {d} ? JS_DupValue(ctx, argv[{d}]) : JS_UNDEFINED);\n", .{ idx, idx });
            },
            .put_arg0 => {
                if (debug) try self.write("    /* put_arg0 */\n");
                try self.write("    if (argc > 0) { JS_FreeValue(ctx, argv[0]); argv[0] = POP(); }\n");
            },
            .put_arg1 => {
                if (debug) try self.write("    /* put_arg1 */\n");
                try self.write("    if (argc > 1) { JS_FreeValue(ctx, argv[1]); argv[1] = POP(); }\n");
            },

            // ==================== LOCALS ====================
            .get_loc0 => {
                if (debug) try self.write("    /* get_loc0 */\n");
                try self.write("    PUSH(JS_DupValue(ctx, locals[0]));\n");
            },
            .get_loc1 => {
                if (debug) try self.write("    /* get_loc1 */\n");
                try self.write("    PUSH(JS_DupValue(ctx, locals[1]));\n");
            },
            .get_loc2 => {
                if (debug) try self.write("    /* get_loc2 */\n");
                try self.write("    PUSH(JS_DupValue(ctx, locals[2]));\n");
            },
            .get_loc3 => {
                if (debug) try self.write("    /* get_loc3 */\n");
                try self.write("    PUSH(JS_DupValue(ctx, locals[3]));\n");
            },
            .get_loc, .get_loc8 => {
                const idx = if (instr.opcode == .get_loc8) instr.operand.u8 else instr.operand.u16;
                if (debug) try self.print("    /* get_loc {d} */\n", .{idx});
                try self.print("    PUSH(JS_DupValue(ctx, locals[{d}]));\n", .{idx});
            },
            .put_loc0 => {
                if (debug) try self.write("    /* put_loc0 */\n");
                try self.write("    JS_FreeValue(ctx, locals[0]); locals[0] = POP();\n");
            },
            .put_loc1 => {
                if (debug) try self.write("    /* put_loc1 */\n");
                try self.write("    JS_FreeValue(ctx, locals[1]); locals[1] = POP();\n");
            },
            .put_loc2 => {
                if (debug) try self.write("    /* put_loc2 */\n");
                try self.write("    JS_FreeValue(ctx, locals[2]); locals[2] = POP();\n");
            },
            .put_loc3 => {
                if (debug) try self.write("    /* put_loc3 */\n");
                try self.write("    JS_FreeValue(ctx, locals[3]); locals[3] = POP();\n");
            },
            .put_loc, .put_loc8 => {
                const idx = if (instr.opcode == .put_loc8) instr.operand.u8 else instr.operand.u16;
                if (debug) try self.print("    /* put_loc {d} */\n", .{idx});
                try self.print("    JS_FreeValue(ctx, locals[{d}]); locals[{d}] = POP();\n", .{ idx, idx });
            },

            // ==================== STACK OPS ====================
            .drop => {
                if (debug) try self.write("    /* drop */\n");
                try self.write("    JS_FreeValue(ctx, POP());\n");
            },
            .dup => {
                if (debug) try self.write("    /* dup */\n");
                try self.write("    PUSH(JS_DupValue(ctx, TOP()));\n");
            },
            .dup2 => {
                if (debug) try self.write("    /* dup2 */\n");
                try self.write("    { JSValue a = stack[sp-2], b = stack[sp-1]; PUSH(JS_DupValue(ctx, a)); PUSH(JS_DupValue(ctx, b)); }\n");
            },

            // ==================== ARITHMETIC ====================
            .add => {
                if (debug) try self.write("    /* add */\n");
                try self.write("    { JSValue b = POP(), a = POP(); JSValue r = frozen_add(ctx, a, b); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); if (JS_IsException(r)) return r; PUSH(r); }\n");
            },
            .sub => {
                if (debug) try self.write("    /* sub */\n");
                try self.write("    { JSValue b = POP(), a = POP(); JSValue r = frozen_sub(ctx, a, b); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); if (JS_IsException(r)) return r; PUSH(r); }\n");
            },
            .mul => {
                if (debug) try self.write("    /* mul */\n");
                try self.write("    { JSValue b = POP(), a = POP(); JSValue r = frozen_mul(ctx, a, b); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); if (JS_IsException(r)) return r; PUSH(r); }\n");
            },
            .div => {
                if (debug) try self.write("    /* div */\n");
                try self.write("    { JSValue b = POP(), a = POP(); JSValue r = frozen_div(ctx, a, b); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); if (JS_IsException(r)) return r; PUSH(r); }\n");
            },
            .mod => {
                if (debug) try self.write("    /* mod */\n");
                try self.write("    { JSValue b = POP(), a = POP(); JSValue r = frozen_mod(ctx, a, b); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); if (JS_IsException(r)) return r; PUSH(r); }\n");
            },
            .neg => {
                if (debug) try self.write("    /* neg */\n");
                try self.write("    { JSValue a = POP(); if (JS_VALUE_GET_TAG(a) == JS_TAG_INT) { int32_t v = JS_VALUE_GET_INT(a); PUSH(v == INT32_MIN ? JS_NewFloat64(ctx, 2147483648.0) : JS_NewInt32(ctx, -v)); } else { double d; JS_ToFloat64(ctx, &d, a); JS_FreeValue(ctx, a); PUSH(JS_NewFloat64(ctx, -d)); } }\n");
            },
            .plus => {
                if (debug) try self.write("    /* plus */\n");
                try self.write("    /* unary plus - keep as is for int */\n");
            },
            .inc => {
                if (debug) try self.write("    /* inc */\n");
                try self.write("    { JSValue a = POP(); PUSH(frozen_add(ctx, a, JS_NewInt32(ctx, 1))); }\n");
            },
            .dec => {
                if (debug) try self.write("    /* dec */\n");
                try self.write("    { JSValue a = POP(); PUSH(frozen_sub(ctx, a, JS_NewInt32(ctx, 1))); }\n");
            },
            .inc_loc => {
                const idx = instr.operand.u8;
                if (debug) try self.print("    /* inc_loc {d} */\n", .{idx});
                try self.print("    {{ JSValue old = locals[{d}]; locals[{d}] = frozen_add(ctx, old, JS_NewInt32(ctx, 1)); }}\n", .{ idx, idx });
            },
            .dec_loc => {
                const idx = instr.operand.u8;
                if (debug) try self.print("    /* dec_loc {d} */\n", .{idx});
                try self.print("    {{ JSValue old = locals[{d}]; locals[{d}] = frozen_sub(ctx, old, JS_NewInt32(ctx, 1)); }}\n", .{ idx, idx });
            },
            .add_loc => {
                const idx = instr.operand.u8;
                if (debug) try self.print("    /* add_loc {d} */\n", .{idx});
                try self.print("    {{ JSValue v = POP(), old = locals[{d}]; locals[{d}] = frozen_add(ctx, old, v); }}\n", .{ idx, idx });
            },

            // ==================== COMPARISON ====================
            .lt => {
                if (debug) try self.write("    /* lt */\n");
                try self.write("    { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_lt(ctx, a, b))); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); }\n");
            },
            .lte => {
                if (debug) try self.write("    /* lte */\n");
                try self.write("    { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_lte(ctx, a, b))); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); }\n");
            },
            .gt => {
                if (debug) try self.write("    /* gt */\n");
                try self.write("    { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_gt(ctx, a, b))); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); }\n");
            },
            .gte => {
                if (debug) try self.write("    /* gte */\n");
                try self.write("    { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_gte(ctx, a, b))); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); }\n");
            },
            .eq => {
                if (debug) try self.write("    /* eq */\n");
                try self.write("    { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_eq(ctx, a, b))); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); }\n");
            },
            .neq => {
                if (debug) try self.write("    /* neq */\n");
                try self.write("    { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_neq(ctx, a, b))); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); }\n");
            },
            .strict_eq => {
                if (debug) try self.write("    /* strict_eq */\n");
                try self.write("    { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_eq(ctx, a, b))); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); }\n");
            },
            .strict_neq => {
                if (debug) try self.write("    /* strict_neq */\n");
                try self.write("    { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_neq(ctx, a, b))); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); }\n");
            },

            // ==================== BITWISE ====================
            .shl => {
                if (debug) try self.write("    /* shl */\n");
                try self.write("    { JSValue b = POP(), a = POP(); int32_t ia, ib; JS_ToInt32(ctx, &ia, a); JS_ToInt32(ctx, &ib, b); PUSH(JS_NewInt32(ctx, ia << (ib & 0x1f))); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); }\n");
            },
            .sar => {
                if (debug) try self.write("    /* sar */\n");
                try self.write("    { JSValue b = POP(), a = POP(); int32_t ia, ib; JS_ToInt32(ctx, &ia, a); JS_ToInt32(ctx, &ib, b); PUSH(JS_NewInt32(ctx, ia >> (ib & 0x1f))); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); }\n");
            },
            .shr => {
                if (debug) try self.write("    /* shr */\n");
                try self.write("    { JSValue b = POP(), a = POP(); uint32_t ia; int32_t ib; JS_ToUint32(ctx, &ia, a); JS_ToInt32(ctx, &ib, b); PUSH(JS_NewUint32(ctx, ia >> (ib & 0x1f))); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); }\n");
            },
            .@"and" => {
                if (debug) try self.write("    /* and */\n");
                try self.write("    { JSValue b = POP(), a = POP(); int32_t ia, ib; JS_ToInt32(ctx, &ia, a); JS_ToInt32(ctx, &ib, b); PUSH(JS_NewInt32(ctx, ia & ib)); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); }\n");
            },
            .@"or" => {
                if (debug) try self.write("    /* or */\n");
                try self.write("    { JSValue b = POP(), a = POP(); int32_t ia, ib; JS_ToInt32(ctx, &ia, a); JS_ToInt32(ctx, &ib, b); PUSH(JS_NewInt32(ctx, ia | ib)); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); }\n");
            },
            .xor => {
                if (debug) try self.write("    /* xor */\n");
                try self.write("    { JSValue b = POP(), a = POP(); int32_t ia, ib; JS_ToInt32(ctx, &ia, a); JS_ToInt32(ctx, &ib, b); PUSH(JS_NewInt32(ctx, ia ^ ib)); JS_FreeValue(ctx, a); JS_FreeValue(ctx, b); }\n");
            },
            .not => {
                if (debug) try self.write("    /* not */\n");
                try self.write("    { JSValue a = POP(); int32_t ia; JS_ToInt32(ctx, &ia, a); PUSH(JS_NewInt32(ctx, ~ia)); JS_FreeValue(ctx, a); }\n");
            },

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
            .@"return" => {
                if (debug) try self.write("    /* return */\n");
                try self.write("    return POP();\n");
            },
            .return_undef => {
                if (debug) try self.write("    /* return_undef */\n");
                try self.write("    return JS_UNDEFINED;\n");
            },

            // ==================== CALLS ====================
            .call0 => {
                if (debug) try self.write("    /* call0 */\n");
                try self.write("    { JSValue func = POP(); JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 0, NULL); JS_FreeValue(ctx, func); if (JS_IsException(ret)) return ret; PUSH(ret); }\n");
            },
            .call1 => {
                if (debug) try self.write("    /* call1 */\n");
                // Check for self-ref marker (0xDEAD) for direct recursion
                try self.print("    {{ JSValue arg0 = POP(); JSValue func = POP(); JSValue ret; if (JS_VALUE_GET_TAG(func) == JS_TAG_INT && JS_VALUE_GET_INT(func) == 0xDEAD) {{ ret = {s}(ctx, JS_UNDEFINED, 1, &arg0); }} else {{ ret = JS_Call(ctx, func, JS_UNDEFINED, 1, &arg0); JS_FreeValue(ctx, func); }} JS_FreeValue(ctx, arg0); if (JS_IsException(ret)) return ret; PUSH(ret); }}\n", .{self.options.func_name});
            },
            .call2 => {
                if (debug) try self.write("    /* call2 */\n");
                try self.write("    { JSValue args[2]; args[1] = POP(); args[0] = POP(); JSValue func = POP(); JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 2, args); JS_FreeValue(ctx, func); JS_FreeValue(ctx, args[0]); JS_FreeValue(ctx, args[1]); if (JS_IsException(ret)) return ret; PUSH(ret); }\n");
            },
            .call3 => {
                if (debug) try self.write("    /* call3 */\n");
                try self.write("    { JSValue args[3]; args[2] = POP(); args[1] = POP(); args[0] = POP(); JSValue func = POP(); JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 3, args); JS_FreeValue(ctx, func); JS_FreeValue(ctx, args[0]); JS_FreeValue(ctx, args[1]); JS_FreeValue(ctx, args[2]); if (JS_IsException(ret)) return ret; PUSH(ret); }\n");
            },

            // ==================== CLOSURE REFS ====================
            // For self-recursion, var_ref0 typically refers to the function itself
            // We use a sentinel value that call1 recognizes for direct recursion
            .get_var_ref0 => {
                if (debug) try self.write("    /* get_var_ref0 - self reference marker */\n");
                // Push a marker that tells call1 to do direct recursion
                try self.write("    PUSH(JS_MKVAL(JS_TAG_INT, 0xDEAD)); /* self-ref marker */\n");
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
            },

            else => {
                const info = instr.getInfo();
                try self.print("    /* TODO: {s} */\n", .{info.name});
                try self.write("    PUSH(JS_UNDEFINED); /* unimplemented opcode */\n");
            },
        }
    }

    fn emitInit(self: *SSACodeGen) !void {
        const fname = self.options.func_name;
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
        , .{ fname, fname, fname, self.options.arg_count, fname });
    }
};
