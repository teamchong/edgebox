//! Stack-based Code Generator
//!
//! Generates Zig or C code that uses a JSValue stack, similar to the QuickJS interpreter
//! but with the interpreter dispatch loop eliminated.
//!
//! Each opcode emits real code that manipulates the stack.

const std = @import("std");
const opcodes = @import("opcodes.zig");
const handlers = @import("opcode_handlers.zig");
const int32_handlers = @import("int32_handlers.zig");
const parser = @import("bytecode_parser.zig");
const cfg_mod = @import("cfg_builder.zig");
const module_parser = @import("module_parser.zig");

const Opcode = opcodes.Opcode;
const Instruction = parser.Instruction;
const CFG = cfg_mod.CFG;
const BasicBlock = cfg_mod.BasicBlock;
const Allocator = std.mem.Allocator;
const ConstValue = module_parser.ConstValue;

pub const OutputLanguage = enum {
    c,      // Generate C code (legacy)
    zig,    // Generate Zig code (default for Zig project)
};

pub const CodeGenOptions = struct {
    func_name: []const u8 = "frozen_func",
    js_name: []const u8 = "", // Name to use for globalThis registration (if different from func_name)
    debug_comments: bool = false,
    arg_count: u16 = 0,
    var_count: u16 = 0,
    max_stack: u16 = 256,
    emit_helpers: bool = true, // Set to false for subsequent functions in the same file
    is_self_recursive: bool = false, // Set to true if function calls itself (enables direct C recursion)
    use_direct_recursion: bool = true, // Use direct C recursion (fast) vs trampoline (safe but slow)
    use_native_int32: bool = false, // Use native int32 locals instead of JSValue stack (18x faster for pure int math)
    constants: []const ConstValue = &.{}, // Constant pool values
    atom_strings: []const []const u8 = &.{}, // Atom table strings for property/variable access
    output_language: OutputLanguage = .zig, // Output language: Zig (default) or C (legacy)
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
        StackUnderflow,
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

    // ===== Language-specific type/syntax helpers =====

    fn isZig(self: *const SSACodeGen) bool {
        return self.options.output_language == .zig;
    }

    fn jsValueType(self: *const SSACodeGen) []const u8 {
        return if (self.isZig()) "qjs.JSValue" else "JSValue";
    }

    fn jsValueConstType(self: *const SSACodeGen) []const u8 {
        return if (self.isZig()) "qjs.JSValueConst" else "JSValueConst";
    }

    fn jsContextPtrType(self: *const SSACodeGen) []const u8 {
        return if (self.isZig()) "*qjs.JSContext" else "JSContext *";
    }

    fn intType(self: *const SSACodeGen) []const u8 {
        return if (self.isZig()) "c_int" else "int";
    }

    // Helper for binary arithmetic opcodes with int32 fast path
    fn emitBinaryArithOp(self: *SSACodeGen, op_name: []const u8, op_symbol: []const u8, overflow_to_float: bool) !void {
        if (self.isZig()) {
            try self.print("    {{ const b = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }}; const a = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }};\n", .{});
            try self.print("      if (qjs.JS_VALUE_GET_TAG(a) == qjs.JS_TAG_INT and qjs.JS_VALUE_GET_TAG(b) == qjs.JS_TAG_INT) {{\n", .{});
            try self.print("          const result: i64 = @as(i64, qjs.JS_VALUE_GET_INT(a)) {s} @as(i64, qjs.JS_VALUE_GET_INT(b));\n", .{op_symbol});
            if (overflow_to_float) {
                try self.write("          if (result >= std.math.minInt(i32) and result <= std.math.maxInt(i32)) {\n");
                try self.write("              stack[@intCast(sp)] = qjs.JS_MKVAL(qjs.JS_TAG_INT, @intCast(result)); sp += 1;\n");
                try self.write("          } else {\n");
                try self.write("              stack[@intCast(sp)] = qjs.JS_NewFloat64(ctx, @floatFromInt(result)); sp += 1;\n");
                try self.write("          }\n");
            } else {
                try self.write("          stack[@intCast(sp)] = qjs.JS_MKVAL(qjs.JS_TAG_INT, @intCast(result)); sp += 1;\n");
            }
            try self.write("      } else {\n");
            try self.print("          const r = qjs.frozen_{s}(ctx, a, b);\n", .{op_name});
            try self.write("          qjs.JS_FreeValue(ctx, a); qjs.JS_FreeValue(ctx, b);\n");
            try self.write("          if (qjs.JS_IsException(r)) return r;\n");
            try self.write("          stack[@intCast(sp)] = r; sp += 1;\n");
            try self.write("      }\n    }\n");
        } else {
            // C code - keep existing
            try self.write("    { JSValue b = POP(), a = POP();\n");
            try self.print("      if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {{\n", .{});
            try self.print("          int64_t result = (int64_t)JS_VALUE_GET_INT(a) {s} JS_VALUE_GET_INT(b);\n", .{op_symbol});
            if (overflow_to_float) {
                try self.write("          if (likely(result >= INT32_MIN && result <= INT32_MAX)) {\n");
                try self.write("              PUSH(JS_MKVAL(JS_TAG_INT, (int32_t)result));\n");
                try self.write("          } else {\n");
                try self.write("              PUSH(JS_NewFloat64(ctx, (double)result));\n");
                try self.write("          }\n");
            } else {
                try self.write("          PUSH(JS_MKVAL(JS_TAG_INT, (int32_t)result));\n");
            }
            try self.write("      } else {\n");
            try self.print("          JSValue r = frozen_{s}(ctx, a, b);\n", .{op_name});
            try self.write("          FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);\n");
            try self.write("          if (JS_IsException(r)) return r;\n");
            try self.write("          PUSH(r);\n");
            try self.write("      }\n    }\n");
        }
    }

    // Helper for division (special: always returns float, div-by-zero -> NaN)
    fn emitDivOp(self: *SSACodeGen) !void {
        if (self.isZig()) {
            try self.write("    { const b = { sp -= 1; const val = stack[@intCast(sp)]; val; }; const a = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
            try self.write("      if (qjs.JS_VALUE_GET_TAG(a) == qjs.JS_TAG_INT and qjs.JS_VALUE_GET_TAG(b) == qjs.JS_TAG_INT) {\n");
            try self.write("          const bv: i32 = qjs.JS_VALUE_GET_INT(b);\n");
            try self.write("          if (bv != 0) {\n");
            try self.write("              const av: i32 = qjs.JS_VALUE_GET_INT(a);\n");
            try self.write("              // JS division always returns float for int/int (even if exact)\n");
            try self.write("              stack[@intCast(sp)] = qjs.JS_NewFloat64(ctx, @as(f64, @floatFromInt(av)) / @as(f64, @floatFromInt(bv))); sp += 1;\n");
            try self.write("          } else {\n");
            try self.write("              // Division by zero -> NaN\n");
            try self.write("              stack[@intCast(sp)] = qjs.JS_NAN; sp += 1;\n");
            try self.write("          }\n");
            try self.write("      } else {\n");
            try self.write("          const r = qjs.frozen_div(ctx, a, b);\n");
            try self.write("          qjs.JS_FreeValue(ctx, a); qjs.JS_FreeValue(ctx, b);\n");
            try self.write("          if (qjs.JS_IsException(r)) return r;\n");
            try self.write("          stack[@intCast(sp)] = r; sp += 1;\n");
            try self.write("      }\n    }\n");
        } else {
            try self.write(
                \\    { JSValue b = POP(), a = POP();
                \\      if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
                \\          int32_t bv = JS_VALUE_GET_INT(b);
                \\          if (likely(bv != 0)) {
                \\              int32_t av = JS_VALUE_GET_INT(a);
                \\              /* JS division always returns float for int/int (even if exact) */
                \\              PUSH(JS_NewFloat64(ctx, (double)av / (double)bv));
                \\          } else {
                \\              /* Division by zero -> NaN */
                \\              PUSH(JS_NAN);
                \\          }
                \\      } else {
                \\          JSValue r = frozen_div(ctx, a, b);  /* Slow path */
                \\          FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);
                \\          if (JS_IsException(r)) return r;
                \\          PUSH(r);
                \\      }
                \\    }
                \\
            );
        }
    }

    // Helper for modulo (div-by-zero -> NaN)
    fn emitModOp(self: *SSACodeGen) !void {
        if (self.isZig()) {
            try self.write("    { const b = { sp -= 1; const val = stack[@intCast(sp)]; val; }; const a = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
            try self.write("      if (qjs.JS_VALUE_GET_TAG(a) == qjs.JS_TAG_INT and qjs.JS_VALUE_GET_TAG(b) == qjs.JS_TAG_INT) {\n");
            try self.write("          const bv: i32 = qjs.JS_VALUE_GET_INT(b);\n");
            try self.write("          if (bv != 0) {\n");
            try self.write("              const av: i32 = qjs.JS_VALUE_GET_INT(a);\n");
            try self.write("              stack[@intCast(sp)] = qjs.JS_MKVAL(qjs.JS_TAG_INT, @rem(av, bv)); sp += 1;\n");
            try self.write("          } else {\n");
            try self.write("              // Modulo by zero -> NaN\n");
            try self.write("              stack[@intCast(sp)] = qjs.JS_NAN; sp += 1;\n");
            try self.write("          }\n");
            try self.write("      } else {\n");
            try self.write("          const r = qjs.frozen_mod(ctx, a, b);\n");
            try self.write("          qjs.JS_FreeValue(ctx, a); qjs.JS_FreeValue(ctx, b);\n");
            try self.write("          if (qjs.JS_IsException(r)) return r;\n");
            try self.write("          stack[@intCast(sp)] = r; sp += 1;\n");
            try self.write("      }\n    }\n");
        } else {
            try self.write(
                \\    { JSValue b = POP(), a = POP();
                \\      if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
                \\          int32_t bv = JS_VALUE_GET_INT(b);
                \\          if (likely(bv != 0)) {
                \\              int32_t av = JS_VALUE_GET_INT(a);
                \\              PUSH(JS_MKVAL(JS_TAG_INT, av % bv));  /* Fast path: no alloc */
                \\          } else {
                \\              /* Modulo by zero -> NaN */
                \\              PUSH(JS_NAN);
                \\          }
                \\      } else {
                \\          JSValue r = frozen_mod(ctx, a, b);  /* Slow path */
                \\          FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);
                \\          if (JS_IsException(r)) return r;
                \\          PUSH(r);
                \\      }
                \\    }
                \\
            );
        }
    }

    // Helper for negation (unary minus)
    fn emitNegOp(self: *SSACodeGen) !void {
        if (self.isZig()) {
            try self.write("    { const a = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
            try self.write("      if (qjs.JS_VALUE_GET_TAG(a) == qjs.JS_TAG_INT) {\n");
            try self.write("          const v: i32 = qjs.JS_VALUE_GET_INT(a);\n");
            try self.write("          if (v != std.math.minInt(i32)) {\n");
            try self.write("              stack[@intCast(sp)] = qjs.JS_MKVAL(qjs.JS_TAG_INT, -v); sp += 1;\n");
            try self.write("          } else {\n");
            try self.write("              // -INT32_MIN overflows to float64\n");
            try self.write("              stack[@intCast(sp)] = qjs.JS_NewFloat64(ctx, -@as(f64, @floatFromInt(v))); sp += 1;\n");
            try self.write("          }\n");
            try self.write("      } else {\n");
            try self.write("          const r = qjs.frozen_neg(ctx, a);\n");
            try self.write("          qjs.JS_FreeValue(ctx, a);\n");
            try self.write("          if (qjs.JS_IsException(r)) return r;\n");
            try self.write("          stack[@intCast(sp)] = r; sp += 1;\n");
            try self.write("      }\n    }\n");
        } else {
            try self.write(
                \\    { JSValue a = POP();
                \\      if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT)) {
                \\          int32_t v = JS_VALUE_GET_INT(a);
                \\          if (likely(v != INT32_MIN)) {
                \\              PUSH(JS_MKVAL(JS_TAG_INT, -v));  /* Fast path: no alloc */
                \\          } else {
                \\              PUSH(JS_NewFloat64(ctx, -(double)v));  /* Overflow to float64 */
                \\          }
                \\      } else {
                \\          JSValue r = frozen_neg(ctx, a);  /* Slow path */
                \\          FROZEN_FREE(ctx, a);
                \\          if (JS_IsException(r)) return r;
                \\          PUSH(r);
                \\      }
                \\    }
                \\
            );
        }
    }

    // Helper for increment (unary +1)
    fn emitIncOp(self: *SSACodeGen) !void {
        if (self.isZig()) {
            try self.write("    { const a = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
            try self.write("      if (qjs.JS_VALUE_GET_TAG(a) == qjs.JS_TAG_INT) {\n");
            try self.write("          const v: i32 = qjs.JS_VALUE_GET_INT(a);\n");
            try self.write("          if (v < std.math.maxInt(i32)) {\n");
            try self.write("              stack[@intCast(sp)] = qjs.JS_MKVAL(qjs.JS_TAG_INT, v + 1); sp += 1;\n");
            try self.write("          } else {\n");
            try self.write("              // Overflow to float64\n");
            try self.write("              stack[@intCast(sp)] = qjs.JS_NewFloat64(ctx, @as(f64, @floatFromInt(v)) + 1); sp += 1;\n");
            try self.write("          }\n");
            try self.write("      } else {\n");
            try self.write("          const r = qjs.frozen_add(ctx, a, qjs.JS_MKVAL(qjs.JS_TAG_INT, 1));\n");
            try self.write("          qjs.JS_FreeValue(ctx, a);\n");
            try self.write("          if (qjs.JS_IsException(r)) return r;\n");
            try self.write("          stack[@intCast(sp)] = r; sp += 1;\n");
            try self.write("      }\n    }\n");
        } else {
            try self.write(
                \\    { JSValue a = POP();
                \\      if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT)) {
                \\          int32_t v = JS_VALUE_GET_INT(a);
                \\          if (likely(v < INT32_MAX)) {
                \\              PUSH(JS_MKVAL(JS_TAG_INT, v + 1));  /* Fast path: no alloc */
                \\          } else {
                \\              PUSH(JS_NewFloat64(ctx, (double)v + 1));  /* Overflow to float64 */
                \\          }
                \\      } else {
                \\          JSValue r = frozen_add(ctx, a, JS_MKVAL(JS_TAG_INT, 1));  /* Slow path */
                \\          FROZEN_FREE(ctx, a);
                \\          if (JS_IsException(r)) return r;
                \\          PUSH(r);
                \\      }
                \\    }
                \\
            );
        }
    }

    // Helper for decrement (unary -1)
    fn emitDecOp(self: *SSACodeGen) !void {
        if (self.isZig()) {
            try self.write("    { const a = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
            try self.write("      if (qjs.JS_VALUE_GET_TAG(a) == qjs.JS_TAG_INT) {\n");
            try self.write("          const v: i32 = qjs.JS_VALUE_GET_INT(a);\n");
            try self.write("          if (v > std.math.minInt(i32)) {\n");
            try self.write("              stack[@intCast(sp)] = qjs.JS_MKVAL(qjs.JS_TAG_INT, v - 1); sp += 1;\n");
            try self.write("          } else {\n");
            try self.write("              // Underflow to float64\n");
            try self.write("              stack[@intCast(sp)] = qjs.JS_NewFloat64(ctx, @as(f64, @floatFromInt(v)) - 1); sp += 1;\n");
            try self.write("          }\n");
            try self.write("      } else {\n");
            try self.write("          const r = qjs.frozen_sub(ctx, a, qjs.JS_MKVAL(qjs.JS_TAG_INT, 1));\n");
            try self.write("          qjs.JS_FreeValue(ctx, a);\n");
            try self.write("          if (qjs.JS_IsException(r)) return r;\n");
            try self.write("          stack[@intCast(sp)] = r; sp += 1;\n");
            try self.write("      }\n    }\n");
        } else {
            try self.write(
                \\    { JSValue a = POP();
                \\      if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT)) {
                \\          int32_t v = JS_VALUE_GET_INT(a);
                \\          if (likely(v > INT32_MIN)) {
                \\              PUSH(JS_MKVAL(JS_TAG_INT, v - 1));  /* Fast path: no alloc */
                \\          } else {
                \\              PUSH(JS_NewFloat64(ctx, (double)v - 1));  /* Overflow to float64 */
                \\          }
                \\      } else {
                \\          JSValue r = frozen_sub(ctx, a, JS_MKVAL(JS_TAG_INT, 1));  /* Slow path */
                \\          FROZEN_FREE(ctx, a);
                \\          if (JS_IsException(r)) return r;
                \\          PUSH(r);
                \\      }
                \\    }
                \\
            );
        }
    }

    /// Emit binary operation that calls a QuickJS function (bitwise, comparison, etc.)
    fn emitBinaryFuncOp(self: *SSACodeGen, func_name: []const u8) !void {
        if (self.isZig()) {
            try self.print("    {{ const b = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }}; const a = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }};\n", .{});
            try self.print("      const r = qjs.{s}(ctx, a, b);\n", .{func_name});
            try self.write("      qjs.JS_FreeValue(ctx, a); qjs.JS_FreeValue(ctx, b);\n");
            try self.write("      if (qjs.JS_IsException(r)) return r;\n");
            try self.write("      stack[@intCast(sp)] = r; sp += 1;\n");
            try self.write("    }\n");
        } else {
            try self.print("    {{ JSValue b = POP(), a = POP(); JSValue r = {s}(ctx, a, b); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); if (JS_IsException(r)) return r; PUSH(r); }}\n", .{func_name});
        }
    }

    /// Emit unary operation that calls a QuickJS function
    fn emitUnaryFuncOp(self: *SSACodeGen, func_name: []const u8) !void {
        if (self.isZig()) {
            try self.print("    {{ const a = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }};\n", .{});
            try self.print("      const r = qjs.{s}(ctx, a);\n", .{func_name});
            try self.write("      qjs.JS_FreeValue(ctx, a);\n");
            try self.write("      if (qjs.JS_IsException(r)) return r;\n");
            try self.write("      stack[@intCast(sp)] = r; sp += 1;\n");
            try self.write("    }\n");
        } else {
            try self.print("    {{ JSValue a = POP(); JSValue r = {s}(ctx, a); FROZEN_FREE(ctx, a); if (JS_IsException(r)) return r; PUSH(r); }}\n", .{func_name});
        }
    }

    /// Emit binary comparison operation that returns boolean
    fn emitBinaryCmpOp(self: *SSACodeGen, func_name: []const u8) !void {
        if (self.isZig()) {
            try self.print("    {{ const b = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }}; const a = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }};\n", .{});
            try self.print("      const result = qjs.JS_NewBool(ctx, qjs.{s}(ctx, a, b));\n", .{func_name});
            try self.write("      qjs.JS_FreeValue(ctx, a); qjs.JS_FreeValue(ctx, b);\n");
            try self.write("      stack[@intCast(sp)] = result; sp += 1;\n");
            try self.write("    }\n");
        } else {
            try self.print("    {{ JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, {s}(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }}\n", .{func_name});
        }
    }

    /// Emit type check operation that returns boolean
    /// @param check_expr: The expression to check (e.g., "qjs.JS_IsUndefined(v)" or "qjs.JS_IsUndefined(v) != 0 or qjs.JS_IsNull(v) != 0")
    fn emitTypeCheckOp(self: *SSACodeGen, check_expr: []const u8) !void {
        if (self.isZig()) {
            try self.write("    { const v = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
            try self.print("      const result = qjs.JS_NewBool(ctx, if ({s}) 1 else 0);\n", .{check_expr});
            try self.write("      qjs.JS_FreeValue(ctx, v);\n");
            try self.write("      stack[@intCast(sp)] = result; sp += 1;\n");
            try self.write("    }\n");
        } else {
            try self.print("    {{ JSValue v = POP(); PUSH(JS_NewBool(ctx, {s})); FROZEN_FREE(ctx, v); }}\n", .{check_expr});
        }
    }

    /// Emit push integer constant to stack
    fn emitPushInt(self: *SSACodeGen, value: anytype) !void {
        if (self.isZig()) {
            try self.print("    stack[@intCast(sp)] = qjs.JS_MKVAL(qjs.JS_TAG_INT, {d}); sp += 1;\n", .{value});
        } else {
            try self.print("    PUSH(JS_MKVAL(JS_TAG_INT, {d}));\n", .{value});
        }
    }

    /// Emit push JS constant to stack (e.g., JS_TRUE, JS_UNDEFINED)
    fn emitPushJSConst(self: *SSACodeGen, const_name: []const u8) !void {
        if (self.isZig()) {
            try self.print("    stack[@intCast(sp)] = qjs.{s}; sp += 1;\n", .{const_name});
        } else {
            try self.print("    PUSH({s});\n", .{const_name});
        }
    }

    /// Emit function signature (forward declaration or definition start)
    fn emitFunctionSignature(self: *SSACodeGen, is_declaration: bool) !void {
        const fname = self.options.func_name;
        if (self.isZig()) {
            // Zig: pub fn name(ctx: *qjs.JSContext, ...) callconv(.C) qjs.JSValue
            if (is_declaration) {
                try self.print("pub fn {s}(ctx: {s}, this_val: {s}, argc: {s}, argv: [*c]{s}) callconv(.C) {s};\n",
                    .{fname, self.jsContextPtrType(), self.jsValueConstType(), self.intType(), self.jsValueConstType(), self.jsValueType()});
            } else {
                try self.print("pub fn {s}(ctx: {s}, this_val: {s}, argc: {s}, argv: [*c]{s}) callconv(.C) {s}",
                    .{fname, self.jsContextPtrType(), self.jsValueConstType(), self.intType(), self.jsValueConstType(), self.jsValueType()});
            }
        } else {
            // C: static JSValue name(JSContext *ctx, ...)
            try self.print("static {s} {s}({s}ctx, {s} this_val, {s} argc, {s} *argv)",
                .{self.jsValueType(), fname, self.jsContextPtrType(), self.jsValueConstType(), self.intType(), self.jsValueConstType()});
            if (is_declaration) try self.write(";\n");
        }
    }

    /// Emit stack variable declaration
    fn emitStackDecl(self: *SSACodeGen, max_stack: u16) !void {
        if (self.isZig()) {
            try self.print("    var stack: [{d}]{s} = undefined;\n", .{max_stack, self.jsValueType()});
            try self.write("    var sp: c_int = 0;\n");
        } else {
            try self.print("    const int max_stack = {d};\n", .{max_stack});
            try self.print("    JSValue stack[{d}];\n", .{max_stack});
            try self.write("    int sp = 0;\n");
        }
    }

    /// Emit locals variable declaration
    fn emitLocalsDecl(self: *SSACodeGen, var_count: u16) !void {
        const actual_var_count = if (var_count > 0) var_count else 1;
        if (self.isZig()) {
            try self.print("    var locals: [{d}]{s} = undefined;\n", .{actual_var_count, self.jsValueType()});
            try self.print("    for (0..{d}) |i| {{\n", .{actual_var_count});
            try self.write("        locals[i] = qjs.JS_UNDEFINED;\n    }\n");
        } else {
            try self.print("    JSValue locals[{d}];\n", .{actual_var_count});
            try self.print("    for (int i = 0; i < {d}; i++) locals[i] = JS_UNDEFINED;\n", .{actual_var_count});
        }
    }

    /// Write a string, escaping special characters for C string literals
    fn writeEscapedString(self: *SSACodeGen, str: []const u8) !void {
        for (str) |c| {
            switch (c) {
                '"' => try self.write("\\\""),
                '\\' => try self.write("\\\\"),
                '\n' => try self.write("\\n"),
                '\r' => try self.write("\\r"),
                '\t' => try self.write("\\t"),
                else => {
                    if (c >= 32 and c < 127) {
                        try self.output.append(self.allocator, c);
                    } else {
                        // Escape non-printable/non-ASCII as hex
                        var buf: [4]u8 = undefined;
                        const escaped = std.fmt.bufPrint(&buf, "\\x{x:0>2}", .{c}) catch return error.FormatError;
                        try self.output.appendSlice(self.allocator, escaped);
                    }
                },
            }
        }
    }

    pub fn generate(self: *SSACodeGen) Error![]const u8 {
        if (self.options.emit_helpers) {
            try self.emitHeader();
        } else {
            // Just emit a forward declaration
            try self.print("static JSValue {s}(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv);\n", .{self.options.func_name});
            // Static variable for special_object type 2 (this_func)
            try self.print("static JSValue _{s}_this_func = {{0}}; /* JS_UNDEFINED */\n\n", .{self.options.func_name});
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
    /// Now just returns the include directive since helpers are in frozen_runtime.h/c
    pub fn emitHelpersOnly(allocator: Allocator) ![]const u8 {
        var output = std.ArrayListUnmanaged(u8){};
        // All helpers are now in frozen_runtime.h (header) and frozen_runtime.c (implementation)
        // The header is included by generated code, implementation is pre-compiled
        try output.appendSlice(allocator,
            \\/* Helpers are provided by frozen_runtime.h (included by each function) */
            \\
        );
        return output.toOwnedSlice(allocator);
    }

    fn emitHeader(self: *SSACodeGen) !void {
        try self.print("//\n// Frozen function: {s}\n// Generated by edgebox-freeze\n//\n\n", .{self.options.func_name});

        if (self.isZig()) {
            // Zig: import QuickJS bindings
            try self.write(
                \\const qjs = @import("quickjs");
                \\
                \\
            );
        } else {
            // C: include frozen_runtime.h
            try self.write(
                \\#include "frozen_runtime.h"
                \\
            );
        }

        // Forward declaration
        try self.emitFunctionSignature(true);

        // Static/var for this_func (special_object type 2)
        if (self.isZig()) {
            try self.print("var _{s}_this_func: {s} = .{{ .u = .{{ .int32 = 0 }} }}; // JS_UNDEFINED\n\n",
                .{self.options.func_name, self.jsValueType()});
        } else {
            try self.print("static JSValue _{s}_this_func = {{0}}; /* JS_UNDEFINED */\n\n", .{self.options.func_name});
        }
    }

    fn emitFunction(self: *SSACodeGen) !void {
        const fname = self.options.func_name;
        const var_count = self.options.var_count;
        const max_stack = self.options.max_stack;

        // Use direct C recursion for self-recursive functions when enabled
        // This is much faster but risks C stack overflow for very deep recursion
        const use_trampoline = self.options.is_self_recursive and !self.options.use_direct_recursion;

        if (use_trampoline) {
            // JSC-style heap-allocated call frames (avoids C stack overflow)
            const actual_var_count = if (var_count > 0) var_count else 1;
            const actual_arg_count = if (self.options.arg_count > 0) self.options.arg_count else 1;

            try self.print(
                \\/* ============================================================================
                \\ * JSC-style Trampoline: Heap-allocated frames, no C stack recursion
                \\ * ============================================================================ */
                \\
                \\/* Call frame lives on heap, not C stack */
                \\typedef struct {s}_Frame {{
                \\    JSValue args[8];               /* Input arguments (fixed size for varargs) */
                \\    JSValue result;                /* Return value */
                \\    JSValue stack[{d}];            /* Operand stack */
                \\    JSValue locals[{d}];           /* Local variables */
                \\    int sp;                        /* Stack pointer */
                \\    int block_id;                  /* Current basic block (PC) */
                \\    int instr_offset;              /* Instruction offset within block */
                \\    uint8_t waiting_for_call;      /* 0=running, 1=waiting for result */
                \\}} {s}_Frame;
                \\
                \\static JSValue {s}(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv)
                \\{{
                \\    (void)this_val;
                \\    (void)argc;
                \\
                \\    /* Allocate frame stack on heap (JSC approach) */
                \\    const int MAX_FRAMES = 10000;
                \\    {s}_Frame *frames = js_malloc(ctx, sizeof({s}_Frame) * MAX_FRAMES);
                \\    if (!frames) return JS_ThrowOutOfMemory(ctx);
                \\
                \\    int frame_depth = 0;
                \\
                \\    /* Initialize root frame */
                \\    for (int i = 0; i < {d}; i++) frames[0].args[i] = (i < argc) ? FROZEN_DUP(ctx, argv[i]) : JS_UNDEFINED;
                \\    frames[0].sp = 0;
                \\    frames[0].block_id = 0;
                \\    frames[0].instr_offset = 0;
                \\    frames[0].waiting_for_call = 0;
                \\    for (int i = 0; i < {d}; i++) frames[0].locals[i] = JS_UNDEFINED;
                \\
                \\    /* Trampoline loop - iterative execution, not recursive! */
                \\    while (frame_depth >= 0) {{
                \\trampoline_continue: ;  /* Jump here when pushing new frame (semicolon for C99 compat) */
                \\        {s}_Frame *frame = &frames[frame_depth];
                \\
                \\        /* Check for stack overflow */
                \\        if (frame_depth >= MAX_FRAMES - 1) {{
                \\            js_free(ctx, frames);
                \\            return JS_ThrowRangeError(ctx, "Maximum call stack size exceeded");
                \\        }}
                \\
                \\        /* Set up execution context for current frame */
                \\        JSValue *stack = frame->stack;
                \\        int sp = frame->sp;
                \\        JSValue *locals = frame->locals;
                \\        JSValue *argv = frame->args;
                \\        int argc_inner = {d};
                \\        (void)argc_inner;
                \\        const int max_stack = {d};
                \\        (void)max_stack;
                \\
                \\        /* Execute bytecode for current frame */
                \\        int next_block = -1;  /* -1 = return, >= 0 = goto block */
                \\        /* printf("[TRAMPOLINE] depth=%d block=%d offset=%d sp=%d\n", frame_depth, frame->block_id, frame->instr_offset, sp); */
                \\
                \\
            , .{ fname, max_stack, actual_var_count, fname, fname, fname, fname, actual_arg_count, actual_var_count, fname, actual_arg_count, max_stack });

            // Generate state machine for each basic block
            try self.write("        switch (frame->block_id) {\n");

            for (self.cfg.blocks.items, 0..) |*block, idx| {
                try self.print("        case {d}:\n", .{idx});
                try self.emitTrampolineBlock(block, idx);
            }

            try self.write(
                \\        default:
                \\            /* Unknown block - return undefined */
                \\            next_block = -1;
                \\            frame->result = JS_UNDEFINED;
                \\            break;
                \\        }
                \\
                \\        /* Update frame state based on execution result */
                \\        frame->sp = sp;
                \\
                \\        if (next_block == -1) {
                \\            /* Current frame returned - pop it */
                \\            if (frame_depth == 0) {
                \\                /* Root frame returned - we're done! */
                \\                JSValue result = frame->result;
                \\                js_free(ctx, frames);
                \\                return result;
                \\            }
                \\            /* Return to parent frame */
                \\            frame_depth--;
                \\        } else {
                \\            /* Continue to next block in same frame */
                \\            frame->block_id = next_block;
                \\            frame->instr_offset = 0;
                \\        }
                \\    }
                \\
                \\    /* Should never reach here */
                \\    js_free(ctx, frames);
                \\    return JS_UNDEFINED;
                \\}
                \\
                \\
            );
        } else if (self.options.use_native_int32) {
            // Native int32 mode - bytecode-driven codegen for maximum performance
            // 18x faster for pure integer math (fib ~51ms vs ~919ms)

            if (self.options.is_self_recursive) {
                // Self-recursive function: generate int32 helper + JSValue wrapper
                // This pattern works for ANY recursive function (fibonacci, tribonacci, factorial, etc.)

                // Generate pure int32 helper function (called recursively)
                try self.print(
                    \\/* ============================================================================
                    \\ * Native int32 mode - zero allocation hot path
                    \\ * Pure int32 internal helper - bytecode-driven codegen
                    \\ * ============================================================================ */
                    \\static int32_t {s}_int32(int32_t n0) {{
                    \\
                , .{fname});

                // Emit bytecode-driven blocks (NOT hardcoded!)
                // Initialize temp counter once for all blocks to avoid redefinition
                var next_temp: u32 = 0;
                for (self.cfg.blocks.items, 0..) |*block, idx| {
                    try self.emitInt32Block(block, idx, &next_temp);
                }

                try self.write("}\n\n");

                // Generate JSValue wrapper that calls int32 helper
                try self.print(
                    \\static JSValue {s}(JSContext *ctx, JSValueConst this_val,
                    \\                   int argc, JSValueConst *argv)
                    \\{{
                    \\    (void)this_val;
                    \\    /* Extract first arg as native int32 */
                    \\    int32_t n0;
                    \\    if (likely(argc > 0 && JS_VALUE_GET_TAG(argv[0]) == JS_TAG_INT)) {{
                    \\        n0 = JS_VALUE_GET_INT(argv[0]);
                    \\    }} else {{
                    \\        if (argc <= 0) return JS_UNDEFINED;
                    \\        if (JS_ToInt32(ctx, &n0, argv[0])) return JS_EXCEPTION;
                    \\    }}
                    \\    /* Call pure int32 helper, box result once */
                    \\    return JS_NewInt32(ctx, {s}_int32(n0));
                    \\}}
                    \\
                    \\
                , .{ fname, fname });
            } else {
                // Non-recursive int32: use direct int32 code
                try self.print(
                    \\/* ============================================================================
                    \\ * Native int32 mode - zero allocation hot path
                    \\ * ============================================================================ */
                    \\static JSValue {s}(JSContext *ctx, JSValueConst this_val,
                    \\                   int argc, JSValueConst *argv)
                    \\{{
                    \\    (void)this_val;
                    \\    /* Extract first arg as native int32 */
                    \\    int32_t n0;
                    \\    if (likely(argc > 0 && JS_VALUE_GET_TAG(argv[0]) == JS_TAG_INT)) {{
                    \\        n0 = JS_VALUE_GET_INT(argv[0]);
                    \\    }} else {{
                    \\        if (argc <= 0) return JS_UNDEFINED;
                    \\        if (JS_ToInt32(ctx, &n0, argv[0])) return JS_EXCEPTION;
                    \\    }}
                    \\
                , .{fname});

                // Emit specialized blocks for non-recursive case
                var next_temp: u32 = 0;
                for (self.cfg.blocks.items, 0..) |*block, idx| {
                    try self.emitInt32Block(block, idx, &next_temp);
                }
                try self.write("\n    return JS_UNDEFINED;\n}\n\n");
            }
        } else {
            // Standard non-recursive function
            try self.emitFunctionSignature(false);
            try self.write(" {\n");

            // Unused parameter suppression
            if (self.isZig()) {
                try self.write("    _ = this_val;\n");
            } else {
                try self.write("    (void)this_val;\n");
            }

            // Stack and locals
            try self.emitStackDecl(max_stack);

            // Stack check (C only for now - Zig needs frozen_runtime equivalent)
            if (!self.isZig()) {
                try self.write("    FROZEN_CHECK_STACK(ctx);\n");
            }
            try self.write("\n");

            // Local variables
            try self.emitLocalsDecl(var_count);

            // V8-style optimization: Pre-declare length cache for arg0 (common array operations)
            // This will be lazily initialized on first .length access
            try self.write("    // Cached length for array operations (V8-style)\n");
            if (self.isZig()) {
                try self.write("    var _arg0_len: i64 = -1;\n");
            } else {
                try self.write("    int64_t _arg0_len = -1;\n");
            }
            try self.write("\n");

            // Process each basic block
            for (self.cfg.blocks.items, 0..) |*block, idx| {
                try self.emitBlock(block, idx);
            }

            // Fallthrough return
            if (self.isZig()) {
                try self.write("\n    return qjs.JS_UNDEFINED;\n}\n\n");
            } else {
                try self.write("\n    FROZEN_EXIT_STACK();\n    return JS_UNDEFINED;\n}\n\n");
            }
        }
    }

    /// Emit a single instruction for trampoline execution
    fn emitTrampolineInstruction(self: *SSACodeGen, instr: Instruction) !void {
        const debug = false;

        switch (instr.opcode) {
            // Control flow - set next_block instead of goto/return
            .@"return" => {
                if (debug) try self.write("            /* return */\n");
                try self.write("            frame->result = POP(); next_block = -1; break;\n");
            },
            .return_undef => {
                if (debug) try self.write("            /* return_undef */\n");
                try self.write("            frame->result = JS_UNDEFINED; next_block = -1; break;\n");
            },
            .if_false, .if_false8 => {
                const target = instr.getJumpTarget() orelse 0;
                const target_block = self.cfg.pc_to_block.get(target) orelse 0;
                if (debug) try self.write("            /* if_false */\n");
                try self.print("            {{ JSValue cond = POP(); if (!JS_ToBool(ctx, cond)) {{ JS_FreeValue(ctx, cond); next_block = {d}; break; }} JS_FreeValue(ctx, cond); }}\n", .{target_block});
            },
            .goto, .goto8, .goto16 => {
                const target = instr.getJumpTarget() orelse 0;
                const target_block = self.cfg.pc_to_block.get(target) orelse 0;
                if (debug) try self.write("            /* goto */\n");
                try self.print("            next_block = {d}; break;\n", .{target_block});
            },

            // Self-reference detection (via closure or global)
            .get_var_ref0 => {
                if (self.options.is_self_recursive) {
                    if (debug) try self.write("            /* get_var_ref0 - self reference */\n");
                    self.pending_self_call = true;
                } else {
                    if (debug) try self.write("            /* get_var_ref0 - closure */\n");
                    try self.write("            PUSH(JS_UNDEFINED);\n");
                }
            },
            // Global variable lookup - treat as potential self-call in self-recursive functions
            .get_var, .get_var_undef => {
                if (self.options.is_self_recursive) {
                    if (debug) try self.write("            /* get_var - potential self reference */\n");
                    self.pending_self_call = true;
                    // Don't push anything - the tail_call will handle it
                } else {
                    // Lookup global variable by atom
                    const atom_idx = instr.operand.atom;
                    if (atom_idx < self.options.atom_strings.len) {
                        const name = self.options.atom_strings[atom_idx];
                        if (name.len > 0) {
                            try self.write("            { JSValue global = JS_GetGlobalObject(ctx);\n");
                            try self.write("              JSValue val = JS_GetPropertyStr(ctx, global, \"");
                            try self.writeEscapedString(name);
                            try self.write("\");\n");
                            try self.write("              JS_FreeValue(ctx, global);\n");
                            if (instr.opcode == .get_var_undef) {
                                try self.write("              PUSH(val); }\n");
                            } else {
                                try self.write("              if (JS_IsException(val)) { next_block = -1; frame->result = val; break; }\n");
                                try self.write("              PUSH(val); }\n");
                            }
                        } else {
                            try self.print("            /* get_var: empty atom string at index {d} */\n", .{atom_idx});
                            try self.write("            PUSH(JS_UNDEFINED);\n");
                        }
                    } else {
                        try self.print("            /* get_var: atom {d} out of bounds */\n", .{atom_idx});
                        try self.write("            PUSH(JS_UNDEFINED);\n");
                    }
                }
            },
            // Set global variable
            .put_var, .put_var_init, .put_var_strict => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        try self.write("            { JSValue val = POP();\n");
                        try self.write("              JSValue global = JS_GetGlobalObject(ctx);\n");
                        try self.write("              JS_SetPropertyStr(ctx, global, \"");
                        try self.writeEscapedString(name);
                        try self.write("\", val);\n");
                        try self.write("              JS_FreeValue(ctx, global); }\n");
                    } else {
                        try self.print("            /* put_var: empty atom at {d} */\n", .{atom_idx});
                        try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
                    }
                } else {
                    try self.print("            /* put_var: atom {d} out of bounds */\n", .{atom_idx});
                    try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
                }
            },
            // Check if global variable exists - pushes boolean
            .check_var => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        try self.write("            { JSValue global = JS_GetGlobalObject(ctx);\n");
                        try self.write("              JSAtom prop = JS_NewAtom(ctx, \"");
                        try self.writeEscapedString(name);
                        try self.write("\");\n");
                        try self.write("              int has = JS_HasProperty(ctx, global, prop);\n");
                        try self.write("              JS_FreeAtom(ctx, prop);\n");
                        try self.write("              JS_FreeValue(ctx, global);\n");
                        try self.write("              PUSH(JS_NewBool(ctx, has > 0)); }\n");
                    } else {
                        try self.write("            PUSH(JS_FALSE);\n");
                    }
                } else {
                    try self.write("            PUSH(JS_FALSE);\n");
                }
            },
            // Check if variable can be defined - for redeclaration checks
            // In global scope, this typically succeeds unless it's a const redefinition
            .check_define_var => {
                // atom_u8 format: atom + u8 (flags)
                // In frozen code, we just allow the definition
                if (debug) try self.write("            /* check_define_var - always passes in global scope */\n");
            },
            // Define global variable
            .define_var => {
                // atom_u8 format: atom + u8 (flags)
                // The u8 contains flags for configurable/writable/enumerable
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        // Define as configurable, writable, enumerable (typical var behavior)
                        try self.write("            { JSValue global = JS_GetGlobalObject(ctx);\n");
                        try self.write("              JSAtom prop = JS_NewAtom(ctx, \"");
                        try self.writeEscapedString(name);
                        try self.write("\");\n");
                        try self.write("              JS_DefinePropertyValue(ctx, global, prop, JS_UNDEFINED, JS_PROP_C_W_E);\n");
                        try self.write("              JS_FreeAtom(ctx, prop);\n");
                        try self.write("              JS_FreeValue(ctx, global); }\n");
                    }
                }
            },
            // Delete global variable - pushes boolean result
            .delete_var => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        try self.write("            { JSValue global = JS_GetGlobalObject(ctx);\n");
                        try self.write("              JSAtom prop = JS_NewAtom(ctx, \"");
                        try self.writeEscapedString(name);
                        try self.write("\");\n");
                        try self.write("              int ret = JS_DeleteProperty(ctx, global, prop, 0);\n");
                        try self.write("              JS_FreeAtom(ctx, prop);\n");
                        try self.write("              JS_FreeValue(ctx, global);\n");
                        try self.write("              if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
                        try self.write("              PUSH(JS_NewBool(ctx, ret > 0)); }\n");
                    } else {
                        try self.write("            PUSH(JS_TRUE);\n");
                    }
                } else {
                    try self.write("            PUSH(JS_TRUE);\n");
                }
            },
            // Delete property: delete obj[key]
            // Stack: [..., obj, key] -> [..., bool]
            .delete => {
                try self.write("            { JSValue key = POP(); JSValue obj = POP();\n");
                try self.write("              JSAtom prop;\n");
                try self.write("              if (JS_IsString(key)) {\n");
                try self.write("                const char *str = JS_ToCString(ctx, key);\n");
                try self.write("                prop = JS_NewAtom(ctx, str);\n");
                try self.write("                JS_FreeCString(ctx, str);\n");
                try self.write("              } else {\n");
                try self.write("                prop = JS_ValueToAtom(ctx, key);\n");
                try self.write("              }\n");
                try self.write("              FROZEN_FREE(ctx, key);\n");
                try self.write("              int ret = JS_DeleteProperty(ctx, obj, prop, 0);\n");
                try self.write("              JS_FreeAtom(ctx, prop);\n");
                try self.write("              FROZEN_FREE(ctx, obj);\n");
                try self.write("              if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
                try self.write("              PUSH(JS_NewBool(ctx, ret > 0)); }\n");
            },

            // Property access - get_field: obj.prop (atom from bytecode)
            .get_field, .get_field2 => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        try self.write("            { JSValue obj = POP();\n");
                        try self.write("              JSValue val = JS_GetPropertyStr(ctx, obj, \"");
                        try self.writeEscapedString(name);
                        try self.write("\");\n");
                        try self.write("              FROZEN_FREE(ctx, obj);\n");
                        try self.write("              if (JS_IsException(val)) { next_block = -1; frame->result = val; break; }\n");
                        try self.write("              PUSH(val); }\n");
                    } else {
                        try self.print("            /* get_field: empty atom at {d} */\n", .{atom_idx});
                        try self.write("            { JSValue obj = POP(); FROZEN_FREE(ctx, obj); PUSH(JS_UNDEFINED); }\n");
                    }
                } else {
                    try self.print("            /* get_field: atom {d} out of bounds */\n", .{atom_idx});
                    try self.write("            { JSValue obj = POP(); FROZEN_FREE(ctx, obj); PUSH(JS_UNDEFINED); }\n");
                }
            },

            // Put property - set_field: obj.prop = val
            .put_field => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        try self.write("            { JSValue val = POP(); JSValue obj = POP();\n");
                        try self.write("              int ret = JS_SetPropertyStr(ctx, obj, \"");
                        try self.writeEscapedString(name);
                        try self.write("\", val);\n");
                        try self.write("              FROZEN_FREE(ctx, obj);\n");
                        try self.write("              if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; } }\n");
                    } else {
                        try self.print("            /* put_field: empty atom at {d} */\n", .{atom_idx});
                        try self.write("            { FROZEN_FREE(ctx, POP()); FROZEN_FREE(ctx, POP()); }\n");
                    }
                } else {
                    try self.print("            /* put_field: atom {d} out of bounds */\n", .{atom_idx});
                    try self.write("            { FROZEN_FREE(ctx, POP()); FROZEN_FREE(ctx, POP()); }\n");
                }
            },

            // Method call - call_method: obj.method(args)
            // Stack: [..., func, this, args...] -> [..., result]
            // npop format: u16 operand is argc
            .call_method => {
                const argc = instr.operand.u16;
                try self.write("            {\n");
                // Pop args into temp array (reverse order)
                if (argc > 0) {
                    try self.print("              JSValue args[{d}];\n", .{argc});
                    var i = argc;
                    while (i > 0) {
                        i -= 1;
                        try self.print("              args[{d}] = POP();\n", .{i});
                    }
                }
                try self.write("              JSValue this_obj = POP();\n");
                try self.write("              JSValue func = POP();\n");
                if (argc > 0) {
                    try self.print("              JSValue result = JS_Call(ctx, func, this_obj, {d}, args);\n", .{argc});
                } else {
                    try self.write("              JSValue result = JS_Call(ctx, func, this_obj, 0, NULL);\n");
                }
                try self.write("              FROZEN_FREE(ctx, func);\n");
                try self.write("              FROZEN_FREE(ctx, this_obj);\n");
                if (argc > 0) {
                    var j: u16 = 0;
                    while (j < argc) : (j += 1) {
                        try self.print("              FROZEN_FREE(ctx, args[{d}]);\n", .{j});
                    }
                }
                try self.write("              if (JS_IsException(result)) { next_block = -1; frame->result = result; break; }\n");
                try self.write("              PUSH(result);\n");
                try self.write("            }\n");
            },

            // TDZ (Temporal Dead Zone) check - get local with check for uninitialized
            // .loc format: u16 operand for local variable index
            .get_loc_check => {
                const idx = instr.operand.loc;
                if (debug) try self.print("            /* get_loc_check {d} */\n", .{idx});
                try self.print("            {{ JSValue v = locals[{d}];\n", .{idx});
                try self.write("              if (JS_IsUninitialized(v)) {\n");
                try self.write("                next_block = -1; frame->result = JS_ThrowReferenceError(ctx, \"Cannot access before initialization\"); break;\n");
                try self.write("              }\n");
                try self.write("              PUSH(FROZEN_DUP(ctx, v)); }\n");
            },

            // Argument access
            .get_arg0 => try self.write("            PUSH(argc_inner > 0 ? FROZEN_DUP(ctx, argv[0]) : JS_UNDEFINED);\n"),
            .get_arg1 => try self.write("            PUSH(argc_inner > 1 ? FROZEN_DUP(ctx, argv[1]) : JS_UNDEFINED);\n"),
            .get_arg2 => try self.write("            PUSH(argc_inner > 2 ? FROZEN_DUP(ctx, argv[2]) : JS_UNDEFINED);\n"),
            .get_arg3 => try self.write("            PUSH(argc_inner > 3 ? FROZEN_DUP(ctx, argv[3]) : JS_UNDEFINED);\n"),
            .get_arg => {
                const idx = instr.operand.u16;
                try self.print("            PUSH(argc_inner > {d} ? FROZEN_DUP(ctx, argv[{d}]) : JS_UNDEFINED);\n", .{ idx, idx });
            },

            // Integer constants
            .push_minus1 => try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, -1));\n"),
            .push_0 => try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 0));\n"),
            .push_1 => try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 1));\n"),
            .push_2 => try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 2));\n"),
            .push_3 => try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 3));\n"),
            .push_4 => try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 4));\n"),
            .push_5 => try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 5));\n"),
            .push_6 => try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 6));\n"),
            .push_7 => try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 7));\n"),
            .push_i8 => {
                try self.print("            PUSH(JS_MKVAL(JS_TAG_INT, {d}));\n", .{instr.operand.i8});
            },
            .push_i16 => {
                try self.print("            PUSH(JS_MKVAL(JS_TAG_INT, {d}));\n", .{instr.operand.i16});
            },
            .push_i32 => {
                try self.print("            PUSH(JS_MKVAL(JS_TAG_INT, {d}));\n", .{instr.operand.i32});
            },

            // Constant pool access
            .push_const, .push_const8 => {
                const idx = instr.operand.const_idx;
                if (idx < self.options.constants.len) {
                    const val = self.options.constants[idx];
                    switch (val) {
                        .null_val => try self.write("            PUSH(JS_NULL);\n"),
                        .undefined_val => try self.write("            PUSH(JS_UNDEFINED);\n"),
                        .bool_val => |b| try self.print("            PUSH({s});\n", .{if (b) "JS_TRUE" else "JS_FALSE"}),
                        .int32 => |i| try self.print("            PUSH(JS_MKVAL(JS_TAG_INT, {d}));\n", .{i}),
                        .float64 => |f| try self.print("            PUSH(JS_NewFloat64(ctx, {d}));\n", .{f}),
                        .string => |s| {
                            // Escape string for C
                            try self.write("            PUSH(JS_NewString(ctx, \"");
                            for (s) |c| {
                                if (c == '"' or c == '\\') {
                                    try self.print("\\{c}", .{c});
                                } else if (c == '\n') {
                                    try self.write("\\n");
                                } else if (c == '\r') {
                                    try self.write("\\r");
                                } else if (c == '\t') {
                                    try self.write("\\t");
                                } else if (c < 32 or c > 126) {
                                    try self.print("\\x{x:0>2}", .{c});
                                } else {
                                    try self.print("{c}", .{c});
                                }
                            }
                            try self.write("\"));\n");
                        },
                        .complex => {
                            // Complex constants (objects, arrays, etc) - unsupported
                            try self.print("            /* Unsupported complex constant at index {d} */\n", .{idx});
                            try self.write("            next_block = -1; frame->result = JS_UNDEFINED; break;\n");
                        },
                    }
                } else {
                    try self.print("            /* Constant index {d} out of bounds */\n", .{idx});
                    try self.write("            next_block = -1; frame->result = JS_UNDEFINED; break;\n");
                }
            },

            // Boolean and null constants (Phase 1)
            .push_false => try self.write("            PUSH(JS_FALSE);\n"),
            .push_true => try self.write("            PUSH(JS_TRUE);\n"),
            .@"null" => try self.write("            PUSH(JS_NULL);\n"),
            .undefined => try self.write("            PUSH(JS_UNDEFINED);\n"),

            // Object creation (Phase 1)
            .object => try self.write("            PUSH(JS_NewObject(ctx));\n"),

            // Comparisons
            .lte => try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_lte(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n"),
            .sub => try self.write(
                \\            { JSValue b = POP(), a = POP();
                \\              if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
                \\                  int64_t diff = (int64_t)JS_VALUE_GET_INT(a) - JS_VALUE_GET_INT(b);
                \\                  if (likely(diff >= INT32_MIN && diff <= INT32_MAX)) {
                \\                      PUSH(JS_MKVAL(JS_TAG_INT, (int32_t)diff));
                \\                  } else {
                \\                      PUSH(JS_NewFloat64(ctx, (double)diff));
                \\                  }
                \\              } else {
                \\                  JSValue r = frozen_sub(ctx, a, b);
                \\                  FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);
                \\                  if (JS_IsException(r)) { next_block = -1; frame->result = r; break; }
                \\                  PUSH(r);
                \\              }
                \\            }
                \\
            ),
            .add => try self.write(
                \\            { JSValue b = POP(), a = POP();
                \\              if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
                \\                  int64_t sum = (int64_t)JS_VALUE_GET_INT(a) + JS_VALUE_GET_INT(b);
                \\                  if (likely(sum >= INT32_MIN && sum <= INT32_MAX)) {
                \\                      PUSH(JS_MKVAL(JS_TAG_INT, (int32_t)sum));
                \\                  } else {
                \\                      PUSH(JS_NewFloat64(ctx, (double)sum));
                \\                  }
                \\              } else {
                \\                  JSValue r = frozen_add(ctx, a, b);
                \\                  FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);
                \\                  if (JS_IsException(r)) { next_block = -1; frame->result = r; break; }
                \\                  PUSH(r);
                \\              }
                \\            }
                \\
            ),
            .mul => try self.write(
                \\            { JSValue b = POP(), a = POP();
                \\              if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
                \\                  int64_t prod = (int64_t)JS_VALUE_GET_INT(a) * JS_VALUE_GET_INT(b);
                \\                  if (likely(prod >= INT32_MIN && prod <= INT32_MAX)) {
                \\                      PUSH(JS_MKVAL(JS_TAG_INT, (int32_t)prod));
                \\                  } else {
                \\                      PUSH(JS_NewFloat64(ctx, (double)prod));
                \\                  }
                \\              } else {
                \\                  JSValue r = frozen_mul(ctx, a, b);
                \\                  FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);
                \\                  if (JS_IsException(r)) { next_block = -1; frame->result = r; break; }
                \\                  PUSH(r);
                \\              }
                \\            }
                \\
            ),
            .div => try self.write(
                \\            { JSValue b = POP(), a = POP();
                \\              JSValue r = frozen_div(ctx, a, b);
                \\              FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);
                \\              if (JS_IsException(r)) { next_block = -1; frame->result = r; break; }
                \\              PUSH(r);
                \\            }
                \\
            ),
            .mod => try self.write(
                \\            { JSValue b = POP(), a = POP();
                \\              JSValue r = frozen_mod(ctx, a, b);
                \\              FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);
                \\              if (JS_IsException(r)) { next_block = -1; frame->result = r; break; }
                \\              PUSH(r);
                \\            }
                \\
            ),

            // More comparisons
            .lt => try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_lt(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n"),
            .gt => try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_gt(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n"),
            .gte => try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_gte(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n"),
            .eq => try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, JS_IsStrictEqual(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n"),
            .neq => try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, !JS_IsStrictEqual(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n"),
            .strict_eq => try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, JS_IsStrictEqual(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n"),
            .strict_neq => try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, !JS_IsStrictEqual(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n"),

            // Unary operators
            .neg => try self.write("            { JSValue a = POP(); PUSH(JS_IsNumber(a) ? JS_NewFloat64(ctx, -JS_VALUE_GET_FLOAT64(JS_ToNumber(ctx, a))) : JS_UNDEFINED); FROZEN_FREE(ctx, a); }\n"),
            .not, .lnot => try self.write("            { JSValue a = POP(); PUSH(JS_NewBool(ctx, !JS_ToBool(ctx, a))); FROZEN_FREE(ctx, a); }\n"),
            .inc => try self.write("            { JSValue a = POP(); if (JS_VALUE_GET_TAG(a) == JS_TAG_INT) { PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) + 1)); } else { PUSH(JS_NewFloat64(ctx, JS_VALUE_GET_FLOAT64(JS_ToNumber(ctx, a)) + 1)); } FROZEN_FREE(ctx, a); }\n"),
            .dec => try self.write("            { JSValue a = POP(); if (JS_VALUE_GET_TAG(a) == JS_TAG_INT) { PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) - 1)); } else { PUSH(JS_NewFloat64(ctx, JS_VALUE_GET_FLOAT64(JS_ToNumber(ctx, a)) - 1)); } FROZEN_FREE(ctx, a); }\n"),

            // Stack operations
            .drop => try self.write("            { FROZEN_FREE(ctx, POP()); }\n"),
            .dup => try self.write("            { JSValue v = TOP(); PUSH(FROZEN_DUP(ctx, v)); }\n"),
            .dup1 => try self.write("            { JSValue v = stack[sp-2]; PUSH(FROZEN_DUP(ctx, v)); }\n"),
            .dup2 => try self.write("            { JSValue a = stack[sp-2], b = stack[sp-1]; PUSH(FROZEN_DUP(ctx, a)); PUSH(FROZEN_DUP(ctx, b)); }\n"),
            .dup3 => try self.write("            { JSValue a = stack[sp-3], b = stack[sp-2], c = stack[sp-1]; PUSH(FROZEN_DUP(ctx, a)); PUSH(FROZEN_DUP(ctx, b)); PUSH(FROZEN_DUP(ctx, c)); }\n"),
            .swap => try self.write("            { JSValue b = stack[sp-1], a = stack[sp-2]; stack[sp-2] = b; stack[sp-1] = a; }\n"),
            .swap2 => try self.write("            { JSValue d = stack[sp-1], c = stack[sp-2], b = stack[sp-3], a = stack[sp-4]; stack[sp-4] = c; stack[sp-3] = d; stack[sp-2] = a; stack[sp-1] = b; }\n"),
            .nip => try self.write("            { JSValue top = POP(); FROZEN_FREE(ctx, POP()); PUSH(top); }\n"),
            .nip1 => try self.write("            { JSValue top = POP(); JSValue second = POP(); FROZEN_FREE(ctx, POP()); PUSH(second); PUSH(top); }\n"),
            .insert2 => try self.write("            { JSValue v = POP(); JSValue a = stack[sp-1]; stack[sp-1] = v; PUSH(a); }\n"),
            .insert3 => try self.write("            { JSValue v = POP(); JSValue b = stack[sp-1]; JSValue a = stack[sp-2]; stack[sp-2] = v; stack[sp-1] = a; PUSH(b); }\n"),
            .insert4 => try self.write("            { JSValue v = POP(); JSValue c = stack[sp-1]; JSValue b = stack[sp-2]; JSValue a = stack[sp-3]; stack[sp-3] = v; stack[sp-2] = a; stack[sp-1] = b; PUSH(c); }\n"),
            .perm3 => try self.write("            { JSValue c = stack[sp-1], b = stack[sp-2], a = stack[sp-3]; stack[sp-3] = b; stack[sp-2] = c; stack[sp-1] = a; }\n"),
            .perm4 => try self.write("            { JSValue d = stack[sp-1], c = stack[sp-2], b = stack[sp-3], a = stack[sp-4]; stack[sp-4] = b; stack[sp-3] = c; stack[sp-2] = d; stack[sp-1] = a; }\n"),
            .perm5 => try self.write("            { JSValue e = stack[sp-1], d = stack[sp-2], c = stack[sp-3], b = stack[sp-4], a = stack[sp-5]; stack[sp-5] = b; stack[sp-4] = c; stack[sp-3] = d; stack[sp-2] = e; stack[sp-1] = a; }\n"),
            .rot3l => try self.write("            { JSValue c = stack[sp-1], b = stack[sp-2], a = stack[sp-3]; stack[sp-3] = b; stack[sp-2] = c; stack[sp-1] = a; }\n"),
            .rot3r => try self.write("            { JSValue c = stack[sp-1], b = stack[sp-2], a = stack[sp-3]; stack[sp-3] = c; stack[sp-2] = a; stack[sp-1] = b; }\n"),
            .rot4l => try self.write("            { JSValue d = stack[sp-1], c = stack[sp-2], b = stack[sp-3], a = stack[sp-4]; stack[sp-4] = b; stack[sp-3] = c; stack[sp-2] = d; stack[sp-1] = a; }\n"),
            .rot5l => try self.write("            { JSValue e = stack[sp-1], d = stack[sp-2], c = stack[sp-3], b = stack[sp-4], a = stack[sp-5]; stack[sp-5] = b; stack[sp-4] = c; stack[sp-3] = d; stack[sp-2] = e; stack[sp-1] = a; }\n"),

            // Local variables (use frame->locals array)
            .get_loc0 => try self.write("            PUSH(FROZEN_DUP(ctx, frame->locals[0]));\n"),
            .get_loc1 => try self.write("            PUSH(FROZEN_DUP(ctx, frame->locals[1]));\n"),
            .get_loc2 => try self.write("            PUSH(FROZEN_DUP(ctx, frame->locals[2]));\n"),
            .get_loc3 => try self.write("            PUSH(FROZEN_DUP(ctx, frame->locals[3]));\n"),
            .get_loc8 => {
                const idx = instr.operand.u8;
                try self.print("            PUSH(FROZEN_DUP(ctx, frame->locals[{d}]));\n", .{idx});
            },
            .get_loc => {
                const idx = instr.operand.u16;
                try self.print("            PUSH(FROZEN_DUP(ctx, frame->locals[{d}]));\n", .{idx});
            },
            .put_loc0 => try self.write("            { FROZEN_FREE(ctx, frame->locals[0]); frame->locals[0] = POP(); }\n"),
            .put_loc1 => try self.write("            { FROZEN_FREE(ctx, frame->locals[1]); frame->locals[1] = POP(); }\n"),
            .put_loc2 => try self.write("            { FROZEN_FREE(ctx, frame->locals[2]); frame->locals[2] = POP(); }\n"),
            .put_loc3 => try self.write("            { FROZEN_FREE(ctx, frame->locals[3]); frame->locals[3] = POP(); }\n"),
            .put_loc8 => {
                const idx = instr.operand.u8;
                try self.print("            {{ FROZEN_FREE(ctx, frame->locals[{d}]); frame->locals[{d}] = POP(); }}\n", .{ idx, idx });
            },
            .put_loc => {
                const idx = instr.operand.u16;
                try self.print("            {{ FROZEN_FREE(ctx, frame->locals[{d}]); frame->locals[{d}] = POP(); }}\n", .{ idx, idx });
            },
            .set_loc0 => try self.write("            { FROZEN_FREE(ctx, frame->locals[0]); frame->locals[0] = FROZEN_DUP(ctx, TOP()); }\n"),
            .set_loc1 => try self.write("            { FROZEN_FREE(ctx, frame->locals[1]); frame->locals[1] = FROZEN_DUP(ctx, TOP()); }\n"),
            .set_loc2 => try self.write("            { FROZEN_FREE(ctx, frame->locals[2]); frame->locals[2] = FROZEN_DUP(ctx, TOP()); }\n"),
            .set_loc3 => try self.write("            { FROZEN_FREE(ctx, frame->locals[3]); frame->locals[3] = FROZEN_DUP(ctx, TOP()); }\n"),
            .set_loc8 => {
                const idx = instr.operand.u8;
                try self.print("            {{ FROZEN_FREE(ctx, frame->locals[{d}]); frame->locals[{d}] = FROZEN_DUP(ctx, TOP()); }}\n", .{ idx, idx });
            },
            .set_loc => {
                const idx = instr.operand.u16;
                try self.print("            {{ FROZEN_FREE(ctx, frame->locals[{d}]); frame->locals[{d}] = FROZEN_DUP(ctx, TOP()); }}\n", .{ idx, idx });
            },

            // Local variable increment/decrement (loop optimizations)
            .inc_loc => {
                const idx = instr.operand.u8;
                try self.print("            {{ JSValue old = frame->locals[{d}]; frame->locals[{d}] = frozen_add(ctx, old, JS_MKVAL(JS_TAG_INT, 1)); }}\n", .{ idx, idx });
            },
            .dec_loc => {
                const idx = instr.operand.u8;
                try self.print("            {{ JSValue old = frame->locals[{d}]; frame->locals[{d}] = frozen_sub(ctx, old, JS_MKVAL(JS_TAG_INT, 1)); }}\n", .{ idx, idx });
            },
            .add_loc => {
                const idx = instr.operand.u8;
                try self.print("            {{ JSValue v = POP(), old = frame->locals[{d}]; frame->locals[{d}] = frozen_add(ctx, old, v); }}\n", .{ idx, idx });
            },

            // Bitwise operations
            .@"and" => try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) & JS_VALUE_GET_INT(b))); }\n"),
            .@"or" => try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) | JS_VALUE_GET_INT(b))); }\n"),
            .xor => try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) ^ JS_VALUE_GET_INT(b))); }\n"),
            .shl => try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) << (JS_VALUE_GET_INT(b) & 31))); }\n"),
            .sar => try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) >> (JS_VALUE_GET_INT(b) & 31))); }\n"),
            .shr => try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_MKVAL(JS_TAG_INT, (int32_t)((uint32_t)JS_VALUE_GET_INT(a) >> (JS_VALUE_GET_INT(b) & 31)))); }\n"),

            // Power operator
            .pow => {
                try self.write("            { JSValue b = POP(), a = POP();\n");
                try self.write("              double da, db;\n");
                try self.write("              JS_ToFloat64(ctx, &da, a); JS_ToFloat64(ctx, &db, b);\n");
                try self.write("              FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);\n");
                try self.write("              PUSH(JS_NewFloat64(ctx, pow(da, db))); }\n");
            },

            // instanceof operator
            .instanceof => {
                try self.write("            { JSValue rhs = POP(), lhs = POP();\n");
                try self.write("              int ret = JS_IsInstanceOf(ctx, lhs, rhs);\n");
                try self.write("              FROZEN_FREE(ctx, lhs); FROZEN_FREE(ctx, rhs);\n");
                try self.write("              if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
                try self.write("              PUSH(JS_NewBool(ctx, ret)); }\n");
            },

            // in operator
            .in => {
                try self.write("            { JSValue rhs = POP(), lhs = POP();\n");
                try self.write("              int ret = JS_HasProperty(ctx, rhs, JS_ValueToAtom(ctx, lhs));\n");
                try self.write("              FROZEN_FREE(ctx, lhs); FROZEN_FREE(ctx, rhs);\n");
                try self.write("              if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
                try self.write("              PUSH(JS_NewBool(ctx, ret)); }\n");
            },

            // Push atom value (string constant from atom table)
            .push_atom_value => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        try self.write("            PUSH(JS_NewString(ctx, \"");
                        try self.writeEscapedString(name);
                        try self.write("\"));\n");
                    } else {
                        try self.write("            PUSH(JS_NewString(ctx, \"\"));\n");
                    }
                } else {
                    try self.print("            /* push_atom_value: atom {d} out of bounds */\n", .{atom_idx});
                    try self.write("            PUSH(JS_UNDEFINED);\n");
                }
            },

            // Non-recursive function calls in trampoline
            .call0 => {
                try self.write("            { JSValue func = POP();\n");
                try self.write("              JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 0, NULL);\n");
                try self.write("              FROZEN_FREE(ctx, func);\n");
                try self.write("              if (JS_IsException(ret)) { next_block = -1; frame->result = ret; break; }\n");
                try self.write("              PUSH(ret); }\n");
                self.pending_self_call = false;
            },
            .call1 => {
                try self.write("            { JSValue arg0 = POP(); JSValue func = POP();\n");
                try self.write("              JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 1, &arg0);\n");
                try self.write("              FROZEN_FREE(ctx, func); FROZEN_FREE(ctx, arg0);\n");
                try self.write("              if (JS_IsException(ret)) { next_block = -1; frame->result = ret; break; }\n");
                try self.write("              PUSH(ret); }\n");
                self.pending_self_call = false;
            },
            .call2 => {
                try self.write("            { JSValue args[2]; args[1] = POP(); args[0] = POP(); JSValue func = POP();\n");
                try self.write("              JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 2, args);\n");
                try self.write("              FROZEN_FREE(ctx, func); FROZEN_FREE(ctx, args[0]); FROZEN_FREE(ctx, args[1]);\n");
                try self.write("              if (JS_IsException(ret)) { next_block = -1; frame->result = ret; break; }\n");
                try self.write("              PUSH(ret); }\n");
                self.pending_self_call = false;
            },
            .call3 => {
                try self.write("            { JSValue args[3]; args[2] = POP(); args[1] = POP(); args[0] = POP(); JSValue func = POP();\n");
                try self.write("              JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 3, args);\n");
                try self.write("              FROZEN_FREE(ctx, func); FROZEN_FREE(ctx, args[0]); FROZEN_FREE(ctx, args[1]); FROZEN_FREE(ctx, args[2]);\n");
                try self.write("              if (JS_IsException(ret)) { next_block = -1; frame->result = ret; break; }\n");
                try self.write("              PUSH(ret); }\n");
                self.pending_self_call = false;
            },
            .call => {
                const argc: u16 = instr.operand.u16;
                try self.write("            {\n");
                // Pop args into temp array (reverse order)
                if (argc > 0) {
                    try self.print("              JSValue args[{d}];\n", .{argc});
                    var i = argc;
                    while (i > 0) {
                        i -= 1;
                        try self.print("              args[{d}] = POP();\n", .{i});
                    }
                }
                try self.write("              JSValue func = POP();\n");
                if (argc > 0) {
                    try self.print("              JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, {d}, args);\n", .{argc});
                } else {
                    try self.write("              JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 0, NULL);\n");
                }
                try self.write("              FROZEN_FREE(ctx, func);\n");
                if (argc > 0) {
                    var j: u16 = 0;
                    while (j < argc) : (j += 1) {
                        try self.print("              FROZEN_FREE(ctx, args[{d}]);\n", .{j});
                    }
                }
                try self.write("              if (JS_IsException(ret)) { next_block = -1; frame->result = ret; break; }\n");
                try self.write("              PUSH(ret);\n");
                try self.write("            }\n");
                self.pending_self_call = false;
            },

            // Constructor call - new Foo(args...)
            .call_constructor => {
                const argc: u16 = instr.operand.u16;
                try self.print("            {{ JSValue args[{d} > 0 ? {d} : 1]; ", .{ argc, argc });
                // Pop args in reverse order
                var i: u16 = argc;
                while (i > 0) {
                    i -= 1;
                    try self.print("args[{d}] = POP(); ", .{i});
                }
                try self.write("JSValue ctor = POP();\n");
                try self.print("              JSValue ret = JS_CallConstructor(ctx, ctor, {d}, args);\n", .{argc});
                try self.write("              FROZEN_FREE(ctx, ctor);");
                i = 0;
                while (i < argc) : (i += 1) {
                    try self.print(" FROZEN_FREE(ctx, args[{d}]);", .{i});
                }
                try self.write("\n              if (JS_IsException(ret)) { next_block = -1; frame->result = ret; break; }\n");
                try self.write("              PUSH(ret); }\n");
                self.pending_self_call = false;
            },

            // Method call with tail call optimization
            .tail_call_method => {
                const argc: u16 = instr.operand.u16;
                try self.print("            {{ JSValue args[{d} > 0 ? {d} : 1]; ", .{ argc, argc });
                var i: u16 = argc;
                while (i > 0) {
                    i -= 1;
                    try self.print("args[{d}] = POP(); ", .{i});
                }
                try self.write("JSValue method = POP(); JSValue this_obj = POP();\n");
                try self.print("              JSValue ret = JS_Call(ctx, method, this_obj, {d}, args);\n", .{argc});
                try self.write("              FROZEN_FREE(ctx, method); FROZEN_FREE(ctx, this_obj);");
                i = 0;
                while (i < argc) : (i += 1) {
                    try self.print(" FROZEN_FREE(ctx, args[{d}]);", .{i});
                }
                try self.write("\n              if (JS_IsException(ret)) { next_block = -1; frame->result = ret; break; }\n");
                try self.write("              frame->result = ret; next_block = -1; break; }\n");
                self.pending_self_call = false;
            },

            // Array.from() / spread operator
            .array_from => {
                // Stack: array, length -> create new array from iterable
                try self.write("            { JSValue len = POP(); JSValue arr = POP();\n");
                try self.write("              JSValue new_arr = JS_NewArray(ctx);\n");
                try self.write("              if (!JS_IsException(new_arr)) {\n");
                try self.write("                int64_t length = 0;\n");
                try self.write("                JS_ToInt64(ctx, &length, len);\n");
                try self.write("                for (int64_t i = 0; i < length; i++) {\n");
                try self.write("                  JSValue elem = JS_GetPropertyInt64(ctx, arr, i);\n");
                try self.write("                  JS_SetPropertyInt64(ctx, new_arr, i, elem);\n");
                try self.write("                }\n");
                try self.write("              }\n");
                try self.write("              FROZEN_FREE(ctx, arr); FROZEN_FREE(ctx, len);\n");
                try self.write("              if (JS_IsException(new_arr)) { next_block = -1; frame->result = new_arr; break; }\n");
                try self.write("              PUSH(new_arr); }\n");
            },

            // typeof_is_function - optimized typeof x === "function" check
            .typeof_is_function => {
                try self.write("            { JSValue v = POP(); PUSH(JS_NewBool(ctx, JS_IsFunction(ctx, v))); FROZEN_FREE(ctx, v); }\n");
            },

            // Closure variable access (get_var_ref0 handled separately for self-recursion detection)
            .get_var_ref1, .get_var_ref2, .get_var_ref3 => {
                // Just push undefined for closure accesses - frozen functions don't support closures
                try self.write("            PUSH(JS_UNDEFINED);\n");
            },
            .set_var_ref0, .set_var_ref1, .set_var_ref2, .set_var_ref3 => {
                // Discard the value for closure sets - frozen functions don't support closures
                try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
            },
            .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3 => {
                // Discard the value for closure puts - frozen functions don't support closures
                try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
            },

            // Array element access
            .get_array_el => {
                try self.write("            { JSValue idx = POP(); JSValue arr = POP();\n");
                try self.write("              int64_t i; JS_ToInt64(ctx, &i, idx);\n");
                try self.write("              JSValue ret = JS_GetPropertyInt64(ctx, arr, i);\n");
                try self.write("              FROZEN_FREE(ctx, arr); FROZEN_FREE(ctx, idx);\n");
                try self.write("              if (JS_IsException(ret)) { next_block = -1; frame->result = ret; break; }\n");
                try self.write("              PUSH(ret); }\n");
            },
            .put_array_el => {
                try self.write("            { JSValue val = POP(); JSValue idx = POP(); JSValue arr = POP();\n");
                try self.write("              int64_t i; JS_ToInt64(ctx, &i, idx);\n");
                try self.write("              int ret = JS_SetPropertyInt64(ctx, arr, i, val);\n");
                try self.write("              FROZEN_FREE(ctx, arr); FROZEN_FREE(ctx, idx);\n");
                try self.write("              if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; } }\n");
            },

            // Define field on object
            .define_field => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    try self.write("            { JSValue val = POP(); JSValue obj = TOP();\n");
                    try self.write("              JSAtom prop = JS_NewAtom(ctx, \"");
                    try self.writeEscapedString(name);
                    try self.write("\");\n");
                    try self.write("              JS_DefinePropertyValue(ctx, obj, prop, val, JS_PROP_C_W_E);\n");
                    try self.write("              JS_FreeAtom(ctx, prop); }\n");
                } else {
                    try self.write("            /* define_field: atom out of bounds */\n");
                }
            },
            .set_name => {
                // Set function name - no-op for frozen functions
                try self.write("            /* set_name: ignored */\n");
            },

            // Convert to property key (string/symbol)
            .to_propkey => {
                try self.write("            { JSValue v = POP(); PUSH(JS_ToPropertyKey(ctx, v)); FROZEN_FREE(ctx, v); }\n");
            },

            // Convert to object wrapper
            .to_object => {
                try self.write("            { JSValue v = POP();\n");
                try self.write("              JSValue obj = JS_ToObject(ctx, v);\n");
                try self.write("              FROZEN_FREE(ctx, v);\n");
                try self.write("              if (JS_IsException(obj)) { next_block = -1; frame->result = obj; break; }\n");
                try self.write("              PUSH(obj); }\n");
            },

            // Exception handling
            .nip_catch => {
                // Remove catch handler from stack
                try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
            },

            // Conditional jump - if_true (opposite of if_false)
            .if_true, .if_true8 => {
                const target = instr.getJumpTarget() orelse 0;
                const target_block = self.cfg.pc_to_block.get(target) orelse 0;
                try self.print("            {{ JSValue cond = POP(); if (JS_ToBool(ctx, cond)) {{ JS_FreeValue(ctx, cond); next_block = {d}; break; }} JS_FreeValue(ctx, cond); }}\n", .{target_block});
            },

            // Push this value
            .push_this => {
                try self.write("            PUSH(JS_DupValue(ctx, this_val));\n");
            },

            // get_super: Get prototype of object (for super.method() calls)
            .get_super => {
                try self.write("            { JSValue obj = POP();\n");
                try self.write("              JSValue proto = JS_GetPrototype(ctx, obj);\n");
                try self.write("              FROZEN_FREE(ctx, obj);\n");
                try self.write("              if (JS_IsException(proto)) { next_block = -1; frame->result = proto; break; }\n");
                try self.write("              PUSH(proto); }\n");
            },

            // regexp: Create RegExp from pattern and flags strings
            .regexp => {
                try self.write("            { JSValue flags = POP(); JSValue pattern = POP();\n");
                try self.write("              JSValue global = JS_GetGlobalObject(ctx);\n");
                try self.write("              JSValue RegExp = JS_GetPropertyStr(ctx, global, \"RegExp\");\n");
                try self.write("              JS_FreeValue(ctx, global);\n");
                try self.write("              JSValue args[2] = { pattern, flags };\n");
                try self.write("              JSValue rx = JS_CallConstructor(ctx, RegExp, 2, args);\n");
                try self.write("              JS_FreeValue(ctx, RegExp);\n");
                try self.write("              FROZEN_FREE(ctx, pattern); FROZEN_FREE(ctx, flags);\n");
                try self.write("              if (JS_IsException(rx)) { next_block = -1; frame->result = rx; break; }\n");
                try self.write("              PUSH(rx); }\n");
            },

            // Throw exception
            .throw => {
                try self.write("            { JSValue exc = POP(); JS_Throw(ctx, exc); next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
            },

            // Catch - push the exception
            .@"catch" => {
                try self.write("            { JSValue exc = JS_GetException(ctx); PUSH(exc); }\n");
            },

            // put_var_ref_check - closure variable assignment with TDZ check
            .put_var_ref_check => {
                // For frozen functions, just discard the value (closures not supported)
                try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
            },

            // TDZ (Temporal Dead Zone) opcodes for let/const
            .set_loc_uninitialized => {
                const idx = instr.operand.loc;
                try self.print("            frame->locals[{d}] = JS_UNINITIALIZED;\n", .{idx});
            },
            .put_loc_check => {
                const idx = instr.operand.loc;
                try self.print("            {{ JSValue v = frame->locals[{d}];\n", .{idx});
                try self.write("              if (JS_IsUninitialized(v)) {\n");
                try self.write("                next_block = -1; frame->result = JS_ThrowReferenceError(ctx, \"Cannot access before initialization\"); break;\n");
                try self.write("              }\n");
                try self.print("              FROZEN_FREE(ctx, frame->locals[{d}]); frame->locals[{d}] = POP(); }}\n", .{ idx, idx });
            },
            .put_loc_check_init => {
                const idx = instr.operand.loc;
                try self.print("            {{ JSValue v = frame->locals[{d}];\n", .{idx});
                try self.write("              if (!JS_IsUninitialized(v)) {\n");
                try self.write("                next_block = -1; frame->result = JS_ThrowReferenceError(ctx, \"Identifier already declared\"); break;\n");
                try self.write("              }\n");
                try self.print("              frame->locals[{d}] = POP(); }}\n", .{idx});
            },
            .get_var_ref_check => {
                // Closure variable with TDZ check - just push undefined (frozen doesn't support closures)
                try self.write("            PUSH(JS_UNDEFINED);\n");
            },
            .get_var_ref => {
                // Generic closure variable access - push undefined
                try self.write("            PUSH(JS_UNDEFINED);\n");
            },
            .set_var_ref => {
                // Generic closure variable set - discard value
                try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
            },

            // Subroutine calls (for finally blocks)
            .gosub => {
                const target = instr.getJumpTarget() orelse 0;
                const target_block = self.cfg.pc_to_block.get(target) orelse 0;
                try self.print("            next_block = {d}; break;\n", .{target_block});
            },
            .ret => {
                // Return from subroutine - for finally blocks
                try self.write("            /* ret: no-op in frozen */\n");
            },

            // Property key conversion variants
            .to_propkey2 => {
                try self.write("            { JSValue v = POP(); PUSH(JS_ToPropertyKey(ctx, v)); FROZEN_FREE(ctx, v); }\n");
            },

            // Closure creation - return undefined
            .fclosure => {
                try self.write("            PUSH(JS_UNDEFINED); /* fclosure not supported */\n");
            },

            // Invalid opcode - should not happen
            .invalid => {
                try self.write("            /* invalid opcode */\n");
            },

            // Argument assignment
            .put_arg0 => try self.write("            { FROZEN_FREE(ctx, frame->args[0]); frame->args[0] = POP(); }\n"),
            .put_arg1 => try self.write("            { FROZEN_FREE(ctx, frame->args[1]); frame->args[1] = POP(); }\n"),
            .put_arg2 => try self.write("            { FROZEN_FREE(ctx, frame->args[2]); frame->args[2] = POP(); }\n"),
            .put_arg3 => try self.write("            { FROZEN_FREE(ctx, frame->args[3]); frame->args[3] = POP(); }\n"),
            .put_arg => {
                const idx = instr.operand.arg;
                try self.print("            {{ FROZEN_FREE(ctx, frame->args[{d}]); frame->args[{d}] = POP(); }}\n", .{ idx, idx });
            },
            .set_arg => {
                const idx = instr.operand.arg;
                // set_arg: like put_arg but leaves value on stack
                try self.print("            {{ FROZEN_FREE(ctx, frame->args[{d}]); frame->args[{d}] = FROZEN_DUP(ctx, TOP()); }}\n", .{ idx, idx });
            },

            // Iterator opcodes
            .for_in_start => {
                try self.write("            if (js_frozen_for_in_start(ctx, &stack[sp - 1])) { next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
            },
            .for_in_next => {
                try self.write("            if (js_frozen_for_in_next(ctx, &stack[sp - 1])) { next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
                try self.write("            sp += 2;\n");
            },
            .for_of_start => {
                try self.write("            if (js_frozen_for_of_start(ctx, &stack[sp - 1], 0)) { next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
                try self.write("            sp += 2;\n");
            },
            .for_of_next => {
                const offset = instr.operand.u8;
                try self.print("            if (js_frozen_for_of_next(ctx, &stack[sp], -{d})) {{ next_block = -1; frame->result = JS_EXCEPTION; break; }}\n", .{offset});
                try self.write("            sp += 2;\n");
            },
            .iterator_close => {
                try self.write("            { sp--; /* drop catch_offset */\n");
                try self.write("              FROZEN_FREE(ctx, stack[--sp]); /* drop next method */\n");
                try self.write("              JSValue iter = stack[--sp];\n");
                try self.write("              if (!JS_IsUndefined(iter)) {\n");
                try self.write("                JSValue ret_method = JS_GetPropertyStr(ctx, iter, \"return\");\n");
                try self.write("                if (!JS_IsUndefined(ret_method) && !JS_IsNull(ret_method)) {\n");
                try self.write("                  JSValue ret = JS_Call(ctx, ret_method, iter, 0, NULL);\n");
                try self.write("                  JS_FreeValue(ctx, ret_method);\n");
                try self.write("                  if (JS_IsException(ret)) { FROZEN_FREE(ctx, iter); next_block = -1; frame->result = ret; break; }\n");
                try self.write("                  JS_FreeValue(ctx, ret);\n");
                try self.write("                } else { JS_FreeValue(ctx, ret_method); }\n");
                try self.write("                FROZEN_FREE(ctx, iter);\n");
                try self.write("              } }\n");
            },
            // iterator_check_object: Verify top of stack is an object
            .iterator_check_object => {
                try self.write("            if (!JS_IsObject(stack[sp - 1])) {\n");
                try self.write("              next_block = -1; frame->result = JS_ThrowTypeError(ctx, \"iterator must return an object\"); break;\n");
                try self.write("            }\n");
            },
            // iterator_get_value_done: Extract value and done from iterator result
            .iterator_get_value_done => {
                try self.write("            { JSValue obj = stack[sp - 1];\n");
                try self.write("              JSValue value = JS_GetPropertyStr(ctx, obj, \"value\");\n");
                try self.write("              JSValue done_val = JS_GetPropertyStr(ctx, obj, \"done\");\n");
                try self.write("              int done = JS_ToBool(ctx, done_val);\n");
                try self.write("              JS_FreeValue(ctx, done_val);\n");
                try self.write("              FROZEN_FREE(ctx, obj);\n");
                try self.write("              stack[sp - 1] = value;\n");
                try self.write("              stack[sp++] = JS_NewBool(ctx, done); }\n");
            },
            // iterator_next: Call next method on iterator
            .iterator_next => {
                try self.write("            { JSValue val = stack[sp - 1];\n");
                try self.write("              JSValue next = stack[sp - 3];\n");
                try self.write("              JSValue iter = stack[sp - 4];\n");
                try self.write("              JSValue ret = JS_Call(ctx, next, iter, 1, &val);\n");
                try self.write("              FROZEN_FREE(ctx, val);\n");
                try self.write("              if (JS_IsException(ret)) { next_block = -1; frame->result = ret; break; }\n");
                try self.write("              stack[sp - 1] = ret; }\n");
            },
            // iterator_call: Call throw/return method on iterator
            .iterator_call => {
                const flags = instr.operand.u8;
                const method_name = if (flags & 1 != 0) "throw" else "return";
                try self.write("            { JSValue val = stack[sp - 1];\n");
                try self.write("              JSValue iter = stack[sp - 4];\n");
                try self.print("              JSValue method = JS_GetPropertyStr(ctx, iter, \"{s}\");\n", .{method_name});
                try self.write("              if (JS_IsUndefined(method) || JS_IsNull(method)) {\n");
                try self.write("                JS_FreeValue(ctx, method);\n");
                try self.write("                stack[sp - 1] = val;\n");
                try self.write("                stack[sp++] = JS_TRUE;\n");
                try self.write("              } else {\n");
                if (flags & 2 != 0) {
                    try self.write("                JSValue ret = JS_Call(ctx, method, iter, 0, NULL);\n");
                    try self.write("                FROZEN_FREE(ctx, val);\n");
                } else {
                    try self.write("                JSValue ret = JS_Call(ctx, method, iter, 1, &val);\n");
                    try self.write("                FROZEN_FREE(ctx, val);\n");
                }
                try self.write("                JS_FreeValue(ctx, method);\n");
                try self.write("                if (JS_IsException(ret)) { next_block = -1; frame->result = ret; break; }\n");
                try self.write("                stack[sp - 1] = ret;\n");
                try self.write("                stack[sp++] = JS_FALSE;\n");
                try self.write("              } }\n");
            },

            // Type operators
            .typeof => {
                try self.write("            { JSValue v = POP(); JSValue t = frozen_typeof(ctx, v); FROZEN_FREE(ctx, v); PUSH(t); }\n");
            },
            .typeof_is_undefined => {
                try self.write("            { JSValue v = POP(); PUSH(JS_NewBool(ctx, JS_IsUndefined(v))); FROZEN_FREE(ctx, v); }\n");
            },
            .get_length => {
                try self.write("            { JSValue obj = POP(); int64_t len = frozen_get_length(ctx, obj); FROZEN_FREE(ctx, obj); PUSH(JS_NewInt64(ctx, len)); }\n");
            },

            // Reference operations (for with/delete)
            .get_ref_value => {
                try self.write("            { JSValue prop = stack[sp - 1];\n");
                try self.write("              JSValue obj = stack[sp - 2];\n");
                try self.write("              if (JS_IsUndefined(obj)) {\n");
                try self.write("                const char *name = JS_ToCString(ctx, prop);\n");
                try self.write("                JS_ThrowReferenceError(ctx, \"%s is not defined\", name ? name : \"?\");\n");
                try self.write("                if (name) JS_FreeCString(ctx, name);\n");
                try self.write("                next_block = -1; frame->result = JS_EXCEPTION; break;\n");
                try self.write("              }\n");
                try self.write("              JSValue val = JS_GetPropertyValue(ctx, obj, JS_DupValue(ctx, prop));\n");
                try self.write("              if (JS_IsException(val)) { next_block = -1; frame->result = val; break; }\n");
                try self.write("              PUSH(val); }\n");
            },
            .make_var_ref => {
                const atom_idx = instr.operand.atom;
                try self.write("            { JSValue global = JS_GetGlobalObject(ctx);\n");
                try self.write("              PUSH(global);\n");
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        try self.write("              PUSH(JS_NewString(ctx, \"");
                        try self.writeEscapedString(name);
                        try self.write("\")); }\n");
                    } else {
                        try self.write("              PUSH(JS_UNDEFINED); }\n");
                    }
                } else {
                    try self.write("              PUSH(JS_UNDEFINED); }\n");
                }
            },
            .put_ref_value => {
                try self.write("            { JSValue val = POP(); JSValue prop = POP(); JSValue obj = POP();\n");
                try self.write("              if (JS_IsUndefined(obj)) {\n");
                try self.write("                obj = JS_GetGlobalObject(ctx);\n");
                try self.write("              }\n");
                try self.write("              int ret = JS_SetPropertyValue(ctx, obj, prop, val, JS_PROP_THROW_STRICT);\n");
                try self.write("              FROZEN_FREE(ctx, obj);\n");
                try self.write("              if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; } }\n");
            },

            // Simple push/check opcodes
            .is_undefined_or_null => {
                try self.write("            { JSValue v = POP(); PUSH(JS_NewBool(ctx, JS_IsUndefined(v) || JS_IsNull(v))); FROZEN_FREE(ctx, v); }\n");
            },
            .push_empty_string => {
                try self.write("            PUSH(JS_NewString(ctx, \"\"));\n");
            },
            .get_loc0_loc1 => {
                // Push both loc0 and loc1
                try self.write("            PUSH(FROZEN_DUP(ctx, locals[0]));\n");
                try self.write("            PUSH(FROZEN_DUP(ctx, locals[1]));\n");
            },
            .special_object => {
                const obj_type = instr.operand.u8;
                switch (obj_type) {
                    0 => try self.write("            PUSH(JS_NewArguments(ctx, argc_inner, argv));\n"), // arguments
                    2 => try self.print("            PUSH(JS_DupValue(ctx, _{s}_this_func));\n", .{self.options.func_name}), // this_func
                    3 => try self.write("            PUSH(JS_DupValue(ctx, this_val));\n"), // new_target
                    5, 7 => try self.write("            PUSH(JS_NewObjectProto(ctx, JS_NULL));\n"), // var_object, null_proto
                    else => try self.write("            PUSH(JS_UNDEFINED); /* unsupported special_object */\n"),
                }
            },
            .add_brand => {
                try self.write("            { JSValue func = POP(); JSValue obj = POP();\n");
                try self.write("              int ret = JS_FrozenAddBrand(ctx, obj, func);\n");
                try self.write("              FROZEN_FREE(ctx, obj); FROZEN_FREE(ctx, func);\n");
                try self.write("              if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; } }\n");
            },
            .check_brand => {
                try self.write("            { int ret = JS_FrozenCheckBrand(ctx, stack[sp - 2], stack[sp - 1]);\n");
                try self.write("              if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; } }\n");
            },
            .private_symbol => {
                const atom_idx = instr.operand.atom;
                // Must create runtime atom from string, bytecode atom indices don't match runtime
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        try self.print("            {{ JSAtom atom = JS_NewAtomLen(ctx, \"{s}\", {d}); PUSH(JS_NewSymbolFromAtom(ctx, atom, JS_ATOM_TYPE_PRIVATE)); JS_FreeAtom(ctx, atom); }}\n", .{ name, name.len });
                    } else {
                        try self.write("            PUSH(JS_UNDEFINED);\n");
                    }
                } else {
                    try self.write("            PUSH(JS_UNDEFINED);\n");
                }
            },
            .private_in => {
                try self.write("            { int ret = js_frozen_private_in(ctx, &stack[sp - 2]);\n");
                try self.write("              if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
                try self.write("              sp--; }\n");
            },

            // Private field access
            .get_private_field => {
                try self.write("            { JSValue name = POP(); JSValue obj = POP();\n");
                try self.write("              JSValue val = JS_FrozenGetPrivateField(ctx, obj, name);\n");
                try self.write("              FROZEN_FREE(ctx, obj); FROZEN_FREE(ctx, name);\n");
                try self.write("              if (JS_IsException(val)) { next_block = -1; frame->result = val; break; }\n");
                try self.write("              PUSH(val); }\n");
            },
            .put_private_field => {
                try self.write("            { JSValue val = POP(); JSValue name = POP(); JSValue obj = POP();\n");
                try self.write("              int ret = JS_FrozenSetPrivateField(ctx, obj, name, val);\n");
                try self.write("              FROZEN_FREE(ctx, obj); FROZEN_FREE(ctx, name);\n");
                try self.write("              if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; } }\n");
            },
            .define_private_field => {
                try self.write("            { JSValue val = POP(); JSValue name = POP(); JSValue obj = POP();\n");
                try self.write("              int ret = JS_FrozenDefinePrivateField(ctx, obj, name, val);\n");
                try self.write("              FROZEN_FREE(ctx, name);\n");
                try self.write("              if (ret < 0) { FROZEN_FREE(ctx, obj); next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
                try self.write("              PUSH(val); }\n");
            },

            // More simple opcodes
            .plus => {
                try self.write("            { JSValue v = POP(); JSValue r = JS_ToNumber(ctx, v); FROZEN_FREE(ctx, v);\n");
                try self.write("              if (JS_IsException(r)) { next_block = -1; frame->result = r; break; }\n");
                try self.write("              PUSH(r); }\n");
            },
            .is_null => {
                try self.write("            { JSValue v = POP(); PUSH(JS_NewBool(ctx, JS_IsNull(v))); FROZEN_FREE(ctx, v); }\n");
            },
            .post_inc, .post_dec => {
                const is_inc = (instr.opcode == .post_inc);
                try self.write("            { JSValue v = POP();\n");
                try self.write("              double d;\n");
                try self.write("              if (JS_ToFloat64(ctx, &d, v) < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
                try self.write("              PUSH(JS_NewFloat64(ctx, d));\n");
                if (is_inc) {
                    try self.write("              PUSH(JS_NewFloat64(ctx, d + 1)); }\n");
                } else {
                    try self.write("              PUSH(JS_NewFloat64(ctx, d - 1)); }\n");
                }
            },
            .rest => {
                const first_arg = instr.operand.u16;
                try self.print("            {{ JSValue arr = JS_NewArray(ctx);\n", .{});
                try self.print("              for (int i = {d}; i < argc_inner; i++) {{\n", .{first_arg});
                try self.write("                JS_SetPropertyUint32(ctx, arr, i - ");
                try self.print("{d}, JS_DupValue(ctx, argv[i]));\n", .{first_arg});
                try self.write("              }\n");
                try self.write("              PUSH(arr); }\n");
            },
            .append => {
                // Append element to array
                try self.write("            { JSValue elem = POP();\n");
                try self.write("              JSValue arr = stack[sp - 1];\n");
                try self.write("              int64_t len = frozen_get_length(ctx, arr);\n");
                try self.write("              JS_SetPropertyInt64(ctx, arr, len, elem); }\n");
            },
            .set_proto => {
                try self.write("            { JSValue proto = POP();\n");
                try self.write("              JSValue obj = stack[sp - 1];\n");
                try self.write("              JS_SetPrototype(ctx, obj, proto);\n");
                try self.write("              FROZEN_FREE(ctx, proto); }\n");
            },

            // Constructor opcodes
            .check_ctor => {
                try self.write("            if (JS_IsUndefined(this_val)) {\n");
                try self.write("              next_block = -1; frame->result = JS_ThrowTypeError(ctx, \"Constructor requires 'new'\"); break;\n");
                try self.write("            }\n");
            },
            .check_ctor_return => {
                try self.write("            { JSValue ret = stack[sp - 1];\n");
                try self.write("              if (!JS_IsObject(ret)) {\n");
                try self.write("                PUSH(JS_DupValue(ctx, this_val));\n");
                try self.write("              } }\n");
            },
            .init_ctor => {
                try self.write("            { JSValue proto = JS_GetPropertyStr(ctx, this_val, \"prototype\");\n");
                try self.write("              JSValue this_obj = JS_NewObjectProtoClass(ctx, proto, JS_CLASS_OBJECT);\n");
                try self.write("              FROZEN_FREE(ctx, proto);\n");
                try self.write("              if (JS_IsException(this_obj)) { next_block = -1; frame->result = this_obj; break; }\n");
                try self.write("              PUSH(this_obj); }\n");
            },

            // define_method: Define a method on an object (for class methods)
            .define_method => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        try self.write("            { JSValue func = POP();\n");
                        try self.write("              JSValue obj = stack[sp - 1];\n");
                        try self.write("              JSAtom atom = JS_NewAtom(ctx, \"");
                        try self.writeEscapedString(name);
                        try self.write("\");\n");
                        try self.write("              int flags = JS_PROP_HAS_CONFIGURABLE | JS_PROP_CONFIGURABLE | JS_PROP_HAS_WRITABLE | JS_PROP_WRITABLE | JS_PROP_HAS_VALUE;\n");
                        try self.write("              int ret = JS_DefineProperty(ctx, obj, atom, func, JS_UNDEFINED, JS_UNDEFINED, flags);\n");
                        try self.write("              JS_FreeAtom(ctx, atom);\n");
                        try self.write("              FROZEN_FREE(ctx, func);\n");
                        try self.write("              if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; } }\n");
                    } else {
                        try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
                    }
                } else {
                    try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
                }
            },
            // define_method_computed: Define method with computed name
            .define_method_computed => {
                try self.write("            { JSValue func = POP(); JSValue key = POP();\n");
                try self.write("              JSValue obj = stack[sp - 1];\n");
                try self.write("              JSAtom atom = JS_ValueToAtom(ctx, key);\n");
                try self.write("              FROZEN_FREE(ctx, key);\n");
                try self.write("              if (atom == JS_ATOM_NULL) { FROZEN_FREE(ctx, func); next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
                try self.write("              int flags = JS_PROP_HAS_CONFIGURABLE | JS_PROP_CONFIGURABLE | JS_PROP_HAS_WRITABLE | JS_PROP_WRITABLE | JS_PROP_HAS_VALUE;\n");
                try self.write("              int ret = JS_DefineProperty(ctx, obj, atom, func, JS_UNDEFINED, JS_UNDEFINED, flags);\n");
                try self.write("              JS_FreeAtom(ctx, atom);\n");
                try self.write("              FROZEN_FREE(ctx, func);\n");
                try self.write("              if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; } }\n");
            },

            // Object spread and apply
            .copy_data_properties => {
                try self.write("            { JSValue source = stack[sp - 2];\n");
                try self.write("              JSValue target = stack[sp - 3];\n");
                try self.write("              if (!JS_IsUndefined(source) && !JS_IsNull(source)) {\n");
                try self.write("                JSValue global = JS_GetGlobalObject(ctx);\n");
                try self.write("                JSValue Object = JS_GetPropertyStr(ctx, global, \"Object\");\n");
                try self.write("                JSValue assign = JS_GetPropertyStr(ctx, Object, \"assign\");\n");
                try self.write("                JS_FreeValue(ctx, global);\n");
                try self.write("                JSValue args[2] = { target, source };\n");
                try self.write("                JSValue result = JS_Call(ctx, assign, Object, 2, args);\n");
                try self.write("                JS_FreeValue(ctx, assign); JS_FreeValue(ctx, Object);\n");
                try self.write("                if (JS_IsException(result)) { next_block = -1; frame->result = result; break; }\n");
                try self.write("                JS_FreeValue(ctx, result);\n");
                try self.write("              } }\n");
            },
            .apply => {
                try self.write("            { JSValue args_array = POP(); JSValue this_obj = POP(); JSValue func = POP();\n");
                try self.write("              JSValue len_val = JS_GetPropertyStr(ctx, args_array, \"length\");\n");
                try self.write("              int64_t argc = 0;\n");
                try self.write("              JS_ToInt64(ctx, &argc, len_val);\n");
                try self.write("              JS_FreeValue(ctx, len_val);\n");
                try self.write("              JSValue *argv = NULL;\n");
                try self.write("              if (argc > 0) {\n");
                try self.write("                argv = js_malloc(ctx, argc * sizeof(JSValue));\n");
                try self.write("                for (int i = 0; i < argc; i++) {\n");
                try self.write("                  argv[i] = JS_GetPropertyUint32(ctx, args_array, i);\n");
                try self.write("                }\n");
                try self.write("              }\n");
                try self.write("              FROZEN_FREE(ctx, args_array);\n");
                try self.write("              JSValue result = JS_Call(ctx, func, this_obj, (int)argc, argv);\n");
                try self.write("              FROZEN_FREE(ctx, func); FROZEN_FREE(ctx, this_obj);\n");
                try self.write("              for (int i = 0; i < argc; i++) { JS_FreeValue(ctx, argv[i]); }\n");
                try self.write("              if (argv) js_free(ctx, argv);\n");
                try self.write("              if (JS_IsException(result)) { next_block = -1; frame->result = result; break; }\n");
                try self.write("              PUSH(result); }\n");
            },

            // All other instructions - unsupported
            else => {
                try self.print("            /* Unsupported opcode {d} in trampoline */\n", .{@intFromEnum(instr.opcode)});
                try self.write("            next_block = -1; frame->result = JS_UNDEFINED; break;\n");
                self.pending_self_call = false;
            },
        }
    }

    /// Emit a basic block for trampoline execution (heap frames, no C recursion)
    fn emitTrampolineBlock(self: *SSACodeGen, block: *const BasicBlock, block_idx: usize) !void {
        // Split block into phases at each recursive call site
        var phase: u32 = 0;
        var need_new_phase = false; // Track if we just emitted a recursive call
        try self.print("            switch (frame->instr_offset) {{\n", .{});

        var current_phase_start: usize = 0;
        for (block.instructions, 0..) |instr, instr_idx| {
            // Start new phase at beginning or at recursive call or after recursive call
            const is_call_opcode = instr.opcode == .call0 or instr.opcode == .call1 or instr.opcode == .call2 or instr.opcode == .call3 or instr.opcode == .call or instr.opcode == .tail_call;
            // For tail_call in self-recursive functions, always treat as self-call
            // (the preceding get_var puts function on stack, but may be before another call in nested cases)
            const is_definitely_self_tail_call = instr.opcode == .tail_call and self.options.is_self_recursive;
            const is_recursive_call = (is_call_opcode and self.pending_self_call and self.options.is_self_recursive) or is_definitely_self_tail_call;
            if (instr_idx == 0 or is_recursive_call or need_new_phase) {
                if (instr_idx > 0) {
                    // Close previous phase - add break if no control flow and not after recursive call
                    if (!need_new_phase) {
                        const prev_instr = block.instructions[instr_idx - 1];
                        const had_cf = prev_instr.opcode == .@"return" or prev_instr.opcode == .return_undef or
                                      prev_instr.opcode == .goto or prev_instr.opcode == .goto8 or prev_instr.opcode == .goto16 or
                                      prev_instr.opcode == .if_false or prev_instr.opcode == .if_false8 or
                                      prev_instr.opcode == .if_true or prev_instr.opcode == .if_true8;
                        if (!had_cf) {
                            // Continue to next phase in same block - fall through to next case
                            // No break here, let it fall through
                        }
                    }
                    phase += 1;
                }
                try self.print("            case {d}:\n", .{phase});
                current_phase_start = instr_idx;

                // If this is a resume point after a recursive call, push the child result
                if (need_new_phase) {
                    try self.write("            /* Resume after recursive call - push child result */\n");
                    try self.write("            if (frame->waiting_for_call) {\n");
                    try self.write("                stack[sp++] = FROZEN_DUP(ctx, frames[frame_depth + 1].result);\n");
                    try self.write("                frame->waiting_for_call = 0;\n");
                    try self.write("            }\n");
                }
                need_new_phase = false;
            }

            // Handle recursive call specially - push frame and save continuation
            if (is_recursive_call) {
                const actual_var_count = if (self.options.var_count > 0) self.options.var_count else 1;
                const actual_arg_count = if (self.options.arg_count > 0) self.options.arg_count else 1;
                const call_argc: u16 = switch (instr.opcode) {
                    .call0 => 0,
                    .call1 => 1,
                    .call2 => 2,
                    .call3 => 3,
                    .call, .tail_call => instr.operand.u16, // npop format has argc in operand
                    else => 1,
                };

                try self.print(
                    \\                /* Recursive call {d} ({d} args) - push frame */
                    \\                {{
                    \\
                , .{ instr_idx, call_argc });

                // Pop arguments in reverse order (last arg on top of stack)
                var j: u16 = call_argc;
                while (j > 0) {
                    j -= 1;
                    try self.print("                    JSValue arg{d} = POP();\n", .{j});
                }

                try self.print(
                    \\                    frame->sp = sp;
                    \\                    frame->instr_offset = {d};  /* Resume at next phase */
                    \\                    frame->waiting_for_call = 1;
                    \\
                    \\                    frame_depth++;
                    \\
                , .{phase + 1});

                // Initialize args array
                var k: u16 = 0;
                while (k < call_argc) : (k += 1) {
                    try self.print("                    frames[frame_depth].args[{d}] = arg{d};\n", .{ k, k });
                }
                // Clear remaining args
                while (k < actual_arg_count) : (k += 1) {
                    try self.print("                    frames[frame_depth].args[{d}] = JS_UNDEFINED;\n", .{k});
                }

                try self.print(
                    \\                    frames[frame_depth].sp = 0;
                    \\                    frames[frame_depth].block_id = 0;
                    \\                    frames[frame_depth].instr_offset = 0;
                    \\                    frames[frame_depth].waiting_for_call = 0;
                    \\                    /* Note: result is uninitialized, will be set when frame returns */
                    \\                    for (int i = 0; i < {d}; i++) frames[frame_depth].locals[i] = JS_UNDEFINED;
                    \\
                    \\                    goto trampoline_continue;
                    \\                }}
                    \\
                , .{actual_var_count});

                // For tail_call, emit resume phase that returns child result directly
                if (instr.opcode == .tail_call) {
                    phase += 1;
                    try self.print("            case {d}:\n", .{phase});
                    try self.write("            /* Resume after tail_call - return child result */\n");
                    try self.write("            frame->result = FROZEN_DUP(ctx, frames[frame_depth + 1].result);\n");
                    try self.write("            next_block = -1; break;\n");
                }

                self.pending_self_call = false;
                need_new_phase = true; // Next instruction needs a new phase
                continue;
            }

            // Handle other instructions normally
            try self.emitTrampolineInstruction(instr);
        }

        // Close last phase - add break if no control flow
        if (block.instructions.len > 0 and !need_new_phase) {
            const last_instr = block.instructions[block.instructions.len - 1];
            const had_cf = last_instr.opcode == .@"return" or last_instr.opcode == .return_undef or
                          last_instr.opcode == .goto or last_instr.opcode == .goto8 or last_instr.opcode == .goto16 or
                          last_instr.opcode == .if_false or last_instr.opcode == .if_false8;
            if (!had_cf) {
                try self.write("            break;\n");
            }
        }

        try self.write("            }\n");  // Close switch for phases

        // If block ends without explicit UNCONDITIONAL control flow, continue to next block
        // Note: if_false is conditional, so we still need fallthrough for the true case
        const last_instr = if (block.instructions.len > 0) block.instructions[block.instructions.len - 1].opcode else null;
        const has_unconditional_control_flow = if (last_instr) |op|
            op == .@"return" or op == .return_undef or op == .goto or op == .goto8 or op == .goto16 or op == .tail_call
        else
            false;

        // Emit fallthrough next_block if needed
        if (!has_unconditional_control_flow) {
            try self.write("            ");
            // If last instruction was if_false, use conditional assignment
            const last_was_if_false = if (last_instr) |op| op == .if_false or op == .if_false8 else false;
            if (last_was_if_false) {
                try self.write("if (next_block == -1) ");
            }
            try self.write("next_block = ");
            if (block_idx + 1 < self.cfg.blocks.items.len) {
                try self.print("{d};\n", .{block_idx + 1});
            } else {
                try self.write("-1;\n");
            }
        }

        // Always emit break to prevent fallthrough to next block case
        try self.write("            break;\n");
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
            .push_minus1 => try self.emitPushInt(-1),
            .push_0 => try self.emitPushInt(0),
            .push_1 => try self.emitPushInt(1),
            .push_2 => try self.emitPushInt(2),
            .push_3 => try self.emitPushInt(3),
            .push_4 => try self.emitPushInt(4),
            .push_5 => try self.emitPushInt(5),
            .push_6 => try self.emitPushInt(6),
            .push_7 => try self.emitPushInt(7),
            .push_i8 => {
                if (debug) try self.print("    /* push_i8 {d} */\n", .{instr.operand.i8});
                try self.emitPushInt(instr.operand.i8);
            },
            .push_i16 => {
                if (debug) try self.print("    /* push_i16 {d} */\n", .{instr.operand.i16});
                try self.emitPushInt(instr.operand.i16);
            },
            .push_i32 => {
                if (debug) try self.print("    /* push_i32 {d} */\n", .{instr.operand.i32});
                try self.emitPushInt(instr.operand.i32);
            },
            .push_false => try self.emitPushJSConst("JS_FALSE"),
            .push_true => try self.emitPushJSConst("JS_TRUE"),
            .undefined => try self.emitPushJSConst("JS_UNDEFINED"),
            .null => try self.emitPushJSConst("JS_NULL"),
            .object => {
                if (self.isZig()) {
                    try self.write("    stack[@intCast(sp)] = qjs.JS_NewObject(ctx); sp += 1;\n");
                } else {
                    try self.write("    PUSH(JS_NewObject(ctx));\n");
                }
            },
            .push_this => {
                if (self.isZig()) {
                    try self.write("    stack[@intCast(sp)] = qjs.FROZEN_DUP(ctx, this_val); sp += 1;\n");
                } else {
                    try self.write("    PUSH(FROZEN_DUP(ctx, this_val));\n");
                }
            },
            .push_empty_string => {
                if (self.isZig()) {
                    try self.write("    stack[@intCast(sp)] = qjs.JS_NewString(ctx, \"\"); sp += 1;\n");
                } else {
                    try self.write("    PUSH(JS_NewString(ctx, \"\"));\n");
                }
            },

            // ==================== ARGUMENTS ====================
            .get_arg0 => {
                if (self.isZig()) {
                    try self.write("    stack[@intCast(sp)] = if (argc > 0) qjs.FROZEN_DUP(ctx, argv[0]) else qjs.JS_UNDEFINED; sp += 1;\n");
                } else {
                    try self.write("    PUSH(argc > 0 ? FROZEN_DUP(ctx, argv[0]) : JS_UNDEFINED);\n");
                }
            },
            .get_arg1 => {
                if (self.isZig()) {
                    try self.write("    stack[@intCast(sp)] = if (argc > 1) qjs.FROZEN_DUP(ctx, argv[1]) else qjs.JS_UNDEFINED; sp += 1;\n");
                } else {
                    try self.write("    PUSH(argc > 1 ? FROZEN_DUP(ctx, argv[1]) : JS_UNDEFINED);\n");
                }
            },
            .get_arg2 => {
                if (self.isZig()) {
                    try self.write("    stack[@intCast(sp)] = if (argc > 2) qjs.FROZEN_DUP(ctx, argv[2]) else qjs.JS_UNDEFINED; sp += 1;\n");
                } else {
                    try self.write("    PUSH(argc > 2 ? FROZEN_DUP(ctx, argv[2]) : JS_UNDEFINED);\n");
                }
            },
            .get_arg3 => {
                if (self.isZig()) {
                    try self.write("    stack[@intCast(sp)] = if (argc > 3) qjs.FROZEN_DUP(ctx, argv[3]) else qjs.JS_UNDEFINED; sp += 1;\n");
                } else {
                    try self.write("    PUSH(argc > 3 ? FROZEN_DUP(ctx, argv[3]) : JS_UNDEFINED);\n");
                }
            },
            .get_arg => {
                const idx = instr.operand.u16;
                if (debug) try self.print("    /* get_arg {d} */\n", .{idx});
                if (self.isZig()) {
                    try self.print("    stack[@intCast(sp)] = if (argc > {d}) qjs.FROZEN_DUP(ctx, argv[{d}]) else qjs.JS_UNDEFINED; sp += 1;\n", .{ idx, idx });
                } else {
                    try self.print("    PUSH(argc > {d} ? FROZEN_DUP(ctx, argv[{d}]) : JS_UNDEFINED);\n", .{ idx, idx });
                }
            },
            .put_arg0 => {
                if (self.isZig()) {
                    try self.write("    if (argc > 0) { qjs.JS_FreeValue(ctx, argv[0]); argv[0] = { sp -= 1; const val = stack[@intCast(sp)]; val; }; }\n");
                } else {
                    try self.write("    if (argc > 0) { JS_FreeValue(ctx, argv[0]); argv[0] = POP(); }\n");
                }
            },
            .put_arg1 => {
                if (self.isZig()) {
                    try self.write("    if (argc > 1) { qjs.JS_FreeValue(ctx, argv[1]); argv[1] = { sp -= 1; const val = stack[@intCast(sp)]; val; }; }\n");
                } else {
                    try self.write("    if (argc > 1) { JS_FreeValue(ctx, argv[1]); argv[1] = POP(); }\n");
                }
            },
            .put_arg2 => {
                if (self.isZig()) {
                    try self.write("    if (argc > 2) { qjs.JS_FreeValue(ctx, argv[2]); argv[2] = { sp -= 1; const val = stack[@intCast(sp)]; val; }; }\n");
                } else {
                    try self.write("    if (argc > 2) { JS_FreeValue(ctx, argv[2]); argv[2] = POP(); }\n");
                }
            },
            .put_arg3 => {
                if (self.isZig()) {
                    try self.write("    if (argc > 3) { qjs.JS_FreeValue(ctx, argv[3]); argv[3] = { sp -= 1; const val = stack[@intCast(sp)]; val; }; }\n");
                } else {
                    try self.write("    if (argc > 3) { JS_FreeValue(ctx, argv[3]); argv[3] = POP(); }\n");
                }
            },
            .set_arg0 => {
                if (self.isZig()) {
                    try self.write("    if (argc > 0) { qjs.JS_FreeValue(ctx, argv[0]); argv[0] = qjs.FROZEN_DUP(ctx, stack[@intCast(sp - 1)]); }\n");
                } else {
                    try self.write("    if (argc > 0) { JS_FreeValue(ctx, argv[0]); argv[0] = FROZEN_DUP(ctx, TOP()); }\n");
                }
            },
            .set_arg1 => {
                if (self.isZig()) {
                    try self.write("    if (argc > 1) { qjs.JS_FreeValue(ctx, argv[1]); argv[1] = qjs.FROZEN_DUP(ctx, stack[@intCast(sp - 1)]); }\n");
                } else {
                    try self.write("    if (argc > 1) { JS_FreeValue(ctx, argv[1]); argv[1] = FROZEN_DUP(ctx, TOP()); }\n");
                }
            },
            .set_arg2 => {
                if (self.isZig()) {
                    try self.write("    if (argc > 2) { qjs.JS_FreeValue(ctx, argv[2]); argv[2] = qjs.FROZEN_DUP(ctx, stack[@intCast(sp - 1)]); }\n");
                } else {
                    try self.write("    if (argc > 2) { JS_FreeValue(ctx, argv[2]); argv[2] = FROZEN_DUP(ctx, TOP()); }\n");
                }
            },
            .set_arg3 => {
                if (self.isZig()) {
                    try self.write("    if (argc > 3) { qjs.JS_FreeValue(ctx, argv[3]); argv[3] = qjs.FROZEN_DUP(ctx, stack[@intCast(sp - 1)]); }\n");
                } else {
                    try self.write("    if (argc > 3) { JS_FreeValue(ctx, argv[3]); argv[3] = FROZEN_DUP(ctx, TOP()); }\n");
                }
            },
            .put_arg => {
                const idx = instr.operand.arg;
                if (debug) try self.print("    /* put_arg {d} */\n", .{idx});
                if (self.isZig()) {
                    try self.print("    if (argc > {d}) {{ qjs.JS_FreeValue(ctx, argv[{d}]); argv[{d}] = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }}; }} else {{ const val = {{ sp -= 1; const val_inner = stack[@intCast(sp)]; val_inner; }}; qjs.JS_FreeValue(ctx, val); }}\n", .{ idx, idx, idx });
                } else {
                    try self.print("    if (argc > {d}) {{ JS_FreeValue(ctx, argv[{d}]); argv[{d}] = POP(); }} else {{ FROZEN_FREE(ctx, POP()); }}\n", .{ idx, idx, idx });
                }
            },
            .set_arg => {
                const idx = instr.operand.arg;
                if (debug) try self.print("    /* set_arg {d} */\n", .{idx});
                if (self.isZig()) {
                    try self.print("    if (argc > {d}) {{ qjs.JS_FreeValue(ctx, argv[{d}]); argv[{d}] = qjs.FROZEN_DUP(ctx, stack[@intCast(sp - 1)]); }}\n", .{ idx, idx, idx });
                } else {
                    try self.print("    if (argc > {d}) {{ JS_FreeValue(ctx, argv[{d}]); argv[{d}] = FROZEN_DUP(ctx, TOP()); }}\n", .{ idx, idx, idx });
                }
            },

            // ==================== LOCALS ====================
            .get_loc0 => {
                if (self.isZig()) {
                    try self.write("    stack[@intCast(sp)] = qjs.FROZEN_DUP(ctx, locals[0]); sp += 1;\n");
                } else {
                    try self.write("    PUSH(FROZEN_DUP(ctx, locals[0]));\n");
                }
            },
            .get_loc1 => {
                if (self.isZig()) {
                    try self.write("    stack[@intCast(sp)] = qjs.FROZEN_DUP(ctx, locals[1]); sp += 1;\n");
                } else {
                    try self.write("    PUSH(FROZEN_DUP(ctx, locals[1]));\n");
                }
            },
            .get_loc2 => {
                if (self.isZig()) {
                    try self.write("    stack[@intCast(sp)] = qjs.FROZEN_DUP(ctx, locals[2]); sp += 1;\n");
                } else {
                    try self.write("    PUSH(FROZEN_DUP(ctx, locals[2]));\n");
                }
            },
            .get_loc3 => {
                if (self.isZig()) {
                    try self.write("    stack[@intCast(sp)] = qjs.FROZEN_DUP(ctx, locals[3]); sp += 1;\n");
                } else {
                    try self.write("    PUSH(FROZEN_DUP(ctx, locals[3]));\n");
                }
            },
            .get_loc, .get_loc8 => {
                const idx = if (instr.opcode == .get_loc8) instr.operand.u8 else instr.operand.u16;
                if (debug) try self.print("    /* get_loc {d} */\n", .{idx});
                if (self.isZig()) {
                    try self.print("    stack[@intCast(sp)] = qjs.FROZEN_DUP(ctx, locals[{d}]); sp += 1;\n", .{idx});
                } else {
                    try self.print("    PUSH(FROZEN_DUP(ctx, locals[{d}]));\n", .{idx});
                }
            },
            .put_loc0 => {
                if (self.isZig()) {
                    try self.write("    qjs.JS_FreeValue(ctx, locals[0]); locals[0] = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                } else {
                    try self.write("    FROZEN_FREE(ctx, locals[0]); locals[0] = POP();\n");
                }
            },
            .put_loc1 => {
                if (self.isZig()) {
                    try self.write("    qjs.JS_FreeValue(ctx, locals[1]); locals[1] = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                } else {
                    try self.write("    FROZEN_FREE(ctx, locals[1]); locals[1] = POP();\n");
                }
            },
            .put_loc2 => {
                if (self.isZig()) {
                    try self.write("    qjs.JS_FreeValue(ctx, locals[2]); locals[2] = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                } else {
                    try self.write("    FROZEN_FREE(ctx, locals[2]); locals[2] = POP();\n");
                }
            },
            .put_loc3 => {
                if (self.isZig()) {
                    try self.write("    qjs.JS_FreeValue(ctx, locals[3]); locals[3] = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                } else {
                    try self.write("    FROZEN_FREE(ctx, locals[3]); locals[3] = POP();\n");
                }
            },
            .put_loc, .put_loc8 => {
                const idx = if (instr.opcode == .put_loc8) instr.operand.u8 else instr.operand.u16;
                if (debug) try self.print("    /* put_loc {d} */\n", .{idx});
                if (self.isZig()) {
                    try self.print("    qjs.JS_FreeValue(ctx, locals[{d}]); locals[{d}] = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }};\n", .{ idx, idx });
                } else {
                    try self.print("    FROZEN_FREE(ctx, locals[{d}]); locals[{d}] = POP();\n", .{ idx, idx });
                }
            },
            .set_loc0 => {
                if (self.isZig()) {
                    try self.write("    qjs.JS_FreeValue(ctx, locals[0]); locals[0] = qjs.FROZEN_DUP(ctx, stack[@intCast(sp - 1)]);\n");
                } else {
                    try self.write("    FROZEN_FREE(ctx, locals[0]); locals[0] = FROZEN_DUP(ctx, TOP());\n");
                }
            },
            .set_loc1 => {
                if (self.isZig()) {
                    try self.write("    qjs.JS_FreeValue(ctx, locals[1]); locals[1] = qjs.FROZEN_DUP(ctx, stack[@intCast(sp - 1)]);\n");
                } else {
                    try self.write("    FROZEN_FREE(ctx, locals[1]); locals[1] = FROZEN_DUP(ctx, TOP());\n");
                }
            },
            .set_loc2 => {
                if (self.isZig()) {
                    try self.write("    qjs.JS_FreeValue(ctx, locals[2]); locals[2] = qjs.FROZEN_DUP(ctx, stack[@intCast(sp - 1)]);\n");
                } else {
                    try self.write("    FROZEN_FREE(ctx, locals[2]); locals[2] = FROZEN_DUP(ctx, TOP());\n");
                }
            },
            .set_loc3 => {
                if (self.isZig()) {
                    try self.write("    qjs.JS_FreeValue(ctx, locals[3]); locals[3] = qjs.FROZEN_DUP(ctx, stack[@intCast(sp - 1)]);\n");
                } else {
                    try self.write("    FROZEN_FREE(ctx, locals[3]); locals[3] = FROZEN_DUP(ctx, TOP());\n");
                }
            },
            .set_loc, .set_loc8 => {
                const idx = if (instr.opcode == .set_loc8) instr.operand.u8 else instr.operand.u16;
                if (debug) try self.print("    /* set_loc {d} */\n", .{idx});
                if (self.isZig()) {
                    try self.print("    qjs.JS_FreeValue(ctx, locals[{d}]); locals[{d}] = qjs.FROZEN_DUP(ctx, stack[@intCast(sp - 1)]);\n", .{ idx, idx });
                } else {
                    try self.print("    FROZEN_FREE(ctx, locals[{d}]); locals[{d}] = FROZEN_DUP(ctx, TOP());\n", .{ idx, idx });
                }
            },

            // ==================== STACK OPS ====================
            .drop => {
                if (self.isZig()) {
                    try self.write("    { const val = { sp -= 1; const v = stack[@intCast(sp)]; v; }; qjs.JS_FreeValue(ctx, val); }\n");
                } else {
                    try self.write("    FROZEN_FREE(ctx, POP());\n");
                }
            },
            .dup => {
                if (self.isZig()) {
                    try self.write("    { const tmp = stack[@intCast(sp - 1)]; stack[@intCast(sp)] = qjs.FROZEN_DUP(ctx, tmp); sp += 1; }\n");
                } else {
                    try self.write("    { JSValue tmp = TOP(); PUSH(FROZEN_DUP(ctx, tmp)); }\n");
                }
            },
            .dup1 => {
                if (self.isZig()) {
                    try self.write("    { const a = stack[@intCast(sp - 2)]; stack[@intCast(sp)] = qjs.FROZEN_DUP(ctx, a); sp += 1; }\n");
                } else {
                    try self.write("    { JSValue a = stack[sp-2]; PUSH(FROZEN_DUP(ctx, a)); }\n");
                }
            },
            .dup2 => {
                if (self.isZig()) {
                    try self.write("    { const a = stack[@intCast(sp - 2)]; const b = stack[@intCast(sp - 1)];\n");
                    try self.write("      stack[@intCast(sp)] = qjs.FROZEN_DUP(ctx, a); sp += 1;\n");
                    try self.write("      stack[@intCast(sp)] = qjs.FROZEN_DUP(ctx, b); sp += 1; }\n");
                } else {
                    try self.write("    { JSValue a = stack[sp-2], b = stack[sp-1]; PUSH(FROZEN_DUP(ctx, a)); PUSH(FROZEN_DUP(ctx, b)); }\n");
                }
            },
            .dup3 => {
                if (self.isZig()) {
                    try self.write("    { const a = stack[@intCast(sp - 3)]; const b = stack[@intCast(sp - 2)]; const c = stack[@intCast(sp - 1)];\n");
                    try self.write("      stack[@intCast(sp)] = qjs.FROZEN_DUP(ctx, a); sp += 1;\n");
                    try self.write("      stack[@intCast(sp)] = qjs.FROZEN_DUP(ctx, b); sp += 1;\n");
                    try self.write("      stack[@intCast(sp)] = qjs.FROZEN_DUP(ctx, c); sp += 1; }\n");
                } else {
                    try self.write("    { JSValue a = stack[sp-3], b = stack[sp-2], c = stack[sp-1]; PUSH(FROZEN_DUP(ctx, a)); PUSH(FROZEN_DUP(ctx, b)); PUSH(FROZEN_DUP(ctx, c)); }\n");
                }
            },
            .nip => {
                if (self.isZig()) {
                    try self.write("    { const top = { sp -= 1; const v = stack[@intCast(sp)]; v; };\n");
                    try self.write("      const second = { sp -= 1; const v = stack[@intCast(sp)]; v; };\n");
                    try self.write("      qjs.JS_FreeValue(ctx, second);\n");
                    try self.write("      stack[@intCast(sp)] = top; sp += 1; }\n");
                } else {
                    try self.write("    { JSValue top = POP(); FROZEN_FREE(ctx, POP()); PUSH(top); }\n");
                }
            },
            .nip1 => {
                if (self.isZig()) {
                    try self.write("    { const top = { sp -= 1; const v = stack[@intCast(sp)]; v; };\n");
                    try self.write("      const s1 = { sp -= 1; const v = stack[@intCast(sp)]; v; };\n");
                    try self.write("      const s2 = { sp -= 1; const v = stack[@intCast(sp)]; v; };\n");
                    try self.write("      qjs.JS_FreeValue(ctx, s2);\n");
                    try self.write("      stack[@intCast(sp)] = s1; sp += 1;\n");
                    try self.write("      stack[@intCast(sp)] = top; sp += 1; }\n");
                } else {
                    try self.write("    { JSValue top = POP(); JSValue s1 = POP(); FROZEN_FREE(ctx, POP()); PUSH(s1); PUSH(top); }\n");
                }
            },
            .swap => {
                if (self.isZig()) {
                    try self.write("    { const tmp = stack[@intCast(sp - 1)]; stack[@intCast(sp - 1)] = stack[@intCast(sp - 2)]; stack[@intCast(sp - 2)] = tmp; }\n");
                } else {
                    try self.write("    { JSValue tmp = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = tmp; }\n");
                }
            },
            .swap2 => {
                if (self.isZig()) {
                    try self.write("    { const t1 = stack[@intCast(sp - 1)]; const t2 = stack[@intCast(sp - 2)];\n");
                    try self.write("      stack[@intCast(sp - 1)] = stack[@intCast(sp - 3)]; stack[@intCast(sp - 2)] = stack[@intCast(sp - 4)];\n");
                    try self.write("      stack[@intCast(sp - 3)] = t1; stack[@intCast(sp - 4)] = t2; }\n");
                } else {
                    try self.write("    { JSValue t1 = stack[sp-1], t2 = stack[sp-2]; stack[sp-1] = stack[sp-3]; stack[sp-2] = stack[sp-4]; stack[sp-3] = t1; stack[sp-4] = t2; }\n");
                }
            },
            .rot3l => {
                if (self.isZig()) {
                    try self.write("    { const tmp = stack[@intCast(sp - 3)];\n");
                    try self.write("      stack[@intCast(sp - 3)] = stack[@intCast(sp - 2)];\n");
                    try self.write("      stack[@intCast(sp - 2)] = stack[@intCast(sp - 1)];\n");
                    try self.write("      stack[@intCast(sp - 1)] = tmp; }\n");
                } else {
                    try self.write("    { JSValue tmp = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = tmp; }\n");
                }
            },
            .rot3r => {
                if (self.isZig()) {
                    try self.write("    { const tmp = stack[@intCast(sp - 1)];\n");
                    try self.write("      stack[@intCast(sp - 1)] = stack[@intCast(sp - 2)];\n");
                    try self.write("      stack[@intCast(sp - 2)] = stack[@intCast(sp - 3)];\n");
                    try self.write("      stack[@intCast(sp - 3)] = tmp; }\n");
                } else {
                    try self.write("    { JSValue tmp = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = stack[sp-3]; stack[sp-3] = tmp; }\n");
                }
            },
            .rot4l => {
                if (self.isZig()) {
                    try self.write("    { const tmp = stack[@intCast(sp - 4)];\n");
                    try self.write("      stack[@intCast(sp - 4)] = stack[@intCast(sp - 3)];\n");
                    try self.write("      stack[@intCast(sp - 3)] = stack[@intCast(sp - 2)];\n");
                    try self.write("      stack[@intCast(sp - 2)] = stack[@intCast(sp - 1)];\n");
                    try self.write("      stack[@intCast(sp - 1)] = tmp; }\n");
                } else {
                    try self.write("    { JSValue tmp = stack[sp-4]; stack[sp-4] = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = tmp; }\n");
                }
            },
            .rot5l => {
                if (self.isZig()) {
                    try self.write("    { const tmp = stack[@intCast(sp - 5)];\n");
                    try self.write("      stack[@intCast(sp - 5)] = stack[@intCast(sp - 4)];\n");
                    try self.write("      stack[@intCast(sp - 4)] = stack[@intCast(sp - 3)];\n");
                    try self.write("      stack[@intCast(sp - 3)] = stack[@intCast(sp - 2)];\n");
                    try self.write("      stack[@intCast(sp - 2)] = stack[@intCast(sp - 1)];\n");
                    try self.write("      stack[@intCast(sp - 1)] = tmp; }\n");
                } else {
                    try self.write("    { JSValue tmp = stack[sp-5]; stack[sp-5] = stack[sp-4]; stack[sp-4] = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = tmp; }\n");
                }
            },
            .insert2 => {
                if (self.isZig()) {
                    try self.write("    { const b = stack[@intCast(sp - 1)];\n");
                    try self.write("      stack[@intCast(sp - 1)] = stack[@intCast(sp - 2)];\n");
                    try self.write("      stack[@intCast(sp - 2)] = qjs.FROZEN_DUP(ctx, b);\n");
                    try self.write("      stack[@intCast(sp)] = b; sp += 1; }\n");
                } else {
                    try self.write("    { JSValue b = TOP(); stack[sp-1] = stack[sp-2]; stack[sp-2] = FROZEN_DUP(ctx, b); PUSH(b); }\n");
                }
            },
            .insert3 => {
                if (self.isZig()) {
                    try self.write("    { const c = stack[@intCast(sp - 1)]; const b = stack[@intCast(sp - 2)]; const a = stack[@intCast(sp - 3)];\n");
                    try self.write("      stack[@intCast(sp - 3)] = qjs.FROZEN_DUP(ctx, c);\n");
                    try self.write("      stack[@intCast(sp - 2)] = a;\n");
                    try self.write("      stack[@intCast(sp - 1)] = b;\n");
                    try self.write("      stack[@intCast(sp)] = c; sp += 1; }\n");
                } else {
                    try self.write("    { JSValue c = TOP(); JSValue b = stack[sp-2]; JSValue a = stack[sp-3]; stack[sp-3] = FROZEN_DUP(ctx, c); stack[sp-2] = a; stack[sp-1] = b; PUSH(c); }\n");
                }
            },
            .insert4 => {
                if (self.isZig()) {
                    try self.write("    { const d = stack[@intCast(sp - 1)]; const c = stack[@intCast(sp - 2)]; const b = stack[@intCast(sp - 3)]; const a = stack[@intCast(sp - 4)];\n");
                    try self.write("      stack[@intCast(sp - 4)] = qjs.FROZEN_DUP(ctx, d);\n");
                    try self.write("      stack[@intCast(sp - 3)] = a;\n");
                    try self.write("      stack[@intCast(sp - 2)] = b;\n");
                    try self.write("      stack[@intCast(sp - 1)] = c;\n");
                    try self.write("      stack[@intCast(sp)] = d; sp += 1; }\n");
                } else {
                    try self.write("    { JSValue d = TOP(); JSValue c = stack[sp-2]; JSValue b = stack[sp-3]; JSValue a = stack[sp-4]; stack[sp-4] = FROZEN_DUP(ctx, d); stack[sp-3] = a; stack[sp-2] = b; stack[sp-1] = c; PUSH(d); }\n");
                }
            },
            .perm3 => {
                if (self.isZig()) {
                    try self.write("    { const a = stack[@intCast(sp - 3)];\n");
                    try self.write("      stack[@intCast(sp - 3)] = stack[@intCast(sp - 2)];\n");
                    try self.write("      stack[@intCast(sp - 2)] = stack[@intCast(sp - 1)];\n");
                    try self.write("      stack[@intCast(sp - 1)] = a; }\n");
                } else {
                    try self.write("    { JSValue a = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = a; }\n");
                }
            },
            .perm4 => {
                if (self.isZig()) {
                    try self.write("    { const a = stack[@intCast(sp - 4)];\n");
                    try self.write("      stack[@intCast(sp - 4)] = stack[@intCast(sp - 3)];\n");
                    try self.write("      stack[@intCast(sp - 3)] = stack[@intCast(sp - 2)];\n");
                    try self.write("      stack[@intCast(sp - 2)] = stack[@intCast(sp - 1)];\n");
                    try self.write("      stack[@intCast(sp - 1)] = a; }\n");
                } else {
                    try self.write("    { JSValue a = stack[sp-4]; stack[sp-4] = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = a; }\n");
                }
            },
            .perm5 => {
                if (self.isZig()) {
                    try self.write("    { const a = stack[@intCast(sp - 5)];\n");
                    try self.write("      stack[@intCast(sp - 5)] = stack[@intCast(sp - 4)];\n");
                    try self.write("      stack[@intCast(sp - 4)] = stack[@intCast(sp - 3)];\n");
                    try self.write("      stack[@intCast(sp - 3)] = stack[@intCast(sp - 2)];\n");
                    try self.write("      stack[@intCast(sp - 2)] = stack[@intCast(sp - 1)];\n");
                    try self.write("      stack[@intCast(sp - 1)] = a; }\n");
                } else {
                    try self.write("    { JSValue a = stack[sp-5]; stack[sp-5] = stack[sp-4]; stack[sp-4] = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = a; }\n");
                }
            },
            .nop => {
                if (debug) try self.write("    /* nop - no operation */\n");
            },

            // ==================== ARITHMETIC (comptime generated) ====================
            // Binary arithmetic ops - int32 fast path (Bun-style) eliminates function call overhead
            .add => {
                if (debug) try self.print("    /* add (inlined) */\n", .{});
                try self.emitBinaryArithOp("add", "+", true);
            },
            .sub => {
                if (debug) try self.print("    /* sub (inlined) */\n", .{});
                try self.emitBinaryArithOp("sub", "-", true);
            },
            .mul => {
                if (debug) try self.print("    /* mul (inlined) */\n", .{});
                try self.emitBinaryArithOp("mul", "*", true);
            },
            .div => {
                if (debug) try self.print("    /* div (inlined) */\n", .{});
                try self.emitDivOp();
            },
            .mod => {
                if (debug) try self.print("    /* mod (inlined) */\n", .{});
                try self.emitModOp();
            },
            .neg => {
                if (debug) try self.print("    /* neg (inlined) */\n", .{});
                try self.emitNegOp();
            },
            .plus => {
                // Unary plus - ToNumber, currently a no-op comment
                if (debug) try self.write("    /* plus - unary plus (ToNumber, keep value) */\n");
            },
            .inc => {
                if (debug) try self.print("    /* inc (inlined) */\n", .{});
                try self.emitIncOp();
            },
            .dec => {
                if (debug) try self.print("    /* dec (inlined) */\n", .{});
                try self.emitDecOp();
            },
            .inc_loc => {
                const idx = instr.operand.u8;
                if (debug) try self.print("    /* inc_loc {d} */\n", .{idx});
                if (self.isZig()) {
                    try self.print("    {{ const old = locals[{d}]; locals[{d}] = qjs.frozen_add(ctx, old, qjs.JS_MKVAL(qjs.JS_TAG_INT, 1)); }}\n", .{ idx, idx });
                } else {
                    try self.print("    {{ JSValue old = locals[{d}]; locals[{d}] = frozen_add(ctx, old, JS_MKVAL(JS_TAG_INT, 1)); }}\n", .{ idx, idx });
                }
            },
            .dec_loc => {
                const idx = instr.operand.u8;
                if (debug) try self.print("    /* dec_loc {d} */\n", .{idx});
                if (self.isZig()) {
                    try self.print("    {{ const old = locals[{d}]; locals[{d}] = qjs.frozen_sub(ctx, old, qjs.JS_MKVAL(qjs.JS_TAG_INT, 1)); }}\n", .{ idx, idx });
                } else {
                    try self.print("    {{ JSValue old = locals[{d}]; locals[{d}] = frozen_sub(ctx, old, JS_MKVAL(JS_TAG_INT, 1)); }}\n", .{ idx, idx });
                }
            },
            .add_loc => {
                const idx = instr.operand.u8;
                if (debug) try self.print("    /* add_loc {d} */\n", .{idx});
                if (self.isZig()) {
                    try self.print("    {{ const v = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }}; const old = locals[{d}]; locals[{d}] = qjs.frozen_add(ctx, old, v); }}\n", .{ idx, idx });
                } else {
                    try self.print("    {{ JSValue v = POP(), old = locals[{d}]; locals[{d}] = frozen_add(ctx, old, v); }}\n", .{ idx, idx });
                }
            },

            // ==================== COMPARISON ====================
            .lt => try self.emitBinaryCmpOp("frozen_lt"),
            .lte => try self.emitBinaryCmpOp("frozen_lte"),
            .gt => try self.emitBinaryCmpOp("frozen_gt"),
            .gte => try self.emitBinaryCmpOp("frozen_gte"),
            .eq => try self.emitBinaryCmpOp("frozen_eq"),
            .neq => try self.emitBinaryCmpOp("frozen_neq"),
            .strict_eq => try self.emitBinaryCmpOp("frozen_eq"),
            .strict_neq => try self.emitBinaryCmpOp("frozen_neq"),

            // ==================== BITWISE ====================
            .shl => try self.emitBinaryFuncOp("frozen_shl"),
            .sar => try self.emitBinaryFuncOp("frozen_sar"),
            .shr => try self.emitBinaryFuncOp("frozen_shr"),
            .@"and" => try self.emitBinaryFuncOp("frozen_and"),
            .@"or" => try self.emitBinaryFuncOp("frozen_or"),
            .xor => try self.emitBinaryFuncOp("frozen_xor"),
            .not => try self.emitUnaryFuncOp("frozen_not"),
            .lnot => {
                if (self.isZig()) {
                    try self.write("    { const v = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("      const result = qjs.JS_NewBool(ctx, if (qjs.JS_ToBool(ctx, v) == 0) 1 else 0);\n");
                    try self.write("      qjs.JS_FreeValue(ctx, v);\n");
                    try self.write("      stack[@intCast(sp)] = result; sp += 1;\n");
                    try self.write("    }\n");
                } else {
                    try self.write("    { JSValue v = POP(); PUSH(JS_NewBool(ctx, !JS_ToBool(ctx, v))); FROZEN_FREE(ctx, v); }\n");
                }
            },

            // ==================== TYPE CHECKS ====================
            .is_undefined => try self.emitTypeCheckOp("qjs.JS_IsUndefined(v) != 0"),
            .is_null => try self.emitTypeCheckOp("qjs.JS_IsNull(v) != 0"),
            .is_undefined_or_null => try self.emitTypeCheckOp("qjs.JS_IsUndefined(v) != 0 or qjs.JS_IsNull(v) != 0"),
            .typeof_is_undefined => try self.emitTypeCheckOp("qjs.JS_IsUndefined(v) != 0"),
            .typeof_is_function => try self.emitTypeCheckOp("qjs.JS_IsFunction(ctx, v) != 0"),

            // ==================== POST INC/DEC ====================
            .post_inc => {
                if (self.isZig()) {
                    try self.write("    { const v = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("      stack[@intCast(sp)] = qjs.FROZEN_DUP(ctx, v); sp += 1;\n");
                    try self.write("      stack[@intCast(sp)] = qjs.frozen_add(ctx, v, qjs.JS_MKVAL(qjs.JS_TAG_INT, 1)); sp += 1;\n");
                    try self.write("      qjs.JS_FreeValue(ctx, v);\n");
                    try self.write("    }\n");
                } else {
                    try self.write("    { JSValue v = POP(); PUSH(FROZEN_DUP(ctx, v)); PUSH(frozen_add(ctx, v, JS_MKVAL(JS_TAG_INT, 1))); FROZEN_FREE(ctx, v); }\n");
                }
            },
            .post_dec => {
                if (self.isZig()) {
                    try self.write("    { const v = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("      stack[@intCast(sp)] = qjs.FROZEN_DUP(ctx, v); sp += 1;\n");
                    try self.write("      stack[@intCast(sp)] = qjs.frozen_sub(ctx, v, qjs.JS_MKVAL(qjs.JS_TAG_INT, 1)); sp += 1;\n");
                    try self.write("      qjs.JS_FreeValue(ctx, v);\n");
                    try self.write("    }\n");
                } else {
                    try self.write("    { JSValue v = POP(); PUSH(FROZEN_DUP(ctx, v)); PUSH(frozen_sub(ctx, v, JS_MKVAL(JS_TAG_INT, 1))); FROZEN_FREE(ctx, v); }\n");
                }
            },

            // ==================== TYPE OPERATORS ====================
            .pow => try self.emitBinaryFuncOp("frozen_pow"),
            .typeof => {
                if (self.isZig()) {
                    try self.write("    { const v = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("      const t = qjs.frozen_typeof(ctx, v);\n");
                    try self.write("      qjs.JS_FreeValue(ctx, v);\n");
                    try self.write("      stack[@intCast(sp)] = t; sp += 1; }\n");
                } else {
                    try self.write("    { JSValue v = POP(); JSValue t = frozen_typeof(ctx, v); FROZEN_FREE(ctx, v); PUSH(t); }\n");
                }
            },
            .instanceof => {
                if (self.isZig()) {
                    try self.write("    { const ctor = { sp -= 1; const val = stack[@intCast(sp)]; val; }; const obj = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("      const r = qjs.JS_IsInstanceOf(ctx, obj, ctor);\n");
                    try self.write("      qjs.JS_FreeValue(ctx, obj); qjs.JS_FreeValue(ctx, ctor);\n");
                    try self.write("      if (r < 0) return qjs.JS_EXCEPTION;\n");
                    try self.write("      stack[@intCast(sp)] = qjs.JS_NewBool(ctx, r); sp += 1; }\n");
                } else {
                    try self.write("    { JSValue ctor = POP(), obj = POP(); int r = JS_IsInstanceOf(ctx, obj, ctor); FROZEN_FREE(ctx, obj); FROZEN_FREE(ctx, ctor); if (r < 0) return JS_EXCEPTION; PUSH(JS_NewBool(ctx, r)); }\n");
                }
            },
            .in => {
                if (self.isZig()) {
                    try self.write("    { const obj = { sp -= 1; const val = stack[@intCast(sp)]; val; }; const key = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("      const r = qjs.frozen_in(ctx, key, obj);\n");
                    try self.write("      qjs.JS_FreeValue(ctx, key); qjs.JS_FreeValue(ctx, obj);\n");
                    try self.write("      if (r < 0) return qjs.JS_EXCEPTION;\n");
                    try self.write("      stack[@intCast(sp)] = qjs.JS_NewBool(ctx, r); sp += 1; }\n");
                } else {
                    try self.write("    { JSValue obj = POP(), key = POP(); int r = frozen_in(ctx, key, obj); FROZEN_FREE(ctx, key); FROZEN_FREE(ctx, obj); if (r < 0) return JS_EXCEPTION; PUSH(JS_NewBool(ctx, r)); }\n");
                }
            },

            // ==================== TYPE COERCION ====================
            .to_object => {
                if (self.isZig()) {
                    try self.write("    { const v = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("      const obj = qjs.JS_ToObject(ctx, v);\n");
                    try self.write("      qjs.JS_FreeValue(ctx, v);\n");
                    try self.write("      if (qjs.JS_IsException(obj)) return obj;\n");
                    try self.write("      stack[@intCast(sp)] = obj; sp += 1; }\n");
                } else {
                    try self.write("    { JSValue v = POP(); JSValue obj = JS_ToObject(ctx, v); FROZEN_FREE(ctx, v); if (JS_IsException(obj)) return obj; PUSH(obj); }\n");
                }
            },
            .to_propkey => {
                if (self.isZig()) {
                    try self.write("    { const v = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("      const atom = qjs.JS_ValueToAtom(ctx, v);\n");
                    try self.write("      qjs.JS_FreeValue(ctx, v);\n");
                    try self.write("      if (atom == qjs.JS_ATOM_NULL) return qjs.JS_EXCEPTION;\n");
                    try self.write("      stack[@intCast(sp)] = qjs.JS_AtomToValue(ctx, atom); sp += 1;\n");
                    try self.write("      qjs.JS_FreeAtom(ctx, atom); }\n");
                } else {
                    try self.write("    { JSValue v = POP(); JSAtom atom = JS_ValueToAtom(ctx, v); FROZEN_FREE(ctx, v); if (atom == JS_ATOM_NULL) return JS_EXCEPTION; PUSH(JS_AtomToValue(ctx, atom)); JS_FreeAtom(ctx, atom); }\n");
                }
            },

            // ==================== CONTROL FLOW ====================
            .if_false, .if_false8 => {
                const target = instr.getJumpTarget() orelse 0;
                const target_block = self.cfg.pc_to_block.get(target) orelse 0;
                if (debug) try self.print("    /* if_false -> block_{d} */\n", .{target_block});
                if (self.isZig()) {
                    try self.print("    {{ const cond = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }}; if (qjs.JS_ToBool(ctx, cond) == 0) {{ qjs.JS_FreeValue(ctx, cond); goto block_{d}; }} qjs.JS_FreeValue(ctx, cond); }}\n", .{target_block});
                } else {
                    try self.print("    {{ JSValue cond = POP(); if (!JS_ToBool(ctx, cond)) {{ JS_FreeValue(ctx, cond); goto block_{d}; }} JS_FreeValue(ctx, cond); }}\n", .{target_block});
                }
            },
            .if_true, .if_true8 => {
                const target = instr.getJumpTarget() orelse 0;
                const target_block = self.cfg.pc_to_block.get(target) orelse 0;
                if (debug) try self.print("    /* if_true -> block_{d} */\n", .{target_block});
                if (self.isZig()) {
                    try self.print("    {{ const cond = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }}; if (qjs.JS_ToBool(ctx, cond) != 0) {{ qjs.JS_FreeValue(ctx, cond); goto block_{d}; }} qjs.JS_FreeValue(ctx, cond); }}\n", .{target_block});
                } else {
                    try self.print("    {{ JSValue cond = POP(); if (JS_ToBool(ctx, cond)) {{ JS_FreeValue(ctx, cond); goto block_{d}; }} JS_FreeValue(ctx, cond); }}\n", .{target_block});
                }
            },
            .goto, .goto8, .goto16 => {
                const target = instr.getJumpTarget() orelse 0;
                const target_block = self.cfg.pc_to_block.get(target) orelse 0;
                if (debug) try self.print("    /* goto block_{d} */\n", .{target_block});
                try self.print("    goto block_{d};\n", .{target_block});
            },
            .@"return" => {
                if (self.isZig()) {
                    try self.write("    FROZEN_EXIT_STACK(); return { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                } else {
                    try self.write("    FROZEN_EXIT_STACK(); return POP();\n");
                }
            },
            .return_undef => {
                if (self.isZig()) {
                    try self.write("    FROZEN_EXIT_STACK(); return qjs.JS_UNDEFINED;\n");
                } else {
                    try self.write("    FROZEN_EXIT_STACK(); return JS_UNDEFINED;\n");
                }
            },

            // ==================== CALLS ====================
            .call0 => {
                if (debug) try self.write("    /* call0 */\n");
                self.pending_self_call = false;
                if (self.isZig()) {
                    try self.write("    { const func = { sp -= 1; const val = stack[@intCast(sp)]; val; }; const ret = qjs.JS_Call(ctx, func, qjs.JS_UNDEFINED, 0, null); qjs.JS_FreeValue(ctx, func); if (qjs.JS_IsException(ret)) return ret; stack[@intCast(sp)] = ret; sp += 1; }\n");
                } else {
                    try self.write("    { JSValue func = POP(); JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 0, NULL); JS_FreeValue(ctx, func); if (JS_IsException(ret)) return ret; PUSH(ret); }\n");
                }
            },
            .call1 => {
                if (debug) try self.write("    /* call1 */\n");
                if (self.pending_self_call and self.options.is_self_recursive) {
                    // Direct recursion - call ourselves
                    if (self.isZig()) {
                        try self.print("    {{ var arg0 = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }}; const ret = {s}(ctx, this_val, 1, &arg0); if (qjs.JS_IsException(ret)) return ret; stack[@intCast(sp)] = ret; sp += 1; }}\n", .{self.options.func_name});
                    } else {
                        try self.print("    {{ JSValue arg0 = POP(); JSValue ret = {s}(ctx, this_val, 1, &arg0); if (JS_IsException(ret)) {{ FROZEN_EXIT_STACK(); return ret; }} PUSH(ret); }}\n", .{self.options.func_name});
                    }
                } else {
                    // Standard JS call
                    if (self.isZig()) {
                        try self.write("    { var arg0 = { sp -= 1; const val = stack[@intCast(sp)]; val; }; const func = { sp -= 1; const val = stack[@intCast(sp)]; val; }; const ret = qjs.JS_Call(ctx, func, qjs.JS_UNDEFINED, 1, &arg0); qjs.JS_FreeValue(ctx, func); qjs.JS_FreeValue(ctx, arg0); if (qjs.JS_IsException(ret)) return ret; stack[@intCast(sp)] = ret; sp += 1; }\n");
                    } else {
                        try self.write("    { JSValue arg0 = POP(); JSValue func = POP(); JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 1, &arg0); JS_FreeValue(ctx, func); JS_FreeValue(ctx, arg0); if (JS_IsException(ret)) return ret; PUSH(ret); }\n");
                    }
                }
                self.pending_self_call = false;
            },
            .call2 => {
                if (debug) try self.write("    /* call2 */\n");
                self.pending_self_call = false;
                if (self.isZig()) {
                    try self.write("    { var args: [2]qjs.JSValue = undefined; args[1] = { sp -= 1; stack[@intCast(sp)]; }; args[0] = { sp -= 1; stack[@intCast(sp)]; }; const func = { sp -= 1; stack[@intCast(sp)]; }; const ret = qjs.JS_Call(ctx, func, qjs.JS_UNDEFINED, 2, &args); qjs.JS_FreeValue(ctx, func); qjs.JS_FreeValue(ctx, args[0]); qjs.JS_FreeValue(ctx, args[1]); if (qjs.JS_IsException(ret)) return ret; stack[@intCast(sp)] = ret; sp += 1; }\n");
                } else {
                    try self.write("    { JSValue args[2]; args[1] = POP(); args[0] = POP(); JSValue func = POP(); JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 2, args); JS_FreeValue(ctx, func); JS_FreeValue(ctx, args[0]); JS_FreeValue(ctx, args[1]); if (JS_IsException(ret)) return ret; PUSH(ret); }\n");
                }
            },
            .call3 => {
                if (debug) try self.write("    /* call3 */\n");
                self.pending_self_call = false;
                if (self.isZig()) {
                    try self.write("    { var args: [3]qjs.JSValue = undefined; args[2] = { sp -= 1; stack[@intCast(sp)]; }; args[1] = { sp -= 1; stack[@intCast(sp)]; }; args[0] = { sp -= 1; stack[@intCast(sp)]; }; const func = { sp -= 1; stack[@intCast(sp)]; }; const ret = qjs.JS_Call(ctx, func, qjs.JS_UNDEFINED, 3, &args); qjs.JS_FreeValue(ctx, func); qjs.JS_FreeValue(ctx, args[0]); qjs.JS_FreeValue(ctx, args[1]); qjs.JS_FreeValue(ctx, args[2]); if (qjs.JS_IsException(ret)) return ret; stack[@intCast(sp)] = ret; sp += 1; }\n");
                } else {
                    try self.write("    { JSValue args[3]; args[2] = POP(); args[1] = POP(); args[0] = POP(); JSValue func = POP(); JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 3, args); JS_FreeValue(ctx, func); JS_FreeValue(ctx, args[0]); JS_FreeValue(ctx, args[1]); JS_FreeValue(ctx, args[2]); if (JS_IsException(ret)) return ret; PUSH(ret); }\n");
                }
            },
            .call => {
                const argc = instr.operand.u16;
                if (debug) try self.print("    /* call argc={d} */\n", .{argc});
                self.pending_self_call = false;
                try self.write("    {\n");
                // Pop args into temp array (reverse order)
                if (argc > 0) {
                    if (self.isZig()) {
                        try self.print("      var args: [{d}]qjs.JSValue = undefined;\n", .{argc});
                        var i = argc;
                        while (i > 0) {
                            i -= 1;
                            try self.print("      args[{d}] = {{ sp -= 1; stack[@intCast(sp)]; }};\n", .{i});
                        }
                    } else {
                        try self.print("      JSValue args[{d}];\n", .{argc});
                        var i = argc;
                        while (i > 0) {
                            i -= 1;
                            try self.print("      args[{d}] = POP();\n", .{i});
                        }
                    }
                }
                if (self.isZig()) {
                    try self.write("      const func = { sp -= 1; stack[@intCast(sp)]; };\n");
                } else {
                    try self.write("      JSValue func = POP();\n");
                }
                if (argc > 0) {
                    if (self.isZig()) {
                        try self.print("      const ret = qjs.JS_Call(ctx, func, qjs.JS_UNDEFINED, {d}, &args);\n", .{argc});
                    } else {
                        try self.print("      JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, {d}, args);\n", .{argc});
                    }
                } else {
                    if (self.isZig()) {
                        try self.write("      const ret = qjs.JS_Call(ctx, func, qjs.JS_UNDEFINED, 0, null);\n");
                    } else {
                        try self.write("      JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 0, NULL);\n");
                    }
                }
                if (self.isZig()) {
                    try self.write("      qjs.JS_FreeValue(ctx, func);\n");
                } else {
                    try self.write("      JS_FreeValue(ctx, func);\n");
                }
                if (argc > 0) {
                    var j: u16 = 0;
                    while (j < argc) : (j += 1) {
                        if (self.isZig()) {
                            try self.print("      qjs.JS_FreeValue(ctx, args[{d}]);\n", .{j});
                        } else {
                            try self.print("      JS_FreeValue(ctx, args[{d}]);\n", .{j});
                        }
                    }
                }
                if (self.isZig()) {
                    try self.write("      if (qjs.JS_IsException(ret)) return ret;\n");
                    try self.write("      stack[@intCast(sp)] = ret; sp += 1;\n");
                } else {
                    try self.write("      if (JS_IsException(ret)) return ret;\n");
                    try self.write("      PUSH(ret);\n");
                }
                try self.write("    }\n");
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
                    if (self.isZig()) {
                        try self.write("    stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1; // TODO: closure var_ref\n");
                    } else {
                        try self.write("    PUSH(JS_UNDEFINED); /* TODO: closure var_ref */\n");
                    }
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
                if (self.isZig()) {
                    try self.write("    stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1; // TODO: closure var_ref\n");
                } else {
                    try self.write("    PUSH(JS_UNDEFINED); /* TODO: closure var_ref */\n");
                }
                self.pending_self_call = false;
            },

            // ==================== TAIL CALL (TCO) ====================
            .tail_call => {
                if (debug) try self.write("    /* tail_call - TCO */\n");
                if (self.pending_self_call and self.options.is_self_recursive) {
                    // Self-recursive tail call: convert to goto (true TCO!)
                    if (self.isZig()) {
                        try self.write("    { frozen_arg0 = { sp -= 1; stack[@intCast(sp)]; }; sp = 0; goto frozen_start; }\n");
                    } else {
                        try self.write("    { frozen_arg0 = POP(); sp = 0; goto frozen_start; }\n");
                    }
                } else {
                    // Non-self-recursive
                    if (self.isZig()) {
                        try self.write("    { var arg = { sp -= 1; stack[@intCast(sp)]; }; const func = { sp -= 1; stack[@intCast(sp)]; }; return qjs.JS_Call(ctx, func, qjs.JS_UNDEFINED, 1, &arg); }\n");
                    } else {
                        try self.write("    { JSValue arg = POP(); JSValue func = POP(); FROZEN_EXIT_STACK(); return JS_Call(ctx, func, JS_UNDEFINED, 1, &arg); }\n");
                    }
                }
                self.pending_self_call = false;
            },
            .tail_call_method => {
                if (debug) try self.write("    /* tail_call_method - TCO */\n");
                if (self.isZig()) {
                    try self.write("    { var arg = { sp -= 1; stack[@intCast(sp)]; }; const this = { sp -= 1; stack[@intCast(sp)]; }; const func = { sp -= 1; stack[@intCast(sp)]; }; return qjs.JS_Call(ctx, func, this, 1, &arg); }\n");
                } else {
                    try self.write("    { JSValue arg = POP(); JSValue this = POP(); JSValue func = POP(); FROZEN_EXIT_STACK(); return JS_Call(ctx, func, this, 1, &arg); }\n");
                }
                self.pending_self_call = false;
            },

            // ==================== PROPERTY ACCESS (use string-based APIs to avoid atom index mismatch) ====================
            .get_field => {
                const atom_idx = instr.operand.atom;
                if (debug) try self.print("    /* get_field atom:{d} */\n", .{atom_idx});
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        if (self.isZig()) {
                            try self.print("    {{ const obj = {{ sp -= 1; stack[@intCast(sp)]; }}; const val = qjs.JS_GetPropertyStr(ctx, obj, \"{s}\"); qjs.JS_FreeValue(ctx, obj); if (qjs.JS_IsException(val)) return val; stack[@intCast(sp)] = val; sp += 1; }}\n", .{name});
                        } else {
                            try self.print("    {{ JSValue obj = POP(); JSValue val = JS_GetPropertyStr(ctx, obj, \"{s}\"); FROZEN_FREE(ctx, obj); if (JS_IsException(val)) return val; PUSH(val); }}\n", .{name});
                        }
                    } else {
                        if (self.isZig()) {
                            try self.write("    { const obj = { sp -= 1; stack[@intCast(sp)]; }; qjs.JS_FreeValue(ctx, obj); stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1; }\n");
                        } else {
                            try self.write("    { JSValue obj = POP(); FROZEN_FREE(ctx, obj); PUSH(JS_UNDEFINED); }\n");
                        }
                    }
                } else {
                    if (self.isZig()) {
                        try self.write("    { const obj = { sp -= 1; stack[@intCast(sp)]; }; qjs.JS_FreeValue(ctx, obj); stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1; }\n");
                    } else {
                        try self.write("    { JSValue obj = POP(); FROZEN_FREE(ctx, obj); PUSH(JS_UNDEFINED); }\n");
                    }
                }
            },
            .get_field2 => {
                const atom_idx = instr.operand.atom;
                if (debug) try self.print("    /* get_field2 atom:{d} */\n", .{atom_idx});
                // Push both obj and obj.field (for method calls: obj.method() needs both)
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        if (self.isZig()) {
                            try self.print("    {{ const obj = stack[@intCast(sp - 1)]; const val = qjs.JS_GetPropertyStr(ctx, obj, \"{s}\"); if (qjs.JS_IsException(val)) return val; stack[@intCast(sp)] = val; sp += 1; }}\n", .{name});
                        } else {
                            try self.print("    {{ JSValue obj = TOP(); JSValue val = JS_GetPropertyStr(ctx, obj, \"{s}\"); if (JS_IsException(val)) return val; PUSH(val); }}\n", .{name});
                        }
                    } else {
                        if (self.isZig()) {
                            try self.write("    { stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1; }\n");
                        } else {
                            try self.write("    { PUSH(JS_UNDEFINED); }\n");
                        }
                    }
                } else {
                    if (self.isZig()) {
                        try self.write("    { stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1; }\n");
                    } else {
                        try self.write("    { PUSH(JS_UNDEFINED); }\n");
                    }
                }
            },
            .put_field => {
                const atom_idx = instr.operand.atom;
                if (debug) try self.print("    /* put_field atom:{d} */\n", .{atom_idx});
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        if (self.isZig()) {
                            try self.print("    {{ const val = {{ sp -= 1; stack[@intCast(sp)]; }}; const obj = {{ sp -= 1; stack[@intCast(sp)]; }}; const r: c_int = qjs.JS_SetPropertyStr(ctx, obj, \"{s}\", val); qjs.JS_FreeValue(ctx, obj); if (r < 0) return qjs.JS_EXCEPTION; }}\n", .{name});
                        } else {
                            try self.print("    {{ JSValue val = POP(); JSValue obj = POP(); int r = JS_SetPropertyStr(ctx, obj, \"{s}\", val); FROZEN_FREE(ctx, obj); if (r < 0) return JS_EXCEPTION; }}\n", .{name});
                        }
                    } else {
                        if (self.isZig()) {
                            try self.write("    { qjs.JS_FreeValue(ctx, { sp -= 1; stack[@intCast(sp)]; }); qjs.JS_FreeValue(ctx, { sp -= 1; stack[@intCast(sp)]; }); }\n");
                        } else {
                            try self.write("    { FROZEN_FREE(ctx, POP()); FROZEN_FREE(ctx, POP()); }\n");
                        }
                    }
                } else {
                    if (self.isZig()) {
                        try self.write("    { qjs.JS_FreeValue(ctx, { sp -= 1; stack[@intCast(sp)]; }); qjs.JS_FreeValue(ctx, { sp -= 1; stack[@intCast(sp)]; }); }\n");
                    } else {
                        try self.write("    { FROZEN_FREE(ctx, POP()); FROZEN_FREE(ctx, POP()); }\n");
                    }
                }
            },

            // ==================== ITERATORS (via QuickJS wrapper functions) ====================
            .for_in_start => {
                if (debug) try self.write("    /* for_in_start */\n");
                // Stack: obj -> enum_obj
                if (self.isZig()) {
                    try self.write("    if (qjs.js_frozen_for_in_start(ctx, &stack[@intCast(sp - 1)]) != 0) return qjs.JS_EXCEPTION;\n");
                } else {
                    try self.write("    if (js_frozen_for_in_start(ctx, &stack[sp - 1])) return JS_EXCEPTION;\n");
                }
            },
            .for_in_next => {
                if (debug) try self.write("    /* for_in_next */\n");
                // Stack: enum_obj -> enum_obj value done (pushes 2 more)
                if (self.isZig()) {
                    try self.write("    if (qjs.js_frozen_for_in_next(ctx, &stack[@intCast(sp - 1)]) != 0) return qjs.JS_EXCEPTION;\n");
                    try self.write("    sp += 2;\n");
                } else {
                    try self.write("    if (js_frozen_for_in_next(ctx, &stack[sp - 1])) return JS_EXCEPTION;\n");
                    try self.write("    sp += 2;\n");
                }
            },
            .for_of_start => {
                if (debug) try self.write("    /* for_of_start */\n");
                // Stack: obj -> enum_obj next_method (pushes 2 more)
                if (self.isZig()) {
                    try self.write("    if (qjs.js_frozen_for_of_start(ctx, &stack[@intCast(sp - 1)], 0) != 0) return qjs.JS_EXCEPTION;\n");
                    try self.write("    sp += 2;\n");
                } else {
                    try self.write("    if (js_frozen_for_of_start(ctx, &stack[sp - 1], 0)) return JS_EXCEPTION;\n");
                    try self.write("    sp += 2;\n");
                }
            },
            .for_of_next => {
                const offset = instr.operand.u8;
                if (debug) try self.print("    /* for_of_next offset:{d} */\n", .{offset});
                // Stack: enum_rec [objs] -> enum_rec [objs] value done (pushes 2 more)
                // offset is the number of objects between enum_rec and top of stack
                if (self.isZig()) {
                    try self.print("    if (qjs.js_frozen_for_of_next(ctx, &stack[@intCast(sp)], -{d}) != 0) return qjs.JS_EXCEPTION;\n", .{offset});
                    try self.write("    sp += 2;\n");
                } else {
                    try self.print("    if (js_frozen_for_of_next(ctx, &stack[sp], -{d})) return JS_EXCEPTION;\n", .{offset});
                    try self.write("    sp += 2;\n");
                }
            },

            // ==================== ADVANCED ITERATOR OPERATIONS ====================
            // iterator_check_object: Verify top of stack is an object
            .iterator_check_object => {
                if (debug) try self.write("    /* iterator_check_object */\n");
                try self.write("    if (!JS_IsObject(stack[sp - 1])) {\n");
                try self.write("      next_block = -1; frame->result = JS_ThrowTypeError(ctx, \"iterator must return an object\"); break;\n");
                try self.write("    }\n");
            },
            // iterator_get_value_done: Extract value and done from iterator result
            // Stack: result_obj -> value, done
            .iterator_get_value_done => {
                if (debug) try self.write("    /* iterator_get_value_done */\n");
                try self.write("    { JSValue obj = stack[sp - 1];\n");
                try self.write("      JSValue value = JS_GetPropertyStr(ctx, obj, \"value\");\n");
                try self.write("      JSValue done_val = JS_GetPropertyStr(ctx, obj, \"done\");\n");
                try self.write("      int done = JS_ToBool(ctx, done_val);\n");
                try self.write("      JS_FreeValue(ctx, done_val);\n");
                try self.write("      FROZEN_FREE(ctx, obj);\n");
                try self.write("      stack[sp - 1] = value;\n");
                try self.write("      stack[sp++] = JS_NewBool(ctx, done); }\n");
            },
            // iterator_close: Close an iterator
            // Stack: iter_obj, next_method, catch_offset -> (empty, pops 3)
            .iterator_close => {
                if (debug) try self.write("    /* iterator_close */\n");
                try self.write("    { sp--; /* drop catch_offset */\n");
                try self.write("      FROZEN_FREE(ctx, stack[--sp]); /* drop next method */\n");
                try self.write("      JSValue iter = stack[--sp];\n");
                try self.write("      if (!JS_IsUndefined(iter)) {\n");
                try self.write("        JSValue ret_method = JS_GetPropertyStr(ctx, iter, \"return\");\n");
                try self.write("        if (!JS_IsUndefined(ret_method) && !JS_IsNull(ret_method)) {\n");
                try self.write("          JSValue ret = JS_Call(ctx, ret_method, iter, 0, NULL);\n");
                try self.write("          JS_FreeValue(ctx, ret_method);\n");
                try self.write("          if (JS_IsException(ret)) { FROZEN_FREE(ctx, iter); FROZEN_EXIT_STACK(); return ret; }\n");
                try self.write("          JS_FreeValue(ctx, ret);\n");
                try self.write("        } else { JS_FreeValue(ctx, ret_method); }\n");
                try self.write("        FROZEN_FREE(ctx, iter);\n");
                try self.write("      } }\n");
            },
            // iterator_next: Call next method on iterator
            // Stack: iter_obj, next, catch_offset, val -> iter_obj, next, catch_offset, result
            .iterator_next => {
                if (debug) try self.write("    /* iterator_next */\n");
                try self.write("    { JSValue val = stack[sp - 1];\n");
                try self.write("      JSValue next = stack[sp - 3];\n");
                try self.write("      JSValue iter = stack[sp - 4];\n");
                try self.write("      JSValue ret = JS_Call(ctx, next, iter, 1, &val);\n");
                try self.write("      FROZEN_FREE(ctx, val);\n");
                try self.write("      if (JS_IsException(ret)) { next_block = -1; frame->result = ret; break; }\n");
                try self.write("      stack[sp - 1] = ret; }\n");
            },
            // iterator_call: Call throw/return method on iterator
            // Stack: iter_obj, next, catch_offset, val -> iter_obj, next, catch_offset, result, done
            .iterator_call => {
                const flags = instr.operand.u8;
                if (debug) try self.print("    /* iterator_call flags:{d} */\n", .{flags});
                // flags & 1: 0=return, 1=throw
                // flags & 2: 0=with arg, 2=no arg
                const method_name = if (flags & 1 != 0) "throw" else "return";
                try self.write("    { JSValue val = stack[sp - 1];\n");
                try self.write("      JSValue iter = stack[sp - 4];\n");
                try self.print("      JSValue method = JS_GetPropertyStr(ctx, iter, \"{s}\");\n", .{method_name});
                try self.write("      if (JS_IsUndefined(method) || JS_IsNull(method)) {\n");
                try self.write("        /* No method - return done=true */\n");
                try self.write("        JS_FreeValue(ctx, method);\n");
                try self.write("        stack[sp - 1] = val;\n");
                try self.write("        stack[sp++] = JS_TRUE;\n");
                try self.write("      } else {\n");
                if (flags & 2 != 0) {
                    // No argument
                    try self.write("        JSValue ret = JS_Call(ctx, method, iter, 0, NULL);\n");
                    try self.write("        FROZEN_FREE(ctx, val);\n");
                } else {
                    // With argument
                    try self.write("        JSValue ret = JS_Call(ctx, method, iter, 1, &val);\n");
                    try self.write("        FROZEN_FREE(ctx, val);\n");
                }
                try self.write("        JS_FreeValue(ctx, method);\n");
                try self.write("        if (JS_IsException(ret)) { next_block = -1; frame->result = ret; break; }\n");
                try self.write("        stack[sp - 1] = ret;\n");
                try self.write("        stack[sp++] = JS_FALSE;\n");
                try self.write("      } }\n");
            },

            // Rest parameters: function foo(a, b, ...rest)
            .rest => {
                const first_idx = instr.operand.u16;
                if (debug) try self.print("    /* rest first_idx:{d} */\n", .{first_idx});
                // Create array from argv[first_idx..argc]
                if (self.isZig()) {
                    try self.print("    {{ const first: c_int = {d};\n", .{first_idx});
                    try self.write("      const arr = qjs.JS_NewArray(ctx);\n");
                    try self.write("      if (qjs.JS_IsException(arr) == 0) {\n");
                    try self.write("        var i: c_int = first;\n");
                    try self.write("        while (i < argc) : (i += 1)\n");
                    try self.write("          _ = qjs.JS_SetPropertyUint32(ctx, arr, @intCast(i - first), qjs.JS_DupValue(ctx, argv[@intCast(i)]));\n");
                    try self.write("      }\n");
                    try self.write("      stack[@intCast(sp)] = arr; sp += 1; }\n");
                } else {
                    try self.print("    {{ int first = {d};\n", .{first_idx});
                    try self.write("      JSValue arr = JS_NewArray(ctx);\n");
                    try self.write("      if (!JS_IsException(arr)) {\n");
                    try self.write("        for (int i = first; i < argc; i++)\n");
                    try self.write("          JS_SetPropertyUint32(ctx, arr, i - first, JS_DupValue(ctx, argv[i]));\n");
                    try self.write("      }\n");
                    try self.write("      stack[sp++] = arr; }\n");
                }
            },

            // Special objects: ALL 8 types supported (except truly impossible mapped_arguments aliasing)
            .special_object => {
                const obj_type = instr.operand.u8;
                if (debug) try self.print("    /* special_object type:{d} */\n", .{obj_type});
                if (self.isZig()) {
                    switch (obj_type) {
                        0 => { // arguments - proper arguments object with exotic behavior
                            try self.write("    stack[@intCast(sp)] = qjs.JS_NewArguments(ctx, argc, argv); sp += 1;\n");
                        },
                        1 => { // mapped_arguments - sloppy mode with callee (aliasing not supported)
                            try self.print("    stack[@intCast(sp)] = qjs.JS_NewMappedArgumentsSimple(ctx, argc, argv, _{s}_this_func); sp += 1;\n", .{self.options.func_name});
                        },
                        2 => { // this_func - current function object (stored in static var)
                            try self.print("    stack[@intCast(sp)] = qjs.JS_DupValue(ctx, _{s}_this_func); sp += 1;\n", .{self.options.func_name});
                        },
                        3 => { // new_target - this_val IS new_target with constructor_or_func
                            try self.write("    stack[@intCast(sp)] = qjs.JS_DupValue(ctx, this_val); sp += 1;\n");
                        },
                        4 => { // home_object - for super calls (from global registry)
                            try self.print("    stack[@intCast(sp)] = qjs.JS_GetFrozenHomeObject(ctx, \"{s}\"); sp += 1;\n", .{self.options.func_name});
                        },
                        5, 7 => { // var_object (5), null_proto (7) - object with null prototype
                            try self.write("    stack[@intCast(sp)] = qjs.JS_NewObjectProto(ctx, qjs.JS_NULL); sp += 1;\n");
                        },
                        6 => { // import_meta - ES module metadata
                            try self.write("    stack[@intCast(sp)] = qjs.JS_GetImportMetaCurrent(ctx); sp += 1;\n");
                        },
                        else => {
                            try self.print("    // UNKNOWN special_object type {d}\n", .{obj_type});
                            try self.unsupported_opcodes.append(self.allocator, "special_object");
                        },
                    }
                } else {
                    switch (obj_type) {
                        0 => { // arguments - proper arguments object with exotic behavior
                            try self.write("    stack[sp++] = JS_NewArguments(ctx, argc, argv);\n");
                        },
                        1 => { // mapped_arguments - sloppy mode with callee (aliasing not supported)
                            try self.print("    stack[sp++] = JS_NewMappedArgumentsSimple(ctx, argc, argv, _{s}_this_func);\n", .{self.options.func_name});
                        },
                        2 => { // this_func - current function object (stored in static var)
                            try self.print("    stack[sp++] = JS_DupValue(ctx, _{s}_this_func);\n", .{self.options.func_name});
                        },
                        3 => { // new_target - this_val IS new_target with constructor_or_func
                            try self.write("    stack[sp++] = JS_DupValue(ctx, this_val);\n");
                        },
                        4 => { // home_object - for super calls (from global registry)
                            try self.print("    stack[sp++] = JS_GetFrozenHomeObject(ctx, \"{s}\");\n", .{self.options.func_name});
                        },
                        5, 7 => { // var_object (5), null_proto (7) - object with null prototype
                            try self.write("    stack[sp++] = JS_NewObjectProto(ctx, JS_NULL);\n");
                        },
                        6 => { // import_meta - ES module metadata
                            try self.write("    stack[sp++] = JS_GetImportMetaCurrent(ctx);\n");
                        },
                        else => {
                            try self.print("    /* UNKNOWN special_object type {d} */\n", .{obj_type});
                            try self.unsupported_opcodes.append(self.allocator, "special_object");
                        },
                    }
                }
            },

            // ==================== ADDITIONAL OPERATORS ====================
            // get_super: Get prototype of object (for super.method() calls)
            // Stack: obj -> prototype
            .get_super => {
                if (debug) try self.write("    /* get_super */\n");
                if (self.isZig()) {
                    try self.write("    { const obj = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("      const proto = qjs.JS_GetPrototype(ctx, obj);\n");
                    try self.write("      qjs.JS_FreeValue(ctx, obj);\n");
                    try self.write("      if (qjs.JS_IsException(proto) != 0) return proto;\n");
                    try self.write("      stack[@intCast(sp)] = proto; sp += 1; }\n");
                } else {
                    try self.write("    { JSValue obj = POP();\n");
                    try self.write("      JSValue proto = JS_GetPrototype(ctx, obj);\n");
                    try self.write("      FROZEN_FREE(ctx, obj);\n");
                    try self.write("      if (JS_IsException(proto)) { next_block = -1; frame->result = proto; break; }\n");
                    try self.write("      PUSH(proto); }\n");
                }
            },
            // regexp: Create RegExp from pattern and flags strings
            // Stack: pattern, flags -> regexp
            .regexp => {
                if (debug) try self.write("    /* regexp */\n");
                // Use RegExp constructor: new RegExp(pattern, flags)
                if (self.isZig()) {
                    try self.write("    { const flags = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("      const pattern = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("      const global = qjs.JS_GetGlobalObject(ctx);\n");
                    try self.write("      const RegExp = qjs.JS_GetPropertyStr(ctx, global, \"RegExp\");\n");
                    try self.write("      qjs.JS_FreeValue(ctx, global);\n");
                    try self.write("      var args = [2]qjs.JSValue{ pattern, flags };\n");
                    try self.write("      const rx = qjs.JS_CallConstructor(ctx, RegExp, 2, &args);\n");
                    try self.write("      qjs.JS_FreeValue(ctx, RegExp);\n");
                    try self.write("      qjs.JS_FreeValue(ctx, pattern); qjs.JS_FreeValue(ctx, flags);\n");
                    try self.write("      if (qjs.JS_IsException(rx) != 0) return rx;\n");
                    try self.write("      stack[@intCast(sp)] = rx; sp += 1; }\n");
                } else {
                    try self.write("    { JSValue flags = POP(); JSValue pattern = POP();\n");
                    try self.write("      JSValue global = JS_GetGlobalObject(ctx);\n");
                    try self.write("      JSValue RegExp = JS_GetPropertyStr(ctx, global, \"RegExp\");\n");
                    try self.write("      JS_FreeValue(ctx, global);\n");
                    try self.write("      JSValue args[2] = { pattern, flags };\n");
                    try self.write("      JSValue rx = JS_CallConstructor(ctx, RegExp, 2, args);\n");
                    try self.write("      JS_FreeValue(ctx, RegExp);\n");
                    try self.write("      FROZEN_FREE(ctx, pattern); FROZEN_FREE(ctx, flags);\n");
                    try self.write("      if (JS_IsException(rx)) { next_block = -1; frame->result = rx; break; }\n");
                    try self.write("      PUSH(rx); }\n");
                }
            },
            // check_ctor: Verify we're called as constructor (new.target exists)
            .check_ctor => {
                if (debug) try self.write("    /* check_ctor - verify called with new */\n");
                // With JS_CFUNC_constructor_or_func, this_val is new.target when called with new
                // If not called with new, this_val is undefined
                if (self.isZig()) {
                    try self.write("    if (qjs.JS_IsUndefined(this_val) != 0) {\n");
                    try self.write("      return qjs.JS_ThrowTypeError(ctx, \"Constructor requires 'new'\");\n");
                    try self.write("    }\n");
                } else {
                    try self.write("    if (JS_IsUndefined(this_val)) {\n");
                    try self.write("      next_block = -1; frame->result = JS_ThrowTypeError(ctx, \"Constructor requires 'new'\"); break;\n");
                    try self.write("    }\n");
                }
            },
            // check_ctor_return: Check constructor return value
            // Stack: ret_val -> ret_val, this (or just ret_val if ret_val is object)
            .check_ctor_return => {
                if (debug) try self.write("    /* check_ctor_return */\n");
                // If return value is an object, use it; otherwise use this
                if (self.isZig()) {
                    try self.write("    { const ret = stack[@intCast(sp - 1)];\n");
                    try self.write("      if (qjs.JS_IsObject(ret) == 0 and qjs.JS_IsException(ret) == 0) {\n");
                    try self.write("        // Return 'this' for non-object returns\n");
                    try self.write("        stack[@intCast(sp)] = qjs.JS_DupValue(ctx, this_val); sp += 1;\n");
                    try self.write("      } else {\n");
                    try self.write("        stack[@intCast(sp)] = qjs.JS_DupValue(ctx, ret); sp += 1;\n");
                    try self.write("      } }\n");
                } else {
                    try self.write("    { JSValue ret = TOP();\n");
                    try self.write("      if (!JS_IsObject(ret) && !JS_IsException(ret)) {\n");
                    try self.write("        /* Return 'this' for non-object returns */\n");
                    try self.write("        PUSH(JS_DupValue(ctx, this_val));\n");
                    try self.write("      } else {\n");
                    try self.write("        PUSH(JS_DupValue(ctx, ret));\n");
                    try self.write("      } }\n");
                }
            },
            // init_ctor: Initialize constructor - create this object
            // Stack: -> this
            .init_ctor => {
                if (debug) try self.write("    /* init_ctor - create this */\n");
                // Create new object with new.target's prototype
                if (self.isZig()) {
                    try self.write("    { const proto = qjs.JS_GetPropertyStr(ctx, this_val, \"prototype\");\n");
                    try self.write("      const this_obj = qjs.JS_NewObjectProtoClass(ctx, proto, qjs.JS_CLASS_OBJECT);\n");
                    try self.write("      qjs.JS_FreeValue(ctx, proto);\n");
                    try self.write("      if (qjs.JS_IsException(this_obj) != 0) return this_obj;\n");
                    try self.write("      stack[@intCast(sp)] = this_obj; sp += 1; }\n");
                } else {
                    try self.write("    { JSValue proto = JS_GetPropertyStr(ctx, this_val, \"prototype\");\n");
                    try self.write("      JSValue this_obj = JS_NewObjectProtoClass(ctx, proto, JS_CLASS_OBJECT);\n");
                    try self.write("      FROZEN_FREE(ctx, proto);\n");
                    try self.write("      if (JS_IsException(this_obj)) { next_block = -1; frame->result = this_obj; break; }\n");
                    try self.write("      PUSH(this_obj); }\n");
                }
            },
            // copy_data_properties: Object spread {...obj}
            // Stack: target, source, excludeList -> target, source, excludeList
            .copy_data_properties => {
                const mask = instr.operand.u8;
                if (debug) try self.print("    /* copy_data_properties mask:{d} */\n", .{mask});
                // mask=0: target is at sp-3, source at sp-2, exclude at sp-1
                // We use Object.assign(target, source) - ignores excludeList for simplicity
                try self.write("    { JSValue source = stack[sp - 2];\n");
                try self.write("      JSValue target = stack[sp - 3];\n");
                try self.write("      if (!JS_IsUndefined(source) && !JS_IsNull(source)) {\n");
                try self.write("        JSValue global = JS_GetGlobalObject(ctx);\n");
                try self.write("        JSValue Object = JS_GetPropertyStr(ctx, global, \"Object\");\n");
                try self.write("        JSValue assign = JS_GetPropertyStr(ctx, Object, \"assign\");\n");
                try self.write("        JS_FreeValue(ctx, global);\n");
                try self.write("        JSValue args[2] = { target, source };\n");
                try self.write("        JSValue result = JS_Call(ctx, assign, Object, 2, args);\n");
                try self.write("        JS_FreeValue(ctx, assign); JS_FreeValue(ctx, Object);\n");
                try self.write("        if (JS_IsException(result)) { FROZEN_EXIT_STACK(); return result; }\n");
                try self.write("        JS_FreeValue(ctx, result);\n");
                try self.write("      } }\n");
            },
            // apply: func.apply(this, argsArray)
            // Stack: func, this, argsArray -> result
            .apply => {
                const magic = instr.operand.u16;
                if (debug) try self.print("    /* apply magic:{d} */\n", .{magic});
                try self.write("    { JSValue args_array = POP(); JSValue this_obj = POP(); JSValue func = POP();\n");
                try self.write("      JSValue len_val = JS_GetPropertyStr(ctx, args_array, \"length\");\n");
                try self.write("      int64_t argc = 0;\n");
                try self.write("      JS_ToInt64(ctx, &argc, len_val);\n");
                try self.write("      JS_FreeValue(ctx, len_val);\n");
                try self.write("      JSValue *argv = NULL;\n");
                try self.write("      if (argc > 0) {\n");
                try self.write("        argv = js_malloc(ctx, argc * sizeof(JSValue));\n");
                try self.write("        for (int i = 0; i < argc; i++) {\n");
                try self.write("          argv[i] = JS_GetPropertyUint32(ctx, args_array, i);\n");
                try self.write("        }\n");
                try self.write("      }\n");
                try self.write("      FROZEN_FREE(ctx, args_array);\n");
                try self.write("      JSValue result = JS_Call(ctx, func, this_obj, (int)argc, argv);\n");
                try self.write("      FROZEN_FREE(ctx, func); FROZEN_FREE(ctx, this_obj);\n");
                try self.write("      for (int i = 0; i < argc; i++) { JS_FreeValue(ctx, argv[i]); }\n");
                try self.write("      if (argv) js_free(ctx, argv);\n");
                try self.write("      if (JS_IsException(result)) { FROZEN_EXIT_STACK(); return result; }\n");
                try self.write("      PUSH(result); }\n");
            },

            // ==================== CLASS/FUNCTION DEFINITION ====================
            // define_func: Define a global function
            // Stack: func -> (pops 1)
            .define_func => {
                const atom_idx = instr.operand.atom;
                if (debug) try self.print("    /* define_func atom:{d} */\n", .{atom_idx});
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        if (self.isZig()) {
                            try self.write("    { const func = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                            try self.write("      const global = qjs.JS_GetGlobalObject(ctx);\n");
                            try self.write("      _ = qjs.JS_SetPropertyStr(ctx, global, \"");
                            try self.writeEscapedString(name);
                            try self.write("\", func);\n");
                            try self.write("      qjs.JS_FreeValue(ctx, global); }\n");
                        } else {
                            try self.write("    { JSValue func = POP();\n");
                            try self.write("      JSValue global = JS_GetGlobalObject(ctx);\n");
                            try self.write("      JS_SetPropertyStr(ctx, global, \"");
                            try self.writeEscapedString(name);
                            try self.write("\", func);\n");
                            try self.write("      JS_FreeValue(ctx, global); }\n");
                        }
                    } else {
                        if (self.isZig()) {
                            try self.write("    { const val = { sp -= 1; const val = stack[@intCast(sp)]; val; }; qjs.JS_FreeValue(ctx, val); }\n");
                        } else {
                            try self.write("    { FROZEN_FREE(ctx, POP()); }\n");
                        }
                    }
                } else {
                    if (self.isZig()) {
                        try self.write("    { const val = { sp -= 1; const val = stack[@intCast(sp)]; val; }; qjs.JS_FreeValue(ctx, val); }\n");
                    } else {
                        try self.write("    { FROZEN_FREE(ctx, POP()); }\n");
                    }
                }
            },
            // define_method: Define a method on an object (for class methods)
            // Stack: obj, func -> obj (pops func, keeps obj)
            .define_method => {
                const atom_idx = instr.operand.atom;
                // The flags byte follows the atom
                if (debug) try self.print("    /* define_method atom:{d} */\n", .{atom_idx});
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        try self.write("    { JSValue func = POP();\n");
                        try self.write("      JSValue obj = stack[sp - 1];\n");
                        try self.write("      JSAtom atom = JS_NewAtom(ctx, \"");
                        try self.writeEscapedString(name);
                        try self.write("\");\n");
                        try self.write("      int flags = JS_PROP_HAS_CONFIGURABLE | JS_PROP_CONFIGURABLE | JS_PROP_HAS_WRITABLE | JS_PROP_WRITABLE | JS_PROP_HAS_VALUE;\n");
                        try self.write("      int ret = JS_DefineProperty(ctx, obj, atom, func, JS_UNDEFINED, JS_UNDEFINED, flags);\n");
                        try self.write("      JS_FreeAtom(ctx, atom);\n");
                        try self.write("      FROZEN_FREE(ctx, func);\n");
                        try self.write("      if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; } }\n");
                    } else {
                        try self.write("    { FROZEN_FREE(ctx, POP()); }\n");
                    }
                } else {
                    try self.write("    { FROZEN_FREE(ctx, POP()); }\n");
                }
            },
            // define_method_computed: Define method with computed name
            // Stack: obj, prop_key, func -> obj (pops prop_key and func)
            .define_method_computed => {
                if (debug) try self.write("    /* define_method_computed */\n");
                try self.write("    { JSValue func = POP(); JSValue key = POP();\n");
                try self.write("      JSValue obj = stack[sp - 1];\n");
                try self.write("      JSAtom atom = JS_ValueToAtom(ctx, key);\n");
                try self.write("      FROZEN_FREE(ctx, key);\n");
                try self.write("      if (atom == JS_ATOM_NULL) { FROZEN_FREE(ctx, func); next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
                try self.write("      int flags = JS_PROP_HAS_CONFIGURABLE | JS_PROP_CONFIGURABLE | JS_PROP_HAS_WRITABLE | JS_PROP_WRITABLE | JS_PROP_HAS_VALUE;\n");
                try self.write("      int ret = JS_DefineProperty(ctx, obj, atom, func, JS_UNDEFINED, JS_UNDEFINED, flags);\n");
                try self.write("      JS_FreeAtom(ctx, atom);\n");
                try self.write("      FROZEN_FREE(ctx, func);\n");
                try self.write("      if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; } }\n");
            },

            // ==================== REFERENCE OPERATIONS ====================
            // make_var_ref: Create reference to global variable
            // Stack: -> obj, prop_name
            .make_var_ref => {
                const atom_idx = instr.operand.atom;
                if (debug) try self.print("    /* make_var_ref atom:{d} */\n", .{atom_idx});
                // Push globalThis and the property name
                try self.write("    { JSValue global = JS_GetGlobalObject(ctx);\n");
                try self.write("      PUSH(global);\n");
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        try self.write("      PUSH(JS_NewString(ctx, \"");
                        try self.writeEscapedString(name);
                        try self.write("\")); }\n");
                    } else {
                        try self.write("      PUSH(JS_UNDEFINED); }\n");
                    }
                } else {
                    try self.write("      PUSH(JS_UNDEFINED); }\n");
                }
            },
            // get_ref_value: Get property value from reference
            // Stack: obj, prop -> obj, prop, value
            .get_ref_value => {
                if (debug) try self.write("    /* get_ref_value */\n");
                try self.write("    { JSValue prop = stack[sp - 1];\n");
                try self.write("      JSValue obj = stack[sp - 2];\n");
                try self.write("      if (JS_IsUndefined(obj)) {\n");
                try self.write("        const char *name = JS_ToCString(ctx, prop);\n");
                try self.write("        JS_ThrowReferenceError(ctx, \"%s is not defined\", name ? name : \"?\");\n");
                try self.write("        if (name) JS_FreeCString(ctx, name);\n");
                try self.write("        next_block = -1; frame->result = JS_EXCEPTION; break;\n");
                try self.write("      }\n");
                try self.write("      JSValue val = JS_GetPropertyValue(ctx, obj, JS_DupValue(ctx, prop));\n");
                try self.write("      if (JS_IsException(val)) { next_block = -1; frame->result = val; break; }\n");
                try self.write("      PUSH(val); }\n");
            },
            // put_ref_value: Set property value on reference
            // Stack: obj, prop, value -> (empty, pops 3)
            .put_ref_value => {
                if (debug) try self.write("    /* put_ref_value */\n");
                try self.write("    { JSValue val = POP(); JSValue prop = POP(); JSValue obj = POP();\n");
                try self.write("      if (JS_IsUndefined(obj)) {\n");
                try self.write("        /* In non-strict mode, assign to global */\n");
                try self.write("        obj = JS_GetGlobalObject(ctx);\n");
                try self.write("      }\n");
                try self.write("      int ret = JS_SetPropertyValue(ctx, obj, prop, val, JS_PROP_THROW_STRICT);\n");
                try self.write("      FROZEN_FREE(ctx, obj);\n");
                try self.write("      if (ret < 0) { FROZEN_EXIT_STACK(); return JS_EXCEPTION; } }\n");
            },

            // ==================== PRIVATE FIELDS ====================
            // private_symbol: Create a private symbol from atom
            // Stack: -> private_symbol
            .private_symbol => {
                const atom_idx = instr.operand.atom;
                if (debug) try self.print("    /* private_symbol atom:{d} */\n", .{atom_idx});
                // Must create runtime atom from string, bytecode atom indices don't match runtime
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        if (self.isZig()) {
                            try self.print("    {{ const atom = qjs.JS_NewAtomLen(ctx, \"{s}\", {d});\n", .{ name, name.len });
                            try self.write("      const sym = qjs.JS_NewSymbolFromAtom(ctx, atom, qjs.JS_ATOM_TYPE_PRIVATE);\n");
                            try self.write("      qjs.JS_FreeAtom(ctx, atom);\n");
                            try self.write("      stack[@intCast(sp)] = sym; sp += 1; }\n");
                        } else {
                            try self.print("    {{ JSAtom atom = JS_NewAtomLen(ctx, \"{s}\", {d}); PUSH(JS_NewSymbolFromAtom(ctx, atom, JS_ATOM_TYPE_PRIVATE)); JS_FreeAtom(ctx, atom); }}\n", .{ name, name.len });
                        }
                    } else {
                        if (self.isZig()) {
                            try self.write("    stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1;\n");
                        } else {
                            try self.write("    PUSH(JS_UNDEFINED);\n");
                        }
                    }
                } else {
                    if (self.isZig()) {
                        try self.write("    stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1;\n");
                    } else {
                        try self.write("    PUSH(JS_UNDEFINED);\n");
                    }
                }
            },
            // check_brand: Verify object has brand (for private field access)
            // Stack: obj, func -> obj, func (throws if no brand)
            .check_brand => {
                if (debug) try self.write("    /* check_brand */\n");
                if (self.isZig()) {
                    try self.write("    { const ret = qjs.JS_FrozenCheckBrand(ctx, stack[@intCast(sp - 2)], stack[@intCast(sp - 1)]);\n");
                    try self.write("      if (ret < 0) return qjs.JS_EXCEPTION; }\n");
                } else {
                    try self.write("    { int ret = JS_FrozenCheckBrand(ctx, stack[sp - 2], stack[sp - 1]);\n");
                    try self.write("      if (ret < 0) { FROZEN_EXIT_STACK(); return JS_EXCEPTION; } }\n");
                }
            },
            // add_brand: Add brand to object (for private field initialization)
            // Stack: obj, func -> (empty)
            .add_brand => {
                if (debug) try self.write("    /* add_brand */\n");
                if (self.isZig()) {
                    try self.write("    { const func = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("      const obj = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("      const ret = qjs.JS_FrozenAddBrand(ctx, obj, func);\n");
                    try self.write("      qjs.JS_FreeValue(ctx, obj); qjs.JS_FreeValue(ctx, func);\n");
                    try self.write("      if (ret < 0) return qjs.JS_EXCEPTION; }\n");
                } else {
                    try self.write("    { JSValue func = POP(); JSValue obj = POP();\n");
                    try self.write("      int ret = JS_FrozenAddBrand(ctx, obj, func);\n");
                    try self.write("      FROZEN_FREE(ctx, obj); FROZEN_FREE(ctx, func);\n");
                    try self.write("      if (ret < 0) { FROZEN_EXIT_STACK(); return JS_EXCEPTION; } }\n");
                }
            },
            // private_in: Check if private field exists (for #field in obj)
            // Stack: field_sym, obj -> bool
            .private_in => {
                if (debug) try self.write("    /* private_in */\n");
                try self.write("    { int ret = js_frozen_private_in(ctx, &stack[sp - 2]);\n");
                try self.write("      if (ret < 0) { FROZEN_EXIT_STACK(); return JS_EXCEPTION; }\n");
                try self.write("      sp--; }\n");
            },

            // Private field access
            .get_private_field => {
                if (debug) try self.write("    /* get_private_field */\n");
                try self.write("    { JSValue name = POP(); JSValue obj = POP();\n");
                try self.write("      JSValue val = JS_FrozenGetPrivateField(ctx, obj, name);\n");
                try self.write("      FROZEN_FREE(ctx, obj); FROZEN_FREE(ctx, name);\n");
                try self.write("      if (JS_IsException(val)) { FROZEN_EXIT_STACK(); return val; }\n");
                try self.write("      PUSH(val); }\n");
            },
            .put_private_field => {
                if (debug) try self.write("    /* put_private_field */\n");
                try self.write("    { JSValue val = POP(); JSValue name = POP(); JSValue obj = POP();\n");
                try self.write("      int ret = JS_FrozenSetPrivateField(ctx, obj, name, val);\n");
                try self.write("      FROZEN_FREE(ctx, obj); FROZEN_FREE(ctx, name);\n");
                try self.write("      if (ret < 0) { FROZEN_EXIT_STACK(); return JS_EXCEPTION; } }\n");
            },
            .define_private_field => {
                if (debug) try self.write("    /* define_private_field */\n");
                try self.write("    { JSValue val = POP(); JSValue name = POP(); JSValue obj = POP();\n");
                try self.write("      int ret = JS_FrozenDefinePrivateField(ctx, obj, name, val);\n");
                try self.write("      FROZEN_FREE(ctx, name);\n");
                try self.write("      if (ret < 0) { FROZEN_FREE(ctx, obj); FROZEN_EXIT_STACK(); return JS_EXCEPTION; }\n");
                try self.write("      PUSH(val); }\n");
            },

            else => {
                const info = instr.getInfo();
                // Track unsupported opcode - function will be skipped
                try self.unsupported_opcodes.append(self.allocator, info.name);
                try self.print("    /* UNSUPPORTED: {s} */\n", .{info.name});
            },
        }
    }

    /// Emit a basic block with native int32 operations (18x faster for pure int math)
    /// Uses operand stack for bytecode-driven codegen
    fn emitInt32Block(self: *SSACodeGen, block: *const BasicBlock, block_idx: usize, next_temp: *u32) !void {
        try self.print("block_{d}:;\n", .{block_idx});

        // Operand stack for tracking int32 temporaries (simulates QuickJS stack)
        var stack = std.ArrayListUnmanaged([]const u8){};
        defer {
            for (stack.items) |item| self.allocator.free(item);
            stack.deinit(self.allocator);
        }

        for (block.instructions) |instr| {
            try self.emitInt32Instruction(instr, &stack, next_temp);
        }
    }

    /// Emit a single instruction using native int32 (no JSValue stack)
    /// Uses operand stack to track int32 temporaries
    /// Now uses handler patterns for reduced duplication
    fn emitInt32Instruction(self: *SSACodeGen, instr: Instruction, stack: *std.ArrayListUnmanaged([]const u8), next_temp: *u32) !void {
        const fname = self.options.func_name;
        const handler = int32_handlers.getInt32Handler(instr.opcode);

        switch (handler.pattern) {
            .push_const_i32 => {
                // Push constant value
                const value_str = if (handler.value) |v|
                    try std.fmt.allocPrint(self.allocator, "{d}", .{v})
                else switch (instr.opcode) {
                    .push_i8 => try std.fmt.allocPrint(self.allocator, "{d}", .{instr.operand.i8}),
                    .push_i16 => try std.fmt.allocPrint(self.allocator, "{d}", .{instr.operand.i16}),
                    .push_i32 => try std.fmt.allocPrint(self.allocator, "{d}", .{instr.operand.i32}),
                    else => unreachable,
                };
                try stack.append(self.allocator, value_str);
            },

            .get_arg_i32 => {
                // Push argument name (n0, n1, n2, n3)
                const arg_name = try std.fmt.allocPrint(self.allocator, "n{d}", .{handler.index.?});
                try stack.append(self.allocator, arg_name);
            },

            .binary_arith_i32, .bitwise_binary_i32, .binary_cmp_i32 => {
                // Binary operations: pop 2, compute, push result
                if (stack.items.len < 2) return error.StackUnderflow;
                const b = stack.pop() orelse return error.StackUnderflow;
                defer self.allocator.free(b);
                const a = stack.pop() orelse return error.StackUnderflow;
                defer self.allocator.free(a);
                const result = try std.fmt.allocPrint(self.allocator, "temp{d}", .{next_temp.*});
                next_temp.* += 1;

                // For mod operator, escape % as %% in format string
                const op = handler.op.?;
                if (std.mem.eql(u8, op, "%")) {
                    try self.print("    int32_t {s} = ({s} %% {s});\n", .{ result, a, b });
                } else {
                    try self.print("    int32_t {s} = ({s} {s} {s});\n", .{ result, a, op, b });
                }
                try stack.append(self.allocator, result);
            },

            .unary_i32 => {
                // Unary operations: pop 1, compute, push result
                if (stack.items.len < 1) return error.StackUnderflow;
                const a = stack.pop() orelse return error.StackUnderflow;
                defer self.allocator.free(a);
                const result = try std.fmt.allocPrint(self.allocator, "temp{d}", .{next_temp.*});
                next_temp.* += 1;
                try self.print("    int32_t {s} = {s}{s};\n", .{ result, handler.op.?, a });
                try stack.append(self.allocator, result);
            },

            .inc_dec_i32 => {
                // Increment/decrement: pop 1, +/- 1, push result
                if (stack.items.len < 1) return error.StackUnderflow;
                const a = stack.pop() orelse return error.StackUnderflow;
                defer self.allocator.free(a);
                const result = try std.fmt.allocPrint(self.allocator, "temp{d}", .{next_temp.*});
                next_temp.* += 1;
                const op = if (handler.is_inc.?) "+" else "-";
                try self.print("    int32_t {s} = {s} {s} 1;\n", .{ result, a, op });
                try stack.append(self.allocator, result);
            },

            .stack_op_i32 => {
                // Stack operations (dup/drop)
                if (std.mem.eql(u8, handler.op.?, "dup")) {
                    if (stack.items.len < 1) return error.StackUnderflow;
                    const top = stack.items[stack.items.len - 1];
                    const dup_val = try std.fmt.allocPrint(self.allocator, "{s}", .{top});
                    try stack.append(self.allocator, dup_val);
                } else {
                    // drop
                    if (stack.items.len < 1) return error.StackUnderflow;
                    const val = stack.pop() orelse return error.StackUnderflow;
                    self.allocator.free(val);
                }
            },

            .self_ref_i32 => {
                // Mark that next call should be treated as self-recursive
                self.pending_self_call = true;
            },

            .call_self_i32 => {
                if (self.pending_self_call and self.options.is_self_recursive) {
                    // Self-recursive call with int32 argument
                    if (stack.items.len < 1) return error.StackUnderflow;
                    const arg = stack.pop() orelse return error.StackUnderflow;
                    defer self.allocator.free(arg);
                    const result = try std.fmt.allocPrint(self.allocator, "temp{d}", .{next_temp.*});
                    next_temp.* += 1;
                    try self.print("    int32_t {s} = {s}_int32({s});\n", .{ result, fname, arg });
                    try stack.append(self.allocator, result);
                    self.pending_self_call = false;
                } else {
                    // Non-self-recursive call not supported in int32 mode
                    const info = instr.getInfo();
                    try self.unsupported_opcodes.append(self.allocator, info.name);
                    try self.print("    /* INT32 UNSUPPORTED: non-self call1 */\n", .{});
                }
            },

            .return_i32 => {
                if (handler.value) |v| {
                    // return_undef
                    try self.print("    return {d}; /* undefined */\n", .{v});
                } else {
                    // return <value>
                    if (stack.items.len < 1) return error.StackUnderflow;
                    const value = stack.pop() orelse return error.StackUnderflow;
                    defer self.allocator.free(value);
                    try self.print("    return {s};\n", .{value});
                }
            },

            .if_false_i32 => {
                // Conditional jump: if (!condition) goto block_N
                if (stack.items.len < 1) return error.StackUnderflow;
                const condition = stack.pop() orelse return error.StackUnderflow;
                defer self.allocator.free(condition);
                const target = instr.getJumpTarget() orelse 0;
                const target_block = self.cfg.pc_to_block.get(target) orelse 0;
                try self.print("    if (!{s}) goto block_{d};\n", .{ condition, target_block });
            },

            .goto_i32 => {
                // Unconditional jump: goto block_N
                const target = instr.getJumpTarget() orelse 0;
                const target_block = self.cfg.pc_to_block.get(target) orelse 0;
                try self.print("    goto block_{d};\n", .{target_block});
            },

            .unsupported => {
                // FAIL FAST AND LOUD: Unsupported opcode in int32 mode
                const info = instr.getInfo();

                // Skip eval-related opcodes (intentionally not supported)
                if (instr.opcode == .eval or instr.opcode == .apply_eval) {
                    try self.unsupported_opcodes.append(self.allocator, info.name);
                    try self.print("    /* INT32 UNSUPPORTED: {s} (eval not allowed) */\n", .{info.name});
                    return;
                }

                // For all other opcodes, fail loudly so we know what to implement
                std.debug.print("\n" ++
                    "========================================\n" ++
                    "INT32 CODEGEN ERROR: Unsupported opcode!\n" ++
                    "========================================\n" ++
                    "Opcode: {s} ({})\n" ++
                    "Function: {s}\n" ++
                    "This opcode needs to be implemented in int32_handlers.zig\n" ++
                    "Location: src/freeze/int32_handlers.zig\n" ++
                    "========================================\n\n", .{ info.name, instr.opcode, self.options.func_name });

                try self.unsupported_opcodes.append(self.allocator, info.name);
                return error.UnsupportedOpcodes;
            },
        }
    }

    /// Generate specialized int32 code for the fib-like pattern
    /// Detected pattern: if (n <= 1) return n; return f(n-1) + f(n-2);
    /// Uses pure int32 internal helper for zero-allocation recursion
    pub fn emitInt32FibPattern(self: *SSACodeGen) !void {
        const fname = self.options.func_name;
        try self.print(
            \\/* Pure int32 internal helper - no JSValue boxing in hot path */
            \\static int32_t {s}_int32(int32_t n0) {{
            \\    if (n0 < 2) return n0;
            \\    return {s}_int32(n0 - 1) + {s}_int32(n0 - 2);
            \\}}
            \\
            \\    /* Entry point: unbox arg, call pure int32, box result */
            \\    return JS_NewInt32(ctx, {s}_int32(n0));
            \\
        , .{ fname, fname, fname, fname });
    }

    fn emitInit(self: *SSACodeGen) !void {
        const fname = self.options.func_name;
        // Use js_name for registration if provided, otherwise use func_name
        const js_name = if (self.options.js_name.len > 0) self.options.js_name else fname;
        try self.print(
            \\int {s}_init(JSContext *ctx)
            \\{{
            \\    JSValue global = JS_GetGlobalObject(ctx);
            \\    /* Use constructor_or_func so this_val = new.target when called with new */
            \\    JSValue func = JS_NewCFunction2(ctx, {s}, "{s}", {d}, JS_CFUNC_constructor_or_func, 0);
            \\    _{s}_this_func = JS_DupValue(ctx, func); /* Store for special_object type 2 */
            \\    JS_SetPropertyStr(ctx, global, "{s}", func);
            \\    JS_FreeValue(ctx, global);
            \\    return 0;
            \\}}
            \\
        , .{ fname, fname, js_name, self.options.arg_count, fname, js_name });
    }
};
