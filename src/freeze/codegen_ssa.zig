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
const c_builder = @import("c_builder.zig");

const Opcode = opcodes.Opcode;
const Instruction = parser.Instruction;
const CFG = cfg_mod.CFG;
const BasicBlock = cfg_mod.BasicBlock;
const Allocator = std.mem.Allocator;
const ConstValue = module_parser.ConstValue;
const CBuilder = c_builder.CBuilder;
const CValue = c_builder.CValue;
const CodeGenContext = c_builder.CodeGenContext;

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
    use_builder_api: bool = false, // Use structured CBuilder API instead of raw string concatenation (Phase 2+)
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
    // Optional: structured code builder (Phase 2+)
    builder: ?*CBuilder = null,

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

    // ===== Builder API helpers (Phase 2+) =====

    fn getBuilder(self: *SSACodeGen) !*CBuilder {
        return self.builder orelse return error.FormatError;
    }

    fn getCodeGenContext(self: *const SSACodeGen, is_trampoline: bool) CodeGenContext {
        return CodeGenContext.init(is_trampoline, self.options.output_language);
    }

    // Helper to emit exception handling code - different for trampoline vs SSA mode
    // Uses builder API when available, falls back to raw strings otherwise
    fn emitExceptionCheck(self: *SSACodeGen, value_name: []const u8, is_trampoline: bool) !void {
        if (self.builder) |builder| {
            // Update builder context for current mode
            builder.context = self.getCodeGenContext(is_trampoline);
            const val = CValue.init(self.allocator, value_name);
            try builder.emitExceptionCheck(val);
            // Copy builder output to self.output and reset
            try self.output.appendSlice(self.allocator, builder.getOutput());
            builder.reset();
        } else {
            // Legacy raw string output
            if (is_trampoline) {
                try self.print("                  if (JS_IsException({s})) {{ next_block = -1; frame->result = {s}; break; }}\n", .{ value_name, value_name });
            } else {
                try self.print("                  if (JS_IsException({s})) return {s};\n", .{ value_name, value_name });
            }
        }
    }

    // Helper to emit error code check (for functions that return <0 on error)
    // Uses builder API when available, falls back to raw strings otherwise
    fn emitErrorCheck(self: *SSACodeGen, check_expr: []const u8, is_trampoline: bool) !void {
        if (self.builder) |builder| {
            builder.context = self.getCodeGenContext(is_trampoline);
            const cond = CValue.init(self.allocator, check_expr);
            try builder.emitErrorCheck(cond);
            try self.output.appendSlice(self.allocator, builder.getOutput());
            builder.reset();
        } else {
            // Legacy raw string output
            if (is_trampoline) {
                try self.print("              if ({s}) {{ next_block = -1; frame->result = JS_EXCEPTION; break; }}\n", .{check_expr});
            } else {
                try self.print("              if ({s}) return JS_EXCEPTION;\n", .{check_expr});
            }
        }
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
        // Initialize builder if using builder API
        var owned_builder: ?CBuilder = null;
        if (self.options.use_builder_api) {
            const ctx = self.getCodeGenContext(false); // Will be updated per-block
            owned_builder = CBuilder.init(self.allocator, ctx);
            self.builder = &owned_builder.?;
        }
        defer if (owned_builder) |*b| {
            b.deinit();
            self.builder = null;
        };

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
            std.debug.print("[freeze] Unsupported opcodes in '{s}': ", .{self.options.func_name});
            for (self.unsupported_opcodes.items, 0..) |opcode, i| {
                if (i > 0) std.debug.print(", ", .{});
                std.debug.print("{s}", .{opcode});
            }
            std.debug.print("\n", .{});
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

            // Alias variables for compatibility with trampoline code
            if (!self.isZig()) {
                try self.write("    int argc_inner = argc;\n");
                try self.write("    (void)argc_inner;\n");
                try self.write("    const int is_trampoline = 0;\n");
                try self.write("    int next_block = -1; /* unused in non-trampoline */\n");
                // Define stub frame with just the result field for error handling
                try self.write("    struct { JSValue result; } _frame_stub, *frame = &_frame_stub;\n");
                try self.write("    frame->result = JS_UNDEFINED;\n");
                try self.write("    (void)is_trampoline; (void)next_block; (void)frame;\n");
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

    /// Emit opcodes that are shared between emitTrampolineInstruction and emitInstruction
    /// Returns true if the opcode was handled, false if caller should handle it
    /// is_trampoline: true if called from trampoline mode (uses frame->), false for SSA mode (uses return)
    fn emitCommonOpcode(self: *SSACodeGen, instr: Instruction, is_trampoline: bool) !bool {
        const debug = self.options.debug_comments;

        switch (instr.opcode) {
            .add => {
                try self.write(
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
                \\
                );
                try self.emitExceptionCheck("r", is_trampoline);
                try self.write(
                \\                  PUSH(r);
                \\              }
                \\            }
                \\
                );
                return true;
            },
            .add_brand => {
                try self.write("            { JSValue func = POP(); JSValue obj = POP();\n");
                try self.write("              int ret = JS_FrozenAddBrand(ctx, obj, func);\n");
                try self.write("              FROZEN_FREE(ctx, obj); FROZEN_FREE(ctx, func);\n");
                try self.emitErrorCheck("ret < 0", is_trampoline);
                try self.write(" }\n");
                return true;
            },
            .add_loc => {
                const idx = instr.operand.u8;
                try self.print("            {{ JSValue v = POP(), old = frame->locals[{d}]; frame->locals[{d}] = frozen_add(ctx, old, v); }}\n", .{ idx, idx });
                return true;
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
                try self.emitExceptionCheck("result", is_trampoline);
                try self.write("              PUSH(result); }\n");
                return true;
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
                try self.emitExceptionCheck("ret", is_trampoline);
                try self.write("              PUSH(ret);\n");
                try self.write("            }\n");
                self.pending_self_call = false;
                return true;
            },
            .call0 => {
                try self.write("            { JSValue func = POP();\n");
                try self.write("              JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 0, NULL);\n");
                try self.write("              FROZEN_FREE(ctx, func);\n");
                try self.emitExceptionCheck("ret", is_trampoline);
                try self.write("              PUSH(ret); }\n");
                self.pending_self_call = false;
                return true;
            },
            .call1 => {
                try self.write("            { JSValue arg0 = POP(); JSValue func = POP();\n");
                try self.write("              JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 1, &arg0);\n");
                try self.write("              FROZEN_FREE(ctx, func); FROZEN_FREE(ctx, arg0);\n");
                try self.emitExceptionCheck("ret", is_trampoline);
                try self.write("              PUSH(ret); }\n");
                self.pending_self_call = false;
                return true;
            },
            .call2 => {
                try self.write("            { JSValue args[2]; args[1] = POP(); args[0] = POP(); JSValue func = POP();\n");
                try self.write("              JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 2, args);\n");
                try self.write("              FROZEN_FREE(ctx, func); FROZEN_FREE(ctx, args[0]); FROZEN_FREE(ctx, args[1]);\n");
                try self.emitExceptionCheck("ret", is_trampoline);
                try self.write("              PUSH(ret); }\n");
                self.pending_self_call = false;
                return true;
            },
            .call3 => {
                try self.write("            { JSValue args[3]; args[2] = POP(); args[1] = POP(); args[0] = POP(); JSValue func = POP();\n");
                try self.write("              JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 3, args);\n");
                try self.write("              FROZEN_FREE(ctx, func); FROZEN_FREE(ctx, args[0]); FROZEN_FREE(ctx, args[1]); FROZEN_FREE(ctx, args[2]);\n");
                try self.emitExceptionCheck("ret", is_trampoline);
                try self.write("              PUSH(ret); }\n");
                self.pending_self_call = false;
                return true;
            },
            .check_brand => {
                try self.write("            { int ret = JS_FrozenCheckBrand(ctx, stack[sp - 2], stack[sp - 1]);\n");
                try self.emitErrorCheck("ret < 0", is_trampoline);
                try self.write(" }\n");
                return true;
            },
            .check_ctor => {
                try self.write("            if (JS_IsUndefined(this_val)) {\n");
                try self.write("              next_block = -1; frame->result = JS_ThrowTypeError(ctx, \"Constructor requires 'new'\"); break;\n");
                try self.write("            }\n");
                return true;
            },
            .check_ctor_return => {
                try self.write("            { JSValue ret = stack[sp - 1];\n");
                try self.write("              if (!JS_IsObject(ret)) {\n");
                try self.write("                PUSH(JS_DupValue(ctx, this_val));\n");
                try self.write("              } }\n");
                return true;
            },
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
                try self.write("                "); try self.emitExceptionCheck("result", is_trampoline);
                try self.write("                JS_FreeValue(ctx, result);\n");
                try self.write("              } }\n");
                return true;
            },
            .dec => {
                try self.write("            { JSValue a = POP(); if (JS_VALUE_GET_TAG(a) == JS_TAG_INT) { PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) - 1)); } else { PUSH(JS_NewFloat64(ctx, JS_VALUE_GET_FLOAT64(JS_ToNumber(ctx, a)) - 1)); } FROZEN_FREE(ctx, a); }\n");
                return true;
            },

            // Stack operations
            // .drop, .dup, .add => moved to emitCommonOpcode
            .dec_loc => {
                const idx = instr.operand.u8;
                try self.print("            {{ JSValue old = frame->locals[{d}]; frame->locals[{d}] = frozen_sub(ctx, old, JS_MKVAL(JS_TAG_INT, 1)); }}\n", .{ idx, idx });
                return true;
            },
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
                        try self.write("              "); try self.emitErrorCheck("ret < 0", is_trampoline); try self.write(" }\n");
                    } else {
                        try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
                    }
                } else {
                    try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
                }
                return true;
            },
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
                try self.write("              "); try self.emitErrorCheck("ret < 0", is_trampoline); try self.write(" }\n");
                return true;
            },
            .define_private_field => {
                try self.write("            { JSValue val = POP(); JSValue name = POP(); JSValue obj = POP();\n");
                try self.write("              int ret = JS_FrozenDefinePrivateField(ctx, obj, name, val);\n");
                try self.write("              FROZEN_FREE(ctx, name);\n");
                try self.write("              if (ret < 0) { FROZEN_FREE(ctx, obj); next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
                try self.write("              PUSH(val); }\n");
                return true;
            },
            .div => {
                try self.write(
                \\            { JSValue b = POP(), a = POP();
                \\              JSValue r = frozen_div(ctx, a, b);
                \\              FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);
                \\              if (JS_IsException(r)) { if (is_trampoline) { next_block = -1; frame->result = r; break; } else { return r; } }
                \\              PUSH(r);
                \\            }
                \\
            );
                return true;
            },
            .drop => {
                try self.write("            FROZEN_FREE(ctx, POP());\n");
                return true;
            },
            .dup => {
                try self.write("            { JSValue tmp = TOP(); PUSH(FROZEN_DUP(ctx, tmp)); }\n");
                return true;
            },
            .dup1 => {
                try self.write("            { JSValue v = stack[sp-2]; PUSH(FROZEN_DUP(ctx, v)); }\n");
                return true;
            },
            .dup2 => {
                try self.write("            { JSValue a = stack[sp-2], b = stack[sp-1]; PUSH(FROZEN_DUP(ctx, a)); PUSH(FROZEN_DUP(ctx, b)); }\n");
                return true;
            },
            .dup3 => {
                try self.write("            { JSValue a = stack[sp-3], b = stack[sp-2], c = stack[sp-1]; PUSH(FROZEN_DUP(ctx, a)); PUSH(FROZEN_DUP(ctx, b)); PUSH(FROZEN_DUP(ctx, c)); }\n");
                return true;
            },
            .eq => {
                try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, JS_IsStrictEqual(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n");
                return true;
            },
            .for_in_next => {
                try self.write("            if (js_frozen_for_in_next(ctx, &stack[sp - 1])) "); try self.emitErrorCheck("true", is_trampoline);
                try self.write("            sp += 2;\n");
                return true;
            },
            .for_in_start => {
                try self.write("            if (js_frozen_for_in_start(ctx, &stack[sp - 1])) "); try self.emitErrorCheck("true", is_trampoline);
                return true;
            },
            .for_of_next => {
                const offset = instr.operand.u8;
                try self.print("            if (js_frozen_for_of_next(ctx, &stack[sp], -{d})) ", .{offset}); try self.emitErrorCheck("true", is_trampoline);
                try self.write("            sp += 2;\n");
                return true;
            },
            .for_of_start => {
                try self.write("            if (js_frozen_for_of_start(ctx, &stack[sp - 1], 0)) "); try self.emitErrorCheck("true", is_trampoline);
                try self.write("            sp += 2;\n");
                return true;
            },
            .get_arg => {
                const idx = instr.operand.u16;
                try self.print("            PUSH(argc_inner > {d} ? FROZEN_DUP(ctx, argv[{d}]) : JS_UNDEFINED);\n", .{ idx, idx });
                return true;
            },
            .get_arg0 => {
                if (self.isZig()) {
                    try self.write("            { const val = if (argc_inner > 0) qjs.JS_DupValue(ctx, argv[0]) else qjs.JS_UNDEFINED;\n");
                    try self.write("              stack[@intCast(sp)] = val; sp += 1; }\n");
                } else {
                    try self.write("            PUSH(argc_inner > 0 ? FROZEN_DUP(ctx, argv[0]) : JS_UNDEFINED);\n");
                }
                return true;
            },
            .get_arg1 => {
                if (self.isZig()) {
                    try self.write("            { const val = if (argc_inner > 1) qjs.JS_DupValue(ctx, argv[1]) else qjs.JS_UNDEFINED;\n");
                    try self.write("              stack[@intCast(sp)] = val; sp += 1; }\n");
                } else {
                    try self.write("            PUSH(argc_inner > 1 ? FROZEN_DUP(ctx, argv[1]) : JS_UNDEFINED);\n");
                }
                return true;
            },
            .get_arg2 => {
                if (self.isZig()) {
                    try self.write("            { const val = if (argc_inner > 2) qjs.JS_DupValue(ctx, argv[2]) else qjs.JS_UNDEFINED;\n");
                    try self.write("              stack[@intCast(sp)] = val; sp += 1; }\n");
                } else {
                    try self.write("            PUSH(argc_inner > 2 ? FROZEN_DUP(ctx, argv[2]) : JS_UNDEFINED);\n");
                }
                return true;
            },
            .get_arg3 => {
                if (self.isZig()) {
                    try self.write("            { const val = if (argc_inner > 3) qjs.JS_DupValue(ctx, argv[3]) else qjs.JS_UNDEFINED;\n");
                    try self.write("              stack[@intCast(sp)] = val; sp += 1; }\n");
                } else {
                    try self.write("            PUSH(argc_inner > 3 ? FROZEN_DUP(ctx, argv[3]) : JS_UNDEFINED);\n");
                }
                return true;
            },
            .get_loc0 => {
                const locals_ref = if (is_trampoline) "frame->locals" else "locals";
                try self.print("            PUSH(FROZEN_DUP(ctx, {s}[0]));\n", .{locals_ref});
                return true;
            },
            .get_loc1 => {
                const locals_ref = if (is_trampoline) "frame->locals" else "locals";
                try self.print("            PUSH(FROZEN_DUP(ctx, {s}[1]));\n", .{locals_ref});
                return true;
            },
            .get_loc2 => {
                const locals_ref = if (is_trampoline) "frame->locals" else "locals";
                try self.print("            PUSH(FROZEN_DUP(ctx, {s}[2]));\n", .{locals_ref});
                return true;
            },
            .get_loc3 => {
                const locals_ref = if (is_trampoline) "frame->locals" else "locals";
                try self.print("            PUSH(FROZEN_DUP(ctx, {s}[3]));\n", .{locals_ref});
                return true;
            },
            .get_private_field => {
                try self.write("            { JSValue name = POP(); JSValue obj = POP();\n");
                try self.write("              JSValue val = JS_FrozenGetPrivateField(ctx, obj, name);\n");
                try self.write("              FROZEN_FREE(ctx, obj); FROZEN_FREE(ctx, name);\n");
                try self.write("              "); try self.emitExceptionCheck("val", is_trampoline);
                try self.write("              PUSH(val); }\n");
                return true;
            },
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
                try self.write("              "); try self.emitExceptionCheck("val", is_trampoline);
                try self.write("              PUSH(val); }\n");
                return true;
            },
            .get_super => {
                if (self.isZig()) {
                    try self.write("            { const obj = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("              const proto = qjs.JS_GetPrototype(ctx, obj);\n");
                    try self.write("              qjs.JS_FreeValue(ctx, obj);\n");
                    try self.write("              if (qjs.JS_IsException(proto) != 0) return proto;\n");
                    try self.write("              stack[@intCast(sp)] = proto; sp += 1; }\n");
                } else {
                    try self.write("            { JSValue obj = POP();\n");
                    try self.write("              JSValue proto = JS_GetPrototype(ctx, obj);\n");
                    try self.write("              FROZEN_FREE(ctx, obj);\n");
                    try self.write("              "); try self.emitExceptionCheck("proto", is_trampoline);
                    try self.write("              PUSH(proto); }\n");
                }
                return true;
            },
            .get_var_ref0 => {
                if (self.options.is_self_recursive) {
                    if (debug) try self.write("            /* get_var_ref0 - self reference */\n");
                    self.pending_self_call = true;
                } else {
                    if (debug) try self.write("            /* get_var_ref0 - closure */\n");
                    try self.write("            PUSH(JS_UNDEFINED);\n");
                }
                return true;
            },
            .gt => {
                try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_gt(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n");
                return true;
            },
            .gte => {
                try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_gte(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n");
                return true;
            },
            .in => {
                try self.write("            { JSValue rhs = POP(), lhs = POP();\n");
                try self.write("              int ret = JS_HasProperty(ctx, rhs, JS_ValueToAtom(ctx, lhs));\n");
                try self.write("              FROZEN_FREE(ctx, lhs); FROZEN_FREE(ctx, rhs);\n");
                try self.write("              "); try self.emitErrorCheck("ret < 0", is_trampoline);
                try self.write("              PUSH(JS_NewBool(ctx, ret)); }\n");
                return true;
            },
            .inc => {
                try self.write("            { JSValue a = POP(); if (JS_VALUE_GET_TAG(a) == JS_TAG_INT) { PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) + 1)); } else { PUSH(JS_NewFloat64(ctx, JS_VALUE_GET_FLOAT64(JS_ToNumber(ctx, a)) + 1)); } FROZEN_FREE(ctx, a); }\n");
                return true;
            },
            .inc_loc => {
                const idx = instr.operand.u8;
                if (is_trampoline) {
                    try self.print("            {{ JSValue old = frame->locals[{d}]; frame->locals[{d}] = frozen_add(ctx, old, JS_MKVAL(JS_TAG_INT, 1)); }}\n", .{ idx, idx });
                } else {
                    try self.print("            {{ JSValue old = locals[{d}]; locals[{d}] = frozen_add(ctx, old, JS_MKVAL(JS_TAG_INT, 1)); }}\n", .{ idx, idx });
                }
                return true;
            },
            .init_ctor => {
                try self.write("            { JSValue proto = JS_GetPropertyStr(ctx, this_val, \"prototype\");\n");
                try self.write("              JSValue this_obj = JS_NewObjectProtoClass(ctx, proto, JS_CLASS_OBJECT);\n");
                try self.write("              FROZEN_FREE(ctx, proto);\n");
                try self.write("              "); try self.emitExceptionCheck("this_obj", is_trampoline);
                try self.write("              PUSH(this_obj); }\n");
                return true;
            },
            .insert2 => {
                try self.write("            { JSValue v = POP(); JSValue a = stack[sp-1]; stack[sp-1] = v; PUSH(a); }\n");
                return true;
            },
            .insert3 => {
                try self.write("            { JSValue v = POP(); JSValue b = stack[sp-1]; JSValue a = stack[sp-2]; stack[sp-2] = v; stack[sp-1] = a; PUSH(b); }\n");
                return true;
            },
            .insert4 => {
                try self.write("            { JSValue v = POP(); JSValue c = stack[sp-1]; JSValue b = stack[sp-2]; JSValue a = stack[sp-3]; stack[sp-3] = v; stack[sp-2] = a; stack[sp-1] = b; PUSH(c); }\n");
                return true;
            },
            .instanceof => {
                try self.write("            { JSValue rhs = POP(), lhs = POP();\n");
                try self.write("              int ret = JS_IsInstanceOf(ctx, lhs, rhs);\n");
                try self.write("              FROZEN_FREE(ctx, lhs); FROZEN_FREE(ctx, rhs);\n");
                try self.write("              "); try self.emitErrorCheck("ret < 0", is_trampoline);
                try self.write("              PUSH(JS_NewBool(ctx, ret)); }\n");
                return true;
            },
            .is_null => {
                try self.write("            { JSValue v = POP(); PUSH(JS_NewBool(ctx, JS_IsNull(v))); FROZEN_FREE(ctx, v); }\n");
                return true;
            },
            .is_undefined_or_null => {
                try self.write("            { JSValue v = POP(); PUSH(JS_NewBool(ctx, JS_IsUndefined(v) || JS_IsNull(v))); FROZEN_FREE(ctx, v); }\n");
                return true;
            },
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
                try self.write("                "); try self.emitExceptionCheck("ret", is_trampoline);
                try self.write("                stack[sp - 1] = ret;\n");
                try self.write("                stack[sp++] = JS_FALSE;\n");
                try self.write("              } }\n");
                return true;
            },
            .iterator_check_object => {
                try self.write("            if (!JS_IsObject(stack[sp - 1])) {\n");
                if (is_trampoline) {
                    try self.write("              next_block = -1; frame->result = JS_ThrowTypeError(ctx, \"iterator must return an object\"); break;\n");
                } else {
                    try self.write("              return JS_ThrowTypeError(ctx, \"iterator must return an object\");\n");
                }
                try self.write("            }\n");
                return true;
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
                if (is_trampoline) {
                    try self.write("                  if (JS_IsException(ret)) { FROZEN_FREE(ctx, iter); next_block = -1; frame->result = ret; break; }\n");
                } else {
                    try self.write("                  if (JS_IsException(ret)) { FROZEN_FREE(ctx, iter); return ret; }\n");
                }
                try self.write("                  JS_FreeValue(ctx, ret);\n");
                try self.write("                } else { JS_FreeValue(ctx, ret_method); }\n");
                try self.write("                FROZEN_FREE(ctx, iter);\n");
                try self.write("              } }\n");
                return true;
            },
            .iterator_get_value_done => {
                try self.write("            { JSValue obj = stack[sp - 1];\n");
                try self.write("              JSValue value = JS_GetPropertyStr(ctx, obj, \"value\");\n");
                try self.write("              JSValue done_val = JS_GetPropertyStr(ctx, obj, \"done\");\n");
                try self.write("              int done = JS_ToBool(ctx, done_val);\n");
                try self.write("              JS_FreeValue(ctx, done_val);\n");
                try self.write("              FROZEN_FREE(ctx, obj);\n");
                try self.write("              stack[sp - 1] = value;\n");
                try self.write("              stack[sp++] = JS_NewBool(ctx, done); }\n");
                return true;
            },
            .iterator_next => {
                try self.write("            { JSValue val = stack[sp - 1];\n");
                try self.write("              JSValue next = stack[sp - 3];\n");
                try self.write("              JSValue iter = stack[sp - 4];\n");
                try self.write("              JSValue ret = JS_Call(ctx, next, iter, 1, &val);\n");
                try self.write("              FROZEN_FREE(ctx, val);\n");
                try self.write("              "); try self.emitExceptionCheck("ret", is_trampoline);
                try self.write("              stack[sp - 1] = ret; }\n");
                return true;
            },
            .lt => {
                try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_lt(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n");
                return true;
            },
            .lte => {
                try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_lte(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n");
                return true;
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
                return true;
            },
            .mod => {
                try self.write(
                    \\            { JSValue b = POP(), a = POP();
                    \\              JSValue r = frozen_mod(ctx, a, b);
                    \\              FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);
                    \\              if (JS_IsException(r)) { if (is_trampoline) { next_block = -1; frame->result = r; break; } else { return r; } }
                    \\              PUSH(r);
                    \\            }
                    \\
                );
                return true;
            },

            // More comparisons
            .mul => {
                try self.write(
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
                \\                  if (JS_IsException(r)) { if (is_trampoline) { next_block = -1; frame->result = r; break; } else { return r; } }
                \\                  PUSH(r);
                \\              }
                \\            }
                \\
            );
                return true;
            },
            .neg => {
                try self.write("            { JSValue a = POP(); PUSH(JS_IsNumber(a) ? JS_NewFloat64(ctx, -JS_VALUE_GET_FLOAT64(JS_ToNumber(ctx, a))) : JS_UNDEFINED); FROZEN_FREE(ctx, a); }\n");
                return true;
            },
            .neq => {
                try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, !JS_IsStrictEqual(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n");
                return true;
            },
            .nip => {
                try self.write("            { JSValue top = POP(); FROZEN_FREE(ctx, POP()); PUSH(top); }\n");
                return true;
            },
            .nip1 => {
                try self.write("            { JSValue top = POP(); JSValue second = POP(); FROZEN_FREE(ctx, POP()); PUSH(second); PUSH(top); }\n");
                return true;
            },
            .object => {
                try self.write("            PUSH(JS_NewObject(ctx));\n");
                return true;
            },
            .perm3 => {
                try self.write("            { JSValue c = stack[sp-1], b = stack[sp-2], a = stack[sp-3]; stack[sp-3] = b; stack[sp-2] = c; stack[sp-1] = a; }\n");
                return true;
            },
            .perm4 => {
                try self.write("            { JSValue d = stack[sp-1], c = stack[sp-2], b = stack[sp-3], a = stack[sp-4]; stack[sp-4] = b; stack[sp-3] = c; stack[sp-2] = d; stack[sp-1] = a; }\n");
                return true;
            },
            .perm5 => {
                try self.write("            { JSValue e = stack[sp-1], d = stack[sp-2], c = stack[sp-3], b = stack[sp-4], a = stack[sp-5]; stack[sp-5] = b; stack[sp-4] = c; stack[sp-3] = d; stack[sp-2] = e; stack[sp-1] = a; }\n");
                return true;
            },
            .plus => {
                try self.write("            { JSValue v = POP(); JSValue r = JS_ToNumber(ctx, v); FROZEN_FREE(ctx, v);\n");
                try self.write("              "); try self.emitExceptionCheck("r", is_trampoline);
                try self.write("              PUSH(r); }\n");
                return true;
            },
            .pow => {
                try self.write("            { JSValue b = POP(), a = POP();\n");
                try self.write("              double da, db;\n");
                try self.write("              JS_ToFloat64(ctx, &da, a); JS_ToFloat64(ctx, &db, b);\n");
                try self.write("              FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);\n");
                try self.write("              PUSH(JS_NewFloat64(ctx, pow(da, db))); }\n");
                return true;
            },
            .private_in => {
                try self.write("            { int ret = js_frozen_private_in(ctx, &stack[sp - 2]);\n");
                try self.write("              "); try self.emitErrorCheck("ret < 0", is_trampoline);
                try self.write("              sp--; }\n");
                return true;
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
                return true;
            },
            .push_0 => {
                try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 0));\n");
                return true;
            },
            .push_1 => {
                try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 1));\n");
                return true;
            },
            .push_2 => {
                try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 2));\n");
                return true;
            },
            .push_3 => {
                try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 3));\n");
                return true;
            },
            .push_4 => {
                try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 4));\n");
                return true;
            },
            .push_5 => {
                try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 5));\n");
                return true;
            },
            .push_6 => {
                try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 6));\n");
                return true;
            },
            .push_7 => {
                try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 7));\n");
                return true;
            },
            .push_empty_string => {
                try self.write("            PUSH(JS_NewString(ctx, \"\"));\n");
                return true;
            },
            .push_false => {
                try self.write("            PUSH(JS_FALSE);\n");
                return true;
            },
            .push_i16 => {
                try self.print("            PUSH(JS_MKVAL(JS_TAG_INT, {d}));\n", .{instr.operand.i16});
                return true;
            },
            .push_i32 => {
                try self.print("            PUSH(JS_MKVAL(JS_TAG_INT, {d}));\n", .{instr.operand.i32});
                return true;
            },
            .push_i8 => {
                try self.print("            PUSH(JS_MKVAL(JS_TAG_INT, {d}));\n", .{instr.operand.i8});
                return true;
            },
            .push_minus1 => {
                try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, -1));\n");
                return true;
            },
            .push_this => {
                try self.write("            PUSH(JS_DupValue(ctx, this_val));\n");
                return true;
            },
            .push_true => {
                try self.write("            PUSH(JS_TRUE);\n");
                return true;
            },
            .put_arg => {
                const idx = instr.operand.arg;
                try self.print("            {{ FROZEN_FREE(ctx, frame->args[{d}]); frame->args[{d}] = POP(); }}\n", .{ idx, idx });
                return true;
            },
            .put_arg0 => {
                try self.write("            { FROZEN_FREE(ctx, frame->args[0]); frame->args[0] = POP(); }\n");
                return true;
            },
            .put_arg1 => {
                try self.write("            { FROZEN_FREE(ctx, frame->args[1]); frame->args[1] = POP(); }\n");
                return true;
            },
            .put_arg2 => {
                try self.write("            { FROZEN_FREE(ctx, frame->args[2]); frame->args[2] = POP(); }\n");
                return true;
            },
            .put_arg3 => {
                try self.write("            { FROZEN_FREE(ctx, frame->args[3]); frame->args[3] = POP(); }\n");
                return true;
            },
            .put_field => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        if (self.isZig()) {
                            try self.write("            { const val = { sp -= 1; const v = stack[@intCast(sp)]; v; };\n");
                            try self.write("              const obj = { sp -= 1; const v = stack[@intCast(sp)]; v; };\n");
                            try self.write("              const ret = qjs.JS_SetPropertyStr(ctx, obj, \"");
                            try self.writeEscapedString(name);
                            try self.write("\", val);\n");
                            try self.write("              qjs.JS_FreeValue(ctx, obj);\n");
                            try self.write("              if (ret < 0) return qjs.JS_EXCEPTION; }\n");
                        } else {
                            try self.write("            { JSValue val = POP(); JSValue obj = POP();\n");
                            try self.write("              int ret = JS_SetPropertyStr(ctx, obj, \"");
                            try self.writeEscapedString(name);
                            try self.write("\", val);\n");
                            try self.write("              FROZEN_FREE(ctx, obj);\n");
                            try self.write("              "); try self.emitErrorCheck("ret < 0", is_trampoline); try self.write(" }\n");
                        }
                    } else {
                        try self.print("            /* put_field: empty atom at {d} */\n", .{atom_idx});
                        if (self.isZig()) {
                            try self.write("            { qjs.JS_FreeValue(ctx, { sp -= 1; const v = stack[@intCast(sp)]; v; });\n");
                            try self.write("              qjs.JS_FreeValue(ctx, { sp -= 1; const v = stack[@intCast(sp)]; v; }); }\n");
                        } else {
                            try self.write("            { FROZEN_FREE(ctx, POP()); FROZEN_FREE(ctx, POP()); }\n");
                        }
                    }
                } else {
                    try self.print("            /* put_field: atom {d} out of bounds */\n", .{atom_idx});
                    if (self.isZig()) {
                        try self.write("            { qjs.JS_FreeValue(ctx, { sp -= 1; const v = stack[@intCast(sp)]; v; });\n");
                        try self.write("              qjs.JS_FreeValue(ctx, { sp -= 1; const v = stack[@intCast(sp)]; v; }); }\n");
                    } else {
                        try self.write("            { FROZEN_FREE(ctx, POP()); FROZEN_FREE(ctx, POP()); }\n");
                    }
                }
                return true;
            },
            .put_loc0 => {
                const locals_ref = if (is_trampoline) "frame->locals" else "locals";
                try self.print("            {{ FROZEN_FREE(ctx, {s}[0]); {s}[0] = POP(); }}\n", .{ locals_ref, locals_ref });
                return true;
            },
            .put_loc1 => {
                const locals_ref = if (is_trampoline) "frame->locals" else "locals";
                try self.print("            {{ FROZEN_FREE(ctx, {s}[1]); {s}[1] = POP(); }}\n", .{ locals_ref, locals_ref });
                return true;
            },
            .put_loc2 => {
                const locals_ref = if (is_trampoline) "frame->locals" else "locals";
                try self.print("            {{ FROZEN_FREE(ctx, {s}[2]); {s}[2] = POP(); }}\n", .{ locals_ref, locals_ref });
                return true;
            },
            .put_loc3 => {
                const locals_ref = if (is_trampoline) "frame->locals" else "locals";
                try self.print("            {{ FROZEN_FREE(ctx, {s}[3]); {s}[3] = POP(); }}\n", .{ locals_ref, locals_ref });
                return true;
            },
            .put_private_field => {
                try self.write("            { JSValue val = POP(); JSValue name = POP(); JSValue obj = POP();\n");
                try self.write("              int ret = JS_FrozenSetPrivateField(ctx, obj, name, val);\n");
                try self.write("              FROZEN_FREE(ctx, obj); FROZEN_FREE(ctx, name);\n");
                try self.write("              "); try self.emitErrorCheck("ret < 0", is_trampoline); try self.write(" }\n");
                return true;
            },
            .put_ref_value => {
                try self.write("            { JSValue val = POP(); JSValue prop = POP(); JSValue obj = POP();\n");
                try self.write("              if (JS_IsUndefined(obj)) {\n");
                try self.write("                obj = JS_GetGlobalObject(ctx);\n");
                try self.write("              }\n");
                try self.write("              int ret = JS_SetPropertyValue(ctx, obj, prop, val, JS_PROP_THROW_STRICT);\n");
                try self.write("              FROZEN_FREE(ctx, obj);\n");
                try self.write("              "); try self.emitErrorCheck("ret < 0", is_trampoline); try self.write(" }\n");
                return true;
            },
            .regexp => {
                if (self.isZig()) {
                    try self.write("            { const flags = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("              const pattern = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("              const global = qjs.JS_GetGlobalObject(ctx);\n");
                    try self.write("              const RegExp = qjs.JS_GetPropertyStr(ctx, global, \"RegExp\");\n");
                    try self.write("              qjs.JS_FreeValue(ctx, global);\n");
                    try self.write("              const args = [2]qjs.JSValue{ pattern, flags };\n");
                    try self.write("              const rx = qjs.JS_CallConstructor(ctx, RegExp, 2, &args);\n");
                    try self.write("              qjs.JS_FreeValue(ctx, RegExp);\n");
                    try self.write("              qjs.JS_FreeValue(ctx, pattern); qjs.JS_FreeValue(ctx, flags);\n");
                    try self.write("              if (qjs.JS_IsException(rx) != 0) return rx;\n");
                    try self.write("              stack[@intCast(sp)] = rx; sp += 1; }\n");
                } else {
                    try self.write("            { JSValue flags = POP(); JSValue pattern = POP();\n");
                    try self.write("              JSValue global = JS_GetGlobalObject(ctx);\n");
                    try self.write("              JSValue RegExp = JS_GetPropertyStr(ctx, global, \"RegExp\");\n");
                    try self.write("              JS_FreeValue(ctx, global);\n");
                    try self.write("              JSValue args[2] = { pattern, flags };\n");
                    try self.write("              JSValue rx = JS_CallConstructor(ctx, RegExp, 2, args);\n");
                    try self.write("              JS_FreeValue(ctx, RegExp);\n");
                    try self.write("              FROZEN_FREE(ctx, pattern); FROZEN_FREE(ctx, flags);\n");
                    try self.write("              "); try self.emitExceptionCheck("rx", is_trampoline);
                    try self.write("              PUSH(rx); }\n");
                }
                return true;
            },
            .rest => {
                const first_arg = instr.operand.u16;
                try self.print("            {{ JSValue arr = JS_NewArray(ctx);\n", .{});
                try self.print("              for (int i = {d}; i < argc_inner; i++) {{\n", .{first_arg});
                try self.write("                JS_SetPropertyUint32(ctx, arr, i - ");
                try self.print("{d}, JS_DupValue(ctx, argv[i]));\n", .{first_arg});
                try self.write("              }\n");
                try self.write("              PUSH(arr); }\n");
                return true;
            },
            .return_undef => {
                if (debug) try self.write("            /* return_undef */\n");
                if (is_trampoline) {
                    try self.write("            frame->result = JS_UNDEFINED; next_block = -1; break;\n");
                } else {
                    try self.write("            return JS_UNDEFINED;\n");
                }
                return true;
            },
            .rot3l => {
                try self.write("            { JSValue c = stack[sp-1], b = stack[sp-2], a = stack[sp-3]; stack[sp-3] = b; stack[sp-2] = c; stack[sp-1] = a; }\n");
                return true;
            },
            .rot3r => {
                try self.write("            { JSValue c = stack[sp-1], b = stack[sp-2], a = stack[sp-3]; stack[sp-3] = c; stack[sp-2] = a; stack[sp-1] = b; }\n");
                return true;
            },
            .rot4l => {
                try self.write("            { JSValue d = stack[sp-1], c = stack[sp-2], b = stack[sp-3], a = stack[sp-4]; stack[sp-4] = b; stack[sp-3] = c; stack[sp-2] = d; stack[sp-1] = a; }\n");
                return true;
            },
            .rot5l => {
                try self.write("            { JSValue e = stack[sp-1], d = stack[sp-2], c = stack[sp-3], b = stack[sp-4], a = stack[sp-5]; stack[sp-5] = b; stack[sp-4] = c; stack[sp-3] = d; stack[sp-2] = e; stack[sp-1] = a; }\n");
                return true;
            },

            // Local variables (use frame->locals array)
            .sar => {
                try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) >> (JS_VALUE_GET_INT(b) & 31))); }\n");
                return true;
            },
            .set_arg => {
                const idx = instr.operand.arg;
                // set_arg: like put_arg but leaves value on stack
                try self.print("            {{ FROZEN_FREE(ctx, frame->args[{d}]); frame->args[{d}] = FROZEN_DUP(ctx, TOP()); }}\n", .{ idx, idx });
                return true;
            },
            .set_loc0 => {
                const locals_ref = if (is_trampoline) "frame->locals" else "locals";
                try self.print("            {{ FROZEN_FREE(ctx, {s}[0]); {s}[0] = FROZEN_DUP(ctx, TOP()); }}\n", .{ locals_ref, locals_ref });
                return true;
            },
            .set_loc1 => {
                const locals_ref = if (is_trampoline) "frame->locals" else "locals";
                try self.print("            {{ FROZEN_FREE(ctx, {s}[1]); {s}[1] = FROZEN_DUP(ctx, TOP()); }}\n", .{ locals_ref, locals_ref });
                return true;
            },
            .set_loc2 => {
                const locals_ref = if (is_trampoline) "frame->locals" else "locals";
                try self.print("            {{ FROZEN_FREE(ctx, {s}[2]); {s}[2] = FROZEN_DUP(ctx, TOP()); }}\n", .{ locals_ref, locals_ref });
                return true;
            },
            .set_loc3 => {
                const locals_ref = if (is_trampoline) "frame->locals" else "locals";
                try self.print("            {{ FROZEN_FREE(ctx, {s}[3]); {s}[3] = FROZEN_DUP(ctx, TOP()); }}\n", .{ locals_ref, locals_ref });
                return true;
            },
            .shl => {
                try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) << (JS_VALUE_GET_INT(b) & 31))); }\n");
                return true;
            },
            .shr => {
                try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_MKVAL(JS_TAG_INT, (int32_t)((uint32_t)JS_VALUE_GET_INT(a) >> (JS_VALUE_GET_INT(b) & 31)))); }\n");
                return true;
            },

            // Power operator
            .special_object => {
                const obj_type = instr.operand.u8;
                switch (obj_type) {
                    0 => try self.write("            PUSH(JS_NewArguments(ctx, argc_inner, argv));\n"), // arguments
                    2 => try self.print("            PUSH(JS_DupValue(ctx, _{s}_this_func));\n", .{self.options.func_name}), // this_func
                    3 => try self.write("            PUSH(JS_DupValue(ctx, this_val));\n"), // new_target
                    5, 7 => try self.write("            PUSH(JS_NewObjectProto(ctx, JS_NULL));\n"), // var_object, null_proto
                    else => try self.write("            PUSH(JS_UNDEFINED); /* unsupported special_object */\n"),
                }
                return true;
            },
            .strict_eq => {
                try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, JS_IsStrictEqual(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n");
                return true;
            },
            .strict_neq => {
                try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, !JS_IsStrictEqual(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n");
                return true;
            },

            // Unary operators
            .sub => {
                try self.write(
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
                \\                  if (JS_IsException(r)) { if (is_trampoline) { next_block = -1; frame->result = r; break; } else { return r; } }
                \\                  PUSH(r);
                \\              }
                \\            }
                \\
            );
                return true;
            },
            .swap => {
                try self.write("            { JSValue b = stack[sp-1], a = stack[sp-2]; stack[sp-2] = b; stack[sp-1] = a; }\n");
                return true;
            },
            .swap2 => {
                try self.write("            { JSValue d = stack[sp-1], c = stack[sp-2], b = stack[sp-3], a = stack[sp-4]; stack[sp-4] = c; stack[sp-3] = d; stack[sp-2] = a; stack[sp-1] = b; }\n");
                return true;
            },
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
                try self.write("\n              "); try self.emitExceptionCheck("ret", is_trampoline);
                try self.write("              frame->result = ret; next_block = -1; break; }\n");
                self.pending_self_call = false;
                return true;
            },

            // Array.from() / spread operator
            .to_object => {
                try self.write("            { JSValue v = POP();\n");
                try self.write("              JSValue obj = JS_ToObject(ctx, v);\n");
                try self.write("              FROZEN_FREE(ctx, v);\n");
                try self.write("              "); try self.emitExceptionCheck("obj", is_trampoline);
                try self.write("              PUSH(obj); }\n");
                return true;
            },
            .to_propkey => {
                try self.write("            { JSValue v = POP(); PUSH(JS_ToPropertyKey(ctx, v)); FROZEN_FREE(ctx, v); }\n");
                return true;
            },
            .typeof => {
                try self.write("            { JSValue v = POP(); JSValue t = frozen_typeof(ctx, v); FROZEN_FREE(ctx, v); PUSH(t); }\n");
                return true;
            },
            .typeof_is_function => {
                try self.write("            { JSValue v = POP(); PUSH(JS_NewBool(ctx, JS_IsFunction(ctx, v))); FROZEN_FREE(ctx, v); }\n");
                return true;
            },
            .typeof_is_undefined => {
                try self.write("            { JSValue v = POP(); PUSH(JS_NewBool(ctx, JS_IsUndefined(v))); FROZEN_FREE(ctx, v); }\n");
                return true;
            },
            .undefined => {
                try self.write("            PUSH(JS_UNDEFINED);\n");
                return true;
            },
            .xor => {
                try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) ^ JS_VALUE_GET_INT(b))); }\n");
                return true;
            },

            // push_atom_value - push atom as string value
            .push_atom_value => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        if (self.isZig()) {
                            try self.write("            { const str = qjs.JS_NewString(ctx, \"");
                            try self.writeEscapedString(name);
                            try self.write("\");\n");
                            try self.write("              stack[@intCast(sp)] = str; sp += 1; }\n");
                        } else {
                            try self.write("            PUSH(JS_NewString(ctx, \"");
                            try self.writeEscapedString(name);
                            try self.write("\"));\n");
                        }
                    } else {
                        if (self.isZig()) {
                            try self.write("            { const str = qjs.JS_NewString(ctx, \"\");\n");
                            try self.write("              stack[@intCast(sp)] = str; sp += 1; }\n");
                        } else {
                            try self.write("            PUSH(JS_NewString(ctx, \"\"));\n");
                        }
                    }
                } else {
                    try self.print("            /* push_atom_value: atom {d} out of bounds */\n", .{atom_idx});
                    if (self.isZig()) {
                        try self.write("            stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1;\n");
                    } else {
                        try self.write("            PUSH(JS_UNDEFINED);\n");
                    }
                }
                return true;
            },

            // get_loc_check - TDZ (Temporal Dead Zone) check for local variable
            .get_loc_check => {
                const idx = instr.operand.loc;
                if (debug) try self.print("            /* get_loc_check {d} */\n", .{idx});
                if (self.isZig()) {
                    try self.print("            {{ const v = locals[{d}];\n", .{idx});
                    try self.write("              if (qjs.JS_IsUninitialized(v) != 0) {\n");
                    try self.write("                return qjs.JS_ThrowReferenceError(ctx, \"Cannot access before initialization\");\n");
                    try self.write("              }\n");
                    try self.write("              const dup = qjs.JS_DupValue(ctx, v);\n");
                    try self.write("              stack[@intCast(sp)] = dup; sp += 1; }\n");
                } else {
                    const locals_ref = if (is_trampoline) "frame->locals" else "locals";
                    try self.print("            {{ JSValue v = {s}[{d}];\n", .{ locals_ref, idx });
                    try self.write("              if (JS_IsUninitialized(v)) {\n");
                    if (is_trampoline) {
                        try self.write("                next_block = -1; frame->result = JS_ThrowReferenceError(ctx, \"Cannot access before initialization\"); break;\n");
                    } else {
                        try self.write("                return JS_ThrowReferenceError(ctx, \"Cannot access before initialization\");\n");
                    }
                    try self.write("              }\n");
                    try self.write("              PUSH(FROZEN_DUP(ctx, v)); }\n");
                }
                return true;
            },

            // set_loc_uninitialized - mark local variable as uninitialized (for TDZ)
            .set_loc_uninitialized => {
                const idx = instr.operand.loc;
                if (self.isZig()) {
                    try self.print("            locals[{d}] = qjs.JS_UNINITIALIZED;\n", .{idx});
                } else {
                    const locals_ref = if (is_trampoline) "frame->locals" else "locals";
                    try self.print("            {s}[{d}] = JS_UNINITIALIZED;\n", .{ locals_ref, idx });
                }
                return true;
            },

            // put_loc_check_init - initialize local variable with TDZ check (must be uninitialized)
            .put_loc_check_init => {
                const idx = instr.operand.loc;
                if (self.isZig()) {
                    try self.print("            {{ const v = locals[{d}];\n", .{idx});
                    try self.write("              if (qjs.JS_IsUninitialized(v) == 0) {{\n");
                    try self.write("                return qjs.JS_ThrowReferenceError(ctx, \"Identifier already declared\");\n");
                    try self.write("              }}\n");
                    try self.print("              locals[{d}] = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }}; }}\n", .{idx});
                } else {
                    const locals_ref = if (is_trampoline) "frame->locals" else "locals";
                    try self.print("            {{ JSValue v = {s}[{d}];\n", .{ locals_ref, idx });
                    try self.write("              if (!JS_IsUninitialized(v)) {{\n");
                    if (is_trampoline) {
                        try self.write("                next_block = -1; frame->result = JS_ThrowReferenceError(ctx, \"Identifier already declared\"); break;\n");
                    } else {
                        try self.write("                return JS_ThrowReferenceError(ctx, \"Identifier already declared\");\n");
                    }
                    try self.write("              }}\n");
                    try self.print("              {s}[{d}] = POP(); }}\n", .{ locals_ref, idx });
                }
                return true;
            },

            // get_var_ref_check - closure variable with TDZ check (frozen doesn't support closures)
            .get_var_ref_check => {
                if (self.isZig()) {
                    try self.write("            stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1;\n");
                } else {
                    try self.write("            PUSH(JS_UNDEFINED);\n");
                }
                return true;
            },

            // call_method - method call: obj.method(args)
            .call_method => {
                const argc = instr.operand.u16;
                if (self.isZig()) {
                    try self.write("            {{\n");
                    if (argc > 0) {
                        try self.print("              var args: [{d}]qjs.JSValue = undefined;\n", .{argc});
                        var i = argc;
                        while (i > 0) {
                            i -= 1;
                            try self.print("              args[{d}] = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }};\n", .{i});
                        }
                    }
                    try self.write("              const this_obj = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("              const func = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    if (argc > 0) {
                        try self.print("              const result = qjs.JS_Call(ctx, func, this_obj, {d}, &args);\n", .{argc});
                    } else {
                        try self.write("              const result = qjs.JS_Call(ctx, func, this_obj, 0, null);\n");
                    }
                    try self.write("              qjs.JS_FreeValue(ctx, func);\n");
                    try self.write("              qjs.JS_FreeValue(ctx, this_obj);\n");
                    if (argc > 0) {
                        var j: u16 = 0;
                        while (j < argc) : (j += 1) {
                            try self.print("              qjs.JS_FreeValue(ctx, args[{d}]);\n", .{j});
                        }
                    }
                    try self.write("              if (qjs.JS_IsException(result) != 0) return result;\n");
                    try self.write("              stack[@intCast(sp)] = result; sp += 1;\n");
                    try self.write("            }}\n");
                } else {
                    try self.write("            {{\n");
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
                    if (is_trampoline) {
                        try self.write("              if (JS_IsException(result)) {{ next_block = -1; frame->result = result; break; }}\n");
                    } else {
                        try self.write("              if (JS_IsException(result)) return result;\n");
                    }
                    try self.write("              PUSH(result);\n");
                    try self.write("            }}\n");
                }
                return true;
            },

            // call_constructor - constructor call: new Foo(args)
            .call_constructor => {
                const argc: u16 = instr.operand.u16;
                if (self.isZig()) {
                    try self.write("            {{\n");
                    if (argc > 0) {
                        try self.print("              var args: [{d}]qjs.JSValue = undefined;\n", .{argc});
                        var i: u16 = argc;
                        while (i > 0) {
                            i -= 1;
                            try self.print("              args[{d}] = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }};\n", .{i});
                        }
                    }
                    try self.write("              const ctor = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }};\n");
                    if (argc > 0) {
                        try self.print("              const ret = qjs.JS_CallConstructor(ctx, ctor, {d}, &args);\n", .{argc});
                    } else {
                        try self.write("              const ret = qjs.JS_CallConstructor(ctx, ctor, 0, null);\n");
                    }
                    try self.write("              qjs.JS_FreeValue(ctx, ctor);\n");
                    if (argc > 0) {
                        var j: u16 = 0;
                        while (j < argc) : (j += 1) {
                            try self.print("              qjs.JS_FreeValue(ctx, args[{d}]);\n", .{j});
                        }
                    }
                    try self.write("              if (qjs.JS_IsException(ret) != 0) return ret;\n");
                    try self.write("              stack[@intCast(sp)] = ret; sp += 1;\n");
                    try self.write("            }}\n");
                } else {
                    try self.print("            {{ JSValue args[{d} > 0 ? {d} : 1]; ", .{ argc, argc });
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
                    if (is_trampoline) {
                        try self.write("\n              if (JS_IsException(ret)) {{ next_block = -1; frame->result = ret; break; }}\n");
                    } else {
                        try self.write("\n              if (JS_IsException(ret)) return ret;\n");
                    }
                    try self.write("              PUSH(ret); }}\n");
                }
                self.pending_self_call = false;
                return true;
            },

            // Global variable access
            .get_var, .get_var_undef => {
                if (self.options.is_self_recursive) {
                    if (debug) try self.write("            /* get_var - potential self reference */\n");
                    self.pending_self_call = true;
                } else {
                    const atom_idx = instr.operand.atom;
                    if (atom_idx < self.options.atom_strings.len) {
                        const name = self.options.atom_strings[atom_idx];
                        if (name.len > 0) {
                            if (self.isZig()) {
                                try self.write("            {{ const global = qjs.JS_GetGlobalObject(ctx);\n");
                                try self.write("              const val = qjs.JS_GetPropertyStr(ctx, global, \"");
                                try self.writeEscapedString(name);
                                try self.write("\");\n");
                                try self.write("              qjs.JS_FreeValue(ctx, global);\n");
                                if (instr.opcode == .get_var_undef) {
                                    try self.write("              stack[@intCast(sp)] = val; sp += 1; }}\n");
                                } else {
                                    try self.write("              if (qjs.JS_IsException(val) != 0) return val;\n");
                                    try self.write("              stack[@intCast(sp)] = val; sp += 1; }}\n");
                                }
                            } else {
                                try self.write("            { JSValue global = JS_GetGlobalObject(ctx);\n");
                                try self.write("              JSValue val = JS_GetPropertyStr(ctx, global, \"");
                                try self.writeEscapedString(name);
                                try self.write("\");\n");
                                try self.write("              JS_FreeValue(ctx, global);\n");
                                if (instr.opcode == .get_var_undef) {
                                    try self.write("              PUSH(val); }\n");
                                } else {
                                    try self.write("              if (JS_IsException(val)) ");
                                    if (is_trampoline) {
                                        try self.write("{ next_block = -1; frame->result = val; break; }\n");
                                    } else {
                                        try self.write("return val;\n");
                                    }
                                    try self.write("              PUSH(val); }\n");
                                }
                            }
                        } else {
                            try self.print("            /* get_var: empty atom string at index {d} */\n", .{atom_idx});
                            if (self.isZig()) {
                                try self.write("            stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1;\n");
                            } else {
                                try self.write("            PUSH(JS_UNDEFINED);\n");
                            }
                        }
                    } else {
                        try self.print("            /* get_var: atom {d} out of bounds */\n", .{atom_idx});
                        if (self.isZig()) {
                            try self.write("            stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1;\n");
                        } else {
                            try self.write("            PUSH(JS_UNDEFINED);\n");
                        }
                    }
                }
                return true;
            },

            // throw - throw exception
            .throw => {
                if (self.isZig()) {
                    try self.write("            {{ const exc = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }};\n");
                    try self.write("              _ = qjs.JS_Throw(ctx, exc);\n");
                    try self.write("              return qjs.JS_EXCEPTION; }}\n");
                } else {
                    if (is_trampoline) {
                        try self.write("            { JSValue exc = POP(); JS_Throw(ctx, exc); next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
                    } else {
                        try self.write("            { JSValue exc = POP(); JS_Throw(ctx, exc); return JS_EXCEPTION; }\n");
                    }
                }
                return true;
            },

            // put_array_el - array element assignment: arr[idx] = val
            .put_array_el => {
                if (self.isZig()) {
                    try self.write("            {{ const val = {{ sp -= 1; const v = stack[@intCast(sp)]; v; }};\n");
                    try self.write("              const idx = {{ sp -= 1; const v = stack[@intCast(sp)]; v; }};\n");
                    try self.write("              const arr = {{ sp -= 1; const v = stack[@intCast(sp)]; v; }};\n");
                    try self.write("              var i: i64 = 0;\n");
                    try self.write("              _ = qjs.JS_ToInt64(ctx, &i, idx);\n");
                    try self.write("              const ret = qjs.JS_SetPropertyInt64(ctx, arr, i, val);\n");
                    try self.write("              qjs.JS_FreeValue(ctx, arr); qjs.JS_FreeValue(ctx, idx);\n");
                    try self.write("              if (ret < 0) return qjs.JS_EXCEPTION; }}\n");
                } else {
                    try self.write("            { JSValue val = POP(); JSValue idx = POP(); JSValue arr = POP();\n");
                    try self.write("              int64_t i; JS_ToInt64(ctx, &i, idx);\n");
                    try self.write("              int ret = JS_SetPropertyInt64(ctx, arr, i, val);\n");
                    try self.write("              FROZEN_FREE(ctx, arr); FROZEN_FREE(ctx, idx);\n");
                    if (is_trampoline) {
                        try self.write("              if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; } }\n");
                    } else {
                        try self.write("              if (ret < 0) return JS_EXCEPTION; }\n");
                    }
                }
                return true;
            },

            // get_length - get array/string length
            .get_length => {
                if (self.isZig()) {
                    try self.write("            {{ const obj = {{ sp -= 1; const v = stack[@intCast(sp)]; v; }};\n");
                    try self.write("              const len = frozen_get_length(ctx, obj);\n");
                    try self.write("              qjs.JS_FreeValue(ctx, obj);\n");
                    try self.write("              stack[@intCast(sp)] = qjs.JS_NewInt64(ctx, len); sp += 1; }}\n");
                } else {
                    try self.write("            { JSValue obj = POP(); int64_t len = frozen_get_length(ctx, obj); FROZEN_FREE(ctx, obj); PUSH(JS_NewInt64(ctx, len)); }\n");
                }
                return true;
            },

            // catch - push exception onto stack
            .@"catch" => {
                if (self.isZig()) {
                    try self.write("            {{ const exc = qjs.JS_GetException(ctx);\n");
                    try self.write("              stack[@intCast(sp)] = exc; sp += 1; }}\n");
                } else {
                    try self.write("            { JSValue exc = JS_GetException(ctx); PUSH(exc); }\n");
                }
                return true;
            },

            // set_var_ref0/1/2/3 - closure variable set (stub for frozen functions)
            .set_var_ref0, .set_var_ref1, .set_var_ref2, .set_var_ref3 => {
                if (self.isZig()) {
                    try self.write("            {{ sp -= 1; const val = stack[@intCast(sp)]; qjs.JS_FreeValue(ctx, val); }}\n");
                } else {
                    try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
                }
                return true;
            },

            // put_var_ref_check - closure variable with TDZ check (stub for frozen functions)
            .put_var_ref_check => {
                if (self.isZig()) {
                    try self.write("            {{ sp -= 1; const val = stack[@intCast(sp)]; qjs.JS_FreeValue(ctx, val); }}\n");
                } else {
                    try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
                }
                return true;
            },

            // nip_catch - remove catch handler from stack
            .nip_catch => {
                if (self.isZig()) {
                    try self.write("            {{ sp -= 1; const val = stack[@intCast(sp)]; qjs.JS_FreeValue(ctx, val); }}\n");
                } else {
                    try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
                }
                return true;
            },

            // get_array_el - array element read: arr[idx]
            .get_array_el => {
                if (self.isZig()) {
                    try self.write("            {{ const idx = {{ sp -= 1; const v = stack[@intCast(sp)]; v; }};\n");
                    try self.write("              const arr = {{ sp -= 1; const v = stack[@intCast(sp)]; v; }};\n");
                    try self.write("              var i: i64 = 0;\n");
                    try self.write("              _ = qjs.JS_ToInt64(ctx, &i, idx);\n");
                    try self.write("              const ret = qjs.JS_GetPropertyInt64(ctx, arr, i);\n");
                    try self.write("              qjs.JS_FreeValue(ctx, arr); qjs.JS_FreeValue(ctx, idx);\n");
                    try self.write("              if (qjs.JS_IsException(ret) != 0) return ret;\n");
                    try self.write("              stack[@intCast(sp)] = ret; sp += 1; }}\n");
                } else {
                    try self.write("            { JSValue idx = POP(); JSValue arr = POP();\n");
                    try self.write("              int64_t i; JS_ToInt64(ctx, &i, idx);\n");
                    try self.write("              JSValue ret = JS_GetPropertyInt64(ctx, arr, i);\n");
                    try self.write("              FROZEN_FREE(ctx, arr); FROZEN_FREE(ctx, idx);\n");
                    if (is_trampoline) {
                        try self.write("              if (JS_IsException(ret)) { next_block = -1; frame->result = ret; break; }\n");
                    } else {
                        try self.write("              if (JS_IsException(ret)) return ret;\n");
                    }
                    try self.write("              PUSH(ret); }\n");
                }
                return true;
            },

            // delete - delete operator: delete obj[key]
            .delete => {
                if (self.isZig()) {
                    try self.write("            {{ const key = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }};\n");
                    try self.write("              const obj = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }};\n");
                    try self.write("              const prop = if (qjs.JS_IsString(key) != 0) blk: {{\n");
                    try self.write("                const str = qjs.JS_ToCString(ctx, key);\n");
                    try self.write("                const atom = qjs.JS_NewAtom(ctx, str);\n");
                    try self.write("                qjs.JS_FreeCString(ctx, str);\n");
                    try self.write("                break :blk atom;\n");
                    try self.write("              }} else qjs.JS_ValueToAtom(ctx, key);\n");
                    try self.write("              qjs.JS_FreeValue(ctx, key);\n");
                    try self.write("              const ret = qjs.JS_DeleteProperty(ctx, obj, prop, 0);\n");
                    try self.write("              qjs.JS_FreeAtom(ctx, prop);\n");
                    try self.write("              qjs.JS_FreeValue(ctx, obj);\n");
                    try self.write("              if (ret < 0) return qjs.JS_EXCEPTION;\n");
                    try self.write("              const result = qjs.JS_NewBool(ctx, if (ret > 0) 1 else 0);\n");
                    try self.write("              stack[@intCast(sp)] = result; sp += 1; }}\n");
                } else {
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
                    if (is_trampoline) {
                        try self.write("              if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
                    } else {
                        try self.write("              if (ret < 0) return JS_EXCEPTION;\n");
                    }
                    try self.write("              PUSH(JS_NewBool(ctx, ret)); }\n");
                }
                return true;
            },

            // put_loc_check - set local variable with TDZ check
            .put_loc_check => {
                const idx = instr.operand.loc;
                if (self.isZig()) {
                    try self.print("            {{ const v = locals[{d}];\n", .{idx});
                    try self.write("              if (qjs.JS_IsUninitialized(v) != 0) {{\n");
                    try self.write("                return qjs.JS_ThrowReferenceError(ctx, \"Cannot access before initialization\");\n");
                    try self.write("              }}\n");
                    try self.print("              qjs.JS_FreeValue(ctx, locals[{d}]);\n", .{idx});
                    try self.print("              locals[{d}] = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }}; }}\n", .{idx});
                } else {
                    const locals_ref = if (is_trampoline) "frame->locals" else "locals";
                    try self.print("            {{ JSValue v = {s}[{d}];\n", .{ locals_ref, idx });
                    try self.write("              if (JS_IsUninitialized(v)) {{\n");
                    if (is_trampoline) {
                        try self.write("                next_block = -1; frame->result = JS_ThrowReferenceError(ctx, \"Cannot access before initialization\"); break;\n");
                    } else {
                        try self.write("                return JS_ThrowReferenceError(ctx, \"Cannot access before initialization\");\n");
                    }
                    try self.write("              }}\n");
                    try self.print("              FROZEN_FREE(ctx, {s}[{d}]); {s}[{d}] = POP(); }}\n", .{ locals_ref, idx, locals_ref, idx });
                }
                return true;
            },

            // Return false for opcodes not in shared list
            else => return false,
        }
    }


    fn emitTrampolineInstruction(self: *SSACodeGen, instr: Instruction) !void {
        const debug = false;

        // Try shared opcodes first
        if (try self.emitCommonOpcode(instr, true)) return;

        switch (instr.opcode) {
            // Control flow - set next_block instead of goto/return
            .@"return" => {
                if (debug) try self.write("            /* return */\n");
                try self.write("            frame->result = POP(); next_block = -1; break;\n");
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
                            if (self.isZig()) {
                                try self.write("            { const global = qjs.JS_GetGlobalObject(ctx);\n");
                                try self.write("              const val = qjs.JS_GetPropertyStr(ctx, global, \"");
                                try self.writeEscapedString(name);
                                try self.write("\");\n");
                                try self.write("              qjs.JS_FreeValue(ctx, global);\n");
                                if (instr.opcode == .get_var_undef) {
                                    try self.write("              stack[@intCast(sp)] = val; sp += 1; }\n");
                                } else {
                                    try self.write("              if (qjs.JS_IsException(val) != 0) return val;\n");
                                    try self.write("              stack[@intCast(sp)] = val; sp += 1; }\n");
                                }
                            } else {
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
                            }
                        } else {
                            try self.print("            /* get_var: empty atom string at index {d} */\n", .{atom_idx});
                            if (self.isZig()) {
                                try self.write("            stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1;\n");
                            } else {
                                try self.write("            PUSH(JS_UNDEFINED);\n");
                            }
                        }
                    } else {
                        try self.print("            /* get_var: atom {d} out of bounds */\n", .{atom_idx});
                        if (self.isZig()) {
                            try self.write("            stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1;\n");
                        } else {
                            try self.write("            PUSH(JS_UNDEFINED);\n");
                        }
                    }
                }
            },
            // Set global variable
            .put_var, .put_var_init, .put_var_strict => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        if (self.isZig()) {
                            try self.write("            { const val = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                            try self.write("              const global = qjs.JS_GetGlobalObject(ctx);\n");
                            try self.write("              _ = qjs.JS_SetPropertyStr(ctx, global, \"");
                            try self.writeEscapedString(name);
                            try self.write("\", val);\n");
                            try self.write("              qjs.JS_FreeValue(ctx, global); }\n");
                        } else {
                            try self.write("            { JSValue val = POP();\n");
                            try self.write("              JSValue global = JS_GetGlobalObject(ctx);\n");
                            try self.write("              JS_SetPropertyStr(ctx, global, \"");
                            try self.writeEscapedString(name);
                            try self.write("\", val);\n");
                            try self.write("              JS_FreeValue(ctx, global); }\n");
                        }
                    } else {
                        try self.print("            /* put_var: empty atom at {d} */\n", .{atom_idx});
                        if (self.isZig()) {
                            try self.write("            { const val = { sp -= 1; const val = stack[@intCast(sp)]; val; }; qjs.JS_FreeValue(ctx, val); }\n");
                        } else {
                            try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
                        }
                    }
                } else {
                    try self.print("            /* put_var: atom {d} out of bounds */\n", .{atom_idx});
                    if (self.isZig()) {
                        try self.write("            { const val = { sp -= 1; const val = stack[@intCast(sp)]; val; }; qjs.JS_FreeValue(ctx, val); }\n");
                    } else {
                        try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
                    }
                }
            },
            // Check if global variable exists - pushes boolean
            .check_var => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        if (self.isZig()) {
                            try self.write("            { const global = qjs.JS_GetGlobalObject(ctx);\n");
                            try self.write("              const prop = qjs.JS_NewAtom(ctx, \"");
                            try self.writeEscapedString(name);
                            try self.write("\");\n");
                            try self.write("              const has = qjs.JS_HasProperty(ctx, global, prop);\n");
                            try self.write("              qjs.JS_FreeAtom(ctx, prop);\n");
                            try self.write("              qjs.JS_FreeValue(ctx, global);\n");
                            try self.write("              stack[@intCast(sp)] = qjs.JS_NewBool(ctx, has > 0); sp += 1; }\n");
                        } else {
                            try self.write("            { JSValue global = JS_GetGlobalObject(ctx);\n");
                            try self.write("              JSAtom prop = JS_NewAtom(ctx, \"");
                            try self.writeEscapedString(name);
                            try self.write("\");\n");
                            try self.write("              int has = JS_HasProperty(ctx, global, prop);\n");
                            try self.write("              JS_FreeAtom(ctx, prop);\n");
                            try self.write("              JS_FreeValue(ctx, global);\n");
                            try self.write("              PUSH(JS_NewBool(ctx, has > 0)); }\n");
                        }
                    } else {
                        if (self.isZig()) {
                            try self.write("            stack[@intCast(sp)] = qjs.JS_FALSE; sp += 1;\n");
                        } else {
                            try self.write("            PUSH(JS_FALSE);\n");
                        }
                    }
                } else {
                    if (self.isZig()) {
                        try self.write("            stack[@intCast(sp)] = qjs.JS_FALSE; sp += 1;\n");
                    } else {
                        try self.write("            PUSH(JS_FALSE);\n");
                    }
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
                        if (self.isZig()) {
                            try self.write("            { const global = qjs.JS_GetGlobalObject(ctx);\n");
                            try self.write("              const prop = qjs.JS_NewAtom(ctx, \"");
                            try self.writeEscapedString(name);
                            try self.write("\");\n");
                            try self.write("              _ = qjs.JS_DefinePropertyValue(ctx, global, prop, qjs.JS_UNDEFINED, qjs.JS_PROP_C_W_E);\n");
                            try self.write("              qjs.JS_FreeAtom(ctx, prop);\n");
                            try self.write("              qjs.JS_FreeValue(ctx, global); }\n");
                        } else {
                            try self.write("            { JSValue global = JS_GetGlobalObject(ctx);\n");
                            try self.write("              JSAtom prop = JS_NewAtom(ctx, \"");
                            try self.writeEscapedString(name);
                            try self.write("\");\n");
                            try self.write("              JS_DefinePropertyValue(ctx, global, prop, JS_UNDEFINED, JS_PROP_C_W_E);\n");
                            try self.write("              JS_FreeAtom(ctx, prop);\n");
                            try self.write("              JS_FreeValue(ctx, global); }\n");
                        }
                    }
                }
            },
            // Delete global variable - pushes boolean result
            .delete_var => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        if (self.isZig()) {
                            try self.write("            { const global = qjs.JS_GetGlobalObject(ctx);\n");
                            try self.write("              const prop = qjs.JS_NewAtom(ctx, \"");
                            try self.writeEscapedString(name);
                            try self.write("\");\n");
                            try self.write("              const ret = qjs.JS_DeleteProperty(ctx, global, prop, 0);\n");
                            try self.write("              qjs.JS_FreeAtom(ctx, prop);\n");
                            try self.write("              qjs.JS_FreeValue(ctx, global);\n");
                            try self.write("              if (ret < 0) return qjs.JS_EXCEPTION;\n");
                            try self.write("              const result = qjs.JS_NewBool(ctx, if (ret > 0) 1 else 0);\n");
                            try self.write("              stack[@intCast(sp)] = result; sp += 1; }\n");
                        } else {
                            try self.write("            { JSValue global = JS_GetGlobalObject(ctx);\n");
                            try self.write("              JSAtom prop = JS_NewAtom(ctx, \"");
                            try self.writeEscapedString(name);
                            try self.write("\");\n");
                            try self.write("              int ret = JS_DeleteProperty(ctx, global, prop, 0);\n");
                            try self.write("              JS_FreeAtom(ctx, prop);\n");
                            try self.write("              JS_FreeValue(ctx, global);\n");
                            try self.write("              if (ret < 0) { next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
                            try self.write("              PUSH(JS_NewBool(ctx, ret > 0)); }\n");
                        }
                    } else {
                        if (self.isZig()) {
                            try self.write("            stack[@intCast(sp)] = qjs.JS_TRUE; sp += 1;\n");
                        } else {
                            try self.write("            PUSH(JS_TRUE);\n");
                        }
                    }
                } else {
                    if (self.isZig()) {
                        try self.write("            stack[@intCast(sp)] = qjs.JS_TRUE; sp += 1;\n");
                    } else {
                        try self.write("            PUSH(JS_TRUE);\n");
                    }
                }
            },
            // Delete property: delete obj[key]
            // Stack: [..., obj, key] -> [..., bool]
            .delete => {
                if (self.isZig()) {
                    try self.write("            { const key = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("              const obj = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("              const prop = if (qjs.JS_IsString(key) != 0) blk: {\n");
                    try self.write("                const str = qjs.JS_ToCString(ctx, key);\n");
                    try self.write("                const atom = qjs.JS_NewAtom(ctx, str);\n");
                    try self.write("                qjs.JS_FreeCString(ctx, str);\n");
                    try self.write("                break :blk atom;\n");
                    try self.write("              } else qjs.JS_ValueToAtom(ctx, key);\n");
                    try self.write("              qjs.JS_FreeValue(ctx, key);\n");
                    try self.write("              const ret = qjs.JS_DeleteProperty(ctx, obj, prop, 0);\n");
                    try self.write("              qjs.JS_FreeAtom(ctx, prop);\n");
                    try self.write("              qjs.JS_FreeValue(ctx, obj);\n");
                    try self.write("              if (ret < 0) return qjs.JS_EXCEPTION;\n");
                    try self.write("              const result = qjs.JS_NewBool(ctx, if (ret > 0) 1 else 0);\n");
                    try self.write("              stack[@intCast(sp)] = result; sp += 1; }\n");
                } else {
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
                }
            },

            // Property access - get_field: obj.prop (atom from bytecode)
            .get_field, .get_field2 => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        if (self.isZig()) {
                            try self.write("            { const obj = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                            try self.write("              const val = qjs.JS_GetPropertyStr(ctx, obj, \"");
                            try self.writeEscapedString(name);
                            try self.write("\");\n");
                            try self.write("              qjs.JS_FreeValue(ctx, obj);\n");
                            try self.write("              if (qjs.JS_IsException(val) != 0) return val;\n");
                            try self.write("              stack[@intCast(sp)] = val; sp += 1; }\n");
                        } else {
                            try self.write("            { JSValue obj = POP();\n");
                            try self.write("              JSValue val = JS_GetPropertyStr(ctx, obj, \"");
                            try self.writeEscapedString(name);
                            try self.write("\");\n");
                            try self.write("              FROZEN_FREE(ctx, obj);\n");
                            try self.write("              if (JS_IsException(val)) { next_block = -1; frame->result = val; break; }\n");
                            try self.write("              PUSH(val); }\n");
                        }
                    } else {
                        try self.print("            /* get_field: empty atom at {d} */\n", .{atom_idx});
                        if (self.isZig()) {
                            try self.write("            { const obj = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                            try self.write("              qjs.JS_FreeValue(ctx, obj);\n");
                            try self.write("              stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1; }\n");
                        } else {
                            try self.write("            { JSValue obj = POP(); FROZEN_FREE(ctx, obj); PUSH(JS_UNDEFINED); }\n");
                        }
                    }
                } else {
                    try self.print("            /* get_field: atom {d} out of bounds */\n", .{atom_idx});
                    if (self.isZig()) {
                        try self.write("            { const obj = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                        try self.write("              qjs.JS_FreeValue(ctx, obj);\n");
                        try self.write("              stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1; }\n");
                    } else {
                        try self.write("            { JSValue obj = POP(); FROZEN_FREE(ctx, obj); PUSH(JS_UNDEFINED); }\n");
                    }
                }
            },

            // Put property - set_field: obj.prop = val

            // Method call - call_method: obj.method(args)
            // Stack: [..., func, this, args...] -> [..., result]
            // npop format: u16 operand is argc
            .call_method => {
                const argc = instr.operand.u16;
                if (self.isZig()) {
                    try self.write("            {\n");
                    // Pop args into temp array (reverse order)
                    if (argc > 0) {
                        try self.print("              var args: [{d}]qjs.JSValue = undefined;\n", .{argc});
                        var i = argc;
                        while (i > 0) {
                            i -= 1;
                            try self.print("              args[{d}] = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }};\n", .{i});
                        }
                    }
                    try self.write("              const this_obj = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("              const func = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    if (argc > 0) {
                        try self.print("              const result = qjs.JS_Call(ctx, func, this_obj, {d}, &args);\n", .{argc});
                    } else {
                        try self.write("              const result = qjs.JS_Call(ctx, func, this_obj, 0, null);\n");
                    }
                    try self.write("              qjs.JS_FreeValue(ctx, func);\n");
                    try self.write("              qjs.JS_FreeValue(ctx, this_obj);\n");
                    if (argc > 0) {
                        var j: u16 = 0;
                        while (j < argc) : (j += 1) {
                            try self.print("              qjs.JS_FreeValue(ctx, args[{d}]);\n", .{j});
                        }
                    }
                    try self.write("              if (qjs.JS_IsException(result) != 0) return result;\n");
                    try self.write("              stack[@intCast(sp)] = result; sp += 1;\n");
                    try self.write("            }\n");
                } else {
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
                }
            },

            // TDZ (Temporal Dead Zone) check - get local with check for uninitialized
            // .loc format: u16 operand for local variable index
            .get_loc_check => {
                const idx = instr.operand.loc;
                if (debug) try self.print("            /* get_loc_check {d} */\n", .{idx});
                if (self.isZig()) {
                    try self.print("            {{ const v = locals[{d}];\n", .{idx});
                    try self.write("              if (qjs.JS_IsUninitialized(v) != 0) {\n");
                    try self.write("                return qjs.JS_ThrowReferenceError(ctx, \"Cannot access before initialization\");\n");
                    try self.write("              }\n");
                    try self.write("              const dup = qjs.JS_DupValue(ctx, v);\n");
                    try self.write("              stack[@intCast(sp)] = dup; sp += 1; }\n");
                } else {
                    try self.print("            {{ JSValue v = locals[{d}];\n", .{idx});
                    try self.write("              if (JS_IsUninitialized(v)) {\n");
                    try self.write("                next_block = -1; frame->result = JS_ThrowReferenceError(ctx, \"Cannot access before initialization\"); break;\n");
                    try self.write("              }\n");
                    try self.write("              PUSH(FROZEN_DUP(ctx, v)); }\n");
                }
            },

            // Argument access

            // Integer constants

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
            .@"null" => try self.write("            PUSH(JS_NULL);\n"),
            .not, .lnot => try self.write("            { JSValue a = POP(); PUSH(JS_NewBool(ctx, !JS_ToBool(ctx, a))); FROZEN_FREE(ctx, a); }\n"),
            .get_loc8 => {
                const idx = instr.operand.u8;
                try self.print("            PUSH(FROZEN_DUP(ctx, frame->locals[{d}]));\n", .{idx});
            },
            .get_loc => {
                const idx = instr.operand.u16;
                try self.print("            PUSH(FROZEN_DUP(ctx, frame->locals[{d}]));\n", .{idx});
            },
            .put_loc8 => {
                const idx = instr.operand.u8;
                try self.print("            {{ FROZEN_FREE(ctx, frame->locals[{d}]); frame->locals[{d}] = POP(); }}\n", .{ idx, idx });
            },
            .put_loc => {
                const idx = instr.operand.u16;
                try self.print("            {{ FROZEN_FREE(ctx, frame->locals[{d}]); frame->locals[{d}] = POP(); }}\n", .{ idx, idx });
            },
            .set_loc8 => {
                const idx = instr.operand.u8;
                try self.print("            {{ FROZEN_FREE(ctx, frame->locals[{d}]); frame->locals[{d}] = FROZEN_DUP(ctx, TOP()); }}\n", .{ idx, idx });
            },
            .set_loc => {
                const idx = instr.operand.u16;
                try self.print("            {{ FROZEN_FREE(ctx, frame->locals[{d}]); frame->locals[{d}] = FROZEN_DUP(ctx, TOP()); }}\n", .{ idx, idx });
            },

            // Local variable increment/decrement (loop optimizations)

            // Bitwise operations
            .@"and" => try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) & JS_VALUE_GET_INT(b))); }\n"),
            .@"or" => try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) | JS_VALUE_GET_INT(b))); }\n"),

            // instanceof operator

            // in operator

            // Push atom value (string constant from atom table)
            .push_atom_value => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.options.atom_strings.len) {
                    const name = self.options.atom_strings[atom_idx];
                    if (name.len > 0) {
                        if (self.isZig()) {
                            try self.write("            { const str = qjs.JS_NewString(ctx, \"");
                            try self.writeEscapedString(name);
                            try self.write("\");\n");
                            try self.write("              stack[@intCast(sp)] = str; sp += 1; }\n");
                        } else {
                            try self.write("            PUSH(JS_NewString(ctx, \"");
                            try self.writeEscapedString(name);
                            try self.write("\"));\n");
                        }
                    } else {
                        if (self.isZig()) {
                            try self.write("            { const str = qjs.JS_NewString(ctx, \"\");\n");
                            try self.write("              stack[@intCast(sp)] = str; sp += 1; }\n");
                        } else {
                            try self.write("            PUSH(JS_NewString(ctx, \"\"));\n");
                        }
                    }
                } else {
                    try self.print("            /* push_atom_value: atom {d} out of bounds */\n", .{atom_idx});
                    if (self.isZig()) {
                        try self.write("            stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1;\n");
                    } else {
                        try self.write("            PUSH(JS_UNDEFINED);\n");
                    }
                }
            },

            // Non-recursive function calls in trampoline

            // Constructor call - new Foo(args...)
            .call_constructor => {
                const argc: u16 = instr.operand.u16;
                if (self.isZig()) {
                    try self.write("            {\n");
                    if (argc > 0) {
                        try self.print("              var args: [{d}]qjs.JSValue = undefined;\n", .{argc});
                        var i: u16 = argc;
                        while (i > 0) {
                            i -= 1;
                            try self.print("              args[{d}] = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }};\n", .{i});
                        }
                    }
                    try self.write("              const ctor = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    if (argc > 0) {
                        try self.print("              const ret = qjs.JS_CallConstructor(ctx, ctor, {d}, &args);\n", .{argc});
                    } else {
                        try self.write("              const ret = qjs.JS_CallConstructor(ctx, ctor, 0, null);\n");
                    }
                    try self.write("              qjs.JS_FreeValue(ctx, ctor);\n");
                    if (argc > 0) {
                        var j: u16 = 0;
                        while (j < argc) : (j += 1) {
                            try self.print("              qjs.JS_FreeValue(ctx, args[{d}]);\n", .{j});
                        }
                    }
                    try self.write("              if (qjs.JS_IsException(ret) != 0) return ret;\n");
                    try self.write("              stack[@intCast(sp)] = ret; sp += 1;\n");
                    try self.write("            }\n");
                } else {
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
                }
                self.pending_self_call = false;
            },

            // Method call with tail call optimization
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

            // Convert to object wrapper

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

            // get_super: Get prototype of object (for super.method() calls)

            // regexp: Create RegExp from pattern and flags strings

            // Throw exception
            .throw => {
                if (self.isZig()) {
                    try self.write("            { const exc = { sp -= 1; const val = stack[@intCast(sp)]; val; };\n");
                    try self.write("              _ = qjs.JS_Throw(ctx, exc);\n");
                    try self.write("              return qjs.JS_EXCEPTION; }\n");
                } else {
                    try self.write("            { JSValue exc = POP(); JS_Throw(ctx, exc); next_block = -1; frame->result = JS_EXCEPTION; break; }\n");
                }
            },

            // Catch - push the exception
            .@"catch" => {
                if (self.isZig()) {
                    try self.write("            { const exc = qjs.JS_GetException(ctx);\n");
                    try self.write("              stack[@intCast(sp)] = exc; sp += 1; }\n");
                } else {
                    try self.write("            { JSValue exc = JS_GetException(ctx); PUSH(exc); }\n");
                }
            },

            // put_var_ref_check - closure variable assignment with TDZ check
            .put_var_ref_check => {
                // For frozen functions, just discard the value (closures not supported)
                try self.write("            { FROZEN_FREE(ctx, POP()); }\n");
            },

            // TDZ (Temporal Dead Zone) opcodes for let/const
            .set_loc_uninitialized => {
                const idx = instr.operand.loc;
                if (self.isZig()) {
                    try self.print("            locals[{d}] = qjs.JS_UNINITIALIZED;\n", .{idx});
                } else {
                    try self.print("            frame->locals[{d}] = JS_UNINITIALIZED;\n", .{idx});
                }
            },
            .put_loc_check => {
                const idx = instr.operand.loc;
                if (self.isZig()) {
                    try self.print("            {{ const v = locals[{d}];\n", .{idx});
                    try self.write("              if (qjs.JS_IsUninitialized(v) != 0) {\n");
                    try self.write("                return qjs.JS_ThrowReferenceError(ctx, \"Cannot access before initialization\");\n");
                    try self.write("              }\n");
                    try self.print("              qjs.JS_FreeValue(ctx, locals[{d}]);\n", .{idx});
                    try self.print("              locals[{d}] = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }}; }}\n", .{idx});
                } else {
                    try self.print("            {{ JSValue v = frame->locals[{d}];\n", .{idx});
                    try self.write("              if (JS_IsUninitialized(v)) {\n");
                    try self.write("                next_block = -1; frame->result = JS_ThrowReferenceError(ctx, \"Cannot access before initialization\"); break;\n");
                    try self.write("              }\n");
                    try self.print("              FROZEN_FREE(ctx, frame->locals[{d}]); frame->locals[{d}] = POP(); }}\n", .{ idx, idx });
                }
            },
            .put_loc_check_init => {
                const idx = instr.operand.loc;
                if (self.isZig()) {
                    try self.print("            {{ const v = locals[{d}];\n", .{idx});
                    try self.write("              if (qjs.JS_IsUninitialized(v) == 0) {\n");
                    try self.write("                return qjs.JS_ThrowReferenceError(ctx, \"Identifier already declared\");\n");
                    try self.write("              }\n");
                    try self.print("              locals[{d}] = {{ sp -= 1; const val = stack[@intCast(sp)]; val; }}; }}\n", .{idx});
                } else {
                    try self.print("            {{ JSValue v = frame->locals[{d}];\n", .{idx});
                    try self.write("              if (!JS_IsUninitialized(v)) {\n");
                    try self.write("                next_block = -1; frame->result = JS_ThrowReferenceError(ctx, \"Identifier already declared\"); break;\n");
                    try self.write("              }\n");
                    try self.print("              frame->locals[{d}] = POP(); }}\n", .{idx});
                }
            },
            .get_var_ref_check => {
                // Closure variable with TDZ check - just push undefined (frozen doesn't support closures)
                if (self.isZig()) {
                    try self.write("            stack[@intCast(sp)] = qjs.JS_UNDEFINED; sp += 1;\n");
                } else {
                    try self.write("            PUSH(JS_UNDEFINED);\n");
                }
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

            // Iterator opcodes
            // iterator_check_object: Verify top of stack is an object
            // iterator_get_value_done: Extract value and done from iterator result
            // iterator_next: Call next method on iterator
            // iterator_call: Call throw/return method on iterator

            // Type operators
            .get_length => {
                try self.write("            { JSValue obj = POP(); int64_t len = frozen_get_length(ctx, obj); FROZEN_FREE(ctx, obj); PUSH(JS_NewInt64(ctx, len)); }\n");
            },

            // Reference operations (for with/delete)

            // Simple push/check opcodes
            .get_loc0_loc1 => {
                // Push both loc0 and loc1
                try self.write("            PUSH(FROZEN_DUP(ctx, locals[0]));\n");
                try self.write("            PUSH(FROZEN_DUP(ctx, locals[1]));\n");
            },

            // Private field access

            // More simple opcodes
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

            // define_method: Define a method on an object (for class methods)
            // define_method_computed: Define method with computed name

            // Object spread and apply

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

            // GENERIC TAIL POSITION DETECTION: check if next instruction is return
            const is_in_tail_position = blk: {
                if (instr_idx + 1 < block.instructions.len) {
                    const next_instr = block.instructions[instr_idx + 1];
                    break :blk next_instr.opcode == .@"return" or next_instr.opcode == .return_undef;
                }
                break :blk false;
            };

            const is_recursive_call = (is_call_opcode and self.pending_self_call and self.options.is_self_recursive) or is_definitely_self_tail_call;
            const is_tail_recursive_call = is_recursive_call and (is_definitely_self_tail_call or is_in_tail_position);
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

            // Handle recursive call specially
            if (is_recursive_call) {
                const call_argc: u16 = switch (instr.opcode) {
                    .call0 => 0,
                    .call1 => 1,
                    .call2 => 2,
                    .call3 => 3,
                    .call, .tail_call => instr.operand.u16,
                    else => 1,
                };

                // TAIL POSITION: Optimize to goto loop (no stack growth)
                if (is_tail_recursive_call) {
                    try self.print(
                        \\                /* Tail recursive call {d} ({d} args) - TCO via goto */
                        \\                {{
                        \\
                    , .{ instr_idx, call_argc });

                    // Pop all arguments in reverse order
                    var j: u16 = call_argc;
                    while (j > 0) {
                        j -= 1;
                        try self.print("                    JSValue new_arg{d} = POP();\n", .{j});
                    }

                    // Reassign argv and reset state
                    j = 0;
                    while (j < call_argc) : (j += 1) {
                        try self.print("                    argv[{d}] = new_arg{d};\n", .{ j, j });
                    }

                    try self.print(
                        \\                    argc = {d};
                        \\                    sp = 0;
                        \\                    goto frozen_start;
                        \\                }}
                        \\
                    , .{call_argc});
                } else {
                    // NON-TAIL POSITION: Use trampoline (push frame and save continuation)
                    const actual_var_count = if (self.options.var_count > 0) self.options.var_count else 1;
                    const actual_arg_count = if (self.options.arg_count > 0) self.options.arg_count else 1;

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

        // Try shared opcodes first
        if (try self.emitCommonOpcode(instr, false)) return;

        switch (instr.opcode) {
            // ==================== PUSH CONSTANTS (comptime generated) ====================
            .null => try self.emitPushJSConst("JS_NULL"),

            // ==================== ARGUMENTS ====================
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

            // ==================== LOCALS ====================
            .get_loc, .get_loc8 => {
                const idx = if (instr.opcode == .get_loc8) instr.operand.u8 else instr.operand.u16;
                if (debug) try self.print("    /* get_loc {d} */\n", .{idx});
                if (self.isZig()) {
                    try self.print("    stack[@intCast(sp)] = qjs.FROZEN_DUP(ctx, locals[{d}]); sp += 1;\n", .{idx});
                } else {
                    try self.print("    PUSH(FROZEN_DUP(ctx, locals[{d}]));\n", .{idx});
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
            .nop => {
                if (debug) try self.write("    /* nop - no operation */\n");
            },
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

            // ==================== COMPARISON ====================
            .@"and" => try self.emitBinaryFuncOp("frozen_and"),
            .@"or" => try self.emitBinaryFuncOp("frozen_or"),
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

            // ==================== TYPE COERCION ====================

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

            // ==================== CALLS ====================

            // ==================== CLOSURE REFS ====================
            // For self-recursion, var_ref0 refers to the function itself
            // When is_self_recursive is true, we skip pushing and set pending_self_call
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
                    const call_argc = instr.operand.u16; // Number of arguments

                    if (self.isZig()) {
                        // Pop all arguments and reassign to argv
                        try self.write("    { ");
                        var i = call_argc;
                        while (i > 0) {
                            i -= 1;
                            try self.print("argv[{d}] = {{ sp -= 1; stack[@intCast(sp)]; }}; ", .{i});
                        }
                        try self.print("argc = {d}; sp = 0; goto frozen_start; }}\n", .{call_argc});
                    } else {
                        // C version: pop all arguments and reassign
                        try self.write("    { ");
                        var i = call_argc;
                        while (i > 0) {
                            i -= 1;
                            try self.print("argv[{d}] = POP(); ", .{i});
                        }
                        try self.print("argc = {d}; sp = 0; goto frozen_start; }}\n", .{call_argc});
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

            // ==================== ITERATORS (via QuickJS wrapper functions) ====================

            // ==================== ADVANCED ITERATOR OPERATIONS ====================
            // iterator_check_object: Verify top of stack is an object
            // iterator_get_value_done: Extract value and done from iterator result
            // Stack: result_obj -> value, done
            // iterator_close: Close an iterator
            // Stack: iter_obj, next_method, catch_offset -> (empty, pops 3)
            // iterator_next: Call next method on iterator
            // Stack: iter_obj, next, catch_offset, val -> iter_obj, next, catch_offset, result
            // iterator_call: Call throw/return method on iterator
            // Stack: iter_obj, next, catch_offset, val -> iter_obj, next, catch_offset, result, done

            // Rest parameters: function foo(a, b, ...rest)

            // ==================== ADDITIONAL OPERATORS ====================
            // get_super: Get prototype of object (for super.method() calls)
            // Stack: obj -> prototype
            // regexp: Create RegExp from pattern and flags strings
            // Stack: pattern, flags -> regexp
            // check_ctor: Verify we're called as constructor (new.target exists)
            // check_ctor_return: Check constructor return value
            // Stack: ret_val -> ret_val, this (or just ret_val if ret_val is object)
            // init_ctor: Initialize constructor - create this object
            // Stack: -> this
            // copy_data_properties: Object spread {...obj}
            // Stack: target, source, excludeList -> target, source, excludeList
            // apply: func.apply(this, argsArray)
            // Stack: func, this, argsArray -> result

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
            // define_method_computed: Define method with computed name
            // Stack: obj, prop_key, func -> obj (pops prop_key and func)

            // ==================== REFERENCE OPERATIONS ====================
            // make_var_ref: Create reference to global variable
            // Stack: -> obj, prop_name
            // put_ref_value: Set property value on reference
            // Stack: obj, prop, value -> (empty, pops 3)

            // ==================== PRIVATE FIELDS ====================
            // private_symbol: Create a private symbol from atom
            // Stack: -> private_symbol
            // add_brand: Add brand to object (for private field initialization)
            // Stack: obj, func -> (empty)
            // private_in: Check if private field exists (for #field in obj)
            // Stack: field_sym, obj -> bool

            // Private field access

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

            .put_arg_i32 => {
                // Pop value and store to argument slot (used for tail recursion)
                if (stack.items.len < 1) return error.StackUnderflow;
                const value = stack.pop() orelse return error.StackUnderflow;
                defer self.allocator.free(value);
                try self.print("    n{d} = {s};\n", .{ handler.index.?, value });
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
