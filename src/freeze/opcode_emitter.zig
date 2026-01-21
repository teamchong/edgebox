//! Shared Opcode Emitter for Frozen Code Generation
//!
//! This module provides opcode handling shared between all codegens.
//! Uses comptime generics - any codegen implementing the required methods can use this.
//!
//! Required methods on CodeGen type:
//!   - vpush(expr: []const u8) !void
//!   - vpushFmt(comptime fmt, args) !void
//!   - vpop() ?[]const u8
//!   - vpeek() ?[]const u8
//!   - flushVstack() !void
//!   - writeLine(line: []const u8) !void
//!   - printLine(comptime fmt, args) !void
//!   - getAtomString(atom_idx: u32) ?[]const u8
//!   - escapeString(input: []const u8) []u8
//!   - setTerminated() void
//!   - allocator: Allocator
//!   - isAllocated(expr) bool
//!   - countStackRefs(expr) usize

const std = @import("std");
const opcodes = @import("opcodes.zig");
const parser = @import("bytecode_parser.zig");

const Opcode = opcodes.Opcode;
const Allocator = std.mem.Allocator;
pub const Instruction = parser.Instruction;

/// Emit code for a single opcode using the provided codegen
/// Returns true if handled, false if unsupported (caller should handle)
pub fn emitOpcode(comptime CodeGen: type, self: *CodeGen, instr: Instruction) !bool {
    switch (instr.opcode) {
        // ============================================================
        // Constants
        // ============================================================
        .push_0 => try self.vpush("CV.newInt(0)"),
        .push_1 => try self.vpush("CV.newInt(1)"),
        .push_2 => try self.vpush("CV.newInt(2)"),
        .push_3 => try self.vpush("CV.newInt(3)"),
        .push_4 => try self.vpush("CV.newInt(4)"),
        .push_5 => try self.vpush("CV.newInt(5)"),
        .push_6 => try self.vpush("CV.newInt(6)"),
        .push_7 => try self.vpush("CV.newInt(7)"),
        .push_minus1 => try self.vpush("CV.newInt(-1)"),
        .push_i8 => try self.vpushFmt("CV.newInt({d})", .{instr.operand.i8}),
        .push_i16 => try self.vpushFmt("CV.newInt({d})", .{instr.operand.i16}),
        .push_i32 => try self.vpushFmt("CV.newInt({d})", .{instr.operand.i32}),
        .push_true => try self.vpush("CV.TRUE"),
        .push_false => try self.vpush("CV.FALSE"),
        .null => try self.vpush("CV.NULL"),
        .undefined => try self.vpush("CV.UNDEFINED"),
        .push_empty_string => try self.vpush("CV.fromJSValue(JSValue.newString(ctx, \"\"))"),

        // String/atom value
        .push_atom_value => {
            const atom_idx = instr.operand.atom;
            if (self.getAtomString(atom_idx)) |str_val| {
                const escaped = self.escapeString(str_val);
                defer self.allocator.free(escaped);
                try self.vpushFmt("CV.fromJSValue(JSValue.newString(ctx, \"{s}\"))", .{escaped});
            } else {
                try self.vpush("CV.UNDEFINED");
            }
        },

        // ============================================================
        // Local Variables
        // ============================================================
        .get_loc0 => try self.vpush("locals[0]"),
        .get_loc1 => try self.vpush("locals[1]"),
        .get_loc2 => try self.vpush("locals[2]"),
        .get_loc3 => try self.vpush("locals[3]"),
        .get_loc, .get_loc8 => try self.vpushFmt("locals[{d}]", .{instr.operand.loc}),
        .get_loc0_loc1 => {
            try self.vpush("locals[0]");
            try self.vpush("locals[1]");
        },

        .put_loc0 => try emitPutLocal(CodeGen, self, 0),
        .put_loc1 => try emitPutLocal(CodeGen, self, 1),
        .put_loc2 => try emitPutLocal(CodeGen, self, 2),
        .put_loc3 => try emitPutLocal(CodeGen, self, 3),
        .put_loc, .put_loc8 => try emitPutLocal(CodeGen, self, instr.operand.loc),

        .set_loc0 => try emitSetLocal(CodeGen, self, 0),
        .set_loc1 => try emitSetLocal(CodeGen, self, 1),
        .set_loc2 => try emitSetLocal(CodeGen, self, 2),
        .set_loc3 => try emitSetLocal(CodeGen, self, 3),
        .set_loc, .set_loc8 => try emitSetLocal(CodeGen, self, instr.operand.loc),

        // ============================================================
        // Arguments
        // ============================================================
        .get_arg0 => try self.vpush("(if (0 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[0])) else CV.UNDEFINED)"),
        .get_arg1 => try self.vpush("(if (1 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[1])) else CV.UNDEFINED)"),
        .get_arg2 => try self.vpush("(if (2 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[2])) else CV.UNDEFINED)"),
        .get_arg3 => try self.vpush("(if (3 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[3])) else CV.UNDEFINED)"),
        .get_arg => try self.vpushFmt("(if ({d} < argc) CV.fromJSValue(JSValue.dup(ctx, argv[{d}])) else CV.UNDEFINED)", .{ instr.operand.arg, instr.operand.arg }),

        // ============================================================
        // Arithmetic
        // ============================================================
        .add => try emitBinaryOp(CodeGen, self, "CV.add"),
        .sub => try emitBinaryOp(CodeGen, self, "CV.sub"),
        .mul => try emitBinaryOp(CodeGen, self, "CV.mul"),
        .div => try emitBinaryOp(CodeGen, self, "CV.div"),
        .mod => try emitBinaryOp(CodeGen, self, "CV.mod"),
        .neg => {
            const a = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(a)) self.allocator.free(a);
            try self.vpushFmt("CV.sub(CV.newInt(0), {s})", .{a});
        },
        .plus => {
            const a = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(a)) self.allocator.free(a);
            try self.vpushFmt("CV.toNumber({s})", .{a});
        },

        // ============================================================
        // Bitwise
        // ============================================================
        .@"and" => try emitBinaryOp(CodeGen, self, "CV.band"),
        .@"or" => try emitBinaryOp(CodeGen, self, "CV.bor"),
        .xor => try emitBinaryOp(CodeGen, self, "CV.bxor"),
        .shl => try emitBinaryOp(CodeGen, self, "CV.shl"),
        .sar => try emitBinaryOp(CodeGen, self, "CV.sar"),
        .shr => try emitBinaryOp(CodeGen, self, "CV.shr"),
        .not => {
            const a = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(a)) self.allocator.free(a);
            try self.vpushFmt("CV.bnot({s})", .{a});
        },

        // ============================================================
        // Comparison
        // ============================================================
        .lt => try emitBinaryOp(CodeGen, self, "CV.lt"),
        .lte => try emitBinaryOp(CodeGen, self, "CV.lte"),
        .gt => try emitBinaryOp(CodeGen, self, "CV.gt"),
        .gte => try emitBinaryOp(CodeGen, self, "CV.gte"),
        .eq => try emitBinaryOp(CodeGen, self, "CV.eq"),
        .neq => try emitBinaryOp(CodeGen, self, "CV.neq"),
        .strict_eq => try emitBinaryOp(CodeGen, self, "CV.strictEq"),
        .strict_neq => try emitBinaryOp(CodeGen, self, "CV.strictNeq"),

        // ============================================================
        // Increment/Decrement
        // ============================================================
        .inc_loc => try self.printLine("locals[{d}] = CV.add(locals[{d}], CV.newInt(1));", .{ instr.operand.loc, instr.operand.loc }),
        .dec_loc => try self.printLine("locals[{d}] = CV.sub(locals[{d}], CV.newInt(1));", .{ instr.operand.loc, instr.operand.loc }),

        // ============================================================
        // Stack Operations
        // ============================================================
        .dup => {
            if (self.vpeek()) |top| {
                try self.vpushFmt("{s}", .{top});
            } else {
                try self.vpush("stack[sp-1]");
            }
        },
        .drop => {
            _ = self.vpop();
            try self.writeLine("sp -= 1;");
        },
        .nip => {
            try self.flushVstack();
            try self.writeLine("stack[sp-2] = stack[sp-1]; sp -= 1;");
        },
        .swap => {
            try self.flushVstack();
            try self.writeLine("{ const tmp = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = tmp; }");
        },

        // ============================================================
        // Property Access
        // ============================================================
        .get_field, .get_field2 => {
            const atom_idx = instr.operand.atom;
            if (self.getAtomString(atom_idx)) |prop_name| {
                const obj = self.vpop() orelse "stack[sp-1]";
                defer if (self.isAllocated(obj)) self.allocator.free(obj);
                try self.vpushFmt("CV.fromJSValue(JSValue.getField(ctx, {s}.toJSValue(), \"{s}\"))", .{ obj, prop_name });
            } else {
                return false;
            }
        },
        .put_field => {
            const atom_idx = instr.operand.atom;
            if (self.getAtomString(atom_idx)) |prop_name| {
                const val = self.vpop() orelse "stack[sp-1]";
                defer if (self.isAllocated(val)) self.allocator.free(val);
                const obj = self.vpop() orelse "stack[sp-2]";
                defer if (self.isAllocated(obj)) self.allocator.free(obj);
                try self.printLine("_ = JSValue.setField(ctx, {s}.toJSValue(), \"{s}\", {s}.toJSValue());", .{ obj, prop_name, val });
            } else {
                return false;
            }
        },
        .get_array_el => {
            const idx = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(idx)) self.allocator.free(idx);
            const arr = self.vpop() orelse "stack[sp-2]";
            defer if (self.isAllocated(arr)) self.allocator.free(arr);
            try self.vpushFmt("CV.fromJSValue(JSValue.getIndex(ctx, {s}.toJSValue(), @intCast({s}.toInt32())))", .{ arr, idx });
        },
        .put_array_el => {
            const val = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(val)) self.allocator.free(val);
            const idx = self.vpop() orelse "stack[sp-2]";
            defer if (self.isAllocated(idx)) self.allocator.free(idx);
            const arr = self.vpop() orelse "stack[sp-3]";
            defer if (self.isAllocated(arr)) self.allocator.free(arr);
            try self.printLine("_ = JSValue.setIndex(ctx, {s}.toJSValue(), @intCast({s}.toInt32()), {s}.toJSValue());", .{ arr, idx, val });
        },

        // ============================================================
        // Global Variables
        // ============================================================
        .get_var => {
            const atom_idx = instr.operand.atom;
            if (self.getAtomString(atom_idx)) |var_name| {
                try self.vpushFmt("CV.fromJSValue(JSValue.getGlobal(ctx, \"{s}\"))", .{var_name});
            } else {
                return false;
            }
        },
        .put_var => {
            const atom_idx = instr.operand.atom;
            if (self.getAtomString(atom_idx)) |var_name| {
                const val = self.vpop() orelse "stack[sp-1]";
                defer if (self.isAllocated(val)) self.allocator.free(val);
                try self.printLine("_ = JSValue.setGlobal(ctx, \"{s}\", {s}.toJSValue());", .{ var_name, val });
            } else {
                return false;
            }
        },

        // ============================================================
        // Closure Variables
        // ============================================================
        .get_var_ref, .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3 => {
            const var_idx = switch (instr.opcode) {
                .get_var_ref0 => @as(u16, 0),
                .get_var_ref1 => 1,
                .get_var_ref2 => 2,
                .get_var_ref3 => 3,
                else => instr.operand.var_ref,
            };
            try self.vpushFmt("(if (var_refs) |vr| CV.fromJSValue(zig_runtime.getVarRef(vr[{d}])) else CV.UNDEFINED)", .{var_idx});
        },
        .put_var_ref, .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3 => {
            const var_idx = switch (instr.opcode) {
                .put_var_ref0 => @as(u16, 0),
                .put_var_ref1 => 1,
                .put_var_ref2 => 2,
                .put_var_ref3 => 3,
                else => instr.operand.var_ref,
            };
            const val = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(val)) self.allocator.free(val);
            try self.printLine("if (var_refs) |vrs| zig_runtime.setClosureVar(ctx, @ptrCast(vrs), {d}, {s}.toJSValue());", .{ var_idx, val });
        },

        // ============================================================
        // Object/Array Creation
        // ============================================================
        .object => try self.vpush("CV.fromJSValue(zig_runtime.jsNewObject(ctx))"),
        .array_from => {
            const count = instr.operand.u16;
            try self.flushVstack();
            try self.printLine("{{ const _arr = JSValue.newArray(ctx); var _i: usize = 0; while (_i < {d}) : (_i += 1) {{ _ = JSValue.setIndex(ctx, _arr, @intCast(_i), stack[sp - {d} + _i].toJSValue()); }} sp -= {d}; stack[sp] = CV.fromJSValue(_arr); sp += 1; }}", .{ count, count, count });
        },

        // ============================================================
        // This
        // ============================================================
        // For functions with explicit "use strict" directive: keep this as-is
        // For regular functions: coerce undefined/null to globalThis (Node.js non-strict mode behavior)
        .push_this => {
            if (self.func.has_use_strict) {
                // Strict mode: keep this as-is (don't coerce undefined/null)
                try self.vpush("CV.fromJSValue(this_val)");
            } else {
                // Non-strict: coerce undefined/null to globalThis
                try self.vpush("CV.fromJSValue(if (this_val.isUndefined() or this_val.isNull()) JSValue.getGlobal(ctx, \"globalThis\") else this_val)");
            }
        },

        // ============================================================
        // Logical
        // ============================================================
        .lnot => {
            const a = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(a)) self.allocator.free(a);
            // CV.toBool() handles all value types correctly (truthy/falsy conversion)
            try self.vpushFmt("(if (({s}).toBool()) CV.FALSE else CV.TRUE)", .{a});
        },

        // ============================================================
        // Type Checks
        // ============================================================
        .typeof => {
            const val = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(val)) self.allocator.free(val);
            try self.vpushFmt("CV.fromJSValue(JSValue.typeofValue(ctx, {s}.toJSValue()))", .{val});
        },
        .instanceof => {
            const ctor = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(ctor)) self.allocator.free(ctor);
            const obj = self.vpop() orelse "stack[sp-2]";
            defer if (self.isAllocated(obj)) self.allocator.free(obj);
            try self.vpushFmt("(if (JSValue.instanceof(ctx, {s}.toJSValue(), {s}.toJSValue())) CV.TRUE else CV.FALSE)", .{ obj, ctor });
        },
        .is_undefined => {
            const val = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(val)) self.allocator.free(val);
            try self.vpushFmt("(if ({s}.isUndefined()) CV.TRUE else CV.FALSE)", .{val});
        },
        .is_null => {
            const val = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(val)) self.allocator.free(val);
            try self.vpushFmt("(if ({s}.isNull()) CV.TRUE else CV.FALSE)", .{val});
        },
        .is_undefined_or_null => {
            const val = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(val)) self.allocator.free(val);
            try self.vpushFmt("(if ({s}.isUndefined() or {s}.isNull()) CV.TRUE else CV.FALSE)", .{ val, val });
        },

        // ============================================================
        // Increment/Decrement (standalone)
        // ============================================================
        .inc => {
            const a = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(a)) self.allocator.free(a);
            try self.vpushFmt("CV.add({s}, CV.newInt(1))", .{a});
        },
        .dec => {
            const a = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(a)) self.allocator.free(a);
            try self.vpushFmt("CV.sub({s}, CV.newInt(1))", .{a});
        },
        .post_inc => {
            // Post-increment: push current value, then increment
            try self.flushVstack();
            try self.writeLine("{ const _v = stack[sp - 1]; stack[sp] = _v; stack[sp - 1] = CV.add(_v, CV.newInt(1)); sp += 1; }");
            try self.vpush("stack[sp - 2]"); // Original value is now at sp-2
        },
        .post_dec => {
            // Post-decrement: push current value, then decrement
            try self.flushVstack();
            try self.writeLine("{ const _v = stack[sp - 1]; stack[sp] = _v; stack[sp - 1] = CV.sub(_v, CV.newInt(1)); sp += 1; }");
            try self.vpush("stack[sp - 2]");
        },

        // ============================================================
        // Length Access
        // ============================================================
        .get_length => {
            const obj = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(obj)) self.allocator.free(obj);
            try self.vpushFmt("CV.fromJSValue(zig_runtime.nativeGetLength(ctx, ({s}).toJSValue()))", .{obj});
        },

        // ============================================================
        // get_array_el2 - pop idx, pop arr, push arr, push arr[idx]
        // ============================================================
        .get_array_el2 => {
            try self.flushVstack();
            try self.writeLine("{ const idx = stack[sp-1]; const arr = stack[sp-2]; var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, idx.toJSValue()); stack[sp-1] = CV.fromJSValue(JSValue.getPropertyUint32(ctx, arr.toJSValue(), @intCast(idx_i32))); }");
            try self.vpush("stack[sp - 2]");
            try self.vpush("stack[sp - 1]");
        },

        // ============================================================
        // Stack Rotations and Permutations
        // ============================================================
        .rot3l => {
            try self.flushVstack();
            try self.writeLine("{ const t = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = t; }");
        },
        .rot3r => {
            try self.flushVstack();
            try self.writeLine("{ const t = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = stack[sp-3]; stack[sp-3] = t; }");
        },
        .rot4l => {
            try self.flushVstack();
            try self.writeLine("{ const t = stack[sp-4]; stack[sp-4] = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = t; }");
        },
        .rot5l => {
            try self.flushVstack();
            try self.writeLine("{ const t = stack[sp-5]; stack[sp-5] = stack[sp-4]; stack[sp-4] = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = t; }");
        },
        .perm3 => {
            try self.flushVstack();
            try self.writeLine("{ const t = stack[sp-2]; stack[sp-2] = stack[sp-3]; stack[sp-3] = t; }");
        },
        .perm4 => {
            try self.flushVstack();
            try self.writeLine("{ const t = stack[sp-2]; stack[sp-2] = stack[sp-4]; stack[sp-4] = t; }");
        },
        .perm5 => {
            try self.flushVstack();
            try self.writeLine("{ const t = stack[sp-2]; stack[sp-2] = stack[sp-5]; stack[sp-5] = t; }");
        },
        .insert2 => {
            try self.flushVstack();
            try self.writeLine("{ const t = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = t; stack[sp] = t; sp += 1; }");
        },
        .insert3 => {
            try self.flushVstack();
            try self.writeLine("{ const t = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = stack[sp-3]; stack[sp-3] = t; stack[sp] = t; sp += 1; }");
        },
        .insert4 => {
            try self.flushVstack();
            try self.writeLine("{ const t = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = stack[sp-3]; stack[sp-3] = stack[sp-4]; stack[sp-4] = t; stack[sp] = t; sp += 1; }");
        },

        // ============================================================
        // Exception Handling
        // ============================================================
        .throw => {
            try self.flushVstack();
            try self.writeLine("{ const exc = stack[sp-1]; sp -= 1; return JSValue.throw(ctx, exc.toJSValue()); }");
            self.block_terminated = true;
        },

        // ============================================================
        // Type Checks
        // ============================================================
        .typeof_is_function => {
            try self.flushVstack();
            try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = if (JSValue.isFunction(ctx, v.toJSValue())) CV.TRUE else CV.FALSE; }");
        },
        .typeof_is_undefined => {
            try self.flushVstack();
            try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = if (v.isUndefined()) CV.TRUE else CV.FALSE; }");
        },

        // ============================================================
        // Object Conversion
        // ============================================================
        .to_object => {
            try self.flushVstack();
            try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = CV.fromJSValue(JSValue.toObject(ctx, v.toJSValue())); }");
        },
        .to_propkey => {
            try self.flushVstack();
            try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = CV.fromJSValue(JSValue.toPropKey(ctx, v.toJSValue())); }");
        },
        .to_propkey2 => {
            try self.flushVstack();
            try self.writeLine("{ const v = stack[sp-1]; stack[sp] = CV.fromJSValue(JSValue.toPropKey(ctx, v.toJSValue())); sp += 1; }");
        },

        // ============================================================
        // Define Operations
        // ============================================================
        .define_array_el => {
            try self.flushVstack();
            // Stack: arr, idx, val - define element without coercing index
            try self.writeLine("{ const val = stack[sp-1]; const idx = stack[sp-2]; const arr = stack[sp-3]; var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, idx.toJSValue()); _ = JSValue.definePropertyUint32(ctx, arr.toJSValue(), @intCast(idx_i32), val.toJSValue()); sp -= 2; }");
        },
        .define_field => {
            const atom_idx = instr.operand.atom;
            if (self.getAtomString(atom_idx)) |prop_name| {
                try self.flushVstack();
                const escaped = self.escapeString(prop_name);
                defer self.allocator.free(escaped);
                try self.printLine("{{ const val = stack[sp-1]; const obj = stack[sp-2]; _ = JSValue.definePropertyStr(ctx, obj.toJSValue(), \"{s}\", val.toJSValue()); sp -= 1; }}", .{escaped});
            } else {
                return false;
            }
        },

        // ============================================================
        // Append (for array spread)
        // ============================================================
        .append => {
            try self.flushVstack();
            try self.writeLine("{");
            try self.writeLine("    const val = stack[sp-1].toJSValue();");
            try self.writeLine("    const arr = stack[sp-2].toJSValue();");
            try self.writeLine("    var len: i64 = 0;");
            try self.writeLine("    _ = JSValue.getLength(ctx, &len, arr);");
            try self.writeLine("    _ = JSValue.definePropertyUint32(ctx, arr, @intCast(len), val);");
            try self.writeLine("    sp -= 1;");
            try self.writeLine("}");
        },

        // ============================================================
        // Rest Parameter
        // ============================================================
        .rest => {
            const first_arg = instr.operand.u16;
            try self.flushVstack();
            try self.printLine("stack[sp] = CV.fromJSValue(zig_runtime.makeRestArray(ctx, argc, argv, {d})); sp += 1;", .{first_arg});
        },

        // ============================================================
        // Special Object
        // ============================================================
        .special_object => {
            try self.flushVstack();
            try self.writeLine("stack[sp] = CV.fromJSValue(JSValue.newObject(ctx)); sp += 1;");
        },

        // ============================================================
        // Constructor Checks
        // ============================================================
        .check_ctor => {
            try self.flushVstack();
            try self.writeLine("if (!zig_runtime.isConstructorCall(this_val)) return JSValue.throwTypeError(ctx, \"Constructor requires 'new'\");");
        },
        .check_ctor_return => {
            try self.flushVstack();
            try self.writeLine("if (stack[sp-1].isUndefined()) { sp -= 1; stack[sp] = CV.fromJSValue(this_val); sp += 1; }");
        },

        // ============================================================
        // Control Flow - caller handles these
        // ============================================================
        .if_false, .if_false8, .if_true, .if_true8, .goto, .goto8, .goto16 => {
            return false;
        },

        // ============================================================
        // Return
        // ============================================================
        .@"return" => {
            try self.flushVstack();
            try self.writeLine("return stack[sp - 1].toJSValue();");
            self.block_terminated = true;
        },
        .return_undef => {
            try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
            self.block_terminated = true;
        },

        // ============================================================
        // Tail Calls - emit as regular calls for now
        // ============================================================
        .tail_call => {
            // Same as call - tail call optimization handled at a higher level
            return false;
        },
        .tail_call_method => {
            // Same as call_method - tail call optimization handled at a higher level
            return false;
        },

        // ============================================================
        // Closures
        // ============================================================
        .fclosure8 => {
            // Create closure - requires special handling by caller
            return false;
        },
        .close_loc => {
            // Close local for closure - no-op in most cases
            const _loc_idx = instr.operand.loc;
            _ = _loc_idx;
            // No-op - closure vars handled differently in frozen code
        },

        // ============================================================
        // Iterator Operations (for-of, for-in)
        // ============================================================
        .for_of_start, .for_of_next, .for_in_start, .for_in_next,
        .iterator_close, .iterator_get_value_done => {
            // Complex iterator operations - caller handles
            return false;
        },

        // ============================================================
        // Apply (Function.prototype.apply)
        // ============================================================
        .apply => {
            // Complex - requires argument array unpacking
            return false;
        },

        // ============================================================
        // Copy Data Properties (spread operator)
        // ============================================================
        .copy_data_properties => {
            // Complex - caller handles
            return false;
        },

        // ============================================================
        // Function Calls - caller handles for optimization
        // ============================================================
        .call, .call0, .call1, .call2, .call3,
        .call_method, .call_constructor => {
            return false;
        },

        // ============================================================
        // Local Variable Checks (TDZ)
        // ============================================================
        .get_loc_check => {
            const loc = instr.operand.loc;
            try self.printLine("{{ const v = locals[{d}]; if (v.isUninitialized()) return JSValue.throwReferenceError(ctx, \"Cannot access before initialization\"); stack[sp] = CV.fromJSValue(JSValue.dup(ctx, v.toJSValue())); sp += 1; }}", .{loc});
            try self.vpush("stack[sp - 1]");
        },
        .put_loc_check => {
            const loc = instr.operand.loc;
            try self.printLine("{{ const v = locals[{d}]; if (v.isUninitialized()) return JSValue.throwReferenceError(ctx, \"Cannot access before initialization\"); const old = locals[{d}]; if (old.isRefType()) JSValue.free(ctx, old.toJSValue()); locals[{d}] = stack[sp - 1]; sp -= 1; }}", .{ loc, loc, loc });
            _ = self.vpop();
        },
        .put_loc_check_init => {
            const loc = instr.operand.loc;
            try self.printLine("{{ const old = locals[{d}]; if (old.isRefType()) JSValue.free(ctx, old.toJSValue()); locals[{d}] = stack[sp - 1]; sp -= 1; }}", .{ loc, loc });
            _ = self.vpop();
        },
        .set_loc_uninitialized => {
            const loc = instr.operand.loc;
            try self.printLine("locals[{d}] = CV.UNINITIALIZED;", .{loc});
        },

        // ============================================================
        // Define Method
        // ============================================================
        .define_method => {
            // Complex - requires function object creation
            return false;
        },

        // ============================================================
        // Set Name (for function.name property)
        // ============================================================
        .set_name => {
            // Usually a no-op in frozen code
            const _atom_idx = instr.operand.atom;
            _ = _atom_idx;
        },

        // ============================================================
        // Get Variable with Undefined Check
        // ============================================================
        .get_var_undef => {
            const atom_idx = instr.operand.atom;
            if (self.getAtomString(atom_idx)) |var_name| {
                try self.vpushFmt("CV.fromJSValue(JSValue.getGlobalUndef(ctx, \"{s}\"))", .{var_name});
            } else {
                try self.vpush("CV.UNDEFINED");
            }
        },

        // ============================================================
        // Push Constants
        // ============================================================
        .push_const8 => {
            // Constant from constant pool - would need constant pool access
            try self.vpush("CV.UNDEFINED");
        },
        .push_const => {
            // Constant from constant pool - would need constant pool access
            try self.vpush("CV.UNDEFINED");
        },

        // ============================================================
        // No-op
        // ============================================================
        .nop => {},

        // Unsupported - caller handles
        else => return false,
    }
    return true;
}

fn emitBinaryOp(comptime CodeGen: type, self: *CodeGen, op: []const u8) !void {
    const b = self.vpop() orelse "stack[sp-1]";
    const free_b = self.isAllocated(b);
    defer if (free_b) self.allocator.free(b);

    const a = self.vpop() orelse "stack[sp-2]";
    const free_a = self.isAllocated(a);
    defer if (free_a) self.allocator.free(a);

    try self.vpushFmt("{s}({s}, {s})", .{ op, a, b });
}

fn emitPutLocal(comptime CodeGen: type, self: *CodeGen, loc: u32) !void {
    if (self.vpop()) |expr| {
        try self.printLine("locals[{d}] = {s};", .{ loc, expr });
        const ref_count = self.countStackRefs(expr);
        if (ref_count > 0) try self.printLine("sp -= {d};", .{ref_count});
        if (self.isAllocated(expr)) self.allocator.free(expr);
    } else {
        try self.printLine("locals[{d}] = stack[sp - 1]; sp -= 1;", .{loc});
    }
}

fn emitSetLocal(comptime CodeGen: type, self: *CodeGen, loc: u32) !void {
    if (self.vpeek()) |expr| {
        try self.printLine("locals[{d}] = {s};", .{ loc, expr });
    } else {
        try self.printLine("locals[{d}] = stack[sp - 1];", .{loc});
    }
}
