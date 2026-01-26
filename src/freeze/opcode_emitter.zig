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
        // Arguments - handled by each codegen (needs has_put_arg check)
        // ============================================================
        .get_arg0, .get_arg1, .get_arg2, .get_arg3, .get_arg => return false,

        // ============================================================
        // Arithmetic
        // ============================================================
        .add => {
            // Use addWithCtx for proper JS + semantics (string concat or numeric add)
            const b = self.vpop() orelse "stack[sp-1]";
            const free_b = self.isAllocated(b);
            defer if (free_b) self.allocator.free(b);
            const a = self.vpop() orelse "stack[sp-2]";
            const free_a = self.isAllocated(a);
            defer if (free_a) self.allocator.free(a);
            try self.vpushFmt("CV.addWithCtx(ctx, {s}, {s})", .{ a, b });
        },
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
        // Use context-aware equality for proper string/object comparison
        .eq => try emitBinaryOpWithCtx(CodeGen, self, "CV.eqWithCtx"),
        .neq => try emitBinaryOpWithCtx(CodeGen, self, "CV.neqWithCtx"),
        .strict_eq => try emitBinaryOpWithCtx(CodeGen, self, "zig_runtime.strictEqWithCtx"),
        .strict_neq => try emitBinaryOpWithCtx(CodeGen, self, "zig_runtime.strictNeqWithCtx"),

        // ============================================================
        // Increment/Decrement
        // ============================================================
        .inc_loc => {
            // FIX: Before modifying a local, flush vstack entries that reference it.
            // This prevents stale references when bytecode has patterns like:
            // get_loc N, inc, dup, put_loc N (where a later get_loc N would see old vstack)
            const loc = instr.operand.loc;
            try flushVstackIfReferencesLocal(CodeGen, self, loc);
            try self.printLine("locals[{d}] = CV.add(locals[{d}], CV.newInt(1));", .{ loc, loc });
            // Track for pre-increment pattern: inc_loc N, get_loc N, inc
            if (@hasField(CodeGen, "last_inc_loc")) {
                self.last_inc_loc = loc;
            }
        },
        .dec_loc => {
            const loc = instr.operand.loc;
            try flushVstackIfReferencesLocal(CodeGen, self, loc);
            try self.printLine("locals[{d}] = CV.sub(locals[{d}], CV.newInt(1));", .{ loc, loc });
            // Track for pre-decrement pattern: dec_loc N, get_loc N, dec
            if (@hasField(CodeGen, "last_dec_loc")) {
                self.last_dec_loc = loc;
            }
        },

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
        // dup1: [a, b] -> [a, b, a] - insert copy of second item at top
        .dup1 => {
            try self.flushVstack();
            try self.writeLine("{");
            try self.writeLine("    const a = stack[sp - 2];");
            try self.writeLine("    const b = stack[sp - 1];");
            try self.writeLine("    stack[sp - 1] = if (a.isRefType()) CV.fromJSValue(JSValue.dup(ctx, a.toJSValueWithCtx(ctx))) else a;");
            try self.writeLine("    stack[sp] = b;");
            try self.writeLine("    sp += 1;");
            try self.writeLine("}");
        },
        // dup2: [a, b] -> [a, b, a, b] - duplicate top 2 items
        .dup2 => {
            try self.flushVstack();
            try self.writeLine("{");
            try self.writeLine("    const a = stack[sp - 2];");
            try self.writeLine("    const b = stack[sp - 1];");
            try self.writeLine("    stack[sp] = if (a.isRefType()) CV.fromJSValue(JSValue.dup(ctx, a.toJSValueWithCtx(ctx))) else a;");
            try self.writeLine("    stack[sp + 1] = if (b.isRefType()) CV.fromJSValue(JSValue.dup(ctx, b.toJSValueWithCtx(ctx))) else b;");
            try self.writeLine("    sp += 2;");
            try self.writeLine("}");
        },
        // dup3: [a, b, c] -> [a, b, c, a, b, c] - duplicate top 3 items
        .dup3 => {
            try self.flushVstack();
            try self.writeLine("{");
            try self.writeLine("    const a = stack[sp - 3];");
            try self.writeLine("    const b = stack[sp - 2];");
            try self.writeLine("    const c = stack[sp - 1];");
            try self.writeLine("    stack[sp] = if (a.isRefType()) CV.fromJSValue(JSValue.dup(ctx, a.toJSValueWithCtx(ctx))) else a;");
            try self.writeLine("    stack[sp + 1] = if (b.isRefType()) CV.fromJSValue(JSValue.dup(ctx, b.toJSValueWithCtx(ctx))) else b;");
            try self.writeLine("    stack[sp + 2] = if (c.isRefType()) CV.fromJSValue(JSValue.dup(ctx, c.toJSValueWithCtx(ctx))) else c;");
            try self.writeLine("    sp += 3;");
            try self.writeLine("}");
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
        .get_field => {
            const atom_idx = instr.operand.atom;
            if (self.getAtomString(atom_idx)) |prop_name| {
                const obj = self.vpop() orelse "stack[sp-1]";
                defer if (self.isAllocated(obj)) self.allocator.free(obj);
                try self.vpushFmt("CV.fromJSValue(JSValue.getField(ctx, {s}.toJSValueWithCtx(ctx), \"{s}\"))", .{ obj, prop_name });
            } else {
                return false;
            }
        },
        .get_field2 => {
            // get_field2: pops object, pushes object AND property value (for method calls)
            // Stack: [obj] -> [obj, method]
            const atom_idx = instr.operand.atom;
            if (self.getAtomString(atom_idx)) |prop_name| {
                const obj = self.vpop() orelse "stack[sp-1]";
                // Push the object back first (for use as 'this' in call_method)
                try self.vpush(obj);
                // Then push the method/property
                try self.vpushFmt("CV.fromJSValue(JSValue.getField(ctx, {s}.toJSValueWithCtx(ctx), \"{s}\"))", .{ obj, prop_name });
                // Free the obj string after both vpushes if it was allocated
                if (self.isAllocated(obj)) self.allocator.free(obj);
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
                // CRITICAL: Dup value before passing to setField - JS_SetProperty consumes the value
                try self.printLine("_ = JSValue.setField(ctx, {s}.toJSValueWithCtx(ctx), \"{s}\", JSValue.dup(ctx, {s}.toJSValueWithCtx(ctx)));", .{ obj, prop_name, val });
            } else {
                return false;
            }
        },
        .get_array_el => {
            const idx = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(idx)) self.allocator.free(idx);
            const arr = self.vpop() orelse "stack[sp-2]";
            defer if (self.isAllocated(arr)) self.allocator.free(arr);
            // Use getPropertyValue which handles both integer and string keys
            try self.vpushFmt("CV.fromJSValue(JSValue.getPropertyValue(ctx, {s}.toJSValueWithCtx(ctx), JSValue.dup(ctx, {s}.toJSValueWithCtx(ctx))))", .{ arr, idx });
        },
        .put_array_el => {
            // For compound assignment like arr[idx] *= func(), the stack layout is:
            // [arr, idx, old_val, new_operand] where val = old_val * new_operand
            // The val expression (from vstack) may reference stack positions for old_val and new_operand.
            // arr and idx must be positioned BELOW those references.
            const val_from_vstack = self.vpop();
            const val = val_from_vstack orelse "stack[sp-1]";
            defer if (self.isAllocated(val)) self.allocator.free(val);

            // Count how many stack positions val's expression uses
            const val_stack_refs = self.countStackRefs(val);
            // If val came from real stack (not vstack expression), it also counts as 1 ref
            const val_consumes: usize = if (val_from_vstack == null) 1 else val_stack_refs;

            const idx_from_vstack = self.vpop();
            // idx is positioned after val's consumed positions
            const idx = idx_from_vstack orelse switch (val_consumes) {
                0 => "stack[sp-1]",
                1 => "stack[sp-2]",
                2 => "stack[sp-3]",
                else => "stack[sp-4]",
            };
            defer if (self.isAllocated(idx)) self.allocator.free(idx);
            const idx_consumes: usize = if (idx_from_vstack == null) 1 else 0;

            const arr_from_vstack = self.vpop();
            // arr is positioned after val's AND idx's consumed positions
            const total_before_arr = val_consumes + idx_consumes;
            const arr = arr_from_vstack orelse switch (total_before_arr) {
                0 => "stack[sp-1]",
                1 => "stack[sp-2]",
                2 => "stack[sp-3]",
                3 => "stack[sp-4]",
                else => "stack[sp-5]",
            };
            defer if (self.isAllocated(arr)) self.allocator.free(arr);
            const arr_consumes: usize = if (arr_from_vstack == null) 1 else 0;

            // Total stack values consumed
            const total_consumed = val_consumes + idx_consumes + arr_consumes;

            // Use atom-based JS_SetProperty which handles both integer and string keys
            // CRITICAL: Dup value before passing to JS_SetProperty - it consumes the value
            try self.printLine("{{ const arr_jsv = {s}.toJSValueWithCtx(ctx); const idx_jsv = {s}.toJSValueWithCtx(ctx);", .{ arr, idx });
            try self.writeLine("  const atom = zig_runtime.quickjs.JS_ValueToAtom(ctx, idx_jsv);");
            try self.printLine("  _ = zig_runtime.quickjs.JS_SetProperty(ctx, arr_jsv, atom, JSValue.dup(ctx, {s}.toJSValueWithCtx(ctx)));", .{val});
            // Emit sp decrement for values consumed from real stack
            if (total_consumed > 0) {
                try self.printLine("  zig_runtime.quickjs.JS_FreeAtom(ctx, atom); sp -= {d}; }}", .{total_consumed});
            } else {
                try self.writeLine("  zig_runtime.quickjs.JS_FreeAtom(ctx, atom); }");
            }
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
                try self.printLine("_ = JSValue.setGlobal(ctx, \"{s}\", {s}.toJSValueWithCtx(ctx));", .{ var_name, val });
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
            // Map bytecode index to position in var_refs array.
            // The bytecode index refers to the Nth closure variable in the function's closure,
            // but var_refs only contains the subset of closure variables actually captured.
            // We need to find where var_idx appears in closure_var_indices to get the array position.
            const cv_pos = findClosureVarPosition(CodeGen, self, var_idx);
            // Use getClosureVar (C FFI) instead of getVarRef because JSVarRef struct
            // layout in C doesn't match Zig's extern struct due to GC header union alignment
            try self.vpushFmt("CV.fromJSValue(zig_runtime.getClosureVar(ctx, @ptrCast(var_refs), {d}))", .{cv_pos});
        },
        .put_var_ref, .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3 => {
            const var_idx = switch (instr.opcode) {
                .put_var_ref0 => @as(u16, 0),
                .put_var_ref1 => 1,
                .put_var_ref2 => 2,
                .put_var_ref3 => 3,
                else => instr.operand.var_ref,
            };
            // Map bytecode index to position in var_refs array (see get_var_ref comment)
            const cv_pos = findClosureVarPosition(CodeGen, self, var_idx);
            const val = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(val)) self.allocator.free(val);
            try self.printLine("if (var_refs) |vrs| zig_runtime.setClosureVar(ctx, @ptrCast(vrs), {d}, {s}.toJSValueWithCtx(ctx));", .{ cv_pos, val });
        },

        // ============================================================
        // Object/Array Creation
        // ============================================================
        .object => try self.vpush("CV.fromJSValue(zig_runtime.jsNewObject(ctx))"),
        .array_from => {
            const count = instr.operand.u16;
            try self.flushVstack();
            // CRITICAL: Dup value before passing to setIndex - it consumes the value
            try self.printLine("{{ const _arr = JSValue.newArray(ctx); var _i: usize = 0; while (_i < {d}) : (_i += 1) {{ _ = JSValue.setIndex(ctx, _arr, @intCast(_i), JSValue.dup(ctx, stack[sp - {d} + _i].toJSValueWithCtx(ctx))); }} sp -= {d}; stack[sp] = CV.fromJSValue(_arr); sp += 1; }}", .{ count, count, count });
        },

        // ============================================================
        // This
        // ============================================================
        // For functions with explicit "use strict" directive: keep this as-is
        // For regular functions: coerce undefined/null to globalThis (Node.js non-strict mode behavior)
        // CRITICAL: Must dup this_val because:
        // 1. getGlobal returns a new reference (refcount=1)
        // 2. this_val is a borrowed reference that must be duped for ownership
        // Without dup, freeing the pushed value corrupts the caller's this_val
        .push_this => {
            if (self.func.has_use_strict) {
                // Strict mode: keep this as-is (don't coerce undefined/null)
                try self.vpush("CV.fromJSValue(JSValue.dup(ctx, this_val))");
            } else {
                // Non-strict: coerce undefined/null to globalThis
                try self.vpush("CV.fromJSValue(if (this_val.isUndefined() or this_val.isNull()) JSValue.getGlobal(ctx, \"globalThis\") else JSValue.dup(ctx, this_val))");
            }
        },

        // ============================================================
        // Logical
        // ============================================================
        .lnot => {
            const a = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(a)) self.allocator.free(a);
            // CV.toBoolWithCtx() handles all value types correctly (truthy/falsy conversion)
            // including empty strings which are falsy
            try self.vpushFmt("(if (({s}).toBoolWithCtx(ctx)) CV.FALSE else CV.TRUE)", .{a});
        },

        // ============================================================
        // Type Checks
        // ============================================================
        .typeof => {
            const val = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(val)) self.allocator.free(val);
            try self.vpushFmt("CV.fromJSValue(JSValue.typeOf(ctx, {s}.toJSValueWithCtx(ctx)))", .{val});
        },
        .instanceof => {
            const ctor = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(ctor)) self.allocator.free(ctor);
            const obj = self.vpop() orelse "stack[sp-2]";
            defer if (self.isAllocated(obj)) self.allocator.free(obj);
            try self.vpushFmt("(if (JSValue.isInstanceOf(ctx, {s}.toJSValueWithCtx(ctx), {s}.toJSValueWithCtx(ctx))) CV.TRUE else CV.FALSE)", .{ obj, ctor });
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
            // This opcode peeks at the value (doesn't consume it) and pushes a boolean
            // The original value stays on the stack for later use
            // Emit directly to avoid vstack consuming the reference during flush
            try self.flushVstack();
            try self.writeLine("stack[sp] = (if (stack[sp-1].isUndefined() or stack[sp-1].isNull()) CV.TRUE else CV.FALSE); sp += 1;");
        },

        // ============================================================
        // Increment/Decrement (standalone)
        // ============================================================
        .inc => {
            const a = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(a)) self.allocator.free(a);
            // FIX: Check for pre-increment pattern "inc_loc N, get_loc N, inc"
            // Only skip the increment if last_inc_loc matches this local index.
            var skip_increment = false;
            if (@hasField(CodeGen, "last_inc_loc")) {
                if (self.last_inc_loc) |inc_loc| {
                    // Check if input is "locals[N]" where N == inc_loc
                    if (std.mem.startsWith(u8, a, "locals[") and std.mem.endsWith(u8, a, "]")) {
                        const num_start = 7; // len("locals[")
                        const num_end = a.len - 1; // before "]"
                        if (std.fmt.parseInt(u32, a[num_start..num_end], 10)) |local_idx| {
                            if (local_idx == inc_loc) {
                                skip_increment = true;
                            }
                        } else |_| {}
                    }
                    // Clear after checking (one-shot)
                    self.last_inc_loc = null;
                }
            }
            if (skip_increment) {
                // Use vpushFmt to create a copy since defer will free 'a'
                try self.vpushFmt("{s}", .{a});
            } else {
                try self.vpushFmt("CV.add({s}, CV.newInt(1))", .{a});
            }
        },
        .dec => {
            const a = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(a)) self.allocator.free(a);
            // FIX: Check for pre-decrement pattern "dec_loc N, get_loc N, dec"
            // Only skip the decrement if last_dec_loc matches this local index.
            var skip_decrement = false;
            if (@hasField(CodeGen, "last_dec_loc")) {
                if (self.last_dec_loc) |dec_loc| {
                    // Check if input is "locals[N]" where N == dec_loc
                    if (std.mem.startsWith(u8, a, "locals[") and std.mem.endsWith(u8, a, "]")) {
                        const num_start = 7; // len("locals[")
                        const num_end = a.len - 1; // before "]"
                        if (std.fmt.parseInt(u32, a[num_start..num_end], 10)) |local_idx| {
                            if (local_idx == dec_loc) {
                                skip_decrement = true;
                            }
                        } else |_| {}
                    }
                    // Clear after checking (one-shot)
                    self.last_dec_loc = null;
                }
            }
            if (skip_decrement) {
                // Use vpushFmt to create a copy since defer will free 'a'
                try self.vpushFmt("{s}", .{a});
            } else {
                try self.vpushFmt("CV.sub({s}, CV.newInt(1))", .{a});
            }
        },
        .post_inc => {
            // Post-increment: push current value, then increment
            // Stack layout after: stack[sp-2] = original, stack[sp-1] = incremented (top)
            // The incremented value (sp-1) is what put_loc will store
            // The original value (sp-2) stays on stack for the expression result
            try self.flushVstack();
            try self.writeLine("{ const _v = stack[sp - 1]; stack[sp] = CV.add(_v, CV.newInt(1)); sp += 1; }");
            try self.vpush("stack[sp - 1]"); // Incremented value at sp-1 for put_loc to store
        },
        .post_dec => {
            // Post-decrement: push current value, then decrement
            // Stack layout after: stack[sp-2] = original, stack[sp-1] = decremented (top)
            // The decremented value (sp-1) is what put_loc will store
            // The original value (sp-2) stays on stack for the expression result
            try self.flushVstack();
            try self.writeLine("{ const _v = stack[sp - 1]; stack[sp] = CV.sub(_v, CV.newInt(1)); sp += 1; }");
            try self.vpush("stack[sp - 1]"); // Decremented value at sp-1 for put_loc to store
        },

        // ============================================================
        // Length Access
        // ============================================================
        .get_length => {
            const obj = self.vpop() orelse "stack[sp-1]";
            defer if (self.isAllocated(obj)) self.allocator.free(obj);
            try self.vpushFmt("zig_runtime.nativeGetLengthCV(ctx, ({s}).toJSValueWithCtx(ctx))", .{obj});
        },

        // ============================================================
        // get_array_el2 - pop idx, pop arr, push arr, push arr[idx]
        // ============================================================
        .get_array_el2 => {
            try self.flushVstack();
            try self.writeLine("{ const idx = stack[sp-1]; const arr = stack[sp-2]; var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, idx.toJSValueWithCtx(ctx)); stack[sp-1] = CV.fromJSValue(JSValue.getPropertyUint32(ctx, arr.toJSValueWithCtx(ctx), @intCast(idx_i32))); }");
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
            try self.writeLine("{ const exc = stack[sp-1]; sp -= 1; return JSValue.throw(ctx, exc.toJSValueWithCtx(ctx)); }");
            self.block_terminated = true;
        },

        // ============================================================
        // Type Checks
        // ============================================================
        .typeof_is_function => {
            // Use QuickJS JS_IsFunction which properly checks object class_id
            // (bytecode function, proxy, or callable object)
            try self.flushVstack();
            try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = if (zig_runtime.quickjs.JS_IsFunction(ctx, v.toJSValueWithCtx(ctx)) != 0) CV.TRUE else CV.FALSE; }");
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
            try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = CV.fromJSValue(JSValue.toObject(ctx, v.toJSValueWithCtx(ctx))); }");
        },
        .to_propkey => {
            try self.flushVstack();
            try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = CV.fromJSValue(JSValue.toPropKey(ctx, v.toJSValueWithCtx(ctx))); }");
        },
        .to_propkey2 => {
            // to_propkey2: [arr, idx] -> [arr, propkey(idx)]
            // Convert index to property key in place while preserving array at stack[sp-2]
            try self.flushVstack();
            try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = CV.fromJSValue(JSValue.toPropKey(ctx, v.toJSValueWithCtx(ctx))); }");
        },

        // ============================================================
        // Define Operations
        // ============================================================
        .define_array_el => {
            try self.flushVstack();
            // Stack: arr, idx, val - define element without coercing index
            // CRITICAL: Dup value before passing to definePropertyUint32 - it consumes the value
            try self.writeLine("{ const val = stack[sp-1]; const idx = stack[sp-2]; const arr = stack[sp-3]; var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, idx.toJSValueWithCtx(ctx)); _ = JSValue.definePropertyUint32(ctx, arr.toJSValueWithCtx(ctx), @intCast(idx_i32), JSValue.dup(ctx, val.toJSValueWithCtx(ctx))); sp -= 2; }");
        },
        .define_field => {
            const atom_idx = instr.operand.atom;
            if (self.getAtomString(atom_idx)) |prop_name| {
                try self.flushVstack();
                const escaped = self.escapeString(prop_name);
                defer self.allocator.free(escaped);
                // CRITICAL: Dup value before passing to definePropertyStr - it consumes the value
                try self.printLine("{{ const val = stack[sp-1]; const obj = stack[sp-2]; _ = JSValue.definePropertyStr(ctx, obj.toJSValueWithCtx(ctx), \"{s}\", JSValue.dup(ctx, val.toJSValueWithCtx(ctx))); sp -= 1; }}", .{escaped});
                // Track object on vstack for chaining (pop 2, push 1 = object remains)
                try self.vpush("stack[sp - 1]");
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
            try self.writeLine("    const val = stack[sp-1].toJSValueWithCtx(ctx);");
            try self.writeLine("    const arr = stack[sp-2].toJSValueWithCtx(ctx);");
            try self.writeLine("    var len: i64 = 0;");
            try self.writeLine("    _ = JSValue.getLength(ctx, &len, arr);");
            // CRITICAL: Dup value before passing to definePropertyUint32 - it consumes the value
            try self.writeLine("    _ = JSValue.definePropertyUint32(ctx, arr, @intCast(len), JSValue.dup(ctx, val));");
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
            // Use toJSValueWithCtx to route through C helper, bypassing LLVM WASM32 u64 return bug
            try self.writeLine("return stack[sp - 1].toJSValueWithCtx(ctx);");
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
            try self.printLine("{{ const v = locals[{d}]; if (v.isUninitialized()) return JSValue.throwReferenceError(ctx, \"Cannot access before initialization\"); stack[sp] = CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))); sp += 1; }}", .{loc});
            try self.vpush("stack[sp - 1]");
        },
        .put_loc_check => {
            const loc = instr.operand.loc;
            try self.printLine("{{ const v = locals[{d}]; if (v.isUninitialized()) return JSValue.throwReferenceError(ctx, \"Cannot access before initialization\"); const old = locals[{d}]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); locals[{d}] = stack[sp - 1]; sp -= 1; }}", .{ loc, loc, loc });
            _ = self.vpop();
        },
        .put_loc_check_init => {
            const loc = instr.operand.loc;
            try self.printLine("{{ const old = locals[{d}]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); locals[{d}] = stack[sp - 1]; sp -= 1; }}", .{ loc, loc });
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
            // Constant from constant pool - use cpool parameter
            const const_idx = instr.operand.const_idx;
            try self.vpushFmt("(if (cpool) |cp| CV.fromJSValue(JSValue.dup(ctx, cp[{d}])) else CV.UNDEFINED)", .{const_idx});
        },
        .push_const => {
            // Constant from constant pool - use cpool parameter
            const const_idx = instr.operand.const_idx;
            try self.vpushFmt("(if (cpool) |cp| CV.fromJSValue(JSValue.dup(ctx, cp[{d}])) else CV.UNDEFINED)", .{const_idx});
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

/// Check if vstack has entries that reference the given local, and if so, flush the vstack.
/// This is needed before modifying a local to ensure vstack expressions are evaluated
/// with the old value, not the new one.
fn flushVstackIfReferencesLocal(comptime CodeGen: type, self: *CodeGen, loc: u32) !void {
    var local_ref_buf: [16]u8 = undefined;
    const local_ref = std.fmt.bufPrint(&local_ref_buf, "locals[{d}]", .{loc}) catch return;

    const vstack_len = self.vstackLen();
    for (0..vstack_len) |i| {
        if (self.vstackGetAt(i)) |entry| {
            if (std.mem.indexOf(u8, entry, local_ref) != null) {
                // Found a reference to this local - flush entire vstack
                try self.flushVstack();
                return;
            }
        }
    }
}

fn emitBinaryOp(comptime CodeGen: type, self: *CodeGen, op: []const u8) !void {
    // Track whether each operand comes from vstack or physical stack
    const b_from_vstack = self.vpop();
    const b = b_from_vstack orelse "stack[sp-1]";
    const free_b = self.isAllocated(b);
    defer if (free_b) self.allocator.free(b);

    // If b came from vstack, a is at sp-1; if b came from physical stack, a is at sp-2
    const b_consumes: usize = if (b_from_vstack == null) 1 else 0;

    const a_from_vstack = self.vpop();
    const a = a_from_vstack orelse if (b_consumes == 0) "stack[sp-1]" else "stack[sp-2]";
    const free_a = self.isAllocated(a);
    defer if (free_a) self.allocator.free(a);

    try self.vpushFmt("{s}({s}, {s})", .{ op, a, b });
}

/// Binary operation with context parameter (for operations that need runtime support like string comparison)
fn emitBinaryOpWithCtx(comptime CodeGen: type, self: *CodeGen, op: []const u8) !void {
    // Track whether each operand comes from vstack or physical stack
    const b_from_vstack = self.vpop();
    const b = b_from_vstack orelse "stack[sp-1]";
    const free_b = self.isAllocated(b);
    defer if (free_b) self.allocator.free(b);

    // If b came from vstack, a is at sp-1; if b came from physical stack, a is at sp-2
    const b_consumes: usize = if (b_from_vstack == null) 1 else 0;

    const a_from_vstack = self.vpop();
    const a = a_from_vstack orelse if (b_consumes == 0) "stack[sp-1]" else "stack[sp-2]";
    const free_a = self.isAllocated(a);
    defer if (free_a) self.allocator.free(a);

    try self.vpushFmt("{s}(ctx, {s}, {s})", .{ op, a, b });
}

fn emitPutLocal(comptime CodeGen: type, self: *CodeGen, loc: u32) !void {
    // FIX: Before modifying a local, check if remaining vstack entries reference it.
    // If so, flush the vstack first to capture the current value before it changes.
    // This fixes bugs like: get_loc 1, inc, dup, put_loc 1, push_2, lt
    // where the vstack entry "CV.add(locals[1], CV.newInt(1))" would evaluate
    // incorrectly after locals[1] is modified.

    // Check if we need to flush: if there are remaining entries AND they reference the local
    const vstack_len = self.vstackLen();
    if (vstack_len >= 2) {
        // Check if any remaining entry (after popping top) references this local
        var local_ref_buf: [16]u8 = undefined;
        const local_ref = std.fmt.bufPrint(&local_ref_buf, "locals[{d}]", .{loc}) catch unreachable;

        var needs_flush = false;
        for (0..vstack_len - 1) |i| {
            if (self.vstackGetAt(i)) |entry| {
                if (std.mem.indexOf(u8, entry, local_ref) != null) {
                    needs_flush = true;
                    break;
                }
            }
        }

        if (needs_flush) {
            try self.flushVstack();
            // After flush, read from actual stack
            try self.printLine("locals[{d}] = stack[sp - 1]; sp -= 1;", .{loc});
            return;
        }
    }

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
    // set_loc pops the value, stores to local, and pushes the value back.
    // FIX: Before modifying the local, check if other vstack entries reference it.
    // If so, flush those entries first to capture their current values.
    // This fixes bugs like: f(x, x += 2, x) where the first 'x' must be captured
    // before x is modified by the += operation.
    const vstack_len = self.vstackLen();
    if (vstack_len >= 2) {
        // Check if any entry (except the top which we're about to pop) references this local
        var local_ref_buf: [16]u8 = undefined;
        const local_ref = std.fmt.bufPrint(&local_ref_buf, "locals[{d}]", .{loc}) catch unreachable;

        var needs_flush = false;
        for (0..vstack_len - 1) |i| {
            if (self.vstackGetAt(i)) |entry| {
                if (std.mem.indexOf(u8, entry, local_ref) != null) {
                    needs_flush = true;
                    break;
                }
            }
        }

        if (needs_flush) {
            try self.flushVstack();
            // After flush, read value from actual stack, store to local, keep on stack
            try self.printLine("locals[{d}] = stack[sp - 1];", .{loc});
            return;
        }
    }

    // After storing, replace vstack with locals[N] to ensure later references
    // get the actual stored value (not a stale expression that would be re-evaluated).
    if (self.vpop()) |expr| {
        defer if (self.isAllocated(expr)) self.allocator.free(expr);
        try self.printLine("locals[{d}] = {s};", .{ loc, expr });
        // Push the local reference back (value is now in locals[N])
        try self.vpushFmt("locals[{d}]", .{loc});
    } else {
        try self.printLine("locals[{d}] = stack[sp - 1];", .{loc});
        // Stack still has value, which is now also in locals[N]
    }
}

/// Find the position of a bytecode var_ref index in the closure_var_indices array.
/// The bytecode uses indices like 0, 1, 2, 3 for get_var_ref0/1/2/3, but the var_refs
/// array passed at runtime only contains the subset of closure variables that are
/// actually captured by this function. This function maps the bytecode index to the
/// correct position in var_refs.
///
/// Returns the position if found, otherwise returns the original index as fallback
/// (which handles the case where closure_var_indices is empty, meaning all vars
/// are captured in order).
fn findClosureVarPosition(comptime CodeGen: type, self: *CodeGen, bytecode_idx: u16) usize {
    const indices = self.func.closure_var_indices;
    // If no closure_var_indices specified, var_refs is in bytecode order
    if (indices.len == 0) {
        return bytecode_idx;
    }
    // Find the position of bytecode_idx in closure_var_indices
    for (indices, 0..) |cv_idx, pos| {
        if (cv_idx == bytecode_idx) {
            return pos;
        }
    }
    // Fallback: if not found, use original index (shouldn't happen for valid bytecode)
    return bytecode_idx;
}
