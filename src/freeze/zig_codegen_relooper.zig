//! Relooper-style Code Generator for Complex Control Flow
//!
//! This generator handles ANY CFG using a simple switch-in-loop state machine.
//! It's used as a fallback when the structured codegen in zig_codegen_full.zig
//! fails with ComplexControlFlow error.
//!
//! Architecture:
//!   bytecode → CFG → Zig source code (Relooper pattern) → LLVM → native
//!
//! Generated code pattern:
//!   fn __frozen_foo(ctx: *JSContext, this: JSValue, argc: c_int, argv: [*]JSValue, var_refs: ?[*]*JSVarRef) callconv(.c) JSValue {
//!       // Hoisted variables
//!       var locals: [N]CV = .{CV.UNDEFINED} ** N;
//!       var stack: [256]CV = .{CV.UNDEFINED} ** 256;
//!       var sp: usize = 0;
//!       var next_block: u32 = 0;  // State variable
//!
//!       // State machine - handles ANY control flow
//!       machine: while (true) {
//!           switch (next_block) {
//!               0 => { /* block 0 code */ next_block = 1; continue :machine; },
//!               1 => { return stack[sp - 1]; },
//!               else => unreachable,
//!           }
//!       }
//!   }

const std = @import("std");
const opcodes = @import("opcodes.zig");
const parser = @import("bytecode_parser.zig");
const cfg_mod = @import("cfg_builder.zig");
const module_parser = @import("module_parser.zig");

const Opcode = opcodes.Opcode;
const Instruction = parser.Instruction;
const BasicBlock = cfg_mod.BasicBlock;
const JS_ATOM_END = module_parser.JS_ATOM_END;
const CFG = cfg_mod.CFG;
const Allocator = std.mem.Allocator;

// Debug flag for Relooper-specific logging
const RELOOPER_DEBUG = false;

/// Information about a function to generate (same as zig_codegen_full)
pub const FunctionInfo = struct {
    name: []const u8,
    arg_count: u16,
    var_count: u16,
    cfg: *const CFG,
    is_self_recursive: bool,
    self_ref_var_idx: i16 = -1,
    closure_var_indices: []const u16 = &.{},
    atom_strings: []const []const u8 = &.{},
    partial_freeze: bool = false,
    js_name: []const u8 = &.{},
    is_pure_int32: bool = false,
};

pub const CodeGenError = error{
    UnsupportedOpcode,
    StackUnderflow,
    OutOfMemory,
    FormatError,
    InvalidBlock,
};

/// Relooper-style code generator
pub const RelooperCodeGen = struct {
    allocator: Allocator,
    output: std.ArrayListUnmanaged(u8),
    func: FunctionInfo,
    indent_level: u32 = 0,

    // Virtual stack for expression-based codegen
    vstack: std.ArrayListUnmanaged([]const u8) = .{},

    // Flags
    uses_this_val: bool = false,
    block_terminated: bool = false,

    const Self = @This();

    pub fn init(allocator: Allocator, func: FunctionInfo) Self {
        return .{
            .allocator = allocator,
            .output = .{},
            .func = func,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.vstack.items) |expr| {
            if (self.isAllocated(expr)) self.allocator.free(expr);
        }
        self.vstack.deinit(self.allocator);
        self.output.deinit(self.allocator);
    }

    /// Generate Relooper-style Zig code for the function
    pub fn generate(self: *Self) ![]u8 {
        // Scan for this_val usage
        self.scanForThisUsage();

        // Function signature
        try self.emitSignature();
        self.pushIndent();

        // Emit comptime branch quota for functions with many blocks
        const block_count = self.func.cfg.blocks.items.len;
        if (block_count > 5) {
            const quota = @max(10000, block_count * 100);
            try self.printLine("@setEvalBranchQuota({d});", .{quota});
        }

        // Stack overflow check
        try self.writeLine("if (zig_runtime.checkStack()) {");
        self.pushIndent();
        try self.writeLine("return zig_runtime.JSValue.throwRangeError(ctx, \"Maximum call stack size exceeded\");");
        self.popIndent();
        try self.writeLine("}");
        try self.writeLine("defer zig_runtime.exitStack();");
        try self.writeLine("");

        // CompressedValue alias (JSValue already declared at module level)
        try self.writeLine("const CV = zig_runtime.CompressedValue;");
        try self.writeLine("_ = &CV;");
        // JSValue is already declared at module level, just use it directly

        // Hoisted locals
        const min_locals = if (self.func.var_count >= 16) self.func.var_count else 16;
        try self.printLine("var locals: [{d}]CV = .{{CV.UNDEFINED}} ** {d};", .{ min_locals, min_locals });
        try self.writeLine("_ = &locals;");
        try self.writeLine("");

        // Hoisted stack
        try self.writeLine("var stack: [256]CV = .{CV.UNDEFINED} ** 256;");
        try self.writeLine("var sp: usize = 0;");
        try self.writeLine("_ = &stack; _ = &sp;");
        try self.writeLine("");

        // State variable
        try self.writeLine("var next_block: u32 = 0;");
        try self.writeLine("_ = &next_block;  // Silence unused warning for early-return functions");
        try self.writeLine("");

        // State machine
        try self.writeLine("machine: while (true) {");
        self.pushIndent();
        try self.writeLine("switch (next_block) {");
        self.pushIndent();

        // Generate each block as a switch case
        const blocks = self.func.cfg.blocks.items;
        for (blocks, 0..) |block, idx| {
            try self.emitBlock(block, @intCast(idx));
        }

        // Default case - use continue to silence unused label warning
        try self.writeLine("else => { continue :machine; },");

        self.popIndent();
        try self.writeLine("}");
        self.popIndent();
        try self.writeLine("}");

        self.popIndent();
        try self.writeLine("}");

        return try self.output.toOwnedSlice(self.allocator);
    }

    fn scanForThisUsage(self: *Self) void {
        for (self.func.cfg.blocks.items) |block| {
            for (block.instructions) |instr| {
                if (instr.opcode == .push_this) {
                    self.uses_this_val = true;
                    return;
                }
            }
        }
    }

    fn emitSignature(self: *Self) !void {
        try self.print(
            \\pub noinline fn __frozen_{s}(ctx: *zig_runtime.JSContext, this_val: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue, var_refs: ?[*]*zig_runtime.JSVarRef) callconv(.c) zig_runtime.JSValue {{
            \\
        , .{self.func.name});

        if (!self.uses_this_val) {
            try self.writeLine("    _ = this_val;");
        }
        try self.writeLine("    _ = @as(usize, @intCast(argc)) +% @intFromPtr(argv) +% @intFromPtr(var_refs);");
    }

    fn emitBlock(self: *Self, block: BasicBlock, block_idx: u32) !void {
        try self.printLine("{d} => {{ // block_{d}", .{ block_idx, block_idx });
        self.pushIndent();

        // Clear vstack for this block
        for (self.vstack.items) |expr| {
            if (self.isAllocated(expr)) self.allocator.free(expr);
        }
        self.vstack.clearRetainingCapacity();
        self.block_terminated = false;

        // Emit each instruction
        for (block.instructions, 0..) |instr, idx| {
            try self.emitInstruction(instr, block, idx);
            if (self.block_terminated) break;
        }

        // Emit block terminator (transition to next state)
        if (!self.block_terminated) {
            try self.emitBlockTerminator(block, block_idx);
        }

        self.popIndent();
        try self.writeLine("},");
    }

    fn emitBlockTerminator(self: *Self, block: BasicBlock, _: u32) !void {
        const successors = block.successors.items;
        const last_op = if (block.instructions.len > 0)
            block.instructions[block.instructions.len - 1].opcode
        else
            .nop;

        // Return instructions already handled
        if (last_op == .@"return" or last_op == .return_undef) {
            return;
        }

        // Conditional branches - check vstack or stack for condition
        if (last_op == .if_false or last_op == .if_false8) {
            if (successors.len >= 2) {
                const false_target = successors[0]; // Jump target when FALSE
                const true_target = successors[1]; // Fall-through when TRUE

                // Flush vstack to get condition
                try self.flushVstack();

                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const _cond = stack[sp - 1];");
                try self.writeLine("sp -= 1;");
                try self.printLine("if (_cond.toBool()) {{ next_block = {d}; }} else {{ next_block = {d}; }}", .{ true_target, false_target });
                try self.writeLine("continue :machine;");
                self.popIndent();
                try self.writeLine("}");
            } else if (successors.len == 1) {
                try self.printLine("next_block = {d}; continue :machine;", .{successors[0]});
            }
            return;
        }

        if (last_op == .if_true or last_op == .if_true8) {
            if (successors.len >= 2) {
                const true_target = successors[0]; // Jump target when TRUE
                const false_target = successors[1]; // Fall-through when FALSE

                try self.flushVstack();

                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const _cond = stack[sp - 1];");
                try self.writeLine("sp -= 1;");
                try self.printLine("if (_cond.toBool()) {{ next_block = {d}; }} else {{ next_block = {d}; }}", .{ true_target, false_target });
                try self.writeLine("continue :machine;");
                self.popIndent();
                try self.writeLine("}");
            } else if (successors.len == 1) {
                try self.printLine("next_block = {d}; continue :machine;", .{successors[0]});
            }
            return;
        }

        // Unconditional jump (goto)
        if (last_op == .goto or last_op == .goto8 or last_op == .goto16) {
            if (successors.len >= 1) {
                try self.printLine("next_block = {d}; continue :machine;", .{successors[0]});
            } else {
                try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
            }
            return;
        }

        // Fall-through to next block
        if (successors.len == 1) {
            try self.printLine("next_block = {d}; continue :machine;", .{successors[0]});
        } else if (successors.len == 0) {
            // No successors - this should be an exit block
            // Check if we have a value on vstack/stack
            try self.flushVstack();
            try self.writeLine("if (sp > 0) { return stack[sp - 1].toJSValue(); }");
            try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
        } else {
            // Multiple successors without conditional - take first
            try self.printLine("next_block = {d}; continue :machine;", .{successors[0]});
        }
    }

    fn emitInstruction(self: *Self, instr: Instruction, block: BasicBlock, idx: usize) !void {
        _ = block;
        _ = idx;

        switch (instr.opcode) {
            // Constants
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

            // Local variables
            .get_loc0 => try self.vpush("locals[0]"),
            .get_loc1 => try self.vpush("locals[1]"),
            .get_loc2 => try self.vpush("locals[2]"),
            .get_loc3 => try self.vpush("locals[3]"),
            .get_loc, .get_loc8 => try self.vpushFmt("locals[{d}]", .{instr.operand.loc}),
            .get_loc0_loc1 => {
                try self.vpush("locals[0]");
                try self.vpush("locals[1]");
            },

            // Put local (pop and store)
            .put_loc0 => try self.emitPutLocal(0),
            .put_loc1 => try self.emitPutLocal(1),
            .put_loc2 => try self.emitPutLocal(2),
            .put_loc3 => try self.emitPutLocal(3),
            .put_loc, .put_loc8 => try self.emitPutLocal(instr.operand.loc),

            // Set local (store but keep on stack)
            .set_loc0 => try self.emitSetLocal(0),
            .set_loc1 => try self.emitSetLocal(1),
            .set_loc2 => try self.emitSetLocal(2),
            .set_loc3 => try self.emitSetLocal(3),
            .set_loc, .set_loc8 => try self.emitSetLocal(instr.operand.loc),

            // Arguments
            .get_arg0 => try self.vpush("(if (0 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[0])) else CV.UNDEFINED)"),
            .get_arg1 => try self.vpush("(if (1 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[1])) else CV.UNDEFINED)"),
            .get_arg2 => try self.vpush("(if (2 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[2])) else CV.UNDEFINED)"),
            .get_arg3 => try self.vpush("(if (3 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[3])) else CV.UNDEFINED)"),
            .get_arg => try self.vpushFmt("(if ({d} < argc) CV.fromJSValue(JSValue.dup(ctx, argv[{d}])) else CV.UNDEFINED)", .{ instr.operand.arg, instr.operand.arg }),

            // Arithmetic
            .add => try self.emitBinaryOp("CV.add"),
            .sub => try self.emitBinaryOp("CV.sub"),
            .mul => try self.emitBinaryOp("CV.mul"),
            .div => try self.emitBinaryOp("CV.div"),
            .mod => try self.emitBinaryOp("CV.mod"),
            .neg => {
                const a = self.vpop() orelse "CV.UNDEFINED";
                defer if (self.isAllocated(a)) self.allocator.free(a);
                try self.vpushFmt("CV.sub(CV.newInt(0), {s})", .{a});
            },

            // Bitwise
            .@"and" => try self.emitBinaryOp("CV.band"),
            .@"or" => try self.emitBinaryOp("CV.bor"),
            .xor => try self.emitBinaryOp("CV.bxor"),
            .shl => try self.emitBinaryOp("CV.shl"),
            .sar => try self.emitBinaryOp("CV.sar"),
            .shr => try self.emitBinaryOp("CV.shr"),
            .not => {
                const a = self.vpop() orelse "CV.UNDEFINED";
                defer if (self.isAllocated(a)) self.allocator.free(a);
                try self.vpushFmt("CV.bnot({s})", .{a});
            },

            // Comparison
            .lt => try self.emitBinaryOp("CV.lt"),
            .lte => try self.emitBinaryOp("CV.lte"),
            .gt => try self.emitBinaryOp("CV.gt"),
            .gte => try self.emitBinaryOp("CV.gte"),
            .eq => try self.emitBinaryOp("CV.eq"),
            .neq => try self.emitBinaryOp("CV.neq"),
            .strict_eq => try self.emitBinaryOp("CV.strictEq"),
            .strict_neq => try self.emitBinaryOp("CV.strictNeq"),

            // Increment/decrement locals
            .inc_loc => try self.printLine("locals[{d}] = CV.add(locals[{d}], CV.newInt(1));", .{ instr.operand.loc, instr.operand.loc }),
            .dec_loc => try self.printLine("locals[{d}] = CV.sub(locals[{d}], CV.newInt(1));", .{ instr.operand.loc, instr.operand.loc }),
            .post_inc => {
                // Post-increment: push current value, then increment
                // Note: post_inc works on stack top, not a local
                try self.flushVstack();
                try self.writeLine("{ const _v = stack[sp - 1]; sp -= 1; stack[sp] = _v; sp += 1; stack[sp] = CV.add(_v, CV.newInt(1)); sp += 1; }");
            },
            .post_dec => {
                // Post-decrement: push current value, then decrement
                try self.flushVstack();
                try self.writeLine("{ const _v = stack[sp - 1]; sp -= 1; stack[sp] = _v; sp += 1; stack[sp] = CV.sub(_v, CV.newInt(1)); sp += 1; }");
            },

            // Stack manipulation
            .dup => {
                if (self.vpeek()) |expr| {
                    try self.vpush(expr);
                } else {
                    try self.vpush("stack[sp - 1]");
                }
            },
            .drop => {
                if (self.vpop()) |expr| {
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    try self.writeLine("sp -= 1;");
                }
            },
            .nip => {
                // Remove second item: [a, b] -> [b]
                const top = self.vpop();
                const _second = self.vpop();
                if (_second) |s| {
                    if (self.isAllocated(s)) self.allocator.free(s);
                }
                if (top) |t| {
                    try self.vstack.append(self.allocator, t);
                } else {
                    try self.writeLine("{ const _t = stack[sp - 1]; stack[sp - 2] = _t; sp -= 1; }");
                }
            },
            .swap => {
                const top = self.vpop();
                const second = self.vpop();
                if (top) |t| {
                    if (second) |s| {
                        try self.vstack.append(self.allocator, t);
                        try self.vstack.append(self.allocator, s);
                    } else {
                        try self.vpush("stack[sp - 1]");
                        try self.vstack.append(self.allocator, t);
                    }
                } else if (second) |s| {
                    try self.vstack.append(self.allocator, s);
                    try self.vpush("stack[sp - 1]");
                } else {
                    try self.writeLine("{ const _t = stack[sp - 1]; stack[sp - 1] = stack[sp - 2]; stack[sp - 2] = _t; }");
                }
            },

            // Return
            .@"return" => {
                try self.flushVstack();
                try self.writeLine("{ const _ret = stack[sp - 1].toJSValue(); return _ret; }");
                self.block_terminated = true;
            },
            .return_undef => {
                try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
                self.block_terminated = true;
            },

            // Control flow - handled by block terminator
            .if_false, .if_false8, .if_true, .if_true8 => {
                // Just pop the condition to vstack, terminator handles the branch
                // Actually keep on vstack for terminator to flush
            },
            .goto, .goto8, .goto16 => {
                // Handled by block terminator
            },

            // This
            .push_this => try self.vpush("CV.fromJSValue(this_val)"),

            // Unary logical
            .lnot => {
                const a = self.vpop() orelse "CV.UNDEFINED";
                defer if (self.isAllocated(a)) self.allocator.free(a);
                try self.vpushFmt("(if ({s}.toJSValue().toBool()) CV.FALSE else CV.TRUE)", .{a});
            },

            // Property access - requires FFI call
            .get_field2 => {
                const atom_idx = instr.operand.atom;
                const prop_name = self.getAtomString(atom_idx);
                const obj = self.vpop() orelse "CV.UNDEFINED";
                defer if (self.isAllocated(obj)) self.allocator.free(obj);
                try self.vpushFmt("CV.fromJSValue(JSValue.getField(ctx, {s}.toJSValue(), \"{s}\"))", .{ obj, prop_name });
            },
            .put_field => {
                const atom_idx = instr.operand.atom;
                const prop_name = self.getAtomString(atom_idx);
                const val = self.vpop() orelse "CV.UNDEFINED";
                defer if (self.isAllocated(val)) self.allocator.free(val);
                const obj = self.vpop() orelse "CV.UNDEFINED";
                defer if (self.isAllocated(obj)) self.allocator.free(obj);
                try self.printLine("_ = JSValue.setField(ctx, {s}.toJSValue(), \"{s}\", {s}.toJSValue());", .{ obj, prop_name, val });
            },
            .get_array_el => {
                const idx_expr = self.vpop() orelse "CV.UNDEFINED";
                defer if (self.isAllocated(idx_expr)) self.allocator.free(idx_expr);
                const arr_expr = self.vpop() orelse "CV.UNDEFINED";
                defer if (self.isAllocated(arr_expr)) self.allocator.free(arr_expr);
                try self.vpushFmt("CV.fromJSValue(JSValue.getIndex(ctx, {s}.toJSValue(), @intCast({s}.toInt32())))", .{ arr_expr, idx_expr });
            },
            .put_array_el => {
                const val = self.vpop() orelse "CV.UNDEFINED";
                defer if (self.isAllocated(val)) self.allocator.free(val);
                const idx_expr = self.vpop() orelse "CV.UNDEFINED";
                defer if (self.isAllocated(idx_expr)) self.allocator.free(idx_expr);
                const arr_expr = self.vpop() orelse "CV.UNDEFINED";
                defer if (self.isAllocated(arr_expr)) self.allocator.free(arr_expr);
                try self.printLine("_ = JSValue.setIndex(ctx, {s}.toJSValue(), @intCast({s}.toInt32()), {s}.toJSValue());", .{ arr_expr, idx_expr, val });
            },

            // Function calls
            .call0 => try self.emitCall(0),
            .call1 => try self.emitCall(1),
            .call2 => try self.emitCall(2),
            .call3 => try self.emitCall(3),
            .call => try self.emitCall(instr.operand.u16),
            .call_method => try self.emitCallMethod(instr.operand.u16),

            // Global variable access
            .get_var => {
                const atom_idx = instr.operand.atom;
                const var_name = self.getAtomString(atom_idx);
                try self.vpushFmt("CV.fromJSValue(JSValue.getGlobal(ctx, \"{s}\"))", .{var_name});
            },
            .put_var => {
                const atom_idx = instr.operand.atom;
                const var_name = self.getAtomString(atom_idx);
                const val = self.vpop() orelse "CV.UNDEFINED";
                defer if (self.isAllocated(val)) self.allocator.free(val);
                try self.printLine("_ = JSValue.setGlobal(ctx, \"{s}\", {s}.toJSValue());", .{ var_name, val });
            },

            // Closure variables
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
                const val = self.vpop() orelse "CV.UNDEFINED";
                defer if (self.isAllocated(val)) self.allocator.free(val);
                try self.printLine("if (var_refs) |vr| zig_runtime.setVarRef(vr[{d}], {s}.toJSValue());", .{ var_idx, val });
            },

            // Type checks
            .typeof => {
                const val = self.vpop() orelse "CV.UNDEFINED";
                defer if (self.isAllocated(val)) self.allocator.free(val);
                try self.vpushFmt("CV.fromJSValue(JSValue.typeofValue(ctx, {s}.toJSValue()))", .{val});
            },
            .instanceof => {
                const ctor = self.vpop() orelse "CV.UNDEFINED";
                defer if (self.isAllocated(ctor)) self.allocator.free(ctor);
                const obj = self.vpop() orelse "CV.UNDEFINED";
                defer if (self.isAllocated(obj)) self.allocator.free(obj);
                try self.vpushFmt("(if (JSValue.instanceof(ctx, {s}.toJSValue(), {s}.toJSValue())) CV.TRUE else CV.FALSE)", .{ obj, ctor });
            },

            // Object/Array creation
            .object => try self.vpush("CV.fromJSValue(JSValue.newObject(ctx))"),
            .array_from => {
                const count = instr.operand.u16;
                try self.flushVstack();
                try self.printLine("{{ const _arr = JSValue.newArray(ctx); var _i: usize = 0; while (_i < {d}) : (_i += 1) {{ _ = JSValue.setIndex(ctx, _arr, @intCast(_i), stack[sp - {d} + _i].toJSValue()); }} sp -= {d}; stack[sp] = CV.fromJSValue(_arr); sp += 1; }}", .{ count, count, count });
            },

            // Unsupported opcodes throw runtime error
            else => {
                const op_byte = @intFromEnum(instr.opcode);
                try self.printLine("// UNSUPPORTED: opcode {d} ({s}) - throw error", .{ op_byte, @tagName(instr.opcode) });
                try self.writeLine("return zig_runtime.JSValue.throwTypeError(ctx, \"Unsupported opcode in frozen function\");");
                self.block_terminated = true;
            },
        }
    }

    fn emitPutLocal(self: *Self, loc: u32) !void {
        if (self.vpop()) |expr| {
            try self.printLine("locals[{d}] = {s};", .{ loc, expr });
            const ref_count = self.countStackRefs(expr);
            if (ref_count > 0) try self.printLine("sp -= {d};", .{ref_count});
            if (self.isAllocated(expr)) self.allocator.free(expr);
        } else {
            try self.printLine("locals[{d}] = stack[sp - 1]; sp -= 1;", .{loc});
        }
    }

    fn emitSetLocal(self: *Self, loc: u32) !void {
        if (self.vpeek()) |expr| {
            try self.printLine("locals[{d}] = {s};", .{ loc, expr });
        } else {
            try self.printLine("locals[{d}] = stack[sp - 1];", .{loc});
        }
    }

    fn emitBinaryOp(self: *Self, op: []const u8) !void {
        const b = self.vpop() orelse "CV.UNDEFINED";
        const free_b = self.isAllocated(b);
        defer if (free_b) self.allocator.free(b);
        const a = self.vpop() orelse "CV.UNDEFINED";
        const free_a = self.isAllocated(a);
        defer if (free_a) self.allocator.free(a);
        try self.vpushFmt("{s}({s}, {s})", .{ op, a, b });
    }

    fn emitCall(self: *Self, argc: u16) !void {
        // Pop arguments in reverse order
        var args = std.ArrayListUnmanaged([]const u8){};
        defer args.deinit(self.allocator);

        var i: u16 = 0;
        while (i < argc) : (i += 1) {
            const arg = self.vpop() orelse "CV.UNDEFINED";
            try args.append(self.allocator, arg);
        }

        // Pop function
        const func_expr = self.vpop() orelse "CV.UNDEFINED";
        defer if (self.isAllocated(func_expr)) self.allocator.free(func_expr);

        // Build args array
        if (argc == 0) {
            try self.vpushFmt("CV.fromJSValue(JSValue.call(ctx, {s}.toJSValue(), zig_runtime.JSValue.UNDEFINED, 0, @as([*]zig_runtime.JSValue, undefined)))", .{func_expr});
        } else {
            try self.flushVstack();
            try self.printLine("{{ const _fn = {s}.toJSValue();", .{func_expr});
            self.pushIndent();

            // Push args from reverse order
            var j: usize = args.items.len;
            while (j > 0) {
                j -= 1;
                const arg = args.items[j];
                try self.printLine("stack[sp] = {s}; sp += 1;", .{arg});
                if (self.isAllocated(arg)) self.allocator.free(arg);
            }

            try self.printLine("const _result = JSValue.call(ctx, _fn, zig_runtime.JSValue.UNDEFINED, {d}, @ptrCast(&stack[sp - {d}]));", .{ argc, argc });
            try self.printLine("sp -= {d};", .{argc});
            try self.writeLine("stack[sp] = CV.fromJSValue(_result); sp += 1;");
            self.popIndent();
            try self.writeLine("}");
        }
    }

    fn emitCallMethod(self: *Self, argc: u16) !void {
        // call_method: the method is already on the stack from get_field2
        // Stack: [this, method, arg0, arg1, ...argN-1] -> [result]

        // Pop arguments in reverse order
        var args = std.ArrayListUnmanaged([]const u8){};
        defer args.deinit(self.allocator);

        var i: u16 = 0;
        while (i < argc) : (i += 1) {
            const arg = self.vpop() orelse "CV.UNDEFINED";
            try args.append(self.allocator, arg);
        }

        // Pop method and this (both pushed by get_field2)
        const method_expr = self.vpop() orelse "CV.UNDEFINED";
        defer if (self.isAllocated(method_expr)) self.allocator.free(method_expr);
        const obj_expr = self.vpop() orelse "CV.UNDEFINED";
        defer if (self.isAllocated(obj_expr)) self.allocator.free(obj_expr);

        try self.flushVstack();
        try self.printLine("{{ const _method = {s}.toJSValue();", .{method_expr});
        self.pushIndent();
        try self.printLine("const _obj = {s}.toJSValue();", .{obj_expr});

        if (argc > 0) {
            var j: usize = args.items.len;
            while (j > 0) {
                j -= 1;
                const arg = args.items[j];
                try self.printLine("stack[sp] = {s}; sp += 1;", .{arg});
                if (self.isAllocated(arg)) self.allocator.free(arg);
            }
            try self.printLine("const _result = JSValue.call(ctx, _method, _obj, {d}, @ptrCast(&stack[sp - {d}]));", .{ argc, argc });
            try self.printLine("sp -= {d};", .{argc});
        } else {
            try self.writeLine("const _result = JSValue.call(ctx, _method, _obj, 0, @as([*]zig_runtime.JSValue, undefined));");
        }

        try self.writeLine("stack[sp] = CV.fromJSValue(_result); sp += 1;");
        self.popIndent();
        try self.writeLine("}");
    }

    fn getAtomString(self: *Self, atom_idx: u32) []const u8 {
        // Builtin atoms are < JS_ATOM_END
        if (atom_idx < JS_ATOM_END) {
            if (atom_idx < module_parser.BUILTIN_ATOMS.len) {
                const name = module_parser.BUILTIN_ATOMS[atom_idx];
                if (name.len > 0 and (name.len < 1 or name[0] != '<')) {
                    return name;
                }
            }
            return "unknown";
        }
        // User atoms are offset by JS_ATOM_END
        const adjusted_idx = atom_idx - JS_ATOM_END;
        if (adjusted_idx < self.func.atom_strings.len) {
            const str = self.func.atom_strings[adjusted_idx];
            if (str.len > 0) {
                return str;
            }
        }
        return "unknown";
    }

    fn flushVstack(self: *Self) !void {
        while (self.vstack.items.len > 0) {
            const expr = self.vstack.orderedRemove(0);
            try self.printLine("stack[sp] = {s}; sp += 1;", .{expr});
            if (self.isAllocated(expr)) self.allocator.free(expr);
        }
    }

    // Virtual stack helpers
    fn vpush(self: *Self, expr: []const u8) !void {
        const owned = try self.allocator.dupe(u8, expr);
        try self.vstack.append(self.allocator, owned);
    }

    fn vpushFmt(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        const expr = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.vstack.append(self.allocator, expr);
    }

    fn vpop(self: *Self) ?[]const u8 {
        if (self.vstack.items.len > 0) {
            return self.vstack.pop();
        }
        return null;
    }

    fn vpeek(self: *Self) ?[]const u8 {
        if (self.vstack.items.len > 0) {
            return self.vstack.items[self.vstack.items.len - 1];
        }
        return null;
    }

    fn isAllocated(self: *Self, expr: []const u8) bool {
        // Check if expression was allocated by us (not a static string)
        _ = self;
        // Static strings from vpush are always < 64 chars
        return expr.len > 0 and !std.mem.startsWith(u8, expr, "locals[") and
            !std.mem.startsWith(u8, expr, "stack[") and
            !std.mem.startsWith(u8, expr, "CV.") and
            !std.mem.eql(u8, expr, "(if (0 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[0])) else CV.UNDEFINED)") and
            !std.mem.eql(u8, expr, "(if (1 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[1])) else CV.UNDEFINED)") and
            !std.mem.eql(u8, expr, "(if (2 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[2])) else CV.UNDEFINED)") and
            !std.mem.eql(u8, expr, "(if (3 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[3])) else CV.UNDEFINED)");
    }

    fn countStackRefs(self: *Self, expr: []const u8) usize {
        _ = self;
        var count: usize = 0;
        var i: usize = 0;
        while (i < expr.len) {
            if (std.mem.startsWith(u8, expr[i..], "stack[sp")) {
                count += 1;
                i += 8;
            } else {
                i += 1;
            }
        }
        return count;
    }

    // Output helpers
    fn print(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        try self.output.ensureUnusedCapacity(self.allocator, 256);
        try self.output.writer(self.allocator).print(fmt, args);
    }

    fn printLine(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        try self.writeIndent();
        try self.print(fmt, args);
        try self.output.append(self.allocator, '\n');
    }

    fn writeLine(self: *Self, line: []const u8) !void {
        try self.writeIndent();
        try self.output.appendSlice(self.allocator, line);
        try self.output.append(self.allocator, '\n');
    }

    fn writeIndent(self: *Self) !void {
        var i: u32 = 0;
        while (i < self.indent_level) : (i += 1) {
            try self.output.appendSlice(self.allocator, "    ");
        }
    }

    fn pushIndent(self: *Self) void {
        self.indent_level += 1;
    }

    fn popIndent(self: *Self) void {
        if (self.indent_level > 0) self.indent_level -= 1;
    }
};

/// Generate Relooper-style Zig code for a function
pub fn generateRelooper(allocator: Allocator, func: FunctionInfo) ![]u8 {
    var codegen = RelooperCodeGen.init(allocator, func);
    defer codegen.deinit();
    return try codegen.generate();
}
