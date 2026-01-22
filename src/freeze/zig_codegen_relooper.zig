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
const opcode_emitter = @import("opcode_emitter.zig");

const Opcode = opcodes.Opcode;
const Instruction = parser.Instruction;
const BasicBlock = cfg_mod.BasicBlock;
const JS_ATOM_END = module_parser.JS_ATOM_END;
const CFG = cfg_mod.CFG;
const Allocator = std.mem.Allocator;

// Debug flag for Relooper-specific logging
const RELOOPER_DEBUG = false;

// ============================================================================
// Switch Pattern Detection
// ============================================================================
// QuickJS compiles switch(x) to chains of: dup, push_const, strict_eq, if_false
// We detect these patterns and emit native Zig switch for O(1) jump tables.

/// A single case in a switch statement
const SwitchCase = struct {
    /// The case constant value (e.g., 1, 2, 3 for case 1, case 2, case 3)
    value: i64,
    /// Block to jump to when this case matches (the case body start)
    target_block: u32,
    /// Block containing the comparison for this case
    comparison_block: u32,
};

/// A detected switch pattern
const SwitchPattern = struct {
    /// Block where the discriminant is first evaluated
    discriminant_block: u32,
    /// All switch cases in order
    cases: std.ArrayListUnmanaged(SwitchCase),
    /// Default case block (when no cases match)
    default_block: u32,
    /// Set of all blocks that are part of the switch comparison chain
    /// These should be skipped during normal block emission
    chain_blocks: std.AutoHashMapUnmanaged(u32, void),

    fn deinit(self: *SwitchPattern, allocator: Allocator) void {
        self.cases.deinit(allocator);
        self.chain_blocks.deinit(allocator);
    }
};

/// Check if a block matches the switch case pattern:
/// Instructions ending with: dup, push_const, strict_eq, if_false
fn isSwitchCaseBlock(block: BasicBlock) bool {
    const instrs = block.instructions;
    if (instrs.len < 4) return false;

    // Check last 4 instructions (or fewer if block is small)
    // Pattern: ... dup, push_i32/push_N, strict_eq, if_false
    const len = instrs.len;

    // Last instruction should be if_false
    const last = instrs[len - 1];
    if (last.opcode != .if_false and last.opcode != .if_false8) return false;

    // Second to last should be strict_eq
    const strict_eq_instr = instrs[len - 2];
    if (strict_eq_instr.opcode != .strict_eq) return false;

    // Third to last should be a constant push
    const push_instr = instrs[len - 3];
    const is_const_push = switch (push_instr.opcode) {
        .push_0, .push_1, .push_2, .push_3, .push_4, .push_5, .push_6, .push_7,
        .push_minus1, .push_i8, .push_i16, .push_i32 => true,
        else => false,
    };
    if (!is_const_push) return false;

    // Fourth to last should be dup
    const dup_instr = instrs[len - 4];
    if (dup_instr.opcode != .dup) return false;

    return true;
}

/// Extract the case constant value from a switch case block
fn extractCaseValue(block: BasicBlock) ?i64 {
    const instrs = block.instructions;
    if (instrs.len < 3) return null;

    // The constant is at len-3 (before strict_eq, if_false)
    const push_instr = instrs[instrs.len - 3];
    return switch (push_instr.opcode) {
        .push_0 => 0,
        .push_1 => 1,
        .push_2 => 2,
        .push_3 => 3,
        .push_4 => 4,
        .push_5 => 5,
        .push_6 => 6,
        .push_7 => 7,
        .push_minus1 => -1,
        .push_i8 => @as(i64, push_instr.operand.i8),
        .push_i16 => @as(i64, push_instr.operand.i16),
        .push_i32 => @as(i64, push_instr.operand.i32),
        else => null,
    };
}

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
    /// Explicit "use strict" directive (for proper 'this' handling)
    has_use_strict: bool = false,
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

    // Switch pattern detection
    switch_patterns: std.AutoHashMapUnmanaged(u32, SwitchPattern) = .{},
    switch_chain_blocks: std.AutoHashMapUnmanaged(u32, void) = .{},

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

        // Clean up switch patterns
        var iter = self.switch_patterns.valueIterator();
        while (iter.next()) |pattern| {
            pattern.cases.deinit(self.allocator);
            pattern.chain_blocks.deinit(self.allocator);
        }
        self.switch_patterns.deinit(self.allocator);
        self.switch_chain_blocks.deinit(self.allocator);
    }

    /// Generate Relooper-style Zig code for the function
    pub fn generate(self: *Self) ![]u8 {
        // Scan for this_val usage
        self.scanForThisUsage();

        // Detect switch patterns for optimization
        try self.detectSwitchPatterns();

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
        // Track iterator positions for for-of loops (stack for nested loops)
        try self.writeLine("var for_of_iter_stack: [8]usize = .{0} ** 8;");
        try self.writeLine("var for_of_depth: usize = 0;");
        try self.writeLine("_ = &stack; _ = &sp; _ = &for_of_iter_stack; _ = &for_of_depth;");
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
            \\pub noinline fn __frozen_{s}(ctx: *zig_runtime.JSContext, this_val: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue, var_refs: ?[*]*zig_runtime.JSVarRef, cpool: ?[*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {{
            \\
        , .{self.func.name});

        if (!self.uses_this_val) {
            try self.writeLine("    _ = this_val;");
        }
        try self.writeLine("    _ = @as(usize, @intCast(argc)) +% @intFromPtr(argv) +% @intFromPtr(var_refs) +% @intFromPtr(cpool);");
    }

    fn emitBlock(self: *Self, block: BasicBlock, block_idx: u32) !void {
        // Check if this block is part of a switch comparison chain (not the first block)
        // If so, skip it - the switch statement handles the dispatch
        if (self.switch_chain_blocks.contains(block_idx)) {
            // Still emit a case that jumps to proper handling (for unreachable paths)
            try self.printLine("{d} => {{ next_block = {d}; continue :machine; }}, // switch chain", .{ block_idx, block_idx });
            return;
        }

        // Check if this block starts a switch pattern
        if (self.switch_patterns.get(block_idx)) |pattern| {
            try self.emitSwitchBlock(block, block_idx, pattern);
            return;
        }

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
            // Use toJSValueWithCtx to route through C helper, bypassing LLVM WASM32 u64 return bug
            try self.writeLine("if (sp > 0) { return stack[sp - 1].toJSValueWithCtx(ctx); }");
            try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
        } else {
            // Multiple successors without conditional - take first
            try self.printLine("next_block = {d}; continue :machine;", .{successors[0]});
        }
    }

    fn emitInstruction(self: *Self, instr: Instruction, block: BasicBlock, idx: usize) !void {
        _ = block;
        _ = idx;

        // Try the shared opcode emitter first
        if (try opcode_emitter.emitOpcode(Self, self, instr)) {
            return;
        }

        // Handle opcodes not covered by shared emitter
        switch (instr.opcode) {
            // Control flow - handled by block terminator
            .if_false, .if_false8, .if_true, .if_true8 => {
                // Keep on vstack for terminator to flush
            },
            .goto, .goto8, .goto16 => {
                // Handled by block terminator
            },

            // Function calls - need Relooper-specific handling
            .call0 => try self.emitCall(0),
            .call1 => try self.emitCall(1),
            .call2 => try self.emitCall(2),
            .call3 => try self.emitCall(3),
            .call => try self.emitCall(instr.operand.u16),
            .call_method => try self.emitCallMethod(instr.operand.u16),

            // Tail calls - emit as regular calls
            .tail_call => {
                const argc = instr.operand.u16;
                try self.emitCall(argc);
            },
            .tail_call_method => {
                const argc = instr.operand.u16;
                try self.emitCallMethod(argc);
            },

            // For-of/For-in iteration - requires special handling
            // Track iterator position for nested loops (sp changes during loop body)
            .for_of_start => {
                try self.flushVstack();
                try self.writeLine("{");
                // Save iterator position before any modifications
                try self.writeLine("    for_of_iter_stack[for_of_depth] = sp - 1;");
                try self.writeLine("    for_of_depth += 1;");
                try self.writeLine("    const _iter_obj = stack[sp-1].toJSValue();");
                try self.writeLine("    const _iter = JSValue.getIterator(ctx, _iter_obj, 0);");
                try self.writeLine("    stack[sp-1] = CV.fromJSValue(_iter);");
                try self.writeLine("}");
            },
            .for_of_next => {
                try self.flushVstack();
                try self.writeLine("{");
                try self.writeLine("    var _done: i32 = 0;");
                // Use saved iterator position (sp may have changed during loop body)
                try self.writeLine("    const _iter_idx = for_of_iter_stack[for_of_depth - 1];");
                try self.writeLine("    const _iter = stack[_iter_idx].toJSValue();");
                try self.writeLine("    const _val = JSValue.iteratorNext(ctx, _iter, &_done);");
                try self.writeLine("    stack[sp] = CV.fromJSValue(_val);");
                try self.writeLine("    stack[sp+1] = if (_done != 0) CV.TRUE else CV.FALSE;");
                try self.writeLine("    sp += 2;");
                try self.writeLine("}");
            },
            .iterator_close => {
                try self.flushVstack();
                try self.writeLine("{");
                // Use saved iterator position and pop from depth stack
                try self.writeLine("    const _iter_idx = for_of_iter_stack[for_of_depth - 1];");
                try self.writeLine("    const _iter = stack[_iter_idx].toJSValue();");
                try self.writeLine("    _ = JSValue.iteratorClose(ctx, _iter, 0);");
                try self.writeLine("    sp = _iter_idx;  // Restore sp to before iterator");
                try self.writeLine("    for_of_depth -= 1;");
                try self.writeLine("}");
            },
            .iterator_get_value_done => {
                try self.flushVstack();
                try self.writeLine("{");
                try self.writeLine("    const _result = stack[sp-1].toJSValue();");
                try self.writeLine("    var _done: i32 = 0;");
                try self.writeLine("    const _val = JSValue.iteratorGetValueDone(ctx, _result, &_done);");
                try self.writeLine("    stack[sp-1] = CV.fromJSValue(_val);");
                try self.writeLine("    stack[sp] = if (_done != 0) CV.TRUE else CV.FALSE;");
                try self.writeLine("    sp += 1;");
                try self.writeLine("}");
            },

            // Fclosure - create function closure
            .fclosure8 => {
                const func_idx = instr.operand.u8;
                try self.flushVstack();
                try self.printLine("stack[sp] = CV.fromJSValue(zig_runtime.createClosure(ctx, {d}, var_refs)); sp += 1;", .{func_idx});
            },

            // Apply - requires complex argument handling
            .apply => {
                try self.flushVstack();
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const _args_array = stack[sp-1].toJSValue();");
                try self.writeLine("const _this_arg = stack[sp-2].toJSValue();");
                try self.writeLine("const _func = stack[sp-3].toJSValue();");
                try self.writeLine("var _len: i64 = 0;");
                try self.writeLine("_ = JSValue.getLength(ctx, &_len, _args_array);");
                try self.writeLine("const _argc: u32 = @intCast(@min(_len, 32));");
                try self.writeLine("var _args: [32]zig_runtime.JSValue = undefined;");
                try self.writeLine("for (0.._argc) |i| { _args[i] = JSValue.getPropertyUint32(ctx, _args_array, @intCast(i)); }");
                try self.writeLine("const _result = JSValue.call(ctx, _func, _this_arg, @intCast(_argc), &_args);");
                try self.writeLine("sp -= 3;");
                try self.writeLine("stack[sp] = CV.fromJSValue(_result);");
                try self.writeLine("sp += 1;");
                self.popIndent();
                try self.writeLine("}");
            },

            // Copy data properties - for spread
            .copy_data_properties => {
                try self.flushVstack();
                try self.writeLine("{");
                try self.writeLine("    const _excludeFlags = stack[sp-1].toInt32();");
                try self.writeLine("    const _src = stack[sp-2].toJSValue();");
                try self.writeLine("    const _dst = stack[sp-3].toJSValue();");
                try self.writeLine("    _ = zig_runtime.copyDataProperties(ctx, _dst, _src, _excludeFlags);");
                try self.writeLine("    sp -= 2;");
                try self.writeLine("}");
            },

            // Define method
            .define_method => {
                try self.flushVstack();
                try self.writeLine("{");
                try self.writeLine("    const _method = stack[sp-1].toJSValue();");
                try self.writeLine("    const _obj = stack[sp-2].toJSValue();");
                try self.writeLine("    _ = JSValue.defineMethod(ctx, _obj, _method);");
                try self.writeLine("    sp -= 1;");
                try self.writeLine("}");
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

            try self.printLine("const _result = JSValue.call(ctx, _fn, zig_runtime.JSValue.UNDEFINED, {d}, @ptrCast(@alignCast(&stack[sp - {d}])));", .{ argc, argc });
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
            try self.printLine("const _result = JSValue.call(ctx, _method, _obj, {d}, @ptrCast(@alignCast(&stack[sp - {d}])));", .{ argc, argc });
            try self.printLine("sp -= {d};", .{argc});
        } else {
            try self.writeLine("const _result = JSValue.call(ctx, _method, _obj, 0, @as([*]zig_runtime.JSValue, undefined));");
        }

        try self.writeLine("stack[sp] = CV.fromJSValue(_result); sp += 1;");
        self.popIndent();
        try self.writeLine("}");
    }

    pub fn getAtomString(self: *Self, atom_idx: u32) ?[]const u8 {
        // Builtin atoms are < JS_ATOM_END
        if (atom_idx < JS_ATOM_END) {
            if (atom_idx < module_parser.BUILTIN_ATOMS.len) {
                const name = module_parser.BUILTIN_ATOMS[atom_idx];
                if (name.len > 0 and (name.len < 1 or name[0] != '<')) {
                    return name;
                }
            }
            return null;
        }
        // User atoms are offset by JS_ATOM_END
        const adjusted_idx = atom_idx - JS_ATOM_END;
        if (adjusted_idx < self.func.atom_strings.len) {
            const str = self.func.atom_strings[adjusted_idx];
            if (str.len > 0) {
                return str;
            }
        }
        return null;
    }

    pub fn escapeString(self: *Self, input: []const u8) []u8 {
        var escaped_len: usize = 0;
        for (input) |c| {
            escaped_len += switch (c) {
                '\\', '"', '\n', '\r', '\t' => 2,
                0...8, 11, 12, 14...31 => 4,
                else => 1,
            };
        }
        const result = self.allocator.alloc(u8, escaped_len) catch return self.allocator.dupe(u8, input) catch @constCast(input);
        var i: usize = 0;
        for (input) |c| {
            switch (c) {
                '\\' => { result[i] = '\\'; result[i + 1] = '\\'; i += 2; },
                '"' => { result[i] = '\\'; result[i + 1] = '"'; i += 2; },
                '\n' => { result[i] = '\\'; result[i + 1] = 'n'; i += 2; },
                '\r' => { result[i] = '\\'; result[i + 1] = 'r'; i += 2; },
                '\t' => { result[i] = '\\'; result[i + 1] = 't'; i += 2; },
                0...8, 11, 12, 14...31 => {
                    result[i] = '\\'; result[i + 1] = 'x';
                    const hex = "0123456789abcdef";
                    result[i + 2] = hex[c >> 4]; result[i + 3] = hex[c & 0xf];
                    i += 4;
                },
                else => { result[i] = c; i += 1; },
            }
        }
        return result;
    }

    pub fn flushVstack(self: *Self) !void {
        while (self.vstack.items.len > 0) {
            const expr = self.vstack.orderedRemove(0);
            try self.printLine("stack[sp] = {s}; sp += 1;", .{expr});
            if (self.isAllocated(expr)) self.allocator.free(expr);
        }
    }

    // Virtual stack helpers
    pub fn vpush(self: *Self, expr: []const u8) !void {
        const owned = try self.allocator.dupe(u8, expr);
        try self.vstack.append(self.allocator, owned);
    }

    pub fn vpushFmt(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        const expr = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.vstack.append(self.allocator, expr);
    }

    pub fn vpop(self: *Self) ?[]const u8 {
        if (self.vstack.items.len > 0) {
            return self.vstack.pop();
        }
        return null;
    }

    pub fn vpeek(self: *Self) ?[]const u8 {
        if (self.vstack.items.len > 0) {
            return self.vstack.items[self.vstack.items.len - 1];
        }
        return null;
    }

    pub fn isAllocated(self: *Self, expr: []const u8) bool {
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

    pub fn countStackRefs(self: *Self, expr: []const u8) usize {
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

    pub fn printLine(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        try self.writeIndent();
        try self.print(fmt, args);
        try self.output.append(self.allocator, '\n');
    }

    pub fn writeLine(self: *Self, line: []const u8) !void {
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

    pub fn pushIndent(self: *Self) void {
        self.indent_level += 1;
    }

    pub fn popIndent(self: *Self) void {
        if (self.indent_level > 0) self.indent_level -= 1;
    }

    // ========================================================================
    // Switch Pattern Detection and Emission
    // ========================================================================

    /// Detect switch patterns in the CFG
    /// A switch pattern is a chain of blocks with: dup, push_const, strict_eq, if_false
    fn detectSwitchPatterns(self: *Self) !void {
        const blocks = self.func.cfg.blocks.items;
        if (blocks.len < 3) return; // Need at least 3 cases for switch optimization

        var processed = std.AutoHashMapUnmanaged(u32, void){};
        defer processed.deinit(self.allocator);

        for (blocks, 0..) |block, idx| {
            const block_idx: u32 = @intCast(idx);

            // Skip if already part of a switch chain
            if (processed.contains(block_idx)) continue;

            // Check if this block starts a switch pattern
            if (!isSwitchCaseBlock(block)) continue;

            // Try to build a switch pattern starting from this block
            var pattern = SwitchPattern{
                .discriminant_block = block_idx,
                .cases = .{},
                .default_block = 0,
                .chain_blocks = .{},
            };

            var current_block_idx = block_idx;
            var case_count: u32 = 0;

            while (current_block_idx < blocks.len) {
                const current_block = blocks[current_block_idx];

                // Check if this block matches the switch case pattern
                if (!isSwitchCaseBlock(current_block)) break;

                // Extract the case value
                const case_value = extractCaseValue(current_block) orelse break;

                // Get successors: if_false has [false_target, true_target]
                const successors = current_block.successors.items;
                if (successors.len < 2) break;

                const false_target = successors[0]; // Next case or default
                const true_target = successors[1]; // Case body

                // Add this case
                try pattern.cases.append(self.allocator, SwitchCase{
                    .value = case_value,
                    .target_block = true_target,
                    .comparison_block = current_block_idx,
                });

                case_count += 1;

                // Mark this block as part of the chain (except first block)
                if (current_block_idx != block_idx) {
                    try pattern.chain_blocks.put(self.allocator, current_block_idx, {});
                    try self.switch_chain_blocks.put(self.allocator, current_block_idx, {});
                }
                try processed.put(self.allocator, current_block_idx, {});

                // Move to next case block (false branch)
                // Stop if false_target is not a switch case block
                if (false_target >= blocks.len) {
                    pattern.default_block = false_target;
                    break;
                }

                if (!isSwitchCaseBlock(blocks[false_target])) {
                    pattern.default_block = false_target;
                    break;
                }

                current_block_idx = false_target;
            }

            // Only use switch optimization for 3+ cases
            if (case_count >= 3) {
                if (RELOOPER_DEBUG) {
                    std.debug.print("[switch] Detected switch at block {d} with {d} cases, default={d}\n", .{ block_idx, case_count, pattern.default_block });
                }
                try self.switch_patterns.put(self.allocator, block_idx, pattern);
            } else {
                // Clean up - not worth optimizing
                pattern.cases.deinit(self.allocator);
                pattern.chain_blocks.deinit(self.allocator);
            }
        }
    }

    /// Emit a native Zig switch for a detected switch pattern
    fn emitSwitchBlock(self: *Self, block: BasicBlock, block_idx: u32, pattern: SwitchPattern) !void {
        try self.printLine("{d} => {{ // switch ({d} cases)", .{ block_idx, pattern.cases.items.len });
        self.pushIndent();

        // Clear vstack for this block
        for (self.vstack.items) |expr| {
            if (self.isAllocated(expr)) self.allocator.free(expr);
        }
        self.vstack.clearRetainingCapacity();

        // Emit instructions BEFORE the switch comparison pattern
        // The switch block ends with: dup, push_const, strict_eq, if_false
        // We want to execute all instructions before the dup
        const instrs = block.instructions;
        self.block_terminated = false;
        if (instrs.len >= 4) {
            for (instrs[0 .. instrs.len - 4], 0..) |instr, idx| {
                try self.emitInstruction(instr, block, idx);
                if (self.block_terminated) break;
            }
        }
        // If block was terminated (return/throw), skip the switch and close the block
        if (self.block_terminated) {
            self.popIndent();
            try self.writeLine("},");
            return;
        }

        // Flush vstack to get the discriminant on the stack
        try self.flushVstack();

        // Emit the native switch
        // The discriminant is on top of stack, use toInt32() for integer switch
        try self.writeLine("{");
        self.pushIndent();
        try self.writeLine("const _switch_val = stack[sp - 1].toInt32();");
        try self.writeLine("sp -= 1; // Pop discriminant");
        try self.writeLine("switch (_switch_val) {");
        self.pushIndent();

        // Emit each case
        for (pattern.cases.items) |case| {
            try self.printLine("{d} => {{ next_block = {d}; continue :machine; }},", .{ case.value, case.target_block });
        }

        // Emit default case
        try self.printLine("else => {{ next_block = {d}; continue :machine; }},", .{pattern.default_block});

        self.popIndent();
        try self.writeLine("}");
        self.popIndent();
        try self.writeLine("}");

        self.popIndent();
        try self.writeLine("},");
    }
};

/// Generate Relooper-style Zig code for a function
pub fn generateRelooper(allocator: Allocator, func: FunctionInfo) ![]u8 {
    var codegen = RelooperCodeGen.init(allocator, func);
    defer codegen.deinit();
    return try codegen.generate();
}
