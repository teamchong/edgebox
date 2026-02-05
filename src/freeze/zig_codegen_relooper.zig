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

// Dispatch table threshold: functions with more than this many blocks use
// runtime dispatch tables instead of compile-time switch statements.
// Higher values reduce generated code size but may hit comptime eval limits.
// Lower values generate more code but avoid comptime explosion.
// NOTE: 10000 effectively disables dispatch tables (uses inline comptime switch)
const DISPATCH_TABLE_THRESHOLD: usize = 10000;

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
    /// Function contains await opcodes and needs async state machine support
    is_async: bool = false,
    /// Constant pool values - used for fclosure bytecode registration
    constants: []const module_parser.ConstValue = &.{},
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

    // Block functions buffer for dispatch table mode (written at module level)
    block_functions: std.ArrayListUnmanaged(u8) = .{},
    block_fn_indent: u32 = 0,

    // Virtual stack for expression-based codegen
    vstack: std.ArrayListUnmanaged([]const u8) = .{},

    // Base stack depth tracking - for correct stack refs when vstack underflows
    // When operations write directly to real stack (e.g., checked ops), they increment
    // base_stack_depth instead of pushing to vstack. vpop uses this to compute correct offsets.
    base_stack_depth: u32 = 0,
    base_popped_count: u32 = 0,

    // Flags
    uses_this_val: bool = false,
    block_terminated: bool = false,
    has_put_arg: bool = false,
    max_arg_idx_used: u32 = 0, // Track max arg index for arg_shadow sizing

    // Track last inc_loc/dec_loc for pre-increment/decrement pattern detection
    // QuickJS emits "inc_loc N, get_loc N, inc" for ++i which would double-increment.
    // We set this when inc_loc/dec_loc runs and check it in .inc/.dec to skip the redundant op.
    last_inc_loc: ?u32 = null,
    last_dec_loc: ?u32 = null,

    // Switch pattern detection
    switch_patterns: std.AutoHashMapUnmanaged(u32, SwitchPattern) = .{},
    switch_chain_blocks: std.AutoHashMapUnmanaged(u32, void) = .{},

    // Async function support
    is_async_function: bool = false,
    await_points: std.ArrayListUnmanaged(AwaitPoint) = .{},

    // Dispatch table mode - use runtime dispatch instead of comptime switch
    dispatch_mode: bool = false,

    const Self = @This();

    /// Tracks an await point for state machine generation
    const AwaitPoint = struct {
        /// Block index where await occurs
        block_id: u32,
        /// Instruction index within the block
        instr_idx: u32,
        /// The next block to resume at after await
        resume_block_id: u32,
    };

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
        self.block_functions.deinit(self.allocator);

        // Clean up switch patterns
        var iter = self.switch_patterns.valueIterator();
        while (iter.next()) |pattern| {
            pattern.cases.deinit(self.allocator);
            pattern.chain_blocks.deinit(self.allocator);
        }
        self.switch_patterns.deinit(self.allocator);
        self.switch_chain_blocks.deinit(self.allocator);

        // Clean up async tracking
        self.await_points.deinit(self.allocator);
    }

    /// Scan for await opcodes to detect async functions and track await points
    fn scanForAsyncOpcodes(self: *Self) void {
        // Check if function is declared as async (func_kind >= 2)
        if (self.func.is_async) {
            self.is_async_function = true;
        }

        const blocks = self.func.cfg.blocks.items;
        for (blocks, 0..) |block, block_idx| {
            for (block.instructions, 0..) |instr, instr_idx| {
                if (instr.opcode == .await) {
                    self.is_async_function = true;
                    // Determine the resume block (successor of current block after await)
                    const resume_block = if (block.successors.items.len > 0)
                        block.successors.items[0]
                    else
                        @as(u32, @intCast(block_idx)) + 1;

                    self.await_points.append(self.allocator, .{
                        .block_id = @intCast(block_idx),
                        .instr_idx = @intCast(instr_idx),
                        .resume_block_id = resume_block,
                    }) catch {};
                }
            }
        }
    }

    /// Generate Relooper-style Zig code for the function
    pub fn generate(self: *Self) ![]u8 {
        // Scan for this_val usage and async opcodes
        self.scanForThisUsage();
        self.scanForAsyncOpcodes();

        // Detect switch patterns for optimization
        try self.detectSwitchPatterns();

        // Check if we should use dispatch table mode
        // NOTE: Async functions cannot use dispatch mode because async state variables
        // (_async_state, _async_promise, etc.) are declared in main function and
        // block functions cannot access them
        const block_count = self.func.cfg.blocks.items.len;
        if (block_count > DISPATCH_TABLE_THRESHOLD and !self.is_async_function) {
            self.dispatch_mode = true;
        }

        // If dispatch mode, emit block functions and types first
        if (self.dispatch_mode) {
            try self.emitBlockContextStruct();
            try self.emitBlockResultType();
            try self.emitBlockFnType();

            // Emit all block functions
            const blocks = self.func.cfg.blocks.items;
            for (blocks, 0..) |block, idx| {
                try self.emitBlockAsFunction(block, @intCast(idx));
            }

            // Emit dispatch table
            try self.emitDispatchTable(block_count);
        }

        // Function signature
        try self.emitSignature();
        self.pushIndent();

        // Emit comptime branch quota for functions with many blocks (only for switch mode)
        if (!self.dispatch_mode and block_count > 5) {
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

        // Only create arg_shadow when function accesses arguments AND modifies them
        // Use max(arg_count, max_arg_idx_used) to handle cases where bytecode arg_count is wrong
        const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
        if (arg_count > 0 and self.has_put_arg) {
            try self.printLine("var arg_shadow: [{d}]CV = undefined;", .{arg_count});
            for (0..arg_count) |i| {
                try self.printLine("arg_shadow[{d}] = if ({d} < argc) CV.fromJSValue(JSValue.dup(ctx, argv[{d}])) else CV.UNDEFINED;", .{ i, i, i });
            }
            try self.writeLine("_ = &arg_shadow;");
            try self.writeLine("");
        }

        // Hoisted stack
        try self.writeLine("var stack: [256]CV = .{CV.UNDEFINED} ** 256;");
        try self.writeLine("var sp: usize = 0;");
        // Track iterator positions for for-of loops (stack for nested loops)
        try self.writeLine("var for_of_iter_stack: [8]usize = .{0} ** 8;");
        try self.writeLine("var for_of_depth: usize = 0;");
        try self.writeLine("_ = &stack; _ = &sp; _ = &for_of_iter_stack; _ = &for_of_depth;");
        try self.writeLine("");

        if (self.dispatch_mode) {
            // Dispatch table mode - emit context and dispatch loop
            try self.emitBlockContextInit();
            try self.emitDispatchLoop(block_count);
        } else {
            // Switch mode - state variable and state machine
            try self.writeLine("var next_block: u32 = 0;");
            try self.writeLine("_ = &next_block;  // Silence unused warning for early-return functions");
            try self.writeLine("");

            // Async function state machine support
            if (self.is_async_function) {
                try self.emitAsyncPrologue();
            }

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
        }

        self.popIndent();
        try self.writeLine("}");

        // If dispatch mode, prepend block functions to output
        if (self.dispatch_mode and self.block_functions.items.len > 0) {
            // Create combined output: block_functions + main function
            var combined = std.ArrayListUnmanaged(u8){};
            try combined.appendSlice(self.allocator, self.block_functions.items);
            try combined.appendSlice(self.allocator, self.output.items);
            self.output.deinit(self.allocator);
            self.output = combined;
        }

        return try self.output.toOwnedSlice(self.allocator);
    }

    /// Emit prologue code for async functions
    fn emitAsyncPrologue(self: *Self) !void {
        try self.writeLine("// Async function support - check for resume mode");
        try self.writeLine("var _async_state: ?*zig_runtime.FrozenAsyncState = null;");
        try self.writeLine("var _async_resolving_funcs: [2]JSValue = .{ JSValue.UNDEFINED, JSValue.UNDEFINED };");
        try self.writeLine("var _async_promise: JSValue = JSValue.UNDEFINED;");
        try self.writeLine("");

        // Check if we're being called in resume mode
        // Resume mode is indicated by first argument being a special state pointer
        try self.writeLine("// Check for resume mode: first arg is the async state pointer");
        try self.writeLine("if (argc >= 2) {");
        self.pushIndent();
        try self.writeLine("const _maybe_state_ptr = argv[0];");
        try self.writeLine("const _maybe_resolved = argv[1];");
        try self.writeLine("// Check if first arg is an integer (state pointer as i64)");
        try self.writeLine("if (_maybe_state_ptr.isInt() or _maybe_state_ptr.isFloat64()) {");
        self.pushIndent();
        try self.writeLine("const _state_addr: usize = @intFromFloat(_maybe_state_ptr.getNumberAsFloat());");
        try self.writeLine("if (_state_addr != 0) {");
        self.pushIndent();
        try self.writeLine("_async_state = @ptrFromInt(_state_addr);");
        try self.writeLine("if (_async_state) |state| {");
        self.pushIndent();
        try self.writeLine("// Validate magic number");
        try self.writeLine("if (state.magic == 0xAF57A7E1) {");
        self.pushIndent();
        try self.writeLine("// Restore state from async state");
        try self.writeLine("state.restore(&stack, &locals);");
        try self.writeLine("sp = state.sp;");
        try self.writeLine("next_block = state.block_id;");
        try self.writeLine("// Push the resolved value onto the stack");
        try self.writeLine("stack[sp] = CV.fromJSValue(JSValue.dup(ctx, _maybe_resolved));");
        try self.writeLine("sp += 1;");
        try self.writeLine("// Get the resolving functions from state (stored externally)");
        try self.writeLine("// The callback already has access to them");
        self.popIndent();
        try self.writeLine("}");
        self.popIndent();
        try self.writeLine("}");
        self.popIndent();
        try self.writeLine("}");
        self.popIndent();
        try self.writeLine("}");
        self.popIndent();
        try self.writeLine("}");
        try self.writeLine("");

        // If not resuming, create the async function's promise
        try self.writeLine("// Create async function's promise if this is the initial call");
        try self.writeLine("if (_async_state == null) {");
        self.pushIndent();
        try self.writeLine("_async_promise = zig_runtime.quickjs.JS_NewPromiseCapability(ctx, &_async_resolving_funcs);");
        try self.writeLine("if (_async_promise.isException()) return _async_promise;");
        self.popIndent();
        try self.writeLine("}");
        try self.writeLine("");
    }

    fn scanForThisUsage(self: *Self) void {
        for (self.func.cfg.blocks.items) |block| {
            for (block.instructions) |instr| {
                if (instr.opcode == .push_this) {
                    self.uses_this_val = true;
                }
                // Check for argument access opcodes and track max index
                switch (instr.opcode) {
                    .get_arg0, .put_arg0, .set_arg0 => {
                        self.max_arg_idx_used = @max(self.max_arg_idx_used, 1);
                    },
                    .get_arg1, .put_arg1, .set_arg1 => {
                        self.max_arg_idx_used = @max(self.max_arg_idx_used, 2);
                    },
                    .get_arg2, .put_arg2, .set_arg2 => {
                        self.max_arg_idx_used = @max(self.max_arg_idx_used, 3);
                    },
                    .get_arg3, .put_arg3, .set_arg3 => {
                        self.max_arg_idx_used = @max(self.max_arg_idx_used, 4);
                    },
                    .get_arg, .put_arg, .set_arg => {
                        self.max_arg_idx_used = @max(self.max_arg_idx_used, instr.operand.arg + 1);
                    },
                    else => {},
                }
                // Check for argument write opcodes
                switch (instr.opcode) {
                    .put_arg, .put_arg0, .put_arg1, .put_arg2, .put_arg3,
                    .set_arg, .set_arg0, .set_arg1, .set_arg2, .set_arg3 => {
                        self.has_put_arg = true;
                    },
                    else => {},
                }
            }
        }
    }

    fn emitSignature(self: *Self) !void {
        try self.print(
            \\pub noinline fn __frozen_{s}(ctx: *zig_runtime.JSContext, this_val: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue, var_refs: ?[*]*zig_runtime.JSVarRef, closure_var_count: c_int, cpool: ?[*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {{
            \\
        , .{self.func.name});

        // Only discard this_val if not used AND not in dispatch mode
        // In dispatch mode, this_val is always used (passed to BlockContext)
        if (!self.uses_this_val and !self.dispatch_mode) {
            try self.writeLine("    _ = this_val;");
        }
        try self.writeLine("    _ = @as(usize, @intCast(argc)) +% @intFromPtr(argv) +% @intFromPtr(var_refs) +% @as(usize, @intCast(closure_var_count)) +% @intFromPtr(cpool);");
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

        // Clear vstack and reset base stack tracking for this block
        for (self.vstack.items) |expr| {
            if (self.isAllocated(expr)) self.allocator.free(expr);
        }
        self.vstack.clearRetainingCapacity();
        self.base_stack_depth = 0;
        self.base_popped_count = 0;
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
                try self.printLine("if (_cond.toBoolWithCtx(ctx)) {{ next_block = {d}; }} else {{ next_block = {d}; }}", .{ true_target, false_target });
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
                try self.printLine("if (_cond.toBoolWithCtx(ctx)) {{ next_block = {d}; }} else {{ next_block = {d}; }}", .{ true_target, false_target });
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
            // Must flush vstack before jumping - ternary branches push values that need to be on stack
            try self.flushVstack();
            if (successors.len >= 1) {
                if (self.dispatch_mode) {
                    try self.printLine("return .{{ .next_block = {d} }};", .{successors[0]});
                } else {
                    try self.printLine("next_block = {d}; continue :machine;", .{successors[0]});
                }
            } else {
                if (self.dispatch_mode) {
                    try self.writeLine("return .return_undef;");
                } else {
                    try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
                }
            }
            return;
        }

        // Fall-through to next block
        if (successors.len == 1) {
            try self.flushVstack();
            if (self.dispatch_mode) {
                try self.printLine("return .{{ .next_block = {d} }};", .{successors[0]});
            } else {
                try self.printLine("next_block = {d}; continue :machine;", .{successors[0]});
            }
        } else if (successors.len == 0) {
            // No successors - this should be an exit block
            // Check if we have a value on vstack/stack
            try self.flushVstack();
            // Return with proper dup and cleanup (matches zig_codegen_full behavior)
            // Must dup ref types because cleanup may free the same object
            if (self.has_put_arg) {
                // Free arg_shadow values before returning (they were duped at function entry)
                const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
                try self.writeLine("if (sp > 0) {");
                try self.writeLine("    const _ret_cv = stack[sp - 1];");
                try self.writeLine("    const _ret_val = if (_ret_cv.isRefType()) JSValue.dup(ctx, _ret_cv.toJSValueWithCtx(ctx)) else _ret_cv.toJSValueWithCtx(ctx);");
                // Free arg_shadow
                for (0..arg_count) |i| {
                    try self.printLine("    if (arg_shadow[{d}].isRefType()) JSValue.free(ctx, arg_shadow[{d}].toJSValueWithCtx(ctx));", .{ i, i });
                }
                if (self.dispatch_mode) {
                    try self.writeLine("    return .{ .return_value = _ret_val };");
                } else {
                    try self.writeLine("    return _ret_val;");
                }
                try self.writeLine("}");
                // Return undefined with cleanup
                for (0..arg_count) |i| {
                    try self.printLine("if (arg_shadow[{d}].isRefType()) JSValue.free(ctx, arg_shadow[{d}].toJSValueWithCtx(ctx));", .{ i, i });
                }
            } else {
                if (self.dispatch_mode) {
                    try self.writeLine("if (sp > 0) { return .{ .return_value = stack[sp - 1].toJSValueWithCtx(ctx) }; }");
                } else {
                    try self.writeLine("if (sp > 0) { return stack[sp - 1].toJSValueWithCtx(ctx); }");
                }
            }
            if (self.dispatch_mode) {
                try self.writeLine("return .return_undef;");
            } else {
                try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
            }
        } else {
            // Multiple successors without conditional - take first
            if (self.dispatch_mode) {
                try self.printLine("return .{{ .next_block = {d} }};", .{successors[0]});
            } else {
                try self.printLine("next_block = {d}; continue :machine;", .{successors[0]});
            }
        }
    }

    /// Emit cleanup code for locals before function return.
    /// Delegates to shared helper in opcode_emitter.zig.
    fn emitLocalsCleanup(self: *Self) !void {
        try opcode_emitter.emitLocalsCleanupShared(Self, self, self.func.var_count, "    ");
    }

    fn emitInstruction(self: *Self, instr: Instruction, block: BasicBlock, idx: usize) !void {
        _ = idx;

        // Handle get_arg specially - use arg_shadow only if function modifies arguments
        switch (instr.opcode) {
            .get_arg0, .get_arg1, .get_arg2, .get_arg3, .get_arg => {
                const arg_idx: u16 = switch (instr.opcode) {
                    .get_arg0 => 0,
                    .get_arg1 => 1,
                    .get_arg2 => 2,
                    .get_arg3 => 3,
                    else => instr.operand.arg,
                };
                if (self.has_put_arg) {
                    // Function modifies arguments - read from arg_shadow (has updated values)
                    try self.vpushFmt("(blk: {{ const v = arg_shadow[{d}]; break :blk if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; }})", .{arg_idx});
                } else {
                    // Read-only access - read directly from argv (faster, no upfront dup)
                    try self.vpushFmt("CV.fromJSValue(if ({d} < argc) JSValue.dup(ctx, argv[{d}]) else JSValue.UNDEFINED)", .{ arg_idx, arg_idx });
                }
                return;
            },
            // Handle closure variable access - use vstack since expression is self-contained
            // (doesn't reference stack positions, so safe to defer evaluation)
            .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3, .get_var_ref => {
                const bytecode_idx: u16 = switch (instr.opcode) {
                    .get_var_ref0 => 0,
                    .get_var_ref1 => 1,
                    .get_var_ref2 => 2,
                    .get_var_ref3 => 3,
                    else => instr.operand.var_ref,
                };
                // Use vpush since closure var expression is self-contained (no stack refs)
                try self.vpushFmt("CV.fromJSValue(zig_runtime.getClosureVarSafe(ctx, var_refs, {d}, closure_var_count))", .{bytecode_idx});
                return;
            },
            .get_var_ref_check => {
                const bytecode_idx = instr.operand.var_ref;
                try self.flushVstack();
                try self.printLine("stack[sp] = CV.fromJSValue(zig_runtime.getClosureVarCheckSafe(ctx, var_refs, {d}, closure_var_count)); sp += 1;", .{bytecode_idx});
                if (self.dispatch_mode) {
                    try self.writeLine("if (stack[sp-1].isException()) return .{ .return_value = stack[sp-1].toJSValueWithCtx(ctx) };");
                } else {
                    try self.writeLine("if (stack[sp-1].isException()) return stack[sp-1].toJSValueWithCtx(ctx);");
                }
                // Track this value on base stack - vpop will compute correct offset when needed
                // Reset base_popped_count because we're starting fresh tracking from this new item
                self.base_stack_depth += 1;
                self.base_popped_count = 0;
                return;
            },
            // Override get_loc_check to use base_stack_depth instead of vstack
            // This ensures correct stack refs when multiple checked ops write to real stack
            .get_loc_check => {
                const loc = instr.operand.loc;
                try self.flushVstack();
                if (self.dispatch_mode) {
                    try self.printLine("{{ const v = locals[{d}]; if (v.isUninitialized()) return .{{ .return_value = JSValue.throwReferenceError(ctx, \"Cannot access before initialization\") }}; stack[sp] = CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))); sp += 1; }}", .{loc});
                } else {
                    try self.printLine("{{ const v = locals[{d}]; if (v.isUninitialized()) return JSValue.throwReferenceError(ctx, \"Cannot access before initialization\"); stack[sp] = CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))); sp += 1; }}", .{loc});
                }
                // Track this value on base stack - vpop will compute correct offset when needed
                // Reset base_popped_count because we're starting fresh tracking from this new item
                self.base_stack_depth += 1;
                self.base_popped_count = 0;
                return;
            },
            // Override put_loc_check to handle dispatch mode return type
            .put_loc_check => {
                const loc = instr.operand.loc;
                try self.flushVstack();
                if (self.dispatch_mode) {
                    try self.printLine("{{ const v = locals[{d}]; if (v.isUninitialized()) return .{{ .return_value = JSValue.throwReferenceError(ctx, \"Cannot access before initialization\") }}; const old = locals[{d}]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); locals[{d}] = stack[sp - 1]; sp -= 1; }}", .{ loc, loc, loc });
                } else {
                    try self.printLine("{{ const v = locals[{d}]; if (v.isUninitialized()) return JSValue.throwReferenceError(ctx, \"Cannot access before initialization\"); const old = locals[{d}]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); locals[{d}] = stack[sp - 1]; sp -= 1; }}", .{ loc, loc, loc });
                }
                return;
            },
            // get_field2: pop obj, push obj AND method (for call_method)
            // Special handling needed when object is on base stack to avoid double-consumption.
            // The shared opcode emitter pushes both object reference and method expression,
            // but both contain "stack[sp-1]" which causes flushVstack to decrement sp twice.
            .get_field2 => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |prop_name| {
                    const escaped_prop = self.escapeString(prop_name);
                    defer self.allocator.free(escaped_prop);

                    if (self.vstack.items.len > 0) {
                        // Object is on vstack - safe to use deferred evaluation
                        // Pop obj, push obj back, push method - both will flush correctly
                        // since the obj expression doesn't contain stack refs
                        const obj = self.vpop().?;
                        try self.vpush(obj);
                        try self.vpushFmt("CV.fromJSValue(JSValue.getPropertyStr(ctx, {s}.toJSValueWithCtx(ctx), \"{s}\"))", .{ obj, escaped_prop });
                        if (self.isAllocated(obj)) self.allocator.free(obj);
                    } else {
                        // Object is on real stack - materialize directly to avoid double-consumption.
                        // Object stays at sp-1, method goes at sp, then sp += 1.
                        // This way the object reference is only evaluated once.
                        try self.printLine("stack[sp] = CV.fromJSValue(JSValue.getPropertyStr(ctx, stack[sp-1].toJSValueWithCtx(ctx), \"{s}\")); sp += 1;", .{escaped_prop});
                        // Track that we've added one item to base stack (now have obj + method)
                        self.base_stack_depth += 1;
                    }
                } else {
                    try self.writeLine("// get_field2: atom index out of range");
                    try self.writeLine("return JSValue.throwTypeError(ctx, \"Invalid property access\");");
                    self.block_terminated = true;
                }
                return;
            },
            // Handle return opcodes specially - must dup return value, cleanup locals and arg_shadow
            // The shared emitter doesn't know about arg_shadow or locals cleanup, so we override it here
            .@"return" => {
                try self.flushVstack();
                try self.writeLine("{");
                // Save return value (dup if ref type to avoid use-after-free during cleanup)
                try self.writeLine("    const _ret_cv = stack[sp - 1];");
                try self.writeLine("    const _ret_val = if (_ret_cv.isRefType()) JSValue.dup(ctx, _ret_cv.toJSValueWithCtx(ctx)) else _ret_cv.toJSValueWithCtx(ctx);");
                // Free the original stack value (the dup created a new ref)
                try self.writeLine("    if (_ret_cv.isRefType()) JSValue.free(ctx, _ret_cv.toJSValueWithCtx(ctx));");
                // Cleanup locals - free all ref-type values to prevent GC leaks
                try self.emitLocalsCleanup();
                // Free arg_shadow values if function modifies arguments (they were duped at function entry)
                if (self.has_put_arg) {
                    const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
                    try opcode_emitter.emitArgShadowCleanupShared(Self, self, arg_count, "    ");
                }
                // Async functions must wrap return value in Promise.resolve()
                if (self.is_async_function) {
                    try self.writeLine("    return JSValue.promiseResolve(ctx, _ret_val);");
                } else if (self.dispatch_mode) {
                    try self.writeLine("    return .{ .return_value = _ret_val };");
                } else {
                    try self.writeLine("    return _ret_val;");
                }
                try self.writeLine("}");
                self.block_terminated = true;
                return;
            },
            .return_undef => {
                // Cleanup locals - free all ref-type values to prevent GC leaks
                try self.emitLocalsCleanup();
                // Free arg_shadow values if function modifies arguments
                if (self.has_put_arg) {
                    const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
                    try opcode_emitter.emitArgShadowCleanupShared(Self, self, arg_count, "");
                }
                // Async functions must wrap undefined in Promise.resolve()
                if (self.is_async_function) {
                    try self.writeLine("return JSValue.promiseResolve(ctx, zig_runtime.JSValue.UNDEFINED);");
                } else if (self.dispatch_mode) {
                    try self.writeLine("return .return_undef;");
                } else {
                    try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
                }
                self.block_terminated = true;
                return;
            },
            // Handle throw opcode specially for dispatch_mode
            .throw => {
                try self.flushVstack();
                if (self.dispatch_mode) {
                    try self.writeLine("{ const exc = stack[sp-1]; sp -= 1; _ = JSValue.throw(ctx, exc.toJSValueWithCtx(ctx)); return .exception; }");
                } else {
                    try self.writeLine("{ const exc = stack[sp-1]; sp -= 1; return JSValue.throw(ctx, exc.toJSValueWithCtx(ctx)); }");
                }
                self.block_terminated = true;
                return;
            },
            // Handle check_ctor specially for dispatch_mode
            .check_ctor => {
                try self.flushVstack();
                if (self.dispatch_mode) {
                    try self.writeLine("if (!zig_runtime.isConstructorCall(ctx, this_val)) return .{ .return_value = JSValue.throwTypeError(ctx, \"Constructor requires 'new'\") };");
                } else {
                    try self.writeLine("if (!zig_runtime.isConstructorCall(ctx, this_val)) return JSValue.throwTypeError(ctx, \"Constructor requires 'new'\");");
                }
                return;
            },
            else => {},
        }

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
            // Use js_frozen_for_of_start/next for proper iterator handling (matches full codegen)
            .for_of_start => {
                try self.flushVstack();
                try self.writeLine("{");
                // Save iterator position before any modifications
                try self.writeLine("    for_of_iter_stack[for_of_depth] = sp - 1;  // Push iterator position");
                try self.writeLine("    for_of_depth += 1;");
                // js_frozen_for_of_start expects JSValue stack, but we have CV stack
                // Use temp buffer: [obj] -> call -> [iterator, next_method]
                try self.writeLine("    var for_of_buf: [2]JSValue = undefined;");
                try self.writeLine("    for_of_buf[0] = stack[sp - 1].toJSValueWithCtx(ctx);");
                try self.writeLine("    for_of_buf[1] = JSValue.UNDEFINED;");
                try self.writeLine("    const rc = zig_runtime.quickjs.js_frozen_for_of_start(ctx, @ptrCast(&for_of_buf[1]), 0);");
                if (self.dispatch_mode) {
                    try self.writeLine("    if (rc != 0) return .{ .return_value = JSValue.EXCEPTION };");
                } else {
                    try self.writeLine("    if (rc != 0) return JSValue.EXCEPTION;");
                }
                try self.writeLine("    stack[sp - 1] = CV.fromJSValue(for_of_buf[0]);  // iterator replaces obj");
                try self.writeLine("    stack[sp] = CV.fromJSValue(for_of_buf[1]); sp += 1;  // next_method");
                try self.writeLine("    stack[sp] = CV.fromJSValue(zig_runtime.newCatchOffset(0)); sp += 1;");
                try self.writeLine("}");
            },
            .for_of_next => {
                try self.flushVstack();
                try self.writeLine("{");
                // Use the saved iterator position from the stack (supports nested loops)
                // This is critical because sp changes during the loop body
                try self.writeLine("    const iter_idx = for_of_iter_stack[for_of_depth - 1];");
                try self.writeLine("    var for_of_buf: [5]JSValue = undefined;");
                try self.writeLine("    for_of_buf[0] = stack[iter_idx].toJSValueWithCtx(ctx);      // iterator");
                try self.writeLine("    for_of_buf[1] = stack[iter_idx + 1].toJSValueWithCtx(ctx);  // next_method");
                try self.writeLine("    for_of_buf[2] = JSValue.UNDEFINED;                // unused slot for C ABI");
                try self.writeLine("    for_of_buf[3] = JSValue.UNDEFINED;                // value (output)");
                try self.writeLine("    for_of_buf[4] = JSValue.UNDEFINED;                // done (output)");
                try self.writeLine("    const rc = zig_runtime.quickjs.js_frozen_for_of_next(ctx, @ptrCast(&for_of_buf[3]), -3);");
                if (self.dispatch_mode) {
                    try self.writeLine("    if (rc != 0) return .{ .return_value = JSValue.EXCEPTION };");
                } else {
                    try self.writeLine("    if (rc != 0) return JSValue.EXCEPTION;");
                }
                // Write iterator back - js_frozen_for_of_next may have freed it and set to UNDEFINED
                try self.writeLine("    stack[iter_idx] = CV.fromJSValue(for_of_buf[0]);");
                try self.writeLine("    stack[sp] = CV.fromJSValue(for_of_buf[3]); sp += 1;  // value");
                try self.writeLine("    stack[sp] = CV.fromJSValue(for_of_buf[4]); sp += 1;  // done");
                try self.writeLine("}");
            },
            .iterator_close => {
                try self.flushVstack();
                try self.writeLine("{");
                // Use saved iterator position and free iterator + next_method
                try self.writeLine("    const _iter_base = for_of_iter_stack[for_of_depth - 1];");
                try self.writeLine("    const _iter = stack[_iter_base];");
                try self.writeLine("    if (_iter.isRefType()) JSValue.free(ctx, _iter.toJSValueWithCtx(ctx));");
                try self.writeLine("    const _next = stack[_iter_base + 1];");
                try self.writeLine("    if (_next.isRefType()) JSValue.free(ctx, _next.toJSValueWithCtx(ctx));");
                try self.writeLine("    sp = _iter_base;");
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

            // For-in iteration
            .for_in_start => {
                try self.flushVstack();
                try self.writeLine("{");
                try self.writeLine("    var _for_in_buf: [2]JSValue = .{stack[sp - 1].toJSValue(), JSValue.UNDEFINED};");
                try self.writeLine("    const _rc = zig_runtime.quickjs.js_frozen_for_in_start(ctx, @ptrCast(&_for_in_buf[1]));");
                if (self.dispatch_mode) {
                    try self.writeLine("    if (_rc < 0) return .{ .return_value = JSValue.EXCEPTION };");
                } else {
                    try self.writeLine("    if (_rc < 0) return JSValue.EXCEPTION;");
                }
                try self.writeLine("    stack[sp - 1] = CV.fromJSValue(_for_in_buf[0]);");
                try self.writeLine("}");
            },
            .for_in_next => {
                try self.flushVstack();
                try self.writeLine("{");
                try self.writeLine("    var _for_in_buf: [3]JSValue = .{stack[sp - 1].toJSValue(), JSValue.UNDEFINED, JSValue.UNDEFINED};");
                try self.writeLine("    const _rc = zig_runtime.quickjs.js_frozen_for_in_next(ctx, @ptrCast(&_for_in_buf[1]));");
                if (self.dispatch_mode) {
                    try self.writeLine("    if (_rc < 0) return .{ .return_value = JSValue.EXCEPTION };");
                } else {
                    try self.writeLine("    if (_rc < 0) return JSValue.EXCEPTION;");
                }
                try self.writeLine("    stack[sp - 1] = CV.fromJSValue(_for_in_buf[0]);");
                try self.writeLine("    stack[sp] = CV.fromJSValue(_for_in_buf[1]); sp += 1;");
                try self.writeLine("    stack[sp] = CV.fromJSValue(_for_in_buf[2]); sp += 1;");
                try self.writeLine("}");
            },

            // 'in' operator: check if property exists in object
            .in => {
                try self.flushVstack();
                try self.writeLine("{");
                try self.writeLine("    const _obj = stack[sp-1].toJSValue();");
                try self.writeLine("    const _prop = stack[sp-2].toJSValue();");
                try self.writeLine("    const _atom = zig_runtime.quickjs.JS_ValueToAtom(ctx, _prop);");
                try self.writeLine("    const _result = zig_runtime.quickjs.JS_HasProperty(ctx, _obj, _atom);");
                try self.writeLine("    zig_runtime.quickjs.JS_FreeAtom(ctx, _atom);");
                if (self.dispatch_mode) {
                    try self.writeLine("    if (_result < 0) return .{ .return_value = JSValue.EXCEPTION };");
                } else {
                    try self.writeLine("    if (_result < 0) return JSValue.EXCEPTION;");
                }
                try self.writeLine("    stack[sp-2] = CV.fromJSValue(JSValue.newBool(_result > 0));");
                try self.writeLine("    sp -= 1;");
                try self.writeLine("}");
            },

            // Fclosure - create function closure (8-bit index variant)
            .fclosure8 => {
                const func_idx = instr.operand.const_idx;
                try self.flushVstack();
                try self.writeLine("{");
                self.pushIndent();
                // Get the function bytecode from constant pool passed to this frozen function
                try self.printLine("const _bfunc = if (cpool) |cp| cp[{d}] else JSValue.UNDEFINED;", .{func_idx});
                // Register bytecode with child frozen function if we have child func info
                if (func_idx < self.func.constants.len) {
                    switch (self.func.constants[func_idx]) {
                        .child_func => |child| {
                            // Register bytecode pointer with the child's frozen function by name lookup
                            try self.printLine("native_dispatch.registerCpoolBytecodeByName(_bfunc, \"{s}@{d}\");", .{ child.name, child.line_num });
                        },
                        else => {},
                    }
                }
                // Convert CompressedValue locals to JSValue array for closure creation
                const var_count = self.func.var_count;
                if (var_count > 0) {
                    try self.printLine("var _locals_js: [{d}]JSValue = undefined;", .{var_count});
                    try self.printLine("for (0..{d}) |_i| {{ _locals_js[_i] = CV.toJSValuePtr(&locals[_i]); }}", .{var_count});
                    try self.printLine("const _closure = JSValue.createClosure(ctx, _bfunc, var_refs, &_locals_js, {d}, argv[0..@intCast(argc)]);", .{var_count});
                } else {
                    try self.writeLine("const _closure = JSValue.createClosure(ctx, _bfunc, var_refs, null, 0, argv[0..@intCast(argc)]);");
                }
                try self.writeLine("stack[sp] = CV.fromJSValue(_closure);");
                try self.writeLine("sp += 1;");
                self.popIndent();
                try self.writeLine("}");
            },

            // Fclosure - create function closure (32-bit index variant)
            .fclosure => {
                const func_idx = instr.operand.const_idx;
                try self.flushVstack();
                try self.writeLine("{");
                self.pushIndent();
                // Get the function bytecode from constant pool passed to this frozen function
                try self.printLine("const _bfunc = if (cpool) |cp| cp[{d}] else JSValue.UNDEFINED;", .{func_idx});
                // Register bytecode with child frozen function if we have child func info
                if (func_idx < self.func.constants.len) {
                    switch (self.func.constants[func_idx]) {
                        .child_func => |child| {
                            // Register bytecode pointer with the child's frozen function by name lookup
                            try self.printLine("native_dispatch.registerCpoolBytecodeByName(_bfunc, \"{s}@{d}\");", .{ child.name, child.line_num });
                        },
                        else => {},
                    }
                }
                // Convert CompressedValue locals to JSValue array for closure creation
                const var_count = self.func.var_count;
                if (var_count > 0) {
                    try self.printLine("var _locals_js: [{d}]JSValue = undefined;", .{var_count});
                    try self.printLine("for (0..{d}) |_i| {{ _locals_js[_i] = CV.toJSValuePtr(&locals[_i]); }}", .{var_count});
                    try self.printLine("const _closure = JSValue.createClosure(ctx, _bfunc, var_refs, &_locals_js, {d}, argv[0..@intCast(argc)]);", .{var_count});
                } else {
                    try self.writeLine("const _closure = JSValue.createClosure(ctx, _bfunc, var_refs, null, 0, argv[0..@intCast(argc)]);");
                }
                try self.writeLine("stack[sp] = CV.fromJSValue(_closure);");
                try self.writeLine("sp += 1;");
                self.popIndent();
                try self.writeLine("}");
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
                try self.writeLine("if (_result.isException()) return zig_runtime.JSValue.EXCEPTION;");
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
                try self.writeLine("    const _excluded = stack[sp-1].toJSValue();");
                try self.writeLine("    const _src = stack[sp-2].toJSValue();");
                try self.writeLine("    const _dst = stack[sp-3].toJSValue();");
                try self.writeLine("    _ = zig_runtime.copyDataProperties(ctx, _dst, _src, _excluded);");
                try self.writeLine("    sp -= 2;");
                try self.writeLine("}");
            },

            // Define method - Flags: 0=method, 1=getter, 2=setter, 4=enumerable
            .define_method => {
                try self.flushVstack();
                const atom = instr.operand.atom_u8.atom;
                const flags = instr.operand.atom_u8.value;
                const method_type = flags & 3; // 0=method, 1=getter, 2=setter
                const is_enumerable = (flags & 4) != 0;
                try self.writeLine("{");
                try self.writeLine("    const _method = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("    const _obj = stack[sp-2].toJSValueWithCtx(ctx);");
                if (method_type == 1) {
                    // Getter
                    const enum_flag = if (is_enumerable) " | JSValue.JS_PROP_ENUMERABLE" else "";
                    try self.printLine("    _ = JSValue.definePropertyGetSet(ctx, _obj, {d}, _method, JSValue.UNDEFINED, JSValue.JS_PROP_CONFIGURABLE | JSValue.JS_PROP_HAS_GET{s});", .{ atom, enum_flag });
                } else if (method_type == 2) {
                    // Setter
                    const enum_flag = if (is_enumerable) " | JSValue.JS_PROP_ENUMERABLE" else "";
                    try self.printLine("    _ = JSValue.definePropertyGetSet(ctx, _obj, {d}, JSValue.UNDEFINED, _method, JSValue.JS_PROP_CONFIGURABLE | JSValue.JS_PROP_HAS_SET{s});", .{ atom, enum_flag });
                } else {
                    // Regular method
                    try self.printLine("    _ = JSValue.definePropertyValueAtom(ctx, _obj, {d}, _method, JSValue.JS_PROP_C_W_E);", .{atom});
                }
                try self.writeLine("    sp -= 1;");
                try self.writeLine("}");
            },

            // Closure variable access opcodes
            // Use vpush since closure var expression is self-contained (no stack refs)
            .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3, .get_var_ref => {
                const bytecode_idx: u16 = switch (instr.opcode) {
                    .get_var_ref0 => 0,
                    .get_var_ref1 => 1,
                    .get_var_ref2 => 2,
                    .get_var_ref3 => 3,
                    else => instr.operand.var_ref,
                };
                try self.vpushFmt("CV.fromJSValue(zig_runtime.getClosureVarSafe(ctx, var_refs, {d}, closure_var_count))", .{bytecode_idx});
            },
            .get_var_ref_check => {
                const bytecode_idx = instr.operand.var_ref;
                try self.flushVstack();
                try self.printLine("stack[sp] = CV.fromJSValue(zig_runtime.getClosureVarCheckSafe(ctx, var_refs, {d}, closure_var_count)); sp += 1;", .{bytecode_idx});
                if (self.dispatch_mode) {
                    try self.writeLine("if (stack[sp-1].isException()) return .{ .return_value = stack[sp-1].toJSValueWithCtx(ctx) };");
                } else {
                    try self.writeLine("if (stack[sp-1].isException()) return stack[sp-1].toJSValueWithCtx(ctx);");
                }
                // Track this value on base stack - vpop will compute correct offset when needed
                self.base_stack_depth += 1;
            },
            .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3, .put_var_ref => {
                const bytecode_idx: u16 = switch (instr.opcode) {
                    .put_var_ref0 => 0,
                    .put_var_ref1 => 1,
                    .put_var_ref2 => 2,
                    .put_var_ref3 => 3,
                    else => instr.operand.var_ref,
                };
                try self.flushVstack();
                try self.printLine("sp -= 1; zig_runtime.setClosureVarSafe(ctx, var_refs, {d}, closure_var_count, stack[sp].toJSValueWithCtx(ctx));", .{bytecode_idx});
            },
            .put_var_ref_check => {
                const bytecode_idx = instr.operand.var_ref;
                try self.flushVstack();
                try self.printLine("sp -= 1; _ = zig_runtime.setClosureVarCheckSafe(ctx, var_refs, {d}, closure_var_count, stack[sp].toJSValueWithCtx(ctx));", .{bytecode_idx});
            },
            .put_var_ref_check_init => {
                const bytecode_idx = instr.operand.var_ref;
                try self.flushVstack();
                try self.printLine("sp -= 1; zig_runtime.setClosureVarSafe(ctx, var_refs, {d}, closure_var_count, stack[sp].toJSValueWithCtx(ctx));", .{bytecode_idx});
            },
            .set_var_ref0, .set_var_ref1, .set_var_ref2, .set_var_ref3, .set_var_ref => {
                const bytecode_idx: u16 = switch (instr.opcode) {
                    .set_var_ref0 => 0,
                    .set_var_ref1 => 1,
                    .set_var_ref2 => 2,
                    .set_var_ref3 => 3,
                    else => instr.operand.var_ref,
                };
                try self.flushVstack();
                try self.printLine("zig_runtime.setClosureVarSafe(ctx, var_refs, {d}, closure_var_count, stack[sp-1].toJSValueWithCtx(ctx));", .{bytecode_idx});
            },

            // New class-related opcodes
            .define_method_computed => {
                try self.flushVstack();
                try self.writeLine("{");
                try self.writeLine("  const _method = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("  const _key = stack[sp-2].toJSValueWithCtx(ctx);");
                try self.writeLine("  const _obj = stack[sp-3].toJSValueWithCtx(ctx);");
                try self.writeLine("  const _atom = zig_runtime.quickjs.JS_ValueToAtom(ctx, _key);");
                try self.writeLine("  _ = JSValue.definePropertyValueAtom(ctx, _obj, _atom, _method, JSValue.JS_PROP_C_W_E);");
                try self.writeLine("  zig_runtime.quickjs.JS_FreeAtom(ctx, _atom);");
                try self.writeLine("  JSValue.free(ctx, _key);");
                try self.writeLine("  sp -= 2;");
                try self.writeLine("}");
            },
            .set_name_computed => {
                try self.flushVstack();
                try self.writeLine("{");
                try self.writeLine("  const _func = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("  const _name = stack[sp-2].toJSValueWithCtx(ctx);");
                try self.writeLine("  _ = JSValue.definePropertyValueStr(ctx, _func, \"name\", JSValue.dup(ctx, _name), JSValue.JS_PROP_CONFIGURABLE);");
                try self.writeLine("}");
            },
            .define_class => {
                try self.flushVstack();
                const atom = instr.operand.atom_u8.atom;
                try self.writeLine("{");
                try self.writeLine("  const _fields = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("  const _parent = stack[sp-2].toJSValueWithCtx(ctx);");
                try self.writeLine("  var _proto: JSValue = undefined;");
                try self.writeLine("  if (_parent.isUndefined()) {");
                try self.writeLine("    _proto = zig_runtime.quickjs.JS_NewObject(ctx);");
                try self.writeLine("  } else {");
                try self.writeLine("    const _parent_proto = JSValue.getPropertyStr(ctx, _parent, \"prototype\");");
                try self.writeLine("    _proto = zig_runtime.quickjs.JS_NewObjectProto(ctx, _parent_proto);");
                try self.writeLine("    JSValue.free(ctx, _parent_proto);");
                try self.writeLine("  }");
                try self.writeLine("  const _ctor = if (_fields.isFunction()) _fields else zig_runtime.quickjs.JS_NewObject(ctx);");
                try self.writeLine("  _ = JSValue.definePropertyValueStr(ctx, _ctor, \"prototype\", JSValue.dup(ctx, _proto), 0);");
                try self.printLine("  _ = JSValue.definePropertyValueStr(ctx, _ctor, \"name\", JSValue.newAtomString(ctx, {d}), JSValue.JS_PROP_CONFIGURABLE);", .{atom});
                try self.writeLine("  _ = JSValue.definePropertyValueStr(ctx, _proto, \"constructor\", JSValue.dup(ctx, _ctor), JSValue.JS_PROP_C_W_E);");
                try self.writeLine("  stack[sp-2] = CV.fromJSValue(_ctor);");
                try self.writeLine("  stack[sp-1] = CV.fromJSValue(_proto);");
                try self.writeLine("}");
            },
            .set_home_object => {
                try self.flushVstack();
                try self.writeLine("{");
                try self.writeLine("  const _func = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("  const _home = stack[sp-2].toJSValueWithCtx(ctx);");
                try self.writeLine("  zig_runtime.quickjs.JS_SetHomeObject(ctx, _func, _home);");
                try self.writeLine("}");
            },

            // call_constructor: new Ctor(args...)
            // Stack layout: [constructor, new.target, arg0, arg1, ...] with sp pointing after last arg
            .call_constructor => {
                const argc = instr.operand.u16;
                try self.flushVstack();
                try self.writeLine("{");
                self.pushIndent();
                // Constructor is at sp - 2 - argc (before new.target and all args)
                try self.printLine("const _ctor = stack[sp - 2 - {d}].toJSValueWithCtx(ctx);", .{argc});
                if (argc > 0) {
                    try self.printLine("var _args: [{d}]zig_runtime.JSValue = undefined;", .{argc});
                    // Args are at sp - argc, sp - argc + 1, ..., sp - 1
                    for (0..argc) |i| {
                        try self.printLine("_args[{d}] = stack[sp - {d}].toJSValueWithCtx(ctx);", .{ i, argc - i });
                    }
                    try self.printLine("const _result = JSValue.callConstructor(ctx, _ctor, &_args);", .{});
                } else {
                    try self.writeLine("const _result = JSValue.callConstructor(ctx, _ctor, &[_]zig_runtime.JSValue{});");
                }
                // Pop constructor, new.target, and all args (argc + 2 total)
                try self.printLine("sp -= {d};", .{argc + 2});
                if (self.dispatch_mode) {
                    try self.writeLine("if (_result.isException()) return .{ .return_value = _result };");
                } else {
                    try self.writeLine("if (_result.isException()) return _result;");
                }
                try self.writeLine("stack[sp] = CV.fromJSValue(_result);");
                try self.writeLine("sp += 1;");
                self.popIndent();
                try self.writeLine("}");
            },

            // put_arg: modify argument in arg_shadow (NOT argv - that's caller's stack!)
            .put_arg0, .put_arg1, .put_arg2, .put_arg3, .put_arg => {
                const arg_idx: u16 = switch (instr.opcode) {
                    .put_arg0 => 0,
                    .put_arg1 => 1,
                    .put_arg2 => 2,
                    .put_arg3 => 3,
                    else => instr.operand.arg,
                };
                try self.flushVstack();
                // NOTE: We do NOT modify argv - that's the caller's stack and modifying it
                // corrupts the caller when called from QuickJS's native interpreter.
                // arg_shadow is our own local storage for modified arguments.
                // Must free old value first to prevent memory leaks when arguments are reassigned in loops
                if (self.has_put_arg) {
                    try self.printLine("{{ const old = arg_shadow[{d}]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); arg_shadow[{d}] = stack[sp-1]; sp -= 1; }}", .{ arg_idx, arg_idx });
                } else {
                    try self.writeLine("sp -= 1; // put_arg without shadow - value discarded");
                }
            },

            // set_arg: modify argument in arg_shadow (NOT argv!), keep value on stack
            .set_arg0, .set_arg1, .set_arg2, .set_arg3, .set_arg => {
                const arg_idx: u16 = switch (instr.opcode) {
                    .set_arg0 => 0,
                    .set_arg1 => 1,
                    .set_arg2 => 2,
                    .set_arg3 => 3,
                    else => instr.operand.arg,
                };
                try self.flushVstack();
                // NOTE: We do NOT modify argv - that's the caller's stack and modifying it
                // corrupts the caller when called from QuickJS's native interpreter.
                // Must free old value first to prevent memory leaks
                if (self.has_put_arg) {
                    // Free old, dup new (since we're keeping it on stack AND in arg_shadow)
                    try self.printLine("{{ const old = arg_shadow[{d}]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); const _v = stack[sp-1]; arg_shadow[{d}] = if (_v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, _v.toJSValueWithCtx(ctx))) else _v; }}", .{ arg_idx, arg_idx });
                }
                // set_arg keeps the value on the stack. Push to vstack so subsequent
                // operations know there's a value available. Use arg_shadow since
                // that's the canonical source after the store.
                if (self.has_put_arg) {
                    try self.vpushFmt("arg_shadow[{d}]", .{arg_idx});
                } else {
                    try self.vpush("stack[sp-1]");
                }
            },

            // Async/await support with full state machine
            // await: Save execution state, create resume callback, attach to promise
            .await => {
                try self.flushVstack();
                try self.writeLine("{");
                self.pushIndent();

                if (self.is_async_function) {
                    // Full async state machine support
                    try self.writeLine("const _awaited_cv = stack[sp - 1];");
                    try self.writeLine("sp -= 1;");
                    try self.writeLine("const _awaited = _awaited_cv.toJSValueWithCtx(ctx);");
                    try self.writeLine("");

                    // Convert to promise
                    try self.writeLine("const _promise = zig_runtime.js_value.toPromise(ctx, _awaited);");
                    try self.writeLine("");

                    // Determine the next block (where to resume after await)
                    const next_block = block.successors.items[0];
                    try self.printLine("const _resume_block: u32 = {d};", .{next_block});
                    try self.writeLine("");

                    // Save state for resumption
                    try self.writeLine("// Save execution state for resumption");
                    try self.printLine("const _state = zig_runtime.FrozenAsyncState.create(", .{});
                    try self.writeLine("    @import(\"std\").heap.c_allocator,");
                    try self.writeLine("    ctx,");
                    try self.writeLine("    0, // func_id - not used for single function");
                    try self.writeLine("    _resume_block,");
                    try self.writeLine("    stack[0..sp],");
                    try self.printLine("    locals[0..{d}],", .{self.func.var_count});
                    try self.writeLine(") catch return JSValue.throwOutOfMemory(ctx);");
                    try self.writeLine("");

                    // Create the resume callback using JS function that calls back into this function
                    try self.writeLine("// Create resume callback");
                    try self.writeLine("const _state_ptr_val = JSValue.newInt64(ctx, @intCast(@intFromPtr(_state)));");
                    try self.writeLine("const _this_func = zig_runtime.quickjs.JS_GetActiveFunction(ctx);");
                    try self.writeLine("");

                    // Create a callback function that calls this function with state pointer
                    try self.writeLine("// Build callback: (resolved) => thisFunc(statePtr, resolved)");
                    try self.writeLine("const _callback = zig_runtime.createAsyncResumeCallback(ctx, _this_func, _state_ptr_val);");
                    try self.writeLine("if (_callback.isException()) {");
                    self.pushIndent();
                    try self.writeLine("_state.destroy();");
                    try self.writeLine("return _callback;");
                    self.popIndent();
                    try self.writeLine("}");
                    try self.writeLine("");

                    // Attach callback to promise
                    try self.writeLine("// Attach .then() handler to awaited promise");
                    try self.writeLine("const _chained = zig_runtime.js_value.promiseThen(ctx, _promise, _callback, JSValue.UNDEFINED);");
                    try self.writeLine("JSValue.free(ctx, _callback);");
                    try self.writeLine("JSValue.free(ctx, _promise);");
                    try self.writeLine("_ = _chained; // Result handled by callback");
                    try self.writeLine("");

                    // Return the async function's promise (created in prologue)
                    try self.writeLine("// Return the async function's promise");
                    try self.writeLine("return if (_async_promise.isUndefined()) JSValue.UNDEFINED else _async_promise;");
                } else {
                    // Non-async function with await - simple passthrough
                    try self.writeLine("const _awaited_cv = stack[sp - 1];");
                    try self.writeLine("sp -= 1;");
                    try self.writeLine("const _awaited = _awaited_cv.toJSValueWithCtx(ctx);");
                    try self.writeLine("const _promise = zig_runtime.js_value.toPromise(ctx, _awaited);");
                    try self.writeLine("return _promise;");
                }

                self.popIndent();
                try self.writeLine("}");
                self.block_terminated = true;
            },

            // return_async: Return from async function - resolve the promise
            .return_async => {
                try self.flushVstack();

                if (self.is_async_function) {
                    // Async function - resolve the promise
                    try self.writeLine("{");
                    self.pushIndent();
                    try self.writeLine("const _ret_cv = stack[sp - 1];");
                    try self.writeLine("const _ret_val = _ret_cv.toJSValueWithCtx(ctx);");
                    try self.writeLine("");

                    // If this is a resume call, the callback handles the resolution
                    // If this is the initial call, resolve the promise we created
                    try self.writeLine("if (_async_state == null and !_async_resolving_funcs[0].isUndefined()) {");
                    self.pushIndent();
                    try self.writeLine("// Resolve the async function's promise");
                    try self.writeLine("var _resolve_args = [_]JSValue{_ret_val};");
                    try self.writeLine("_ = zig_runtime.quickjs.JS_Call(ctx, _async_resolving_funcs[0], JSValue.UNDEFINED, 1, &_resolve_args);");
                    try self.writeLine("JSValue.free(ctx, _async_resolving_funcs[0]);");
                    try self.writeLine("JSValue.free(ctx, _async_resolving_funcs[1]);");
                    self.popIndent();
                    try self.writeLine("}");
                    try self.writeLine("");

                    // Return the promise or the value (for resume case)
                    try self.writeLine("return if (_async_promise.isUndefined()) _ret_val else _async_promise;");
                    self.popIndent();
                    try self.writeLine("}");
                } else if (self.has_put_arg) {
                    const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
                    try self.writeLine("{");
                    try self.writeLine("    const _ret_cv = stack[sp - 1];");
                    try self.writeLine("    const _ret_val = if (_ret_cv.isRefType()) JSValue.dup(ctx, _ret_cv.toJSValueWithCtx(ctx)) else _ret_cv.toJSValueWithCtx(ctx);");
                    for (0..arg_count) |i| {
                        try self.printLine("    if (arg_shadow[{d}].isRefType()) JSValue.free(ctx, arg_shadow[{d}].toJSValueWithCtx(ctx));", .{ i, i });
                    }
                    if (self.dispatch_mode) {
                        try self.writeLine("    return .{ .return_value = _ret_val };");
                    } else {
                        try self.writeLine("    return _ret_val;");
                    }
                    try self.writeLine("}");
                } else {
                    if (self.dispatch_mode) {
                        try self.writeLine("return .{ .return_value = stack[sp - 1].toJSValueWithCtx(ctx) };");
                    } else {
                        try self.writeLine("return stack[sp - 1].toJSValueWithCtx(ctx);");
                    }
                }
                self.block_terminated = true;
            },

            // Unsupported opcodes throw runtime error
            else => {
                const op_byte = @intFromEnum(instr.opcode);
                try self.printLine("// UNSUPPORTED: opcode {d} ({s}) - throw error", .{ op_byte, @tagName(instr.opcode) });
                if (self.dispatch_mode) {
                    try self.writeLine("return .{ .return_value = zig_runtime.JSValue.throwTypeError(ctx, \"Unsupported opcode in frozen function\") };");
                } else {
                    try self.writeLine("return zig_runtime.JSValue.throwTypeError(ctx, \"Unsupported opcode in frozen function\");");
                }
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
        // For Relooper, always flush vstack first, then use real stack positions
        // This handles cross-block values correctly
        try self.flushVstack();

        // Bytecode stack layout for call: [func, arg0, arg1, ..., argN-1]
        // After flush: values are at stack[sp - argc - 1] (func) through stack[sp - 1] (last arg)
        try self.writeLine("{");
        self.pushIndent();

        // Get function from real stack position
        try self.printLine("const _fn = stack[sp - {d}].toJSValueWithCtx(ctx);", .{argc + 1});

        if (argc > 0) {
            // Build args array from real stack positions
            try self.printLine("var _args: [{d}]JSValue = undefined;", .{argc});
            try self.printLine("for (0..{d}) |_i| {{ _args[_i] = CV.toJSValuePtr(&stack[sp - {d} + _i]); }}", .{ argc, argc });
            try self.writeLine("const _result = JSValue.call(ctx, _fn, zig_runtime.JSValue.UNDEFINED, @intCast(_args.len), &_args);");
        } else {
            try self.writeLine("const _result = JSValue.call(ctx, _fn, zig_runtime.JSValue.UNDEFINED, 0, @as([*]zig_runtime.JSValue, undefined));");
        }

        // Check for exception and propagate it
        try self.writeLine("if (_result.isException()) return zig_runtime.JSValue.EXCEPTION;");

        // Pop func + args, push result
        try self.printLine("sp -= {d};", .{argc + 1});
        try self.writeLine("stack[sp] = CV.fromJSValue(_result); sp += 1;");
        self.popIndent();
        try self.writeLine("}");
    }

    fn emitCallMethod(self: *Self, argc: u16) !void {
        // call_method: the method is already on the stack from get_field2
        // Stack: [this, method, arg0, arg1, ...argN-1] -> [result]
        // For Relooper, always flush vstack first, then use real stack positions
        try self.flushVstack();

        try self.writeLine("{");
        self.pushIndent();

        // Get this and method from real stack positions
        // Stack layout: [obj, method, arg0, arg1, ...]
        try self.printLine("const _obj = stack[sp - {d}].toJSValueWithCtx(ctx);", .{argc + 2});
        try self.printLine("const _method = stack[sp - {d}].toJSValueWithCtx(ctx);", .{argc + 1});

        if (argc > 0) {
            // Build args array from real stack positions
            try self.printLine("var _args: [{d}]JSValue = undefined;", .{argc});
            try self.printLine("for (0..{d}) |_i| {{ _args[_i] = CV.toJSValuePtr(&stack[sp - {d} + _i]); }}", .{ argc, argc });
            try self.writeLine("const _result = JSValue.call(ctx, _method, _obj, @intCast(_args.len), &_args);");
        } else {
            try self.writeLine("const _result = JSValue.call(ctx, _method, _obj, 0, @as([*]zig_runtime.JSValue, undefined));");
        }

        // Check for exception and propagate it
        try self.writeLine("if (_result.isException()) return zig_runtime.JSValue.EXCEPTION;");

        // Pop obj + method + args, push result
        try self.printLine("sp -= {d};", .{argc + 2});
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

    /// Find the position of a bytecode var_ref index in the closure_var_indices array.
    /// The bytecode uses indices like 0, 1, 2, 3 for get_var_ref0/1/2/3, but the var_refs
    /// array passed at runtime only contains the subset of closure variables actually
    /// captured by this function. This maps bytecode index to var_refs array position.
    pub fn findClosureVarPosition(self: *Self, bytecode_idx: u16) usize {
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
            // Count stack references that need to be consumed
            const ref_count = self.countStackRefs(expr);
            if (ref_count > 0) {
                // Expression references stack values - store result then adjust sp
                // This replaces the referenced values with the computed result
                try self.printLine("{{ const _tmp = {s}; sp -= {d}; stack[sp] = _tmp; sp += 1; }}", .{ expr, ref_count });
            } else {
                try self.printLine("stack[sp] = {s}; sp += 1;", .{expr});
            }
            if (self.isAllocated(expr)) self.allocator.free(expr);
        }
    }

    /// Check if an expression is "stable" - meaning it can be safely re-evaluated
    /// across block boundaries without depending on local state.
    fn isStableExpr(expr: []const u8) bool {
        // Closure var access is stable - var_refs doesn't change during execution
        if (std.mem.indexOf(u8, expr, "getClosureVar")) |_| {
            return true;
        }
        // Constants are stable
        if (std.mem.startsWith(u8, expr, "CV.newInt(") or
            std.mem.startsWith(u8, expr, "CV.newFloat(") or
            std.mem.startsWith(u8, expr, "CV.TRUE") or
            std.mem.startsWith(u8, expr, "CV.FALSE") or
            std.mem.startsWith(u8, expr, "CV.NULL") or
            std.mem.startsWith(u8, expr, "CV.UNDEFINED"))
        {
            return true;
        }
        return false;
    }

    /// Flush vstack but keep stable expressions (like closure vars) for cross-block use
    pub fn flushVstackKeepStable(self: *Self) !void {
        var i: usize = 0;
        while (i < self.vstack.items.len) {
            const expr = self.vstack.items[i];
            if (isStableExpr(expr)) {
                // Keep stable expressions - they can be re-evaluated in other blocks
                i += 1;
            } else {
                // Flush non-stable expression to real stack
                _ = self.vstack.orderedRemove(i);
                const ref_count = self.countStackRefs(expr);
                if (ref_count > 0) {
                    try self.printLine("{{ const _tmp = {s}; sp -= {d}; stack[sp] = _tmp; sp += 1; }}", .{ expr, ref_count });
                } else {
                    try self.printLine("stack[sp] = {s}; sp += 1;", .{expr});
                }
                if (self.isAllocated(expr)) self.allocator.free(expr);
                // Don't increment i - next item shifted into current position
            }
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
        // vstack empty - check if there are base stack values we can reference
        if (self.base_stack_depth > 0) {
            // Generate a reference to the topmost base stack value
            // offset = 1 + number already popped from base
            // First pop: offset = 1 -> stack[sp - 1] (the actual top)
            // Second pop: offset = 2 -> stack[sp - 2]
            const offset = 1 + self.base_popped_count;
            const ref = std.fmt.allocPrint(self.allocator, "stack[sp - {d}]", .{offset}) catch @panic("OOM");
            self.base_stack_depth -= 1;
            self.base_popped_count += 1;
            return ref;
        }
        return null;
    }

    pub fn vpeek(self: *Self) ?[]const u8 {
        if (self.vstack.items.len > 0) {
            return self.vstack.items[self.vstack.items.len - 1];
        }
        return null;
    }

    pub fn vstackLen(self: *Self) usize {
        return self.vstack.items.len;
    }

    pub fn vstackGetAt(self: *Self, idx: usize) ?[]const u8 {
        if (idx < self.vstack.items.len) {
            return self.vstack.items[idx];
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

    // Block buffer methods for dispatch table mode (writes to module-level block_functions)
    fn writeIndentBlock(self: *Self) !void {
        var i: u32 = 0;
        while (i < self.block_fn_indent) : (i += 1) {
            try self.block_functions.appendSlice(self.allocator, "    ");
        }
    }

    fn writeLineBlock(self: *Self, line: []const u8) !void {
        try self.writeIndentBlock();
        try self.block_functions.appendSlice(self.allocator, line);
        try self.block_functions.append(self.allocator, '\n');
    }

    fn printLineBlock(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        try self.writeIndentBlock();
        try self.block_functions.ensureUnusedCapacity(self.allocator, 256);
        try self.block_functions.writer(self.allocator).print(fmt, args);
        try self.block_functions.append(self.allocator, '\n');
    }

    fn pushIndentBlock(self: *Self) void {
        self.block_fn_indent += 1;
    }

    fn popIndentBlock(self: *Self) void {
        if (self.block_fn_indent > 0) self.block_fn_indent -= 1;
    }

    // ========================================================================
    // Dispatch Table Mode Support
    // ========================================================================

    fn getFuncPrefix(self: *const Self) []const u8 {
        return self.func.name;
    }

    /// Emit the BlockContext struct for dispatch table mode.
    fn emitBlockContextStruct(self: *Self) !void {
        const prefix = self.getFuncPrefix();
        const min_locals = if (self.func.var_count >= 16) self.func.var_count else 16;
        try self.printLineBlock("const __frozen_{s}_BlockContext = struct {{", .{prefix});
        self.pushIndentBlock();
        try self.writeLineBlock("ctx: *zig_runtime.JSContext,");
        try self.writeLineBlock("this_val: zig_runtime.JSValue,");
        try self.writeLineBlock("argc: c_int,");
        try self.writeLineBlock("argv: [*]zig_runtime.JSValue,");
        try self.writeLineBlock("var_refs: ?[*]*zig_runtime.JSVarRef,");
        try self.writeLineBlock("closure_var_count: c_int,");
        try self.writeLineBlock("cpool: ?[*]zig_runtime.JSValue,");
        try self.writeLineBlock("stack: *[256]zig_runtime.CompressedValue,");
        try self.writeLineBlock("sp: *usize,");
        try self.printLineBlock("locals: *[{d}]zig_runtime.CompressedValue,", .{min_locals});
        try self.writeLineBlock("for_of_iter_stack: *[8]usize,");
        try self.writeLineBlock("for_of_depth: *usize,");
        // Add arg_shadow if function has put_arg
        if (self.has_put_arg) {
            const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
            if (arg_count > 0) {
                try self.printLineBlock("arg_shadow: *[{d}]zig_runtime.CompressedValue,", .{arg_count});
            }
        }
        self.popIndentBlock();
        try self.writeLineBlock("};");
        try self.writeLineBlock("");
    }

    /// Emit the BlockResult tagged union.
    fn emitBlockResultType(self: *Self) !void {
        const prefix = self.getFuncPrefix();
        try self.printLineBlock("const __frozen_{s}_BlockResult = union(enum) {{", .{prefix});
        self.pushIndentBlock();
        try self.writeLineBlock("next_block: u32,");
        try self.writeLineBlock("return_value: zig_runtime.JSValue,");
        try self.writeLineBlock("return_undef: void,");
        try self.writeLineBlock("exception: void,");
        self.popIndentBlock();
        try self.writeLineBlock("};");
        try self.writeLineBlock("");
    }

    /// Emit the BlockFn type alias.
    fn emitBlockFnType(self: *Self) !void {
        const prefix = self.getFuncPrefix();
        try self.printLineBlock("const __frozen_{s}_BlockFn = *const fn (*__frozen_{s}_BlockContext) __frozen_{s}_BlockResult;", .{ prefix, prefix, prefix });
        try self.writeLineBlock("");
    }

    /// Emit the dispatch table array.
    fn emitDispatchTable(self: *Self, block_count: usize) !void {
        const prefix = self.getFuncPrefix();
        try self.printLineBlock("const __frozen_{s}_block_table = [_]__frozen_{s}_BlockFn{{", .{ prefix, prefix });
        self.pushIndentBlock();
        for (0..block_count) |i| {
            try self.printLineBlock("__frozen_{s}_block_{d},", .{ prefix, i });
        }
        self.popIndentBlock();
        try self.writeLineBlock("};");
        try self.writeLineBlock("");
    }

    /// Emit the dispatch loop.
    fn emitDispatchLoop(self: *Self, block_count: usize) !void {
        const prefix = self.getFuncPrefix();
        try self.writeLine("var block_id: u32 = 0;");
        try self.writeLine("while (true) {");
        self.pushIndent();
        try self.printLine("if (block_id >= {d}) return zig_runtime.JSValue.UNDEFINED;", .{block_count});
        try self.printLine("switch (__frozen_{s}_block_table[block_id](&bctx)) {{", .{prefix});
        self.pushIndent();
        try self.writeLine(".next_block => |n| block_id = n,");
        try self.writeLine(".return_value => |v| return v,");
        try self.writeLine(".return_undef => return zig_runtime.JSValue.UNDEFINED,");
        try self.writeLine(".exception => return zig_runtime.JSValue.EXCEPTION,");
        self.popIndent();
        try self.writeLine("}");
        self.popIndent();
        try self.writeLine("}");
    }

    /// Emit block context initialization for dispatch mode.
    fn emitBlockContextInit(self: *Self) !void {
        const prefix = self.getFuncPrefix();
        const min_locals = if (self.func.var_count >= 16) self.func.var_count else 16;
        try self.printLine("var bctx = __frozen_{s}_BlockContext{{", .{prefix});
        self.pushIndent();
        try self.writeLine(".ctx = ctx,");
        try self.writeLine(".this_val = this_val,");
        try self.writeLine(".argc = argc,");
        try self.writeLine(".argv = argv,");
        try self.writeLine(".var_refs = var_refs,");
        try self.writeLine(".closure_var_count = closure_var_count,");
        try self.writeLine(".cpool = cpool,");
        try self.writeLine(".stack = &stack,");
        try self.writeLine(".sp = &sp,");
        try self.printLine(".locals = @as(*[{d}]zig_runtime.CompressedValue, &locals),", .{min_locals});
        try self.writeLine(".for_of_iter_stack = &for_of_iter_stack,");
        try self.writeLine(".for_of_depth = &for_of_depth,");
        if (self.has_put_arg) {
            const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
            if (arg_count > 0) {
                try self.printLine(".arg_shadow = @as(*[{d}]zig_runtime.CompressedValue, &arg_shadow),", .{arg_count});
            }
        }
        self.popIndent();
        try self.writeLine("};");
        try self.writeLine("_ = &bctx;");
        try self.writeLine("");
    }

    /// Emit a block as a standalone function for dispatch table mode.
    fn emitBlockAsFunction(self: *Self, block: BasicBlock, block_idx: u32) !void {
        const prefix = self.getFuncPrefix();

        // Function header
        try self.printLineBlock("fn __frozen_{s}_block_{d}(bctx: *__frozen_{s}_BlockContext) __frozen_{s}_BlockResult {{", .{ prefix, block_idx, prefix, prefix });
        self.pushIndentBlock();

        // Alias context fields
        try self.writeLineBlock("const ctx = bctx.ctx;");
        try self.writeLineBlock("const this_val = bctx.this_val;");
        try self.writeLineBlock("const argc = bctx.argc;");
        try self.writeLineBlock("const argv = bctx.argv;");
        try self.writeLineBlock("const var_refs = bctx.var_refs;");
        try self.writeLineBlock("const closure_var_count = bctx.closure_var_count;");
        try self.writeLineBlock("const cpool = bctx.cpool;");
        try self.writeLineBlock("const stack = bctx.stack;");
        try self.writeLineBlock("var sp = bctx.sp.*;");
        try self.writeLineBlock("const locals = bctx.locals;");
        try self.writeLineBlock("const for_of_iter_stack = bctx.for_of_iter_stack;");
        try self.writeLineBlock("var for_of_depth = bctx.for_of_depth.*;");
        try self.writeLineBlock("defer bctx.sp.* = sp;");
        try self.writeLineBlock("defer bctx.for_of_depth.* = for_of_depth;");
        try self.writeLineBlock("const CV = zig_runtime.CompressedValue;");
        // Silence unused warnings
        try self.writeLineBlock("std.debug.assert(@intFromPtr(ctx) | @intFromPtr(&this_val) | @intFromPtr(argv) | @intFromPtr(var_refs) | @intFromPtr(cpool) | @intFromPtr(stack) | @intFromPtr(locals) | @intFromPtr(for_of_iter_stack) != 0);");
        try self.writeLineBlock("std.debug.assert(argc >= 0 and closure_var_count >= 0);");
        try self.writeLineBlock("std.debug.assert(sp < 256 and for_of_depth < 8);");
        try self.writeLineBlock("sp = sp; for_of_depth = for_of_depth;");
        if (self.has_put_arg) {
            const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
            if (arg_count > 0) {
                try self.writeLineBlock("const arg_shadow = bctx.arg_shadow;");
                try self.writeLineBlock("std.debug.assert(@intFromPtr(arg_shadow) != 0);");
            }
        }
        try self.writeLineBlock("std.debug.assert(@sizeOf(CV) > 0);");
        try self.writeLineBlock("");

        // Check if this block is a switch chain block
        if (self.switch_chain_blocks.contains(block_idx)) {
            try self.printLineBlock("return .{{ .next_block = {d} }};", .{block_idx});
            self.popIndentBlock();
            try self.writeLineBlock("}");
            try self.writeLineBlock("");
            return;
        }

        // Check if this block starts a switch pattern
        if (self.switch_patterns.get(block_idx)) |pattern| {
            try self.emitSwitchBlockDispatch(block, block_idx, pattern);
            self.popIndentBlock();
            try self.writeLineBlock("}");
            try self.writeLineBlock("");
            return;
        }

        // Clear vstack and reset base stack tracking for this block
        for (self.vstack.items) |expr| {
            if (self.isAllocated(expr)) self.allocator.free(expr);
        }
        self.vstack.clearRetainingCapacity();
        self.base_stack_depth = 0;
        self.base_popped_count = 0;
        self.block_terminated = false;

        // Temporarily swap output to block_functions buffer
        const saved_output = self.output;
        const saved_indent = self.indent_level;
        self.output = self.block_functions;
        self.indent_level = self.block_fn_indent;

        // Emit each instruction
        for (block.instructions, 0..) |instr, idx| {
            try self.emitInstruction(instr, block, idx);
            if (self.block_terminated) break;
        }

        // Emit block terminator for dispatch mode
        if (!self.block_terminated) {
            try self.emitBlockTerminatorDispatch(block);
        }

        // Restore output buffer
        self.block_functions = self.output;
        self.block_fn_indent = self.indent_level;
        self.output = saved_output;
        self.indent_level = saved_indent;

        self.popIndentBlock();
        try self.writeLineBlock("}");
        try self.writeLineBlock("");
    }

    /// Emit block terminator for dispatch mode (returns BlockResult instead of continue).
    fn emitBlockTerminatorDispatch(self: *Self, block: BasicBlock) !void {
        const successors = block.successors.items;
        const last_op = if (block.instructions.len > 0)
            block.instructions[block.instructions.len - 1].opcode
        else
            .nop;

        // Return instructions already handled in emitInstruction
        if (last_op == .@"return" or last_op == .return_undef) {
            return;
        }

        // Conditional branches
        if (last_op == .if_false or last_op == .if_false8) {
            if (successors.len >= 2) {
                const false_target = successors[0];
                const true_target = successors[1];
                try self.flushVstack();
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const _cond = stack[sp - 1];");
                try self.writeLine("sp -= 1;");
                try self.printLine("if (_cond.toBoolWithCtx(ctx)) return .{{ .next_block = {d} }} else return .{{ .next_block = {d} }};", .{ true_target, false_target });
                self.popIndent();
                try self.writeLine("}");
            } else if (successors.len == 1) {
                try self.printLine("return .{{ .next_block = {d} }};", .{successors[0]});
            }
            return;
        }

        if (last_op == .if_true or last_op == .if_true8) {
            if (successors.len >= 2) {
                const true_target = successors[0];
                const false_target = successors[1];
                try self.flushVstack();
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const _cond = stack[sp - 1];");
                try self.writeLine("sp -= 1;");
                try self.printLine("if (_cond.toBoolWithCtx(ctx)) return .{{ .next_block = {d} }} else return .{{ .next_block = {d} }};", .{ true_target, false_target });
                self.popIndent();
                try self.writeLine("}");
            } else if (successors.len == 1) {
                try self.printLine("return .{{ .next_block = {d} }};", .{successors[0]});
            }
            return;
        }

        // Unconditional jump or fall-through
        if (successors.len > 0) {
            try self.flushVstack();
            try self.printLine("return .{{ .next_block = {d} }};", .{successors[0]});
        } else {
            try self.writeLine("return .return_undef;");
        }
    }

    /// Emit a switch block in dispatch mode.
    fn emitSwitchBlockDispatch(self: *Self, block: BasicBlock, block_idx: u32, pattern: SwitchPattern) !void {
        _ = block_idx;

        // Temporarily swap output
        const saved_output = self.output;
        const saved_indent = self.indent_level;
        self.output = self.block_functions;
        self.indent_level = self.block_fn_indent;

        // Emit instructions before switch (up to dup)
        for (self.vstack.items) |expr| {
            if (self.isAllocated(expr)) self.allocator.free(expr);
        }
        self.vstack.clearRetainingCapacity();
        self.base_stack_depth = 0;
        self.base_popped_count = 0;
        self.block_terminated = false;

        // Find and emit instructions before the switch pattern
        const instrs = block.instructions;
        for (instrs, 0..) |instr, idx| {
            if (instr.opcode == .dup and idx + 3 < instrs.len) {
                break;
            }
            try self.emitInstruction(instr, block, idx);
        }

        try self.flushVstack();

        // Emit switch on discriminant
        try self.writeLine("{");
        self.pushIndent();
        try self.writeLine("const _switch_val = stack[sp - 1];");
        try self.writeLine("sp -= 1;");
        try self.writeLine("const _switch_jsv = _switch_val.toJSValue();");
        try self.writeLine("const _switch_int: i64 = if (_switch_jsv.isInt()) _switch_jsv.getInt() else @intFromFloat(_switch_jsv.getNumberAsFloat());");
        try self.writeLine("switch (_switch_int) {");
        self.pushIndent();

        for (pattern.cases.items) |case| {
            try self.printLine("{d} => return .{{ .next_block = {d} }},", .{ case.value, case.target_block });
        }
        try self.printLine("else => return .{{ .next_block = {d} }},", .{pattern.default_block});

        self.popIndent();
        try self.writeLine("}");
        self.popIndent();
        try self.writeLine("}");

        // Restore output
        self.block_functions = self.output;
        self.block_fn_indent = self.indent_level;
        self.output = saved_output;
        self.indent_level = saved_indent;
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

