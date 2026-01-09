//! Full Zig Code Generator for Frozen Functions
//!
//! Generates Zig code for ALL freezable functions (not just pure-int).
//! Uses switch-based block dispatch for control flow.
//!
//! Architecture:
//!   bytecode → CFG → Zig source code → LLVM → native
//!
//! Generated code pattern:
//!   fn __frozen_foo(ctx: *JSContext, this: JSValue, argc: c_int, argv: [*]JSValue) JSValue {
//!       var block_id: u32 = 0;
//!       var stack: [256]JSValue = undefined;
//!       var sp: usize = 0;
//!       while (true) {
//!           switch (block_id) {
//!               0 => { ... },
//!               1 => { return stack[sp - 1]; },
//!               else => unreachable,
//!           }
//!       }
//!   }

const std = @import("std");
const opcodes = @import("opcodes.zig");
const parser = @import("bytecode_parser.zig");
const cfg_mod = @import("cfg_builder.zig");

const Opcode = opcodes.Opcode;
const Instruction = parser.Instruction;
const BasicBlock = cfg_mod.BasicBlock;
const CFG = cfg_mod.CFG;
const Allocator = std.mem.Allocator;

// Global counter for tracking unsupported opcodes during code generation
// Used for discovering which opcodes need to be implemented
pub var unsupported_opcode_counts: [256]usize = [_]usize{0} ** 256;

/// Print report of unsupported opcodes
pub fn printUnsupportedOpcodeReport() void {
    var total: usize = 0;
    for (unsupported_opcode_counts) |c| total += c;
    if (total == 0) return;

    std.debug.print("\n=== Unsupported Opcode Report ===\n", .{});
    std.debug.print("Total unsupported opcode occurrences: {d}\n\n", .{total});

    // Collect non-zero opcodes
    const OpCount = struct { op: u8, count: usize };
    var items: [256]OpCount = undefined;
    var count: usize = 0;
    for (0..256) |i| {
        if (unsupported_opcode_counts[i] > 0) {
            items[count] = .{ .op = @intCast(i), .count = unsupported_opcode_counts[i] };
            count += 1;
        }
    }

    // Simple bubble sort for non-zero items (typically small count)
    const slice = items[0..count];
    for (0..count) |i| {
        for (i + 1..count) |j| {
            if (slice[i].count < slice[j].count) {
                const tmp = slice[i];
                slice[i] = slice[j];
                slice[j] = tmp;
            }
        }
    }

    std.debug.print("Top unsupported opcodes:\n", .{});
    const show_count = @min(30, count);
    for (slice[0..show_count]) |item| {
        // Try to get the enum name, fall back to numeric value for invalid/extended opcodes
        const name = if (item.op < 256) blk: {
            const op_enum: opcodes.Opcode = @enumFromInt(item.op);
            break :blk @tagName(op_enum);
        } else "unknown";
        std.debug.print("  {s:30}: {d:10}\n", .{ name, item.count });
    }
    std.debug.print("=================================\n\n", .{});
}

/// Reset unsupported opcode counts
pub fn resetUnsupportedOpcodeCounts() void {
    for (&unsupported_opcode_counts) |*c| c.* = 0;
}

pub const CodeGenError = error{
    UnsupportedOpcode,
    StackUnderflow,
    OutOfMemory,
    FormatError,
    InvalidBlock,
};

/// Information about a function to generate
pub const FunctionInfo = struct {
    /// Function name
    name: []const u8,
    /// Number of arguments
    arg_count: u16,
    /// Number of local variables
    var_count: u16,
    /// Control flow graph
    cfg: *const CFG,
    /// Is this a self-recursive function?
    is_self_recursive: bool,
    /// Closure variable indices
    closure_var_indices: []const u16 = &.{},
    /// Atom strings for property names
    atom_strings: []const []const u8 = &.{},
    /// Enable partial freeze (emit fallback for contaminated blocks)
    partial_freeze: bool = false,
    /// Original JS function name (for fallback registration)
    js_name: []const u8 = &.{},
};

/// Full Zig code generator
pub const ZigCodeGen = struct {
    allocator: Allocator,
    output: std.ArrayListUnmanaged(u8),
    func: FunctionInfo,
    indent: usize,
    /// Track which frozen functions exist (for direct calls)
    frozen_functions: []const []const u8,

    const Self = @This();

    pub fn init(allocator: Allocator, func: FunctionInfo) Self {
        return .{
            .allocator = allocator,
            .output = .{},
            .func = func,
            .indent = 0,
            .frozen_functions = &.{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.output.deinit(self.allocator);
    }

    /// Set list of frozen function names for direct call optimization
    pub fn setFrozenFunctions(self: *Self, names: []const []const u8) void {
        self.frozen_functions = names;
    }

    // ========================================================================
    // Output Helpers
    // ========================================================================

    fn write(self: *Self, str: []const u8) !void {
        try self.output.appendSlice(self.allocator, str);
    }

    fn writeLine(self: *Self, str: []const u8) !void {
        try self.writeIndent();
        try self.output.appendSlice(self.allocator, str);
        try self.output.append(self.allocator, '\n');
    }

    fn print(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        try self.output.writer(self.allocator).print(fmt, args);
    }

    fn printLine(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        try self.writeIndent();
        try self.output.writer(self.allocator).print(fmt, args);
        try self.output.append(self.allocator, '\n');
    }

    fn writeIndent(self: *Self) !void {
        for (0..self.indent) |_| {
            try self.output.appendSlice(self.allocator, "    ");
        }
    }

    fn pushIndent(self: *Self) void {
        self.indent += 1;
    }

    fn popIndent(self: *Self) void {
        if (self.indent > 0) self.indent -= 1;
    }

    // ========================================================================
    // Code Generation Entry Point
    // ========================================================================

    /// Generate complete Zig function
    pub fn generate(self: *Self) ![]const u8 {
        // Function signature
        try self.emitSignature();

        self.pushIndent();

        // Stack overflow check
        try self.writeLine("if (zig_runtime.checkStack()) {");
        self.pushIndent();
        try self.writeLine("return JSValue.throwRangeError(ctx, \"Maximum call stack size exceeded\");");
        self.popIndent();
        try self.writeLine("}");
        try self.writeLine("defer zig_runtime.exitStack();");
        try self.writeLine("");

        // Local variables
        try self.emitLocals();
        try self.writeLine("");

        // Stack
        try self.writeLine("var stack: [256]JSValue = undefined;");
        try self.writeLine("var sp: usize = 0;");
        try self.writeLine("");

        // Block dispatch
        try self.writeLine("var block_id: u32 = 0;");
        try self.writeLine("while (true) {");
        self.pushIndent();
        try self.writeLine("switch (block_id) {");
        self.pushIndent();

        // Generate each block
        const blocks = self.func.cfg.blocks.items;
        for (blocks, 0..) |block, idx| {
            try self.emitBlock(block, @intCast(idx));
        }

        // Default case
        try self.writeLine("else => unreachable,");

        self.popIndent();
        try self.writeLine("}");
        self.popIndent();
        try self.writeLine("}");

        self.popIndent();
        try self.writeLine("}");

        return try self.output.toOwnedSlice(self.allocator);
    }

    // ========================================================================
    // Function Signature
    // ========================================================================

    fn emitSignature(self: *Self) !void {
        try self.print(
            \\pub fn __frozen_{s}(ctx: *zig_runtime.JSContext, this_val: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue) zig_runtime.JSValue {{
            \\
        , .{self.func.name});
        try self.printLine("    _ = this_val;", .{});
    }

    // ========================================================================
    // Local Variables
    // ========================================================================

    fn emitLocals(self: *Self) !void {
        if (self.func.var_count > 0) {
            try self.printLine("var locals: [{d}]JSValue = .{{JSValue.UNDEFINED}} ** {d};", .{ self.func.var_count, self.func.var_count });
        }
    }

    // ========================================================================
    // Block Generation
    // ========================================================================

    fn emitBlock(self: *Self, block: BasicBlock, block_idx: u32) !void {
        try self.printLine("{d} => {{ // block_{d}", .{ block_idx, block_idx });
        self.pushIndent();

        // Check if block is contaminated (has never_freeze opcodes)
        if (self.func.partial_freeze and block.is_contaminated) {
            // Emit fallback to interpreter for contaminated block
            const reason = block.contamination_reason orelse "unknown";
            try self.printLine("// CONTAMINATED BLOCK - reason: {s}", .{reason});
            const js_name = if (self.func.js_name.len > 0) self.func.js_name else self.func.name;
            try self.writeLine("{");
            self.pushIndent();
            try self.writeLine("var next_block_fallback: usize = 0;");
            try self.writeLine("const fallback_result = zig_runtime.blockFallback(");
            self.pushIndent();
            try self.printLine("ctx, \"{s}\", this_val, argc, argv,", .{js_name});
            try self.printLine("&locals, {d}, &stack, &sp, {d}, &next_block_fallback);", .{ self.func.var_count, block_idx });
            self.popIndent();
            try self.writeLine("if (!fallback_result.isUndefined()) {");
            self.pushIndent();
            try self.writeLine("// Function completed in interpreter");
            try self.writeLine("return fallback_result;");
            self.popIndent();
            try self.writeLine("}");
            try self.writeLine("// Continue to next block from interpreter");
            try self.writeLine("block_id = @intCast(next_block_fallback);");
            try self.writeLine("continue;");
            self.popIndent();
            try self.writeLine("}");
        } else {
            // Clean block - emit native Zig instructions
            for (block.instructions) |instr| {
                try self.emitInstruction(instr);
            }

            // Block terminator (jump to successor blocks)
            const successors = block.successors.items;
            if (successors.len == 2) {
                // Conditional branch (2 successors: true path, false path)
                try self.writeLine("const cond = stack[sp - 1];");
                try self.writeLine("sp -= 1;");
                try self.printLine("if (JSValue.toBool(ctx, cond) != 0) {{ block_id = {d}; continue; }}", .{successors[0]});
                try self.printLine("block_id = {d}; continue;", .{successors[1]});
            } else if (successors.len == 1) {
                // Unconditional jump
                try self.printLine("block_id = {d}; continue;", .{successors[0]});
            }
            // If no successor, block ends with return (handled by return opcode)
        }

        self.popIndent();
        try self.writeLine("},");
    }

    // ========================================================================
    // Instruction Emission
    // ========================================================================

    fn emitInstruction(self: *Self, instr: Instruction) !void {
        switch (instr.opcode) {
            // Constants
            .push_0 => try self.writeLine("stack[sp] = JSValue.newInt(0); sp += 1;"),
            .push_1 => try self.writeLine("stack[sp] = JSValue.newInt(1); sp += 1;"),
            .push_i32 => try self.printLine("stack[sp] = JSValue.newInt({d}); sp += 1;", .{instr.operand.i32}),
            .push_i8 => try self.printLine("stack[sp] = JSValue.newInt({d}); sp += 1;", .{instr.operand.i8}),
            .push_true => try self.writeLine("stack[sp] = JSValue.TRUE; sp += 1;"),
            .push_false => try self.writeLine("stack[sp] = JSValue.FALSE; sp += 1;"),
            .null => try self.writeLine("stack[sp] = JSValue.NULL; sp += 1;"),
            .undefined => try self.writeLine("stack[sp] = JSValue.UNDEFINED; sp += 1;"),

            // Stack operations
            .dup => try self.writeLine("stack[sp] = JSValue.dup(ctx, stack[sp - 1]); sp += 1;"),
            .drop => {
                try self.writeLine("JSValue.free(ctx, stack[sp - 1]); sp -= 1;");
            },
            .swap => {
                try self.writeLine("{ const tmp = stack[sp - 1]; stack[sp - 1] = stack[sp - 2]; stack[sp - 2] = tmp; }");
            },

            // Local variables
            .get_loc => {
                const idx = instr.operand.loc;
                try self.printLine("stack[sp] = JSValue.dup(ctx, locals[{d}]); sp += 1;", .{idx});
            },
            .put_loc => {
                const idx = instr.operand.loc;
                try self.printLine("JSValue.free(ctx, locals[{d}]); locals[{d}] = stack[sp - 1]; sp -= 1;", .{ idx, idx });
            },
            .set_loc => {
                const idx = instr.operand.loc;
                try self.printLine("JSValue.free(ctx, locals[{d}]); locals[{d}] = JSValue.dup(ctx, stack[sp - 1]);", .{ idx, idx });
            },
            .get_loc0 => try self.writeLine("stack[sp] = JSValue.dup(ctx, locals[0]); sp += 1;"),
            .get_loc1 => try self.writeLine("stack[sp] = JSValue.dup(ctx, locals[1]); sp += 1;"),
            .get_loc2 => try self.writeLine("stack[sp] = JSValue.dup(ctx, locals[2]); sp += 1;"),
            .get_loc3 => try self.writeLine("stack[sp] = JSValue.dup(ctx, locals[3]); sp += 1;"),
            .put_loc0 => try self.writeLine("JSValue.free(ctx, locals[0]); locals[0] = stack[sp - 1]; sp -= 1;"),
            .put_loc1 => try self.writeLine("JSValue.free(ctx, locals[1]); locals[1] = stack[sp - 1]; sp -= 1;"),
            .put_loc2 => try self.writeLine("JSValue.free(ctx, locals[2]); locals[2] = stack[sp - 1]; sp -= 1;"),
            .put_loc3 => try self.writeLine("JSValue.free(ctx, locals[3]); locals[3] = stack[sp - 1]; sp -= 1;"),

            // Arguments
            .get_arg => {
                const idx = instr.operand.arg;
                try self.printLine("stack[sp] = if ({d} < argc) JSValue.dup(ctx, argv[{d}]) else JSValue.UNDEFINED; sp += 1;", .{ idx, idx });
            },
            .get_arg0 => try self.writeLine("stack[sp] = if (0 < argc) JSValue.dup(ctx, argv[0]) else JSValue.UNDEFINED; sp += 1;"),
            .get_arg1 => try self.writeLine("stack[sp] = if (1 < argc) JSValue.dup(ctx, argv[1]) else JSValue.UNDEFINED; sp += 1;"),
            .get_arg2 => try self.writeLine("stack[sp] = if (2 < argc) JSValue.dup(ctx, argv[2]) else JSValue.UNDEFINED; sp += 1;"),
            .get_arg3 => try self.writeLine("stack[sp] = if (3 < argc) JSValue.dup(ctx, argv[3]) else JSValue.UNDEFINED; sp += 1;"),

            // Arithmetic
            .add => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.add(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),
            .sub => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.sub(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),
            .mul => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.mul(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),
            .div => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.div(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),
            .mod => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.mod(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),
            .neg => try self.writeLine("{ const a = stack[sp-1]; stack[sp-1] = zig_runtime.neg(ctx, a); JSValue.free(ctx, a); }"),

            // Comparisons
            .lt => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.lt(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),
            .lte => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.lte(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),
            .gt => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.gt(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),
            .gte => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.gte(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),
            .eq => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.eq(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),
            .neq => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.neq(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),

            // Bitwise
            .@"and" => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.bitAnd(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),
            .@"or" => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.bitOr(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),
            .xor => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.bitXor(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),
            .not => try self.writeLine("{ const a = stack[sp-1]; stack[sp-1] = zig_runtime.bitNot(ctx, a); JSValue.free(ctx, a); }"),
            .shl => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.shl(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),
            .sar => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.sar(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),
            .shr => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.shr(ctx, a, b); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }"),

            // Control flow (jumps handled by block terminators)
            .if_false, .if_true, .if_false8, .if_true8, .goto, .goto8, .goto16 => {
                // These are handled by block successor logic, not individual instructions
            },

            // Return
            .@"return" => {
                try self.writeLine("const result = stack[sp - 1];");
                try self.writeLine("// Free remaining stack and locals");
                try self.writeLine("for (0..sp - 1) |i| JSValue.free(ctx, stack[i]);");
                if (self.func.var_count > 0) {
                    try self.writeLine("for (&locals) |*loc| JSValue.free(ctx, loc.*);");
                }
                try self.writeLine("return result;");
            },
            .return_undef => {
                try self.writeLine("// Free remaining stack and locals");
                try self.writeLine("for (0..sp) |i| JSValue.free(ctx, stack[i]);");
                if (self.func.var_count > 0) {
                    try self.writeLine("for (&locals) |*loc| JSValue.free(ctx, loc.*);");
                }
                try self.writeLine("return JSValue.UNDEFINED;");
            },

            // Increment/Decrement
            .inc => {
                try self.writeLine("{ const a = stack[sp-1]; stack[sp-1] = zig_runtime.add(ctx, a, JSValue.newInt(1)); JSValue.free(ctx, a); }");
            },
            .dec => {
                try self.writeLine("{ const a = stack[sp-1]; stack[sp-1] = zig_runtime.sub(ctx, a, JSValue.newInt(1)); JSValue.free(ctx, a); }");
            },

            // ================================================================
            // Closure Variables
            // Closure vars are passed as extra array at argv[argc]
            // closure_var_indices maps bytecode index -> position in array
            // ================================================================

            // get_var_ref0-3: push closure variable
            .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3 => {
                // Get bytecode index from opcode (0-3)
                const bytecode_idx: u16 = switch (instr.opcode) {
                    .get_var_ref0 => 0,
                    .get_var_ref1 => 1,
                    .get_var_ref2 => 2,
                    .get_var_ref3 => 3,
                    else => unreachable,
                };
                // Find position in closure vars array
                const pos = self.findClosureVarPosition(bytecode_idx);
                if (pos) |p| {
                    try self.printLine("stack[sp] = zig_runtime.getClosureVar(ctx, argv, argc, {d}); sp += 1;", .{p});
                } else if (self.func.is_self_recursive and bytecode_idx == 0) {
                    // Self-reference - push undefined for now (will be handled by call optimization)
                    try self.writeLine("// get_var_ref0: self-reference (handled by call optimization)");
                    try self.writeLine("stack[sp] = JSValue.UNDEFINED; sp += 1;");
                } else {
                    // Fallback to undefined if not in closure_var_indices
                    try self.printLine("// get_var_ref{d}: not found in closure_var_indices, using undefined", .{bytecode_idx});
                    try self.writeLine("stack[sp] = JSValue.UNDEFINED; sp += 1;");
                }
            },

            // ================================================================
            // Property Access
            // ================================================================

            // get_field: pop obj, push obj.prop
            .get_field => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.func.atom_strings.len) {
                    const prop_name = self.func.atom_strings[atom_idx];
                    try self.printLine("{{ const obj = stack[sp-1]; stack[sp-1] = JSValue.getPropertyStr(ctx, obj, \"{s}\"); JSValue.free(ctx, obj); }}", .{prop_name});
                } else {
                    try self.writeLine("// get_field: atom index out of range");
                    try self.writeLine("return JSValue.throwTypeError(ctx, \"Invalid property access\");");
                }
            },

            // get_field2: pop obj, push obj, obj.prop (keeps object on stack)
            .get_field2 => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.func.atom_strings.len) {
                    const prop_name = self.func.atom_strings[atom_idx];
                    try self.printLine("{{ const obj = stack[sp-1]; stack[sp] = JSValue.getPropertyStr(ctx, obj, \"{s}\"); sp += 1; }}", .{prop_name});
                } else {
                    try self.writeLine("// get_field2: atom index out of range");
                    try self.writeLine("return JSValue.throwTypeError(ctx, \"Invalid property access\");");
                }
            },

            // put_field: pop val, pop obj, obj.prop = val
            .put_field => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.func.atom_strings.len) {
                    const prop_name = self.func.atom_strings[atom_idx];
                    try self.printLine("{{ const val = stack[sp-1]; const obj = stack[sp-2]; _ = JSValue.setPropertyStr(ctx, obj, \"{s}\", val); JSValue.free(ctx, obj); sp -= 2; }}", .{prop_name});
                } else {
                    try self.writeLine("// put_field: atom index out of range");
                    try self.writeLine("return JSValue.throwTypeError(ctx, \"Invalid property access\");");
                }
            },

            // get_array_el: pop idx, pop arr, push arr[idx]
            .get_array_el => {
                try self.writeLine("{ const idx = stack[sp-1]; const arr = stack[sp-2]; var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, idx); stack[sp-2] = JSValue.getPropertyUint32(ctx, arr, @intCast(idx_i32)); JSValue.free(ctx, arr); JSValue.free(ctx, idx); sp -= 1; }");
            },

            // get_array_el2: pop idx, pop arr, push arr, push arr[idx]
            .get_array_el2 => {
                try self.writeLine("{ const idx = stack[sp-1]; const arr = stack[sp-2]; var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, idx); stack[sp-1] = JSValue.getPropertyUint32(ctx, arr, @intCast(idx_i32)); JSValue.free(ctx, idx); }");
            },

            // put_array_el: pop val, pop idx, pop arr, arr[idx] = val
            .put_array_el => {
                try self.writeLine("{ const val = stack[sp-1]; const idx = stack[sp-2]; const arr = stack[sp-3]; var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, idx); _ = JSValue.setPropertyUint32(ctx, arr, @intCast(idx_i32), val); JSValue.free(ctx, arr); JSValue.free(ctx, idx); sp -= 3; }");
            },

            // ================================================================
            // Function Calls
            // ================================================================

            // call0: pop func, push result (0 args)
            .call0 => {
                try self.emitCall(0);
            },

            // call1: pop arg, pop func, push result (1 arg)
            .call1 => {
                try self.emitCall(1);
            },

            // call2: pop arg2, pop arg1, pop func, push result (2 args)
            .call2 => {
                try self.emitCall(2);
            },

            // call3: pop arg3, pop arg2, pop arg1, pop func, push result (3 args)
            .call3 => {
                try self.emitCall(3);
            },

            // call: pop argc args, pop func, push result
            .call => {
                const argc = instr.operand.u16;
                try self.emitCall(argc);
            },

            // call_method: pop argc args, pop this, pop method, push result
            .call_method => {
                const argc = instr.operand.u16;
                try self.emitCallMethod(argc);
            },

            // ================================================================
            // Additional High-Frequency Opcodes (discovered from TSC analysis)
            // ================================================================

            // get_loc_check: get local with TDZ (Temporal Dead Zone) check
            .get_loc_check => {
                const idx = instr.operand.loc;
                try self.printLine("{{ const v = locals[{d}]; if (v.isUninitialized()) return JSValue.throwReferenceError(ctx, \"Cannot access before initialization\"); stack[sp] = JSValue.dup(ctx, v); sp += 1; }}", .{idx});
            },

            // get_var: get variable by name from scope
            .get_var => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.func.atom_strings.len) {
                    const var_name = self.func.atom_strings[atom_idx];
                    try self.printLine("stack[sp] = JSValue.getGlobal(ctx, \"{s}\"); sp += 1;", .{var_name});
                } else {
                    try self.writeLine("return JSValue.throwReferenceError(ctx, \"Variable not found\");");
                }
            },

            // push_atom_value: push atom as value (usually for property names/strings)
            .push_atom_value => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.func.atom_strings.len) {
                    const str_val = self.func.atom_strings[atom_idx];
                    try self.printLine("stack[sp] = JSValue.newString(ctx, \"{s}\"); sp += 1;", .{str_val});
                } else {
                    try self.writeLine("stack[sp] = JSValue.UNDEFINED; sp += 1;");
                }
            },

            // set_loc_uninitialized: set local to uninitialized state (for TDZ)
            .set_loc_uninitialized => {
                const idx = instr.operand.loc;
                try self.printLine("locals[{d}] = JSValue.UNINITIALIZED;", .{idx});
            },

            // get_var_ref_check: get closure var with TDZ check
            .get_var_ref_check => {
                const bytecode_idx = instr.operand.var_ref;
                const pos = self.findClosureVarPosition(bytecode_idx);
                if (pos) |p| {
                    try self.printLine("stack[sp] = zig_runtime.getClosureVarCheck(ctx, argv, argc, {d}); sp += 1;", .{p});
                    try self.writeLine("if (stack[sp-1].isException()) return stack[sp-1];");
                } else {
                    try self.writeLine("// get_var_ref_check: not in closure_var_indices");
                    try self.writeLine("stack[sp] = JSValue.UNDEFINED; sp += 1;");
                }
            },

            // get_var_ref: get closure var by index (generic version)
            .get_var_ref => {
                const bytecode_idx = instr.operand.var_ref;
                const pos = self.findClosureVarPosition(bytecode_idx);
                if (pos) |p| {
                    try self.printLine("stack[sp] = zig_runtime.getClosureVar(ctx, argv, argc, {d}); sp += 1;", .{p});
                } else {
                    try self.printLine("// get_var_ref {d}: not in closure_var_indices", .{bytecode_idx});
                    try self.writeLine("stack[sp] = JSValue.UNDEFINED; sp += 1;");
                }
            },

            // put_var_ref0-3: set closure variable
            .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3 => {
                const bytecode_idx: u16 = switch (instr.opcode) {
                    .put_var_ref0 => 0,
                    .put_var_ref1 => 1,
                    .put_var_ref2 => 2,
                    .put_var_ref3 => 3,
                    else => unreachable,
                };
                const pos = self.findClosureVarPosition(bytecode_idx);
                if (pos) |p| {
                    try self.printLine("sp -= 1; zig_runtime.setClosureVar(ctx, argv, argc, {d}, stack[sp]);", .{p});
                } else {
                    try self.printLine("// put_var_ref{d}: not in closure_var_indices, discarding value", .{bytecode_idx});
                    try self.writeLine("sp -= 1; JSValue.free(ctx, stack[sp]);");
                }
            },

            // put_var_ref: set closure var by index (generic version)
            .put_var_ref => {
                const bytecode_idx = instr.operand.var_ref;
                const pos = self.findClosureVarPosition(bytecode_idx);
                if (pos) |p| {
                    try self.printLine("sp -= 1; zig_runtime.setClosureVar(ctx, argv, argc, {d}, stack[sp]);", .{p});
                } else {
                    try self.printLine("// put_var_ref {d}: not in closure_var_indices, discarding value", .{bytecode_idx});
                    try self.writeLine("sp -= 1; JSValue.free(ctx, stack[sp]);");
                }
            },

            // put_var_ref_check: set closure var with TDZ check
            .put_var_ref_check => {
                const bytecode_idx = instr.operand.var_ref;
                const pos = self.findClosureVarPosition(bytecode_idx);
                if (pos) |p| {
                    try self.writeLine("{");
                    self.pushIndent();
                    try self.writeLine("sp -= 1;");
                    try self.printLine("const err = zig_runtime.setClosureVarCheck(ctx, argv, argc, {d}, stack[sp]);", .{p});
                    try self.writeLine("if (err) return JSValue.EXCEPTION;");
                    self.popIndent();
                    try self.writeLine("}");
                } else {
                    try self.printLine("// put_var_ref_check {d}: not in closure_var_indices, discarding value", .{bytecode_idx});
                    try self.writeLine("sp -= 1; JSValue.free(ctx, stack[sp]);");
                }
            },

            // set_var_ref0-3: same as put_var_ref but for set semantics
            .set_var_ref0, .set_var_ref1, .set_var_ref2, .set_var_ref3 => {
                const bytecode_idx: u16 = switch (instr.opcode) {
                    .set_var_ref0 => 0,
                    .set_var_ref1 => 1,
                    .set_var_ref2 => 2,
                    .set_var_ref3 => 3,
                    else => unreachable,
                };
                const pos = self.findClosureVarPosition(bytecode_idx);
                if (pos) |p| {
                    try self.printLine("sp -= 1; zig_runtime.setClosureVar(ctx, argv, argc, {d}, stack[sp]);", .{p});
                } else {
                    try self.printLine("// set_var_ref{d}: not in closure_var_indices, discarding value", .{bytecode_idx});
                    try self.writeLine("sp -= 1; JSValue.free(ctx, stack[sp]);");
                }
            },

            // set_var_ref: generic set closure var
            .set_var_ref => {
                const bytecode_idx = instr.operand.var_ref;
                const pos = self.findClosureVarPosition(bytecode_idx);
                if (pos) |p| {
                    try self.printLine("sp -= 1; zig_runtime.setClosureVar(ctx, argv, argc, {d}, stack[sp]);", .{p});
                } else {
                    try self.printLine("// set_var_ref {d}: not in closure_var_indices, discarding value", .{bytecode_idx});
                    try self.writeLine("sp -= 1; JSValue.free(ctx, stack[sp]);");
                }
            },

            // put_var_ref_check_init: initialize closure var (for TDZ)
            .put_var_ref_check_init => {
                const bytecode_idx = instr.operand.var_ref;
                const pos = self.findClosureVarPosition(bytecode_idx);
                if (pos) |p| {
                    try self.printLine("sp -= 1; zig_runtime.setClosureVar(ctx, argv, argc, {d}, stack[sp]);", .{p});
                } else {
                    try self.printLine("// put_var_ref_check_init {d}: not in closure_var_indices, discarding value", .{bytecode_idx});
                    try self.writeLine("sp -= 1; JSValue.free(ctx, stack[sp]);");
                }
            },

            // push_this: push current 'this' value
            .push_this => {
                try self.writeLine("stack[sp] = JSValue.dup(ctx, this_val); sp += 1;");
            },

            // define_field: define a field on object (for object literals)
            .define_field => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.func.atom_strings.len) {
                    const field_name = self.func.atom_strings[atom_idx];
                    try self.printLine("{{ const val = stack[sp-1]; const obj = stack[sp-2]; _ = JSValue.definePropertyStr(ctx, obj, \"{s}\", val); sp -= 1; }}", .{field_name});
                } else {
                    try self.writeLine("return JSValue.throwTypeError(ctx, \"Invalid field name\");");
                }
            },

            // fclosure: create closure (8-bit index variant)
            .fclosure8 => {
                try self.writeLine("// fclosure8: closure creation - not yet implemented");
                try self.writeLine("stack[sp] = JSValue.UNDEFINED; sp += 1;");
            },

            // call_constructor: call function as constructor (new X())
            .call_constructor => {
                const argc = instr.operand.u16;
                try self.emitCallConstructor(argc);
            },

            // object: create empty object
            .object => {
                try self.writeLine("stack[sp] = JSValue.newObject(ctx); sp += 1;");
            },

            // strict_eq: strict equality (===)
            .strict_eq => {
                try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = JSValue.newBool(JSValue.strictEq(a, b)); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }");
            },

            // strict_neq: strict inequality (!==)
            .strict_neq => {
                try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = JSValue.newBool(!JSValue.strictEq(a, b)); JSValue.free(ctx, a); JSValue.free(ctx, b); sp -= 1; }");
            },

            // tail_call_method: tail call optimization for method calls
            .tail_call_method => {
                const argc = instr.operand.u16;
                // For now, emit as regular call_method followed by return
                try self.emitCallMethod(argc);
                try self.writeLine("return stack[sp - 1];");
            },

            // get_loc8: get local (8-bit index, common variant)
            .get_loc8 => {
                const idx = instr.operand.loc;
                try self.printLine("stack[sp] = JSValue.dup(ctx, locals[{d}]); sp += 1;", .{idx});
            },

            // put_loc8: put local (8-bit index, common variant)
            .put_loc8 => {
                const idx = instr.operand.loc;
                try self.printLine("{{ const prev = locals[{d}]; locals[{d}] = stack[sp-1]; JSValue.free(ctx, prev); sp -= 1; }}", .{ idx, idx });
            },

            // put_loc_check: put local with TDZ check
            .put_loc_check => {
                const idx = instr.operand.loc;
                try self.printLine("{{ const prev = locals[{d}]; locals[{d}] = stack[sp-1]; if (!prev.isUninitialized()) JSValue.free(ctx, prev); sp -= 1; }}", .{ idx, idx });
            },

            // get_length: get .length property (optimized)
            .get_length => {
                try self.writeLine("{ const obj = stack[sp-1]; stack[sp-1] = JSValue.getPropertyStr(ctx, obj, \"length\"); JSValue.free(ctx, obj); }");
            },

            // push_i16: push 16-bit integer
            .push_i16 => {
                const val = instr.operand.i16;
                try self.printLine("stack[sp] = JSValue.newInt({d}); sp += 1;", .{val});
            },

            // push_2: push literal 2
            .push_2 => {
                try self.writeLine("stack[sp] = JSValue.newInt(2); sp += 1;");
            },

            // array_from: create array from stack elements
            .array_from => {
                const count = instr.operand.u16;
                try self.emitArrayFrom(count);
            },

            // to_propkey: convert to property key
            .to_propkey => {
                try self.writeLine("// to_propkey: convert TOS to property key (no-op for strings/numbers)");
            },

            // to_propkey2: convert second item to property key
            .to_propkey2 => {
                try self.writeLine("// to_propkey2: convert second stack item to property key");
            },

            // throw: throw exception
            .throw => {
                try self.writeLine("{ const exc = stack[sp-1]; sp -= 1; return JSValue.throw(ctx, exc); }");
            },

            // typeof: get type string
            .typeof => {
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = JSValue.typeOf(ctx, v); JSValue.free(ctx, v); }");
            },

            // lnot: logical not
            .lnot => {
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = JSValue.newBool(!JSValue.toBool(ctx, v)); JSValue.free(ctx, v); }");
            },

            // typeof_is_function: check if typeof == "function"
            .typeof_is_function => {
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = JSValue.newBool(JSValue.isFunction(v)); JSValue.free(ctx, v); }");
            },

            // typeof_is_undefined: check if typeof == "undefined"
            .typeof_is_undefined => {
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = JSValue.newBool(v.isUndefined()); JSValue.free(ctx, v); }");
            },

            // special_object: create special object (arguments, etc.)
            .special_object => {
                try self.writeLine("// special_object: creating special object");
                try self.writeLine("stack[sp] = JSValue.newObject(ctx); sp += 1;");
            },

            // check_ctor: check if in constructor context
            .check_ctor => {
                try self.writeLine("// check_ctor: constructor context check (no-op for now)");
            },

            // check_ctor_return: check constructor return value
            .check_ctor_return => {
                try self.writeLine("// check_ctor_return: checking constructor return");
            },

            // push_empty_string: push ""
            .push_empty_string => {
                try self.writeLine("stack[sp] = JSValue.newString(ctx, \"\"); sp += 1;");
            },

            // is_undefined_or_null: check if undefined or null
            .is_undefined_or_null => {
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = JSValue.newBool(v.isUndefined() or v.isNull()); }");
            },

            // set_name: set function name
            .set_name => {
                try self.writeLine("// set_name: setting function name (no-op for frozen)");
            },

            // ================================================================
            // Additional Opcodes (second batch from discovery)
            // ================================================================

            // tail_call: tail call optimization (treat as regular call + return)
            .tail_call => {
                const argc = instr.operand.u16;
                try self.emitCall(argc);
                try self.writeLine("return stack[sp - 1];");
            },

            // is_undefined: check if value is undefined
            .is_undefined => {
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = JSValue.newBool(v.isUndefined()); }");
            },

            // is_null: check if value is null
            .is_null => {
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = JSValue.newBool(v.isNull()); }");
            },

            // put_loc_check_init: put local with TDZ check for initialization
            .put_loc_check_init => {
                const idx = instr.operand.loc;
                try self.printLine("{{ locals[{d}] = stack[sp-1]; sp -= 1; }}", .{idx});
            },

            // post_inc: post-increment (x++)
            .post_inc => {
                try self.writeLine("{ const v = stack[sp-1]; var val: i32 = 0; _ = JSValue.toInt32(ctx, &val, v); stack[sp-1] = v; stack[sp] = JSValue.newInt(val + 1); sp += 1; }");
            },

            // post_dec: post-decrement (x--)
            .post_dec => {
                try self.writeLine("{ const v = stack[sp-1]; var val: i32 = 0; _ = JSValue.toInt32(ctx, &val, v); stack[sp-1] = v; stack[sp] = JSValue.newInt(val - 1); sp += 1; }");
            },

            // put_arg0-3: put argument
            .put_arg0 => {
                try self.writeLine("{ if (argc > 0) { JSValue.free(ctx, argv[0]); argv[0] = stack[sp-1]; } sp -= 1; }");
            },
            .put_arg1 => {
                try self.writeLine("{ if (argc > 1) { JSValue.free(ctx, argv[1]); argv[1] = stack[sp-1]; } sp -= 1; }");
            },
            .put_arg2 => {
                try self.writeLine("{ if (argc > 2) { JSValue.free(ctx, argv[2]); argv[2] = stack[sp-1]; } sp -= 1; }");
            },
            .put_arg3 => {
                try self.writeLine("{ if (argc > 3) { JSValue.free(ctx, argv[3]); argv[3] = stack[sp-1]; } sp -= 1; }");
            },

            // set_arg0-3: set argument (like put but keeps on stack)
            .set_arg0 => {
                try self.writeLine("{ if (argc > 0) { JSValue.free(ctx, argv[0]); argv[0] = JSValue.dup(ctx, stack[sp-1]); } }");
            },
            .set_arg1 => {
                try self.writeLine("{ if (argc > 1) { JSValue.free(ctx, argv[1]); argv[1] = JSValue.dup(ctx, stack[sp-1]); } }");
            },
            .set_arg2 => {
                try self.writeLine("{ if (argc > 2) { JSValue.free(ctx, argv[2]); argv[2] = JSValue.dup(ctx, stack[sp-1]); } }");
            },
            .set_arg3 => {
                try self.writeLine("{ if (argc > 3) { JSValue.free(ctx, argv[3]); argv[3] = JSValue.dup(ctx, stack[sp-1]); } }");
            },

            // push_3-7: push literal integers
            .push_3 => {
                try self.writeLine("stack[sp] = JSValue.newInt(3); sp += 1;");
            },
            .push_4 => {
                try self.writeLine("stack[sp] = JSValue.newInt(4); sp += 1;");
            },
            .push_5 => {
                try self.writeLine("stack[sp] = JSValue.newInt(5); sp += 1;");
            },
            .push_6 => {
                try self.writeLine("stack[sp] = JSValue.newInt(6); sp += 1;");
            },
            .push_7 => {
                try self.writeLine("stack[sp] = JSValue.newInt(7); sp += 1;");
            },
            .push_minus1 => {
                try self.writeLine("stack[sp] = JSValue.newInt(-1); sp += 1;");
            },

            // set_loc8: set local (keeps value on stack)
            .set_loc8 => {
                const idx = instr.operand.loc;
                try self.printLine("{{ const prev = locals[{d}]; locals[{d}] = JSValue.dup(ctx, stack[sp-1]); JSValue.free(ctx, prev); }}", .{ idx, idx });
            },

            // get_loc0_loc1: push both loc0 and loc1
            .get_loc0_loc1 => {
                try self.writeLine("stack[sp] = JSValue.dup(ctx, locals[0]); stack[sp+1] = JSValue.dup(ctx, locals[1]); sp += 2;");
            },

            // add_loc: add to local variable
            .add_loc => {
                const idx = instr.operand.loc;
                try self.printLine("{{ const b = stack[sp-1]; locals[{d}] = zig_runtime.add(ctx, locals[{d}], b); JSValue.free(ctx, b); sp -= 1; }}", .{ idx, idx });
            },

            // close_loc: close local variable (for closures)
            .close_loc => {
                try self.writeLine("// close_loc: closing local for closure (no-op for frozen)");
            },

            // instanceof: check instanceof
            .instanceof => {
                try self.writeLine("{ const ctor = stack[sp-1]; const obj = stack[sp-2]; stack[sp-2] = JSValue.newBool(JSValue.isInstanceOf(ctx, obj, ctor)); JSValue.free(ctx, obj); JSValue.free(ctx, ctor); sp -= 1; }");
            },

            // get_var_undef: get variable (undefined if not found)
            .get_var_undef => {
                const atom_idx = instr.operand.atom;
                if (atom_idx < self.func.atom_strings.len) {
                    const var_name = self.func.atom_strings[atom_idx];
                    try self.printLine("stack[sp] = JSValue.getGlobalUndef(ctx, \"{s}\"); sp += 1;", .{var_name});
                } else {
                    try self.writeLine("stack[sp] = JSValue.UNDEFINED; sp += 1;");
                }
            },

            // push_const8: push constant from pool (8-bit index)
            .push_const8 => {
                try self.writeLine("// push_const8: constant pool access - not yet implemented");
                try self.writeLine("stack[sp] = JSValue.UNDEFINED; sp += 1;");
            },

            // define_method: define method on class
            .define_method => {
                try self.writeLine("// define_method: class method definition");
                try self.writeLine("{ const method = stack[sp-1]; const obj = stack[sp-2]; _ = JSValue.setPropertyStr(ctx, obj, \"method\", method); sp -= 1; }");
            },

            // set_loc0-3: set local (keeps value on stack)
            .set_loc0 => {
                try self.writeLine("{ const prev = locals[0]; locals[0] = JSValue.dup(ctx, stack[sp-1]); JSValue.free(ctx, prev); }");
            },
            .set_loc1 => {
                try self.writeLine("{ const prev = locals[1]; locals[1] = JSValue.dup(ctx, stack[sp-1]); JSValue.free(ctx, prev); }");
            },
            .set_loc2 => {
                try self.writeLine("{ const prev = locals[2]; locals[2] = JSValue.dup(ctx, stack[sp-1]); JSValue.free(ctx, prev); }");
            },
            .set_loc3 => {
                try self.writeLine("{ const prev = locals[3]; locals[3] = JSValue.dup(ctx, stack[sp-1]); JSValue.free(ctx, prev); }");
            },

            // inc_loc: increment local variable
            .inc_loc => {
                const idx = instr.operand.loc;
                try self.printLine("{{ const v = locals[{d}]; var val: i32 = 0; _ = JSValue.toInt32(ctx, &val, v); JSValue.free(ctx, v); locals[{d}] = JSValue.newInt(val + 1); }}", .{ idx, idx });
            },

            // dec_loc: decrement local variable
            .dec_loc => {
                const idx = instr.operand.loc;
                try self.printLine("{{ const v = locals[{d}]; var val: i32 = 0; _ = JSValue.toInt32(ctx, &val, v); JSValue.free(ctx, v); locals[{d}] = JSValue.newInt(val - 1); }}", .{ idx, idx });
            },

            // put_arg: put argument (generic with index)
            .put_arg => {
                const idx = instr.operand.arg;
                try self.printLine("{{ if (argc > {d}) {{ JSValue.free(ctx, argv[{d}]); argv[{d}] = stack[sp-1]; }} sp -= 1; }}", .{ idx, idx, idx });
            },

            // set_arg: set argument (generic with index)
            .set_arg => {
                const idx = instr.operand.arg;
                try self.printLine("{{ if (argc > {d}) {{ JSValue.free(ctx, argv[{d}]); argv[{d}] = JSValue.dup(ctx, stack[sp-1]); }} }}", .{ idx, idx, idx });
            },

            // delete: delete property
            .delete => {
                try self.writeLine("{ const prop = stack[sp-1]; const obj = stack[sp-2]; _ = JSValue.deleteProperty(ctx, obj, prop); JSValue.free(ctx, prop); JSValue.free(ctx, obj); sp -= 2; stack[sp] = JSValue.TRUE; sp += 1; }");
            },

            // append: append to array
            .append => {
                try self.writeLine("{ const val = stack[sp-1]; const arr = stack[sp-2]; _ = JSValue.appendArray(ctx, arr, val); sp -= 1; }");
            },

            // rot3l, rot3r: rotate stack
            .rot3l => {
                try self.writeLine("{ const c = stack[sp-1]; const b = stack[sp-2]; const a = stack[sp-3]; stack[sp-3] = b; stack[sp-2] = c; stack[sp-1] = a; }");
            },
            .rot3r => {
                try self.writeLine("{ const c = stack[sp-1]; const b = stack[sp-2]; const a = stack[sp-3]; stack[sp-3] = c; stack[sp-2] = a; stack[sp-1] = b; }");
            },

            // perm3, perm4, perm5: permute stack
            .perm3 => {
                try self.writeLine("{ const c = stack[sp-1]; const b = stack[sp-2]; const a = stack[sp-3]; stack[sp-3] = a; stack[sp-2] = c; stack[sp-1] = b; }");
            },
            .perm4 => {
                try self.writeLine("{ const d = stack[sp-1]; const c = stack[sp-2]; const b = stack[sp-3]; const a = stack[sp-4]; stack[sp-4] = a; stack[sp-3] = d; stack[sp-2] = b; stack[sp-1] = c; }");
            },
            .perm5 => {
                try self.writeLine("{ const e = stack[sp-1]; const d = stack[sp-2]; const c = stack[sp-3]; const b = stack[sp-4]; const a = stack[sp-5]; stack[sp-5] = a; stack[sp-4] = e; stack[sp-3] = b; stack[sp-2] = d; stack[sp-1] = c; }");
            },

            // insert2: insert TOS at position 2 (move val down past 1 item)
            // Before: [a, b, val] -> After: [a, val, b]
            .insert2 => {
                try self.writeLine("{ const val = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = val; }");
            },

            // insert3: insert TOS at position 3 (move val down past 2 items)
            // Before: [a, b, c, val] -> After: [a, val, b, c]
            .insert3 => {
                try self.writeLine("{ const val = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = stack[sp-3]; stack[sp-3] = val; }");
            },

            // insert4: insert TOS at position 4 (move val down past 3 items)
            // Before: [a, b, c, d, val] -> After: [a, val, b, c, d]
            .insert4 => {
                try self.writeLine("{ const val = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = stack[sp-3]; stack[sp-3] = stack[sp-4]; stack[sp-4] = val; }");
            },

            // rot4l: rotate top 4 values left
            // Before: [a, b, c, d] -> After: [b, c, d, a]
            .rot4l => {
                try self.writeLine("{ const a = stack[sp-4]; stack[sp-4] = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = a; }");
            },

            // rot5l: rotate top 5 values left
            // Before: [a, b, c, d, e] -> After: [b, c, d, e, a]
            .rot5l => {
                try self.writeLine("{ const a = stack[sp-5]; stack[sp-5] = stack[sp-4]; stack[sp-4] = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = a; }");
            },

            // rest: create rest array from arguments
            .rest => {
                try self.writeLine("// rest: creating rest array from arguments");
                try self.writeLine("stack[sp] = JSValue.newArray(ctx); sp += 1;");
            },

            // to_object: convert to object
            .to_object => {
                try self.writeLine("// to_object: converting to object (no-op if already object)");
            },

            // define_array_el: define array element (for array literals)
            .define_array_el => {
                try self.writeLine("{ const val = stack[sp-1]; const idx = stack[sp-2]; const arr = stack[sp-3]; var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, idx); _ = JSValue.setPropertyUint32(ctx, arr, @intCast(idx_i32), val); JSValue.free(ctx, idx); sp -= 2; }");
            },

            // ================================================================
            // Iterator Protocol (for-of loops)
            // ================================================================

            // for_of_start: Initialize for-of iterator
            // Stack before: [obj] at sp-1
            // Stack after: [iterator, next_method, catch_offset] (sp increases by 2)
            .for_of_start => {
                try self.writeLine("{");
                self.pushIndent();
                // js_frozen_for_of_start expects &stack[sp] (pointer PAST top of stack)
                // It reads sp[-1] (obj), replaces sp[-1] with iterator, writes sp[0] with next_method
                try self.writeLine("const rc = zig_runtime.quickjs.js_frozen_for_of_start(ctx, @ptrCast(&stack[sp]), 0);");
                try self.writeLine("if (rc != 0) return JSValue.EXCEPTION;");
                try self.writeLine("sp += 1;  // next_method is now at stack[sp-1]");
                try self.writeLine("stack[sp] = zig_runtime.newCatchOffset(0);");
                try self.writeLine("sp += 1;");
                self.popIndent();
                try self.writeLine("}");
            },

            // for_of_next: Get next value from iterator
            // Stack before: [iterator, next_method, catch_offset, ...] (iterator at sp-3-offset)
            // Stack after: [..., value, done] (sp increases by 2)
            .for_of_next => {
                const offset = instr.operand.u8;
                try self.writeLine("{");
                self.pushIndent();
                // js_frozen_for_of_next expects &stack[sp] and negative offset to iterator
                // offset 0 from bytecode means iterator is at sp-3 (hence -(offset+3))
                try self.printLine("const rc = zig_runtime.quickjs.js_frozen_for_of_next(ctx, @ptrCast(&stack[sp]), -{d});", .{@as(i32, @intCast(offset)) + 3});
                try self.writeLine("if (rc != 0) return JSValue.EXCEPTION;");
                try self.writeLine("sp += 2;  // value at sp-2, done at sp-1");
                self.popIndent();
                try self.writeLine("}");
            },

            // iterator_close: Cleanup iterator after for-of loop
            // Stack before: [iterator, next_method, catch_offset] at sp-3, sp-2, sp-1
            // Stack after: [] (sp decreases by 3)
            .iterator_close => {
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("// Free catch_offset, next_method, iterator");
                try self.writeLine("sp -= 1;  // catch_offset - no need to free (primitive)");
                try self.writeLine("sp -= 1; JSValue.free(ctx, stack[sp]);  // next_method");
                try self.writeLine("sp -= 1; JSValue.free(ctx, stack[sp]);  // iterator");
                self.popIndent();
                try self.writeLine("}");
            },

            // iterator_get_value_done: Extract value and done from iterator result
            // Stack before: [result] at sp-1
            // Stack after: [value, done] (sp increases by 1)
            .iterator_get_value_done => {
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const result = stack[sp - 1];");
                try self.writeLine("const done_val = JSValue.getPropertyStr(ctx, result, \"done\");");
                try self.writeLine("const value_val = JSValue.getPropertyStr(ctx, result, \"value\");");
                try self.writeLine("JSValue.free(ctx, result);");
                try self.writeLine("stack[sp - 1] = value_val;");
                try self.writeLine("stack[sp] = done_val;");
                try self.writeLine("sp += 1;");
                self.popIndent();
                try self.writeLine("}");
            },

            // Unsupported opcodes log but don't fail - allows discovery of needed opcodes
            else => {
                // Log unsupported opcode for discovery
                const op_byte = @intFromEnum(instr.opcode);
                if (op_byte < 256) {
                    unsupported_opcode_counts[op_byte] += 1;
                }
                try self.printLine("// UNSUPPORTED: opcode {d} - fallback to interpreter", .{op_byte});
                try self.writeLine("return JSValue.throwTypeError(ctx, \"Unsupported opcode in frozen function\");");
            },
        }
    }

    // ========================================================================
    // Closure Variable Helpers
    // ========================================================================

    /// Find the position of a bytecode index in closure_var_indices
    /// Returns null if not found
    fn findClosureVarPosition(self: *Self, bytecode_idx: u16) ?usize {
        for (self.func.closure_var_indices, 0..) |cv_idx, pos| {
            if (cv_idx == bytecode_idx) {
                return pos;
            }
        }
        return null;
    }

    // ========================================================================
    // Function Call Emission
    // ========================================================================

    /// Emit code for call0-call3 and call opcodes
    /// Stack: [func, arg0, arg1, ...argN-1] -> [result]
    fn emitCall(self: *Self, argc: u16) !void {
        // For self-recursive calls, we could detect if func is __frozen_self and call directly
        // For now, use JS_Call for all calls (Phase 3 will add frozen function detection)

        if (argc == 0) {
            // call0: func is at sp-1, no args
            try self.writeLine("{");
            self.pushIndent();
            try self.writeLine("const func = stack[sp - 1];");
            try self.writeLine("const result = JSValue.call(ctx, func, JSValue.UNDEFINED, &.{});");
            try self.writeLine("JSValue.free(ctx, func);");
            try self.writeLine("stack[sp - 1] = result;");
            self.popIndent();
            try self.writeLine("}");
        } else {
            // callN: func at sp-1-argc, args at sp-argc..sp-1
            try self.writeLine("{");
            self.pushIndent();
            try self.printLine("const func = stack[sp - 1 - {d}];", .{argc});
            try self.printLine("var args: [{d}]JSValue = undefined;", .{argc});
            // Copy args from stack to args array
            for (0..argc) |i| {
                try self.printLine("args[{d}] = stack[sp - {d}];", .{ i, argc - i });
            }
            try self.writeLine("const result = JSValue.call(ctx, func, JSValue.UNDEFINED, &args);");
            // Free func and args
            try self.writeLine("JSValue.free(ctx, func);");
            for (0..argc) |i| {
                try self.printLine("JSValue.free(ctx, args[{d}]);", .{i});
            }
            try self.printLine("sp -= {d};", .{argc});
            try self.writeLine("stack[sp - 1] = result;");
            self.popIndent();
            try self.writeLine("}");
        }
    }

    /// Emit code for call_method opcode
    /// Stack: [method, this, arg0, arg1, ...argN-1] -> [result]
    fn emitCallMethod(self: *Self, argc: u16) !void {
        try self.writeLine("{");
        self.pushIndent();

        // method at sp-2-argc, this at sp-1-argc, args at sp-argc..sp-1
        try self.printLine("const method = stack[sp - 2 - {d}];", .{argc});
        try self.printLine("const this_val = stack[sp - 1 - {d}];", .{argc});

        if (argc == 0) {
            try self.writeLine("const result = JSValue.call(ctx, method, this_val, &.{});");
        } else {
            try self.printLine("var args: [{d}]JSValue = undefined;", .{argc});
            for (0..argc) |i| {
                try self.printLine("args[{d}] = stack[sp - {d}];", .{ i, argc - i });
            }
            try self.writeLine("const result = JSValue.call(ctx, method, this_val, &args);");
            // Free args
            for (0..argc) |i| {
                try self.printLine("JSValue.free(ctx, args[{d}]);", .{i});
            }
        }

        // Free method and this
        try self.writeLine("JSValue.free(ctx, method);");
        try self.writeLine("JSValue.free(ctx, this_val);");
        try self.printLine("sp -= {d} + 2;", .{argc});
        try self.writeLine("stack[sp] = result;");
        try self.writeLine("sp += 1;");

        self.popIndent();
        try self.writeLine("}");
    }

    /// Emit code for call_constructor opcode (new X())
    /// Stack: [constructor, arg0, arg1, ...argN-1] -> [new_object]
    fn emitCallConstructor(self: *Self, argc: u16) !void {
        try self.writeLine("{");
        self.pushIndent();

        // constructor at sp-1-argc, args at sp-argc..sp-1
        try self.printLine("const ctor = stack[sp - 1 - {d}];", .{argc});

        if (argc == 0) {
            try self.writeLine("const result = JSValue.callConstructor(ctx, ctor, &.{});");
        } else {
            try self.printLine("var args: [{d}]JSValue = undefined;", .{argc});
            for (0..argc) |i| {
                try self.printLine("args[{d}] = stack[sp - {d}];", .{ i, argc - i });
            }
            try self.writeLine("const result = JSValue.callConstructor(ctx, ctor, &args);");
            // Free args
            for (0..argc) |i| {
                try self.printLine("JSValue.free(ctx, args[{d}]);", .{i});
            }
        }

        // Free constructor
        try self.writeLine("JSValue.free(ctx, ctor);");
        try self.printLine("sp -= {d};", .{argc});
        try self.writeLine("stack[sp - 1] = result;");

        self.popIndent();
        try self.writeLine("}");
    }

    /// Emit code for array_from opcode
    /// Stack: [elem0, elem1, ...elemN-1] -> [array]
    fn emitArrayFrom(self: *Self, count: u16) !void {
        try self.writeLine("{");
        self.pushIndent();

        try self.writeLine("const arr = JSValue.newArray(ctx);");

        if (count > 0) {
            for (0..count) |i| {
                const stack_offset = count - i;
                try self.printLine("{{ const elem = stack[sp - {d}]; _ = JSValue.setPropertyUint32(ctx, arr, {d}, elem); }}", .{ stack_offset, i });
            }
        }

        try self.printLine("sp -= {d};", .{count});
        try self.writeLine("stack[sp] = arr;");
        try self.writeLine("sp += 1;");

        self.popIndent();
        try self.writeLine("}");
    }
};

// ============================================================================
// Module-level code generation
// ============================================================================

/// Generate a complete frozen module with all functions
pub fn generateFrozenModule(
    allocator: Allocator,
    functions: []const FunctionInfo,
    module_name: []const u8,
) ![]const u8 {
    var output = std.ArrayListUnmanaged(u8){};
    errdefer output.deinit(allocator);

    // Header
    try output.writer(allocator).print(
        \\// Auto-generated frozen module: {s}
        \\// DO NOT EDIT - Generated by EdgeBox freeze system
        \\
        \\const std = @import("std");
        \\const zig_runtime = @import("zig_runtime");
        \\const JSValue = zig_runtime.JSValue;
        \\const JSContext = zig_runtime.JSContext;
        \\
        \\
    , .{module_name});

    // Generate each function
    for (functions) |func| {
        var gen = ZigCodeGen.init(allocator, func);
        defer gen.deinit();

        const code = try gen.generate();
        defer allocator.free(code);

        try output.appendSlice(allocator, code);
        try output.append(allocator, '\n');
    }

    // Init function
    try output.writer(allocator).print(
        \\/// Register all frozen functions with QuickJS
        \\pub fn frozen_init(ctx: *JSContext) void {{
        \\    const global = zig_runtime.quickjs.JS_GetGlobalObject(ctx);
        \\    defer JSValue.free(ctx, global);
        \\
    , .{});

    for (functions) |func| {
        try output.writer(allocator).print(
            \\    // Register {s}
            \\    _ = JSValue.setPropertyStr(ctx, global, "__frozen_{s}",
            \\        // TODO: Create JS wrapper function
            \\        JSValue.UNDEFINED);
            \\
        , .{ func.name, func.name });
    }

    try output.appendSlice(allocator, "}\n");

    return try output.toOwnedSlice(allocator);
}

// ============================================================================
// Tests
// ============================================================================

test "ZigCodeGen basic structure" {
    const allocator = std.testing.allocator;

    // Create a minimal CFG with one block
    var cfg = cfg_mod.CFG.init(allocator);

    // Create a block - don't set instructions (use empty default)
    const block = BasicBlock.init(allocator, 0, 0);
    // Note: instructions will be empty slice by default
    try cfg.blocks.append(allocator, block);

    const func = FunctionInfo{
        .name = "test_func",
        .arg_count = 1,
        .var_count = 0,
        .cfg = &cfg,
        .is_self_recursive = false,
    };

    var gen = ZigCodeGen.init(allocator, func);
    defer gen.deinit();

    // Just verify it doesn't crash (block with no instructions)
    const code = try gen.generate();
    defer allocator.free(code);

    // Clean up CFG manually since we didn't allocate instructions
    for (cfg.blocks.items) |*b| {
        b.successors.deinit(allocator);
    }
    cfg.blocks.deinit(allocator);

    try std.testing.expect(code.len > 0);
    try std.testing.expect(std.mem.indexOf(u8, code, "__frozen_test_func") != null);
}

test "ZigCodeGen double function" {
    const allocator = std.testing.allocator;

    // double(x) { return x * 2; }
    // Bytecode: get_arg 0, push_i8 2, mul, return
    var instrs = [_]Instruction{
        .{ .pc = 0, .opcode = .get_arg, .operand = .{ .arg = 0 }, .size = 2 },
        .{ .pc = 2, .opcode = .push_i8, .operand = .{ .i8 = 2 }, .size = 2 },
        .{ .pc = 4, .opcode = .mul, .operand = .{ .none = {} }, .size = 1 },
        .{ .pc = 5, .opcode = .@"return", .operand = .{ .none = {} }, .size = 1 },
    };

    var cfg = cfg_mod.CFG.init(allocator);
    var block = BasicBlock.init(allocator, 0, 0);
    block.instructions = &instrs;
    block.end_pc = 6;
    try cfg.blocks.append(allocator, block);

    const func = FunctionInfo{
        .name = "double",
        .arg_count = 1,
        .var_count = 0,
        .cfg = &cfg,
        .is_self_recursive = false,
    };

    var gen = ZigCodeGen.init(allocator, func);
    defer gen.deinit();

    const code = try gen.generate();
    defer allocator.free(code);

    // Clean up
    for (cfg.blocks.items) |*b| {
        b.successors.deinit(allocator);
    }
    cfg.blocks.deinit(allocator);

    // Verify code structure
    try std.testing.expect(std.mem.indexOf(u8, code, "__frozen_double") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "get_arg") != null or std.mem.indexOf(u8, code, "argv") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "mul") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "return") != null);

    // Print generated code for debugging
    std.debug.print("\n=== Generated double(x) code ===\n{s}\n=== End ===\n", .{code});
}

test "ZigCodeGen fib with control flow" {
    const allocator = std.testing.allocator;

    // Simulate fib(n) { if (n < 2) return n; return fib(n-1) + fib(n-2); }
    // This tests: get_arg, push_i8, less_than, if_false8, return, sub, (recursive call placeholder)

    // Block 0: Check n < 2, branch to block 1 or 2
    var instrs0 = [_]Instruction{
        .{ .pc = 0, .opcode = .get_arg, .operand = .{ .arg = 0 }, .size = 2 },
        .{ .pc = 2, .opcode = .push_i8, .operand = .{ .i8 = 2 }, .size = 2 },
        .{ .pc = 4, .opcode = .lt, .operand = .{ .none = {} }, .size = 1 },
        .{ .pc = 5, .opcode = .if_false8, .operand = .{ .label = 5 }, .size = 2 }, // Jump to block 2 if false
    };

    // Block 1: Return n (base case)
    var instrs1 = [_]Instruction{
        .{ .pc = 7, .opcode = .get_arg, .operand = .{ .arg = 0 }, .size = 2 },
        .{ .pc = 9, .opcode = .@"return", .operand = .{ .none = {} }, .size = 1 },
    };

    // Block 2: Recursive case - just return 0 for now (placeholder for recursive calls)
    var instrs2 = [_]Instruction{
        .{ .pc = 10, .opcode = .push_0, .operand = .{ .implicit_int = 0 }, .size = 1 },
        .{ .pc = 11, .opcode = .@"return", .operand = .{ .none = {} }, .size = 1 },
    };

    var cfg = cfg_mod.CFG.init(allocator);

    var block0 = BasicBlock.init(allocator, 0, 0);
    block0.instructions = &instrs0;
    block0.end_pc = 7;
    try block0.successors.append(allocator, 1);
    try block0.successors.append(allocator, 2);
    try cfg.blocks.append(allocator, block0);

    var block1 = BasicBlock.init(allocator, 1, 7);
    block1.instructions = &instrs1;
    block1.end_pc = 10;
    try cfg.blocks.append(allocator, block1);

    var block2 = BasicBlock.init(allocator, 2, 10);
    block2.instructions = &instrs2;
    block2.end_pc = 12;
    try cfg.blocks.append(allocator, block2);

    const func = FunctionInfo{
        .name = "fib",
        .arg_count = 1,
        .var_count = 0,
        .cfg = &cfg,
        .is_self_recursive = true,
    };

    var gen = ZigCodeGen.init(allocator, func);
    defer gen.deinit();

    const code = try gen.generate();
    defer allocator.free(code);

    // Clean up
    for (cfg.blocks.items) |*b| {
        b.successors.deinit(allocator);
    }
    cfg.blocks.deinit(allocator);

    // Verify structure
    try std.testing.expect(std.mem.indexOf(u8, code, "__frozen_fib") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "block_id") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "0 =>") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "1 =>") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "2 =>") != null);

    // Print generated code
    std.debug.print("\n=== Generated fib(n) code ===\n{s}\n=== End ===\n", .{code});
}

test "ZigCodeGen property access - getKind" {
    const allocator = std.testing.allocator;

    // getKind(node) { return node.kind; }
    // Bytecode: get_arg 0, get_field "kind", return
    var instrs = [_]Instruction{
        .{ .pc = 0, .opcode = .get_arg, .operand = .{ .arg = 0 }, .size = 2 },
        .{ .pc = 2, .opcode = .get_field, .operand = .{ .atom = 0 }, .size = 5 }, // atom 0 = "kind"
        .{ .pc = 7, .opcode = .@"return", .operand = .{ .none = {} }, .size = 1 },
    };

    var cfg = cfg_mod.CFG.init(allocator);
    var block = BasicBlock.init(allocator, 0, 0);
    block.instructions = &instrs;
    block.end_pc = 8;
    try cfg.blocks.append(allocator, block);

    // Atom strings table - index 0 = "kind"
    const atom_strings = [_][]const u8{"kind"};

    const func = FunctionInfo{
        .name = "getKind",
        .arg_count = 1,
        .var_count = 0,
        .cfg = &cfg,
        .is_self_recursive = false,
        .atom_strings = &atom_strings,
    };

    var gen = ZigCodeGen.init(allocator, func);
    defer gen.deinit();

    const code = try gen.generate();
    defer allocator.free(code);

    // Clean up
    for (cfg.blocks.items) |*b| {
        b.successors.deinit(allocator);
    }
    cfg.blocks.deinit(allocator);

    // Verify structure
    try std.testing.expect(std.mem.indexOf(u8, code, "__frozen_getKind") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "getPropertyStr") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "\"kind\"") != null);

    // Print generated code
    std.debug.print("\n=== Generated getKind(node) code ===\n{s}\n=== End ===\n", .{code});
}

test "ZigCodeGen function call - call1" {
    const allocator = std.testing.allocator;

    // test(f, x) { return f(x); }
    // Bytecode: get_arg 0, get_arg 1, call1, return
    var instrs = [_]Instruction{
        .{ .pc = 0, .opcode = .get_arg, .operand = .{ .arg = 0 }, .size = 2 }, // push f
        .{ .pc = 2, .opcode = .get_arg, .operand = .{ .arg = 1 }, .size = 2 }, // push x
        .{ .pc = 4, .opcode = .call1, .operand = .{ .implicit_argc = 1 }, .size = 1 },
        .{ .pc = 5, .opcode = .@"return", .operand = .{ .none = {} }, .size = 1 },
    };

    var cfg = cfg_mod.CFG.init(allocator);
    var block = BasicBlock.init(allocator, 0, 0);
    block.instructions = &instrs;
    block.end_pc = 6;
    try cfg.blocks.append(allocator, block);

    const func = FunctionInfo{
        .name = "callTest",
        .arg_count = 2,
        .var_count = 0,
        .cfg = &cfg,
        .is_self_recursive = false,
    };

    var gen = ZigCodeGen.init(allocator, func);
    defer gen.deinit();

    const code = try gen.generate();
    defer allocator.free(code);

    // Clean up
    for (cfg.blocks.items) |*b| {
        b.successors.deinit(allocator);
    }
    cfg.blocks.deinit(allocator);

    // Verify structure
    try std.testing.expect(std.mem.indexOf(u8, code, "__frozen_callTest") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "JSValue.call") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "args[0]") != null);

    // Print generated code
    std.debug.print("\n=== Generated callTest(f, x) code ===\n{s}\n=== End ===\n", .{code});
}

test "ZigCodeGen complete fib with recursive calls" {
    const allocator = std.testing.allocator;

    // fib(n) { if (n < 2) return n; return fib(n-1) + fib(n-2); }
    // Simplified bytecode (assumes fib is in closure var ref 0):
    // Block 0: n < 2? goto block 1 or 2
    // Block 1: return n (base case)
    // Block 2: get_var_ref0, get_arg - 1, call1, get_var_ref0, get_arg - 2, call1, add, return

    // Block 0: Check n < 2, branch
    var instrs0 = [_]Instruction{
        .{ .pc = 0, .opcode = .get_arg, .operand = .{ .arg = 0 }, .size = 2 },
        .{ .pc = 2, .opcode = .push_i8, .operand = .{ .i8 = 2 }, .size = 2 },
        .{ .pc = 4, .opcode = .lt, .operand = .{ .none = {} }, .size = 1 },
        .{ .pc = 5, .opcode = .if_false8, .operand = .{ .label = 5 }, .size = 2 },
    };

    // Block 1: return n
    var instrs1 = [_]Instruction{
        .{ .pc = 7, .opcode = .get_arg, .operand = .{ .arg = 0 }, .size = 2 },
        .{ .pc = 9, .opcode = .@"return", .operand = .{ .none = {} }, .size = 1 },
    };

    // Block 2: fib(n-1) + fib(n-2) - simplified using get_var_ref0 for fib reference
    var instrs2 = [_]Instruction{
        // fib(n-1)
        .{ .pc = 10, .opcode = .get_var_ref0, .operand = .{ .var_ref = 0 }, .size = 1 }, // push fib
        .{ .pc = 11, .opcode = .get_arg, .operand = .{ .arg = 0 }, .size = 2 }, // push n
        .{ .pc = 13, .opcode = .push_1, .operand = .{ .implicit_int = 1 }, .size = 1 }, // push 1
        .{ .pc = 14, .opcode = .sub, .operand = .{ .none = {} }, .size = 1 }, // n - 1
        .{ .pc = 15, .opcode = .call1, .operand = .{ .implicit_argc = 1 }, .size = 1 }, // fib(n-1)
        // fib(n-2)
        .{ .pc = 16, .opcode = .get_var_ref0, .operand = .{ .var_ref = 0 }, .size = 1 }, // push fib
        .{ .pc = 17, .opcode = .get_arg, .operand = .{ .arg = 0 }, .size = 2 }, // push n
        .{ .pc = 19, .opcode = .push_i8, .operand = .{ .i8 = 2 }, .size = 2 }, // push 2
        .{ .pc = 21, .opcode = .sub, .operand = .{ .none = {} }, .size = 1 }, // n - 2
        .{ .pc = 22, .opcode = .call1, .operand = .{ .implicit_argc = 1 }, .size = 1 }, // fib(n-2)
        // Add and return
        .{ .pc = 23, .opcode = .add, .operand = .{ .none = {} }, .size = 1 },
        .{ .pc = 24, .opcode = .@"return", .operand = .{ .none = {} }, .size = 1 },
    };

    var cfg = cfg_mod.CFG.init(allocator);

    var block0 = BasicBlock.init(allocator, 0, 0);
    block0.instructions = &instrs0;
    block0.end_pc = 7;
    try block0.successors.append(allocator, 1);
    try block0.successors.append(allocator, 2);
    try cfg.blocks.append(allocator, block0);

    var block1 = BasicBlock.init(allocator, 1, 7);
    block1.instructions = &instrs1;
    block1.end_pc = 10;
    try cfg.blocks.append(allocator, block1);

    var block2 = BasicBlock.init(allocator, 2, 10);
    block2.instructions = &instrs2;
    block2.end_pc = 25;
    try cfg.blocks.append(allocator, block2);

    const func = FunctionInfo{
        .name = "fib",
        .arg_count = 1,
        .var_count = 0,
        .cfg = &cfg,
        .is_self_recursive = true,
    };

    var gen = ZigCodeGen.init(allocator, func);
    defer gen.deinit();

    const code = try gen.generate();
    defer allocator.free(code);

    // Clean up
    for (cfg.blocks.items) |*b| {
        b.successors.deinit(allocator);
    }
    cfg.blocks.deinit(allocator);

    // Verify structure
    try std.testing.expect(std.mem.indexOf(u8, code, "__frozen_fib") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "block_id") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "JSValue.call") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "zig_runtime.add") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "zig_runtime.sub") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "zig_runtime.lt") != null);

    // Print generated code
    std.debug.print("\n=== Generated fib(n) with recursive calls ===\n{s}\n=== End ===\n", .{code});
}
