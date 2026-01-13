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
const module_parser = @import("module_parser.zig");

const Opcode = opcodes.Opcode;
const Instruction = parser.Instruction;
const BasicBlock = cfg_mod.BasicBlock;
const JS_ATOM_END = module_parser.JS_ATOM_END;
const CFG = cfg_mod.CFG;
const CountedLoop = cfg_mod.CountedLoop;
const Allocator = std.mem.Allocator;

// Debug flag - set to true for verbose codegen logging
const CODEGEN_DEBUG = false;

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

/// Escape a string for use in Zig string literals
/// Escapes: backslash, double quote, and control characters
fn escapeZigString(allocator: Allocator, input: []const u8) ![]u8 {
    // Count escaped length
    var escaped_len: usize = 0;
    for (input) |c| {
        escaped_len += switch (c) {
            '\\' => 2, // \\ -> \\
            '"' => 2, // " -> \"
            '\n' => 2, // newline -> \n
            '\r' => 2, // cr -> \r
            '\t' => 2, // tab -> \t
            0...8, 11, 12, 14...31 => 4, // \xNN
            else => 1,
        };
    }

    const result = try allocator.alloc(u8, escaped_len);
    var i: usize = 0;
    for (input) |c| {
        switch (c) {
            '\\' => {
                result[i] = '\\';
                result[i + 1] = '\\';
                i += 2;
            },
            '"' => {
                result[i] = '\\';
                result[i + 1] = '"';
                i += 2;
            },
            '\n' => {
                result[i] = '\\';
                result[i + 1] = 'n';
                i += 2;
            },
            '\r' => {
                result[i] = '\\';
                result[i + 1] = 'r';
                i += 2;
            },
            '\t' => {
                result[i] = '\\';
                result[i + 1] = 't';
                i += 2;
            },
            0...8, 11, 12, 14...31 => {
                result[i] = '\\';
                result[i + 1] = 'x';
                const hex_chars = "0123456789abcdef";
                result[i + 2] = hex_chars[c >> 4];
                result[i + 3] = hex_chars[c & 0xf];
                i += 4;
            },
            else => {
                result[i] = c;
                i += 1;
            },
        }
    }
    return result;
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
    /// Index of the self-reference closure variable (-1 if none)
    self_ref_var_idx: i16 = -1,
    /// Closure variable indices
    closure_var_indices: []const u16 = &.{},
    /// Atom strings for property names
    atom_strings: []const []const u8 = &.{},
    /// Enable partial freeze (emit fallback for contaminated blocks)
    partial_freeze: bool = false,
    /// Original JS function name (for fallback registration)
    js_name: []const u8 = &.{},
    /// Is this a pure int32 function (can use optimized Zig int32 path)
    is_pure_int32: bool = false,
};

/// Full Zig code generator
pub const ZigCodeGen = struct {
    allocator: Allocator,
    output: std.ArrayListUnmanaged(u8),
    func: FunctionInfo,
    indent: usize,
    /// Track which frozen functions exist (for direct calls)
    frozen_functions: []const []const u8,
    /// Track if last instruction was get_var_ref0 for self-reference (pending self-call optimization)
    pending_self_call: bool,
    /// Track pending Math.method call for native optimization
    /// When we see get_var("Math") followed by get_field2("abs"/"sqrt"/etc), we can emit native math
    pending_math_method: ?[]const u8 = null,
    /// Detected counted loops for optimization
    counted_loops: []CountedLoop = &.{},
    /// Detected natural loops for native while codegen
    natural_loops: []cfg_mod.NaturalLoop = &.{},
    /// Blocks to skip (part of optimized loops)
    skip_blocks: std.AutoHashMapUnmanaged(u32, void) = .{},
    /// Current block index (for fallback code generation)
    current_block_idx: u32 = 0,
    /// Virtual stack for expression-based codegen (tracks symbolic expressions)
    vstack: std.ArrayListUnmanaged([]const u8) = .{},
    /// Force stack-based codegen after a fallback (to avoid vstack/stack mismatches)
    force_stack_mode: bool = false,
    /// Block terminated by return/throw - skip remaining instructions
    block_terminated: bool = false,
    /// Temp variable counter for expression codegen
    temp_counter: u32 = 0,
    /// Use expression-based codegen (no stack machine)
    use_expr_codegen: bool = false,
    /// Track if-statement depth for proper closing braces
    if_body_depth: u32 = 0,
    /// Stack of target blocks for nested if-statements (to close braces when reached)
    if_target_blocks: std.ArrayListUnmanaged(u32) = .{},
    /// Debug mode for tracing codegen issues
    debug_mode: bool = true,
    /// Track if argc/argv parameters are used
    uses_argc_argv: bool = false,
    /// Track if this_val parameter is used
    uses_this_val: bool = false,

    const Self = @This();

    pub fn init(allocator: Allocator, func: FunctionInfo) Self {
        return .{
            .allocator = allocator,
            .output = .{},
            .func = func,
            .indent = 0,
            .frozen_functions = &.{},
            .pending_self_call = false,
            .counted_loops = &.{},
            .skip_blocks = .{},
            .current_block_idx = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.output.deinit(self.allocator);
        if (self.counted_loops.len > 0) {
            self.allocator.free(self.counted_loops);
        }
        if (self.natural_loops.len > 0) {
            for (self.natural_loops) |loop| {
                self.allocator.free(loop.body_blocks);
            }
            self.allocator.free(self.natural_loops);
        }
        self.skip_blocks.deinit(self.allocator);
        self.if_target_blocks.deinit(self.allocator);
        // Free virtual stack expressions
        for (self.vstack.items) |expr| {
            self.allocator.free(expr);
        }
        self.vstack.deinit(self.allocator);
    }

    /// Set list of frozen function names for direct call optimization
    pub fn setFrozenFunctions(self: *Self, names: []const []const u8) void {
        self.frozen_functions = names;
    }

    /// Scan the CFG to detect if argc/argv and this_val are used
    fn scanParameterUsage(self: *Self) void {
        // Reset flags - important since ZigCodeGen may be reused
        self.uses_argc_argv = false;
        self.uses_this_val = false;
        // Scan all blocks for parameter usage
        for (self.func.cfg.blocks.items) |block| {
            for (block.instructions) |instr| {
                switch (instr.opcode) {
                    // Check for argument access opcodes
                    .get_arg, .get_arg0, .get_arg1, .get_arg2, .get_arg3,
                    .put_arg, .put_arg0, .put_arg1, .put_arg2, .put_arg3,
                    .set_arg, .set_arg0, .set_arg1, .set_arg2, .set_arg3 => {
                        self.uses_argc_argv = true;
                    },
                    // Check for this_val access - only push_this directly uses the parameter
                    .push_this => {
                        self.uses_this_val = true;
                    },
                    else => {},
                }
            }
        }
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

    // ========================================================================
    // Atom String Lookup
    // ========================================================================

    /// Look up atom string by bytecode index, handling builtin vs user atoms
    fn getAtomString(self: *const Self, atom_idx: u32) ?[]const u8 {
        // Bytecode instruction atoms use raw indices:
        //   idx < JS_ATOM_END (227): built-in atom index
        //   idx >= JS_ATOM_END: user atom at index (idx - JS_ATOM_END)

        // Built-in atoms are < JS_ATOM_END - look up in BUILTIN_ATOMS table
        if (atom_idx < JS_ATOM_END) {
            if (atom_idx < module_parser.BUILTIN_ATOMS.len) {
                const name = module_parser.BUILTIN_ATOMS[atom_idx];
                // Skip internal atoms (empty or starting with <)
                if (name.len > 0 and (name.len < 1 or name[0] != '<')) {
                    return name;
                }
            }
            return null;
        }

        // User atom - look up in our parsed atom table
        const user_idx = atom_idx - JS_ATOM_END;
        if (user_idx < self.func.atom_strings.len) {
            const str = self.func.atom_strings[user_idx];
            if (str.len > 0) {
                return str;
            }
        }
        return null;
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
        // Check for pure int32 optimization (fib-like functions)
        if (self.func.is_pure_int32 and self.func.is_self_recursive) {
            try self.emitInt32Specialized();
            return self.output.toOwnedSlice(self.allocator);
        }

        // Detect loops early - needed for native specialization check
        self.natural_loops = cfg_mod.detectNaturalLoops(self.func.cfg, self.allocator) catch &.{};
        self.counted_loops = cfg_mod.detectCountedLoops(self.func.cfg, self.allocator) catch &.{};

        // Check for complex control flow patterns that cause Zig codegen issues
        // Skip to C codegen fallback for: multiple depth-0 loops, contaminated blocks in loops
        var depth0_loop_count: u32 = 0;
        var has_contaminated_in_loop = false;
        for (self.natural_loops) |loop| {
            if (loop.depth == 0) depth0_loop_count += 1;
            // Check for contaminated blocks in loop body
            for (loop.body_blocks) |bid| {
                if (bid < self.func.cfg.blocks.items.len) {
                    const block = self.func.cfg.blocks.items[bid];
                    if (block.is_contaminated) {
                        has_contaminated_in_loop = true;
                        break;
                    }
                }
            }
        }
        if (depth0_loop_count > 1 or has_contaminated_in_loop) {
            if (CODEGEN_DEBUG) std.debug.print("[codegen] {s}: Skipping Zig codegen (complex pattern: {} depth-0 loops, contaminated_in_loop={})\n", .{ self.func.name, depth0_loop_count, has_contaminated_in_loop });
            return error.ComplexControlFlow;
        }

        // Check for native specialization (array + numeric functions)
        // This generates ZERO FFI code - all JS types extracted once at entry
        const can_native = self.canUseNativeSpecialization();
        if (CODEGEN_DEBUG) std.debug.print("[codegen] {s}: canUseNativeSpecialization = {}\n", .{ self.func.name, can_native });
        if (can_native) {
            if (CODEGEN_DEBUG) std.debug.print("[codegen] {s}: Using ZERO-FFI native specialization!\n", .{self.func.name});
            try self.emitNativeSpecialized();
            return self.output.toOwnedSlice(self.allocator);
        }

        // Function signature
        try self.emitSignature();

        self.pushIndent();

        // For functions with block dispatch, increase comptime branch quota
        // to avoid "evaluation exceeded 1000 backwards branches" during compilation
        // Even small functions can hit this limit due to switch statement analysis
        const block_count = self.func.cfg.blocks.items.len;
        if (block_count > 5) {
            // Use 100x block count as quota (generous for complex control flow)
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

        // CompressedValue alias - must be before locals which use CV
        // Use _ = &CV to silence unused warning without "pointless discard" error
        try self.writeLine("const CV = zig_runtime.CompressedValue;");
        try self.writeLine("_ = &CV;");

        // Local variables
        try self.emitLocals();
        try self.writeLine("");

        // Check if we need stack/sp/block_id (simple functions may not use them)
        const blocks = self.func.cfg.blocks.items;

        // Special case: single-block function with only return_undef - no stack needed
        const is_simple_return_undef = blocks.len == 1 and
            blocks[0].instructions.len == 1 and
            blocks[0].instructions[0].opcode == .return_undef;

        // Empty function
        if (blocks.len == 0) {
            try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
        } else if (is_simple_return_undef) {
            // Just return undefined, no stack/sp needed
            try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
        } else {
            // For all other functions, emit stack and instructions using CompressedValue (8-byte)
            try self.writeLine("var stack: [256]CV = .{CV.UNDEFINED} ** 256;");
            try self.writeLine("var sp: usize = 0;");
            // Silence "unused" warnings in ReleaseFast when early return happens
            try self.writeLine("_ = &stack; _ = &sp;");
            try self.writeLine("");

            // Check if we need block dispatch (multiple blocks with jumps)
            const needs_dispatch = blocks.len > 1;

            if (needs_dispatch) {
                // Detect natural loops for native while codegen
                self.natural_loops = cfg_mod.detectNaturalLoops(self.func.cfg, self.allocator) catch &.{};

                // Detect counted loops for array sum optimization
                self.counted_loops = cfg_mod.detectCountedLoops(self.func.cfg, self.allocator) catch &.{};

                // Mark ALL loop body blocks to skip from switch dispatch
                for (self.natural_loops) |loop| {
                    for (loop.body_blocks) |bid| {
                        try self.skip_blocks.put(self.allocator, bid, {});
                    }
                }

                // Mark counted loop body blocks
                for (self.counted_loops) |loop| {
                    if (loop.body_pattern == .array_sum or loop.body_pattern == .array_product) {
                        try self.skip_blocks.put(self.allocator, loop.body_block, {});
                    }
                }

                // Check if all blocks are in loops (no switch needed)
                const all_in_loops = self.skip_blocks.count() == blocks.len;

                if (all_in_loops and self.natural_loops.len > 0) {
                    // All code is in loops - emit native loops only
                    try self.emitNativeLoops(blocks);
                } else {
                    // Some blocks outside loops - need switch dispatch
                    try self.writeLine("var block_id: u32 = 0;");
                    try self.writeLine("_ = &block_id;");  // Silence "never mutated" warning
                    try self.writeLine("dispatch: while (true) {");
                    self.pushIndent();
                    try self.writeLine("switch (block_id) {");
                    self.pushIndent();

                    // Generate each block
                    for (blocks, 0..) |block, idx| {
                        const block_idx: u32 = @intCast(idx);

                        // Check if this is a loop header - emit native loop
                        var is_loop_header = false;
                        for (self.natural_loops) |loop| {
                            if (loop.header_block == block_idx and loop.depth == 0) {
                                try self.emitNativeLoopBlock(loop, blocks, block_idx);
                                is_loop_header = true;
                                break;
                            }
                        }

                        if (is_loop_header) continue;

                        // Skip blocks that are part of loops (emitted inside native loop)
                        if (self.skip_blocks.contains(block_idx)) {
                            continue;
                        }

                        // Check if this is an optimized array sum loop header
                        var is_optimized_loop = false;
                        for (self.counted_loops) |loop| {
                            if (loop.header_block == block_idx and (loop.body_pattern == .array_sum or loop.body_pattern == .array_product)) {
                                try self.emitOptimizedArraySumLoop(loop, block_idx);
                                is_optimized_loop = true;
                                break;
                            }
                        }

                        if (!is_optimized_loop) {
                            try self.emitBlock(block, block_idx);
                        }
                    }

                    // Default case - use continue :dispatch to silence unused label warning
                    try self.writeLine("else => continue :dispatch,");

                    self.popIndent();
                    try self.writeLine("}");
                    self.popIndent();
                    try self.writeLine("}");
                }
            } else if (blocks.len == 1) {
                // Single block - emit instructions directly without dispatch
                const block = blocks[0];
                if (self.func.partial_freeze and block.is_contaminated) {
                    // Contaminated single block - emit fallback
                    const reason = block.contamination_reason orelse "unknown";
                    try self.printLine("// CONTAMINATED BLOCK - reason: {s}", .{reason});
                    const js_name = if (self.func.js_name.len > 0) self.func.js_name else self.func.name;
                    try self.writeLine("{");
                    self.pushIndent();
                    try self.writeLine("var next_block_fallback: usize = 0;");
                    try self.writeLine("const fallback_result = zig_runtime.blockFallback(");
                    self.pushIndent();
                    try self.printLine("ctx, \"{s}\", this_val, argc, argv,", .{js_name});
                    try self.printLine("&locals, {d}, &stack, &sp, 0, &next_block_fallback);", .{self.func.var_count});
                    self.popIndent();
                    try self.writeLine("return fallback_result;");
                    self.popIndent();
                    try self.writeLine("}");
                } else {
                    // Clean single block - emit instructions directly
                    for (block.instructions, 0..) |instr, idx| {
                        const continues = try self.emitInstruction(instr, block.instructions, idx);
                        if (!continues) break; // Stop after terminating instruction
                    }
                }
            }
        }

        self.popIndent();
        try self.writeLine("}");

        return try self.output.toOwnedSlice(self.allocator);
    }

    // ========================================================================
    // Function Signature
    // ========================================================================

    fn emitSignature(self: *Self) !void {
        // Scan CFG to detect which parameters are actually used
        self.scanParameterUsage();

        // All frozen functions must use C calling convention for FFI compatibility
        try self.print(
            \\pub fn __frozen_{s}(ctx: *zig_runtime.JSContext, this_val: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {{
            \\
        , .{self.func.name});
        // Suppress unused warnings only for parameters that are not used
        // For this_val: only discard if no push_this opcode detected
        if (!self.uses_this_val) {
            try self.writeLine("    _ = this_val;");
        }
        // For argc/argv: use @intFromPtr trick to "use" without triggering pointless discard
        // This satisfies unused warnings even when the params are also accessed later
        try self.writeLine("    _ = @as(usize, @intCast(argc)) +% @intFromPtr(argv);");

    }

    // ========================================================================
    // Local Variables
    // ========================================================================

    fn emitLocals(self: *Self) !void {
        // Always emit locals array using CompressedValue (8-byte)
        // Even if var_count=0, we need locals declared because unreachable blocks
        // may still contain get_loc* instructions that reference it
        if (self.func.var_count > 0) {
            try self.printLine("var locals: [{d}]CV = .{{CV.UNDEFINED}} ** {d};", .{ self.func.var_count, self.func.var_count });
            // Silence "unused" warnings in ReleaseFast when early return happens
            try self.writeLine("_ = &locals;");
        } else {
            // Always declare empty locals array - dead code may still reference it
            try self.writeLine("var locals: [0]CV = .{};");
            try self.writeLine("_ = &locals;");
        }
    }

    // ========================================================================
    // Block Generation
    // ========================================================================

    /// Find the last inc_loc or dec_loc instruction in a block and return the local index.
    /// This is used for loop back-edge stack fixup - we need to push the loop counter.
    fn findLastIncLocal(self: *const Self, instructions: []const Instruction) ?u32 {
        _ = self;
        var result: ?u32 = null;
        for (instructions) |instr| {
            switch (instr.opcode) {
                .inc_loc, .dec_loc => {
                    // These opcodes have the local index in the .loc operand
                    if (instr.operand == .loc) {
                        result = instr.operand.loc;
                    }
                },
                else => {},
            }
        }
        return result;
    }

    fn emitBlock(self: *Self, block: BasicBlock, block_idx: u32) !void {
        self.current_block_idx = block_idx; // Store for use in emitInstruction fallbacks

        // Debug: print instructions in this block
        if (self.debug_mode) {
            if (CODEGEN_DEBUG) std.debug.print("[emitBlock] Block {d} has {d} instructions:\n", .{ block_idx, block.instructions.len });
            for (block.instructions, 0..) |instr, i| {
                std.debug.print("  [{d}] {s}\n", .{ i, @tagName(instr.opcode) });
            }
        }

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
            // Clean block - use expression-based codegen (same as loops)
            // Don't clear vstack so values flow from previous blocks
            // Reset block_terminated for this block
            self.block_terminated = false;
            for (block.instructions, 0..) |instr, idx| {
                try self.emitInstructionExpr(instr, block, null, idx);
                // Stop processing remaining instructions if block was terminated by return/throw
                if (self.block_terminated) break;
            }

            // Block terminator (jump to successor blocks)
            const successors = block.successors.items;
            // Check if block ends with control flow instruction (or was terminated by unsupported opcode)
            const last_op = if (block.instructions.len > 0) block.instructions[block.instructions.len - 1].opcode else .nop;
            const is_return = last_op == .@"return" or last_op == .return_undef or
                last_op == .tail_call or last_op == .tail_call_method;

            if (!is_return and !self.block_terminated) {
                if (successors.len == 2) {
                    // Conditional branch - value should be on vstack
                    const cond_expr = self.vpop() orelse "CV.FALSE";
                    const should_free = self.isAllocated(cond_expr);
                    defer if (should_free) self.allocator.free(cond_expr);

                    // Detect if terminator is if_false or if_true by checking last instruction
                    const last_instr = block.instructions[block.instructions.len - 1];
                    const is_if_false = last_instr.opcode == .if_false or last_instr.opcode == .if_false8;

                    if (is_if_false) {
                        // if_false: jump to successors[0] when FALSE, fall to successors[1] when TRUE
                        try self.printLine("if (!({s}).toBool()) {{ block_id = {d}; continue; }}", .{ cond_expr, successors[0] });
                        try self.printLine("block_id = {d}; continue;", .{successors[1]});
                    } else {
                        // if_true: jump to successors[0] when TRUE, fall to successors[1] when FALSE
                        try self.printLine("if (({s}).toBool()) {{ block_id = {d}; continue; }}", .{ cond_expr, successors[0] });
                        try self.printLine("block_id = {d}; continue;", .{successors[1]});
                    }
                } else if (successors.len == 1) {
                    // Unconditional jump
                    // For loop back-edges, we may need to push the loop counter
                    const target_block_id = successors[0];
                    if (target_block_id < block_idx) {
                        // This is a back-edge - check if target expects stack values
                        if (target_block_id < self.func.cfg.blocks.items.len) {
                            const target_block = &self.func.cfg.blocks.items[target_block_id];
                            if (target_block.stack_depth_in > 0) {
                                // Target expects values on stack - push the loop counter
                                // Find the last inc_loc instruction to determine which local
                                const loop_local = self.findLastIncLocal(block.instructions);
                                if (loop_local) |local_idx| {
                                    if (CODEGEN_DEBUG) std.debug.print("[zig-codegen] Loop back-edge: block {d} -> {d}, pushing locals[{d}]\n", .{ block_idx, target_block_id, local_idx });
                                    try self.printLine("stack[sp] = locals[{d}]; sp += 1;", .{local_idx});
                                }
                            }
                        }
                    }
                    try self.printLine("block_id = {d}; continue;", .{successors[0]});
                } else {
                    // No successors but control continues - unusual but can happen
                    // with certain opcodes. Emit return undefined for safety.
                    try self.printLine("// block {d}: no explicit successors, returning undefined", .{block_idx});
                    try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
                }
            }
            // If control doesn't continue, block ends with return (handled by return opcode)
        }

        self.popIndent();
        try self.writeLine("},");
    }

    /// Emit optimized code for array sum loop pattern: for (i=0; i<arr.length; i++) acc += arr[i]
    /// This generates tight native Zig loop with direct array access and TypedArray fast path
    fn emitOptimizedArraySumLoop(self: *Self, loop: CountedLoop, block_idx: u32) !void {
        const acc_local = loop.accumulator_local orelse 0;
        const counter_local = loop.counter_local;
        const exit_block = loop.exit_block;

        try self.printLine("{d} => {{ // OPTIMIZED ARRAY SUM LOOP", .{block_idx});
        self.pushIndent();

        // Get the array from arg0
        try self.writeLine("const _arr = if (0 < argc) argv[0] else JSValue.UNDEFINED;");

        // Get cached length (or compute it)
        try self.writeLine("const _len: i64 = blk: {");
        self.pushIndent();
        try self.writeLine("var len_val: i32 = 0;");
        try self.writeLine("const len_jsval = JSValue.getPropertyStr(ctx, _arr, \"length\");");
        try self.writeLine("_ = JSValue.toInt32(ctx, &len_val, len_jsval);");
        try self.writeLine("JSValue.free(ctx, len_jsval);");
        try self.writeLine("break :blk len_val;");
        self.popIndent();
        try self.writeLine("};");
        try self.writeLine("");

        // Try TypedArray fast path first
        try self.writeLine("// TypedArray fast path");
        try self.writeLine("const typed_result = zig_runtime.sumTypedArrayFast(ctx, _arr);");
        try self.writeLine("if (typed_result.success) {");
        self.pushIndent();
        try self.writeLine("// TypedArray fast path succeeded");
        try self.printLine("var _init_acc: i64 = 0;", .{});
        try self.printLine("var acc_i32: i32 = 0;", .{});
        try self.printLine("if (JSValue.toInt32(ctx, &acc_i32, locals[{d}]) == 0) _init_acc = acc_i32;", .{acc_local});
        try self.printLine("JSValue.free(ctx, locals[{d}]);", .{acc_local});
        try self.printLine("locals[{d}] = JSValue.newInt64(ctx, _init_acc + typed_result.sum);", .{acc_local});
        try self.writeLine("// Update loop counter to end value");
        try self.printLine("JSValue.free(ctx, locals[{d}]);", .{counter_local});
        try self.printLine("locals[{d}] = JSValue.newInt64(ctx, _len);", .{counter_local});
        try self.printLine("block_id = {d}; continue;", .{exit_block});
        self.popIndent();
        try self.writeLine("}");
        try self.writeLine("");

        // Regular array loop with SMI fast path
        try self.writeLine("// Regular array loop with SMI fast path");
        try self.writeLine("var _acc: i64 = 0;");
        try self.printLine("var acc_i32_2: i32 = 0;", .{});
        try self.printLine("if (JSValue.toInt32(ctx, &acc_i32_2, locals[{d}]) == 0) _acc = acc_i32_2;", .{acc_local});
        try self.writeLine("var _i: i64 = 0;");
        try self.printLine("var counter_i32: i32 = 0;", .{});
        try self.printLine("if (JSValue.toInt32(ctx, &counter_i32, locals[{d}]) == 0) _i = counter_i32;", .{counter_local});
        try self.writeLine("");

        // The tight loop
        try self.writeLine("while (_i < _len) : (_i += 1) {");
        self.pushIndent();
        try self.writeLine("const _v = JSValue.getPropertyUint32(ctx, _arr, @intCast(_i));");
        try self.writeLine("var elem_i32: i32 = 0;");
        try self.writeLine("if (JSValue.toInt32(ctx, &elem_i32, _v) == 0) {");
        self.pushIndent();
        try self.writeLine("// SMI fast path");
        try self.writeLine("_acc += elem_i32;");
        self.popIndent();
        try self.writeLine("} else {");
        self.pushIndent();
        try self.writeLine("// Float or object - convert to number");
        try self.writeLine("var d: f64 = 0;");
        try self.writeLine("if (JSValue.toFloat64(ctx, &d, _v) == 0) {");
        self.pushIndent();
        try self.writeLine("_acc += @as(i64, @intFromFloat(d));");
        self.popIndent();
        try self.writeLine("}");
        self.popIndent();
        try self.writeLine("}");
        try self.writeLine("JSValue.free(ctx, _v);");
        self.popIndent();
        try self.writeLine("}");
        try self.writeLine("");

        // Update locals with final values
        try self.writeLine("// Update locals with final values");
        try self.printLine("JSValue.free(ctx, locals[{d}]);", .{acc_local});
        try self.printLine("locals[{d}] = JSValue.newInt64(ctx, _acc);", .{acc_local});
        try self.printLine("JSValue.free(ctx, locals[{d}]);", .{counter_local});
        try self.printLine("locals[{d}] = JSValue.newInt64(ctx, _i);", .{counter_local});
        try self.printLine("block_id = {d}; continue;", .{exit_block});

        self.popIndent();
        try self.writeLine("},");
    }

    // ========================================================================
    // Native Loop Emission
    // ========================================================================

    /// Emit all code as native loops (when all blocks are in loops)
    fn emitNativeLoops(self: *Self, blocks: []const BasicBlock) !void {
        // Find outermost loops and emit them
        for (self.natural_loops) |loop| {
            if (loop.depth == 0) {
                // Emit initialization blocks before loop (block 0 to header-1)
                if (loop.header_block > 0) {
                    for (0..loop.header_block) |i| {
                        try self.emitBlockInstructionsInline(blocks[i]);
                    }
                }

                // Emit the loop
                try self.emitNativeLoop(loop, blocks);

                // Emit exit blocks after loop
                if (loop.exit_block) |exit_id| {
                    if (exit_id < blocks.len) {
                        for (exit_id..blocks.len) |i| {
                            // Skip if this block is part of another loop
                            var in_other_loop = false;
                            for (self.natural_loops) |other| {
                                if (other.header_block != loop.header_block and other.containsBlock(@intCast(i))) {
                                    in_other_loop = true;
                                    break;
                                }
                            }
                            if (!in_other_loop) {
                                try self.emitBlockInstructionsInline(blocks[i]);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Emit a native while loop using expression-based codegen
    fn emitNativeLoop(self: *Self, loop: cfg_mod.NaturalLoop, blocks: []const BasicBlock) !void {
        try self.writeLine("while (true) {");
        self.pushIndent();

        // Emit all blocks in the loop using expression-based codegen
        for (loop.body_blocks) |bid| {
            if (bid >= blocks.len) continue;
            const block = blocks[bid];

            // Check if this block is a nested loop header
            // Only emit if this nested loop is a DIRECT child (parent is current loop)
            var is_nested_loop = false;
            for (self.natural_loops) |nested| {
                if (nested.header_block == bid and nested.depth > loop.depth) {
                    // Check if this nested loop's parent is the current loop
                    if (nested.parent_header != null and nested.parent_header.? == loop.header_block) {
                        try self.emitNativeLoop(nested, blocks);
                    }
                    is_nested_loop = true;
                    break;
                }
            }

            if (is_nested_loop) continue;

            // Check if already emitted as part of nested loop
            var in_nested = false;
            for (self.natural_loops) |nested| {
                if (nested.header_block != loop.header_block and
                    nested.header_block > loop.header_block and
                    nested.latch_block < loop.latch_block and
                    nested.containsBlock(bid))
                {
                    in_nested = true;
                    break;
                }
            }
            if (in_nested) continue;

            // Check if we've reached any target block for if-statements - close their braces
            // Process in reverse order to close nested ifs correctly (innermost first)
            while (self.if_target_blocks.items.len > 0) {
                const last_target = self.if_target_blocks.items[self.if_target_blocks.items.len - 1];
                if (bid >= last_target) {
                    _ = self.if_target_blocks.pop();
                    self.popIndent();
                    try self.writeLine("}");
                    self.if_body_depth -= 1;
                } else {
                    break;
                }
            }

            // Use expression-based codegen for loop blocks (no stack machine overhead)
            try self.emitBlockExpr(block, loop);
        }

        // Close any remaining unclosed if-blocks before closing the while loop
        while (self.if_target_blocks.items.len > 0 and self.if_body_depth > 0) {
            _ = self.if_target_blocks.pop();
            self.popIndent();
            try self.writeLine("}");
            self.if_body_depth -= 1;
        }

        self.popIndent();
        try self.writeLine("}");
    }

    /// Emit a native loop as a switch case (for mixed loop/non-loop code)
    fn emitNativeLoopBlock(self: *Self, loop: cfg_mod.NaturalLoop, blocks: []const BasicBlock, header_idx: u32) !void {
        try self.printLine("{d} => {{ // native loop", .{header_idx});
        self.pushIndent();

        try self.writeLine("while (true) {");
        self.pushIndent();

        // Emit all blocks in the loop using expression-based codegen
        for (loop.body_blocks) |bid| {
            if (bid >= blocks.len) continue;
            const block = blocks[bid];

            // Check if this is a nested loop header
            // Only emit if this nested loop is a DIRECT child (parent is current loop)
            var is_nested_loop = false;
            for (self.natural_loops) |nested| {
                if (nested.header_block == bid and nested.depth > loop.depth) {
                    // Check if this nested loop's parent is the current loop
                    if (nested.parent_header != null and nested.parent_header.? == loop.header_block) {
                        try self.emitNativeLoop(nested, blocks);
                    }
                    is_nested_loop = true;
                    break;
                }
            }
            if (is_nested_loop) continue;

            // Check if in nested loop
            var in_nested = false;
            for (self.natural_loops) |nested| {
                if (nested.header_block != loop.header_block and
                    nested.header_block > loop.header_block and
                    nested.latch_block < loop.latch_block and
                    nested.containsBlock(bid))
                {
                    in_nested = true;
                    break;
                }
            }
            if (in_nested) continue;

            // Check if we've reached any target block for if-statements - close their braces
            // Process in reverse order to close nested ifs correctly (innermost first)
            while (self.if_target_blocks.items.len > 0) {
                const last_target = self.if_target_blocks.items[self.if_target_blocks.items.len - 1];
                if (bid >= last_target) {
                    _ = self.if_target_blocks.pop();
                    self.popIndent();
                    try self.writeLine("}");
                    self.if_body_depth -= 1;
                } else {
                    break;
                }
            }

            // Use expression-based codegen
            try self.emitBlockExpr(block, loop);
        }

        // Close any remaining unclosed if-blocks before closing the while loop
        while (self.if_target_blocks.items.len > 0 and self.if_body_depth > 0) {
            _ = self.if_target_blocks.pop();
            self.popIndent();
            try self.writeLine("}");
            self.if_body_depth -= 1;
        }

        self.popIndent();
        try self.writeLine("}");

        // After loop exits, continue to exit block
        if (loop.exit_block) |exit_id| {
            try self.printLine("block_id = {d}; continue :dispatch;", .{exit_id});
        } else {
            try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
        }

        self.popIndent();
        try self.writeLine("},");
    }

    /// Emit block instructions inline using expression-based codegen
    fn emitBlockInstructionsInline(self: *Self, block: BasicBlock) !void {
        try self.emitBlockExpr(block, null);
    }

    /// Emit block instructions for a loop - kept for compatibility (uses emitBlockExpr now)
    fn emitBlockInstructionsForLoop(self: *Self, block: BasicBlock, loop: cfg_mod.NaturalLoop) !void {
        try self.emitBlockExpr(block, loop);
    }

    // ========================================================================
    // Expression-Based Codegen (no stack machine overhead)
    // ========================================================================

    /// Push an expression onto the virtual stack
    fn vpush(self: *Self, expr: []const u8) !void {
        const owned = try self.allocator.dupe(u8, expr);
        try self.vstack.append(self.allocator, owned);
    }

    /// Push a formatted expression onto the virtual stack
    fn vpushFmt(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        const expr = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.vstack.append(self.allocator, expr);
    }

    /// Pop an expression from the virtual stack
    fn vpop(self: *Self) ?[]const u8 {
        if (self.vstack.items.len == 0) return null;
        return self.vstack.pop();
    }

    /// Peek at the top expression without popping
    fn vpeek(self: *Self) ?[]const u8 {
        if (self.vstack.items.len == 0) return null;
        return self.vstack.items[self.vstack.items.len - 1];
    }

    /// Check if a string was heap-allocated (vs a literal fallback)
    fn isAllocated(self: *Self, str: []const u8) bool {
        _ = self;
        // Known literal fallbacks - compare pointer addresses
        const fallbacks = [_][]const u8{ "CV.FALSE", "CV.TRUE", "CV.NULL", "CV.UNDEFINED", "CV.newInt(0)", "stack[sp - 1]" };
        for (fallbacks) |f| {
            if (str.ptr == f.ptr) return false;
        }
        return true;
    }

    /// Get next temp variable name
    fn nextTemp(self: *Self) ![]const u8 {
        const name = try std.fmt.allocPrint(self.allocator, "_t{d}", .{self.temp_counter});
        self.temp_counter += 1;
        return name;
    }

    /// Emit a block using expression-based codegen (no stack machine)
    fn emitBlockExpr(self: *Self, block: BasicBlock, loop: ?cfg_mod.NaturalLoop) !void {
        // Debug: print block info BEFORE reset
        if (self.debug_mode and block.id == 10) {
            if (CODEGEN_DEBUG) std.debug.print("[emitBlockExpr] block {d}: BEFORE RESET force_stack_mode={}\n", .{ block.id, self.force_stack_mode });
        }

        // If block already terminated (by return/throw in a previous block), skip this block
        if (self.block_terminated) return;

        // Reset force_stack_mode for each block (block_terminated is preserved)
        self.force_stack_mode = false;

        if (self.debug_mode) {
            if (CODEGEN_DEBUG) std.debug.print("[emitBlockExpr] block {d}: {d} instructions\n", .{ block.id, block.instructions.len });
            for (block.instructions, 0..) |instr, i| {
                std.debug.print("  [{d}] {s}\n", .{ i, @tagName(instr.opcode) });
            }
        }

        // Determine if this is a loop header block
        const is_loop_header = if (loop) |l| block.id == l.header_block else false;

        // For loop headers that expect incoming stack values, keep exactly that many items
        // This handles the case where block 0 initializes x and leaves it on stack for loop condition
        if (is_loop_header and block.stack_depth_in > 0) {
            const keep_count: usize = @intCast(block.stack_depth_in);
            // If we have more items than expected, trim to expected count
            while (self.vstack.items.len > keep_count) {
                if (self.vstack.pop()) |expr| {
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                }
            }
            // If we have fewer items, the codegen will handle it (use CV.UNDEFINED fallback)
        } else {
            // Clear vstack to avoid stale symbolic references
            for (self.vstack.items) |expr| {
                if (self.isAllocated(expr)) self.allocator.free(expr);
            }
            self.vstack.clearRetainingCapacity();
        }

        for (block.instructions, 0..) |instr, idx| {
            try self.emitInstructionExpr(instr, block, loop, idx);
            // Stop processing remaining instructions if block was terminated by return/throw
            if (self.block_terminated) break;
        }
    }

    /// Emit instruction using expression-based codegen
    fn emitInstructionExpr(self: *Self, instr: Instruction, block: BasicBlock, loop: ?cfg_mod.NaturalLoop, idx: usize) !void {
        // Skip remaining instructions if block already terminated (by return/throw/unsupported opcode)
        if (self.block_terminated) return;

        // If force_stack_mode is set (after a fallback), use stack-based codegen for remaining ops
        // EXCEPT for control flow (if_false/if_true) which needs special loop handling
        if (self.force_stack_mode) {
            // Control flow still needs special handling for loop break/continue and if-then
            switch (instr.opcode) {
                .if_false, .if_false8 => {
                    // Stack-based if_false: use stack[sp-1] as condition
                    if (loop) |l| {
                        const target = block.successors.items[0];
                        if (l.exit_block != null and target == l.exit_block.?) {
                            try self.writeLine("{ const _cond = stack[sp - 1]; sp -= 1; if (!_cond.toBool()) break; }");
                        } else if (target == l.header_block) {
                            try self.writeLine("{ const _cond = stack[sp - 1]; sp -= 1; if (!_cond.toBool()) continue; }");
                        } else {
                            // Target is within loop body - this is an if-statement
                            // if_false jumps when condition is FALSE, so body executes when TRUE
                            try self.writeLine("sp -= 1; // pop condition");
                            try self.writeLine("if (stack[sp].toBool()) {");
                            self.pushIndent();
                            self.if_body_depth += 1;
                            try self.if_target_blocks.append(self.allocator, target);
                        }
                    } else {
                        try self.writeLine("sp -= 1; // pop condition");
                    }
                    return;
                },
                .if_true, .if_true8 => {
                    // Stack-based if_true: use stack[sp-1] as condition
                    if (loop) |l| {
                        const target = block.successors.items[0];
                        if (l.exit_block != null and target == l.exit_block.?) {
                            try self.writeLine("{ const _cond = stack[sp - 1]; sp -= 1; if (_cond.toBool()) break; }");
                        } else if (target == l.header_block) {
                            try self.writeLine("{ const _cond = stack[sp - 1]; sp -= 1; if (_cond.toBool()) continue; }");
                        } else {
                            // Target is within loop body - this is an if-statement
                            // if_true jumps when condition is TRUE, so body executes when FALSE
                            try self.writeLine("sp -= 1; // pop condition");
                            try self.writeLine("if (!stack[sp].toBool()) {");
                            self.pushIndent();
                            self.if_body_depth += 1;
                            try self.if_target_blocks.append(self.allocator, target);
                        }
                    } else {
                        try self.writeLine("sp -= 1; // pop condition");
                    }
                    return;
                },
                else => {},
            }
            const continues = try self.emitInstruction(instr, block.instructions, idx);
            if (!continues) {
                self.block_terminated = true;
            }
            return;
        }

        // Check if this instruction should be skipped for native Math optimization
        if (self.shouldSkipForNativeMath(block.instructions, idx)) |_| {
            // Skip this get_var("Math") or get_field2("method") - native fast path will handle it
            return;
        }

        switch (instr.opcode) {
            // Constants - push expression string
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

            // Local variables - push reference expression
            .get_loc0 => try self.vpush("locals[0]"),
            .get_loc1 => try self.vpush("locals[1]"),
            .get_loc2 => try self.vpush("locals[2]"),
            .get_loc3 => try self.vpush("locals[3]"),
            .get_loc, .get_loc8 => try self.vpushFmt("locals[{d}]", .{instr.operand.loc}),
            .get_loc0_loc1 => {
                try self.vpush("locals[0]");
                try self.vpush("locals[1]");
            },

            // Put local - emit assignment, pop from stack
            .put_loc0 => {
                if (self.vpop()) |expr| {
                    try self.printLine("locals[0] = {s};", .{expr});
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    try self.writeLine("locals[0] = stack[sp - 1]; sp -= 1;");
                }
            },
            .put_loc1 => {
                if (self.vpop()) |expr| {
                    try self.printLine("locals[1] = {s};", .{expr});
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    try self.writeLine("locals[1] = stack[sp - 1]; sp -= 1;");
                }
            },
            .put_loc2 => {
                if (self.vpop()) |expr| {
                    try self.printLine("locals[2] = {s};", .{expr});
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    try self.writeLine("locals[2] = stack[sp - 1]; sp -= 1;");
                }
            },
            .put_loc3 => {
                if (self.vpop()) |expr| {
                    try self.printLine("locals[3] = {s};", .{expr});
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    try self.writeLine("locals[3] = stack[sp - 1]; sp -= 1;");
                }
            },
            .put_loc, .put_loc8 => {
                if (self.vpop()) |expr| {
                    try self.printLine("locals[{d}] = {s};", .{ instr.operand.loc, expr });
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    try self.printLine("locals[{d}] = stack[sp - 1]; sp -= 1;", .{instr.operand.loc});
                }
            },

            // Set local (keep on stack) - emit assignment and push back
            .set_loc0 => {
                if (self.vpeek()) |expr| {
                    try self.printLine("locals[0] = {s};", .{expr});
                } else {
                    try self.writeLine("locals[0] = stack[sp - 1];");
                }
            },
            .set_loc1 => {
                if (self.vpeek()) |expr| {
                    try self.printLine("locals[1] = {s};", .{expr});
                } else {
                    try self.writeLine("locals[1] = stack[sp - 1];");
                }
            },
            .set_loc2 => {
                if (self.vpeek()) |expr| {
                    try self.printLine("locals[2] = {s};", .{expr});
                } else {
                    try self.writeLine("locals[2] = stack[sp - 1];");
                }
            },
            .set_loc3 => {
                if (self.vpeek()) |expr| {
                    try self.printLine("locals[3] = {s};", .{expr});
                } else {
                    try self.writeLine("locals[3] = stack[sp - 1];");
                }
            },
            .set_loc, .set_loc8 => {
                if (self.vpeek()) |expr| {
                    try self.printLine("locals[{d}] = {s};", .{ instr.operand.loc, expr });
                } else {
                    try self.printLine("locals[{d}] = stack[sp - 1];", .{instr.operand.loc});
                }
            },

            // Arguments
            .get_arg0 => try self.vpush("(if (0 < argc) CV.fromJSValue(argv[0]) else CV.UNDEFINED)"),
            .get_arg1 => try self.vpush("(if (1 < argc) CV.fromJSValue(argv[1]) else CV.UNDEFINED)"),
            .get_arg2 => try self.vpush("(if (2 < argc) CV.fromJSValue(argv[2]) else CV.UNDEFINED)"),
            .get_arg3 => try self.vpush("(if (3 < argc) CV.fromJSValue(argv[3]) else CV.UNDEFINED)"),
            .get_arg => try self.vpushFmt("(if ({d} < argc) CV.fromJSValue(argv[{d}]) else CV.UNDEFINED)", .{ instr.operand.arg, instr.operand.arg }),

            // Arithmetic - pop operands, push result expression
            .add => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.add({s}, {s})", .{ a, b });
            },
            .sub => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.sub({s}, {s})", .{ a, b });
            },
            .mul => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.mul({s}, {s})", .{ a, b });
            },
            .div => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.div({s}, {s})", .{ a, b });
            },
            .mod => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.mod({s}, {s})", .{ a, b });
            },
            .neg => {
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.sub(CV.newInt(0), {s})", .{a});
            },

            // Comparisons
            .lt => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.lt({s}, {s})", .{ a, b });
            },
            .lte => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("(if (CV.gt({s}, {s}).toBool()) CV.FALSE else CV.TRUE)", .{ a, b });
            },
            .gt => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.gt({s}, {s})", .{ a, b });
            },
            .gte => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("(if (CV.lt({s}, {s}).toBool()) CV.FALSE else CV.TRUE)", .{ a, b });
            },
            .eq => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.eq({s}, {s})", .{ a, b });
            },
            .neq => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("(if (CV.eq({s}, {s}).toBool()) CV.FALSE else CV.TRUE)", .{ a, b });
            },

            // Increment/decrement
            .inc => {
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.add({s}, CV.newInt(1))", .{a});
            },
            .dec => {
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.sub({s}, CV.newInt(1))", .{a});
            },

            // Inc/dec local directly
            .inc_loc => {
                const loc_idx = instr.operand.loc;
                try self.printLine("locals[{d}] = CV.add(locals[{d}], CV.newInt(1));", .{ loc_idx, loc_idx });
            },
            .dec_loc => {
                const loc_idx = instr.operand.loc;
                try self.printLine("locals[{d}] = CV.sub(locals[{d}], CV.newInt(1));", .{ loc_idx, loc_idx });
            },

            // get_length: pop obj, push obj.length
            // Push directly to real stack to avoid cross-block temp scope issues
            .get_length => {
                const obj_expr = self.vpop() orelse "CV.UNDEFINED";
                const should_free = self.isAllocated(obj_expr);
                defer if (should_free) self.allocator.free(obj_expr);
                // Emit inline FFI call with CV<->JSValue conversion, push directly to stack
                // Check if obj_expr is a direct argv reference (avoid CV round-trip for objects)
                if (std.mem.indexOf(u8, obj_expr, "CV.fromJSValue(argv[")) |start_idx| {
                    // Extract the argv[N] part and use it directly, with argc bounds check
                    const argv_start = std.mem.indexOf(u8, obj_expr, "argv[").?;
                    const bracket_end = std.mem.indexOf(u8, obj_expr[argv_start..], "]").? + argv_start;
                    const arg_num_str = obj_expr[argv_start + 5 .. bracket_end]; // Get the N in argv[N]
                    _ = start_idx;
                    try self.printLine("stack[sp] = if ({s} < argc) CV.fromJSValue(JSValue.getPropertyStr(ctx, argv[{s}], \"length\")) else CV.UNDEFINED; sp += 1;", .{ arg_num_str, arg_num_str });
                } else {
                    try self.printLine("{{ const obj = ({s}).toJSValue(); stack[sp] = CV.fromJSValue(JSValue.getPropertyStr(ctx, obj, \"length\")); JSValue.free(ctx, obj); sp += 1; }}", .{obj_expr});
                }
                // Don't add to vstack - value is on real stack
            },

            // get_array_el: pop idx, pop arr, push arr[idx]
            // Push directly to real stack to avoid cross-block temp scope issues
            .get_array_el => {
                const idx_expr = self.vpop() orelse "CV.UNDEFINED";
                const idx_free = self.isAllocated(idx_expr);
                defer if (idx_free) self.allocator.free(idx_expr);
                const arr_expr = self.vpop() orelse "CV.UNDEFINED";
                const arr_free = self.isAllocated(arr_expr);
                defer if (arr_free) self.allocator.free(arr_expr);
                // Emit inline FFI call, push directly to stack
                // Check if arr_expr is a direct argv reference (avoid CV round-trip for objects)
                if (std.mem.indexOf(u8, arr_expr, "CV.fromJSValue(argv[")) |_| {
                    // Extract the argv[N] part and use it directly, with argc bounds check
                    const argv_start = std.mem.indexOf(u8, arr_expr, "argv[").?;
                    const bracket_end = std.mem.indexOf(u8, arr_expr[argv_start..], "]").? + argv_start;
                    const arg_num_str = arr_expr[argv_start + 5 .. bracket_end]; // Get the N in argv[N]
                    try self.printLine("stack[sp] = if ({s} < argc) blk: {{ var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, ({s}).toJSValue()); break :blk CV.fromJSValue(JSValue.getPropertyUint32(ctx, argv[{s}], @intCast(idx_i32))); }} else CV.UNDEFINED; sp += 1;", .{ arg_num_str, idx_expr, arg_num_str });
                } else {
                    try self.printLine("{{ const arr = ({s}).toJSValue(); var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, ({s}).toJSValue()); stack[sp] = CV.fromJSValue(JSValue.getPropertyUint32(ctx, arr, @intCast(idx_i32))); sp += 1; }}", .{ arr_expr, idx_expr });
                }
                // Don't add to vstack - value is on real stack
            },

            // get_array_el2: pop idx, pop arr, push arr, push arr[idx]
            // Push directly to real stack to avoid cross-block temp scope issues
            .get_array_el2 => {
                const idx_expr = self.vpop() orelse "CV.UNDEFINED";
                const idx_free = self.isAllocated(idx_expr);
                defer if (idx_free) self.allocator.free(idx_expr);
                const arr_expr = self.vpop() orelse "CV.UNDEFINED";
                const arr_free = self.isAllocated(arr_expr);
                defer if (arr_free) self.allocator.free(arr_expr);
                // Push arr back, then element - both to real stack
                try self.printLine("{{ const arr_val = ({s}).toJSValue(); var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, ({s}).toJSValue()); stack[sp] = {s}; stack[sp + 1] = CV.fromJSValue(JSValue.getPropertyUint32(ctx, arr_val, @intCast(idx_i32))); sp += 2; }}", .{ arr_expr, idx_expr, arr_expr });
                // Don't add to vstack - values are on real stack
            },

            // Stack operations
            .dup => {
                if (self.vpeek()) |expr| {
                    try self.vpush(expr);
                }
            },
            .drop => {
                if (self.vpop()) |expr| {
                    self.allocator.free(expr);
                }
            },
            .swap => {
                const b = self.vpop();
                const a = self.vpop();
                if (b) |be| try self.vstack.append(self.allocator, be);
                if (a) |ae| try self.vstack.append(self.allocator, ae);
            },

            // Control flow
            .if_false, .if_false8 => {
                const cond_expr = self.vpop() orelse "stack[sp - 1]";
                const should_free = self.isAllocated(cond_expr);
                defer if (should_free) self.allocator.free(cond_expr);
                // if_false always consumes the condition from stack
                const needs_sp_dec = std.mem.startsWith(u8, cond_expr, "stack[sp");
                if (loop) |l| {
                    const target = block.successors.items[0];
                    if (l.exit_block != null and target == l.exit_block.?) {
                        // Jump to loop exit
                        if (needs_sp_dec) {
                            try self.writeLine("{ const _cond = stack[sp - 1]; sp -= 1; if (!_cond.toBool()) break; }");
                        } else {
                            try self.printLine("if (!({s}).toBool()) break;", .{cond_expr});
                        }
                    } else if (target == l.header_block) {
                        // Jump back to loop header (continue)
                        if (needs_sp_dec) {
                            try self.writeLine("{ const _cond = stack[sp - 1]; sp -= 1; if (!_cond.toBool()) continue; }");
                        } else {
                            try self.printLine("if (!({s}).toBool()) continue;", .{cond_expr});
                        }
                    } else {
                        // Target is within loop body - this is an if-statement, not loop control
                        // Pop condition BEFORE emitting if, body will be indented
                        if (needs_sp_dec) {
                            try self.writeLine("sp -= 1; // pop condition");
                            try self.writeLine("if (stack[sp].toBool()) {");
                        } else {
                            try self.printLine("if (({s}).toBool()) {{", .{cond_expr});
                        }
                        self.pushIndent();
                        self.if_body_depth += 1;
                        try self.if_target_blocks.append(self.allocator, target);
                    }
                }
            },
            .if_true, .if_true8 => {
                const cond_expr = self.vpop() orelse "stack[sp - 1]";
                const should_free = self.isAllocated(cond_expr);
                defer if (should_free) self.allocator.free(cond_expr);
                // if_true always consumes the condition from stack
                const needs_sp_dec = std.mem.startsWith(u8, cond_expr, "stack[sp");
                if (loop) |l| {
                    const target = block.successors.items[0];
                    if (l.exit_block != null and target == l.exit_block.?) {
                        // Jump to loop exit
                        if (needs_sp_dec) {
                            try self.writeLine("{ const _cond = stack[sp - 1]; sp -= 1; if (_cond.toBool()) break; }");
                        } else {
                            try self.printLine("if (({s}).toBool()) break;", .{cond_expr});
                        }
                    } else if (target == l.header_block) {
                        // Jump back to loop header (continue)
                        if (needs_sp_dec) {
                            try self.writeLine("{ const _cond = stack[sp - 1]; sp -= 1; if (_cond.toBool()) continue; }");
                        } else {
                            try self.printLine("if (({s}).toBool()) continue;", .{cond_expr});
                        }
                    } else {
                        // Target is within loop body - this is an if-statement, not loop control
                        // Pop condition BEFORE emitting if, body will be indented
                        if (needs_sp_dec) {
                            try self.writeLine("sp -= 1; // pop condition");
                            try self.writeLine("if (!stack[sp].toBool()) {");
                        } else {
                            try self.printLine("if (!({s}).toBool()) {{", .{cond_expr});
                        }
                        self.pushIndent();
                        self.if_body_depth += 1;
                        try self.if_target_blocks.append(self.allocator, target);
                    }
                }
            },
            .goto, .goto8, .goto16 => {
                if (loop) |l| {
                    if (block.successors.items.len > 0) {
                        const target = block.successors.items[0];
                        if (self.debug_mode) {
                            if (CODEGEN_DEBUG) std.debug.print("[goto] block {d} -> target {d}, loop_header={d}, exit={?}\n", .{ block.id, target, l.header_block, l.exit_block });
                        }
                        if (target == l.header_block) {
                            try self.writeLine("continue;");
                        } else if (l.exit_block != null and target == l.exit_block.?) {
                            try self.writeLine("break;");
                        } else {
                            // Check if target is a parent loop's header
                            for (self.natural_loops) |parent| {
                                if (parent.header_block == target) {
                                    if (self.debug_mode) {
                                        if (CODEGEN_DEBUG) std.debug.print("[goto] found parent loop header, emitting continue\n", .{});
                                    }
                                    try self.writeLine("continue;");
                                    break;
                                }
                            }
                        }
                    }
                }
            },

            // Return
            .@"return" => {
                const result = self.vpop() orelse "CV.UNDEFINED";
                const should_free = self.isAllocated(result);
                defer if (should_free) self.allocator.free(result);
                try self.printLine("return ({s}).toJSValue();", .{result});
                self.block_terminated = true;
            },
            .return_undef => {
                try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
                self.block_terminated = true;
            },

            // Fallback to stack-based for unsupported opcodes
            else => {
                // Materialize virtual stack to real stack for complex ops
                try self.materializeVStack();
                // Emit using stack-based codegen (pass idx for pattern matching like tryEmitNativeMathCall)
                const continues = try self.emitInstruction(instr, block.instructions, idx);
                if (!continues) {
                    // Block terminated by return/throw/unsupported opcode
                    
                    self.block_terminated = true;
                    return;
                }
                // After fallback, force all subsequent ops in this block to use stack-based codegen
                // This avoids vstack/stack mismatches when mixing expression and stack modes
                self.force_stack_mode = true;
            },
        }
    }

    /// Materialize virtual stack expressions to the real stack
    fn materializeVStack(self: *Self) !void {
        for (self.vstack.items) |expr| {
            try self.printLine("stack[sp] = {s}; sp += 1;", .{expr});
            self.allocator.free(expr);
        }
        self.vstack.clearRetainingCapacity();
    }

    /// Check if an opcode pushes a result onto the stack that should be tracked
    fn pushesResult(self: *Self, opcode: Opcode) bool {
        _ = self;
        return switch (opcode) {
            // Call instructions - pop function/args, push result
            .call, .call0, .call1, .call2, .call3, .call_method, .call_constructor => true,
            // Property access - pop object, push value
            .get_field, .get_field2, .get_array_el, .get_array_el2 => true,
            // Global access - push value
            .get_var, .get_var_undef => true,
            // Most other opcodes don't push a net result that needs tracking
            else => false,
        };
    }

    // ========================================================================
    // Instruction Emission
    // ========================================================================

    /// Emit instruction code. Returns true if control continues, false if it terminates.
    /// Takes block instructions and current index for lookahead support.
    fn emitInstruction(self: *Self, instr: Instruction, block_instrs: []const Instruction, instr_idx: usize) !bool {
        switch (instr.opcode) {
            // Constants - all use CV (8-byte NaN-boxed, no refcount)
            .push_0 => try self.writeLine("stack[sp] = CV.newInt(0); sp += 1;"),
            .push_1 => try self.writeLine("stack[sp] = CV.newInt(1); sp += 1;"),
            .push_i32 => try self.printLine("stack[sp] = CV.newInt({d}); sp += 1;", .{instr.operand.i32}),
            .push_i8 => try self.printLine("stack[sp] = CV.newInt({d}); sp += 1;", .{instr.operand.i8}),
            .push_true => try self.writeLine("stack[sp] = CV.TRUE; sp += 1;"),
            .push_false => try self.writeLine("stack[sp] = CV.FALSE; sp += 1;"),
            .null => try self.writeLine("stack[sp] = CV.NULL; sp += 1;"),
            .undefined => try self.writeLine("stack[sp] = CV.UNDEFINED; sp += 1;"),

            // Stack operations - CV is value type, no dup/free needed
            .dup => try self.writeLine("stack[sp] = stack[sp - 1]; sp += 1;"),
            .drop => {
                try self.writeLine("sp -= 1;");
            },
            .swap => {
                try self.writeLine("{ const tmp = stack[sp - 1]; stack[sp - 1] = stack[sp - 2]; stack[sp - 2] = tmp; }");
            },

            // Local variables - CV is value type, no dup/free needed
            .get_loc => {
                const loc_idx = instr.operand.loc;
                try self.printLine("stack[sp] = locals[{d}]; sp += 1;", .{loc_idx});
            },
            .put_loc => {
                const loc_idx = instr.operand.loc;
                try self.printLine("locals[{d}] = stack[sp - 1]; sp -= 1;", .{loc_idx});
            },
            .set_loc => {
                const loc_idx = instr.operand.loc;
                try self.printLine("locals[{d}] = stack[sp - 1];", .{loc_idx});
            },
            .get_loc0 => try self.writeLine("stack[sp] = locals[0]; sp += 1;"),
            .get_loc1 => try self.writeLine("stack[sp] = locals[1]; sp += 1;"),
            .get_loc2 => try self.writeLine("stack[sp] = locals[2]; sp += 1;"),
            .get_loc3 => try self.writeLine("stack[sp] = locals[3]; sp += 1;"),
            .put_loc0 => try self.writeLine("locals[0] = stack[sp - 1]; sp -= 1;"),
            .put_loc1 => try self.writeLine("locals[1] = stack[sp - 1]; sp -= 1;"),
            .put_loc2 => try self.writeLine("locals[2] = stack[sp - 1]; sp -= 1;"),
            .put_loc3 => try self.writeLine("locals[3] = stack[sp - 1]; sp -= 1;"),

            // Arguments - convert JSValue → CV at entry
            .get_arg => {
                const arg_idx = instr.operand.arg;
                try self.printLine("stack[sp] = if ({d} < argc) CV.fromJSValue(argv[{d}]) else CV.UNDEFINED; sp += 1;", .{ arg_idx, arg_idx });
            },
            .get_arg0 => try self.writeLine("stack[sp] = if (0 < argc) CV.fromJSValue(argv[0]) else CV.UNDEFINED; sp += 1;"),
            .get_arg1 => try self.writeLine("stack[sp] = if (1 < argc) CV.fromJSValue(argv[1]) else CV.UNDEFINED; sp += 1;"),
            .get_arg2 => try self.writeLine("stack[sp] = if (2 < argc) CV.fromJSValue(argv[2]) else CV.UNDEFINED; sp += 1;"),
            .get_arg3 => try self.writeLine("stack[sp] = if (3 < argc) CV.fromJSValue(argv[3]) else CV.UNDEFINED; sp += 1;"),

            // Arithmetic - always use CompressedValue (8-byte NaN-boxed)
            .add => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.add(a, b); sp -= 1; }"),
            .sub => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.sub(a, b); sp -= 1; }"),
            .mul => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.mul(a, b); sp -= 1; }"),
            .div => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.div(a, b); sp -= 1; }"),
            .mod => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.mod(a, b); sp -= 1; }"),
            .neg => try self.writeLine("{ const a = stack[sp-1]; stack[sp-1] = CV.sub(CV.newInt(0), a); }"),

            // Comparisons - always use CompressedValue
            .lt => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.lt(a, b); sp -= 1; }"),
            .lte => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = if (CV.gt(a, b).toBool()) CV.FALSE else CV.TRUE; sp -= 1; }"),
            .gt => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.gt(a, b); sp -= 1; }"),
            .gte => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = if (CV.lt(a, b).toBool()) CV.FALSE else CV.TRUE; sp -= 1; }"),
            .eq => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.eq(a, b); sp -= 1; }"),
            .neq => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = if (CV.eq(a, b).toBool()) CV.FALSE else CV.TRUE; sp -= 1; }"),

            // Bitwise - all use CV inline (no refcount)
            .@"and" => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.bitAnd(a, b); sp -= 1; }"),
            .@"or" => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.bitOr(a, b); sp -= 1; }"),
            .xor => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.bitXor(a, b); sp -= 1; }"),
            .not => try self.writeLine("{ const a = stack[sp-1]; stack[sp-1] = CV.bitNot(a); }"),
            .shl => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.shl(a, b); sp -= 1; }"),
            .sar => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.sar(a, b); sp -= 1; }"),
            .shr => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.shr(a, b); sp -= 1; }"),

            // Control flow (jumps handled by block terminators)
            .if_false, .if_true, .if_false8, .if_true8, .goto, .goto8, .goto16 => {
                // These are handled by block successor logic, not individual instructions
            },

            // Return - control terminates (CV→JSValue at exit)
            .@"return" => {
                try self.writeLine("return stack[sp - 1].toJSValue();");
                return false; // Control terminates
            },
            .return_undef => {
                try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
                return false; // Control terminates
            },

            // Increment/Decrement - all CV, no refcount
            .inc => {
                try self.writeLine("{ const a = stack[sp-1]; stack[sp-1] = CV.add(a, CV.newInt(1)); }");
            },
            .dec => {
                try self.writeLine("{ const a = stack[sp-1]; stack[sp-1] = CV.sub(a, CV.newInt(1)); }");
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
                } else if (self.func.is_self_recursive and self.func.self_ref_var_idx >= 0 and bytecode_idx == @as(u16, @intCast(self.func.self_ref_var_idx))) {
                    // Self-reference: Check if this leads to a self-call by looking ahead
                    // If the next instruction that consumes from the stack is a call (call1-3),
                    // this is a self-call pattern and we can use direct recursion.
                    // Otherwise, the value is used for something else (like counter++).
                    const is_self_call = self.isFollowedBySelfCall(block_instrs, instr_idx);
                    if (is_self_call) {
                        // Self-call pattern - don't push, set flag for call optimization
                        try self.printLine("// get_var_ref{d}: self-reference for self-call", .{bytecode_idx});
                        self.pending_self_call = true;
                    } else {
                        // Not a self-call pattern - push UNDEFINED (no closure access for self-ref)
                        try self.printLine("// get_var_ref{d}: self-reference (non-call usage)", .{bytecode_idx});
                        try self.writeLine("stack[sp] = CV.UNDEFINED; sp += 1;");
                    }
                } else {
                    // Closure var not found - fall back to interpreter for this block
                    // This handles module-level variables accessed via get_var_ref
                    const js_name = if (self.func.js_name.len > 0) self.func.js_name else self.func.name;
                    try self.printLine("// get_var_ref{d}: not in closure - falling back to interpreter", .{bytecode_idx});
                    try self.writeLine("{");
                    try self.writeLine("    var next_block_fb: usize = 0;");
                    try self.printLine("    const fb_result = zig_runtime.blockFallback(ctx, \"{s}\", this_val, argc, argv, &locals, {d}, &stack, &sp, {d}, &next_block_fb);", .{ js_name, self.func.var_count, self.current_block_idx });
                    try self.writeLine("    if (!fb_result.isUndefined()) return fb_result;");
                    try self.writeLine("    block_id = @intCast(next_block_fb);");
                    try self.writeLine("    continue;");
                    try self.writeLine("}");
                }
            },

            // ================================================================
            // Property Access
            // ================================================================

            // get_field: pop obj, push obj.prop
            // Uses native shape access for known AST properties (kind, flags, pos, end, parent)
            .get_field => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |prop_name| {
                    // Check if this is a native-optimizable property
                    if (std.mem.eql(u8, prop_name, "kind")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValue(); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetKind(ctx, obj)); JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "flags")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValue(); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetFlags(ctx, obj)); JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "pos")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValue(); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetPos(ctx, obj)); JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "end")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValue(); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetEnd(ctx, obj)); JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "parent")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValue(); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetParent(ctx, obj)); JSValue.free(ctx, obj); }");
                    } else {
                        // Standard property access via QuickJS - convert CV <-> JSValue
                        const escaped_prop = escapeZigString(self.allocator, prop_name) catch prop_name;
                        defer if (escaped_prop.ptr != prop_name.ptr) self.allocator.free(escaped_prop);
                        try self.printLine("{{ const obj = stack[sp-1].toJSValue(); stack[sp-1] = CV.fromJSValue(JSValue.getPropertyStr(ctx, obj, \"{s}\")); JSValue.free(ctx, obj); }}", .{escaped_prop});
                    }
                    // Sync vstack: get_field replaces top of stack, update vstack accordingly
                    if (self.vpop()) |old_expr| {
                        if (self.isAllocated(old_expr)) self.allocator.free(old_expr);
                    }
                    try self.vpush("stack[sp - 1]");
                } else {
                    try self.writeLine("// get_field: atom index out of range");
                    try self.writeLine("return JSValue.throwTypeError(ctx, \"Invalid property access\");");
                    return false; // Control terminates
                }
            },

            // get_field2: pop obj, push obj, obj.prop (keeps object on stack)
            // Uses native shape access for known AST properties (kind, flags, pos, end, parent)
            .get_field2 => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |prop_name| {
                    // Check if this is a native-optimizable property
                    if (std.mem.eql(u8, prop_name, "kind")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValue(); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetKind(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "flags")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValue(); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetFlags(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "pos")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValue(); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetPos(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "end")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValue(); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetEnd(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "parent")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValue(); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetParent(ctx, obj)); sp += 1; }");
                    } else {
                        // Standard property access via QuickJS - convert CV <-> JSValue
                        const escaped_prop = escapeZigString(self.allocator, prop_name) catch prop_name;
                        defer if (escaped_prop.ptr != prop_name.ptr) self.allocator.free(escaped_prop);
                        try self.printLine("{{ const obj = stack[sp-1].toJSValue(); stack[sp] = CV.fromJSValue(JSValue.getPropertyStr(ctx, obj, \"{s}\")); sp += 1; }}", .{escaped_prop});
                    }
                    // Sync vstack: get_field2 keeps object and pushes property value
                    // vstack now has property value at top (stack[sp-1] after sp was incremented)
                    try self.vpush("stack[sp - 1]");
                } else {
                    try self.writeLine("// get_field2: atom index out of range");
                    try self.writeLine("return JSValue.throwTypeError(ctx, \"Invalid property access\");");
                    return false; // Control terminates
                }
            },

            // put_field: pop val, pop obj, obj.prop = val
            .put_field => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |prop_name| {
                    const escaped_prop = escapeZigString(self.allocator, prop_name) catch prop_name;
                    defer if (escaped_prop.ptr != prop_name.ptr) self.allocator.free(escaped_prop);
                    // Convert CV to JSValue for setPropertyStr
                    try self.printLine("{{ const val = stack[sp-1].toJSValue(); const obj = stack[sp-2].toJSValue(); _ = JSValue.setPropertyStr(ctx, obj, \"{s}\", val); JSValue.free(ctx, obj); sp -= 2; }}", .{escaped_prop});
                    // Sync vstack: pops 2 (val, obj)
                    if (self.vpop()) |e| if (self.isAllocated(e)) self.allocator.free(e);
                    if (self.vpop()) |e| if (self.isAllocated(e)) self.allocator.free(e);
                } else {
                    try self.writeLine("// put_field: atom index out of range");
                    try self.writeLine("return JSValue.throwTypeError(ctx, \"Invalid property access\");");
                    return false; // Control terminates
                }
            },

            // get_array_el: pop idx, pop arr, push arr[idx]
            // Note: Don't free arr/idx as they may be function arguments we don't own
            .get_array_el => {
                try self.writeLine("{ const idx = stack[sp-1]; const arr = stack[sp-2]; var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, idx.toJSValue()); stack[sp-2] = CV.fromJSValue(JSValue.getPropertyUint32(ctx, arr.toJSValue(), @intCast(idx_i32))); sp -= 1; }");
                // Sync vstack: pops 2 (idx, arr), pushes 1 (result)
                if (self.vpop()) |e| if (self.isAllocated(e)) self.allocator.free(e);
                if (self.vpop()) |e| if (self.isAllocated(e)) self.allocator.free(e);
                try self.vpush("stack[sp - 1]");
            },

            // get_array_el2: pop idx, pop arr, push arr, push arr[idx]
            // Note: Don't free idx as it may be a function argument we don't own
            .get_array_el2 => {
                try self.writeLine("{ const idx = stack[sp-1]; const arr = stack[sp-2]; var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, idx.toJSValue()); stack[sp-1] = CV.fromJSValue(JSValue.getPropertyUint32(ctx, arr.toJSValue(), @intCast(idx_i32))); }");
                // Sync vstack: pops 2 (idx, arr), pushes 2 (arr, result) - net 0 but top changes
                if (self.vpop()) |e| if (self.isAllocated(e)) self.allocator.free(e);
                if (self.vpop()) |e| if (self.isAllocated(e)) self.allocator.free(e);
                try self.vpush("stack[sp - 2]");
                try self.vpush("stack[sp - 1]");
            },

            // put_array_el: pop val, pop idx, pop arr, arr[idx] = val
            // Stack order is [arr, idx, val] - arr at bottom, then index, then value on top
            // Note: JS_SetPropertyUint32 consumes val but not arr or idx, so don't free them
            // (they may be function arguments that we don't own)
            .put_array_el => {
                try self.writeLine("{ const val = stack[sp-1]; const idx = stack[sp-2]; const arr = stack[sp-3]; var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, idx.toJSValue()); _ = JSValue.setPropertyUint32(ctx, arr.toJSValue(), @intCast(idx_i32), val.toJSValue()); sp -= 3; }");
                // Sync vstack: pops 3 (val, arr, idx)
                if (self.vpop()) |e| if (self.isAllocated(e)) self.allocator.free(e);
                if (self.vpop()) |e| if (self.isAllocated(e)) self.allocator.free(e);
                if (self.vpop()) |e| if (self.isAllocated(e)) self.allocator.free(e);
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
                // Check for Math.method pattern and emit native code if possible
                // NOTE: Don't use native Math in force_stack_mode because get_var/get_field2
                // weren't skipped, so the stack has Math object and method on it
                if (!self.force_stack_mode and try self.tryEmitNativeMathCall(argc, block_instrs, instr_idx)) {
                    // Native math emitted - sync vstack
                    // Math.method(arg) replaces stack[sp-1] in-place
                    // Pop the arg from vstack and push result reference
                    _ = self.vpop(); // pop the argument
                    try self.vpush("stack[sp - 1]"); // push result reference
                } else {
                    try self.emitCallMethod(argc);
                }
            },

            // ================================================================
            // Additional High-Frequency Opcodes (discovered from TSC analysis)
            // ================================================================

            // get_loc_check: get local with TDZ (Temporal Dead Zone) check
            .get_loc_check => {
                const loc_idx = instr.operand.loc;
                try self.printLine("{{ const v = locals[{d}]; if (v.isUninitialized()) return JSValue.throwReferenceError(ctx, \"Cannot access before initialization\"); stack[sp] = CV.fromJSValue(JSValue.dup(ctx, v.toJSValue())); sp += 1; }}", .{loc_idx});
            },

            // get_var: get variable by name from scope
            .get_var => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |var_name| {
                    const escaped_name = escapeZigString(self.allocator, var_name) catch var_name;
                    defer if (escaped_name.ptr != var_name.ptr) self.allocator.free(escaped_name);
                    try self.printLine("stack[sp] = CV.fromJSValue(JSValue.getGlobal(ctx, \"{s}\")); sp += 1;", .{escaped_name});
                } else {
                    try self.writeLine("return JSValue.throwReferenceError(ctx, \"Variable not found\");");
                }
            },

            // push_atom_value: push atom as value (usually for property names/strings)
            .push_atom_value => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |str_val| {
                    const escaped_str = escapeZigString(self.allocator, str_val) catch str_val;
                    defer if (escaped_str.ptr != str_val.ptr) self.allocator.free(escaped_str);
                    try self.printLine("stack[sp] = CV.fromJSValue(JSValue.newString(ctx, \"{s}\")); sp += 1;", .{escaped_str});
                } else {
                    try self.writeLine("stack[sp] = CV.UNDEFINED; sp += 1;");
                }
            },

            // set_loc_uninitialized: set local to uninitialized state (for TDZ)
            .set_loc_uninitialized => {
                const loc_idx = instr.operand.loc;
                try self.printLine("locals[{d}] = CV.UNINITIALIZED;", .{loc_idx});
            },

            // get_var_ref_check: get closure var with TDZ check
            .get_var_ref_check => {
                const bytecode_idx = instr.operand.var_ref;
                const pos = self.findClosureVarPosition(bytecode_idx);
                if (pos) |p| {
                    try self.printLine("stack[sp] = CV.fromJSValue(zig_runtime.getClosureVarCheck(ctx, argv, argc, {d})); sp += 1;", .{p});
                    try self.writeLine("if (stack[sp-1].isException()) return stack[sp-1].toJSValue();");
                } else {
                    try self.writeLine("// get_var_ref_check: not in closure_var_indices");
                    try self.writeLine("stack[sp] = CV.UNDEFINED; sp += 1;");
                }
            },

            // get_var_ref: get closure var by index (generic version)
            .get_var_ref => {
                const bytecode_idx = instr.operand.var_ref;
                const pos = self.findClosureVarPosition(bytecode_idx);
                if (pos) |p| {
                    try self.printLine("stack[sp] = CV.fromJSValue(zig_runtime.getClosureVar(ctx, argv, argc, {d})); sp += 1;", .{p});
                } else {
                    try self.printLine("// get_var_ref {d}: not in closure_var_indices", .{bytecode_idx});
                    try self.writeLine("stack[sp] = CV.UNDEFINED; sp += 1;");
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
                try self.writeLine("stack[sp] = CV.fromJSValue(JSValue.dup(ctx, this_val)); sp += 1;");
            },

            // define_field: define a field on object (for object literals)
            .define_field => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |field_name| {
                    const escaped_field = escapeZigString(self.allocator, field_name) catch field_name;
                    defer if (escaped_field.ptr != field_name.ptr) self.allocator.free(escaped_field);
                    try self.printLine("{{ const val = stack[sp-1].toJSValue(); const obj = stack[sp-2].toJSValue(); _ = JSValue.definePropertyStr(ctx, obj, \"{s}\", val); sp -= 1; }}", .{escaped_field});
                } else {
                    try self.writeLine("return JSValue.throwTypeError(ctx, \"Invalid field name\");");
                    return false; // Control terminates
                }
            },

            // fclosure: create closure (8-bit index variant)
            .fclosure8 => {
                try self.writeLine("// fclosure8: closure creation - not yet implemented");
                try self.writeLine("stack[sp] = CV.UNDEFINED; sp += 1;");
            },

            // call_constructor: call function as constructor (new X())
            .call_constructor => {
                const argc = instr.operand.u16;
                try self.emitCallConstructor(argc);
            },

            // object: create empty object
            .object => {
                try self.writeLine("stack[sp] = CV.fromJSValue(JSValue.newObject(ctx)); sp += 1;");
            },

            // strict_eq: strict equality (===)
            .strict_eq => {
                // Pop both operands from vstack (they're being consumed)
                _ = self.vpop();
                _ = self.vpop();
                // Note: Don't free a/b as they may be function arguments we don't own
                try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.fromJSValue(JSValue.newBool(JSValue.strictEq(a.toJSValue(), b.toJSValue()))); sp -= 1; }");
                // Push result reference to vstack so if_false can pop it
                try self.vpush("stack[sp - 1]");
            },

            // strict_neq: strict inequality (!==)
            .strict_neq => {
                // Pop both operands from vstack (they're being consumed)
                _ = self.vpop();
                _ = self.vpop();
                // Note: Don't free a/b as they may be function arguments we don't own
                try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.fromJSValue(JSValue.newBool(!JSValue.strictEq(a.toJSValue(), b.toJSValue()))); sp -= 1; }");
                // Push result reference to vstack so if_false can pop it
                try self.vpush("stack[sp - 1]");
            },

            // tail_call_method: tail call optimization for method calls
            .tail_call_method => {
                const argc = instr.operand.u16;
                // For now, emit as regular call_method followed by return
                try self.emitCallMethod(argc);
                try self.writeLine("return stack[sp - 1].toJSValue();");
                return false; // Control terminates
            },

            // get_loc8: get local (8-bit index, common variant)
            .get_loc8 => {
                const loc_idx = instr.operand.loc;
                try self.printLine("stack[sp] = locals[{d}]; sp += 1;", .{loc_idx});
            },

            // put_loc8: put local (8-bit index, common variant) - CV is value type, no free
            .put_loc8 => {
                const loc_idx = instr.operand.loc;
                try self.printLine("locals[{d}] = stack[sp-1]; sp -= 1;", .{loc_idx});
            },

            // put_loc_check: put local with TDZ check - CV is value type
            .put_loc_check => {
                const loc_idx = instr.operand.loc;
                try self.printLine("locals[{d}] = stack[sp-1]; sp -= 1;", .{loc_idx});
            },

            // get_length: get .length property (optimized)
            .get_length => {
                // Convert CV <-> JSValue for property access
                try self.writeLine("{ const obj = stack[sp-1].toJSValue(); stack[sp-1] = CV.fromJSValue(JSValue.getPropertyStr(ctx, obj, \"length\")); JSValue.free(ctx, obj); }");
                // Sync vstack: replaces top of stack
                if (self.vpop()) |e| if (self.isAllocated(e)) self.allocator.free(e);
                try self.vpush("stack[sp - 1]");
            },

            // push_i16: push 16-bit integer
            .push_i16 => {
                const val = instr.operand.i16;
                try self.printLine("stack[sp] = CV.newInt({d}); sp += 1;", .{val});
            },

            // push_2: push literal 2
            .push_2 => {
                try self.writeLine("stack[sp] = CV.newInt(2); sp += 1;");
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

            // to_propkey2: convert second item to property key (NOT IMPLEMENTED)
            .to_propkey2 => {
                
                try self.writeLine("// to_propkey2: convert second stack item to property key");
                try self.writeLine("// UNSUPPORTED: opcode 19 - fallback to interpreter");
                try self.writeLine("return JSValue.throwTypeError(ctx, \"Unsupported opcode in frozen function\");");
                return false; // Control terminates
            },

            // throw: throw exception
            .throw => {
                try self.writeLine("{ const exc = stack[sp-1]; sp -= 1; return JSValue.throw(ctx, exc.toJSValue()); }");
            },

            // typeof: get type string - CV to JSValue conversion
            .typeof => {
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = CV.fromJSValue(JSValue.typeOf(ctx, v.toJSValue())); }");
            },

            // lnot: logical not - CV needs conversion to JSValue for toBool call
            .lnot => {
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = if (JSValue.toBool(ctx, v.toJSValue()) != 0) CV.FALSE else CV.TRUE; }");
            },

            // typeof_is_function: check if typeof == "function" - CV to JSValue conversion
            .typeof_is_function => {
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = if (JSValue.isFunction(v.toJSValue())) CV.TRUE else CV.FALSE; }");
            },

            // typeof_is_undefined: check if typeof == "undefined" - use CV's isUndefined method
            .typeof_is_undefined => {
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = if (v.isUndefined()) CV.TRUE else CV.FALSE; }");
            },

            // special_object: create special object (arguments, etc.)
            .special_object => {
                try self.writeLine("// special_object: creating special object");
                try self.writeLine("stack[sp] = CV.fromJSValue(JSValue.newObject(ctx)); sp += 1;");
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
                try self.writeLine("stack[sp] = CV.fromJSValue(JSValue.newString(ctx, \"\")); sp += 1;");
            },

            // is_undefined_or_null: check if undefined or null
            .is_undefined_or_null => {
                // Pop operand from vstack (being consumed)
                _ = self.vpop();
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = if (v.bits == CV.UNDEFINED.bits or v.bits == CV.NULL.bits) CV.TRUE else CV.FALSE; }");
                // Push result reference to vstack so if_true/if_false can pop it
                try self.vpush("stack[sp - 1]");
            },

            // set_name: set function name
            .set_name => {
                try self.writeLine("// set_name: setting function name (no-op for frozen)");
            },

            // ================================================================
            // Additional Opcodes (second batch from discovery)
            // ================================================================

            // tail_call: tail call optimization
            .tail_call => {
                const argc = instr.operand.u16;
                // Check for self-recursive tail call - use TCO via block_id reset
                if (self.pending_self_call and self.func.is_self_recursive) {
                    self.pending_self_call = false;
                    try self.writeLine("{");
                    self.pushIndent();
                    try self.writeLine("// TCO: Self-recursive tail call - reset to block 0");
                    // Reassign argv with new arguments from stack
                    if (argc > 0) {
                        for (0..argc) |i| {
                            // Args are on stack at sp-argc..sp-1
                            // New argv[i] = stack[sp - argc + i]
                            try self.printLine("{{ const new_arg = stack[sp - {d}]; JSValue.free(ctx, argv[{d}]); argv[{d}] = new_arg; }}", .{ argc - i, i, i });
                        }
                        try self.printLine("sp -= {d};", .{argc});
                    }
                    // Free remaining stack and locals before restart
                    try self.writeLine("for (0..sp) |i| JSValue.free(ctx, stack[i]);");
                    try self.writeLine("for (&locals) |*loc| {{ JSValue.free(ctx, loc.*); loc.* = JSValue.UNDEFINED; }}");
                    try self.writeLine("sp = 0;");
                    try self.writeLine("block_id = 0; continue; // TCO restart");
                    self.popIndent();
                    try self.writeLine("}");
                    return false; // Control terminates (via continue)
                }
                // Non-self tail call - fall back to regular call + return
                try self.emitCall(argc);
                try self.writeLine("return stack[sp - 1].toJSValue();");
                return false; // Control terminates
            },

            // is_undefined: check if value is undefined
            .is_undefined => {
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = if (v.isUndefined()) CV.TRUE else CV.FALSE; }");
            },

            // is_null: check if value is null
            .is_null => {
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = if (v.isNull()) CV.TRUE else CV.FALSE; }");
            },

            // put_loc_check_init: put local with TDZ check for initialization
            .put_loc_check_init => {
                const loc_idx = instr.operand.loc;
                try self.printLine("{{ locals[{d}] = stack[sp-1]; sp -= 1; }}", .{loc_idx});
            },

            // post_inc: post-increment (x++)
            .post_inc => {
                try self.writeLine("{ const v = stack[sp-1]; var val: i32 = 0; _ = JSValue.toInt32(ctx, &val, v.toJSValue()); stack[sp-1] = v; stack[sp] = CV.newInt(val + 1); sp += 1; }");
            },

            // post_dec: post-decrement (x--)
            .post_dec => {
                try self.writeLine("{ const v = stack[sp-1]; var val: i32 = 0; _ = JSValue.toInt32(ctx, &val, v.toJSValue()); stack[sp-1] = v; stack[sp] = CV.newInt(val - 1); sp += 1; }");
            },

            // put_arg0-3: put argument - CV to JSValue conversion for argv array
            .put_arg0 => {
                try self.writeLine("{ if (argc > 0) { JSValue.free(ctx, argv[0]); argv[0] = stack[sp-1].toJSValue(); } sp -= 1; }");
            },
            .put_arg1 => {
                try self.writeLine("{ if (argc > 1) { JSValue.free(ctx, argv[1]); argv[1] = stack[sp-1].toJSValue(); } sp -= 1; }");
            },
            .put_arg2 => {
                try self.writeLine("{ if (argc > 2) { JSValue.free(ctx, argv[2]); argv[2] = stack[sp-1].toJSValue(); } sp -= 1; }");
            },
            .put_arg3 => {
                try self.writeLine("{ if (argc > 3) { JSValue.free(ctx, argv[3]); argv[3] = stack[sp-1].toJSValue(); } sp -= 1; }");
            },

            // set_arg0-3: set argument (like put but keeps on stack) - CV to JSValue conversion
            .set_arg0 => {
                try self.writeLine("{ if (argc > 0) { JSValue.free(ctx, argv[0]); argv[0] = JSValue.dup(ctx, stack[sp-1].toJSValue()); } }");
            },
            .set_arg1 => {
                try self.writeLine("{ if (argc > 1) { JSValue.free(ctx, argv[1]); argv[1] = JSValue.dup(ctx, stack[sp-1].toJSValue()); } }");
            },
            .set_arg2 => {
                try self.writeLine("{ if (argc > 2) { JSValue.free(ctx, argv[2]); argv[2] = JSValue.dup(ctx, stack[sp-1].toJSValue()); } }");
            },
            .set_arg3 => {
                try self.writeLine("{ if (argc > 3) { JSValue.free(ctx, argv[3]); argv[3] = JSValue.dup(ctx, stack[sp-1].toJSValue()); } }");
            },

            // push_3-7: push literal integers
            .push_3 => {
                try self.writeLine("stack[sp] = CV.newInt(3); sp += 1;");
            },
            .push_4 => {
                try self.writeLine("stack[sp] = CV.newInt(4); sp += 1;");
            },
            .push_5 => {
                try self.writeLine("stack[sp] = CV.newInt(5); sp += 1;");
            },
            .push_6 => {
                try self.writeLine("stack[sp] = CV.newInt(6); sp += 1;");
            },
            .push_7 => {
                try self.writeLine("stack[sp] = CV.newInt(7); sp += 1;");
            },
            .push_minus1 => {
                try self.writeLine("stack[sp] = CV.newInt(-1); sp += 1;");
            },

            // set_loc8: set local (keeps value on stack) - CV is value type
            .set_loc8 => {
                const loc_idx = instr.operand.loc;
                try self.printLine("locals[{d}] = stack[sp-1];", .{loc_idx});
            },

            // get_loc0_loc1: push both loc0 and loc1
            .get_loc0_loc1 => {
                // Push both locals to the actual stack (not vstack - this is stack-based codegen)
                try self.writeLine("stack[sp] = locals[0]; sp += 1;");
                try self.writeLine("stack[sp] = locals[1]; sp += 1;");
            },

            // add_loc: add to local variable - CV.add inline
            .add_loc => {
                const loc_idx = instr.operand.loc;
                try self.printLine("locals[{d}] = CV.add(locals[{d}], stack[sp-1]); sp -= 1;", .{ loc_idx, loc_idx });
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
                if (self.getAtomString(atom_idx)) |var_name| {
                    const escaped_name = escapeZigString(self.allocator, var_name) catch var_name;
                    defer if (escaped_name.ptr != var_name.ptr) self.allocator.free(escaped_name);
                    try self.printLine("stack[sp] = CV.fromJSValue(JSValue.getGlobalUndef(ctx, \"{s}\")); sp += 1;", .{escaped_name});
                } else {
                    try self.writeLine("stack[sp] = CV.UNDEFINED; sp += 1;");
                }
            },

            // push_const8: push constant from pool (8-bit index)
            .push_const8 => {
                try self.writeLine("// push_const8: constant pool access - not yet implemented");
                try self.writeLine("stack[sp] = CV.UNDEFINED; sp += 1;");
            },

            // define_method: define method on class
            .define_method => {
                try self.writeLine("// define_method: class method definition");
                try self.writeLine("{ const method = stack[sp-1]; const obj = stack[sp-2]; _ = JSValue.setPropertyStr(ctx, obj.toJSValue(), \"method\", method.toJSValue()); sp -= 1; }");
            },

            // set_loc0-3: set local (keeps value on stack) - CV is value type
            .set_loc0 => {
                try self.writeLine("locals[0] = stack[sp-1];");
            },
            .set_loc1 => {
                try self.writeLine("locals[1] = stack[sp-1];");
            },
            .set_loc2 => {
                try self.writeLine("locals[2] = stack[sp-1];");
            },
            .set_loc3 => {
                try self.writeLine("locals[3] = stack[sp-1];");
            },

            // inc_loc: increment local variable - CV.add inline
            .inc_loc => {
                const loc_idx = instr.operand.loc;
                try self.printLine("locals[{d}] = CV.add(locals[{d}], CV.newInt(1));", .{ loc_idx, loc_idx });
            },

            // dec_loc: decrement local variable - CV.sub inline
            .dec_loc => {
                const loc_idx = instr.operand.loc;
                try self.printLine("locals[{d}] = CV.sub(locals[{d}], CV.newInt(1));", .{ loc_idx, loc_idx });
            },

            // put_arg: put argument (generic with index) - CV to JSValue conversion
            .put_arg => {
                const idx = instr.operand.arg;
                try self.printLine("{{ if (argc > {d}) {{ JSValue.free(ctx, argv[{d}]); argv[{d}] = stack[sp-1].toJSValue(); }} sp -= 1; }}", .{ idx, idx, idx });
            },

            // set_arg: set argument (generic with index)
            .set_arg => {
                const idx = instr.operand.arg;
                try self.printLine("{{ if (argc > {d}) {{ JSValue.free(ctx, argv[{d}]); argv[{d}] = JSValue.dup(ctx, stack[sp-1].toJSValue()); }} }}", .{ idx, idx, idx });
            },

            // delete: delete property
            .delete => {
                try self.writeLine("{ const prop = stack[sp-1]; const obj = stack[sp-2]; _ = JSValue.deleteProperty(ctx, obj.toJSValue(), prop.toJSValue()); JSValue.free(ctx, prop.toJSValue()); JSValue.free(ctx, obj.toJSValue()); sp -= 2; stack[sp] = CV.TRUE; sp += 1; }");
            },

            // append: append to array
            .append => {
                try self.writeLine("{ const val = stack[sp-1]; const arr = stack[sp-2]; _ = JSValue.appendArray(ctx, arr.toJSValue(), val.toJSValue()); sp -= 1; }");
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
                try self.writeLine("stack[sp] = CV.fromJSValue(JSValue.newArray(ctx)); sp += 1;");
            },

            // to_object: convert to object
            .to_object => {
                try self.writeLine("// to_object: converting to object (no-op if already object)");
            },

            // define_array_el: define array element (for array literals)
            .define_array_el => {
                try self.writeLine("{ const val = stack[sp-1]; const idx = stack[sp-2]; const arr = stack[sp-3]; var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, idx.toJSValue()); _ = JSValue.setPropertyUint32(ctx, arr.toJSValue(), @intCast(idx_i32), val.toJSValue()); JSValue.free(ctx, idx.toJSValue()); sp -= 2; }");
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
                // js_frozen_for_of_start expects JSValue stack, but we have CV stack
                // Use temp buffer: [obj] -> call -> [iterator, next_method]
                try self.writeLine("var for_of_buf: [2]JSValue = .{ stack[sp - 1].toJSValue(), JSValue.UNDEFINED };");
                try self.writeLine("const rc = zig_runtime.quickjs.js_frozen_for_of_start(ctx, @ptrCast(&for_of_buf[1]), 0);");
                try self.writeLine("if (rc != 0) return JSValue.EXCEPTION;");
                try self.writeLine("stack[sp - 1] = CV.fromJSValue(for_of_buf[0]);  // iterator replaces obj");
                try self.writeLine("stack[sp] = CV.fromJSValue(for_of_buf[1]); sp += 1;  // next_method");
                try self.writeLine("stack[sp] = CV.fromJSValue(zig_runtime.newCatchOffset(0)); sp += 1;");
                self.popIndent();
                try self.writeLine("}");
            },

            // for_of_next: Get next value from iterator
            // Stack before: [iterator, next_method, catch_offset, ...] (iterator at sp-3-offset)
            // Stack after: [..., value, done] (sp increases by 2)
            .for_of_next => {
                const offset = instr.operand.u8;
                const iter_offset = @as(i32, @intCast(offset)) + 3;
                try self.writeLine("{");
                self.pushIndent();
                // js_frozen_for_of_next expects JSValue stack, but we have CV stack
                // Build temp buffer: [iterator, next_method, _, value, done] where _ is placeholder for sp
                try self.printLine("const iter_idx = sp - {d};", .{iter_offset});
                try self.writeLine("var for_of_buf: [5]JSValue = .{");
                self.pushIndent();
                try self.writeLine("stack[iter_idx].toJSValue(),      // iterator");
                try self.writeLine("stack[iter_idx + 1].toJSValue(),  // next_method");
                try self.writeLine("JSValue.UNDEFINED,                // placeholder");
                try self.writeLine("JSValue.UNDEFINED,                // value (output)");
                try self.writeLine("JSValue.UNDEFINED,                // done (output)");
                self.popIndent();
                try self.writeLine("};");
                try self.printLine("const rc = zig_runtime.quickjs.js_frozen_for_of_next(ctx, @ptrCast(&for_of_buf[3]), -{d});", .{iter_offset});
                try self.writeLine("if (rc != 0) return JSValue.EXCEPTION;");
                try self.writeLine("stack[sp] = CV.fromJSValue(for_of_buf[3]); sp += 1;  // value");
                try self.writeLine("stack[sp] = CV.fromJSValue(for_of_buf[4]); sp += 1;  // done");
                self.popIndent();
                try self.writeLine("}");
            },

            // iterator_close: Cleanup iterator after for-of loop
            // Stack before: [iterator, next_method, catch_offset] at sp-3, sp-2, sp-1
            // Stack after: [] (sp decreases by 3)
            .iterator_close => {
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("// Free catch_offset, next_method, iterator (convert CV to JSValue)");
                try self.writeLine("sp -= 1;  // catch_offset - no need to free (primitive)");
                try self.writeLine("sp -= 1; JSValue.free(ctx, stack[sp].toJSValue());  // next_method");
                try self.writeLine("sp -= 1; JSValue.free(ctx, stack[sp].toJSValue());  // iterator");
                self.popIndent();
                try self.writeLine("}");
            },

            // iterator_get_value_done: Extract value and done from iterator result
            // Stack before: [result] at sp-1
            // Stack after: [value, done] (sp increases by 1)
            .iterator_get_value_done => {
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const result_cv = stack[sp - 1];");
                try self.writeLine("const result = result_cv.toJSValue();  // convert CV to JSValue");
                try self.writeLine("const done_val = JSValue.getPropertyStr(ctx, result, \"done\");");
                try self.writeLine("const value_val = JSValue.getPropertyStr(ctx, result, \"value\");");
                try self.writeLine("JSValue.free(ctx, result);");
                try self.writeLine("stack[sp - 1] = CV.fromJSValue(value_val);");
                try self.writeLine("stack[sp] = CV.fromJSValue(done_val);");
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
                return false; // Control terminates
            },
        }
        return true; // Control continues for normal instructions
    }

    // ========================================================================
    // Opcode Analysis Helpers
    // ========================================================================

    /// Check if an opcode terminates control flow in generated code
    /// Returns true for:
    /// - Explicit return/throw opcodes
    /// - Opcodes not supported by our Zig codegen (which emit throwTypeError and return)
    fn isOpcodeTerminating(self: *Self, opcode: Opcode) bool {
        _ = self;
        // First check if it's a return/throw type opcode
        if (opcodes.isTerminator(opcode)) return true;

        // Check if opcode is NOT in our supported set (emitInstruction would emit throwTypeError)
        // IMPORTANT: This list MUST match exactly what emitInstruction handles
        // Any opcode NOT handled in emitInstruction should NOT be in this list
        return switch (opcode) {
            // Constants (from emitInstruction)
            .push_i32, .push_i8, .push_i16, .push_const8, .push_atom_value,
            .undefined, .null, .push_this, .push_false, .push_true,
            .push_minus1, .push_0, .push_1, .push_2, .push_3, .push_4, .push_5, .push_6, .push_7,
            .push_empty_string, .object, .special_object, .rest,
            // Stack manipulation
            .drop, .dup, .swap, .insert2, .insert3, .insert4,
            .perm3, .perm4, .perm5, .rot3l, .rot3r, .rot4l, .rot5l,
            // Locals
            .get_loc, .put_loc, .set_loc,
            .get_loc0, .get_loc1, .get_loc2, .get_loc3,
            .put_loc0, .put_loc1, .put_loc2, .put_loc3,
            .set_loc0, .set_loc1, .set_loc2, .set_loc3,
            .get_loc8, .put_loc8, .set_loc8,
            .get_loc0_loc1, .get_loc_check, .put_loc_check, .put_loc_check_init,
            .set_loc_uninitialized,
            // Arguments
            .get_arg, .put_arg, .set_arg, .get_arg0, .get_arg1, .get_arg2, .get_arg3,
            .put_arg0, .put_arg1, .put_arg2, .put_arg3,
            .set_arg0, .set_arg1, .set_arg2, .set_arg3,
            // Closure variables
            .get_var_ref, .put_var_ref, .set_var_ref, .get_var_ref_check,
            .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3,
            .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3,
            .set_var_ref0, .set_var_ref1, .set_var_ref2, .set_var_ref3,
            .put_var_ref_check, .put_var_ref_check_init,
            // Arithmetic
            .add, .sub, .mul, .div, .mod, .neg,
            .inc, .dec, .post_inc, .post_dec, .inc_loc, .dec_loc, .add_loc,
            // Comparisons
            .eq, .neq, .strict_eq, .strict_neq, .lt, .lte, .gt, .gte,
            .is_null, .is_undefined, .is_undefined_or_null,
            .typeof_is_undefined, .typeof_is_function, .instanceof, .typeof,
            // Bitwise
            .shl, .sar, .shr, .@"or", .@"and", .xor, .not,
            // Logical
            .lnot,
            // Control flow (non-terminating jumps)
            .if_true, .if_false, .if_true8, .if_false8, .goto, .goto8, .goto16,
            // Property access
            .get_field, .put_field, .get_field2, .define_field,
            .get_array_el, .put_array_el, .get_array_el2, .define_array_el, .append,
            .get_length,
            // Function calls
            .call, .call_method, .call0, .call1, .call2, .call3,
            .call_constructor, .tail_call, .tail_call_method,
            // Object/Array creation
            .array_from, .define_method, .set_name,
            .check_ctor, .check_ctor_return,
            // Variable operations
            .get_var_undef, .get_var,
            // Type conversions
            .to_object, .to_propkey, .to_propkey2,
            // Iterator protocol
            .for_of_start, .for_of_next, .iterator_close, .iterator_get_value_done,
            // Closures
            .fclosure8, .close_loc,
            // Misc handled
            .delete,
            => false, // Supported - does NOT terminate

            // Everything else is unsupported - terminates via throwTypeError
            else => true,
        };
    }

    // ========================================================================
    // Self-Call Detection Helpers
    // ========================================================================

    /// Check if the current get_var_ref0 instruction is followed by a self-call pattern.
    /// This looks ahead for call1/call2/call3 instructions and checks if the stack
    /// manipulation between get_var_ref0 and the call is consistent with a self-call.
    fn isFollowedBySelfCall(self: *Self, block_instrs: []const Instruction, start_idx: usize) bool {
        _ = self; // May need self later for more sophisticated analysis

        // Look at instructions after the current one
        var net_stack_push: i32 = 0; // Track net stack effect after get_var_ref0

        // Debug: Check if we have enough instructions
        if (start_idx + 1 >= block_instrs.len) {
            if (CODEGEN_DEBUG) std.debug.print("[isFollowedBySelfCall] start_idx={d} >= block_instrs.len={d}, returning false\n", .{ start_idx, block_instrs.len });
            return false;
        }

        for (block_instrs[start_idx + 1 ..], 0..) |future_instr, offset| {
            const op = future_instr.opcode;

            // Check for call instructions - these consume the function reference
            if (op == .call1 or op == .call2 or op == .call3 or op == .call) {
                // For call1: expects [func, arg] so net_push should be 1 (just the arg)
                // For call2: expects [func, arg0, arg1] so net_push should be 2
                // For call3: expects [func, arg0, arg1, arg2] so net_push should be 3
                const expected_argc: i32 = switch (op) {
                    .call1 => 1,
                    .call2 => 2,
                    .call3 => 3,
                    .call => @as(i32, @intCast(future_instr.operand.u16)),
                    else => unreachable,
                };
                // If net stack push matches expected arg count, it's a self-call pattern
                // (function was NOT pushed, only arguments were)
                return net_stack_push == expected_argc;
            }

            // Track stack effects of intermediate instructions
            // This is a simplified analysis - we track net push/pop
            switch (op) {
                // Push operations (net +1)
                .push_0, .push_1, .push_2, .push_3, .push_4, .push_5, .push_6, .push_7,
                .push_i32, .push_i8, .push_i16, .push_const, .push_const8,
                .push_true, .push_false, .null, .undefined, .push_this, .push_atom_value,
                .get_arg0, .get_arg1, .get_arg2, .get_arg3, .get_arg,
                .get_loc0, .get_loc1, .get_loc2, .get_loc3, .get_loc,
                .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3, .get_var_ref,
                .dup, .fclosure8,
                => net_stack_push += 1,

                // Pop operations (net -1)
                .drop,
                .put_arg0, .put_arg1, .put_arg2, .put_arg3, .put_arg,
                .put_loc0, .put_loc1, .put_loc2, .put_loc3, .put_loc,
                .set_arg0, .set_arg1, .set_arg2, .set_arg3, .set_arg,
                .set_loc0, .set_loc1, .set_loc2, .set_loc3, .set_loc,
                .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3, .put_var_ref,
                .set_var_ref0, .set_var_ref1, .set_var_ref2, .set_var_ref3, .set_var_ref,
                => net_stack_push -= 1,

                // Binary operations: pop 2, push 1 (net -1)
                .add, .sub, .mul, .div, .mod,
                .eq, .neq, .strict_eq, .strict_neq, .lt, .lte, .gt, .gte,
                .shl, .sar, .shr, .@"or", .@"and", .xor,
                => net_stack_push -= 1,

                // Unary operations: pop 1, push 1 (net 0)
                .neg, .not, .lnot, .typeof, .inc, .dec, .post_inc, .post_dec,
                => {},

                // Control flow terminators - stop looking
                .@"return", .return_undef, .if_true, .if_false, .goto, .goto8, .goto16,
                .if_true8, .if_false8,
                => return false,

                // Other instructions - assume it's not a simple self-call pattern
                else => {
                    if (CODEGEN_DEBUG) std.debug.print("[isFollowedBySelfCall] Unknown opcode at offset {d}: {s}, returning false\n", .{ offset, @tagName(op) });
                    return false;
                },
            }
        }

        // Reached end of block without finding a call
        return false;
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
    /// For self-calls: Stack: [arg0, arg1, ...argN-1] -> [result] (no func on stack)
    fn emitCall(self: *Self, argc: u16) !void {
        // Check for pending self-call (from get_var_ref0 with lookahead detection)
        if (self.pending_self_call) {
            self.pending_self_call = false;
            try self.writeLine("{");
            self.pushIndent();
            try self.writeLine("// Self-recursive call - direct function invocation");
            // Function names use __frozen_{name} format, names like "1369_fib" need @"..." escaping
            const needs_escape = self.func.name.len > 0 and std.ascii.isDigit(self.func.name[0]);
            if (argc == 0) {
                // No args - direct call
                if (needs_escape) {
                    try self.printLine("const result = @\"__frozen_{s}\"(ctx, JSValue.UNDEFINED, 0, undefined);", .{self.func.name});
                } else {
                    try self.printLine("const result = __frozen_{s}(ctx, JSValue.UNDEFINED, 0, undefined);", .{self.func.name});
                }
                try self.writeLine("stack[sp] = result; sp += 1;");
            } else {
                // With args - copy from stack (no func on stack for self-call)
                try self.printLine("var args: [{d}]JSValue = undefined;", .{argc});
                for (0..argc) |i| {
                    try self.printLine("args[{d}] = stack[sp - {d}].toJSValue();", .{ i, argc - i });
                }
                if (needs_escape) {
                    try self.printLine("const result = @\"__frozen_{s}\"(ctx, JSValue.UNDEFINED, {d}, &args);", .{ self.func.name, argc });
                } else {
                    try self.printLine("const result = __frozen_{s}(ctx, JSValue.UNDEFINED, {d}, &args);", .{ self.func.name, argc });
                }
                for (0..argc) |i| {
                    try self.printLine("JSValue.free(ctx, args[{d}]);", .{i});
                }
                try self.printLine("sp -= {d};", .{argc});
                try self.writeLine("stack[sp] = result; sp += 1;");
            }
            self.popIndent();
            try self.writeLine("}");
            return;
        }

        // Normal call - function is on the stack
        if (argc == 0) {
            // call0: func is at sp-1, no args
            try self.writeLine("{");
            self.pushIndent();
            try self.writeLine("const func = stack[sp - 1].toJSValue();");
            try self.writeLine("var no_args: [0]JSValue = .{};");
            try self.writeLine("const result = CV.fromJSValue(JSValue.call(ctx, func, JSValue.UNDEFINED, 0, @ptrCast(&no_args)));");
            try self.writeLine("JSValue.free(ctx, func);");
            try self.writeLine("stack[sp - 1] = result;");
            self.popIndent();
            try self.writeLine("}");
        } else {
            // callN: func at sp-1-argc, args at sp-argc..sp-1
            try self.writeLine("{");
            self.pushIndent();
            try self.printLine("const func = stack[sp - 1 - {d}].toJSValue();", .{argc});
            try self.printLine("var args: [{d}]JSValue = undefined;", .{argc});
            // Copy args from stack to args array
            for (0..argc) |i| {
                try self.printLine("args[{d}] = stack[sp - {d}].toJSValue();", .{ i, argc - i });
            }
            try self.printLine("const result = CV.fromJSValue(JSValue.call(ctx, func, JSValue.UNDEFINED, {d}, @ptrCast(&args)));", .{argc});
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

    /// Check if this instruction should be skipped because it's part of a native Math call pattern
    /// Returns the Math method name if this instruction should be skipped, null otherwise
    fn shouldSkipForNativeMath(self: *Self, instrs: []const Instruction, idx: usize) ?[]const u8 {
        if (idx >= instrs.len) return null;
        const instr = instrs[idx];

        // Check if this is get_var("Math") or get_var_undef("Math")
        if (instr.opcode == .get_var or instr.opcode == .get_var_undef) {
            const var_name = self.getAtomString(instr.operand.atom) orelse return null;
            if (!std.mem.eql(u8, var_name, "Math")) return null;

            // Look ahead for get_field2 with a known Math method
            if (idx + 1 >= instrs.len) return null;
            const next_instr = instrs[idx + 1];
            if (next_instr.opcode != .get_field2 and next_instr.opcode != .get_field) return null;

            const method_name = self.getAtomString(next_instr.operand.atom) orelse return null;
            if (!self.isKnownMathMethod(method_name)) return null;

            // Look ahead for call_method(1) - the actual call
            var i: usize = idx + 2;
            while (i < instrs.len) : (i += 1) {
                const future_instr = instrs[i];
                if (future_instr.opcode == .call_method) {
                    if (future_instr.operand.u16 == 1) {
                        return method_name; // Skip this get_var
                    }
                    break;
                }
                // If we hit another call or control flow, stop looking
                if (future_instr.opcode == .call or future_instr.opcode == .call0 or
                    future_instr.opcode == .if_true or future_instr.opcode == .if_false or
                    future_instr.opcode == .goto or future_instr.opcode == .@"return")
                {
                    break;
                }
            }
            return null;
        }

        // Check if this is get_field2("method") for a known Math method
        if (instr.opcode == .get_field2 or instr.opcode == .get_field) {
            // Check if previous instruction was get_var("Math")
            if (idx < 1) return null;
            const prev_instr = instrs[idx - 1];
            if (prev_instr.opcode != .get_var and prev_instr.opcode != .get_var_undef) return null;

            const var_name = self.getAtomString(prev_instr.operand.atom) orelse return null;
            if (!std.mem.eql(u8, var_name, "Math")) return null;

            const method_name = self.getAtomString(instr.operand.atom) orelse return null;
            if (!self.isKnownMathMethod(method_name)) return null;

            // Look ahead for call_method(1)
            var i: usize = idx + 1;
            while (i < instrs.len) : (i += 1) {
                const future_instr = instrs[i];
                if (future_instr.opcode == .call_method) {
                    if (future_instr.operand.u16 == 1) {
                        return method_name; // Skip this get_field2
                    }
                    break;
                }
                if (future_instr.opcode == .call or future_instr.opcode == .call0 or
                    future_instr.opcode == .if_true or future_instr.opcode == .if_false or
                    future_instr.opcode == .goto or future_instr.opcode == .@"return")
                {
                    break;
                }
            }
            return null;
        }

        return null;
    }

    /// Check if a method name is a known Math method we can optimize
    fn isKnownMathMethod(self: *Self, name: []const u8) bool {
        _ = self;
        return std.mem.eql(u8, name, "abs") or
            std.mem.eql(u8, name, "sqrt") or
            std.mem.eql(u8, name, "floor") or
            std.mem.eql(u8, name, "ceil") or
            std.mem.eql(u8, name, "round");
    }

    /// Try to emit native Math.method call instead of going through QuickJS
    /// Returns true if native code was emitted, false otherwise
    /// Pattern: get_var("Math") -> get_field2("abs"/"sqrt"/etc) -> [arg computation] -> call_method
    fn tryEmitNativeMathCall(self: *Self, argc: u16, instrs: []const Instruction, call_idx: usize) !bool {
        // Only optimize single-arg Math functions for now
        if (argc != 1) return false;

        // Search backwards for get_field2 with known Math method
        // The pattern is: get_var("Math"), get_field2("method"), ...arg computation..., call_method
        var get_field_idx: ?usize = null;
        var method_name: ?[]const u8 = null;

        // Search backwards from call_method to find get_field or get_field2
        var i: usize = call_idx;
        while (i > 0) : (i -= 1) {
            const instr = instrs[i - 1];
            if (instr.opcode == .get_field2 or instr.opcode == .get_field) {
                const atom = instr.operand.atom;
                if (self.getAtomString(atom)) |name| {
                    // Check if this is a known Math method
                    if (std.mem.eql(u8, name, "abs") or
                        std.mem.eql(u8, name, "sqrt") or
                        std.mem.eql(u8, name, "floor") or
                        std.mem.eql(u8, name, "ceil") or
                        std.mem.eql(u8, name, "round"))
                    {
                        get_field_idx = i - 1;
                        method_name = name;
                        break;
                    }
                }
            }
        }

        if (get_field_idx == null or method_name == null) return false;

        // Check for get_var/get_var_undef("Math") immediately before get_field
        if (get_field_idx.? < 1) return false;
        const get_var_instr = instrs[get_field_idx.? - 1];
        if (get_var_instr.opcode != .get_var and get_var_instr.opcode != .get_var_undef) return false;

        const var_atom = get_var_instr.operand.atom;
        const var_name = self.getAtomString(var_atom) orelse return false;
        if (!std.mem.eql(u8, var_name, "Math")) return false;

        // We have Math.method(arg) pattern - emit native code
        // Stack state: [arg] (get_var/get_field2 were skipped) -> need to produce [result]
        const name = method_name.?;

        if (std.mem.eql(u8, name, "abs")) {
            try self.writeLine("{ // Native Math.abs (pure Zig - no JS Math lookup)");
            self.pushIndent();
            try self.writeLine("const arg = stack[sp - 1];");
            try self.writeLine("const f = if (arg.isFloat()) arg.getFloat() else if (arg.isInt()) @as(f64, @floatFromInt(arg.getInt())) else 0.0;");
            try self.writeLine("stack[sp - 1] = CV.newFloat(@abs(f));");
            self.popIndent();
            try self.writeLine("}");
            return true;
        } else if (std.mem.eql(u8, name, "sqrt")) {
            try self.writeLine("{ // Native Math.sqrt (pure Zig - no JS Math lookup)");
            self.pushIndent();
            try self.writeLine("const arg = stack[sp - 1];");
            try self.writeLine("const f = if (arg.isFloat()) arg.getFloat() else if (arg.isInt()) @as(f64, @floatFromInt(arg.getInt())) else 0.0;");
            try self.writeLine("stack[sp - 1] = CV.newFloat(@sqrt(f));");
            self.popIndent();
            try self.writeLine("}");
            return true;
        } else if (std.mem.eql(u8, name, "floor")) {
            try self.writeLine("{ // Native Math.floor (pure Zig - no JS Math lookup)");
            self.pushIndent();
            try self.writeLine("const arg = stack[sp - 1];");
            try self.writeLine("const f = if (arg.isFloat()) arg.getFloat() else if (arg.isInt()) @as(f64, @floatFromInt(arg.getInt())) else 0.0;");
            try self.writeLine("stack[sp - 1] = CV.newFloat(@floor(f));");
            self.popIndent();
            try self.writeLine("}");
            return true;
        } else if (std.mem.eql(u8, name, "ceil")) {
            try self.writeLine("{ // Native Math.ceil (pure Zig - no JS Math lookup)");
            self.pushIndent();
            try self.writeLine("const arg = stack[sp - 1];");
            try self.writeLine("const f = if (arg.isFloat()) arg.getFloat() else if (arg.isInt()) @as(f64, @floatFromInt(arg.getInt())) else 0.0;");
            try self.writeLine("stack[sp - 1] = CV.newFloat(@ceil(f));");
            self.popIndent();
            try self.writeLine("}");
            return true;
        } else if (std.mem.eql(u8, name, "round")) {
            try self.writeLine("{ // Native Math.round (pure Zig - no JS Math lookup)");
            self.pushIndent();
            try self.writeLine("const arg = stack[sp - 1];");
            try self.writeLine("const f = if (arg.isFloat()) arg.getFloat() else if (arg.isInt()) @as(f64, @floatFromInt(arg.getInt())) else 0.0;");
            try self.writeLine("stack[sp - 1] = CV.newFloat(@round(f));");
            self.popIndent();
            try self.writeLine("}");
            return true;
        }

        return false;
    }

    /// Emit code for call_method opcode
    /// Stack: [this, method, arg0, arg1, ...argN-1] -> [result]
    /// Note: get_field2 pushes obj then obj.prop, so this comes before method
    fn emitCallMethod(self: *Self, argc: u16) !void {
        try self.writeLine("{");
        self.pushIndent();

        // this at sp-2-argc, method at sp-1-argc, args at sp-argc..sp-1
        try self.printLine("const method = stack[sp - 1 - {d}].toJSValue();", .{argc});
        try self.printLine("const call_this = stack[sp - 2 - {d}].toJSValue();", .{argc});

        if (argc == 0) {
            try self.writeLine("const result = JSValue.call(ctx, method, call_this, 0, @as([*]JSValue, undefined));");
        } else {
            try self.printLine("var args: [{d}]JSValue = undefined;", .{argc});
            for (0..argc) |i| {
                try self.printLine("args[{d}] = stack[sp - {d}].toJSValue();", .{ i, argc - i });
            }
            try self.printLine("const result = JSValue.call(ctx, method, call_this, {d}, &args);", .{argc});
            // Free args
            for (0..argc) |i| {
                try self.printLine("JSValue.free(ctx, args[{d}]);", .{i});
            }
        }

        // Free method and this
        try self.writeLine("JSValue.free(ctx, method);");
        try self.writeLine("JSValue.free(ctx, call_this);");
        try self.printLine("sp -= {d} + 2;", .{argc});
        try self.writeLine("stack[sp] = CV.fromJSValue(result);");
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
        try self.printLine("const ctor = stack[sp - 1 - {d}].toJSValue();", .{argc});

        if (argc == 0) {
            try self.writeLine("const result = JSValue.callConstructor(ctx, ctor, &.{});");
        } else {
            try self.printLine("var args: [{d}]JSValue = undefined;", .{argc});
            for (0..argc) |i| {
                try self.printLine("args[{d}] = stack[sp - {d}].toJSValue();", .{ i, argc - i });
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
        try self.writeLine("stack[sp - 1] = CV.fromJSValue(result);");

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
                try self.printLine("{{ const elem = stack[sp - {d}].toJSValue(); _ = JSValue.setPropertyUint32(ctx, arr, {d}, elem); }}", .{ stack_offset, i });
            }
        }

        try self.printLine("sp -= {d};", .{count});
        try self.writeLine("stack[sp] = CV.fromJSValue(arr);");
        try self.writeLine("sp += 1;");

        self.popIndent();
        try self.writeLine("}");
    }

    // ========================================================================
    // Int32 Specialized Code Generation (fib-like pure integer functions)
    // ========================================================================

    /// Generate optimized pure int32 Zig function for recursive int functions like fib
    /// This generates machine-code-speed execution with no JSValue overhead in the hot path
    fn emitInt32Specialized(self: *Self) !void {
        const func_name = self.func.name;
        const argc = self.func.arg_count;

        // Escape function name if it starts with digit
        const needs_escape = func_name.len > 0 and std.ascii.isDigit(func_name[0]);

        // Generate the pure int32 internal helper function
        try self.write("/// Pure int32 helper - no JSValue boxing in hot path (18x faster)\n");
        if (needs_escape) {
            try self.print("fn @\"__frozen_{s}_int32\"(", .{func_name});
        } else {
            try self.print("fn __frozen_{s}_int32(", .{func_name});
        }

        // Generate int32 parameters
        for (0..argc) |i| {
            if (i > 0) try self.write(", ");
            try self.print("n{d}: i32", .{i});
        }
        try self.write(") i32 {\n");
        self.pushIndent();

        // Emit int32 specialized code from CFG
        try self.emitInt32Body();

        self.popIndent();
        try self.write("}\n\n");

        // Generate the wrapper function that converts JSValue <-> i32
        if (needs_escape) {
            try self.print("pub fn @\"__frozen_{s}\"(ctx: *zig_runtime.JSContext, _: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {{\n", .{func_name});
        } else {
            try self.print("pub fn __frozen_{s}(ctx: *zig_runtime.JSContext, _: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {{\n", .{func_name});
        }
        self.pushIndent();

        // Unbox arguments - argc/argv are always used in the wrapper
        // Only suppress if no arguments
        if (argc == 0) {
            try self.writeLine("_ = argc; _ = argv;");
        }
        for (0..argc) |i| {
            try self.print("    var n{d}: i32 = 0;\n", .{i});
            try self.print("    _ = zig_runtime.JSValue.toInt32(ctx, &n{d}, argv[{d}]);\n", .{ i, i });
        }

        // Call pure int32 helper and box result
        if (needs_escape) {
            try self.print("    return zig_runtime.JSValue.newInt(@\"__frozen_{s}_int32\"(", .{func_name});
        } else {
            try self.print("    return zig_runtime.JSValue.newInt(__frozen_{s}_int32(", .{func_name});
        }
        for (0..argc) |i| {
            if (i > 0) try self.write(", ");
            try self.print("n{d}", .{i});
        }
        try self.write("));\n");

        self.popIndent();
        try self.write("}\n");
    }

    /// Emit pure int32 function body from CFG
    fn emitInt32Body(self: *Self) !void {
        const int32_handlers = @import("int32_handlers.zig");
        const blocks = self.func.cfg.blocks.items;

        // Simple case: emit blocks sequentially
        // For fib pattern: if (n < 2) return n; return fib(n-1) + fib(n-2);
        var stack: [16][]const u8 = undefined;
        var sp: usize = 0;
        var expr_buf: [256]u8 = undefined;

        for (blocks) |block| {
            for (block.instructions) |instr| {
                const handler = int32_handlers.getInt32Handler(instr.opcode);
                switch (handler.pattern) {
                    .push_const_i32 => {
                        const val = handler.value orelse @as(i32, @intCast(instr.operand.i32));
                        const s = std.fmt.bufPrint(&expr_buf, "{d}", .{val}) catch "0";
                        stack[sp] = self.allocator.dupe(u8, s) catch "0";
                        sp += 1;
                    },
                    .get_arg_i32 => {
                        const handler_idx = handler.index orelse 0;
                        const s = std.fmt.bufPrint(&expr_buf, "n{d}", .{handler_idx}) catch "n0";
                        stack[sp] = self.allocator.dupe(u8, s) catch "n0";
                        sp += 1;
                    },
                    .binary_arith_i32 => {
                        if (sp >= 2) {
                            const b = stack[sp - 1];
                            const a = stack[sp - 2];
                            const op = handler.op orelse "+";
                            const s = std.fmt.bufPrint(&expr_buf, "({s} {s} {s})", .{ a, op, b }) catch "(0)";
                            stack[sp - 2] = self.allocator.dupe(u8, s) catch "(0)";
                            sp -= 1;
                        }
                    },
                    .binary_cmp_i32 => {
                        if (sp >= 2) {
                            const b = stack[sp - 1];
                            const a = stack[sp - 2];
                            const op = handler.op orelse "<";
                            const s = std.fmt.bufPrint(&expr_buf, "({s} {s} {s})", .{ a, op, b }) catch "false";
                            stack[sp - 2] = self.allocator.dupe(u8, s) catch "false";
                            sp -= 1;
                        }
                    },
                    .call_self_i32 => {
                        // Self-recursive call - emit inline call
                        const argc = instr.operand.u16;
                        const func_name = self.func.name;
                        const needs_escape = func_name.len > 0 and std.ascii.isDigit(func_name[0]);
                        var call_expr: [128]u8 = undefined;
                        var call_len: usize = 0;
                        if (needs_escape) {
                            call_len = (std.fmt.bufPrint(&call_expr, "@\"__frozen_{s}_int32\"(", .{func_name}) catch "").len;
                        } else {
                            call_len = (std.fmt.bufPrint(&call_expr, "__frozen_{s}_int32(", .{func_name}) catch "").len;
                        }
                        // Add args from stack
                        for (0..argc) |i| {
                            if (i > 0) {
                                call_expr[call_len] = ',';
                                call_expr[call_len + 1] = ' ';
                                call_len += 2;
                            }
                            const arg = stack[sp - argc + i];
                            for (arg) |c| {
                                call_expr[call_len] = c;
                                call_len += 1;
                            }
                        }
                        call_expr[call_len] = ')';
                        call_len += 1;
                        sp -= argc;
                        stack[sp] = self.allocator.dupe(u8, call_expr[0..call_len]) catch "0";
                        sp += 1;
                    },
                    .if_false_i32 => {
                        if (sp >= 1) {
                            const cond = stack[sp - 1];
                            sp -= 1;
                            try self.print("if (!({s})) ", .{cond});
                        }
                    },
                    .return_i32 => {
                        if (sp >= 1) {
                            const val = stack[sp - 1];
                            sp -= 1;
                            try self.print("return {s};\n", .{val});
                        } else {
                            try self.writeLine("return 0;");
                        }
                    },
                    .goto_i32 => {
                        // Skip - will be handled by sequential block emission
                    },
                    else => {
                        // Unsupported in int32 mode - fall back to default
                        try self.print("// unsupported int32 op: {}\n", .{instr.opcode});
                    },
                }
            }
        }

        // If no return was emitted, return 0
        try self.writeLine("return 0;");
    }

    // ========================================================================
    // Native Specialization (Zero-FFI)
    // ========================================================================

    /// Check if function can use native specialization (zero FFI in hot path)
    /// Now uses RUNTIME type checking - generates dual-path code:
    ///   - If arg0 is TypedArray: use zero-FFI path with direct buffer access
    ///   - If arg0 is regular Array: fall back to regular FFI path
    fn canUseNativeSpecialization(self: *const Self) bool {
        // Criteria for native specialization:
        // 1. Has at least one argument (arg0 will be the array)
        // 2. Has array access opcodes (get_array_el, put_array_el)
        // 3. Has a counted loop pattern
        // 4. ALL opcodes must be supported by native codegen (no TODOs)
        // 5. Limited to 2 arguments (arr + len/scalar) - more args may be multi-array
        if (self.func.arg_count == 0) return false;
        if (self.func.arg_count > 3) {
            // Functions with >3 args often have complex interactions
            // Native specialization handles single-array (arg0) patterns and simple scalars
            if (CODEGEN_DEBUG) std.debug.print("[codegen] {s}: Native blocked - too many args ({d} > 3)\n", .{ self.func.name, self.func.arg_count });
            return false;
        }
        if (self.counted_loops.len == 0 and self.natural_loops.len == 0) return false;

        // Check if function uses array access AND all opcodes are supported
        var has_array_access = false;
        for (self.func.cfg.blocks.items) |block| {
            for (block.instructions) |instr| {
                if (instr.opcode == .get_array_el or instr.opcode == .put_array_el or
                    instr.opcode == .get_array_el2)
                {
                    has_array_access = true;
                }
                // Check if this opcode is supported by native codegen
                if (!isNativeSupportedOpcode(instr.opcode)) {
                    if (CODEGEN_DEBUG) std.debug.print("[codegen] {s}: Native blocked by unsupported opcode: {}\n", .{ self.func.name, instr.opcode });
                    return false;
                }
            }
        }

        return has_array_access;
    }

    /// Check if an opcode is supported by native (zero-FFI) codegen
    fn isNativeSupportedOpcode(opcode: Opcode) bool {
        return switch (opcode) {
            // Numeric constants
            .push_i32, .push_0, .push_1, .push_2, .push_3, .push_4, .push_5, .push_6, .push_7,
            .push_minus1, .push_i8, .push_i16, .push_const8, .null, .undefined,
            // Arguments
            .get_arg, .get_arg0, .get_arg1, .get_arg2, .get_arg3,
            // Locals (all variants including 8-bit encoded)
            .get_loc, .get_loc0, .get_loc1, .get_loc2, .get_loc3, .get_loc_check,
            .get_loc8, // 8-bit encoded local index
            .put_loc, .put_loc0, .put_loc1, .put_loc2, .put_loc3, .put_loc_check,
            .put_loc8, // 8-bit encoded local index
            .set_loc, .set_loc0, .set_loc1, .set_loc2, .set_loc3,
            .set_loc8, // 8-bit encoded local index
            .inc_loc, .dec_loc, .add_loc,
            // Array access
            .get_array_el, .get_array_el2, .put_array_el, .get_length,
            .to_propkey, .to_propkey2,
            // Property access
            .get_field, .get_field2,
            // Arithmetic
            .add, .sub, .mul, .div, .mod, .neg,
            // Bitwise
            .shl, .sar, .shr, .@"and", .@"or", .xor, .not,
            // Comparison
            .lt, .gt, .lte, .gte, .eq, .strict_eq, .neq, .strict_neq,
            // Control flow
            .if_false, .if_false8, .if_true, .if_true8,
            .goto, .goto8, .goto16, .return_undef, .@"return",
            // Stack operations
            .drop, .dup, .swap, .nip, .rot3l, .rot3r,
            .perm3, .perm4, .perm5,
            // Type checks
            .typeof, .is_null, .is_undefined, .is_undefined_or_null,
            // Inc/dec
            .inc, .dec, .post_inc, .post_dec,
            // Global access (needed for Math optimization)
            .get_var,
            // No-ops
            .nop, .fclosure, .push_atom_value, .push_const, .set_loc_uninitialized,
            => true,
            // All other opcodes are NOT supported
            else => false,
        };
    }

    /// Emit native specialized function with ZERO FFI in hot path
    /// Architecture: DUAL-PATH with runtime TypedArray detection
    ///   1. Try TypedArray fast path (zero FFI in hot loop)
    ///   2. If not TypedArray, fall back to regular FFI path
    fn emitNativeSpecialized(self: *Self) !void {
        const func_name = self.func.name;
        const argc = self.func.arg_count;

        // Escape function name if it starts with digit
        const needs_escape = func_name.len > 0 and std.ascii.isDigit(func_name[0]);

        // ================================================================
        // 1. Generate the pure native inner function (for TypedArray fast path)
        // ================================================================
        try self.write("/// Pure native function - ZERO FFI in hot path (TypedArray only)\n");
        if (needs_escape) {
            try self.print("fn @\"__frozen_{s}_native\"(", .{func_name});
        } else {
            try self.print("fn __frozen_{s}_native(", .{func_name});
        }

        // Generate native parameters: data buffer + length + numeric args
        if (argc > 0) {
            try self.write("data: [*]i32, data_len: usize");
        }
        for (1..argc) |i| {
            try self.print(", n{d}: i32", .{i});
        }
        try self.write(") f64 {\n");
        self.pushIndent();

        // Note: data_len is used for arr.length access in native loops
        // Don't discard it unconditionally - let the body use it

        // Emit pure native body using expression-based codegen
        try self.emitNativeBody();

        self.popIndent();
        try self.write("}\n\n");

        // ================================================================
        // 2. Generate the main wrapper function with DUAL-PATH
        // ================================================================
        if (needs_escape) {
            try self.print("pub fn @\"__frozen_{s}\"(ctx: *zig_runtime.JSContext, this_val: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {{\n", .{func_name});
        } else {
            try self.print("pub fn __frozen_{s}(ctx: *zig_runtime.JSContext, this_val: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {{\n", .{func_name});
        }
        self.pushIndent();

        try self.writeLine("_ = this_val;");
        try self.writeLine("");

        // ================================================================
        // PATH 1: Try TypedArray fast path
        // ================================================================
        if (argc > 0) {
            try self.writeLine("// PATH 1: Try TypedArray fast path (zero FFI in hot loop)");
            try self.writeLine("if (argc > 0) {");
            self.pushIndent();

            try self.writeLine("var byte_offset: usize = 0;");
            try self.writeLine("var byte_length: usize = 0;");
            try self.writeLine("var bytes_per_element: usize = 0;");
            try self.writeLine("const buffer = zig_runtime.quickjs.JS_GetTypedArrayBuffer(ctx, argv[0], &byte_offset, &byte_length, &bytes_per_element);");
            try self.writeLine("");
            try self.writeLine("// Check if it's a TypedArray (Int32Array expected)");
            try self.writeLine("if (!buffer.isException() and bytes_per_element == 4) {");
            self.pushIndent();

            try self.writeLine("// Clear any pending exception");
            try self.writeLine("_ = zig_runtime.quickjs.JS_GetException(ctx);");
            try self.writeLine("");
            try self.writeLine("var buf_size: usize = 0;");
            try self.writeLine("const buf_ptr = zig_runtime.quickjs.JS_GetArrayBuffer(ctx, &buf_size, buffer);");
            try self.writeLine("if (buf_ptr != null) {");
            self.pushIndent();

            try self.writeLine("const data_ptr: [*]i32 = @ptrCast(@alignCast(buf_ptr.? + byte_offset));");
            try self.writeLine("const data_len = byte_length / 4;");
            try self.writeLine("");

            // Extract numeric args for native call
            for (1..argc) |i| {
                try self.print("var n{d}: i32 = 0;\n", .{i});
                try self.print("_ = zig_runtime.JSValue.toInt32(ctx, &n{d}, argv[{d}]);\n", .{ i, i });
            }

            try self.writeLine("");
            try self.writeLine("// ZERO-FFI execution path");

            // Call pure native helper
            if (needs_escape) {
                try self.print("const result = @\"__frozen_{s}_native\"(data_ptr, data_len", .{func_name});
            } else {
                try self.print("const result = __frozen_{s}_native(data_ptr, data_len", .{func_name});
            }
            for (1..argc) |i| {
                try self.print(", n{d}", .{i});
            }
            try self.write(");\n");

            try self.writeLine("return zig_runtime.JSValue.newFloat64(result);");

            self.popIndent();
            try self.writeLine("}");

            self.popIndent();
            try self.writeLine("} else {");
            self.pushIndent();
            try self.writeLine("// Clear exception from failed TypedArray check");
            try self.writeLine("const exc = zig_runtime.quickjs.JS_GetException(ctx);");
            try self.writeLine("zig_runtime.JSValue.free(ctx, exc);");
            self.popIndent();
            try self.writeLine("}");

            self.popIndent();
            try self.writeLine("}");
            try self.writeLine("");
        }

        // ================================================================
        // PATH 2: Regular Array - ZERO-FFI via direct JSObject access
        // ================================================================
        if (argc > 0) {
            try self.writeLine("// PATH 2: Regular Array - direct internal access (zero FFI)");
            try self.writeLine("if (argc > 0) {");
            self.pushIndent();

            // Try to get fast array pointer directly (no FFI)
            try self.writeLine("const fast_arr = zig_runtime.getFastArrayDirect(argv[0]);");
            try self.writeLine("if (fast_arr.success) {");
            self.pushIndent();

            try self.writeLine("const js_values = fast_arr.values.?;");
            try self.writeLine("const data_len: usize = fast_arr.count;");
            try self.writeLine("");

            // Copy JSValue array to native int32 buffer inline (no FFI)
            try self.writeLine("// Copy to native buffer inline (zero FFI - direct memory access)");
            try self.writeLine("var stack_buf: [1024 * 1024]i32 = undefined;");
            try self.writeLine("const data_ptr: [*]i32 = &stack_buf;");
            try self.writeLine("for (0..data_len) |i| {");
            self.pushIndent();
            try self.writeLine("data_ptr[i] = zig_runtime.jsValueToInt32Inline(js_values[i]);");
            self.popIndent();
            try self.writeLine("}");
            try self.writeLine("");

            // Extract numeric args
            for (1..argc) |i| {
                try self.print("var n{d}: i32 = 0;\n", .{i});
                try self.print("_ = zig_runtime.JSValue.toInt32(ctx, &n{d}, argv[{d}]);\n", .{ i, i });
            }

            try self.writeLine("");
            try self.writeLine("// ZERO-FFI execution path");

            // Call the same pure native helper
            if (needs_escape) {
                try self.print("const result = @\"__frozen_{s}_native\"(data_ptr, data_len", .{func_name});
            } else {
                try self.print("const result = __frozen_{s}_native(data_ptr, data_len", .{func_name});
            }
            for (1..argc) |i| {
                try self.print(", n{d}", .{i});
            }
            try self.write(");\n");

            try self.writeLine("return zig_runtime.JSValue.newFloat64(result);");

            self.popIndent();
            try self.writeLine("}");

            self.popIndent();
            try self.writeLine("}");
        }

        try self.writeLine("");
        try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");

        self.popIndent();
        try self.write("}\n");
    }

    /// Emit the fallback body using regular FFI (for regular Arrays)
    fn emitFallbackBody(self: *Self) !void {
        // Clear vstack for fresh start
        self.vstack.clearRetainingCapacity();
        self.temp_counter = 100; // Start temps at 100 to avoid conflicts with native path

        try self.writeLine("const CV = zig_runtime.CompressedValue;");
        try self.writeLine("_ = &CV;");

        // Emit locals - always declare even if var_count=0 (dead code may reference it)
        if (self.func.var_count > 0) {
            try self.printLine("var locals: [{d}]CV = .{{CV.UNDEFINED}} ** {d};", .{ self.func.var_count, self.func.var_count });
            try self.writeLine("_ = &locals;");
        } else {
            try self.writeLine("var locals: [0]CV = .{};");
            try self.writeLine("_ = &locals;");
        }
        try self.writeLine("");

        // Emit stack (for complex operations)
        try self.writeLine("var stack: [256]CV = .{CV.UNDEFINED} ** 256;");
        try self.writeLine("var sp: usize = 0;");
        try self.writeLine("_ = &stack; _ = &sp;");
        try self.writeLine("");

        // Re-detect loops for this codegen path
        self.natural_loops = cfg_mod.detectNaturalLoops(self.func.cfg, self.allocator) catch &.{};

        // Clear skip_blocks for fresh emission
        self.skip_blocks.clearRetainingCapacity();
        for (self.natural_loops) |loop| {
            for (loop.body_blocks) |bid| {
                if (bid != loop.header_block) {
                    self.skip_blocks.put(self.allocator, bid, {}) catch {};
                }
            }
        }

        // Emit blocks using expression-based codegen
        const blocks = self.func.cfg.blocks.items;
        const all_in_loops = self.skip_blocks.count() == blocks.len;

        if (all_in_loops and self.natural_loops.len > 0) {
            try self.emitNativeLoops(blocks);
        } else {
            try self.writeLine("var block_id: u32 = 0;");
            try self.writeLine("_ = &block_id;");  // Silence "never mutated" warning
            try self.writeLine("dispatch: while (true) {");
            self.pushIndent();
            try self.writeLine("switch (block_id) {");
            self.pushIndent();

            for (blocks, 0..) |block, idx| {
                const block_idx: u32 = @intCast(idx);

                var is_loop_header = false;
                for (self.natural_loops) |loop| {
                    if (loop.header_block == block_idx and loop.depth == 0) {
                        try self.emitNativeLoopBlock(loop, blocks, block_idx);
                        is_loop_header = true;
                        break;
                    }
                }
                if (is_loop_header) continue;

                if (self.skip_blocks.contains(block_idx)) continue;

                try self.printLine("{d} => {{ // block_{d}", .{ block_idx, block_idx });
                self.pushIndent();
                self.current_block_idx = block_idx;

                const continues = try self.emitBlockExpressionBased(block);

                if (continues) {
                    if (block.successors.items.len > 0) {
                        try self.printLine("block_id = {d}; continue;", .{block.successors.items[0]});
                    } else {
                        try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
                    }
                }

                self.popIndent();
                try self.writeLine("},");
            }

            // Default case - use continue :dispatch to silence unused label warning
            try self.writeLine("else => continue :dispatch,");
            self.popIndent();
            try self.writeLine("}");
            self.popIndent();
            try self.writeLine("}");
        }
    }

    /// Emit block using expression-based codegen (for fallback path)
    fn emitBlockExpressionBased(self: *Self, block: BasicBlock) !bool {
        // Reset block_terminated for this block
        self.block_terminated = false;
        for (block.instructions, 0..) |instr, idx| {
            try self.emitInstructionExpr(instr, block, null, idx);
            // Check for terminal instructions (return/unsupported opcode)
            if (self.block_terminated or instr.opcode == .@"return" or instr.opcode == .return_undef) {
                return false; // Block ends with return/error, don't add continue
            }
        }
        return true;
    }

    /// Emit pure native function body with zero FFI using stack-based codegen
    fn emitNativeBody(self: *Self) !void {
        const blocks = self.func.cfg.blocks.items;

        // Detect natural loops for native while codegen
        self.natural_loops = cfg_mod.detectNaturalLoops(self.func.cfg, self.allocator) catch &.{};

        // Mark loop body blocks (excluding header) to skip from main emission
        for (self.natural_loops) |loop| {
            for (loop.body_blocks) |bid| {
                if (bid != loop.header_block) {
                    self.skip_blocks.put(self.allocator, bid, {}) catch {};
                }
            }
        }

        // Emit locals as native types (i32 for now)
        if (self.func.var_count > 0) {
            try self.printLine("var locals: [{d}]i64 = .{{0}} ** {d};", .{ self.func.var_count, self.func.var_count });
        }
        try self.writeLine("");

        // Stack-based codegen: track expressions on virtual stack
        var stack: [32][]const u8 = undefined;
        var sp: usize = 0;

        // Sequential block emission with stack tracking
        for (blocks, 0..) |block, block_idx| {
            // Skip blocks that are loop body (emitted inside while)
            if (self.skip_blocks.get(@intCast(block_idx)) != null) continue;

            try self.printLine("// block_{d}", .{block_idx});

            // Check if this block is a loop header - emit while loop
            var is_loop_header = false;
            for (self.natural_loops) |loop| {
                if (loop.header_block == block_idx) {
                    is_loop_header = true;
                    try self.emitNativeWhileLoop(loop, &stack, &sp);
                    break;
                }
            }

            if (!is_loop_header) {
                for (block.instructions) |instr| {
                    try self.emitZeroFFINativeOp(instr, &stack, &sp);
                }
            }
        }

        // NOTE: Don't emit final return - the blocks already contain return_undef
        // Adding a second return causes "unreachable code" error
    }

    /// Emit a while loop with zero-FFI native operations
    fn emitNativeWhileLoop(self: *Self, loop: cfg_mod.NaturalLoop, stack: *[32][]const u8, sp: *usize) !void {
        try self.writeLine("while (true) {");
        self.pushIndent();

        // Emit header block (contains condition check)
        const header = self.func.cfg.blocks.items[loop.header_block];
        for (header.instructions) |instr| {
            try self.emitZeroFFINativeOp(instr, stack, sp);
        }

        // Emit body blocks in order
        for (loop.body_blocks) |bid| {
            if (bid == loop.header_block) continue; // Already emitted

            // Check for nested loops
            var is_nested_header = false;
            for (self.natural_loops) |nested| {
                if (nested.header_block == bid and nested.depth > loop.depth) {
                    is_nested_header = true;
                    try self.emitNativeWhileLoop(nested, stack, sp);
                    break;
                }
            }

            if (is_nested_header) continue;

            // Check if this block is INSIDE a nested loop (but not the header)
            // If so, skip it - it was already emitted by the nested loop
            var in_nested_loop = false;
            for (self.natural_loops) |nested| {
                if (nested.header_block != loop.header_block and
                    nested.depth > loop.depth and
                    nested.containsBlock(bid))
                {
                    in_nested_loop = true;
                    break;
                }
            }

            if (!in_nested_loop) {
                const body_block = self.func.cfg.blocks.items[bid];
                for (body_block.instructions) |instr| {
                    try self.emitZeroFFINativeOp(instr, stack, sp);
                }
            }
        }

        self.popIndent();
        try self.writeLine("}");
    }

    /// Emit a single instruction as pure native Zig (zero FFI)
    /// Uses stack-based codegen to build expressions
    fn emitZeroFFINativeOp(self: *Self, instr: Instruction, stack: *[32][]const u8, sp: *usize) !void {
        var expr_buf: [256]u8 = undefined;

        switch (instr.opcode) {
            // ============ Numeric constants ============
            .push_i32 => {
                const s = std.fmt.bufPrint(&expr_buf, "{d}", .{instr.operand.i32}) catch "0";
                stack[sp.*] = self.allocator.dupe(u8, s) catch "0";
                sp.* += 1;
            },
            .push_0 => {
                stack[sp.*] = "0";
                sp.* += 1;
            },
            .push_1 => {
                stack[sp.*] = "1";
                sp.* += 1;
            },
            .push_2 => {
                stack[sp.*] = "2";
                sp.* += 1;
            },
            .push_3 => {
                stack[sp.*] = "3";
                sp.* += 1;
            },
            .push_4 => {
                stack[sp.*] = "4";
                sp.* += 1;
            },
            .push_5 => {
                stack[sp.*] = "5";
                sp.* += 1;
            },
            .push_6 => {
                stack[sp.*] = "6";
                sp.* += 1;
            },
            .push_7 => {
                stack[sp.*] = "7";
                sp.* += 1;
            },
            .push_minus1 => {
                stack[sp.*] = "-1";
                sp.* += 1;
            },
            .push_i8 => {
                const val: i8 = @bitCast(@as(u8, @truncate(instr.operand.u16)));
                const s = std.fmt.bufPrint(&expr_buf, "{d}", .{val}) catch "0";
                stack[sp.*] = self.allocator.dupe(u8, s) catch "0";
                sp.* += 1;
            },
            .push_i16 => {
                const val: i16 = @bitCast(instr.operand.u16);
                const s = std.fmt.bufPrint(&expr_buf, "{d}", .{val}) catch "0";
                stack[sp.*] = self.allocator.dupe(u8, s) catch "0";
                sp.* += 1;
            },
            .push_const8 => {
                const s = std.fmt.bufPrint(&expr_buf, "{d}", .{instr.operand.u16}) catch "0";
                stack[sp.*] = self.allocator.dupe(u8, s) catch "0";
                sp.* += 1;
            },
            .null, .undefined => {
                stack[sp.*] = "0"; // Null/undefined as 0 in native
                sp.* += 1;
            },

            // ============ Arguments (n1, n2, etc.) ============
            .get_arg => {
                const idx = instr.operand.loc;
                if (idx == 0) {
                    // arg0 is the array buffer - push a marker
                    stack[sp.*] = "__data__";
                } else {
                    const s = std.fmt.bufPrint(&expr_buf, "n{d}", .{idx}) catch "n0";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "n0";
                }
                sp.* += 1;
            },
            .get_arg0 => {
                stack[sp.*] = "__data__";
                sp.* += 1;
            },
            .get_arg1 => {
                stack[sp.*] = "n1";
                sp.* += 1;
            },
            .get_arg2 => {
                stack[sp.*] = "n2";
                sp.* += 1;
            },
            .get_arg3 => {
                stack[sp.*] = "n3";
                sp.* += 1;
            },

            // ============ Local variables ============
            .get_loc, .get_loc0, .get_loc1, .get_loc2, .get_loc3, .get_loc_check, .get_loc8 => {
                const loc = switch (instr.opcode) {
                    .get_loc0 => @as(u16, 0),
                    .get_loc1 => @as(u16, 1),
                    .get_loc2 => @as(u16, 2),
                    .get_loc3 => @as(u16, 3),
                    else => instr.operand.loc,
                };
                const s = std.fmt.bufPrint(&expr_buf, "locals[{d}]", .{loc}) catch "locals[0]";
                stack[sp.*] = self.allocator.dupe(u8, s) catch "locals[0]";
                sp.* += 1;
            },

            .put_loc, .put_loc0, .put_loc1, .put_loc2, .put_loc3, .put_loc_check, .put_loc8 => {
                const loc = switch (instr.opcode) {
                    .put_loc0 => @as(u16, 0),
                    .put_loc1 => @as(u16, 1),
                    .put_loc2 => @as(u16, 2),
                    .put_loc3 => @as(u16, 3),
                    else => instr.operand.loc,
                };
                if (sp.* >= 1) {
                    const val = stack[sp.* - 1];
                    sp.* -= 1;
                    try self.printLine("locals[{d}] = @intCast({s});", .{ loc, val });
                }
            },

            .set_loc, .set_loc0, .set_loc1, .set_loc2, .set_loc3, .set_loc8 => {
                const loc = switch (instr.opcode) {
                    .set_loc0 => @as(u16, 0),
                    .set_loc1 => @as(u16, 1),
                    .set_loc2 => @as(u16, 2),
                    .set_loc3 => @as(u16, 3),
                    else => instr.operand.loc,
                };
                if (sp.* >= 1) {
                    const val = stack[sp.* - 1];
                    // set_loc keeps value on stack
                    try self.printLine("locals[{d}] = @intCast({s});", .{ loc, val });
                }
            },

            // ============ Local increment/decrement ============
            .inc_loc => {
                try self.printLine("locals[{d}] += 1;", .{instr.operand.loc});
            },
            .dec_loc => {
                try self.printLine("locals[{d}] -= 1;", .{instr.operand.loc});
            },
            .add_loc => {
                // add_loc pops a value and adds it to a local
                if (sp.* >= 1) {
                    const val = stack[sp.* - 1];
                    sp.* -= 1;
                    try self.printLine("locals[{d}] += @as(i64, @intCast({s}));", .{ instr.operand.loc, val });
                }
            },

            // ============ Array access - DIRECT BUFFER ============
            .get_array_el => {
                // Stack: [array, index] -> [value]
                if (sp.* >= 2) {
                    const idx = stack[sp.* - 1];
                    const arr = stack[sp.* - 2];
                    sp.* -= 2;
                    if (std.mem.eql(u8, arr, "__data__")) {
                        const s = std.fmt.bufPrint(&expr_buf, "data[@as(usize, @intCast({s}))]", .{idx}) catch "data[0]";
                        stack[sp.*] = self.allocator.dupe(u8, s) catch "data[0]";
                    } else {
                        // Nested array access
                        const s = std.fmt.bufPrint(&expr_buf, "{s}[@as(usize, @intCast({s}))]", .{ arr, idx }) catch "0";
                        stack[sp.*] = self.allocator.dupe(u8, s) catch "0";
                    }
                    sp.* += 1;
                }
            },

            .put_array_el => {
                // Stack: [array, index, value] -> []
                if (sp.* >= 3) {
                    const val = stack[sp.* - 1];
                    const idx = stack[sp.* - 2];
                    const arr = stack[sp.* - 3];
                    sp.* -= 3;
                    if (std.mem.eql(u8, arr, "__data__")) {
                        try self.printLine("data[@as(usize, @intCast({s}))] = @intCast({s});", .{ idx, val });
                    } else {
                        try self.printLine("{s}[@as(usize, @intCast({s}))] = @intCast({s});", .{ arr, idx, val });
                    }
                }
            },

            .get_length => {
                // Stack: [array] -> [length]
                if (sp.* >= 1) {
                    const arr = stack[sp.* - 1];
                    sp.* -= 1;
                    if (std.mem.eql(u8, arr, "__data__")) {
                        // For data array, use data_len parameter
                        stack[sp.*] = "@as(i32, @intCast(data_len))";
                    } else {
                        // Shouldn't happen in native mode, but fallback
                        stack[sp.*] = "0";
                    }
                    sp.* += 1;
                }
            },

            .to_propkey, .to_propkey2 => {
                // No-op in native mode - index is already native
            },

            // ============ Arithmetic ============
            .add => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    const s = std.fmt.bufPrint(&expr_buf, "({s} + {s})", .{ a, b }) catch "(0)";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)";
                    sp.* += 1;
                }
            },
            .sub => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    const s = std.fmt.bufPrint(&expr_buf, "({s} - {s})", .{ a, b }) catch "(0)";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)";
                    sp.* += 1;
                }
            },
            .mul => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    const s = std.fmt.bufPrint(&expr_buf, "({s} * {s})", .{ a, b }) catch "(0)";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)";
                    sp.* += 1;
                }
            },
            .div => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    // Use float division for JS semantics
                    const s = std.fmt.bufPrint(&expr_buf, "(@as(f64, @floatFromInt({s})) / @as(f64, @floatFromInt({s})))", .{ a, b }) catch "(0.0)";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "(0.0)";
                    sp.* += 1;
                }
            },
            .mod => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    const s = std.fmt.bufPrint(&expr_buf, "@mod({s}, {s})", .{ a, b }) catch "(0)";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)";
                    sp.* += 1;
                }
            },
            .neg => {
                if (sp.* >= 1) {
                    const a = stack[sp.* - 1];
                    sp.* -= 1;
                    const s = std.fmt.bufPrint(&expr_buf, "-({s})", .{a}) catch "(0)";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)";
                    sp.* += 1;
                }
            },

            // ============ Bitwise ============
            .shl => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    const s = std.fmt.bufPrint(&expr_buf, "({s} << @intCast({s}))", .{ a, b }) catch "(0)";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)";
                    sp.* += 1;
                }
            },
            .sar => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    const s = std.fmt.bufPrint(&expr_buf, "({s} >> @intCast({s}))", .{ a, b }) catch "(0)";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)";
                    sp.* += 1;
                }
            },
            .shr => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    const s = std.fmt.bufPrint(&expr_buf, "@as(i32, @bitCast(@as(u32, @bitCast({s})) >> @intCast({s})))", .{ a, b }) catch "(0)";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)";
                    sp.* += 1;
                }
            },
            .@"and" => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    const s = std.fmt.bufPrint(&expr_buf, "({s} & {s})", .{ a, b }) catch "(0)";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)";
                    sp.* += 1;
                }
            },
            .@"or" => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    const s = std.fmt.bufPrint(&expr_buf, "({s} | {s})", .{ a, b }) catch "(0)";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)";
                    sp.* += 1;
                }
            },
            .xor => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    const s = std.fmt.bufPrint(&expr_buf, "({s} ^ {s})", .{ a, b }) catch "(0)";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)";
                    sp.* += 1;
                }
            },
            .not => {
                if (sp.* >= 1) {
                    const a = stack[sp.* - 1];
                    sp.* -= 1;
                    const s = std.fmt.bufPrint(&expr_buf, "~({s})", .{a}) catch "(0)";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)";
                    sp.* += 1;
                }
            },

            // ============ Comparisons ============
            .lt => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    const s = std.fmt.bufPrint(&expr_buf, "@intFromBool({s} < {s})", .{ a, b }) catch "0";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "0";
                    sp.* += 1;
                }
            },
            .lte => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    const s = std.fmt.bufPrint(&expr_buf, "@intFromBool({s} <= {s})", .{ a, b }) catch "0";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "0";
                    sp.* += 1;
                }
            },
            .gt => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    const s = std.fmt.bufPrint(&expr_buf, "@intFromBool({s} > {s})", .{ a, b }) catch "0";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "0";
                    sp.* += 1;
                }
            },
            .gte => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    const s = std.fmt.bufPrint(&expr_buf, "@intFromBool({s} >= {s})", .{ a, b }) catch "0";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "0";
                    sp.* += 1;
                }
            },
            .eq, .strict_eq => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    const s = std.fmt.bufPrint(&expr_buf, "@intFromBool({s} == {s})", .{ a, b }) catch "0";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "0";
                    sp.* += 1;
                }
            },
            .neq, .strict_neq => {
                if (sp.* >= 2) {
                    const b = stack[sp.* - 1];
                    const a = stack[sp.* - 2];
                    sp.* -= 2;
                    const s = std.fmt.bufPrint(&expr_buf, "@intFromBool({s} != {s})", .{ a, b }) catch "0";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "0";
                    sp.* += 1;
                }
            },

            // ============ Control flow ============
            .if_false, .if_false8 => {
                if (sp.* >= 1) {
                    const cond = stack[sp.* - 1];
                    sp.* -= 1;
                    try self.printLine("if ({s} == 0) break;", .{cond});
                }
            },
            .if_true, .if_true8 => {
                if (sp.* >= 1) {
                    const cond = stack[sp.* - 1];
                    sp.* -= 1;
                    try self.printLine("if ({s} != 0) break;", .{cond});
                }
            },
            .goto, .goto8, .goto16 => {
                // In native mode, goto to lower block = continue (loop back)
                // goto to higher block = break (exit loop)
                // For simplicity, emit continue - the block structure handles flow
                try self.writeLine("continue;");
            },
            .return_undef => {
                try self.writeLine("return 0.0;");
            },
            .@"return" => {
                if (sp.* >= 1) {
                    const val = stack[sp.* - 1];
                    sp.* -= 1;
                    // Return value - could be i64 or f64 expression
                    // If it's a division result (already f64), use directly
                    // Otherwise convert int to float
                    if (std.mem.indexOf(u8, val, "f64") != null) {
                        try self.printLine("return {s};", .{val});
                    } else {
                        try self.printLine("return @floatFromInt({s});", .{val});
                    }
                } else {
                    try self.writeLine("return 0.0;");
                }
            },

            // ============ Stack operations ============
            .drop => {
                if (sp.* >= 1) sp.* -= 1;
            },
            .dup => {
                if (sp.* >= 1) {
                    stack[sp.*] = stack[sp.* - 1];
                    sp.* += 1;
                }
            },
            .swap => {
                if (sp.* >= 2) {
                    const tmp = stack[sp.* - 1];
                    stack[sp.* - 1] = stack[sp.* - 2];
                    stack[sp.* - 2] = tmp;
                }
            },
            .nip => {
                // Remove second item: [a, b] -> [b]
                if (sp.* >= 2) {
                    stack[sp.* - 2] = stack[sp.* - 1];
                    sp.* -= 1;
                }
            },
            .rot3l => {
                // Rotate left: [a, b, c] -> [b, c, a]
                if (sp.* >= 3) {
                    const a = stack[sp.* - 3];
                    stack[sp.* - 3] = stack[sp.* - 2];
                    stack[sp.* - 2] = stack[sp.* - 1];
                    stack[sp.* - 1] = a;
                }
            },
            .rot3r => {
                // Rotate right: [a, b, c] -> [c, a, b]
                if (sp.* >= 3) {
                    const c = stack[sp.* - 1];
                    stack[sp.* - 1] = stack[sp.* - 2];
                    stack[sp.* - 2] = stack[sp.* - 3];
                    stack[sp.* - 3] = c;
                }
            },
            .perm3, .perm4, .perm5 => {
                // Complex permutations - skip in native mode
            },

            // ============ Type checks (return 0/1 in native) ============
            .typeof => {
                if (sp.* >= 1) {
                    sp.* -= 1;
                    stack[sp.*] = "0"; // Type number in native
                    sp.* += 1;
                }
            },
            .is_null, .is_undefined, .is_undefined_or_null => {
                if (sp.* >= 1) {
                    const a = stack[sp.* - 1];
                    sp.* -= 1;
                    // __data__ is already validated in wrapper, never null
                    if (std.mem.eql(u8, a, "__data__")) {
                        stack[sp.*] = "0"; // Always false - data is validated
                    } else {
                        const s = std.fmt.bufPrint(&expr_buf, "@intFromBool({s} == 0)", .{a}) catch "0";
                        stack[sp.*] = self.allocator.dupe(u8, s) catch "0";
                    }
                    sp.* += 1;
                }
            },

            // ============ Increment/decrement ============
            .inc => {
                if (sp.* >= 1) {
                    const a = stack[sp.* - 1];
                    sp.* -= 1;
                    const s = std.fmt.bufPrint(&expr_buf, "({s} + 1)", .{a}) catch "(1)";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "(1)";
                    sp.* += 1;
                }
            },
            .dec => {
                if (sp.* >= 1) {
                    const a = stack[sp.* - 1];
                    sp.* -= 1;
                    const s = std.fmt.bufPrint(&expr_buf, "({s} - 1)", .{a}) catch "(-1)";
                    stack[sp.*] = self.allocator.dupe(u8, s) catch "(-1)";
                    sp.* += 1;
                }
            },
            .post_inc => {
                // Push current value, then increment
                if (sp.* >= 1) {
                    // For post_inc on local, we'd need to know the target
                    // For now, just increment
                    const a = stack[sp.* - 1];
                    const s = std.fmt.bufPrint(&expr_buf, "({s} + 1)", .{a}) catch "(1)";
                    stack[sp.* - 1] = self.allocator.dupe(u8, s) catch "(1)";
                }
            },
            .post_dec => {
                if (sp.* >= 1) {
                    const a = stack[sp.* - 1];
                    const s = std.fmt.bufPrint(&expr_buf, "({s} - 1)", .{a}) catch "(-1)";
                    stack[sp.* - 1] = self.allocator.dupe(u8, s) catch "(-1)";
                }
            },

            // ============ No-ops in native mode ============
            .nop, .fclosure, .push_atom_value, .push_const,
            .set_loc_uninitialized, // TDZ marker - no-op in native (all locals init to 0)
            => {},

            else => {
                try self.printLine("// TODO native: {}", .{instr.opcode});
            },
        }
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

    // Verify structure - native shape access for "kind" property
    try std.testing.expect(std.mem.indexOf(u8, code, "__frozen_getKind") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "nativeGetKind") != null);

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
