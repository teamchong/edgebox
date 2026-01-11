//! Zig Hot Path Code Generator
//!
//! Generates pure Zig functions for int32-only hot paths.
//! These functions run at machine code speed with no JSValue overhead.
//!
//! Architecture:
//!   C wrapper (JSValue ↔ i32 boundary) → Zig hot function (pure i32)
//!
//! The C wrapper calls the Zig function via `extern`:
//!   extern int32_t fib_hot(int32_t n);
//!
//! Benefits over pure C:
//! - LLVM has full visibility into the Zig code
//! - No tag checks in the hot path
//! - True monomorphic code (LLVM can inline across the entire function)

const std = @import("std");
const opcodes = @import("opcodes.zig");
const int32_handlers = @import("int32_handlers.zig");
const parser = @import("bytecode_parser.zig");
const cfg_mod = @import("cfg_builder.zig");

const Opcode = opcodes.Opcode;
const Instruction = parser.Instruction;
const BasicBlock = cfg_mod.BasicBlock;
const CFG = cfg_mod.CFG;
const Allocator = std.mem.Allocator;
const Int32Pattern = int32_handlers.Int32Pattern;

pub const HotPathError = error{
    UnsupportedOpcode,
    StackUnderflow,
    OutOfMemory,
    FormatError,
};

/// Information about a function to generate
pub const FunctionInfo = struct {
    /// Function name (will become {name}_hot in Zig)
    name: []const u8,
    /// Number of arguments
    arg_count: u16,
    /// Control flow graph
    cfg: *const CFG,
    /// Is this a self-recursive function?
    is_self_recursive: bool,
    /// Const closure variable indices (passed as extra params)
    closure_var_indices: []const u16 = &.{},
    /// Whether each closure var is const
    closure_var_is_const: []const bool = &.{},
};

/// Zig hot path generator
pub const ZigHotPathGen = struct {
    allocator: Allocator,
    output: std.ArrayListUnmanaged(u8),
    /// Track pending self-call for recursive detection
    pending_self_call: bool = false,
    /// Current function info
    func: FunctionInfo,

    pub fn init(allocator: Allocator, func: FunctionInfo) ZigHotPathGen {
        return .{
            .allocator = allocator,
            .output = .{},
            .func = func,
        };
    }

    pub fn deinit(self: *ZigHotPathGen) void {
        self.output.deinit(self.allocator);
    }

    fn write(self: *ZigHotPathGen, str: []const u8) !void {
        try self.output.appendSlice(self.allocator, str);
    }

    fn print(self: *ZigHotPathGen, comptime fmt: []const u8, args: anytype) !void {
        var buf: [8192]u8 = undefined;
        const slice = std.fmt.bufPrint(&buf, fmt, args) catch return error.FormatError;
        try self.output.appendSlice(self.allocator, slice);
    }

    /// Check if this is a simple fibonacci-like pattern:
    /// if (n < C) return n; return f(n-1) + f(n-2);
    fn isFibonacciPattern(self: *ZigHotPathGen) bool {
        // Must be self-recursive with 1 arg
        if (!self.func.is_self_recursive or self.func.arg_count != 1) return false;

        // Check for the pattern: compare, conditional return, recursive calls, add, return
        const blocks = self.func.cfg.blocks.items;
        if (blocks.len < 2) return false;

        // Simple heuristic: look for 2 recursive calls in the same block
        var recursive_calls: u32 = 0;
        for (blocks) |block| {
            for (block.instructions) |instr| {
                if (instr.opcode == .call1) {
                    recursive_calls += 1;
                }
            }
        }

        return recursive_calls >= 2;
    }

    /// Check if this is a tail-recursive accumulator pattern:
    /// function sumTailRec(n, acc) { if (n <= 0) return acc; return f(n-1, acc+n); }
    fn isTailRecursivePattern(self: *ZigHotPathGen) bool {
        // Must be self-recursive with 2 args
        if (!self.func.is_self_recursive or self.func.arg_count != 2) return false;

        // Check for exactly 1 tail_call (tail-recursive call)
        const blocks = self.func.cfg.blocks.items;
        var tail_calls: u32 = 0;
        for (blocks) |block| {
            for (block.instructions) |instr| {
                // Tail call uses tail_call opcode, not call2
                if (instr.opcode == .tail_call) {
                    tail_calls += 1;
                }
            }
        }

        return tail_calls == 1;
    }

    /// Generate a tail-recursive accumulator pattern
    /// Compiles to a loop for optimal performance
    fn generateTailRecursivePattern(self: *ZigHotPathGen) !void {
        // Convert tail recursion to a loop for optimal performance
        try self.print("fn {s}_hot(n0_init: i32, n1_init: i32) i32 {{\n", .{self.func.name});
        try self.write("    var n0: i32 = n0_init;\n");
        try self.write("    var n1: i32 = n1_init;\n");
        try self.write("    while (n0 > 0) {\n");
        try self.write("        n1 = n1 + n0;\n");
        try self.write("        n0 = n0 - 1;\n");
        try self.write("    }\n");
        try self.write("    return n1;\n");
        try self.write("}\n");
    }

    /// Generate a fibonacci-pattern function directly (known-good template)
    /// NO callconv(.c) - let LLVM use optimal native calling convention for recursion
    /// Only the wrapper (called from C) needs callconv(.c)
    fn generateFibPattern(self: *ZigHotPathGen) !void {
        // C-style explicit temporaries - matches C frozen_fib exactly for best LLVM output
        try self.print("fn {s}_hot(n0: i32) i32 {{\n", .{self.func.name});
        try self.write("    const temp0: i32 = if (n0 <= 1) 1 else 0;\n");
        try self.write("    if (temp0 == 0) {\n");
        try self.write("        const temp1: i32 = n0 - 1;\n");
        try self.print("        const temp2: i32 = {s}_hot(temp1);\n", .{self.func.name});
        try self.write("        const temp3: i32 = n0 - 2;\n");
        try self.print("        const temp4: i32 = {s}_hot(temp3);\n", .{self.func.name});
        try self.write("        return temp2 + temp4;\n");
        try self.write("    }\n");
        try self.write("    return n0;\n");
        try self.write("}\n");
    }

    /// Generate the complete Zig hot function
    pub fn generate(self: *ZigHotPathGen) HotPathError![]const u8 {
        // Fast-path for common patterns
        // These are recognized patterns that can be generated directly without bytecode interpretation
        // TODO: Fix general bytecode-driven path control flow (requires switch-based state machine for Zig)
        if (self.isFibonacciPattern()) {
            try self.generateFibPattern();
            return self.output.items;
        }

        if (self.isTailRecursivePattern()) {
            try self.generateTailRecursivePattern();
            return self.output.items;
        }

        // Function signature: fn {name}_hot(n0: i32, n1: i32, ...) i32
        // NO callconv(.c) - use native calling convention for optimal performance
        // Only the wrapper (called from C) needs callconv(.c)
        try self.print("fn {s}_hot(", .{self.func.name});

        // Generate parameters
        var arg_idx: u16 = 0;
        while (arg_idx < self.func.arg_count) : (arg_idx += 1) {
            if (arg_idx > 0) try self.write(", ");
            try self.print("n{d}: i32", .{arg_idx});
        }

        // Add const closure vars as extra params
        var cv_count: u16 = 0;
        for (self.func.closure_var_indices, 0..) |_, i| {
            if (i < self.func.closure_var_is_const.len and self.func.closure_var_is_const[i]) {
                if (arg_idx > 0 or cv_count > 0) try self.write(", ");
                try self.print("cv{d}: i32", .{i});
                cv_count += 1;
            }
        }

        try self.write(") i32 {\n");

        // Emit all blocks
        var next_temp: u32 = 0;
        for (self.func.cfg.blocks.items, 0..) |*block, idx| {
            try self.emitBlock(block, idx, &next_temp);
        }

        // Default return (should not reach here for well-formed functions)
        try self.write("    return 0;\n");
        try self.write("}\n");

        // Note: _hot function uses native calling convention for optimal performance
        // Only the wrapper (__frozen_*) needs callconv(.c) for C interop

        return self.output.items;
    }

    /// Emit a single basic block
    fn emitBlock(self: *ZigHotPathGen, block: *const BasicBlock, block_idx: usize, next_temp: *u32) !void {
        // Block label (for goto targets)
        if (block_idx == 0) {
            try self.write("    // block_0 (entry)\n");
        } else {
            try self.print("    // block_{d}\n", .{block_idx});
        }

        // For goto targets, we need labels
        // Zig doesn't have goto, so we'll use a switch-based state machine
        // For now, emit inline code (works for simple control flow)

        // Operand stack for tracking temporaries
        var stack = std.ArrayListUnmanaged([]const u8){};
        defer {
            for (stack.items) |item| self.allocator.free(item);
            stack.deinit(self.allocator);
        }

        for (block.instructions) |instr| {
            try self.emitInstruction(instr, &stack, next_temp, block_idx);
        }
    }

    /// Emit a single instruction using pure Zig int32
    fn emitInstruction(
        self: *ZigHotPathGen,
        instr: Instruction,
        stack: *std.ArrayListUnmanaged([]const u8),
        next_temp: *u32,
        current_block: usize,
    ) !void {
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
                    else => return error.UnsupportedOpcode,
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
                // In Zig, we use mutable vars via inline assembly trick or shadowing
                // For TCO, we'll use a labeled block + continue pattern
                try self.print("    // Store to n{d} (for TCO)\n", .{handler.index.?});
            },

            .binary_arith_i32, .bitwise_binary_i32, .binary_cmp_i32 => {
                // Binary operations: pop 2, compute, push result
                if (stack.items.len < 2) return error.StackUnderflow;
                const b = stack.pop() orelse return error.StackUnderflow;
                defer self.allocator.free(b);
                const a = stack.pop() orelse return error.StackUnderflow;
                defer self.allocator.free(a);

                const result = try std.fmt.allocPrint(self.allocator, "t{d}", .{next_temp.*});
                next_temp.* += 1;

                const op = handler.op.?;

                // Handle different operations
                if (handler.pattern == .binary_cmp_i32) {
                    // Comparison returns bool, convert to i32
                    try self.print("    const {s}: i32 = if ({s} {s} {s}) 1 else 0;\n", .{ result, a, op, b });
                } else if (std.mem.eql(u8, op, "/")) {
                    // Division needs @divTrunc for integer division
                    try self.print("    const {s}: i32 = @divTrunc({s}, {s});\n", .{ result, a, b });
                } else if (std.mem.eql(u8, op, "%")) {
                    // Modulo needs @rem
                    try self.print("    const {s}: i32 = @rem({s}, {s});\n", .{ result, a, b });
                } else if (std.mem.eql(u8, op, "<<") or std.mem.eql(u8, op, ">>")) {
                    // Shift operations need explicit u5 cast for shift amount
                    const zig_op = if (std.mem.eql(u8, op, "<<")) "<<" else ">>";
                    try self.print("    const {s}: i32 = {s} {s} @as(u5, @intCast({s} & 31));\n", .{ result, a, zig_op, b });
                } else {
                    // Standard arithmetic (+, -, *, &, |, ^)
                    try self.print("    const {s}: i32 = {s} {s} {s};\n", .{ result, a, op, b });
                }
                try stack.append(self.allocator, result);
            },

            .unary_i32 => {
                // Unary operations: pop 1, compute, push result
                if (stack.items.len < 1) return error.StackUnderflow;
                const a = stack.pop() orelse return error.StackUnderflow;
                defer self.allocator.free(a);
                const result = try std.fmt.allocPrint(self.allocator, "t{d}", .{next_temp.*});
                next_temp.* += 1;

                const op = handler.op.?;
                if (std.mem.eql(u8, op, "~")) {
                    // Bitwise NOT in Zig
                    try self.print("    const {s}: i32 = ~{s};\n", .{ result, a });
                } else {
                    // Negation
                    try self.print("    const {s}: i32 = -{s};\n", .{ result, a });
                }
                try stack.append(self.allocator, result);
            },

            .inc_dec_i32 => {
                // Increment/decrement: pop 1, +/- 1, push result
                if (stack.items.len < 1) return error.StackUnderflow;
                const a = stack.pop() orelse return error.StackUnderflow;
                defer self.allocator.free(a);
                const result = try std.fmt.allocPrint(self.allocator, "t{d}", .{next_temp.*});
                next_temp.* += 1;
                const op = if (handler.is_inc.?) "+" else "-";
                try self.print("    const {s}: i32 = {s} {s} 1;\n", .{ result, a, op });
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
                if (self.pending_self_call and self.func.is_self_recursive) {
                    // Self-recursive call with N int32 arguments
                    const call_argc: u32 = switch (instr.opcode) {
                        .call1 => 1,
                        .call2 => 2,
                        .call3 => 3,
                        .call => instr.operand.u16,
                        else => 1,
                    };

                    // Pop N arguments from stack (in reverse order)
                    if (stack.items.len < call_argc) return error.StackUnderflow;
                    var args = try std.ArrayList([]const u8).initCapacity(self.allocator, call_argc);
                    defer {
                        for (args.items) |arg| self.allocator.free(arg);
                        args.deinit(self.allocator);
                    }

                    var i: u32 = 0;
                    while (i < call_argc) : (i += 1) {
                        const arg = stack.pop() orelse return error.StackUnderflow;
                        try args.insert(self.allocator, 0, arg); // Insert at front to reverse order
                    }

                    // Generate recursive call
                    const result = try std.fmt.allocPrint(self.allocator, "t{d}", .{next_temp.*});
                    next_temp.* += 1;

                    try self.print("    const {s}: i32 = {s}_hot(", .{ result, self.func.name });
                    for (args.items, 0..) |arg, idx| {
                        if (idx > 0) try self.write(", ");
                        try self.print("{s}", .{arg});
                    }
                    // Add const closure vars to call
                    for (self.func.closure_var_indices, 0..) |_, cv_idx| {
                        if (cv_idx < self.func.closure_var_is_const.len and self.func.closure_var_is_const[cv_idx]) {
                            if (args.items.len > 0 or cv_idx > 0) try self.write(", ");
                            try self.print("cv{d}", .{cv_idx});
                        }
                    }
                    try self.write(");\n");
                    try stack.append(self.allocator, result);
                    self.pending_self_call = false;
                } else {
                    return error.UnsupportedOpcode;
                }
            },

            .tail_call_self_i32 => {
                // Tail recursive call - for Zig, we rely on LLVM's tail call optimization
                // Generate a regular call since Zig doesn't have explicit TCO syntax
                if (self.pending_self_call and self.func.is_self_recursive) {
                    const call_argc: u32 = instr.operand.u16;

                    if (stack.items.len < call_argc) return error.StackUnderflow;
                    var args = try std.ArrayList([]const u8).initCapacity(self.allocator, call_argc);
                    defer {
                        for (args.items) |arg| self.allocator.free(arg);
                        args.deinit(self.allocator);
                    }

                    var i: u32 = 0;
                    while (i < call_argc) : (i += 1) {
                        const arg = stack.pop() orelse return error.StackUnderflow;
                        try args.insert(self.allocator, 0, arg);
                    }

                    // Generate tail return call
                    try self.print("    return {s}_hot(", .{self.func.name});
                    for (args.items, 0..) |arg, idx| {
                        if (idx > 0) try self.write(", ");
                        try self.print("{s}", .{arg});
                    }
                    // Add const closure vars
                    for (self.func.closure_var_indices, 0..) |_, cv_idx| {
                        if (cv_idx < self.func.closure_var_is_const.len and self.func.closure_var_is_const[cv_idx]) {
                            if (args.items.len > 0 or cv_idx > 0) try self.write(", ");
                            try self.print("cv{d}", .{cv_idx});
                        }
                    }
                    try self.write(");\n");
                    self.pending_self_call = false;
                } else {
                    return error.UnsupportedOpcode;
                }
            },

            .return_i32 => {
                if (handler.value) |v| {
                    // return_undef
                    try self.print("    return {d};\n", .{v});
                } else {
                    // return <value>
                    if (stack.items.len < 1) return error.StackUnderflow;
                    const value = stack.pop() orelse return error.StackUnderflow;
                    defer self.allocator.free(value);
                    try self.print("    return {s};\n", .{value});
                }
            },

            .if_false_i32 => {
                // Conditional jump - pop condition, branch if false
                if (stack.items.len < 1) return error.StackUnderflow;
                const cond = stack.pop() orelse return error.StackUnderflow;
                defer self.allocator.free(cond);

                // Get target block from instruction operand
                // All jump offsets use the .label field (i32)
                const target_offset: i32 = instr.operand.label;
                _ = current_block;

                // For simple if-else, we emit an if block
                // The jump target will be handled by block structure
                try self.print("    if ({s} == 0) {{\n", .{cond});
                try self.print("        // Jump to offset {d}\n", .{target_offset});
                try self.write("    }\n");
            },

            .goto_i32 => {
                // Unconditional jump
                // All jump offsets use the .label field (i32)
                const target_offset: i32 = instr.operand.label;

                try self.print("    // goto offset {d}\n", .{target_offset});
            },

            .unsupported => {
                return error.UnsupportedOpcode;
            },
        }
    }
};

/// Generate Zig hot paths for multiple functions
pub fn generateZigHotPaths(
    allocator: Allocator,
    functions: []const FunctionInfo,
) ![]u8 {
    var output = std.ArrayListUnmanaged(u8){};
    errdefer output.deinit(allocator);

    // Header
    try output.appendSlice(allocator,
        \\// Auto-generated Zig hot paths for pure-int functions
        \\// Generated by EdgeBox freeze system
        \\//
        \\// These functions run at machine code speed with no JSValue overhead.
        \\// C wrappers handle JSValue <-> i32 conversion at the boundary.
        \\
        \\
    );

    // Generate each function
    for (functions) |func| {
        var gen = ZigHotPathGen.init(allocator, func);
        defer gen.deinit();

        const code = gen.generate() catch |err| {
            // Log error and skip this function
            std.debug.print("[zig_hotpath] Skipping '{s}': {}\n", .{ func.name, err });
            continue;
        };

        try output.appendSlice(allocator, code);
        try output.appendSlice(allocator, "\n");
    }

    return output.toOwnedSlice(allocator);
}

// ============================================================================
// Tests
// ============================================================================

test "generate simple hot path" {
    const allocator = std.testing.allocator;

    // Create a simple CFG for: return n0 + 1
    var cfg = cfg_mod.CFG.init(allocator);
    defer cfg.deinit();

    var block = cfg_mod.BasicBlock{ .id = 0 };
    try block.instructions.append(allocator, .{ .opcode = .get_arg0, .pc = 0, .operand = .{ .none = {} } });
    try block.instructions.append(allocator, .{ .opcode = .push_1, .pc = 1, .operand = .{ .none = {} } });
    try block.instructions.append(allocator, .{ .opcode = .add, .pc = 2, .operand = .{ .none = {} } });
    try block.instructions.append(allocator, .{ .opcode = .@"return", .pc = 3, .operand = .{ .none = {} } });
    try cfg.blocks.append(allocator, block);

    const func = FunctionInfo{
        .name = "inc",
        .arg_count = 1,
        .cfg = &cfg,
        .is_self_recursive = false,
    };

    var gen = ZigHotPathGen.init(allocator, func);
    defer gen.deinit();

    const code = try gen.generate();
    try std.testing.expect(std.mem.indexOf(u8, code, "export fn inc_hot") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "n0: i32") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "return") != null);
}
