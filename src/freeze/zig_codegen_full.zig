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
const zig_runtime = @import("zig_runtime.zig");
const opcode_emitter = @import("opcode_emitter.zig");

const Opcode = opcodes.Opcode;
const Instruction = parser.Instruction;
const BasicBlock = cfg_mod.BasicBlock;
const JS_ATOM_END = module_parser.JS_ATOM_END;
const CFG = cfg_mod.CFG;
const CountedLoop = cfg_mod.CountedLoop;
const Allocator = std.mem.Allocator;

// Debug flag - set to true for verbose codegen logging
const CODEGEN_DEBUG = false;

// Runtime tracing - generates std.debug.print at entry of each frozen function
// Enable this to trace which frozen functions are being called (for debugging infinite loops)
const TRACE_FROZEN_CALLS = false;

// Native loop optimization converts certain CFG patterns to direct while(true) loops.
// Disabled because the CFG analysis has edge cases that generate infinite loops
// in complex code (e.g., tRPC procedure builders). Block dispatch handles all patterns correctly.
const ENABLE_NATIVE_LOOPS = false;

// Dispatch table threshold: functions with more than this many blocks use
// runtime dispatch tables instead of compile-time switch statements.
// This avoids hitting Zig's comptime eval branch quota for large functions.
// Higher values reduce generated code size but may hit comptime eval limits.
// NOTE: 10000 effectively disables dispatch tables (uses inline comptime switch)
const DISPATCH_TABLE_THRESHOLD: usize = 10000;

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
    ComplexControlFlow,
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
    /// Actual closure variable count (from bytecode metadata, for bounds checking)
    closure_var_count: u32 = 0,
    /// Atom strings for property names
    atom_strings: []const []const u8 = &.{},
    /// Enable partial freeze (emit fallback for contaminated blocks)
    partial_freeze: bool = false,
    /// Original JS function name (for fallback registration)
    js_name: []const u8 = &.{},
    /// Is this a pure int32 function (can use optimized Zig int32 path)
    is_pure_int32: bool = false,
    /// Explicit "use strict" directive (for proper 'this' handling)
    has_use_strict: bool = false,
    /// Is this an async function (func_kind >= 2)
    is_async: bool = false,
    /// Constant pool values - used for fclosure bytecode registration
    constants: []const module_parser.ConstValue = &.{},
    /// Stack size (from bytecode, for sizing the operand stack array)
    stack_size: u32 = 256,
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
    /// Track that shouldSkipForNativeMath skipped get_var/get_field2 instructions,
    /// so call_method must still use tryEmitNativeMathCall even in force_stack_mode
    native_math_skipped: bool = false,
    /// Block terminated by return/throw - skip remaining instructions
    block_terminated: bool = false,
    /// Function returned (not just break/continue) - don't clear block_terminated
    function_returned: bool = false,
    /// Temp variable counter for expression codegen
    temp_counter: u32 = 0,
    /// Use expression-based codegen (no stack machine)
    use_expr_codegen: bool = false,
    /// Track if-statement depth for proper closing braces
    if_body_depth: u32 = 0,
    /// Stack of target blocks for nested if-statements (to close braces when reached)
    if_target_blocks: std.ArrayListUnmanaged(u32) = .{},
    /// Parallel stack of fall-through blocks that should be inside each if
    /// Don't close if until this block has been processed
    if_fall_through_blocks: std.ArrayListUnmanaged(u32) = .{},
    /// Track which blocks have been emitted (for if-closing logic)
    emitted_blocks: std.AutoHashMapUnmanaged(u32, void) = .{},
    /// Blocks to skip during normal iteration (for inverted if targets)
    /// When target < fall_through, we need to defer the target block
    deferred_blocks: std.AutoHashMapUnmanaged(u32, void) = .{},
    /// Debug mode for tracing codegen issues
    debug_mode: bool = false,
    /// Track if argc/argv parameters are used
    uses_argc_argv: bool = false,
    /// Track if this_val parameter is used
    uses_this_val: bool = false,
    /// Use argument caching to prevent repeated dups in loops
    /// IMPORTANT: Disabled when function has put_arg operations, since cache would become stale
    uses_arg_cache: bool = false,
    /// Maximum argument index used in loops (for arg_cache size)
    max_loop_arg_idx: u32 = 0,
    /// Track if function has any put_arg operations (disables arg_cache)
    has_put_arg: bool = false,
    /// Track max arg index used (for arg_shadow sizing when bytecode arg_count is wrong)
    max_arg_idx_used: u32 = 0,
    /// Track if function creates closures (fclosure/fclosure8) - needs shared var_ref tracking
    has_fclosure: bool = false,
    /// Track if a closure has been created yet in the current block
    /// Only emit closure var sync after this is true
    closure_created_in_block: bool = false,
    /// Track depth of body: blocks (for nested loops)
    /// Only emit break :body when depth > 0
    body_block_depth: u32 = 0,
    /// Track whether break :body was emitted in current body: block
    /// Used to determine if trivial use is needed
    body_label_used: bool = false,
    /// Base stack depth at block entry - existing values from previous blocks
    /// Used to generate stack refs when vstack underflows
    base_stack_depth: u32 = 0,
    /// Number of base stack items already popped (for correct offset calculation)
    base_popped_count: u32 = 0,
    /// Dispatch table mode - block functions return BlockResult instead of JSValue
    /// Used for large functions (>50 blocks) to avoid comptime switch explosion
    dispatch_mode: bool = false,
    /// Module-level block functions (for dispatch table mode)
    /// These must be emitted before the main function since Zig doesn't allow nested fns
    block_functions: std.ArrayListUnmanaged(u8) = .{},
    /// Indent level for block functions output
    block_fn_indent: usize = 0,
    /// Unique function index for block function naming
    func_idx: u32 = 0,
    /// Track if any unsupported opcodes were encountered - if true, skip function entirely
    has_unsupported_opcodes: bool = false,

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
            .block_functions = .{},
            .block_fn_indent = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.output.deinit(self.allocator);
        self.block_functions.deinit(self.allocator);
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
        self.if_fall_through_blocks.deinit(self.allocator);
        self.emitted_blocks.deinit(self.allocator);
        self.deferred_blocks.deinit(self.allocator);
        // Free virtual stack expressions
        for (self.vstack.items) |expr| {
            if (self.isAllocated(expr)) self.allocator.free(expr);
        }
        self.vstack.deinit(self.allocator);
    }

    /// Set list of frozen function names for direct call optimization
    pub fn setFrozenFunctions(self: *Self, names: []const []const u8) void {
        self.frozen_functions = names;
    }

    /// Redirect a jump target if it's inside a native loop (skip_blocks).
    /// Returns the loop header if target is skipped, otherwise returns target unchanged.
    fn redirectJumpTarget(self: *Self, target: u32) u32 {
        // If target is not in skip_blocks, no redirect needed
        if (!self.skip_blocks.contains(target)) return target;

        // Find the natural loop that contains this block
        for (self.natural_loops) |loop| {
            for (loop.body_blocks) |bid| {
                if (bid == target) {
                    // Redirect to loop header
                    return loop.header_block;
                }
            }
        }

        // Not found in any loop - shouldn't happen, but return original
        return target;
    }

    /// Scan the CFG to detect if argc/argv and this_val are used
    fn scanParameterUsage(self: *Self) void {
        // Reset flags - important since ZigCodeGen may be reused
        self.uses_argc_argv = false;
        self.uses_this_val = false;
        self.has_put_arg = false;
        self.max_arg_idx_used = 0;
        // Scan all blocks for parameter usage
        for (self.func.cfg.blocks.items) |block| {
            for (block.instructions) |instr| {
                switch (instr.opcode) {
                    // Check for argument read opcodes and track max index
                    .get_arg0 => {
                        self.uses_argc_argv = true;
                        self.max_arg_idx_used = @max(self.max_arg_idx_used, 1);
                    },
                    .get_arg1 => {
                        self.uses_argc_argv = true;
                        self.max_arg_idx_used = @max(self.max_arg_idx_used, 2);
                    },
                    .get_arg2 => {
                        self.uses_argc_argv = true;
                        self.max_arg_idx_used = @max(self.max_arg_idx_used, 3);
                    },
                    .get_arg3 => {
                        self.uses_argc_argv = true;
                        self.max_arg_idx_used = @max(self.max_arg_idx_used, 4);
                    },
                    .get_arg => {
                        self.uses_argc_argv = true;
                        self.max_arg_idx_used = @max(self.max_arg_idx_used, instr.operand.arg + 1);
                    },
                    // Check for argument write opcodes - disables arg_cache
                    .put_arg0, .set_arg0 => {
                        self.uses_argc_argv = true;
                        self.has_put_arg = true;
                        self.max_arg_idx_used = @max(self.max_arg_idx_used, 1);
                    },
                    .put_arg1, .set_arg1 => {
                        self.uses_argc_argv = true;
                        self.has_put_arg = true;
                        self.max_arg_idx_used = @max(self.max_arg_idx_used, 2);
                    },
                    .put_arg2, .set_arg2 => {
                        self.uses_argc_argv = true;
                        self.has_put_arg = true;
                        self.max_arg_idx_used = @max(self.max_arg_idx_used, 3);
                    },
                    .put_arg3, .set_arg3 => {
                        self.uses_argc_argv = true;
                        self.has_put_arg = true;
                        self.max_arg_idx_used = @max(self.max_arg_idx_used, 4);
                    },
                    .put_arg, .set_arg => {
                        self.uses_argc_argv = true;
                        self.has_put_arg = true;
                        self.max_arg_idx_used = @max(self.max_arg_idx_used, instr.operand.arg + 1);
                    },
                    // Check for this_val access - these opcodes use this_val
                    .push_this, .init_ctor, .check_ctor, .check_ctor_return => {
                        self.uses_this_val = true;
                    },
                    // special_object with THIS_FUNC (2) or NEW_TARGET (3) uses this_val
                    .special_object => {
                        const obj_type = instr.operand.u8;
                        if (obj_type == 2 or obj_type == 3) {
                            self.uses_this_val = true;
                        }
                    },
                    // Check for closure creation - needs shared var_ref tracking for function hoisting
                    .fclosure, .fclosure8 => {
                        self.has_fclosure = true;
                    },
                    else => {},
                }
            }
        }
    }

    /// Scan loop bodies for argument access - if args are accessed in loops,
    /// we need to cache them to prevent repeated JSValue.dup leaks
    fn scanLoopArgUsage(self: *Self) void {
        self.uses_arg_cache = false;
        self.max_loop_arg_idx = 0;

        // No loops = no caching needed
        if (self.natural_loops.len == 0) return;

        // If function has put_arg operations, don't use arg_cache since it would become stale
        if (self.has_put_arg) return;

        // Collect all block indices that are part of any loop body
        var loop_blocks = std.AutoHashMapUnmanaged(u32, void){};
        defer loop_blocks.deinit(self.allocator);

        for (self.natural_loops) |loop| {
            for (loop.body_blocks) |bid| {
                loop_blocks.put(self.allocator, bid, {}) catch {};
            }
        }

        // Scan loop body blocks for argument access
        for (self.func.cfg.blocks.items, 0..) |block, idx| {
            if (!loop_blocks.contains(@intCast(idx))) continue;

            for (block.instructions) |instr| {
                const arg_idx: ?u32 = switch (instr.opcode) {
                    .get_arg0 => 0,
                    .get_arg1 => 1,
                    .get_arg2 => 2,
                    .get_arg3 => 3,
                    .get_arg => instr.operand.arg,
                    else => null,
                };
                if (arg_idx) |idx_val| {
                    self.uses_arg_cache = true;
                    self.max_loop_arg_idx = @max(self.max_loop_arg_idx, idx_val);
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

    pub fn printLine(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        try self.writeIndent();
        try self.output.writer(self.allocator).print(fmt, args);
        try self.output.append(self.allocator, '\n');
    }

    /// Get the pointer expression for _var_ref_list.
    /// In dispatch_mode, it's accessed via bctx.var_ref_list (already a pointer).
    /// In normal mode, it's a local variable so we take its address.
    fn getVarRefListPtr(self: *Self) []const u8 {
        return if (self.dispatch_mode) "bctx.var_ref_list" else "&_var_ref_list";
    }

    /// Get the pointer expression for _locals_jsv.
    /// In dispatch_mode, it's accessed via bctx.locals_jsv (already a pointer).
    /// In normal mode, it's a local array so we take its address.
    fn getLocalsJsvPtr(self: *Self) []const u8 {
        return if (self.dispatch_mode) "bctx.locals_jsv" else "&_locals_jsv";
    }

    /// Get the expression for _locals_jsv element access.
    /// In dispatch_mode, it's bctx.locals_jsv[idx].
    /// In normal mode, it's _locals_jsv[idx].
    fn getLocalsJsvExpr(self: *Self) []const u8 {
        return if (self.dispatch_mode) "bctx.locals_jsv" else "_locals_jsv";
    }

    /// Emit cleanup code for locals before function return.
    /// Delegates to shared helper in opcode_emitter.zig.
    fn emitLocalsCleanup(self: *Self) !void {
        try opcode_emitter.emitLocalsCleanupShared(Self, self, self.func.var_count, "");
    }

    /// Emit block-level interpreter fallback for a single unsupported opcode
    /// Calls js_frozen_exec_opcode which executes one opcode via interpreter
    fn emitBlockInterpreterFallback(self: *Self, instr: Instruction, block_instrs: []const Instruction, instr_idx: usize) !void {
        _ = block_instrs;
        _ = instr_idx;
        const info = instr.getInfo();
        const op_byte = @intFromEnum(instr.opcode);

        // Get operand value
        const operand: u32 = switch (instr.operand) {
            .none => 0,
            .loc => |v| v,
            .u8 => |v| v,
            .u16 => |v| v,
            .u32 => |v| v,
            .i8 => |v| @bitCast(@as(i32, v)),
            .i16 => |v| @bitCast(@as(i32, v)),
            .i32 => |v| @bitCast(v),
            .atom => |v| v,
            .label => |v| @bitCast(v),
            .const_idx => |v| v,
            .arg => |v| v,
            .var_ref => |v| v,
            .implicit_int => |v| @bitCast(v),
            .implicit_loc => |v| v,
            .implicit_arg => |v| v,
            .implicit_argc => |v| v,
            .atom_u8 => |v| v.atom,
            .atom_u16 => |v| v.atom,
            .atom_label_u8 => |v| v.atom,
            .atom_label_u16 => |v| v.atom,
            .label_u16 => |v| @bitCast(v.label),
            .u32x2 => |v| v.first,
        };

        if (self.vstack.items.len > 0) {
            try self.materializeVStack();
        }

        try self.printLine("// Block-level fallback: execute opcode {s} via interpreter", .{info.name});
        try self.writeLine("{");
        self.pushIndent();

        // Convert frozen stack to JSValue array for interpreter
        const interp_stack_size = @max(self.func.var_count + self.func.stack_size, 256);
        try self.printLine("var _interp_stack: [{d}]zig_runtime.JSValue = undefined;", .{interp_stack_size});
        try self.writeLine("for (0..sp) |_i| { _interp_stack[_i] = stack[_i].toJSValueWithCtx(ctx); }");

        // Convert locals to JSValue array
        try self.printLine("var _interp_locals: [{d}]zig_runtime.JSValue = undefined;", .{@max(1, self.func.var_count)});
        for (0..self.func.var_count) |i| {
            try self.printLine("_interp_locals[{d}] = locals[{d}].toJSValueWithCtx(ctx);", .{ i, i });
        }

        // Call single-opcode interpreter
        try self.printLine("var _new_sp: c_int = @intCast(sp);", .{});
        try self.printLine("const _result = zig_runtime.quickjs.js_frozen_exec_opcode(ctx, {d}, {d}, &_interp_stack, &_new_sp, &_interp_locals, {d});", .{ op_byte, operand, self.func.var_count });

        // Check for exception
        try self.writeLine("if (_result.isException()) {");
        self.pushIndent();
        try self.emitLocalsCleanup();
        if (self.dispatch_mode) {
            try self.writeLine("return .{ .return_value = _result };");
        } else {
            try self.writeLine("return _result;");
        }
        self.popIndent();
        try self.writeLine("}");

        // Restore stack from interpreter result
        try self.writeLine("sp = @intCast(_new_sp);");
        try self.writeLine("for (0..sp) |_i| { stack[_i] = CV.fromJSValue(_interp_stack[_i]); }");

        // Restore locals
        for (0..self.func.var_count) |i| {
            try self.printLine("locals[{d}] = CV.fromJSValue(_interp_locals[{d}]);", .{ i, i });
        }

        self.popIndent();
        try self.writeLine("}");
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
    // Block Functions Output Helpers (for dispatch table mode)
    // These write to the block_functions buffer which is emitted at module level
    // ========================================================================

    fn writeBlockIndent(self: *Self) !void {
        for (0..self.block_fn_indent) |_| {
            try self.block_functions.appendSlice(self.allocator, "    ");
        }
    }

    fn writeLineBlock(self: *Self, str: []const u8) !void {
        try self.writeBlockIndent();
        try self.block_functions.appendSlice(self.allocator, str);
        try self.block_functions.append(self.allocator, '\n');
    }

    fn printLineBlock(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        try self.writeBlockIndent();
        try self.block_functions.writer(self.allocator).print(fmt, args);
        try self.block_functions.append(self.allocator, '\n');
    }

    fn pushIndentBlock(self: *Self) void {
        self.block_fn_indent += 1;
    }

    fn popIndentBlock(self: *Self) void {
        if (self.block_fn_indent > 0) self.block_fn_indent -= 1;
    }

    /// Generate a safe identifier prefix from the function name
    fn getFuncPrefix(self: *const Self) []const u8 {
        // Use function name as-is (already sanitized during frozen code generation)
        return self.func.name;
    }

    /// Emit a return statement that handles dispatch_mode appropriately.
    /// In dispatch_mode (block functions), returns must be wrapped in BlockResult union.
    /// - For exception: return .exception
    /// - For regular value: return .{ .return_value = VALUE }
    fn emitReturnException(self: *Self) !void {
        try self.emitVarRefDetach();
        // Free arg_shadow values before returning (they were duped at function entry)
        if (self.has_put_arg) {
            const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
            for (0..arg_count) |i| {
                try self.printLine("if (arg_shadow[{d}].isRefType()) JSValue.free(ctx, arg_shadow[{d}].toJSValueWithCtx(ctx));", .{ i, i });
            }
        }
        // Cleanup locals before returning (skip in dispatch_mode - done in dispatch loop)
        if (!self.dispatch_mode) {
            try self.emitLocalsCleanup();
        }
        if (self.dispatch_mode) {
            try self.writeLine("return .exception;");
        } else {
            try self.writeLine("return JSValue.EXCEPTION;");
        }
    }

    fn emitReturnValueFmt(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        try self.emitVarRefDetach();
        // Free arg_shadow values before returning (they were duped at function entry)
        if (self.has_put_arg) {
            const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
            for (0..arg_count) |i| {
                try self.printLine("if (arg_shadow[{d}].isRefType()) JSValue.free(ctx, arg_shadow[{d}].toJSValueWithCtx(ctx));", .{ i, i });
            }
        }
        // Cleanup locals before returning (skip in dispatch_mode - done in dispatch loop)
        if (!self.dispatch_mode) {
            try self.emitLocalsCleanup();
        }
        if (self.dispatch_mode) {
            try self.printLine("return .{{ .return_value = " ++ fmt ++ " }};", args);
        } else {
            try self.printLine("return " ++ fmt ++ ";", args);
        }
    }

    fn emitReturnValue(self: *Self, expr: []const u8) !void {
        try self.emitVarRefDetach();
        // Free arg_shadow values before returning (they were duped at function entry)
        if (self.has_put_arg) {
            const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
            for (0..arg_count) |i| {
                try self.printLine("if (arg_shadow[{d}].isRefType()) JSValue.free(ctx, arg_shadow[{d}].toJSValueWithCtx(ctx));", .{ i, i });
            }
        }
        // Cleanup locals before returning (skip in dispatch_mode - done in dispatch loop)
        if (!self.dispatch_mode) {
            try self.emitLocalsCleanup();
        }
        if (self.dispatch_mode) {
            try self.printLine("return .{{ .return_value = {s} }};", .{expr});
        } else {
            try self.printLine("return {s};", .{expr});
        }
    }

    /// Emit var_ref detach with required _locals_jsv sync.
    /// MUST be used instead of emitting js_frozen_var_ref_list_detach directly.
    /// Syncs locals → _locals_jsv first so var_refs see current values, then detaches.
    /// Skips in dispatch_mode (cleanup done in dispatch loop).
    fn emitVarRefDetach(self: *Self) !void {
        if (self.has_fclosure and self.func.var_count > 0 and !self.dispatch_mode) {
            try self.printLine("for (0..{d}) |_i| {{ {s}[_i] = CV.toJSValuePtr(&locals[_i]); }}", .{ self.func.var_count, self.getLocalsJsvExpr() });
            try self.printLine("zig_runtime.quickjs.js_frozen_var_ref_list_detach(ctx, {s});", .{self.getVarRefListPtr()});
        }
    }

    /// Emit an inline exception check with proper cleanup.
    /// Replaces bare `if (x.isException()) return JSValue.EXCEPTION;` patterns
    /// with code that detaches _var_ref_list and frees locals before returning.
    fn emitInlineExceptionReturn(self: *Self, condition_var: []const u8) !void {
        try self.printLine("if ({s}.isException()) {{", .{condition_var});
        self.pushIndent();
        try self.emitReturnException();
        self.popIndent();
        try self.writeLine("}");
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

        // Scan for parameter usage first - needed before scanLoopArgUsage
        // (has_put_arg must be known before deciding to use arg_cache)
        self.scanParameterUsage();

        // Scan for argument usage in loops - enables arg caching to prevent dup leaks
        self.scanLoopArgUsage();

        // NOTE: No fallback to interpreter - codegen must handle all control flow patterns
        // Previously bailed on: multiple depth-0 loops, contaminated blocks in loops, complex self-recursion
        // Now we attempt to codegen everything

        // Check for native specialization (array + numeric functions)
        // This generates ZERO FFI code - all JS types extracted once at entry
        const can_native = self.canUseNativeSpecialization();
        if (CODEGEN_DEBUG) std.debug.print("[codegen] {s}: canUseNativeSpecialization = {}\n", .{ self.func.name, can_native });
        if (can_native) {
            if (CODEGEN_DEBUG) std.debug.print("[codegen] {s}: Using ZERO-FFI native specialization!\n", .{self.func.name});
            try self.emitNativeSpecialized();
            return self.output.toOwnedSlice(self.allocator);
        }

        // Detect dispatch table mode early - needed for correct function signature generation
        // Large functions use dispatch tables where this_val is always used (passed to BlockContext)
        // Note: Single-block functions never use dispatch tables, so check for > 1 blocks
        const block_count_early = self.func.cfg.blocks.items.len;
        if (block_count_early > 1 and block_count_early > DISPATCH_TABLE_THRESHOLD) {
            self.dispatch_mode = true;
        }

        // Function signature
        try self.emitSignature();

        self.pushIndent();

        // For functions with block dispatch, increase comptime branch quota
        // to avoid "evaluation exceeded 1000 backwards branches" during compilation
        // Even small functions can hit this limit due to switch statement analysis
        // Skip for large functions - they use dispatch tables which don't need comptime quota
        const block_count = self.func.cfg.blocks.items.len;
        if (block_count > 5 and block_count <= DISPATCH_TABLE_THRESHOLD) {
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
            const stack_array_size = @max(self.func.var_count + self.func.stack_size, 256);
            try self.printLine("var stack: [{d}]CV = .{{CV.UNDEFINED}} ** {d};", .{ stack_array_size, stack_array_size });
            try self.writeLine("var sp: usize = 0;");
            // Track iterator positions for for-of loops (stack for nested loops)
            try self.writeLine("var for_of_iter_stack: [8]usize = .{0} ** 8;");
            try self.writeLine("var for_of_depth: usize = 0;");
            // Silence "unused" warnings in ReleaseFast when early return happens
            try self.writeLine("_ = &stack; _ = &sp; _ = &for_of_iter_stack; _ = &for_of_depth;");
            try self.writeLine("");

            // Emit argument cache for functions with loops that access arguments
            // Use JSValue array directly for arg_cache (no compression) to ensure correct FFI
            if (self.uses_arg_cache) {
                const cache_size = self.max_loop_arg_idx + 1;
                // Store duped JSValues directly - no compression
                try self.printLine("var arg_cache_js: [{d}]JSValue = undefined;", .{cache_size});
                try self.printLine("for (0..@min(@as(usize, @intCast(argc)), {d})) |_i| {{", .{cache_size});
                self.pushIndent();
                try self.writeLine("arg_cache_js[_i] = JSValue.dup(ctx, argv[_i]);");
                self.popIndent();
                try self.writeLine("}");
                // Also create CV cache for stack operations
                try self.printLine("var arg_cache: [{d}]CV = undefined;", .{cache_size});
                try self.printLine("for (0..@min(@as(usize, @intCast(argc)), {d})) |_i| {{", .{cache_size});
                self.pushIndent();
                try self.writeLine("arg_cache[_i] = CV.fromJSValue(arg_cache_js[_i]);");
                self.popIndent();
                try self.writeLine("}");
                // Add defer to free cached args using ORIGINAL JSValues (not reconstructed)
                try self.printLine("defer for (0..@min(@as(usize, @intCast(argc)), {d})) |_i| {{", .{cache_size});
                self.pushIndent();
                try self.writeLine("JSValue.free(ctx, arg_cache_js[_i]);");
                self.popIndent();
                try self.writeLine("};");
                try self.writeLine("");
            }

            // Check if we need block dispatch (multiple blocks with jumps)
            const needs_dispatch = blocks.len > 1;

            if (needs_dispatch) {
                // Detect natural loops for native while codegen
                self.natural_loops = cfg_mod.detectNaturalLoops(self.func.cfg, self.allocator) catch &.{};

                // Detect counted loops for array sum optimization
                self.counted_loops = cfg_mod.detectCountedLoops(self.func.cfg, self.allocator) catch &.{};

                // Mark ALL loop body blocks to skip from switch dispatch
                // SAFETY: Don't skip blocks for loops with incompatible ops:
                // - put_array_el/define_array_el: stack underflow issue
                // - for_of_next/for_in_next: iterator loops have complex control flow
                // Also: if a loop's ancestor has these ops, don't skip this loop's blocks either
                // (the ancestor will use block dispatch, so nested loops must also use block dispatch)

                // First pass: identify which loops have incompatible ops (including in nested loops)
                var loops_with_incompatible_ops = std.AutoHashMapUnmanaged(u32, void){};
                defer loops_with_incompatible_ops.deinit(self.allocator);

                for (self.natural_loops) |loop| {
                    var has_incompatible_op = false;
                    for (loop.body_blocks) |bid| {
                        if (bid >= blocks.len) continue;
                        for (blocks[bid].instructions) |instr| {
                            if (instr.opcode == .put_array_el or instr.opcode == .define_array_el or
                                instr.opcode == .for_of_next or instr.opcode == .for_in_next or
                                instr.opcode == .to_object)
                            {
                                has_incompatible_op = true;
                                break;
                            }
                        }
                        if (has_incompatible_op) break;
                    }
                    if (has_incompatible_op) {
                        try loops_with_incompatible_ops.put(self.allocator, loop.header_block, {});
                    }
                }

                // Second pass: for each loop, check if any ancestor has incompatible ops
                for (self.natural_loops) |loop| {
                    var skip_this_loop = true;

                    // Check if this loop has incompatible ops
                    if (loops_with_incompatible_ops.contains(loop.header_block)) {
                        skip_this_loop = false;
                    }

                    // Check if any ancestor loop has incompatible ops
                    if (skip_this_loop and loop.parent_header != null) {
                        var parent = loop.parent_header;
                        while (parent) |p| {
                            if (loops_with_incompatible_ops.contains(p)) {
                                skip_this_loop = false;
                                break;
                            }
                            // Find parent's parent
                            var found_parent: ?u32 = null;
                            for (self.natural_loops) |pl| {
                                if (pl.header_block == p) {
                                    found_parent = pl.parent_header;
                                    break;
                                }
                            }
                            parent = found_parent;
                        }
                    }

                    // Only skip blocks if native loops are enabled
                    if (ENABLE_NATIVE_LOOPS and skip_this_loop) {
                        for (loop.body_blocks) |bid| {
                            try self.skip_blocks.put(self.allocator, bid, {});
                        }
                    }
                }

                // Mark counted loop body blocks (only when native loops are enabled)
                if (ENABLE_NATIVE_LOOPS) {
                    for (self.counted_loops) |loop| {
                        if (loop.body_pattern == .array_sum or loop.body_pattern == .array_product) {
                            try self.skip_blocks.put(self.allocator, loop.body_block, {});
                        }
                    }
                }

                // Check if all blocks are in loops (no switch needed)
                var all_in_loops = self.skip_blocks.count() == blocks.len;

                // SAFETY: Disable all_in_loops mode if loops contain put_array_el or for_of_next/for_in_next
                // The native loop codegen has issues with if-statements followed by array writes
                // that cause stack underflow. Iterator loops have complex control flow that native loops don't handle.
                // Fall back to block dispatch which handles these correctly.
                if (all_in_loops) {
                    for (self.natural_loops) |loop| {
                        for (loop.body_blocks) |bid| {
                            if (bid >= blocks.len) continue;
                            for (blocks[bid].instructions) |instr| {
                                if (instr.opcode == .put_array_el or instr.opcode == .define_array_el or
                                    instr.opcode == .for_of_next or instr.opcode == .for_in_next or
                                    instr.opcode == .to_object)
                                {
                                    all_in_loops = false;
                                    break;
                                }
                            }
                            if (!all_in_loops) break;
                        }
                        if (!all_in_loops) break;
                    }
                }

                if (ENABLE_NATIVE_LOOPS and all_in_loops and self.natural_loops.len > 0) {
                    // All code is in loops - emit native loops only
                    try self.emitNativeLoops(blocks);
                } else if (blocks.len > DISPATCH_TABLE_THRESHOLD) {
                    // Large function - use dispatch table to avoid comptime switch explosion
                    // This generates block functions + dispatch table for O(1) runtime dispatch
                    self.dispatch_mode = true;
                    defer self.dispatch_mode = false;

                    // Emit type definitions
                    try self.emitBlockContextStruct();
                    try self.emitBlockResultType();
                    try self.emitBlockFnType();

                    // Emit all block functions
                    for (blocks, 0..) |block, idx| {
                        const block_idx: u32 = @intCast(idx);
                        try self.emitBlockAsFunction(block, block_idx);
                    }

                    // Emit dispatch table
                    try self.emitDispatchTable(blocks.len);

                    // Create block context using function-specific type
                    const min_locals = if (self.func.var_count >= 16) self.func.var_count else 16;
                    const prefix = self.getFuncPrefix();
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
                    if (self.uses_arg_cache) {
                        try self.writeLine(".arg_cache = &arg_cache,");
                    }
                    if (self.has_put_arg) {
                        const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
                        if (arg_count > 0) {
                            try self.writeLine(".arg_shadow = &arg_shadow,");
                        }
                    }
                    // Add var_ref_list if function creates closures
                    if (self.has_fclosure and self.func.var_count > 0) {
                        try self.writeLine(".var_ref_list = &_var_ref_list,");
                    }
                    // Add locals_jsv if function creates closures
                    if (self.has_fclosure and self.func.var_count > 0) {
                        try self.writeLine(".locals_jsv = &_locals_jsv,");
                    }
                    self.popIndent();
                    try self.writeLine("};");
                    try self.writeLine("");

                    // Emit dispatch loop
                    try self.emitDispatchLoop(blocks.len);
                } else {
                    // Small function - use switch dispatch (simpler, potentially faster)
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
                        // SAFETY: Skip native loop if it has put_array_el (stack underflow issue)
                        // SAFETY: Skip native loop if it has for_of_next/for_in_next (iterator loops have complex control flow)
                        var is_loop_header = false;
                        for (self.natural_loops) |loop| {
                            if (loop.header_block == block_idx and loop.depth == 0) {
                                // Check if loop has array writes or iterator ops
                                var has_incompatible_op = false;
                                for (loop.body_blocks) |bid| {
                                    if (bid >= blocks.len) continue;
                                    for (blocks[bid].instructions) |instr| {
                                        if (instr.opcode == .put_array_el or instr.opcode == .define_array_el or
                                            instr.opcode == .for_of_next or instr.opcode == .for_in_next or
                                            instr.opcode == .to_object)
                                        {
                                            has_incompatible_op = true;
                                            break;
                                        }
                                    }
                                    if (has_incompatible_op) break;
                                }

                                if (ENABLE_NATIVE_LOOPS and !has_incompatible_op) {
                                    try self.emitNativeLoopBlock(loop, blocks, block_idx);
                                    is_loop_header = true;
                                }
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
                    try self.writeLine("const fallback_result = zig_runtime.blockFallbackCV(");
                    self.pushIndent();
                    try self.printLine("ctx, \"{s}\", this_val, argc, argv,", .{js_name});
                    try self.printLine("&locals, {d}, &stack, &sp, 0, &next_block_fallback);", .{self.func.var_count});
                    self.popIndent();
                    try self.writeLine("return fallback_result;");
                    self.popIndent();
                    try self.writeLine("}");
                } else {
                    // Clean single block - emit instructions directly
                    // CRITICAL: Clear vstack after each instruction to prevent stale entries.
                    // emitInstruction has mixed vstack behavior: some handlers push (emitCall,
                    // fclosure's vpushStackRef) but others don't pop (drop, put_loc). Without
                    // clearing, stale vstack entries accumulate and corrupt materializeVStack
                    // when a later opcode (like set_name) triggers materialization.
                    for (block.instructions, 0..) |instr, idx| {
                        // Check if this should be skipped for native Array.push optimization
                        if (self.shouldSkipForNativeArrayPush(block.instructions, idx)) {
                            continue; // Skip - tryEmitNativeArrayPush will handle it
                        }
                        // Check if this should be skipped for native Math optimization
                        if (self.shouldSkipForNativeMath(block.instructions, idx) != null) {
                            continue; // Skip - native Math fast path will handle it
                        }
                        const continues = try self.emitInstruction(instr, block.instructions, idx);
                        // Clear vstack after emitInstruction to prevent stale entries
                        // (matches force_stack_mode cleanup in emitInstructionExpr)
                        for (self.vstack.items) |expr| {
                            if (self.isAllocated(expr)) self.allocator.free(expr);
                        }
                        self.vstack.clearRetainingCapacity();
                        if (!continues) break; // Stop after terminating instruction
                    }
                }
            }
        }

        self.popIndent();
        try self.writeLine("}");

        // If dispatch mode was used, prepend block functions to output
        // Block functions must be at module level (before the main function)
        if (self.block_functions.items.len > 0) {
            var combined = std.ArrayListUnmanaged(u8){};
            try combined.appendSlice(self.allocator, self.block_functions.items);
            try combined.append(self.allocator, '\n');
            try combined.appendSlice(self.allocator, self.output.items);
            self.output.deinit(self.allocator);
            self.output = combined;
        }

        return try self.output.toOwnedSlice(self.allocator);
    }

    // ========================================================================
    // Function Signature
    // ========================================================================

    fn emitSignature(self: *Self) !void {
        // Scan CFG to detect which parameters are actually used
        self.scanParameterUsage();

        // All frozen functions must use C calling convention for FFI compatibility
        // var_refs is passed for closure variable access, closure_var_count for bounds checking, cpool for constant pool access
        // noinline prevents LLVM inliner from exploding compile times on 19k+ functions
        try self.print(
            \\pub noinline fn __frozen_{s}(ctx: *zig_runtime.JSContext, this_val: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue, var_refs: ?[*]*zig_runtime.JSVarRef, closure_var_count: c_int, cpool: ?[*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {{
            \\
        , .{self.func.name});
        // Suppress unused warnings only for parameters that are not used
        // For this_val: only discard if no push_this opcode detected AND not in dispatch mode
        // In dispatch mode, this_val is always used (passed to BlockContext)
        if (!self.uses_this_val and !self.dispatch_mode) {
            try self.writeLine("    _ = this_val;");
        }
        // For argc/argv/var_refs/closure_var_count/cpool: use @intFromPtr trick to "use" without triggering pointless discard
        // This satisfies unused warnings even when the params are also accessed later
        // Using @intFromPtr for optional pointers works because null becomes 0
        try self.writeLine("    _ = @as(usize, @intCast(argc)) +% @intFromPtr(argv) +% @intFromPtr(var_refs) +% @as(usize, @intCast(closure_var_count)) +% @intFromPtr(cpool);");

        // Runtime tracing - emit debug print at function entry and exit
        if (TRACE_FROZEN_CALLS) {
            try self.printLine("    std.debug.print(\"[FROZEN] ENTER {s}\\n\", .{{}});", .{self.func.name});
            try self.printLine("    defer std.debug.print(\"[FROZEN] EXIT  {s}\\n\", .{{}});", .{self.func.name});
        }
    }

    // ========================================================================
    // Local Variables
    // ========================================================================

    fn emitLocals(self: *Self) !void {
        // Always emit locals array using CompressedValue (8-byte)
        // Even if var_count=0, we need locals declared because unreachable blocks
        // may still contain get_loc* instructions that reference it
        // Use at least size 16 to cover most dead code scenarios
        const min_locals = if (self.func.var_count >= 16) self.func.var_count else 16;
        try self.printLine("var locals: [{d}]CV = .{{CV.UNDEFINED}} ** {d};", .{ min_locals, min_locals });
        // Silence "unused" warnings in ReleaseFast when early return happens
        try self.writeLine("_ = &locals;");

        // When function creates closures, we need JSValue shadow array for shared var_refs
        // This enables function hoisting - closures see updates to locals assigned later
        if (self.has_fclosure and self.func.var_count > 0) {
            try self.printLine("var _locals_jsv: [{d}]JSValue = .{{JSValue.UNDEFINED}} ** {d};", .{ self.func.var_count, self.func.var_count });
            try self.writeLine("var _var_ref_list: zig_runtime.ListHead = .{ .prev = null, .next = null };");
            try self.writeLine("zig_runtime.quickjs.js_frozen_var_ref_list_init(&_var_ref_list);");
        }

        // When function has put_arg operations, we need shadow storage for arguments
        // This allows assignment to work even when argc < param_index
        // Use max(arg_count, max_arg_idx_used) to handle cases where bytecode arg_count is wrong
        if (self.has_put_arg) {
            const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
            if (arg_count > 0) {
                try self.printLine("var arg_shadow: [{d}]CV = undefined;", .{arg_count});
                // Initialize from argv if provided, else UNDEFINED
                for (0..arg_count) |i| {
                    try self.printLine("arg_shadow[{d}] = if ({d} < argc) CV.fromJSValue(JSValue.dup(ctx, argv[{d}])) else CV.UNDEFINED;", .{ i, i, i });
                }
                try self.writeLine("_ = &arg_shadow;");
            }
        }
    }

    // ========================================================================
    // Dispatch Table Support (for large functions)
    // ========================================================================

    /// Emit the BlockContext struct that holds all shared state for block functions.
    /// This is used for functions with many blocks to avoid comptime switch explosion.
    /// Written to block_functions buffer (module level) with function-specific name.
    fn emitBlockContextStruct(self: *Self) !void {
        const prefix = self.getFuncPrefix();
        try self.printLineBlock("const __frozen_{s}_BlockContext = struct {{", .{prefix});
        self.pushIndentBlock();
        try self.writeLineBlock("ctx: *zig_runtime.JSContext,");
        try self.writeLineBlock("this_val: zig_runtime.JSValue,");
        try self.writeLineBlock("argc: c_int,");
        try self.writeLineBlock("argv: [*]zig_runtime.JSValue,");
        try self.writeLineBlock("var_refs: ?[*]*zig_runtime.JSVarRef,");
        try self.writeLineBlock("closure_var_count: c_int,");
        try self.writeLineBlock("cpool: ?[*]zig_runtime.JSValue,");
        const block_stack_size = @max(self.func.var_count + self.func.stack_size, 256);
        try self.printLineBlock("stack: *[{d}]zig_runtime.CompressedValue,", .{block_stack_size});
        try self.writeLineBlock("sp: *usize,");
        const min_locals = if (self.func.var_count >= 16) self.func.var_count else 16;
        try self.printLineBlock("locals: *[{d}]zig_runtime.CompressedValue,", .{min_locals});
        try self.writeLineBlock("for_of_iter_stack: *[8]usize,");
        try self.writeLineBlock("for_of_depth: *usize,");
        // Add optional arg_cache if function uses it
        if (self.uses_arg_cache) {
            const cache_size = self.max_loop_arg_idx + 1;
            try self.printLineBlock("arg_cache: *[{d}]zig_runtime.CompressedValue,", .{cache_size});
        }
        // Add optional arg_shadow if function has put_arg
        if (self.has_put_arg) {
            const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
            if (arg_count > 0) {
                try self.printLineBlock("arg_shadow: *[{d}]zig_runtime.CompressedValue,", .{arg_count});
            }
        }
        // Add _var_ref_list if function creates closures
        if (self.has_fclosure and self.func.var_count > 0) {
            try self.writeLineBlock("var_ref_list: *zig_runtime.ListHead,");
        }
        // Add _locals_jsv if function creates closures (for passing to createClosureV2)
        if (self.has_fclosure and self.func.var_count > 0) {
            try self.printLineBlock("locals_jsv: *[{d}]zig_runtime.JSValue,", .{self.func.var_count});
        }
        self.popIndentBlock();
        try self.writeLineBlock("};");
        try self.writeLineBlock("");
    }

    /// Emit the BlockResult tagged union for block function returns.
    /// Written to block_functions buffer (module level) with function-specific name.
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

    /// Emit the BlockFn type alias for dispatch table entries.
    /// Written to block_functions buffer (module level) with function-specific name.
    fn emitBlockFnType(self: *Self) !void {
        const prefix = self.getFuncPrefix();
        try self.printLineBlock("const __frozen_{s}_BlockFn = *const fn (*__frozen_{s}_BlockContext) __frozen_{s}_BlockResult;", .{ prefix, prefix, prefix });
        try self.writeLineBlock("");
    }

    /// Emit the dispatch loop that uses the block function table.
    fn emitDispatchLoop(self: *Self, block_count: usize) !void {
        const prefix = self.getFuncPrefix();
        const needs_var_ref_cleanup = self.has_fclosure and self.func.var_count > 0;
        try self.writeLine("var block_id: u32 = 0;");
        try self.writeLine("while (true) {");
        self.pushIndent();
        try self.printLine("if (block_id >= {d}) return zig_runtime.JSValue.UNDEFINED;", .{block_count});
        try self.printLine("switch (__frozen_{s}_block_table[block_id](&bctx)) {{", .{prefix});
        self.pushIndent();
        try self.writeLine(".next_block => |n| block_id = n,");
        // Do var_ref_list cleanup before returning from dispatch loop
        if (needs_var_ref_cleanup) {
            try self.printLine(".return_value => |v| {{ for (0..{d}) |_i| {{ _locals_jsv[_i] = CV.toJSValuePtr(&locals[_i]); }} zig_runtime.quickjs.js_frozen_var_ref_list_detach(ctx, &_var_ref_list); return v; }},", .{self.func.var_count});
            try self.printLine(".return_undef => {{ for (0..{d}) |_i| {{ _locals_jsv[_i] = CV.toJSValuePtr(&locals[_i]); }} zig_runtime.quickjs.js_frozen_var_ref_list_detach(ctx, &_var_ref_list); return zig_runtime.JSValue.UNDEFINED; }},", .{self.func.var_count});
            try self.printLine(".exception => {{ for (0..{d}) |_i| {{ _locals_jsv[_i] = CV.toJSValuePtr(&locals[_i]); }} zig_runtime.quickjs.js_frozen_var_ref_list_detach(ctx, &_var_ref_list); return zig_runtime.JSValue.EXCEPTION; }},", .{self.func.var_count});
        } else {
            try self.writeLine(".return_value => |v| return v,");
            try self.writeLine(".return_undef => return zig_runtime.JSValue.UNDEFINED,");
            try self.writeLine(".exception => return zig_runtime.JSValue.EXCEPTION,");
        }
        self.popIndent();
        try self.writeLine("}");
        self.popIndent();
        try self.writeLine("}");
    }

    /// Emit a block as a standalone function for dispatch table use.
    /// Written to block_functions buffer (module level) with function-specific name.
    fn emitBlockAsFunction(self: *Self, block: BasicBlock, block_idx: u32) !void {
        self.current_block_idx = block_idx;
        const prefix = self.getFuncPrefix();

        // Temporarily swap output buffer to block_functions
        // This allows all existing codegen functions to work unchanged
        const saved_output = self.output;
        const saved_indent = self.indent;
        self.output = self.block_functions;
        self.indent = self.block_fn_indent;

        // Function header with function-specific naming
        try self.printLine("fn __frozen_{s}_block_{d}(bctx: *__frozen_{s}_BlockContext) __frozen_{s}_BlockResult {{", .{ prefix, block_idx, prefix, prefix });
        self.pushIndent();

        // Alias context fields for easier codegen (matches existing code expectations)
        try self.writeLine("const ctx = bctx.ctx;");
        try self.writeLine("const this_val = bctx.this_val;");
        try self.writeLine("const argc = bctx.argc;");
        try self.writeLine("const argv = bctx.argv;");
        try self.writeLine("const var_refs = bctx.var_refs;");
        try self.writeLine("const closure_var_count = bctx.closure_var_count;");
        try self.writeLine("const cpool = bctx.cpool;");
        try self.writeLine("const stack = bctx.stack;");
        try self.writeLine("var sp = bctx.sp.*;");
        try self.writeLine("const locals = bctx.locals;");
        try self.writeLine("const for_of_iter_stack = bctx.for_of_iter_stack;");
        try self.writeLine("var for_of_depth = bctx.for_of_depth.*;");
        try self.writeLine("defer bctx.sp.* = sp;");
        try self.writeLine("defer bctx.for_of_depth.* = for_of_depth;");
        // CV alias for CompressedValue (JSValue is already imported at module level)
        try self.writeLine("const CV = zig_runtime.CompressedValue;");
        // Silence unused warnings using std.debug.assert - references all vars without "pointless discard"
        // In release builds, the assert is optimized away completely
        try self.writeLine("std.debug.assert(@intFromPtr(ctx) | @intFromPtr(&this_val) | @intFromPtr(argv) | @intFromPtr(var_refs) | @intFromPtr(cpool) | @intFromPtr(stack) | @intFromPtr(locals) | @intFromPtr(for_of_iter_stack) != 0);");
        try self.writeLine("std.debug.assert(argc >= 0 and closure_var_count >= 0);");
        try self.writeLine("std.debug.assert(sp < 256 and for_of_depth < 8);");
        // Prevent "never mutated" warnings for variables that may or may not be mutated
        try self.writeLine("sp = sp; for_of_depth = for_of_depth;");

        if (self.uses_arg_cache) {
            try self.writeLine("const arg_cache = bctx.arg_cache;");
            try self.writeLine("std.debug.assert(@intFromPtr(arg_cache) != 0);");
        }
        if (self.has_put_arg) {
            const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
            if (arg_count > 0) {
                try self.writeLine("const arg_shadow = bctx.arg_shadow;");
                try self.writeLine("std.debug.assert(@intFromPtr(arg_shadow) != 0);");
            }
        }
        // Note: In dispatch mode, _var_ref_list and _locals_jsv are accessed via bctx pointers
        // They are NOT extracted as local variables - use bctx.var_ref_list and bctx.locals_jsv directly
        // Reference CV type to prevent unused errors
        try self.writeLine("std.debug.assert(@sizeOf(CV) > 0);");
        try self.writeLine("");

        // SP overflow detection for dispatch table blocks
        try self.printLine("if (sp > 250) zig_runtime.spOverflow(\"{s}\", {d}, sp);", .{ self.getFuncPrefix(), block_idx });

        // Check if block is contaminated
        if (self.func.partial_freeze and block.is_contaminated) {
            const reason = block.contamination_reason orelse "unknown";
            try self.printLine("// CONTAMINATED BLOCK - reason: {s}", .{reason});
            const js_name = if (self.func.js_name.len > 0) self.func.js_name else self.func.name;
            try self.writeLine("{");
            self.pushIndent();
            try self.writeLine("var next_block_fallback: usize = 0;");
            try self.writeLine("const fallback_result = zig_runtime.blockFallbackCV(");
            self.pushIndent();
            try self.printLine("ctx, \"{s}\", this_val, argc, argv,", .{js_name});
            try self.printLine("locals, {d}, stack, &sp, {d}, &next_block_fallback);", .{ self.func.var_count, block_idx });
            self.popIndent();
            try self.writeLine("if (!fallback_result.isUndefined()) {");
            self.pushIndent();
            try self.writeLine("return .{ .return_value = fallback_result };");
            self.popIndent();
            try self.writeLine("}");
            try self.writeLine("return .{ .next_block = @intCast(next_block_fallback) };");
            self.popIndent();
            try self.writeLine("}");
        } else {
            // Clean block - emit instructions with expression-based codegen
            for (self.vstack.items) |expr| {
                if (self.isAllocated(expr)) self.allocator.free(expr);
            }
            self.vstack.clearRetainingCapacity();

            const expected_depth: u32 = @intCast(@max(0, block.stack_depth_in));
            self.base_stack_depth = expected_depth;
            self.base_popped_count = 0;
            self.block_terminated = false;
            self.force_stack_mode = false;

            for (block.instructions, 0..) |instr, idx| {
                try self.emitInstructionExpr(instr, block, null, idx);
                if (self.block_terminated) break;
            }

            // Block terminator
            const successors = block.successors.items;
            const last_op = if (block.instructions.len > 0) block.instructions[block.instructions.len - 1].opcode else .nop;
            const is_return = last_op == .@"return" or last_op == .return_undef or
                last_op == .tail_call or last_op == .tail_call_method;

            if (!is_return and !self.block_terminated) {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }

                if (successors.len == 2) {
                    try self.writeLine("sp -= 1;");
                    const cond_expr = "stack[sp]";
                    const last_instr = block.instructions[block.instructions.len - 1];
                    const is_if_false = last_instr.opcode == .if_false or last_instr.opcode == .if_false8;
                    const target0 = self.redirectJumpTarget(successors[0]);
                    const target1 = self.redirectJumpTarget(successors[1]);

                    if (is_if_false) {
                        try self.printLine("if (!({s}).toBoolWithCtx(ctx)) return .{{ .next_block = {d} }};", .{ cond_expr, target0 });
                        try self.printLine("return .{{ .next_block = {d} }};", .{target1});
                    } else {
                        try self.printLine("if (({s}).toBoolWithCtx(ctx)) return .{{ .next_block = {d} }};", .{ cond_expr, target0 });
                        try self.printLine("return .{{ .next_block = {d} }};", .{target1});
                    }
                } else if (successors.len == 1) {
                    const target_block_id = self.redirectJumpTarget(successors[0]);
                    try self.printLine("return .{{ .next_block = {d} }};", .{target_block_id});
                } else {
                    try self.writeLine("return .return_undef;");
                }
            }
        }

        self.popIndent();
        try self.writeLine("}");
        try self.writeLine("");

        // Restore output buffer
        self.block_functions = self.output;
        self.block_fn_indent = self.indent;
        self.output = saved_output;
        self.indent = saved_indent;
    }

    /// Emit the dispatch table array.
    /// Written to block_functions buffer (module level) with function-specific name.
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

        // SP overflow detection - emit bounds check at every block entry
        const func_prefix = self.getFuncPrefix();
        try self.printLine("if (sp > 250) zig_runtime.spOverflow(\"{s}\", {d}, sp);", .{ func_prefix, block_idx });

        // Check if block is contaminated (has never_freeze opcodes)
        if (self.func.partial_freeze and block.is_contaminated) {
            // Emit fallback to interpreter for contaminated block
            const reason = block.contamination_reason orelse "unknown";
            try self.printLine("// CONTAMINATED BLOCK - reason: {s}", .{reason});
            const js_name = if (self.func.js_name.len > 0) self.func.js_name else self.func.name;
            try self.writeLine("{");
            self.pushIndent();
            try self.writeLine("var next_block_fallback: usize = 0;");
            try self.writeLine("const fallback_result = zig_runtime.blockFallbackCV(");
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
            // Clean block - use expression-based codegen
            // Clear existing vstack - don't pre-populate with existing stack refs
            // Instead, set base_stack_depth so vpop can compute refs when vstack underflows
            for (self.vstack.items) |expr| {
                if (self.isAllocated(expr)) self.allocator.free(expr);
            }
            self.vstack.clearRetainingCapacity();

            // Track base stack depth - existing values that were on stack when block started
            // vpop will use this to generate stack refs when vstack is empty
            const expected_depth: u32 = @intCast(@max(0, block.stack_depth_in));
            self.base_stack_depth = expected_depth;
            self.base_popped_count = 0;
            // Reset flags for this block
            self.block_terminated = false;
            self.force_stack_mode = false;
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

            // Only flush vstack and emit terminator if block wasn't already terminated
            // (e.g., by return instruction which already handled its value)
            if (!is_return and !self.block_terminated) {
                // Flush vstack values to real stack before block terminator
                // In block dispatch mode, values need to be on real stack for:
                // - Short-circuit returns (e.g., && chain going to return block)
                // - Values that carry over between blocks
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }

                if (successors.len == 2) {
                    // Conditional branch - value is on real stack after flush
                    // Pop and check the condition (top of stack)
                    try self.writeLine("sp -= 1; // pop condition");
                    const cond_expr = "stack[sp]";

                    // Detect if terminator is if_false or if_true by checking last instruction
                    const last_instr = block.instructions[block.instructions.len - 1];
                    const is_if_false = last_instr.opcode == .if_false or last_instr.opcode == .if_false8;

                    if (is_if_false) {
                        // if_false: jump to successors[0] when FALSE, fall to successors[1] when TRUE
                        // Redirect targets that are inside native loops to their loop header
                        const target0 = self.redirectJumpTarget(successors[0]);
                        const target1 = self.redirectJumpTarget(successors[1]);
                        try self.printLine("if (!({s}).toBoolWithCtx(ctx)) {{ block_id = {d}; continue; }}", .{ cond_expr, target0 });
                        try self.printLine("block_id = {d}; continue;", .{target1});
                    } else {
                        // if_true: jump to successors[0] when TRUE, fall to successors[1] when FALSE
                        // Redirect targets that are inside native loops to their loop header
                        const target0 = self.redirectJumpTarget(successors[0]);
                        const target1 = self.redirectJumpTarget(successors[1]);
                        try self.printLine("if (({s}).toBoolWithCtx(ctx)) {{ block_id = {d}; continue; }}", .{ cond_expr, target0 });
                        try self.printLine("block_id = {d}; continue;", .{target1});
                    }
                } else if (successors.len == 1) {
                    // Unconditional jump
                    // Redirect target if it's inside a native loop
                    const target_block_id = self.redirectJumpTarget(successors[0]);
                    if (target_block_id < block_idx) {
                        // This is a back-edge - check if target expects stack values
                        if (target_block_id < self.func.cfg.blocks.items.len) {
                            const target_block = &self.func.cfg.blocks.items[target_block_id];
                            if (target_block.stack_depth_in > 0) {
                                // Target expects values on stack - UPDATE the loop counter (not push)
                                // The counter is already on the stack from the initial entry, but
                                // it holds the old value. We need to update it with the new value.
                                // Find the last inc_loc instruction to determine which local
                                const loop_local = self.findLastIncLocal(block.instructions);
                                if (loop_local) |local_idx| {
                                    if (CODEGEN_DEBUG) std.debug.print("[zig-codegen] Loop back-edge: block {d} -> {d}, updating stack with locals[{d}]\n", .{ block_idx, target_block_id, local_idx });
                                    // Update the topmost stack value with the new counter
                                    // This avoids stack growth at each iteration
                                    // Guard with sp > 0 to avoid stack[-1] access when loop body consumed all values
                                    try self.printLine("if (sp > 0) stack[sp - 1] = locals[{d}];", .{local_idx});
                                }
                            }
                        }
                    }
                    try self.printLine("block_id = {d}; continue;", .{target_block_id});
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

        // Get the array from the correct source (local variable, not argv)
        if (loop.array_local) |arr_local| {
            try self.printLine("const _arr = locals[{d}].toJSValueWithCtx(ctx);", .{arr_local});
        } else {
            // Fallback to arg0 if no local was detected (legacy behavior)
            try self.writeLine("const _arr = if (0 < argc) argv[0] else JSValue.UNDEFINED;");
        }

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
        try self.printLine("if (JSValue.toInt32(ctx, &acc_i32, locals[{d}].toJSValueWithCtx(ctx)) == 0) _init_acc = acc_i32;", .{acc_local});
        try self.printLine("if (locals[{d}].isRefType()) JSValue.free(ctx, locals[{d}].toJSValueWithCtx(ctx));", .{ acc_local, acc_local });
        try self.printLine("locals[{d}] = CV.fromJSValue(JSValue.newInt64(ctx, _init_acc + typed_result.sum));", .{acc_local});
        try self.writeLine("// Update loop counter to end value");
        try self.printLine("if (locals[{d}].isRefType()) JSValue.free(ctx, locals[{d}].toJSValueWithCtx(ctx));", .{ counter_local, counter_local });
        try self.printLine("locals[{d}] = CV.fromJSValue(JSValue.newInt64(ctx, _len));", .{counter_local});
        try self.printLine("block_id = {d}; continue;", .{exit_block});
        self.popIndent();
        try self.writeLine("}");
        try self.writeLine("");

        // PATH 2a: Regular Array - ZERO-FFI via direct JSObject access (read-only sum)
        try self.writeLine("// Regular array fast path - zero FFI via direct JSObject access");
        try self.writeLine("const fast_arr = zig_runtime.getFastArrayDirect(_arr);");
        try self.writeLine("if (fast_arr.success) {");
        self.pushIndent();
        try self.writeLine("const js_values = fast_arr.values.?;");
        try self.writeLine("const elem_count: usize = fast_arr.count;");
        try self.writeLine("");
        try self.writeLine("// Get initial accumulator value");
        try self.writeLine("var _acc: i64 = 0;");
        try self.printLine("var acc_i32_fast: i32 = 0;", .{});
        try self.printLine("if (JSValue.toInt32(ctx, &acc_i32_fast, locals[{d}].toJSValueWithCtx(ctx)) == 0) _acc = acc_i32_fast;", .{acc_local});
        try self.writeLine("");
        try self.writeLine("// Direct iteration - no FFI per element!");
        try self.writeLine("for (0..elem_count) |i| {");
        self.pushIndent();
        try self.writeLine("_acc += zig_runtime.jsValueToInt32Inline(js_values[i]);");
        self.popIndent();
        try self.writeLine("}");
        try self.writeLine("");
        try self.writeLine("// Update locals with final values");
        try self.printLine("if (locals[{d}].isRefType()) JSValue.free(ctx, locals[{d}].toJSValueWithCtx(ctx));", .{ acc_local, acc_local });
        try self.printLine("locals[{d}] = CV.fromJSValue(JSValue.newInt64(ctx, _acc));", .{acc_local});
        try self.printLine("if (locals[{d}].isRefType()) JSValue.free(ctx, locals[{d}].toJSValueWithCtx(ctx));", .{ counter_local, counter_local });
        try self.printLine("locals[{d}] = CV.fromJSValue(JSValue.newInt64(ctx, @intCast(elem_count)));", .{counter_local});
        try self.printLine("block_id = {d}; continue;", .{exit_block});
        self.popIndent();
        try self.writeLine("}");
        try self.writeLine("");

        // Fallback: Regular array loop with FFI (for sparse arrays, arrays with holes, etc.)
        try self.writeLine("// Fallback: Regular array loop with FFI (sparse arrays, holes, etc.)");
        try self.writeLine("var _acc: i64 = 0;");
        try self.printLine("var acc_i32_2: i32 = 0;", .{});
        try self.printLine("if (JSValue.toInt32(ctx, &acc_i32_2, locals[{d}].toJSValueWithCtx(ctx)) == 0) _acc = acc_i32_2;", .{acc_local});
        try self.writeLine("var _i: i64 = 0;");
        try self.printLine("var counter_i32: i32 = 0;", .{});
        try self.printLine("if (JSValue.toInt32(ctx, &counter_i32, locals[{d}].toJSValueWithCtx(ctx)) == 0) _i = counter_i32;", .{counter_local});
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
        try self.printLine("if (locals[{d}].isRefType()) JSValue.free(ctx, locals[{d}].toJSValueWithCtx(ctx));", .{ acc_local, acc_local });
        try self.printLine("locals[{d}] = CV.fromJSValue(JSValue.newInt64(ctx, _acc));", .{acc_local});
        try self.printLine("if (locals[{d}].isRefType()) JSValue.free(ctx, locals[{d}].toJSValueWithCtx(ctx));", .{ counter_local, counter_local });
        try self.printLine("locals[{d}] = CV.fromJSValue(JSValue.newInt64(ctx, _i));", .{counter_local});
        try self.printLine("block_id = {d}; continue;", .{exit_block});

        self.popIndent();
        try self.writeLine("},");
    }

    // ========================================================================
    // Native Loop Emission
    // ========================================================================

    /// Check if a loop needs a body: label for break :body
    /// Only needed for EARLY exits to latch (if_true -> latch pattern)
    /// NOT needed for natural fall-through (goto -> latch at end of body)
    /// Also returns false if there's an unconditional return before the if_true -> latch
    fn loopNeedsBodyLabel(self: *Self, loop: cfg_mod.NaturalLoop, blocks: []const BasicBlock) bool {
        _ = self;
        if (loop.latch_block == loop.header_block) return false;
        if (loop.latch_block >= blocks.len) return false;

        // Check if latch has actual code (not just jumps)
        const latch = blocks[loop.latch_block];
        var latch_has_code = false;
        for (latch.instructions) |instr| {
            switch (instr.opcode) {
                .goto, .goto8, .goto16, .if_true, .if_true8, .if_false, .if_false8 => {},
                else => {
                    latch_has_code = true;
                    break;
                },
            }
        }
        if (!latch_has_code) return false;

        // First, check if there's an unconditional return/throw in any body block
        // If there's an unconditional terminator before any if_true -> latch,
        // the break :body would be unreachable
        var has_unconditional_return = false;
        for (loop.body_blocks) |bid| {
            if (bid >= blocks.len) continue;
            if (bid == loop.header_block or bid == loop.latch_block) continue;
            const block = blocks[bid];
            for (block.instructions) |instr| {
                switch (instr.opcode) {
                    .@"return", .return_undef, .throw, .throw_error => {
                        // Check if this return is unconditional by looking at what comes before
                        // In the same block - if there's no if_true/if_false before it, it's unconditional
                        var is_conditional = false;
                        for (block.instructions) |prev_instr| {
                            if (prev_instr.pc >= instr.pc) break;
                            switch (prev_instr.opcode) {
                                .if_true, .if_true8, .if_false, .if_false8 => {
                                    is_conditional = true;
                                    break;
                                },
                                else => {},
                            }
                        }
                        if (!is_conditional) {
                            has_unconditional_return = true;
                        }
                    },
                    else => {},
                }
            }
        }

        // If there's an unconditional return in the body, don't create body: label
        // as any break :body after it would be unreachable
        if (has_unconditional_return) return false;

        // Look for if_true -> latch patterns (conditional early exits)
        for (loop.body_blocks) |bid| {
            if (bid >= blocks.len) continue;
            if (bid == loop.header_block or bid == loop.latch_block) continue;

            const block = blocks[bid];
            if (block.instructions.len == 0) continue;
            if (block.successors.items.len == 0) continue;

            // Check for if_true with first successor (TRUE branch) targeting latch
            for (block.instructions) |instr| {
                switch (instr.opcode) {
                    .if_true, .if_true8 => {
                        // Check if TRUE branch targets latch (continue pattern)
                        if (block.successors.items[0] == loop.latch_block) {
                            return true; // Need body: for break :body
                        }
                    },
                    else => {},
                }
            }
        }
        return false;
    }

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
    /// This follows the same init/condition separation pattern as emitNativeLoopBlock
    fn emitNativeLoop(self: *Self, loop: cfg_mod.NaturalLoop, blocks: []const BasicBlock) !void {
        const header_block = blocks[loop.header_block];

        // Check for iterator loops (for-of/for-in)
        var is_iterator_loop = false;
        for (loop.body_blocks) |bid| {
            if (bid >= blocks.len) continue;
            for (blocks[bid].instructions) |instr| {
                if (instr.opcode == .for_of_next or instr.opcode == .for_in_next) {
                    is_iterator_loop = true;
                    break;
                }
            }
            if (is_iterator_loop) break;
        }

        // Find init/condition boundary (like emitNativeLoopBlock does)
        const condition_start = if (is_iterator_loop) 0 else self.findConditionStart(header_block);

        // Clear vstack and reconstruct from incoming stack values
        for (self.vstack.items) |expr| {
            if (self.isAllocated(expr)) self.allocator.free(expr);
        }
        self.vstack.clearRetainingCapacity();

        // If header expects incoming stack values, trace back where they came from
        const expected_depth = header_block.stack_depth_in;
        if (expected_depth > 0) {
            // Look at predecessors to find what pushed to stack
            for (header_block.predecessors.items) |pred_id| {
                if (pred_id >= blocks.len) continue;
                const pred_block = blocks[pred_id];
                if (pred_block.instructions.len == 0) continue;

                // Check last instructions for get_loc pattern
                var found_count: usize = 0;
                var i = pred_block.instructions.len;
                while (i > 0 and found_count < @as(usize, @intCast(expected_depth))) : (i -= 1) {
                    const instr = pred_block.instructions[i - 1];
                    switch (instr.opcode) {
                        .get_loc0 => {
                            try self.vpush("locals[0]");
                            found_count += 1;
                        },
                        .get_loc1 => {
                            try self.vpush("locals[1]");
                            found_count += 1;
                        },
                        .get_loc2 => {
                            try self.vpush("locals[2]");
                            found_count += 1;
                        },
                        .get_loc3 => {
                            try self.vpush("locals[3]");
                            found_count += 1;
                        },
                        .get_loc, .get_loc8 => {
                            const loc_idx = instr.operand.loc;
                            const ref = std.fmt.allocPrint(self.allocator, "locals[{d}]", .{loc_idx}) catch @panic("OOM");
                            try self.vstack.append(self.allocator, ref);
                            found_count += 1;
                        },
                        else => break, // Stop at non-get_loc instruction
                    }
                }

                // Only use first predecessor's info
                if (found_count > 0) break;
            }
        }

        // Emit initialization code ONCE (before while loop)
        if (condition_start > 0) {
            for (header_block.instructions[0..condition_start], 0..) |instr, idx| {
                try self.emitInstructionExpr(instr, header_block, null, idx);
                if (self.block_terminated) return;
            }
        }

        try self.writeLine("while (true) {");
        self.pushIndent();

        // Check if we need a labeled body block for break :body (used by goto->latch)
        // Only add the label if there's actually a goto instruction targeting the latch block
        // IMPORTANT: Open body_0 BEFORE condition code so if_false8 in header doesn't put body_0 inside an if
        const need_body_label = self.loopNeedsBodyLabel(loop, blocks);
        if (need_body_label) {
            // Use unique label names for nested loops: body_0, body_1, etc.
            try self.printLine("body_{d}: {{", .{self.body_block_depth});
            self.pushIndent();
            self.body_block_depth += 1;
            self.body_label_used = false; // Track if break :body is emitted
        }

        // Emit condition code at START of each iteration (with loop context for break)
        // This is the critical fix: condition must run every iteration, not be skipped
        if (condition_start < header_block.instructions.len) {
            for (header_block.instructions[condition_start..], condition_start..) |instr, idx| {
                try self.emitInstructionExpr(instr, header_block, loop, idx);
                if (self.block_terminated) {
                    // Close body_0 if we opened it
                    if (need_body_label) {
                        // Add dummy break if label wasn't used
                        if (!self.body_label_used) {
                            try self.printLine("if (false) break :body_{d};", .{self.body_block_depth - 1});
                        }
                        self.popIndent();
                        try self.writeLine("}");
                        self.body_block_depth -= 1;
                    }
                    self.popIndent();
                    try self.writeLine("}");
                    return;
                }
            }
        }

        // NOTE: We no longer close if-blocks unconditionally here
        // With the deferred target logic, the fall-through content needs to stay inside the if
        // The if will be closed when the fall-through block is processed in body_blocks

        // Emit remaining body blocks (skip header and latch - latch emitted after body label)
        for (loop.body_blocks) |bid| {
            if (bid >= blocks.len) continue;
            if (bid == loop.header_block) continue; // Skip header - already emitted
            if (need_body_label and bid == loop.latch_block) continue; // Skip latch - emitted after body: block

            const block = blocks[bid];

            // Check if this block is a deeper nested loop header
            var is_nested_loop = false;
            for (self.natural_loops) |nested| {
                if (nested.header_block == bid and nested.depth > loop.depth) {
                    if (nested.parent_header != null and nested.parent_header.? == loop.header_block) {
                        // Save and restore vstack around nested loop
                        var saved_vstack = std.ArrayListUnmanaged([]const u8){};
                        defer saved_vstack.deinit(self.allocator);
                        for (self.vstack.items) |item| {
                            const copy = try self.allocator.dupe(u8, item);
                            try saved_vstack.append(self.allocator, copy);
                        }

                        try self.emitNativeLoop(nested, blocks);

                        // Reset block_terminated after nested loop ONLY if it wasn't a return
                        // The nested loop sets this when emitting continue/break
                        // but we need to emit blocks after the nested loop
                        // Don't clear if function returned - that terminates everything
                        if (!self.function_returned) {
                            self.block_terminated = false;
                        }

                        // Restore vstack
                        for (self.vstack.items) |expr| {
                            if (self.isAllocated(expr)) self.allocator.free(expr);
                        }
                        self.vstack.clearRetainingCapacity();
                        for (saved_vstack.items) |item| {
                            try self.vstack.append(self.allocator, item);
                        }
                        saved_vstack.clearRetainingCapacity();
                    }
                    is_nested_loop = true;
                    break;
                }
            }
            if (is_nested_loop) continue;

            // Check if in a deeper nested loop (skip - will be emitted by that loop)
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

            // Skip emitting block if previous block terminated
            // (unreachable code - return/throw already executed)
            if (self.block_terminated) {
                continue;
            }

            // IMPORTANT: Close if-blocks after the fall-through block is emitted
            // The if body is ONLY the fall-through block, not subsequent blocks
            // NOTE: This must run BEFORE the deferred_blocks skip check, because
            // the deferred block itself may be the trigger to close the if and emit
            // itself! If we skip deferred blocks first, we never reach this logic.
            while (self.if_target_blocks.items.len > 0) {
                const last_fall_through = if (self.if_fall_through_blocks.items.len > 0)
                    self.if_fall_through_blocks.items[self.if_fall_through_blocks.items.len - 1]
                else
                    0;

                // Check if fall-through block has been emitted
                const fall_through_emitted = self.emitted_blocks.contains(last_fall_through);

                // Close if: fall-through was emitted AND current block is NOT the fall-through
                // This ensures we close immediately after the if body, not wait for the target
                if (fall_through_emitted and bid != last_fall_through) {
                    const popped_target = self.if_target_blocks.pop().?;
                    if (self.if_fall_through_blocks.items.len > 0) {
                        _ = self.if_fall_through_blocks.pop();
                    }
                    self.popIndent();
                    try self.writeLine("}");
                    self.if_body_depth -= 1;

                    // If target was deferred, remove it from deferred list and emit it now
                    // (it represents the "continue chain" path for optional chaining)
                    if (self.deferred_blocks.contains(popped_target)) {
                        _ = self.deferred_blocks.remove(popped_target);
                        // Emit the deferred block now (outside the if-block we just closed)
                        if (popped_target < blocks.len and !self.emitted_blocks.contains(popped_target)) {
                            const deferred_block = blocks[popped_target];
                            try self.emitBlockExpr(deferred_block, loop);
                            try self.emitted_blocks.put(self.allocator, popped_target, {});
                        }
                    }
                } else break;
            }

            // Skip deferred blocks - they were already emitted above when closing if
            // (or will be emitted when a future block triggers the if-closing)
            if (self.deferred_blocks.contains(bid)) {
                continue;
            }

            // Skip if this block was already emitted (e.g., as a deferred block above)
            if (self.emitted_blocks.contains(bid)) {
                continue;
            }

            // Emit block using expression-based codegen
            try self.emitBlockExpr(block, loop);
            // Mark this block as emitted for if-closing logic
            try self.emitted_blocks.put(self.allocator, bid, {});
        }

        // Close any remaining if-blocks
        while (self.if_target_blocks.items.len > 0 and self.if_body_depth > 0) {
            _ = self.if_target_blocks.pop();
            if (self.if_fall_through_blocks.items.len > 0) {
                _ = self.if_fall_through_blocks.pop();
            }
            self.popIndent();
            try self.writeLine("}");
            self.if_body_depth -= 1;
        }

        // Close body: block if we used one
        if (need_body_label) {
            // Add trivial use of body label to suppress unused warning
            // Always emit if break :body wasn't emitted (regardless of block_terminated)
            if (!self.body_label_used) {
                try self.printLine("if (false) break :body_{d};", .{self.body_block_depth - 1});
            }
            self.popIndent();
            try self.writeLine("}");
            self.body_block_depth -= 1;
        }

        // Emit latch block at end of while loop body (contains the increment)
        // Only if it wasn't already emitted as part of body_blocks
        if (loop.latch_block < blocks.len and loop.latch_block != loop.header_block) {
            // Skip if latch was already emitted
            if (!self.emitted_blocks.contains(loop.latch_block)) {
                const latch_block = blocks[loop.latch_block];
                // Check if latch has actual code (not just jumps back to header)
                var latch_has_code = false;
                for (latch_block.instructions) |instr| {
                    switch (instr.opcode) {
                        .goto, .goto8, .goto16 => {},
                        else => {
                            latch_has_code = true;
                            break;
                        },
                    }
                }
                if (latch_has_code and !self.block_terminated) {
                    // Only emit latch if body didn't terminate with return/throw
                    try self.emitBlockExpr(latch_block, loop);
                    try self.emitted_blocks.put(self.allocator, loop.latch_block, {});
                }
            }
        }

        self.popIndent();
        try self.writeLine("}");
    }

    /// Emit a native loop as a switch case (for mixed loop/non-loop code)
    fn emitNativeLoopBlock(self: *Self, loop: cfg_mod.NaturalLoop, blocks: []const BasicBlock, header_idx: u32) !void {
        try self.printLine("{d} => {{ // native loop", .{header_idx});
        self.pushIndent();

        // Reset function_returned flag when entering native loop
        // The loop blocks are reachable via a different CFG path from any earlier returns
        // (e.g., early return in an if-body doesn't affect the else branch or subsequent code)
        if (CODEGEN_DEBUG) std.debug.print("[emitNativeLoopBlock] RESETTING function_returned (was {})\n", .{self.function_returned});
        self.function_returned = false;

        // Check if this is a for-of/for-in loop (any block contains for_of_next/for_in_next)
        var is_iterator_loop = false;
        for (loop.body_blocks) |bid| {
            if (bid >= blocks.len) continue;
            for (blocks[bid].instructions) |instr| {
                if (instr.opcode == .for_of_next or instr.opcode == .for_in_next) {
                    is_iterator_loop = true;
                    break;
                }
            }
            if (is_iterator_loop) break;
        }

        // Find the condition boundary in the header block
        // Initialization code runs ONCE before the loop
        // Condition check code runs EVERY iteration
        const header_block = blocks[header_idx];
        // For iterator loops, all code runs every iteration (no initialization)
        const condition_start = if (is_iterator_loop) 0 else self.findConditionStart(header_block);

        // Clear vstack and reconstruct from incoming stack values
        // For loops, predecessors often push values (like loop counters) that the header expects
        for (self.vstack.items) |expr| {
            if (self.isAllocated(expr)) self.allocator.free(expr);
        }
        self.vstack.clearRetainingCapacity();

        // If header expects incoming stack values, trace back where they came from
        // The init block (predecessor of loop) typically ends with get_loc N to push the counter
        // We should use locals[N] in the condition (not stack) so updates via inc_loc are visible
        const expected_depth = header_block.stack_depth_in;
        if (expected_depth > 0) {
            // Look at predecessors to find what pushed to stack
            for (header_block.predecessors.items) |pred_id| {
                if (pred_id >= blocks.len) continue;
                const pred_block = blocks[pred_id];
                if (pred_block.instructions.len == 0) continue;

                // Check last instructions for get_loc pattern
                var found_count: usize = 0;
                var i = pred_block.instructions.len;
                while (i > 0 and found_count < @as(usize, @intCast(expected_depth))) : (i -= 1) {
                    const instr = pred_block.instructions[i - 1];
                    switch (instr.opcode) {
                        .get_loc0 => {
                            try self.vpush("locals[0]");
                            found_count += 1;
                        },
                        .get_loc1 => {
                            try self.vpush("locals[1]");
                            found_count += 1;
                        },
                        .get_loc2 => {
                            try self.vpush("locals[2]");
                            found_count += 1;
                        },
                        .get_loc3 => {
                            try self.vpush("locals[3]");
                            found_count += 1;
                        },
                        .get_loc, .get_loc8 => {
                            const loc_idx = instr.operand.loc;
                            const ref = std.fmt.allocPrint(self.allocator, "locals[{d}]", .{loc_idx}) catch @panic("OOM");
                            try self.vstack.append(self.allocator, ref);
                            found_count += 1;
                        },
                        else => break, // Stop at non-get_loc instruction
                    }
                }

                // Only use first predecessor's info (init block)
                if (found_count > 0) break;
            }
        }

        // Emit initialization code ONCE (before while loop)
        // This includes variable declarations and constant assignments
        if (condition_start > 0) {
            try self.writeLine("// Loop initialization (runs once)");
            for (header_block.instructions[0..condition_start], 0..) |instr, idx| {
                try self.emitInstructionExpr(instr, header_block, null, idx);
                // If block terminated (e.g., unsupported opcode), stop early
                if (self.block_terminated) {
                    self.popIndent();
                    try self.writeLine("},");
                    return;
                }
            }
        }

        try self.writeLine("while (true) {");
        self.pushIndent();

        // Check if we need a labeled body block for break :body (used by goto->latch)
        // Only add the label if there's actually a goto instruction targeting the latch block
        // IMPORTANT: Open body_0 BEFORE condition code so if_false8 in header doesn't put body_0 inside an if
        const need_body_label = self.loopNeedsBodyLabel(loop, blocks);
        if (need_body_label) {
            // Use unique label names for nested loops: body_0, body_1, etc.
            try self.printLine("body_{d}: {{", .{self.body_block_depth});
            self.pushIndent();
            self.body_block_depth += 1;
            self.body_label_used = false; // Track if break :body is emitted
        }

        // For iterator loops, emit the for_of_next block FIRST (condition check at start)
        var iterator_block_emitted = false;
        if (is_iterator_loop) {
            for (loop.body_blocks) |bid| {
                if (bid == header_idx) continue;
                if (bid >= blocks.len) continue;
                const block = blocks[bid];
                // Check if this block contains for_of_next
                var has_iterator_op = false;
                for (block.instructions) |instr| {
                    if (instr.opcode == .for_of_next or instr.opcode == .for_in_next) {
                        has_iterator_op = true;
                        break;
                    }
                }
                if (has_iterator_op) {
                    try self.emitBlockExpr(block, loop);
                    iterator_block_emitted = true;
                    break; // Only emit one iterator block
                }
            }
        }

        // Emit condition check at the start of each iteration
        // Use expression-based codegen with loop context so if_false becomes break
        if (condition_start < header_block.instructions.len) {
            // DEBUG: Print what instructions are in the condition
            if (CODEGEN_DEBUG) {
                std.debug.print("[emitNativeLoopBlock] header_idx={d} condition_start={d} total_instrs={d} cond_instrs={d}\n", .{ header_idx, condition_start, header_block.instructions.len, header_block.instructions.len - condition_start });
                for (header_block.instructions[condition_start..], condition_start..) |instr, i| {
                    std.debug.print("  [{d}] {s}\n", .{ i, @tagName(instr.opcode) });
                }
            }
            for (header_block.instructions[condition_start..], condition_start..) |instr, idx| {
                try self.emitInstructionExpr(instr, header_block, loop, idx);
                // If block terminated, close the while loop and return
                if (self.block_terminated) {
                    // Close body_0 if we opened it
                    if (need_body_label) {
                        // Add dummy break if label wasn't used
                        if (!self.body_label_used) {
                            try self.printLine("if (false) break :body_{d};", .{self.body_block_depth - 1});
                        }
                        self.popIndent();
                        try self.writeLine("}");
                        self.body_block_depth -= 1;
                    }
                    self.popIndent();
                    try self.writeLine("}");
                    self.popIndent();
                    try self.writeLine("},");
                    return;
                }
            }
        }

        // NOTE: We no longer close if-blocks unconditionally here
        // With the deferred target logic, the fall-through content needs to stay inside the if
        // The if will be closed when the fall-through block is processed in body_blocks

        // Emit remaining blocks in the loop (skip header and latch - latch emitted after body:)
        if (CODEGEN_DEBUG) {
            std.debug.print("[emitNativeLoopBlock] header={d} latch={d} body_blocks={any} need_body_label={}\n", .{ header_idx, loop.latch_block, loop.body_blocks, need_body_label });
        }
        for (loop.body_blocks) |bid| {
            if (CODEGEN_DEBUG) {
                std.debug.print("[emitNativeLoopBlock] checking bid={d} block_terminated={} function_returned={} header_idx={d} latch={d}\n", .{ bid, self.block_terminated, self.function_returned, header_idx, loop.latch_block });
            }
            // Skip the iterator block if already emitted
            if (iterator_block_emitted) {
                var is_iterator_block = false;
                if (bid < blocks.len) {
                    for (blocks[bid].instructions) |instr| {
                        if (instr.opcode == .for_of_next or instr.opcode == .for_in_next) {
                            is_iterator_block = true;
                            break;
                        }
                    }
                }
                if (is_iterator_block) continue;
            }
            if (bid == header_idx) continue; // Skip header block - already emitted
            if (need_body_label and bid == loop.latch_block) continue; // Skip latch - emitted after body:
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
                        // Reset block_terminated after nested loop ONLY if it wasn't a return
                        // The nested loop sets this when emitting continue/break
                        // but we need to emit blocks after the nested loop
                        // Don't clear if function returned - that terminates everything
                        if (!self.function_returned) {
                            self.block_terminated = false;
                        }
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

            // Skip block if previous block terminated
            if (self.block_terminated) {
                continue;
            }

            // IMPORTANT: Close if-blocks after the fall-through block is emitted
            // The if body is ONLY the fall-through block, not subsequent blocks
            // NOTE: This must run BEFORE the deferred_blocks skip check, because
            // the deferred block itself may be the trigger to close the if and emit
            // itself! If we skip deferred blocks first, we never reach this logic.
            // (emitNativeLoopBlock version)
            if (CODEGEN_DEBUG and self.if_target_blocks.items.len > 0) {
                std.debug.print("[if-close-before] bid={d} targets={any} fall_throughs={any}\n", .{ bid, self.if_target_blocks.items, self.if_fall_through_blocks.items });
            }
            while (self.if_target_blocks.items.len > 0) {
                const last_fall_through = if (self.if_fall_through_blocks.items.len > 0)
                    self.if_fall_through_blocks.items[self.if_fall_through_blocks.items.len - 1]
                else
                    0;

                // Check if fall-through block has been emitted
                const fall_through_emitted = self.emitted_blocks.contains(last_fall_through);

                if (CODEGEN_DEBUG) {
                    std.debug.print("[if-close-check] bid={d} last_fall_through={d} emitted={}\n", .{ bid, last_fall_through, fall_through_emitted });
                }

                // Close if: fall-through was emitted AND current block is NOT the fall-through
                // This ensures we close immediately after the if body, not wait for the target
                if (fall_through_emitted and bid != last_fall_through) {
                    const popped_target = self.if_target_blocks.pop().?;
                    if (self.if_fall_through_blocks.items.len > 0) {
                        _ = self.if_fall_through_blocks.pop();
                    }
                    self.popIndent();
                    try self.writeLine("}");
                    self.if_body_depth -= 1;

                    if (CODEGEN_DEBUG) {
                        std.debug.print("[if-close] closed if for target={d}, remaining targets={any}\n", .{ popped_target, self.if_target_blocks.items });
                    }

                    // Remove target from deferred list but DON'T emit it yet if other ifs target it
                    // We need to wait until ALL ifs targeting this block are closed
                    // (multiple nested ifs might have the same target)
                    if (self.deferred_blocks.contains(popped_target)) {
                        // Only remove and emit if no other if still targets this block
                        var still_targeted = false;
                        for (self.if_target_blocks.items) |t| {
                            if (t == popped_target) {
                                still_targeted = true;
                                break;
                            }
                        }
                        if (!still_targeted) {
                            _ = self.deferred_blocks.remove(popped_target);
                            // Emit the deferred block now (outside ALL if-blocks targeting it)
                            if (popped_target < blocks.len and !self.emitted_blocks.contains(popped_target)) {
                                if (CODEGEN_DEBUG) {
                                    std.debug.print("[if-close] emitting deferred block {d}\n", .{popped_target});
                                }
                                const deferred_block = blocks[popped_target];
                                try self.emitBlockExpr(deferred_block, loop);
                                try self.emitted_blocks.put(self.allocator, popped_target, {});
                            }
                        }
                    }
                } else break;
            }

            // Skip deferred blocks - they were already emitted above when closing if
            // (or will be emitted when a future block triggers the if-closing)
            if (self.deferred_blocks.contains(bid)) {
                continue;
            }

            // Skip if this block was already emitted (e.g., as a deferred block above)
            if (self.emitted_blocks.contains(bid)) {
                continue;
            }

            // Use expression-based codegen
            if (CODEGEN_DEBUG) {
                std.debug.print("[emitNativeLoopBlock] EMITTING block {d}, {d} instructions\n", .{ bid, block.instructions.len });
            }
            try self.emitBlockExpr(block, loop);
            if (CODEGEN_DEBUG) {
                std.debug.print("[emitNativeLoopBlock] DONE emitting block {d}, block_terminated={}\n", .{ bid, self.block_terminated });
            }
            // Mark this block as emitted for if-closing logic
            try self.emitted_blocks.put(self.allocator, bid, {});

            // If function_returned (unconditional return), stop emitting remaining blocks
            // But if only block_terminated (return inside if-body), we may still have
            // subsequent blocks reachable via different CFG paths (like the latch block)
            if (self.function_returned) {
                break;
            }
            // Reset block_terminated for next block - a return in an if-body doesn't
            // prevent the else branch or subsequent code from being emitted
            self.block_terminated = false;
        }

        // Close any remaining unclosed if-blocks before closing the body: block
        while (self.if_target_blocks.items.len > 0 and self.if_body_depth > 0) {
            _ = self.if_target_blocks.pop();
            if (self.if_fall_through_blocks.items.len > 0) {
                _ = self.if_fall_through_blocks.pop();
            }
            self.popIndent();
            try self.writeLine("}");
            self.if_body_depth -= 1;
        }

        // Close body: block if we used one
        if (need_body_label) {
            // Add trivial use of body label to suppress unused warning
            // Always emit if break :body wasn't emitted (regardless of block_terminated)
            if (!self.body_label_used) {
                try self.printLine("if (false) break :body_{d};", .{self.body_block_depth - 1});
            }
            self.popIndent();
            try self.writeLine("}");
            self.body_block_depth -= 1;
        }

        // Emit latch block at end of while loop body (contains the increment)
        // Only if it wasn't already emitted as part of body_blocks
        if (loop.latch_block < blocks.len and loop.latch_block != loop.header_block) {
            // Skip if latch was already emitted
            if (!self.emitted_blocks.contains(loop.latch_block)) {
                const latch_block = blocks[loop.latch_block];
                // Check if latch has actual code (not just jumps back to header)
                var latch_has_code = false;
                for (latch_block.instructions) |instr| {
                    switch (instr.opcode) {
                        .goto, .goto8, .goto16 => {},
                        else => {
                            latch_has_code = true;
                            break;
                        },
                    }
                }
                if (latch_has_code and !self.block_terminated) {
                    // Only emit latch if body didn't terminate with return/throw
                    try self.emitBlockExpr(latch_block, loop);
                    try self.emitted_blocks.put(self.allocator, loop.latch_block, {});
                }
            }
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

    /// Push a stack reference to vstack and adjust existing stack refs to account for sp change
    /// This fixes the issue where multiple get_array_el calls create stale "stack[sp-1]" references
    fn vpushStackRef(self: *Self) !void {
        // When we push a new stack value, existing stack references become stale
        // "stack[sp-N]" or "stack[sp - N]" now refers to a different position. Update all existing refs.
        // Note: References may be embedded in expressions like "CV.add(stack[sp-1], locals[13])"
        for (self.vstack.items) |*entry| {
            // Find and replace all "stack[sp-N]" and "stack[sp - N]" patterns in the entry
            var new_expr = std.ArrayListUnmanaged(u8){};
            var i: usize = 0;
            const expr = entry.*;
            while (i < expr.len) {
                // Look for "stack[sp-" pattern (9 chars, no spaces)
                if (i + 9 <= expr.len and std.mem.eql(u8, expr[i .. i + 9], "stack[sp-")) {
                    // Found a stack reference without spaces, extract the N value
                    const start = i + 9;
                    var end = start;
                    while (end < expr.len and expr[end] >= '0' and expr[end] <= '9') {
                        end += 1;
                    }
                    if (end > start and end < expr.len and expr[end] == ']') {
                        const n_str = expr[start..end];
                        if (std.fmt.parseInt(usize, n_str, 10)) |n| {
                            // Append adjusted reference: stack[sp-(N+1)]
                            var buf: [32]u8 = undefined;
                            const adjusted = std.fmt.bufPrint(&buf, "stack[sp-{d}]", .{n + 1}) catch "stack[sp-1]";
                            new_expr.appendSlice(self.allocator, adjusted) catch {};
                            i = end + 1; // Skip past the "]"
                            continue;
                        } else |_| {}
                    }
                }
                // Look for "stack[sp - " pattern (11 chars, with spaces)
                if (i + 11 <= expr.len and std.mem.eql(u8, expr[i .. i + 11], "stack[sp - ")) {
                    // Found a stack reference with spaces, extract the N value
                    const start = i + 11;
                    var end = start;
                    while (end < expr.len and expr[end] >= '0' and expr[end] <= '9') {
                        end += 1;
                    }
                    if (end > start and end < expr.len and expr[end] == ']') {
                        const n_str = expr[start..end];
                        if (std.fmt.parseInt(usize, n_str, 10)) |n| {
                            // Append adjusted reference: stack[sp - (N+1)]
                            var buf: [32]u8 = undefined;
                            const adjusted = std.fmt.bufPrint(&buf, "stack[sp - {d}]", .{n + 1}) catch "stack[sp - 1]";
                            new_expr.appendSlice(self.allocator, adjusted) catch {};
                            i = end + 1; // Skip past the "]"
                            continue;
                        } else |_| {}
                    }
                }
                // Not a stack reference, copy character as-is
                new_expr.append(self.allocator, expr[i]) catch {};
                i += 1;
            }
            // Replace the entry with the adjusted expression
            self.allocator.free(entry.*);
            entry.* = new_expr.toOwnedSlice(self.allocator) catch "";
        }
        // Now push the new stack reference
        try self.vpush("stack[sp-1]");
    }

    /// Push a formatted expression onto the virtual stack
    fn vpushFmt(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        const expr = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.vstack.append(self.allocator, expr);
    }

    /// Pop an expression from the virtual stack
    /// If vstack is empty but base_stack_depth > 0, returns a ref to existing stack value
    fn vpop(self: *Self) ?[]const u8 {
        if (self.vstack.items.len > 0) {
            return self.vstack.pop() orelse return null;
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

    /// Pop and free in one step - use when discarding values
    fn vpopAndFree(self: *Self) void {
        if (self.vpop()) |expr| {
            if (self.isAllocated(expr)) self.allocator.free(expr);
        }
    }

    /// Peek at the top expression without popping
    fn vpeek(self: *Self) ?[]const u8 {
        if (self.vstack.items.len > 0) {
            return self.vstack.items[self.vstack.items.len - 1];
        }
        // vstack empty but base stack exists
        // The top is at offset = 1 + base_popped_count
        if (self.base_stack_depth > 0) {
            // Use a fixed literal if offset is 1 (most common case)
            const offset = 1 + self.base_popped_count;
            if (offset == 1) return "stack[sp - 1]";
            if (offset == 2) return "stack[sp - 2]";
            if (offset == 3) return "stack[sp - 3]";
            // For larger offsets, we can't return a static string
            // This is a limitation but deep stacks are rare in peek scenarios
            return "stack[sp - 1]";
        }
        return null;
    }

    /// Check if a string was heap-allocated (vs a literal fallback)
    /// Check if a vstack expression references live storage (locals, arg_cache)
    /// that must not be consumed by operations like addWithCtx.
    fn isStorageRef(self: *Self, expr: []const u8) bool {
        _ = self;
        if (std.mem.startsWith(u8, expr, "locals[")) return true;
        if (std.mem.startsWith(u8, expr, "(if (") and std.mem.indexOf(u8, expr, "arg_cache[") != null) return true;
        return false;
    }

    fn isAllocated(self: *Self, str: []const u8) bool {
        _ = self;
        // Known literal fallbacks - compare pointer addresses
        const fallbacks = [_][]const u8{ "CV.FALSE", "CV.TRUE", "CV.NULL", "CV.UNDEFINED", "CV.newInt(0)", "stack[sp - 1]" };
        for (fallbacks) |f| {
            if (str.ptr == f.ptr) return false;
        }
        return true;
    }

    /// Count UNIQUE stack slot references (stack[sp-N] patterns) in an expression.
    /// If stack[sp-1] appears twice, it counts as 1 consumed slot, not 2.
    fn countStackRefs(_: *Self, expr: []const u8) usize {
        // Track unique offsets seen (up to 16 should be more than enough)
        var seen_offsets: [16]u16 = .{0} ** 16;
        var seen_count: usize = 0;

        var i: usize = 0;
        while (i < expr.len) {
            var offset: ?u16 = null;

            // Check for "stack[sp-N]" (no spaces)
            if (i + 9 <= expr.len and std.mem.eql(u8, expr[i .. i + 9], "stack[sp-")) {
                i += 9;
                var num: u16 = 0;
                while (i < expr.len and expr[i] >= '0' and expr[i] <= '9') {
                    num = num * 10 + @as(u16, @intCast(expr[i] - '0'));
                    i += 1;
                }
                if (i < expr.len and expr[i] == ']') i += 1;
                offset = num;
            }
            // Check for "stack[sp - N]" (with spaces)
            else if (i + 11 <= expr.len and std.mem.eql(u8, expr[i .. i + 11], "stack[sp - ")) {
                i += 11;
                var num: u16 = 0;
                while (i < expr.len and expr[i] >= '0' and expr[i] <= '9') {
                    num = num * 10 + @as(u16, @intCast(expr[i] - '0'));
                    i += 1;
                }
                if (i < expr.len and expr[i] == ']') i += 1;
                offset = num;
            } else {
                i += 1;
            }

            // Add to seen_offsets if not already present
            if (offset) |off| {
                var already_seen = false;
                for (seen_offsets[0..seen_count]) |seen| {
                    if (seen == off) {
                        already_seen = true;
                        break;
                    }
                }
                if (!already_seen and seen_count < 16) {
                    seen_offsets[seen_count] = off;
                    seen_count += 1;
                }
            }
        }
        return seen_count;
    }

    /// Count unique stack references across multiple expressions.
    /// Unlike countStackRefs (single expression), this deduplicates across ALL items.
    fn countStackRefsMulti(_: *Self, exprs: []const []const u8) usize {
        var seen_offsets: [16]u16 = .{0} ** 16;
        var seen_count: usize = 0;

        for (exprs) |expr| {
            var i: usize = 0;
            while (i < expr.len) {
                var offset: ?u16 = null;

                if (i + 9 <= expr.len and std.mem.eql(u8, expr[i .. i + 9], "stack[sp-")) {
                    i += 9;
                    var num: u16 = 0;
                    while (i < expr.len and expr[i] >= '0' and expr[i] <= '9') {
                        num = num * 10 + @as(u16, @intCast(expr[i] - '0'));
                        i += 1;
                    }
                    if (i < expr.len and expr[i] == ']') i += 1;
                    offset = num;
                } else if (i + 11 <= expr.len and std.mem.eql(u8, expr[i .. i + 11], "stack[sp - ")) {
                    i += 11;
                    var num: u16 = 0;
                    while (i < expr.len and expr[i] >= '0' and expr[i] <= '9') {
                        num = num * 10 + @as(u16, @intCast(expr[i] - '0'));
                        i += 1;
                    }
                    if (i < expr.len and expr[i] == ']') i += 1;
                    offset = num;
                } else {
                    i += 1;
                }

                if (offset) |off| {
                    var already_seen = false;
                    for (seen_offsets[0..seen_count]) |seen| {
                        if (seen == off) {
                            already_seen = true;
                            break;
                        }
                    }
                    if (!already_seen and seen_count < 16) {
                        seen_offsets[seen_count] = off;
                        seen_count += 1;
                    }
                }
            }
        }
        return seen_count;
    }

    /// Get next temp variable name
    fn nextTemp(self: *Self) ![]const u8 {
        const name = try std.fmt.allocPrint(self.allocator, "_t{d}", .{self.temp_counter});
        self.temp_counter += 1;
        return name;
    }

    /// Find the index where the loop condition check starts in a header block.
    /// Returns the index of the first instruction that's part of the condition check.
    /// Everything before this index is "initialization" (runs once).
    /// Everything from this index onward is "condition" (runs every iteration).
    fn findConditionStart(self: *Self, block: BasicBlock) usize {
        _ = self;
        const instrs = block.instructions;

        // For for-of loops (containing for_of_next), ALL instructions run every iteration
        // because put_loc stores the current iteration value, not initialization
        for (instrs) |instr| {
            if (instr.opcode == .for_of_next or instr.opcode == .for_in_next) {
                return 0; // No initialization, everything is in the loop body
            }
        }

        // Find where initialization ends and condition checking begins.
        //
        // For a loop like: for (let i = 0, length = arr.length; i < length; ) { ... }
        // The header block contains:
        //   [0-N] Initialization: set_loc_uninitialized, push values, put_loc (store i, length)
        //   [N+1-M] Condition: get_loc (read i), get_loc (read length), lt, if_false8
        //   [M+1-end] Body continuation
        //
        // We need to split at N+1 so initialization runs ONCE, condition runs EVERY iteration.
        //
        // Strategy: Find the LAST put_loc/set_loc instruction that appears BEFORE the
        // branch (if_false/if_true). Everything up to and including that last store
        // is initialization.

        // First, find the branch instruction
        var branch_idx: usize = instrs.len;
        for (instrs, 0..) |instr, i| {
            if (instr.opcode == .if_false or instr.opcode == .if_false8 or
                instr.opcode == .if_true or instr.opcode == .if_true8)
            {
                branch_idx = i;
                break;
            }
        }

        // Now find the last put_loc/set_loc before the branch
        // This marks the end of initialization
        var last_store_idx: ?usize = null;
        var i: usize = 0;
        while (i < branch_idx) : (i += 1) {
            const op = instrs[i].opcode;
            if (op == .put_loc or op == .put_loc0 or op == .put_loc1 or
                op == .put_loc2 or op == .put_loc3 or op == .put_loc8 or
                op == .set_loc or op == .set_loc0 or op == .set_loc1 or
                op == .set_loc2 or op == .set_loc3 or op == .set_loc8 or
                op == .set_loc_uninitialized)
            {
                last_store_idx = i;
            }
        }

        // If we found a store instruction, condition starts right after it
        // Otherwise, the entire block is condition (no initialization)
        if (last_store_idx) |idx| {
            return idx + 1;
        } else {
            return 0;
        }
    }

    /// Emit a block using expression-based codegen (no stack machine)
    fn emitBlockExpr(self: *Self, block: BasicBlock, loop: ?cfg_mod.NaturalLoop) !void {
        // Debug: print block info BEFORE reset
        if (self.debug_mode and block.id == 10) {
            if (CODEGEN_DEBUG) std.debug.print("[emitBlockExpr] block {d}: BEFORE RESET force_stack_mode={}\n", .{ block.id, self.force_stack_mode });
        }

        // If block already terminated (by return/throw in a previous block), skip this block
        if (self.block_terminated or self.function_returned) {
            if (CODEGEN_DEBUG) std.debug.print("[emitBlockExpr] EARLY RETURN block {d}: block_terminated={}, function_returned={}\n", .{ block.id, self.block_terminated, self.function_returned });
            return;
        }

        // Reset flags for this block
        // block_terminated is reset to false for each new block
        // force_stack_mode is also reset (fallback within a block doesn't affect other blocks)
        self.block_terminated = false;
        self.force_stack_mode = false;
        self.native_math_skipped = false;

        if (CODEGEN_DEBUG) {
            std.debug.print("[emitBlockExpr] block {d}: {d} instructions\n", .{ block.id, block.instructions.len });
            for (block.instructions, 0..) |instr, i| {
                std.debug.print("  [{d}] {s}\n", .{ i, @tagName(instr.opcode) });
            }
        }

        // Clear vstack at block entry in native loop mode
        // Each block should build its own vstack from instructions (get_loc, get_arg, etc.)
        // Don't reconstruct from stack_depth_in as that can cause issues with nested loops
        // where the condition should read from locals, not outer stack values
        for (self.vstack.items) |expr| {
            if (self.isAllocated(expr)) self.allocator.free(expr);
        }
        self.vstack.clearRetainingCapacity();

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
            // Handle return opcodes first - terminate block immediately
            if (instr.opcode == .@"return") {
                // Save return value (dup if ref type), cleanup locals, then return
                // Must dup because cleanup may free the same object (e.g., returning a local)
                try self.writeLine("{ const _ret_cv = stack[sp - 1]; const _ret_val = if (_ret_cv.isRefType()) JSValue.dup(ctx, _ret_cv.toJSValueWithCtx(ctx)) else _ret_cv.toJSValueWithCtx(ctx);");
                // Free the original stack value to avoid GC leaks (dup created a new reference)
                try self.writeLine("if (_ret_cv.isRefType()) JSValue.free(ctx, _ret_cv.toJSValueWithCtx(ctx));");
                try self.emitVarRefDetach();
                try self.emitLocalsCleanup();
                if (self.dispatch_mode) {
                    try self.writeLine("return .{ .return_value = _ret_val }; }");
                } else {
                    try self.writeLine("return _ret_val; }");
                }
                self.block_terminated = true;
                return;
            }
            if (instr.opcode == .return_undef) {
                try self.emitVarRefDetach();
                try self.emitLocalsCleanup();
                if (self.dispatch_mode) {
                    try self.writeLine("return .return_undef;");
                } else {
                    try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
                }
                self.block_terminated = true;
                return;
            }

            // Check if this instruction should be skipped for native Array.push optimization
            // This works in force_stack_mode because the stack layout is the same
            if (self.shouldSkipForNativeArrayPush(block.instructions, idx)) {
                return;
            }

            // Control flow still needs special handling for loop break/continue and if-then
            switch (instr.opcode) {
                .if_false, .if_false8 => {
                    // Pop from vstack to stay in sync (strict_eq may have pushed a reference)
                    self.vpopAndFree();
                    // Stack-based if_false: use stack[sp-1] as condition
                    // if_false: jump to target when FALSE, fall through when TRUE
                    if (loop) |l| {
                        const target = block.successors.items[0];
                        // Only break on if_false to exit if we're in the header block (loop condition)
                        // Inner if-statements in body blocks should emit if-statement, not break
                        const is_header = block.id == l.header_block;
                        if (is_header and l.exit_block != null and target == l.exit_block.?) {
                            // FALSE case goes to exit → break when condition is FALSE (loop condition)
                            try self.writeLine("{ const _cond = stack[sp - 1]; sp -= 1; if (!_cond.toBoolWithCtx(ctx)) break; }");
                        } else if (target == l.header_block) {
                            // FALSE case goes to header (continue loop), TRUE case should exit
                            // For for-of loops: condition is `done` flag - TRUE means exhausted, break out
                            try self.writeLine("{ const _cond = stack[sp - 1]; sp -= 1; if (_cond.toBoolWithCtx(ctx)) break; }");
                        } else {
                            // Target is within loop body (including latch) - this is an if-statement
                            // if_false jumps when condition is FALSE, so body executes when TRUE
                            const fall_through = if (block.successors.items.len > 1) block.successors.items[1] else block.id + 1;

                            // Always defer target so it gets emitted AFTER the if closes
                            try self.deferred_blocks.put(self.allocator, target, {});

                            try self.writeLine("sp -= 1; // pop condition");
                            try self.writeLine("if (stack[sp].toBoolWithCtx(ctx)) {");
                            self.pushIndent();
                            self.if_body_depth += 1;
                            try self.if_target_blocks.append(self.allocator, target);
                            // Track fall-through block (should be inside the if)
                            try self.if_fall_through_blocks.append(self.allocator, fall_through);
                        }
                    } else {
                        // Non-loop case: don't emit sp decrement here
                        // The terminator code will handle the condition pop and branch
                    }
                    return;
                },
                .if_true, .if_true8 => {
                    // Pop from vstack to stay in sync (like if_false)
                    self.vpopAndFree();
                    // Stack-based if_true: use stack[sp-1] as condition
                    if (loop) |l| {
                        const target = block.successors.items[0];
                        // Only break on if_true to exit if we're in the header block (loop condition)
                        const is_header = block.id == l.header_block;
                        if (is_header and l.exit_block != null and target == l.exit_block.?) {
                            try self.writeLine("{ const _cond = stack[sp - 1]; sp -= 1; if (_cond.toBoolWithCtx(ctx)) break; }");
                        } else if (target == l.header_block) {
                            // Jump to header is continue (NOT latch - use if-statement for latch)
                            try self.writeLine("{ const _cond = stack[sp - 1]; sp -= 1; if (_cond.toBoolWithCtx(ctx)) continue; }");
                        } else if (target == l.latch_block and self.body_block_depth > 0) {
                            // if_true -> latch means "if (cond) continue;" pattern
                            // Jump to latch when TRUE - use break :body to skip remaining body
                            // Only use break :body if we're inside a body: block
                            try self.printLine("{{ const _cond = stack[sp - 1]; sp -= 1; if (_cond.toBoolWithCtx(ctx)) break :body_{d}; }}", .{self.body_block_depth - 1});
                            self.body_label_used = true;
                        } else {
                            // Target is within loop body (including latch) - this is an if-statement
                            // if_true jumps when condition is TRUE, so body executes when FALSE
                            // Also handles latch target when not inside body: block (wrap in if instead)
                            const fall_through = if (block.successors.items.len > 1) block.successors.items[1] else block.id + 1;

                            // Always defer target so it gets emitted AFTER the if closes
                            try self.deferred_blocks.put(self.allocator, target, {});

                            try self.writeLine("sp -= 1; // pop condition");
                            try self.writeLine("if (!stack[sp].toBoolWithCtx(ctx)) {");
                            self.pushIndent();
                            self.if_body_depth += 1;
                            try self.if_target_blocks.append(self.allocator, target);
                            // Track fall-through block (should be inside the if)
                            try self.if_fall_through_blocks.append(self.allocator, fall_through);
                        }
                    } else {
                        // Non-loop case: don't emit sp decrement here
                        // The terminator code will handle the condition pop and branch
                    }
                    return;
                },
                else => {},
            }
            // Double-check block_terminated before emitting any more code
            // This ensures no code is emitted after a return even if there's a code path issue
            if (self.block_terminated) return;
            const continues = try self.emitInstruction(instr, block.instructions, idx);
            // Clear vstack after emitInstruction in force_stack_mode
            // emitInstruction uses real stack operations, so any vstack pushes (e.g., from
            // emitCallMethod) should be cleared to prevent stale data at block terminator flush
            for (self.vstack.items) |expr| {
                if (self.isAllocated(expr)) self.allocator.free(expr);
            }
            self.vstack.clearRetainingCapacity();
            if (!continues) {
                self.block_terminated = true;
            }
            return;
        }

        // Check if this instruction should be skipped for native Math optimization
        if (self.shouldSkipForNativeMath(block.instructions, idx)) |_| {
            // Skip this get_var("Math") or get_field2("method") - native fast path will handle it
            // Track that we skipped so call_method uses native math even if force_stack_mode gets set later
            self.native_math_skipped = true;
            return;
        }

        // Check if this instruction should be skipped for native Array.push optimization
        if (self.shouldSkipForNativeArrayPush(block.instructions, idx)) {
            // Skip this get_field2("push") - tryEmitNativeArrayPush will handle it inline
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

            // push_this - keep in expression mode for nullish coalescing patterns
            .push_this => {
                if (self.func.has_use_strict) {
                    try self.vpush("CV.fromJSValue(JSValue.dup(ctx, this_val))");
                } else {
                    // Non-strict mode: undefined/null this becomes globalThis
                    try self.vpushFmt("CV.fromJSValue(if (this_val.isUndefined() or this_val.isNull()) JSValue.getGlobal(ctx, \"{s}\") else JSValue.dup(ctx, this_val))", .{"globalThis"});
                }
            },

            // object - create empty object (keep in expression mode)
            .object => try self.vpush("CV.fromJSValue(JSValue.newObject(ctx))"),

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
                    // If expr contains stack references, we need to pop from real stack
                    const ref_count = self.countStackRefs(expr);
                    if (ref_count > 0) try self.printLine("sp -= {d};", .{ref_count});
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    try self.writeLine("locals[0] = stack[sp - 1]; sp -= 1;");
                }
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0) {
                    try self.printLine("{s}[0] = CV.toJSValuePtr(&locals[0]);", .{self.getLocalsJsvExpr()});
                }
            },
            .put_loc1 => {
                if (self.vpop()) |expr| {
                    try self.printLine("locals[1] = {s};", .{expr});
                    const ref_count = self.countStackRefs(expr);
                    if (ref_count > 0) try self.printLine("sp -= {d};", .{ref_count});
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    try self.writeLine("locals[1] = stack[sp - 1]; sp -= 1;");
                }
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0) {
                    try self.printLine("{s}[1] = CV.toJSValuePtr(&locals[1]);", .{self.getLocalsJsvExpr()});
                }
            },
            .put_loc2 => {
                if (self.vpop()) |expr| {
                    try self.printLine("locals[2] = {s};", .{expr});
                    const ref_count = self.countStackRefs(expr);
                    if (ref_count > 0) try self.printLine("sp -= {d};", .{ref_count});
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    try self.writeLine("locals[2] = stack[sp - 1]; sp -= 1;");
                }
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0) {
                    try self.printLine("{s}[2] = CV.toJSValuePtr(&locals[2]);", .{self.getLocalsJsvExpr()});
                }
            },
            .put_loc3 => {
                if (self.vpop()) |expr| {
                    try self.printLine("locals[3] = {s};", .{expr});
                    const ref_count = self.countStackRefs(expr);
                    if (ref_count > 0) try self.printLine("sp -= {d};", .{ref_count});
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    try self.writeLine("locals[3] = stack[sp - 1]; sp -= 1;");
                }
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0) {
                    try self.printLine("{s}[3] = CV.toJSValuePtr(&locals[3]);", .{self.getLocalsJsvExpr()});
                }
            },
            .put_loc, .put_loc8 => {
                const loc_idx = instr.operand.loc;
                if (self.vpop()) |expr| {
                    try self.printLine("locals[{d}] = {s};", .{ loc_idx, expr });
                    // If expr contains stack references, we need to pop from real stack
                    const ref_count = self.countStackRefs(expr);
                    if (ref_count > 0) try self.printLine("sp -= {d};", .{ref_count});
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    try self.printLine("locals[{d}] = stack[sp - 1]; sp -= 1;", .{loc_idx});
                }
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0 and loc_idx < self.func.var_count) {
                    try self.printLine("{s}[{d}] = CV.toJSValuePtr(&locals[{d}]);", .{ self.getLocalsJsvExpr(), loc_idx, loc_idx });
                }
            },

            // Set local (keep on stack) - emit assignment and push back
            // set_loc: stack keeps ref, local gets NEW ref -> dup to local, free old local
            // This is critical for correct refcounting when the stack value is later freed
            // IMPORTANT: After storing, we must replace the vstack expression with a reference
            // to locals[N] so that subsequent uses don't re-evaluate the original expression
            // (e.g., JSValue.newObject() would create a different object each time)
            .set_loc0 => {
                if (self.vpop()) |expr| {
                    try self.printLine("{{ const _old = locals[0]; if (_old.isRefType()) JSValue.free(ctx, _old.toJSValueWithCtx(ctx)); const _v = {s}; locals[0] = if (_v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, _v.toJSValueWithCtx(ctx))) else _v; }}", .{expr});
                    // If expr is a stack reference (from base stack vpop), push it back to preserve
                    // sp accounting in materializeVStack. Otherwise use locals[0] to avoid re-evaluation.
                    if (std.mem.startsWith(u8, expr, "stack[sp")) {
                        try self.vpushFmt("{s}", .{expr});
                    } else {
                        try self.vpush("locals[0]");
                    }
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    try self.writeLine("{ const _old = locals[0]; if (_old.isRefType()) JSValue.free(ctx, _old.toJSValueWithCtx(ctx)); const _v = stack[sp - 1]; locals[0] = if (_v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, _v.toJSValueWithCtx(ctx))) else _v; }");
                }
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0) {
                    try self.printLine("{s}[0] = CV.toJSValuePtr(&locals[0]);", .{self.getLocalsJsvExpr()});
                }
            },
            .set_loc1 => {
                if (self.vpop()) |expr| {
                    try self.printLine("{{ const _old = locals[1]; if (_old.isRefType()) JSValue.free(ctx, _old.toJSValueWithCtx(ctx)); const _v = {s}; locals[1] = if (_v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, _v.toJSValueWithCtx(ctx))) else _v; }}", .{expr});
                    if (std.mem.startsWith(u8, expr, "stack[sp")) {
                        try self.vpushFmt("{s}", .{expr});
                    } else {
                        try self.vpush("locals[1]");
                    }
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    try self.writeLine("{ const _old = locals[1]; if (_old.isRefType()) JSValue.free(ctx, _old.toJSValueWithCtx(ctx)); const _v = stack[sp - 1]; locals[1] = if (_v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, _v.toJSValueWithCtx(ctx))) else _v; }");
                }
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0) {
                    try self.printLine("{s}[1] = CV.toJSValuePtr(&locals[1]);", .{self.getLocalsJsvExpr()});
                }
            },
            .set_loc2 => {
                if (self.vpop()) |expr| {
                    try self.printLine("{{ const _old = locals[2]; if (_old.isRefType()) JSValue.free(ctx, _old.toJSValueWithCtx(ctx)); const _v = {s}; locals[2] = if (_v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, _v.toJSValueWithCtx(ctx))) else _v; }}", .{expr});
                    if (std.mem.startsWith(u8, expr, "stack[sp")) {
                        try self.vpushFmt("{s}", .{expr});
                    } else {
                        try self.vpush("locals[2]");
                    }
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    try self.writeLine("{ const _old = locals[2]; if (_old.isRefType()) JSValue.free(ctx, _old.toJSValueWithCtx(ctx)); const _v = stack[sp - 1]; locals[2] = if (_v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, _v.toJSValueWithCtx(ctx))) else _v; }");
                }
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0) {
                    try self.printLine("{s}[2] = CV.toJSValuePtr(&locals[2]);", .{self.getLocalsJsvExpr()});
                }
            },
            .set_loc3 => {
                if (self.vpop()) |expr| {
                    try self.printLine("{{ const _old = locals[3]; if (_old.isRefType()) JSValue.free(ctx, _old.toJSValueWithCtx(ctx)); const _v = {s}; locals[3] = if (_v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, _v.toJSValueWithCtx(ctx))) else _v; }}", .{expr});
                    if (std.mem.startsWith(u8, expr, "stack[sp")) {
                        try self.vpushFmt("{s}", .{expr});
                    } else {
                        try self.vpush("locals[3]");
                    }
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    try self.writeLine("{ const _old = locals[3]; if (_old.isRefType()) JSValue.free(ctx, _old.toJSValueWithCtx(ctx)); const _v = stack[sp - 1]; locals[3] = if (_v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, _v.toJSValueWithCtx(ctx))) else _v; }");
                }
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0) {
                    try self.printLine("{s}[3] = CV.toJSValuePtr(&locals[3]);", .{self.getLocalsJsvExpr()});
                }
            },
            .set_loc, .set_loc8 => {
                const loc_idx = instr.operand.loc;
                if (self.vpop()) |expr| {
                    try self.printLine("{{ const _old = locals[{d}]; if (_old.isRefType()) JSValue.free(ctx, _old.toJSValueWithCtx(ctx)); const _v = {s}; locals[{d}] = if (_v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, _v.toJSValueWithCtx(ctx))) else _v; }}", .{ loc_idx, expr, loc_idx });
                    if (std.mem.startsWith(u8, expr, "stack[sp")) {
                        try self.vpushFmt("{s}", .{expr});
                    } else {
                        try self.vpushFmt("locals[{d}]", .{loc_idx});
                    }
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    try self.printLine("{{ const _old = locals[{d}]; if (_old.isRefType()) JSValue.free(ctx, _old.toJSValueWithCtx(ctx)); const _v = stack[sp - 1]; locals[{d}] = if (_v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, _v.toJSValueWithCtx(ctx))) else _v; }}", .{ loc_idx, loc_idx });
                }
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0 and loc_idx < self.func.var_count) {
                    try self.printLine("{s}[{d}] = CV.toJSValuePtr(&locals[{d}]);", .{ self.getLocalsJsvExpr(), loc_idx, loc_idx });
                }
            },

            // Arguments - use cached values if available (prevents repeated dup leaks in loops)
            // arg_cache is populated at function start and freed on exit via defer
            // Must check max_loop_arg_idx to ensure the index is within cache bounds
            // CRITICAL: When has_put_arg is true, we must materialize and use arg_shadow,
            // NOT the vfold optimization, because arguments may have been reassigned.
            .get_arg0 => {
                if (self.has_put_arg) {
                    // Argument may be reassigned - must use arg_shadow, can't vfold
                    if (self.vstack.items.len > 0) try self.materializeVStack();
                    try self.writeLine("{ const v = arg_shadow[0]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }");
                    self.base_stack_depth += 1;
                    self.base_popped_count = 0; // Reset - fresh value at stack top
                } else if (self.uses_arg_cache and 0 <= self.max_loop_arg_idx) {
                    try self.vpush("(if (0 < argc) arg_cache[0] else CV.UNDEFINED)");
                } else {
                    try self.vpush("(if (0 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[0])) else CV.UNDEFINED)");
                }
            },
            .get_arg1 => {
                if (self.has_put_arg) {
                    if (self.vstack.items.len > 0) try self.materializeVStack();
                    try self.writeLine("{ const v = arg_shadow[1]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }");
                    self.base_stack_depth += 1;
                    self.base_popped_count = 0; // Reset - fresh value at stack top
                } else if (self.uses_arg_cache and 1 <= self.max_loop_arg_idx) {
                    try self.vpush("(if (1 < argc) arg_cache[1] else CV.UNDEFINED)");
                } else {
                    try self.vpush("(if (1 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[1])) else CV.UNDEFINED)");
                }
            },
            .get_arg2 => {
                if (self.has_put_arg) {
                    if (self.vstack.items.len > 0) try self.materializeVStack();
                    try self.writeLine("{ const v = arg_shadow[2]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }");
                    self.base_stack_depth += 1;
                    self.base_popped_count = 0; // Reset - fresh value at stack top
                } else if (self.uses_arg_cache and 2 <= self.max_loop_arg_idx) {
                    try self.vpush("(if (2 < argc) arg_cache[2] else CV.UNDEFINED)");
                } else {
                    try self.vpush("(if (2 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[2])) else CV.UNDEFINED)");
                }
            },
            .get_arg3 => {
                if (self.has_put_arg) {
                    if (self.vstack.items.len > 0) try self.materializeVStack();
                    try self.writeLine("{ const v = arg_shadow[3]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }");
                    self.base_stack_depth += 1;
                    self.base_popped_count = 0; // Reset - fresh value at stack top
                } else if (self.uses_arg_cache and 3 <= self.max_loop_arg_idx) {
                    try self.vpush("(if (3 < argc) arg_cache[3] else CV.UNDEFINED)");
                } else {
                    try self.vpush("(if (3 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[3])) else CV.UNDEFINED)");
                }
            },
            .get_arg => {
                if (self.has_put_arg) {
                    if (self.vstack.items.len > 0) try self.materializeVStack();
                    try self.printLine("{{ const v = arg_shadow[{d}]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }}", .{instr.operand.arg});
                    self.base_stack_depth += 1;
                    self.base_popped_count = 0; // Reset - fresh value at stack top
                } else if (self.uses_arg_cache and instr.operand.arg <= self.max_loop_arg_idx) {
                    try self.vpushFmt("(if ({d} < argc) arg_cache[{d}] else CV.UNDEFINED)", .{ instr.operand.arg, instr.operand.arg });
                } else {
                    try self.vpushFmt("(if ({d} < argc) CV.fromJSValue(JSValue.dup(ctx, argv[{d}])) else CV.UNDEFINED)", .{ instr.operand.arg, instr.operand.arg });
                }
            },

            // Arithmetic - pop operands, push result expression
            .add => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.addWithCtx(ctx, {s}, {s})", .{ a, b });
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

            // Bitwise operations (vstack-aware to avoid stack leaks in loops)
            .@"and" => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.bitAnd({s}, {s})", .{ a, b });
            },
            .@"or" => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.bitOr({s}, {s})", .{ a, b });
            },
            .xor => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.bitXor({s}, {s})", .{ a, b });
            },
            .shl => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.shl({s}, {s})", .{ a, b });
            },
            .sar => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.sar({s}, {s})", .{ a, b });
            },
            .shr => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.ushr({s}, {s})", .{ a, b });
            },
            .not => {
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("CV.bitNot({s})", .{a});
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
                // Must use eqWithCtx for JS == (abstract equality: undefined == null is true)
                try self.vpushFmt("CV.eqWithCtx(ctx, {s}, {s})", .{ a, b });
            },
            .neq => {
                const b = self.vpop() orelse "CV.UNDEFINED";
                const free_b = self.isAllocated(b);
                defer if (free_b) self.allocator.free(b);
                const a = self.vpop() orelse "CV.UNDEFINED";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                // Must use eqWithCtx for JS != (abstract equality: undefined == null is true)
                try self.vpushFmt("CV.neqWithCtx(ctx, {s}, {s})", .{ a, b });
            },

            // Logical NOT - uses CV.toBoolWithCtx() for proper truthiness (empty string is falsy)
            .lnot => {
                const a = self.vpop() orelse "stack[sp - 1]";
                const free_a = self.isAllocated(a);
                defer if (free_a) self.allocator.free(a);
                try self.vpushFmt("(if (({s}).toBoolWithCtx(ctx)) CV.FALSE else CV.TRUE)", .{a});
            },

            // is_undefined_or_null - PEEK at TOS and PUSH boolean (don't consume original)
            // This opcode reads TOS without consuming, so we can't use vstack folding.
            // Materialize vstack first, then emit the check directly.
            .is_undefined_or_null => {
                try self.materializeVStack();
                try self.writeLine("stack[sp] = (if (stack[sp-1].isUndefined() or stack[sp-1].isNull()) CV.TRUE else CV.FALSE); sp += 1;");
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
            // Uses nativeGetLength for O(1) array/string length access
            // IMPORTANT: Stack semantics are pop 1 push 1 (net 0), not net +1
            .get_length => {
                const obj_expr = self.vpop() orelse "CV.UNDEFINED";
                const should_free = self.isAllocated(obj_expr);
                defer if (should_free) self.allocator.free(obj_expr);
                // Check if object came from base_stack (stack reference) vs vstack (expression)
                // If from base_stack, replace in place (net 0); if from vstack, push (was already not on stack)
                const from_base_stack = std.mem.startsWith(u8, obj_expr, "stack[sp");

                if (std.mem.indexOf(u8, obj_expr, "CV.fromJSValue(argv[")) |start_idx| {
                    // Direct argv reference - use nativeGetLengthCV to avoid LLVM return corruption on WASM32
                    const argv_start = std.mem.indexOf(u8, obj_expr, "argv[").?;
                    const bracket_end = std.mem.indexOf(u8, obj_expr[argv_start..], "]").? + argv_start;
                    const arg_num_str = obj_expr[argv_start + 5 .. bracket_end];
                    _ = start_idx;
                    try self.printLine("stack[sp] = if ({s} < argc) zig_runtime.nativeGetLengthCV(ctx, argv[{s}]) else CV.UNDEFINED; sp += 1;", .{ arg_num_str, arg_num_str });
                    // vpushStackRef adjusts existing refs and adds new ref for pushed value
                    try self.vpushStackRef();
                } else if (from_base_stack) {
                    // Object was on real stack - replace in place (pop 1 push 1 = net 0)
                    // Extract the stack offset to know where to write back
                    // obj_expr is "stack[sp - N]" where N is the offset
                    try self.printLine("{{ const obj = ({s}).toJSValueWithCtx(ctx); {s} = zig_runtime.nativeGetLengthCV(ctx, obj); }}", .{ obj_expr, obj_expr });
                    // The value is still at the same position, push a reference to it
                    // But vpop already decremented base_stack_depth, so increment it back
                    self.base_stack_depth += 1;
                    self.base_popped_count -= 1;
                } else {
                    // Object was from vstack expression - push result to real stack
                    try self.printLine("{{ const obj = ({s}).toJSValueWithCtx(ctx); stack[sp] = zig_runtime.nativeGetLengthCV(ctx, obj); sp += 1; }}", .{obj_expr});
                    // vpushStackRef adjusts existing refs and adds new ref for pushed value
                    try self.vpushStackRef();
                }
            },

            // get_array_el: pop idx, pop arr, push arr[idx]
            // Push directly to real stack to avoid cross-block temp scope issues
            // arr and idx were dup'd on push (get_arg dups), so we must free them
            // (matches old C codegen: FROZEN_FREE(ctx, arr); FROZEN_FREE(ctx, idx);)
            // EXCEPTION: Direct local references (locals[N]) should NOT be freed
            .get_array_el => {
                // Track how many values came from base_stack to adjust sp correctly
                // Bytecode semantics: pop idx, pop arr, push result (net sp change: -1)
                const base_popped_before = self.base_popped_count;
                const idx_expr = self.vpop() orelse "CV.UNDEFINED";
                const idx_free = self.isAllocated(idx_expr);
                defer if (idx_free) self.allocator.free(idx_expr);
                const arr_expr = self.vpop() orelse "CV.UNDEFINED";
                const arr_free = self.isAllocated(arr_expr);
                defer if (arr_free) self.allocator.free(arr_expr);
                const base_consumed = self.base_popped_count - base_popped_before;

                // CRITICAL: If there are remaining vstack items, they were pushed BEFORE
                // the values we just popped. When we write the result to real stack,
                // those vstack items need to be materialized first to preserve ordering.
                // Otherwise the next vpop would incorrectly pop from vstack instead of
                // getting our result from the real stack.
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }

                // Check if expressions are direct local references (don't free those)
                const arr_is_local = std.mem.startsWith(u8, arr_expr, "locals[");
                const idx_is_local = std.mem.startsWith(u8, idx_expr, "locals[");
                // Use getPropertyValue for proper dynamic property access (supports string keys)
                // Check if arr_expr uses arg_cache - arg_cache_js doesn't need freeing (not dup'd)
                // In dispatch_mode, arg_cache_js is not available from block functions
                if (self.uses_arg_cache and std.mem.indexOf(u8, arr_expr, "arg_cache[") != null and !self.dispatch_mode) {
                    // Use arg_cache_js directly for array access (avoid CV→JSValue round-trip corruption)
                    // arg_cache_js itself doesn't need freeing, but idx is consumed (if not a local)
                    const cache_start = std.mem.indexOf(u8, arr_expr, "arg_cache[").?;
                    const bracket_end = std.mem.indexOf(u8, arr_expr[cache_start..], "]").? + cache_start;
                    const arg_num_str = arr_expr[cache_start + 10 .. bracket_end]; // Get the N in arg_cache[N]
                    try self.printLine("{{ const idx_cv = {s}; const idx_jsv = idx_cv.toJSValueWithCtx(ctx);", .{idx_expr});
                    // Adjust result slot based on how many base_stack values were consumed
                    if (base_consumed == 0) {
                        try self.printLine("  stack[sp] = if ({s} < argc) CV.fromJSValue(JSValue.getPropertyValue(ctx, arg_cache_js[{s}], JSValue.dup(ctx, idx_jsv))) else CV.UNDEFINED;", .{ arg_num_str, arg_num_str });
                    } else if (base_consumed == 1) {
                        try self.printLine("  stack[sp - 1] = if ({s} < argc) CV.fromJSValue(JSValue.getPropertyValue(ctx, arg_cache_js[{s}], JSValue.dup(ctx, idx_jsv))) else CV.UNDEFINED;", .{ arg_num_str, arg_num_str });
                    } else {
                        try self.printLine("  stack[sp - 2] = if ({s} < argc) CV.fromJSValue(JSValue.getPropertyValue(ctx, arg_cache_js[{s}], JSValue.dup(ctx, idx_jsv))) else CV.UNDEFINED;", .{ arg_num_str, arg_num_str });
                    }
                    if (!idx_is_local) {
                        try self.writeLine("  if (idx_cv.isRefType()) JSValue.free(ctx, idx_jsv);");
                    }
                    // Adjust sp based on consumed values: 0 consumed → sp += 1, 1 → no change, 2 → sp -= 1
                    if (base_consumed == 0) {
                        try self.writeLine("  sp += 1; }");
                    } else if (base_consumed == 2) {
                        try self.writeLine("  sp -= 1; }");
                    } else {
                        try self.writeLine("  }");
                    }
                    // Reset base_popped_count after stack modifications
                    if (base_consumed > 0) {
                        self.base_popped_count = 0;
                    }
                } else {
                    // Generic path: free arr/idx only if they were dup'd (not direct locals)
                    try self.printLine("{{ const arr_cv = {s}; const idx_cv = {s};", .{ arr_expr, idx_expr });
                    try self.writeLine("  const arr_jsv = arr_cv.toJSValueWithCtx(ctx);");
                    try self.writeLine("  const idx_jsv = idx_cv.toJSValueWithCtx(ctx);");
                    try self.writeLine("  const result = CV.fromJSValue(JSValue.getPropertyValue(ctx, arr_jsv, JSValue.dup(ctx, idx_jsv)));");
                    if (!arr_is_local) {
                        try self.writeLine("  if (arr_cv.isRefType()) JSValue.free(ctx, arr_jsv);");
                    }
                    if (!idx_is_local) {
                        try self.writeLine("  if (idx_cv.isRefType()) JSValue.free(ctx, idx_jsv);");
                    }
                    // Adjust result slot and sp based on how many base_stack values were consumed
                    // 0 consumed: push to stack[sp], sp += 1
                    // 1 consumed: replace at stack[sp-1], sp unchanged
                    // 2 consumed: replace at stack[sp-2], sp -= 1
                    if (base_consumed == 0) {
                        try self.writeLine("  stack[sp] = result; sp += 1; }");
                    } else if (base_consumed == 1) {
                        try self.writeLine("  stack[sp - 1] = result; }");
                    } else {
                        try self.writeLine("  stack[sp - 2] = result; sp -= 1; }");
                    }
                }
                // Track that value is on real stack via base_stack_depth
                // Don't use vpushStackRef - it adds a vstack entry that gets duplicated later
                self.base_stack_depth += 1;
                // CRITICAL: Always reset base_popped_count when pushing to base stack.
                // The push generates sp += 1, so the new value is at stack[sp-1].
                // Previous pops already adjusted sp, so base_popped_count must restart from 0.
                self.base_popped_count = 0;
            },

            // get_array_el2: pop idx, pop arr, push arr, push arr[idx]
            // Stack: [arr, idx] → [arr, value] - arr stays, idx is consumed and must be freed
            // EXCEPTION: Direct local references (locals[N]) should NOT be freed
            .get_array_el2 => {
                const idx_expr = self.vpop() orelse "CV.UNDEFINED";
                const idx_free = self.isAllocated(idx_expr);
                defer if (idx_free) self.allocator.free(idx_expr);
                const arr_expr = self.vpop() orelse "CV.UNDEFINED";
                const arr_free = self.isAllocated(arr_expr);
                defer if (arr_free) self.allocator.free(arr_expr);

                // CRITICAL: Materialize remaining vstack before pushing to real stack
                // (same issue as get_array_el - preserve ordering)
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }

                // Check if idx is a direct local reference (don't free those)
                const idx_is_local = std.mem.startsWith(u8, idx_expr, "locals[");
                // Use getPropertyValue for proper dynamic property access (supports string keys)
                // Check if arr_expr uses arg_cache - if so, use arg_cache_js directly
                // In dispatch_mode, arg_cache_js is not available from block functions
                if (self.uses_arg_cache and std.mem.indexOf(u8, arr_expr, "arg_cache[") != null and !self.dispatch_mode) {
                    // Use arg_cache_js directly - arr stays on stack, idx is consumed (if not local)
                    const cache_start = std.mem.indexOf(u8, arr_expr, "arg_cache[").?;
                    const bracket_end = std.mem.indexOf(u8, arr_expr[cache_start..], "]").? + cache_start;
                    const arg_num_str = arr_expr[cache_start + 10 .. bracket_end]; // Get the N in arg_cache[N]
                    try self.printLine("{{ const idx_cv = {s}; const idx_jsv = idx_cv.toJSValueWithCtx(ctx);", .{idx_expr});
                    try self.printLine("  stack[sp] = {s};", .{arr_expr});
                    try self.printLine("  stack[sp + 1] = if ({s} < argc) CV.fromJSValue(JSValue.getPropertyValue(ctx, arg_cache_js[{s}], JSValue.dup(ctx, idx_jsv))) else CV.UNDEFINED;", .{ arg_num_str, arg_num_str });
                    if (!idx_is_local) {
                        try self.writeLine("  if (idx_cv.isRefType()) JSValue.free(ctx, idx_jsv);");
                    }
                    try self.writeLine("  sp += 2; }");
                } else {
                    // Generic path: arr stays on stack, idx is consumed (if not local)
                    try self.printLine("{{ const idx_cv = {s};", .{idx_expr});
                    try self.writeLine("  const idx_jsv = idx_cv.toJSValueWithCtx(ctx);");
                    try self.printLine("  const arr_val = ({s}).toJSValueWithCtx(ctx);", .{arr_expr});
                    try self.printLine("  stack[sp] = {s};", .{arr_expr});
                    try self.writeLine("  stack[sp + 1] = CV.fromJSValue(JSValue.getPropertyValue(ctx, arr_val, JSValue.dup(ctx, idx_jsv)));");
                    if (!idx_is_local) {
                        try self.writeLine("  if (idx_cv.isRefType()) JSValue.free(ctx, idx_jsv);");
                    }
                    try self.writeLine("  sp += 2; }");
                }
                // Track that 2 values are on real stack via base_stack_depth
                // Don't use vpushStackRef - it adds vstack entries that get duplicated later
                self.base_stack_depth += 2;
                self.base_popped_count = 0;
            },

            // Stack operations
            .dup => {
                // Always materialize vstack before dup to avoid double-evaluation.
                // Re-pushing computed expressions causes them to be evaluated twice during
                // materializeVStack, which can produce incorrect results or side effects.
                // Stack references also can't be re-pushed (sp accounting issues).
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                try self.writeLine("{ const v = stack[sp - 1]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }");
                self.base_stack_depth += 1;
                self.base_popped_count = 0;
            },
            .drop => {
                if (self.vpop()) |expr| {
                    // Check if this is a reference to an existing value on the real stack
                    // (returned from base_stack_depth when vstack was empty)
                    // If so, we need to emit actual drop code to decrement sp and free ref
                    const is_stack_ref = std.mem.startsWith(u8, expr, "stack[sp - ");
                    if (is_stack_ref) {
                        try self.writeLine("{ const v = stack[sp - 1]; if (v.isRefType()) JSValue.free(ctx, v.toJSValueWithCtx(ctx)); sp -= 1; }");
                    }
                    // Free the expression string if it was allocated
                    if (self.isAllocated(expr)) self.allocator.free(expr);
                } else {
                    // vstack empty AND no base stack values - emit real stack drop as fallback
                    try self.writeLine("{ const v = stack[sp - 1]; if (v.isRefType()) JSValue.free(ctx, v.toJSValueWithCtx(ctx)); sp -= 1; }");
                }
            },
            .swap => {
                // If vstack has items, materialize first to avoid mixing vstack expressions
                // with base_stack refs (stack[sp-N] refs are relative to current sp)
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                    // materializeVStack now updates base_stack_depth internally
                }
                const b = self.vpop();
                const a = self.vpop();
                // After materializing, both operands should be stack refs
                // Check if both are simple stack refs - if so, do in-place swap
                const a_is_stack_ref = if (a) |ae| std.mem.startsWith(u8, ae, "stack[sp - ") else false;
                const b_is_stack_ref = if (b) |be| std.mem.startsWith(u8, be, "stack[sp - ") else false;
                if (a_is_stack_ref and b_is_stack_ref) {
                    // Both are stack refs - generate in-place swap on real stack
                    try self.writeLine("{ const tmp = stack[sp - 1]; stack[sp - 1] = stack[sp - 2]; stack[sp - 2] = tmp; }");
                    // Free the allocated refs
                    if (a) |ae| if (self.isAllocated(ae)) self.allocator.free(ae);
                    if (b) |be| if (self.isAllocated(be)) self.allocator.free(be);
                    // Track that 2 values still exist on real stack via base_stack_depth
                    // The vpop calls may have decremented base_stack_depth, restore it
                    // so subsequent vpops can generate correct refs
                    self.base_stack_depth += 2;
                    // Also reset base_popped_count since we're "restoring" the base stack state
                    if (self.base_popped_count >= 2) {
                        self.base_popped_count -= 2;
                    } else {
                        self.base_popped_count = 0;
                    }
                } else {
                    // Use vstack-based swap (shouldn't happen after materialization, but handle it)
                    if (b) |be| try self.vstack.append(self.allocator, be);
                    if (a) |ae| try self.vstack.append(self.allocator, ae);
                }
            },

            // Control flow
            .if_false, .if_false8 => {
                // Only handle if_false here when inside a loop context
                // When loop is null, let the block terminator handler in emitBlock deal with it
                if (loop) |l| {
                    // FIX: Materialize vstack FIRST so real stack matches CFG expectations
                    // This prevents vstack/real-stack mismatch at control flow boundaries
                    // (e.g., DUP in vstack mode doesn't modify real stack, but CFG assumes it does)
                    if (self.vstack.items.len > 0) {
                        try self.materializeVStack();
                        // materializeVStack now updates base_stack_depth internally
                    }
                    const cond_expr = self.vpop() orelse "stack[sp - 1]";
                    const should_free = self.isAllocated(cond_expr);
                    defer if (should_free) self.allocator.free(cond_expr);
                    // Count ALL stack references in the condition - they all need to be popped
                    // This handles composite expressions like CV.lt(locals[1], stack[sp-1])
                    const stack_ref_count = self.countStackRefs(cond_expr);
                    const target = block.successors.items[0];
                    if (l.exit_block != null and target == l.exit_block.?) {
                        // Jump to loop exit - evaluate condition FIRST, then pop, then check
                        if (stack_ref_count > 0) {
                            try self.printLine("{{ const _cond = ({s}).toBoolWithCtx(ctx); sp -= {d}; if (!_cond) break; }}", .{ cond_expr, stack_ref_count });
                        } else {
                            try self.printLine("if (!({s}).toBoolWithCtx(ctx)) break;", .{cond_expr});
                        }
                    } else if (target == l.header_block) {
                        // if_false: FALSE case jumps to header (continue), TRUE case exits
                        // For for-of loops: condition is `done` - TRUE means exhausted, break out
                        if (stack_ref_count > 0) {
                            try self.printLine("{{ const _cond = ({s}).toBoolWithCtx(ctx); sp -= {d}; if (_cond) break; }}", .{ cond_expr, stack_ref_count });
                        } else {
                            try self.printLine("if (({s}).toBoolWithCtx(ctx)) break;", .{cond_expr});
                        }
                    } else {
                        // Target is within loop body (including latch) - this is an if-statement
                        // For if_false, we wrap the TRUE case (fall-through) in if(cond)
                        // This correctly handles patterns by wrapping remaining code in an if-block
                        const fall_through = if (block.successors.items.len > 1) block.successors.items[1] else block.id + 1;

                        // Always defer target so it gets emitted AFTER the if closes
                        // Target is the "else" case (condition FALSE), must be outside the if body
                        try self.deferred_blocks.put(self.allocator, target, {});

                        // Evaluate condition FIRST, then pop stack refs, then start if-block
                        if (stack_ref_count > 0) {
                            try self.printLine("const _cond_if_{d} = ({s}).toBoolWithCtx(ctx); sp -= {d}; if (_cond_if_{d}) {{", .{ self.if_body_depth, cond_expr, stack_ref_count, self.if_body_depth });
                        } else {
                            try self.printLine("if (({s}).toBoolWithCtx(ctx)) {{", .{cond_expr});
                        }
                        self.pushIndent();
                        self.if_body_depth += 1;
                        try self.if_target_blocks.append(self.allocator, target);
                        // Track fall-through block (should be inside the if)
                        try self.if_fall_through_blocks.append(self.allocator, fall_through);
                    }
                }
                // When loop is null, don't pop from vstack - let emitBlock terminator handle it
            },
            .if_true, .if_true8 => {
                // Only handle if_true here when inside a loop context
                // When loop is null, let the block terminator handler in emitBlock deal with it
                if (loop) |l| {
                    // FIX: Materialize vstack FIRST so real stack matches CFG expectations
                    // This prevents vstack/real-stack mismatch at control flow boundaries
                    // (e.g., DUP in vstack mode doesn't modify real stack, but CFG assumes it does)
                    if (self.vstack.items.len > 0) {
                        try self.materializeVStack();
                        // materializeVStack now updates base_stack_depth internally
                    }
                    const cond_expr = self.vpop() orelse "stack[sp - 1]";
                    const should_free = self.isAllocated(cond_expr);
                    defer if (should_free) self.allocator.free(cond_expr);
                    // Count ALL stack references in the condition - they all need to be popped
                    // This handles composite expressions like CV.lt(locals[1], stack[sp-1])
                    const stack_ref_count = self.countStackRefs(cond_expr);
                    const target = block.successors.items[0];
                    if (l.exit_block != null and target == l.exit_block.?) {
                        // Jump to loop exit - evaluate condition FIRST, then pop, then check
                        if (stack_ref_count > 0) {
                            try self.printLine("{{ const _cond = ({s}).toBoolWithCtx(ctx); sp -= {d}; if (_cond) break; }}", .{ cond_expr, stack_ref_count });
                        } else {
                            try self.printLine("if (({s}).toBoolWithCtx(ctx)) break;", .{cond_expr});
                        }
                    } else if (target == l.header_block) {
                        // Jump back to loop header (continue) - only header, NOT latch
                        // For latch_block, we use if-statement to wrap remaining body
                        if (stack_ref_count > 0) {
                            try self.printLine("{{ const _cond = ({s}).toBoolWithCtx(ctx); sp -= {d}; if (_cond) continue; }}", .{ cond_expr, stack_ref_count });
                        } else {
                            try self.printLine("if (({s}).toBoolWithCtx(ctx)) continue;", .{cond_expr});
                        }
                    } else if (target == l.latch_block and self.body_block_depth > 0) {
                        // if_true -> latch means "if (cond) continue;" pattern
                        // Jump to latch when TRUE - use break :body to skip remaining body
                        // Only use break :body if we're inside a body: block
                        if (stack_ref_count > 0) {
                            try self.printLine("{{ const _cond = ({s}).toBoolWithCtx(ctx); sp -= {d}; if (_cond) break :body_{d}; }}", .{ cond_expr, stack_ref_count, self.body_block_depth - 1 });
                        } else {
                            try self.printLine("if (({s}).toBoolWithCtx(ctx)) break :body_{d};", .{ cond_expr, self.body_block_depth - 1 });
                        }
                        self.body_label_used = true;
                    } else {
                        // Target is within loop body (not latch) - this is an if-statement
                        // For if_true, we wrap the FALSE case (fall-through) in if(!cond)
                        // Also handles latch target when not inside body: block (wrap in if instead)
                        const fall_through = if (block.successors.items.len > 1) block.successors.items[1] else block.id + 1;

                        // Always defer target so it gets emitted AFTER the if closes
                        // Target is the "if-true" case, must be outside the if body
                        try self.deferred_blocks.put(self.allocator, target, {});

                        // Evaluate condition FIRST, then pop stack refs, then start if-block
                        if (stack_ref_count > 0) {
                            try self.printLine("const _cond_if_{d} = ({s}).toBoolWithCtx(ctx); sp -= {d}; if (!_cond_if_{d}) {{", .{ self.if_body_depth, cond_expr, stack_ref_count, self.if_body_depth });
                        } else {
                            try self.printLine("if (!({s}).toBoolWithCtx(ctx)) {{", .{cond_expr});
                        }
                        self.pushIndent();
                        self.if_body_depth += 1;
                        try self.if_target_blocks.append(self.allocator, target);
                        // Track fall-through block (should be inside the if)
                        try self.if_fall_through_blocks.append(self.allocator, fall_through);
                    }
                }
                // When loop is null, don't pop from vstack - let emitBlock terminator handle it
            },
            .goto, .goto8, .goto16 => {
                if (loop) |l| {
                    // FIX: Materialize vstack FIRST so real stack matches CFG expectations
                    // This prevents vstack/real-stack mismatch at control flow boundaries
                    // (e.g., DUP in vstack mode doesn't modify real stack, but CFG assumes it does)
                    if (self.vstack.items.len > 0) {
                        try self.materializeVStack();
                        // materializeVStack now updates base_stack_depth internally
                    }
                    if (block.successors.items.len > 0) {
                        const target = block.successors.items[0];
                        if (self.debug_mode) {
                            if (CODEGEN_DEBUG) std.debug.print("[goto] block {d} -> target {d}, loop_header={d}, exit={?}\n", .{ block.id, target, l.header_block, l.exit_block });
                        }
                        if (target == l.header_block) {
                            // goto to header is continue
                            try self.writeLine("continue;");
                            self.block_terminated = true;
                        } else if (target == l.latch_block) {
                            // goto to latch (continue in JS) - skip remaining body and go to latch
                            if (self.body_block_depth > 0) {
                                try self.printLine("break :body_{d};", .{self.body_block_depth - 1});
                                self.body_label_used = true;
                            } else {
                                // Not in body: block (e.g., in header condition)
                                // Use continue which will re-execute condition then latch
                                try self.writeLine("continue;");
                            }
                            self.block_terminated = true;
                        } else if (l.exit_block != null and target == l.exit_block.?) {
                            try self.writeLine("break;");
                            self.block_terminated = true;
                        } else {
                            // Check if target is a parent loop's header
                            for (self.natural_loops) |parent| {
                                if (parent.header_block == target) {
                                    if (self.debug_mode) {
                                        if (CODEGEN_DEBUG) std.debug.print("[goto] found parent loop header, emitting continue\n", .{});
                                    }
                                    try self.writeLine("continue;");
                                    self.block_terminated = true;
                                    break;
                                }
                            }
                        }
                    }
                }
            },

            // Return
            .@"return" => {
                // In block dispatch mode, vstack may be empty but real stack has the value
                const result = self.vpop() orelse "stack[sp - 1]";
                const should_free = self.isAllocated(result);
                defer if (should_free) self.allocator.free(result);
                // Check if returning from stack vs local - only free stack values manually
                // (locals are freed by emitLocalsCleanup)
                const is_stack_value = std.mem.startsWith(u8, result, "stack[");
                // Save return value (dup if ref type), cleanup locals, then return
                // Must dup because cleanup may free the same object (e.g., returning a local)
                try self.printLine("{{ const _ret_cv = {s}; const _ret_val = if (_ret_cv.isRefType()) JSValue.dup(ctx, _ret_cv.toJSValueWithCtx(ctx)) else _ret_cv.toJSValueWithCtx(ctx);", .{result});
                // Free the original stack value to avoid GC leaks (dup created a new reference)
                // Only for stack values - locals are freed by emitLocalsCleanup
                if (is_stack_value) {
                    try self.writeLine("if (_ret_cv.isRefType()) JSValue.free(ctx, _ret_cv.toJSValueWithCtx(ctx));");
                }
                try self.emitVarRefDetach();
                // Free arg_shadow values before returning (they were duped at function entry)
                if (self.has_put_arg) {
                    const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
                    for (0..arg_count) |i| {
                        try self.printLine("if (arg_shadow[{d}].isRefType()) JSValue.free(ctx, arg_shadow[{d}].toJSValueWithCtx(ctx));", .{ i, i });
                    }
                }
                try self.emitLocalsCleanup();
                if (self.dispatch_mode) {
                    // Async functions must wrap return value in Promise.resolve()
                    if (self.func.is_async) {
                        try self.writeLine("return .{ .return_value = JSValue.promiseResolve(ctx, _ret_val) }; }");
                    } else {
                        try self.writeLine("return .{ .return_value = _ret_val }; }");
                    }
                } else {
                    // Async functions must wrap return value in Promise.resolve()
                    if (self.func.is_async) {
                        try self.writeLine("return JSValue.promiseResolve(ctx, _ret_val); }");
                    } else {
                        try self.writeLine("return _ret_val; }");
                    }
                }
                self.block_terminated = true;
                // Only set function_returned if not inside an if-body
                // (if inside an if-body, the else branch may still have code)
                if (self.if_body_depth == 0) {
                    self.function_returned = true;
                }
                if (CODEGEN_DEBUG) std.debug.print("[return] block_terminated={}, function_returned={}, if_body_depth={}\n", .{ self.block_terminated, self.function_returned, self.if_body_depth });
            },
            .return_undef => {
                try self.emitVarRefDetach();
                // Free arg_shadow values before returning (they were duped at function entry)
                if (self.has_put_arg) {
                    const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
                    for (0..arg_count) |i| {
                        try self.printLine("if (arg_shadow[{d}].isRefType()) JSValue.free(ctx, arg_shadow[{d}].toJSValueWithCtx(ctx));", .{ i, i });
                    }
                }
                try self.emitLocalsCleanup();
                if (self.dispatch_mode) {
                    // Async functions must wrap undefined in Promise.resolve()
                    if (self.func.is_async) {
                        try self.writeLine("return .{ .return_value = JSValue.promiseResolve(ctx, zig_runtime.JSValue.UNDEFINED) };");
                    } else {
                        try self.writeLine("return .return_undef;");
                    }
                } else {
                    // Async functions must wrap undefined in Promise.resolve()
                    if (self.func.is_async) {
                        try self.writeLine("return JSValue.promiseResolve(ctx, zig_runtime.JSValue.UNDEFINED);");
                    } else {
                        try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
                    }
                }
                self.block_terminated = true;
                // Only set function_returned if not inside an if-body
                if (self.if_body_depth == 0) {
                    self.function_returned = true;
                }
                if (CODEGEN_DEBUG) std.debug.print("[return_undef] block_terminated={}, function_returned={}, if_body_depth={}\n", .{ self.block_terminated, self.function_returned, self.if_body_depth });
            },

            // Fallback to stack-based for unsupported opcodes
            else => {
                // Check if this should be skipped for native Array.push optimization
                if (self.shouldSkipForNativeArrayPush(block.instructions, idx)) {
                    return; // Skip - tryEmitNativeArrayPush will handle it
                }
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
    /// IMPORTANT: We evaluate all expressions FIRST before incrementing sp,
    /// because expressions may contain relative stack references like "stack[sp-1]"
    /// that become invalid once sp changes.
    fn materializeVStack(self: *Self) !void {
        const count = self.vstack.items.len;
        if (count == 0) return;

        // Count total UNIQUE stack references consumed across ALL expressions
        // This is needed to correctly adjust sp after materialization.
        // CRITICAL: Must deduplicate across all vstack items, not just within each one.
        // E.g., if vstack has [bitAnd(stack[sp-1], 4), bitAnd(stack[sp-1], 4)],
        // both reference stack[sp-1] = 1 unique ref, not 2.
        const total_stack_refs_consumed = self.countStackRefsMulti(self.vstack.items);

        try self.writeLine("{");
        self.pushIndent();
        // First, evaluate all expressions while sp is unchanged
        for (self.vstack.items, 0..) |expr, i| {
            try self.printLine("const vf_{d} = {s};", .{ i, expr });
        }
        // Then assign to stack in FIFO order
        // If expression is a reference to existing storage, we need to dup the reftype to avoid sharing
        // Calculate the net stack position change: +count for outputs, -refs_consumed for inputs
        var assign_offset: i32 = -@as(i32, @intCast(total_stack_refs_consumed));
        for (self.vstack.items, 0..) |expr, i| {
            const is_stack_ref = std.mem.startsWith(u8, expr, "stack[");
            const is_locals_ref = std.mem.startsWith(u8, expr, "locals[");
            // Check if expression IS a direct arg_cache reference (not just contains one)
            // Pattern: "(if (N < argc) arg_cache[N] else CV.UNDEFINED)"
            const is_direct_arg_cache = std.mem.startsWith(u8, expr, "(if (") and
                std.mem.indexOf(u8, expr, " arg_cache[") != null;
            const is_storage_ref = is_stack_ref or is_locals_ref or is_direct_arg_cache;
            if (is_storage_ref) {
                // Storage references need duping to create independent ownership
                // In dispatch_mode, arg_cache_js is not available from block functions, use fallback
                if (self.uses_arg_cache and is_direct_arg_cache and !self.dispatch_mode) {
                    // Use arg_cache_js directly to avoid CV→JSValue reconstruction issues
                    // Extract N from "arg_cache[N]" in the expression
                    if (std.mem.indexOf(u8, expr, "arg_cache[")) |cache_start| {
                        const after_bracket = cache_start + 10; // "arg_cache[" is 10 chars
                        if (std.mem.indexOfPos(u8, expr, after_bracket, "]")) |bracket_end| {
                            const arg_num_str = expr[after_bracket..bracket_end];
                            if (assign_offset >= 0) {
                                try self.printLine("stack[sp + {d}] = if (vf_{d}.isRefType()) CV.fromJSValue(JSValue.dup(ctx, arg_cache_js[{s}])) else vf_{d};", .{ @as(usize, @intCast(assign_offset)), i, arg_num_str, i });
                            } else {
                                try self.printLine("stack[sp - {d}] = if (vf_{d}.isRefType()) CV.fromJSValue(JSValue.dup(ctx, arg_cache_js[{s}])) else vf_{d};", .{ @as(usize, @intCast(-assign_offset)), i, arg_num_str, i });
                            }
                        } else {
                            if (assign_offset >= 0) {
                                try self.printLine("stack[sp + {d}] = if (vf_{d}.isRefType()) CV.fromJSValue(JSValue.dup(ctx, vf_{d}.toJSValueWithCtx(ctx))) else vf_{d};", .{ @as(usize, @intCast(assign_offset)), i, i, i });
                            } else {
                                try self.printLine("stack[sp - {d}] = if (vf_{d}.isRefType()) CV.fromJSValue(JSValue.dup(ctx, vf_{d}.toJSValueWithCtx(ctx))) else vf_{d};", .{ @as(usize, @intCast(-assign_offset)), i, i, i });
                            }
                        }
                    } else {
                        if (assign_offset >= 0) {
                            try self.printLine("stack[sp + {d}] = if (vf_{d}.isRefType()) CV.fromJSValue(JSValue.dup(ctx, vf_{d}.toJSValueWithCtx(ctx))) else vf_{d};", .{ @as(usize, @intCast(assign_offset)), i, i, i });
                        } else {
                            try self.printLine("stack[sp - {d}] = if (vf_{d}.isRefType()) CV.fromJSValue(JSValue.dup(ctx, vf_{d}.toJSValueWithCtx(ctx))) else vf_{d};", .{ @as(usize, @intCast(-assign_offset)), i, i, i });
                        }
                    }
                } else {
                    if (assign_offset >= 0) {
                        try self.printLine("stack[sp + {d}] = if (vf_{d}.isRefType()) CV.fromJSValue(JSValue.dup(ctx, vf_{d}.toJSValueWithCtx(ctx))) else vf_{d};", .{ @as(usize, @intCast(assign_offset)), i, i, i });
                    } else {
                        try self.printLine("stack[sp - {d}] = if (vf_{d}.isRefType()) CV.fromJSValue(JSValue.dup(ctx, vf_{d}.toJSValueWithCtx(ctx))) else vf_{d};", .{ @as(usize, @intCast(-assign_offset)), i, i, i });
                    }
                }
            } else {
                // Expression creates a new value, no dup needed
                if (assign_offset >= 0) {
                    try self.printLine("stack[sp + {d}] = vf_{d};", .{ @as(usize, @intCast(assign_offset)), i });
                } else {
                    try self.printLine("stack[sp - {d}] = vf_{d};", .{ @as(usize, @intCast(-assign_offset)), i });
                }
            }
            assign_offset += 1;
        }
        // Net sp change: +count outputs, -stack_refs_consumed inputs
        const net_sp_change: i32 = @as(i32, @intCast(count)) - @as(i32, @intCast(total_stack_refs_consumed));
        if (net_sp_change > 0) {
            try self.printLine("sp += {d};", .{@as(usize, @intCast(net_sp_change))});
        } else if (net_sp_change < 0) {
            try self.printLine("sp -= {d};", .{@as(usize, @intCast(-net_sp_change))});
        }
        self.popIndent();
        try self.writeLine("}");

        // Free allocated expressions
        for (self.vstack.items) |expr| {
            if (self.isAllocated(expr)) self.allocator.free(expr);
        }
        self.vstack.clearRetainingCapacity();

        // CRITICAL: Update base_stack_depth to reflect values now on real stack
        // vpop already decremented base_stack_depth for each consumed stack value
        // So we just need to add the number of outputs (count), not the net change
        // Formula: base_stack_depth_after = base_stack_depth_after_vpops + count
        //        = (base_stack_depth_before - consumed) + count
        // This correctly handles all cases including when consumed > count
        self.base_stack_depth += @intCast(count);

        // Also reset base_popped_count since stack layout has changed
        self.base_popped_count = 0;
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

            // Stack operations - must handle reference counting for ref types
            .dup => try self.writeLine("{ const v = stack[sp - 1]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }"),
            .drop => {
                try self.writeLine("{ const v = stack[sp - 1]; if (v.isRefType()) JSValue.free(ctx, v.toJSValueWithCtx(ctx)); sp -= 1; }");
            },
            .swap => {
                // Always perform the swap - removing the optimization that skipped swap before put_array_el
                // The optimization was incorrect when vstack-based swaps happened before to_propkey
                try self.writeLine("{ const tmp = stack[sp - 1]; stack[sp - 1] = stack[sp - 2]; stack[sp - 2] = tmp; }");
            },
            // dup1: [a, b] -> [a, a, b] - duplicate first item and insert before second
            .dup1 => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                try self.writeLine("{");
                try self.writeLine("    const a = stack[sp - 2];");
                try self.writeLine("    const b = stack[sp - 1];");
                try self.writeLine("    stack[sp - 1] = if (a.isRefType()) CV.fromJSValue(JSValue.dup(ctx, a.toJSValueWithCtx(ctx))) else a;");
                try self.writeLine("    stack[sp] = b;");
                try self.writeLine("    sp += 1;");
                try self.writeLine("}");
            },
            // dup2: [a, b] -> [a, b, a, b] - duplicate both items
            .dup2 => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                try self.writeLine("{");
                try self.writeLine("    const a = stack[sp - 2];");
                try self.writeLine("    const b = stack[sp - 1];");
                try self.writeLine("    stack[sp] = if (a.isRefType()) CV.fromJSValue(JSValue.dup(ctx, a.toJSValueWithCtx(ctx))) else a;");
                try self.writeLine("    stack[sp + 1] = if (b.isRefType()) CV.fromJSValue(JSValue.dup(ctx, b.toJSValueWithCtx(ctx))) else b;");
                try self.writeLine("    sp += 2;");
                try self.writeLine("}");
            },
            // dup3: [a, b, c] -> [a, b, c, a, b, c] - duplicate all 3 items
            .dup3 => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
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
            // nip: remove 2nd item from stack [a, b] -> [b]
            .nip => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                try self.writeLine("{ const top = stack[sp - 1]; const v = stack[sp - 2]; if (v.isRefType()) JSValue.free(ctx, v.toJSValueWithCtx(ctx)); stack[sp - 2] = top; sp -= 1; }");
            },
            // nip1: remove 2nd item from stack (alias for nip)
            .nip1 => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                try self.writeLine("{ const top = stack[sp - 1]; const v = stack[sp - 2]; if (v.isRefType()) JSValue.free(ctx, v.toJSValueWithCtx(ctx)); stack[sp - 2] = top; sp -= 1; }");
            },
            // swap2: swap top 2 pairs [a,b,c,d] -> [c,d,a,b]
            .swap2 => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                try self.writeLine("{ const a = stack[sp - 4]; const b = stack[sp - 3]; stack[sp - 4] = stack[sp - 2]; stack[sp - 3] = stack[sp - 1]; stack[sp - 2] = a; stack[sp - 1] = b; }");
            },

            // Local variables - must handle reference counting for ref types
            // get_loc: local keeps ref, stack gets new ref -> dup
            // put_loc: stack value moves to local (with pop) -> free old local, ownership transfers
            // set_loc: stack keeps ref, local gets new ref -> dup to local, free old local
            .get_loc => {
                const loc_idx = instr.operand.loc;
                try self.printLine("{{ const v = locals[{d}]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }}", .{loc_idx});
            },
            .put_loc => {
                const loc_idx = instr.operand.loc;
                try self.printLine("{{ const old = locals[{d}]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); locals[{d}] = stack[sp - 1]; sp -= 1; }}", .{ loc_idx, loc_idx });
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0 and loc_idx < self.func.var_count) {
                    try self.printLine("{s}[{d}] = CV.toJSValuePtr(&locals[{d}]);", .{ self.getLocalsJsvExpr(), loc_idx, loc_idx });
                }
            },
            .set_loc => {
                const loc_idx = instr.operand.loc;
                try self.printLine("{{ const old = locals[{d}]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); const v = stack[sp - 1]; locals[{d}] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; }}", .{ loc_idx, loc_idx });
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0 and loc_idx < self.func.var_count) {
                    try self.printLine("{s}[{d}] = CV.toJSValuePtr(&locals[{d}]);", .{ self.getLocalsJsvExpr(), loc_idx, loc_idx });
                }
            },
            .get_loc0 => try self.writeLine("{ const v = locals[0]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }"),
            .get_loc1 => try self.writeLine("{ const v = locals[1]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }"),
            .get_loc2 => try self.writeLine("{ const v = locals[2]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }"),
            .get_loc3 => try self.writeLine("{ const v = locals[3]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }"),
            .put_loc0 => {
                try self.writeLine("{ const old = locals[0]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); locals[0] = stack[sp - 1]; sp -= 1; }");
                if (self.has_fclosure and self.func.var_count > 0) {
                    try self.printLine("{s}[0] = CV.toJSValuePtr(&locals[0]);", .{self.getLocalsJsvExpr()});
                }
            },
            .put_loc1 => {
                try self.writeLine("{ const old = locals[1]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); locals[1] = stack[sp - 1]; sp -= 1; }");
                if (self.has_fclosure and self.func.var_count > 0) {
                    try self.printLine("{s}[1] = CV.toJSValuePtr(&locals[1]);", .{self.getLocalsJsvExpr()});
                }
            },
            .put_loc2 => {
                try self.writeLine("{ const old = locals[2]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); locals[2] = stack[sp - 1]; sp -= 1; }");
                if (self.has_fclosure and self.func.var_count > 0) {
                    try self.printLine("{s}[2] = CV.toJSValuePtr(&locals[2]);", .{self.getLocalsJsvExpr()});
                }
            },
            .put_loc3 => {
                try self.writeLine("{ const old = locals[3]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); locals[3] = stack[sp - 1]; sp -= 1; }");
                if (self.has_fclosure and self.func.var_count > 0) {
                    try self.printLine("{s}[3] = CV.toJSValuePtr(&locals[3]);", .{self.getLocalsJsvExpr()});
                }
            },

            // Arguments - use cached values if available (prevents repeated dup leaks in loops)
            // Use arg_cache_js directly to avoid CV→JSValue reconstruction issues
            // In dispatch_mode, use argv directly as arg_cache_js is not accessible from block functions
            // CRITICAL: When has_put_arg is true, must use arg_shadow to reflect default parameter values
            .get_arg => {
                const arg_idx = instr.operand.arg;
                if (self.has_put_arg) {
                    // Argument may be reassigned - must use arg_shadow, matches get_arg0-3
                    try self.printLine("{{ const v = arg_shadow[{d}]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }}", .{arg_idx});
                } else if (self.uses_arg_cache and arg_idx <= self.max_loop_arg_idx and !self.dispatch_mode) {
                    // Dup from arg_cache_js (original JSValue) to avoid CV reconstruction
                    try self.printLine("stack[sp] = if ({d} < argc) CV.fromJSValue(JSValue.dup(ctx, arg_cache_js[{d}])) else CV.UNDEFINED; sp += 1;", .{ arg_idx, arg_idx });
                } else {
                    try self.printLine("stack[sp] = if ({d} < argc) CV.fromJSValue(JSValue.dup(ctx, argv[{d}])) else CV.UNDEFINED; sp += 1;", .{ arg_idx, arg_idx });
                }
            },
            .get_arg0 => {
                if (self.has_put_arg) {
                    // Use arg_shadow for functions that reassign parameters
                    try self.writeLine("{ const v = arg_shadow[0]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }");
                } else if (self.uses_arg_cache and 0 <= self.max_loop_arg_idx and !self.dispatch_mode) {
                    // Dup from arg_cache_js (original JSValue) to avoid CV reconstruction
                    try self.writeLine("stack[sp] = if (0 < argc) CV.fromJSValue(JSValue.dup(ctx, arg_cache_js[0])) else CV.UNDEFINED; sp += 1;");
                } else {
                    try self.writeLine("stack[sp] = if (0 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[0])) else CV.UNDEFINED; sp += 1;");
                }
            },
            .get_arg1 => {
                if (self.has_put_arg) {
                    try self.writeLine("{ const v = arg_shadow[1]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }");
                } else if (self.uses_arg_cache and 1 <= self.max_loop_arg_idx and !self.dispatch_mode) {
                    // Dup from arg_cache_js (original JSValue) to avoid CV reconstruction
                    try self.writeLine("stack[sp] = if (1 < argc) CV.fromJSValue(JSValue.dup(ctx, arg_cache_js[1])) else CV.UNDEFINED; sp += 1;");
                } else {
                    try self.writeLine("stack[sp] = if (1 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[1])) else CV.UNDEFINED; sp += 1;");
                }
            },
            .get_arg2 => {
                if (self.has_put_arg) {
                    try self.writeLine("{ const v = arg_shadow[2]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }");
                } else if (self.uses_arg_cache and 2 <= self.max_loop_arg_idx and !self.dispatch_mode) {
                    // Dup from arg_cache_js (original JSValue) to avoid CV reconstruction
                    try self.writeLine("stack[sp] = if (2 < argc) CV.fromJSValue(JSValue.dup(ctx, arg_cache_js[2])) else CV.UNDEFINED; sp += 1;");
                } else {
                    try self.writeLine("stack[sp] = if (2 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[2])) else CV.UNDEFINED; sp += 1;");
                }
            },
            .get_arg3 => {
                if (self.has_put_arg) {
                    try self.writeLine("{ const v = arg_shadow[3]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }");
                } else if (self.uses_arg_cache and 3 <= self.max_loop_arg_idx and !self.dispatch_mode) {
                    // Dup from arg_cache_js (original JSValue) to avoid CV reconstruction
                    try self.writeLine("stack[sp] = if (3 < argc) CV.fromJSValue(JSValue.dup(ctx, arg_cache_js[3])) else CV.UNDEFINED; sp += 1;");
                } else {
                    try self.writeLine("stack[sp] = if (3 < argc) CV.fromJSValue(JSValue.dup(ctx, argv[3])) else CV.UNDEFINED; sp += 1;");
                }
            },

            // Arithmetic - always use CompressedValue (8-byte NaN-boxed)
            .add => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.addWithCtx(ctx, a, b); sp -= 1; }"),
            .sub => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.sub(a, b); sp -= 1; }"),
            .mul => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.mul(a, b); sp -= 1; }"),
            .div => try self.writeLine("{ if (comptime @import(\"builtin\").cpu.arch == .wasm32) { CV.divOnStack(stack[0..].ptr, sp); } else { const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.div(a, b); } sp -= 1; }"),
            .mod => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.mod(a, b); sp -= 1; }"),
            .neg => try self.writeLine("{ const a = stack[sp-1]; stack[sp-1] = CV.sub(CV.newInt(0), a); }"),

            // Comparisons - always use CompressedValue
            .lt => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.lt(a, b); sp -= 1; }"),
            .lte => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = if (CV.gt(a, b).toBool()) CV.FALSE else CV.TRUE; sp -= 1; }"),
            .gt => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.gt(a, b); sp -= 1; }"),
            .gte => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = if (CV.lt(a, b).toBool()) CV.FALSE else CV.TRUE; sp -= 1; }"),
            // Use context-aware equality for proper string/object comparison
            .eq => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.eqWithCtx(ctx, a, b); sp -= 1; }"),
            .neq => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.neqWithCtx(ctx, a, b); sp -= 1; }"),

            // Bitwise - all use CV inline (no refcount)
            .@"and" => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.bitAnd(a, b); sp -= 1; }"),
            .@"or" => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.bitOr(a, b); sp -= 1; }"),
            .xor => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.bitXor(a, b); sp -= 1; }"),
            .not => try self.writeLine("{ const a = stack[sp-1]; stack[sp-1] = CV.bitNot(a); }"),
            .shl => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.shl(a, b); sp -= 1; }"),
            .sar => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.sar(a, b); sp -= 1; }"),
            .shr => try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = CV.ushr(a, b); sp -= 1; }"),

            // Control flow (jumps handled by block terminators)
            .if_false, .if_true, .if_false8, .if_true8, .goto, .goto8, .goto16 => {
                // These are handled by block successor logic, not individual instructions
            },

            // Return - control terminates (CV→JSValue at exit)
            .@"return" => {
                // Save return value (dup if ref type), cleanup locals, then return
                // Must dup because cleanup may free the same object (e.g., returning a local)
                try self.writeLine("{ const _ret_cv = stack[sp - 1]; const _ret_val = if (_ret_cv.isRefType()) JSValue.dup(ctx, _ret_cv.toJSValueWithCtx(ctx)) else _ret_cv.toJSValueWithCtx(ctx);");
                // Free the original stack value to avoid GC leaks (dup created a new reference)
                try self.writeLine("if (_ret_cv.isRefType()) JSValue.free(ctx, _ret_cv.toJSValueWithCtx(ctx));");
                try self.emitVarRefDetach();
                // Free arg_shadow values before returning (they were duped at function entry)
                if (self.has_put_arg) {
                    const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
                    for (0..arg_count) |i| {
                        try self.printLine("if (arg_shadow[{d}].isRefType()) JSValue.free(ctx, arg_shadow[{d}].toJSValueWithCtx(ctx));", .{ i, i });
                    }
                }
                try self.emitLocalsCleanup();
                if (self.dispatch_mode) {
                    try self.writeLine("return .{ .return_value = _ret_val }; }");
                } else {
                    try self.writeLine("return _ret_val; }");
                }
                return false; // Control terminates
            },
            .return_undef => {
                try self.emitVarRefDetach();
                // Free arg_shadow values before returning (they were duped at function entry)
                if (self.has_put_arg) {
                    const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
                    for (0..arg_count) |i| {
                        try self.printLine("if (arg_shadow[{d}].isRefType()) JSValue.free(ctx, arg_shadow[{d}].toJSValueWithCtx(ctx));", .{ i, i });
                    }
                }
                try self.emitLocalsCleanup();
                if (self.dispatch_mode) {
                    try self.writeLine("return .return_undef;");
                } else {
                    try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
                }
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
            // Uses safe version with bounds checking via closure_var_count
            .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3 => {
                // Get bytecode index from opcode (0-3)
                const bytecode_idx: u16 = switch (instr.opcode) {
                    .get_var_ref0 => 0,
                    .get_var_ref1 => 1,
                    .get_var_ref2 => 2,
                    .get_var_ref3 => 3,
                    else => unreachable,
                };
                // Use bytecode index directly - var_refs is indexed by bytecode index
                // Note: var_refs contains ALL closure variables for this function AND its
                // descendants, so we must use the original bytecode index, not a compressed position.
                const var_pos = bytecode_idx;
                // Check for self-recursive call optimization first
                if (self.func.is_self_recursive and self.func.self_ref_var_idx >= 0 and bytecode_idx == @as(u16, @intCast(self.func.self_ref_var_idx))) {
                    // Self-reference: Check if this leads to a self-call by looking ahead
                    const is_self_call = self.isFollowedBySelfCall(block_instrs, instr_idx);
                    if (is_self_call) {
                        try self.printLine("// get_var_ref{d}: self-reference for self-call", .{bytecode_idx});
                        self.pending_self_call = true;
                    } else {
                        // Not a self-call - use safe runtime lookup with bounds checking
                        try self.printLine("stack[sp] = CV.fromJSValue(zig_runtime.getClosureVarSafe(ctx, var_refs, {d}, {d})); sp += 1;", .{ var_pos, self.func.closure_var_count });
                    }
                } else {
                    // Use safe runtime lookup with bounds checking
                    try self.printLine("stack[sp] = CV.fromJSValue(zig_runtime.getClosureVarSafe(ctx, var_refs, {d}, {d})); sp += 1;", .{ var_pos, self.func.closure_var_count });
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
                    // Free old CV if it's a ref type since we're replacing it
                    // Native shape properties (use cached native values)
                    if (std.mem.eql(u8, prop_name, "kind")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetKind(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "flags")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetFlags(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "pos")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetPos(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "end")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetEnd(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "parent")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetParent(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "length")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = zig_runtime.nativeGetLengthCV(ctx, obj); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    // Cached atom properties (skip string hashing, ~10 cycles faster)
                    } else if (std.mem.eql(u8, prop_name, "symbol")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetSymbol(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "escapedName")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetEscapedName(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "declarations")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetDeclarations(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "valueDeclaration")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetValueDeclaration(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "members")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetMembers(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "properties")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetProperties(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "target")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetTarget(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "constraint")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetConstraint(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "modifiers")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetModifiers(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "name")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetName(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "text")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetText(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "type")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetType(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "checker")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetChecker(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "typeArguments")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetTypeArguments(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else if (std.mem.eql(u8, prop_name, "arguments")) {
                        try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = CV.fromJSValue(zig_runtime.nativeGetArguments(ctx, obj)); if (cv.isRefType()) JSValue.free(ctx, obj); }");
                    } else {
                        // Standard property access via QuickJS - convert CV <-> JSValue
                        // Free old CV if it's a ref type since we're replacing it
                        const escaped_prop = escapeZigString(self.allocator, prop_name) catch prop_name;
                        defer if (escaped_prop.ptr != prop_name.ptr) self.allocator.free(escaped_prop);
                        try self.writeLine("{");
                        self.pushIndent();
                        try self.writeLine("const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx);");
                        try self.printLine("const _gf = JSValue.getPropertyStr(ctx, obj, \"{s}\");", .{escaped_prop});
                        try self.writeLine("if (_gf.isException()) {");
                        self.pushIndent();
                        try self.emitReturnException();
                        self.popIndent();
                        try self.writeLine("}");
                        try self.writeLine("stack[sp-1] = CV.fromJSValue(_gf); if (cv.isRefType()) JSValue.free(ctx, obj);");
                        self.popIndent();
                        try self.writeLine("}");
                    }
                    // NOTE: Do NOT manipulate vstack here - emitInstruction is stack-based only.
                    // The old vpop/vpush pattern polluted vstack with stale references.
                    // get_field replaces stack[sp-1] in place, sp doesn't change.
                } else {
                    try self.writeLine("// get_field: atom index out of range");
                    try self.emitReturnValue("JSValue.throwTypeError(ctx, \"Invalid property access\")");
                    return false; // Control terminates
                }
            },

            // get_field2: pop obj, push obj, obj.prop (keeps object on stack)
            // Uses native shape access for known AST properties (kind, flags, pos, end, parent)
            .get_field2 => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |prop_name| {
                    // Native shape properties (use cached native values)
                    if (std.mem.eql(u8, prop_name, "kind")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetKind(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "flags")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetFlags(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "pos")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetPos(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "end")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetEnd(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "parent")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetParent(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "length")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = zig_runtime.nativeGetLengthCV(ctx, obj); sp += 1; }");
                    // Cached atom properties (skip string hashing, ~10 cycles faster)
                    } else if (std.mem.eql(u8, prop_name, "symbol")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetSymbol(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "escapedName")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetEscapedName(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "declarations")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetDeclarations(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "valueDeclaration")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetValueDeclaration(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "members")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetMembers(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "properties")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetProperties(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "target")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetTarget(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "constraint")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetConstraint(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "modifiers")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetModifiers(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "name")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetName(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "text")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetText(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "type")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetType(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "checker")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetChecker(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "typeArguments")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetTypeArguments(ctx, obj)); sp += 1; }");
                    } else if (std.mem.eql(u8, prop_name, "arguments")) {
                        try self.writeLine("{ const obj = stack[sp-1].toJSValueWithCtx(ctx); stack[sp] = CV.fromJSValue(zig_runtime.nativeGetArguments(ctx, obj)); sp += 1; }");
                    } else {
                        // Standard property access via QuickJS - convert CV <-> JSValue
                        const escaped_prop = escapeZigString(self.allocator, prop_name) catch prop_name;
                        defer if (escaped_prop.ptr != prop_name.ptr) self.allocator.free(escaped_prop);
                        try self.writeLine("{");
                        self.pushIndent();
                        try self.writeLine("const obj = stack[sp-1].toJSValueWithCtx(ctx);");
                        try self.printLine("const _gf = JSValue.getPropertyStr(ctx, obj, \"{s}\");", .{escaped_prop});
                        try self.emitInlineExceptionReturn("_gf");
                        try self.writeLine("stack[sp] = CV.fromJSValue(_gf); sp += 1;");
                        self.popIndent();
                        try self.writeLine("}");
                    }
                    // NOTE: Do NOT call vpush here - emitInstruction is stack-based only.
                    // Calling vpush pollutes vstack with stale references that cause
                    // materializeVStack to generate incorrect code.
                    // The stack is self-tracking via sp.
                } else {
                    try self.writeLine("// get_field2: atom index out of range");
                    try self.emitReturnValue("JSValue.throwTypeError(ctx, \"Invalid property access\")");
                    return false; // Control terminates
                }
            },

            // put_field: pop val, pop obj, obj.prop = val
            .put_field => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |prop_name| {
                    const escaped_prop = escapeZigString(self.allocator, prop_name) catch prop_name;
                    defer if (escaped_prop.ptr != prop_name.ptr) self.allocator.free(escaped_prop);
                    // CRITICAL: Dup value before passing to setPropertyStr - JS_SetPropertyStr consumes the value
                    try self.writeLine("{");
                    try self.writeLine("  const val = JSValue.dup(ctx, stack[sp-1].toJSValueWithCtx(ctx));");
                    try self.writeLine("  const obj = stack[sp-2].toJSValueWithCtx(ctx);");
                    if (std.mem.eql(u8, prop_name, "flags")) {
                        // Use nativeSetFlags to update both JS property AND native shape cache
                        try self.writeLine("  zig_runtime.nativeSetFlags(ctx, obj, val);");
                    } else {
                        try self.printLine("  _ = JSValue.setPropertyStr(ctx, obj, \"{s}\", val);", .{escaped_prop});
                    }
                    try self.writeLine("  sp -= 2;");
                    try self.writeLine("}");
                    // Sync vstack: pops 2 (val, obj)
                    if (self.vpop()) |e| if (self.isAllocated(e)) self.allocator.free(e);
                    if (self.vpop()) |e| if (self.isAllocated(e)) self.allocator.free(e);
                } else {
                    try self.writeLine("// put_field: atom index out of range");
                    try self.emitReturnValue("JSValue.throwTypeError(ctx, \"Invalid property access\")");
                    return false; // Control terminates
                }
            },

            // get_array_el: pop idx, pop arr, push arr[idx]
            // Use getPropertyUint32 for efficient integer indexing
            // arr and idx were dup'd on push (get_arg dups), so we must free them
            // (matches old C codegen: FROZEN_FREE(ctx, arr); FROZEN_FREE(ctx, idx);)
            .get_array_el => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                try self.writeLine("{ const idx = stack[sp-1]; const arr = stack[sp-2];");
                try self.writeLine("  const arr_jsv = arr.toJSValueWithCtx(ctx);");
                try self.writeLine("  const idx_jsv = idx.toJSValueWithCtx(ctx);");
                // Use getPropertyValue which handles both integer and string keys
                try self.writeLine("  const result = CV.fromJSValue(JSValue.getPropertyValue(ctx, arr_jsv, JSValue.dup(ctx, idx_jsv)));");
                try self.writeLine("  if (arr.isRefType()) JSValue.free(ctx, arr_jsv);");
                try self.writeLine("  if (idx.isRefType()) JSValue.free(ctx, idx_jsv);");
                try self.writeLine("  stack[sp-2] = result; sp -= 1; }");
            },

            // get_array_el2: pop idx, pop arr, push arr, push arr[idx]
            // Stack: [arr, idx] → [arr, value] - arr stays, idx is consumed and must be freed
            .get_array_el2 => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                try self.writeLine("{ const idx = stack[sp-1]; const arr = stack[sp-2];");
                try self.writeLine("  const idx_jsv = idx.toJSValueWithCtx(ctx);");
                // Use getPropertyValue which handles both integer and string keys
                try self.writeLine("  const result = CV.fromJSValue(JSValue.getPropertyValue(ctx, arr.toJSValueWithCtx(ctx), JSValue.dup(ctx, idx_jsv)));");
                try self.writeLine("  if (idx.isRefType()) JSValue.free(ctx, idx_jsv);");
                try self.writeLine("  stack[sp-1] = result; }");
            },

            // put_array_el: pop val, pop idx, pop arr, arr[idx] = val
            // Stack order is [arr, idx, val] - arr at bottom, then index, then value on top
            // Use setPropertyUint32 for integer indices (properly updates array length)
            // Fall back to atom-based approach for string keys
            .put_array_el => {
                // CRITICAL: Materialize vstack first, or values won't be on real stack!
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                try self.writeLine("{ const val = stack[sp-1]; const idx = stack[sp-2]; const arr = stack[sp-3];");
                try self.writeLine("  const arr_jsv = arr.toJSValueWithCtx(ctx);");
                // CRITICAL: Dup value before passing to setPropertyUint32/JS_SetProperty - they consume the value
                try self.writeLine("  const val_jsv = JSValue.dup(ctx, val.toJSValueWithCtx(ctx));");
                // Use setPropertyUint32 for integer indices (properly updates array length)
                // Fall back to atom-based approach for non-integer keys
                try self.writeLine("  if (idx.isInt()) {");
                try self.writeLine("    const idx_u32: u32 = @intCast(@max(0, idx.getInt()));");
                try self.writeLine("    _ = JSValue.setPropertyUint32(ctx, arr_jsv, idx_u32, val_jsv);");
                try self.writeLine("  } else {");
                try self.writeLine("    const idx_jsv = idx.toJSValueWithCtx(ctx);");
                try self.writeLine("    const atom = zig_runtime.quickjs.JS_ValueToAtom(ctx, idx_jsv);");
                try self.writeLine("    _ = zig_runtime.quickjs.JS_SetProperty(ctx, arr_jsv, atom, val_jsv);");
                try self.writeLine("    zig_runtime.quickjs.JS_FreeAtom(ctx, atom);");
                try self.writeLine("    if (idx.isRefType()) JSValue.free(ctx, idx_jsv);");
                try self.writeLine("  }");
                try self.writeLine("  if (arr.isRefType()) JSValue.free(ctx, arr_jsv);");
                try self.writeLine("  sp -= 3; }");
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
                // Use native Math if: (a) not in force_stack_mode (normal path), OR
                // (b) native_math_skipped is set (shouldSkipForNativeMath already skipped
                //     get_var/get_field2 before force_stack_mode was triggered by a later opcode)
                if ((self.native_math_skipped or !self.force_stack_mode) and try self.tryEmitNativeMathCall(argc, block_instrs, instr_idx)) {
                    self.native_math_skipped = false;
                    // Native math emitted - sync vstack
                    // Math.method(arg) replaces stack[sp-1] in-place
                    // Pop the arg from vstack and push result reference
                    self.vpopAndFree(); // pop the argument
                    try self.vpush("stack[sp - 1]"); // push result reference
                } else if (try self.tryEmitNativeArrayPush(argc, block_instrs, instr_idx)) {
                    // Native Array.push emitted - handled internally
                    // NOTE: Array.push works in force_stack_mode because stack state is the same:
                    // [arr, arr.push, val...] regardless of whether get_field2 was expr-mode or stack-mode
                } else if (try self.tryEmitNativeCharCodeAt(argc, block_instrs, instr_idx)) {
                    // Native String.charCodeAt emitted - handled internally
                    // NOTE: charCodeAt works in force_stack_mode - stack has [str, str.charCodeAt, idx]
                } else if (try self.tryEmitNativeStringSlice(argc, block_instrs, instr_idx)) {
                    // Native String.slice/substring emitted - handled internally
                    // NOTE: slice/substring works in force_stack_mode - stack has correct layout
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
                if (self.dispatch_mode) {
                    try self.printLine("{{ const v = locals[{d}]; if (v.isUninitialized()) return .{{ .return_value = JSValue.throwReferenceError(ctx, \"Cannot access before initialization\") }}; stack[sp] = CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))); sp += 1; }}", .{loc_idx});
                } else {
                    try self.printLine("{{ const v = locals[{d}]; if (v.isUninitialized()) return JSValue.throwReferenceError(ctx, \"Cannot access before initialization\"); stack[sp] = CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))); sp += 1; }}", .{loc_idx});
                }
                // Track result on real stack via base_stack_depth (NOT vstack)
                // Using vpush("stack[sp-1]") causes double-tracking: both vstack AND real stack
                // This leads to wrong stack references when subsequent vpops use base_stack_depth
                self.base_stack_depth += 1;
                self.base_popped_count = 0; // Reset - fresh value at stack top
            },

            // get_var: get variable by name from scope
            .get_var => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |var_name| {
                    const escaped_name = escapeZigString(self.allocator, var_name) catch var_name;
                    defer if (escaped_name.ptr != var_name.ptr) self.allocator.free(escaped_name);
                    try self.printLine("stack[sp] = CV.fromJSValue(JSValue.getGlobal(ctx, \"{s}\")); sp += 1;", .{escaped_name});
                    // Track result on real stack via base_stack_depth (NOT vstack)
                    // Using vpush("stack[sp-1]") causes double-tracking leading to wrong references
                    self.base_stack_depth += 1;
                    self.base_popped_count = 0; // Reset - fresh value at stack top
                } else {
                    try self.emitReturnValue("JSValue.throwReferenceError(ctx, \"Variable not found\")");
                    self.block_terminated = true;
                    return false; // Control terminates
                }
            },

            // push_atom_value: push atom as value (usually for property names/strings)
            .push_atom_value => {
                const atom_idx = instr.operand.atom;
                if (CODEGEN_DEBUG) std.debug.print("[push_atom_value] func={s} atom_idx={d} JS_ATOM_END={d} atom_strings.len={d}\n", .{ self.func.name, atom_idx, JS_ATOM_END, self.func.atom_strings.len });
                if (self.getAtomString(atom_idx)) |str_val| {
                    if (CODEGEN_DEBUG) std.debug.print("[push_atom_value]   -> found string: \"{s}\"\n", .{str_val});
                    const escaped_str = escapeZigString(self.allocator, str_val) catch str_val;
                    defer if (escaped_str.ptr != str_val.ptr) self.allocator.free(escaped_str);
                    try self.printLine("stack[sp] = CV.fromJSValue(JSValue.newString(ctx, \"{s}\")); sp += 1;", .{escaped_str});
                } else {
                    if (CODEGEN_DEBUG) std.debug.print("[push_atom_value]   -> NOT FOUND, emitting CV.UNDEFINED\n", .{});
                    try self.writeLine("stack[sp] = CV.UNDEFINED; sp += 1;");
                }
            },

            // set_loc_uninitialized: set local to uninitialized state (for TDZ)
            .set_loc_uninitialized => {
                const loc_idx = instr.operand.loc;
                try self.printLine("locals[{d}] = CV.UNINITIALIZED;", .{loc_idx});
            },

            // get_var_ref_check: get closure var with TDZ check
            // Uses safe version with bounds checking via closure_var_count
            .get_var_ref_check => {
                const bytecode_idx = instr.operand.var_ref;
                // Use bytecode index directly - var_refs is indexed by bytecode index
                try self.printLine("stack[sp] = CV.fromJSValue(zig_runtime.getClosureVarCheckSafe(ctx, var_refs, {d}, {d})); sp += 1;", .{ bytecode_idx, self.func.closure_var_count });
                if (self.dispatch_mode) {
                    try self.writeLine("if (stack[sp-1].isException()) return .{ .return_value = stack[sp-1].toJSValueWithCtx(ctx) };");
                } else {
                    try self.writeLine("if (stack[sp-1].isException()) return stack[sp-1].toJSValueWithCtx(ctx);");
                }
            },

            // get_var_ref: get closure var by index (generic version)
            // Uses safe version with bounds checking via closure_var_count
            .get_var_ref => {
                const bytecode_idx = instr.operand.var_ref;
                // Use bytecode index directly - var_refs is indexed by bytecode index
                try self.printLine("stack[sp] = CV.fromJSValue(zig_runtime.getClosureVarSafe(ctx, var_refs, {d}, {d})); sp += 1;", .{ bytecode_idx, self.func.closure_var_count });
            },

            // put_var_ref0-3: set closure variable
            // Uses safe version with bounds checking via closure_var_count
            .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3 => {
                const bytecode_idx: u16 = switch (instr.opcode) {
                    .put_var_ref0 => 0,
                    .put_var_ref1 => 1,
                    .put_var_ref2 => 2,
                    .put_var_ref3 => 3,
                    else => unreachable,
                };
                // Use bytecode index directly - var_refs is indexed by bytecode index
                try self.printLine("sp -= 1; zig_runtime.setClosureVarSafe(ctx, var_refs, {d}, {d}, stack[sp].toJSValueWithCtx(ctx));", .{ bytecode_idx, self.func.closure_var_count });
            },

            // put_var_ref: set closure var by index (generic version)
            // Uses safe version with bounds checking via closure_var_count
            .put_var_ref => {
                const bytecode_idx = instr.operand.var_ref;
                // Use bytecode index directly - var_refs is indexed by bytecode index
                try self.printLine("sp -= 1; zig_runtime.setClosureVarSafe(ctx, var_refs, {d}, {d}, stack[sp].toJSValueWithCtx(ctx));", .{ bytecode_idx, self.func.closure_var_count });
            },

            // put_var_ref_check: set closure var with TDZ check
            // Uses safe version with bounds checking via closure_var_count
            .put_var_ref_check => {
                const bytecode_idx = instr.operand.var_ref;
                // Use bytecode index directly - var_refs is indexed by bytecode index
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("sp -= 1;");
                try self.printLine("const err = zig_runtime.setClosureVarCheckSafe(ctx, var_refs, {d}, {d}, stack[sp].toJSValueWithCtx(ctx));", .{ bytecode_idx, self.func.closure_var_count });
                try self.writeLine("if (err) {");
                self.pushIndent();
                try self.emitReturnException();
                self.popIndent();
                try self.writeLine("}");
                self.popIndent();
                try self.writeLine("}");
            },

            // set_var_ref0-3: set closure var but KEEP value on stack (for expressions like ++x)
            // Uses safe version with bounds checking via closure_var_count
            // Unlike put_var_ref which pops, set_var_ref keeps the value on stack
            .set_var_ref0, .set_var_ref1, .set_var_ref2, .set_var_ref3 => {
                const bytecode_idx: u16 = switch (instr.opcode) {
                    .set_var_ref0 => 0,
                    .set_var_ref1 => 1,
                    .set_var_ref2 => 2,
                    .set_var_ref3 => 3,
                    else => unreachable,
                };
                // Use bytecode index directly - var_refs is indexed by bytecode index
                // Must dup the value since it stays on stack AND goes to var_ref
                try self.printLine("zig_runtime.setClosureVarDupSafe(ctx, var_refs, {d}, {d}, stack[sp - 1].toJSValueWithCtx(ctx));", .{ bytecode_idx, self.func.closure_var_count });
            },

            // set_var_ref: generic set closure var (keeps value on stack)
            // Uses safe version with bounds checking via closure_var_count
            .set_var_ref => {
                const bytecode_idx = instr.operand.var_ref;
                // Use bytecode index directly - var_refs is indexed by bytecode index
                // Must dup the value since it stays on stack AND goes to var_ref
                try self.printLine("zig_runtime.setClosureVarDupSafe(ctx, var_refs, {d}, {d}, stack[sp - 1].toJSValueWithCtx(ctx));", .{ bytecode_idx, self.func.closure_var_count });
            },

            // put_var_ref_check_init: initialize closure var (for TDZ)
            // Uses safe version with bounds checking via closure_var_count
            .put_var_ref_check_init => {
                const bytecode_idx = instr.operand.var_ref;
                // Use bytecode index directly - var_refs is indexed by bytecode index
                try self.printLine("sp -= 1; zig_runtime.setClosureVarSafe(ctx, var_refs, {d}, {d}, stack[sp].toJSValueWithCtx(ctx));", .{ bytecode_idx, self.func.closure_var_count });
            },

            // push_this: push current 'this' value
            // For functions with explicit "use strict" directive: keep this as-is (undefined/null preserved)
            // For regular functions: coerce undefined/null to globalThis (Node.js non-strict mode behavior)
            // Note: We use has_use_strict (explicit directive) instead of is_strict_mode which QuickJS-ng
            // incorrectly sets to true for ANY ES6 features in the file.
            // Note 2: Bundlers like Bun strip "use strict" directives, so this flag may not be set
            // even for code that was originally strict mode. In practice, bundled CJS output behaves
            // as non-strict, so coercing `this` to globalThis is correct for most cases.
            .push_this => {
                if (self.func.has_use_strict) {
                    // Strict mode: keep this as-is (don't coerce undefined/null)
                    try self.writeLine("stack[sp] = CV.fromJSValue(JSValue.dup(ctx, this_val)); sp += 1;");
                } else {
                    // Non-strict: coerce undefined/null to globalThis (Node.js-compatible behavior)
                    try self.writeLine("{ const _this = if (this_val.isUndefined() or this_val.isNull()) JSValue.getGlobal(ctx, \"globalThis\") else JSValue.dup(ctx, this_val); stack[sp] = CV.fromJSValue(_this); sp += 1; }");
                }
            },

            // define_field: define a field on object (for object literals)
            .define_field => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |field_name| {
                    const escaped_field = escapeZigString(self.allocator, field_name) catch field_name;
                    defer if (escaped_field.ptr != field_name.ptr) self.allocator.free(escaped_field);
                    try self.writeLine("{");
                    // CRITICAL: Dup value before passing to definePropertyStr - it consumes the value
                    try self.writeLine("  const val = JSValue.dup(ctx, stack[sp-1].toJSValueWithCtx(ctx));");
                    try self.writeLine("  const obj = stack[sp-2].toJSValueWithCtx(ctx);");
                    try self.printLine("  _ = JSValue.definePropertyStr(ctx, obj, \"{s}\", val);", .{escaped_field});
                    try self.writeLine("  sp -= 1;");
                    try self.writeLine("}");
                    // Sync vstack: pops 2 (val, obj), pushes 1 (obj for chaining)
                    if (self.vpop()) |e| if (self.isAllocated(e)) self.allocator.free(e);
                    if (self.vpop()) |e| if (self.isAllocated(e)) self.allocator.free(e);
                    try self.vpush("stack[sp - 1]");
                } else {
                    try self.emitReturnValue("JSValue.throwTypeError(ctx, \"Invalid field name\")");
                    return false; // Control terminates
                }
            },

            // fclosure: create closure (8-bit index variant)
            .fclosure8 => {
                const func_idx = instr.operand.const_idx;
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

                const var_count = self.func.var_count;
                const arg_count = self.func.arg_count;

                // Use V2 closure creation with shared var_refs for function hoisting support
                if (self.has_fclosure and var_count > 0) {
                    // Sync _locals_jsv with current locals before closure creation
                    // In dispatch_mode, access via bctx pointers
                    const locals_jsv_expr = self.getLocalsJsvExpr();
                    try self.printLine("for (0..{d}) |_i| {{ {s}[_i] = CV.toJSValuePtr(&locals[_i]); }}", .{ var_count, locals_jsv_expr });

                    const var_ref_ptr = self.getVarRefListPtr();
                    const locals_jsv_ptr = self.getLocalsJsvPtr();
                    if (self.has_put_arg and arg_count > 0) {
                        try self.printLine("var _args_js: [{d}]JSValue = undefined;", .{arg_count});
                        try self.printLine("for (0..{d}) |_i| {{ _args_js[_i] = CV.toJSValuePtr(&arg_shadow[_i]); }}", .{arg_count});
                        try self.printLine("const _closure = JSValue.createClosureV2(ctx, _bfunc, var_refs, {s}, {s}, {d}, &_args_js);", .{ var_ref_ptr, locals_jsv_ptr, var_count });
                    } else {
                        try self.printLine("const _closure = JSValue.createClosureV2(ctx, _bfunc, var_refs, {s}, {s}, {d}, argv[0..@intCast(argc)]);", .{ var_ref_ptr, locals_jsv_ptr, var_count });
                    }
                } else {
                    // Fallback to original closure creation (no locals or not tracking)
                    if (var_count > 0) {
                        try self.printLine("var _locals_js: [{d}]JSValue = undefined;", .{var_count});
                        try self.printLine("for (0..{d}) |_i| {{ _locals_js[_i] = CV.toJSValuePtr(&locals[_i]); }}", .{var_count});
                    } else {
                        try self.writeLine("var _locals_js: [0]JSValue = undefined;");
                    }
                    if (self.has_put_arg and arg_count > 0) {
                        try self.printLine("var _args_js: [{d}]JSValue = undefined;", .{arg_count});
                        try self.printLine("for (0..{d}) |_i| {{ _args_js[_i] = CV.toJSValuePtr(&arg_shadow[_i]); }}", .{arg_count});
                        try self.printLine("const _closure = JSValue.createClosure(ctx, _bfunc, var_refs, &_locals_js, {d}, &_args_js);", .{var_count});
                    } else {
                        try self.printLine("const _closure = JSValue.createClosure(ctx, _bfunc, var_refs, if ({d} > 0) &_locals_js else null, {d}, argv[0..@intCast(argc)]);", .{ var_count, var_count });
                    }
                }
                try self.writeLine("stack[sp] = CV.fromJSValue(_closure);");
                try self.writeLine("sp += 1;");
                self.popIndent();
                try self.writeLine("}");
                // Mark that a closure has been created - enables closure var sync after calls
                self.closure_created_in_block = true;
                // Track that we pushed a value to the real stack
                try self.vpushStackRef();
            },

            // fclosure: create closure (32-bit index variant)
            .fclosure => {
                const func_idx = instr.operand.const_idx;
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

                const var_count = self.func.var_count;
                const arg_count = self.func.arg_count;

                // Use V2 closure creation with shared var_refs for function hoisting support
                if (self.has_fclosure and var_count > 0) {
                    // Sync _locals_jsv with current locals before closure creation
                    // In dispatch_mode, access via bctx pointers
                    const locals_jsv_expr = self.getLocalsJsvExpr();
                    try self.printLine("for (0..{d}) |_i| {{ {s}[_i] = CV.toJSValuePtr(&locals[_i]); }}", .{ var_count, locals_jsv_expr });

                    const var_ref_ptr = self.getVarRefListPtr();
                    const locals_jsv_ptr = self.getLocalsJsvPtr();
                    if (self.has_put_arg and arg_count > 0) {
                        try self.printLine("var _args_js: [{d}]JSValue = undefined;", .{arg_count});
                        try self.printLine("for (0..{d}) |_i| {{ _args_js[_i] = CV.toJSValuePtr(&arg_shadow[_i]); }}", .{arg_count});
                        try self.printLine("const _closure = JSValue.createClosureV2(ctx, _bfunc, var_refs, {s}, {s}, {d}, &_args_js);", .{ var_ref_ptr, locals_jsv_ptr, var_count });
                    } else {
                        try self.printLine("const _closure = JSValue.createClosureV2(ctx, _bfunc, var_refs, {s}, {s}, {d}, argv[0..@intCast(argc)]);", .{ var_ref_ptr, locals_jsv_ptr, var_count });
                    }
                } else {
                    // Fallback to original closure creation (no locals or not tracking)
                    if (var_count > 0) {
                        try self.printLine("var _locals_js: [{d}]JSValue = undefined;", .{var_count});
                        try self.printLine("for (0..{d}) |_i| {{ _locals_js[_i] = CV.toJSValuePtr(&locals[_i]); }}", .{var_count});
                    } else {
                        try self.writeLine("var _locals_js: [0]JSValue = undefined;");
                    }
                    if (self.has_put_arg and arg_count > 0) {
                        try self.printLine("var _args_js: [{d}]JSValue = undefined;", .{arg_count});
                        try self.printLine("for (0..{d}) |_i| {{ _args_js[_i] = CV.toJSValuePtr(&arg_shadow[_i]); }}", .{arg_count});
                        try self.printLine("const _closure = JSValue.createClosure(ctx, _bfunc, var_refs, &_locals_js, {d}, &_args_js);", .{var_count});
                    } else {
                        try self.printLine("const _closure = JSValue.createClosure(ctx, _bfunc, var_refs, if ({d} > 0) &_locals_js else null, {d}, argv[0..@intCast(argc)]);", .{ var_count, var_count });
                    }
                }
                try self.writeLine("stack[sp] = CV.fromJSValue(_closure);");
                try self.writeLine("sp += 1;");
                self.popIndent();
                try self.writeLine("}");
                // Mark that a closure has been created - enables closure var sync after calls
                self.closure_created_in_block = true;
                // Track that we pushed a value to the real stack
                try self.vpushStackRef();
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
                self.vpopAndFree();
                self.vpopAndFree();
                // Use strictEqWithCtx which properly handles strings through FFI
                // (CV.strictEq only compares bits, failing for different string objects with same content)
                try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.strictEqWithCtx(ctx, a, b); sp -= 1; }");
                // Push result reference to vstack so if_false can pop it
                try self.vpush("stack[sp - 1]");
            },

            // strict_neq: strict inequality (!==)
            .strict_neq => {
                // Pop both operands from vstack (they're being consumed)
                self.vpopAndFree();
                self.vpopAndFree();
                // Use strictNeqWithCtx which properly handles strings through FFI
                try self.writeLine("{ const b = stack[sp-1]; const a = stack[sp-2]; stack[sp-2] = zig_runtime.strictNeqWithCtx(ctx, a, b); sp -= 1; }");
                // Push result reference to vstack so if_false can pop it
                try self.vpush("stack[sp - 1]");
            },

            // tail_call_method: tail call optimization for method calls
            .tail_call_method => {
                const argc = instr.operand.u16;
                // Emit as regular call_method followed by return
                try self.emitCallMethod(argc);
                try self.emitVarRefDetach();
                // Cleanup locals before returning (skip in dispatch_mode - done in dispatch loop)
                if (!self.dispatch_mode) {
                    try self.emitLocalsCleanup();
                }
                if (self.dispatch_mode) {
                    try self.writeLine("return .{ .return_value = CV.toJSValuePtr(&stack[sp - 1]) };");
                } else {
                    try self.writeLine("return CV.toJSValuePtr(&stack[sp - 1]);");
                }
                return false; // Control terminates
            },

            // get_loc8: get local (8-bit index, common variant)
            // Must dup ref types to maintain proper refcount when stack values are freed
            .get_loc8 => {
                const loc_idx = instr.operand.loc;
                try self.printLine("{{ const v = locals[{d}]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }}", .{loc_idx});
            },

            // put_loc8: put local (8-bit index, common variant)
            // Must free old ref type value before overwriting
            .put_loc8 => {
                const loc_idx = instr.operand.loc;
                try self.printLine("{{ const old = locals[{d}]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); locals[{d}] = stack[sp - 1]; sp -= 1; }}", .{ loc_idx, loc_idx });
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0 and loc_idx < self.func.var_count) {
                    try self.printLine("{s}[{d}] = CV.toJSValuePtr(&locals[{d}]);", .{ self.getLocalsJsvExpr(), loc_idx, loc_idx });
                }
            },

            // put_loc_check: put local with TDZ check
            // Must free old ref type value before overwriting
            .put_loc_check => {
                const loc_idx = instr.operand.loc;
                try self.printLine("{{ const old = locals[{d}]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); locals[{d}] = stack[sp - 1]; sp -= 1; }}", .{ loc_idx, loc_idx });
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0 and loc_idx < self.func.var_count) {
                    try self.printLine("{s}[{d}] = CV.toJSValuePtr(&locals[{d}]);", .{ self.getLocalsJsvExpr(), loc_idx, loc_idx });
                }
            },

            // get_length: get .length property (optimized via JS_GetLength)
            .get_length => {
                // Use nativeGetLengthCV for O(1) array/string length access (avoids LLVM return corruption on WASM32)
                try self.writeLine("{ const cv = stack[sp-1]; const obj = cv.toJSValueWithCtx(ctx); stack[sp-1] = zig_runtime.nativeGetLengthCV(ctx, obj); if (cv.isRefType()) JSValue.free(ctx, obj); }");
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

            // to_propkey: convert TOS to property key (string/symbol)
            .to_propkey => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                // Convert stack[sp-1] to property key if not already string/int
                // Use isStr() and isInt() which are the correct CompressedValue methods
                try self.writeLine("{ const v = stack[sp - 1]; if (!v.isStr() and !v.isInt()) { const key = JSValue.toString(ctx, v.toJSValueWithCtx(ctx)); if (v.isRefType()) JSValue.free(ctx, v.toJSValueWithCtx(ctx)); stack[sp - 1] = CV.fromJSValue(key); } }");
            },

            // to_propkey2: convert TOS to property key (string/symbol) while keeping second item
            // This is used before dup2 in compound assignment: [arr, idx] -> [arr, propkey(idx)]
            .to_propkey2 => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                // Convert stack[sp-1] (the index) to property key if not already string/int
                // The array at stack[sp-2] is preserved unchanged
                try self.writeLine("{ const v = stack[sp - 1]; if (!v.isStr() and !v.isInt()) { const key = JSValue.toString(ctx, v.toJSValueWithCtx(ctx)); if (v.isRefType()) JSValue.free(ctx, v.toJSValueWithCtx(ctx)); stack[sp - 1] = CV.fromJSValue(key); } }");
            },

            // throw: throw exception
            .throw => {
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const exc = stack[sp-1]; sp -= 1;");
                try self.writeLine("_ = JSValue.throw(ctx, exc.toJSValueWithCtx(ctx));");
                try self.emitReturnException();
                self.popIndent();
                try self.writeLine("}");
            },

            // typeof: get type string - CV to JSValue conversion
            .typeof => {
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = CV.fromJSValue(JSValue.typeOf(ctx, v.toJSValueWithCtx(ctx))); }");
            },

            // lnot: logical not - CV needs conversion to JSValue for toBool call
            .lnot => {
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = if (JSValue.toBool(ctx, v.toJSValueWithCtx(ctx)) != 0) CV.FALSE else CV.TRUE; }");
            },

            // typeof_is_function: check if typeof == "function" - use QuickJS JS_IsFunction
            // which properly checks object class_id (bytecode function, proxy, or callable object)
            // Must free the value if it's a ref type before overwriting with boolean result
            .typeof_is_function => {
                try self.writeLine("{ const v = stack[sp-1]; const v_jsv = v.toJSValueWithCtx(ctx); const result = if (zig_runtime.quickjs.JS_IsFunction(ctx, v_jsv) != 0) CV.TRUE else CV.FALSE; if (v.isRefType()) JSValue.free(ctx, v_jsv); stack[sp-1] = result; }");
            },

            // typeof_is_undefined: check if typeof == "undefined" - use CV's isUndefined method
            // Must free the value if it's a ref type before overwriting with boolean result
            .typeof_is_undefined => {
                try self.writeLine("{ const v = stack[sp-1]; const result = if (v.isUndefined()) CV.TRUE else CV.FALSE; if (v.isRefType()) JSValue.free(ctx, v.toJSValueWithCtx(ctx)); stack[sp-1] = result; }");
            },

            // special_object: create special object (arguments, etc.)
            // Operand: 0=ARGUMENTS, 1=MAPPED_ARGUMENTS, 2=THIS_FUNC, 3=NEW_TARGET, etc.
            .special_object => {
                const obj_type = instr.operand.u8;
                if (obj_type == 0 or obj_type == 1) {
                    // ARGUMENTS or MAPPED_ARGUMENTS: create array-like object from argc/argv
                    try self.writeLine("// special_object: creating arguments object from argc/argv");
                    try self.writeLine("{");
                    self.pushIndent();
                    try self.writeLine("const args_arr = JSValue.newArray(ctx);");
                    try self.writeLine("var _i: usize = 0;");
                    try self.writeLine("while (_i < @as(usize, @intCast(argc))) : (_i += 1) {");
                    self.pushIndent();
                    try self.writeLine("_ = JSValue.setPropertyUint32(ctx, args_arr, @intCast(_i), JSValue.dup(ctx, argv[_i]));");
                    self.popIndent();
                    try self.writeLine("}");
                    try self.writeLine("stack[sp] = CV.fromJSValue(args_arr); sp += 1;");
                    self.popIndent();
                    try self.writeLine("}");
                } else if (obj_type == 2) {
                    // THIS_FUNC: push current function (use this_val as approximation)
                    try self.writeLine("// special_object: THIS_FUNC");
                    try self.writeLine("stack[sp] = CV.fromJSValue(JSValue.dup(ctx, this_val)); sp += 1;");
                } else if (obj_type == 3) {
                    // NEW_TARGET: for constructor calls
                    try self.writeLine("// special_object: NEW_TARGET");
                    try self.writeLine("stack[sp] = CV.fromJSValue(JSValue.dup(ctx, this_val)); sp += 1;");
                } else {
                    // Other types: create empty object as fallback
                    try self.printLine("// special_object: type {d} (fallback to empty object)", .{obj_type});
                    try self.writeLine("stack[sp] = CV.fromJSValue(JSValue.newObject(ctx)); sp += 1;");
                }
            },

            // check_ctor: check if called with 'new'
            // When called as constructor, this_val is the newly created object.
            // When called as regular function, this_val is undefined/global.
            // Check that this_val is a proper object (not undefined/null).
            .check_ctor => {
                if (self.dispatch_mode) {
                    try self.writeLine("{ if (!this_val.isObject()) return .{ .return_value = JSValue.throwTypeError(ctx, \"Constructor requires 'new'\") }; }");
                } else {
                    try self.writeLine("{ if (!this_val.isObject()) return JSValue.throwTypeError(ctx, \"Constructor requires 'new'\"); }");
                }
            },

            // check_ctor_return: validate constructor return value
            .check_ctor_return => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                // If return value is object, use it; otherwise use 'this'
                // this_val is JSValue, wrap it in CV before assigning
                try self.writeLine("{ const ret = stack[sp - 1]; if (!ret.isObject() and !ret.isUndefined()) { if (ret.isRefType()) JSValue.free(ctx, ret.toJSValueWithCtx(ctx)); stack[sp - 1] = CV.fromJSValue(this_val); } }");
            },

            // push_empty_string: push ""
            .push_empty_string => {
                try self.writeLine("stack[sp] = CV.fromJSValue(JSValue.newString(ctx, \"\")); sp += 1;");
            },

            // is_undefined_or_null: check if undefined or null
            // Must free the value if it's a ref type before overwriting with boolean result
            .is_undefined_or_null => {
                // Pop operand from vstack (being consumed)
                self.vpopAndFree();
                try self.writeLine("{ const v = stack[sp-1]; const result = if (v.isUndefined() or v.isNull()) CV.TRUE else CV.FALSE; if (v.isRefType()) JSValue.free(ctx, v.toJSValueWithCtx(ctx)); stack[sp-1] = result; }");
                // Push result reference to vstack so if_true/if_false can pop it
                try self.vpush("stack[sp - 1]");
            },

            // set_name: set function name property
            .set_name => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                const atom = instr.operand.atom;
                // Set the 'name' property on the function at TOS
                try self.printLine("{{ const func = stack[sp - 1].toJSValueWithCtx(ctx); _ = JSValue.definePropertyValueStr(ctx, func, \"name\", JSValue.newAtomString(ctx, {d}), JSValue.JS_PROP_CONFIGURABLE); }}", .{atom});
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
                try self.emitVarRefDetach();
                // Cleanup locals before returning (skip in dispatch_mode - done in dispatch loop)
                if (!self.dispatch_mode) {
                    try self.emitLocalsCleanup();
                }
                if (self.dispatch_mode) {
                    try self.writeLine("return .{ .return_value = CV.toJSValuePtr(&stack[sp - 1]) };");
                } else {
                    try self.writeLine("return CV.toJSValuePtr(&stack[sp - 1]);");
                }
                return false; // Control terminates
            },

            // is_undefined: check if value is undefined
            // Must free the value if it's a ref type before overwriting with boolean result
            .is_undefined => {
                try self.writeLine("{ const v = stack[sp-1]; const result = if (v.isUndefined()) CV.TRUE else CV.FALSE; if (v.isRefType()) JSValue.free(ctx, v.toJSValueWithCtx(ctx)); stack[sp-1] = result; }");
            },

            // is_null: check if value is null
            // Must free the value if it's a ref type before overwriting with boolean result
            .is_null => {
                try self.writeLine("{ const v = stack[sp-1]; const result = if (v.isNull()) CV.TRUE else CV.FALSE; if (v.isRefType()) JSValue.free(ctx, v.toJSValueWithCtx(ctx)); stack[sp-1] = result; }");
            },

            // put_loc_check_init: put local with TDZ check for initialization
            .put_loc_check_init => {
                const loc_idx = instr.operand.loc;
                try self.printLine("{{ locals[{d}] = stack[sp-1]; sp -= 1; }}", .{loc_idx});
            },

            // post_inc: post-increment (x++)
            .post_inc => {
                try self.writeLine("{ const v = stack[sp-1]; var val: i32 = 0; _ = JSValue.toInt32(ctx, &val, v.toJSValueWithCtx(ctx)); stack[sp-1] = v; stack[sp] = CV.newInt(val + 1); sp += 1; }");
            },

            // post_dec: post-decrement (x--)
            .post_dec => {
                try self.writeLine("{ const v = stack[sp-1]; var val: i32 = 0; _ = JSValue.toInt32(ctx, &val, v.toJSValueWithCtx(ctx)); stack[sp-1] = v; stack[sp] = CV.newInt(val - 1); sp += 1; }");
            },

            // put_arg0-3: put argument - use arg_shadow when has_put_arg
            // This fixes the bug where argc < param_index caused assignment to be lost
            // IMPORTANT: Must free old value first to prevent memory leaks when arguments are reassigned in loops
            .put_arg0 => {
                // has_put_arg is always true for put_arg opcodes, use arg_shadow
                try self.writeLine("{ const old = arg_shadow[0]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); arg_shadow[0] = stack[sp-1]; sp -= 1; }");
            },
            .put_arg1 => {
                try self.writeLine("{ const old = arg_shadow[1]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); arg_shadow[1] = stack[sp-1]; sp -= 1; }");
            },
            .put_arg2 => {
                try self.writeLine("{ const old = arg_shadow[2]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); arg_shadow[2] = stack[sp-1]; sp -= 1; }");
            },
            .put_arg3 => {
                try self.writeLine("{ const old = arg_shadow[3]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); arg_shadow[3] = stack[sp-1]; sp -= 1; }");
            },

            // set_arg0-3: set argument (like put but keeps on stack) - use arg_shadow
            // This fixes the bug where argc < param_index caused assignment to be lost
            // IMPORTANT: Must free old value first to prevent memory leaks when arguments are reassigned in loops
            .set_arg0 => {
                // has_put_arg is always true for set_arg opcodes, use arg_shadow
                // Note: set_arg keeps value on stack (doesn't pop), so we copy the reference
                try self.writeLine("{ const old = arg_shadow[0]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); const v = stack[sp-1]; arg_shadow[0] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; }");
            },
            .set_arg1 => {
                try self.writeLine("{ const old = arg_shadow[1]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); const v = stack[sp-1]; arg_shadow[1] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; }");
            },
            .set_arg2 => {
                try self.writeLine("{ const old = arg_shadow[2]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); const v = stack[sp-1]; arg_shadow[2] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; }");
            },
            .set_arg3 => {
                try self.writeLine("{ const old = arg_shadow[3]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); const v = stack[sp-1]; arg_shadow[3] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; }");
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

            // set_loc8: set local (keeps value on stack)
            // Must free old ref type value and dup new value (since stack keeps it too)
            .set_loc8 => {
                const loc_idx = instr.operand.loc;
                try self.printLine("{{ const old = locals[{d}]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); const v = stack[sp - 1]; locals[{d}] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; }}", .{ loc_idx, loc_idx });
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0 and loc_idx < self.func.var_count) {
                    try self.printLine("{s}[{d}] = CV.toJSValuePtr(&locals[{d}]);", .{ self.getLocalsJsvExpr(), loc_idx, loc_idx });
                }
            },

            // get_loc0_loc1: push both loc0 and loc1
            // Must dup ref types to maintain proper refcount when stack values are freed
            .get_loc0_loc1 => {
                // Push both locals to the actual stack (not vstack - this is stack-based codegen)
                try self.writeLine("{ const v = locals[0]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }");
                try self.writeLine("{ const v = locals[1]; stack[sp] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; sp += 1; }");
            },

            // add_loc: add to local variable - CV.add inline
            // addWithCtx is non-consuming, so we must explicitly free the old local and stack values.
            .add_loc => {
                const loc_idx = instr.operand.loc;
                try self.printLine("locals[{d}] = CV.addWithCtx(ctx, locals[{d}], stack[sp-1]); sp -= 1;", .{ loc_idx, loc_idx });
                // In native loops, the loop condition reads from stack[sp-1] which may
                // reference this local. After updating the local, sync it back to the stack
                // so the condition check sees the updated value.
                // But only if sp > 0 after the decrement (avoid stack[-1] access)
                if (self.natural_loops.len > 0) {
                    try self.printLine("if (sp > 0) stack[sp - 1] = locals[{d}];", .{loc_idx});
                }
            },

            // close_loc: close local variable (for closures) - free the local being closed over
            .close_loc => {
                const loc_idx = instr.operand.loc;
                // Free the local variable being closed over and reset to undefined
                try self.printLine("{{ const v = locals[{d}]; if (v.isRefType()) JSValue.free(ctx, v.toJSValueWithCtx(ctx)); locals[{d}] = CV.UNDEFINED; }}", .{ loc_idx, loc_idx });
            },

            // instanceof: check instanceof (convert CV to JSValue, don't free)
            .instanceof => {
                try self.writeLine("{");
                try self.writeLine("  const ctor = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("  const obj = stack[sp-2].toJSValueWithCtx(ctx);");
                try self.writeLine("  stack[sp-2] = CV.fromJSValue(JSValue.newBool(JSValue.isInstanceOf(ctx, obj, ctor)));");
                try self.writeLine("  sp -= 1;");
                try self.writeLine("}");
            },

            // in: check if property exists in object (prop in obj)
            .in => {
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const obj = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("const prop = stack[sp-2].toJSValueWithCtx(ctx);");
                try self.writeLine("const atom = zig_runtime.quickjs.JS_ValueToAtom(ctx, prop);");
                try self.writeLine("const result = zig_runtime.quickjs.JS_HasProperty(ctx, obj, atom);");
                try self.writeLine("zig_runtime.quickjs.JS_FreeAtom(ctx, atom);");
                try self.writeLine("if (result < 0) {");
                self.pushIndent();
                try self.emitReturnException();
                self.popIndent();
                try self.writeLine("}");
                try self.writeLine("stack[sp-2] = CV.fromJSValue(JSValue.newBool(result > 0));");
                try self.writeLine("sp -= 1;");
                self.popIndent();
                try self.writeLine("}");
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
                const const_idx = instr.operand.const_idx;
                try self.printLine("stack[sp] = if (cpool) |cp| CV.fromJSValue(JSValue.dup(ctx, cp[{d}])) else CV.UNDEFINED; sp += 1;", .{const_idx});
            },

            // push_const: push constant from pool (32-bit index)
            .push_const => {
                const const_idx = instr.operand.const_idx;
                try self.printLine("stack[sp] = if (cpool) |cp| CV.fromJSValue(JSValue.dup(ctx, cp[{d}])) else CV.UNDEFINED; sp += 1;", .{const_idx});
            },

            // define_method: define method on class with atom name
            // Flags: 0=method, 1=getter, 2=setter, 4=enumerable
            .define_method => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                const atom = instr.operand.atom_u8.atom;
                const flags = instr.operand.atom_u8.value;
                const method_type = flags & 3; // 0=method, 1=getter, 2=setter
                const is_enumerable = (flags & 4) != 0;

                // Stack: [obj, method] -> [obj] (method is consumed)
                try self.writeLine("{");
                try self.writeLine("  const method = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("  const obj = stack[sp-2].toJSValueWithCtx(ctx);");

                // Get the property name string and create a runtime atom
                // (bytecode atoms are module-local, need to convert to runtime atoms)
                if (self.getAtomString(atom)) |prop_name| {
                    const escaped_prop = escapeZigString(self.allocator, prop_name) catch prop_name;
                    defer if (escaped_prop.ptr != prop_name.ptr) self.allocator.free(escaped_prop);
                    try self.printLine("  const _atom = zig_runtime.quickjs.JS_NewAtom(ctx, \"{s}\");", .{escaped_prop});
                    try self.writeLine("  defer zig_runtime.quickjs.JS_FreeAtom(ctx, _atom);");

                    if (method_type == 1) {
                        // Getter: use definePropertyGetSet with getter and NULL setter
                        const enum_flag = if (is_enumerable) " | JSValue.JS_PROP_ENUMERABLE" else "";
                        try self.printLine("  _ = JSValue.definePropertyGetSet(ctx, obj, _atom, method, JSValue.UNDEFINED, JSValue.JS_PROP_CONFIGURABLE | JSValue.JS_PROP_HAS_GET{s});", .{enum_flag});
                    } else if (method_type == 2) {
                        // Setter: use definePropertyGetSet with NULL getter and setter
                        const enum_flag = if (is_enumerable) " | JSValue.JS_PROP_ENUMERABLE" else "";
                        try self.printLine("  _ = JSValue.definePropertyGetSet(ctx, obj, _atom, JSValue.UNDEFINED, method, JSValue.JS_PROP_CONFIGURABLE | JSValue.JS_PROP_HAS_SET{s});", .{enum_flag});
                    } else {
                        // Regular method: use definePropertyValueAtom
                        try self.writeLine("  _ = JSValue.definePropertyValueAtom(ctx, obj, _atom, method, JSValue.JS_PROP_C_W_E);");
                    }
                } else {
                    // Fallback to raw atom (shouldn't happen for valid bytecode)
                    if (method_type == 1) {
                        const enum_flag = if (is_enumerable) " | JSValue.JS_PROP_ENUMERABLE" else "";
                        try self.printLine("  _ = JSValue.definePropertyGetSet(ctx, obj, {d}, method, JSValue.UNDEFINED, JSValue.JS_PROP_CONFIGURABLE | JSValue.JS_PROP_HAS_GET{s});", .{ atom, enum_flag });
                    } else if (method_type == 2) {
                        const enum_flag = if (is_enumerable) " | JSValue.JS_PROP_ENUMERABLE" else "";
                        try self.printLine("  _ = JSValue.definePropertyGetSet(ctx, obj, {d}, JSValue.UNDEFINED, method, JSValue.JS_PROP_CONFIGURABLE | JSValue.JS_PROP_HAS_SET{s});", .{ atom, enum_flag });
                    } else {
                        try self.printLine("  _ = JSValue.definePropertyValueAtom(ctx, obj, {d}, method, JSValue.JS_PROP_C_W_E);", .{atom});
                    }
                }
                try self.writeLine("  sp -= 1;");
                try self.writeLine("}");
            },

            // define_method_computed: define method on class with computed key
            // Flags: 0=method, 1=getter, 2=setter, 4=enumerable
            // Stack: [obj, key, method] -> [obj] (key and method consumed)
            .define_method_computed => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                const flags = instr.operand.u8;
                const method_type = flags & 3; // 0=method, 1=getter, 2=setter
                const is_enumerable = (flags & 4) != 0;

                try self.writeLine("{");
                try self.writeLine("  const method = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("  const key = stack[sp-2].toJSValueWithCtx(ctx);");
                try self.writeLine("  const obj = stack[sp-3].toJSValueWithCtx(ctx);");
                try self.writeLine("  const atom = zig_runtime.quickjs.JS_ValueToAtom(ctx, key);");

                if (method_type == 1) {
                    // Getter: use definePropertyGetSet with getter and NULL setter
                    const enum_flag = if (is_enumerable) " | JSValue.JS_PROP_ENUMERABLE" else "";
                    try self.printLine("  _ = JSValue.definePropertyGetSet(ctx, obj, atom, method, JSValue.UNDEFINED, JSValue.JS_PROP_CONFIGURABLE | JSValue.JS_PROP_HAS_GET{s});", .{enum_flag});
                } else if (method_type == 2) {
                    // Setter: use definePropertyGetSet with NULL getter and setter
                    const enum_flag = if (is_enumerable) " | JSValue.JS_PROP_ENUMERABLE" else "";
                    try self.printLine("  _ = JSValue.definePropertyGetSet(ctx, obj, atom, JSValue.UNDEFINED, method, JSValue.JS_PROP_CONFIGURABLE | JSValue.JS_PROP_HAS_SET{s});", .{enum_flag});
                } else {
                    // Regular method: use definePropertyValueAtom
                    try self.writeLine("  _ = JSValue.definePropertyValueAtom(ctx, obj, atom, method, JSValue.JS_PROP_C_W_E);");
                }
                try self.writeLine("  zig_runtime.quickjs.JS_FreeAtom(ctx, atom);");
                try self.writeLine("  JSValue.free(ctx, key);");
                try self.writeLine("  sp -= 2;");
                try self.writeLine("}");
            },

            // set_name_computed: set function name from computed value
            // Stack: [name, func] -> [name, func] (both kept on stack)
            .set_name_computed => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                try self.writeLine("{");
                try self.writeLine("  const func = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("  const name = stack[sp-2].toJSValueWithCtx(ctx);");
                try self.writeLine("  _ = JSValue.definePropertyValueStr(ctx, func, \"name\", JSValue.dup(ctx, name), JSValue.JS_PROP_CONFIGURABLE);");
                try self.writeLine("}");
            },

            // define_class: define ES6 class
            // Stack: [parent, fields] -> [ctor, proto]
            // The class constructor is created from the bytecode function on stack
            .define_class => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                const atom = instr.operand.atom_u8.atom;
                const class_flags = instr.operand.atom_u8.value;
                const var_count = self.func.var_count;
                const arg_count = self.func.arg_count;

                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const _bfunc = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("const _parent = stack[sp-2].toJSValueWithCtx(ctx);");
                try self.writeLine("var _ctor: JSValue = undefined;");
                try self.writeLine("var _proto: JSValue = undefined;");

                // Set up locals and args for closure variable capture
                if (self.has_fclosure and var_count > 0) {
                    // Sync _locals_jsv with current locals before class creation
                    // In dispatch_mode, access via bctx pointers
                    const locals_jsv_expr = self.getLocalsJsvExpr();
                    try self.printLine("for (0..{d}) |_i| {{ {s}[_i] = CV.toJSValuePtr(&locals[_i]); }}", .{ var_count, locals_jsv_expr });

                    const var_ref_ptr = self.getVarRefListPtr();
                    const locals_jsv_ptr = self.getLocalsJsvPtr();
                    if (self.has_put_arg and arg_count > 0) {
                        try self.printLine("var _args_js: [{d}]JSValue = undefined;", .{arg_count});
                        try self.printLine("for (0..{d}) |_i| {{ _args_js[_i] = CV.toJSValuePtr(&arg_shadow[_i]); }}", .{arg_count});
                        try self.printLine("const _res = zig_runtime.quickjs.js_frozen_define_class(ctx, _bfunc, _parent, {d}, {d}, @ptrCast(var_refs), {s}, {s}, {d}, &_args_js, {d}, &_ctor, &_proto);", .{ class_flags, atom, var_ref_ptr, locals_jsv_ptr, var_count, arg_count });
                    } else {
                        try self.printLine("const _res = zig_runtime.quickjs.js_frozen_define_class(ctx, _bfunc, _parent, {d}, {d}, @ptrCast(var_refs), {s}, {s}, {d}, if (argc > 0) argv else null, @intCast(argc), &_ctor, &_proto);", .{ class_flags, atom, var_ref_ptr, locals_jsv_ptr, var_count });
                    }
                } else {
                    // No closures or no locals - simpler path
                    if (var_count > 0) {
                        try self.printLine("var _locals_js: [{d}]JSValue = undefined;", .{var_count});
                        try self.printLine("for (0..{d}) |_i| {{ _locals_js[_i] = CV.toJSValuePtr(&locals[_i]); }}", .{var_count});
                        try self.printLine("const _res = zig_runtime.quickjs.js_frozen_define_class(ctx, _bfunc, _parent, {d}, {d}, @ptrCast(var_refs), null, &_locals_js, {d}, if (argc > 0) argv else null, @intCast(argc), &_ctor, &_proto);", .{ class_flags, atom, var_count });
                    } else {
                        try self.printLine("const _res = zig_runtime.quickjs.js_frozen_define_class(ctx, _bfunc, _parent, {d}, {d}, @ptrCast(var_refs), null, null, 0, if (argc > 0) argv else null, @intCast(argc), &_ctor, &_proto);", .{ class_flags, atom });
                    }
                }

                // Check for error
                try self.writeLine("if (_res < 0) {");
                self.pushIndent();
                try self.emitReturnException();
                self.popIndent();
                try self.writeLine("}");

                try self.writeLine("stack[sp-2] = CV.fromJSValue(_ctor);");
                try self.writeLine("stack[sp-1] = CV.fromJSValue(_proto);");
                self.popIndent();
                try self.writeLine("}");
            },

            // set_home_object: set the [[HomeObject]] internal slot for 'super' calls
            // Stack: [home_object, func] -> [home_object, func] (both kept on stack)
            // This sets the home object on a method so super.* works correctly
            .set_home_object => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                // Use QuickJS internal API to set home object on the function
                try self.writeLine("{");
                try self.writeLine("  const func = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("  const home = stack[sp-2].toJSValueWithCtx(ctx);");
                try self.writeLine("  zig_runtime.quickjs.JS_SetHomeObject(ctx, func, home);");
                try self.writeLine("}");
            },

            // get_super: get prototype of object (for super calls)
            .get_super => {
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const obj = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("const proto = zig_runtime.quickjs.JS_GetPrototype(ctx, obj);");
                try self.emitInlineExceptionReturn("proto");
                try self.writeLine("stack[sp-1] = CV.fromJSValue(proto);");
                self.popIndent();
                try self.writeLine("}");
            },

            // init_ctor: initialize constructor - create new this object
            .init_ctor => {
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const proto = JSValue.getPropertyStr(ctx, this_val, \"prototype\");");
                try self.writeLine("const this_obj = zig_runtime.quickjs.JS_NewObjectProtoClass(ctx, proto, zig_runtime.quickjs.JS_CLASS_OBJECT);");
                try self.writeLine("JSValue.free(ctx, proto);");
                try self.emitInlineExceptionReturn("this_obj");
                try self.writeLine("stack[sp] = CV.fromJSValue(this_obj); sp += 1;");
                self.popIndent();
                try self.writeLine("}");
            },

            // regexp: create RegExp from pattern and flags
            .regexp => {
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const flags = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("const pattern = stack[sp-2].toJSValueWithCtx(ctx);");
                try self.writeLine("const global = JSValue.getGlobalObject(ctx);");
                try self.writeLine("const RegExpCtor = JSValue.getPropertyStr(ctx, global, \"RegExp\");");
                try self.writeLine("JSValue.free(ctx, global);");
                try self.writeLine("var args = [2]JSValue{ pattern, flags };");
                try self.writeLine("const rx = JSValue.callConstructor(ctx, RegExpCtor, 2, &args);");
                try self.writeLine("JSValue.free(ctx, RegExpCtor);");
                try self.emitInlineExceptionReturn("rx");
                try self.writeLine("stack[sp-2] = CV.fromJSValue(rx); sp -= 1;");
                self.popIndent();
                try self.writeLine("}");
            },

            // set_loc0-3: set local (keeps value on stack) - must dup to local and free old
            .set_loc0 => {
                try self.writeLine("{ const _old = locals[0]; if (_old.isRefType()) JSValue.free(ctx, _old.toJSValueWithCtx(ctx)); const _v = stack[sp-1]; locals[0] = if (_v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, _v.toJSValueWithCtx(ctx))) else _v; }");
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0) {
                    try self.printLine("{s}[0] = CV.toJSValuePtr(&locals[0]);", .{self.getLocalsJsvExpr()});
                }
            },
            .set_loc1 => {
                try self.writeLine("{ const _old = locals[1]; if (_old.isRefType()) JSValue.free(ctx, _old.toJSValueWithCtx(ctx)); const _v = stack[sp-1]; locals[1] = if (_v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, _v.toJSValueWithCtx(ctx))) else _v; }");
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0) {
                    try self.printLine("{s}[1] = CV.toJSValuePtr(&locals[1]);", .{self.getLocalsJsvExpr()});
                }
            },
            .set_loc2 => {
                try self.writeLine("{ const _old = locals[2]; if (_old.isRefType()) JSValue.free(ctx, _old.toJSValueWithCtx(ctx)); const _v = stack[sp-1]; locals[2] = if (_v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, _v.toJSValueWithCtx(ctx))) else _v; }");
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0) {
                    try self.printLine("{s}[2] = CV.toJSValuePtr(&locals[2]);", .{self.getLocalsJsvExpr()});
                }
            },
            .set_loc3 => {
                try self.writeLine("{ const _old = locals[3]; if (_old.isRefType()) JSValue.free(ctx, _old.toJSValueWithCtx(ctx)); const _v = stack[sp-1]; locals[3] = if (_v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, _v.toJSValueWithCtx(ctx))) else _v; }");
                // Sync JSValue shadow for shared var_refs (function hoisting support)
                if (self.has_fclosure and self.func.var_count > 0) {
                    try self.printLine("{s}[3] = CV.toJSValuePtr(&locals[3]);", .{self.getLocalsJsvExpr()});
                }
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

            // put_arg: put argument (generic with index) - use arg_shadow
            // has_put_arg is always true when put_arg exists, use arg_shadow to match put_arg0-3
            .put_arg => {
                const idx = instr.operand.arg;
                try self.printLine("{{ const old = arg_shadow[{d}]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); arg_shadow[{d}] = stack[sp-1]; sp -= 1; }}", .{ idx, idx });
            },

            // set_arg: set argument (generic with index) - use arg_shadow
            // has_put_arg is always true when set_arg exists, use arg_shadow to match set_arg0-3
            .set_arg => {
                const idx = instr.operand.arg;
                try self.printLine("{{ const old = arg_shadow[{d}]; if (old.isRefType()) JSValue.free(ctx, old.toJSValueWithCtx(ctx)); const v = stack[sp-1]; arg_shadow[{d}] = if (v.isRefType()) CV.fromJSValue(JSValue.dup(ctx, v.toJSValueWithCtx(ctx))) else v; }}", .{ idx, idx });
            },

            // delete: delete property with proper reference handling
            .delete => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                // Free prop and obj references after deletion since we're consuming them
                try self.writeLine("{");
                try self.writeLine("  const prop_cv = stack[sp-1];");
                try self.writeLine("  const obj_cv = stack[sp-2];");
                try self.writeLine("  const prop = prop_cv.toJSValueWithCtx(ctx);");
                try self.writeLine("  const obj = obj_cv.toJSValueWithCtx(ctx);");
                try self.writeLine("  const result = JSValue.deleteProperty(ctx, obj, prop);");
                try self.writeLine("  if (prop_cv.isRefType()) JSValue.free(ctx, prop);");
                try self.writeLine("  if (obj_cv.isRefType()) JSValue.free(ctx, obj);");
                try self.writeLine("  sp -= 2;");
                try self.writeLine("  stack[sp] = if (result >= 0) CV.TRUE else CV.FALSE;");
                try self.writeLine("  sp += 1;");
                try self.writeLine("}");
            },

            // append: spread elements from iterable to array
            // Stack: [array, pos, enumobj] -> [array, pos] (enumobj consumed, elements appended)
            .append => {
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const enumobj = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("var pos: i32 = stack[sp-2].getInt();");
                try self.writeLine("const arr = stack[sp-3].toJSValueWithCtx(ctx);");
                try self.writeLine("// Only spread if enumobj is defined (skip undefined/null)");
                try self.writeLine("if (!enumobj.isUndefined() and !enumobj.isNull()) {");
                self.pushIndent();
                try self.writeLine("// Get length of enumobj and copy elements");
                try self.writeLine("const src_len_val = JSValue.getPropertyStr(ctx, enumobj, \"length\");");
                try self.writeLine("var src_len: i32 = 0;");
                try self.writeLine("_ = JSValue.toInt32(ctx, &src_len, src_len_val);");
                try self.writeLine("JSValue.free(ctx, src_len_val);");
                try self.writeLine("var i: i32 = 0;");
                try self.writeLine("while (i < src_len) : (i += 1) {");
                self.pushIndent();
                try self.writeLine("const elem = JSValue.getPropertyUint32(ctx, enumobj, @intCast(i));");
                try self.writeLine("_ = JSValue.setPropertyUint32(ctx, arr, @intCast(pos), elem);");
                try self.writeLine("pos += 1;");
                self.popIndent();
                try self.writeLine("}");
                self.popIndent();
                try self.writeLine("}");
                try self.writeLine("stack[sp-2] = CV.newInt(pos);");
                try self.writeLine("if (CV.fromJSValue(enumobj).isRefType()) JSValue.free(ctx, enumobj);");
                try self.writeLine("sp -= 1;");
                self.popIndent();
                try self.writeLine("}");
            },

            // rot3l, rot3r: rotate stack
            .rot3l => {
                try self.writeLine("{ const c = stack[sp-1]; const b = stack[sp-2]; const a = stack[sp-3]; stack[sp-3] = b; stack[sp-2] = c; stack[sp-1] = a; }");
            },
            .rot3r => {
                try self.writeLine("{ const c = stack[sp-1]; const b = stack[sp-2]; const a = stack[sp-3]; stack[sp-3] = c; stack[sp-2] = a; stack[sp-1] = b; }");
            },

            // perm3, perm4, perm5: permute stack
            // perm3: [obj, a, b] -> [a, obj, b] (QuickJS 213 permutation)
            .perm3 => {
                try self.writeLine("{ const c = stack[sp-1]; const b = stack[sp-2]; const a = stack[sp-3]; stack[sp-3] = b; stack[sp-2] = a; stack[sp-1] = c; }");
            },
            // perm4: [obj, prop, a, b] -> [a, obj, prop, b] (QuickJS permutation)
            .perm4 => {
                try self.writeLine("{ const d = stack[sp-1]; const c = stack[sp-2]; const b = stack[sp-3]; const a = stack[sp-4]; stack[sp-4] = c; stack[sp-3] = a; stack[sp-2] = b; stack[sp-1] = d; }");
            },
            // perm5: [this, obj, prop, a, b] -> [a, this, obj, prop, b] (QuickJS permutation)
            .perm5 => {
                try self.writeLine("{ const e = stack[sp-1]; const d = stack[sp-2]; const c = stack[sp-3]; const b = stack[sp-4]; const a = stack[sp-5]; stack[sp-5] = d; stack[sp-4] = a; stack[sp-3] = b; stack[sp-2] = c; stack[sp-1] = e; }");
            },

            // insert2: [obj, val] -> [val, obj, val] (dup_x1)
            // Duplicates val and inserts it below obj, used for assignment expressions
            .insert2 => {
                try self.writeLine("{ const val = stack[sp-1]; const obj = stack[sp-2]; stack[sp] = val; stack[sp-1] = obj; stack[sp-2] = if (val.isRefType()) CV.fromJSValue(JSValue.dup(ctx, val.toJSValueWithCtx(ctx))) else val; sp += 1; }");
            },

            // insert3: [obj, prop, val] -> [val, obj, prop, val] (dup_x2)
            // Duplicates val and inserts it below obj and prop
            .insert3 => {
                try self.writeLine("{ const val = stack[sp-1]; const c = stack[sp-2]; const b = stack[sp-3]; stack[sp] = val; stack[sp-1] = c; stack[sp-2] = b; stack[sp-3] = if (val.isRefType()) CV.fromJSValue(JSValue.dup(ctx, val.toJSValueWithCtx(ctx))) else val; sp += 1; }");
            },

            // insert4: [this, obj, prop, val] -> [val, this, obj, prop, val]
            // Duplicates val and inserts it below this, obj and prop
            .insert4 => {
                try self.writeLine("{ const val = stack[sp-1]; const d = stack[sp-2]; const c = stack[sp-3]; const b = stack[sp-4]; stack[sp] = val; stack[sp-1] = d; stack[sp-2] = c; stack[sp-3] = b; stack[sp-4] = if (val.isRefType()) CV.fromJSValue(JSValue.dup(ctx, val.toJSValueWithCtx(ctx))) else val; sp += 1; }");
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

            // rest: create rest array from arguments starting at first_arg index
            .rest => {
                const first_arg = instr.operand.u16;
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const arr = JSValue.newArray(ctx);");
                try self.printLine("var i: c_int = {d};", .{first_arg});
                try self.writeLine("while (i < argc) : (i += 1) {");
                self.pushIndent();
                try self.printLine("const idx: u32 = @intCast(i - {d});", .{first_arg});
                try self.writeLine("_ = JSValue.setPropertyUint32(ctx, arr, idx, JSValue.dup(ctx, argv[@intCast(i)]));");
                self.popIndent();
                try self.writeLine("}");
                try self.writeLine("stack[sp] = CV.fromJSValue(arr); sp += 1;");
                self.popIndent();
                try self.writeLine("}");
            },

            // to_object: convert to object
            .to_object => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = CV.fromJSValue(JSValue.toObject(ctx, v.toJSValueWithCtx(ctx))); }");
            },

            // define_array_el: define array element (for array literals)
            // Stack: [array, idx, val] -> [array, idx] (only val is consumed)
            // QuickJS uses js_dup(sp[-2]) to preserve idx for subsequent inc operations
            .define_array_el => {
                try self.writeLine("{");
                // CRITICAL: Dup value before passing to JS_SetProperty - it consumes the value
                try self.writeLine("  const val = JSValue.dup(ctx, stack[sp-1].toJSValueWithCtx(ctx));");
                try self.writeLine("  const idx = stack[sp-2].toJSValueWithCtx(ctx);");
                try self.writeLine("  const arr = stack[sp-3].toJSValueWithCtx(ctx);");
                try self.writeLine("  const atom = zig_runtime.quickjs.JS_ValueToAtom(ctx, idx);");
                try self.writeLine("  _ = zig_runtime.quickjs.JS_SetProperty(ctx, arr, atom, val);");
                try self.writeLine("  zig_runtime.quickjs.JS_FreeAtom(ctx, atom);");
                try self.writeLine("  sp -= 1;"); // Only pop val, keep idx for inc
                try self.writeLine("}");
            },

            // ================================================================
            // Iterator Protocol (for-of loops)
            // ================================================================

            // for_of_start: Initialize for-of iterator
            // Stack before: [obj] at sp-1
            // Stack after: [iterator, next_method, catch_offset] (sp increases by 2)
            // IMPORTANT: Save the iterator's absolute stack position for for_of_next
            .for_of_start => {
                try self.writeLine("{");
                self.pushIndent();
                // Push the iterator's absolute stack position onto the stack for nested loops
                // The iterator will be at sp-1 after this block (where the object currently is)
                try self.writeLine("for_of_iter_stack[for_of_depth] = sp - 1;  // Push iterator position");
                try self.writeLine("for_of_depth += 1;");
                // js_frozen_for_of_start expects JSValue stack, but we have CV stack
                // Use temp buffer: [obj] -> call -> [iterator, next_method]
                try self.writeLine("var for_of_buf: [2]JSValue = undefined;");
                try self.writeLine("for_of_buf[0] = stack[sp - 1].toJSValueWithCtx(ctx);");
                try self.writeLine("for_of_buf[1] = JSValue.UNDEFINED;");
                try self.writeLine("const rc = zig_runtime.quickjs.js_frozen_for_of_start(ctx, @ptrCast(&for_of_buf[1]), 0);");
                try self.writeLine("if (rc != 0) {");
                self.pushIndent();
                try self.emitReturnException();
                self.popIndent();
                try self.writeLine("}");
                try self.writeLine("stack[sp - 1] = CV.fromJSValue(for_of_buf[0]);  // iterator replaces obj");
                try self.writeLine("stack[sp] = CV.fromJSValue(for_of_buf[1]); sp += 1;  // next_method");
                try self.writeLine("stack[sp] = CV.fromJSValue(zig_runtime.newCatchOffset(0)); sp += 1;");
                self.popIndent();
                try self.writeLine("}");
            },

            // for_of_next: Get next value from iterator
            // Stack before: [iterator, next_method, catch_offset, ...] (iterator at for_of_iter_base)
            // Stack after: [..., value, done] (sp increases by 2)
            // Use for_of_iter_base (saved by for_of_start) instead of sp-based calculation
            // because sp may change during the loop body
            .for_of_next => {
                try self.writeLine("{");
                self.pushIndent();
                // Use the saved iterator position from the stack (supports nested loops)
                // This is critical because sp changes during the loop body
                try self.writeLine("const iter_idx = for_of_iter_stack[for_of_depth - 1];");
                try self.writeLine("var for_of_buf: [5]JSValue = undefined;");
                try self.writeLine("for_of_buf[0] = stack[iter_idx].toJSValueWithCtx(ctx);      // iterator");
                try self.writeLine("for_of_buf[1] = stack[iter_idx + 1].toJSValueWithCtx(ctx);  // next_method");
                try self.writeLine("for_of_buf[2] = JSValue.UNDEFINED;                // unused slot for C ABI");
                try self.writeLine("for_of_buf[3] = JSValue.UNDEFINED;                // value (output)");
                try self.writeLine("for_of_buf[4] = JSValue.UNDEFINED;                // done (output)");
                try self.writeLine("const rc = zig_runtime.quickjs.js_frozen_for_of_next(ctx, @ptrCast(&for_of_buf[3]), -3);");
                try self.writeLine("if (rc != 0) {");
                self.pushIndent();
                try self.emitReturnException();
                self.popIndent();
                try self.writeLine("}");
                // Write iterator back - js_frozen_for_of_next may have freed it and set to UNDEFINED
                try self.writeLine("stack[iter_idx] = CV.fromJSValue(for_of_buf[0]);");
                try self.writeLine("stack[sp] = CV.fromJSValue(for_of_buf[3]); sp += 1;  // value");
                try self.writeLine("stack[sp] = CV.fromJSValue(for_of_buf[4]); sp += 1;  // done");
                self.popIndent();
                try self.writeLine("}");
            },

            // for_in_start: Start a for-in loop iteration
            // Stack before: [object] at sp-1
            // Stack after: [iterator] at sp-1 (object replaced with iterator)
            .for_in_start => {
                try self.writeLine("{");
                self.pushIndent();
                // js_frozen_for_in_start expects sp[-1] to contain the source object
                // It replaces sp[-1] with the iterator in-place
                // Buffer: [source, UNUSED] - pass &buf[1] so sp[-1] = buf[0]
                try self.writeLine("var for_in_buf: [2]JSValue = .{stack[sp - 1].toJSValueWithCtx(ctx), JSValue.UNDEFINED};");
                try self.writeLine("const rc = zig_runtime.quickjs.js_frozen_for_in_start(ctx, @ptrCast(&for_in_buf[1]));");
                try self.writeLine("if (rc < 0) {");
                self.pushIndent();
                try self.emitReturnException();
                self.popIndent();
                try self.writeLine("}");
                try self.writeLine("stack[sp - 1] = CV.fromJSValue(for_in_buf[0]);  // iterator replaces object");
                self.popIndent();
                try self.writeLine("}");
            },

            // for_in_next: Get next property from for-in iterator
            // Stack before: [iterator] at sp-1
            // Stack after: [iterator, property, done] (sp increases by 2)
            .for_in_next => {
                try self.writeLine("{");
                self.pushIndent();
                // js_frozen_for_in_next expects sp[-1] = iterator, writes sp[0] = property, sp[1] = done
                // Buffer: [iterator, property, done] - pass &buf[1] so sp[-1] = buf[0]
                try self.writeLine("var for_in_buf: [3]JSValue = .{stack[sp - 1].toJSValueWithCtx(ctx), JSValue.UNDEFINED, JSValue.UNDEFINED};");
                try self.writeLine("const rc = zig_runtime.quickjs.js_frozen_for_in_next(ctx, @ptrCast(&for_in_buf[1]));");
                try self.writeLine("if (rc < 0) {");
                self.pushIndent();
                try self.emitReturnException();
                self.popIndent();
                try self.writeLine("}");
                try self.writeLine("stack[sp - 1] = CV.fromJSValue(for_in_buf[0]);  // iterator (unchanged)");
                try self.writeLine("stack[sp] = CV.fromJSValue(for_in_buf[1]); sp += 1;  // property name");
                try self.writeLine("stack[sp] = CV.fromJSValue(for_in_buf[2]); sp += 1;  // done flag");
                self.popIndent();
                try self.writeLine("}");
            },

            // iterator_close: Cleanup iterator after for-of loop
            // Stack before: [iterator, next_method, catch_offset] at for_of_iter_base, +1, +2
            // Stack after: [] (sp set to for_of_iter_base)
            // Use for_of_iter_stack since sp may have drifted during loop body
            .iterator_close => {
                try self.writeLine("{");
                self.pushIndent();
                // Get the iter_base from the stack (supports nested loops)
                try self.writeLine("const _iter_base = for_of_iter_stack[for_of_depth - 1];");
                // Free iterator at _iter_base (only if not already freed by for_of_next on completion)
                try self.writeLine("const _iter = stack[_iter_base];");
                try self.writeLine("if (_iter.isRefType()) JSValue.free(ctx, _iter.toJSValueWithCtx(ctx));");
                // Free next_method at _iter_base + 1
                try self.writeLine("const _next = stack[_iter_base + 1];");
                try self.writeLine("if (_next.isRefType()) JSValue.free(ctx, _next.toJSValueWithCtx(ctx));");
                // catch_offset at _iter_base + 2 doesn't need freeing
                // Restore sp to before the iterator tuple
                try self.writeLine("sp = _iter_base;");
                // Pop from the for-of depth stack
                try self.writeLine("for_of_depth -= 1;");
                self.popIndent();
                try self.writeLine("}");
            },

            // iterator_get_value_done: Extract value and done from iterator result
            // Stack before: [result] at sp-1
            // Stack after: [value, done] (sp increases by 1)
            .iterator_get_value_done => {
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const result = stack[sp - 1].toJSValueWithCtx(ctx);");
                try self.writeLine("const done_val = JSValue.getPropertyStr(ctx, result, \"done\");");
                try self.writeLine("const value_val = JSValue.getPropertyStr(ctx, result, \"value\");");
                try self.writeLine("// Don't free result - CV doesn't own the reference");
                try self.writeLine("stack[sp - 1] = CV.fromJSValue(value_val);");
                try self.writeLine("stack[sp] = CV.fromJSValue(done_val);");
                try self.writeLine("sp += 1;");
                self.popIndent();
                try self.writeLine("}");
            },

            // ================================================================
            // Arithmetic Operations (migrated from C codegen)
            // ================================================================

            // plus: unary plus - convert to number (ToNumber)
            // Stack: [value] -> [number]
            // Note: If value is already a number, keep it. Otherwise convert to float.
            .plus => {
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const _plus_v = stack[sp - 1].toJSValueWithCtx(ctx);");
                try self.writeLine("if (_plus_v.isInt() or _plus_v.isFloat64()) {");
                self.pushIndent();
                try self.writeLine("// Already a number, no conversion needed");
                self.popIndent();
                try self.writeLine("} else {");
                self.pushIndent();
                try self.writeLine("var _plus_f: f64 = 0;");
                try self.writeLine("_ = JSValue.toFloat64(ctx, &_plus_f, _plus_v);");
                try self.writeLine("stack[sp - 1] = CV.fromJSValue(JSValue.newFloat64(_plus_f));");
                self.popIndent();
                try self.writeLine("}");
                self.popIndent();
                try self.writeLine("}");
            },

            // ================================================================
            // Function Call Operations (migrated from C codegen)
            // ================================================================

            // apply: Function.prototype.apply(thisArg, argsArray)
            // Stack: [func, thisArg, argsArray] -> [result]
            .apply => {
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const _apply_args_array = stack[sp - 1].toJSValueWithCtx(ctx);");
                try self.writeLine("const _apply_this_obj = stack[sp - 2].toJSValueWithCtx(ctx);");
                try self.writeLine("const _apply_func = CV.toJSValuePtr(&stack[sp - 3]);");
                try self.writeLine("sp -= 3;");
                try self.writeLine("");
                try self.writeLine("// Handle undefined/null args array (call with no args per JS spec)");
                try self.writeLine("if (_apply_args_array.isUndefined() or _apply_args_array.isNull()) {");
                self.pushIndent();
                try self.writeLine("var _apply_empty_args: [1]JSValue = .{JSValue.UNDEFINED};");
                // Sync locals TO _locals_jsv before call (ensures current values for round-trip)
                try self.emitClosureVarSyncTo();
                try self.writeLine("const _apply_result = JSValue.call(ctx, _apply_func, _apply_this_obj, 0, &_apply_empty_args);");
                try self.emitInlineExceptionReturn("_apply_result");
                try self.writeLine("stack[sp] = CV.fromJSValue(_apply_result);");
                try self.writeLine("sp += 1;");
                // Sync closure variables back from _locals_jsv (closure may have written to them)
                try self.emitClosureVarSync();
                self.popIndent();
                try self.writeLine("} else {");
                self.pushIndent();
                try self.writeLine("// Get array length as i32 (arrays can't exceed 2^32)");
                try self.writeLine("const _apply_len_val = JSValue.getPropertyStr(ctx, _apply_args_array, \"length\");");
                try self.writeLine("var _apply_arg_count: i32 = 0;");
                try self.writeLine("_ = JSValue.toInt32(ctx, &_apply_arg_count, _apply_len_val);");
                try self.writeLine("JSValue.free(ctx, _apply_len_val);");
                try self.writeLine("");
                try self.writeLine("// Extract args from array");
                try self.writeLine("var _apply_argv_buf: [32]JSValue = undefined;");
                try self.writeLine("const _apply_count_u: usize = if (_apply_arg_count > 0) @intCast(_apply_arg_count) else 0;");
                if (self.dispatch_mode) {
                    try self.writeLine("if (_apply_count_u > 32) return .{ .return_value = JSValue.throwTypeError(ctx, \"apply: too many arguments\") };");
                } else {
                    try self.writeLine("if (_apply_count_u > 32) return JSValue.throwTypeError(ctx, \"apply: too many arguments\");");
                }
                try self.writeLine("var _apply_idx: usize = 0;");
                try self.writeLine("while (_apply_idx < _apply_count_u) : (_apply_idx += 1) {");
                self.pushIndent();
                try self.writeLine("_apply_argv_buf[_apply_idx] = JSValue.getPropertyUint32(ctx, _apply_args_array, @intCast(_apply_idx));");
                self.popIndent();
                try self.writeLine("}");
                try self.writeLine("");
                try self.writeLine("// Call function with extracted args");
                // Sync locals TO _locals_jsv before call (ensures current values for round-trip)
                try self.emitClosureVarSyncTo();
                try self.writeLine("const _apply_result = JSValue.call(ctx, _apply_func, _apply_this_obj, _apply_arg_count, &_apply_argv_buf);");
                try self.writeLine("");
                try self.writeLine("// Free extracted args");
                try self.writeLine("_apply_idx = 0;");
                try self.writeLine("while (_apply_idx < _apply_count_u) : (_apply_idx += 1) {");
                self.pushIndent();
                try self.writeLine("JSValue.free(ctx, _apply_argv_buf[_apply_idx]);");
                self.popIndent();
                try self.writeLine("}");
                try self.writeLine("");
                try self.emitInlineExceptionReturn("_apply_result");
                try self.writeLine("stack[sp] = CV.fromJSValue(_apply_result);");
                try self.writeLine("sp += 1;");
                // Sync closure variables back from _locals_jsv (closure may have written to them)
                try self.emitClosureVarSync();
                self.popIndent();
                try self.writeLine("}");
                self.popIndent();
                try self.writeLine("}");
            },

            // ================================================================
            // Object Operations (migrated from C codegen)
            // ================================================================

            // copy_data_properties: Object spread with exclusion list
            // Mask encodes: bits 0-1 = target offset - 1, bits 2-4 = source offset - 1, bits 5-7 = exclude offset - 1
            // Stack layout varies but typically: [target, source, excludeList]
            .copy_data_properties => {
                const mask = instr.operand.u8;
                const target_off = @as(i32, @intCast(mask & 3)) + 1;
                const source_off = @as(i32, @intCast((mask >> 2) & 7)) + 1;
                const exclude_off = @as(i32, @intCast((mask >> 5) & 7)) + 1;

                try self.writeLine("{");
                self.pushIndent();
                // Get all three values from stack using mask-encoded offsets
                try self.printLine("const _cdp_target = stack[sp - {d}].toJSValueWithCtx(ctx);", .{target_off});
                try self.printLine("const _cdp_source = stack[sp - {d}].toJSValueWithCtx(ctx);", .{source_off});
                try self.printLine("const _cdp_excluded = stack[sp - {d}].toJSValueWithCtx(ctx);", .{exclude_off});
                try self.writeLine("_ = zig_runtime.copyDataProperties(ctx, _cdp_target, _cdp_source, _cdp_excluded);");
                // Don't modify sp - stack cleanup is handled by subsequent bytecode instructions
                self.popIndent();
                try self.writeLine("}");
            },

            // Async/await support
            // await: Pop the value to await, convert to promise, and return it.
            // QuickJS async machinery handles promise resolution and continuation.
            .await => {
                const awaited_expr = self.vpop() orelse "stack[sp - 1]";
                const should_free = self.isAllocated(awaited_expr);
                defer if (should_free) self.allocator.free(awaited_expr);

                try self.writeLine("{");
                self.pushIndent();
                try self.printLine("const _awaited_cv = {s};", .{awaited_expr});
                try self.writeLine("const _awaited = _awaited_cv.toJSValueWithCtx(ctx);");
                try self.writeLine("const _promise = zig_runtime.js_value.toPromise(ctx, _awaited);");
                try self.emitVarRefDetach();
                if (self.dispatch_mode) {
                    try self.writeLine("return .{ .return_value = _promise };");
                } else {
                    try self.writeLine("return _promise;");
                }
                self.popIndent();
                try self.writeLine("}");
                self.block_terminated = true;
                return false; // Control terminates
            },

            // return_async: Return from async function with the given value.
            // The return value becomes the resolution value of the async function's promise.
            // IMPORTANT: Must wrap return value in Promise.resolve() since async functions always return Promises
            .return_async => {
                const result = self.vpop() orelse "stack[sp - 1]";
                const should_free = self.isAllocated(result);
                defer if (should_free) self.allocator.free(result);

                try self.printLine("{{ const _ret_cv = {s}; const _ret_val = _ret_cv.toJSValueWithCtx(ctx);", .{result});
                try self.emitVarRefDetach();
                // Free arg_shadow values before returning (they were duped at function entry)
                if (self.has_put_arg) {
                    const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
                    for (0..arg_count) |i| {
                        try self.printLine("if (arg_shadow[{d}].isRefType()) JSValue.free(ctx, arg_shadow[{d}].toJSValueWithCtx(ctx));", .{ i, i });
                    }
                }
                try self.emitLocalsCleanup();
                // Async functions ALWAYS wrap return value in Promise.resolve()
                if (self.dispatch_mode) {
                    try self.writeLine("return .{ .return_value = JSValue.promiseResolve(ctx, _ret_val) }; }");
                } else {
                    try self.writeLine("return JSValue.promiseResolve(ctx, _ret_val); }");
                }
                self.block_terminated = true;
                return false; // Control terminates
            },

            // set_proto: [obj, proto] -> [obj] (sets obj.__proto__ = proto)
            .set_proto => {
                if (self.vstack.items.len > 0) {
                    try self.materializeVStack();
                }
                try self.writeLine("{");
                try self.writeLine("    const proto = stack[sp - 1].toJSValueWithCtx(ctx);");
                try self.writeLine("    const obj = stack[sp - 2].toJSValueWithCtx(ctx);");
                try self.writeLine("    _ = zig_runtime.quickjs.js_frozen_set_proto(ctx, obj, proto);");
                try self.writeLine("    if (stack[sp - 1].isRefType()) JSValue.free(ctx, proto);");
                try self.writeLine("    sp -= 1;");
                try self.writeLine("}");
            },

            // Fallback: emit single-opcode interpreter call, then continue
            else => {
                const op_byte = @intFromEnum(instr.opcode);
                if (op_byte < 256) {
                    unsupported_opcode_counts[op_byte] += 1;
                }
                // Emit block-level interpreter fallback for this single opcode
                try self.emitBlockInterpreterFallback(instr, block_instrs, instr_idx);
                // Continue to next instruction (control does not terminate)
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
            .add, .sub, .mul, .div, .mod, .neg, .plus,
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
            .call_constructor, .tail_call, .tail_call_method, .apply,
            // Object/Array creation
            .array_from, .define_method, .set_name, .copy_data_properties,
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

                // Pop operations (net -1) - put_* consumes value from stack
                .drop,
                .put_arg0, .put_arg1, .put_arg2, .put_arg3, .put_arg,
                .put_loc0, .put_loc1, .put_loc2, .put_loc3, .put_loc,
                .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3, .put_var_ref,
                => net_stack_push -= 1,

                // Binary operations: pop 2, push 1 (net -1)
                .add, .sub, .mul, .div, .mod,
                .eq, .neq, .strict_eq, .strict_neq, .lt, .lte, .gt, .gte,
                .shl, .sar, .shr, .@"or", .@"and", .xor,
                => net_stack_push -= 1,

                // Unary operations and set_* (pop 1, push 1 = net 0)
                // set_* keeps value on stack while storing to loc/arg/var_ref
                .neg, .not, .lnot, .typeof, .inc, .dec, .post_inc, .post_dec,
                .set_arg0, .set_arg1, .set_arg2, .set_arg3, .set_arg,
                .set_loc0, .set_loc1, .set_loc2, .set_loc3, .set_loc,
                .set_var_ref0, .set_var_ref1, .set_var_ref2, .set_var_ref3, .set_var_ref,
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

    /// Check if a self-recursive function only uses its self-reference for direct calls.
    /// Returns true if safe to codegen (all self-refs lead to calls), false if self-ref
    /// is used for other purposes (e.g., passed as callback, assigned to variable).
    fn isSelfReferenceOnlyForCalls(self: *Self) bool {
        // If not self-recursive or no self-ref var, it's safe
        if (!self.func.is_self_recursive or self.func.self_ref_var_idx < 0) return true;

        const self_ref_idx: u16 = @intCast(self.func.self_ref_var_idx);

        // Scan all blocks for self-reference accesses
        for (self.func.cfg.blocks.items) |block| {
            const instrs = block.instructions;
            for (instrs, 0..) |instr, idx| {
                // Check if this instruction accesses the self-reference variable
                const accesses_self_ref = switch (instr.opcode) {
                    .get_var_ref0 => self_ref_idx == 0,
                    .get_var_ref1 => self_ref_idx == 1,
                    .get_var_ref2 => self_ref_idx == 2,
                    .get_var_ref3 => self_ref_idx == 3,
                    .get_var_ref => instr.operand.var_ref == self_ref_idx,
                    else => false,
                };

                if (accesses_self_ref) {
                    // Check if this self-ref access is followed by a self-call
                    if (!self.isFollowedBySelfCall(instrs, idx)) {
                        // Self-ref used for something other than a call (e.g., callback)
                        if (CODEGEN_DEBUG) std.debug.print("[isSelfReferenceOnlyForCalls] {s}: self-ref at idx {d} not followed by call\n", .{ self.func.name, idx });
                        return false;
                    }
                }
            }
        }

        return true;
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

    /// Emit sync code to copy locals to _locals_jsv BEFORE a call.
    /// This ensures _locals_jsv has current values before we potentially restore from it.
    /// Without this, modifications to locals between closure creation and subsequent calls
    /// (e.g., loop counter increments) would be lost when we sync FROM.
    fn emitClosureVarSyncTo(self: *Self) !void {
        if (self.has_fclosure and self.func.var_count > 0 and self.closure_created_in_block) {
            try self.printLine("for (0..{d}) |_i| {{ {s}[_i] = CV.toJSValuePtr(&locals[_i]); }}", .{ self.func.var_count, self.getLocalsJsvExpr() });
        }
    }

    /// Emit sync code to copy closure variable updates from _locals_jsv back to locals.
    /// This is needed after any call that might execute a closure that writes to captured variables.
    /// The closure writes to var_refs which point into _locals_jsv, but the parent reads from locals.
    /// IMPORTANT: Only emit this after a closure has been created (closure_created_in_block is true),
    /// otherwise we'd overwrite valid locals with UNDEFINED since _locals_jsv is initialized to UNDEFINED.
    fn emitClosureVarSyncFrom(self: *Self) !void {
        if (self.has_fclosure and self.func.var_count > 0 and self.closure_created_in_block) {
            try self.printLine("for (0..{d}) |_i| {{ locals[_i] = CV.fromJSValue({s}[_i]); }}", .{ self.func.var_count, self.getLocalsJsvExpr() });
        }
    }

    /// Emit bidirectional sync: TO before call, FROM after call.
    /// This ensures the round-trip preserves current local values while still
    /// picking up any modifications made by closures.
    fn emitClosureVarSync(self: *Self) !void {
        // Now just emits sync FROM (sync TO is called before the call)
        try self.emitClosureVarSyncFrom();
    }

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
                // No args - direct call (pass through closure context)
                // Sync locals TO _locals_jsv before call (ensures current values for round-trip)
                try self.emitClosureVarSyncTo();
                if (needs_escape) {
                    try self.printLine("const result = @\"__frozen_{s}\"(ctx, JSValue.UNDEFINED, 0, undefined, var_refs, closure_var_count, cpool);", .{self.func.name});
                } else {
                    try self.printLine("const result = __frozen_{s}(ctx, JSValue.UNDEFINED, 0, undefined, var_refs, closure_var_count, cpool);", .{self.func.name});
                }
                try self.writeLine("stack[sp] = result; sp += 1;");
                // Sync closure variables back from _locals_jsv (closure may have written to them)
                try self.emitClosureVarSync();
                // Sync vstack: push result expression
                try self.vpush("stack[sp - 1]");
            } else {
                // With args - copy from stack (no func on stack for self-call)
                try self.printLine("var args: [{d}]JSValue = undefined;", .{argc});
                for (0..argc) |i| {
                    try self.printLine("args[{d}] = CV.toJSValuePtr(&stack[sp - {d}]);", .{ i, argc - i });
                }
                // Sync locals TO _locals_jsv before call (ensures current values for round-trip)
                try self.emitClosureVarSyncTo();
                if (needs_escape) {
                    try self.printLine("const call_result = @\"__frozen_{s}\"(ctx, JSValue.UNDEFINED, {d}, &args, var_refs, closure_var_count, cpool);", .{ self.func.name, argc });
                } else {
                    try self.printLine("const call_result = __frozen_{s}(ctx, JSValue.UNDEFINED, {d}, &args, var_refs, closure_var_count, cpool);", .{ self.func.name, argc });
                }
                // Store result directly - call returns a new ref, no dup needed
                try self.writeLine("const result = CV.fromJSValue(call_result);");
                // Free the CVs we're abandoning (collect-then-free to avoid g_return_slot corruption)
                try self.printLine("var _to_free: [{d}]JSValue = undefined;", .{argc});
                try self.writeLine("var _free_count: usize = 0;");
                for (0..argc) |i| {
                    try self.printLine("{{ const v = stack[sp - {d}]; if (v.isRefType()) {{ _to_free[_free_count] = v.toJSValueWithCtx(ctx); _free_count += 1; }} }}", .{argc - i});
                }
                try self.writeLine("for (0.._free_count) |_fi| { JSValue.free(ctx, _to_free[_fi]); }");
                try self.printLine("sp -= {d};", .{argc});
                try self.writeLine("stack[sp] = result; sp += 1;");
                // Sync closure variables back from _locals_jsv (closure may have written to them)
                try self.emitClosureVarSync();
                // Sync vstack: clear argc args, push result
                for (0..argc) |_| {
                    self.vpopAndFree();
                }
                try self.vpush("stack[sp - 1]");
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
            try self.writeLine("const func = CV.toJSValuePtr(&stack[sp - 1]);");
            try self.writeLine("var no_args: [0]JSValue = undefined;");
            // Sync locals TO _locals_jsv before call (ensures current values for round-trip)
            try self.emitClosureVarSyncTo();
            try self.writeLine("const call_result = JSValue.call(ctx, func, JSValue.UNDEFINED, 0, @ptrCast(&no_args));");
            // Check for exception and propagate it
            try self.emitInlineExceptionReturn("call_result");
            // Free the func CV we're about to overwrite
            try self.writeLine("{ const v = stack[sp - 1]; if (v.isRefType()) JSValue.free(ctx, v.toJSValueWithCtx(ctx)); }");
            try self.writeLine("stack[sp - 1] = CV.fromJSValue(call_result);");
            // Sync closure variables back from _locals_jsv (closure may have written to them)
            try self.emitClosureVarSync();
            // Sync vstack: clear func, push result
            self.vpopAndFree();
            try self.vpush("stack[sp - 1]");
            self.popIndent();
            try self.writeLine("}");
        } else {
            // callN: func at sp-1-argc, args at sp-argc..sp-1
            try self.writeLine("{");
            self.pushIndent();
            try self.printLine("const func = CV.toJSValuePtr(&stack[sp - 1 - {d}]);", .{argc});
            try self.printLine("var args: [{d}]JSValue = undefined;", .{argc});
            // Copy args from stack to args array
            for (0..argc) |i| {
                try self.printLine("args[{d}] = CV.toJSValuePtr(&stack[sp - {d}]);", .{ i, argc - i });
            }
            // Sync locals TO _locals_jsv before call (ensures current values for round-trip)
            try self.emitClosureVarSyncTo();
            // Call - JS_Call returns a new ref, no dup needed
            try self.printLine("const call_result = JSValue.call(ctx, func, JSValue.UNDEFINED, {d}, @ptrCast(&args));", .{argc});
            // Check for exception and propagate it
            try self.emitInlineExceptionReturn("call_result");
            try self.writeLine("const result = CV.fromJSValue(call_result);");
            // Free the CVs we're abandoning (collect-then-free to avoid g_return_slot corruption)
            try self.printLine("var _to_free: [{d}]JSValue = undefined;", .{argc + 1});
            try self.writeLine("var _free_count: usize = 0;");
            for (0..argc) |i| {
                try self.printLine("{{ const v = stack[sp - {d}]; if (v.isRefType()) {{ _to_free[_free_count] = v.toJSValueWithCtx(ctx); _free_count += 1; }} }}", .{argc - i});
            }
            try self.printLine("{{ const v = stack[sp - 1 - {d}]; if (v.isRefType()) {{ _to_free[_free_count] = v.toJSValueWithCtx(ctx); _free_count += 1; }} }}", .{argc}); // func
            try self.writeLine("for (0.._free_count) |_fi| { JSValue.free(ctx, _to_free[_fi]); }");
            try self.printLine("sp -= {d};", .{argc});
            try self.writeLine("stack[sp - 1] = result;");
            // Sync closure variables back from _locals_jsv (closure may have written to them)
            try self.emitClosureVarSync();
            // Sync vstack: clear func + argc args, push result
            for (0..argc + 1) |_| {
                self.vpopAndFree();
            }
            try self.vpush("stack[sp - 1]");
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

    /// Check if this instruction should be skipped because it's part of a native Array.push pattern
    /// Returns true if this is get_field2("push") that will be handled by tryEmitNativeArrayPush
    fn shouldSkipForNativeArrayPush(self: *Self, instrs: []const Instruction, idx: usize) bool {
        if (idx >= instrs.len) return false;
        const instr = instrs[idx];

        // Only skip get_field2("push")
        if (instr.opcode != .get_field2) return false;

        const method_name = self.getAtomString(instr.operand.atom) orelse return false;
        if (!std.mem.eql(u8, method_name, "push")) return false;

        // Look ahead for call_method with argc >= 1
        var i: usize = idx + 1;
        while (i < instrs.len) : (i += 1) {
            const future_instr = instrs[i];
            if (future_instr.opcode == .call_method) {
                // Native array push needs at least 1 argument
                if (future_instr.operand.u16 >= 1) {
                    return true; // Skip this get_field2("push")
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
        return false;
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
            try self.writeLine("stack[sp - 1] = CV.newFloat(@floor(f + 0.5));");
            self.popIndent();
            try self.writeLine("}");
            return true;
        }

        return false;
    }

    /// Try to emit native Array.push(val...) instead of going through QuickJS
    /// Returns true if native code was emitted, false otherwise
    /// Pattern: get_field2("push") -> [val computations] -> call_method(argc)
    /// Stack before (skipped): [arr, val0, val1, ...] Stack after: [newLength]
    /// Stack before (not skipped): [arr, arr.push, val0, val1, ...] Stack after: [newLength]
    /// Supports any number of arguments (argc >= 1)
    fn tryEmitNativeArrayPush(self: *Self, argc: u16, instrs: []const Instruction, call_idx: usize) !bool {
        // Need at least 1 argument
        if (argc == 0) return false;

        // Search backwards from call_method to find get_field2("push")
        var i: usize = call_idx;
        var get_field2_idx: ?usize = null;
        while (i > 0) : (i -= 1) {
            const instr = instrs[i - 1];
            if (instr.opcode == .get_field2) {
                const atom = instr.operand.atom;
                if (self.getAtomString(atom)) |name| {
                    if (std.mem.eql(u8, name, "push")) {
                        get_field2_idx = i - 1;
                        break;
                    }
                }
                // Not push - stop searching
                break;
            }
            // Stop at any other call or mutation - but NOT get_field since it
            // just transforms the stack top (obj → obj.field) without changing depth.
            // The forward search in shouldSkipForNativeArrayPush doesn't stop at get_field
            // either, so both searches must be consistent.
            if (instr.opcode == .call or instr.opcode == .call_method or
                instr.opcode == .put_field)
            {
                break;
            }
        }

        if (get_field2_idx == null) return false;

        // Check if the get_field2("push") was skipped by shouldSkipForNativeArrayPush
        // by looking forward from it to see if there's a call_method
        const was_skipped = self.shouldSkipForNativeArrayPush(instrs, get_field2_idx.?);

        if (was_skipped) {
            // get_field2("push") was SKIPPED - stack has [arr, val0, val1, ...]
            // Array is at stack[sp - 1 - argc], values at stack[sp - argc]..stack[sp - 1]
            try self.printLine("{{ // Native Array.push inline ({d} args, skipped get_field2)", .{argc});
            self.pushIndent();
            try self.printLine("const arr = stack[sp - 1 - {d}].toJSValueWithCtx(ctx);", .{argc});
            try self.writeLine("var len: i64 = 0;");
            try self.writeLine("_ = zig_runtime.quickjs.JS_GetLength(ctx, arr, &len);");

            // Push each argument at consecutive indices
            // CRITICAL: Dup values before passing to setPropertyUint32 - it consumes the value
            for (0..argc) |arg_idx| {
                try self.printLine("_ = JSValue.setPropertyUint32(ctx, arr, @intCast(len + {d}), JSValue.dup(ctx, stack[sp - {d}].toJSValueWithCtx(ctx)));", .{ arg_idx, argc - arg_idx });
            }

            try self.printLine("const new_len = JSValue.newInt64(ctx, len + {d});", .{argc});
            // Free arg values (they were duped for setPropertyUint32, originals still on stack)
            for (0..argc) |arg_idx| {
                try self.printLine("{{ const _arg_cv = stack[sp - {d}]; if (_arg_cv.isRefType()) JSValue.free(ctx, _arg_cv.toJSValueWithCtx(ctx)); }}", .{argc - arg_idx});
            }
            // Free the array reference (it was duped when read from local)
            try self.printLine("{{ const arr_cv = stack[sp - 1 - {d}]; if (arr_cv.isRefType()) JSValue.free(ctx, arr_cv.toJSValueWithCtx(ctx)); }}", .{argc});
            try self.printLine("sp -= {d};", .{argc + 1}); // Pop arr and all args (no method)
            try self.writeLine("stack[sp] = CV.fromJSValue(new_len);");
            try self.writeLine("sp += 1;");
            self.popIndent();
            try self.writeLine("}");
            // Sync vstack: pop arr + argc args, push result reference
            for (0..argc + 1) |_| {
                self.vpopAndFree();
            }
            try self.vpush("stack[sp - 1]");
        } else {
            // get_field2("push") was NOT skipped - stack has [arr, arr.push, val0, val1, ...]
            try self.printLine("{{ // Native Array.push inline ({d} args)", .{argc});
            self.pushIndent();
            try self.printLine("const arr = stack[sp - 2 - {d}].toJSValueWithCtx(ctx);", .{argc});
            try self.writeLine("var len: i64 = 0;");
            try self.writeLine("_ = zig_runtime.quickjs.JS_GetLength(ctx, arr, &len);");

            // Push each argument at consecutive indices
            // CRITICAL: Dup values before passing to setPropertyUint32 - it consumes the value
            for (0..argc) |arg_idx| {
                try self.printLine("_ = JSValue.setPropertyUint32(ctx, arr, @intCast(len + {d}), JSValue.dup(ctx, stack[sp - {d}].toJSValueWithCtx(ctx)));", .{ arg_idx, argc - arg_idx });
            }

            try self.printLine("const new_len = JSValue.newInt64(ctx, len + {d});", .{argc});
            // Free arg values (they were duped for setPropertyUint32, originals still on stack)
            for (0..argc) |arg_idx| {
                try self.printLine("{{ const _arg_cv = stack[sp - {d}]; if (_arg_cv.isRefType()) JSValue.free(ctx, _arg_cv.toJSValueWithCtx(ctx)); }}", .{argc - arg_idx});
            }
            // Free method (arr.push function) and the array reference (it was duped when read from local)
            try self.printLine("JSValue.free(ctx, stack[sp - 1 - {d}].toJSValueWithCtx(ctx));", .{argc});
            try self.printLine("{{ const arr_cv = stack[sp - 2 - {d}]; if (arr_cv.isRefType()) JSValue.free(ctx, arr_cv.toJSValueWithCtx(ctx)); }}", .{argc});
            try self.printLine("sp -= {d};", .{argc + 2}); // Pop arr, method, and all args
            try self.writeLine("stack[sp] = CV.fromJSValue(new_len);");
            try self.writeLine("sp += 1;");
            self.popIndent();
            try self.writeLine("}");
            // Sync vstack: pop arr + method + argc args, push result reference
            for (0..argc + 2) |_| {
                self.vpopAndFree();
            }
            try self.vpush("stack[sp - 1]");
        }

        return true;
    }

    /// Try to emit native String.charCodeAt(index) instead of going through QuickJS
    /// Returns true if native code was emitted, false otherwise
    /// Pattern: get_field2("charCodeAt") -> [index computation] -> call_method(1)
    /// Stack before: [str, str.charCodeAt, index]  Stack after: [charCode]
    fn tryEmitNativeCharCodeAt(self: *Self, argc: u16, instrs: []const Instruction, call_idx: usize) !bool {
        // charCodeAt takes exactly 1 argument
        if (argc != 1) return false;

        // Search backwards from call_method to find get_field2("charCodeAt")
        var i: usize = call_idx;
        while (i > 0) : (i -= 1) {
            const instr = instrs[i - 1];
            if (instr.opcode == .get_field2) {
                const atom = instr.operand.atom;
                if (self.getAtomString(atom)) |name| {
                    if (std.mem.eql(u8, name, "charCodeAt")) {
                        // Found str.charCodeAt(idx) pattern - emit native code
                        // Stack: [str, str.charCodeAt, idx] at sp-3, sp-2, sp-1
                        try self.writeLine("{ // Native String.charCodeAt inline");
                        self.pushIndent();
                        try self.writeLine("const str_val = stack[sp - 3].toJSValueWithCtx(ctx);");
                        try self.writeLine("const idx_val = stack[sp - 1].toJSValueWithCtx(ctx);");
                        try self.writeLine("var str_len: usize = 0;");
                        try self.writeLine("const str_ptr = zig_runtime.quickjs.JS_ToCStringLen(ctx, &str_len, str_val);");
                        try self.writeLine("var idx: i32 = 0;");
                        try self.writeLine("_ = JSValue.toInt32(ctx, &idx, idx_val);");
                        try self.writeLine("var result: JSValue = undefined;");
                        try self.writeLine("if (str_ptr != null and idx >= 0 and @as(usize, @intCast(idx)) < str_len) {");
                        self.pushIndent();
                        try self.writeLine("result = JSValue.newInt(@intCast(str_ptr.?[@intCast(idx)]));");
                        self.popIndent();
                        try self.writeLine("} else {");
                        self.pushIndent();
                        try self.writeLine("result = JSValue.newFloat64(std.math.nan(f64));");
                        self.popIndent();
                        try self.writeLine("}");
                        try self.writeLine("if (str_ptr) |p| zig_runtime.quickjs.JS_FreeCString(ctx, p);");
                        // Free method and index, but NOT str (still live)
                        try self.writeLine("JSValue.free(ctx, stack[sp - 2].toJSValueWithCtx(ctx));");
                        try self.writeLine("JSValue.free(ctx, idx_val);");
                        try self.writeLine("sp -= 3;");
                        try self.writeLine("stack[sp] = CV.fromJSValue(result);");
                        try self.writeLine("sp += 1;");
                        self.popIndent();
                        try self.writeLine("}");
                        return true;
                    }
                }
                break;
            }
            if (instr.opcode == .call or instr.opcode == .call_method or
                instr.opcode == .get_field or instr.opcode == .put_field)
            {
                break;
            }
        }

        return false;
    }

    /// Try to emit native String.slice/substring instead of going through QuickJS
    /// Returns true if native code was emitted, false otherwise
    /// Pattern: get_field2("slice"|"substring") -> [start, end?] -> call_method(1|2)
    /// Stack before: [str, str.slice, start, end?]  Stack after: [substring]
    fn tryEmitNativeStringSlice(self: *Self, argc: u16, instrs: []const Instruction, call_idx: usize) !bool {
        // slice/substring takes 1 or 2 arguments
        if (argc != 1 and argc != 2) return false;

        // Search backwards from call_method to find get_field2("slice"|"substring")
        var i: usize = call_idx;
        while (i > 0) : (i -= 1) {
            const instr = instrs[i - 1];
            if (instr.opcode == .get_field2) {
                const atom = instr.operand.atom;
                if (self.getAtomString(atom)) |name| {
                    if (std.mem.eql(u8, name, "slice") or std.mem.eql(u8, name, "substring")) {
                        // Found .slice(start, end?) pattern - emit native code with runtime type guard
                        // IMPORTANT: .slice() can be called on strings OR arrays. Only inline for strings;
                        // fall back to generic call_method for arrays and other types.
                        try self.printLine("{{ // Guarded String.{s} inline ({d} args)", .{ name, argc });
                        self.pushIndent();
                        try self.printLine("const _so = stack[sp - 2 - {d}].toJSValueWithCtx(ctx);", .{argc});
                        try self.writeLine("if (_so.isString()) {");
                        self.pushIndent();

                        // === FAST PATH: inline string slice ===
                        try self.writeLine("const str_val = _so;");
                        try self.writeLine("var str_len: usize = 0;");
                        try self.writeLine("const str_ptr = zig_runtime.quickjs.JS_ToCStringLen(ctx, &str_len, str_val);");
                        try self.writeLine("var start_raw: i32 = 0;");
                        try self.printLine("_ = JSValue.toInt32(ctx, &start_raw, stack[sp - {d}].toJSValueWithCtx(ctx));", .{argc});

                        if (argc == 2) {
                            try self.writeLine("var end_raw: i32 = 0;");
                            try self.writeLine("_ = JSValue.toInt32(ctx, &end_raw, stack[sp - 1].toJSValueWithCtx(ctx));");
                        }

                        try self.writeLine("var result: JSValue = undefined;");
                        try self.writeLine("if (str_ptr) |s| {");
                        self.pushIndent();
                        try self.writeLine("const slen: i32 = @intCast(str_len);");
                        // Handle negative indices for slice
                        if (std.mem.eql(u8, name, "slice")) {
                            try self.writeLine("const start: usize = @intCast(if (start_raw < 0) @max(0, slen + start_raw) else @min(start_raw, slen));");
                            if (argc == 2) {
                                try self.writeLine("const end: usize = @intCast(if (end_raw < 0) @max(0, slen + end_raw) else @min(end_raw, slen));");
                            } else {
                                try self.writeLine("const end: usize = str_len;");
                            }
                        } else {
                            // substring clamps to 0 and swaps if start > end
                            try self.writeLine("const s1: usize = @intCast(@max(0, @min(start_raw, slen)));");
                            if (argc == 2) {
                                try self.writeLine("const s2: usize = @intCast(@max(0, @min(end_raw, slen)));");
                            } else {
                                try self.writeLine("const s2: usize = str_len;");
                            }
                            try self.writeLine("const start = @min(s1, s2);");
                            try self.writeLine("const end = @max(s1, s2);");
                        }
                        try self.writeLine("if (start <= end and end <= str_len) {");
                        self.pushIndent();
                        try self.writeLine("result = JSValue.newStringLen(ctx, s + start, end - start);");
                        self.popIndent();
                        try self.writeLine("} else {");
                        self.pushIndent();
                        try self.writeLine("result = JSValue.newString(ctx, \"\");");
                        self.popIndent();
                        try self.writeLine("}");
                        try self.writeLine("zig_runtime.quickjs.JS_FreeCString(ctx, s);");
                        self.popIndent();
                        try self.writeLine("} else {");
                        self.pushIndent();
                        try self.writeLine("result = JSValue.newString(ctx, \"\");");
                        self.popIndent();
                        try self.writeLine("}");
                        // Free method and args
                        try self.printLine("JSValue.free(ctx, stack[sp - 1 - {d}].toJSValueWithCtx(ctx));", .{argc}); // method
                        for (0..argc) |arg_i| {
                            try self.printLine("JSValue.free(ctx, stack[sp - {d}].toJSValueWithCtx(ctx));", .{argc - arg_i});
                        }
                        try self.printLine("sp -= {d};", .{argc + 2});
                        try self.writeLine("stack[sp] = CV.fromJSValue(result);");
                        try self.writeLine("sp += 1;");

                        self.popIndent();
                        try self.writeLine("} else {");
                        self.pushIndent();

                        // === FALLBACK: generic call_method for non-string types (arrays, etc.) ===
                        try self.printLine("const _m = stack[sp - 1 - {d}].toJSValueWithCtx(ctx);", .{argc});
                        try self.writeLine("const _ct = _so;");
                        try self.printLine("var _args: [{d}]JSValue = undefined;", .{argc});
                        for (0..argc) |arg_i| {
                            try self.printLine("_args[{d}] = CV.toJSValuePtr(&stack[sp - {d}]);", .{ arg_i, argc - arg_i });
                        }
                        try self.printLine("const _cr = JSValue.call(ctx, _m, _ct, {d}, &_args);", .{argc});
                        // Check for exception and propagate
                        try self.emitInlineExceptionReturn("_cr");
                        // Free method, args, and this (collect-then-free pattern)
                        try self.printLine("var _tf: [{d}]JSValue = undefined;", .{argc + 2});
                        try self.writeLine("var _fc: usize = 0;");
                        for (0..argc) |arg_i| {
                            try self.printLine("{{ const _v = stack[sp - {d}]; if (_v.isRefType()) {{ _tf[_fc] = _v.toJSValueWithCtx(ctx); _fc += 1; }} }}", .{argc - arg_i});
                        }
                        try self.printLine("{{ const _v = stack[sp - 1 - {d}]; if (_v.isRefType()) {{ _tf[_fc] = _v.toJSValueWithCtx(ctx); _fc += 1; }} }}", .{argc}); // method
                        try self.printLine("{{ const _v = stack[sp - 2 - {d}]; if (_v.isRefType()) {{ _tf[_fc] = _v.toJSValueWithCtx(ctx); _fc += 1; }} }}", .{argc}); // this
                        try self.writeLine("for (0.._fc) |_fi| { JSValue.free(ctx, _tf[_fi]); }");
                        try self.printLine("sp -= {d};", .{argc + 2});
                        try self.writeLine("stack[sp] = CV.fromJSValue(_cr);");
                        try self.writeLine("sp += 1;");

                        self.popIndent();
                        try self.writeLine("}");
                        self.popIndent();
                        try self.writeLine("}");
                        return true;
                    }
                }
                break;
            }
            if (instr.opcode == .call or instr.opcode == .call_method or
                instr.opcode == .get_field or instr.opcode == .put_field)
            {
                break;
            }
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
        try self.printLine("const method = stack[sp - 1 - {d}].toJSValueWithCtx(ctx);", .{argc});
        try self.printLine("const call_this = stack[sp - 2 - {d}].toJSValueWithCtx(ctx);", .{argc});

        if (argc == 0) {
            // Sync locals TO _locals_jsv before call (ensures current values for round-trip)
            try self.emitClosureVarSyncTo();
            try self.writeLine("const call_result = JSValue.call(ctx, method, call_this, 0, @as([*]JSValue, undefined));");
            // Check for exception and propagate it
            try self.emitInlineExceptionReturn("call_result");
        } else {
            try self.printLine("var args: [{d}]JSValue = undefined;", .{argc});
            for (0..argc) |i| {
                try self.printLine("args[{d}] = CV.toJSValuePtr(&stack[sp - {d}]);", .{ i, argc - i });
            }
            // Sync locals TO _locals_jsv before call (ensures current values for round-trip)
            try self.emitClosureVarSyncTo();
            try self.printLine("const call_result = JSValue.call(ctx, method, call_this, {d}, &args);", .{argc});
            // Check for exception and propagate it
            try self.emitInlineExceptionReturn("call_result");
            // Note: Don't free args - CVs on stack still own the references
        }
        // Store result directly - JS_Call returns a new ref, no dup needed
        try self.writeLine("const result = call_result;");
        // Free the CVs we're abandoning (collect-then-free to avoid g_return_slot corruption)
        try self.printLine("var _to_free: [{d}]JSValue = undefined;", .{argc + 2});
        try self.writeLine("var _free_count: usize = 0;");
        for (0..argc) |i| {
            try self.printLine("{{ const v = stack[sp - {d}]; if (v.isRefType()) {{ _to_free[_free_count] = v.toJSValueWithCtx(ctx); _free_count += 1; }} }}", .{argc - i});
        }
        try self.printLine("{{ const v = stack[sp - 1 - {d}]; if (v.isRefType()) {{ _to_free[_free_count] = v.toJSValueWithCtx(ctx); _free_count += 1; }} }}", .{argc}); // method
        try self.printLine("{{ const v = stack[sp - 2 - {d}]; if (v.isRefType()) {{ _to_free[_free_count] = v.toJSValueWithCtx(ctx); _free_count += 1; }} }}", .{argc}); // this
        try self.writeLine("for (0.._free_count) |_fi| { JSValue.free(ctx, _to_free[_fi]); }");
        try self.printLine("sp -= {d} + 2;", .{argc});
        try self.writeLine("stack[sp] = CV.fromJSValue(result);");
        try self.writeLine("sp += 1;");
        // Sync closure variables back from _locals_jsv (closure may have written to them)
        try self.emitClosureVarSync();
        // Sync vstack: clear this + method + argc args, push result
        for (0..argc + 2) |_| {
            self.vpopAndFree();
        }
        try self.vpush("stack[sp - 1]");

        self.popIndent();
        try self.writeLine("}");
    }

    /// Emit code for call_constructor opcode (new X())
    /// QuickJS-ng stack layout: [constructor, new.target, arg0, arg1, ...argN-1] -> [new_object]
    /// constructor at sp-2-argc, new.target at sp-1-argc, args at sp-argc..sp-1
    fn emitCallConstructor(self: *Self, argc: u16) !void {
        try self.writeLine("{");
        self.pushIndent();

        // constructor at sp-2-argc, new.target at sp-1-argc
        try self.printLine("const ctor = stack[sp - 2 - {d}].toJSValueWithCtx(ctx);", .{argc});

        if (argc == 0) {
            // Sync locals TO _locals_jsv before call (ensures current values for round-trip)
            try self.emitClosureVarSyncTo();
            try self.writeLine("const call_result = JSValue.callConstructor(ctx, ctor, &.{});");
        } else {
            try self.printLine("var args: [{d}]JSValue = undefined;", .{argc});
            for (0..argc) |i| {
                try self.printLine("args[{d}] = CV.toJSValuePtr(&stack[sp - {d}]);", .{ i, argc - i });
            }
            // Sync locals TO _locals_jsv before call (ensures current values for round-trip)
            try self.emitClosureVarSyncTo();
            try self.writeLine("const call_result = JSValue.callConstructor(ctx, ctor, &args);");
            // Note: Don't free args - CVs on stack still own the references
        }
        // Store result directly - callConstructor returns a new ref, no dup needed
        try self.writeLine("const result = call_result;");
        // Free the CVs we're abandoning (collect-then-free to avoid g_return_slot corruption)
        try self.printLine("var _to_free: [{d}]JSValue = undefined;", .{argc + 2});
        try self.writeLine("var _free_count: usize = 0;");
        for (0..argc) |i| {
            try self.printLine("{{ const v = stack[sp - {d}]; if (v.isRefType()) {{ _to_free[_free_count] = v.toJSValueWithCtx(ctx); _free_count += 1; }} }}", .{argc - i});
        }
        try self.printLine("{{ const v = stack[sp - 1 - {d}]; if (v.isRefType()) {{ _to_free[_free_count] = v.toJSValueWithCtx(ctx); _free_count += 1; }} }}", .{argc}); // new.target
        try self.printLine("{{ const v = stack[sp - 2 - {d}]; if (v.isRefType()) {{ _to_free[_free_count] = v.toJSValueWithCtx(ctx); _free_count += 1; }} }}", .{argc}); // ctor
        try self.writeLine("for (0.._free_count) |_fi| { JSValue.free(ctx, _to_free[_fi]); }");
        try self.printLine("sp -= {d} + 2;", .{argc});
        try self.writeLine("stack[sp] = CV.fromJSValue(result);");
        try self.writeLine("sp += 1;");
        // Sync closure variables back from _locals_jsv (closure may have written to them)
        try self.emitClosureVarSync();
        // Sync vstack: clear ctor + new.target + argc args, push result
        for (0..argc + 2) |_| {
            self.vpopAndFree();
        }
        try self.vpush("stack[sp - 1]");

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
                // CRITICAL: Dup value before passing to setPropertyUint32 - it consumes the value
                try self.printLine("{{ const elem = JSValue.dup(ctx, stack[sp - {d}].toJSValueWithCtx(ctx)); _ = JSValue.setPropertyUint32(ctx, arr, {d}, elem); }}", .{ stack_offset, i });
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
        // noinline prevents LLVM inliner from exploding compile times
        if (needs_escape) {
            try self.print("pub noinline fn @\"__frozen_{s}\"(ctx: *zig_runtime.JSContext, _: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue, var_refs: ?[*]*zig_runtime.JSVarRef, closure_var_count: c_int, cpool: ?[*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {{\n", .{func_name});
        } else {
            try self.print("pub noinline fn __frozen_{s}(ctx: *zig_runtime.JSContext, _: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue, var_refs: ?[*]*zig_runtime.JSVarRef, closure_var_count: c_int, cpool: ?[*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {{\n", .{func_name});
        }
        self.pushIndent();

        // Unbox arguments - argc/argv are always used in the wrapper
        // Only suppress if no arguments
        if (argc == 0) {
            try self.writeLine("_ = argc; _ = argv; _ = var_refs; _ = closure_var_count; _ = cpool;");
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

    // Native Specialization (Zero-FFI)
    // ========================================================================

    /// Check if function can use native specialization (zero FFI in hot path)
    /// DISABLED: Native specialization produces broken code for some function patterns
    /// (e.g., lodash's arrayPush returns __data__ which is a buffer pointer, not numeric)
    /// TODO: Re-enable when proper return value validation is added
    fn canUseNativeSpecialization(self: *const Self) bool {
        _ = self;
        return false;
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

        // Generate native parameters: data buffer + length + bytes_per_element + numeric args
        // Use [*]u8 for raw buffer access with bytes_per_element for proper element stride
        if (argc > 0) {
            try self.write("data: [*]u8, data_len: usize, bytes_per_element: usize");
        }
        for (1..argc) |i| {
            try self.print(", n{d}: i32", .{i});
        }
        try self.write(") f64 {\n");
        self.pushIndent();

        // Check if data_len is used (only when .get_length accesses the data array)
        // If not used, discard it to suppress unused parameter warning
        var uses_data_len = false;
        var uses_array_access = false;
        var has_array_write = false;
        for (self.func.cfg.blocks.items) |block| {
            for (block.instructions) |instr| {
                if (instr.opcode == .get_length) {
                    uses_data_len = true;
                }
                if (instr.opcode == .get_array_el or instr.opcode == .get_array_el2 or instr.opcode == .put_array_el) {
                    uses_array_access = true;
                }
                if (instr.opcode == .put_array_el) {
                    has_array_write = true;
                }
            }
        }
        // Note: has_array_write is used below to determine if copy-back is needed
        if (argc > 0 and !uses_data_len) {
            try self.writeLine("_ = data_len;");
        }
        if (argc > 0 and !uses_array_access) {
            try self.writeLine("_ = bytes_per_element;");
        }

        // Emit pure native body using expression-based codegen
        try self.emitNativeBody();

        self.popIndent();
        try self.write("}\n\n");

        // ================================================================
        // 1b. Generate JSArray native function - ZERO-COPY, ZERO-FFI
        // Takes JSValue[] directly without any buffer copy
        // Skip when has_array_write - native JSArray path doesn't support put_array_el
        // ================================================================
        if (!has_array_write) {
            try self.write("/// Pure native function for regular JS arrays - ZERO-COPY, ZERO-FFI\n");
            if (needs_escape) {
                try self.print("fn @\"__frozen_{s}_native_jsarray\"(", .{func_name});
            } else {
                try self.print("fn __frozen_{s}_native_jsarray(", .{func_name});
            }

            // Parameters: js_values pointer + element count + numeric args
            if (argc > 0) {
                try self.write("js_values: [*]zig_runtime.JSValue, elem_count: usize");
            }
            for (1..argc) |i| {
                try self.print(", n{d}: i32", .{i});
            }
            try self.write(") f64 {\n");
            self.pushIndent();

            // Suppress unused warnings if needed
            if (argc > 0 and !uses_data_len) {
                try self.writeLine("_ = elem_count;");
            }

            // Emit JSArray-specific native body
            try self.emitNativeBodyJSArray();

            self.popIndent();
            try self.write("}\n\n");
        }


        // ================================================================
        // 2. Generate the main wrapper function with DUAL-PATH
        // noinline prevents LLVM inliner from exploding compile times
        // ================================================================
        if (needs_escape) {
            try self.print("pub noinline fn @\"__frozen_{s}\"(ctx: *zig_runtime.JSContext, this_val: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue, var_refs: ?[*]*zig_runtime.JSVarRef, closure_var_count: c_int, cpool: ?[*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {{\n", .{func_name});
        } else {
            try self.print("pub noinline fn __frozen_{s}(ctx: *zig_runtime.JSContext, this_val: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue, var_refs: ?[*]*zig_runtime.JSVarRef, closure_var_count: c_int, cpool: ?[*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {{\n", .{func_name});
        }
        self.pushIndent();

        // Only discard this_val if the function doesn't use it
        if (!self.uses_this_val) {
            try self.writeLine("_ = this_val;");
        }
        try self.writeLine("_ = var_refs;");
        try self.writeLine("_ = closure_var_count;");
        try self.writeLine("_ = cpool;");
        try self.writeLine("");

        // ================================================================
        // PATH 1: Try TypedArray fast path (skip on WASM32 - JS_GetArrayBuffer FFI issues)
        // NOTE: Skip native paths (PATH 1 & 2) when function has array writes, as the native
        // codegen doesn't correctly handle multiple array arguments in put_array_el operations.
        // ================================================================
        if (argc > 0 and !has_array_write) {
            try self.writeLine("// PATH 1: Try TypedArray fast path (zero FFI in hot loop)");
            try self.writeLine("// Skip on WASM32 - JS_GetArrayBuffer returns wrong pointer due to FFI calling convention issues");
            try self.writeLine("if (comptime @sizeOf(*anyopaque) != 4) {");
            self.pushIndent();
            try self.writeLine("if (argc > 0) {");
            self.pushIndent();

            try self.writeLine("var byte_offset: usize = 0;");
            try self.writeLine("var byte_length: usize = 0;");
            try self.writeLine("var bytes_per_element: usize = 0;");
            try self.writeLine("const buffer = zig_runtime.quickjs.JS_GetTypedArrayBuffer(ctx, argv[0], &byte_offset, &byte_length, &bytes_per_element);");
            try self.writeLine("");
            try self.writeLine("// Check if it's any valid TypedArray (Uint8Array, Int8Array, Int16Array, Int32Array, etc.)");
            try self.writeLine("if (!buffer.isException() and bytes_per_element > 0) {");
            self.pushIndent();

            try self.writeLine("// Clear any pending exception");
            try self.writeLine("_ = zig_runtime.quickjs.JS_GetException(ctx);");
            try self.writeLine("");
            try self.writeLine("var buf_size: usize = 0;");
            try self.writeLine("const buf_ptr = zig_runtime.quickjs.JS_GetArrayBuffer(ctx, &buf_size, buffer);");
            try self.writeLine("if (buf_ptr != null) {");
            self.pushIndent();

            try self.writeLine("const data_ptr: [*]u8 = @ptrCast(buf_ptr.? + byte_offset);");
            try self.writeLine("const data_len = byte_length;");
            try self.writeLine("");

            // Extract numeric args for native call
            for (1..argc) |i| {
                try self.print("var n{d}: i32 = 0;\n", .{i});
                try self.print("_ = zig_runtime.JSValue.toInt32(ctx, &n{d}, argv[{d}]);\n", .{ i, i });
            }

            try self.writeLine("");
            try self.writeLine("// ZERO-FFI execution path");

            // Call pure native helper with bytes_per_element
            if (needs_escape) {
                try self.print("const result = @\"__frozen_{s}_native\"(data_ptr, data_len, bytes_per_element", .{func_name});
            } else {
                try self.print("const result = __frozen_{s}_native(data_ptr, data_len, bytes_per_element", .{func_name});
            }
            for (1..argc) |i| {
                try self.print(", n{d}", .{i});
            }
            try self.write(");\n");

            // Free the buffer reference before returning
            try self.writeLine("zig_runtime.JSValue.free(ctx, buffer);");
            try self.writeLine("return zig_runtime.JSValue.newFloat64(result);");

            self.popIndent();
            try self.writeLine("}");

            // Free buffer if buf_ptr was null
            try self.writeLine("zig_runtime.JSValue.free(ctx, buffer);");

            self.popIndent();
            try self.writeLine("} else {");
            self.pushIndent();
            try self.writeLine("// Clear exception from failed TypedArray check");
            try self.writeLine("const exc = zig_runtime.quickjs.JS_GetException(ctx);");
            try self.writeLine("zig_runtime.JSValue.free(ctx, exc);");
            // Also free buffer in the else case (it may still be valid but not u8/i32)
            try self.writeLine("if (!buffer.isException()) zig_runtime.JSValue.free(ctx, buffer);");
            self.popIndent();
            try self.writeLine("}");

            self.popIndent();
            try self.writeLine("}");
            self.popIndent();
            try self.writeLine("}"); // Close comptime block
            try self.writeLine("");
        }

        // ================================================================
        // PATH 2: Regular Array - ZERO-FFI via direct JSObject access
        // Skip on WASM32 - JSObject struct layout has issues on WAMR AOT
        // NOTE: Skip when has_array_write - native path doesn't handle multi-array correctly
        // ================================================================
        if (argc > 0 and !has_array_write) {
            try self.writeLine("// PATH 2: Regular Array - direct internal access (zero FFI)");
            try self.writeLine("// Skip on WASM32 - JSObject struct layout has issues on WAMR AOT");
            try self.writeLine("if (comptime @sizeOf(*anyopaque) != 4) {");
            self.pushIndent();
            try self.writeLine("if (argc > 0) {");
            self.pushIndent();

            // Try to get fast array pointer directly (no FFI)
            try self.writeLine("const fast_arr = zig_runtime.getFastArrayDirect(argv[0]);");
            try self.writeLine("if (fast_arr.success) {");
            self.pushIndent();

            try self.writeLine("const js_values = fast_arr.values.?;");
            try self.writeLine("const elem_count: usize = fast_arr.count;");
            try self.writeLine("");

            // ZERO-COPY: Call native function that takes JSValue[] directly
            // No size limit - works for any array size without copying
            try self.writeLine("// ZERO-COPY: Call native function with JSValue[] directly");

            // Extract numeric args
            for (1..argc) |i| {
                try self.print("var n{d}: i32 = 0;\n", .{i});
                try self.print("_ = zig_runtime.JSValue.toInt32(ctx, &n{d}, argv[{d}]);\n", .{ i, i });
            }

            try self.writeLine("");
            try self.writeLine("// ZERO-FFI, ZERO-COPY execution path");

            // Call the JSArray native helper
            if (needs_escape) {
                try self.print("const result = @\"__frozen_{s}_native_jsarray\"(js_values, elem_count", .{func_name});
            } else {
                try self.print("const result = __frozen_{s}_native_jsarray(js_values, elem_count", .{func_name});
            }
            for (1..argc) |i| {
                try self.print(", n{d}", .{i});
            }
            try self.write(");\n");

            try self.writeLine("return zig_runtime.JSValue.newFloat64(result);");

            self.popIndent();
            try self.writeLine("}"); // Close fast_arr.success

            self.popIndent();
            try self.writeLine("}"); // Close argc > 0
            self.popIndent();
            try self.writeLine("}"); // Close comptime block
        }

        try self.writeLine("");
        try self.writeLine("// PATH 3: Fallback - use stack-based interpreter");
        try self.emitFallbackBody();

        self.popIndent();
        try self.write("}\n");
    }

    /// Emit the fallback body using regular FFI (for regular Arrays)
    fn emitFallbackBody(self: *Self) !void {
        // Clear vstack for fresh start - free allocated items first
        for (self.vstack.items) |expr| {
            if (self.isAllocated(expr)) self.allocator.free(expr);
        }
        self.vstack.clearRetainingCapacity();
        self.temp_counter = 100; // Start temps at 100 to avoid conflicts with native path

        try self.writeLine("const CV = zig_runtime.CompressedValue;");
        try self.writeLine("_ = &CV;");

        // Emit locals - always declare even if var_count=0 (dead code may reference it)
        // Use at least size 16 to cover most dead code scenarios
        const min_locals = if (self.func.var_count >= 16) self.func.var_count else 16;
        try self.printLine("var locals: [{d}]CV = .{{CV.UNDEFINED}} ** {d};", .{ min_locals, min_locals });
        try self.writeLine("_ = &locals;");
        try self.writeLine("");

        // Emit stack (for complex operations)
        const stack_array_size2 = @max(self.func.var_count + self.func.stack_size, 256);
        try self.printLine("var stack: [{d}]CV = .{{CV.UNDEFINED}} ** {d};", .{ stack_array_size2, stack_array_size2 });
        try self.writeLine("var sp: usize = 0;");
        // Track iterator positions for for-of loops (stack for nested loops)
        try self.writeLine("var for_of_iter_stack: [8]usize = .{0} ** 8;");
        try self.writeLine("var for_of_depth: usize = 0;");
        try self.writeLine("_ = &stack; _ = &sp; _ = &for_of_iter_stack; _ = &for_of_depth;");
        try self.writeLine("");

        // When function has put_arg operations, we need shadow storage for arguments
        // This allows assignment to work even when argc < param_index
        if (self.has_put_arg) {
            const arg_count = self.func.arg_count;
            if (arg_count > 0) {
                try self.printLine("var arg_shadow: [{d}]CV = undefined;", .{arg_count});
                // Initialize from argv if provided, else UNDEFINED
                for (0..arg_count) |i| {
                    try self.printLine("arg_shadow[{d}] = if ({d} < argc) CV.fromJSValue(JSValue.dup(ctx, argv[{d}])) else CV.UNDEFINED;", .{ i, i, i });
                }
                try self.writeLine("_ = &arg_shadow;");
                try self.writeLine("");
            }
        }

        // Emit argument cache for functions with loops that access arguments
        // Use JSValue array directly for arg_cache (no compression) to ensure correct FFI
        if (self.uses_arg_cache) {
            const cache_size = self.max_loop_arg_idx + 1;
            // Store duped JSValues directly - no compression
            try self.printLine("var arg_cache_js: [{d}]JSValue = undefined;", .{cache_size});
            try self.printLine("for (0..@min(@as(usize, @intCast(argc)), {d})) |_i| {{", .{cache_size});
            self.pushIndent();
            try self.writeLine("arg_cache_js[_i] = JSValue.dup(ctx, argv[_i]);");
            self.popIndent();
            try self.writeLine("}");
            // Also create CV cache for stack operations
            try self.printLine("var arg_cache: [{d}]CV = undefined;", .{cache_size});
            try self.printLine("for (0..@min(@as(usize, @intCast(argc)), {d})) |_i| {{", .{cache_size});
            self.pushIndent();
            try self.writeLine("arg_cache[_i] = CV.fromJSValue(arg_cache_js[_i]);");
            self.popIndent();
            try self.writeLine("}");
            // Add defer to free cached args using ORIGINAL JSValues (not reconstructed)
            try self.printLine("defer for (0..@min(@as(usize, @intCast(argc)), {d})) |_i| {{", .{cache_size});
            self.pushIndent();
            try self.writeLine("JSValue.free(ctx, arg_cache_js[_i]);");
            self.popIndent();
            try self.writeLine("};");
            try self.writeLine("");
        }

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
                        // Redirect target if it's inside a native loop
                        const target = self.redirectJumpTarget(block.successors.items[0]);
                        try self.printLine("block_id = {d}; continue;", .{target});
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
            // Skip remaining instructions if block already terminated
            if (self.block_terminated) return false;
            try self.emitInstructionExpr(instr, block, null, idx);
            // Check for terminal instructions (return/throw/unsupported opcode)
            if (self.block_terminated or instr.opcode == .@"return" or instr.opcode == .return_undef or
                instr.opcode == .ret or instr.opcode == .tail_call or instr.opcode == .tail_call_method or
                instr.opcode == .throw or instr.opcode == .throw_error)
            {
                return false; // Block ends with return/throw/error, don't add continue
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

    /// Emit pure native function body for JSValue[] arrays - ZERO-COPY, ZERO-FFI
    fn emitNativeBodyJSArray(self: *Self) !void {
        const blocks = self.func.cfg.blocks.items;
        self.natural_loops = cfg_mod.detectNaturalLoops(self.func.cfg, self.allocator) catch &.{};

        // Check if this is a simple sum pattern: 2 locals, single loop, sum of array elements
        // Pattern: locals[0] = accumulator, locals[1] = index
        // Loop: while (index < length) { acc += arr[index]; index++; }
        if (self.func.var_count == 2 and self.natural_loops.len == 1) {
            const loop = self.natural_loops[0];
            if (self.isSimpleSumLoop(loop)) {
                // Check if post-loop code divides by array length (average pattern)
                const has_post_loop_div = self.hasPostLoopDivision(loop);

                // Emit unrolled loop with 4 accumulators for instruction-level parallelism
                try self.writeLine("// UNROLLED SUM: 4 independent accumulators for ILP");
                try self.writeLine("var acc0: i64 = 0;");
                try self.writeLine("var acc1: i64 = 0;");
                try self.writeLine("var acc2: i64 = 0;");
                try self.writeLine("var acc3: i64 = 0;");
                try self.writeLine("const len4 = elem_count & ~@as(usize, 3);");
                try self.writeLine("var i: usize = 0;");
                try self.writeLine("while (i < len4) : (i += 4) {");
                self.pushIndent();
                try self.writeLine("acc0 += zig_runtime.jsValueToInt64Inline(js_values[i]);");
                try self.writeLine("acc1 += zig_runtime.jsValueToInt64Inline(js_values[i + 1]);");
                try self.writeLine("acc2 += zig_runtime.jsValueToInt64Inline(js_values[i + 2]);");
                try self.writeLine("acc3 += zig_runtime.jsValueToInt64Inline(js_values[i + 3]);");
                self.popIndent();
                try self.writeLine("}");
                try self.writeLine("// Handle remainder");
                try self.writeLine("while (i < elem_count) : (i += 1) {");
                self.pushIndent();
                try self.writeLine("acc0 += zig_runtime.jsValueToInt64Inline(js_values[i]);");
                self.popIndent();
                try self.writeLine("}");
                if (has_post_loop_div) {
                    // Average pattern: return sum / arr.length
                    try self.writeLine("return @as(f64, @floatFromInt(acc0 + acc1 + acc2 + acc3)) / @as(f64, @floatFromInt(elem_count));");
                } else {
                    // Pure sum pattern: return sum
                    try self.writeLine("return @floatFromInt(acc0 + acc1 + acc2 + acc3);");
                }
                return;
            }
        }

        for (self.natural_loops) |loop| {
            for (loop.body_blocks) |bid| {
                if (bid != loop.header_block) {
                    self.skip_blocks.put(self.allocator, bid, {}) catch {};
                }
            }
        }

        if (self.func.var_count > 0) {
            try self.printLine("var locals: [{d}]i64 = .{{0}} ** {d};", .{ self.func.var_count, self.func.var_count });
        }
        try self.writeLine("");

        var stack: [32][]const u8 = undefined;
        var sp: usize = 0;

        for (blocks, 0..) |block, block_idx| {
            if (self.skip_blocks.get(@intCast(block_idx)) != null) continue;
            try self.printLine("// block_{d}", .{block_idx});

            var is_loop_header = false;
            for (self.natural_loops) |loop| {
                if (loop.header_block == block_idx) {
                    is_loop_header = true;
                    try self.emitNativeWhileLoopJSArray(loop, &stack, &sp);
                    break;
                }
            }

            if (!is_loop_header) {
                for (block.instructions) |instr| {
                    try self.emitZeroFFINativeOpJSArray(instr, &stack, &sp);
                }
            }
        }
    }

    /// Check if a loop is a simple sum pattern: acc += arr[i]; i++
    /// Returns true if the loop body contains only: get_loc, get_field_prop (length), lt, if_false,
    /// get_loc, get_array_el, add, put_loc, inc_loc, goto
    fn isSimpleSumLoop(self: *Self, loop: cfg_mod.NaturalLoop) bool {
        const blocks = self.func.cfg.blocks.items;

        // Check all instructions in the loop
        var has_get_array_el = false;
        var has_add = false;
        var has_inc_loc = false;

        for (loop.body_blocks) |bid| {
            const block = blocks[bid];
            for (block.instructions) |instr| {
                switch (instr.opcode) {
                    // Allowed opcodes for simple sum
                    .get_loc, .get_loc0, .get_loc1, .get_loc2, .get_loc3,
                    .put_loc, .put_loc0, .put_loc1, .put_loc2, .put_loc3,
                    .set_loc, .set_loc0, .set_loc1, .set_loc2, .set_loc3,
                    .get_arg, .get_arg0, .get_arg1, .get_arg2, .get_arg3,
                    .get_field, .get_field2, .get_length,
                    .get_array_el, .get_array_el2,
                    .push_0, .push_1, .push_i32, .push_i8, .push_i16,
                    .lt, .if_false, .if_false8, .if_true, .if_true8,
                    .add, .sub, .mul, .div, .mod,
                    .inc_loc, .dec_loc, .add_loc,
                    .goto, .goto8, .goto16,
                    .drop, .nip, .dup,
                    .@"return", .return_undef,
                    => {},
                    // Any other opcode disqualifies
                    else => return false,
                }

                // Track pattern elements
                if (instr.opcode == .get_array_el or instr.opcode == .get_array_el2) has_get_array_el = true;
                if (instr.opcode == .add) has_add = true;
                if (instr.opcode == .inc_loc or instr.opcode == .add_loc) has_inc_loc = true;
            }
        }

        // Must have all three elements of sum pattern
        return has_get_array_el and has_add and has_inc_loc;
    }

    /// Check if there's a division by array length after the loop (average pattern)
    /// Pattern: return sum / arr.length
    fn hasPostLoopDivision(self: *Self, loop: cfg_mod.NaturalLoop) bool {
        const blocks = self.func.cfg.blocks.items;

        // Find exit block (first block not in loop that follows loop blocks)
        const exit_block_idx = loop.exit_block orelse return false;
        if (exit_block_idx >= blocks.len) return false;

        const exit_block = blocks[exit_block_idx];

        // Look for pattern: get_loc(sum), get_arg/get_loc(arr), get_length, div, return
        var has_div = false;
        var has_get_length = false;
        var has_return = false;

        for (exit_block.instructions) |instr| {
            switch (instr.opcode) {
                .div => has_div = true,
                .get_length => has_get_length = true,
                .@"return" => has_return = true,
                else => {},
            }
        }

        // Average pattern requires all three: get_length, div, return
        return has_div and has_get_length and has_return;
    }

    /// Emit a while loop for JSArray path
    fn emitNativeWhileLoopJSArray(self: *Self, loop: cfg_mod.NaturalLoop, stack: *[32][]const u8, sp: *usize) !void {
        try self.writeLine("while (true) {");
        self.pushIndent();

        const header = self.func.cfg.blocks.items[loop.header_block];
        for (header.instructions) |instr| {
            try self.emitZeroFFINativeOpJSArray(instr, stack, sp);
        }

        for (loop.body_blocks) |bid| {
            if (bid == loop.header_block) continue;

            var is_nested_header = false;
            for (self.natural_loops) |nested| {
                if (nested.header_block == bid and nested.depth > loop.depth) {
                    is_nested_header = true;
                    try self.emitNativeWhileLoopJSArray(nested, stack, sp);
                    break;
                }
            }
            if (is_nested_header) continue;

            var in_nested_loop = false;
            for (self.natural_loops) |nested| {
                if (nested.header_block != loop.header_block and nested.depth > loop.depth and nested.containsBlock(bid)) {
                    in_nested_loop = true;
                    break;
                }
            }

            if (!in_nested_loop) {
                const body_block = self.func.cfg.blocks.items[bid];
                for (body_block.instructions) |instr| {
                    try self.emitZeroFFINativeOpJSArray(instr, stack, sp);
                }
            }
        }

        self.popIndent();
        try self.writeLine("}");
    }

    /// Emit instruction for JSArray path - ZERO-COPY, uses jsValueToInt64Inline
    fn emitZeroFFINativeOpJSArray(self: *Self, instr: Instruction, stack: *[32][]const u8, sp: *usize) !void {
        var expr_buf: [512]u8 = undefined;
        switch (instr.opcode) {
            .push_i32 => { const s = std.fmt.bufPrint(&expr_buf, "{d}", .{instr.operand.i32}) catch "0"; stack[sp.*] = self.allocator.dupe(u8, s) catch "0"; sp.* += 1; },
            .push_0 => { stack[sp.*] = "0"; sp.* += 1; },
            .push_1 => { stack[sp.*] = "1"; sp.* += 1; },
            .push_2 => { stack[sp.*] = "2"; sp.* += 1; },
            .push_3 => { stack[sp.*] = "3"; sp.* += 1; },
            .push_4 => { stack[sp.*] = "4"; sp.* += 1; },
            .push_5 => { stack[sp.*] = "5"; sp.* += 1; },
            .push_6 => { stack[sp.*] = "6"; sp.* += 1; },
            .push_7 => { stack[sp.*] = "7"; sp.* += 1; },
            .push_minus1 => { stack[sp.*] = "-1"; sp.* += 1; },
            .push_i8 => { const s = std.fmt.bufPrint(&expr_buf, "{d}", .{instr.operand.i8}) catch "0"; stack[sp.*] = self.allocator.dupe(u8, s) catch "0"; sp.* += 1; },
            .push_i16 => { const s = std.fmt.bufPrint(&expr_buf, "{d}", .{instr.operand.i16}) catch "0"; stack[sp.*] = self.allocator.dupe(u8, s) catch "0"; sp.* += 1; },
            .push_const8, .null, .undefined => { stack[sp.*] = "0"; sp.* += 1; },
            .get_arg => {
                const arg_idx = instr.operand.arg;
                if (arg_idx == 0) { stack[sp.*] = "__jsarray__"; }
                else { const s = std.fmt.bufPrint(&expr_buf, "n{d}", .{arg_idx}) catch "0"; stack[sp.*] = self.allocator.dupe(u8, s) catch "0"; }
                sp.* += 1;
            },
            .get_arg0 => { stack[sp.*] = "__jsarray__"; sp.* += 1; },
            .get_arg1 => { stack[sp.*] = "n1"; sp.* += 1; },
            .get_arg2 => { stack[sp.*] = "n2"; sp.* += 1; },
            .get_arg3 => { stack[sp.*] = "n3"; sp.* += 1; },
            .get_loc => { const s = std.fmt.bufPrint(&expr_buf, "locals[{d}]", .{instr.operand.loc}) catch "locals[0]"; stack[sp.*] = self.allocator.dupe(u8, s) catch "locals[0]"; sp.* += 1; },
            .put_loc => { if (sp.* >= 1) { const val = stack[sp.* - 1]; sp.* -= 1; try self.printLine("locals[{d}] = @as(i64, @intCast({s}));", .{ instr.operand.loc, val }); } },
            .set_loc => { if (sp.* >= 1) { const val = stack[sp.* - 1]; try self.printLine("locals[{d}] = @as(i64, @intCast({s}));", .{ instr.operand.loc, val }); } },
            .get_loc0 => { stack[sp.*] = "locals[0]"; sp.* += 1; },
            .get_loc1 => { stack[sp.*] = "locals[1]"; sp.* += 1; },
            .get_loc2 => { stack[sp.*] = "locals[2]"; sp.* += 1; },
            .get_loc3 => { stack[sp.*] = "locals[3]"; sp.* += 1; },
            .put_loc0, .put_loc1, .put_loc2, .put_loc3 => {
                if (sp.* >= 1) {
                    const val = stack[sp.* - 1]; sp.* -= 1;
                    const loc: u8 = switch (instr.opcode) { .put_loc0 => 0, .put_loc1 => 1, .put_loc2 => 2, .put_loc3 => 3, else => 0 };
                    try self.printLine("locals[{d}] = @as(i64, @intCast({s}));", .{ loc, val });
                }
            },
            .set_loc0, .set_loc1, .set_loc2, .set_loc3 => {
                if (sp.* >= 1) {
                    const val = stack[sp.* - 1];
                    const loc: u8 = switch (instr.opcode) { .set_loc0 => 0, .set_loc1 => 1, .set_loc2 => 2, .set_loc3 => 3, else => 0 };
                    try self.printLine("locals[{d}] = @as(i64, @intCast({s}));", .{ loc, val });
                }
            },
            .add_loc => { if (sp.* >= 1) { const val = stack[sp.* - 1]; sp.* -= 1; try self.printLine("locals[{d}] += @as(i64, @intCast({s}));", .{ instr.operand.loc, val }); } },
            .inc_loc => { try self.printLine("locals[{d}] += 1;", .{instr.operand.loc}); },
            .dec_loc => { try self.printLine("locals[{d}] -= 1;", .{instr.operand.loc}); },
            .get_array_el => {
                if (sp.* >= 2) {
                    const idx = stack[sp.* - 1]; const arr = stack[sp.* - 2]; sp.* -= 2;
                    if (std.mem.eql(u8, arr, "__jsarray__")) {
                        const s = std.fmt.bufPrint(&expr_buf, "zig_runtime.jsValueToInt64Inline(js_values[@as(usize, @intCast({s}))])", .{idx}) catch "0";
                        stack[sp.*] = self.allocator.dupe(u8, s) catch "0";
                    } else {
                        const s = std.fmt.bufPrint(&expr_buf, "{s}[@as(usize, @intCast({s}))]", .{ arr, idx }) catch "0";
                        stack[sp.*] = self.allocator.dupe(u8, s) catch "0";
                    }
                    sp.* += 1;
                }
            },
            .get_length => {
                if (sp.* >= 1) {
                    const arr = stack[sp.* - 1]; sp.* -= 1;
                    if (std.mem.eql(u8, arr, "__jsarray__")) { stack[sp.*] = "@as(i64, @intCast(elem_count))"; }
                    else { stack[sp.*] = "0"; }
                    sp.* += 1;
                }
            },
            .to_propkey, .to_propkey2 => {},
            .add => { if (sp.* >= 2) { const b = stack[sp.* - 1]; const a = stack[sp.* - 2]; sp.* -= 2; const s = std.fmt.bufPrint(&expr_buf, "({s} + {s})", .{ a, b }) catch "(0)"; stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)"; sp.* += 1; } },
            .sub => { if (sp.* >= 2) { const b = stack[sp.* - 1]; const a = stack[sp.* - 2]; sp.* -= 2; const s = std.fmt.bufPrint(&expr_buf, "({s} - {s})", .{ a, b }) catch "(0)"; stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)"; sp.* += 1; } },
            .mul => { if (sp.* >= 2) { const b = stack[sp.* - 1]; const a = stack[sp.* - 2]; sp.* -= 2; const s = std.fmt.bufPrint(&expr_buf, "({s} * {s})", .{ a, b }) catch "(0)"; stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)"; sp.* += 1; } },
            .div => { if (sp.* >= 2) { const b = stack[sp.* - 1]; const a = stack[sp.* - 2]; sp.* -= 2; const s = std.fmt.bufPrint(&expr_buf, "(@as(f64, @floatFromInt({s})) / @as(f64, @floatFromInt({s})))", .{ a, b }) catch "(0.0)"; stack[sp.*] = self.allocator.dupe(u8, s) catch "(0.0)"; sp.* += 1; } },
            .mod => { if (sp.* >= 2) { const b = stack[sp.* - 1]; const a = stack[sp.* - 2]; sp.* -= 2; const s = std.fmt.bufPrint(&expr_buf, "@mod({s}, {s})", .{ a, b }) catch "(0)"; stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)"; sp.* += 1; } },
            .neg => { if (sp.* >= 1) { const a = stack[sp.* - 1]; sp.* -= 1; const s = std.fmt.bufPrint(&expr_buf, "-({s})", .{a}) catch "(0)"; stack[sp.*] = self.allocator.dupe(u8, s) catch "(0)"; sp.* += 1; } },
            .lt => { if (sp.* >= 2) { const b = stack[sp.* - 1]; const a = stack[sp.* - 2]; sp.* -= 2; const s = std.fmt.bufPrint(&expr_buf, "@intFromBool({s} < {s})", .{ a, b }) catch "0"; stack[sp.*] = self.allocator.dupe(u8, s) catch "0"; sp.* += 1; } },
            .lte => { if (sp.* >= 2) { const b = stack[sp.* - 1]; const a = stack[sp.* - 2]; sp.* -= 2; const s = std.fmt.bufPrint(&expr_buf, "@intFromBool({s} <= {s})", .{ a, b }) catch "0"; stack[sp.*] = self.allocator.dupe(u8, s) catch "0"; sp.* += 1; } },
            .gt => { if (sp.* >= 2) { const b = stack[sp.* - 1]; const a = stack[sp.* - 2]; sp.* -= 2; const s = std.fmt.bufPrint(&expr_buf, "@intFromBool({s} > {s})", .{ a, b }) catch "0"; stack[sp.*] = self.allocator.dupe(u8, s) catch "0"; sp.* += 1; } },
            .gte => { if (sp.* >= 2) { const b = stack[sp.* - 1]; const a = stack[sp.* - 2]; sp.* -= 2; const s = std.fmt.bufPrint(&expr_buf, "@intFromBool({s} >= {s})", .{ a, b }) catch "0"; stack[sp.*] = self.allocator.dupe(u8, s) catch "0"; sp.* += 1; } },
            .eq, .strict_eq => { if (sp.* >= 2) { const b = stack[sp.* - 1]; const a = stack[sp.* - 2]; sp.* -= 2; const s = std.fmt.bufPrint(&expr_buf, "@intFromBool({s} == {s})", .{ a, b }) catch "0"; stack[sp.*] = self.allocator.dupe(u8, s) catch "0"; sp.* += 1; } },
            .neq, .strict_neq => { if (sp.* >= 2) { const b = stack[sp.* - 1]; const a = stack[sp.* - 2]; sp.* -= 2; const s = std.fmt.bufPrint(&expr_buf, "@intFromBool({s} != {s})", .{ a, b }) catch "0"; stack[sp.*] = self.allocator.dupe(u8, s) catch "0"; sp.* += 1; } },
            .if_false, .if_false8 => {
                if (sp.* >= 1) {
                    const cond = stack[sp.* - 1];
                    sp.* -= 1;
                    // Skip condition if it's __jsarray__ - array is always valid in JSArray path
                    if (!std.mem.eql(u8, cond, "__jsarray__")) {
                        try self.printLine("if ({s} == 0) break;", .{cond});
                    }
                }
            },
            .if_true, .if_true8 => {
                if (sp.* >= 1) {
                    const cond = stack[sp.* - 1];
                    sp.* -= 1;
                    // Skip condition if it's __jsarray__ - array is always valid in JSArray path
                    if (!std.mem.eql(u8, cond, "__jsarray__")) {
                        try self.printLine("if ({s} != 0) break;", .{cond});
                    }
                }
            },
            .goto, .goto8, .goto16 => { try self.writeLine("continue;"); },
            .return_undef => { try self.writeLine("return 0.0;"); },
            .@"return" => {
                if (sp.* >= 1) { const val = stack[sp.* - 1]; sp.* -= 1;
                    if (std.mem.indexOf(u8, val, "f64") != null) { try self.printLine("return {s};", .{val}); }
                    else { try self.printLine("return @floatFromInt({s});", .{val}); }
                } else { try self.writeLine("return 0.0;"); }
            },
            .drop => { if (sp.* >= 1) sp.* -= 1; },
            .dup => { if (sp.* >= 1) { stack[sp.*] = stack[sp.* - 1]; sp.* += 1; } },
            .swap => { if (sp.* >= 2) { const tmp = stack[sp.* - 1]; stack[sp.* - 1] = stack[sp.* - 2]; stack[sp.* - 2] = tmp; } },
            .inc => { if (sp.* >= 1) { const a = stack[sp.* - 1]; sp.* -= 1; const s = std.fmt.bufPrint(&expr_buf, "({s} + 1)", .{a}) catch "(1)"; stack[sp.*] = self.allocator.dupe(u8, s) catch "(1)"; sp.* += 1; } },
            .dec => { if (sp.* >= 1) { const a = stack[sp.* - 1]; sp.* -= 1; const s = std.fmt.bufPrint(&expr_buf, "({s} - 1)", .{a}) catch "(-1)"; stack[sp.*] = self.allocator.dupe(u8, s) catch "(-1)"; sp.* += 1; } },
            .nop, .fclosure, .push_atom_value, .push_const, .set_loc_uninitialized => {},
            else => { try self.printLine("// TODO native JSArray: {}", .{instr.opcode}); },
        }
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
        var expr_buf: [512]u8 = undefined;

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
                const val = instr.operand.i8;
                const s = std.fmt.bufPrint(&expr_buf, "{d}", .{val}) catch "0";
                stack[sp.*] = self.allocator.dupe(u8, s) catch "0";
                sp.* += 1;
            },
            .push_i16 => {
                const val = instr.operand.i16;
                const s = std.fmt.bufPrint(&expr_buf, "{d}", .{val}) catch "0";
                stack[sp.*] = self.allocator.dupe(u8, s) catch "0";
                sp.* += 1;
            },
            .push_const8 => {
                // Constant pool access - not fully implemented in native mode
                // Just push 0 as placeholder
                stack[sp.*] = "0";
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
                    // Division produces f64, use @intFromFloat; otherwise use @intCast
                    if (std.mem.indexOf(u8, val, "@as(f64") != null) {
                        try self.printLine("locals[{d}] = @intFromFloat({s});", .{ loc, val });
                    } else {
                        try self.printLine("locals[{d}] = @intCast({s});", .{ loc, val });
                    }
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
                    // Division produces f64, use @intFromFloat; otherwise use @intCast
                    if (std.mem.indexOf(u8, val, "@as(f64") != null) {
                        try self.printLine("locals[{d}] = @intFromFloat({s});", .{ loc, val });
                    } else {
                        try self.printLine("locals[{d}] = @intCast({s});", .{ loc, val });
                    }
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
                        // Use bytes_per_element to read correct element size via sint optimization
                        // For i32 (bytes_per_element=4): read 4 bytes as signed int using unaligned pointer
                        const s = std.fmt.bufPrint(&expr_buf, "(if (bytes_per_element == 4) @as(i64, @as(*align(1) const i32, @ptrCast(data + @as(usize, @intCast({s})) * 4)).*) else if (bytes_per_element == 2) @as(i64, @as(*align(1) const i16, @ptrCast(data + @as(usize, @intCast({s})) * 2)).*) else @as(i64, data[@as(usize, @intCast({s}))]))", .{ idx, idx, idx }) catch "data[0]";
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
                        // Write to correct element size based on bytes_per_element
                        // For u8 (bytes_per_element=1): clamp to [0,255]
                        // For i16 (bytes_per_element=2): write as i16
                        // For i32 (bytes_per_element=4): write as i32
                        try self.printLine("{{ const v: i64 = {s}; if (bytes_per_element == 4) {{ @as(*align(1) i32, @ptrCast(data + @as(usize, @intCast({s})) * 4)).* = @intCast(v); }} else if (bytes_per_element == 2) {{ @as(*align(1) i16, @ptrCast(data + @as(usize, @intCast({s})) * 2)).* = @intCast(v); }} else {{ const vf: f64 = @floatFromInt(v); data[@as(usize, @intCast({s}))] = if (vf < 0) 0 else if (vf > 255) 255 else @as(u8, @intFromFloat(vf)); }} }}", .{ val, idx, idx, idx });
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
                        // For data array, use data_len / bytes_per_element for element count
                        stack[sp.*] = "@as(i32, @intCast(data_len / bytes_per_element))";
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
    // User atoms start at JS_ATOM_END (227), so atom index 227 = user atom 0 = "kind"
    var instrs = [_]Instruction{
        .{ .pc = 0, .opcode = .get_arg, .operand = .{ .arg = 0 }, .size = 2 },
        .{ .pc = 2, .opcode = .get_field, .operand = .{ .atom = 227 }, .size = 5 }, // atom 227 = user atom 0 = "kind"
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

    // Print generated code first for debugging
    std.debug.print("\n=== Generated getKind(node) code ===\n{s}\n=== End ===\n", .{code});

    // Verify structure - native shape access for "kind" property
    try std.testing.expect(std.mem.indexOf(u8, code, "__frozen_getKind") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "nativeGetKind") != null);
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
        .self_ref_var_idx = 0, // get_var_ref0 is self-reference
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

    // Print generated code first for debugging
    std.debug.print("\n=== Generated fib(n) with recursive calls ===\n{s}\n=== End ===\n", .{code});

    // Verify structure - note: uses CV.add/CV.sub, not zig_runtime.add/sub
    // Self-recursive functions use direct calls to __frozen_fib, not JSValue.call
    try std.testing.expect(std.mem.indexOf(u8, code, "__frozen_fib") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "block_id") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "__frozen_fib(ctx,") != null); // direct self-call
    try std.testing.expect(std.mem.indexOf(u8, code, "CV.add") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "CV.sub") != null);
    try std.testing.expect(std.mem.indexOf(u8, code, "CV.lt") != null);
}
