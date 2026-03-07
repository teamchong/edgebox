//! Thin Code Generator for Cold (Non-Hot) Frozen Functions
//!
//! Generates minimal Zig code that eliminates interpreter dispatch overhead
//! with minimum code size. Used for the ~10,628 "cold" functions that don't
//! justify full inline codegen.
//!
//! Design principles:
//!   - ONE call per opcode (no virtual stack, no expression folding)
//!   - Inline helpers for simple ops (2-10 instructions, inlined away by LLVM)
//!   - Noinline helpers for JS FFI boundary (call, get_field, etc.)
//!   - Switch-in-loop state machine for control flow (same as Relooper)
//!
//! Generated code is ~5-10x smaller per function than full codegen,
//! at the cost of missing optimizations (native loops, math fast paths, etc.)

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

/// Information about a function to generate (same fields as Relooper/Full)
pub const FunctionInfo = struct {
    name: []const u8,
    arg_count: u16,
    var_count: u16,
    cfg: *const CFG,
    is_self_recursive: bool,
    self_ref_var_idx: i16 = -1,
    closure_var_indices: []const u16 = &.{},
    closure_var_count: u32 = 0,
    atom_strings: []const []const u8 = &.{},
    partial_freeze: bool = false,
    js_name: []const u8 = &.{},
    has_use_strict: bool = false,
    is_async: bool = false,
    constants: []const module_parser.ConstValue = &.{},
    stack_size: u32 = 256,
    captured_local_indices: []const u16 = &.{},
};

/// Thin-tier code generator
pub const ThinCodeGen = struct {
    allocator: Allocator,
    output: std.ArrayListUnmanaged(u8),
    func: FunctionInfo,
    indent_level: u32 = 0,

    // Flags (determined by pre-scan)
    uses_this_val: bool = false,
    has_put_arg: bool = false,
    max_arg_idx_used: u32 = 0,
    has_fclosure: bool = false,
    has_for_of: bool = false,
    ic_count: u32 = 0,
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
        self.output.deinit(self.allocator);
    }

    // ================================================================
    // Output Helpers
    // ================================================================

    fn write(self: *Self, data: []const u8) !void {
        try self.output.appendSlice(self.allocator, data);
    }

    fn writeLine(self: *Self, line: []const u8) !void {
        try self.writeIndent();
        try self.write(line);
        try self.write("\n");
    }

    fn print(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        var buf: [4096]u8 = undefined;
        const s = std.fmt.bufPrint(&buf, fmt, args) catch {
            // Buffer too small - use allocator
            const allocated = try std.fmt.allocPrint(self.allocator, fmt, args);
            defer self.allocator.free(allocated);
            try self.write(allocated);
            return;
        };
        try self.write(s);
    }

    fn printLine(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        try self.writeIndent();
        try self.print(fmt, args);
        try self.write("\n");
    }

    fn writeIndent(self: *Self) !void {
        for (0..self.indent_level) |_| {
            try self.write("    ");
        }
    }

    fn pushIndent(self: *Self) void {
        self.indent_level += 1;
    }

    fn popIndent(self: *Self) void {
        if (self.indent_level > 0) self.indent_level -= 1;
    }

    fn nextIcSlot(self: *Self) u32 {
        const idx = self.ic_count;
        self.ic_count += 1;
        return idx;
    }

    // ================================================================
    // Pre-scan
    // ================================================================

    fn scanFlags(self: *Self) void {
        for (self.func.cfg.blocks.items) |block| {
            for (block.instructions) |instr| {
                switch (instr.opcode) {
                    .push_this, .check_ctor, .check_ctor_return => self.uses_this_val = true,
                    .fclosure, .fclosure8 => self.has_fclosure = true,
                    .for_of_start => self.has_for_of = true,
                    .put_arg, .put_arg0, .put_arg1, .put_arg2, .put_arg3,
                    .set_arg, .set_arg0, .set_arg1, .set_arg2, .set_arg3,
                    => self.has_put_arg = true,
                    else => {},
                }
                // Track max arg index
                switch (instr.opcode) {
                    .get_arg0, .put_arg0, .set_arg0 => self.max_arg_idx_used = @max(self.max_arg_idx_used, 1),
                    .get_arg1, .put_arg1, .set_arg1 => self.max_arg_idx_used = @max(self.max_arg_idx_used, 2),
                    .get_arg2, .put_arg2, .set_arg2 => self.max_arg_idx_used = @max(self.max_arg_idx_used, 3),
                    .get_arg3, .put_arg3, .set_arg3 => self.max_arg_idx_used = @max(self.max_arg_idx_used, 4),
                    .get_arg, .put_arg, .set_arg => self.max_arg_idx_used = @max(self.max_arg_idx_used, instr.operand.arg + 1),
                    else => {},
                }
            }
        }
    }

    fn scanIcSlots(self: *Self) u32 {
        var count: u32 = 0;
        for (self.func.cfg.blocks.items) |block| {
            for (block.instructions) |instr| {
                switch (instr.opcode) {
                    .get_field, .get_field2 => {
                        if (self.getAtomString(instr.operand.atom)) |_| {
                            count += 1;
                        }
                    },
                    else => {},
                }
            }
        }
        return count;
    }

    // ================================================================
    // Atom String Lookup
    // ================================================================

    fn getAtomString(self: *Self, atom_idx: u32) ?[]const u8 {
        if (atom_idx < JS_ATOM_END) {
            if (atom_idx < module_parser.BUILTIN_ATOMS.len) {
                const name = module_parser.BUILTIN_ATOMS[atom_idx];
                if (name.len > 0 and (name.len < 1 or name[0] != '<')) {
                    return name;
                }
            }
            return null;
        }
        const adjusted_idx = atom_idx - JS_ATOM_END;
        if (adjusted_idx < self.func.atom_strings.len) {
            const str = self.func.atom_strings[adjusted_idx];
            if (str.len > 0) return str;
        }
        return null;
    }

    fn escapeString(self: *Self, input: []const u8) []u8 {
        // Count chars that need escaping (worst case: each byte -> \xNN = 4 chars)
        var extra: usize = 0;
        for (input) |c| {
            if (c == '"' or c == '\\' or c == '\n' or c == '\r' or c == '\t') {
                extra += 1; // 1 byte -> 2 bytes
            } else if (c < 0x20 or c == 0x7f) {
                extra += 3; // 1 byte -> 4 bytes (\xNN)
            }
        }
        const result = self.allocator.alloc(u8, input.len + extra) catch return @constCast(input);
        var i: usize = 0;
        for (input) |c| {
            switch (c) {
                '"' => {
                    result[i] = '\\';
                    i += 1;
                    result[i] = '"';
                },
                '\\' => {
                    result[i] = '\\';
                    i += 1;
                    result[i] = '\\';
                },
                '\n' => {
                    result[i] = '\\';
                    i += 1;
                    result[i] = 'n';
                },
                '\r' => {
                    result[i] = '\\';
                    i += 1;
                    result[i] = 'r';
                },
                '\t' => {
                    result[i] = '\\';
                    i += 1;
                    result[i] = 't';
                },
                else => {
                    if (c < 0x20 or c == 0x7f) {
                        // Escape non-printable control characters as \xNN
                        const hex = "0123456789abcdef";
                        result[i] = '\\';
                        result[i + 1] = 'x';
                        result[i + 2] = hex[c >> 4];
                        result[i + 3] = hex[c & 0xf];
                        i += 3; // +1 below
                    } else {
                        result[i] = c;
                    }
                },
            }
            i += 1;
        }
        return result[0..i];
    }

    // ================================================================
    // Shared parameter emission helpers
    // ================================================================

    /// Emit the common cleanup parameter suffix: local_count, locals_jsv, var_ref_list, arg_shadow, arg_count
    /// Used by exceptionCleanup, returnFromStack, and returnUndef calls.
    fn emitCleanupParams(self: *Self) !void {
        try self.print("{d}, ", .{self.func.var_count});
        if (self.has_fclosure and self.func.var_count > 0) {
            try self.write("&_locals_jsv, _var_ref_list, ");
        } else {
            try self.write("null, null, ");
        }
        if (self.has_put_arg) {
            const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
            try self.print("&arg_shadow, {d}", .{arg_count});
        } else {
            try self.write("null, 0");
        }
    }

    /// Emit captured_local_indices as a Zig array literal: &[_]u16{ 0, 3, ... }
    fn emitCapturedIndicesLiteral(self: *Self) !void {
        const indices = self.func.captured_local_indices;
        if (indices.len == 0) {
            try self.write("&[_]u16{}");
        } else {
            try self.write("&[_]u16{ ");
            for (indices, 0..) |idx, i| {
                if (i > 0) try self.write(", ");
                try self.print("{d}", .{idx});
            }
            try self.write(" }");
        }
    }

    /// Emit closure-related parameters for op_call/op_call_method/op_apply:
    /// locals, locals_jsv, var_count, closure_alive, captured_indices
    fn emitCallClosureParams(self: *Self) !void {
        if (self.has_fclosure and self.func.var_count > 0) {
            try self.print("&locals, &_locals_jsv, {d}, &_closure_alive, ", .{self.func.var_count});
        } else {
            try self.print("&locals, null, {d}, null, ", .{self.func.var_count});
        }
        try self.emitCapturedIndicesLiteral();
    }

    // ================================================================
    // Exception cleanup expression
    // ================================================================

    fn emitExceptionCleanup(self: *Self) !void {
        try self.writeIndent();
        try self.write("return zig_runtime.thin.exceptionCleanup(ctx, &locals, ");
        try self.emitCleanupParams();
        try self.write(");\n");
    }

    /// Emit a catch-return pattern for operations that return GetFieldError
    fn emitCatchReturn(self: *Self) !void {
        try self.write(" catch {\n");
        self.pushIndent();
        try self.emitExceptionCleanup();
        self.popIndent();
        try self.writeIndent();
        try self.write("};\n");
    }

    /// Emit a return expression with proper cleanup (dup return value, free stack/closures/locals)
    fn emitReturnCleanup(self: *Self) !void {
        try self.writeIndent();
        try self.write("return zig_runtime.thin.returnFromStack(ctx, &stack, sp, &locals, ");
        try self.emitCleanupParams();
        try self.write(");\n");
    }

    // ================================================================
    // Main Generate
    // ================================================================

    pub fn generate(self: *Self) ![]u8 {
        self.scanFlags();
        const total_ic_slots = self.scanIcSlots();

        // Function signature
        try self.print(
            "pub noinline fn __frozen_{s}(ctx: *zig_runtime.JSContext, this_val: zig_runtime.JSValue, argc: c_int, argv: [*]zig_runtime.JSValue, var_refs: ?[*]*zig_runtime.JSVarRef, closure_var_count: c_int, cpool: ?[*]zig_runtime.JSValue) callconv(.c) zig_runtime.JSValue {{\n",
            .{self.func.name},
        );
        self.pushIndent();

        // Suppress unused warnings
        if (!self.uses_this_val) {
            try self.writeLine("_ = this_val;");
        }
        try self.writeLine("_ = @as(usize, @intCast(argc)) +% @intFromPtr(argv) +% @intFromPtr(var_refs) +% @as(usize, @intCast(closure_var_count)) +% @intFromPtr(cpool);");

        // Stack overflow check
        try self.writeLine("if (zig_runtime.checkStack()) {");
        self.pushIndent();
        try self.writeLine("return zig_runtime.JSValue.throwRangeError(ctx, \"Maximum call stack size exceeded\");");
        self.popIndent();
        try self.writeLine("}");
        try self.writeLine("defer zig_runtime.exitStack();");
        try self.writeLine("");

        // Note: JSValue alias is provided by the shard header; CV is needed locally
        try self.writeLine("const CV = zig_runtime.CompressedValue;");
        try self.writeLine("");

        // Locals
        const min_locals = @max(self.func.var_count, 1);
        try self.printLine("var locals: [{d}]CV = .{{CV.UNDEFINED}} ** {d};", .{ min_locals, min_locals });
        try self.writeLine("_ = &locals;");

        // V2 closure support
        if (self.has_fclosure and self.func.var_count > 0) {
            try self.printLine("var _locals_jsv: [{d}]zig_runtime.JSValue = .{{zig_runtime.JSValue.UNDEFINED}} ** {d};", .{ self.func.var_count, self.func.var_count });
            try self.writeLine("_ = &_locals_jsv;"); // Pin to stack - prevent optimizer from splitting/reusing slots
            try self.writeLine("var _closure_alive: bool = false;");
            try self.writeLine("const _var_ref_list: *zig_runtime.ListHead = @ptrCast(@alignCast(zig_runtime.quickjs.js_malloc(ctx, @sizeOf(zig_runtime.ListHead)) orelse return zig_runtime.JSValue.EXCEPTION));");
            try self.writeLine("defer zig_runtime.quickjs.js_free(ctx, @ptrCast(_var_ref_list));");
            try self.writeLine("zig_runtime.quickjs.js_frozen_var_ref_list_init(_var_ref_list);");
        }
        try self.writeLine("");

        // Arg shadow — sized to declared arg count (not argc), handles default params
        const arg_count = @max(self.func.arg_count, self.max_arg_idx_used);
        if (arg_count > 0 and self.has_put_arg) {
            try self.printLine("var arg_shadow: [{d}]CV = undefined;", .{arg_count});
            for (0..arg_count) |i| {
                try self.printLine("arg_shadow[{d}] = if ({d} < argc) CV.fromJSValue(JSValue.dup(ctx, argv[{d}])) else CV.UNDEFINED;", .{ i, i, i });
            }
            try self.writeLine("_ = &arg_shadow;");
            try self.writeLine("");
        }

        // Stack
        const stack_array_size = @max(self.func.var_count + self.func.stack_size, 4);
        try self.printLine("var stack: [{d}]CV = .{{CV.UNDEFINED}} ** {d};", .{ stack_array_size, stack_array_size });
        try self.writeLine("var sp: usize = 0;");
        if (self.has_for_of) {
            try self.writeLine("var for_of_iter_stack: [8]usize = .{0} ** 8;");
            try self.writeLine("var for_of_depth: usize = 0;");
            try self.writeLine("_ = &stack; _ = &sp; _ = &for_of_iter_stack; _ = &for_of_depth;");
        } else {
            try self.writeLine("_ = &stack; _ = &sp;");
        }

        // IC slots
        if (total_ic_slots > 0) {
            try self.printLine("const _IC = struct {{ var s: [{d}]zig_runtime.ICSlot = .{{zig_runtime.ICSlot.ZERO}} ** {d}; }};", .{ total_ic_slots, total_ic_slots });
            try self.writeLine("_ = &_IC;");
        }
        try self.writeLine("");

        // Generate blocks
        const blocks = self.func.cfg.blocks.items;
        if (blocks.len == 1) {
            // Single block: emit instructions directly, no state machine overhead
            self.block_terminated = false;
            for (blocks[0].instructions) |instr| {
                if (self.block_terminated) break;
                try self.emitInstruction(instr);
            }
            if (!self.block_terminated) {
                try self.emitBlockTerminator(blocks[0]);
            }
        } else {
            // Multi-block: state machine with switch/while
            try self.writeLine("var next_block: u32 = 0;");
            try self.writeLine("_ = &next_block;");
            try self.writeLine("");
            try self.writeLine("machine: while (true) {");
            self.pushIndent();
            try self.writeLine("switch (next_block) {");
            self.pushIndent();

            for (blocks, 0..) |block, idx| {
                try self.emitBlock(block, @intCast(idx));
            }

            // Default case
            try self.writeLine("else => { continue :machine; },");

            self.popIndent();
            try self.writeLine("}");
            self.popIndent();
            try self.writeLine("}");
        }

        self.popIndent();
        try self.writeLine("}");

        return try self.output.toOwnedSlice(self.allocator);
    }

    // ================================================================
    // Block Emission
    // ================================================================

    fn emitBlock(self: *Self, block: BasicBlock, block_idx: u32) !void {
        try self.printLine("{d} => {{", .{block_idx});
        self.pushIndent();

        self.block_terminated = false;

        // Emit each instruction
        for (block.instructions) |instr| {
            if (self.block_terminated) break;
            try self.emitInstruction(instr);
        }

        // Block terminator (control flow)
        if (!self.block_terminated) {
            try self.emitBlockTerminator(block);
        }

        self.popIndent();
        try self.writeLine("},");
    }

    // ================================================================
    // Instruction Emission — One-line-per-opcode
    // ================================================================

    fn emitInstruction(self: *Self, instr: Instruction) !void {
        switch (instr.opcode) {
            // ========== Constants ==========
            .push_0 => try self.writeLine("zig_runtime.thin.push_i32(&stack, &sp, 0);"),
            .push_1 => try self.writeLine("zig_runtime.thin.push_i32(&stack, &sp, 1);"),
            .push_2 => try self.writeLine("zig_runtime.thin.push_i32(&stack, &sp, 2);"),
            .push_3 => try self.writeLine("zig_runtime.thin.push_i32(&stack, &sp, 3);"),
            .push_4 => try self.writeLine("zig_runtime.thin.push_i32(&stack, &sp, 4);"),
            .push_5 => try self.writeLine("zig_runtime.thin.push_i32(&stack, &sp, 5);"),
            .push_6 => try self.writeLine("zig_runtime.thin.push_i32(&stack, &sp, 6);"),
            .push_7 => try self.writeLine("zig_runtime.thin.push_i32(&stack, &sp, 7);"),
            .push_minus1 => try self.writeLine("zig_runtime.thin.push_i32(&stack, &sp, -1);"),
            .push_i8 => try self.printLine("zig_runtime.thin.push_i32(&stack, &sp, {d});", .{instr.operand.i8}),
            .push_i16 => try self.printLine("zig_runtime.thin.push_i32(&stack, &sp, {d});", .{instr.operand.i16}),
            .push_i32 => try self.printLine("zig_runtime.thin.push_i32(&stack, &sp, {d});", .{instr.operand.i32}),
            .push_true => try self.writeLine("zig_runtime.thin.push_true(&stack, &sp);"),
            .push_false => try self.writeLine("zig_runtime.thin.push_false(&stack, &sp);"),
            .null => try self.writeLine("zig_runtime.thin.push_null(&stack, &sp);"),
            .undefined => try self.writeLine("zig_runtime.thin.push_undefined(&stack, &sp);"),
            .push_empty_string => try self.writeLine("zig_runtime.thin.push_cv(&stack, &sp, CV.fromJSValue(JSValue.newString(ctx, \"\")));"),

            .push_atom_value => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |str_val| {
                    const escaped = self.escapeString(str_val);
                    defer self.allocator.free(escaped);
                    try self.printLine("zig_runtime.thin.push_cv(&stack, &sp, CV.fromJSValue(JSValue.newString(ctx, \"{s}\")));", .{escaped});
                } else {
                    try self.writeLine("zig_runtime.thin.push_undefined(&stack, &sp);");
                }
            },

            .push_const8, .push_const => {
                const const_idx = instr.operand.const_idx;
                try self.printLine("zig_runtime.thin.push_cv(&stack, &sp, if (cpool) |cp| CV.fromJSValue(JSValue.dup(ctx, cp[{d}])) else CV.UNDEFINED);", .{const_idx});
            },

            // ========== Local Variables ==========
            .get_loc0 => try self.writeLine("zig_runtime.thin.get_loc(ctx, &stack, &sp, &locals, 0);"),
            .get_loc1 => try self.writeLine("zig_runtime.thin.get_loc(ctx, &stack, &sp, &locals, 1);"),
            .get_loc2 => try self.writeLine("zig_runtime.thin.get_loc(ctx, &stack, &sp, &locals, 2);"),
            .get_loc3 => try self.writeLine("zig_runtime.thin.get_loc(ctx, &stack, &sp, &locals, 3);"),
            .get_loc, .get_loc8 => try self.printLine("zig_runtime.thin.get_loc(ctx, &stack, &sp, &locals, {d});", .{instr.operand.loc}),
            .get_loc0_loc1 => {
                try self.writeLine("zig_runtime.thin.get_loc(ctx, &stack, &sp, &locals, 0);");
                try self.writeLine("zig_runtime.thin.get_loc(ctx, &stack, &sp, &locals, 1);");
            },

            .put_loc0 => try self.emitPutLoc(0),
            .put_loc1 => try self.emitPutLoc(1),
            .put_loc2 => try self.emitPutLoc(2),
            .put_loc3 => try self.emitPutLoc(3),
            .put_loc, .put_loc8 => try self.emitPutLoc(instr.operand.loc),

            .set_loc0 => try self.emitSetLoc(0),
            .set_loc1 => try self.emitSetLoc(1),
            .set_loc2 => try self.emitSetLoc(2),
            .set_loc3 => try self.emitSetLoc(3),
            .set_loc, .set_loc8 => try self.emitSetLoc(instr.operand.loc),

            // ========== Arguments ==========
            .get_arg0, .get_arg1, .get_arg2, .get_arg3, .get_arg => {
                const arg_idx: u16 = switch (instr.opcode) {
                    .get_arg0 => 0,
                    .get_arg1 => 1,
                    .get_arg2 => 2,
                    .get_arg3 => 3,
                    else => instr.operand.arg,
                };
                if (self.has_put_arg) {
                    try self.printLine("zig_runtime.thin.get_arg_shadow(&stack, &sp, &arg_shadow, {d});", .{arg_idx});
                } else {
                    try self.printLine("zig_runtime.thin.get_arg(ctx, &stack, &sp, argc, argv, {d});", .{arg_idx});
                }
            },
            .put_arg0, .put_arg1, .put_arg2, .put_arg3, .put_arg => {
                const arg_idx: u16 = switch (instr.opcode) {
                    .put_arg0 => 0,
                    .put_arg1 => 1,
                    .put_arg2 => 2,
                    .put_arg3 => 3,
                    else => instr.operand.arg,
                };
                try self.printLine("zig_runtime.thin.put_arg(ctx, &stack, &sp, &arg_shadow, {d});", .{arg_idx});
            },
            .set_arg0, .set_arg1, .set_arg2, .set_arg3, .set_arg => {
                const arg_idx: u16 = switch (instr.opcode) {
                    .set_arg0 => 0,
                    .set_arg1 => 1,
                    .set_arg2 => 2,
                    .set_arg3 => 3,
                    else => instr.operand.arg,
                };
                // set_arg: copy top-of-stack → arg_shadow without popping
                try self.printLine("{{ const v = stack[sp-1]; const old = arg_shadow[{d}]; arg_shadow[{d}] = CV.dupRef(v); CV.freeRef(ctx, old); }}", .{ arg_idx, arg_idx });
            },

            // ========== Closure Variables ==========
            .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3, .get_var_ref => {
                const bytecode_idx: u16 = switch (instr.opcode) {
                    .get_var_ref0 => 0,
                    .get_var_ref1 => 1,
                    .get_var_ref2 => 2,
                    .get_var_ref3 => 3,
                    else => instr.operand.var_ref,
                };
                try self.printLine("zig_runtime.thin.get_var_ref(ctx, &stack, &sp, var_refs, {d}, {d});", .{ bytecode_idx, self.func.closure_var_count });
            },
            .get_var_ref_check => {
                const bytecode_idx = instr.operand.var_ref;
                try self.printLine("stack[sp] = CV.fromJSValue(zig_runtime.getClosureVarCheckSafe(ctx, var_refs, {d}, {d})); sp += 1;", .{ bytecode_idx, self.func.closure_var_count });
                try self.writeLine("if (stack[sp-1].isException()) return stack[sp-1].toJSValueWithCtx(ctx);");
            },
            .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3, .put_var_ref => {
                const bytecode_idx: u16 = switch (instr.opcode) {
                    .put_var_ref0 => 0,
                    .put_var_ref1 => 1,
                    .put_var_ref2 => 2,
                    .put_var_ref3 => 3,
                    else => instr.operand.var_ref,
                };
                try self.printLine("zig_runtime.thin.put_var_ref(ctx, &stack, &sp, var_refs, {d}, {d});", .{ bytecode_idx, self.func.closure_var_count });
            },
            .put_var_ref_check => {
                const bytecode_idx = instr.operand.var_ref;
                try self.printLine("sp -= 1; _ = zig_runtime.setClosureVarCheckSafe(ctx, var_refs, {d}, {d}, stack[sp].toJSValueWithCtx(ctx));", .{ bytecode_idx, self.func.closure_var_count });
            },
            .put_var_ref_check_init => {
                const bytecode_idx = instr.operand.var_ref;
                try self.printLine("sp -= 1; zig_runtime.setClosureVarSafe(ctx, var_refs, {d}, {d}, stack[sp].toJSValueWithCtx(ctx));", .{ bytecode_idx, self.func.closure_var_count });
            },
            .set_var_ref0, .set_var_ref1, .set_var_ref2, .set_var_ref3, .set_var_ref => {
                const bytecode_idx: u16 = switch (instr.opcode) {
                    .set_var_ref0 => 0,
                    .set_var_ref1 => 1,
                    .set_var_ref2 => 2,
                    .set_var_ref3 => 3,
                    else => instr.operand.var_ref,
                };
                try self.printLine("zig_runtime.thin.set_var_ref(ctx, &stack, &sp, var_refs, {d}, {d});", .{ bytecode_idx, self.func.closure_var_count });
            },

            // ========== Stack Operations ==========
            .dup => try self.writeLine("zig_runtime.thin.dup_top(ctx, &stack, &sp);"),
            .dup1 => try self.writeLine("zig_runtime.thin.dup1(ctx, &stack, &sp);"),
            .dup2 => try self.writeLine("zig_runtime.thin.dup2(ctx, &stack, &sp);"),
            .dup3 => try self.writeLine("zig_runtime.thin.dup3(ctx, &stack, &sp);"),
            .drop => try self.writeLine("zig_runtime.thin.drop(ctx, &stack, &sp);"),
            .nip => try self.writeLine("zig_runtime.thin.nip(ctx, &stack, &sp);"),
            .swap => try self.writeLine("zig_runtime.thin.swap(&stack, &sp);"),

            // ========== Arithmetic ==========
            .add => try self.writeLine("zig_runtime.thin.op_add(ctx, &stack, &sp);"),
            .sub => try self.writeLine("zig_runtime.thin.op_sub(&stack, &sp);"),
            .mul => try self.writeLine("zig_runtime.thin.op_mul(&stack, &sp);"),
            .div => try self.writeLine("zig_runtime.thin.op_div(&stack, &sp);"),
            .mod => try self.writeLine("zig_runtime.thin.op_mod(&stack, &sp);"),
            .pow => try self.writeLine("zig_runtime.thin.op_pow(&stack, &sp);"),
            .neg => try self.writeLine("zig_runtime.thin.op_neg(&stack, &sp);"),
            .plus => try self.writeLine("zig_runtime.thin.op_plus(ctx, &stack, &sp);"),

            // ========== Bitwise ==========
            .@"and" => try self.writeLine("zig_runtime.thin.op_band(&stack, &sp);"),
            .@"or" => try self.writeLine("zig_runtime.thin.op_bor(&stack, &sp);"),
            .xor => try self.writeLine("zig_runtime.thin.op_bxor(&stack, &sp);"),
            .shl => try self.writeLine("zig_runtime.thin.op_shl(&stack, &sp);"),
            .sar => try self.writeLine("zig_runtime.thin.op_sar(&stack, &sp);"),
            .shr => try self.writeLine("zig_runtime.thin.op_shr(&stack, &sp);"),
            .not => try self.writeLine("zig_runtime.thin.op_bnot(&stack, &sp);"),

            // ========== Comparison ==========
            .lt => try self.writeLine("zig_runtime.thin.op_lt(&stack, &sp);"),
            .lte => try self.writeLine("zig_runtime.thin.op_lte(&stack, &sp);"),
            .gt => try self.writeLine("zig_runtime.thin.op_gt(&stack, &sp);"),
            .gte => try self.writeLine("zig_runtime.thin.op_gte(&stack, &sp);"),
            .eq => try self.writeLine("zig_runtime.thin.op_eq(ctx, &stack, &sp);"),
            .neq => try self.writeLine("zig_runtime.thin.op_neq(ctx, &stack, &sp);"),
            .strict_eq => try self.writeLine("zig_runtime.thin.op_strict_eq(ctx, &stack, &sp);"),
            .strict_neq => try self.writeLine("zig_runtime.thin.op_strict_neq(ctx, &stack, &sp);"),

            // ========== Logical / Type ==========
            .lnot => try self.writeLine("zig_runtime.thin.op_lnot(ctx, &stack, &sp);"),
            .typeof => try self.writeLine("zig_runtime.thin.op_typeof(ctx, &stack, &sp);"),

            // ========== Increment / Decrement ==========
            .inc_loc => try self.printLine("zig_runtime.thin.inc_loc(&locals, {d});", .{instr.operand.loc}),
            .dec_loc => try self.printLine("zig_runtime.thin.dec_loc(&locals, {d});", .{instr.operand.loc}),
            .inc => try self.writeLine("stack[sp-1] = CV.add(stack[sp-1], CV.newInt(1));"),
            .dec => try self.writeLine("stack[sp-1] = CV.sub(stack[sp-1], CV.newInt(1));"),
            .post_inc => try self.writeLine("{ const _v = stack[sp - 1]; stack[sp] = CV.add(_v, CV.newInt(1)); sp += 1; }"),
            .post_dec => try self.writeLine("{ const _v = stack[sp - 1]; stack[sp] = CV.sub(_v, CV.newInt(1)); sp += 1; }"),

            // ========== Property Access ==========
            .get_field => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |prop_name| {
                    const escaped = self.escapeString(prop_name);
                    defer self.allocator.free(escaped);
                    const ic_idx = self.nextIcSlot();
                    try self.writeIndent();
                    try self.print("zig_runtime.thin.op_get_field_ic(ctx, &stack, &sp, &_IC.s[{d}], \"{s}\")", .{ ic_idx, escaped });
                    try self.emitCatchReturn();
                } else {
                    try self.writeLine("return zig_runtime.JSValue.throwTypeError(ctx, \"Invalid property access\");");
                    self.block_terminated = true;
                }
            },
            .get_field2 => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |prop_name| {
                    const escaped = self.escapeString(prop_name);
                    defer self.allocator.free(escaped);
                    const ic_idx = self.nextIcSlot();
                    try self.writeIndent();
                    try self.print("zig_runtime.thin.op_get_field2_ic(ctx, &stack, &sp, &_IC.s[{d}], \"{s}\")", .{ ic_idx, escaped });
                    try self.emitCatchReturn();
                } else {
                    try self.writeLine("return zig_runtime.JSValue.throwTypeError(ctx, \"Invalid property access\");");
                    self.block_terminated = true;
                }
            },
            .put_field => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |prop_name| {
                    const escaped = self.escapeString(prop_name);
                    defer self.allocator.free(escaped);
                    try self.writeIndent();
                    try self.print("zig_runtime.thin.op_put_field(ctx, &stack, &sp, \"{s}\")", .{escaped});
                    try self.emitCatchReturn();
                } else {
                    try self.writeLine("return zig_runtime.JSValue.throwTypeError(ctx, \"Invalid property access\");");
                    self.block_terminated = true;
                }
            },

            // ========== Array Access ==========
            .get_array_el => {
                try self.writeLine("{ const idx = stack[sp-1]; const arr = stack[sp-2]; stack[sp-2] = CV.fromJSValue(JSValue.getPropertyValue(ctx, arr.toJSValueWithCtx(ctx), JSValue.dup(ctx, idx.toJSValueWithCtx(ctx)))); CV.freeRef(ctx, arr); CV.freeRef(ctx, idx); sp -= 1; }");
            },
            .get_array_el2 => {
                try self.writeLine("{ const idx = stack[sp-1]; const arr = stack[sp-2]; var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, idx.toJSValueWithCtx(ctx)); stack[sp-1] = CV.fromJSValue(JSValue.getPropertyUint32(ctx, arr.toJSValueWithCtx(ctx), @intCast(idx_i32))); }");
            },
            .put_array_el => {
                try self.writeLine("zig_runtime.thin.op_put_array_el(ctx, &stack, &sp);");
            },

            // ========== Global Variables ==========
            .get_var => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |var_name| {
                    const escaped = self.escapeString(var_name);
                    defer self.allocator.free(escaped);
                    try self.printLine("zig_runtime.thin.push_cv(&stack, &sp, CV.fromJSValue(JSValue.getGlobal(ctx, \"{s}\")));", .{escaped});
                } else {
                    try self.writeLine("zig_runtime.thin.push_undefined(&stack, &sp);");
                }
            },
            .get_var_undef => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |var_name| {
                    const escaped = self.escapeString(var_name);
                    defer self.allocator.free(escaped);
                    try self.printLine("zig_runtime.thin.push_cv(&stack, &sp, CV.fromJSValue(JSValue.getGlobalUndef(ctx, \"{s}\")));", .{escaped});
                } else {
                    try self.writeLine("zig_runtime.thin.push_undefined(&stack, &sp);");
                }
            },
            .put_var => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |var_name| {
                    const escaped = self.escapeString(var_name);
                    defer self.allocator.free(escaped);
                    try self.printLine("sp -= 1; _ = JSValue.setGlobal(ctx, \"{s}\", stack[sp].toJSValueWithCtx(ctx));", .{escaped});
                } else {
                    try self.writeLine("sp -= 1;");
                }
            },

            // ========== Object/Array Creation ==========
            .object => try self.writeLine("zig_runtime.thin.push_cv(&stack, &sp, CV.fromJSValue(zig_runtime.jsNewObject(ctx)));"),
            .array_from => {
                const count = instr.operand.u16;
                try self.printLine("{{ const _arr = JSValue.newArray(ctx); var _i: usize = 0; while (_i < {d}) : (_i += 1) {{ _ = JSValue.setIndex(ctx, _arr, @intCast(_i), JSValue.dup(ctx, stack[sp - {d} + _i].toJSValueWithCtx(ctx))); }} sp -= {d}; stack[sp] = CV.fromJSValue(_arr); sp += 1; }}", .{ count, count, count });
            },

            // ========== This ==========
            .push_this => {
                if (self.func.has_use_strict) {
                    try self.writeLine("zig_runtime.thin.push_cv(&stack, &sp, CV.fromJSValue(JSValue.dup(ctx, this_val)));");
                } else {
                    try self.writeLine("zig_runtime.thin.push_cv(&stack, &sp, CV.fromJSValue(if (this_val.isUndefined() or this_val.isNull()) JSValue.getGlobal(ctx, \"globalThis\") else JSValue.dup(ctx, this_val)));");
                }
            },

            // ========== Type checks ==========
            .instanceof => {
                try self.writeLine("{ const ctor = stack[sp-1]; const obj = stack[sp-2]; stack[sp-2] = if (JSValue.isInstanceOf(ctx, obj.toJSValueWithCtx(ctx), ctor.toJSValueWithCtx(ctx))) CV.TRUE else CV.FALSE; CV.freeRef(ctx, obj); CV.freeRef(ctx, ctor); sp -= 1; }");
            },
            .is_undefined => try self.writeLine("stack[sp-1] = if (stack[sp-1].isUndefined()) CV.TRUE else CV.FALSE;"),
            .is_null => try self.writeLine("stack[sp-1] = if (stack[sp-1].isNull()) CV.TRUE else CV.FALSE;"),
            .is_undefined_or_null => try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = if (v.isUndefined() or v.isNull()) CV.TRUE else CV.FALSE; }"),
            .typeof_is_function => try self.writeLine("{ const v = stack[sp-1]; stack[sp-1] = if (zig_runtime.quickjs.JS_IsFunction(ctx, v.toJSValueWithCtx(ctx))) CV.TRUE else CV.FALSE; }"),
            .typeof_is_undefined => try self.writeLine("stack[sp-1] = if (stack[sp-1].isUndefined()) CV.TRUE else CV.FALSE;"),

            // ========== Length ==========
            .get_length => try self.writeLine("{ const _old = stack[sp-1]; stack[sp-1] = zig_runtime.nativeGetLengthCV(ctx, _old.toJSValueWithCtx(ctx)); CV.freeRef(ctx, _old); }"),

            // ========== Conversions ==========
            .to_object => try self.writeLine("{ const _old = stack[sp-1]; stack[sp-1] = CV.fromJSValue(JSValue.toObject(ctx, _old.toJSValueWithCtx(ctx))); CV.freeRef(ctx, _old); }"),
            .to_propkey => try self.writeLine("{ const _old = stack[sp-1]; stack[sp-1] = CV.fromJSValue(JSValue.toPropKey(ctx, _old.toJSValueWithCtx(ctx))); CV.freeRef(ctx, _old); }"),
            .to_propkey2 => try self.writeLine("{ const _old = stack[sp-1]; stack[sp-1] = CV.fromJSValue(JSValue.toPropKey(ctx, _old.toJSValueWithCtx(ctx))); CV.freeRef(ctx, _old); }"),

            // ========== Rotations ==========
            .rot3l => try self.writeLine("{ const t = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = t; }"),
            .rot3r => try self.writeLine("{ const t = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = stack[sp-3]; stack[sp-3] = t; }"),
            .rot4l => try self.writeLine("{ const t = stack[sp-4]; stack[sp-4] = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = t; }"),
            .rot5l => try self.writeLine("{ const t = stack[sp-5]; stack[sp-5] = stack[sp-4]; stack[sp-4] = stack[sp-3]; stack[sp-3] = stack[sp-2]; stack[sp-2] = stack[sp-1]; stack[sp-1] = t; }"),
            .perm3 => try self.writeLine("{ const t = stack[sp-2]; stack[sp-2] = stack[sp-3]; stack[sp-3] = t; }"),
            .perm4 => try self.writeLine("{ const t = stack[sp-2]; stack[sp-2] = stack[sp-4]; stack[sp-4] = t; }"),
            .perm5 => try self.writeLine("{ const t = stack[sp-2]; stack[sp-2] = stack[sp-5]; stack[sp-5] = t; }"),
            .insert2 => try self.writeLine("{ const t = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = t; stack[sp] = CV.dupRef(t); sp += 1; }"),
            .insert3 => try self.writeLine("{ const t = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = stack[sp-3]; stack[sp-3] = t; stack[sp] = CV.dupRef(t); sp += 1; }"),
            .insert4 => try self.writeLine("{ const t = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = stack[sp-3]; stack[sp-3] = stack[sp-4]; stack[sp-4] = t; stack[sp] = CV.dupRef(t); sp += 1; }"),

            // ========== Function Calls ==========
            .call0 => try self.emitCall(0),
            .call1 => try self.emitCall(1),
            .call2 => try self.emitCall(2),
            .call3 => try self.emitCall(3),
            .call => try self.emitCall(instr.operand.u16),
            .tail_call => try self.emitCall(instr.operand.u16),
            .call_method => try self.emitCallMethod(instr.operand.u16),
            .tail_call_method => try self.emitCallMethod(instr.operand.u16),

            .call_constructor => {
                const argc_val = instr.operand.u16;
                try self.writeIndent();
                try self.print("zig_runtime.thin.op_call_constructor(ctx, &stack, &sp, {d})", .{argc_val});
                try self.emitCatchReturn();
            },

            // ========== Apply ==========
            .apply => {
                try self.writeIndent();
                try self.print("zig_runtime.thin.op_apply(ctx, &stack, &sp, ", .{});
                try self.emitCallClosureParams();
                try self.write(")");
                try self.emitCatchReturn();
            },

            // ========== Define Operations ==========
            .define_array_el => {
                try self.writeLine("{ const val = stack[sp-1]; const idx = stack[sp-2]; const arr = stack[sp-3]; var idx_i32: i32 = 0; _ = JSValue.toInt32(ctx, &idx_i32, idx.toJSValueWithCtx(ctx)); _ = JSValue.definePropertyUint32(ctx, arr.toJSValueWithCtx(ctx), @intCast(idx_i32), JSValue.dup(ctx, val.toJSValueWithCtx(ctx))); sp -= 1; }");
            },
            .define_field => {
                const atom_idx = instr.operand.atom;
                if (self.getAtomString(atom_idx)) |prop_name| {
                    const escaped = self.escapeString(prop_name);
                    defer self.allocator.free(escaped);
                    try self.printLine("{{ const val = stack[sp-1]; const obj = stack[sp-2]; _ = JSValue.definePropertyStr(ctx, obj.toJSValueWithCtx(ctx), \"{s}\", JSValue.dup(ctx, val.toJSValueWithCtx(ctx))); sp -= 1; }}", .{escaped});
                }
            },

            // ========== Return ==========
            .@"return" => {
                try self.emitReturnCleanup();
                self.block_terminated = true;
            },
            .return_undef => {
                try self.writeIndent();
                try self.write("return zig_runtime.thin.returnUndef(ctx, &locals, ");
                try self.emitCleanupParams();
                try self.write(");\n");
                self.block_terminated = true;
            },

            // ========== Throw ==========
            .throw => {
                // Throw sets the exception and returns EXCEPTION; cleanup is handled via exceptionCleanup
                try self.writeLine("{ const exc = stack[sp-1]; sp -= 1;");
                self.pushIndent();
                try self.writeLine("_ = JSValue.throw(ctx, exc.toJSValueWithCtx(ctx));");
                try self.emitExceptionCleanup();
                self.popIndent();
                try self.writeLine("}");
                self.block_terminated = true;
            },

            // ========== TDZ Checks ==========
            .get_loc_check => {
                const loc = instr.operand.loc;
                try self.printLine("if (locals[{d}].isUninitialized()) return JSValue.throwReferenceError(ctx, \"Cannot access before initialization\");", .{loc});
                try self.printLine("zig_runtime.thin.get_loc(ctx, &stack, &sp, &locals, {d});", .{loc});
            },
            .put_loc_check => {
                const loc = instr.operand.loc;
                try self.printLine("if (locals[{d}].isUninitialized()) return JSValue.throwReferenceError(ctx, \"Cannot access before initialization\");", .{loc});
                try self.emitPutLoc(loc);
            },
            .put_loc_check_init => {
                const loc = instr.operand.loc;
                try self.emitPutLoc(loc);
            },
            .set_loc_uninitialized => {
                const loc = instr.operand.loc;
                try self.printLine("locals[{d}] = CV.UNINITIALIZED;", .{loc});
            },

            // ========== Constructor Checks ==========
            .check_ctor => {
                try self.writeLine("if (!zig_runtime.isConstructorCall(ctx, this_val)) return JSValue.throwTypeError(ctx, \"Constructor requires 'new'\");");
            },
            .check_ctor_return => {
                // Must dup this_val — the CV on stack will be freed at cleanup
                try self.writeLine("if (stack[sp-1].isUndefined()) { sp -= 1; stack[sp] = CV.fromJSValue(JSValue.dup(ctx, this_val)); sp += 1; }");
            },

            // ========== Closures ==========
            .fclosure8, .fclosure => {
                const func_idx = instr.operand.const_idx;
                try self.writeLine("{");
                self.pushIndent();
                try self.printLine("const _bfunc = if (cpool) |cp| cp[{d}] else JSValue.UNDEFINED;", .{func_idx});
                // Register bytecode with child
                if (func_idx < self.func.constants.len) {
                    switch (self.func.constants[func_idx]) {
                        .child_func => |child| {
                            try self.printLine("native_dispatch.registerCpoolBytecodeByName(_bfunc, \"{s}@{d}\");", .{ child.name, child.line_num });
                        },
                        else => {},
                    }
                }
                // Sync modified args back to argv so child closure captures them.
                // Only sync indices < argc (args beyond argc aren't in argv).
                // Must dup: arg_shadow owns one ref, argv owns another.
                if (self.has_put_arg) {
                    const ac = @max(self.func.arg_count, self.max_arg_idx_used);
                    for (0..ac) |i| {
                        try self.printLine("if ({d} < argc) {{ const _old = argv[{d}]; argv[{d}] = JSValue.dup(ctx, arg_shadow[{d}].toJSValueWithCtx(ctx)); JSValue.free(ctx, _old); }}", .{ i, i, i, i });
                    }
                }
                const var_count = self.func.var_count;
                if (self.has_fclosure and var_count > 0) {
                    try self.printLine("for (0..{d}) |_i| {{ _locals_jsv[_i] = CV.toJSValuePtr(&locals[_i]); }}", .{var_count});
                    try self.printLine("const _closure = JSValue.createClosureV2(ctx, _bfunc, var_refs, _var_ref_list, &_locals_jsv, {d}, argv[0..@intCast(argc)]);", .{var_count});
                    try self.writeLine("_closure_alive = true;");
                } else if (var_count > 0) {
                    try self.printLine("var _locals_js: [{d}]zig_runtime.JSValue = undefined;", .{var_count});
                    try self.printLine("for (0..{d}) |_i| {{ _locals_js[_i] = CV.toJSValuePtr(&locals[_i]); }}", .{var_count});
                    try self.printLine("const _closure = JSValue.createClosure(ctx, _bfunc, var_refs, &_locals_js, {d}, argv[0..@intCast(argc)]);", .{var_count});
                } else {
                    try self.writeLine("const _closure = JSValue.createClosure(ctx, _bfunc, var_refs, null, 0, argv[0..@intCast(argc)]);");
                }
                try self.writeLine("stack[sp] = CV.fromJSValue(_closure); sp += 1;");
                self.popIndent();
                try self.writeLine("}");
            },
            .close_loc => {}, // no-op

            // ========== For-Of/For-In ==========
            .for_of_start => {
                try self.writeIndent();
                try self.write("zig_runtime.thin.op_for_of_start(ctx, &stack, &sp, &for_of_iter_stack, &for_of_depth)");
                try self.emitCatchReturn();
            },
            .for_of_next => {
                try self.writeIndent();
                try self.write("zig_runtime.thin.op_for_of_next(ctx, &stack, &sp, &for_of_iter_stack, &for_of_depth)");
                try self.emitCatchReturn();
            },
            .iterator_close => {
                try self.writeLine("zig_runtime.thin.op_iterator_close(ctx, &stack, &sp, &for_of_iter_stack, &for_of_depth);");
            },
            .iterator_get_value_done => {
                try self.writeLine("zig_runtime.thin.op_iterator_get_value_done(ctx, &stack, &sp);");
            },
            .for_in_start => {
                try self.writeIndent();
                try self.write("zig_runtime.thin.op_for_in_start(ctx, &stack, &sp)");
                try self.emitCatchReturn();
            },
            .for_in_next => {
                try self.writeIndent();
                try self.write("zig_runtime.thin.op_for_in_next(ctx, &stack, &sp)");
                try self.emitCatchReturn();
            },
            .in => {
                try self.writeIndent();
                try self.write("zig_runtime.thin.op_in(ctx, &stack, &sp)");
                try self.emitCatchReturn();
            },

            // ========== Special Object ==========
            .special_object => {
                const obj_type = instr.operand.u8;
                switch (obj_type) {
                    0, 1 => try self.writeLine("stack[sp] = CV.fromJSValue(zig_runtime.quickjs.JS_NewArguments(ctx, argc, argv)); sp += 1;"),
                    2, 3, 4 => try self.writeLine("stack[sp] = CV.UNDEFINED; sp += 1;"),
                    5 => try self.writeLine("stack[sp] = CV.fromJSValue(JSValue.newObject(ctx)); sp += 1;"),
                    6 => try self.writeLine("stack[sp] = CV.fromJSValue(zig_runtime.quickjs.JS_GetImportMetaCurrent(ctx)); sp += 1;"),
                    7 => try self.writeLine("stack[sp] = CV.fromJSValue(zig_runtime.quickjs.JS_NewObjectProto(ctx, zig_runtime.JSValue.NULL)); sp += 1;"),
                    else => {
                        try self.printLine("return JSValue.throwTypeError(ctx, \"Unknown special_object type: {d}\");", .{obj_type});
                        self.block_terminated = true;
                    },
                }
            },

            // ========== Rest / Spread ==========
            .rest => {
                const first_arg = instr.operand.u16;
                try self.printLine("stack[sp] = CV.fromJSValue(zig_runtime.makeRestArray(ctx, argc, argv, {d})); sp += 1;", .{first_arg});
            },
            .copy_data_properties => {
                // n_pop=3, n_push=3 → net stack effect = 0
                // Mask encodes stack offsets: bits 0-1 = target, bits 2-4 = source, bits 5-7 = exclude
                // Subsequent bytecode drop instructions handle stack cleanup
                const mask = instr.operand.u8;
                const target_off = @as(u32, mask & 3) + 1;
                const source_off = @as(u32, (mask >> 2) & 7) + 1;
                const exclude_off = @as(u32, (mask >> 5) & 7) + 1;
                try self.writeLine("{");
                self.pushIndent();
                try self.printLine("const _cdp_target = stack[sp - {d}].toJSValueWithCtx(ctx);", .{target_off});
                try self.printLine("const _cdp_source = stack[sp - {d}].toJSValueWithCtx(ctx);", .{source_off});
                try self.printLine("const _cdp_excluded = stack[sp - {d}].toJSValueWithCtx(ctx);", .{exclude_off});
                try self.writeLine("_ = zig_runtime.copyDataProperties(ctx, _cdp_target, _cdp_source, _cdp_excluded);");
                self.popIndent();
                try self.writeLine("}");
            },
            .append => {
                try self.writeLine("zig_runtime.thin.op_append(ctx, &stack, &sp);");
            },

            // ========== Class / Define Method ==========
            .define_method => {
                const atom = instr.operand.atom_u8.atom;
                const flags = instr.operand.atom_u8.value;
                const method_type = flags & 3;
                const is_enumerable = (flags & 4) != 0;
                try self.writeLine("{");
                try self.writeLine("    const _method = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("    const _obj = stack[sp-2].toJSValueWithCtx(ctx);");
                // Bytecode atom IDs are module-local and don't match runtime atom IDs.
                // Resolve to string at compile time, create runtime atom via JS_NewAtom.
                if (self.getAtomString(atom)) |prop_name| {
                    const escaped = self.escapeString(prop_name);
                    defer self.allocator.free(escaped);
                    try self.printLine("    const _atom = zig_runtime.quickjs.JS_NewAtom(ctx, \"{s}\");", .{escaped});
                    try self.writeLine("    defer zig_runtime.quickjs.JS_FreeAtom(ctx, _atom);");
                    if (method_type == 1) {
                        const enum_flag = if (is_enumerable) " | JSValue.JS_PROP_ENUMERABLE" else "";
                        try self.printLine("    _ = JSValue.definePropertyGetSet(ctx, _obj, _atom, _method, JSValue.UNDEFINED, JSValue.JS_PROP_CONFIGURABLE | JSValue.JS_PROP_HAS_GET{s});", .{enum_flag});
                    } else if (method_type == 2) {
                        const enum_flag = if (is_enumerable) " | JSValue.JS_PROP_ENUMERABLE" else "";
                        try self.printLine("    _ = JSValue.definePropertyGetSet(ctx, _obj, _atom, JSValue.UNDEFINED, _method, JSValue.JS_PROP_CONFIGURABLE | JSValue.JS_PROP_HAS_SET{s});", .{enum_flag});
                    } else {
                        try self.writeLine("    _ = JSValue.definePropertyValueAtom(ctx, _obj, _atom, _method, JSValue.JS_PROP_C_W_E);");
                    }
                } else {
                    // Fallback to raw atom for builtin atoms (< JS_ATOM_END) which have stable IDs
                    if (method_type == 1) {
                        const enum_flag = if (is_enumerable) " | JSValue.JS_PROP_ENUMERABLE" else "";
                        try self.printLine("    _ = JSValue.definePropertyGetSet(ctx, _obj, {d}, _method, JSValue.UNDEFINED, JSValue.JS_PROP_CONFIGURABLE | JSValue.JS_PROP_HAS_GET{s});", .{ atom, enum_flag });
                    } else if (method_type == 2) {
                        const enum_flag = if (is_enumerable) " | JSValue.JS_PROP_ENUMERABLE" else "";
                        try self.printLine("    _ = JSValue.definePropertyGetSet(ctx, _obj, {d}, JSValue.UNDEFINED, _method, JSValue.JS_PROP_CONFIGURABLE | JSValue.JS_PROP_HAS_SET{s});", .{ atom, enum_flag });
                    } else {
                        try self.printLine("    _ = JSValue.definePropertyValueAtom(ctx, _obj, {d}, _method, JSValue.JS_PROP_C_W_E);", .{atom});
                    }
                }
                try self.writeLine("    sp -= 1;");
                try self.writeLine("}");
            },
            .define_method_computed => {
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
                try self.writeLine("{");
                try self.writeLine("  const _func = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("  const _name = stack[sp-2].toJSValueWithCtx(ctx);");
                try self.writeLine("  _ = JSValue.definePropertyValueStr(ctx, _func, \"name\", JSValue.dup(ctx, _name), JSValue.JS_PROP_CONFIGURABLE);");
                try self.writeLine("}");
            },
            .define_class => {
                const atom = instr.operand.atom_u8.atom;
                try self.writeLine("{");
                try self.writeLine("  const _fields = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("  const _parent = stack[sp-2].toJSValueWithCtx(ctx);");
                try self.writeLine("  var _proto: zig_runtime.JSValue = undefined;");
                try self.writeLine("  if (_parent.isUndefined()) {");
                try self.writeLine("    _proto = zig_runtime.quickjs.JS_NewObject(ctx);");
                try self.writeLine("  } else {");
                try self.writeLine("    const _parent_proto = JSValue.getPropertyStr(ctx, _parent, \"prototype\");");
                try self.writeLine("    _proto = zig_runtime.quickjs.JS_NewObjectProto(ctx, _parent_proto);");
                try self.writeLine("    JSValue.free(ctx, _parent_proto);");
                try self.writeLine("  }");
                try self.writeLine("  const _ctor = if (_fields.isFunction()) _fields else zig_runtime.quickjs.JS_NewObject(ctx);");
                try self.writeLine("  _ = JSValue.definePropertyValueStr(ctx, _ctor, \"prototype\", JSValue.dup(ctx, _proto), 0);");
                // Resolve atom to string at compile time — raw atom IDs are module-local
                if (self.getAtomString(atom)) |class_name| {
                    const escaped = self.escapeString(class_name);
                    defer self.allocator.free(escaped);
                    try self.printLine("  _ = JSValue.definePropertyValueStr(ctx, _ctor, \"name\", zig_runtime.quickjs.JS_NewString(ctx, \"{s}\"), JSValue.JS_PROP_CONFIGURABLE);", .{escaped});
                } else {
                    try self.printLine("  _ = JSValue.definePropertyValueStr(ctx, _ctor, \"name\", JSValue.newAtomString(ctx, {d}), JSValue.JS_PROP_CONFIGURABLE);", .{atom});
                }
                try self.writeLine("  _ = JSValue.definePropertyValueStr(ctx, _proto, \"constructor\", JSValue.dup(ctx, _ctor), JSValue.JS_PROP_C_W_E);");
                try self.writeLine("  stack[sp-2] = CV.fromJSValue(_ctor);");
                try self.writeLine("  stack[sp-1] = CV.fromJSValue(_proto);");
                try self.writeLine("}");
            },
            .set_home_object => {
                try self.writeLine("{");
                try self.writeLine("  const _func = stack[sp-1].toJSValueWithCtx(ctx);");
                try self.writeLine("  const _home = stack[sp-2].toJSValueWithCtx(ctx);");
                try self.writeLine("  zig_runtime.quickjs.JS_SetHomeObject(ctx, _func, _home);");
                try self.writeLine("}");
            },

            // ========== No-op / Misc ==========
            .nop, .set_name => {},

            // ========== Control Flow — handled by block terminator ==========
            .if_false, .if_false8, .if_true, .if_true8, .goto, .goto8, .goto16 => {},

            // ========== Unsupported — skip function ==========
            else => {
                // Return error so caller skips this function entirely
                // (falls back to interpreter at runtime)
                return error.UnsupportedOpcode;
            },
        }
    }

    // ================================================================
    // Call Emission
    // ================================================================

    fn emitCall(self: *Self, argc: u16) !void {
        try self.writeIndent();
        try self.print("zig_runtime.thin.op_call(ctx, &stack, &sp, {d}, ", .{argc});
        try self.emitCallClosureParams();
        try self.write(")");
        try self.emitCatchReturn();
    }

    fn emitCallMethod(self: *Self, argc: u16) !void {
        try self.writeIndent();
        try self.print("zig_runtime.thin.op_call_method(ctx, &stack, &sp, {d}, ", .{argc});
        try self.emitCallClosureParams();
        try self.write(")");
        try self.emitCatchReturn();
    }

    // ================================================================
    // put_loc / set_loc with closure sync
    // ================================================================

    fn emitPutLoc(self: *Self, loc: u32) !void {
        try self.printLine("zig_runtime.thin.put_loc(ctx, &stack, &sp, &locals, {d});", .{loc});
        if (self.has_fclosure and self.func.var_count > 0 and loc < self.func.var_count) {
            try self.printLine("zig_runtime.thin.sync_local_jsv(&locals, &_locals_jsv, {d});", .{loc});
        }
    }

    fn emitSetLoc(self: *Self, loc: u32) !void {
        try self.printLine("zig_runtime.thin.set_loc(ctx, &stack, &sp, &locals, {d});", .{loc});
        if (self.has_fclosure and self.func.var_count > 0 and loc < self.func.var_count) {
            try self.printLine("zig_runtime.thin.sync_local_jsv(&locals, &_locals_jsv, {d});", .{loc});
        }
    }

    // ================================================================
    // Block Terminator
    // ================================================================

    fn emitBlockTerminator(self: *Self, block: BasicBlock) !void {
        const successors = block.successors.items;
        const last_op = if (block.instructions.len > 0)
            block.instructions[block.instructions.len - 1].opcode
        else
            .nop;

        // Return instructions already handled
        if (last_op == .@"return" or last_op == .return_undef) return;

        // Conditional branches
        if (last_op == .if_false or last_op == .if_false8) {
            if (successors.len >= 2) {
                const false_target = successors[0];
                const true_target = successors[1];
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const _cond = stack[sp - 1]; sp -= 1;");
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
                const true_target = successors[0];
                const false_target = successors[1];
                try self.writeLine("{");
                self.pushIndent();
                try self.writeLine("const _cond = stack[sp - 1]; sp -= 1;");
                try self.printLine("if (_cond.toBoolWithCtx(ctx)) {{ next_block = {d}; }} else {{ next_block = {d}; }}", .{ true_target, false_target });
                try self.writeLine("continue :machine;");
                self.popIndent();
                try self.writeLine("}");
            } else if (successors.len == 1) {
                try self.printLine("next_block = {d}; continue :machine;", .{successors[0]});
            }
            return;
        }

        // Unconditional jump
        if (last_op == .goto or last_op == .goto8 or last_op == .goto16) {
            if (successors.len >= 1) {
                try self.printLine("next_block = {d}; continue :machine;", .{successors[0]});
            } else {
                try self.writeLine("return zig_runtime.JSValue.UNDEFINED;");
            }
            return;
        }

        // Fall-through
        if (successors.len == 1) {
            try self.printLine("next_block = {d}; continue :machine;", .{successors[0]});
        } else if (successors.len == 0) {
            // Exit block — use return helpers for cleanup
            try self.writeLine("if (sp > 0) {");
            self.pushIndent();
            try self.writeIndent();
            try self.write("return zig_runtime.thin.returnFromStack(ctx, &stack, sp, &locals, ");
            try self.emitCleanupParams();
            try self.write(");\n");
            self.popIndent();
            try self.writeLine("}");
            try self.writeIndent();
            try self.write("return zig_runtime.thin.returnUndef(ctx, &locals, ");
            try self.emitCleanupParams();
            try self.write(");\n");
        } else {
            try self.printLine("next_block = {d}; continue :machine;", .{successors[0]});
        }
    }
};

/// Public API: generate thin-tier Zig code for a function
pub fn generateThin(allocator: Allocator, func: FunctionInfo) ![]u8 {
    var codegen = ThinCodeGen.init(allocator, func);
    defer codegen.deinit();
    return try codegen.generate();
}
