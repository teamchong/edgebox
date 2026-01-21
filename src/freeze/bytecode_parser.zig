//! QuickJS Bytecode Parser
//! Parses raw bytecode into decoded instructions

const std = @import("std");
const opcodes = @import("opcodes.zig");

const Opcode = opcodes.Opcode;
const Format = opcodes.Format;
const OpcodeInfo = opcodes.OpcodeInfo;

/// Decoded operand value
pub const Operand = union(enum) {
    none: void,
    i8: i8,
    u8: u8,
    i16: i16,
    u16: u16,
    i32: i32,
    u32: u32,
    /// Constant pool index
    const_idx: u32,
    /// Atom (string interned ID)
    atom: u32,
    /// Jump offset (relative to current PC)
    label: i32,
    /// Local variable index
    loc: u16,
    /// Argument index
    arg: u16,
    /// Closure variable reference index
    var_ref: u16,
    /// Implicit integer (push_0..push_7)
    implicit_int: i32,
    /// Implicit local index (get_loc0..get_loc3)
    implicit_loc: u16,
    /// Implicit arg index (get_arg0..get_arg3)
    implicit_arg: u16,
    /// Implicit call argc (call0..call3)
    implicit_argc: u16,
    /// Combined atom + u8
    atom_u8: struct { atom: u32, value: u8 },
    /// Combined atom + u16
    atom_u16: struct { atom: u32, value: u16 },
    /// Combined atom + label + u8
    atom_label_u8: struct { atom: u32, label: i32, value: u8 },
    /// Combined atom + label + u16
    atom_label_u16: struct { atom: u32, label: i32, value: u16 },
    /// Combined label + u16
    label_u16: struct { label: i32, value: u16 },
    /// Two u32 values
    u32x2: struct { first: u32, second: u32 },
};

/// A decoded instruction
pub const Instruction = struct {
    /// Program counter (byte offset in bytecode)
    pc: u32,
    /// Opcode enum
    opcode: Opcode,
    /// Decoded operand
    operand: Operand,
    /// Original instruction size in bytes
    size: u8,

    /// Get opcode info
    pub fn getInfo(self: Instruction) OpcodeInfo {
        return opcodes.getInfo(self.opcode);
    }

    /// Check if this is a jump instruction
    pub fn isJump(self: Instruction) bool {
        return opcodes.isJump(self.opcode);
    }

    /// Check if this terminates a basic block
    pub fn isTerminator(self: Instruction) bool {
        return opcodes.isTerminator(self.opcode);
    }

    /// Get jump target PC (for jump instructions)
    /// QuickJS: When executing a jump, pc points to the operand (pc = instruction_pc + 1).
    /// Then pc += offset, so: target = instruction_pc + 1 + offset.
    /// This works for all jump variants (goto8, goto16, goto) since all opcodes are 1 byte.
    pub fn getJumpTarget(self: Instruction) ?u32 {
        const offset: i32 = switch (self.operand) {
            .label => |l| l,
            .atom_label_u8 => |a| a.label,
            .atom_label_u16 => |a| a.label,
            .label_u16 => |l| l.label,
            else => return null,
        };
        // Formula: target = pc + 1 + offset (pc is instruction start, +1 skips opcode to operand)
        const target = @as(i64, self.pc) + 1 + @as(i64, offset);
        if (target < 0) return null;
        // Security: Verify target fits in u32 (don't silently truncate large values)
        if (target > 0xFFFFFFFF) return null;
        return @intCast(target);
    }

    /// Format instruction for debugging
    pub fn format(self: Instruction, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const info = self.getInfo();
        try writer.print("{d:>4}: {s}", .{ self.pc, info.name });

        switch (self.operand) {
            .none => {},
            .i8 => |v| try writer.print(" {d}", .{v}),
            .u8 => |v| try writer.print(" {d}", .{v}),
            .i16 => |v| try writer.print(" {d}", .{v}),
            .u16 => |v| try writer.print(" {d}", .{v}),
            .i32 => |v| try writer.print(" {d}", .{v}),
            .u32 => |v| try writer.print(" {d}", .{v}),
            .const_idx => |v| try writer.print(" const[{d}]", .{v}),
            .atom => |v| try writer.print(" atom:{d}", .{v}),
            .label => |v| try writer.print(" -> {d}", .{@as(i64, self.pc) + 1 + @as(i64, v)}),
            .loc => |v| try writer.print(" loc[{d}]", .{v}),
            .arg => |v| try writer.print(" arg[{d}]", .{v}),
            .var_ref => |v| try writer.print(" var_ref[{d}]", .{v}),
            .implicit_int => |v| try writer.print(" ({d})", .{v}),
            .implicit_loc => |v| try writer.print(" (loc[{d}])", .{v}),
            .implicit_arg => |v| try writer.print(" (arg[{d}])", .{v}),
            .implicit_argc => |v| try writer.print(" (argc={d})", .{v}),
            .atom_u8 => |v| try writer.print(" atom:{d} {d}", .{ v.atom, v.value }),
            .atom_u16 => |v| try writer.print(" atom:{d} {d}", .{ v.atom, v.value }),
            .atom_label_u8 => |v| try writer.print(" atom:{d} -> {d} {d}", .{ v.atom, @as(i64, self.pc) + 1 + @as(i64, v.label), v.value }),
            .atom_label_u16 => |v| try writer.print(" atom:{d} -> {d} {d}", .{ v.atom, @as(i64, self.pc) + 1 + @as(i64, v.label), v.value }),
            .label_u16 => |v| try writer.print(" -> {d} {d}", .{ @as(i64, self.pc) + 1 + @as(i64, v.label), v.value }),
            .u32x2 => |v| try writer.print(" {d} {d}", .{ v.first, v.second }),
        }
    }
};

/// Bytecode parser
pub const BytecodeParser = struct {
    bytecode: []const u8,
    pc: u32,

    pub fn init(bytecode: []const u8) BytecodeParser {
        return .{
            .bytecode = bytecode,
            .pc = 0,
        };
    }

    /// Check if more instructions available
    pub fn hasMore(self: *const BytecodeParser) bool {
        return self.pc < self.bytecode.len;
    }

    /// Read next instruction
    pub fn next(self: *BytecodeParser) !Instruction {
        if (self.pc >= self.bytecode.len) {
            return error.EndOfBytecode;
        }

        const start_pc = self.pc;
        const op_byte = self.bytecode[self.pc];
        self.pc += 1;

        const opcode: Opcode = @enumFromInt(op_byte);
        const info = opcodes.getInfoByByte(op_byte);
        const operand = try self.readOperand(info.format, opcode);

        return .{
            .pc = start_pc,
            .opcode = opcode,
            .operand = operand,
            .size = info.size,
        };
    }

    /// Read operand based on format
    fn readOperand(self: *BytecodeParser, format: Format, opcode: Opcode) !Operand {
        return switch (format) {
            .none => .{ .none = {} },
            .none_int => .{ .implicit_int = opcodes.getImplicitInt(opcode) orelse 0 },
            .none_loc => .{ .implicit_loc = opcodes.getImplicitLocal(opcode) orelse 0 },
            .none_arg => .{ .implicit_arg = opcodes.getImplicitArg(opcode) orelse 0 },
            .none_var_ref => blk: {
                // var_ref0-3 have implicit index
                const idx: u16 = switch (opcode) {
                    .get_var_ref0, .put_var_ref0, .set_var_ref0 => 0,
                    .get_var_ref1, .put_var_ref1, .set_var_ref1 => 1,
                    .get_var_ref2, .put_var_ref2, .set_var_ref2 => 2,
                    .get_var_ref3, .put_var_ref3, .set_var_ref3 => 3,
                    else => 0,
                };
                break :blk .{ .var_ref = idx };
            },
            .u8 => .{ .u8 = try self.readU8() },
            .i8 => .{ .i8 = @bitCast(try self.readU8()) },
            .loc8 => .{ .loc = try self.readU8() },
            .const8 => .{ .const_idx = try self.readU8() },
            .label8 => .{ .label = @as(i32, @as(i8, @bitCast(try self.readU8()))) },
            .u16 => .{ .u16 = try self.readU16() },
            .i16 => .{ .i16 = @bitCast(try self.readU16()) },
            .label16 => .{ .label = @as(i32, @as(i16, @bitCast(try self.readU16()))) },
            .npop, .npop_u16 => .{ .u16 = try self.readU16() },
            .npopx => .{ .implicit_argc = opcodes.getImplicitCallArgc(opcode) orelse 0 },
            .loc => .{ .loc = try self.readU16() },
            .arg => .{ .arg = try self.readU16() },
            .var_ref => .{ .var_ref = try self.readU16() },
            .u32 => .{ .u32 = try self.readU32() },
            .i32 => .{ .i32 = @bitCast(try self.readU32()) },
            .@"const" => .{ .const_idx = try self.readU32() },
            .label => .{ .label = @bitCast(try self.readU32()) },
            .atom => .{ .atom = try self.readU32() },
            .atom_u8 => .{ .atom_u8 = .{ .atom = try self.readU32(), .value = try self.readU8() } },
            .atom_u16 => .{ .atom_u16 = .{ .atom = try self.readU32(), .value = try self.readU16() } },
            .atom_label_u8 => .{ .atom_label_u8 = .{
                .atom = try self.readU32(),
                .label = @bitCast(try self.readU32()),
                .value = try self.readU8(),
            } },
            .atom_label_u16 => .{ .atom_label_u16 = .{
                .atom = try self.readU32(),
                .label = @bitCast(try self.readU32()),
                .value = try self.readU16(),
            } },
            .label_u16 => .{ .label_u16 = .{
                .label = @bitCast(try self.readU32()),
                .value = try self.readU16(),
            } },
            .u32x2 => .{ .u32x2 = .{
                .first = try self.readU32(),
                .second = try self.readU32(),
            } },
        };
    }

    fn readU8(self: *BytecodeParser) !u8 {
        if (self.pc >= self.bytecode.len) return error.UnexpectedEndOfBytecode;
        const v = self.bytecode[self.pc];
        self.pc += 1;
        return v;
    }

    fn readU16(self: *BytecodeParser) !u16 {
        if (self.pc + 2 > self.bytecode.len) return error.UnexpectedEndOfBytecode;
        // QuickJS uses little-endian
        const v = std.mem.readInt(u16, self.bytecode[self.pc..][0..2], .little);
        self.pc += 2;
        return v;
    }

    fn readU32(self: *BytecodeParser) !u32 {
        if (self.pc + 4 > self.bytecode.len) return error.UnexpectedEndOfBytecode;
        const v = std.mem.readInt(u32, self.bytecode[self.pc..][0..4], .little);
        self.pc += 4;
        return v;
    }

    /// Parse all instructions into a list
    pub fn parseAll(self: *BytecodeParser, allocator: std.mem.Allocator) ![]Instruction {
        var instructions = std.ArrayListUnmanaged(Instruction){};
        errdefer instructions.deinit(allocator);

        while (self.hasMore()) {
            const instr = try self.next();
            try instructions.append(allocator, instr);
        }

        return instructions.toOwnedSlice(allocator);
    }
};

/// Function metadata (extracted from JSFunctionBytecode)
pub const FunctionMeta = struct {
    name: []const u8,
    arg_count: u16,
    var_count: u16,
    stack_size: u16,
    is_strict: bool,
    has_prototype: bool,
};

/// Parse function from bytecode with metadata
pub const FunctionParser = struct {
    bytecode: []const u8,
    meta: FunctionMeta,
    instructions: []Instruction,

    pub fn deinit(self: *FunctionParser, allocator: std.mem.Allocator) void {
        allocator.free(self.instructions);
    }
};

/// Check if bytecode contains any unfrozen opcodes
pub fn canFreezeFunction(instructions: []const Instruction) struct { can_freeze: bool, reason: ?[]const u8 } {
    for (instructions) |instr| {
        if (!opcodes.canFreeze(instr.opcode)) {
            const info = instr.getInfo();
            return .{ .can_freeze = false, .reason = info.name };
        }
    }
    return .{ .can_freeze = true, .reason = null };
}

/// Disassemble bytecode for debugging
pub fn disassemble(bytecode: []const u8, writer: anytype) !void {
    var parser = BytecodeParser.init(bytecode);
    while (parser.hasMore()) {
        const instr = try parser.next();
        try writer.print("{}\n", .{instr});
    }
}

test "parse simple bytecode" {
    // Simulated bytecode for: push_1, push_2, add, return
    const bytecode = [_]u8{
        @intFromEnum(Opcode.push_1), // 0: push_1
        @intFromEnum(Opcode.push_2), // 1: push_2
        @intFromEnum(Opcode.add), // 2: add
        @intFromEnum(Opcode.@"return"), // 3: return
    };

    var parser = BytecodeParser.init(&bytecode);
    const instrs = try parser.parseAll(std.testing.allocator);
    defer std.testing.allocator.free(instrs);

    try std.testing.expectEqual(@as(usize, 4), instrs.len);
    try std.testing.expectEqual(Opcode.push_1, instrs[0].opcode);
    try std.testing.expectEqual(Opcode.push_2, instrs[1].opcode);
    try std.testing.expectEqual(Opcode.add, instrs[2].opcode);
    try std.testing.expectEqual(Opcode.@"return", instrs[3].opcode);
}

test "parse bytecode with operands" {
    // push_i32 42, get_loc 5, if_false8 +10
    const bytecode = [_]u8{
        @intFromEnum(Opcode.push_i32), 42, 0, 0, 0, // push_i32 42
        @intFromEnum(Opcode.get_loc), 5, 0, // get_loc 5
        @intFromEnum(Opcode.if_false8), 10, // if_false8 +10
    };

    var parser = BytecodeParser.init(&bytecode);
    const instrs = try parser.parseAll(std.testing.allocator);
    defer std.testing.allocator.free(instrs);

    try std.testing.expectEqual(@as(usize, 3), instrs.len);

    // Check push_i32
    try std.testing.expectEqual(Opcode.push_i32, instrs[0].opcode);
    try std.testing.expectEqual(@as(i32, 42), instrs[0].operand.i32);

    // Check get_loc
    try std.testing.expectEqual(Opcode.get_loc, instrs[1].opcode);
    try std.testing.expectEqual(@as(u16, 5), instrs[1].operand.loc);

    // Check if_false8
    try std.testing.expectEqual(Opcode.if_false8, instrs[2].opcode);
    try std.testing.expectEqual(@as(i32, 10), instrs[2].operand.label);
}

test "jump target calculation" {
    // if_false at PC=10, size=2, offset=+5 -> target = 10 + 2 + 5 - 1 = 16 (QuickJS quirk: -1)
    const instr = Instruction{
        .pc = 10,
        .opcode = .if_false8,
        .operand = .{ .label = 5 },
        .size = 2,
    };
    try std.testing.expectEqual(@as(u32, 16), instr.getJumpTarget().?);
}

test "can freeze check" {
    const freezable = [_]Instruction{
        .{ .pc = 0, .opcode = .push_1, .operand = .{ .implicit_int = 1 }, .size = 1 },
        .{ .pc = 1, .opcode = .add, .operand = .{ .none = {} }, .size = 1 },
        .{ .pc = 2, .opcode = .@"return", .operand = .{ .none = {} }, .size = 1 },
    };
    const result1 = canFreezeFunction(&freezable);
    try std.testing.expect(result1.can_freeze);

    const not_freezable = [_]Instruction{
        .{ .pc = 0, .opcode = .eval, .operand = .{ .u16 = 0 }, .size = 5 },
    };
    const result2 = canFreezeFunction(&not_freezable);
    try std.testing.expect(!result2.can_freeze);
    try std.testing.expectEqualStrings("eval", result2.reason.?);
}
