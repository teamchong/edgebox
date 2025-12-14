//! Control Flow Graph Builder
//! Builds CFG from parsed bytecode instructions

const std = @import("std");
const opcodes = @import("opcodes.zig");
const parser = @import("bytecode_parser.zig");

const Opcode = opcodes.Opcode;
const Instruction = parser.Instruction;
const Allocator = std.mem.Allocator;

/// A basic block in the CFG
pub const BasicBlock = struct {
    /// Unique block ID
    id: u32,
    /// Start PC (first instruction)
    start_pc: u32,
    /// End PC (last instruction, exclusive)
    end_pc: u32,
    /// Instructions in this block
    instructions: []const Instruction,
    /// Successor block IDs
    successors: std.ArrayListUnmanaged(u32),
    /// Predecessor block IDs
    predecessors: std.ArrayListUnmanaged(u32),
    /// Stack depth at block entry
    stack_depth_in: i32,
    /// Stack depth at block exit
    stack_depth_out: i32,
    /// Is this an exception handler entry?
    is_exception_handler: bool,
    /// Allocator for owned slices
    allocator: Allocator,

    pub fn init(allocator: Allocator, id: u32, start_pc: u32) BasicBlock {
        return .{
            .id = id,
            .start_pc = start_pc,
            .end_pc = start_pc,
            .instructions = &[_]Instruction{},
            .successors = .{},
            .predecessors = .{},
            .stack_depth_in = 0,
            .stack_depth_out = 0,
            .is_exception_handler = false,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *BasicBlock) void {
        self.successors.deinit(self.allocator);
        self.predecessors.deinit(self.allocator);
        if (self.instructions.len > 0) {
            self.allocator.free(self.instructions);
        }
    }

    /// Get the last instruction in this block
    pub fn lastInstruction(self: *const BasicBlock) ?Instruction {
        if (self.instructions.len == 0) return null;
        return self.instructions[self.instructions.len - 1];
    }

    /// Check if this block ends with a terminator
    pub fn isTerminated(self: *const BasicBlock) bool {
        const last = self.lastInstruction() orelse return false;
        return last.isTerminator() or opcodes.isJump(last.opcode);
    }

    /// Format for debugging
    pub fn format(self: *const BasicBlock, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Block {d} (PC {d}-{d}, {d} instrs)", .{
            self.id,
            self.start_pc,
            self.end_pc,
            self.instructions.len,
        });
        if (self.successors.items.len > 0) {
            try writer.writeAll(" -> [");
            for (self.successors.items, 0..) |succ, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("{d}", .{succ});
            }
            try writer.writeAll("]");
        }
    }
};

/// Control Flow Graph
pub const CFG = struct {
    allocator: Allocator,
    /// All basic blocks
    blocks: std.ArrayListUnmanaged(BasicBlock),
    /// Map from PC to block ID
    pc_to_block: std.AutoHashMapUnmanaged(u32, u32),
    /// Entry block ID
    entry_block: u32,
    /// Exit block IDs (blocks ending with return)
    exit_blocks: std.ArrayListUnmanaged(u32),

    pub fn init(allocator: Allocator) CFG {
        return .{
            .allocator = allocator,
            .blocks = .{},
            .pc_to_block = .{},
            .entry_block = 0,
            .exit_blocks = .{},
        };
    }

    pub fn deinit(self: *CFG) void {
        for (self.blocks.items) |*block| {
            block.deinit();
        }
        self.blocks.deinit(self.allocator);
        self.pc_to_block.deinit(self.allocator);
        self.exit_blocks.deinit(self.allocator);
    }

    /// Get block by ID
    pub fn getBlock(self: *const CFG, id: u32) ?*const BasicBlock {
        if (id >= self.blocks.items.len) return null;
        return &self.blocks.items[id];
    }

    /// Get mutable block by ID
    pub fn getBlockMut(self: *CFG, id: u32) ?*BasicBlock {
        if (id >= self.blocks.items.len) return null;
        return &self.blocks.items[id];
    }

    /// Get block containing PC
    pub fn getBlockAtPC(self: *const CFG, pc: u32) ?*const BasicBlock {
        const id = self.pc_to_block.get(pc) orelse return null;
        return self.getBlock(id);
    }

    /// Print CFG for debugging
    pub fn dump(self: *const CFG, writer: anytype) !void {
        try writer.print("=== Control Flow Graph ({d} blocks) ===\n", .{self.blocks.items.len});
        for (self.blocks.items) |*block| {
            try writer.print("\n", .{});
            try block.format("", .{}, writer);
            try writer.print("\n", .{});
            for (block.instructions) |instr| {
                try writer.print("  ", .{});
                try instr.format("", .{}, writer);
                try writer.print("\n", .{});
            }
        }
    }

    /// Print CFG using std.debug.print (no Writer needed)
    pub fn dumpDebug(self: *const CFG) void {
        std.debug.print("=== Control Flow Graph ({d} blocks) ===\n", .{self.blocks.items.len});
        for (self.blocks.items) |block| {
            std.debug.print("\nBlock {d} (PC {d}-{d}, {d} instrs)", .{
                block.id,
                block.start_pc,
                block.end_pc,
                block.instructions.len,
            });
            if (block.successors.items.len > 0) {
                std.debug.print(" -> [", .{});
                for (block.successors.items, 0..) |succ, i| {
                    if (i > 0) std.debug.print(", ", .{});
                    std.debug.print("{d}", .{succ});
                }
                std.debug.print("]", .{});
            }
            std.debug.print("\n", .{});
            for (block.instructions) |instr| {
                const info = instr.getInfo();
                std.debug.print("  {d:>4}: {s}\n", .{ instr.pc, info.name });
            }
        }
    }
};

/// Build CFG from instructions
pub fn buildCFG(allocator: Allocator, instructions: []const Instruction) !CFG {
    var cfg = CFG.init(allocator);
    errdefer cfg.deinit();

    if (instructions.len == 0) {
        return cfg;
    }

    // Security: Limit CFG size to prevent DoS from huge bytecode
    if (instructions.len > 100000) {
        return error.CfgTooLarge;
    }

    // Step 1: Identify leaders (first instructions of basic blocks)
    var leaders = std.AutoHashMapUnmanaged(u32, void){};
    defer leaders.deinit(allocator);

    // First instruction is always a leader
    try leaders.put(allocator, instructions[0].pc, {});

    // Find all jump targets and instructions after jumps
    for (instructions) |instr| {
        // Jump targets are leaders
        if (instr.getJumpTarget()) |target| {
            try leaders.put(allocator, target, {});
        }

        // Instructions after jumps/terminators are leaders
        if (instr.isJump() or instr.isTerminator()) {
            const next_pc = instr.pc + instr.size;
            // Only add if there's an instruction at this PC
            for (instructions) |other| {
                if (other.pc == next_pc) {
                    try leaders.put(allocator, next_pc, {});
                    break;
                }
            }
        }

        // Exception handlers (catch) are leaders
        if (instr.opcode == .@"catch") {
            if (instr.getJumpTarget()) |target| {
                try leaders.put(allocator, target, {});
            }
        }
    }

    // Step 2: Create basic blocks
    var sorted_leaders = std.ArrayListUnmanaged(u32){};
    defer sorted_leaders.deinit(allocator);

    var leader_iter = leaders.keyIterator();
    while (leader_iter.next()) |pc| {
        try sorted_leaders.append(allocator, pc.*);
    }
    std.mem.sort(u32, sorted_leaders.items, {}, std.sort.asc(u32));

    // Create blocks for each leader
    for (sorted_leaders.items, 0..) |leader_pc, idx| {
        var block = BasicBlock.init(allocator, @intCast(idx), leader_pc);

        // Find end PC (next leader or end of bytecode)
        const next_leader_pc = if (idx + 1 < sorted_leaders.items.len)
            sorted_leaders.items[idx + 1]
        else
            instructions[instructions.len - 1].pc + instructions[instructions.len - 1].size;

        block.end_pc = next_leader_pc;

        // Collect instructions for this block
        var block_instrs = std.ArrayListUnmanaged(Instruction){};
        for (instructions) |instr| {
            if (instr.pc >= leader_pc and instr.pc < next_leader_pc) {
                try block_instrs.append(allocator, instr);
            }
        }
        block.instructions = try block_instrs.toOwnedSlice(allocator);

        // Map PCs to block ID
        for (block.instructions) |instr| {
            try cfg.pc_to_block.put(allocator, instr.pc, block.id);
        }

        try cfg.blocks.append(allocator, block);
    }

    // Step 3: Connect blocks (add edges)
    for (cfg.blocks.items) |*block| {
        const last = block.lastInstruction() orelse continue;

        // Add jump edge
        if (last.getJumpTarget()) |target| {
            if (cfg.pc_to_block.get(target)) |target_block| {
                // Bounds check before array access to prevent OOB
                if (target_block < cfg.blocks.items.len) {
                    try block.successors.append(allocator, target_block);
                    try cfg.blocks.items[target_block].predecessors.append(allocator, block.id);
                }
            }
        }

        // Add fall-through edge (if not an unconditional jump or terminator)
        const has_fallthrough = switch (last.opcode) {
            .goto, .goto8, .goto16, .@"return", .return_undef, .return_async, .throw, .tail_call, .tail_call_method => false,
            else => true,
        };

        if (has_fallthrough) {
            const next_pc = last.pc + last.size;
            if (cfg.pc_to_block.get(next_pc)) |next_block| {
                // Avoid duplicate edges
                var already_connected = false;
                for (block.successors.items) |succ| {
                    if (succ == next_block) {
                        already_connected = true;
                        break;
                    }
                }
                if (!already_connected) {
                    // Bounds check before array access to prevent OOB
                    if (next_block < cfg.blocks.items.len) {
                        try block.successors.append(allocator, next_block);
                        try cfg.blocks.items[next_block].predecessors.append(allocator, block.id);
                    }
                }
            }
        }

        // Track exit blocks
        if (last.opcode == .@"return" or last.opcode == .return_undef or last.opcode == .return_async) {
            try cfg.exit_blocks.append(allocator, block.id);
        }
    }

    // Step 4: Compute stack depths
    try computeStackDepths(&cfg);

    return cfg;
}

/// Compute stack depth at each block entry/exit
fn computeStackDepths(cfg: *CFG) !void {
    if (cfg.blocks.items.len == 0) return;

    // Initialize entry block with depth 0
    cfg.blocks.items[0].stack_depth_in = 0;

    // Worklist algorithm
    var worklist = std.ArrayListUnmanaged(u32){};
    defer worklist.deinit(cfg.allocator);
    try worklist.append(cfg.allocator, 0);

    var visited = std.AutoHashMapUnmanaged(u32, void){};
    defer visited.deinit(cfg.allocator);

    while (worklist.items.len > 0) {
        const block_id = worklist.pop().?;
        if (visited.contains(block_id)) continue;
        try visited.put(cfg.allocator, block_id, {});

        const block = &cfg.blocks.items[block_id];
        var depth = block.stack_depth_in;

        // Simulate stack for each instruction
        for (block.instructions) |instr| {
            const info = instr.getInfo();
            depth -= info.n_pop;
            depth += info.n_push;
        }
        block.stack_depth_out = depth;

        // Propagate to successors
        for (block.successors.items) |succ_id| {
            // Bounds check to prevent OOB access
            if (succ_id >= cfg.blocks.items.len) continue;
            const succ = &cfg.blocks.items[succ_id];
            if (!visited.contains(succ_id)) {
                succ.stack_depth_in = depth;
                try worklist.append(cfg.allocator, succ_id);
            }
        }
    }
}

/// Get blocks in reverse post-order (good for forward dataflow)
pub fn reversePostOrder(cfg: *const CFG, allocator: Allocator) ![]u32 {
    var order = std.ArrayListUnmanaged(u32){};
    var visited = std.AutoHashMapUnmanaged(u32, void){};
    defer visited.deinit(allocator);

    // DFS from entry
    const StackItem = struct { id: u32, children_visited: bool };
    var stack = std.ArrayListUnmanaged(StackItem){};
    defer stack.deinit(allocator);

    try stack.append(allocator, .{ .id = cfg.entry_block, .children_visited = false });

    while (stack.items.len > 0) {
        var item = &stack.items[stack.items.len - 1];
        const block_id = item.id;

        if (visited.contains(block_id)) {
            _ = stack.pop();
            continue;
        }

        if (!item.children_visited) {
            item.children_visited = true;
            try visited.put(allocator, block_id, {});

            // Push successors
            const block = cfg.getBlock(block_id) orelse continue;
            for (block.successors.items) |succ| {
                if (!visited.contains(succ)) {
                    try stack.append(allocator, .{ .id = succ, .children_visited = false });
                }
            }
        } else {
            _ = stack.pop();
            try order.append(allocator, block_id);
        }
    }

    // Reverse for reverse post-order
    std.mem.reverse(u32, order.items);
    return order.toOwnedSlice(allocator);
}

test "build simple CFG" {
    // Linear: push_1, push_2, add, return
    const instrs = [_]Instruction{
        .{ .pc = 0, .opcode = .push_1, .operand = .{ .implicit_int = 1 }, .size = 1 },
        .{ .pc = 1, .opcode = .push_2, .operand = .{ .implicit_int = 2 }, .size = 1 },
        .{ .pc = 2, .opcode = .add, .operand = .{ .none = {} }, .size = 1 },
        .{ .pc = 3, .opcode = .@"return", .operand = .{ .none = {} }, .size = 1 },
    };

    var cfg = try buildCFG(std.testing.allocator, &instrs);
    defer cfg.deinit();

    // Should be single block (no branches)
    try std.testing.expectEqual(@as(usize, 1), cfg.blocks.items.len);
    try std.testing.expectEqual(@as(usize, 4), cfg.blocks.items[0].instructions.len);
}

test "build CFG with branch" {
    // if_false8 +3, push_1, goto8 +1, push_2, return
    const instrs = [_]Instruction{
        .{ .pc = 0, .opcode = .if_false8, .operand = .{ .label = 3 }, .size = 2 },
        .{ .pc = 2, .opcode = .push_1, .operand = .{ .implicit_int = 1 }, .size = 1 },
        .{ .pc = 3, .opcode = .goto8, .operand = .{ .label = 1 }, .size = 2 },
        .{ .pc = 5, .opcode = .push_2, .operand = .{ .implicit_int = 2 }, .size = 1 },
        .{ .pc = 6, .opcode = .@"return", .operand = .{ .none = {} }, .size = 1 },
    };

    var cfg = try buildCFG(std.testing.allocator, &instrs);
    defer cfg.deinit();

    // Should have multiple blocks due to branches
    try std.testing.expect(cfg.blocks.items.len > 1);

    // Entry block should have successors
    try std.testing.expect(cfg.blocks.items[0].successors.items.len > 0);
}

test "stack depth computation" {
    // push_1, push_2, add -> depths: 0, 1, 2, 1
    const instrs = [_]Instruction{
        .{ .pc = 0, .opcode = .push_1, .operand = .{ .implicit_int = 1 }, .size = 1 },
        .{ .pc = 1, .opcode = .push_2, .operand = .{ .implicit_int = 2 }, .size = 1 },
        .{ .pc = 2, .opcode = .add, .operand = .{ .none = {} }, .size = 1 },
        .{ .pc = 3, .opcode = .@"return", .operand = .{ .none = {} }, .size = 1 },
    };

    var cfg = try buildCFG(std.testing.allocator, &instrs);
    defer cfg.deinit();

    try std.testing.expectEqual(@as(i32, 0), cfg.blocks.items[0].stack_depth_in);
    // After push_1: 1, push_2: 2, add: 1, return: 0
    try std.testing.expectEqual(@as(i32, 0), cfg.blocks.items[0].stack_depth_out);
}
