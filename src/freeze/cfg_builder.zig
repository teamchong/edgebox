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
    /// Block contains never_freeze opcodes (direct contamination)
    has_unfreezable_opcode: bool,
    /// Block is contaminated (unreachable without going through unfreezable block)
    is_contaminated: bool,
    /// Reason for contamination (opcode name that caused it)
    contamination_reason: ?[]const u8,
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
            .has_unfreezable_opcode = false,
            .is_contaminated = false,
            .contamination_reason = null,
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

/// Analyze block contamination for partial freezing
/// A block is contaminated if:
/// 1. It contains a never_freeze opcode (direct contamination), OR
/// 2. ALL paths from entry to this block go through a contaminated block
///
/// This enables "early bailout" code generation where:
/// - Clean blocks execute frozen C code
/// - At branch to contaminated block, bail to interpreter
pub fn analyzeContamination(cfg: *CFG) void {
    // Step 1: Mark blocks with never_freeze opcodes (direct contamination)
    for (cfg.blocks.items) |*block| {
        for (block.instructions) |instr| {
            const info = instr.getInfo();
            if (info.category == .never_freeze) {
                block.has_unfreezable_opcode = true;
                block.is_contaminated = true;
                block.contamination_reason = info.name;
                break;
            }
        }
    }

    // Step 2: Find blocks reachable from entry WITHOUT going through contaminated blocks
    // Any block NOT in this set is contaminated (can only be reached through contaminated blocks)
    var reachable_clean = std.AutoHashMapUnmanaged(u32, void){};
    defer reachable_clean.deinit(cfg.allocator);

    // BFS from entry, stopping at contaminated blocks
    var worklist = std.ArrayListUnmanaged(u32){};
    defer worklist.deinit(cfg.allocator);

    // Handle empty CFG
    if (cfg.blocks.items.len == 0) return;

    // Entry block is reachable clean if it's not contaminated
    if (!cfg.blocks.items[0].is_contaminated) {
        reachable_clean.put(cfg.allocator, 0, {}) catch return;
        worklist.append(cfg.allocator, 0) catch return;
    }

    while (worklist.items.len > 0) {
        const block_id = worklist.pop().?;
        const block = &cfg.blocks.items[block_id];

        for (block.successors.items) |succ_id| {
            if (succ_id >= cfg.blocks.items.len) continue;

            // Skip if already visited
            if (reachable_clean.contains(succ_id)) continue;

            const succ = &cfg.blocks.items[succ_id];
            // If successor is not directly contaminated, it's reachable clean
            if (!succ.has_unfreezable_opcode) {
                reachable_clean.put(cfg.allocator, succ_id, {}) catch continue;
                worklist.append(cfg.allocator, succ_id) catch continue;
            }
        }
    }

    // Step 3: Mark blocks not reachable clean as contaminated
    for (cfg.blocks.items, 0..) |*block, idx| {
        if (!reachable_clean.contains(@intCast(idx))) {
            block.is_contaminated = true;
            if (block.contamination_reason == null) {
                block.contamination_reason = "unreachable without contaminated block";
            }
        }
    }
}

/// Check if function can be partially frozen (has at least one clean block)
pub fn hasCleanBlocks(cfg: *const CFG) bool {
    for (cfg.blocks.items) |block| {
        if (!block.is_contaminated) return true;
    }
    return false;
}

/// Count clean vs contaminated blocks for stats
pub fn countBlocks(cfg: *const CFG) struct { clean: usize, contaminated: usize } {
    var clean: usize = 0;
    var contaminated: usize = 0;
    for (cfg.blocks.items) |block| {
        if (block.is_contaminated) {
            contaminated += 1;
        } else {
            clean += 1;
        }
    }
    return .{ .clean = clean, .contaminated = contaminated };
}

/// Counted Loop - detected for optimization
/// Pattern: for (let i = init; i < bound; i += step) { body }
pub const CountedLoop = struct {
    /// Block containing the loop condition (header)
    header_block: u32,
    /// Block containing the loop body
    body_block: u32,
    /// Block after the loop (exit target)
    exit_block: u32,

    /// Induction variable (loop counter)
    counter_local: u32,
    /// Initial value of counter (usually 0)
    init_value: i32,
    /// Step value (+1 for i++, -1 for i--)
    step: i32,

    /// Bound type for loop termination
    bound_type: BoundType,
    /// Bound value (interpretation depends on bound_type)
    bound_value: u32,

    /// Detected body pattern for specialized codegen
    body_pattern: BodyPattern,
    /// Array local (for array patterns)
    array_local: ?u32,
    /// Accumulator local (for sum/product patterns)
    accumulator_local: ?u32,

    pub const BoundType = enum {
        constant, // i < 100
        local, // i < n (n is a local variable)
        array_length, // i < arr.length
        arg_length, // i < argv[n].length
    };

    pub const BodyPattern = enum {
        generic, // Unknown pattern, use normal codegen
        array_sum, // acc += arr[i]
        array_product, // acc *= arr[i]
    };

    pub fn format(self: *const CountedLoop, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("CountedLoop(header={d}, body={d}, exit={d}, counter=local[{d}], init={d}, step={d})", .{
            self.header_block,
            self.body_block,
            self.exit_block,
            self.counter_local,
            self.init_value,
            self.step,
        });
        switch (self.body_pattern) {
            .array_sum => try writer.print(", pattern=array_sum(arr=local[{?d}], acc=local[{?d}])", .{ self.array_local, self.accumulator_local }),
            .array_product => try writer.print(", pattern=array_product(arr=local[{?d}], acc=local[{?d}])", .{ self.array_local, self.accumulator_local }),
            .generic => {},
        }
    }
};

/// Detect counted loops in CFG for optimization
/// Returns a list of detected loops with their patterns
pub fn detectCountedLoops(cfg: *const CFG, allocator: Allocator) ![]CountedLoop {
    var loops = std.ArrayListUnmanaged(CountedLoop){};
    errdefer loops.deinit(allocator);

    // Find back-edges: edges from block B to block H where H dominates B (simplified: H < B)
    for (cfg.blocks.items) |*block| {
        for (block.successors.items) |succ_id| {
            // Back-edge: successor has lower block ID (target of goto)
            if (succ_id < block.id) {
                // succ_id is the loop header
                if (try detectLoopPattern(cfg, succ_id, block.id, allocator)) |loop| {
                    try loops.append(allocator, loop);
                }
            }
        }
    }

    return loops.toOwnedSlice(allocator);
}

/// Try to detect a counted loop pattern starting at header
fn detectLoopPattern(cfg: *const CFG, header_id: u32, latch_id: u32, allocator: Allocator) !?CountedLoop {
    _ = allocator;

    const header = cfg.getBlock(header_id) orelse return null;
    const latch = cfg.getBlock(latch_id) orelse return null;

    // Header should have exactly 2 successors (body and exit)
    if (header.successors.items.len != 2) return null;

    // Find exit block (successor that's not the body/latch path)
    var body_block: ?u32 = null;
    var exit_block: ?u32 = null;
    for (header.successors.items) |succ| {
        if (succ == latch_id or (succ > header_id and succ <= latch_id)) {
            body_block = succ;
        } else {
            exit_block = succ;
        }
    }

    const body_id = body_block orelse return null;
    const exit_id = exit_block orelse return null;

    // Match header pattern: get_loc[I], get_length/const, lt/lte, if_false
    const header_match = matchHeaderPattern(header) orelse return null;

    // Match latch pattern: inc_loc[I] at end
    const step = matchLatchPattern(latch, header_match.counter_local) orelse return null;

    // Try to match body pattern (array sum, etc.)
    const body = cfg.getBlock(body_id) orelse return null;
    const body_pattern = matchBodyPattern(body, header_match.counter_local, header_match.array_local);

    return CountedLoop{
        .header_block = header_id,
        .body_block = body_id,
        .exit_block = exit_id,
        .counter_local = header_match.counter_local,
        .init_value = 0, // Assume 0 for now (would need to check init block)
        .step = step,
        .bound_type = header_match.bound_type,
        .bound_value = header_match.bound_value,
        .body_pattern = body_pattern.pattern,
        .array_local = body_pattern.array_local,
        .accumulator_local = body_pattern.accumulator_local,
    };
}

const HeaderMatch = struct {
    counter_local: u32,
    bound_type: CountedLoop.BoundType,
    bound_value: u32,
    array_local: ?u32,
};

/// Match header pattern: get_loc[I], (get_length or const), (lt or lte), if_false
fn matchHeaderPattern(header: *const BasicBlock) ?HeaderMatch {
    if (header.instructions.len < 3) return null;

    var counter_local: ?u32 = null;
    var bound_type: ?CountedLoop.BoundType = null;
    var bound_value: u32 = 0;
    var has_comparison = false;
    var has_branch = false;

    for (header.instructions, 0..) |instr, i| {
        _ = i;
        switch (instr.opcode) {
            // Get counter variable
            .get_loc, .get_loc8 => {
                if (counter_local == null) {
                    counter_local = instr.operand.loc;
                }
            },
            .get_loc0 => if (counter_local == null) {
                counter_local = 0;
            },
            .get_loc1 => if (counter_local == null) {
                counter_local = 1;
            },
            .get_loc2 => if (counter_local == null) {
                counter_local = 2;
            },
            .get_loc3 => if (counter_local == null) {
                counter_local = 3;
            },

            // Get array length (argv[0].length pattern)
            .get_length => {
                bound_type = .arg_length;
                bound_value = 0; // argv[0]
            },

            // Constant bound
            .push_i32 => {
                if (bound_type == null) {
                    bound_type = .constant;
                    bound_value = @bitCast(instr.operand.i32);
                }
            },
            .push_0, .push_1, .push_2, .push_3, .push_4, .push_5, .push_6, .push_7 => {
                if (bound_type == null) {
                    bound_type = .constant;
                    bound_value = switch (instr.opcode) {
                        .push_0 => 0,
                        .push_1 => 1,
                        .push_2 => 2,
                        .push_3 => 3,
                        .push_4 => 4,
                        .push_5 => 5,
                        .push_6 => 6,
                        .push_7 => 7,
                        else => 0,
                    };
                }
            },

            // Comparison
            .lt, .lte => {
                has_comparison = true;
            },

            // Branch
            .if_false, .if_false8 => {
                has_branch = true;
            },

            else => {},
        }
    }

    if (counter_local != null and bound_type != null and has_comparison and has_branch) {
        return HeaderMatch{
            .counter_local = counter_local.?,
            .bound_type = bound_type.?,
            .bound_value = bound_value,
            .array_local = null,
        };
    }

    return null;
}

/// Match latch pattern: look for inc_loc[I] or dec_loc[I] at end
fn matchLatchPattern(latch: *const BasicBlock, counter_local: u32) ?i32 {
    // Look for inc_loc/dec_loc that matches counter
    for (latch.instructions) |instr| {
        switch (instr.opcode) {
            .inc_loc => {
                if (instr.operand.u8 == counter_local) return 1;
            },
            .dec_loc => {
                if (instr.operand.u8 == counter_local) return -1;
            },
            else => {},
        }
    }
    return null;
}

const BodyMatch = struct {
    pattern: CountedLoop.BodyPattern,
    array_local: ?u32,
    accumulator_local: ?u32,
};

/// Match body pattern: look for acc += arr[i] or similar
fn matchBodyPattern(body: *const BasicBlock, counter_local: u32, array_local_hint: ?u32) BodyMatch {
    _ = array_local_hint;

    var has_array_get = false;
    var uses_counter = false;
    var has_add = false;
    var accumulator: ?u32 = null;

    // Simple pattern matching: look for get_array_el + add + put_loc
    for (body.instructions) |instr| {
        switch (instr.opcode) {
            .get_array_el, .get_array_el2 => {
                has_array_get = true;
            },
            .get_loc, .get_loc8 => {
                const loc = if (instr.opcode == .get_loc8)
                    instr.operand.u8
                else
                    instr.operand.loc;
                if (loc == counter_local) uses_counter = true;
            },
            .get_loc0 => if (counter_local == 0) {
                uses_counter = true;
            },
            .get_loc1 => if (counter_local == 1) {
                uses_counter = true;
            },
            .get_loc2 => if (counter_local == 2) {
                uses_counter = true;
            },
            .get_loc3 => if (counter_local == 3) {
                uses_counter = true;
            },
            .add => {
                has_add = true;
            },
            .put_loc, .put_loc8 => {
                const loc = if (instr.opcode == .put_loc8)
                    instr.operand.u8
                else
                    instr.operand.loc;
                if (loc != counter_local) {
                    accumulator = loc;
                }
            },
            .put_loc0 => if (counter_local != 0) {
                accumulator = 0;
            },
            .put_loc1 => if (counter_local != 1) {
                accumulator = 1;
            },
            .put_loc2 => if (counter_local != 2) {
                accumulator = 2;
            },
            .put_loc3 => if (counter_local != 3) {
                accumulator = 3;
            },
            else => {},
        }
    }

    // Check if it looks like array sum pattern
    if (has_array_get and uses_counter and has_add and accumulator != null) {
        return BodyMatch{
            .pattern = .array_sum,
            .array_local = null, // Would need more analysis to determine
            .accumulator_local = accumulator,
        };
    }

    return BodyMatch{
        .pattern = .generic,
        .array_local = null,
        .accumulator_local = null,
    };
}

/// Info about closure variable usage in a function
pub const ClosureVarUsage = struct {
    /// Which var_ref indices are read (get_var_ref*)
    read_indices: []const u16,
    /// Which var_ref indices are written (put_var_ref*, set_var_ref*)
    write_indices: []const u16,
    /// All unique indices (union of read and write)
    all_indices: []const u16,
    /// Highest var_ref index used + 1 (for sizing arrays)
    max_index: u16,

    pub fn deinit(self: *ClosureVarUsage, allocator: Allocator) void {
        allocator.free(self.read_indices);
        allocator.free(self.write_indices);
        allocator.free(self.all_indices);
    }
};

/// Analyze which closure variables are used in a function
/// Returns indices of var_ref used, separated by read/write
pub fn analyzeClosureVars(allocator: Allocator, instructions: []const Instruction) !ClosureVarUsage {
    var read_set = std.AutoHashMapUnmanaged(u16, void){};
    defer read_set.deinit(allocator);
    var write_set = std.AutoHashMapUnmanaged(u16, void){};
    defer write_set.deinit(allocator);

    for (instructions) |instr| {
        const idx: ?u16 = switch (instr.opcode) {
            // Read operations (get_var_ref*)
            .get_var_ref, .get_var_ref_check => instr.operand.var_ref,
            .get_var_ref0 => 0,
            .get_var_ref1 => 1,
            .get_var_ref2 => 2,
            .get_var_ref3 => 3,
            // Write operations (put_var_ref*, set_var_ref*)
            .put_var_ref, .put_var_ref_check, .put_var_ref_check_init,
            .set_var_ref => instr.operand.var_ref,
            .put_var_ref0, .set_var_ref0 => 0,
            .put_var_ref1, .set_var_ref1 => 1,
            .put_var_ref2, .set_var_ref2 => 2,
            .put_var_ref3, .set_var_ref3 => 3,
            else => null,
        };

        if (idx) |i| {
            // Determine if read or write
            const is_write = switch (instr.opcode) {
                .put_var_ref, .put_var_ref_check, .put_var_ref_check_init,
                .set_var_ref, .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3,
                .set_var_ref0, .set_var_ref1, .set_var_ref2, .set_var_ref3 => true,
                else => false,
            };

            if (is_write) {
                try write_set.put(allocator, i, {});
            } else {
                try read_set.put(allocator, i, {});
            }
        }
    }

    // Convert sets to sorted slices
    var read_list = std.ArrayListUnmanaged(u16){};
    defer read_list.deinit(allocator);
    var write_list = std.ArrayListUnmanaged(u16){};
    defer write_list.deinit(allocator);
    var all_set = std.AutoHashMapUnmanaged(u16, void){};
    defer all_set.deinit(allocator);

    var read_iter = read_set.keyIterator();
    while (read_iter.next()) |key| {
        try read_list.append(allocator, key.*);
        try all_set.put(allocator, key.*, {});
    }

    var write_iter = write_set.keyIterator();
    while (write_iter.next()) |key| {
        try write_list.append(allocator, key.*);
        try all_set.put(allocator, key.*, {});
    }

    var all_list = std.ArrayListUnmanaged(u16){};
    defer all_list.deinit(allocator);
    var all_iter = all_set.keyIterator();
    while (all_iter.next()) |key| {
        try all_list.append(allocator, key.*);
    }

    // Sort for deterministic output
    std.mem.sort(u16, read_list.items, {}, std.sort.asc(u16));
    std.mem.sort(u16, write_list.items, {}, std.sort.asc(u16));
    std.mem.sort(u16, all_list.items, {}, std.sort.asc(u16));

    // Find max index
    var max_idx: u16 = 0;
    for (all_list.items) |i| {
        if (i >= max_idx) max_idx = i + 1;
    }

    return .{
        .read_indices = try allocator.dupe(u16, read_list.items),
        .write_indices = try allocator.dupe(u16, write_list.items),
        .all_indices = try allocator.dupe(u16, all_list.items),
        .max_index = max_idx,
    };
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

test "contamination analysis - all clean" {
    // Simple function with no never_freeze opcodes
    const instrs = [_]Instruction{
        .{ .pc = 0, .opcode = .push_1, .operand = .{ .implicit_int = 1 }, .size = 1 },
        .{ .pc = 1, .opcode = .@"return", .operand = .{ .none = {} }, .size = 1 },
    };

    var cfg = try buildCFG(std.testing.allocator, &instrs);
    defer cfg.deinit();

    analyzeContamination(&cfg);

    // All blocks should be clean
    const counts = countBlocks(&cfg);
    try std.testing.expectEqual(@as(usize, 1), counts.clean);
    try std.testing.expectEqual(@as(usize, 0), counts.contaminated);
    try std.testing.expect(hasCleanBlocks(&cfg));
}

test "contamination analysis - with eval" {
    // Function with eval (never_freeze)
    const instrs = [_]Instruction{
        .{ .pc = 0, .opcode = .push_1, .operand = .{ .implicit_int = 1 }, .size = 1 },
        .{ .pc = 1, .opcode = .eval, .operand = .{ .u16 = 0 }, .size = 5 }, // never_freeze
        .{ .pc = 6, .opcode = .@"return", .operand = .{ .none = {} }, .size = 1 },
    };

    var cfg = try buildCFG(std.testing.allocator, &instrs);
    defer cfg.deinit();

    analyzeContamination(&cfg);

    // Single block with eval should be contaminated
    try std.testing.expect(cfg.blocks.items[0].is_contaminated);
    try std.testing.expect(cfg.blocks.items[0].has_unfreezable_opcode);
}
