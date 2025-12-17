//! Symbolic Stack Analysis
//!
//! Converts stack-based bytecode to SSA-style value flow at compile time.
//! Each stack position at each program point is tracked symbolically.

const std = @import("std");
const opcodes = @import("opcodes.zig");
const parser = @import("bytecode_parser.zig");

const Opcode = opcodes.Opcode;
const Instruction = parser.Instruction;
const Allocator = std.mem.Allocator;

/// Symbolic value types
pub const SymType = enum {
    int, // Known to be int32
    value, // Unknown JSValue
    self_ref, // Reference to this function (for recursion)
    bool_val, // Boolean result
};

/// Symbolic value - represents a compile-time known value
pub const SymValue = struct {
    /// Unique ID for this value
    id: u32,
    /// Type of this value
    typ: SymType,
    /// How this value was created
    origin: Origin,

    pub const Origin = union(enum) {
        /// Function argument
        arg: u16,
        /// Integer constant
        int_const: i32,
        /// Binary operation on two values
        binop: struct {
            op: BinOp,
            left: u32, // SymValue id
            right: u32, // SymValue id
        },
        /// Comparison of two values
        compare: struct {
            op: CmpOp,
            left: u32,
            right: u32,
        },
        /// Result of calling a function
        call: struct {
            func: u32, // SymValue id of the function
            args: []const u32, // SymValue ids of arguments
        },
        /// Self-reference for recursion
        self_ref: void,
        /// Local variable read
        local: u16,
        /// Undefined
        undefined: void,
    };

    pub const BinOp = enum { add, sub, mul, div, mod, band, bor, bxor, shl, shr };
    pub const CmpOp = enum { lt, lte, gt, gte, eq, neq, strict_eq, strict_neq };
};

/// Symbolic stack for compile-time analysis
pub const SymbolicStack = struct {
    allocator: Allocator,
    /// All values created during analysis
    values: std.ArrayListUnmanaged(SymValue),
    /// Current stack (holds value IDs)
    stack: std.ArrayListUnmanaged(u32),
    /// Local variables (holds value IDs)
    locals: std.ArrayListUnmanaged(u32),
    /// Next value ID
    next_id: u32,

    pub fn init(allocator: Allocator, var_count: u16) !SymbolicStack {
        var locals = std.ArrayListUnmanaged(u32){};
        // Initialize locals to undefined
        var i: u16 = 0;
        while (i < var_count) : (i += 1) {
            const undef = SymValue{
                .id = i,
                .typ = .value,
                .origin = .{ .undefined = {} },
            };
            _ = undef;
            try locals.append(allocator, 0); // Will be set properly
        }
        return .{
            .allocator = allocator,
            .values = .{},
            .stack = .{},
            .locals = locals,
            .next_id = var_count,
        };
    }

    pub fn deinit(self: *SymbolicStack) void {
        self.values.deinit(self.allocator);
        self.stack.deinit(self.allocator);
        self.locals.deinit(self.allocator);
    }

    /// Create a new symbolic value
    pub fn newValue(self: *SymbolicStack, typ: SymType, origin: SymValue.Origin) !u32 {
        const id = self.next_id;
        self.next_id += 1;
        try self.values.append(self.allocator, .{
            .id = id,
            .typ = typ,
            .origin = origin,
        });
        return id;
    }

    /// Push a value onto the stack
    pub fn push(self: *SymbolicStack, val_id: u32) !void {
        try self.stack.append(self.allocator, val_id);
    }

    /// Pop a value from the stack
    pub fn pop(self: *SymbolicStack) ?u32 {
        if (self.stack.items.len == 0) return null;
        return self.stack.pop();
    }

    /// Peek at top of stack
    pub fn peek(self: *SymbolicStack) ?u32 {
        if (self.stack.items.len == 0) return null;
        return self.stack.items[self.stack.items.len - 1];
    }

    /// Get value by ID
    pub fn getValue(self: *SymbolicStack, id: u32) ?SymValue {
        for (self.values.items) |v| {
            if (v.id == id) return v;
        }
        return null;
    }

    /// Check if a value is a self-reference
    pub fn isSelfRef(self: *SymbolicStack, id: u32) bool {
        if (self.getValue(id)) |v| {
            return v.typ == .self_ref;
        }
        return false;
    }

    /// Process an instruction symbolically
    pub fn processInstruction(self: *SymbolicStack, instr: Instruction) !void {
        switch (instr.opcode) {
            // Push argument
            .get_arg0 => try self.push(try self.newValue(.int, .{ .arg = 0 })),
            .get_arg1 => try self.push(try self.newValue(.int, .{ .arg = 1 })),
            .get_arg2 => try self.push(try self.newValue(.int, .{ .arg = 2 })),
            .get_arg3 => try self.push(try self.newValue(.int, .{ .arg = 3 })),

            // Push constant
            .push_0 => try self.push(try self.newValue(.int, .{ .int_const = 0 })),
            .push_1 => try self.push(try self.newValue(.int, .{ .int_const = 1 })),
            .push_2 => try self.push(try self.newValue(.int, .{ .int_const = 2 })),
            .push_3 => try self.push(try self.newValue(.int, .{ .int_const = 3 })),
            .push_i8 => try self.push(try self.newValue(.int, .{ .int_const = instr.operand.i8 })),

            // Load closure variable - assume it's the current function for self-recursion detection
            // This works for the common case: direct self-recursion like fib(n) { return fib(n-1) + fib(n-2) }
            // More complex cases (loading other closure variables) may cause false positives, but they'll
            // fall back to JSValue path safely (no wrong code generation)
            .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3 => {
                try self.push(try self.newValue(.self_ref, .{ .self_ref = {} }));
            },

            // Binary operations
            .add, .sub, .mul => {
                const right = self.pop() orelse return error.StackUnderflow;
                const left = self.pop() orelse return error.StackUnderflow;
                const op: SymValue.BinOp = switch (instr.opcode) {
                    .add => .add,
                    .sub => .sub,
                    .mul => .mul,
                    else => unreachable,
                };
                try self.push(try self.newValue(.int, .{ .binop = .{ .op = op, .left = left, .right = right } }));
            },

            // Comparisons
            .lt, .lte, .gt, .gte, .eq, .neq => {
                const right = self.pop() orelse return error.StackUnderflow;
                const left = self.pop() orelse return error.StackUnderflow;
                const op: SymValue.CmpOp = switch (instr.opcode) {
                    .lt => .lt,
                    .lte => .lte,
                    .gt => .gt,
                    .gte => .gte,
                    .eq => .eq,
                    .neq => .neq,
                    else => unreachable,
                };
                try self.push(try self.newValue(.bool_val, .{ .compare = .{ .op = op, .left = left, .right = right } }));
            },

            // Conditional branches - consume the condition
            .if_false, .if_false8, .if_true, .if_true8 => {
                _ = self.pop();
            },

            // Function calls
            .call0, .call1, .call2, .call3 => {
                const argc: u16 = switch (instr.opcode) {
                    .call0 => 0,
                    .call1 => 1,
                    .call2 => 2,
                    .call3 => 3,
                    else => unreachable,
                };

                // Pop arguments
                var args = std.ArrayListUnmanaged(u32){};
                var i: u16 = 0;
                while (i < argc) : (i += 1) {
                    if (self.pop()) |arg| {
                        try args.append(self.allocator, arg);
                    }
                }

                // Pop function
                const func = self.pop() orelse return error.StackUnderflow;

                // Create call result
                const result_type: SymType = if (self.isSelfRef(func)) .int else .value;
                try self.push(try self.newValue(result_type, .{ .call = .{
                    .func = func,
                    .args = args.items,
                } }));
            },

            // Return - no stack change needed for analysis
            .@"return", .return_undef => {},

            // Stack manipulation
            .drop => {
                _ = self.pop();
            },
            .dup => {
                if (self.peek()) |top| {
                    try self.push(top);
                }
            },

            else => {
                // Unknown instruction - treat as opaque
            },
        }
    }

    /// Analyze a sequence of instructions
    pub fn analyze(self: *SymbolicStack, instructions: []const Instruction) !void {
        for (instructions) |instr| {
            try self.processInstruction(instr);
        }
    }
};

/// Collected information about a call site
pub const CallSite = struct {
    /// Is this a self-recursive call?
    is_self_recursive: bool,
    /// Symbolic IDs of arguments
    arg_ids: []const u32,
    /// PC of the call instruction
    pc: u32,
    /// Number of arguments
    argc: u16,
};

/// Analyze a basic block and extract call site information
pub fn analyzeBlock(allocator: Allocator, instructions: []const Instruction) !struct {
    stack: SymbolicStack,
    call_sites: []CallSite,
} {
    var stack = try SymbolicStack.init(allocator, 0);
    var call_sites = std.ArrayListUnmanaged(CallSite){};

    for (instructions) |instr| {
        // Check for call instructions (including tail_call for TCO)
        switch (instr.opcode) {
            .call0, .call1, .call2, .call3, .tail_call => {
                const argc: u16 = switch (instr.opcode) {
                    .call0 => 0,
                    .call1 => 1,
                    .call2 => 2,
                    .call3 => 3,
                    .tail_call => instr.operand.u16, // npop format - operand contains argc
                    else => unreachable,
                };

                // Check if function on stack is self-ref
                // Stack state: [..., func, arg0, arg1, ...]
                // Guard against underflow when stack doesn't have enough elements
                const needed = @as(usize, argc) + 1;
                const is_self = if (stack.stack.items.len >= needed) blk: {
                    const func_pos = stack.stack.items.len - needed;
                    break :blk stack.isSelfRef(stack.stack.items[func_pos]);
                } else false;

                // Collect argument IDs (only if stack has enough elements)
                var args = std.ArrayListUnmanaged(u32){};
                if (stack.stack.items.len >= argc) {
                    var i: usize = 0;
                    while (i < argc) : (i += 1) {
                        const arg_pos = stack.stack.items.len - @as(usize, argc) + i;
                        try args.append(allocator, stack.stack.items[arg_pos]);
                    }
                }

                try call_sites.append(allocator, .{
                    .is_self_recursive = is_self,
                    .arg_ids = args.items,
                    .pc = instr.pc,
                    .argc = argc,
                });
            },
            else => {},
        }

        try stack.processInstruction(instr);
    }

    return .{
        .stack = stack,
        .call_sites = call_sites.items,
    };
}
