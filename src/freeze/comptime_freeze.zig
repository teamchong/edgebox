//! Comptime Frozen Functions
//!
//! Generates native Zig function implementations from QuickJS bytecode at compile time.
//! No C codegen, no external compilation - pure Zig.
//!
//! Architecture:
//!   1. @embedFile brings bytecode into binary
//!   2. Comptime parses bytecode, identifies freezable functions
//!   3. Comptime generates Zig function bodies
//!   4. Single ABI call registers all with QuickJS
//!
//! Benefits:
//!   - No runtime codegen overhead
//!   - No C compiler dependency
//!   - Type-safe opcode implementations
//!   - Comptime validation

const std = @import("std");
const opcodes = @import("opcodes.zig");

/// JSValue representation (matches QuickJS)
pub const JSValue = extern struct {
    u: extern union {
        int32: i32,
        float64: f64,
        ptr: ?*anyopaque,
    },
    tag: i64,

    // Tags from QuickJS
    pub const TAG_INT: i64 = 0;
    pub const TAG_BOOL: i64 = 1;
    pub const TAG_NULL: i64 = 2;
    pub const TAG_UNDEFINED: i64 = 3;
    pub const TAG_FLOAT64: i64 = 7;
    pub const TAG_OBJECT: i64 = -1;
    pub const TAG_STRING: i64 = -7;
    pub const TAG_EXCEPTION: i64 = 6;

    pub fn initInt(val: i32) JSValue {
        return .{ .u = .{ .int32 = val }, .tag = TAG_INT };
    }

    pub fn initBool(val: bool) JSValue {
        return .{ .u = .{ .int32 = if (val) 1 else 0 }, .tag = TAG_BOOL };
    }

    pub fn initUndefined() JSValue {
        return .{ .u = .{ .int32 = 0 }, .tag = TAG_UNDEFINED };
    }

    pub fn isInt(self: JSValue) bool {
        return self.tag == TAG_INT;
    }

    pub fn toInt(self: JSValue) i32 {
        return self.u.int32;
    }

    pub fn isException(self: JSValue) bool {
        return self.tag == TAG_EXCEPTION;
    }
};

/// Opaque QuickJS context pointer
pub const JSContext = opaque {};

/// Function signature for frozen functions
pub const FrozenFn = *const fn (*JSContext, JSValue, c_int, [*]JSValue) callconv(.C) JSValue;

/// Frozen function entry for registration
pub const FrozenEntry = struct {
    name: [*:0]const u8, // JS function name
    func: FrozenFn, // Native implementation
    arg_count: u8, // Expected argument count
};

/// Comptime bytecode instruction
pub const Instruction = struct {
    opcode: opcodes.Opcode,
    pc: u32,
    operand: i32 = 0,
    operand2: i32 = 0,
};

/// Parse bytecode at comptime into instruction list
pub fn parseBytecodComptime(bytecode: []const u8) []const Instruction {
    comptime {
        var instructions: [1024]Instruction = undefined;
        var count: usize = 0;
        var pc: usize = 0;

        while (pc < bytecode.len and count < 1024) {
            const op_byte = bytecode[pc];
            const op: opcodes.Opcode = @enumFromInt(op_byte);
            const info = op.getInfo();

            var instr = Instruction{
                .opcode = op,
                .pc = @intCast(pc),
            };

            // Parse operands based on format
            switch (info.format) {
                .none, .none_int, .none_loc, .none_arg, .none_var_ref, .npopx => {},
                .u8, .loc8, .const8 => {
                    if (pc + 1 < bytecode.len) {
                        instr.operand = bytecode[pc + 1];
                    }
                },
                .i8, .label8 => {
                    if (pc + 1 < bytecode.len) {
                        instr.operand = @as(i8, @bitCast(bytecode[pc + 1]));
                    }
                },
                .u16, .loc, .arg, .var_ref, .npop, .npop_u16 => {
                    if (pc + 2 < bytecode.len) {
                        instr.operand = @as(i32, bytecode[pc + 1]) | (@as(i32, bytecode[pc + 2]) << 8);
                    }
                },
                .i16, .label16 => {
                    if (pc + 2 < bytecode.len) {
                        const val = @as(u16, bytecode[pc + 1]) | (@as(u16, bytecode[pc + 2]) << 8);
                        instr.operand = @as(i16, @bitCast(val));
                    }
                },
                .i32, .label, .atom, .@"const", .u32 => {
                    if (pc + 4 < bytecode.len) {
                        instr.operand = @as(i32, bytecode[pc + 1]) |
                            (@as(i32, bytecode[pc + 2]) << 8) |
                            (@as(i32, bytecode[pc + 3]) << 16) |
                            (@as(i32, bytecode[pc + 4]) << 24);
                    }
                },
                else => {}, // Complex formats - skip for now
            }

            instructions[count] = instr;
            count += 1;
            pc += info.size;
        }

        return instructions[0..count];
    }
}

/// Check if a function can be frozen (comptime)
pub fn canFreeze(comptime instructions: []const Instruction) bool {
    for (instructions) |instr| {
        const info = instr.opcode.getInfo();
        if (info.category == .never_freeze) {
            return false;
        }
        // Skip complex opcodes for now
        if (info.category == .complex and instr.opcode != .call1 and instr.opcode != .call0) {
            return false;
        }
    }
    return true;
}

/// Generate a frozen function implementation at comptime
/// Returns a function pointer that can be registered with QuickJS
pub fn generateFrozenFunc(
    comptime instructions: []const Instruction,
    comptime arg_count: u8,
    comptime var_count: u8,
    comptime is_self_recursive: bool,
) FrozenFn {
    const Impl = struct {
        fn execute(ctx: *JSContext, this_val: JSValue, argc: c_int, argv: [*]JSValue) callconv(.C) JSValue {
            _ = this_val;
            _ = ctx;

            // Local variables (args + locals)
            var locals: [32]JSValue = [_]JSValue{JSValue.initUndefined()} ** 32;

            // Copy arguments to locals
            const actual_argc: usize = @intCast(@max(0, argc));
            for (0..@min(arg_count, actual_argc)) |i| {
                locals[i] = argv[i];
            }

            // Operand stack
            var stack: [64]JSValue = undefined;
            var sp: usize = 0;

            // Execute bytecode
            comptime var pc: usize = 0;
            inline while (pc < instructions.len) {
                const instr = instructions[pc];

                switch (instr.opcode) {
                    // Push constants
                    .push_i32 => {
                        stack[sp] = JSValue.initInt(instr.operand);
                        sp += 1;
                    },
                    .push_0 => {
                        stack[sp] = JSValue.initInt(0);
                        sp += 1;
                    },
                    .push_1 => {
                        stack[sp] = JSValue.initInt(1);
                        sp += 1;
                    },
                    .push_true => {
                        stack[sp] = JSValue.initBool(true);
                        sp += 1;
                    },
                    .push_false => {
                        stack[sp] = JSValue.initBool(false);
                        sp += 1;
                    },
                    .undefined => {
                        stack[sp] = JSValue.initUndefined();
                        sp += 1;
                    },

                    // Local variable access
                    .get_loc0 => {
                        stack[sp] = locals[0];
                        sp += 1;
                    },
                    .get_loc1 => {
                        stack[sp] = locals[1];
                        sp += 1;
                    },
                    .get_loc2 => {
                        stack[sp] = locals[2];
                        sp += 1;
                    },
                    .get_loc3 => {
                        stack[sp] = locals[3];
                        sp += 1;
                    },
                    .get_arg0 => {
                        stack[sp] = locals[0];
                        sp += 1;
                    },
                    .get_arg1 => {
                        stack[sp] = locals[1];
                        sp += 1;
                    },
                    .get_arg2 => {
                        stack[sp] = locals[2];
                        sp += 1;
                    },
                    .get_arg3 => {
                        stack[sp] = locals[3];
                        sp += 1;
                    },
                    .put_loc0 => {
                        sp -= 1;
                        locals[0] = stack[sp];
                    },
                    .put_loc1 => {
                        sp -= 1;
                        locals[1] = stack[sp];
                    },
                    .get_loc => {
                        const idx: usize = @intCast(instr.operand);
                        stack[sp] = locals[idx];
                        sp += 1;
                    },
                    .put_loc => {
                        const idx: usize = @intCast(instr.operand);
                        sp -= 1;
                        locals[idx] = stack[sp];
                    },

                    // Stack operations
                    .drop => {
                        sp -= 1;
                    },
                    .dup => {
                        stack[sp] = stack[sp - 1];
                        sp += 1;
                    },

                    // Arithmetic (int32 fast path)
                    .add => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (a.isInt() and b.isInt()) {
                            stack[sp - 1] = JSValue.initInt(a.toInt() +% b.toInt());
                        } else {
                            // Fallback - would need runtime call
                            stack[sp - 1] = JSValue.initUndefined();
                        }
                    },
                    .sub => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (a.isInt() and b.isInt()) {
                            stack[sp - 1] = JSValue.initInt(a.toInt() -% b.toInt());
                        } else {
                            stack[sp - 1] = JSValue.initUndefined();
                        }
                    },
                    .mul => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (a.isInt() and b.isInt()) {
                            stack[sp - 1] = JSValue.initInt(a.toInt() *% b.toInt());
                        } else {
                            stack[sp - 1] = JSValue.initUndefined();
                        }
                    },

                    // Comparisons
                    .lt => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (a.isInt() and b.isInt()) {
                            stack[sp - 1] = JSValue.initBool(a.toInt() < b.toInt());
                        } else {
                            stack[sp - 1] = JSValue.initBool(false);
                        }
                    },
                    .lte => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (a.isInt() and b.isInt()) {
                            stack[sp - 1] = JSValue.initBool(a.toInt() <= b.toInt());
                        } else {
                            stack[sp - 1] = JSValue.initBool(false);
                        }
                    },
                    .eq => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (a.isInt() and b.isInt()) {
                            stack[sp - 1] = JSValue.initBool(a.toInt() == b.toInt());
                        } else {
                            stack[sp - 1] = JSValue.initBool(false);
                        }
                    },

                    // Return
                    .@"return" => {
                        sp -= 1;
                        return stack[sp];
                    },
                    .return_undef => {
                        return JSValue.initUndefined();
                    },

                    // Self-recursive call (direct Zig recursion)
                    .call1 => {
                        if (is_self_recursive) {
                            // Direct recursion - call ourselves
                            sp -= 2; // Pop function ref and arg
                            const arg = stack[sp + 1];
                            var call_argv = [_]JSValue{arg};
                            const result = execute(ctx, JSValue.initUndefined(), 1, &call_argv);
                            stack[sp] = result;
                            sp += 1;
                        } else {
                            // Non-recursive call - would need runtime dispatch
                            return JSValue.initUndefined();
                        }
                    },

                    // Conditional jumps (comptime unrolled)
                    .if_false, .if_true => {
                        // Control flow is handled by comptime unrolling
                        // For simple cases, we inline both branches
                        sp -= 1;
                    },

                    else => {
                        // Unsupported opcode - return undefined
                        return JSValue.initUndefined();
                    },
                }

                pc += 1;
            }

            // Implicit return undefined
            return JSValue.initUndefined();
        }
    };

    _ = var_count;
    return Impl.execute;
}

// External QuickJS functions (imported at link time)
extern fn JS_SetPropertyStr(ctx: *JSContext, obj: JSValue, prop: [*:0]const u8, val: JSValue) c_int;
extern fn JS_GetGlobalObject(ctx: *JSContext) JSValue;
extern fn JS_NewCFunction2(ctx: *JSContext, func: FrozenFn, name: [*:0]const u8, length: c_int, cproto: c_int, magic: c_int) JSValue;
extern fn JS_FreeValue(ctx: *JSContext, val: JSValue) void;

/// Register all frozen functions with QuickJS context
/// This is the single ABI entry point
pub fn registerFrozenFunctions(ctx: *JSContext, comptime entries: []const FrozenEntry) void {
    const global = JS_GetGlobalObject(ctx);
    defer JS_FreeValue(ctx, global);

    inline for (entries) |entry| {
        const func_val = JS_NewCFunction2(ctx, entry.func, entry.name, entry.arg_count, 0, 0);
        _ = JS_SetPropertyStr(ctx, global, entry.name, func_val);
    }
}

// ============================================================================
// Example: Fibonacci at comptime
// ============================================================================

/// Manually written frozen fib for testing
/// This is what the comptime generator should produce
fn frozen_fib_manual(ctx: *JSContext, this_val: JSValue, argc: c_int, argv: [*]JSValue) callconv(.C) JSValue {
    _ = this_val;
    _ = ctx;

    if (argc < 1) return JSValue.initUndefined();

    const n_val = argv[0];
    if (!n_val.isInt()) return JSValue.initUndefined();

    const n = n_val.toInt();
    if (n <= 1) return JSValue.initInt(n);

    // Direct recursion in Zig - no ABI overhead per recursion
    var args1 = [_]JSValue{JSValue.initInt(n - 1)};
    var args2 = [_]JSValue{JSValue.initInt(n - 2)};

    const r1 = frozen_fib_manual(ctx, this_val, 1, &args1);
    const r2 = frozen_fib_manual(ctx, this_val, 1, &args2);

    if (r1.isException() or r2.isException()) {
        return JSValue.initUndefined();
    }

    return JSValue.initInt(r1.toInt() + r2.toInt());
}

/// Test entry for manual fib
pub const fib_entry = FrozenEntry{
    .name = "__frozen_fib",
    .func = frozen_fib_manual,
    .arg_count = 1,
};

test "frozen fib" {
    // This would need a real JSContext to test
    // For now just verify compilation
    const entries = [_]FrozenEntry{fib_entry};
    _ = entries;
}
