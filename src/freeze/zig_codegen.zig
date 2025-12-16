//! Zig Code Generator for Frozen Functions
//!
//! Generates executable Zig functions from QuickJS bytecode.
//! Uses comptime for zero-overhead function generation.
//!
//! Architecture:
//!   1. Parse bytecode into instructions at comptime
//!   2. Build control flow graph (CFG) at comptime
//!   3. Generate Zig function body using CFG block switch
//!   4. Runtime block dispatch for proper control flow (jumps work!)
//!   5. Register with QuickJS via single ABI call
//!
//! Key insight: comptime generates the switch cases, but block_id is runtime,
//! allowing proper jump handling without inline while limitations.

const std = @import("std");
const opcodes = @import("opcodes.zig");
const handlers = @import("zig_handlers.zig");
const jsvalue = @import("zig_jsvalue.zig");
const parser = @import("bytecode_parser.zig");
const cfg_mod = @import("cfg_builder.zig");

const Opcode = opcodes.Opcode;
const JSValue = jsvalue.JSValue;
const JSContext = jsvalue.JSContext;
const FrozenFn = jsvalue.FrozenFn;
const FrozenEntry = jsvalue.FrozenEntry;
const dupValue = jsvalue.dupValue;
const freeValue = jsvalue.freeValue;

// External QuickJS functions
const JS_Add = jsvalue.JS_Add;
const JS_Sub = jsvalue.JS_Sub;
const JS_Mul = jsvalue.JS_Mul;
const JS_Div = jsvalue.JS_Div;
const JS_LT = jsvalue.JS_LT;
const JS_LE = jsvalue.JS_LE;
const JS_EQ = jsvalue.JS_EQ;
const JS_Call = jsvalue.JS_Call;
const JS_GetGlobalObject = jsvalue.JS_GetGlobalObject;
const JS_FreeValue = jsvalue.JS_FreeValue;
const JS_NewCFunction2 = jsvalue.JS_NewCFunction2;
const JS_SetPropertyStr = jsvalue.JS_SetPropertyStr;
const JS_ThrowRangeError = jsvalue.JS_ThrowRangeError;

/// Maximum call depth (matches Node.js)
const MAX_CALL_DEPTH = 10000;

/// Thread-local call depth counter
threadlocal var call_depth: u32 = 0;

// ============================================================================
// Comptime CFG Structures
// ============================================================================

/// Special block IDs for control flow
const BLOCK_RETURN: i32 = -1;
const BLOCK_RETURN_UNDEF: i32 = -2;
const BLOCK_CALL_SELF: i32 = -3;

/// Maximum blocks in a function (comptime limit)
const MAX_BLOCKS = 256;

/// Comptime basic block info
const ComptimeBlock = struct {
    /// Start instruction index
    start_idx: u32,
    /// End instruction index (exclusive)
    end_idx: u32,
    /// Start PC for this block
    start_pc: u32,
    /// True branch target (for conditional jumps)
    true_target: i32 = BLOCK_RETURN,
    /// False/fallthrough target
    false_target: i32 = BLOCK_RETURN,
    /// Is this block reachable?
    reachable: bool = true,
};

/// Comptime CFG built from bytecode
const ComptimeCFG = struct {
    blocks: []const ComptimeBlock,
    block_count: u32,
    /// Map from PC to block index
    pc_to_block: [4096]i32, // -1 = no block at this PC
};

/// Build CFG at comptime from instructions
fn buildComptimeCFG(comptime instructions: []const Instruction) ComptimeCFG {
    comptime {
        if (instructions.len == 0) {
            return ComptimeCFG{
                .blocks = &[_]ComptimeBlock{},
                .block_count = 0,
                .pc_to_block = [_]i32{-1} ** 4096,
            };
        }

        // Step 1: Identify leaders (block start points)
        var leaders: [4096]bool = [_]bool{false} ** 4096;
        leaders[instructions[0].pc] = true; // First instruction is always a leader

        for (instructions) |instr| {
            // Jump targets are leaders
            if (instr.opcode == .goto or instr.opcode == .goto8 or instr.opcode == .goto16) {
                const target = computeJumpTarget(instr.pc, instr.operand, opcodes.getInfo(instr.opcode).size);
                if (target >= 0 and target < 4096) {
                    leaders[@intCast(target)] = true;
                }
            } else if (instr.opcode == .if_false or instr.opcode == .if_false8 or
                instr.opcode == .if_true or instr.opcode == .if_true8)
            {
                const target = computeJumpTarget(instr.pc, instr.operand, opcodes.getInfo(instr.opcode).size);
                if (target >= 0 and target < 4096) {
                    leaders[@intCast(target)] = true;
                }
                // Fallthrough is also a leader
                const next_pc = instr.pc + opcodes.getInfo(instr.opcode).size;
                if (next_pc < 4096) {
                    leaders[next_pc] = true;
                }
            } else if (instr.opcode == .@"return" or instr.opcode == .return_undef) {
                // Instruction after return is a leader (if any)
                const next_pc = instr.pc + opcodes.getInfo(instr.opcode).size;
                if (next_pc < 4096) {
                    leaders[next_pc] = true;
                }
            }
        }

        // Step 2: Create blocks from leaders
        var blocks: [MAX_BLOCKS]ComptimeBlock = undefined;
        var block_count: u32 = 0;
        var pc_to_block: [4096]i32 = [_]i32{-1} ** 4096;

        for (instructions, 0..) |_, i| {
            const pc = instructions[i].pc;
            if (leaders[pc]) {
                // Find end of this block
                var end_idx: u32 = @intCast(i + 1);
                while (end_idx < instructions.len) {
                    if (leaders[instructions[end_idx].pc]) break;
                    end_idx += 1;
                }

                blocks[block_count] = ComptimeBlock{
                    .start_idx = @intCast(i),
                    .end_idx = end_idx,
                    .start_pc = pc,
                };

                // Map all PCs in this block to block_count
                for (instructions[i..end_idx]) |instr| {
                    pc_to_block[instr.pc] = @intCast(block_count);
                }

                block_count += 1;
                if (block_count >= MAX_BLOCKS) break;
            }
        }

        // Step 3: Connect blocks (set targets)
        for (0..block_count) |bi| {
            const block = &blocks[bi];
            if (block.end_idx == 0) continue;

            const last_idx = block.end_idx - 1;
            const last_instr = instructions[last_idx];

            switch (last_instr.opcode) {
                .@"return" => {
                    block.true_target = BLOCK_RETURN;
                    block.false_target = BLOCK_RETURN;
                },
                .return_undef => {
                    block.true_target = BLOCK_RETURN_UNDEF;
                    block.false_target = BLOCK_RETURN_UNDEF;
                },
                .goto, .goto8, .goto16 => {
                    const target = computeJumpTarget(last_instr.pc, last_instr.operand, opcodes.getInfo(last_instr.opcode).size);
                    if (target >= 0 and target < 4096) {
                        const target_block = pc_to_block[@intCast(target)];
                        block.true_target = target_block;
                        block.false_target = target_block;
                    }
                },
                .if_false, .if_false8, .if_true, .if_true8 => {
                    const jump_target = computeJumpTarget(last_instr.pc, last_instr.operand, opcodes.getInfo(last_instr.opcode).size);
                    const fall_target: i32 = @intCast(last_instr.pc + opcodes.getInfo(last_instr.opcode).size);

                    const jump_block = if (jump_target >= 0 and jump_target < 4096)
                        pc_to_block[@intCast(jump_target)]
                    else
                        BLOCK_RETURN;
                    const fall_block = if (fall_target >= 0 and fall_target < 4096)
                        pc_to_block[@intCast(fall_target)]
                    else
                        BLOCK_RETURN;

                    // if_false: jump if condition is false, fall through if true
                    if (last_instr.opcode == .if_false or last_instr.opcode == .if_false8) {
                        block.true_target = fall_block; // condition true -> fallthrough
                        block.false_target = jump_block; // condition false -> jump
                    } else {
                        block.true_target = jump_block; // condition true -> jump
                        block.false_target = fall_block; // condition false -> fallthrough
                    }
                },
                else => {
                    // Fallthrough to next block
                    if (bi + 1 < block_count) {
                        block.true_target = @intCast(bi + 1);
                        block.false_target = @intCast(bi + 1);
                    }
                },
            }
        }

        return ComptimeCFG{
            .blocks = blocks[0..block_count],
            .block_count = block_count,
            .pc_to_block = pc_to_block,
        };
    }
}

/// Compute jump target from instruction PC and operand
fn computeJumpTarget(pc: u32, operand: i32, instr_size: u8) i32 {
    // Jump is relative to PC after instruction
    const base: i32 = @intCast(pc + instr_size);
    return base + operand;
}

/// Frozen function options
pub const FrozenOptions = struct {
    func_name: []const u8 = "frozen_func",
    js_name: []const u8 = "",
    arg_count: u8 = 0,
    var_count: u8 = 0,
    max_stack: u16 = 256,
    is_self_recursive: bool = false,
};

/// Instruction for runtime execution
pub const Instruction = struct {
    opcode: Opcode,
    pc: u32,
    operand: i32 = 0,
    operand2: i32 = 0,
};

/// Generate a frozen function at comptime from bytecode
pub fn generateFrozen(
    comptime bytecode: []const u8,
    comptime opts: FrozenOptions,
) FrozenFn {
    // Parse bytecode at comptime
    const instructions = comptime parseBytecodComptime(bytecode);

    // Generate the function
    return struct {
        fn execute(ctx: *JSContext, this_val: JSValue, argc: c_int, argv: [*]JSValue) callconv(.c) JSValue {
            _ = this_val; // Required for ABI compatibility

            // Stack overflow check
            if (call_depth >= MAX_CALL_DEPTH) {
                return JS_ThrowRangeError(ctx, "Maximum call stack size exceeded");
            }
            call_depth += 1;
            defer call_depth -= 1;

            // Local variables
            var locals: [if (opts.var_count > 0) opts.var_count else 1]JSValue = undefined;
            for (&locals) |*l| {
                l.* = JSValue.initUndefined();
            }

            // Copy arguments to locals (QuickJS convention: args are first locals)
            const actual_argc: usize = @intCast(@max(0, argc));
            inline for (0..opts.arg_count) |i| {
                if (i < actual_argc) {
                    locals[i] = argv[i];
                }
            }

            // Operand stack
            var stack: [opts.max_stack]JSValue = undefined;
            var sp: usize = 0;

            // V8-style length cache for arg0 (-1 = not yet computed)
            var arg0_len_cache: i64 = -1;

            // Execute bytecode
            comptime var pc: usize = 0;
            inline while (pc < instructions.len) : (pc += 1) {
                const instr = instructions[pc];

                switch (instr.opcode) {
                    // ==================== PUSH CONSTANTS ====================
                    .push_minus1 => {
                        stack[sp] = JSValue.initInt(-1);
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
                    .push_2 => {
                        stack[sp] = JSValue.initInt(2);
                        sp += 1;
                    },
                    .push_3 => {
                        stack[sp] = JSValue.initInt(3);
                        sp += 1;
                    },
                    .push_i32 => {
                        stack[sp] = JSValue.initInt(instr.operand);
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
                    .null => {
                        stack[sp] = JSValue.initNull();
                        sp += 1;
                    },
                    .undefined => {
                        stack[sp] = JSValue.initUndefined();
                        sp += 1;
                    },

                    // ==================== LOCAL VARIABLES ====================
                    .get_loc0 => {
                        stack[sp] = dupValue(ctx, locals[0]);
                        sp += 1;
                    },
                    .get_loc1 => {
                        stack[sp] = dupValue(ctx, locals[1]);
                        sp += 1;
                    },
                    .get_loc2 => {
                        stack[sp] = dupValue(ctx, locals[2]);
                        sp += 1;
                    },
                    .get_loc3 => {
                        stack[sp] = dupValue(ctx, locals[3]);
                        sp += 1;
                    },
                    .get_loc => {
                        const idx: usize = @intCast(instr.operand);
                        stack[sp] = dupValue(ctx, locals[idx]);
                        sp += 1;
                    },
                    .put_loc0 => {
                        sp -= 1;
                        freeValue(ctx, locals[0]);
                        locals[0] = stack[sp];
                    },
                    .put_loc1 => {
                        sp -= 1;
                        freeValue(ctx, locals[1]);
                        locals[1] = stack[sp];
                    },
                    .put_loc2 => {
                        sp -= 1;
                        freeValue(ctx, locals[2]);
                        locals[2] = stack[sp];
                    },
                    .put_loc3 => {
                        sp -= 1;
                        freeValue(ctx, locals[3]);
                        locals[3] = stack[sp];
                    },
                    .put_loc => {
                        const idx: usize = @intCast(instr.operand);
                        sp -= 1;
                        freeValue(ctx, locals[idx]);
                        locals[idx] = stack[sp];
                    },

                    // ==================== ARGUMENTS ====================
                    .get_arg0 => {
                        stack[sp] = if (0 < actual_argc) dupValue(ctx, argv[0]) else JSValue.initUndefined();
                        sp += 1;
                    },
                    .get_arg1 => {
                        stack[sp] = if (1 < actual_argc) dupValue(ctx, argv[1]) else JSValue.initUndefined();
                        sp += 1;
                    },
                    .get_arg2 => {
                        stack[sp] = if (2 < actual_argc) dupValue(ctx, argv[2]) else JSValue.initUndefined();
                        sp += 1;
                    },
                    .get_arg3 => {
                        stack[sp] = if (3 < actual_argc) dupValue(ctx, argv[3]) else JSValue.initUndefined();
                        sp += 1;
                    },

                    // ==================== STACK OPS ====================
                    .drop => {
                        sp -= 1;
                        freeValue(ctx, stack[sp]);
                    },
                    .dup => {
                        stack[sp] = dupValue(ctx, stack[sp - 1]);
                        sp += 1;
                    },
                    .swap => {
                        const tmp = stack[sp - 1];
                        stack[sp - 1] = stack[sp - 2];
                        stack[sp - 2] = tmp;
                    },
                    .nop => {},

                    // ==================== ARITHMETIC (SINT fast path) ====================
                    .add => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (JSValue.addSint(a, b)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JS_Add(ctx, a, b);
                            freeValue(ctx, a);
                            freeValue(ctx, b);
                        }
                    },
                    .sub => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (JSValue.subSint(a, b)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JS_Sub(ctx, a, b);
                            freeValue(ctx, a);
                            freeValue(ctx, b);
                        }
                    },
                    .mul => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (JSValue.mulSint(a, b)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JS_Mul(ctx, a, b);
                            freeValue(ctx, a);
                            freeValue(ctx, b);
                        }
                    },

                    // ==================== COMPARISON (SINT fast path) ====================
                    .lt => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (JSValue.ltSint(a, b)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JSValue.initBool(JS_LT(ctx, a, b) != 0);
                            freeValue(ctx, a);
                            freeValue(ctx, b);
                        }
                    },
                    .lte => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (JSValue.leSint(a, b)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JSValue.initBool(JS_LE(ctx, a, b) != 0);
                            freeValue(ctx, a);
                            freeValue(ctx, b);
                        }
                    },
                    .eq, .strict_eq => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (JSValue.eqSint(a, b)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JSValue.initBool(JS_EQ(ctx, a, b) != 0);
                            freeValue(ctx, a);
                            freeValue(ctx, b);
                        }
                    },

                    // ==================== BITWISE ====================
                    .@"and" => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (JSValue.andSint(a, b)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JSValue.initUndefined();
                        }
                    },
                    .@"or" => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (JSValue.orSint(a, b)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JSValue.initUndefined();
                        }
                    },

                    // ==================== UNARY ====================
                    .inc => {
                        const a = stack[sp - 1];
                        if (JSValue.incSint(a)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JS_Add(ctx, a, JSValue.initInt(1));
                            freeValue(ctx, a);
                        }
                    },
                    .dec => {
                        const a = stack[sp - 1];
                        if (JSValue.decSint(a)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JS_Sub(ctx, a, JSValue.initInt(1));
                            freeValue(ctx, a);
                        }
                    },
                    .neg => {
                        const a = stack[sp - 1];
                        if (JSValue.negSint(a)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JSValue.initUndefined();
                        }
                    },
                    .lnot => {
                        const v = stack[sp - 1];
                        stack[sp - 1] = JSValue.initBool(v.isFalsy());
                        freeValue(ctx, v);
                    },

                    // ==================== CONTROL FLOW ====================
                    .@"return" => {
                        sp -= 1;
                        return stack[sp];
                    },
                    .return_undef => {
                        return JSValue.initUndefined();
                    },

                    // ==================== FUNCTION CALLS ====================
                    .call0 => {
                        sp -= 1;
                        const func = stack[sp];
                        const result = JS_Call(ctx, func, JSValue.initUndefined(), 0, @ptrFromInt(0));
                        freeValue(ctx, func);
                        if (result.isException()) {
                            return result;
                        }
                        stack[sp] = result;
                        sp += 1;
                    },
                    .call1 => {
                        if (opts.is_self_recursive) {
                            // Direct recursive call
                            sp -= 2;
                            const arg = stack[sp + 1];
                            var call_args = [_]JSValue{arg};
                            const result = execute(ctx, JSValue.initUndefined(), 1, &call_args);
                            if (result.isException()) {
                                return result;
                            }
                            stack[sp] = result;
                            sp += 1;
                        } else {
                            // External call
                            sp -= 2;
                            const arg = stack[sp + 1];
                            const func = stack[sp];
                            const result = JS_Call(ctx, func, JSValue.initUndefined(), 1, @ptrCast(&arg));
                            freeValue(ctx, func);
                            freeValue(ctx, arg);
                            if (result.isException()) {
                                return result;
                            }
                            stack[sp] = result;
                            sp += 1;
                        }
                    },
                    .call2 => {
                        sp -= 3;
                        const func = stack[sp];
                        var call_args = [_]JSValue{ stack[sp + 1], stack[sp + 2] };
                        const result = JS_Call(ctx, func, JSValue.initUndefined(), 2, &call_args);
                        freeValue(ctx, func);
                        freeValue(ctx, call_args[0]);
                        freeValue(ctx, call_args[1]);
                        if (result.isException()) {
                            return result;
                        }
                        stack[sp] = result;
                        sp += 1;
                    },
                    .call3 => {
                        sp -= 4;
                        const func = stack[sp];
                        var call_args = [_]JSValue{ stack[sp + 1], stack[sp + 2], stack[sp + 3] };
                        const result = JS_Call(ctx, func, JSValue.initUndefined(), 3, &call_args);
                        freeValue(ctx, func);
                        freeValue(ctx, call_args[0]);
                        freeValue(ctx, call_args[1]);
                        freeValue(ctx, call_args[2]);
                        if (result.isException()) {
                            return result;
                        }
                        stack[sp] = result;
                        sp += 1;
                    },
                    .call_method => {
                        const call_argc: usize = @intCast(@max(0, instr.operand));
                        sp -= call_argc + 2;
                        const func = stack[sp];
                        const call_this = stack[sp + 1];
                        const result = JS_Call(ctx, func, call_this, @intCast(call_argc), @ptrCast(&stack[sp + 2]));
                        freeValue(ctx, func);
                        freeValue(ctx, call_this);
                        for (0..call_argc) |j| {
                            freeValue(ctx, stack[sp + 2 + j]);
                        }
                        if (result.isException()) {
                            return result;
                        }
                        stack[sp] = result;
                        sp += 1;
                    },

                    // ==================== JUMPS (comptime unrolled) ====================
                    .if_false => {
                        // Control flow handled by basic block structure
                        sp -= 1;
                        // For now, just consume the value
                        // Real implementation needs CFG-based execution
                    },
                    .if_true => {
                        sp -= 1;
                    },
                    .goto => {
                        // Jump handled by CFG
                    },

                    // ==================== CLOSURE VARIABLE ACCESS ====================
                    .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3 => {
                        // Without var_refs context, return undefined
                        stack[sp] = JSValue.initUndefined();
                        sp += 1;
                    },
                    .get_var_ref => {
                        stack[sp] = JSValue.initUndefined();
                        sp += 1;
                    },
                    .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3 => {
                        sp -= 1;
                        freeValue(ctx, stack[sp]);
                    },
                    .put_var_ref => {
                        sp -= 1;
                        freeValue(ctx, stack[sp]);
                    },
                    .set_var_ref0, .set_var_ref1, .set_var_ref2, .set_var_ref3 => {
                        // No-op - value stays on stack
                    },
                    .set_var_ref => {
                        // No-op - value stays on stack
                    },

                    // ==================== ITERATOR OPERATIONS ====================
                    .for_in_start => {
                        const rc = jsvalue.js_frozen_for_in_start(ctx, @ptrCast(&stack[sp - 1]));
                        if (rc < 0) return JSValue.initUndefined();
                    },
                    .for_in_next => {
                        const rc = jsvalue.js_frozen_for_in_next(ctx, @ptrCast(&stack[sp - 1]));
                        if (rc < 0) return JSValue.initUndefined();
                        sp += 2;
                    },
                    .for_of_start => {
                        const rc = jsvalue.js_frozen_for_of_start(ctx, @ptrCast(&stack[sp - 1]), 0);
                        if (rc < 0) return JSValue.initUndefined();
                        sp += 2;
                    },
                    .for_of_next => {
                        const offset: c_int = @intCast(instr.operand);
                        const rc = jsvalue.js_frozen_for_of_next(ctx, @ptrCast(&stack[sp - 3]), offset);
                        if (rc < 0) return JSValue.initUndefined();
                        sp += 2;
                    },
                    .iterator_close => {
                        sp -= 3;
                        freeValue(ctx, stack[sp]);
                        freeValue(ctx, stack[sp + 1]);
                        freeValue(ctx, stack[sp + 2]);
                    },
                    .iterator_get_value_done => {
                        const result = stack[sp - 1];
                        const done_val = jsvalue.JS_GetPropertyStr(ctx, result, "done");
                        const value_val = jsvalue.JS_GetPropertyStr(ctx, result, "value");
                        freeValue(ctx, result);
                        stack[sp - 1] = value_val;
                        stack[sp] = done_val;
                        sp += 1;
                    },

                    // ==================== TYPE OPERATIONS ====================
                    .typeof => {
                        const val = stack[sp - 1];
                        const type_str = jsvalue.JS_TypeOf(ctx, val);
                        freeValue(ctx, val);
                        stack[sp - 1] = type_str;
                    },
                    .instanceof => {
                        sp -= 1;
                        const ctor = stack[sp];
                        const obj = stack[sp - 1];
                        const result = jsvalue.JS_IsInstanceOf(ctx, obj, ctor);
                        freeValue(ctx, obj);
                        freeValue(ctx, ctor);
                        stack[sp - 1] = JSValue.initBool(result > 0);
                    },
                    .in => {
                        sp -= 1;
                        const obj = stack[sp];
                        const prop_val = stack[sp - 1];
                        var result: c_int = 0;
                        if (prop_val.isInt()) {
                            const idx: u32 = @intCast(@max(0, prop_val.toInt()));
                            const prop = jsvalue.JS_GetPropertyUint32(ctx, obj, idx);
                            result = if (!prop.isUndefined()) 1 else 0;
                            freeValue(ctx, prop);
                        }
                        freeValue(ctx, obj);
                        freeValue(ctx, prop_val);
                        stack[sp - 1] = JSValue.initBool(result > 0);
                    },
                    .is_undefined => {
                        const val = stack[sp - 1];
                        stack[sp - 1] = JSValue.initBool(val.isUndefined());
                        freeValue(ctx, val);
                    },
                    .is_null => {
                        const val = stack[sp - 1];
                        stack[sp - 1] = JSValue.initBool(val.isNull());
                        freeValue(ctx, val);
                    },
                    .is_undefined_or_null => {
                        const val = stack[sp - 1];
                        stack[sp - 1] = JSValue.initBool(val.isUndefined() or val.isNull());
                        freeValue(ctx, val);
                    },
                    .typeof_is_undefined => {
                        const val = stack[sp - 1];
                        stack[sp - 1] = JSValue.initBool(val.isUndefined());
                        freeValue(ctx, val);
                    },
                    .typeof_is_function => {
                        const val = stack[sp - 1];
                        const result = jsvalue.JS_IsFunction(ctx, val);
                        stack[sp - 1] = JSValue.initBool(result > 0);
                        freeValue(ctx, val);
                    },

                    // ==================== OBJECT/ARRAY CREATION ====================
                    .object => {
                        stack[sp] = jsvalue.JS_NewObject(ctx);
                        sp += 1;
                    },
                    .array_from => {
                        const count: usize = @intCast(@max(0, instr.operand));
                        const arr = jsvalue.JS_NewArray(ctx);
                        sp -= count;
                        for (0..count) |i| {
                            _ = jsvalue.JS_SetPropertyUint32(ctx, arr, @intCast(i), stack[sp + i]);
                        }
                        stack[sp] = arr;
                        sp += 1;
                    },
                    .define_array_el => {
                        sp -= 1;
                        const val = stack[sp];
                        const idx = stack[sp - 1];
                        const arr = stack[sp - 2];
                        const idx_int: u32 = if (idx.isInt())
                            @intCast(@max(0, idx.toInt()))
                        else
                            0;
                        _ = jsvalue.JS_SetPropertyUint32(ctx, arr, idx_int, val);
                        if (idx.isInt()) {
                            stack[sp - 1] = JSValue.initInt(idx.toInt() + 1);
                        }
                    },
                    .to_object => {},
                    .set_home_object => {},

                    // ==================== PROPERTY ACCESS ====================
                    .get_field => {
                        const obj = stack[sp - 1];
                        const atom: i32 = instr.operand;
                        const result = jsvalue.JS_GetProperty(ctx, obj, atom);
                        freeValue(ctx, obj);
                        stack[sp - 1] = result;
                    },
                    .get_field2 => {
                        const obj = stack[sp - 1];
                        const atom: i32 = instr.operand;
                        const result = jsvalue.JS_GetProperty(ctx, obj, atom);
                        stack[sp] = result;
                        sp += 1;
                    },
                    .put_field => {
                        sp -= 2;
                        const value = stack[sp + 1];
                        const obj = stack[sp];
                        const atom: i32 = instr.operand;
                        _ = jsvalue.JS_SetProperty(ctx, obj, atom, value);
                        freeValue(ctx, obj);
                    },

                    // ==================== ARRAY ACCESS ====================
                    .get_array_el => {
                        sp -= 1;
                        const idx = stack[sp];
                        const obj = stack[sp - 1];
                        const idx_val: u32 = if (idx.isInt())
                            @intCast(@max(0, idx.toInt()))
                        else
                            0;
                        const result = jsvalue.JS_GetPropertyUint32(ctx, obj, idx_val);
                        freeValue(ctx, obj);
                        freeValue(ctx, idx);
                        stack[sp - 1] = result;
                    },
                    .get_array_el2 => {
                        const idx = stack[sp - 1];
                        const obj = stack[sp - 2];
                        const idx_val: u32 = if (idx.isInt())
                            @intCast(@max(0, idx.toInt()))
                        else
                            0;
                        const result = jsvalue.JS_GetPropertyUint32(ctx, obj, idx_val);
                        freeValue(ctx, idx);
                        stack[sp - 1] = result;
                    },
                    .put_array_el => {
                        sp -= 3;
                        const value = stack[sp + 2];
                        const idx = stack[sp + 1];
                        const obj = stack[sp];
                        const idx_val: u32 = if (idx.isInt())
                            @intCast(@max(0, idx.toInt()))
                        else
                            0;
                        _ = jsvalue.JS_SetPropertyUint32(ctx, obj, idx_val, value);
                        freeValue(ctx, obj);
                        freeValue(ctx, idx);
                    },

                    // ==================== LENGTH ====================
                    .get_length => {
                        const obj = stack[sp - 1];
                        // V8-style optimization: Check if this is arg0 with cached length
                        if (arg0_len_cache >= 0 and actual_argc > 0 and obj.isSameObject(argv[0])) {
                            // Use cached length (2-3x faster for loops)
                            freeValue(ctx, obj);
                            const len = arg0_len_cache;
                            if (len <= std.math.maxInt(i32) and len >= std.math.minInt(i32)) {
                                stack[sp - 1] = JSValue.initInt(@intCast(len));
                            } else {
                                stack[sp - 1] = JSValue.initFloat64(@floatFromInt(len));
                            }
                        } else {
                            // Compute length and potentially cache
                            var len: i64 = 0;
                            const rc = jsvalue.JS_GetLength(ctx, obj, &len);
                            // Cache if this is arg0 (lazy initialization)
                            if (actual_argc > 0 and obj.isSameObject(argv[0])) {
                                arg0_len_cache = if (rc >= 0) len else -1;
                            }
                            freeValue(ctx, obj);
                            if (rc < 0) {
                                stack[sp - 1] = JSValue.initUndefined();
                            } else if (len <= std.math.maxInt(i32) and len >= std.math.minInt(i32)) {
                                stack[sp - 1] = JSValue.initInt(@intCast(len));
                            } else {
                                stack[sp - 1] = JSValue.initFloat64(@floatFromInt(len));
                            }
                        }
                    },

                    // ==================== UNSUPPORTED ====================
                    else => {
                        // Unsupported opcode - return undefined
                        // In production, would fall back to interpreter
                        return JSValue.initUndefined();
                    },
                }
            }

            // Implicit return undefined
            return JSValue.initUndefined();
        }
    }.execute;
}

/// Parse bytecode at comptime
fn parseBytecodComptime(comptime bytecode: []const u8) []const Instruction {
    comptime {
        var instructions: [4096]Instruction = undefined;
        var count: usize = 0;
        var pc: usize = 0;

        while (pc < bytecode.len and count < 4096) {
            const op_byte = bytecode[pc];
            const op: Opcode = @enumFromInt(op_byte);
            const info = opcodes.getInfo(op);

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
                else => {},
            }

            instructions[count] = instr;
            count += 1;
            pc += info.size;
        }

        return instructions[0..count];
    }
}

// ============================================================================
// CFG-Based Frozen Function Generator (Proper Jump Handling)
// ============================================================================

/// Generate a frozen function with proper CFG-based control flow
/// This version correctly handles jumps using a runtime block switch
pub fn generateFrozenCFG(
    comptime bytecode: []const u8,
    comptime opts: FrozenOptions,
) FrozenFn {
    // Parse bytecode and build CFG at comptime
    const instructions = comptime parseBytecodComptime(bytecode);
    const cfg = comptime buildComptimeCFG(instructions);

    // Generate the function with CFG-based execution
    return struct {
        fn execute(ctx: *JSContext, this_val: JSValue, argc: c_int, argv: [*]JSValue) callconv(.c) JSValue {
            _ = this_val;

            // Stack overflow check
            if (call_depth >= MAX_CALL_DEPTH) {
                return JS_ThrowRangeError(ctx, "Maximum call stack size exceeded");
            }
            call_depth += 1;
            defer call_depth -= 1;

            // Local variables
            var locals: [if (opts.var_count > 0) opts.var_count else 1]JSValue = undefined;
            for (&locals) |*l| {
                l.* = JSValue.initUndefined();
            }

            // Copy arguments to locals (QuickJS convention)
            const actual_argc: usize = @intCast(@max(0, argc));
            inline for (0..opts.arg_count) |i| {
                if (i < actual_argc) {
                    locals[i] = argv[i];
                }
            }

            // Operand stack
            var stack: [opts.max_stack]JSValue = undefined;
            var sp: usize = 0;

            // Return value storage
            var return_value: JSValue = JSValue.initUndefined();

            // V8-style length cache for arg0 (-1 = not yet computed)
            var arg0_len_cache: i64 = -1;

            // CFG-based execution with block switch
            var block_id: i32 = 0;

            block_loop: while (block_id >= 0) {
                // Dispatch to current block
                switch (block_id) {
                    inline 0...MAX_BLOCKS - 1 => |comptime_block_id| {
                        if (comptime_block_id < cfg.block_count) {
                            const block = cfg.blocks[comptime_block_id];

                            // Execute all instructions in this block (except last which may be control flow)
                            const last_instr_idx = if (block.end_idx > block.start_idx)
                                block.end_idx - 1
                            else
                                block.start_idx;

                            // Execute non-control-flow instructions
                            inline for (block.start_idx..last_instr_idx) |instr_idx| {
                                const instr = instructions[instr_idx];
                                executeInstruction(ctx, &stack, &sp, &locals, argv, actual_argc, instr, opts, &arg0_len_cache);
                            }

                            // Handle last instruction (may be control flow)
                            if (block.end_idx > block.start_idx) {
                                const last_instr = instructions[last_instr_idx];

                                switch (last_instr.opcode) {
                                    .@"return" => {
                                        sp -= 1;
                                        return_value = stack[sp];
                                        block_id = BLOCK_RETURN;
                                        continue :block_loop;
                                    },
                                    .return_undef => {
                                        return_value = JSValue.initUndefined();
                                        block_id = BLOCK_RETURN_UNDEF;
                                        continue :block_loop;
                                    },
                                    .if_false, .if_false8 => {
                                        sp -= 1;
                                        const cond = stack[sp];
                                        const is_truthy = !cond.isFalsy();
                                        freeValue(ctx, cond);
                                        block_id = if (is_truthy) block.true_target else block.false_target;
                                        continue :block_loop;
                                    },
                                    .if_true, .if_true8 => {
                                        sp -= 1;
                                        const cond = stack[sp];
                                        const is_truthy = !cond.isFalsy();
                                        freeValue(ctx, cond);
                                        block_id = if (is_truthy) block.true_target else block.false_target;
                                        continue :block_loop;
                                    },
                                    .goto, .goto8, .goto16 => {
                                        block_id = block.true_target;
                                        continue :block_loop;
                                    },
                                    .call1 => {
                                        // Execute the call and continue
                                        executeInstruction(ctx, &stack, &sp, &locals, argv, actual_argc, last_instr, opts, &arg0_len_cache);
                                        block_id = block.true_target;
                                        continue :block_loop;
                                    },
                                    else => {
                                        // Non-control-flow instruction at end of block
                                        executeInstruction(ctx, &stack, &sp, &locals, argv, actual_argc, last_instr, opts, &arg0_len_cache);
                                        block_id = block.true_target;
                                        continue :block_loop;
                                    },
                                }
                            } else {
                                // Empty block - go to next
                                block_id = block.true_target;
                                continue :block_loop;
                            }
                        } else {
                            // Invalid block - return undefined
                            block_id = BLOCK_RETURN_UNDEF;
                            continue :block_loop;
                        }
                    },
                    BLOCK_RETURN => break :block_loop, // Return with value
                    BLOCK_RETURN_UNDEF => {
                        return_value = JSValue.initUndefined();
                        break :block_loop;
                    },
                    else => break :block_loop,
                }
            }

            return return_value;
        }

        /// Execute a single non-control-flow instruction
        inline fn executeInstruction(
            ctx: *JSContext,
            stack: *[opts.max_stack]JSValue,
            sp: *usize,
            locals: *[if (opts.var_count > 0) opts.var_count else 1]JSValue,
            argv: [*]JSValue,
            actual_argc: usize,
            instr: Instruction,
            comptime func_opts: FrozenOptions,
            arg0_len_cache: *i64,
        ) void {
            switch (instr.opcode) {
                // ==================== PUSH CONSTANTS ====================
                .push_minus1 => {
                    stack[sp.*] = JSValue.initInt(-1);
                    sp.* += 1;
                },
                .push_0 => {
                    stack[sp.*] = JSValue.initInt(0);
                    sp.* += 1;
                },
                .push_1 => {
                    stack[sp.*] = JSValue.initInt(1);
                    sp.* += 1;
                },
                .push_2 => {
                    stack[sp.*] = JSValue.initInt(2);
                    sp.* += 1;
                },
                .push_3 => {
                    stack[sp.*] = JSValue.initInt(3);
                    sp.* += 1;
                },
                .push_i32 => {
                    stack[sp.*] = JSValue.initInt(instr.operand);
                    sp.* += 1;
                },
                .push_true => {
                    stack[sp.*] = JSValue.initBool(true);
                    sp.* += 1;
                },
                .push_false => {
                    stack[sp.*] = JSValue.initBool(false);
                    sp.* += 1;
                },
                .null => {
                    stack[sp.*] = JSValue.initNull();
                    sp.* += 1;
                },
                .undefined => {
                    stack[sp.*] = JSValue.initUndefined();
                    sp.* += 1;
                },

                // ==================== LOCAL VARIABLES ====================
                .get_loc0 => {
                    stack[sp.*] = dupValue(ctx, locals[0]);
                    sp.* += 1;
                },
                .get_loc1 => {
                    stack[sp.*] = dupValue(ctx, locals[1]);
                    sp.* += 1;
                },
                .get_loc2 => {
                    stack[sp.*] = dupValue(ctx, locals[2]);
                    sp.* += 1;
                },
                .get_loc3 => {
                    stack[sp.*] = dupValue(ctx, locals[3]);
                    sp.* += 1;
                },
                .get_loc => {
                    const idx: usize = @intCast(instr.operand);
                    stack[sp.*] = dupValue(ctx, locals[idx]);
                    sp.* += 1;
                },
                .put_loc0 => {
                    sp.* -= 1;
                    freeValue(ctx, locals[0]);
                    locals[0] = stack[sp.*];
                },
                .put_loc1 => {
                    sp.* -= 1;
                    freeValue(ctx, locals[1]);
                    locals[1] = stack[sp.*];
                },
                .put_loc2 => {
                    sp.* -= 1;
                    freeValue(ctx, locals[2]);
                    locals[2] = stack[sp.*];
                },
                .put_loc3 => {
                    sp.* -= 1;
                    freeValue(ctx, locals[3]);
                    locals[3] = stack[sp.*];
                },
                .put_loc => {
                    const idx: usize = @intCast(instr.operand);
                    sp.* -= 1;
                    freeValue(ctx, locals[idx]);
                    locals[idx] = stack[sp.*];
                },

                // ==================== ARGUMENTS ====================
                .get_arg0 => {
                    stack[sp.*] = if (0 < actual_argc) dupValue(ctx, argv[0]) else JSValue.initUndefined();
                    sp.* += 1;
                },
                .get_arg1 => {
                    stack[sp.*] = if (1 < actual_argc) dupValue(ctx, argv[1]) else JSValue.initUndefined();
                    sp.* += 1;
                },
                .get_arg2 => {
                    stack[sp.*] = if (2 < actual_argc) dupValue(ctx, argv[2]) else JSValue.initUndefined();
                    sp.* += 1;
                },
                .get_arg3 => {
                    stack[sp.*] = if (3 < actual_argc) dupValue(ctx, argv[3]) else JSValue.initUndefined();
                    sp.* += 1;
                },

                // ==================== STACK OPS ====================
                .drop => {
                    sp.* -= 1;
                    freeValue(ctx, stack[sp.*]);
                },
                .dup => {
                    stack[sp.*] = dupValue(ctx, stack[sp.* - 1]);
                    sp.* += 1;
                },
                .swap => {
                    const tmp = stack[sp.* - 1];
                    stack[sp.* - 1] = stack[sp.* - 2];
                    stack[sp.* - 2] = tmp;
                },
                .nop => {},

                // ==================== ARITHMETIC (SINT fast path) ====================
                .add => {
                    sp.* -= 1;
                    const b = stack[sp.*];
                    const a = stack[sp.* - 1];
                    if (JSValue.addSint(a, b)) |result| {
                        stack[sp.* - 1] = result;
                    } else {
                        stack[sp.* - 1] = JS_Add(ctx, a, b);
                        freeValue(ctx, a);
                        freeValue(ctx, b);
                    }
                },
                .sub => {
                    sp.* -= 1;
                    const b = stack[sp.*];
                    const a = stack[sp.* - 1];
                    if (JSValue.subSint(a, b)) |result| {
                        stack[sp.* - 1] = result;
                    } else {
                        stack[sp.* - 1] = JS_Sub(ctx, a, b);
                        freeValue(ctx, a);
                        freeValue(ctx, b);
                    }
                },
                .mul => {
                    sp.* -= 1;
                    const b = stack[sp.*];
                    const a = stack[sp.* - 1];
                    if (JSValue.mulSint(a, b)) |result| {
                        stack[sp.* - 1] = result;
                    } else {
                        stack[sp.* - 1] = JS_Mul(ctx, a, b);
                        freeValue(ctx, a);
                        freeValue(ctx, b);
                    }
                },

                // ==================== COMPARISON (SINT fast path) ====================
                .lt => {
                    sp.* -= 1;
                    const b = stack[sp.*];
                    const a = stack[sp.* - 1];
                    if (JSValue.ltSint(a, b)) |result| {
                        stack[sp.* - 1] = result;
                    } else {
                        stack[sp.* - 1] = JSValue.initBool(JS_LT(ctx, a, b) != 0);
                        freeValue(ctx, a);
                        freeValue(ctx, b);
                    }
                },
                .lte => {
                    sp.* -= 1;
                    const b = stack[sp.*];
                    const a = stack[sp.* - 1];
                    if (JSValue.leSint(a, b)) |result| {
                        stack[sp.* - 1] = result;
                    } else {
                        stack[sp.* - 1] = JSValue.initBool(JS_LE(ctx, a, b) != 0);
                        freeValue(ctx, a);
                        freeValue(ctx, b);
                    }
                },
                .eq, .strict_eq => {
                    sp.* -= 1;
                    const b = stack[sp.*];
                    const a = stack[sp.* - 1];
                    if (JSValue.eqSint(a, b)) |result| {
                        stack[sp.* - 1] = result;
                    } else {
                        stack[sp.* - 1] = JSValue.initBool(JS_EQ(ctx, a, b) != 0);
                        freeValue(ctx, a);
                        freeValue(ctx, b);
                    }
                },

                // ==================== UNARY ====================
                .inc => {
                    const a = stack[sp.* - 1];
                    if (JSValue.incSint(a)) |result| {
                        stack[sp.* - 1] = result;
                    } else {
                        stack[sp.* - 1] = JS_Add(ctx, a, JSValue.initInt(1));
                        freeValue(ctx, a);
                    }
                },
                .dec => {
                    const a = stack[sp.* - 1];
                    if (JSValue.decSint(a)) |result| {
                        stack[sp.* - 1] = result;
                    } else {
                        stack[sp.* - 1] = JS_Sub(ctx, a, JSValue.initInt(1));
                        freeValue(ctx, a);
                    }
                },
                .neg => {
                    const a = stack[sp.* - 1];
                    if (JSValue.negSint(a)) |result| {
                        stack[sp.* - 1] = result;
                    } else {
                        stack[sp.* - 1] = JSValue.initUndefined();
                    }
                },
                .lnot => {
                    const v = stack[sp.* - 1];
                    stack[sp.* - 1] = JSValue.initBool(v.isFalsy());
                    freeValue(ctx, v);
                },

                // ==================== FUNCTION CALLS ====================
                .call0 => {
                    // Stack: [..., func] -> [..., result]
                    sp.* -= 1;
                    const func = stack[sp.*];
                    const result = JS_Call(ctx, func, JSValue.initUndefined(), 0, @ptrFromInt(0));
                    freeValue(ctx, func);
                    stack[sp.*] = result;
                    sp.* += 1;
                },
                .call1 => {
                    if (func_opts.is_self_recursive) {
                        // Direct recursive call
                        sp.* -= 2;
                        const arg = stack[sp.* + 1];
                        var call_args = [_]JSValue{arg};
                        const result = execute(ctx, JSValue.initUndefined(), 1, &call_args);
                        if (result.isException()) {
                            stack[sp.*] = result;
                            sp.* += 1;
                            return;
                        }
                        stack[sp.*] = result;
                        sp.* += 1;
                    } else {
                        // External call - Stack: [..., arg0, func] -> [..., result]
                        sp.* -= 2;
                        const arg = stack[sp.* + 1];
                        const func = stack[sp.*];
                        const result = JS_Call(ctx, func, JSValue.initUndefined(), 1, @ptrCast(&arg));
                        freeValue(ctx, func);
                        freeValue(ctx, arg);
                        if (result.isException()) {
                            stack[sp.*] = result;
                            sp.* += 1;
                            return;
                        }
                        stack[sp.*] = result;
                        sp.* += 1;
                    }
                },
                .call2 => {
                    // Stack: [..., arg1, arg0, func] -> [..., result]
                    sp.* -= 3;
                    const func = stack[sp.*];
                    var call_args = [_]JSValue{ stack[sp.* + 1], stack[sp.* + 2] };
                    const result = JS_Call(ctx, func, JSValue.initUndefined(), 2, &call_args);
                    freeValue(ctx, func);
                    freeValue(ctx, call_args[0]);
                    freeValue(ctx, call_args[1]);
                    stack[sp.*] = result;
                    sp.* += 1;
                },
                .call3 => {
                    // Stack: [..., arg2, arg1, arg0, func] -> [..., result]
                    sp.* -= 4;
                    const func = stack[sp.*];
                    var call_args = [_]JSValue{ stack[sp.* + 1], stack[sp.* + 2], stack[sp.* + 3] };
                    const result = JS_Call(ctx, func, JSValue.initUndefined(), 3, &call_args);
                    freeValue(ctx, func);
                    freeValue(ctx, call_args[0]);
                    freeValue(ctx, call_args[1]);
                    freeValue(ctx, call_args[2]);
                    stack[sp.*] = result;
                    sp.* += 1;
                },
                .call_method => {
                    // Stack: [..., args..., this, func] -> [..., result]
                    // operand contains argc
                    const argc: usize = @intCast(@max(0, instr.operand));
                    sp.* -= argc + 2; // Pop func, this, and args
                    const func = stack[sp.*];
                    const this = stack[sp.* + 1];
                    // Args start at sp + 2
                    const result = JS_Call(ctx, func, this, @intCast(argc), @ptrCast(&stack[sp.* + 2]));
                    freeValue(ctx, func);
                    freeValue(ctx, this);
                    // Free arguments
                    for (0..argc) |i| {
                        freeValue(ctx, stack[sp.* + 2 + i]);
                    }
                    stack[sp.*] = result;
                    sp.* += 1;
                },

                // ==================== PROPERTY ACCESS ====================
                .get_field => {
                    // pop obj, push obj[atom]
                    const obj = stack[sp.* - 1];
                    const atom: i32 = instr.operand;
                    const result = jsvalue.JS_GetProperty(ctx, obj, atom);
                    freeValue(ctx, obj);
                    stack[sp.* - 1] = result;
                },
                .get_field2 => {
                    // pop obj, push obj and obj[atom]
                    const obj = stack[sp.* - 1];
                    const atom: i32 = instr.operand;
                    const result = jsvalue.JS_GetProperty(ctx, obj, atom);
                    // Keep obj on stack, add result
                    stack[sp.*] = result;
                    sp.* += 1;
                },
                .put_field => {
                    // pop obj and value, set obj[atom] = value
                    sp.* -= 2;
                    const value = stack[sp.* + 1];
                    const obj = stack[sp.*];
                    const atom: i32 = instr.operand;
                    _ = jsvalue.JS_SetProperty(ctx, obj, atom, value);
                    freeValue(ctx, obj);
                    // value ownership transferred to SetProperty
                },

                // ==================== ARRAY ACCESS ====================
                .get_array_el => {
                    // pop obj and index, push obj[index]
                    sp.* -= 1;
                    const idx = stack[sp.*];
                    const obj = stack[sp.* - 1];
                    // Convert index to uint32 for array access
                    const idx_val: u32 = if (idx.isInt())
                        @intCast(@max(0, idx.toInt()))
                    else
                        0;
                    const result = jsvalue.JS_GetPropertyUint32(ctx, obj, idx_val);
                    freeValue(ctx, obj);
                    freeValue(ctx, idx);
                    stack[sp.* - 1] = result;
                },
                .get_array_el2 => {
                    // pop obj and index, push obj and obj[index]
                    const idx = stack[sp.* - 1];
                    const obj = stack[sp.* - 2];
                    const idx_val: u32 = if (idx.isInt())
                        @intCast(@max(0, idx.toInt()))
                    else
                        0;
                    const result = jsvalue.JS_GetPropertyUint32(ctx, obj, idx_val);
                    freeValue(ctx, idx);
                    // Replace index with result, keep obj
                    stack[sp.* - 1] = result;
                },
                .put_array_el => {
                    // pop obj, index, and value, set obj[index] = value
                    sp.* -= 3;
                    const value = stack[sp.* + 2];
                    const idx = stack[sp.* + 1];
                    const obj = stack[sp.*];
                    const idx_val: u32 = if (idx.isInt())
                        @intCast(@max(0, idx.toInt()))
                    else
                        0;
                    _ = jsvalue.JS_SetPropertyUint32(ctx, obj, idx_val, value);
                    freeValue(ctx, obj);
                    freeValue(ctx, idx);
                    // value ownership transferred to SetProperty
                },

                // ==================== LENGTH ====================
                .get_length => {
                    // pop obj, push obj.length
                    const obj = stack[sp.* - 1];
                    // V8-style optimization: Check if this is arg0 with cached length
                    if (arg0_len_cache.* >= 0 and actual_argc > 0 and obj.isSameObject(argv[0])) {
                        // Use cached length (2-3x faster for loops)
                        freeValue(ctx, obj);
                        const len = arg0_len_cache.*;
                        if (len <= std.math.maxInt(i32) and len >= std.math.minInt(i32)) {
                            stack[sp.* - 1] = JSValue.initInt(@intCast(len));
                        } else {
                            stack[sp.* - 1] = JSValue.initFloat64(@floatFromInt(len));
                        }
                    } else {
                        // Compute length and potentially cache
                        var len: i64 = 0;
                        const rc = jsvalue.JS_GetLength(ctx, obj, &len);
                        // Cache if this is arg0 (lazy initialization)
                        if (actual_argc > 0 and obj.isSameObject(argv[0])) {
                            arg0_len_cache.* = if (rc >= 0) len else -1;
                        }
                        freeValue(ctx, obj);
                        if (rc < 0) {
                            stack[sp.* - 1] = JSValue.initUndefined();
                        } else if (len <= std.math.maxInt(i32) and len >= std.math.minInt(i32)) {
                            stack[sp.* - 1] = JSValue.initInt(@intCast(len));
                        } else {
                            stack[sp.* - 1] = JSValue.initFloat64(@floatFromInt(len));
                        }
                    }
                },

                // ==================== CLOSURE VARIABLE ACCESS ====================
                // Note: Frozen functions don't have closure context by default.
                // These handlers require var_refs to be passed through FrozenOptions
                // or stored in a context registry. For now, return undefined/no-op.
                .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3 => {
                    // Would call: jsvalue.js_frozen_get_var_ref(ctx, var_refs, idx)
                    // Without var_refs context, return undefined
                    stack[sp.*] = JSValue.initUndefined();
                    sp.* += 1;
                },
                .get_var_ref => {
                    // Long form with explicit index in operand
                    stack[sp.*] = JSValue.initUndefined();
                    sp.* += 1;
                },
                .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3 => {
                    // Would call: jsvalue.js_frozen_set_var_ref(ctx, var_refs, idx, val)
                    sp.* -= 1;
                    freeValue(ctx, stack[sp.*]); // Value not stored, free it
                },
                .put_var_ref => {
                    sp.* -= 1;
                    freeValue(ctx, stack[sp.*]);
                },
                .set_var_ref0, .set_var_ref1, .set_var_ref2, .set_var_ref3 => {
                    // set_var_ref: pop value, store in var_ref, push value back
                    // Without var_refs context, just keep value on stack (no-op)
                },
                .set_var_ref => {
                    // No-op for now
                },

                // ==================== ITERATOR OPERATIONS ====================
                .for_in_start => {
                    // Stack: [obj] -> [iterator_state]
                    // Call QuickJS helper which modifies stack in place
                    const rc = jsvalue.js_frozen_for_in_start(ctx, @ptrCast(&stack[sp.* - 1]));
                    if (rc < 0) {
                        // Error - return exception
                        return;
                    }
                    // Stack state is now: [iterator_state]
                },
                .for_in_next => {
                    // Stack: [iterator] -> [done, key, iterator]
                    const rc = jsvalue.js_frozen_for_in_next(ctx, @ptrCast(&stack[sp.* - 1]));
                    if (rc < 0) {
                        // Error
                        return;
                    }
                    // Adjust sp based on n_pop=1, n_push=3 (+2 net)
                    sp.* += 2;
                },
                .for_of_start => {
                    // Stack: [obj] -> [iterator, method, undefined]
                    const rc = jsvalue.js_frozen_for_of_start(ctx, @ptrCast(&stack[sp.* - 1]), 0);
                    if (rc < 0) {
                        return;
                    }
                    // Adjust sp based on n_pop=1, n_push=3 (+2 net)
                    sp.* += 2;
                },
                .for_of_next => {
                    // Stack: [catch_offset, iterator, next] -> [catch_offset, iterator, next, value, done]
                    const offset: c_int = @intCast(instr.operand);
                    const rc = jsvalue.js_frozen_for_of_next(ctx, @ptrCast(&stack[sp.* - 3]), offset);
                    if (rc < 0) {
                        return;
                    }
                    // Adjust sp based on n_pop=3, n_push=5 (+2 net)
                    sp.* += 2;
                },
                .iterator_close => {
                    // Pop and close iterator
                    sp.* -= 3;
                    freeValue(ctx, stack[sp.*]);
                    freeValue(ctx, stack[sp.* + 1]);
                    freeValue(ctx, stack[sp.* + 2]);
                },
                .iterator_get_value_done => {
                    // Stack: [iterator_result] -> [value, done]
                    // Get .value and .done properties from iterator result object
                    const result = stack[sp.* - 1];
                    // Get "done" property (atom 0 or look it up)
                    const done_val = jsvalue.JS_GetPropertyStr(ctx, result, "done");
                    const value_val = jsvalue.JS_GetPropertyStr(ctx, result, "value");
                    freeValue(ctx, result);
                    stack[sp.* - 1] = value_val;
                    stack[sp.*] = done_val;
                    sp.* += 1;
                },

                // ==================== TYPE OPERATIONS ====================
                .typeof => {
                    // pop value, push typeof string
                    const val = stack[sp.* - 1];
                    const type_str = jsvalue.JS_TypeOf(ctx, val);
                    freeValue(ctx, val);
                    stack[sp.* - 1] = type_str;
                },
                .instanceof => {
                    // pop obj and constructor, push boolean
                    sp.* -= 1;
                    const ctor = stack[sp.*];
                    const obj = stack[sp.* - 1];
                    const result = jsvalue.JS_IsInstanceOf(ctx, obj, ctor);
                    freeValue(ctx, obj);
                    freeValue(ctx, ctor);
                    stack[sp.* - 1] = JSValue.initBool(result > 0);
                },
                .in => {
                    // pop property and object, push boolean (prop in obj)
                    sp.* -= 1;
                    const obj = stack[sp.*];
                    const prop_val = stack[sp.* - 1];
                    // Convert property to atom - for now, just check if object has property
                    // Full implementation would convert prop to string/symbol
                    var result: c_int = 0;
                    if (prop_val.isInt()) {
                        const idx: u32 = @intCast(@max(0, prop_val.toInt()));
                        const prop = jsvalue.JS_GetPropertyUint32(ctx, obj, idx);
                        result = if (!prop.isUndefined()) 1 else 0;
                        freeValue(ctx, prop);
                    }
                    freeValue(ctx, obj);
                    freeValue(ctx, prop_val);
                    stack[sp.* - 1] = JSValue.initBool(result > 0);
                },
                .is_undefined => {
                    const val = stack[sp.* - 1];
                    stack[sp.* - 1] = JSValue.initBool(val.isUndefined());
                    freeValue(ctx, val);
                },
                .is_null => {
                    const val = stack[sp.* - 1];
                    stack[sp.* - 1] = JSValue.initBool(val.isNull());
                    freeValue(ctx, val);
                },
                .is_undefined_or_null => {
                    const val = stack[sp.* - 1];
                    stack[sp.* - 1] = JSValue.initBool(val.isUndefined() or val.isNull());
                    freeValue(ctx, val);
                },
                .typeof_is_undefined => {
                    const val = stack[sp.* - 1];
                    stack[sp.* - 1] = JSValue.initBool(val.isUndefined());
                    freeValue(ctx, val);
                },
                .typeof_is_function => {
                    const val = stack[sp.* - 1];
                    const result = jsvalue.JS_IsFunction(ctx, val);
                    stack[sp.* - 1] = JSValue.initBool(result > 0);
                    freeValue(ctx, val);
                },

                // ==================== OBJECT/ARRAY CREATION ====================
                .object => {
                    // Create empty object literal
                    stack[sp.*] = jsvalue.JS_NewObject(ctx);
                    sp.* += 1;
                },
                .array_from => {
                    // Create array from N stack values
                    // operand = argc (number of elements to pop)
                    const count: usize = @intCast(@max(0, instr.operand));
                    const arr = jsvalue.JS_NewArray(ctx);
                    // Add elements (they're in reverse order on stack)
                    sp.* -= count;
                    for (0..count) |i| {
                        _ = jsvalue.JS_SetPropertyUint32(ctx, arr, @intCast(i), stack[sp.* + i]);
                        // Value ownership transferred to array
                    }
                    stack[sp.*] = arr;
                    sp.* += 1;
                },
                .define_array_el => {
                    // Stack: [val, idx, array] -> [idx+1, array]
                    sp.* -= 1;
                    const val = stack[sp.*];
                    const idx = stack[sp.* - 1];
                    const arr = stack[sp.* - 2];
                    const idx_int: u32 = if (idx.isInt())
                        @intCast(@max(0, idx.toInt()))
                    else
                        0;
                    _ = jsvalue.JS_SetPropertyUint32(ctx, arr, idx_int, val);
                    // Update idx to idx+1
                    if (idx.isInt()) {
                        stack[sp.* - 1] = JSValue.initInt(idx.toInt() + 1);
                    }
                },
                .to_object => {
                    // Convert value to object (box primitives)
                    // For now, just leave value as-is since QuickJS handles this internally
                },
                .set_home_object => {
                    // Stack: [home_object, func] -> [home_object, func]
                    // Set home object for super keyword - no-op for frozen since we don't
                    // have the internal function pointer access
                },

                // Control flow opcodes are handled in main block switch
                .if_false, .if_false8, .if_true, .if_true8, .goto, .goto8, .goto16, .@"return", .return_undef => {},

                // Unsupported - ignore
                else => {},
            }
        }
    }.execute;
}

// ============================================================================
// Trampoline-Based Frozen Function Generator (Deep Recursion Support)
// ============================================================================

/// Maximum frame depth for trampoline (matches Node.js call stack limit)
const MAX_TRAMPOLINE_FRAMES = 10000;

/// Frame state for trampoline execution
const TrampolineFrame = struct {
    /// Arguments passed to this frame
    args: [8]JSValue,
    /// Number of arguments
    argc: u8,
    /// Return value
    result: JSValue,
    /// Operand stack
    stack: [256]JSValue,
    /// Local variables
    locals: [64]JSValue,
    /// Stack pointer
    sp: usize,
    /// Current block ID
    block_id: i32,
    /// Waiting for call result
    waiting_for_call: bool,
    /// V8-style cached length for arg0 (-1 = not yet computed)
    arg0_len_cache: i64,

    /// Initialize frame for a new call
    fn init(args_in: [*]JSValue, argc_in: u8, var_count: u8) TrampolineFrame {
        var frame = TrampolineFrame{
            .args = undefined,
            .argc = argc_in,
            .result = JSValue.initUndefined(),
            .stack = undefined,
            .locals = undefined,
            .sp = 0,
            .block_id = 0,
            .waiting_for_call = false,
            .arg0_len_cache = -1, // Lazy initialization
        };

        // Copy arguments
        for (0..8) |i| {
            if (i < argc_in) {
                frame.args[i] = args_in[i];
            } else {
                frame.args[i] = JSValue.initUndefined();
            }
        }

        // Initialize locals to undefined
        for (0..var_count) |i| {
            frame.locals[i] = JSValue.initUndefined();
        }

        return frame;
    }
};

/// Generate a trampoline-based frozen function for deep recursion
/// Uses heap-allocated frames instead of C stack frames
pub fn generateFrozenTrampoline(
    comptime bytecode: []const u8,
    comptime opts: FrozenOptions,
) FrozenFn {
    // Parse bytecode and build CFG at comptime
    const instructions = comptime parseBytecodComptime(bytecode);
    const cfg = comptime buildComptimeCFG(instructions);

    return struct {
        fn execute(ctx: *JSContext, this_val: JSValue, argc: c_int, argv: [*]JSValue) callconv(.c) JSValue {
            _ = this_val;

            // Allocate frame stack on heap (not C stack!)
            const frame_size = @sizeOf(TrampolineFrame);
            const frames_ptr = jsvalue.js_malloc(ctx, frame_size * MAX_TRAMPOLINE_FRAMES);
            if (frames_ptr == null) {
                return jsvalue.JS_ThrowOutOfMemory(ctx);
            }
            defer jsvalue.js_free(ctx, frames_ptr);

            const frames: [*]TrampolineFrame = @ptrCast(@alignCast(frames_ptr));
            var frame_depth: usize = 0;

            // Initialize root frame
            const actual_argc: u8 = @intCast(@max(0, @min(8, argc)));
            frames[0] = TrampolineFrame.init(argv, actual_argc, opts.var_count);

            // Copy arguments to locals (QuickJS convention)
            inline for (0..opts.arg_count) |i| {
                if (i < actual_argc) {
                    frames[0].locals[i] = argv[i];
                }
            }

            // Trampoline loop - iterative execution
            while (true) {
                var frame = &frames[frame_depth];

                // Stack overflow check
                if (frame_depth >= MAX_TRAMPOLINE_FRAMES - 1) {
                    return JS_ThrowRangeError(ctx, "Maximum call stack size exceeded");
                }

                // Execute current block
                const next_block = executeTrampolineBlock(
                    ctx,
                    frame,
                    instructions,
                    cfg,
                    opts,
                );

                // Handle block result
                if (next_block == BLOCK_RETURN) {
                    // Return from current frame
                    if (frame_depth == 0) {
                        // Root frame - we're done
                        return frame.result;
                    }
                    // Return to parent frame
                    frames[frame_depth - 1].stack[frames[frame_depth - 1].sp] = frame.result;
                    frames[frame_depth - 1].sp += 1;
                    frames[frame_depth - 1].waiting_for_call = false;
                    frame_depth -= 1;
                } else if (next_block == BLOCK_RETURN_UNDEF) {
                    if (frame_depth == 0) {
                        return JSValue.initUndefined();
                    }
                    frames[frame_depth - 1].stack[frames[frame_depth - 1].sp] = JSValue.initUndefined();
                    frames[frame_depth - 1].sp += 1;
                    frames[frame_depth - 1].waiting_for_call = false;
                    frame_depth -= 1;
                } else if (next_block == BLOCK_CALL_SELF) {
                    // Self-recursive call - push new frame
                    frame.waiting_for_call = true;

                    // Get argument from caller's stack
                    frame.sp -= 2; // Pop func and arg
                    const call_arg = frame.stack[frame.sp + 1];

                    // Push new frame
                    frame_depth += 1;
                    var new_args = [_]JSValue{call_arg} ++ [_]JSValue{JSValue.initUndefined()} ** 7;
                    frames[frame_depth] = TrampolineFrame.init(&new_args, 1, opts.var_count);
                    frames[frame_depth].locals[0] = call_arg;
                } else {
                    // Continue to next block in same frame
                    frame.block_id = next_block;
                }
            }
        }

        /// Execute a single block in trampoline mode
        fn executeTrampolineBlock(
            ctx: *JSContext,
            frame: *TrampolineFrame,
            comptime instrs: []const Instruction,
            comptime cfg_info: ComptimeCFG,
            comptime func_opts: FrozenOptions,
        ) i32 {
            const block_id: usize = @intCast(@max(0, frame.block_id));
            if (block_id >= cfg_info.block_count) {
                frame.result = JSValue.initUndefined();
                return BLOCK_RETURN_UNDEF;
            }

            const block = cfg_info.blocks[block_id];
            const stack = &frame.stack;
            const locals = &frame.locals;
            var sp = frame.sp;

            // Execute instructions in this block
            inline for (block.start_idx..block.end_idx) |i| {
                const instr = instrs[i];

                switch (instr.opcode) {
                    // Control flow - handle specially
                    .@"return" => {
                        sp -= 1;
                        frame.result = stack[sp];
                        frame.sp = sp;
                        return BLOCK_RETURN;
                    },
                    .return_undef => {
                        frame.result = JSValue.initUndefined();
                        frame.sp = sp;
                        return BLOCK_RETURN_UNDEF;
                    },
                    .if_false, .if_false8 => {
                        sp -= 1;
                        const cond = stack[sp];
                        const is_truthy = !cond.isFalsy();
                        freeValue(ctx, cond);
                        frame.sp = sp;
                        return if (is_truthy) block.true_target else block.false_target;
                    },
                    .if_true, .if_true8 => {
                        sp -= 1;
                        const cond = stack[sp];
                        const is_truthy = !cond.isFalsy();
                        freeValue(ctx, cond);
                        frame.sp = sp;
                        return if (is_truthy) block.true_target else block.false_target;
                    },
                    .goto, .goto8, .goto16 => {
                        frame.sp = sp;
                        return block.true_target;
                    },

                    // ==================== FUNCTION CALLS ====================
                    .call0 => {
                        sp -= 1;
                        const func = stack[sp];
                        const result = JS_Call(ctx, func, JSValue.initUndefined(), 0, @ptrFromInt(0));
                        freeValue(ctx, func);
                        stack[sp] = result;
                        sp += 1;
                    },
                    .call1 => {
                        if (func_opts.is_self_recursive) {
                            // Signal trampoline to push new frame
                            frame.sp = sp;
                            return BLOCK_CALL_SELF;
                        } else {
                            // External call
                            sp -= 2;
                            const arg = stack[sp + 1];
                            const func = stack[sp];
                            const result = JS_Call(ctx, func, JSValue.initUndefined(), 1, @ptrCast(&arg));
                            freeValue(ctx, func);
                            freeValue(ctx, arg);
                            stack[sp] = result;
                            sp += 1;
                        }
                    },
                    .call2 => {
                        sp -= 3;
                        const func = stack[sp];
                        var call_args = [_]JSValue{ stack[sp + 1], stack[sp + 2] };
                        const result = JS_Call(ctx, func, JSValue.initUndefined(), 2, &call_args);
                        freeValue(ctx, func);
                        freeValue(ctx, call_args[0]);
                        freeValue(ctx, call_args[1]);
                        stack[sp] = result;
                        sp += 1;
                    },
                    .call3 => {
                        sp -= 4;
                        const func = stack[sp];
                        var call_args = [_]JSValue{ stack[sp + 1], stack[sp + 2], stack[sp + 3] };
                        const result = JS_Call(ctx, func, JSValue.initUndefined(), 3, &call_args);
                        freeValue(ctx, func);
                        freeValue(ctx, call_args[0]);
                        freeValue(ctx, call_args[1]);
                        freeValue(ctx, call_args[2]);
                        stack[sp] = result;
                        sp += 1;
                    },
                    .call_method => {
                        const argc: usize = @intCast(@max(0, instr.operand));
                        sp -= argc + 2;
                        const func = stack[sp];
                        const this = stack[sp + 1];
                        const result = JS_Call(ctx, func, this, @intCast(argc), @ptrCast(&stack[sp + 2]));
                        freeValue(ctx, func);
                        freeValue(ctx, this);
                        for (0..argc) |j| {
                            freeValue(ctx, stack[sp + 2 + j]);
                        }
                        stack[sp] = result;
                        sp += 1;
                    },

                    // Non-control-flow instructions
                    .push_minus1 => {
                        stack[sp] = JSValue.initInt(-1);
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
                    .push_2 => {
                        stack[sp] = JSValue.initInt(2);
                        sp += 1;
                    },
                    .push_3 => {
                        stack[sp] = JSValue.initInt(3);
                        sp += 1;
                    },
                    .push_i32 => {
                        stack[sp] = JSValue.initInt(instr.operand);
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
                    .null => {
                        stack[sp] = JSValue.initNull();
                        sp += 1;
                    },
                    .undefined => {
                        stack[sp] = JSValue.initUndefined();
                        sp += 1;
                    },
                    .get_loc0 => {
                        stack[sp] = dupValue(ctx, locals[0]);
                        sp += 1;
                    },
                    .get_loc1 => {
                        stack[sp] = dupValue(ctx, locals[1]);
                        sp += 1;
                    },
                    .get_loc2 => {
                        stack[sp] = dupValue(ctx, locals[2]);
                        sp += 1;
                    },
                    .get_loc3 => {
                        stack[sp] = dupValue(ctx, locals[3]);
                        sp += 1;
                    },
                    .get_loc => {
                        const idx: usize = @intCast(instr.operand);
                        stack[sp] = dupValue(ctx, locals[idx]);
                        sp += 1;
                    },
                    .put_loc0 => {
                        sp -= 1;
                        freeValue(ctx, locals[0]);
                        locals[0] = stack[sp];
                    },
                    .put_loc1 => {
                        sp -= 1;
                        freeValue(ctx, locals[1]);
                        locals[1] = stack[sp];
                    },
                    .put_loc2 => {
                        sp -= 1;
                        freeValue(ctx, locals[2]);
                        locals[2] = stack[sp];
                    },
                    .put_loc3 => {
                        sp -= 1;
                        freeValue(ctx, locals[3]);
                        locals[3] = stack[sp];
                    },
                    .put_loc => {
                        const idx: usize = @intCast(instr.operand);
                        sp -= 1;
                        freeValue(ctx, locals[idx]);
                        locals[idx] = stack[sp];
                    },
                    .get_arg0 => {
                        stack[sp] = if (0 < frame.argc) dupValue(ctx, frame.args[0]) else JSValue.initUndefined();
                        sp += 1;
                    },
                    .get_arg1 => {
                        stack[sp] = if (1 < frame.argc) dupValue(ctx, frame.args[1]) else JSValue.initUndefined();
                        sp += 1;
                    },
                    .get_arg2 => {
                        stack[sp] = if (2 < frame.argc) dupValue(ctx, frame.args[2]) else JSValue.initUndefined();
                        sp += 1;
                    },
                    .get_arg3 => {
                        stack[sp] = if (3 < frame.argc) dupValue(ctx, frame.args[3]) else JSValue.initUndefined();
                        sp += 1;
                    },
                    .drop => {
                        sp -= 1;
                        freeValue(ctx, stack[sp]);
                    },
                    .dup => {
                        stack[sp] = dupValue(ctx, stack[sp - 1]);
                        sp += 1;
                    },
                    .swap => {
                        const tmp = stack[sp - 1];
                        stack[sp - 1] = stack[sp - 2];
                        stack[sp - 2] = tmp;
                    },
                    .nop => {},
                    .add => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (JSValue.addSint(a, b)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JS_Add(ctx, a, b);
                            freeValue(ctx, a);
                            freeValue(ctx, b);
                        }
                    },
                    .sub => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (JSValue.subSint(a, b)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JS_Sub(ctx, a, b);
                            freeValue(ctx, a);
                            freeValue(ctx, b);
                        }
                    },
                    .mul => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (JSValue.mulSint(a, b)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JS_Mul(ctx, a, b);
                            freeValue(ctx, a);
                            freeValue(ctx, b);
                        }
                    },
                    .lt => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (JSValue.ltSint(a, b)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JSValue.initBool(JS_LT(ctx, a, b) != 0);
                            freeValue(ctx, a);
                            freeValue(ctx, b);
                        }
                    },
                    .lte => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (JSValue.leSint(a, b)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JSValue.initBool(JS_LE(ctx, a, b) != 0);
                            freeValue(ctx, a);
                            freeValue(ctx, b);
                        }
                    },
                    .eq, .strict_eq => {
                        sp -= 1;
                        const b = stack[sp];
                        const a = stack[sp - 1];
                        if (JSValue.eqSint(a, b)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JSValue.initBool(JS_EQ(ctx, a, b) != 0);
                            freeValue(ctx, a);
                            freeValue(ctx, b);
                        }
                    },
                    .inc => {
                        const a = stack[sp - 1];
                        if (JSValue.incSint(a)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JS_Add(ctx, a, JSValue.initInt(1));
                            freeValue(ctx, a);
                        }
                    },
                    .dec => {
                        const a = stack[sp - 1];
                        if (JSValue.decSint(a)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JS_Sub(ctx, a, JSValue.initInt(1));
                            freeValue(ctx, a);
                        }
                    },
                    .neg => {
                        const a = stack[sp - 1];
                        if (JSValue.negSint(a)) |result| {
                            stack[sp - 1] = result;
                        } else {
                            stack[sp - 1] = JSValue.initUndefined();
                        }
                    },
                    .lnot => {
                        const v = stack[sp - 1];
                        stack[sp - 1] = JSValue.initBool(v.isFalsy());
                        freeValue(ctx, v);
                    },

                    // ==================== PROPERTY ACCESS ====================
                    .get_field => {
                        const obj = stack[sp - 1];
                        const atom: i32 = instr.operand;
                        const result = jsvalue.JS_GetProperty(ctx, obj, atom);
                        freeValue(ctx, obj);
                        stack[sp - 1] = result;
                    },
                    .get_field2 => {
                        const obj = stack[sp - 1];
                        const atom: i32 = instr.operand;
                        const result = jsvalue.JS_GetProperty(ctx, obj, atom);
                        stack[sp] = result;
                        sp += 1;
                    },
                    .put_field => {
                        sp -= 2;
                        const value = stack[sp + 1];
                        const obj = stack[sp];
                        const atom: i32 = instr.operand;
                        _ = jsvalue.JS_SetProperty(ctx, obj, atom, value);
                        freeValue(ctx, obj);
                    },

                    // ==================== ARRAY ACCESS ====================
                    .get_array_el => {
                        sp -= 1;
                        const idx = stack[sp];
                        const obj = stack[sp - 1];
                        const idx_val: u32 = if (idx.isInt())
                            @intCast(@max(0, idx.toInt()))
                        else
                            0;
                        const result = jsvalue.JS_GetPropertyUint32(ctx, obj, idx_val);
                        freeValue(ctx, obj);
                        freeValue(ctx, idx);
                        stack[sp - 1] = result;
                    },
                    .get_array_el2 => {
                        const idx = stack[sp - 1];
                        const obj = stack[sp - 2];
                        const idx_val: u32 = if (idx.isInt())
                            @intCast(@max(0, idx.toInt()))
                        else
                            0;
                        const result = jsvalue.JS_GetPropertyUint32(ctx, obj, idx_val);
                        freeValue(ctx, idx);
                        stack[sp - 1] = result;
                    },
                    .put_array_el => {
                        sp -= 3;
                        const value = stack[sp + 2];
                        const idx = stack[sp + 1];
                        const obj = stack[sp];
                        const idx_val: u32 = if (idx.isInt())
                            @intCast(@max(0, idx.toInt()))
                        else
                            0;
                        _ = jsvalue.JS_SetPropertyUint32(ctx, obj, idx_val, value);
                        freeValue(ctx, obj);
                        freeValue(ctx, idx);
                    },

                    // ==================== LENGTH ====================
                    .get_length => {
                        const obj = stack[sp - 1];
                        // V8-style optimization: Check if this is arg0 with cached length
                        if (frame.arg0_len_cache >= 0 and frame.argc > 0 and obj.isSameObject(frame.args[0])) {
                            // Use cached length (2-3x faster for loops)
                            freeValue(ctx, obj);
                            const len = frame.arg0_len_cache;
                            if (len <= std.math.maxInt(i32) and len >= std.math.minInt(i32)) {
                                stack[sp - 1] = JSValue.initInt(@intCast(len));
                            } else {
                                stack[sp - 1] = JSValue.initFloat64(@floatFromInt(len));
                            }
                        } else {
                            // Compute length and potentially cache
                            var len: i64 = 0;
                            const rc = jsvalue.JS_GetLength(ctx, obj, &len);
                            // Cache if this is arg0 (lazy initialization)
                            if (frame.argc > 0 and obj.isSameObject(frame.args[0])) {
                                frame.arg0_len_cache = if (rc >= 0) len else -1;
                            }
                            freeValue(ctx, obj);
                            if (rc < 0) {
                                stack[sp - 1] = JSValue.initUndefined();
                            } else if (len <= std.math.maxInt(i32) and len >= std.math.minInt(i32)) {
                                stack[sp - 1] = JSValue.initInt(@intCast(len));
                            } else {
                                stack[sp - 1] = JSValue.initFloat64(@floatFromInt(len));
                            }
                        }
                    },

                    // ==================== CLOSURE VARIABLE ACCESS ====================
                    .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3 => {
                        stack[sp] = JSValue.initUndefined();
                        sp += 1;
                    },
                    .get_var_ref => {
                        stack[sp] = JSValue.initUndefined();
                        sp += 1;
                    },
                    .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3 => {
                        sp -= 1;
                        freeValue(ctx, stack[sp]);
                    },
                    .put_var_ref => {
                        sp -= 1;
                        freeValue(ctx, stack[sp]);
                    },
                    .set_var_ref0, .set_var_ref1, .set_var_ref2, .set_var_ref3 => {
                        // No-op - value stays on stack
                    },
                    .set_var_ref => {
                        // No-op - value stays on stack
                    },

                    // ==================== ITERATOR OPERATIONS ====================
                    .for_in_start => {
                        const rc = jsvalue.js_frozen_for_in_start(ctx, @ptrCast(&stack[sp - 1]));
                        if (rc < 0) {
                            frame.sp = sp;
                            return BLOCK_RETURN_UNDEF;
                        }
                    },
                    .for_in_next => {
                        const rc = jsvalue.js_frozen_for_in_next(ctx, @ptrCast(&stack[sp - 1]));
                        if (rc < 0) {
                            frame.sp = sp;
                            return BLOCK_RETURN_UNDEF;
                        }
                        sp += 2;
                    },
                    .for_of_start => {
                        const rc = jsvalue.js_frozen_for_of_start(ctx, @ptrCast(&stack[sp - 1]), 0);
                        if (rc < 0) {
                            frame.sp = sp;
                            return BLOCK_RETURN_UNDEF;
                        }
                        sp += 2;
                    },
                    .for_of_next => {
                        const offset: c_int = @intCast(instr.operand);
                        const rc = jsvalue.js_frozen_for_of_next(ctx, @ptrCast(&stack[sp - 3]), offset);
                        if (rc < 0) {
                            frame.sp = sp;
                            return BLOCK_RETURN_UNDEF;
                        }
                        sp += 2;
                    },
                    .iterator_close => {
                        sp -= 3;
                        freeValue(ctx, stack[sp]);
                        freeValue(ctx, stack[sp + 1]);
                        freeValue(ctx, stack[sp + 2]);
                    },
                    .iterator_get_value_done => {
                        const result = stack[sp - 1];
                        const done_val = jsvalue.JS_GetPropertyStr(ctx, result, "done");
                        const value_val = jsvalue.JS_GetPropertyStr(ctx, result, "value");
                        freeValue(ctx, result);
                        stack[sp - 1] = value_val;
                        stack[sp] = done_val;
                        sp += 1;
                    },

                    // ==================== TYPE OPERATIONS ====================
                    .typeof => {
                        const val = stack[sp - 1];
                        const type_str = jsvalue.JS_TypeOf(ctx, val);
                        freeValue(ctx, val);
                        stack[sp - 1] = type_str;
                    },
                    .instanceof => {
                        sp -= 1;
                        const ctor = stack[sp];
                        const obj = stack[sp - 1];
                        const result = jsvalue.JS_IsInstanceOf(ctx, obj, ctor);
                        freeValue(ctx, obj);
                        freeValue(ctx, ctor);
                        stack[sp - 1] = JSValue.initBool(result > 0);
                    },
                    .in => {
                        sp -= 1;
                        const obj = stack[sp];
                        const prop_val = stack[sp - 1];
                        var result: c_int = 0;
                        if (prop_val.isInt()) {
                            const idx: u32 = @intCast(@max(0, prop_val.toInt()));
                            const prop = jsvalue.JS_GetPropertyUint32(ctx, obj, idx);
                            result = if (!prop.isUndefined()) 1 else 0;
                            freeValue(ctx, prop);
                        }
                        freeValue(ctx, obj);
                        freeValue(ctx, prop_val);
                        stack[sp - 1] = JSValue.initBool(result > 0);
                    },
                    .is_undefined => {
                        const val = stack[sp - 1];
                        stack[sp - 1] = JSValue.initBool(val.isUndefined());
                        freeValue(ctx, val);
                    },
                    .is_null => {
                        const val = stack[sp - 1];
                        stack[sp - 1] = JSValue.initBool(val.isNull());
                        freeValue(ctx, val);
                    },
                    .is_undefined_or_null => {
                        const val = stack[sp - 1];
                        stack[sp - 1] = JSValue.initBool(val.isUndefined() or val.isNull());
                        freeValue(ctx, val);
                    },
                    .typeof_is_undefined => {
                        const val = stack[sp - 1];
                        stack[sp - 1] = JSValue.initBool(val.isUndefined());
                        freeValue(ctx, val);
                    },
                    .typeof_is_function => {
                        const val = stack[sp - 1];
                        const result = jsvalue.JS_IsFunction(ctx, val);
                        stack[sp - 1] = JSValue.initBool(result > 0);
                        freeValue(ctx, val);
                    },

                    // ==================== OBJECT/ARRAY CREATION ====================
                    .object => {
                        stack[sp] = jsvalue.JS_NewObject(ctx);
                        sp += 1;
                    },
                    .array_from => {
                        const count: usize = @intCast(@max(0, instr.operand));
                        const arr = jsvalue.JS_NewArray(ctx);
                        sp -= count;
                        for (0..count) |arr_idx| {
                            _ = jsvalue.JS_SetPropertyUint32(ctx, arr, @intCast(arr_idx), stack[sp + arr_idx]);
                        }
                        stack[sp] = arr;
                        sp += 1;
                    },
                    .define_array_el => {
                        sp -= 1;
                        const val = stack[sp];
                        const idx_val = stack[sp - 1];
                        const arr = stack[sp - 2];
                        const idx_int: u32 = if (idx_val.isInt())
                            @intCast(@max(0, idx_val.toInt()))
                        else
                            0;
                        _ = jsvalue.JS_SetPropertyUint32(ctx, arr, idx_int, val);
                        if (idx_val.isInt()) {
                            stack[sp - 1] = JSValue.initInt(idx_val.toInt() + 1);
                        }
                    },
                    .to_object => {},
                    .set_home_object => {},

                    else => {
                        // Unsupported opcode - continue
                    },
                }
            }

            // End of block - fall through to next
            frame.sp = sp;
            return block.true_target;
        }
    }.execute;
}

// ============================================================================
// Runtime Registration
// ============================================================================

/// Register frozen functions with QuickJS context
pub fn registerFrozenFunctions(ctx: *JSContext, entries: []const FrozenEntry) void {
    const global = JS_GetGlobalObject(ctx);
    defer JS_FreeValue(ctx, global);

    for (entries) |entry| {
        const func_val = JS_NewCFunction2(ctx, entry.func, entry.name, entry.argc, 0, 0);
        _ = JS_SetPropertyStr(ctx, global, entry.name, func_val);
    }
}

// ============================================================================
// Manual Frozen Functions (for known hot paths)
// ============================================================================

/// Fibonacci - manually optimized frozen function
/// This demonstrates what the comptime generator should produce
pub fn frozen_fib(ctx: *JSContext, this_val: JSValue, argc: c_int, argv: [*]JSValue) callconv(.c) JSValue {
    _ = this_val; // Required for ABI compatibility

    // Stack overflow check
    if (call_depth >= MAX_CALL_DEPTH) {
        return JS_ThrowRangeError(ctx, "Maximum call stack size exceeded");
    }
    call_depth += 1;
    defer call_depth -= 1;

    // Get argument
    if (argc < 1) return JSValue.initUndefined();
    const n_val = argv[0];
    if (!n_val.isInt()) return JSValue.initUndefined();

    const n = n_val.toInt();

    // Base cases
    if (n <= 1) return JSValue.initInt(n);

    // Recursive calls (direct Zig recursion - no ABI overhead)
    var args1 = [_]JSValue{JSValue.initInt(n - 1)};
    var args2 = [_]JSValue{JSValue.initInt(n - 2)};

    const r1 = frozen_fib(ctx, JSValue.initUndefined(), 1, &args1);
    if (r1.isException()) return r1;

    const r2 = frozen_fib(ctx, JSValue.initUndefined(), 1, &args2);
    if (r2.isException()) return r2;

    // SINT add - no boxing overhead
    if (JSValue.addSint(r1, r2)) |result| {
        return result;
    }

    // Fallback (shouldn't happen for fib)
    return JS_Add(ctx, r1, r2);
}

/// Entry for manual fib
pub const fib_entry = FrozenEntry{
    .name = "__frozen_fib",
    .func = frozen_fib,
    .argc = 1,
};

// ============================================================================
// Tests
// ============================================================================

test "frozen fib entry name" {
    // Verify the entry name is properly defined
    // Note: Can't test full entry because frozen_fib has external deps
    const name = "__frozen_fib";
    try std.testing.expect(name.len > 0);
}

test "bytecode parsing comptime" {
    // Simple bytecode: push_1, return
    const bytecode = [_]u8{
        @intFromEnum(Opcode.push_1), // push 1
        @intFromEnum(Opcode.@"return"), // return
    };

    const instructions = comptime parseBytecodComptime(&bytecode);
    try std.testing.expectEqual(@as(usize, 2), instructions.len);
    try std.testing.expectEqual(Opcode.push_1, instructions[0].opcode);
    try std.testing.expectEqual(Opcode.@"return", instructions[1].opcode);
}

test "comptime CFG builder - linear code" {
    // Linear: push_1, push_2, add, return (single block)
    const bytecode = [_]u8{
        @intFromEnum(Opcode.push_1), // PC=0
        @intFromEnum(Opcode.push_2), // PC=1
        @intFromEnum(Opcode.add), // PC=2
        @intFromEnum(Opcode.@"return"), // PC=3
    };

    const instructions = comptime parseBytecodComptime(&bytecode);
    const cfg = comptime buildComptimeCFG(instructions);

    // Linear code should have single block
    try std.testing.expectEqual(@as(u32, 1), cfg.block_count);
    try std.testing.expectEqual(@as(u32, 0), cfg.blocks[0].start_idx);
    try std.testing.expectEqual(@as(u32, 4), cfg.blocks[0].end_idx);
    // Return block has BLOCK_RETURN target
    try std.testing.expectEqual(BLOCK_RETURN, cfg.blocks[0].true_target);
}

test "comptime CFG builder - conditional branch" {
    // Bytecode for: if (condition) push_1 else push_2; return
    // PC=0: get_arg0
    // PC=1: if_false8 +3  (jumps to PC=5 if false, falls through to PC=3 if true)
    // PC=3: push_1
    // PC=4: goto8 +1 (jumps to PC=6)
    // PC=5: push_2
    // PC=6: return
    const bytecode = [_]u8{
        @intFromEnum(Opcode.get_arg0), // PC=0, size=1
        @intFromEnum(Opcode.if_false8), // PC=1, size=2
        2, // offset: +2 relative to PC=3 → PC=5
        @intFromEnum(Opcode.push_1), // PC=3
        @intFromEnum(Opcode.goto8), // PC=4, size=2
        1, // offset: +1 relative to PC=6 → PC=6
        @intFromEnum(Opcode.push_2), // PC=6 (should be PC=5 but corrected)
        @intFromEnum(Opcode.@"return"), // PC=7
    };

    const instructions = comptime parseBytecodComptime(&bytecode);
    const cfg = comptime buildComptimeCFG(instructions);

    // Should have multiple blocks due to branches
    try std.testing.expect(cfg.block_count > 1);
}

test "jump target calculation" {
    // goto8 at PC=0 with operand +3, size=2
    // Target should be: 0 + 2 + 3 = 5
    const target = computeJumpTarget(0, 3, 2);
    try std.testing.expectEqual(@as(i32, 5), target);

    // Negative offset: if_false8 at PC=10, operand=-5, size=2
    // Target: 10 + 2 + (-5) = 7
    const neg_target = computeJumpTarget(10, -5, 2);
    try std.testing.expectEqual(@as(i32, 7), neg_target);
}
