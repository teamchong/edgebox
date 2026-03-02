//! LLVM IR Code Generator for Frozen Functions
//!
//! Generates LLVM IR directly from QuickJS bytecode, skipping the Zig
//! parser/type-checker stage. This is the core of the LLVM IR backend.
//!
//! Architecture:
//!   bytecode → CFG → LLVM IR (in-memory) → LLVM optimize → native (.o)
//!
//! Currently supports:
//!   - Tier A: Int32 specialized functions (pure i32 ops, no JSValue boxing)
//!   - Tier B: Thin codegen (all functions, calls llvm_rt_* runtime helpers)

const std = @import("std");
const llvm = @import("llvm_ir.zig");
const opcodes = @import("opcodes.zig");
const int32_handlers = @import("int32_handlers.zig");
const parser = @import("bytecode_parser.zig");
const cfg_mod = @import("cfg_builder.zig");
const module_parser = @import("module_parser.zig");
const frozen_registry = @import("frozen_registry.zig");

const Opcode = opcodes.Opcode;
const Instruction = parser.Instruction;
const CFGBasicBlock = cfg_mod.BasicBlock;
const CFG = cfg_mod.CFG;
const Allocator = std.mem.Allocator;
const Int32Pattern = int32_handlers.Int32Pattern;
const AnalyzedFunction = frozen_registry.AnalyzedFunction;

const c = llvm.c;

pub const CodegenError = error{
    UnsupportedOpcode,
    StackUnderflow,
    OutOfMemory,
    LLVMError,
    FormatError,
};

// ============================================================================
// Shard-level LLVM IR generation
// ============================================================================

/// Result of generating a shard as an LLVM module
pub const LLVMShardResult = struct {
    /// Number of functions successfully generated
    func_count: u32,
    /// Whether any functions were generated
    has_functions: bool,
};

/// Verify, optimize (O2), and emit an LLVM module to an object file.
/// Returns true to indicate the module was consumed (caller should not dispose).
fn finalizeAndEmitShard(
    native: anytype,
    obj_path: [*:0]const u8,
    label: [*:0]const u8,
) CodegenError!bool {
    // Verify module
    native.module.verify() catch {
        const ir = native.module.printToString();
        defer llvm.disposeMessage(ir);
        std.debug.print("[{s}] Module verification failed. IR:\n{s}\n", .{ label, ir });
        return CodegenError.LLVMError;
    };

    // Run optimization passes (-O2)
    {
        const pass_opts = c.LLVMCreatePassBuilderOptions();
        defer c.LLVMDisposePassBuilderOptions(pass_opts);
        const err_ref = c.LLVMRunPasses(native.module.ref, "default<O2>", native.tm.ref, pass_opts);
        if (err_ref) |err| {
            const msg = c.LLVMGetErrorMessage(err);
            if (msg) |m| {
                std.debug.print("[{s}] Optimization failed: {s}\n", .{ label, m });
                c.LLVMDisposeErrorMessage(m);
            }
        }
    }

    // Emit to object file
    native.tm.emitToFile(native.module, obj_path) catch return CodegenError.LLVMError;
    return true; // module consumed
}

/// Generate a complete shard as an LLVM IR module and emit to object file.
/// Returns the number of functions generated.
pub fn generateShard(
    allocator: Allocator,
    functions: []const ShardFunction,
    shard_idx: usize,
    obj_path: [*:0]const u8,
) CodegenError!LLVMShardResult {
    // Create LLVM module for this shard
    var name_buf: [64]u8 = undefined;
    const mod_name = std.fmt.bufPrintZ(&name_buf, "frozen_shard_{d}", .{shard_idx}) catch
        return CodegenError.FormatError;

    const native = llvm.createNativeModule(mod_name) catch return CodegenError.LLVMError;
    // Note: ctx is the global context (not owned by us), so don't dispose it
    defer native.tm.dispose();
    // Module disposed by emitToFile or on error
    var module_disposed = false;
    defer if (!module_disposed) native.module.dispose();

    const builder = llvm.Builder.create(native.ctx);
    defer builder.dispose();

    const register_fn = declareNativeDispatchRegister(native.module);

    // Generate each function
    var generated_count: u32 = 0;
    var generated_infos = std.ArrayListUnmanaged(GeneratedFuncInfo){};
    defer generated_infos.deinit(allocator);

    for (functions) |sf| {
        if (generateInt32Function(allocator, native.module, builder, sf)) |_| {
            generated_infos.append(allocator, .{
                .name = sf.name,
                .func_index = sf.func_index,
                .line_num = sf.line_num,
                .llvm_func_name = sf.llvm_func_name,
            }) catch continue;
            generated_count += 1;
        } else |_| {
            // Function generation failed — skip it
            continue;
        }
    }

    if (generated_count == 0) {
        return .{ .func_count = 0, .has_functions = false };
    }

    generateShardInit(allocator, native.module, builder, shard_idx, generated_infos.items, register_fn) catch
        return CodegenError.LLVMError;

    module_disposed = try finalizeAndEmitShard(native, obj_path, "llvm");
    return .{ .func_count = generated_count, .has_functions = true };
}

/// Information about a function to generate in a shard
pub const ShardFunction = struct {
    /// Original JS function name (for dispatch key)
    name: []const u8,
    /// Function index in the module
    func_index: usize,
    /// Line number (for dispatch key)
    line_num: u32,
    /// Sanitized LLVM function name (e.g., "__frozen_42_isDigit")
    llvm_func_name: []const u8,
    /// The analyzed function data
    func: AnalyzedFunction,
    /// Pre-built CFG
    cfg: *const CFG,
};

const GeneratedFuncInfo = struct {
    name: []const u8,
    func_index: usize,
    line_num: u32,
    llvm_func_name: []const u8,
};

// ============================================================================
// External function declarations
// ============================================================================

fn declareNativeDispatchRegister(module: llvm.Module) llvm.Value {
    // void native_dispatch_register(const char* name, FrozenFnPtr func)
    const param_types = [_]llvm.Type{ llvm.ptrType(), llvm.ptrType() };
    const fn_ty = llvm.functionType(llvm.voidType(), &param_types, false);
    const func = module.addFunction("native_dispatch_register", fn_ty);
    llvm.setLinkage(func, c.LLVMExternalLinkage);
    return func;
}


// ============================================================================
// Int32 Function Generation (Tier A)
// ============================================================================

/// Generate a single int32 function as LLVM IR.
/// Creates both the inner _hot function (pure i32) and the JSValue wrapper.
fn generateInt32Function(
    allocator: Allocator,
    module: llvm.Module,
    builder: llvm.Builder,
    sf: ShardFunction,
) CodegenError!void {
    const func = sf.func;
    const cfg = sf.cfg;

    // Pre-validate: check block count and instruction support BEFORE adding to module
    const blocks = cfg.blocks.items;
    if (blocks.len == 0 or blocks.len > 256) return CodegenError.UnsupportedOpcode;

    // Pre-validate: check all instructions are supported
    for (blocks) |block| {
        for (block.instructions) |instr| {
            const handler = int32_handlers.getInt32Handler(instr.opcode);
            if (handler.pattern == .unsupported) return CodegenError.UnsupportedOpcode;
        }
    }

    // Create the inner _hot function: i32 @name_hot(i32 %n0, i32 %n1, ...)
    var hot_name_buf: [512]u8 = undefined;
    const hot_name = std.fmt.bufPrintZ(&hot_name_buf, "{s}_hot", .{sf.llvm_func_name}) catch
        return CodegenError.FormatError;

    var param_types_buf: [8]llvm.Type = undefined;
    for (0..func.arg_count) |i| {
        param_types_buf[i] = llvm.i32Type();
    }
    const hot_fn_ty = llvm.functionType(llvm.i32Type(), param_types_buf[0..func.arg_count], false);
    const hot_fn = module.addFunction(hot_name, hot_fn_ty);
    llvm.setLinkage(hot_fn, c.LLVMInternalLinkage);

    // Generate the hot function body
    // If this fails, clean up by replacing the function with a valid stub
    // to avoid corrupting the LLVM module state.
    generateInt32Body(allocator, builder, hot_fn, func, cfg) catch |err| {
        // Replace all basic blocks with a minimal valid body
        while (c.LLVMGetFirstBasicBlock(hot_fn)) |bb| {
            // Remove all instructions from this block first
            while (c.LLVMGetFirstInstruction(bb)) |inst| {
                c.LLVMInstructionEraseFromParent(inst);
            }
            c.LLVMDeleteBasicBlock(bb);
        }
        // Add minimal valid body
        const fb = llvm.appendBasicBlock(hot_fn, "entry");
        builder.positionAtEnd(fb);
        _ = builder.buildRet(llvm.constInt32(0));
        return err;
    };

    // Create the JSValue wrapper:
    // i64 @__frozen_N_name(ptr %ctx, i64 %this, i32 %argc, ptr %argv, ptr %var_refs, i32 %closure_cnt, ptr %cpool)
    var wrapper_name_buf: [512]u8 = undefined;
    const wrapper_name = std.fmt.bufPrintZ(&wrapper_name_buf, "{s}", .{sf.llvm_func_name}) catch
        return CodegenError.FormatError;

    const wrapper_params = [_]llvm.Type{
        llvm.ptrType(), // ctx
        llvm.i64Type(), // this_val
        llvm.i32Type(), // argc
        llvm.ptrType(), // argv
        llvm.ptrType(), // var_refs
        llvm.i32Type(), // closure_var_count
        llvm.ptrType(), // cpool
    };
    const wrapper_fn_ty = llvm.functionType(llvm.i64Type(), &wrapper_params, false);
    const wrapper_fn = module.addFunction(wrapper_name, wrapper_fn_ty);
    llvm.setLinkage(wrapper_fn, c.LLVMExternalLinkage);
    llvm.setFunctionCallConv(wrapper_fn, c.LLVMCCallConv);

    // Build wrapper body
    const entry = llvm.appendBasicBlock(wrapper_fn, "entry");
    builder.positionAtEnd(entry);

    // Extract int32 arguments from argv: argv[i] is i64, extract low 32 bits
    const argv_param = c.LLVMGetParam(wrapper_fn, 3); // argv: ptr
    const argc_param = c.LLVMGetParam(wrapper_fn, 2); // argc: i32

    var call_args_buf: [8]llvm.Value = undefined;
    for (0..func.arg_count) |i| {
        // Check if i < argc
        const idx = llvm.constInt32(@intCast(i));
        const in_bounds = builder.buildICmp(c.LLVMIntSLT, idx, argc_param, "inb");

        // Load argv[i] as i64
        var gep_indices = [_]llvm.Value{llvm.constInt(llvm.i64Type(), i, false)};
        const argv_ptr = builder.buildInBoundsGEP(llvm.i64Type(), argv_param, &gep_indices, "argp");
        const argv_val = builder.buildLoad(llvm.i64Type(), argv_ptr, "argv");

        // Extract int32: truncate i64 to i32 (low 32 bits contain the int value)
        const arg_i32 = builder.buildTrunc(argv_val, llvm.i32Type(), "argi");

        // Select: if in bounds use arg, else 0
        const zero_i32 = llvm.constInt32(0);
        call_args_buf[i] = builder.buildSelect(in_bounds, arg_i32, zero_i32, "arg");
    }

    // Call hot function
    const call_args = call_args_buf[0..func.arg_count];
    const result = builder.buildCall(hot_fn_ty, hot_fn, call_args, "res");

    // Box result as JSValue: JSValue.newInt(result)
    // In CompressedValue layout: (JS_TAG_INT << 32) | (value & 0xFFFFFFFF)
    // JS_TAG_INT = 7 (from js_types.zig)
    const result_i64 = builder.buildSExt(result, llvm.i64Type(), "res64");
    const tag = llvm.constInt64(7 << 32); // JS_TAG_INT << 32
    const masked = builder.buildAnd(result_i64, llvm.constInt64(0xFFFFFFFF), "mask");
    const boxed = builder.buildOr(masked, tag, "jsval");

    _ = builder.buildRet(boxed);
}

/// Generate the body of an int32 hot function using LLVM IR.
/// Uses switch-based block dispatch matching the Zig codegen pattern.
fn generateInt32Body(
    allocator: Allocator,
    builder: llvm.Builder,
    func: llvm.Value,
    analyzed: AnalyzedFunction,
    cfg: *const CFG,
) CodegenError!void {
    const blocks = cfg.blocks.items;
    if (blocks.len == 0) return;

    // Create LLVM basic blocks for each CFG block
    var llvm_blocks = allocator.alloc(llvm.BasicBlock, blocks.len) catch return CodegenError.OutOfMemory;
    defer allocator.free(llvm_blocks);

    for (blocks, 0..) |_, i| {
        var bb_name_buf: [32]u8 = undefined;
        const bb_name = std.fmt.bufPrintZ(&bb_name_buf, "blk_{d}", .{i}) catch "blk";
        llvm_blocks[i] = llvm.appendBasicBlock(func, bb_name);
    }

    // Create a default unreachable block (for switch default)
    const unreachable_bb = llvm.appendBasicBlock(func, "unreachable");
    builder.positionAtEnd(unreachable_bb);
    _ = builder.buildUnreachable();

    // Allocate local variables if needed
    builder.positionAtEnd(llvm_blocks[0]);
    var locals_buf: [16]llvm.Value = undefined;
    const local_count = analyzed.var_count;
    for (0..@min(local_count, 16)) |i| {
        var loc_name: [16]u8 = undefined;
        const name = std.fmt.bufPrintZ(&loc_name, "loc_{d}", .{i}) catch "loc";
        locals_buf[i] = builder.buildAlloca(llvm.i32Type(), name);
        _ = builder.buildStore(llvm.constInt32(0), locals_buf[i]);
    }

    // Get function parameters
    var params_buf: [8]llvm.Value = undefined;
    for (0..analyzed.arg_count) |i| {
        params_buf[i] = c.LLVMGetParam(func, @intCast(i));
    }

    // Generate each block
    for (blocks, 0..) |block, block_idx| {
        builder.positionAtEnd(llvm_blocks[block_idx]);

        // Value stack for this block (SSA values)
        var vstack = std.ArrayListUnmanaged(llvm.Value){};
        defer vstack.deinit(allocator);

        // Pre-populate stack from predecessor if needed
        // (For int32 functions, initial stack depth is typically 0)

        var block_terminated = false;

        for (block.instructions) |instr| {
            if (block_terminated) break;

            try emitInt32Instruction(
                allocator,
                builder,
                &vstack,
                instr,
                &params_buf,
                analyzed.arg_count,
                &locals_buf,
                local_count,
                llvm_blocks,
                blocks.len,
                block.successors.items,
                func,
                &block_terminated,
            );
        }

        // If block not terminated, fall through to next block
        if (!block_terminated) {
            if (block_idx + 1 < blocks.len) {
                _ = builder.buildBr(llvm_blocks[block_idx + 1]);
            } else {
                // Last block with no terminator — return 0
                _ = builder.buildRet(llvm.constInt32(0));
            }
        }
    }
}

/// Emit a single int32 instruction as LLVM IR
fn emitInt32Instruction(
    allocator: Allocator,
    builder: llvm.Builder,
    vstack: *std.ArrayListUnmanaged(llvm.Value),
    instr: Instruction,
    params: *[8]llvm.Value,
    arg_count: u32,
    locals: *[16]llvm.Value,
    local_count: u32,
    llvm_blocks: []llvm.BasicBlock,
    block_count: usize,
    successors: []const u32,
    func: llvm.Value,
    block_terminated: *bool,
) CodegenError!void {
    const handler = int32_handlers.getInt32Handler(instr.opcode);

    // Helper: pop from value stack (unwraps the ?T → T since we check len before calling)
    const vstackPop = struct {
        fn pop(vs: *std.ArrayListUnmanaged(llvm.Value)) llvm.Value {
            const v = vs.items[vs.items.len - 1];
            vs.items.len -= 1;
            return v;
        }
    }.pop;

    switch (handler.pattern) {
        .push_const_i32 => {
            const val: i32 = if (handler.value) |v|
                v
            else switch (instr.opcode) {
                .push_i8 => @as(i32, instr.operand.i8),
                .push_i16 => @as(i32, instr.operand.i16),
                .push_i32 => instr.operand.i32,
                else => return CodegenError.UnsupportedOpcode,
            };
            vstack.append(allocator, llvm.constInt32(val)) catch return CodegenError.OutOfMemory;
        },

        .push_bool_i32 => {
            const val: i32 = handler.value orelse 0;
            vstack.append(allocator, llvm.constInt32(val)) catch return CodegenError.OutOfMemory;
        },

        .get_arg_i32 => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .arg => |a| a,
                .u8 => |a| a,
                else => 0,
            };
            if (idx >= arg_count) return CodegenError.UnsupportedOpcode;
            vstack.append(allocator, params[idx]) catch return CodegenError.OutOfMemory;
        },

        .put_arg_i32 => {
            // For TCO: store to argument — not supported in this tier yet
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            _ = vstackPop(vstack);
            // TODO: implement TCO via mutable variables
        },

        .get_loc_i32 => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .u8 => |a| a,
                .u16 => |a| a,
                else => 0,
            };
            if (idx >= @min(local_count, 16)) return CodegenError.UnsupportedOpcode;
            const loc_ptr = locals.*[idx];
            const val = builder.buildLoad(llvm.i32Type(), loc_ptr, "loc");
            vstack.append(allocator, val) catch return CodegenError.OutOfMemory;
        },

        .put_loc_i32 => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .u8 => |a| a,
                .u16 => |a| a,
                else => 0,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= @min(local_count, 16)) return CodegenError.UnsupportedOpcode;
            const val = vstackPop(vstack);
            _ = builder.buildStore(val, locals.*[idx]);
        },

        .set_loc_i32 => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .u8 => |a| a,
                .u16 => |a| a,
                else => 0,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= @min(local_count, 16)) return CodegenError.UnsupportedOpcode;
            const val = vstack.items[vstack.items.len - 1]; // peek, don't pop
            _ = builder.buildStore(val, locals.*[idx]);
        },

        .binary_arith_i32 => {
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const rhs = vstackPop(vstack);
            const lhs = vstackPop(vstack);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;

            const result = if (std.mem.eql(u8, op, "+"))
                builder.buildAdd(lhs, rhs, "add")
            else if (std.mem.eql(u8, op, "-"))
                builder.buildSub(lhs, rhs, "sub")
            else if (std.mem.eql(u8, op, "*"))
                builder.buildMul(lhs, rhs, "mul")
            else if (std.mem.eql(u8, op, "/"))
                builder.buildSDiv(lhs, rhs, "div")
            else if (std.mem.eql(u8, op, "%"))
                builder.buildSRem(lhs, rhs, "rem")
            else
                return CodegenError.UnsupportedOpcode;

            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .bitwise_binary_i32 => {
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const rhs = vstackPop(vstack);
            const lhs = vstackPop(vstack);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;

            const result = if (std.mem.eql(u8, op, "&"))
                builder.buildAnd(lhs, rhs, "and")
            else if (std.mem.eql(u8, op, "|"))
                builder.buildOr(lhs, rhs, "or")
            else if (std.mem.eql(u8, op, "^"))
                builder.buildXor(lhs, rhs, "xor")
            else if (std.mem.eql(u8, op, "<<"))
                builder.buildShl(lhs, rhs, "shl")
            else if (std.mem.eql(u8, op, ">>"))
                builder.buildAShr(lhs, rhs, "shr")
            else
                return CodegenError.UnsupportedOpcode;

            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .binary_cmp_i32 => {
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const rhs = vstackPop(vstack);
            const lhs = vstackPop(vstack);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;

            const pred: c.LLVMIntPredicate = if (std.mem.eql(u8, op, "<"))
                c.LLVMIntSLT
            else if (std.mem.eql(u8, op, "<="))
                c.LLVMIntSLE
            else if (std.mem.eql(u8, op, ">"))
                c.LLVMIntSGT
            else if (std.mem.eql(u8, op, ">="))
                c.LLVMIntSGE
            else if (std.mem.eql(u8, op, "=="))
                c.LLVMIntEQ
            else if (std.mem.eql(u8, op, "!="))
                c.LLVMIntNE
            else
                return CodegenError.UnsupportedOpcode;

            const cmp = builder.buildICmp(pred, lhs, rhs, "cmp");
            // Zero-extend i1 to i32 (false=0, true=1)
            const result = builder.buildZExt(cmp, llvm.i32Type(), "cmpz");
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .unary_i32 => {
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const val = vstackPop(vstack);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;

            const result = if (std.mem.eql(u8, op, "-"))
                builder.buildNeg(val, "neg")
            else if (std.mem.eql(u8, op, "~"))
                builder.buildNot(val, "bnot")
            else
                return CodegenError.UnsupportedOpcode;

            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .lnot_i32 => {
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const val = vstackPop(vstack);
            // Logical NOT: 0 → 1, nonzero → 0
            const is_zero = builder.buildICmp(c.LLVMIntEQ, val, llvm.constInt32(0), "iszero");
            const result = builder.buildZExt(is_zero, llvm.i32Type(), "lnot");
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .inc_dec_i32 => {
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const val = vstackPop(vstack);
            const one = llvm.constInt32(1);
            const result = if (handler.is_inc orelse false)
                builder.buildAdd(val, one, "inc")
            else
                builder.buildSub(val, one, "dec");
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .stack_op_i32 => {
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;
            if (std.mem.eql(u8, op, "dup")) {
                if (vstack.items.len < 1) return CodegenError.StackUnderflow;
                const top = vstack.items[vstack.items.len - 1];
                vstack.append(allocator, top) catch return CodegenError.OutOfMemory;
            } else {
                // drop
                if (vstack.items.len < 1) return CodegenError.StackUnderflow;
                _ = vstackPop(vstack);
            }
        },

        .self_ref_i32 => {
            // Self-reference marker — no LLVM IR needed, just skip
            // The subsequent call_self will handle it
        },

        .call_self_i32 => {
            // Self-recursive call — generate LLVM call to self
            const call_argc: u32 = switch (instr.opcode) {
                .call1 => 1,
                .call2 => 2,
                .call3 => 3,
                .call => instr.operand.u16,
                else => 1,
            };

            if (vstack.items.len < call_argc) return CodegenError.StackUnderflow;

            // Pop args in reverse order
            var call_args: [8]llvm.Value = undefined;
            var i: u32 = call_argc;
            while (i > 0) {
                i -= 1;
                call_args[i] = vstackPop(vstack);
            }

            // Build the self-recursive call
            var param_types_buf: [8]llvm.Type = undefined;
            for (0..call_argc) |j| {
                param_types_buf[j] = llvm.i32Type();
            }
            const self_fn_ty = llvm.functionType(llvm.i32Type(), param_types_buf[0..call_argc], false);
            const args_slice = call_args[0..call_argc];
            const result = builder.buildCall(self_fn_ty, func, args_slice, "self");
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .tail_call_self_i32 => {
            // Tail recursive call — emit as regular call + return
            const call_argc: u32 = instr.operand.u16;

            if (vstack.items.len < call_argc) return CodegenError.StackUnderflow;

            var call_args: [8]llvm.Value = undefined;
            var i: u32 = call_argc;
            while (i > 0) {
                i -= 1;
                call_args[i] = vstackPop(vstack);
            }

            var param_types_buf: [8]llvm.Type = undefined;
            for (0..call_argc) |j| {
                param_types_buf[j] = llvm.i32Type();
            }
            const self_fn_ty = llvm.functionType(llvm.i32Type(), param_types_buf[0..call_argc], false);
            const args_slice = call_args[0..call_argc];
            const result = builder.buildCall(self_fn_ty, func, args_slice, "tail");
            // TODO: mark as tail call via LLVM API
            _ = builder.buildRet(result);
            block_terminated.* = true;
        },

        .return_i32 => {
            if (handler.value) |v| {
                // return_undef → return constant
                _ = builder.buildRet(llvm.constInt32(v));
            } else {
                if (vstack.items.len < 1) return CodegenError.StackUnderflow;
                const val = vstackPop(vstack);
                _ = builder.buildRet(val);
            }
            block_terminated.* = true;
        },

        .if_false_i32 => {
            // Conditional branch: pop condition, branch if false (== 0)
            // CFG successors: [0] = jump target (false branch), [1] = fall-through (true branch)
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const cond_val = vstackPop(vstack);

            if (successors.len >= 2) {
                const false_target = successors[0]; // jump target (when condition is false/zero)
                const true_target = successors[1]; // fall-through (when condition is true/nonzero)

                if (false_target < block_count and true_target < block_count) {
                    const is_zero = builder.buildICmp(c.LLVMIntEQ, cond_val, llvm.constInt32(0), "ifz");
                    _ = builder.buildCondBr(is_zero, llvm_blocks[false_target], llvm_blocks[true_target]);
                    block_terminated.* = true;
                }
            } else if (successors.len == 1) {
                // Only one successor — unconditional branch (degenerate case)
                if (successors[0] < block_count) {
                    _ = builder.buildBr(llvm_blocks[successors[0]]);
                    block_terminated.* = true;
                }
            }
        },

        .if_true_i32 => {
            // Conditional branch: pop condition, branch if true (!= 0)
            // CFG successors: [0] = jump target (true branch), [1] = fall-through (false branch)
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const cond_val = vstackPop(vstack);

            if (successors.len >= 2) {
                const true_target = successors[0]; // jump target (when condition is true/nonzero)
                const false_target = successors[1]; // fall-through (when condition is false/zero)

                if (true_target < block_count and false_target < block_count) {
                    const is_nonzero = builder.buildICmp(c.LLVMIntNE, cond_val, llvm.constInt32(0), "ift");
                    _ = builder.buildCondBr(is_nonzero, llvm_blocks[true_target], llvm_blocks[false_target]);
                    block_terminated.* = true;
                }
            } else if (successors.len == 1) {
                if (successors[0] < block_count) {
                    _ = builder.buildBr(llvm_blocks[successors[0]]);
                    block_terminated.* = true;
                }
            }
        },

        .goto_i32 => {
            // Unconditional branch: jump to the sole successor
            if (successors.len >= 1 and successors[0] < block_count) {
                _ = builder.buildBr(llvm_blocks[successors[0]]);
                block_terminated.* = true;
            }
        },

        .unsupported => {
            return CodegenError.UnsupportedOpcode;
        },
    }
}

// ============================================================================
// Shard Init Function Generation
// ============================================================================

/// Generate frozen_init_llvm_shard_N() function that registers all frozen functions
fn generateShardInit(
    allocator: Allocator,
    module: llvm.Module,
    builder: llvm.Builder,
    shard_idx: usize,
    funcs: []const GeneratedFuncInfo,
    register_fn: llvm.Value,
) CodegenError!void {

    // Create function: i32 @frozen_init_llvm_shard_N()
    var name_buf: [64]u8 = undefined;
    const fn_name = std.fmt.bufPrintZ(&name_buf, "frozen_init_llvm_shard_{d}", .{shard_idx}) catch
        return CodegenError.FormatError;

    const fn_ty = llvm.functionType(llvm.i32Type(), &.{}, false);
    const init_fn = module.addFunction(fn_name, fn_ty);
    llvm.setLinkage(init_fn, c.LLVMExternalLinkage);
    llvm.setFunctionCallConv(init_fn, c.LLVMCCallConv);

    const entry = llvm.appendBasicBlock(init_fn, "entry");
    builder.positionAtEnd(entry);

    // For each function, create a string constant and call native_dispatch_register
    var count: i32 = 0;
    for (funcs) |fi| {
        // Create dispatch key string: "name@line_num\0"
        var key_buf: [512]u8 = undefined;
        const key = std.fmt.bufPrintZ(&key_buf, "{s}@{d}", .{ fi.name, fi.line_num }) catch continue;

        // Use LLVMBuildGlobalStringPtr — handles string constant + GEP automatically
        var str_name_buf: [256]u8 = undefined;
        const str_name = std.fmt.bufPrintZ(&str_name_buf, ".str.{s}", .{fi.llvm_func_name}) catch continue;
        const str_ptr = c.LLVMBuildGlobalStringPtr(builder.ref, key.ptr, str_name.ptr);

        // Get pointer to the frozen function
        var func_name_buf: [512]u8 = undefined;
        const func_name = std.fmt.bufPrintZ(&func_name_buf, "{s}", .{fi.llvm_func_name}) catch continue;
        const frozen_fn = module.getNamedFunction(func_name) orelse continue;

        // Build call: native_dispatch_register(str_ptr, frozen_fn)
        const reg_fn_ty = llvm.functionType(llvm.voidType(), &[_]llvm.Type{ llvm.ptrType(), llvm.ptrType() }, false);
        var call_args = [_]llvm.Value{ str_ptr, frozen_fn };
        _ = builder.buildCall(reg_fn_ty, register_fn, &call_args, "");

        count += 1;
    }

    _ = builder.buildRet(llvm.constInt32(count));
    _ = allocator;
}

// ============================================================================
// Tier B: Thin Codegen (all functions, calling llvm_rt_* runtime helpers)
// ============================================================================

/// Alias for backward compatibility — ThinShardFunction is identical to ShardFunction
pub const ThinShardFunction = ShardFunction;

/// Generate a complete shard of thin-codegen functions as LLVM IR → .o file.
pub fn generateThinShard(
    allocator: Allocator,
    functions: []const ThinShardFunction,
    shard_idx: usize,
    obj_path: [*:0]const u8,
) CodegenError!LLVMShardResult {
    var name_buf: [64]u8 = undefined;
    const mod_name = std.fmt.bufPrintZ(&name_buf, "frozen_thin_shard_{d}", .{shard_idx}) catch
        return CodegenError.FormatError;

    const native = llvm.createNativeModule(mod_name) catch return CodegenError.LLVMError;
    defer native.tm.dispose();
    var module_disposed = false;
    defer if (!module_disposed) native.module.dispose();

    const builder = llvm.Builder.create(native.ctx);
    defer builder.dispose();

    // Declare external runtime functions
    const register_fn = declareNativeDispatchRegister(native.module);
    var rt_decls = ThinRuntimeDecls.declare(native.module);

    // Generate each function
    var generated_count: u32 = 0;
    var generated_infos = std.ArrayListUnmanaged(GeneratedFuncInfo){};
    defer generated_infos.deinit(allocator);

    for (functions) |sf| {
        if (generateThinFunction(allocator, native.module, builder, sf, &rt_decls)) |_| {
            generated_infos.append(allocator, .{
                .name = sf.name,
                .func_index = sf.func_index,
                .line_num = sf.line_num,
                .llvm_func_name = sf.llvm_func_name,
            }) catch continue;
            generated_count += 1;
        } else |err| {
            switch (err) {
                CodegenError.UnsupportedOpcode => {},
                else => std.debug.print("[llvm-thin] Function '{s}' failed: {}\n", .{ sf.name, err }),
            }
            continue;
        }
    }

    if (generated_count == 0) {
        return .{ .func_count = 0, .has_functions = false };
    }

    generateShardInit(allocator, native.module, builder, shard_idx, generated_infos.items, register_fn) catch
        return CodegenError.LLVMError;

    module_disposed = try finalizeAndEmitShard(native, obj_path, "llvm-thin");
    return .{ .func_count = generated_count, .has_functions = true };
}

/// Declarations for all llvm_rt_* runtime functions needed by thin codegen
const ThinRuntimeDecls = struct {
    // Stack operations
    push_i32: llvm.Value,
    push_true: llvm.Value,
    push_false: llvm.Value,
    push_null: llvm.Value,
    push_undefined: llvm.Value,
    push_cv: llvm.Value,

    // Local variable access
    get_loc: llvm.Value,
    put_loc: llvm.Value,
    set_loc: llvm.Value,

    // Argument access
    get_arg: llvm.Value,
    get_arg_shadow: llvm.Value,
    put_arg: llvm.Value,

    // Closure variable access
    get_var_ref: llvm.Value,
    put_var_ref: llvm.Value,
    set_var_ref: llvm.Value,

    // Stack manipulation
    dup_top: llvm.Value,
    dup1: llvm.Value,
    dup2: llvm.Value,
    dup3: llvm.Value,
    drop: llvm.Value,
    nip: llvm.Value,
    swap: llvm.Value,

    // Arithmetic
    op_add: llvm.Value,
    op_sub: llvm.Value,
    op_mul: llvm.Value,
    op_div: llvm.Value,
    op_mod: llvm.Value,
    op_pow: llvm.Value,
    op_neg: llvm.Value,
    op_plus: llvm.Value,

    // Bitwise
    op_band: llvm.Value,
    op_bor: llvm.Value,
    op_bxor: llvm.Value,
    op_bnot: llvm.Value,
    op_shl: llvm.Value,
    op_sar: llvm.Value,
    op_shr: llvm.Value,

    // Comparison
    op_lt: llvm.Value,
    op_lte: llvm.Value,
    op_gt: llvm.Value,
    op_gte: llvm.Value,
    op_eq: llvm.Value,
    op_neq: llvm.Value,
    op_strict_eq: llvm.Value,
    op_strict_neq: llvm.Value,

    // Logical
    op_lnot: llvm.Value,
    op_typeof: llvm.Value,

    // Increment
    inc_loc: llvm.Value,
    dec_loc: llvm.Value,

    // Complex ops (return c_int for error)
    op_call: llvm.Value,
    op_call_method: llvm.Value,
    op_call_constructor: llvm.Value,
    op_get_field_ic: llvm.Value,
    op_get_field2_ic: llvm.Value,
    op_put_field: llvm.Value,
    op_get_var: llvm.Value,
    op_apply: llvm.Value,

    // Return helpers
    returnFromStack: llvm.Value,
    returnUndef: llvm.Value,
    exceptionCleanup: llvm.Value,

    // CV operations
    cv_from_jsvalue: llvm.Value,
    cv_dup_ref: llvm.Value,
    cv_free_ref: llvm.Value,

    // Stack overflow
    check_stack: llvm.Value,
    exit_stack: llvm.Value,

    // Iterator ops
    op_for_of_start: llvm.Value,
    op_for_of_next: llvm.Value,
    op_iterator_close: llvm.Value,
    op_for_in_start: llvm.Value,
    op_for_in_next: llvm.Value,

    // Array ops
    op_append: llvm.Value,
    op_put_array_el: llvm.Value,

    // Opcode fallback
    exec_opcode: llvm.Value,

    fn declare(module: llvm.Module) ThinRuntimeDecls {
        const ptr = llvm.ptrType();
        const i32t = llvm.i32Type();
        const i64t = llvm.i64Type();
        const voidt = llvm.voidType();

        // Common signatures
        // void(stack, sp, i32)
        const stack_sp_i32 = llvm.functionType(voidt, &.{ ptr, ptr, i32t }, false);
        // void(stack, sp)
        const stack_sp = llvm.functionType(voidt, &.{ ptr, ptr }, false);
        // void(ctx, stack, sp)
        const ctx_stack_sp = llvm.functionType(voidt, &.{ ptr, ptr, ptr }, false);
        // void(ctx, stack, sp, locals, u32)
        const ctx_stack_sp_loc_u32 = llvm.functionType(voidt, &.{ ptr, ptr, ptr, ptr, i32t }, false);
        // void(stack, sp, cv) - cv is i64
        const stack_sp_cv = llvm.functionType(voidt, &.{ ptr, ptr, i64t }, false);
        // void(locals, u32)
        const locals_u32 = llvm.functionType(voidt, &.{ ptr, i32t }, false);
        // i32(ctx, stack, sp, u16) - call ops returning error code
        const call_op = llvm.functionType(i32t, &.{ ptr, ptr, ptr, i32t }, false);  // u16 passed as i32
        // i32(ctx, stack, sp, ptr, ptr) - field ops returning error code
        const field_ic_op = llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr, ptr }, false);
        // i32(ctx, stack, sp, ptr) - simpler field/var op
        const field_op = llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr }, false);
        // i64(ctx, stack, sp, locals, usize, ptr, ptr, ptr, usize) - return helper
        const return_helper = llvm.functionType(i64t, &.{ ptr, ptr, ptr, ptr, i64t, ptr, ptr, ptr, i64t }, false);
        // i64(ctx, locals, usize, ptr, ptr, ptr, usize) - returnUndef/exceptionCleanup
        const cleanup_helper = llvm.functionType(i64t, &.{ ptr, ptr, i64t, ptr, ptr, ptr, i64t }, false);

        return .{
            .push_i32 = declareExtern(module, "llvm_rt_push_i32", stack_sp_i32),
            .push_true = declareExtern(module, "llvm_rt_push_true", stack_sp),
            .push_false = declareExtern(module, "llvm_rt_push_false", stack_sp),
            .push_null = declareExtern(module, "llvm_rt_push_null", stack_sp),
            .push_undefined = declareExtern(module, "llvm_rt_push_undefined", stack_sp),
            .push_cv = declareExtern(module, "llvm_rt_push_cv", stack_sp_cv),

            .get_loc = declareExtern(module, "llvm_rt_get_loc", ctx_stack_sp_loc_u32),
            .put_loc = declareExtern(module, "llvm_rt_put_loc", ctx_stack_sp_loc_u32),
            .set_loc = declareExtern(module, "llvm_rt_set_loc", ctx_stack_sp_loc_u32),

            .get_arg = declareExtern(module, "llvm_rt_get_arg", llvm.functionType(voidt, &.{ ptr, ptr, ptr, i32t, ptr, i32t }, false)),
            .get_arg_shadow = declareExtern(module, "llvm_rt_get_arg_shadow", llvm.functionType(voidt, &.{ ptr, ptr, ptr, i32t }, false)),
            .put_arg = declareExtern(module, "llvm_rt_put_arg", ctx_stack_sp_loc_u32),

            .get_var_ref = declareExtern(module, "llvm_rt_get_var_ref", llvm.functionType(voidt, &.{ ptr, ptr, ptr, ptr, i32t, i32t }, false)),
            .put_var_ref = declareExtern(module, "llvm_rt_put_var_ref", llvm.functionType(voidt, &.{ ptr, ptr, ptr, ptr, i32t, i32t }, false)),
            .set_var_ref = declareExtern(module, "llvm_rt_set_var_ref", llvm.functionType(voidt, &.{ ptr, ptr, ptr, ptr, i32t, i32t }, false)),

            .dup_top = declareExtern(module, "llvm_rt_dup_top", ctx_stack_sp),
            .dup1 = declareExtern(module, "llvm_rt_dup1", ctx_stack_sp),
            .dup2 = declareExtern(module, "llvm_rt_dup2", ctx_stack_sp),
            .dup3 = declareExtern(module, "llvm_rt_dup3", ctx_stack_sp),
            .drop = declareExtern(module, "llvm_rt_drop", ctx_stack_sp),
            .nip = declareExtern(module, "llvm_rt_nip", ctx_stack_sp),
            .swap = declareExtern(module, "llvm_rt_swap", stack_sp),

            .op_add = declareExtern(module, "llvm_rt_op_add", ctx_stack_sp),
            .op_sub = declareExtern(module, "llvm_rt_op_sub", stack_sp),
            .op_mul = declareExtern(module, "llvm_rt_op_mul", stack_sp),
            .op_div = declareExtern(module, "llvm_rt_op_div", stack_sp),
            .op_mod = declareExtern(module, "llvm_rt_op_mod", stack_sp),
            .op_pow = declareExtern(module, "llvm_rt_op_pow", stack_sp),
            .op_neg = declareExtern(module, "llvm_rt_op_neg", stack_sp),
            .op_plus = declareExtern(module, "llvm_rt_op_plus", stack_sp),

            .op_band = declareExtern(module, "llvm_rt_op_band", stack_sp),
            .op_bor = declareExtern(module, "llvm_rt_op_bor", stack_sp),
            .op_bxor = declareExtern(module, "llvm_rt_op_bxor", stack_sp),
            .op_bnot = declareExtern(module, "llvm_rt_op_bnot", stack_sp),
            .op_shl = declareExtern(module, "llvm_rt_op_shl", stack_sp),
            .op_sar = declareExtern(module, "llvm_rt_op_sar", stack_sp),
            .op_shr = declareExtern(module, "llvm_rt_op_shr", stack_sp),

            .op_lt = declareExtern(module, "llvm_rt_op_lt", stack_sp),
            .op_lte = declareExtern(module, "llvm_rt_op_lte", stack_sp),
            .op_gt = declareExtern(module, "llvm_rt_op_gt", stack_sp),
            .op_gte = declareExtern(module, "llvm_rt_op_gte", stack_sp),
            .op_eq = declareExtern(module, "llvm_rt_op_eq", ctx_stack_sp),
            .op_neq = declareExtern(module, "llvm_rt_op_neq", ctx_stack_sp),
            .op_strict_eq = declareExtern(module, "llvm_rt_op_strict_eq", ctx_stack_sp),
            .op_strict_neq = declareExtern(module, "llvm_rt_op_strict_neq", ctx_stack_sp),

            .op_lnot = declareExtern(module, "llvm_rt_op_lnot", ctx_stack_sp),
            .op_typeof = declareExtern(module, "llvm_rt_op_typeof", ctx_stack_sp),

            .inc_loc = declareExtern(module, "llvm_rt_inc_loc", locals_u32),
            .dec_loc = declareExtern(module, "llvm_rt_dec_loc", locals_u32),

            .op_call = declareExtern(module, "llvm_rt_op_call", call_op),
            .op_call_method = declareExtern(module, "llvm_rt_op_call_method", call_op),
            .op_call_constructor = declareExtern(module, "llvm_rt_op_call_constructor", call_op),
            .op_get_field_ic = declareExtern(module, "llvm_rt_op_get_field_ic", field_ic_op),
            .op_get_field2_ic = declareExtern(module, "llvm_rt_op_get_field2_ic", field_ic_op),
            .op_put_field = declareExtern(module, "llvm_rt_op_put_field", field_op),
            .op_get_var = declareExtern(module, "llvm_rt_op_get_var", field_op),
            .op_apply = declareExtern(module, "llvm_rt_op_apply", llvm.functionType(i32t, &.{ ptr, ptr, ptr }, false)),

            .returnFromStack = declareExtern(module, "llvm_rt_returnFromStack", return_helper),
            .returnUndef = declareExtern(module, "llvm_rt_returnUndef", cleanup_helper),
            .exceptionCleanup = declareExtern(module, "llvm_rt_exceptionCleanup", cleanup_helper),

            .cv_from_jsvalue = declareExtern(module, "llvm_rt_cv_from_jsvalue", llvm.functionType(i64t, &.{i64t}, false)),
            .cv_dup_ref = declareExtern(module, "llvm_rt_cv_dup_ref", llvm.functionType(i64t, &.{i64t}, false)),
            .cv_free_ref = declareExtern(module, "llvm_rt_cv_free_ref", llvm.functionType(voidt, &.{ ptr, i64t }, false)),

            .check_stack = declareExtern(module, "llvm_rt_check_stack", llvm.functionType(llvm.i1Type(), &.{}, false)),
            .exit_stack = declareExtern(module, "llvm_rt_exit_stack", llvm.functionType(voidt, &.{}, false)),

            .op_for_of_start = declareExtern(module, "llvm_rt_op_for_of_start", llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr, ptr }, false)),
            .op_for_of_next = declareExtern(module, "llvm_rt_op_for_of_next", llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr, ptr }, false)),
            .op_iterator_close = declareExtern(module, "llvm_rt_op_iterator_close", llvm.functionType(voidt, &.{ ptr, ptr, ptr, ptr, ptr }, false)),
            .op_for_in_start = declareExtern(module, "llvm_rt_op_for_in_start", field_op),
            .op_for_in_next = declareExtern(module, "llvm_rt_op_for_in_next", field_op),

            .op_append = declareExtern(module, "llvm_rt_op_append", ctx_stack_sp),
            .op_put_array_el = declareExtern(module, "llvm_rt_op_put_array_el", ctx_stack_sp),

            .exec_opcode = declareExtern(module, "llvm_rt_exec_opcode", llvm.functionType(i32t, &.{ ptr, llvm.i8Type(), i32t, ptr, ptr, ptr, i32t }, false)),
        };
    }
};

fn declareExtern(module: llvm.Module, name: [*:0]const u8, fn_ty: llvm.Type) llvm.Value {
    const func = module.addFunction(name, fn_ty);
    llvm.setLinkage(func, c.LLVMExternalLinkage);
    return func;
}

/// Thin codegen context — holds LLVM references for the current function being generated
const ThinCodegenCtx = struct {
    builder: llvm.Builder,
    module: llvm.Module,
    rt: *ThinRuntimeDecls,
    allocator: Allocator,

    // Function parameters
    ctx_param: llvm.Value, // JSContext*
    this_param: llvm.Value, // JSValue (i64)
    argc_param: llvm.Value, // i32
    argv_param: llvm.Value, // ptr
    var_refs_param: llvm.Value, // ptr
    closure_var_count_param: llvm.Value, // i32
    cpool_param: llvm.Value, // ptr

    // Locals
    stack_ptr: llvm.Value, // [N x i64]*
    sp_ptr: llvm.Value, // usize*
    locals_ptr: llvm.Value, // [N x i64]*

    // Function info
    func: AnalyzedFunction,
    ic_count: u32 = 0,

    // Flags (from pre-scan)
    has_put_arg: bool = false,
    has_fclosure: bool = false,
    has_for_of: bool = false,

    // Arg shadow
    arg_shadow_ptr: ?llvm.Value = null,

    // Exception cleanup block
    cleanup_bb: llvm.BasicBlock,

    // Stack/locals sizes
    stack_array_size: u32,
    local_count: u32,

    fn nextIcSlot(self: *ThinCodegenCtx) u32 {
        const idx = self.ic_count;
        self.ic_count += 1;
        return idx;
    }
};

/// Generate a single thin-codegen function as LLVM IR.
/// Creates a state-machine function calling llvm_rt_* runtime helpers.
fn generateThinFunction(
    allocator: Allocator,
    module: llvm.Module,
    builder: llvm.Builder,
    sf: ThinShardFunction,
    rt: *ThinRuntimeDecls,
) CodegenError!void {
    const func = sf.func;
    const cfg = sf.cfg;
    const blocks = cfg.blocks.items;
    if (blocks.len == 0) return CodegenError.UnsupportedOpcode;

    // Pre-scan for flags
    var has_put_arg = false;
    var has_fclosure = false;
    var has_for_of = false;
    var ic_count: u32 = 0;

    for (blocks) |block| {
        for (block.instructions) |instr| {
            switch (instr.opcode) {
                .put_arg, .put_arg0, .put_arg1, .put_arg2, .put_arg3,
                .set_arg, .set_arg0, .set_arg1, .set_arg2, .set_arg3,
                => has_put_arg = true,
                .fclosure, .fclosure8 => has_fclosure = true,
                .for_of_start => has_for_of = true,
                .get_field, .get_field2 => ic_count += 1,
                else => {},
            }
        }
    }

    // Create the wrapper function:
    // i64 @__frozen_N_name(ptr %ctx, i64 %this, i32 %argc, ptr %argv, ptr %var_refs, i32 %closure_cnt, ptr %cpool)
    var wrapper_name_buf: [512]u8 = undefined;
    const wrapper_name = std.fmt.bufPrintZ(&wrapper_name_buf, "{s}", .{sf.llvm_func_name}) catch
        return CodegenError.FormatError;

    const wrapper_params = [_]llvm.Type{
        llvm.ptrType(), // ctx
        llvm.i64Type(), // this_val
        llvm.i32Type(), // argc
        llvm.ptrType(), // argv
        llvm.ptrType(), // var_refs
        llvm.i32Type(), // closure_var_count
        llvm.ptrType(), // cpool
    };
    const wrapper_fn_ty = llvm.functionType(llvm.i64Type(), &wrapper_params, false);
    const wrapper_fn = module.addFunction(wrapper_name, wrapper_fn_ty);
    llvm.setLinkage(wrapper_fn, c.LLVMExternalLinkage);
    llvm.setFunctionCallConv(wrapper_fn, c.LLVMCCallConv);
    // Mark as noinline (thin functions shouldn't be inlined)
    {
        const kind = c.LLVMGetEnumAttributeKindForName("noinline", 8);
        if (kind != 0) {
            const attr = c.LLVMCreateEnumAttribute(c.LLVMGetGlobalContext(), kind, 0);
            // LLVMAttributeFunctionIndex is -1, need @bitCast for c_uint
            const func_idx: c.LLVMAttributeIndex = @bitCast(@as(c_int, -1));
            c.LLVMAddAttributeAtIndex(wrapper_fn, func_idx, attr);
        }
    }

    // Entry block
    const entry_bb = llvm.appendBasicBlock(wrapper_fn, "entry");
    // Cleanup block (for exceptions)
    const cleanup_bb = llvm.appendBasicBlock(wrapper_fn, "cleanup");

    builder.positionAtEnd(entry_bb);

    // Extract parameters
    const ctx_param = c.LLVMGetParam(wrapper_fn, 0);
    const this_param = c.LLVMGetParam(wrapper_fn, 1);
    const argc_param = c.LLVMGetParam(wrapper_fn, 2);
    const argv_param = c.LLVMGetParam(wrapper_fn, 3);
    const var_refs_param = c.LLVMGetParam(wrapper_fn, 4);
    const closure_var_count_param = c.LLVMGetParam(wrapper_fn, 5);
    const cpool_param = c.LLVMGetParam(wrapper_fn, 6);

    _ = this_param;
    _ = closure_var_count_param;

    // Stack overflow check: if (check_stack()) return throwRangeError
    {
        const overflow = builder.buildCall(
            llvm.functionType(llvm.i1Type(), &.{}, false),
            rt.check_stack, &.{}, "stk_chk",
        );
        const no_overflow_bb = llvm.appendBasicBlock(wrapper_fn, "no_overflow");
        const overflow_bb = llvm.appendBasicBlock(wrapper_fn, "overflow");
        _ = builder.buildCondBr(overflow, overflow_bb, no_overflow_bb);

        // Overflow: return EXCEPTION value (0x0006_0000_0000_0000 = JS_TAG_EXCEPTION << 32)
        builder.positionAtEnd(overflow_bb);
        _ = builder.buildRet(llvm.constInt64(0x0006_0000_0000_0000));

        builder.positionAtEnd(no_overflow_bb);
    }

    // Allocate locals: var locals: [N]CV  (CV = i64 = CompressedValue)
    const local_count: u32 = @max(func.var_count, 1);
    const locals_ty = llvm.arrayType(llvm.i64Type(), local_count);
    const locals_ptr = builder.buildAlloca(locals_ty, "locals");
    // Initialize to UNDEFINED (0x7FFC_0000_0000_0000)
    const cv_undefined = llvm.constInt64(0x7FFC_0000_0000_0000);
    for (0..local_count) |i| {
        var indices = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(@intCast(i)) };
        const elem = builder.buildInBoundsGEP(locals_ty, locals_ptr, &indices, "loc_init");
        _ = builder.buildStore(cv_undefined, elem);
    }

    // Allocate stack
    const stack_array_size: u32 = @max(func.var_count + func.stack_size, 4);
    const stack_ty = llvm.arrayType(llvm.i64Type(), stack_array_size);
    const stack_alloc = builder.buildAlloca(stack_ty, "stack");

    // Stack pointer (usize = i64 on x86-64)
    const sp_ptr = builder.buildAlloca(llvm.i64Type(), "sp");
    _ = builder.buildStore(llvm.constInt64(0), sp_ptr);

    // Arg shadow (if has_put_arg)
    var arg_shadow_ptr: ?llvm.Value = null;
    if (has_put_arg and func.arg_count > 0) {
        const shadow_ty = llvm.arrayType(llvm.i64Type(), func.arg_count);
        arg_shadow_ptr = builder.buildAlloca(shadow_ty, "arg_shadow");
        // Initialize: arg_shadow[i] = i < argc ? cv_from_jsvalue(argv[i]) : UNDEFINED
        for (0..func.arg_count) |i| {
            const idx_val = llvm.constInt32(@intCast(i));
            const in_bounds = builder.buildICmp(c.LLVMIntSLT, idx_val, argc_param, "inb");

            // Load argv[i]
            var argv_idx = [_]llvm.Value{llvm.constInt(llvm.i64Type(), i, false)};
            const argv_elem_ptr = builder.buildInBoundsGEP(llvm.i64Type(), argv_param, &argv_idx, "avp");
            const argv_val = builder.buildLoad(llvm.i64Type(), argv_elem_ptr, "av");

            // Convert to CV
            const cv_val = builder.buildCall(
                llvm.functionType(llvm.i64Type(), &.{llvm.i64Type()}, false),
                rt.cv_from_jsvalue, &.{argv_val}, "cv",
            );

            const shadow_val = builder.buildSelect(in_bounds, cv_val, cv_undefined, "shv");
            var shadow_idx = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(@intCast(i)) };
            const shadow_elem = builder.buildInBoundsGEP(
                llvm.arrayType(llvm.i64Type(), func.arg_count),
                arg_shadow_ptr.?, &shadow_idx, "shp",
            );
            _ = builder.buildStore(shadow_val, shadow_elem);
        }
    }

    // Cast stack and locals to flat [*]i64 pointers for runtime calls
    // (LLVM alloca gives us [N x i64]*, runtime wants i64* / ptr)
    var stack_cast_idx = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
    const stack_flat = builder.buildInBoundsGEP(stack_ty, stack_alloc, &stack_cast_idx, "stk");
    var locals_cast_idx = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
    const locals_flat = builder.buildInBoundsGEP(locals_ty, locals_ptr, &locals_cast_idx, "locs");

    // IC slots are handled per-instruction in emitThinFieldOp
    // (each get_field/get_field2 gets a null IC for now, string-based lookup fallback)

    // Save the no_overflow insertion point
    const no_overflow_bb = c.LLVMGetInsertBlock(builder.ref);

    // Build the cleanup block (exception path)
    builder.positionAtEnd(cleanup_bb);
    {
        // Call exit_stack, then exceptionCleanup
        _ = builder.buildCall(llvm.functionType(llvm.voidType(), &.{}, false), rt.exit_stack, &.{}, "");
        const cleanup_result = builder.buildCall(
            llvm.functionType(llvm.i64Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.i64Type(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i64Type() }, false),
            rt.exceptionCleanup,
            &.{
                ctx_param,
                locals_flat,
                llvm.constInt64(local_count),
                llvm.constNull(llvm.ptrType()), // locals_jsv (null for non-closure)
                llvm.constNull(llvm.ptrType()), // var_ref_list (null)
                if (arg_shadow_ptr) |sp2| blk: {
                    var si = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
                    break :blk builder.buildInBoundsGEP(llvm.arrayType(llvm.i64Type(), func.arg_count), sp2, &si, "asp");
                } else llvm.constNull(llvm.ptrType()),
                llvm.constInt64(if (has_put_arg) func.arg_count else 0),
            },
            "exc_ret",
        );
        _ = builder.buildRet(cleanup_result);
    }

    // Restore builder position to no_overflow block for main codegen
    builder.positionAtEnd(no_overflow_bb);

    // Build the thin codegen context
    var tctx = ThinCodegenCtx{
        .builder = builder,
        .module = module,
        .rt = rt,
        .allocator = allocator,
        .ctx_param = ctx_param,
        .this_param = c.LLVMGetParam(wrapper_fn, 1),
        .argc_param = argc_param,
        .argv_param = argv_param,
        .var_refs_param = var_refs_param,
        .closure_var_count_param = c.LLVMGetParam(wrapper_fn, 5),
        .cpool_param = cpool_param,
        .stack_ptr = stack_flat,
        .sp_ptr = sp_ptr,
        .locals_ptr = locals_flat,
        .func = func,
        .has_put_arg = has_put_arg,
        .has_fclosure = has_fclosure,
        .has_for_of = has_for_of,
        .arg_shadow_ptr = arg_shadow_ptr,
        .cleanup_bb = cleanup_bb,
        .stack_array_size = stack_array_size,
        .local_count = local_count,
    };

    // At this point, builder is positioned at the end of the "no_overflow" block.
    // We saved our position there after the stack overflow check.

    // Generate block dispatch
    if (blocks.len == 1) {
        // Single block: emit directly, no state machine
        // Builder is already positioned at no_overflow

        for (blocks[0].instructions) |instr| {
            emitThinInstruction(&tctx, instr) catch |err| {
                switch (err) {
                    CodegenError.UnsupportedOpcode => {
                        // Use fallback: call exec_opcode
                        emitFallbackOpcode(&tctx, instr);
                    },
                    else => return err,
                }
            };
        }

        // Default return: exit_stack + returnUndef
        if (!isBlockTerminated(blocks[0])) {
            _ = builder.buildCall(llvm.functionType(llvm.voidType(), &.{}, false), rt.exit_stack, &.{}, "");
            const ret = builder.buildCall(
                llvm.functionType(llvm.i64Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.i64Type(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i64Type() }, false),
                rt.returnUndef,
                &.{
                    ctx_param, locals_flat, llvm.constInt64(local_count),
                    llvm.constNull(llvm.ptrType()), llvm.constNull(llvm.ptrType()),
                    llvm.constNull(llvm.ptrType()), llvm.constInt64(0),
                },
                "ret",
            );
            _ = builder.buildRet(ret);
        }
    } else {
        // Multi-block: switch-dispatch state machine
        // Builder is already positioned at no_overflow

        // Allocate block_id variable
        const block_id_ptr = builder.buildAlloca(llvm.i32Type(), "block_id");
        _ = builder.buildStore(llvm.constInt32(0), block_id_ptr);

        // Create dispatch loop
        const dispatch_bb = llvm.appendBasicBlock(wrapper_fn, "dispatch");
        _ = builder.buildBr(dispatch_bb);

        // Create LLVM basic blocks for each CFG block
        var llvm_blocks = allocator.alloc(llvm.BasicBlock, blocks.len) catch return CodegenError.OutOfMemory;
        defer allocator.free(llvm_blocks);

        for (0..blocks.len) |i| {
            var bb_name_buf: [32]u8 = undefined;
            const bb_name = std.fmt.bufPrintZ(&bb_name_buf, "blk_{d}", .{i}) catch "blk";
            llvm_blocks[i] = llvm.appendBasicBlock(wrapper_fn, bb_name);
        }

        // Default block (unreachable)
        const default_bb = llvm.appendBasicBlock(wrapper_fn, "default");
        builder.positionAtEnd(default_bb);
        _ = builder.buildBr(dispatch_bb); // loop back (should never happen)

        // Dispatch block: load block_id, switch
        builder.positionAtEnd(dispatch_bb);
        const block_id = builder.buildLoad(llvm.i32Type(), block_id_ptr, "bid");
        const switch_inst = builder.buildSwitch(block_id, default_bb, @intCast(blocks.len));

        for (0..blocks.len) |i| {
            llvm.addSwitchCase(switch_inst, llvm.constInt32(@intCast(i)), llvm_blocks[i]);
        }

        // Generate each block's body
        for (blocks, 0..) |block, block_idx| {
            builder.positionAtEnd(llvm_blocks[block_idx]);

            for (block.instructions) |instr| {
                // Skip control flow ops (handled by terminator)
                switch (instr.opcode) {
                    .if_false, .if_false8, .if_true, .if_true8, .goto, .goto8, .goto16 => continue,
                    else => {},
                }

                emitThinInstruction(&tctx, instr) catch |err| {
                    switch (err) {
                        CodegenError.UnsupportedOpcode => emitFallbackOpcode(&tctx, instr),
                        else => return err,
                    }
                };
            }

            // Block terminator
            emitThinBlockTerminator(&tctx, block, block_id_ptr, dispatch_bb, llvm_blocks);
        }
    }
}

fn isBlockTerminated(block: CFGBasicBlock) bool {
    if (block.instructions.len == 0) return false;
    const last = block.instructions[block.instructions.len - 1].opcode;
    return last == .@"return" or last == .return_undef or last == .throw;
}

/// Emit a single thin-codegen instruction as LLVM IR calls to llvm_rt_* functions.
fn emitThinInstruction(
    tctx: *ThinCodegenCtx,
    instr: Instruction,
) CodegenError!void {
    const b = tctx.builder;
    const rt = tctx.rt;
    const ctx_p = tctx.ctx_param;
    const stk = tctx.stack_ptr;
    const sp = tctx.sp_ptr;
    const locs = tctx.locals_ptr;

    switch (instr.opcode) {
        // ========== Constants ==========
        .push_0 => callVoid3i(b, rt.push_i32, stk, sp, llvm.constInt32(0)),
        .push_1 => callVoid3i(b, rt.push_i32, stk, sp, llvm.constInt32(1)),
        .push_2 => callVoid3i(b, rt.push_i32, stk, sp, llvm.constInt32(2)),
        .push_3 => callVoid3i(b, rt.push_i32, stk, sp, llvm.constInt32(3)),
        .push_4 => callVoid3i(b, rt.push_i32, stk, sp, llvm.constInt32(4)),
        .push_5 => callVoid3i(b, rt.push_i32, stk, sp, llvm.constInt32(5)),
        .push_6 => callVoid3i(b, rt.push_i32, stk, sp, llvm.constInt32(6)),
        .push_7 => callVoid3i(b, rt.push_i32, stk, sp, llvm.constInt32(7)),
        .push_minus1 => callVoid3i(b, rt.push_i32, stk, sp, llvm.constInt32(-1)),
        .push_i8 => callVoid3i(b, rt.push_i32, stk, sp, llvm.constInt32(instr.operand.i8)),
        .push_i16 => callVoid3i(b, rt.push_i32, stk, sp, llvm.constInt32(instr.operand.i16)),
        .push_i32 => callVoid3i(b, rt.push_i32, stk, sp, llvm.constInt32(instr.operand.i32)),
        .push_true => callVoid2(b, rt.push_true, stk, sp),
        .push_false => callVoid2(b, rt.push_false, stk, sp),
        .null => callVoid2(b, rt.push_null, stk, sp),
        .undefined => callVoid2(b, rt.push_undefined, stk, sp),

        .push_const8, .push_const => {
            // push_cv(&stack, &sp, if (cpool) cv_from_jsvalue(cpool[idx]) else UNDEFINED)
            // For now, push UNDEFINED as fallback (cpool handling needs runtime support)
            callVoid2(b, rt.push_undefined, stk, sp);
        },

        // ========== Local Variables ==========
        .get_loc0 => callVoid4u(b, rt.get_loc, ctx_p, stk, sp, locs, 0),
        .get_loc1 => callVoid4u(b, rt.get_loc, ctx_p, stk, sp, locs, 1),
        .get_loc2 => callVoid4u(b, rt.get_loc, ctx_p, stk, sp, locs, 2),
        .get_loc3 => callVoid4u(b, rt.get_loc, ctx_p, stk, sp, locs, 3),
        .get_loc, .get_loc8 => callVoid4u(b, rt.get_loc, ctx_p, stk, sp, locs, instr.operand.loc),
        .get_loc0_loc1 => {
            callVoid4u(b, rt.get_loc, ctx_p, stk, sp, locs, 0);
            callVoid4u(b, rt.get_loc, ctx_p, stk, sp, locs, 1);
        },

        .put_loc0 => callVoid4u(b, rt.put_loc, ctx_p, stk, sp, locs, 0),
        .put_loc1 => callVoid4u(b, rt.put_loc, ctx_p, stk, sp, locs, 1),
        .put_loc2 => callVoid4u(b, rt.put_loc, ctx_p, stk, sp, locs, 2),
        .put_loc3 => callVoid4u(b, rt.put_loc, ctx_p, stk, sp, locs, 3),
        .put_loc, .put_loc8 => callVoid4u(b, rt.put_loc, ctx_p, stk, sp, locs, instr.operand.loc),

        .set_loc0 => callVoid4u(b, rt.set_loc, ctx_p, stk, sp, locs, 0),
        .set_loc1 => callVoid4u(b, rt.set_loc, ctx_p, stk, sp, locs, 1),
        .set_loc2 => callVoid4u(b, rt.set_loc, ctx_p, stk, sp, locs, 2),
        .set_loc3 => callVoid4u(b, rt.set_loc, ctx_p, stk, sp, locs, 3),
        .set_loc, .set_loc8 => callVoid4u(b, rt.set_loc, ctx_p, stk, sp, locs, instr.operand.loc),

        // ========== Arguments ==========
        .get_arg0, .get_arg1, .get_arg2, .get_arg3, .get_arg => {
            const idx: u32 = switch (instr.opcode) {
                .get_arg0 => 0,
                .get_arg1 => 1,
                .get_arg2 => 2,
                .get_arg3 => 3,
                else => instr.operand.arg,
            };
            if (tctx.has_put_arg) {
                // get_arg_shadow(stack, sp, arg_shadow, idx)
                if (tctx.arg_shadow_ptr) |asp| {
                    var si = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
                    const asp_flat = b.buildInBoundsGEP(
                        llvm.arrayType(llvm.i64Type(), tctx.func.arg_count),
                        asp, &si, "asp",
                    );
                    _ = b.buildCall(
                        llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false),
                        rt.get_arg_shadow, &.{ stk, sp, asp_flat, llvm.constInt32(@intCast(idx)) }, "",
                    );
                }
            } else {
                // get_arg(ctx, stack, sp, argc, argv, idx)
                _ = b.buildCall(
                    llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type(), llvm.ptrType(), llvm.i32Type() }, false),
                    rt.get_arg, &.{ ctx_p, stk, sp, tctx.argc_param, tctx.argv_param, llvm.constInt32(@intCast(idx)) }, "",
                );
            }
        },

        .put_arg0, .put_arg1, .put_arg2, .put_arg3, .put_arg => {
            const idx: u32 = switch (instr.opcode) {
                .put_arg0 => 0,
                .put_arg1 => 1,
                .put_arg2 => 2,
                .put_arg3 => 3,
                else => instr.operand.arg,
            };
            if (tctx.arg_shadow_ptr) |asp| {
                var si = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
                const asp_flat = b.buildInBoundsGEP(
                    llvm.arrayType(llvm.i64Type(), tctx.func.arg_count),
                    asp, &si, "asp",
                );
                callVoid4u(b, rt.put_arg, ctx_p, stk, sp, asp_flat, idx);
            }
        },

        // ========== Closure Variables ==========
        .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3, .get_var_ref,
        .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3, .put_var_ref,
        .set_var_ref0, .set_var_ref1, .set_var_ref2, .set_var_ref3, .set_var_ref,
        => {
            const idx: u32 = switch (instr.opcode) {
                .get_var_ref0, .put_var_ref0, .set_var_ref0 => 0,
                .get_var_ref1, .put_var_ref1, .set_var_ref1 => 1,
                .get_var_ref2, .put_var_ref2, .set_var_ref2 => 2,
                .get_var_ref3, .put_var_ref3, .set_var_ref3 => 3,
                else => instr.operand.var_ref,
            };
            const rt_fn = switch (instr.opcode) {
                .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3, .get_var_ref => rt.get_var_ref,
                .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3, .put_var_ref => rt.put_var_ref,
                else => rt.set_var_ref,
            };
            _ = b.buildCall(
                llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type(), llvm.i32Type() }, false),
                rt_fn, &.{ ctx_p, stk, sp, tctx.var_refs_param, llvm.constInt32(@intCast(idx)), tctx.closure_var_count_param }, "",
            );
        },

        // ========== Stack Operations ==========
        .dup => callVoid3(b, rt.dup_top, ctx_p, stk, sp),
        .dup1 => callVoid3(b, rt.dup1, ctx_p, stk, sp),
        .dup2 => callVoid3(b, rt.dup2, ctx_p, stk, sp),
        .dup3 => callVoid3(b, rt.dup3, ctx_p, stk, sp),
        .drop => callVoid3(b, rt.drop, ctx_p, stk, sp),
        .nip => callVoid3(b, rt.nip, ctx_p, stk, sp),
        .swap => callVoid2(b, rt.swap, stk, sp),

        // ========== Arithmetic ==========
        .add => callVoid3(b, rt.op_add, ctx_p, stk, sp),
        .sub => callVoid2(b, rt.op_sub, stk, sp),
        .mul => callVoid2(b, rt.op_mul, stk, sp),
        .div => callVoid2(b, rt.op_div, stk, sp),
        .mod => callVoid2(b, rt.op_mod, stk, sp),
        .pow => callVoid2(b, rt.op_pow, stk, sp),
        .neg => callVoid2(b, rt.op_neg, stk, sp),
        .plus => callVoid2(b, rt.op_plus, stk, sp),

        // ========== Bitwise ==========
        .@"and" => callVoid2(b, rt.op_band, stk, sp),
        .@"or" => callVoid2(b, rt.op_bor, stk, sp),
        .xor => callVoid2(b, rt.op_bxor, stk, sp),
        .shl => callVoid2(b, rt.op_shl, stk, sp),
        .sar => callVoid2(b, rt.op_sar, stk, sp),
        .shr => callVoid2(b, rt.op_shr, stk, sp),
        .not => callVoid2(b, rt.op_bnot, stk, sp),

        // ========== Comparison ==========
        .lt => callVoid2(b, rt.op_lt, stk, sp),
        .lte => callVoid2(b, rt.op_lte, stk, sp),
        .gt => callVoid2(b, rt.op_gt, stk, sp),
        .gte => callVoid2(b, rt.op_gte, stk, sp),
        .eq => callVoid3(b, rt.op_eq, ctx_p, stk, sp),
        .neq => callVoid3(b, rt.op_neq, ctx_p, stk, sp),
        .strict_eq => callVoid3(b, rt.op_strict_eq, ctx_p, stk, sp),
        .strict_neq => callVoid3(b, rt.op_strict_neq, ctx_p, stk, sp),

        // ========== Logical / Type ==========
        .lnot => callVoid3(b, rt.op_lnot, ctx_p, stk, sp),
        .typeof => callVoid3(b, rt.op_typeof, ctx_p, stk, sp),

        // ========== Increment / Decrement ==========
        .inc_loc, .dec_loc => {
            const rt_fn = if (instr.opcode == .inc_loc) rt.inc_loc else rt.dec_loc;
            _ = b.buildCall(
                llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.i32Type() }, false),
                rt_fn, &.{ locs, llvm.constInt32(instr.operand.loc) }, "",
            );
        },

        // ========== Function Calls (with error check) ==========
        .call0 => emitThinCallOp(tctx, rt.op_call, 0),
        .call1 => emitThinCallOp(tctx, rt.op_call, 1),
        .call2 => emitThinCallOp(tctx, rt.op_call, 2),
        .call3 => emitThinCallOp(tctx, rt.op_call, 3),
        .call, .tail_call => emitThinCallOp(tctx, rt.op_call, instr.operand.u16),
        .call_method, .tail_call_method => emitThinCallOp(tctx, rt.op_call_method, instr.operand.u16),
        .call_constructor => emitThinCallOp(tctx, rt.op_call_constructor, instr.operand.u16),

        // ========== Property Access (with error check) ==========
        .get_field => {
            emitThinFieldOp(tctx, rt.op_get_field_ic, instr);
        },
        .get_field2 => {
            emitThinFieldOp(tctx, rt.op_get_field2_ic, instr);
        },
        .put_field => {
            emitThinPutFieldOp(tctx, instr);
        },

        // ========== Apply ==========
        .apply => {
            const err_code = b.buildCall(
                llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                rt.op_apply, &.{ ctx_p, stk, sp }, "err",
            );
            emitErrorCheck(tctx, err_code);
        },

        // ========== Return ==========
        .@"return" => {
            _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{}, false), rt.exit_stack, &.{}, "");
            const ret = b.buildCall(
                llvm.functionType(llvm.i64Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i64Type(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i64Type() }, false),
                rt.returnFromStack,
                &.{
                    ctx_p, stk, sp, locs, llvm.constInt64(tctx.local_count),
                    llvm.constNull(llvm.ptrType()), llvm.constNull(llvm.ptrType()),
                    if (tctx.arg_shadow_ptr) |asp| blk: {
                        var si = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
                        break :blk b.buildInBoundsGEP(llvm.arrayType(llvm.i64Type(), tctx.func.arg_count), asp, &si, "asp");
                    } else llvm.constNull(llvm.ptrType()),
                    llvm.constInt64(if (tctx.has_put_arg) tctx.func.arg_count else 0),
                },
                "ret",
            );
            _ = b.buildRet(ret);
        },

        .return_undef => {
            _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{}, false), rt.exit_stack, &.{}, "");
            const ret = b.buildCall(
                llvm.functionType(llvm.i64Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.i64Type(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i64Type() }, false),
                rt.returnUndef,
                &.{
                    ctx_p, locs, llvm.constInt64(tctx.local_count),
                    llvm.constNull(llvm.ptrType()), llvm.constNull(llvm.ptrType()),
                    llvm.constNull(llvm.ptrType()), llvm.constInt64(0),
                },
                "ret",
            );
            _ = b.buildRet(ret);
        },

        // ========== No-op / Control flow handled by terminator ==========
        .nop, .set_name, .close_loc => {},
        .if_false, .if_false8, .if_true, .if_true8, .goto, .goto8, .goto16 => {},

        // ========== Throw — delegate to interpreter, then cleanup ==========
        .throw => {
            // Execute throw opcode via interpreter (sets pending exception)
            emitFallbackOpcode(tctx, instr);
            // After error check, the cont_bb is never reached for throw,
            // but LLVM requires a terminator. Branch to cleanup.
            _ = b.buildBr(tctx.cleanup_bb);
        },

        // ========== Array operations ==========
        .append => callVoid3(b, rt.op_append, ctx_p, stk, sp),
        .put_array_el => callVoid3(b, rt.op_put_array_el, ctx_p, stk, sp),

        // ========== Unsupported — use fallback ==========
        else => return CodegenError.UnsupportedOpcode,
    }
}

/// Emit error check: if (err != 0) goto cleanup
fn emitErrorCheck(tctx: *ThinCodegenCtx, err_code: llvm.Value) void {
    const b = tctx.builder;
    const is_err = b.buildICmp(c.LLVMIntNE, err_code, llvm.constInt32(0), "iserr");

    // Get current function
    const current_bb = c.LLVMGetInsertBlock(b.ref);
    const current_fn = c.LLVMGetBasicBlockParent(current_bb);

    const continue_bb = llvm.appendBasicBlock(current_fn, "cont");
    _ = b.buildCondBr(is_err, tctx.cleanup_bb, continue_bb);
    b.positionAtEnd(continue_bb);
}

/// Emit a call op with error check (call, call_method, call_constructor)
fn emitThinCallOp(tctx: *ThinCodegenCtx, func: llvm.Value, argc: u16) void {
    const b = tctx.builder;
    const err_code = b.buildCall(
        llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false),
        func, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, llvm.constInt32(@intCast(argc)) }, "err",
    );
    emitErrorCheck(tctx, err_code);
}

/// Emit get_field_ic / get_field2_ic with error check
fn emitThinFieldOp(tctx: *ThinCodegenCtx, func: llvm.Value, instr: Instruction) void {
    const b = tctx.builder;
    const atom_idx = instr.operand.atom;

    // Get atom string for this field access
    const name = getAtomStringStatic(tctx.func, atom_idx) orelse return;

    // Create global string for field name
    const current_bb = c.LLVMGetInsertBlock(b.ref);
    const current_fn = c.LLVMGetBasicBlockParent(current_bb);
    _ = current_fn;

    var str_label_buf: [64]u8 = undefined;
    const ic_idx = tctx.nextIcSlot();
    const str_label = std.fmt.bufPrintZ(&str_label_buf, ".field_{d}", .{ic_idx}) catch ".field";

    // Create the string constant
    const str_ptr = c.LLVMBuildGlobalStringPtr(b.ref, name, str_label.ptr);

    // For IC slots, we need a zeroed global (2 x i64). For simplicity, pass null IC for now.
    // TODO: allocate proper IC slot globals for better perf
    const err_code = b.buildCall(
        llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
        func, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, llvm.constNull(llvm.ptrType()), str_ptr }, "err",
    );
    emitErrorCheck(tctx, err_code);
}

/// Emit put_field with error check
fn emitThinPutFieldOp(tctx: *ThinCodegenCtx, instr: Instruction) void {
    const b = tctx.builder;
    const atom_idx = instr.operand.atom;
    const name = getAtomStringStatic(tctx.func, atom_idx) orelse return;

    var str_label_buf: [64]u8 = undefined;
    const str_label = std.fmt.bufPrintZ(&str_label_buf, ".put_field_{d}", .{tctx.ic_count}) catch ".pf";

    const str_ptr = c.LLVMBuildGlobalStringPtr(b.ref, name, str_label.ptr);
    const err_code = b.buildCall(
        llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
        tctx.rt.op_put_field, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, str_ptr }, "err",
    );
    emitErrorCheck(tctx, err_code);
}

/// Emit fallback: delegate to interpreter for unsupported opcodes
fn emitFallbackOpcode(tctx: *ThinCodegenCtx, instr: Instruction) void {
    const b = tctx.builder;
    const opcode_val = llvm.constInt(llvm.i8Type(), @intFromEnum(instr.opcode), false);
    const operand_val = llvm.constInt32(switch (instr.operand) {
        .none => 0,
        .i8 => |v| @as(i32, v),
        .i16 => |v| @as(i32, v),
        .i32 => |v| v,
        .u8 => |v| @as(i32, @intCast(v)),
        .u16 => |v| @as(i32, @intCast(v)),
        .atom => |v| @as(i32, @intCast(v)),
        .loc => |v| @as(i32, @intCast(v)),
        .arg => |v| @as(i32, @intCast(v)),
        .var_ref => |v| @as(i32, @intCast(v)),
        .const_idx => |v| @as(i32, @intCast(v)),
        .label => |v| v,
        else => 0,
    });

    const err_code = b.buildCall(
        llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.i8Type(), llvm.i32Type(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false),
        tctx.rt.exec_opcode, &.{ tctx.ctx_param, opcode_val, operand_val, tctx.stack_ptr, tctx.sp_ptr, tctx.locals_ptr, llvm.constInt32(@intCast(tctx.func.var_count)) }, "err",
    );
    emitErrorCheck(tctx, err_code);
}

/// Emit block terminator for thin codegen (branch/return)
fn emitThinBlockTerminator(
    tctx: *ThinCodegenCtx,
    block: CFGBasicBlock,
    block_id_ptr: llvm.Value,
    dispatch_bb: llvm.BasicBlock,
    llvm_blocks: []llvm.BasicBlock,
) void {
    const b = tctx.builder;
    const successors = block.successors.items;

    const last_op: Opcode = if (block.instructions.len > 0)
        block.instructions[block.instructions.len - 1].opcode
    else
        .nop;

    // Return already handled by instruction emitter
    if (last_op == .@"return" or last_op == .return_undef or last_op == .throw) return;

    // Conditional branches: pop CV from stack, convert to bool, branch
    if (last_op == .if_false or last_op == .if_false8 or last_op == .if_true or last_op == .if_true8) {
        if (successors.len >= 2) {
            // Pop the condition value from stack and test it
            // Load sp, decrement, load CV, test if truthy
            const sp_val = b.buildLoad(llvm.i64Type(), tctx.sp_ptr, "sp");
            const new_sp = b.buildSub(sp_val, llvm.constInt64(1), "sp1");
            _ = b.buildStore(new_sp, tctx.sp_ptr);

            // Load the CV value at stack[new_sp]
            const new_sp_i32 = b.buildTrunc(new_sp, llvm.i32Type(), "spi");
            var indices = [_]llvm.Value{new_sp_i32};
            const elem_ptr = b.buildInBoundsGEP(llvm.i64Type(), tctx.stack_ptr, &indices, "cvp");
            const cv_val = b.buildLoad(llvm.i64Type(), elem_ptr, "cv");

            // Quick boolean test: CV.FALSE = 0x7FFA_0000_0000_0000, CV.TRUE has lo=1
            // Simplified: check if value is "falsy" — CV with tag INT and value 0, or FALSE, or NULL, or UNDEFINED
            // For now use a simplified check: compare against known FALSE/NULL/UNDEFINED/INT(0)
            const cv_false = llvm.constInt64(0x7FFA_0000_0000_0000);
            const cv_null = llvm.constInt64(0x7FFB_0000_0000_0000);
            const cv_undef = llvm.constInt64(0x7FFC_0000_0000_0000);
            const cv_int0 = llvm.constInt64(0x7FFF_0000_0000_0000); // INT tag with value 0

            const is_false = b.buildICmp(c.LLVMIntEQ, cv_val, cv_false, "isf");
            const is_null = b.buildICmp(c.LLVMIntEQ, cv_val, cv_null, "isn");
            const is_undef = b.buildICmp(c.LLVMIntEQ, cv_val, cv_undef, "isu");
            const is_int0 = b.buildICmp(c.LLVMIntEQ, cv_val, cv_int0, "isz");

            var is_falsy = b.buildOr(is_false, is_null, "f1");
            is_falsy = b.buildOr(is_falsy, is_undef, "f2");
            is_falsy = b.buildOr(is_falsy, is_int0, "f3");

            if (last_op == .if_false or last_op == .if_false8) {
                // if_false: jump to successors[0] if false, successors[1] if true
                const false_target = successors[0];
                const true_target = successors[1];
                if (false_target < llvm_blocks.len and true_target < llvm_blocks.len) {
                    // Set block_id and branch
                    const false_target_bb = llvm_blocks[false_target];
                    const true_target_bb = llvm_blocks[true_target];
                    _ = block_id_ptr;
                    _ = dispatch_bb;
                    // Direct branch to target blocks
                    _ = b.buildCondBr(is_falsy, false_target_bb, true_target_bb);
                    return;
                }
            } else {
                // if_true: jump to successors[0] if true, successors[1] if false
                const true_target = successors[0];
                const false_target = successors[1];
                if (true_target < llvm_blocks.len and false_target < llvm_blocks.len) {
                    const is_truthy = b.buildICmp(c.LLVMIntEQ, is_falsy, llvm.constInt(llvm.i1Type(), 0, false), "truthy");
                    _ = b.buildCondBr(is_truthy, llvm_blocks[true_target], llvm_blocks[false_target]);
                    return;
                }
            }
        }
        // Fallthrough if successors don't match
        if (successors.len >= 1 and successors[0] < llvm_blocks.len) {
            _ = b.buildBr(llvm_blocks[successors[0]]);
            return;
        }
    }

    // Unconditional goto
    if (last_op == .goto or last_op == .goto8 or last_op == .goto16) {
        if (successors.len >= 1 and successors[0] < llvm_blocks.len) {
            _ = b.buildBr(llvm_blocks[successors[0]]);
            return;
        }
    }

    // Fall-through to next block or return
    if (successors.len == 1 and successors[0] < llvm_blocks.len) {
        _ = b.buildBr(llvm_blocks[successors[0]]);
    } else if (successors.len == 0) {
        // End of function — return undef
        _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{}, false), tctx.rt.exit_stack, &.{}, "");
        const ret = b.buildCall(
            llvm.functionType(llvm.i64Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.i64Type(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i64Type() }, false),
            tctx.rt.returnUndef,
            &.{
                tctx.ctx_param, tctx.locals_ptr, llvm.constInt64(tctx.local_count),
                llvm.constNull(llvm.ptrType()), llvm.constNull(llvm.ptrType()),
                llvm.constNull(llvm.ptrType()), llvm.constInt64(0),
            },
            "ret",
        );
        _ = b.buildRet(ret);
    } else if (successors.len >= 1 and successors[0] < llvm_blocks.len) {
        _ = b.buildBr(llvm_blocks[successors[0]]);
    } else {
        // Safety fallback: unreachable (should never get here in well-formed CFG)
        _ = b.buildUnreachable();
    }
}

/// Resolve atom index to null-terminated string at compile time
fn getAtomStringStatic(func: AnalyzedFunction, atom_idx: u32) ?[*:0]const u8 {
    if (atom_idx < module_parser.JS_ATOM_END) {
        if (atom_idx < module_parser.BUILTIN_ATOMS.len) {
            const name = module_parser.BUILTIN_ATOMS[atom_idx];
            if (name.len > 0 and (name.len < 1 or name[0] != '<')) {
                // BUILTIN_ATOMS are comptime string literals — always null-terminated
                return @ptrCast(name.ptr);
            }
        }
        return null;
    }
    const adjusted_idx = atom_idx - module_parser.JS_ATOM_END;
    if (adjusted_idx < func.atom_strings.len) {
        const str = func.atom_strings[adjusted_idx];
        if (str.len > 0) {
            // atom_strings are null-terminated from the parser
            return @ptrCast(str.ptr);
        }
    }
    return null;
}

// ============================================================================
// Thin codegen LLVM IR call helpers
// ============================================================================

/// Call void(ptr, ptr) — e.g., push_true(stack, sp)
fn callVoid2(b: llvm.Builder, func: llvm.Value, a: llvm.Value, bv: llvm.Value) void {
    _ = b.buildCall(
        llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType() }, false),
        func, &.{ a, bv }, "",
    );
}

/// Call void(ptr, ptr, ptr) — e.g., op_add(ctx, stack, sp)
fn callVoid3(b: llvm.Builder, func: llvm.Value, a: llvm.Value, bv: llvm.Value, cv: llvm.Value) void {
    _ = b.buildCall(
        llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
        func, &.{ a, bv, cv }, "",
    );
}

/// Call void(ptr, ptr, i32) — e.g., push_i32(stack, sp, val)
fn callVoid3i(b: llvm.Builder, func: llvm.Value, a: llvm.Value, bv: llvm.Value, ival: llvm.Value) void {
    _ = b.buildCall(
        llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false),
        func, &.{ a, bv, ival }, "",
    );
}

/// Call void(ctx, stack, sp, locals, idx) — e.g., get_loc(ctx, stack, sp, locals, 0)
fn callVoid4u(b: llvm.Builder, func: llvm.Value, ctx_p: llvm.Value, stk: llvm.Value, sp: llvm.Value, locs: llvm.Value, idx: u32) void {
    _ = b.buildCall(
        llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false),
        func, &.{ ctx_p, stk, sp, locs, llvm.constInt32(@intCast(idx)) }, "",
    );
}

