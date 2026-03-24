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
const CountedLoop = cfg_mod.CountedLoop;
const Allocator = std.mem.Allocator;
const Int32Pattern = int32_handlers.Int32Pattern;
const numeric_handlers = @import("numeric_handlers.zig");
const ValueKind = numeric_handlers.ValueKind;
const NumericPattern = numeric_handlers.NumericPattern;
const OpKind = numeric_handlers.OpKind;
const AnalyzedFunction = frozen_registry.AnalyzedFunction;

const c = llvm.c;

/// Map OpKind comparison to LLVM integer predicate.
fn intPred(op: OpKind) ?c.LLVMIntPredicate {
    return switch (op) {
        .lt => c.LLVMIntSLT,
        .lte => c.LLVMIntSLE,
        .gt => c.LLVMIntSGT,
        .gte => c.LLVMIntSGE,
        .eq => c.LLVMIntEQ,
        .neq => c.LLVMIntNE,
        else => null,
    };
}

/// Map OpKind comparison to LLVM float predicate.
fn realPred(op: OpKind) ?c.LLVMRealPredicate {
    return switch (op) {
        .lt => c.LLVMRealOLT,
        .lte => c.LLVMRealOLE,
        .gt => c.LLVMRealOGT,
        .gte => c.LLVMRealOGE,
        .eq => c.LLVMRealOEQ,
        .neq => c.LLVMRealONE,
        else => null,
    };
}

/// Target-dependent type information for frozen codegen.
/// On native x86_64: JSValue = {i64, i64} (16 bytes), usize = i64
/// On WASM32: JSValue = i64 (NaN-boxed, 8 bytes), usize = i32
pub const TargetInfo = struct {
    is_wasm32: bool = false,

    /// JSValue type for the target platform's C ABI.
    pub fn jsvalueType(self: TargetInfo) llvm.Type {
        if (self.is_wasm32) return llvm.i64Type(); // NaN-boxed uint64_t
        var fields = [_]llvm.Type{ llvm.i64Type(), llvm.i64Type() };
        return c.LLVMStructType(&fields, fields.len, 0);
    }

    /// CompressedValue type for stack/local slots.
    /// On native: CV = JSValue = {i64, i64} (16 bytes, zero-cost conversion).
    /// On WASM32: CV = NaN-boxed i64 (8 bytes).
    pub fn cvType(self: TargetInfo) llvm.Type {
        if (self.is_wasm32) return llvm.i64Type();
        return self.jsvalueType(); // CV = JSValue on native
    }

    /// usize type: i64 on native, i32 on WASM32
    pub fn usizeType(self: TargetInfo) llvm.Type {
        return if (self.is_wasm32) llvm.i32Type() else llvm.i64Type();
    }

    /// Create a usize constant
    pub fn constUsize(self: TargetInfo, val: u64) llvm.Value {
        return if (self.is_wasm32) llvm.constInt32(@intCast(val)) else llvm.constInt64(@intCast(val));
    }

    /// Create a JS_EXCEPTION return value
    pub fn makeException(self: TargetInfo, builder: llvm.Builder) llvm.Value {
        if (self.is_wasm32) {
            // NaN-boxed: JS_MKVAL(JS_TAG_EXCEPTION, 0) = (6 << 32)
            return llvm.constInt64(@as(u64, 6) << 32);
        }
        const jsv_ty = self.jsvalueType();
        var exc_val = c.LLVMGetUndef(jsv_ty);
        exc_val = c.LLVMBuildInsertValue(builder.ref, exc_val, llvm.constInt64(0), 0, "exc0");
        exc_val = c.LLVMBuildInsertValue(builder.ref, exc_val, llvm.constInt64(6), 1, "exc1");
        return exc_val;
    }
};

/// Default native target
pub const native_target = TargetInfo{ .is_wasm32 = false };

// Legacy wrapper for int32 tier (always native)
fn jsvalueType() llvm.Type {
    return native_target.jsvalueType();
}

// JS_TAG_INT on native = 0 (tag field)
const JS_TAG_INT_NATIVE: i64 = 0;

pub const CodegenError = error{
    UnsupportedOpcode,
    StackUnderflow,
    OutOfMemory,
    LLVMError,
    FormatError,
};

// ============================================================================
// CompressedValue (CV) NaN-boxing constants for inline LLVM IR
// ============================================================================

// On x86_64, CV is a 64-bit NaN-boxed value. The upper 16 bits encode the type tag.
pub const CV_QNAN_INT: u64 = 0x7FF9_0000_0000_0000; // integer tag
pub const CV_TAG_MASK: u64 = 0xFFFF_0000_0000_0000; // mask for type tag
pub const CV_QNAN_PTR: u64 = 0x7FFD_0000_0000_0000; // object pointer (TAG_PTR)
pub const CV_QNAN_STR: u64 = 0x7FFF_0000_0000_0000; // string
pub const CV_UNDEFINED: u64 = 0x7FFC_0000_0000_0000;
pub const CV_NULL: u64 = 0x7FFB_0000_0000_0000;
pub const CV_TRUE: u64 = 0x7FFA_0000_0000_0001;
pub const CV_FALSE: u64 = 0x7FFA_0000_0000_0000;
pub const CV_PAYLOAD_MASK: u64 = 0x0000_FFFF_FFFF_FFFF; // lower 48 bits
pub const CV_UNINITIALIZED: u64 = 0x7FFE_0000_0000_0000; // QNAN | TAG_UNINIT (TDZ sentinel)
// Negative NaN space ref types (sign bit set)
pub const CV_QNAN_SYMBOL: u64 = 0xFFF9_0000_0000_0000; // symbol pointer
pub const CV_QNAN_BIGINT: u64 = 0xFFFA_0000_0000_0000; // bigint pointer
pub const CV_QNAN_FUNCBC: u64 = 0xFFFB_0000_0000_0000; // function bytecode pointer

/// Create a CV integer constant at comptime: QNAN_INT | (val & 0xFFFFFFFF)
pub fn cvConstInt(val: i32) u64 {
    return CV_QNAN_INT | (@as(u64, @as(u32, @bitCast(val))));
}

// ============================================================================
// Inline LLVM IR helpers for CV operations
// ============================================================================

/// Load the stack pointer value: sp = *sp_ptr (usize: i64 native, i32 wasm32)
fn inlineLoadSp(b: llvm.Builder, sp_ptr: llvm.Value) llvm.Value {
    return b.buildLoad(llvm.i64Type(), sp_ptr, "sp");
}

/// Store the stack pointer value: *sp_ptr = sp
fn inlineStoreSp(b: llvm.Builder, sp_ptr: llvm.Value, sp_val: llvm.Value) void {
    _ = b.buildStore(sp_val, sp_ptr);
}

/// Get pointer to stack[index] where index is an i64 LLVM value
fn inlineStackSlot(b: llvm.Builder, stk: llvm.Value, index: llvm.Value) llvm.Value {
    return b.buildInBoundsGEP(llvm.i64Type(), stk, &.{index}, "slot");
}

// inlineDupRef/inlineFreeRef moved to llvm_thin_codegen.zig (only used by thin codegen)

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

/// Verify, optimize, and emit an LLVM module to an object file.
/// `opt_passes` controls the LLVM pass pipeline (e.g. "default<O1>" or "default<O2>").
/// Returns true to indicate the module was consumed (caller should not dispose).
pub fn finalizeAndEmitShard(
    native: anytype,
    obj_path: [*:0]const u8,
    label: [*:0]const u8,
    opt_passes: [*:0]const u8,
) CodegenError!bool {
    // Verify module
    native.module.verify() catch {
        const ir = native.module.printToString();
        defer llvm.disposeMessage(ir);
        std.debug.print("[{s}] Module verification failed. IR:\n{s}\n", .{ label, ir });
        return CodegenError.LLVMError;
    };

    // Dump IR before optimization (debug)
    if (std.posix.getenv("EDGEBOX_DUMP_IR")) |_| {
        const ir = native.module.printToString();
        defer llvm.disposeMessage(ir);
        std.debug.print("=== PRE-OPT IR ({s}) ===\n{s}\n=== END PRE-OPT ===\n", .{ label, ir });
    }

    // Run optimization passes (-O1 or -O2 depending on shard type)
    {
        const pass_opts = c.LLVMCreatePassBuilderOptions();
        defer c.LLVMDisposePassBuilderOptions(pass_opts);
        const err_ref = c.LLVMRunPasses(native.module.ref, opt_passes, native.tm.ref, pass_opts);
        if (err_ref) |err| {
            const msg = c.LLVMGetErrorMessage(err);
            if (msg) |m| {
                std.debug.print("[{s}] Optimization failed: {s}\n", .{ label, m });
                c.LLVMDisposeErrorMessage(m);
            }
        }
    }

    // Dump IR after optimization (debug)
    if (std.posix.getenv("EDGEBOX_DUMP_IR")) |_| {
        const ir2 = native.module.printToString();
        defer llvm.disposeMessage(ir2);
        std.debug.print("=== POST-OPT IR ({s}) ===\n{s}\n=== END POST-OPT ===\n", .{ label, ir2 });
    }

    // Emit to object file
    native.tm.emitToFile(native.module, obj_path) catch return CodegenError.LLVMError;
    return true; // module consumed
}

/// Generate standalone WASM module with pure numeric functions — no QuickJS runtime.
/// These functions are exported directly as WASM exports, callable from JS Workers.
/// Supports both i32 (pure integer) and f64 (float-capable) functions.
pub fn generateStandaloneWasm(
    allocator: Allocator,
    functions: []const ShardFunction,
    obj_path: [*:0]const u8,
) CodegenError!LLVMShardResult {
    const native = llvm.createWasmModule("standalone", c.LLVMCodeGenLevelAggressive) catch
        return CodegenError.LLVMError;
    defer native.tm.dispose();
    var module_disposed = false;
    defer if (!module_disposed) native.module.dispose();

    const builder = llvm.Builder.create(native.ctx);
    defer builder.dispose();

    // === Provide fmod for f64 tier (no libc in standalone WASM) ===
    // fmod(a, b) = a - trunc(a/b) * b, where trunc = fptosi + sitofp (toward zero)
    {
        var has_f64 = false;
        for (functions) |sf| {
            if (sf.value_kind == .f64) { has_f64 = true; break; }
        }
        if (has_f64) {
            const f64_ty = llvm.doubleType();
            var fmod_params = [_]llvm.Type{ f64_ty, f64_ty };
            const fmod_fn_ty = llvm.functionType(f64_ty, &fmod_params, false);
            const fmod_fn = native.module.addFunction("fmod", fmod_fn_ty);
            // Must be external linkage so LLVM's frem→fmod lowering can resolve it
            llvm.setLinkage(fmod_fn, c.LLVMExternalLinkage);
            const fmod_entry = llvm.appendBasicBlock(fmod_fn, "entry");
            builder.positionAtEnd(fmod_entry);
            const a = c.LLVMGetParam(fmod_fn, 0);
            const b = c.LLVMGetParam(fmod_fn, 1);
            const div = builder.buildFDiv(a, b, "div");
            // trunc toward zero: fptosi(f64→i64) then sitofp(i64→f64)
            const trunc_i64 = builder.buildFPToSI(div, llvm.i64Type(), "trunc_i");
            const truncated = builder.buildSIToFP(trunc_i64, f64_ty, "trunc_f");
            const mul = builder.buildFMul(truncated, b, "mul");
            const result = builder.buildFSub(a, mul, "fmod");
            _ = builder.buildRet(result);
        }
    }

    // === Pass 1: Declare all functions (so cross-function calls can resolve) ===
    // For functions that access arr.length, we add extra i32 params for each array length.
    var length_args_buf: [64]u8 = undefined; // per-function length_args bitmask
    // Synthetic names for anonymous functions (arrow/IIFE): "__anon_L{line_num}"
    var synth_names: [64][32]u8 = undefined;
    for (functions, 0..) |sf, fi| {
        const func = sf.func;

        // Generate synthetic name for anonymous functions (null-terminated for sliceTo)
        if (sf.name.len == 0 and fi < synth_names.len) {
            _ = std.fmt.bufPrintZ(&synth_names[fi], "__anon_L{d}", .{sf.func.line_num}) catch |err| {
                std.debug.print("[freeze] bufPrintZ for anon name failed: {}\n", .{err});
            };
        }

        const elem_type: llvm.Type = switch (sf.value_kind) {
            .i32 => llvm.i32Type(),
            .f64 => llvm.doubleType(),
        };

        // Detect which array args have .length accessed
        const length_args: u8 = if (fi < length_args_buf.len)
            numeric_handlers.detectLengthArgs(func.instructions, func.arg_count)
        else
            0;
        if (fi < length_args_buf.len) length_args_buf[fi] = length_args;

        var export_name_buf: [256]u8 = undefined;
        const func_name = if (sf.name.len == 0 and fi < synth_names.len)
            std.mem.sliceTo(&synth_names[fi], 0)
        else
            sf.name;
        const export_name = std.fmt.bufPrintZ(&export_name_buf, "{s}", .{func_name}) catch continue;

        // Count extra length params (one per array arg with .length)
        var extra_len_count: u32 = 0;
        for (0..func.arg_count) |i| {
            if (length_args & (@as(u8, 1) << @intCast(i)) != 0) extra_len_count += 1;
        }

        var param_types_buf: [16]llvm.Type = undefined;
        for (0..func.arg_count) |i| {
            param_types_buf[i] = elem_type;
        }
        // Append i32 length params for array args with .length
        for (0..extra_len_count) |i| {
            param_types_buf[func.arg_count + i] = llvm.i32Type();
        }
        const total_params = func.arg_count + extra_len_count;
        const fn_ty = llvm.functionType(elem_type, param_types_buf[0..total_params], false);
        llvm.setLinkage(native.module.addFunction(export_name, fn_ty), c.LLVMExternalLinkage);
    }

    // === Pass 2: Generate bodies (all callees already declared) ===
    var generated_count: u32 = 0;
    for (functions, 0..) |sf, fi| {
        const func = sf.func;
        const length_args: u8 = if (fi < length_args_buf.len) length_args_buf[fi] else 0;

        var export_name_buf: [256]u8 = undefined;
        const func_name2 = if (sf.name.len == 0 and fi < synth_names.len)
            std.mem.sliceTo(&synth_names[fi], 0)
        else
            sf.name;
        const export_name = std.fmt.bufPrintZ(&export_name_buf, "{s}", .{func_name2}) catch continue;
        const standalone_fn = native.module.getNamedFunction(export_name) orelse continue;

        // Generate body using generic numeric codegen (handles i32, f64, array access, and array length)
        const gen_ok = switch (sf.value_kind) {
            .i32 => blk: {
                generateNumericBody(.i32, allocator, builder, standalone_fn, func, sf.cfg, native.module, length_args, sf.struct_layout) catch |err| {
                    std.debug.print("[standalone-wasm] {s}: codegen failed: {}\n", .{ sf.name, err });
                    break :blk false;
                };
                break :blk true;
            },
            .f64 => blk: {
                generateNumericBody(.f64, allocator, builder, standalone_fn, func, sf.cfg, native.module, length_args, sf.struct_layout) catch |err| {
                    std.debug.print("[standalone-wasm] {s}: codegen failed: {}\n", .{ sf.name, err });
                    break :blk false;
                };
                break :blk true;
            },
        };

        if (!gen_ok) {
            // Clean up failed function — replace with valid stub
            while (c.LLVMGetFirstBasicBlock(standalone_fn)) |bb| {
                while (c.LLVMGetFirstInstruction(bb)) |inst| {
                    c.LLVMInstructionEraseFromParent(inst);
                }
                c.LLVMDeleteBasicBlock(bb);
            }
            const fb = llvm.appendBasicBlock(standalone_fn, "entry");
            builder.positionAtEnd(fb);
            _ = builder.buildRet(switch (sf.value_kind) {
                .i32 => llvm.constInt32(0),
                .f64 => llvm.constF64(0.0),
            });
            continue;
        }

        generated_count += 1;
    }

    if (generated_count == 0) {
        return .{ .func_count = 0, .has_functions = false };
    }

    // === Pass 3: Generate _batch wrappers for struct functions ===
    // For struct functions, generate a batch variant that takes (pool, count, ...scalars)
    // and loops internally, calling the original per-element. LLVM inlines the callee
    // at O3, producing a tight loop over contiguous memory.
    for (functions, 0..) |sf, fi| {
        const si = sf.struct_layout orelse continue;
        const func = sf.func;

        // Compute struct stride (total fields across all struct args)
        var struct_stride: u32 = 0;
        var struct_arg_idx: u3 = 0;
        for (0..@min(func.arg_count, 8)) |ai| {
            if (si.struct_args & (@as(u8, 1) << @intCast(ai)) != 0) {
                struct_stride += si.field_counts[ai];
                struct_arg_idx = @intCast(ai);
                break; // batch over first struct arg only
            }
        }
        if (struct_stride == 0) continue;

        const func_name2 = if (sf.name.len == 0 and fi < synth_names.len)
            std.mem.sliceTo(&synth_names[fi], 0)
        else
            sf.name;

        // Look up the original function
        var orig_name_buf: [256]u8 = undefined;
        const orig_name = std.fmt.bufPrintZ(&orig_name_buf, "{s}", .{func_name2}) catch continue;
        const orig_fn = native.module.getNamedFunction(orig_name) orelse continue;

        // Count scalar (non-struct) args
        var scalar_count: u32 = 0;
        for (0..func.arg_count) |ai| {
            if (si.struct_args & (@as(u8, 1) << @intCast(ai)) == 0) scalar_count += 1;
        }

        // Batch function: (pool_ptr: i32, count: i32, ...scalar_args: elem_type) → elem_type
        const elem_type: llvm.Type = switch (sf.value_kind) {
            .i32 => llvm.i32Type(),
            .f64 => llvm.doubleType(),
        };
        var batch_params: [16]llvm.Type = undefined;
        batch_params[0] = llvm.i32Type(); // pool_ptr
        batch_params[1] = llvm.i32Type(); // count
        var bp_idx: u32 = 2;
        for (0..func.arg_count) |ai| {
            if (si.struct_args & (@as(u8, 1) << @intCast(ai)) == 0) {
                batch_params[bp_idx] = elem_type;
                bp_idx += 1;
            }
        }
        const batch_fn_ty = llvm.functionType(elem_type, batch_params[0..bp_idx], false);

        var batch_name_buf: [256]u8 = undefined;
        const batch_name = std.fmt.bufPrintZ(&batch_name_buf, "{s}_batch", .{func_name2}) catch continue;
        const batch_fn = native.module.addFunction(batch_name, batch_fn_ty);
        llvm.setLinkage(batch_fn, c.LLVMExternalLinkage);

        // Function body: if (count <= 0) return 0; sum = 0; for (i = 0; i < count; i++) sum += orig(pool + i*stride, scalars...); return sum;
        const entry_bb = llvm.appendBasicBlock(batch_fn, "entry");
        const loop_bb = llvm.appendBasicBlock(batch_fn, "loop");
        const exit_bb = llvm.appendBasicBlock(batch_fn, "exit");

        builder.positionAtEnd(entry_bb);
        const pool_param = c.LLVMGetParam(batch_fn, 0);
        const count_param = c.LLVMGetParam(batch_fn, 1);
        // Guard: skip loop if count <= 0
        const has_items = c.LLVMBuildICmp(builder.ref, c.LLVMIntSGT, count_param, llvm.constInt32(0), "has_items");
        _ = builder.buildCondBr(has_items, loop_bb, exit_bb);

        builder.positionAtEnd(loop_bb);
        // PHI nodes: i (i32), sum (elem_type)
        const phi_i = c.LLVMBuildPhi(builder.ref, llvm.i32Type(), "i");
        const phi_sum = c.LLVMBuildPhi(builder.ref, elem_type, "sum");

        // ptr = pool + i * stride * 4
        const stride_bytes = llvm.constInt32(@intCast(struct_stride * 4));
        const offset = builder.buildMul(phi_i, stride_bytes, "off");
        const ptr = builder.buildAdd(pool_param, offset, "ptr");

        // Build call args: struct arg gets ptr, scalars get batch params
        var call_args: [16]llvm.Value = undefined;
        var ca_idx: u32 = 0;
        var scalar_param_idx: u32 = 2;
        for (0..func.arg_count) |ai| {
            if (si.struct_args & (@as(u8, 1) << @intCast(ai)) != 0) {
                // Struct arg → pointer (i32 in i32 tier, convert for f64 tier)
                call_args[ca_idx] = switch (sf.value_kind) {
                    .i32 => ptr,
                    .f64 => builder.buildSIToFP(ptr, llvm.doubleType(), "ptr_f64"),
                };
            } else {
                // Scalar arg → pass through from batch params
                call_args[ca_idx] = c.LLVMGetParam(batch_fn, @intCast(scalar_param_idx));
                scalar_param_idx += 1;
            }
            ca_idx += 1;
        }

        // Call original function
        const orig_fn_ty = c.LLVMGlobalGetValueType(orig_fn);
        const call_result = builder.buildCall(orig_fn_ty, orig_fn, call_args[0..ca_idx], "r");

        // sum += result
        const new_sum = switch (sf.value_kind) {
            .i32 => builder.buildAdd(phi_sum, call_result, "sum_next"),
            .f64 => builder.buildFAdd(phi_sum, call_result, "sum_next"),
        };

        // i++
        const new_i = builder.buildAdd(phi_i, llvm.constInt32(1), "i_next");

        // Loop condition: i < count
        const cond = c.LLVMBuildICmp(builder.ref, c.LLVMIntSLT, new_i, count_param, "cond");
        _ = builder.buildCondBr(cond, loop_bb, exit_bb);

        // Wire PHI incoming values
        var phi_i_vals = [_]llvm.Value{ llvm.constInt32(0), new_i };
        var phi_i_bbs = [_]llvm.BasicBlock{ entry_bb, loop_bb };
        c.LLVMAddIncoming(phi_i, &phi_i_vals, &phi_i_bbs, 2);

        const zero_sum: llvm.Value = switch (sf.value_kind) {
            .i32 => llvm.constInt32(0),
            .f64 => llvm.constF64(0.0),
        };
        var phi_sum_vals = [_]llvm.Value{ zero_sum, new_sum };
        var phi_sum_bbs = [_]llvm.BasicBlock{ entry_bb, loop_bb };
        c.LLVMAddIncoming(phi_sum, &phi_sum_vals, &phi_sum_bbs, 2);

        // Exit: PHI merges zero (from entry guard) and final sum (from loop)
        builder.positionAtEnd(exit_bb);
        const exit_phi = c.LLVMBuildPhi(builder.ref, elem_type, "result");
        var exit_vals = [_]llvm.Value{ zero_sum, new_sum };
        var exit_bbs = [_]llvm.BasicBlock{ entry_bb, loop_bb };
        c.LLVMAddIncoming(exit_phi, &exit_vals, &exit_bbs, 2);
        _ = builder.buildRet(exit_phi);

        generated_count += 1;
    }

    // Dump pre-optimization IR for debugging
    if (std.posix.getenv("EDGEBOX_DUMP_WASM_IR")) |_| {
        const ir = native.module.printToString();
        defer llvm.disposeMessage(ir);
        std.debug.print("=== STANDALONE WASM IR (pre-opt) ===\n{s}\n=== END ===\n", .{ir});
    }
    // Use O3 for standalone WASM — enables IndVarSimplify + LoopVectorize for float loops
    module_disposed = try finalizeAndEmitShard(native, obj_path, "standalone-wasm", "default<O3>");
    return .{ .func_count = generated_count, .has_functions = true };
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

    const register_fn = declareNativeDispatchRegisterByIndex(native.module);

    // Generate each function
    var generated_count: u32 = 0;
    var generated_infos = std.ArrayListUnmanaged(GeneratedFuncInfo){};
    defer generated_infos.deinit(allocator);

    for (functions) |sf| {
        if (generateInt32Function(allocator, native.module, builder, sf)) |_| {
            generated_infos.append(allocator, .{
                .name = sf.name,
                .func_index = sf.func_index,
                .parser_index = sf.parser_index,
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

    module_disposed = try finalizeAndEmitShard(native, obj_path, "llvm", "default<O1>");
    return .{ .func_count = generated_count, .has_functions = true };
}

/// Information about a function to generate in a shard
pub const ShardFunction = struct {
    /// Original JS function name (for debug/naming)
    name: []const u8,
    /// Function index in the analysis (for LLVM symbol naming)
    func_index: usize,
    /// Parser index for index-based dispatch (matches JS_ReadFunctionTag counter)
    parser_index: u32 = 0,
    /// Line number (for debug output)
    line_num: u32,
    /// Sanitized LLVM function name (e.g., "__frozen_42_isDigit")
    llvm_func_name: []const u8,
    /// The analyzed function data
    func: AnalyzedFunction,
    /// Pre-built CFG
    cfg: *const CFG,
    /// Numeric tier: i32 (pure integer) or f64 (float-capable)
    value_kind: ValueKind = .i32,
    /// Struct arg layout info (null = no struct args, all args are scalar/array)
    struct_layout: ?*const numeric_handlers.StructArgInfo = null,
};

pub const GeneratedFuncInfo = struct {
    name: []const u8,
    func_index: usize,
    parser_index: u32 = 0,
    line_num: u32,
    llvm_func_name: []const u8,
    has_tail_call: bool = false,
};

// ============================================================================
// External function declarations
// ============================================================================

pub fn declareNativeDispatchRegisterByIndex(module: llvm.Module) llvm.Value {
    // void native_dispatch_register_by_index(u32 idx, FrozenFnPtr func)
    const param_types = [_]llvm.Type{ llvm.i32Type(), llvm.ptrType() };
    const fn_ty = llvm.functionType(llvm.voidType(), &param_types, false);
    const func = module.addFunction("native_dispatch_register_by_index", fn_ty);
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
    generateInt32Body(allocator, builder, hot_fn, func, cfg, module) catch |err| {
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
    // {i64,i64} @__frozen_N_name(ptr %ctx, {i64,i64} %this, i32 %argc, ptr %argv, ptr %var_refs, i32 %closure_cnt, ptr %cpool)
    // JSValue on native is 16-byte struct {JSValueUnion, tag} — must match C ABI
    var wrapper_name_buf: [512]u8 = undefined;
    const wrapper_name = std.fmt.bufPrintZ(&wrapper_name_buf, "{s}", .{sf.llvm_func_name}) catch
        return CodegenError.FormatError;

    const jsv_ty = jsvalueType();
    const wrapper_params = [_]llvm.Type{
        llvm.ptrType(), // ctx
        jsv_ty, // this_val: JSValue = {i64, i64}
        llvm.i32Type(), // argc
        llvm.ptrType(), // argv
        llvm.ptrType(), // var_refs
        llvm.i32Type(), // closure_var_count
        llvm.ptrType(), // cpool
    };
    const wrapper_fn_ty = llvm.functionType(jsv_ty, &wrapper_params, false);
    const wrapper_fn = module.addFunction(wrapper_name, wrapper_fn_ty);
    // Internal linkage: avoid symbol collision with Zig-compiled versions.
    // Init function references these by pointer within the same .o file.
    llvm.setLinkage(wrapper_fn, c.LLVMInternalLinkage);
    llvm.setFunctionCallConv(wrapper_fn, c.LLVMCCallConv);

    // Build wrapper body
    const entry = llvm.appendBasicBlock(wrapper_fn, "entry");
    builder.positionAtEnd(entry);

    // Extract int32 arguments from argv: each argv[i] is a JSValue {i64, i64}
    // The int32 value is in field 0 (JSValueUnion.int32 = low 32 bits of first i64)
    const argv_param = c.LLVMGetParam(wrapper_fn, 3); // argv: ptr
    const argc_param = c.LLVMGetParam(wrapper_fn, 2); // argc: i32

    var call_args_buf: [8]llvm.Value = undefined;
    for (0..func.arg_count) |i| {
        // Check if i < argc
        const idx = llvm.constInt32(@intCast(i));
        const in_bounds = builder.buildICmp(c.LLVMIntSLT, idx, argc_param, "inb");

        // Load argv[i] as JSValue {i64, i64} — stride is 16 bytes
        var gep_indices = [_]llvm.Value{llvm.constInt(llvm.i64Type(), i, false)};
        const argv_ptr = builder.buildInBoundsGEP(jsv_ty, argv_param, &gep_indices, "argp");
        // Load field 0 (JSValueUnion) as i64, then truncate to i32
        var field_indices = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
        const union_ptr = builder.buildInBoundsGEP(jsv_ty, argv_ptr, &field_indices, "up");
        const argv_val = builder.buildLoad(llvm.i64Type(), union_ptr, "argv");

        // Extract int32: truncate i64 to i32 (low 32 bits contain the int value)
        const arg_i32 = builder.buildTrunc(argv_val, llvm.i32Type(), "argi");

        // Select: if in bounds use arg, else 0
        const zero_i32 = llvm.constInt32(0);
        call_args_buf[i] = builder.buildSelect(in_bounds, arg_i32, zero_i32, "arg");
    }

    // Call hot function
    const call_args = call_args_buf[0..func.arg_count];
    const result = builder.buildCall(hot_fn_ty, hot_fn, call_args, "res");

    // Box result as native JSValue: { .u = { .int32 = result }, .tag = JS_TAG_INT }
    // Native JSValue is { i64, i64 } where field 0 = union value, field 1 = tag
    // JS_TAG_INT on native = 0
    const result_i64 = builder.buildSExt(result, llvm.i64Type(), "res64");
    var jsval = c.LLVMGetUndef(jsv_ty);
    jsval = c.LLVMBuildInsertValue(builder.ref, jsval, result_i64, 0, "jsv0");
    jsval = c.LLVMBuildInsertValue(builder.ref, jsval, llvm.constInt64(JS_TAG_INT_NATIVE), 1, "jsv1");

    _ = builder.buildRet(jsval);
}

/// Generate the body of an int32 hot function using LLVM IR.
/// Uses switch-based block dispatch matching the Zig codegen pattern.
fn generateInt32Body(
    allocator: Allocator,
    builder: llvm.Builder,
    func: llvm.Value,
    analyzed: AnalyzedFunction,
    cfg: *const CFG,
    module: llvm.Module,
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
    var locals_buf: [256]llvm.Value = undefined;
    const local_count = analyzed.var_count;
    for (0..@min(local_count, 256)) |i| {
        var loc_name: [16]u8 = undefined;
        const name = std.fmt.bufPrintZ(&loc_name, "loc_{d}", .{i}) catch "loc";
        locals_buf[i] = builder.buildAlloca(llvm.i32Type(), name);
        _ = builder.buildStore(llvm.constInt32(0), locals_buf[i]);
    }

    // Shadow function parameters with allocas so set_arg can store to them.
    // LLVM function params are immutable SSA values — can't buildStore to them.
    // mem2reg will promote these back to SSA if they're never stored to.
    var params_buf: [8]llvm.Value = undefined;
    for (0..analyzed.arg_count) |i| {
        var arg_name: [16]u8 = undefined;
        const name = std.fmt.bufPrintZ(&arg_name, "arg_{d}", .{i}) catch "arg";
        params_buf[i] = builder.buildAlloca(llvm.i32Type(), name);
        _ = builder.buildStore(c.LLVMGetParam(func, @intCast(i)), params_buf[i]);
    }

    // Generate each block
    for (blocks, 0..) |block, block_idx| {
        builder.positionAtEnd(llvm_blocks[block_idx]);

        // Value stack for this block (SSA values)
        var vstack = std.ArrayListUnmanaged(llvm.Value){};
        defer vstack.deinit(allocator);

        var block_terminated = false;
        var pending_callee: ?llvm.Value = null;

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
                &pending_callee,
                module,
                analyzed,
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
    locals: *[256]llvm.Value,
    local_count: u32,
    llvm_blocks: []llvm.BasicBlock,
    block_count: usize,
    successors: []const u32,
    func: llvm.Value,
    block_terminated: *bool,
    pending_callee: *?llvm.Value,
    module: llvm.Module,
    analyzed: AnalyzedFunction,
) CodegenError!void {
    const handler = int32_handlers.getInt32Handler(instr.opcode);

    // Helper: pop from int32 value stack (unwraps the ?T → T since we check len before calling)
    const i32VstackPop = struct {
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

        .push_cpool_i32 => {
            // Load constant from constant pool at compile time.
            // Large hex constants (e.g., 0xEDB88320) are stored as float64 in
            // QuickJS cpool because they exceed i32 range. We reinterpret the
            // lower 32 bits as i32 (matching JavaScript's ToInt32 semantics).
            const idx: u32 = switch (instr.operand) {
                .const_idx => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (idx >= analyzed.constants.len) return CodegenError.UnsupportedOpcode;
            const val: i32 = switch (analyzed.constants[idx]) {
                .int32 => |v| v,
                .float64 => |v| blk: {
                    // Match JS ToInt32: truncate f64 to i32 via u32 (wrapping)
                    const as_i64: i64 = @intFromFloat(v);
                    break :blk @as(i32, @truncate(as_i64));
                },
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
                else => return CodegenError.UnsupportedOpcode,
            };
            if (idx >= arg_count) return CodegenError.UnsupportedOpcode;
            const val = builder.buildLoad(llvm.i32Type(), params[idx], "arg");
            vstack.append(allocator, val) catch return CodegenError.OutOfMemory;
        },

        .put_arg_i32 => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .arg => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= arg_count) return CodegenError.UnsupportedOpcode;
            const val = i32VstackPop(vstack);
            _ = builder.buildStore(val, params.*[idx]);
        },

        .get_loc_i32 => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (idx >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            const loc_ptr = locals.*[idx];
            const val = builder.buildLoad(llvm.i32Type(), loc_ptr, "loc");
            vstack.append(allocator, val) catch return CodegenError.OutOfMemory;
        },

        .put_loc_i32 => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            const val = i32VstackPop(vstack);
            _ = builder.buildStore(val, locals.*[idx]);
        },

        .set_loc_i32 => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            const val = vstack.items[vstack.items.len - 1]; // peek, don't pop
            _ = builder.buildStore(val, locals.*[idx]);
        },

        .binary_arith_i32 => {
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const rhs = i32VstackPop(vstack);
            const lhs = i32VstackPop(vstack);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;

            const result = switch (op) {
                .add => builder.buildAdd(lhs, rhs, "add"),
                .sub => builder.buildSub(lhs, rhs, "sub"),
                .mul => mul_blk: {
                    // JS mul is f64: large products lose precision in float64,
                    // so ToInt32(f64_mul(a,b)) differs from i32_wrap(a*b).
                    // fptosi to i64 first (safe: i32*i32 max ~2^62 < 2^63),
                    // then trunc to i32 = low 32 bits = JS ToInt32() wrapping.
                    const fa = builder.buildSIToFP(lhs, llvm.doubleType(), "fma");
                    const fb = builder.buildSIToFP(rhs, llvm.doubleType(), "fmb");
                    const fprod = builder.buildFMul(fa, fb, "fmprod");
                    const i64_prod = builder.buildFPToSI(fprod, llvm.i64Type(), "mul64");
                    break :mul_blk builder.buildTrunc(i64_prod, llvm.i32Type(), "mul32");
                },
                .div => builder.buildSDiv(lhs, rhs, "div"),
                .mod => builder.buildSRem(lhs, rhs, "rem"),
                else => return CodegenError.UnsupportedOpcode,
            };

            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .bitwise_binary_i32 => {
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const rhs = i32VstackPop(vstack);
            const lhs = i32VstackPop(vstack);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;

            const result = switch (op) {
                .band => builder.buildAnd(lhs, rhs, "and"),
                .bor => builder.buildOr(lhs, rhs, "or"),
                .bxor => builder.buildXor(lhs, rhs, "xor"),
                .shl => builder.buildShl(lhs, rhs, "shl"),
                .sar => builder.buildAShr(lhs, rhs, "shr"),
                .shr => builder.buildLShr(lhs, rhs, "shru"),
                else => return CodegenError.UnsupportedOpcode,
            };

            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .binary_cmp_i32 => {
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const rhs = i32VstackPop(vstack);
            const lhs = i32VstackPop(vstack);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;

            const pred: c.LLVMIntPredicate = switch (op) {
                .lt => c.LLVMIntSLT,
                .lte => c.LLVMIntSLE,
                .gt => c.LLVMIntSGT,
                .gte => c.LLVMIntSGE,
                .eq => c.LLVMIntEQ,
                .neq => c.LLVMIntNE,
                else => return CodegenError.UnsupportedOpcode,
            };

            const cmp = builder.buildICmp(pred, lhs, rhs, "cmp");
            // Zero-extend i1 to i32 (false=0, true=1)
            const result = builder.buildZExt(cmp, llvm.i32Type(), "cmpz");
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .unary_i32 => {
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const val = i32VstackPop(vstack);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;

            const result = switch (op) {
                .neg => builder.buildNeg(val, "neg"),
                .bnot => builder.buildNot(val, "bnot"),
                else => return CodegenError.UnsupportedOpcode,
            };

            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .lnot_i32 => {
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const val = i32VstackPop(vstack);
            // Logical NOT: 0 → 1, nonzero → 0
            const is_zero = builder.buildICmp(c.LLVMIntEQ, val, llvm.constInt32(0), "iszero");
            const result = builder.buildZExt(is_zero, llvm.i32Type(), "lnot");
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .get_loc_check_i32 => {
            // Same as get_loc — in int32 context, locals are always initialized
            const idx: u32 = switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (idx >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            const loc_ptr = locals.*[idx];
            const val = builder.buildLoad(llvm.i32Type(), loc_ptr, "loc");
            vstack.append(allocator, val) catch return CodegenError.OutOfMemory;
        },

        .put_loc_check_i32 => {
            // Same as put_loc — const check irrelevant in int32 context
            const idx: u32 = switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            const val = i32VstackPop(vstack);
            _ = builder.buildStore(val, locals.*[idx]);
        },

        .set_loc_uninitialized_i32 => {
            // In int32 context, "uninitialized" = 0
            const idx: u32 = switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (idx >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            _ = builder.buildStore(llvm.constInt32(0), locals.*[idx]);
        },

        .swap_i32 => {
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const a = vstack.items[vstack.items.len - 1];
            const b = vstack.items[vstack.items.len - 2];
            vstack.items[vstack.items.len - 1] = b;
            vstack.items[vstack.items.len - 2] = a;
        },

        .nop_i32 => {},

        .set_arg_i32 => {
            // set_arg: store to arg slot, keep value on stack
            const idx: u32 = if (handler.index) |i| i else switch (instr.operand) {
                .arg => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= arg_count) return CodegenError.UnsupportedOpcode;
            const val = vstack.items[vstack.items.len - 1]; // peek, don't pop
            _ = builder.buildStore(val, params.*[idx]);
        },

        .add_loc_i32 => {
            // add_loc: local[N] += pop()
            const idx: u32 = switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            const val = i32VstackPop(vstack);
            const loc_ptr = locals.*[idx];
            const old_val = builder.buildLoad(llvm.i32Type(), loc_ptr, "addloc_old");
            const new_val = builder.buildAdd(old_val, val, "addloc_new");
            _ = builder.buildStore(new_val, loc_ptr);
        },

        .inc_loc_i32 => {
            // inc_loc: local[N]++
            const idx: u32 = switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (idx >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            const loc_ptr = locals.*[idx];
            const old_val = builder.buildLoad(llvm.i32Type(), loc_ptr, "incloc_old");
            const new_val = builder.buildNSWAdd(old_val, llvm.constInt32(1), "incloc_new");
            _ = builder.buildStore(new_val, loc_ptr);
        },

        .dec_loc_i32 => {
            // dec_loc: local[N]--
            const idx: u32 = switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (idx >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            const loc_ptr = locals.*[idx];
            const old_val = builder.buildLoad(llvm.i32Type(), loc_ptr, "decloc_old");
            const new_val = builder.buildNSWSub(old_val, llvm.constInt32(1), "decloc_new");
            _ = builder.buildStore(new_val, loc_ptr);
        },

        .inc_dec_i32 => {
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const val = i32VstackPop(vstack);
            const one = llvm.constInt32(1);
            const result = if (handler.is_inc orelse false)
                builder.buildAdd(val, one, "inc")
            else
                builder.buildSub(val, one, "dec");
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .stack_op_i32 => {
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;
            switch (op) {
                .dup => {
                    if (vstack.items.len < 1) return CodegenError.StackUnderflow;
                    const top = vstack.items[vstack.items.len - 1];
                    vstack.append(allocator, top) catch return CodegenError.OutOfMemory;
                },
                .drop => {
                    if (vstack.items.len < 1) return CodegenError.StackUnderflow;
                    _ = i32VstackPop(vstack);
                },
                else => return CodegenError.UnsupportedOpcode,
            }
        },

        .self_ref_i32 => {
            // Function reference marker. Resolve the callee for cross-function calls.
            // - get_var/get_var_undef: atom operand → look up by name in module
            // - get_var_ref0: closure var[0].name → look up by name in module
            if (instr.opcode == .get_var or instr.opcode == .get_var_undef) {
                switch (instr.operand) {
                    .atom => |atom_idx| {
                        if (getAtomStringStatic(analyzed, atom_idx)) |callee_name| {
                            if (module.getNamedFunction(callee_name)) |callee_fn| {
                                if (callee_fn != func) {
                                    pending_callee.* = callee_fn;
                                }
                            }
                        }
                    },
                    else => {},
                }
            } else if (instr.opcode == .get_var_ref0) {
                // Closure variable ref — look up captured function by name
                if (analyzed.closure_vars.len > 0) {
                    const cv_name = analyzed.closure_vars[0].name;
                    if (cv_name.len > 0) {
                        // closure_vars names are slices, need null-terminated for LLVM lookup
                        var name_buf: [256]u8 = undefined;
                        if (cv_name.len < name_buf.len) {
                            @memcpy(name_buf[0..cv_name.len], cv_name);
                            name_buf[cv_name.len] = 0;
                            const name_z: [*:0]const u8 = @ptrCast(name_buf[0..cv_name.len]);
                            if (module.getNamedFunction(name_z)) |callee_fn| {
                                if (callee_fn != func) {
                                    pending_callee.* = callee_fn;
                                }
                            }
                        }
                    }
                }
            }
        },

        .call_self_i32 => {
            // Function call — self-recursive or cross-function
            const call_argc: u32 = switch (instr.opcode) {
                .call0 => 0,
                .call1 => 1,
                .call2 => 2,
                .call3 => 3,
                .call => instr.operand.u16,
                else => 1,
            };

            if (vstack.items.len < call_argc) return CodegenError.StackUnderflow;

            // Determine call target: cross-function callee or self
            const call_target = if (pending_callee.*) |callee| blk: {
                pending_callee.* = null;
                // Verify arg count matches callee's param count
                if (c.LLVMCountParams(callee) == call_argc) break :blk callee;
                break :blk func; // Mismatch — fall back to self
            } else func;

            // Pop args in reverse order
            var call_args: [8]llvm.Value = undefined;
            var i: u32 = call_argc;
            while (i > 0) {
                i -= 1;
                call_args[i] = i32VstackPop(vstack);
            }

            // Build the call
            var param_types_buf: [8]llvm.Type = undefined;
            for (0..call_argc) |j| {
                param_types_buf[j] = llvm.i32Type();
            }
            const call_fn_ty = llvm.functionType(llvm.i32Type(), param_types_buf[0..call_argc], false);
            const args_slice = call_args[0..call_argc];
            const result = builder.buildCall(call_fn_ty, call_target, args_slice, "call");
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .tail_call_self_i32 => {
            // Tail call — self-recursive or cross-function
            const call_argc: u32 = instr.operand.u16;

            if (vstack.items.len < call_argc) return CodegenError.StackUnderflow;

            const call_target = if (pending_callee.*) |callee| blk: {
                pending_callee.* = null;
                if (c.LLVMCountParams(callee) == call_argc) break :blk callee;
                break :blk func;
            } else func;

            var call_args: [8]llvm.Value = undefined;
            var i: u32 = call_argc;
            while (i > 0) {
                i -= 1;
                call_args[i] = i32VstackPop(vstack);
            }

            var param_types_buf: [8]llvm.Type = undefined;
            for (0..call_argc) |j| {
                param_types_buf[j] = llvm.i32Type();
            }
            const call_fn_ty = llvm.functionType(llvm.i32Type(), param_types_buf[0..call_argc], false);
            const args_slice = call_args[0..call_argc];
            const result = builder.buildCall(call_fn_ty, call_target, args_slice, "tail");
            _ = builder.buildRet(result);
            block_terminated.* = true;
        },

        .return_i32 => {
            if (handler.value) |v| {
                // return_undef → return constant
                _ = builder.buildRet(llvm.constInt32(v));
            } else {
                if (vstack.items.len < 1) return CodegenError.StackUnderflow;
                const val = i32VstackPop(vstack);
                _ = builder.buildRet(val);
            }
            block_terminated.* = true;
        },

        .if_false_i32 => {
            // Conditional branch: pop condition, branch if false (== 0)
            // CFG successors: [0] = jump target (false branch), [1] = fall-through (true branch)
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const cond_val = i32VstackPop(vstack);

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
            const cond_val = i32VstackPop(vstack);

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
// Comptime Numeric Body Generator (f64 tier)
// ============================================================================

// Thread-local struct layout for field_get codegen (workaround for Zig 0.15
// parameter tracking limitation in complex switch arms)
var struct_layout_global: ?*const numeric_handlers.StructArgInfo = null;

/// Generate function body for any numeric ValueKind using comptime dispatch.
/// The i32 tier uses generateInt32Body (legacy, well-tested).
/// The f64 tier uses this function with comptime kind = .f64.
fn generateNumericBody(
    comptime kind: ValueKind,
    allocator: Allocator,
    builder: llvm.Builder,
    func: llvm.Value,
    analyzed: AnalyzedFunction,
    cfg: *const CFG,
    module: llvm.Module,
    length_args: u8,
    _struct_layout: ?*const numeric_handlers.StructArgInfo,
) CodegenError!void {
    // Store struct layout for use in field_get handler (Zig 0.15 can't trace
    // parameter usage through complex switch arms)
    struct_layout_global = _struct_layout;
    const blocks = cfg.blocks.items;
    if (blocks.len == 0) return;

    // Comptime-selected LLVM element type
    const elem_type: llvm.Type = switch (kind) {
        .i32 => llvm.i32Type(),
        .f64 => llvm.doubleType(),
    };
    const zero_val: llvm.Value = switch (kind) {
        .i32 => llvm.constInt32(0),
        .f64 => llvm.constF64(0.0),
    };

    // Create LLVM basic blocks.
    // Entry block holds only allocas + br to blk_0, so blk_0 can have loop backedges.
    // (LLVM requires the entry block to have no predecessors.)
    const entry_bb = llvm.appendBasicBlock(func, "entry");

    var llvm_blocks = allocator.alloc(llvm.BasicBlock, blocks.len) catch return CodegenError.OutOfMemory;
    defer allocator.free(llvm_blocks);

    for (blocks, 0..) |_, i| {
        var bb_name_buf: [32]u8 = undefined;
        const bb_name = std.fmt.bufPrintZ(&bb_name_buf, "blk_{d}", .{i}) catch "blk";
        llvm_blocks[i] = llvm.appendBasicBlock(func, bb_name);
    }

    const unreachable_bb = llvm.appendBasicBlock(func, "unreachable");
    builder.positionAtEnd(unreachable_bb);
    _ = builder.buildUnreachable();

    // Allocate locals in entry block (LLVM mem2reg requires allocas in entry)
    builder.positionAtEnd(entry_bb);
    var locals_buf: [256]llvm.Value = undefined;
    const local_count = analyzed.var_count;
    for (0..@min(local_count, 256)) |i| {
        var loc_name: [16]u8 = undefined;
        const name = std.fmt.bufPrintZ(&loc_name, "loc_{d}", .{i}) catch "loc";
        locals_buf[i] = builder.buildAlloca(elem_type, name);
        _ = builder.buildStore(zero_val, locals_buf[i]);
    }

    // Shadow function parameters with allocas
    var params_buf: [8]llvm.Value = undefined;
    for (0..analyzed.arg_count) |i| {
        var arg_name: [16]u8 = undefined;
        const name = std.fmt.bufPrintZ(&arg_name, "arg_{d}", .{i}) catch "arg";
        params_buf[i] = builder.buildAlloca(elem_type, name);
        _ = builder.buildStore(c.LLVMGetParam(func, @intCast(i)), params_buf[i]);
    }

    // === Array length parameters ===
    // For each array arg with .length access, we have an extra i32 param appended
    // after the regular args. Build a map from arg index → length param LLVM value.
    var length_params: [8]?llvm.Value = .{null} ** 8;
    {
        var len_param_idx: u32 = analyzed.arg_count;
        for (0..analyzed.arg_count) |i| {
            if (length_args & (@as(u8, 1) << @intCast(i)) != 0) {
                length_params[i] = c.LLVMGetParam(func, @intCast(len_param_idx));
                len_param_idx += 1;
            }
        }
    }

    // === i32 shadow allocas for f64 tier loop counters ===
    // LLVM can't auto-vectorize loops with f64 induction variables (fptosi
    // in the GEP breaks stride detection). We maintain i32 shadows for locals
    // modified by inc_loc/dec_loc. get_loc pushes sitofp(shadow) — the sitofp
    // is peephole-eliminated in array_get, giving LLVM a clean i32 stride.
    var counter_mask: u64 = 0;
    var i32_shadows: [256]llvm.Value = undefined;
    if (kind == .f64) {
        for (blocks) |block| {
            for (block.instructions) |cinstr| {
                const ch = numeric_handlers.getHandler(cinstr.opcode);
                if (ch.pattern == .inc_loc or ch.pattern == .dec_loc) {
                    const cidx: u32 = switch (cinstr.operand) {
                        .loc => |a| a,
                        .u8 => |a| a,
                        else => continue,
                    };
                    if (cidx < 64) counter_mask |= @as(u64, 1) << @intCast(cidx);
                }
            }
        }
        if (counter_mask != 0) {
            for (0..@min(local_count, 256)) |i| {
                if (counter_mask & (@as(u64, 1) << @intCast(i)) != 0) {
                    var sname: [16]u8 = undefined;
                    const sn = std.fmt.bufPrintZ(&sname, "is_{d}", .{i}) catch "is";
                    i32_shadows[i] = builder.buildAlloca(llvm.i32Type(), sn);
                    _ = builder.buildStore(llvm.constInt32(0), i32_shadows[i]);
                }
            }
        }
    }

    // === Cross-block spill slots for vstack persistence ===
    // QuickJS bytecode can have values live on the evaluation stack across basic
    // block boundaries. We use individual alloca spill slots (not an array) so
    // LLVM's mem2reg promotes them to SSA values / WASM locals — avoiding linear
    // memory aliasing with user data arrays.
    const max_spill = 8;
    var spill_slots: [max_spill]llvm.Value = undefined;
    for (0..max_spill) |i| {
        var name_buf: [16]u8 = undefined;
        const name = std.fmt.bufPrintZ(&name_buf, "spill_{d}", .{i}) catch "spill";
        spill_slots[i] = builder.buildAlloca(elem_type, name);
        _ = builder.buildStore(zero_val, spill_slots[i]);
    }

    // Branch from entry to first CFG block (entry stays predecessor-free)
    _ = builder.buildBr(llvm_blocks[0]);

    // Compute entry stack depth for each block via static analysis
    var entry_depths = allocator.alloc(i32, blocks.len) catch return CodegenError.OutOfMemory;
    defer allocator.free(entry_depths);
    @memset(entry_depths, -1); // -1 = unvisited
    entry_depths[0] = 0;

    // Forward pass: compute stack depth at each block entry
    for (blocks, 0..) |block, block_idx| {
        var depth: i32 = entry_depths[block_idx];
        if (depth < 0) depth = 0; // unreachable block

        for (block.instructions) |instr| {
            depth += numericStackEffect(instr);
        }

        // Propagate to successors
        for (block.successors.items) |succ| {
            if (succ < blocks.len) {
                if (entry_depths[succ] < 0) {
                    entry_depths[succ] = depth;
                }
            }
        }
        // Fall-through successor
        if (block.successors.items.len == 0 and block_idx + 1 < blocks.len) {
            if (entry_depths[block_idx + 1] < 0) {
                entry_depths[block_idx + 1] = depth;
            }
        }
    }

    // Generate each block
    for (blocks, 0..) |block, block_idx| {
        builder.positionAtEnd(llvm_blocks[block_idx]);

        var vstack = std.ArrayListUnmanaged(llvm.Value){};
        defer vstack.deinit(allocator);

        // Reload live stack values from spill slots at block entry
        const entry_depth: u32 = @intCast(@max(entry_depths[block_idx], 0));
        for (0..@min(entry_depth, max_spill)) |i| {
            const val = builder.buildLoad(elem_type, spill_slots[i], "reload");
            vstack.append(allocator, val) catch return CodegenError.OutOfMemory;
        }

        var block_terminated = false;
        var pending_callee: ?llvm.Value = null;
        // Stack of pending Math intrinsics for nested patterns like Math.floor(Math.abs(x))
        var math_intrinsic_stack: [8]frozen_registry.MathIntrinsic = undefined;
        var math_intrinsic_depth: u32 = 0;
        var skip_count: u32 = 0;

        // Process instructions. For the last instruction that is a terminator,
        // spill the vstack BEFORE emitting the branch.
        for (block.instructions, 0..) |instr, instr_idx| {
            if (skip_count > 0) { skip_count -= 1; continue; }
            if (block_terminated) break;

            // Peephole: Math.abs/sqrt/floor/ceil/round/trunc intrinsics
            // Phase 1a: Detect get_var("Math") + get_field2("method") → push to stack
            if (instr.opcode == .get_var or instr.opcode == .get_var_undef) {
                if (frozen_registry.detectMathSetup(analyzed, block.instructions, instr_idx)) |intrinsic| {
                    if (math_intrinsic_depth < math_intrinsic_stack.len) {
                        math_intrinsic_stack[math_intrinsic_depth] = intrinsic;
                        math_intrinsic_depth += 1;
                    }
                    skip_count = 1; // skip get_field2
                    continue;
                }
            }
            // Phase 1b: Detect destructured Math (get_var_ref{N} for known Math builtin)
            if (frozen_registry.detectDestructuredMathRef(analyzed, block.instructions, instr_idx)) |intrinsic| {
                if (math_intrinsic_depth < math_intrinsic_stack.len) {
                    math_intrinsic_stack[math_intrinsic_depth] = intrinsic;
                    math_intrinsic_depth += 1;
                }
                continue; // skip the get_var_ref (no get_field2 to skip for destructured)
            }
            // Phase 2: At call_method/call/tail_call_method/tail_call, emit the most recent intrinsic
            if (math_intrinsic_depth > 0 and (instr.opcode == .call_method or instr.opcode == .tail_call_method or
                instr.opcode == .call1 or instr.opcode == .call2 or instr.opcode == .call3 or
                instr.opcode == .call or instr.opcode == .tail_call))
            {
                const argc: u16 = switch (instr.opcode) {
                    .call1 => 1,
                    .call2 => 2,
                    .call3 => 3,
                    else => switch (instr.operand) {
                        .u16 => |v| v,
                        else => 0,
                    },
                };
                if (argc == 1 or argc == 2) {
                    math_intrinsic_depth -= 1;
                    const intrinsic = math_intrinsic_stack[math_intrinsic_depth];
                    const expected_arity = intrinsic.arity();
                    if (argc == expected_arity) {
                        const is_tail = (instr.opcode == .tail_call_method or instr.opcode == .tail_call);
                        if (argc == 1) {
                            // Unary: pop 1 arg
                            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
                            const arg = vstack.items[vstack.items.len - 1];
                            vstack.items.len -= 1;
                            const f64_arg = switch (kind) {
                                .f64 => arg,
                                .i32 => builder.buildSIToFP(arg, llvm.doubleType(), "math_f64"),
                            };
                            const result_f64 = emitMathIntrinsic(builder, module, intrinsic, f64_arg);
                            const result = switch (kind) {
                                .f64 => result_f64,
                                .i32 => builder.buildFPToSI(result_f64, llvm.i32Type(), "math_i32"),
                            };
                            if (is_tail) {
                                _ = builder.buildRet(result);
                                block_terminated = true;
                            } else {
                                vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
                            }
                            continue;
                        } else {
                            // Binary: pop 2 args (stack order: arg0 pushed first, arg1 on top)
                            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
                            const arg1 = vstack.items[vstack.items.len - 1];
                            const arg0 = vstack.items[vstack.items.len - 2];
                            vstack.items.len -= 2;
                            const f64_arg0 = switch (kind) {
                                .f64 => arg0,
                                .i32 => builder.buildSIToFP(arg0, llvm.doubleType(), "math_a_f64"),
                            };
                            const f64_arg1 = switch (kind) {
                                .f64 => arg1,
                                .i32 => builder.buildSIToFP(arg1, llvm.doubleType(), "math_b_f64"),
                            };
                            const result_f64 = emitMathBinaryIntrinsic(builder, module, intrinsic, f64_arg0, f64_arg1);
                            const result = switch (kind) {
                                .f64 => result_f64,
                                .i32 => builder.buildFPToSI(result_f64, llvm.i32Type(), "math_i32"),
                            };
                            if (is_tail) {
                                _ = builder.buildRet(result);
                                block_terminated = true;
                            } else {
                                vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
                            }
                            continue;
                        }
                    }
                }
            }

            // Peephole: get_arg(N) + get_length → push length_params[N]
            // When an array arg's .length is accessed, we use the extra WASM param
            // instead of dereferencing a JS object.
            if (numeric_handlers.getHandler(instr.opcode).pattern == .get_arg and
                instr_idx + 1 < block.instructions.len)
            {
                const next = block.instructions[instr_idx + 1];
                if (numeric_handlers.getHandler(next.opcode).pattern == .array_length) {
                    const arg_handler = numeric_handlers.getHandler(instr.opcode);
                    const arg_idx: u32 = arg_handler.index orelse switch (instr.operand) {
                        .arg => |a| a,
                        .u8 => |a| a,
                        else => 255,
                    };
                    if (arg_idx < 8) {
                        if (length_params[arg_idx]) |len_param| {
                            // Fuse get_arg + get_length into a single length param load
                            const result = switch (kind) {
                                .i32 => len_param,
                                .f64 => builder.buildSIToFP(len_param, llvm.doubleType(), "len_f64"),
                            };
                            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
                            skip_count = 1;
                            continue;
                        }
                    }
                }
            }

            // Check if this instruction is a terminator — if so, spill first
            const is_terminator = switch (numeric_handlers.getHandler(instr.opcode).pattern) {
                .if_false, .if_true, .goto_br, .ret, .tail_call_self => true,
                else => false,
            };
            if (is_terminator) {
                // Spill vstack items that the successor blocks will need.
                // The terminator will pop some values (condition for if/goto, args for tail_call).
                // We spill everything EXCEPT the values the terminator will consume.
                const term_pops: usize = switch (numeric_handlers.getHandler(instr.opcode).pattern) {
                    .if_false, .if_true, .ret => 1,
                    .tail_call_self => @intCast(instr.operand.u16),
                    .goto_br => 0,
                    else => 0,
                };
                const spill_count = if (vstack.items.len > term_pops)
                    vstack.items.len - term_pops
                else
                    0;
                for (0..@min(spill_count, max_spill)) |i| {
                    _ = builder.buildStore(vstack.items[i], spill_slots[i]);
                }
            }

            try emitNumericInstruction(
                kind,
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
                &pending_callee,
                module,
                analyzed,
                &length_params,
                &i32_shadows,
                counter_mask,
            );
        }

        // For non-terminated blocks (fall-through), spill and branch
        if (!block_terminated) {
            for (0..@min(vstack.items.len, max_spill)) |i| {
                _ = builder.buildStore(vstack.items[i], spill_slots[i]);
            }
            if (block_idx + 1 < blocks.len) {
                _ = builder.buildBr(llvm_blocks[block_idx + 1]);
            } else {
                _ = builder.buildRet(zero_val);
            }
        }
    }
}

/// Compute the net stack effect of a numeric instruction.
/// Returns the change in stack depth (positive = push, negative = pop).
fn numericStackEffect(instr: Instruction) i32 {
    const handler = numeric_handlers.getHandler(instr.opcode);
    return switch (handler.pattern) {
        .push_const, .push_cpool, .push_bool, .get_arg, .get_loc, .get_loc_check => 1,
        .get_loc_pair => 2, // push 2 locals
        .put_arg, .put_loc, .put_loc_check, .stack_drop, .add_loc => -1,
        .set_arg, .set_loc, .nop, .set_loc_uninitialized, .inc_loc, .dec_loc => 0,
        .binary_arith, .binary_cmp, .bitwise_binary, .array_get => -1, // pop 2, push 1
        .unary, .lnot, .inc_dec, .always_false, .array_length, .field_get => 0, // pop 1, push 1
        .closure_read => 1, // push 1 (closure variable value)
        .post_inc_dec => 1, // pop 1, push 2
        .stack_dup => 1,
        .stack_dup2 => 2,
        .stack_swap => 0,
        .self_ref => 0, // doesn't affect vstack in our impl (sets pending_callee)
        .call_self => blk: {
            const argc: i32 = switch (instr.opcode) {
                .call0 => 0, .call1 => 1, .call2 => 2, .call3 => 3,
                .call => @as(i32, @intCast(instr.operand.u16)),
                else => 1,
            };
            break :blk 1 - argc; // pop argc, push 1
        },
        .tail_call_self => blk: {
            const argc: i32 = @intCast(instr.operand.u16);
            break :blk -argc; // pop argc (terminates, no push)
        },
        .ret, .if_false, .if_true => -1, // pop 1
        .goto_br => 0,
        .array_get2 => 0, // pop 1 (index), peek base, push 1 (result) = net 0
        .array_put => -3, // pop 3
        .unsupported => 0,
    };
}

/// Emit an LLVM intrinsic call for a Math.* function.
/// Maps Math.abs → llvm.fabs.f64, Math.sqrt → llvm.sqrt.f64, etc.
/// LLVM auto-lowers these to WASM intrinsics (f64.abs, f64.sqrt, etc.).
fn emitMathIntrinsic(builder: llvm.Builder, module: llvm.Module, intrinsic: frozen_registry.MathIntrinsic, arg: llvm.Value) llvm.Value {
    const f64_ty = llvm.doubleType();
    const unary_fn_ty = llvm.functionType(f64_ty, &.{f64_ty}, false);

    switch (intrinsic) {
        .abs => {
            const f = getOrDeclareIntrinsic(module, "llvm.fabs.f64", unary_fn_ty);
            return builder.buildCall(unary_fn_ty, f, &.{arg}, "abs");
        },
        .sqrt => {
            const f = getOrDeclareIntrinsic(module, "llvm.sqrt.f64", unary_fn_ty);
            return builder.buildCall(unary_fn_ty, f, &.{arg}, "sqrt");
        },
        .floor => {
            const f = getOrDeclareIntrinsic(module, "llvm.floor.f64", unary_fn_ty);
            return builder.buildCall(unary_fn_ty, f, &.{arg}, "floor");
        },
        .ceil => {
            const f = getOrDeclareIntrinsic(module, "llvm.ceil.f64", unary_fn_ty);
            return builder.buildCall(unary_fn_ty, f, &.{arg}, "ceil");
        },
        .trunc => {
            const f = getOrDeclareIntrinsic(module, "llvm.trunc.f64", unary_fn_ty);
            return builder.buildCall(unary_fn_ty, f, &.{arg}, "trunc");
        },
        .round => {
            // JS Math.round(x) = floor(x + 0.5) — NOT the same as llvm.round
            // (llvm.round rounds ties away from zero; JS rounds ties toward +Inf)
            const half = llvm.constF64(0.5);
            const added = builder.buildFAdd(arg, half, "round_add");
            const f = getOrDeclareIntrinsic(module, "llvm.floor.f64", unary_fn_ty);
            return builder.buildCall(unary_fn_ty, f, &.{added}, "round");
        },
        // Binary intrinsics should not reach here
        .min, .max, .pow => unreachable,
    }
}

/// Emit inline LLVM IR for a binary Math.* function.
/// Uses fcmp+select for min/max (avoids external fmin/fmax calls in standalone WASM).
/// pow uses an integer exponent fast path with loop.
fn emitMathBinaryIntrinsic(builder: llvm.Builder, module: llvm.Module, intrinsic: frozen_registry.MathIntrinsic, arg0: llvm.Value, arg1: llvm.Value) llvm.Value {
    switch (intrinsic) {
        .min => {
            // Math.min(a, b): a < b ? a : b, with NaN propagation (JS spec)
            const cmp = builder.buildFCmp(c.LLVMRealOLT, arg0, arg1, "min_cmp");
            const min_val = builder.buildSelect(cmp, arg0, arg1, "min_val");
            // If either arg is NaN, fcmp UNO returns true → return NaN
            const is_nan = builder.buildFCmp(c.LLVMRealUNO, arg0, arg1, "is_nan");
            const nan_const = llvm.constF64(std.math.nan(f64));
            return builder.buildSelect(is_nan, nan_const, min_val, "min");
        },
        .max => {
            // Math.max(a, b): a > b ? a : b, with NaN propagation (JS spec)
            const cmp = builder.buildFCmp(c.LLVMRealOGT, arg0, arg1, "max_cmp");
            const max_val = builder.buildSelect(cmp, arg0, arg1, "max_val");
            const is_nan = builder.buildFCmp(c.LLVMRealUNO, arg0, arg1, "is_nan");
            const nan_const = llvm.constF64(std.math.nan(f64));
            return builder.buildSelect(is_nan, nan_const, max_val, "max");
        },
        .pow => {
            // Math.pow(base, exp): use llvm.pow.f64 intrinsic
            // LLVM optimizes pow(x,2)→x*x, pow(x,0.5)→sqrt(x), etc.
            // For general cases, LLVM lowers to a call to "pow" which becomes
            // a WASM import from "env" (provided by host as Math.pow).
            const f64_ty = llvm.doubleType();
            var pow_params = [_]llvm.Type{ f64_ty, f64_ty };
            const pow_fn_ty = llvm.functionType(f64_ty, &pow_params, false);
            const pow_fn = getOrDeclareIntrinsic(module, "llvm.pow.f64", pow_fn_ty);
            return builder.buildCall(pow_fn_ty, pow_fn, &.{ arg0, arg1 }, "pow");
        },
        // Unary intrinsics should not reach here
        .abs, .sqrt, .floor, .ceil, .round, .trunc => unreachable,
    }
}

/// Get or declare an LLVM intrinsic function by name.
pub fn getOrDeclareIntrinsic(module: llvm.Module, name: [*:0]const u8, fn_ty: llvm.Type) llvm.Value {
    return module.getNamedFunction(name) orelse blk: {
        const f = module.addFunction(name, fn_ty);
        // LLVM intrinsics are always available — no linkage needed
        break :blk f;
    };
}

/// Push a value to the memory-backed vstack array
inline fn memVstackPush(builder: llvm.Builder, vstack_array: llvm.Value, sp_alloca: llvm.Value, val: llvm.Value, elem_type: llvm.Type) void {
    const sp = builder.buildLoad(llvm.i32Type(), sp_alloca, "sp_ld");
    var indices = [_]llvm.Value{ llvm.constInt32(0), sp };
    const slot = c.LLVMBuildGEP2(builder.ref, c.LLVMArrayType2(elem_type, 16), vstack_array, &indices, 2, "vs_push");
    _ = builder.buildStore(val, slot);
    const new_sp = c.LLVMBuildAdd(builder.ref, sp, llvm.constInt32(1), "sp_inc");
    _ = builder.buildStore(new_sp, sp_alloca);
}

/// Pop a value from the memory-backed vstack array
inline fn memVstackPop(builder: llvm.Builder, vstack_array: llvm.Value, sp_alloca: llvm.Value, elem_type: llvm.Type) llvm.Value {
    const sp = builder.buildLoad(llvm.i32Type(), sp_alloca, "sp_ld");
    const new_sp = c.LLVMBuildSub(builder.ref, sp, llvm.constInt32(1), "sp_dec");
    _ = builder.buildStore(new_sp, sp_alloca);
    var indices = [_]llvm.Value{ llvm.constInt32(0), new_sp };
    const slot = c.LLVMBuildGEP2(builder.ref, c.LLVMArrayType2(elem_type, 16), vstack_array, &indices, 2, "vs_pop");
    return builder.buildLoad(elem_type, slot, "vs_val");
}

/// Peek at the top of the memory-backed vstack (no pop)
inline fn memVstackPeek(builder: llvm.Builder, vstack_array: llvm.Value, sp_alloca: llvm.Value, elem_type: llvm.Type) llvm.Value {
    const sp = builder.buildLoad(llvm.i32Type(), sp_alloca, "sp_ld");
    const top = c.LLVMBuildSub(builder.ref, sp, llvm.constInt32(1), "sp_top");
    var indices = [_]llvm.Value{ llvm.constInt32(0), top };
    const slot = c.LLVMBuildGEP2(builder.ref, c.LLVMArrayType2(elem_type, 16), vstack_array, &indices, 2, "vs_peek");
    return builder.buildLoad(elem_type, slot, "vs_top");
}

/// Emit a single instruction using comptime ValueKind for type dispatch.
/// At comptime, Zig monomorphizes this — the f64 path emits fXxx instructions,
/// the i32 path emits integer instructions. Dead branches are eliminated.
fn emitNumericInstruction(
    comptime kind: ValueKind,
    allocator: Allocator,
    builder: llvm.Builder,
    vstack: *std.ArrayListUnmanaged(llvm.Value),
    instr: Instruction,
    params: *[8]llvm.Value,
    arg_count: u32,
    locals: *[256]llvm.Value,
    local_count: u32,
    llvm_blocks: []llvm.BasicBlock,
    block_count: usize,
    successors: []const u32,
    func: llvm.Value,
    block_terminated: *bool,
    pending_callee: *?llvm.Value,
    module: llvm.Module,
    analyzed: AnalyzedFunction,
    length_params: *const [8]?llvm.Value,
    i32_shadows: *[256]llvm.Value,
    counter_mask: u64,
) CodegenError!void {
    _ = length_params; // Handled by peephole in generateNumericBody
    const handler = numeric_handlers.getHandler(instr.opcode);

    // Comptime type constants
    const elem_type: llvm.Type = switch (kind) {
        .i32 => llvm.i32Type(),
        .f64 => llvm.doubleType(),
    };
    const zero_val: llvm.Value = switch (kind) {
        .i32 => llvm.constInt32(0),
        .f64 => llvm.constF64(0.0),
    };
    const one_val: llvm.Value = switch (kind) {
        .i32 => llvm.constInt32(1),
        .f64 => llvm.constF64(1.0),
    };

    const numVstackPop = struct {
        fn pop(vs: *std.ArrayListUnmanaged(llvm.Value)) llvm.Value {
            const v = vs.items[vs.items.len - 1];
            vs.items.len -= 1;
            return v;
        }
    }.pop;

    switch (handler.pattern) {
        .push_const => {
            const val = if (handler.value) |v| switch (kind) {
                .i32 => llvm.constInt32(v),
                .f64 => llvm.constF64(@floatFromInt(v)),
            } else switch (instr.opcode) {
                .push_i8 => switch (kind) {
                    .i32 => llvm.constInt32(@as(i32, instr.operand.i8)),
                    .f64 => llvm.constF64(@as(f64, @floatFromInt(instr.operand.i8))),
                },
                .push_i16 => switch (kind) {
                    .i32 => llvm.constInt32(@as(i32, instr.operand.i16)),
                    .f64 => llvm.constF64(@as(f64, @floatFromInt(instr.operand.i16))),
                },
                .push_i32 => switch (kind) {
                    .i32 => llvm.constInt32(instr.operand.i32),
                    .f64 => llvm.constF64(@as(f64, @floatFromInt(instr.operand.i32))),
                },
                else => return CodegenError.UnsupportedOpcode,
            };
            vstack.append(allocator, val) catch return CodegenError.OutOfMemory;
        },

        .push_cpool => {
            // Load constant from constant pool at compile time
            const idx: u32 = switch (instr.operand) {
                .const_idx => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (idx >= analyzed.constants.len) return CodegenError.UnsupportedOpcode;
            const val: llvm.Value = switch (analyzed.constants[idx]) {
                .int32 => |v| switch (kind) {
                    .i32 => llvm.constInt32(v),
                    .f64 => llvm.constF64(@as(f64, @floatFromInt(v))),
                },
                .float64 => |v| switch (kind) {
                    .i32 => blk: {
                        const u: u32 = if (v >= 0 and v <= @as(f64, @floatFromInt(std.math.maxInt(u32))))
                            @intFromFloat(v)
                        else if (v < 0 and v >= @as(f64, @floatFromInt(std.math.minInt(i32))))
                            @bitCast(@as(i32, @intFromFloat(v)))
                        else
                            return CodegenError.UnsupportedOpcode;
                        break :blk llvm.constInt32(@bitCast(u));
                    },
                    .f64 => llvm.constF64(v),
                },
                else => return CodegenError.UnsupportedOpcode,
            };
            vstack.append(allocator, val) catch return CodegenError.OutOfMemory;
        },

        .push_bool => {
            const val = if (handler.value != null and handler.value.? != 0) one_val else zero_val;
            vstack.append(allocator, val) catch return CodegenError.OutOfMemory;
        },

        .get_arg => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .arg => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (idx >= arg_count) return CodegenError.UnsupportedOpcode;
            const val = builder.buildLoad(elem_type, params[idx], "arg");
            vstack.append(allocator, val) catch return CodegenError.OutOfMemory;
        },

        .put_arg => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .arg => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= arg_count) return CodegenError.UnsupportedOpcode;
            const val = numVstackPop(vstack);
            _ = builder.buildStore(val, params.*[idx]);
        },

        .set_arg => {
            const idx: u32 = if (handler.index) |i| i else switch (instr.operand) {
                .arg => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= arg_count) return CodegenError.UnsupportedOpcode;
            const val = vstack.items[vstack.items.len - 1]; // peek
            _ = builder.buildStore(val, params.*[idx]);
        },

        .get_loc, .get_loc_check => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (idx >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            // f64 tier: for loop counter locals, load i32 shadow and sitofp.
            // array_get peephole detects the sitofp and uses the i32 directly.
            if (kind == .f64 and counter_mask != 0 and idx < 256 and
                counter_mask & (@as(u64, 1) << @intCast(idx)) != 0)
            {
                const i32_val = builder.buildLoad(llvm.i32Type(), i32_shadows.*[idx], "loc_i32");
                const f64_val = builder.buildSIToFP(i32_val, llvm.doubleType(), "loc_sitofp");
                vstack.append(allocator, f64_val) catch return CodegenError.OutOfMemory;
            } else {
                const val = builder.buildLoad(elem_type, locals.*[idx], "loc");
                vstack.append(allocator, val) catch return CodegenError.OutOfMemory;
            }
        },

        .get_loc_pair => {
            // Fused: push loc0, then push loc1
            if (0 >= @min(local_count, 256) or 1 >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            const val0 = builder.buildLoad(elem_type, locals.*[0], "loc0");
            const val1 = builder.buildLoad(elem_type, locals.*[1], "loc1");
            vstack.append(allocator, val0) catch return CodegenError.OutOfMemory;
            vstack.append(allocator, val1) catch return CodegenError.OutOfMemory;
        },

        .put_loc, .put_loc_check => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            const val = numVstackPop(vstack);
            _ = builder.buildStore(val, locals.*[idx]);
            // Sync i32 shadow for loop counter locals
            if (kind == .f64 and counter_mask != 0 and idx < 256 and
                counter_mask & (@as(u64, 1) << @intCast(idx)) != 0)
            {
                _ = builder.buildStore(
                    builder.buildFPToSI(val, llvm.i32Type(), "sh_fptosi"),
                    i32_shadows.*[idx],
                );
            }
        },

        .set_loc => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            const val = vstack.items[vstack.items.len - 1]; // peek
            _ = builder.buildStore(val, locals.*[idx]);
            if (kind == .f64 and counter_mask != 0 and idx < 256 and
                counter_mask & (@as(u64, 1) << @intCast(idx)) != 0)
            {
                _ = builder.buildStore(
                    builder.buildFPToSI(val, llvm.i32Type(), "sh_fptosi"),
                    i32_shadows.*[idx],
                );
            }
        },

        .set_loc_uninitialized => {
            const idx: u32 = switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (idx >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            _ = builder.buildStore(zero_val, locals.*[idx]);
            if (kind == .f64 and counter_mask != 0 and idx < 256 and
                counter_mask & (@as(u64, 1) << @intCast(idx)) != 0)
            {
                _ = builder.buildStore(llvm.constInt32(0), i32_shadows.*[idx]);
            }
        },

        .binary_arith => {
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const rhs = numVstackPop(vstack);
            const lhs = numVstackPop(vstack);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;

            const result = switch (kind) {
                .i32 => blk: {
                    break :blk switch (op) {
                        .add => builder.buildAdd(lhs, rhs, "add"),
                        .sub => builder.buildSub(lhs, rhs, "sub"),
                        .mul => mul_blk: {
                            // JS mul is f64: large products lose precision in float64,
                            // so ToInt32(f64_mul(a,b)) differs from i32_wrap(a*b).
                            // fptosi to i64 first (safe: i32*i32 max ~2^62 < 2^63),
                            // then trunc to i32 = low 32 bits = JS ToInt32() wrapping.
                            const fa = builder.buildSIToFP(lhs, llvm.doubleType(), "fma");
                            const fb = builder.buildSIToFP(rhs, llvm.doubleType(), "fmb");
                            const fprod = builder.buildFMul(fa, fb, "fmprod");
                            const i64_prod = builder.buildFPToSI(fprod, llvm.i64Type(), "mul64");
                            break :mul_blk builder.buildTrunc(i64_prod, llvm.i32Type(), "mul32");
                        },
                        .div => builder.buildSDiv(lhs, rhs, "div"),
                        .mod => builder.buildSRem(lhs, rhs, "rem"),
                        else => return CodegenError.UnsupportedOpcode,
                    };
                },
                .f64 => blk: {
                    const fval = switch (op) {
                        .add => builder.buildFAdd(lhs, rhs, "fadd"),
                        .sub => builder.buildFSub(lhs, rhs, "fsub"),
                        .mul => builder.buildFMul(lhs, rhs, "fmul"),
                        .div => builder.buildFDiv(lhs, rhs, "fdiv"),
                        .mod => builder.buildFRem(lhs, rhs, "frem"),
                        else => return CodegenError.UnsupportedOpcode,
                    };
                    // Allow reassociation + FMA contraction for SIMD vectorization
                    llvm.Builder.setFastMathFlags(fval, llvm.Builder.FMF_VECTORIZE);
                    break :blk fval;
                },
            };
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .bitwise_binary => {
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;

            // For f64 tier, convert to i32 using JS ToInt32 semantics:
            // ToInt32(x) = fptosi(x, i64) & 0xFFFFFFFF, then trunc to i32.
            // This matches JS where (large_float | 0) wraps modularly.
            // Direct fptosi(f64, i32) would saturate at i32_max for large values.
            const rhs_raw = numVstackPop(vstack);
            const lhs_raw = numVstackPop(vstack);
            const lhs = switch (kind) {
                .i32 => lhs_raw,
                .f64 => blk: {
                    const i64_val = builder.buildFPToSI(lhs_raw, llvm.i64Type(), "bw_lhs64");
                    break :blk builder.buildTrunc(i64_val, llvm.i32Type(), "bw_lhs");
                },
            };
            const rhs = switch (kind) {
                .i32 => rhs_raw,
                .f64 => blk: {
                    const i64_val = builder.buildFPToSI(rhs_raw, llvm.i64Type(), "bw_rhs64");
                    break :blk builder.buildTrunc(i64_val, llvm.i32Type(), "bw_rhs");
                },
            };

            const i32_result = switch (op) {
                .band => builder.buildAnd(lhs, rhs, "and"),
                .bor => builder.buildOr(lhs, rhs, "or"),
                .bxor => builder.buildXor(lhs, rhs, "xor"),
                .shl => builder.buildShl(lhs, rhs, "shl"),
                .sar => builder.buildAShr(lhs, rhs, "shr"),
                .shr => builder.buildLShr(lhs, rhs, "shru"),
                else => return CodegenError.UnsupportedOpcode,
            };

            const result = switch (kind) {
                .i32 => i32_result,
                .f64 => builder.buildSIToFP(i32_result, llvm.doubleType(), "bw_f64"),
            };
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .binary_cmp => {
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const rhs = numVstackPop(vstack);
            const lhs = numVstackPop(vstack);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;

            const cmp = switch (kind) {
                .i32 => blk: {
                    const pred = intPred(op) orelse return CodegenError.UnsupportedOpcode;
                    break :blk builder.buildICmp(pred, lhs, rhs, "cmp");
                },
                .f64 => blk: {
                    // Peephole: if one operand is sitofp(i32) (from i32 shadow counter),
                    // use icmp instead of fcmp. This gives SCEV a clean integer trip count
                    // enabling LoopVectorize to emit f64x2 SIMD.
                    const lhs_is_sitofp = c.LLVMGetInstructionOpcode(lhs) == c.LLVMSIToFP;
                    const rhs_is_sitofp = c.LLVMGetInstructionOpcode(rhs) == c.LLVMSIToFP;
                    if (lhs_is_sitofp or rhs_is_sitofp) {
                        const pred_i = intPred(op) orelse return CodegenError.UnsupportedOpcode;
                        const i32_lhs = if (lhs_is_sitofp)
                            c.LLVMGetOperand(lhs, 0)
                        else
                            builder.buildFPToSI(lhs, llvm.i32Type(), "cmp_lhs_i32");
                        const i32_rhs = if (rhs_is_sitofp)
                            c.LLVMGetOperand(rhs, 0)
                        else
                            builder.buildFPToSI(rhs, llvm.i32Type(), "cmp_rhs_i32");
                        break :blk builder.buildICmp(pred_i, i32_lhs, i32_rhs, "icmp_iv");
                    }

                    const pred = realPred(op) orelse return CodegenError.UnsupportedOpcode;
                    const fc = builder.buildFCmp(pred, lhs, rhs, "fcmp");
                    llvm.Builder.setFastMathFlags(fc, llvm.Builder.FMF_VECTORIZE | 0x2);
                    break :blk fc;
                },
            };

            // If cmp is i1 (from icmp peephole), push directly for if_false to consume
            if (kind == .f64 and c.LLVMGetTypeKind(c.LLVMTypeOf(cmp)) == c.LLVMIntegerTypeKind and
                c.LLVMGetIntTypeWidth(c.LLVMTypeOf(cmp)) == 1)
            {
                vstack.append(allocator, cmp) catch return CodegenError.OutOfMemory;
            } else {
                // Normal path: zext → sitofp for f64 tier
                const cmpz = builder.buildZExt(cmp, llvm.i32Type(), "cmpz");
                const result = switch (kind) {
                    .i32 => cmpz,
                    .f64 => builder.buildSIToFP(cmpz, llvm.doubleType(), "cmpf"),
                };
                vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
            }
        },

        .unary => {
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const val = numVstackPop(vstack);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;

            const result = switch (op) {
                .neg => switch (kind) {
                    .i32 => builder.buildNeg(val, "neg"),
                    .f64 => builder.buildFNeg(val, "fneg"),
                },
                .bnot => blk: {
                    if (kind != .i32) return CodegenError.UnsupportedOpcode;
                    break :blk builder.buildNot(val, "bnot");
                },
                else => return CodegenError.UnsupportedOpcode,
            };

            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .lnot => {
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const val = numVstackPop(vstack);
            // Logical NOT: 0 → 1, nonzero → 0
            const is_zero = switch (kind) {
                .i32 => builder.buildICmp(c.LLVMIntEQ, val, llvm.constInt32(0), "iszero"),
                .f64 => builder.buildFCmp(c.LLVMRealOEQ, val, llvm.constF64(0.0), "fiszero"),
            };
            const cmpz = builder.buildZExt(is_zero, llvm.i32Type(), "lnot");
            const result = switch (kind) {
                .i32 => cmpz,
                .f64 => builder.buildSIToFP(cmpz, llvm.doubleType(), "lnotf"),
            };
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .inc_dec => {
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const val = numVstackPop(vstack);
            const result = if (handler.is_inc orelse false) switch (kind) {
                .i32 => builder.buildAdd(val, one_val, "inc"),
                .f64 => builder.buildFAdd(val, one_val, "finc"),
            } else switch (kind) {
                .i32 => builder.buildSub(val, one_val, "dec"),
                .f64 => builder.buildFSub(val, one_val, "fdec"),
            };
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .post_inc_dec => {
            // QuickJS post_inc: stack = [... val] → [... val, val+1]
            // TOS = new (incremented), below = old (original)
            // Usage: put_loc saves TOS (new) back, leaving old for array index
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const val = numVstackPop(vstack);
            const new_val = if (handler.is_inc orelse false) switch (kind) {
                .i32 => builder.buildAdd(val, one_val, "postinc"),
                .f64 => builder.buildFAdd(val, one_val, "fpostinc"),
            } else switch (kind) {
                .i32 => builder.buildSub(val, one_val, "postdec"),
                .f64 => builder.buildFSub(val, one_val, "fpostdec"),
            };
            // Push old value first (deeper), then new value (TOS)
            vstack.append(allocator, val) catch return CodegenError.OutOfMemory;
            vstack.append(allocator, new_val) catch return CodegenError.OutOfMemory;
        },

        .add_loc => {
            const idx: u32 = switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            const val = numVstackPop(vstack);
            const loc_ptr = locals.*[idx];
            const old_val = builder.buildLoad(elem_type, loc_ptr, "addloc_old");
            const new_val = switch (kind) {
                .i32 => builder.buildAdd(old_val, val, "addloc"),
                .f64 => blk: {
                    const fv = builder.buildFAdd(old_val, val, "faddloc");
                    llvm.Builder.setFastMathFlags(fv, llvm.Builder.FMF_VECTORIZE);
                    break :blk fv;
                },
            };
            _ = builder.buildStore(new_val, loc_ptr);
            if (kind == .f64 and counter_mask != 0 and idx < 256 and
                counter_mask & (@as(u64, 1) << @intCast(idx)) != 0)
            {
                _ = builder.buildStore(
                    builder.buildFPToSI(new_val, llvm.i32Type(), "sh_addloc"),
                    i32_shadows.*[idx],
                );
            }
        },

        .inc_loc => {
            const idx: u32 = switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (idx >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            const loc_ptr = locals.*[idx];
            const old_val = builder.buildLoad(elem_type, loc_ptr, "incloc_old");
            const new_val = switch (kind) {
                .i32 => builder.buildNSWAdd(old_val, one_val, "incloc"),
                .f64 => blk: {
                    const fv = builder.buildFAdd(old_val, one_val, "fincloc");
                    llvm.Builder.setFastMathFlags(fv, llvm.Builder.FMF_VECTORIZE);
                    break :blk fv;
                },
            };
            _ = builder.buildStore(new_val, loc_ptr);
            // Maintain i32 shadow: shadow += 1 (clean stride for vectorization)
            if (kind == .f64 and counter_mask != 0 and idx < 256 and
                counter_mask & (@as(u64, 1) << @intCast(idx)) != 0)
            {
                const sh_old = builder.buildLoad(llvm.i32Type(), i32_shadows.*[idx], "sh_inc_old");
                _ = builder.buildStore(
                    builder.buildNSWAdd(sh_old, llvm.constInt32(1), "sh_inc"),
                    i32_shadows.*[idx],
                );
            }
        },

        .dec_loc => {
            const idx: u32 = switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            if (idx >= @min(local_count, 256)) return CodegenError.UnsupportedOpcode;
            const loc_ptr = locals.*[idx];
            const old_val = builder.buildLoad(elem_type, loc_ptr, "decloc_old");
            const new_val = switch (kind) {
                .i32 => builder.buildNSWSub(old_val, one_val, "decloc"),
                .f64 => blk: {
                    const fv = builder.buildFSub(old_val, one_val, "fdecloc");
                    llvm.Builder.setFastMathFlags(fv, llvm.Builder.FMF_VECTORIZE);
                    break :blk fv;
                },
            };
            _ = builder.buildStore(new_val, loc_ptr);
            if (kind == .f64 and counter_mask != 0 and idx < 256 and
                counter_mask & (@as(u64, 1) << @intCast(idx)) != 0)
            {
                const sh_old = builder.buildLoad(llvm.i32Type(), i32_shadows.*[idx], "sh_dec_old");
                _ = builder.buildStore(
                    builder.buildNSWSub(sh_old, llvm.constInt32(1), "sh_dec"),
                    i32_shadows.*[idx],
                );
            }
        },

        .stack_dup => {
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const top = vstack.items[vstack.items.len - 1];
            vstack.append(allocator, top) catch return CodegenError.OutOfMemory;
        },

        .stack_dup2 => {
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const a = vstack.items[vstack.items.len - 2];
            const b = vstack.items[vstack.items.len - 1];
            vstack.append(allocator, a) catch return CodegenError.OutOfMemory;
            vstack.append(allocator, b) catch return CodegenError.OutOfMemory;
        },

        .stack_drop => {
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            _ = numVstackPop(vstack);
        },

        .stack_swap => {
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const a = vstack.items[vstack.items.len - 1];
            const b = vstack.items[vstack.items.len - 2];
            vstack.items[vstack.items.len - 1] = b;
            vstack.items[vstack.items.len - 2] = a;
        },

        .nop => {},

        .self_ref => {
            // Function reference marker. Resolve the callee for cross-function calls.
            if (instr.opcode == .get_var or instr.opcode == .get_var_undef) {
                switch (instr.operand) {
                    .atom => |atom_idx| {
                        if (getAtomStringStatic(analyzed, atom_idx)) |callee_name| {
                            if (module.getNamedFunction(callee_name)) |callee_fn| {
                                if (callee_fn != func) {
                                    pending_callee.* = callee_fn;
                                }
                            }
                        }
                    },
                    else => {},
                }
            } else {
                // Handle get_var_ref0/1/2/3/get_var_ref — resolve closure var index to callee
                const cv_idx: u32 = switch (instr.opcode) {
                    .get_var_ref0 => 0,
                    .get_var_ref1 => 1,
                    .get_var_ref2 => 2,
                    .get_var_ref3 => 3,
                    .get_var_ref => switch (instr.operand) {
                        .u16 => |v| v,
                        else => 0xFFFFFFFF,
                    },
                    else => 0xFFFFFFFF,
                };
                if (cv_idx < analyzed.closure_vars.len) {
                    const cv_name = analyzed.closure_vars[cv_idx].name;
                    if (cv_name.len > 0) {
                        var name_buf: [256]u8 = undefined;
                        if (cv_name.len < name_buf.len) {
                            @memcpy(name_buf[0..cv_name.len], cv_name);
                            name_buf[cv_name.len] = 0;
                            const name_z: [*:0]const u8 = @ptrCast(name_buf[0..cv_name.len]);
                            if (module.getNamedFunction(name_z)) |callee_fn| {
                                if (callee_fn != func) {
                                    pending_callee.* = callee_fn;
                                }
                            }
                        }
                    }
                }
            }
        },

        .call_self => {
            const call_argc: u32 = switch (instr.opcode) {
                .call0 => 0,
                .call1 => 1,
                .call2 => 2,
                .call3 => 3,
                .call => instr.operand.u16,
                else => 1,
            };
            if (vstack.items.len < call_argc) return CodegenError.StackUnderflow;

            // Determine call target: cross-function callee or self
            const call_target = if (pending_callee.*) |callee| blk: {
                pending_callee.* = null;
                if (c.LLVMCountParams(callee) == call_argc) break :blk callee;
                break :blk func; // Mismatch — fall back to self
            } else func;

            var call_args: [8]llvm.Value = undefined;
            var i: u32 = call_argc;
            while (i > 0) {
                i -= 1;
                call_args[i] = numVstackPop(vstack);
            }

            // Cross-tier call: detect if callee has different param types (i32 vs f64)
            // and insert conversions (sitofp for i32→f64, fptosi for f64→i32)
            const callee_type = c.LLVMGlobalGetValueType(call_target);
            const callee_ret = c.LLVMGetReturnType(callee_type);
            const needs_conv = call_target != func and callee_ret != elem_type;

            if (needs_conv) {
                // Convert args from caller type to callee type
                const callee_param_type = if (call_argc > 0)
                    c.LLVMGetReturnType(callee_type) // callee uses same type for params and return
                else
                    elem_type;
                for (0..call_argc) |j| {
                    if (c.LLVMTypeOf(call_args[j]) != callee_param_type) {
                        call_args[j] = if (kind == .i32)
                            builder.buildSIToFP(call_args[j], callee_param_type, "xcall_a")
                        else
                            builder.buildFPToSI(call_args[j], callee_param_type, "xcall_a");
                    }
                }
            }

            var param_types_buf: [8]llvm.Type = undefined;
            const call_elem = if (needs_conv) callee_ret else elem_type;
            for (0..call_argc) |j| {
                param_types_buf[j] = call_elem;
            }
            const call_fn_ty = llvm.functionType(call_elem, param_types_buf[0..call_argc], false);
            var result = builder.buildCall(call_fn_ty, call_target, call_args[0..call_argc], "call");

            // Convert return value back to caller type
            if (needs_conv) {
                result = if (kind == .i32)
                    builder.buildFPToSI(result, elem_type, "xcall_r")
                else
                    builder.buildSIToFP(result, elem_type, "xcall_r");
            }
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .tail_call_self => {
            const call_argc: u32 = instr.operand.u16;
            if (vstack.items.len < call_argc) return CodegenError.StackUnderflow;

            const call_target = if (pending_callee.*) |callee| blk: {
                pending_callee.* = null;
                if (c.LLVMCountParams(callee) == call_argc) break :blk callee;
                break :blk func;
            } else func;

            var call_args: [8]llvm.Value = undefined;
            var i: u32 = call_argc;
            while (i > 0) {
                i -= 1;
                call_args[i] = numVstackPop(vstack);
            }

            // Cross-tier tail call: same conversion logic as call_self
            const tc_callee_type = c.LLVMGlobalGetValueType(call_target);
            const tc_callee_ret = c.LLVMGetReturnType(tc_callee_type);
            const tc_needs_conv = call_target != func and tc_callee_ret != elem_type;

            if (tc_needs_conv) {
                for (0..call_argc) |j| {
                    if (c.LLVMTypeOf(call_args[j]) != tc_callee_ret) {
                        call_args[j] = if (kind == .i32)
                            builder.buildSIToFP(call_args[j], tc_callee_ret, "xtc_a")
                        else
                            builder.buildFPToSI(call_args[j], tc_callee_ret, "xtc_a");
                    }
                }
            }

            var param_types_buf: [8]llvm.Type = undefined;
            const tc_elem = if (tc_needs_conv) tc_callee_ret else elem_type;
            for (0..call_argc) |j| {
                param_types_buf[j] = tc_elem;
            }
            const call_fn_ty = llvm.functionType(tc_elem, param_types_buf[0..call_argc], false);
            var result = builder.buildCall(call_fn_ty, call_target, call_args[0..call_argc], "tail");

            // Convert return value back to caller's type before returning
            if (tc_needs_conv) {
                result = if (kind == .i32)
                    builder.buildFPToSI(result, elem_type, "xtc_r")
                else
                    builder.buildSIToFP(result, elem_type, "xtc_r");
            }
            _ = builder.buildRet(result);
            block_terminated.* = true;
        },

        .ret => {
            if (handler.value) |v| {
                _ = builder.buildRet(switch (kind) {
                    .i32 => llvm.constInt32(v),
                    .f64 => llvm.constF64(@floatFromInt(v)),
                });
            } else {
                if (vstack.items.len < 1) return CodegenError.StackUnderflow;
                const val = numVstackPop(vstack);
                _ = builder.buildRet(val);
            }
            block_terminated.* = true;
        },

        .if_false => {
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const cond_val = numVstackPop(vstack);

            // For f64 tier: compare != 0.0 to get truthiness as i1
            // Peephole: if cond_val is already i1 (from icmp peephole), invert directly
            const is_false = if (kind == .f64 and
                c.LLVMGetTypeKind(c.LLVMTypeOf(cond_val)) == c.LLVMIntegerTypeKind and
                c.LLVMGetIntTypeWidth(c.LLVMTypeOf(cond_val)) == 1)
                // i1 from icmp — invert: if_false branches when cmp is false
                builder.buildNot(cond_val, "ifz_inv")
            else switch (kind) {
                .i32 => builder.buildICmp(c.LLVMIntEQ, cond_val, llvm.constInt32(0), "ifz"),
                .f64 => builder.buildFCmp(c.LLVMRealOEQ, cond_val, llvm.constF64(0.0), "fifz"),
            };

            if (successors.len >= 2) {
                const false_target = successors[0];
                const true_target = successors[1];
                if (false_target < block_count and true_target < block_count) {
                    _ = builder.buildCondBr(is_false, llvm_blocks[false_target], llvm_blocks[true_target]);
                    block_terminated.* = true;
                }
            } else if (successors.len == 1 and successors[0] < block_count) {
                _ = builder.buildBr(llvm_blocks[successors[0]]);
                block_terminated.* = true;
            }
        },

        .if_true => {
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const cond_val = numVstackPop(vstack);

            // Peephole: if cond_val is already i1 (from icmp peephole), use directly
            const is_true = if (kind == .f64 and
                c.LLVMGetTypeKind(c.LLVMTypeOf(cond_val)) == c.LLVMIntegerTypeKind and
                c.LLVMGetIntTypeWidth(c.LLVMTypeOf(cond_val)) == 1)
                cond_val // i1 from icmp — use directly
            else switch (kind) {
                .i32 => builder.buildICmp(c.LLVMIntNE, cond_val, llvm.constInt32(0), "ift"),
                .f64 => builder.buildFCmp(c.LLVMRealONE, cond_val, llvm.constF64(0.0), "fift"),
            };

            if (successors.len >= 2) {
                const true_target = successors[0];
                const false_target = successors[1];
                if (true_target < block_count and false_target < block_count) {
                    _ = builder.buildCondBr(is_true, llvm_blocks[true_target], llvm_blocks[false_target]);
                    block_terminated.* = true;
                }
            } else if (successors.len == 1 and successors[0] < block_count) {
                _ = builder.buildBr(llvm_blocks[successors[0]]);
                block_terminated.* = true;
            }
        },

        .goto_br => {
            if (successors.len >= 1 and successors[0] < block_count) {
                _ = builder.buildBr(llvm_blocks[successors[0]]);
                block_terminated.* = true;
            }
        },

        .always_false => {
            // Pop a value, push false (0). In numeric context, values are never undefined/null.
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            _ = numVstackPop(vstack);
            vstack.append(allocator, zero_val) catch return CodegenError.OutOfMemory;
        },

        .array_get => {
            // arr[idx]: pop index, pop base pointer, load from WASM linear memory
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const idx_raw = numVstackPop(vstack);
            const base_raw = numVstackPop(vstack);
            // For f64 tier, base and index are f64 — convert to i32 for memory addressing.
            // Peephole: if value is sitofp(i32), grab the i32 operand directly.
            const base_i32 = switch (kind) {
                .i32 => base_raw,
                .f64 => if (c.LLVMGetInstructionOpcode(base_raw) == c.LLVMSIToFP)
                    c.LLVMGetOperand(base_raw, 0)
                else
                    builder.buildFPToSI(base_raw, llvm.i32Type(), "arr_base_i32"),
            };
            const idx_i32 = switch (kind) {
                .i32 => idx_raw,
                .f64 => if (c.LLVMGetInstructionOpcode(idx_raw) == c.LLVMSIToFP)
                    c.LLVMGetOperand(idx_raw, 0)
                else
                    builder.buildFPToSI(idx_raw, llvm.i32Type(), "arr_idx_i32"),
            };

            // Check if this is an array-of-struct arg (pool pointer).
            // For pool args, compute element address (base + index * stride)
            // instead of loading a value. The subsequent field_get loads the field.
            var is_pool_arg = false;
            if (struct_layout_global) |si| {
                if (si.array_of_struct_args != 0) {
                    var source = base_i32;
                    if (c.LLVMGetInstructionOpcode(source) == c.LLVMLoad)
                        source = c.LLVMGetOperand(source, 0);
                    for (0..@min(arg_count, 8)) |ai| {
                        if (si.array_of_struct_args & (@as(u8, 1) << @intCast(ai)) != 0 and
                            params.*[@intCast(ai)] == source)
                        {
                            // Stride in bytes: field_count * sizeof(i32)
                            const stride_bytes: i32 = @intCast(@as(u32, si.field_counts[@intCast(ai)]) * 4);
                            const stride_val = llvm.constInt32(stride_bytes);
                            const offset = c.LLVMBuildMul(builder.ref, idx_i32, stride_val, "aos_stride");
                            const elem_addr = c.LLVMBuildAdd(builder.ref, base_i32, offset, "aos_elem");
                            const result = switch (kind) {
                                .i32 => elem_addr,
                                .f64 => builder.buildSIToFP(elem_addr, llvm.doubleType(), "aos_elem_f64"),
                            };
                            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
                            is_pool_arg = true;
                            break;
                        }
                    }
                }
            }

            if (!is_pool_arg) {
                const base_ptr = c.LLVMBuildIntToPtr(builder.ref, base_i32, llvm.ptrType(), "arr_base");
                const arr_elem_type: llvm.Type = switch (kind) {
                    .i32 => llvm.i32Type(),
                    .f64 => llvm.doubleType(),
                };
                var gep_indices = [_]llvm.Value{idx_i32};
                const elem_ptr = c.LLVMBuildGEP2(builder.ref, arr_elem_type, base_ptr, &gep_indices, 1, "arr_gep");
                const loaded = builder.buildLoad(arr_elem_type, elem_ptr, "arr_elem");
                vstack.append(allocator, loaded) catch return CodegenError.OutOfMemory;
            }
        },

        .closure_read => {
            // Closure variable read: map get_var_ref{N} to extra WASM param.
            // The JS trampoline passes closure values as extra i32 args after
            // the regular function params.
            const closure_idx: u32 = switch (instr.opcode) {
                .get_var_ref0 => 0,
                .get_var_ref1 => 1,
                .get_var_ref2 => 2,
                .get_var_ref3 => 3,
                .get_var_ref => switch (instr.operand) {
                    .var_ref => |v| v,
                    .u16 => |v| v,
                    else => 0,
                },
                else => 0,
            };
            // Read from param[arg_count + closure_idx]
            // NOTE: currently extra params aren't added to WASM signature.
            // This needs the function signature to include extra params.
            // For now, push 0 (will be fixed when signature is extended).
            _ = closure_idx;
            const zero = switch (kind) {
                .i32 => llvm.constInt32(0),
                .f64 => llvm.constF64(0.0),
            };
            vstack.append(allocator, zero) catch return CodegenError.OutOfMemory;
        },
        .field_get => {
            // obj.prop: pop struct pointer, load i32 at fixed field offset
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const base_raw = numVstackPop(vstack);
            const base_i32 = switch (kind) {
                .i32 => base_raw,
                .f64 => if (c.LLVMGetInstructionOpcode(base_raw) == c.LLVMSIToFP)
                    c.LLVMGetOperand(base_raw, 0)
                else
                    builder.buildFPToSI(base_raw, llvm.i32Type(), "struct_base_i32"),
            };
            // Look up field offset from struct layout
            const atom: u32 = switch (instr.operand) {
                .atom => |a| a,
                else => return CodegenError.UnsupportedOpcode,
            };
            var field_offset: u32 = 0;
            if (struct_layout_global) |si| {
                // Identify which arg produced this struct pointer by tracing
                // the LLVM value back through load/sitofp/add to the param alloca.
                var source_alloca = base_i32;
                // AOS path: base is add(load(alloca), mul(idx, stride))
                if (c.LLVMGetInstructionOpcode(source_alloca) == c.LLVMAdd)
                    source_alloca = c.LLVMGetOperand(source_alloca, 0);
                if (c.LLVMGetInstructionOpcode(source_alloca) == c.LLVMLoad)
                    source_alloca = c.LLVMGetOperand(source_alloca, 0);
                const combined_mask = si.struct_args | si.array_of_struct_args;
                var found_arg: u3 = 0;
                for (0..@min(arg_count, 8)) |ai| {
                    if (combined_mask & (@as(u8, 1) << @intCast(ai)) != 0 and
                        params.*[@intCast(ai)] == source_alloca)
                    {
                        found_arg = @intCast(ai);
                        break;
                    }
                }
                // Find atom in field list → offset
                for (si.field_atoms[found_arg][0..si.field_counts[found_arg]], 0..) |fa, oi| {
                    if (fa == atom) {
                        field_offset = @intCast(oi);
                        break;
                    }
                }
                if (std.posix.getenv("EDGEBOX_WASM_DEBUG") != null) {
                    std.debug.print("[codegen] field_get: arg={d} atom={d} offset={d} fields={d}\n", .{ found_arg, atom, field_offset, si.field_counts[found_arg] });
                }
            }
            const base_ptr = c.LLVMBuildIntToPtr(builder.ref, base_i32, llvm.ptrType(), "struct_ptr");
            const offset_val = llvm.constInt32(@intCast(field_offset));
            var gep_indices = [_]llvm.Value{offset_val};
            const field_ptr = c.LLVMBuildGEP2(builder.ref, llvm.i32Type(), base_ptr, &gep_indices, 1, "field_gep");
            const loaded = builder.buildLoad(llvm.i32Type(), field_ptr, "field_val");
            // In f64 tier, convert loaded i32 to f64
            const result = switch (kind) {
                .i32 => loaded,
                .f64 => builder.buildSIToFP(loaded, llvm.doubleType(), "field_f64"),
            };
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .array_get2 => {
            // arr[idx] keeping arr on stack: pop index, peek base, load, push result
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const idx_raw = numVstackPop(vstack);
            const base_raw = vstack.items[vstack.items.len - 1]; // peek
            const base_i32 = switch (kind) {
                .i32 => base_raw,
                .f64 => if (c.LLVMGetInstructionOpcode(base_raw) == c.LLVMSIToFP)
                    c.LLVMGetOperand(base_raw, 0)
                else
                    builder.buildFPToSI(base_raw, llvm.i32Type(), "arr_base2_i32"),
            };
            const idx_i32 = switch (kind) {
                .i32 => idx_raw,
                .f64 => if (c.LLVMGetInstructionOpcode(idx_raw) == c.LLVMSIToFP)
                    c.LLVMGetOperand(idx_raw, 0)
                else
                    builder.buildFPToSI(idx_raw, llvm.i32Type(), "arr_idx2_i32"),
            };
            const base_ptr = c.LLVMBuildIntToPtr(builder.ref, base_i32, llvm.ptrType(), "arr_base2");
            const arr_elem_type: llvm.Type = switch (kind) {
                .i32 => llvm.i32Type(),
                .f64 => llvm.doubleType(),
            };
            var gep_indices = [_]llvm.Value{idx_i32};
            const elem_ptr = c.LLVMBuildGEP2(builder.ref, arr_elem_type, base_ptr, &gep_indices, 1, "arr_gep2");
            const loaded = builder.buildLoad(arr_elem_type, elem_ptr, "arr_elem2");
            vstack.append(allocator, loaded) catch return CodegenError.OutOfMemory;
        },

        .array_put => {
            // arr[idx] = val: pop value, pop index, pop base, store to memory
            if (vstack.items.len < 3) return CodegenError.StackUnderflow;
            const val = numVstackPop(vstack);
            const idx_raw = numVstackPop(vstack);
            const base_raw = numVstackPop(vstack);
            const base_i32 = switch (kind) {
                .i32 => base_raw,
                .f64 => if (c.LLVMGetInstructionOpcode(base_raw) == c.LLVMSIToFP)
                    c.LLVMGetOperand(base_raw, 0)
                else
                    builder.buildFPToSI(base_raw, llvm.i32Type(), "arr_basew_i32"),
            };
            const idx_i32 = switch (kind) {
                .i32 => idx_raw,
                .f64 => if (c.LLVMGetInstructionOpcode(idx_raw) == c.LLVMSIToFP)
                    c.LLVMGetOperand(idx_raw, 0)
                else
                    builder.buildFPToSI(idx_raw, llvm.i32Type(), "arr_idxw_i32"),
            };
            const base_ptr = c.LLVMBuildIntToPtr(builder.ref, base_i32, llvm.ptrType(), "arr_base_w");
            const arr_elem_type: llvm.Type = switch (kind) {
                .i32 => llvm.i32Type(),
                .f64 => llvm.doubleType(),
            };
            var gep_indices = [_]llvm.Value{idx_i32};
            const elem_ptr = c.LLVMBuildGEP2(builder.ref, arr_elem_type, base_ptr, &gep_indices, 1, "arr_gep_w");
            _ = builder.buildStore(val, elem_ptr);
        },

        .array_length => {
            // Fallback: get_length not preceded by get_arg — not supported in standalone WASM
            // (The peephole in generateNumericBody handles the common get_arg+get_length pattern)
            return CodegenError.UnsupportedOpcode;
        },

        .unsupported => return CodegenError.UnsupportedOpcode,
    }
}

// ============================================================================
// Shard Init Function Generation
// ============================================================================

/// Generate frozen_init_llvm_shard_N() function that registers all frozen functions.
pub fn generateShardInit(
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

    // For each function, register by parser index for O(1) dispatch lookup
    var count: i32 = 0;
    for (funcs) |fi| {
        // Skip Phase 2 (byte-scanned) functions — they have no valid runtime index
        if (fi.parser_index == 0xFFFFFFFF) continue;

        // Get pointer to the frozen function
        var func_name_buf: [512]u8 = undefined;
        const func_name = std.fmt.bufPrintZ(&func_name_buf, "{s}", .{fi.llvm_func_name}) catch continue;
        const frozen_fn = module.getNamedFunction(func_name) orelse continue;

        // Build call: native_dispatch_register_by_index(parser_index, frozen_fn)
        const reg_fn_ty = llvm.functionType(llvm.voidType(), &[_]llvm.Type{ llvm.i32Type(), llvm.ptrType() }, false);
        var call_args = [_]llvm.Value{ llvm.constInt32(@intCast(fi.parser_index)), frozen_fn };
        _ = builder.buildCall(reg_fn_ty, register_fn, &call_args, "");

        count += 1;
    }

    _ = builder.buildRet(llvm.constInt32(count));
    _ = allocator;
}


/// Look up atom string by index — returns null-terminated C string or null.
pub fn getAtomStringStatic(func: AnalyzedFunction, atom_idx: u32) ?[*:0]const u8 {
    if (atom_idx < module_parser.JS_ATOM_END) {
        if (atom_idx < module_parser.BUILTIN_ATOMS.len) {
            const name = module_parser.BUILTIN_ATOMS[atom_idx];
            if (name.len > 0 and name[0] != '<') {
                return @ptrCast(name.ptr);
            }
        }
        return null;
    }
    const adjusted_idx = atom_idx - module_parser.JS_ATOM_END;
    if (adjusted_idx < func.atom_strings.len) {
        const str = func.atom_strings[adjusted_idx];
        if (str.len > 0) {
            return @ptrCast(str.ptr);
        }
        std.debug.print("[ATOM_WARN] User atom {d} (adjusted {d}) has empty string in func '{s}'\n", .{ atom_idx, adjusted_idx, func.name });
    } else {
        std.debug.print("[ATOM_WARN] User atom {d} (adjusted {d}) out of range (atom_strings.len={d}) in func '{s}'\n", .{ atom_idx, adjusted_idx, func.atom_strings.len, func.name });
    }
    return null;
}

// ============================================================================
// Tier B: Thin Codegen — extracted to llvm_thin_codegen.zig
// ============================================================================
const thin = @import("llvm_thin_codegen.zig");
pub const ThinShardFunction = ShardFunction;
pub const generateThinShard = thin.generateThinShard;
pub const generateThinShardWasm = thin.generateThinShardWasm;
