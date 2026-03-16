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
const AnalyzedFunction = frozen_registry.AnalyzedFunction;

const c = llvm.c;

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
const native_target = TargetInfo{ .is_wasm32 = false };

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
const CV_QNAN_INT: u64 = 0x7FF9_0000_0000_0000; // integer tag
const CV_TAG_MASK: u64 = 0xFFFF_0000_0000_0000; // mask for type tag
const CV_QNAN_PTR: u64 = 0x7FFD_0000_0000_0000; // object pointer (TAG_PTR)
const CV_QNAN_STR: u64 = 0x7FFF_0000_0000_0000; // string
const CV_UNDEFINED: u64 = 0x7FFC_0000_0000_0000;
const CV_NULL: u64 = 0x7FFB_0000_0000_0000;
const CV_TRUE: u64 = 0x7FFA_0000_0000_0001;
const CV_FALSE: u64 = 0x7FFA_0000_0000_0000;
const CV_PAYLOAD_MASK: u64 = 0x0000_FFFF_FFFF_FFFF; // lower 48 bits
const CV_UNINITIALIZED: u64 = 0x7FFE_0000_0000_0000; // QNAN | TAG_UNINIT (TDZ sentinel)
// Negative NaN space ref types (sign bit set)
const CV_QNAN_SYMBOL: u64 = 0xFFF9_0000_0000_0000; // symbol pointer
const CV_QNAN_BIGINT: u64 = 0xFFFA_0000_0000_0000; // bigint pointer
const CV_QNAN_FUNCBC: u64 = 0xFFFB_0000_0000_0000; // function bytecode pointer

/// Create a CV integer constant at comptime: QNAN_INT | (val & 0xFFFFFFFF)
fn cvConstInt(val: i32) u64 {
    return CV_QNAN_INT | (@as(u64, @as(u32, @bitCast(val))));
}

// ============================================================================
// Inline LLVM IR helpers for CV operations
// ============================================================================

/// Load the stack pointer value: sp = *sp_ptr (usize: i64 native, i32 wasm32)
fn inlineLoadSp(b: llvm.Builder, sp_ptr: llvm.Value) llvm.Value {
    return b.buildLoad(llvm.i64Type(), sp_ptr, "sp");
}

fn inlineLoadSpTarget(b: llvm.Builder, sp_ptr: llvm.Value, target: TargetInfo) llvm.Value {
    return b.buildLoad(target.usizeType(), sp_ptr, "sp");
}

/// Store the stack pointer value: *sp_ptr = sp
fn inlineStoreSp(b: llvm.Builder, sp_ptr: llvm.Value, sp_val: llvm.Value) void {
    _ = b.buildStore(sp_val, sp_ptr);
}

/// Get pointer to stack[index] where index is an i64 LLVM value
fn inlineStackSlot(b: llvm.Builder, stk: llvm.Value, index: llvm.Value) llvm.Value {
    return b.buildInBoundsGEP(llvm.i64Type(), stk, &.{index}, "slot");
}

/// Get pointer to locals[idx] where idx is a u32 constant
fn inlineLocalSlot(b: llvm.Builder, locs: llvm.Value, idx: u32) llvm.Value {
    return b.buildInBoundsGEP(llvm.i64Type(), locs, &.{llvm.constInt(llvm.i64Type(), idx, false)}, "lslot");
}

/// Push a constant CV (u64) onto the stack: stack[sp] = cv_bits; sp++
fn inlinePushCV(b: llvm.Builder, stk: llvm.Value, sp_ptr: llvm.Value, cv_bits: u64) void {
    const sp = inlineLoadSp(b, sp_ptr);
    const slot = inlineStackSlot(b, stk, sp);
    _ = b.buildStore(llvm.constInt(llvm.i64Type(), cv_bits, false), slot);
    const new_sp = b.buildAdd(sp, llvm.constInt64(1), "sp1");
    inlineStoreSp(b, sp_ptr, new_sp);
}

/// Push a dynamic i64 value onto the stack: stack[sp] = val; sp++
fn inlinePushValue(b: llvm.Builder, stk: llvm.Value, sp_ptr: llvm.Value, val: llvm.Value) void {
    const sp = inlineLoadSp(b, sp_ptr);
    const slot = inlineStackSlot(b, stk, sp);
    _ = b.buildStore(val, slot);
    const new_sp = b.buildAdd(sp, llvm.constInt64(1), "sp1");
    inlineStoreSp(b, sp_ptr, new_sp);
}

/// Check if a CV value is an integer: (cv & TAG_MASK) == QNAN_INT
fn inlineIsInt(b: llvm.Builder, val: llvm.Value) llvm.Value {
    const tag = b.buildAnd(val, llvm.constInt(llvm.i64Type(), CV_TAG_MASK, false), "tag");
    return b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt(llvm.i64Type(), CV_QNAN_INT, false), "isint");
}

/// Extract i32 from a CV integer: trunc(cv) to i32
fn inlineGetInt(b: llvm.Builder, val: llvm.Value) llvm.Value {
    return b.buildTrunc(val, llvm.i32Type(), "ival");
}

/// Create a CV integer from an i32: QNAN_INT | zext(val)
fn inlineNewInt(b: llvm.Builder, i32_val: llvm.Value) llvm.Value {
    const ext = b.buildZExt(i32_val, llvm.i64Type(), "zext");
    return b.buildOr(ext, llvm.constInt(llvm.i64Type(), CV_QNAN_INT, false), "newcv");
}

/// Inline dupRef: if isRefType(cv) then call rt_cv_dup_ref, else return cv unchanged.
/// Returns the (possibly dup'd) CV value.
/// Target-aware: uses NaN-boxing tags on WASM32, JSValue tag field on native.
fn inlineDupRef(tctx: *ThinCodegenCtx, val: llvm.Value) llvm.Value {
    const b = tctx.builder;
    const cv_type = tctx.cvTy();

    // Check if ref type
    const is_ref = if (tctx.target.is_wasm32) blk: {
        // WASM32: NaN-boxing — check upper 16-bit tags
        const tag = b.buildAnd(val, llvm.constInt(llvm.i64Type(), CV_TAG_MASK, false), "dtag");
        const is_ptr = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt(llvm.i64Type(), CV_QNAN_PTR, false), "isptr");
        const is_str = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt(llvm.i64Type(), CV_QNAN_STR, false), "isstr");
        const is_sym = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt(llvm.i64Type(), CV_QNAN_SYMBOL, false), "issym");
        const is_big = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt(llvm.i64Type(), CV_QNAN_BIGINT, false), "isbig");
        const is_fbc = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt(llvm.i64Type(), CV_QNAN_FUNCBC, false), "isfbc");
        break :blk b.buildOr(b.buildOr(b.buildOr(b.buildOr(is_ptr, is_str, "r1"), is_sym, "r2"), is_big, "r3"), is_fbc, "isref");
    } else blk: {
        // Native: JSValue tag field (element 1). All ref-counted types have negative tags:
        // OBJECT(-1), FUNCTION_BYTECODE(-2), STRING(-7), SYMBOL(-8), BIG_INT(-9)
        // Non-ref types are non-negative: INT(0), BOOL(1), NULL(2), UNDEFINED(3), FLOAT64(8), etc.
        const tag = c.LLVMBuildExtractValue(b.ref, val, 1, "dtag");
        break :blk b.buildICmp(c.LLVMIntSLT, tag, llvm.constInt64(0), "isref");
    };

    // Branch
    const current_bb = c.LLVMGetInsertBlock(b.ref);
    const current_fn = c.LLVMGetBasicBlockParent(current_bb);
    const dup_bb = llvm.appendBasicBlock(current_fn, "dup_ref");
    const merge_bb = llvm.appendBasicBlock(current_fn, "dup_merge");
    _ = b.buildCondBr(is_ref, dup_bb, merge_bb);

    // Ref path: call cv_dup_ref
    b.positionAtEnd(dup_bb);
    const duped = b.buildCall(
        llvm.functionType(cv_type, &.{cv_type}, false),
        tctx.rt.cv_dup_ref, &.{val}, "duped",
    );
    _ = b.buildBr(merge_bb);

    // Merge with phi
    b.positionAtEnd(merge_bb);
    const phi = b.buildPhi(cv_type, "dupv");
    var vals = [_]llvm.Value{ val, duped };
    var bbs = [_]llvm.BasicBlock{ current_bb, dup_bb };
    llvm.addIncoming(phi, &vals, &bbs);

    return phi;
}

/// Inline freeRef: if isRefType(cv) then call rt_cv_free_ref(ctx, cv)
/// Target-aware: uses NaN-boxing tags on WASM32, JSValue tag field on native.
fn inlineFreeRef(tctx: *ThinCodegenCtx, val: llvm.Value) void {
    const b = tctx.builder;
    const cv_type = tctx.cvTy();

    // Check if ref type
    const is_ref = if (tctx.target.is_wasm32) blk: {
        const tag = b.buildAnd(val, llvm.constInt(llvm.i64Type(), CV_TAG_MASK, false), "ftag");
        const is_ptr = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt(llvm.i64Type(), CV_QNAN_PTR, false), "fisptr");
        const is_str = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt(llvm.i64Type(), CV_QNAN_STR, false), "fisstr");
        const is_sym = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt(llvm.i64Type(), CV_QNAN_SYMBOL, false), "fissym");
        const is_big = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt(llvm.i64Type(), CV_QNAN_BIGINT, false), "fisbig");
        const is_fbc = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt(llvm.i64Type(), CV_QNAN_FUNCBC, false), "fisfbc");
        break :blk b.buildOr(b.buildOr(b.buildOr(b.buildOr(is_ptr, is_str, "fr1"), is_sym, "fr2"), is_big, "fr3"), is_fbc, "fisref");
    } else blk: {
        // Native: all ref-counted types have negative tags (tag < 0)
        const tag = c.LLVMBuildExtractValue(b.ref, val, 1, "ftag");
        break :blk b.buildICmp(c.LLVMIntSLT, tag, llvm.constInt64(0), "fisref");
    };

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const free_bb = llvm.appendBasicBlock(current_fn, "free_ref");
    const skip_bb = llvm.appendBasicBlock(current_fn, "free_skip");
    _ = b.buildCondBr(is_ref, free_bb, skip_bb);

    b.positionAtEnd(free_bb);
    _ = b.buildCall(
        llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), cv_type }, false),
        tctx.rt.cv_free_ref, &.{ tctx.ctx_param, val }, "",
    );
    _ = b.buildBr(skip_bb);

    b.positionAtEnd(skip_bb);
}

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
fn finalizeAndEmitShard(
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

    // === Pass 1: Declare all functions (so cross-function calls can resolve) ===
    // For functions that access arr.length, we add extra i32 params for each array length.
    var length_args_buf: [64]u8 = undefined; // per-function length_args bitmask
    // Synthetic names for anonymous functions (arrow/IIFE): "__anon_L{line_num}"
    var synth_names: [64][32]u8 = undefined;
    for (functions, 0..) |sf, fi| {
        const func = sf.func;

        // Generate synthetic name for anonymous functions (null-terminated for sliceTo)
        if (sf.name.len == 0 and fi < synth_names.len) {
            _ = std.fmt.bufPrintZ(&synth_names[fi], "__anon_L{d}", .{sf.func.line_num}) catch {};
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
                generateNumericBody(.i32, allocator, builder, standalone_fn, func, sf.cfg, native.module, length_args) catch |err| {
                    std.debug.print("[standalone-wasm] {s}: codegen failed: {}\n", .{ sf.name, err });
                    break :blk false;
                };
                break :blk true;
            },
            .f64 => blk: {
                generateNumericBody(.f64, allocator, builder, standalone_fn, func, sf.cfg, native.module, length_args) catch |err| {
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

    // Dump pre-optimization IR for debugging
    if (std.posix.getenv("EDGEBOX_DUMP_WASM_IR")) |_| {
        const ir = native.module.printToString();
        defer llvm.disposeMessage(ir);
        std.debug.print("=== STANDALONE WASM IR (pre-opt) ===\n{s}\n=== END ===\n", .{ir});
    }
    // Use O2 for standalone WASM — enables auto-vectorization for float loops
    module_disposed = try finalizeAndEmitShard(native, obj_path, "standalone-wasm", "default<O2>");
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
};

const GeneratedFuncInfo = struct {
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

fn declareNativeDispatchRegisterByIndex(module: llvm.Module) llvm.Value {
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
    var locals_buf: [48]llvm.Value = undefined;
    const local_count = analyzed.var_count;
    for (0..@min(local_count, 48)) |i| {
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
    locals: *[48]llvm.Value,
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
                else => 0,
            };
            if (idx >= arg_count) return CodegenError.UnsupportedOpcode;
            const val = builder.buildLoad(llvm.i32Type(), params[idx], "arg");
            vstack.append(allocator, val) catch return CodegenError.OutOfMemory;
        },

        .put_arg_i32 => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .u8 => |a| a,
                .u16 => |a| a,
                else => 0,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= arg_count) return CodegenError.UnsupportedOpcode;
            const val = i32VstackPop(vstack);
            _ = builder.buildStore(val, params.*[idx]);
        },

        .get_loc_i32 => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .u8 => |a| a,
                .u16 => |a| a,
                else => 0,
            };
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
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
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            const val = i32VstackPop(vstack);
            _ = builder.buildStore(val, locals.*[idx]);
        },

        .set_loc_i32 => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .u8 => |a| a,
                .u16 => |a| a,
                else => 0,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            const val = vstack.items[vstack.items.len - 1]; // peek, don't pop
            _ = builder.buildStore(val, locals.*[idx]);
        },

        .binary_arith_i32 => {
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const rhs = i32VstackPop(vstack);
            const lhs = i32VstackPop(vstack);
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
            const rhs = i32VstackPop(vstack);
            const lhs = i32VstackPop(vstack);
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
            else if (std.mem.eql(u8, op, ">>>"))
                builder.buildLShr(lhs, rhs, "shru")
            else
                return CodegenError.UnsupportedOpcode;

            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .binary_cmp_i32 => {
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const rhs = i32VstackPop(vstack);
            const lhs = i32VstackPop(vstack);
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
            const val = i32VstackPop(vstack);
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
            const val = i32VstackPop(vstack);
            // Logical NOT: 0 → 1, nonzero → 0
            const is_zero = builder.buildICmp(c.LLVMIntEQ, val, llvm.constInt32(0), "iszero");
            const result = builder.buildZExt(is_zero, llvm.i32Type(), "lnot");
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .get_loc_check_i32 => {
            // Same as get_loc — in int32 context, locals are always initialized
            const idx: u32 = switch (instr.operand) {
                .u8 => |a| a,
                .u16 => |a| a,
                else => 0,
            };
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            const loc_ptr = locals.*[idx];
            const val = builder.buildLoad(llvm.i32Type(), loc_ptr, "loc");
            vstack.append(allocator, val) catch return CodegenError.OutOfMemory;
        },

        .put_loc_check_i32 => {
            // Same as put_loc — const check irrelevant in int32 context
            const idx: u32 = switch (instr.operand) {
                .u8 => |a| a,
                .u16 => |a| a,
                else => 0,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            const val = i32VstackPop(vstack);
            _ = builder.buildStore(val, locals.*[idx]);
        },

        .set_loc_uninitialized_i32 => {
            // In int32 context, "uninitialized" = 0
            const idx: u32 = switch (instr.operand) {
                .u8 => |a| a,
                .u16 => |a| a,
                else => 0,
            };
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
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
                .u8 => |a| a,
                .u16 => |a| a,
                else => 0,
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
                .u16 => |a| a,
                else => 0,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
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
                .u16 => |a| a,
                else => 0,
            };
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            const loc_ptr = locals.*[idx];
            const old_val = builder.buildLoad(llvm.i32Type(), loc_ptr, "incloc_old");
            const new_val = builder.buildNSWAdd(old_val, llvm.constInt32(1), "incloc_new");
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
            if (std.mem.eql(u8, op, "dup")) {
                if (vstack.items.len < 1) return CodegenError.StackUnderflow;
                const top = vstack.items[vstack.items.len - 1];
                vstack.append(allocator, top) catch return CodegenError.OutOfMemory;
            } else {
                // drop
                if (vstack.items.len < 1) return CodegenError.StackUnderflow;
                _ = i32VstackPop(vstack);
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
) CodegenError!void {
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

    // Create LLVM basic blocks
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

    // Allocate locals with comptime-selected type
    builder.positionAtEnd(llvm_blocks[0]);
    var locals_buf: [48]llvm.Value = undefined;
    const local_count = analyzed.var_count;
    for (0..@min(local_count, 48)) |i| {
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
        var skip_next = false;

        // Process instructions. For the last instruction that is a terminator,
        // spill the vstack BEFORE emitting the branch.
        for (block.instructions, 0..) |instr, instr_idx| {
            if (skip_next) { skip_next = false; continue; }
            if (block_terminated) break;

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
                            skip_next = true;
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
        .set_arg, .set_loc, .nop, .set_loc_uninitialized, .inc_loc => 0,
        .binary_arith, .binary_cmp, .bitwise_binary, .array_get => -1, // pop 2, push 1
        .unary, .lnot, .inc_dec, .always_false, .array_length => 0, // pop 1, push 1
        .post_inc_dec => 1, // pop 1, push 2
        .stack_dup => 1,
        .stack_swap => 0,
        .self_ref => 0, // doesn't affect vstack in our impl (sets pending_callee)
        .call_self => blk: {
            const argc: i32 = switch (instr.opcode) {
                .call1 => 1, .call2 => 2, .call3 => 3,
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

/// Memory-backed version of emitNumericInstruction. Uses an alloca array + sp
/// for the evaluation stack, enabling values to persist across basic block
/// boundaries (required when QuickJS bytecode has live stack values at branches).
fn emitNumericInstructionMem(
    comptime kind: ValueKind,
    builder: llvm.Builder,
    instr: Instruction,
    params: *[8]llvm.Value,
    arg_count: u32,
    locals: *[48]llvm.Value,
    local_count: u32,
    llvm_blocks: []llvm.BasicBlock,
    block_count: usize,
    successors: []const u32,
    func: llvm.Value,
    block_terminated: *bool,
    pending_callee: *?llvm.Value,
    module: llvm.Module,
    analyzed: AnalyzedFunction,
    va: llvm.Value, // vstack array alloca
    sp: llvm.Value, // sp alloca (i32)
    zero_val: llvm.Value,
    elem_type: llvm.Type,
) CodegenError!void {
    const handler = numeric_handlers.getHandler(instr.opcode);
    const one_val: llvm.Value = switch (kind) {
        .i32 => llvm.constInt32(1),
        .f64 => llvm.constF64(1.0),
    };

    // Helpers bound to our memory stack
    const push = memVstackPush;
    const pop = memVstackPop;
    const peek = memVstackPeek;

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
            push(builder, va, sp, val, elem_type);
        },

        .push_cpool => {
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
                    .i32 => llvm.constInt32(@as(i32, @intFromFloat(v))),
                    .f64 => llvm.constF64(v),
                },
                else => return CodegenError.UnsupportedOpcode,
            };
            push(builder, va, sp, val, elem_type);
        },

        .push_bool => {
            const val = if (handler.value != null and handler.value.? != 0) one_val else zero_val;
            push(builder, va, sp, val, elem_type);
        },

        .get_arg => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .arg => |a| a, .u8 => |a| a, else => 0,
            };
            if (idx >= arg_count) return CodegenError.UnsupportedOpcode;
            const val = builder.buildLoad(elem_type, params[idx], "arg");
            push(builder, va, sp, val, elem_type);
        },

        .put_arg => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .arg => |a| a, .u8 => |a| a, .u16 => |a| a, else => 0,
            };
            if (idx >= arg_count) return CodegenError.UnsupportedOpcode;
            _ = builder.buildStore(pop(builder, va, sp, elem_type), params.*[idx]);
        },

        .set_arg => {
            const idx: u32 = if (handler.index) |i| i else switch (instr.operand) {
                .arg => |a| a, .u8 => |a| a, .u16 => |a| a, else => 0,
            };
            if (idx >= arg_count) return CodegenError.UnsupportedOpcode;
            _ = builder.buildStore(peek(builder, va, sp, elem_type), params.*[idx]);
        },

        .get_loc, .get_loc_check => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .loc => |a| a, .u8 => |a| a, .u16 => |a| a, else => 0,
            };
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            push(builder, va, sp, builder.buildLoad(elem_type, locals.*[idx], "loc"), elem_type);
        },

        .get_loc_pair => {
            if (0 >= @min(local_count, 48) or 1 >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            push(builder, va, sp, builder.buildLoad(elem_type, locals.*[0], "loc0"), elem_type);
            push(builder, va, sp, builder.buildLoad(elem_type, locals.*[1], "loc1"), elem_type);
        },

        .put_loc, .put_loc_check => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .u8 => |a| a, .u16 => |a| a, .loc => |a| a, else => 0,
            };
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            _ = builder.buildStore(pop(builder, va, sp, elem_type), locals.*[idx]);
        },

        .set_loc => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .u8 => |a| a, .u16 => |a| a, .loc => |a| a, else => 0,
            };
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            _ = builder.buildStore(peek(builder, va, sp, elem_type), locals.*[idx]);
        },

        .set_loc_uninitialized => {
            const idx: u32 = switch (instr.operand) {
                .u8 => |a| a, .u16 => |a| a, .loc => |a| a, else => 0,
            };
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            _ = builder.buildStore(zero_val, locals.*[idx]);
        },

        .binary_arith => {
            const rhs = pop(builder, va, sp, elem_type);
            const lhs = pop(builder, va, sp, elem_type);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;
            const result = switch (kind) {
                .i32 => if (std.mem.eql(u8, op, "+")) builder.buildAdd(lhs, rhs, "add")
                    else if (std.mem.eql(u8, op, "-")) builder.buildSub(lhs, rhs, "sub")
                    else if (std.mem.eql(u8, op, "*")) builder.buildMul(lhs, rhs, "mul")
                    else if (std.mem.eql(u8, op, "/")) builder.buildSDiv(lhs, rhs, "div")
                    else if (std.mem.eql(u8, op, "%")) builder.buildSRem(lhs, rhs, "rem")
                    else return CodegenError.UnsupportedOpcode,
                .f64 => if (std.mem.eql(u8, op, "+")) builder.buildFAdd(lhs, rhs, "fadd")
                    else if (std.mem.eql(u8, op, "-")) builder.buildFSub(lhs, rhs, "fsub")
                    else if (std.mem.eql(u8, op, "*")) builder.buildFMul(lhs, rhs, "fmul")
                    else if (std.mem.eql(u8, op, "/")) builder.buildFDiv(lhs, rhs, "fdiv")
                    else if (std.mem.eql(u8, op, "%")) builder.buildFRem(lhs, rhs, "frem")
                    else return CodegenError.UnsupportedOpcode,
            };
            push(builder, va, sp, result, elem_type);
        },

        .bitwise_binary => {
            if (kind != .i32) return CodegenError.UnsupportedOpcode;
            const rhs = pop(builder, va, sp, elem_type);
            const lhs = pop(builder, va, sp, elem_type);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;
            const result = if (std.mem.eql(u8, op, "&")) builder.buildAnd(lhs, rhs, "and")
                else if (std.mem.eql(u8, op, "|")) builder.buildOr(lhs, rhs, "or")
                else if (std.mem.eql(u8, op, "^")) builder.buildXor(lhs, rhs, "xor")
                else if (std.mem.eql(u8, op, "<<")) builder.buildShl(lhs, rhs, "shl")
                else if (std.mem.eql(u8, op, ">>")) builder.buildAShr(lhs, rhs, "shr")
                else if (std.mem.eql(u8, op, ">>>")) builder.buildLShr(lhs, rhs, "shru")
                else return CodegenError.UnsupportedOpcode;
            push(builder, va, sp, result, elem_type);
        },

        .binary_cmp => {
            const rhs = pop(builder, va, sp, elem_type);
            const lhs = pop(builder, va, sp, elem_type);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;
            const cmp = switch (kind) {
                .i32 => blk: {
                    const pred: c.LLVMIntPredicate = if (std.mem.eql(u8, op, "<")) c.LLVMIntSLT
                        else if (std.mem.eql(u8, op, "<=")) c.LLVMIntSLE
                        else if (std.mem.eql(u8, op, ">")) c.LLVMIntSGT
                        else if (std.mem.eql(u8, op, ">=")) c.LLVMIntSGE
                        else if (std.mem.eql(u8, op, "==")) c.LLVMIntEQ
                        else if (std.mem.eql(u8, op, "!=")) c.LLVMIntNE
                        else return CodegenError.UnsupportedOpcode;
                    break :blk builder.buildICmp(pred, lhs, rhs, "cmp");
                },
                .f64 => blk: {
                    const pred: c.LLVMRealPredicate = if (std.mem.eql(u8, op, "<")) c.LLVMRealOLT
                        else if (std.mem.eql(u8, op, "<=")) c.LLVMRealOLE
                        else if (std.mem.eql(u8, op, ">")) c.LLVMRealOGT
                        else if (std.mem.eql(u8, op, ">=")) c.LLVMRealOGE
                        else if (std.mem.eql(u8, op, "==")) c.LLVMRealOEQ
                        else if (std.mem.eql(u8, op, "!=")) c.LLVMRealONE
                        else return CodegenError.UnsupportedOpcode;
                    break :blk builder.buildFCmp(pred, lhs, rhs, "fcmp");
                },
            };
            const cmpz = builder.buildZExt(cmp, llvm.i32Type(), "cmpz");
            const result = switch (kind) {
                .i32 => cmpz,
                .f64 => builder.buildSIToFP(cmpz, llvm.doubleType(), "cmpf"),
            };
            push(builder, va, sp, result, elem_type);
        },

        .unary => {
            const val = pop(builder, va, sp, elem_type);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;
            const result = if (std.mem.eql(u8, op, "-")) switch (kind) {
                .i32 => builder.buildNeg(val, "neg"),
                .f64 => builder.buildFNeg(val, "fneg"),
            } else if (std.mem.eql(u8, op, "~")) blk: {
                if (kind != .i32) return CodegenError.UnsupportedOpcode;
                break :blk builder.buildNot(val, "bnot");
            } else return CodegenError.UnsupportedOpcode;
            push(builder, va, sp, result, elem_type);
        },

        .lnot => {
            const val = pop(builder, va, sp, elem_type);
            const is_zero = switch (kind) {
                .i32 => builder.buildICmp(c.LLVMIntEQ, val, llvm.constInt32(0), "iszero"),
                .f64 => builder.buildFCmp(c.LLVMRealOEQ, val, llvm.constF64(0.0), "fiszero"),
            };
            const cmpz = builder.buildZExt(is_zero, llvm.i32Type(), "lnot");
            const result = switch (kind) {
                .i32 => cmpz,
                .f64 => builder.buildSIToFP(cmpz, llvm.doubleType(), "lnotf"),
            };
            push(builder, va, sp, result, elem_type);
        },

        .inc_dec => {
            const val = pop(builder, va, sp, elem_type);
            const result = if (handler.is_inc orelse false) switch (kind) {
                .i32 => builder.buildAdd(val, one_val, "inc"),
                .f64 => builder.buildFAdd(val, one_val, "finc"),
            } else switch (kind) {
                .i32 => builder.buildSub(val, one_val, "dec"),
                .f64 => builder.buildFSub(val, one_val, "fdec"),
            };
            push(builder, va, sp, result, elem_type);
        },

        .post_inc_dec => {
            const val = pop(builder, va, sp, elem_type);
            const new_val = if (handler.is_inc orelse false) switch (kind) {
                .i32 => builder.buildAdd(val, one_val, "postinc"),
                .f64 => builder.buildFAdd(val, one_val, "fpostinc"),
            } else switch (kind) {
                .i32 => builder.buildSub(val, one_val, "postdec"),
                .f64 => builder.buildFSub(val, one_val, "fpostdec"),
            };
            // Push old first (deeper), new on TOS — matches QuickJS semantics
            push(builder, va, sp, val, elem_type);
            push(builder, va, sp, new_val, elem_type);
        },

        .add_loc => {
            const idx: u32 = switch (instr.operand) {
                .loc => |a| a, .u8 => |a| a, .u16 => |a| a, else => 0,
            };
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            const val = pop(builder, va, sp, elem_type);
            const old = builder.buildLoad(elem_type, locals.*[idx], "addloc_old");
            const new_val = switch (kind) {
                .i32 => builder.buildAdd(old, val, "addloc"),
                .f64 => builder.buildFAdd(old, val, "faddloc"),
            };
            _ = builder.buildStore(new_val, locals.*[idx]);
        },

        .inc_loc => {
            const idx: u32 = switch (instr.operand) {
                .loc => |a| a, .u8 => |a| a, .u16 => |a| a, else => 0,
            };
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            const old = builder.buildLoad(elem_type, locals.*[idx], "incloc_old");
            const new_val = switch (kind) {
                .i32 => builder.buildNSWAdd(old, one_val, "incloc"),
                .f64 => builder.buildFAdd(old, one_val, "fincloc"),
            };
            _ = builder.buildStore(new_val, locals.*[idx]);
        },

        .stack_dup => {
            push(builder, va, sp, peek(builder, va, sp, elem_type), elem_type);
        },

        .stack_drop => {
            _ = pop(builder, va, sp, elem_type);
        },

        .stack_swap => {
            // Pop top two, push in reversed order
            const a = pop(builder, va, sp, elem_type);
            const b = pop(builder, va, sp, elem_type);
            push(builder, va, sp, a, elem_type);
            push(builder, va, sp, b, elem_type);
        },

        .nop => {},

        .self_ref => {
            if (instr.opcode == .get_var or instr.opcode == .get_var_undef) {
                switch (instr.operand) {
                    .atom => |atom_idx| {
                        if (getAtomStringStatic(analyzed, atom_idx)) |callee_name| {
                            if (module.getNamedFunction(callee_name)) |callee_fn| {
                                if (callee_fn != func) pending_callee.* = callee_fn;
                            }
                        }
                    },
                    else => {},
                }
            } else if (instr.opcode == .get_var_ref0) {
                if (analyzed.closure_vars.len > 0) {
                    const cv_name = analyzed.closure_vars[0].name;
                    if (cv_name.len > 0) {
                        var name_buf: [256]u8 = undefined;
                        if (cv_name.len < name_buf.len) {
                            @memcpy(name_buf[0..cv_name.len], cv_name);
                            name_buf[cv_name.len] = 0;
                            const name_z: [*:0]const u8 = @ptrCast(name_buf[0..cv_name.len]);
                            if (module.getNamedFunction(name_z)) |callee_fn| {
                                if (callee_fn != func) pending_callee.* = callee_fn;
                            }
                        }
                    }
                }
            }
        },

        .call_self => {
            const call_argc: u32 = switch (instr.opcode) {
                .call1 => 1, .call2 => 2, .call3 => 3,
                .call => instr.operand.u16, else => 1,
            };
            const call_target = if (pending_callee.*) |callee| blk: {
                pending_callee.* = null;
                break :blk if (c.LLVMCountParams(callee) == call_argc) callee else func;
            } else func;
            var call_args: [8]llvm.Value = undefined;
            var i: u32 = call_argc;
            while (i > 0) {
                i -= 1;
                call_args[i] = pop(builder, va, sp, elem_type);
            }
            var param_types_buf: [8]llvm.Type = undefined;
            for (0..call_argc) |j| param_types_buf[j] = elem_type;
            const call_fn_ty = llvm.functionType(elem_type, param_types_buf[0..call_argc], false);
            push(builder, va, sp, builder.buildCall(call_fn_ty, call_target, call_args[0..call_argc], "call"), elem_type);
        },

        .tail_call_self => {
            const call_argc: u32 = instr.operand.u16;
            const call_target = if (pending_callee.*) |callee| blk: {
                pending_callee.* = null;
                break :blk if (c.LLVMCountParams(callee) == call_argc) callee else func;
            } else func;
            var call_args: [8]llvm.Value = undefined;
            var i: u32 = call_argc;
            while (i > 0) {
                i -= 1;
                call_args[i] = pop(builder, va, sp, elem_type);
            }
            var param_types_buf: [8]llvm.Type = undefined;
            for (0..call_argc) |j| param_types_buf[j] = elem_type;
            const call_fn_ty = llvm.functionType(elem_type, param_types_buf[0..call_argc], false);
            _ = builder.buildRet(builder.buildCall(call_fn_ty, call_target, call_args[0..call_argc], "tail"));
            block_terminated.* = true;
        },

        .ret => {
            if (handler.value) |v| {
                _ = builder.buildRet(switch (kind) {
                    .i32 => llvm.constInt32(v),
                    .f64 => llvm.constF64(@floatFromInt(v)),
                });
            } else {
                _ = builder.buildRet(pop(builder, va, sp, elem_type));
            }
            block_terminated.* = true;
        },

        .if_false => {
            const cond = pop(builder, va, sp, elem_type);
            const is_false = switch (kind) {
                .i32 => builder.buildICmp(c.LLVMIntEQ, cond, llvm.constInt32(0), "ifz"),
                .f64 => builder.buildFCmp(c.LLVMRealOEQ, cond, llvm.constF64(0.0), "fifz"),
            };
            if (successors.len >= 2) {
                if (successors[0] < block_count and successors[1] < block_count) {
                    _ = builder.buildCondBr(is_false, llvm_blocks[successors[0]], llvm_blocks[successors[1]]);
                    block_terminated.* = true;
                }
            } else if (successors.len == 1 and successors[0] < block_count) {
                _ = builder.buildBr(llvm_blocks[successors[0]]);
                block_terminated.* = true;
            }
        },

        .if_true => {
            const cond = pop(builder, va, sp, elem_type);
            const is_true = switch (kind) {
                .i32 => builder.buildICmp(c.LLVMIntNE, cond, llvm.constInt32(0), "ift"),
                .f64 => builder.buildFCmp(c.LLVMRealONE, cond, llvm.constF64(0.0), "fift"),
            };
            if (successors.len >= 2) {
                if (successors[0] < block_count and successors[1] < block_count) {
                    _ = builder.buildCondBr(is_true, llvm_blocks[successors[0]], llvm_blocks[successors[1]]);
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
            _ = pop(builder, va, sp, elem_type);
            push(builder, va, sp, zero_val, elem_type);
        },

        .array_get => {
            const idx_val = pop(builder, va, sp, elem_type);
            const base_val = pop(builder, va, sp, elem_type);
            const base_ptr = c.LLVMBuildIntToPtr(builder.ref, base_val, llvm.ptrType(), "arr_base");
            var gep_idx = [_]llvm.Value{idx_val};
            const elem_ptr = c.LLVMBuildGEP2(builder.ref, llvm.i32Type(), base_ptr, &gep_idx, 1, "arr_gep");
            const loaded = builder.buildLoad(llvm.i32Type(), elem_ptr, "arr_elem");
            const result = switch (kind) {
                .i32 => loaded,
                .f64 => builder.buildSIToFP(loaded, llvm.doubleType(), "arr_f64"),
            };
            push(builder, va, sp, result, elem_type);
        },

        .array_get2 => {
            const idx_val = pop(builder, va, sp, elem_type);
            const base_val = peek(builder, va, sp, elem_type); // keep base on stack
            const base_ptr = c.LLVMBuildIntToPtr(builder.ref, base_val, llvm.ptrType(), "arr_base2");
            var gep_idx = [_]llvm.Value{idx_val};
            const elem_ptr = c.LLVMBuildGEP2(builder.ref, llvm.i32Type(), base_ptr, &gep_idx, 1, "arr_gep2");
            const loaded = builder.buildLoad(llvm.i32Type(), elem_ptr, "arr_elem2");
            const result = switch (kind) {
                .i32 => loaded,
                .f64 => builder.buildSIToFP(loaded, llvm.doubleType(), "arr_f64_2"),
            };
            push(builder, va, sp, result, elem_type);
        },

        .array_put => {
            const val = pop(builder, va, sp, elem_type);
            const idx_val = pop(builder, va, sp, elem_type);
            const base_val = pop(builder, va, sp, elem_type);
            const base_ptr = c.LLVMBuildIntToPtr(builder.ref, base_val, llvm.ptrType(), "arr_base_w");
            var gep_idx = [_]llvm.Value{idx_val};
            const elem_ptr = c.LLVMBuildGEP2(builder.ref, llvm.i32Type(), base_ptr, &gep_idx, 1, "arr_gep_w");
            const store_val = switch (kind) {
                .i32 => val,
                .f64 => builder.buildFPToSI(val, llvm.i32Type(), "arr_i32_w"),
            };
            _ = builder.buildStore(store_val, elem_ptr);
        },

        .array_length => {
            // Fallback for unmatched get_length (peephole in generateNumericBody handles most cases)
            return CodegenError.UnsupportedOpcode;
        },

        .unsupported => return CodegenError.UnsupportedOpcode,
    }
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
    locals: *[48]llvm.Value,
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
                    .i32 => llvm.constInt32(@as(i32, @intFromFloat(v))),
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
                else => 0,
            };
            if (idx >= arg_count) return CodegenError.UnsupportedOpcode;
            const val = builder.buildLoad(elem_type, params[idx], "arg");
            vstack.append(allocator, val) catch return CodegenError.OutOfMemory;
        },

        .put_arg => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .arg => |a| a,
                .u8 => |a| a,
                .u16 => |a| a,
                else => 0,
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
                .u16 => |a| a,
                else => 0,
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
                .u16 => |a| a,
                else => 0,
            };
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            const val = builder.buildLoad(elem_type, locals.*[idx], "loc");
            vstack.append(allocator, val) catch return CodegenError.OutOfMemory;
        },

        .get_loc_pair => {
            // Fused: push loc0, then push loc1
            if (0 >= @min(local_count, 48) or 1 >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            const val0 = builder.buildLoad(elem_type, locals.*[0], "loc0");
            const val1 = builder.buildLoad(elem_type, locals.*[1], "loc1");
            vstack.append(allocator, val0) catch return CodegenError.OutOfMemory;
            vstack.append(allocator, val1) catch return CodegenError.OutOfMemory;
        },

        .put_loc, .put_loc_check => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                .u16 => |a| a,
                else => 0,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            const val = numVstackPop(vstack);
            _ = builder.buildStore(val, locals.*[idx]);
        },

        .set_loc => {
            const idx: u32 = handler.index orelse switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                .u16 => |a| a,
                else => 0,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            const val = vstack.items[vstack.items.len - 1]; // peek
            _ = builder.buildStore(val, locals.*[idx]);
        },

        .set_loc_uninitialized => {
            const idx: u32 = switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                .u16 => |a| a,
                else => 0,
            };
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            _ = builder.buildStore(zero_val, locals.*[idx]);
        },

        .binary_arith => {
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const rhs = numVstackPop(vstack);
            const lhs = numVstackPop(vstack);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;

            const result = switch (kind) {
                .i32 => blk: {
                    break :blk if (std.mem.eql(u8, op, "+"))
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
                },
                .f64 => blk: {
                    break :blk if (std.mem.eql(u8, op, "+"))
                        builder.buildFAdd(lhs, rhs, "fadd")
                    else if (std.mem.eql(u8, op, "-"))
                        builder.buildFSub(lhs, rhs, "fsub")
                    else if (std.mem.eql(u8, op, "*"))
                        builder.buildFMul(lhs, rhs, "fmul")
                    else if (std.mem.eql(u8, op, "/"))
                        builder.buildFDiv(lhs, rhs, "fdiv")
                    else if (std.mem.eql(u8, op, "%"))
                        builder.buildFRem(lhs, rhs, "frem")
                    else
                        return CodegenError.UnsupportedOpcode;
                },
            };
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .bitwise_binary => {
            // Bitwise ops only valid for i32 tier — f64 tier rejects at analysis time
            if (kind != .i32) return CodegenError.UnsupportedOpcode;
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const rhs = numVstackPop(vstack);
            const lhs = numVstackPop(vstack);
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
            else if (std.mem.eql(u8, op, ">>>"))
                builder.buildLShr(lhs, rhs, "shru")
            else
                return CodegenError.UnsupportedOpcode;

            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .binary_cmp => {
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const rhs = numVstackPop(vstack);
            const lhs = numVstackPop(vstack);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;

            const cmp = switch (kind) {
                .i32 => blk: {
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
                    break :blk builder.buildICmp(pred, lhs, rhs, "cmp");
                },
                .f64 => blk: {
                    // Use ordered comparisons (OLT etc.) — NaN propagation
                    const pred: c.LLVMRealPredicate = if (std.mem.eql(u8, op, "<"))
                        c.LLVMRealOLT
                    else if (std.mem.eql(u8, op, "<="))
                        c.LLVMRealOLE
                    else if (std.mem.eql(u8, op, ">"))
                        c.LLVMRealOGT
                    else if (std.mem.eql(u8, op, ">="))
                        c.LLVMRealOGE
                    else if (std.mem.eql(u8, op, "=="))
                        c.LLVMRealOEQ
                    else if (std.mem.eql(u8, op, "!="))
                        c.LLVMRealONE
                    else
                        return CodegenError.UnsupportedOpcode;
                    break :blk builder.buildFCmp(pred, lhs, rhs, "fcmp");
                },
            };

            // Comparisons always produce i32 result (0 or 1), even in f64 tier
            // For f64 tier: zero-extend i1 → i32, then sitofp → f64
            const cmpz = builder.buildZExt(cmp, llvm.i32Type(), "cmpz");
            const result = switch (kind) {
                .i32 => cmpz,
                .f64 => builder.buildSIToFP(cmpz, llvm.doubleType(), "cmpf"),
            };
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .unary => {
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const val = numVstackPop(vstack);
            const op = handler.op orelse return CodegenError.UnsupportedOpcode;

            const result = if (std.mem.eql(u8, op, "-")) switch (kind) {
                .i32 => builder.buildNeg(val, "neg"),
                .f64 => builder.buildFNeg(val, "fneg"),
            } else if (std.mem.eql(u8, op, "~")) blk: {
                if (kind != .i32) return CodegenError.UnsupportedOpcode;
                break :blk builder.buildNot(val, "bnot");
            } else return CodegenError.UnsupportedOpcode;

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
                .u16 => |a| a,
                else => 0,
            };
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            const val = numVstackPop(vstack);
            const loc_ptr = locals.*[idx];
            const old_val = builder.buildLoad(elem_type, loc_ptr, "addloc_old");
            const new_val = switch (kind) {
                .i32 => builder.buildAdd(old_val, val, "addloc"),
                .f64 => builder.buildFAdd(old_val, val, "faddloc"),
            };
            _ = builder.buildStore(new_val, loc_ptr);
        },

        .inc_loc => {
            const idx: u32 = switch (instr.operand) {
                .loc => |a| a,
                .u8 => |a| a,
                .u16 => |a| a,
                else => 0,
            };
            if (idx >= @min(local_count, 48)) return CodegenError.UnsupportedOpcode;
            const loc_ptr = locals.*[idx];
            const old_val = builder.buildLoad(elem_type, loc_ptr, "incloc_old");
            const new_val = switch (kind) {
                .i32 => builder.buildNSWAdd(old_val, one_val, "incloc"),
                .f64 => builder.buildFAdd(old_val, one_val, "fincloc"),
            };
            _ = builder.buildStore(new_val, loc_ptr);
        },

        .stack_dup => {
            if (vstack.items.len < 1) return CodegenError.StackUnderflow;
            const top = vstack.items[vstack.items.len - 1];
            vstack.append(allocator, top) catch return CodegenError.OutOfMemory;
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
            } else if (instr.opcode == .get_var_ref0) {
                if (analyzed.closure_vars.len > 0) {
                    const cv_name = analyzed.closure_vars[0].name;
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

            var param_types_buf: [8]llvm.Type = undefined;
            for (0..call_argc) |j| {
                param_types_buf[j] = elem_type;
            }
            const call_fn_ty = llvm.functionType(elem_type, param_types_buf[0..call_argc], false);
            const result = builder.buildCall(call_fn_ty, call_target, call_args[0..call_argc], "call");
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

            var param_types_buf: [8]llvm.Type = undefined;
            for (0..call_argc) |j| {
                param_types_buf[j] = elem_type;
            }
            const call_fn_ty = llvm.functionType(elem_type, param_types_buf[0..call_argc], false);
            const result = builder.buildCall(call_fn_ty, call_target, call_args[0..call_argc], "tail");
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
            const is_false = switch (kind) {
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

            const is_true = switch (kind) {
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
            const idx_val = numVstackPop(vstack);
            const base_val = numVstackPop(vstack);
            // base_val is a byte offset into WASM linear memory (i32)
            // Convert to pointer, GEP by index (element size = sizeof(i32) = 4 bytes)
            const base_ptr = c.LLVMBuildIntToPtr(builder.ref, base_val, llvm.ptrType(), "arr_base");
            var gep_indices = [_]llvm.Value{idx_val};
            const elem_ptr = c.LLVMBuildGEP2(builder.ref, llvm.i32Type(), base_ptr, &gep_indices, 1, "arr_gep");
            const loaded = builder.buildLoad(llvm.i32Type(), elem_ptr, "arr_elem");
            // For f64 tier, convert loaded i32 to f64
            const result = switch (kind) {
                .i32 => loaded,
                .f64 => builder.buildSIToFP(loaded, llvm.doubleType(), "arr_f64"),
            };
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .array_get2 => {
            // arr[idx] keeping arr on stack: pop index, peek base, load, push result
            if (vstack.items.len < 2) return CodegenError.StackUnderflow;
            const idx_val = numVstackPop(vstack);
            // Peek at base (don't pop)
            const base_val = vstack.items[vstack.items.len - 1];
            const base_ptr = c.LLVMBuildIntToPtr(builder.ref, base_val, llvm.ptrType(), "arr_base2");
            var gep_indices = [_]llvm.Value{idx_val};
            const elem_ptr = c.LLVMBuildGEP2(builder.ref, llvm.i32Type(), base_ptr, &gep_indices, 1, "arr_gep2");
            const loaded = builder.buildLoad(llvm.i32Type(), elem_ptr, "arr_elem2");
            const result = switch (kind) {
                .i32 => loaded,
                .f64 => builder.buildSIToFP(loaded, llvm.doubleType(), "arr_f64_2"),
            };
            vstack.append(allocator, result) catch return CodegenError.OutOfMemory;
        },

        .array_put => {
            // arr[idx] = val: pop value, pop index, pop base, store to memory
            if (vstack.items.len < 3) return CodegenError.StackUnderflow;
            const val = numVstackPop(vstack);
            const idx_val = numVstackPop(vstack);
            const base_val = numVstackPop(vstack);
            const base_ptr = c.LLVMBuildIntToPtr(builder.ref, base_val, llvm.ptrType(), "arr_base_w");
            var gep_indices = [_]llvm.Value{idx_val};
            const elem_ptr = c.LLVMBuildGEP2(builder.ref, llvm.i32Type(), base_ptr, &gep_indices, 1, "arr_gep_w");
            // For f64 tier, convert f64 value to i32 for storage
            const store_val = switch (kind) {
                .i32 => val,
                .f64 => builder.buildFPToSI(val, llvm.i32Type(), "arr_i32_w"),
            };
            _ = builder.buildStore(store_val, elem_ptr);
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
    return generateThinShardWithTarget(allocator, functions, shard_idx, obj_path, native_target);
}

pub fn generateThinShardWasm(
    allocator: Allocator,
    functions: []const ThinShardFunction,
    shard_idx: usize,
    obj_path: [*:0]const u8,
) CodegenError!LLVMShardResult {
    return generateThinShardWithTarget(allocator, functions, shard_idx, obj_path, TargetInfo{ .is_wasm32 = true });
}

fn generateThinShardWithTarget(
    allocator: Allocator,
    functions: []const ThinShardFunction,
    shard_idx: usize,
    obj_path: [*:0]const u8,
    target: TargetInfo,
) CodegenError!LLVMShardResult {
    var name_buf: [64]u8 = undefined;
    const mod_name = std.fmt.bufPrintZ(&name_buf, "frozen_thin_shard_{d}", .{shard_idx}) catch
        return CodegenError.FormatError;

    // Create LLVM module for the target (native or wasm32)
    const native = if (target.is_wasm32)
        llvm.createWasmModule(mod_name, c.LLVMCodeGenLevelDefault) catch return CodegenError.LLVMError
    else
        llvm.createNativeModuleWithOpt(mod_name, c.LLVMCodeGenLevelDefault) catch return CodegenError.LLVMError;
    defer native.tm.dispose();
    var module_disposed = false;
    defer if (!module_disposed) native.module.dispose();

    const builder = llvm.Builder.create(native.ctx);
    defer builder.dispose();

    // Declare external runtime functions
    const register_fn = declareNativeDispatchRegisterByIndex(native.module);
    var rt_decls = ThinRuntimeDecls.declareWithTarget(native.module, target);

    // Generate each function
    var generated_count: u32 = 0;
    var generated_infos = std.ArrayListUnmanaged(GeneratedFuncInfo){};
    defer generated_infos.deinit(allocator);

    var skipped_unsafe: u32 = 0;
    var skipped_large: u32 = 0;
    var skipped_close_fc: u32 = 0;
    var skipped_opcode: u32 = 0;
    var unsafe_opcode_counts: [256]u32 = [_]u32{0} ** 256;
    for (functions) |sf| {
        // Skip very large functions — they can have codegen bugs in edge cases
        // and the LLVM compile time is disproportionate. The interpreter handles them correctly.
        if (sf.func.instructions.len > 10000) {
            skipped_unsafe += 1;
            skipped_large += 1;
            continue;
        }
        // close_loc+fclosure: previously skipped, now enabled with proper
        // locals_jsv sync in set_loc/put_loc when has_fclosure is true.
        {
            var has_close = false;
            var has_fc = false;
            for (sf.cfg.blocks.items) |block| {
                for (block.instructions) |bi| {
                    if (bi.opcode == .close_loc) has_close = true;
                    if (bi.opcode == .fclosure or bi.opcode == .fclosure8) has_fc = true;
                }
            }
            if (has_close and has_fc) {
                skipped_close_fc += 1; // Track count but don't skip
            }
        }
        if (generateThinFunction(allocator, native.module, builder, sf, &rt_decls, target)) |has_unsafe_fallback| {
            if (has_unsafe_fallback) {
                // Function uses opcodes that js_frozen_exec_opcode can't handle —
                // don't register it, let the interpreter handle this function.
                var fn_name_buf: [512]u8 = undefined;
                const fn_name_z = std.fmt.bufPrintZ(&fn_name_buf, "{s}", .{sf.llvm_func_name}) catch continue;
                if (native.module.getNamedFunction(fn_name_z)) |func_val| {
                    llvm.setLinkage(func_val, c.LLVMInternalLinkage);
                }
                for (sf.cfg.blocks.items) |block| {
                    for (block.instructions) |bi| {
                        switch (bi.opcode) {
                            .push_i32, .push_const, .push_const8,
                            .push_0, .push_1, .push_2, .push_3, .push_4, .push_5, .push_6, .push_7,
                            .push_i8, .push_i16, .push_minus1, .push_true, .push_false, .null, .undefined,
                            .push_this, .push_empty_string,
                            .get_loc0, .get_loc1, .get_loc2, .get_loc3, .get_loc, .get_loc8,
                            .put_loc0, .put_loc1, .put_loc2, .put_loc3, .put_loc, .put_loc8,
                            .set_loc0, .set_loc1, .set_loc2, .set_loc3, .set_loc, .set_loc8,
                            .get_loc0_loc1,
                            .get_arg0, .get_arg1, .get_arg2, .get_arg3, .get_arg,
                            .put_arg0, .put_arg1, .put_arg2, .put_arg3, .put_arg,
                            .set_arg0, .set_arg1, .set_arg2, .set_arg3, .set_arg,
                            .get_var_ref_check, .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3,
                            .get_var_ref, .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3,
                            .put_var_ref, .set_var_ref0, .set_var_ref1, .set_var_ref2, .set_var_ref3, .set_var_ref,
                            .dup, .dup1, .dup2, .dup3, .drop, .nip, .nip1, .swap, .insert2, .insert3, .insert4,
                            .perm3, .perm4, .perm5, .rot3l, .rot3r,
                            .add, .sub, .mul, .div, .mod, .neg, .plus, .pow,
                            .@"and", .@"or", .xor, .shl, .sar, .shr, .not,
                            .lt, .lte, .gt, .gte, .eq, .neq, .strict_eq, .strict_neq,
                            .lnot, .typeof, .is_undefined, .is_null, .is_undefined_or_null,
                            .inc, .dec, .post_inc, .post_dec, .inc_loc, .dec_loc,
                            .object, .append, .put_array_el, .get_array_el, .get_array_el2, .get_length, .add_loc,
                            .call0, .call1, .call2, .call3, .call, .call_method,
                            .call_constructor, .tail_call, .tail_call_method,
                            .get_field, .get_field2, .put_field,
                            .apply,
                            .get_loc_check, .put_loc_check, .put_loc_check_init,
                            .get_var, .get_var_undef, .push_atom_value, .define_field,
                            .typeof_is_function, .fclosure8, .fclosure,
                            .@"return", .return_undef,
                            .if_false, .if_false8, .if_true, .if_true8, .goto, .goto8, .goto16,
                            .throw, .nop, .set_name, .close_loc, .set_loc_uninitialized, .set_home_object,
                            .set_proto,
                            // exec_opcode-handled opcodes (safe to delegate to C):
                            // .to_propkey, .to_propkey2 now inline-handled
                            .to_propkey, .to_propkey2, .to_object,
                            .instanceof, .in, .delete, .typeof_is_undefined,
                            .get_super, .define_array_el, .set_name_computed, .copy_data_properties,
                            .put_var_ref_check,
                            .rest, .regexp, .push_bigint_i32,
                            // check_ctor, init_ctor, special_object need func_obj/new_target
                            // which frozen functions don't receive — skip these
                            .define_method, .define_method_computed,
                            .put_ref_value,
                            .check_define_var, .define_var, .define_func,
                            // .get_var, .get_var_undef already inline-handled above
                            .put_var, .put_var_init, .put_var_strict, .check_var,
                            // .for_in_start, .for_in_next now inline-handled
                            .for_of_start, .for_of_next, .for_in_start, .for_in_next,
                            .iterator_close,
                            .array_from,
                            => {},
                            else => unsafe_opcode_counts[@intFromEnum(bi.opcode)] += 1,
                        }
                    }
                }
                skipped_unsafe += 1;
                skipped_opcode += 1;
                continue;
            }
            // Check if function has tail_call opcodes
            var func_has_tail_call = false;
            for (sf.cfg.blocks.items) |block| {
                for (block.instructions) |bi| {
                    if (bi.opcode == .tail_call or bi.opcode == .tail_call_method) {
                        func_has_tail_call = true;
                        break;
                    }
                }
                if (func_has_tail_call) break;
            }
            generated_infos.append(allocator, .{
                .name = sf.name,
                .func_index = sf.func_index,
                .parser_index = sf.parser_index,
                .line_num = sf.line_num,
                .llvm_func_name = sf.llvm_func_name,
                .has_tail_call = func_has_tail_call,
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
    if (skipped_unsafe > 0) {
        std.debug.print("[llvm-thin] Skipped {d} functions (large:{d} opcode:{d}), {d} close_loc+fc compiled\n", .{ skipped_unsafe, skipped_large, skipped_opcode, skipped_close_fc });
        const OpcodeCount = struct { idx: u8, count: u32 };
        var top: [256]OpcodeCount = undefined;
        for (0..256) |i| top[i] = .{ .idx = @intCast(i), .count = unsafe_opcode_counts[i] };
        for (0..256) |i| {
            for (i + 1..256) |j| {
                if (top[j].count > top[i].count) {
                    const tmp = top[i];
                    top[i] = top[j];
                    top[j] = tmp;
                }
            }
        }
        var printed: u32 = 0;
        for (top[0..]) |entry| {
            if (entry.count == 0 or printed >= 15) break;
            const op: opcodes.Opcode = @enumFromInt(entry.idx);
            std.debug.print("  {s}: {d}\n", .{ @tagName(op), entry.count });
            printed += 1;
        }
    }
    if (generated_count == 0) {
        return .{ .func_count = 0, .has_functions = false };
    }

    generateShardInit(allocator, native.module, builder, shard_idx, generated_infos.items, register_fn) catch
        return CodegenError.LLVMError;

    // O2 enables LLVM LoopVectorize: auto-vectorizes Int32Array counted loops
    // to SIMD (e.g. scalar `acc += arr[i]` → <4 x i32> vector add).
    // Int32 shards use O1 (smaller code for recursive functions like fib).
    module_disposed = try finalizeAndEmitShard(native, obj_path, "llvm-thin", "default<O2>");
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
    push_const: llvm.Value,
    push_this: llvm.Value,
    push_empty_string: llvm.Value,

    // Local variable access
    get_loc: llvm.Value,
    put_loc: llvm.Value,
    set_loc: llvm.Value,

    // Argument access
    get_arg: llvm.Value,
    get_arg_shadow: llvm.Value,
    put_arg: llvm.Value,
    set_arg: llvm.Value,

    // Closure variable access
    get_var_ref: llvm.Value,
    get_var_ref_check: llvm.Value,
    put_var_ref: llvm.Value,
    set_var_ref: llvm.Value,

    // Stack manipulation (advanced)
    insert2: llvm.Value,
    insert3: llvm.Value,
    insert4: llvm.Value,
    perm3: llvm.Value,
    perm4: llvm.Value,
    perm5: llvm.Value,
    rot3l: llvm.Value,
    rot3r: llvm.Value,
    nip1: llvm.Value,

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

    // Increment (local)
    inc_loc: llvm.Value,
    dec_loc: llvm.Value,

    // Increment (stack)
    inc: llvm.Value,
    dec: llvm.Value,
    post_inc: llvm.Value,
    post_dec: llvm.Value,

    // Type checks
    is_undefined: llvm.Value,
    is_null: llvm.Value,
    is_undefined_or_null: llvm.Value,

    // Object creation
    object: llvm.Value,

    // Complex ops (return c_int for error)
    op_call: llvm.Value,
    op_call_method: llvm.Value,
    op_call_constructor: llvm.Value,
    op_get_field_ic: llvm.Value,
    op_get_field2_ic: llvm.Value,
    op_put_field: llvm.Value,
    op_get_var: llvm.Value,
    op_get_var_undef: llvm.Value,
    op_apply: llvm.Value,

    // TDZ helpers
    throw_tdz: llvm.Value,

    // Atom/string push
    push_atom_value: llvm.Value,

    // Define field
    define_field: llvm.Value,

    // typeof_is_function
    typeof_is_function: llvm.Value,

    // Closure creation
    fclosure: llvm.Value,
    fclosure_v2: llvm.Value,

    // Closure sync
    sync_locals_to: llvm.Value,
    sync_locals_from: llvm.Value,
    var_ref_list_detach: llvm.Value,

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

    // close_loc: detach var_refs for a specific local, then set to undefined
    close_loc: llvm.Value,

    // Iterator ops
    op_for_of_start: llvm.Value,
    op_for_of_next: llvm.Value,
    op_iterator_close: llvm.Value,
    op_for_in_start: llvm.Value,
    op_for_in_next: llvm.Value,
    op_to_propkey: llvm.Value,
    op_to_propkey2: llvm.Value,

    // Array ops
    op_array_from: llvm.Value,
    op_append: llvm.Value,
    op_put_array_el: llvm.Value,

    // Fast array access helpers
    fast_array_probe: llvm.Value, // i32(i64, ptr, ptr) — probe arr → {values_ptr, count}
    fast_array_get_el: llvm.Value, // i64(ptr, i64, i64) — combined (legacy)
    fast_array_get_len: llvm.Value, // i64(i64) — combined is_fast+count
    jsvalue_to_cv: llvm.Value, // i64(ptr, ptr, i32) — load JSValue[idx] → CV
    // Slow-path fallbacks (stack-based)
    get_array_el: llvm.Value, // i32(ptr, ptr, ptr, ptr, i32)
    get_array_el2: llvm.Value, // i32(ptr, ptr, ptr, ptr, i32)
    get_length: llvm.Value, // i32(ptr, ptr, ptr, ptr, i32)
    add_loc: llvm.Value, // i32(ptr, ptr, ptr, ptr, i32)

    // Boolean conversion (handles strings, NaN, 0.0 — full JS semantics)
    cv_to_bool: llvm.Value,

    // Specialized method inlines (stack-based, with slow-path fallback)
    call_method_char_code_at: llvm.Value,
    call_method_array_push: llvm.Value,
    call_method_math_unary: llvm.Value,

    // Combined tail_call+return
    tail_call_return: llvm.Value,
    tail_call_method_return: llvm.Value,

    // Opcode fallback
    exec_opcode: llvm.Value,

    fn declare(module: llvm.Module) ThinRuntimeDecls {
        return declareWithTarget(module, native_target);
    }

    fn declareWithTarget(module: llvm.Module, target: TargetInfo) ThinRuntimeDecls {
        const ptr = llvm.ptrType();
        const i32t = llvm.i32Type();
        const voidt = llvm.voidType();
        const jsv_ty = target.jsvalueType(); // {i64, i64} native, i64 wasm32
        const cv_ty = target.cvType(); // {i64, i64} native (= JSValue), i64 wasm32
        const usize_t = target.usizeType(); // i64 native, i32 wasm32

        // Common signatures
        // void(stack, sp, i32)
        const stack_sp_i32 = llvm.functionType(voidt, &.{ ptr, ptr, i32t }, false);
        // void(stack, sp)
        const stack_sp = llvm.functionType(voidt, &.{ ptr, ptr }, false);
        // void(ctx, stack, sp)
        const ctx_stack_sp = llvm.functionType(voidt, &.{ ptr, ptr, ptr }, false);
        // void(ctx, stack, sp, locals, u32)
        const ctx_stack_sp_loc_u32 = llvm.functionType(voidt, &.{ ptr, ptr, ptr, ptr, i32t }, false);
        // void(stack, sp, cv) - cv is CV type (i64 on WASM32, {i64,i64} on native)
        const stack_sp_cv = llvm.functionType(voidt, &.{ ptr, ptr, cv_ty }, false);
        // void(locals, u32)
        const locals_u32 = llvm.functionType(voidt, &.{ ptr, i32t }, false);
        // i32(ctx, stack, sp, u16) - call ops returning error code
        const call_op = llvm.functionType(i32t, &.{ ptr, ptr, ptr, i32t }, false);  // u16 passed as i32
        // i32(ctx, stack, sp, ptr, ptr) - field ops returning error code
        const field_ic_op = llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr, ptr }, false);
        // i32(ctx, stack, sp, ptr) - simpler field/var op
        const field_op = llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr }, false);
        // JSValue(ctx, stack, sp, locals, usize, ptr, ptr, ptr, usize) - return helper
        const return_helper = llvm.functionType(jsv_ty, &.{ ptr, ptr, ptr, ptr, usize_t, ptr, ptr, ptr, usize_t }, false);
        // JSValue(ctx, locals, usize, ptr, ptr, ptr, usize) - returnUndef/exceptionCleanup
        const cleanup_helper = llvm.functionType(jsv_ty, &.{ ptr, ptr, usize_t, ptr, ptr, ptr, usize_t }, false);

        return .{
            .push_i32 = declareExtern(module, "llvm_rt_push_i32", stack_sp_i32),
            .push_true = declareExtern(module, "llvm_rt_push_true", stack_sp),
            .push_false = declareExtern(module, "llvm_rt_push_false", stack_sp),
            .push_null = declareExtern(module, "llvm_rt_push_null", stack_sp),
            .push_undefined = declareExtern(module, "llvm_rt_push_undefined", stack_sp),
            .push_cv = declareExtern(module, "llvm_rt_push_cv", stack_sp_cv),
            .push_const = declareExtern(module, "llvm_rt_push_const", llvm.functionType(voidt, &.{ ptr, ptr, ptr, ptr, i32t }, false)),
            .push_this = declareExtern(module, "llvm_rt_push_this", llvm.functionType(voidt, &.{ ptr, ptr, ptr, jsv_ty }, false)),
            .push_empty_string = declareExtern(module, "llvm_rt_push_empty_string", ctx_stack_sp),

            .insert2 = declareExtern(module, "llvm_rt_insert2", ctx_stack_sp),
            .insert3 = declareExtern(module, "llvm_rt_insert3", ctx_stack_sp),
            .insert4 = declareExtern(module, "llvm_rt_insert4", ctx_stack_sp),
            .perm3 = declareExtern(module, "llvm_rt_perm3", stack_sp),
            .perm4 = declareExtern(module, "llvm_rt_perm4", stack_sp),
            .perm5 = declareExtern(module, "llvm_rt_perm5", stack_sp),
            .rot3l = declareExtern(module, "llvm_rt_rot3l", stack_sp),
            .rot3r = declareExtern(module, "llvm_rt_rot3r", stack_sp),
            .nip1 = declareExtern(module, "llvm_rt_nip1", ctx_stack_sp),

            .get_loc = declareExtern(module, "llvm_rt_get_loc", ctx_stack_sp_loc_u32),
            .put_loc = declareExtern(module, "llvm_rt_put_loc", ctx_stack_sp_loc_u32),
            .set_loc = declareExtern(module, "llvm_rt_set_loc", ctx_stack_sp_loc_u32),

            .get_arg = declareExtern(module, "llvm_rt_get_arg", llvm.functionType(voidt, &.{ ptr, ptr, ptr, i32t, ptr, i32t }, false)),
            .get_arg_shadow = declareExtern(module, "llvm_rt_get_arg_shadow", llvm.functionType(voidt, &.{ ptr, ptr, ptr, i32t }, false)),
            .put_arg = declareExtern(module, "llvm_rt_put_arg", ctx_stack_sp_loc_u32),
            .set_arg = declareExtern(module, "llvm_rt_set_arg", ctx_stack_sp_loc_u32),

            .get_var_ref = declareExtern(module, "llvm_rt_get_var_ref", llvm.functionType(voidt, &.{ ptr, ptr, ptr, ptr, i32t, i32t }, false)),
            .get_var_ref_check = declareExtern(module, "llvm_rt_get_var_ref_check", llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr, i32t, i32t }, false)),
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
            .op_plus = declareExtern(module, "llvm_rt_op_plus", ctx_stack_sp),

            .op_band = declareExtern(module, "llvm_rt_op_band", stack_sp),
            .op_bor = declareExtern(module, "llvm_rt_op_bor", stack_sp),
            .op_bxor = declareExtern(module, "llvm_rt_op_bxor", stack_sp),
            .op_bnot = declareExtern(module, "llvm_rt_op_bnot", stack_sp),
            .op_shl = declareExtern(module, "llvm_rt_op_shl", stack_sp),
            .op_sar = declareExtern(module, "llvm_rt_op_sar", stack_sp),
            .op_shr = declareExtern(module, "llvm_rt_op_shr", stack_sp),

            .op_lt = declareExtern(module, "llvm_rt_op_lt", ctx_stack_sp),
            .op_lte = declareExtern(module, "llvm_rt_op_lte", ctx_stack_sp),
            .op_gt = declareExtern(module, "llvm_rt_op_gt", ctx_stack_sp),
            .op_gte = declareExtern(module, "llvm_rt_op_gte", ctx_stack_sp),
            .op_eq = declareExtern(module, "llvm_rt_op_eq", ctx_stack_sp),
            .op_neq = declareExtern(module, "llvm_rt_op_neq", ctx_stack_sp),
            .op_strict_eq = declareExtern(module, "llvm_rt_op_strict_eq", ctx_stack_sp),
            .op_strict_neq = declareExtern(module, "llvm_rt_op_strict_neq", ctx_stack_sp),

            .op_lnot = declareExtern(module, "llvm_rt_op_lnot", ctx_stack_sp),
            .op_typeof = declareExtern(module, "llvm_rt_op_typeof", ctx_stack_sp),

            .inc_loc = declareExtern(module, "llvm_rt_inc_loc", locals_u32),
            .dec_loc = declareExtern(module, "llvm_rt_dec_loc", locals_u32),

            .inc = declareExtern(module, "llvm_rt_inc", stack_sp),
            .dec = declareExtern(module, "llvm_rt_dec", stack_sp),
            .post_inc = declareExtern(module, "llvm_rt_post_inc", stack_sp),
            .post_dec = declareExtern(module, "llvm_rt_post_dec", stack_sp),

            .is_undefined = declareExtern(module, "llvm_rt_is_undefined", ctx_stack_sp),
            .is_null = declareExtern(module, "llvm_rt_is_null", ctx_stack_sp),
            .is_undefined_or_null = declareExtern(module, "llvm_rt_is_undefined_or_null", ctx_stack_sp),

            .object = declareExtern(module, "llvm_rt_object", ctx_stack_sp),

            .op_call = declareExtern(module, "llvm_rt_op_call", call_op),
            .op_call_method = declareExtern(module, "llvm_rt_op_call_method", call_op),
            .op_call_constructor = declareExtern(module, "llvm_rt_op_call_constructor", call_op),
            .op_get_field_ic = declareExtern(module, "llvm_rt_op_get_field_ic", field_ic_op),
            .op_get_field2_ic = declareExtern(module, "llvm_rt_op_get_field2_ic", field_ic_op),
            .op_put_field = declareExtern(module, "llvm_rt_op_put_field", field_op),
            .op_get_var = declareExtern(module, "llvm_rt_op_get_var", field_op),
            .op_get_var_undef = declareExtern(module, "llvm_rt_op_get_var_undef", field_op),
            .op_apply = declareExtern(module, "llvm_rt_op_apply", llvm.functionType(i32t, &.{ ptr, ptr, ptr }, false)),

            // TDZ: i32(ctx) — throws ReferenceError, always returns 1
            .throw_tdz = declareExtern(module, "llvm_rt_throw_tdz", llvm.functionType(i32t, &.{ptr}, false)),

            // push_atom_value: void(ctx, stack, sp, name)
            .push_atom_value = declareExtern(module, "llvm_rt_push_atom_value", llvm.functionType(voidt, &.{ ptr, ptr, ptr, ptr }, false)),

            // define_field: i32(ctx, stack, sp, name) — returns error code
            .define_field = declareExtern(module, "llvm_rt_define_field", field_op),

            // typeof_is_function: void(ctx, stack, sp)
            .typeof_is_function = declareExtern(module, "llvm_rt_typeof_is_function", ctx_stack_sp),

            // fclosure: i32(ctx, stack, sp, cpool, func_idx, var_refs, locals, local_count, argv, argc)
            .fclosure = declareExtern(module, "llvm_rt_fclosure", llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr, i32t, ptr, ptr, i32t, ptr, i32t }, false)),

            // fclosure_v2: i32(ctx, stack, sp, cpool, func_idx, var_refs, locals, local_count, argv, argc, locals_jsv, var_ref_list, arg_shadow, arg_count)
            .fclosure_v2 = declareExtern(module, "llvm_rt_fclosure_v2", llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr, i32t, ptr, ptr, i32t, ptr, i32t, ptr, ptr, ptr, i32t }, false)),

            // sync_locals_to/from: void(locals, locals_jsv, local_count)
            .sync_locals_to = declareExtern(module, "llvm_rt_sync_locals_to", llvm.functionType(voidt, &.{ ptr, ptr, i32t }, false)),
            .sync_locals_from = declareExtern(module, "llvm_rt_sync_locals_from", llvm.functionType(voidt, &.{ ptr, ptr, i32t }, false)),

            // var_ref_list_detach: void(ctx, var_ref_list)
            .var_ref_list_detach = declareExtern(module, "llvm_rt_var_ref_list_detach", llvm.functionType(voidt, &.{ ptr, ptr }, false)),

            .returnFromStack = declareExtern(module, "llvm_rt_returnFromStack", return_helper),
            .returnUndef = declareExtern(module, "llvm_rt_returnUndef", cleanup_helper),
            .exceptionCleanup = declareExtern(module, "llvm_rt_exceptionCleanup", cleanup_helper),

            .cv_from_jsvalue = declareExtern(module, "llvm_rt_cv_from_jsvalue", llvm.functionType(cv_ty, &.{jsv_ty}, false)),
            .cv_dup_ref = declareExtern(module, "llvm_rt_cv_dup_ref", llvm.functionType(cv_ty, &.{cv_ty}, false)),
            .cv_free_ref = declareExtern(module, "llvm_rt_cv_free_ref", llvm.functionType(voidt, &.{ ptr, cv_ty }, false)),

            .check_stack = declareExtern(module, "llvm_rt_check_stack", llvm.functionType(llvm.i1Type(), &.{}, false)),
            .exit_stack = declareExtern(module, "llvm_rt_exit_stack", llvm.functionType(voidt, &.{}, false)),

            // close_loc: void(ctx, locals_cv, locals_jsv, var_ref_list, idx)
            .close_loc = declareExtern(module, "llvm_rt_close_loc", llvm.functionType(voidt, &.{ ptr, ptr, ptr, ptr, i32t }, false)),

            .op_for_of_start = declareExtern(module, "llvm_rt_op_for_of_start", llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr, ptr }, false)),
            .op_for_of_next = declareExtern(module, "llvm_rt_op_for_of_next", llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr, ptr }, false)),
            .op_iterator_close = declareExtern(module, "llvm_rt_op_iterator_close", llvm.functionType(voidt, &.{ ptr, ptr, ptr, ptr, ptr }, false)),
            .op_for_in_start = declareExtern(module, "llvm_rt_op_for_in_start", llvm.functionType(i32t, &.{ ptr, ptr, ptr }, false)),
            .op_for_in_next = declareExtern(module, "llvm_rt_op_for_in_next", llvm.functionType(i32t, &.{ ptr, ptr, ptr }, false)),
            .op_to_propkey = declareExtern(module, "llvm_rt_op_to_propkey", llvm.functionType(i32t, &.{ ptr, ptr, ptr }, false)),
            .op_to_propkey2 = declareExtern(module, "llvm_rt_op_to_propkey2", llvm.functionType(i32t, &.{ ptr, ptr, ptr }, false)),

            .op_array_from = declareExtern(module, "llvm_rt_array_from", llvm.functionType(i32t, &.{ ptr, ptr, ptr, i32t }, false)),
            .op_append = declareExtern(module, "llvm_rt_op_append", ctx_stack_sp),
            .op_put_array_el = declareExtern(module, "llvm_rt_op_put_array_el", ctx_stack_sp),

            // Fast array access helpers
            .fast_array_probe = declareExtern(module, "llvm_rt_fast_array_probe", llvm.functionType(i32t, &.{ cv_ty, ptr, ptr }, false)),
            .fast_array_get_el = declareExtern(module, "llvm_rt_fast_array_get_el", llvm.functionType(cv_ty, &.{ ptr, cv_ty, cv_ty }, false)),
            .fast_array_get_len = declareExtern(module, "llvm_rt_fast_array_get_len", llvm.functionType(cv_ty, &.{cv_ty}, false)),
            .jsvalue_to_cv = declareExtern(module, "llvm_rt_jsvalue_to_cv", llvm.functionType(cv_ty, &.{ ptr, ptr, i32t }, false)),
            // Slow-path fallbacks (stack-based)
            .get_array_el = declareExtern(module, "llvm_rt_get_array_el", llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr, i32t }, false)),
            .get_array_el2 = declareExtern(module, "llvm_rt_get_array_el2", llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr, i32t }, false)),
            .get_length = declareExtern(module, "llvm_rt_get_length", llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr, i32t }, false)),
            .add_loc = declareExtern(module, "llvm_rt_add_loc", llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr, i32t }, false)),

            .cv_to_bool = declareExtern(module, "llvm_rt_cv_to_bool", llvm.functionType(i32t, &.{ ptr, ptr, ptr }, false)),

            // Specialized method inlines: (ctx, stack, sp) -> i32 error code
            .call_method_char_code_at = declareExtern(module, "llvm_rt_call_method_char_code_at", llvm.functionType(i32t, &.{ ptr, ptr, ptr }, false)),
            .call_method_array_push = declareExtern(module, "llvm_rt_call_method_array_push", llvm.functionType(i32t, &.{ ptr, ptr, ptr }, false)),
            .call_method_math_unary = declareExtern(module, "llvm_rt_call_method_math_unary", llvm.functionType(i32t, &.{ ptr, ptr, ptr, i32t }, false)),

            // exec_opcode: i32(ctx, op, operand, stack, sp, locals, var_count, var_refs, closure_var_count, argv, argc, arg_buf, func_obj, new_target, cpool, operand2, atom_name)
            .exec_opcode = declareExtern(module, "llvm_rt_exec_opcode", llvm.functionType(i32t, &.{ ptr, llvm.i8Type(), i32t, ptr, ptr, ptr, i32t, ptr, i32t, ptr, i32t, ptr, jsv_ty, jsv_ty, ptr, i32t, ptr }, false)),

            // Combined tail_call+return: (ctx, stack, sp, argc, locals, local_count, arg_shadow, arg_count) -> JSValue
            .tail_call_return = declareExtern(module, "llvm_rt_tail_call_return", llvm.functionType(jsv_ty, &.{ ptr, ptr, ptr, i32t, ptr, usize_t, ptr, usize_t }, false)),
            .tail_call_method_return = declareExtern(module, "llvm_rt_tail_call_method_return", llvm.functionType(jsv_ty, &.{ ptr, ptr, ptr, i32t, ptr, usize_t, ptr, usize_t }, false)),

        };
    }
};

fn declareExtern(module: llvm.Module, name: [*:0]const u8, fn_ty: llvm.Type) llvm.Value {
    const func = module.addFunction(name, fn_ty);
    llvm.setLinkage(func, c.LLVMExternalLinkage);
    return func;
}

/// Virtual stack entry — tracks LLVM SSA values instead of physical stack memory.
/// Values are only "flushed" to the physical stack when a runtime call needs them.
const VStackEntry = struct {
    value: llvm.Value, // LLVM SSA i64 value
    owned: bool, // true = we own the ref count; false = borrowed from locals/args
    local_idx: i32 = -1, // source local index for borrowed entries, -1 if not from a local
};

/// Pending method inline kind — tracks which method pattern was detected at get_field2
const PendingMethodKind = enum { none, char_code_at, array_push, math_abs, math_sqrt, math_floor, math_ceil, math_round };

/// Thin codegen context — holds LLVM references for the current function being generated
const ThinCodegenCtx = struct {
    builder: llvm.Builder,
    module: llvm.Module,
    rt: *ThinRuntimeDecls,
    allocator: Allocator,
    target: TargetInfo = .{},

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

    // for_of iter tracking (allocated when has_for_of)
    for_of_iter_stack_ptr: ?llvm.Value = null,
    for_of_depth_ptr: ?llvm.Value = null,

    // Arg shadow
    arg_shadow_ptr: ?llvm.Value = null,

    // Closure sync: allocated only for functions with fclosure/fclosure8 opcodes
    locals_jsv_ptr: ?llvm.Value = null,
    var_ref_list_ptr: ?llvm.Value = null,

    // Tracks whether any fallback opcode was emitted that js_frozen_exec_opcode can't handle.
    // If true, this function should not be registered as frozen (falls back to interpreter).
    has_unsafe_fallback: bool = false,

    // Pending method inline: set by get_field2, consumed by call_method
    pending_method: PendingMethodKind = .none,

    // 2-phase Math detection: set by get_var("Math"), consumed by get_field2
    pending_math_obj: bool = false,

    // Exception cleanup block
    cleanup_bb: llvm.BasicBlock,

    // Entry block for placing allocas
    entry_bb: llvm.BasicBlock,

    // Stack/locals sizes
    stack_array_size: u32,
    local_count: u32,

    // Virtual stack: SSA values tracked in registers instead of physical stack memory
    vstack: std.ArrayListUnmanaged(VStackEntry) = .{},

    // Fast array cache: per-local-variable cache for JSObject probe results.
    // Avoids re-probing the same array on every get_array_el/get_length in a loop.
    // Keyed by local index. Invalidated when local is written (put_loc/set_loc/add_loc).
    array_cache: std.AutoHashMapUnmanaged(u32, ArrayCacheEntry) = .{},

    fn nextIcSlot(self: *ThinCodegenCtx) u32 {
        const idx = self.ic_count;
        self.ic_count += 1;
        return idx;
    }

    /// Load SP value with correct type for target (i64 native, i32 WASM32)
    fn loadSp(self: *ThinCodegenCtx) llvm.Value {
        return self.builder.buildLoad(self.target.usizeType(), self.sp_ptr, "sp");
    }

    /// Store SP value
    fn storeSp(self: *ThinCodegenCtx, val: llvm.Value) void {
        _ = self.builder.buildStore(val, self.sp_ptr);
    }

    /// Create a constant matching SP type (i64 native, i32 WASM32)
    fn spConst(self: *ThinCodegenCtx, val: u64) llvm.Value {
        return self.target.constUsize(val);
    }

    /// SP add: sp + N with correct type
    fn spAdd(self: *ThinCodegenCtx, sp: llvm.Value, n: u64, name: [*:0]const u8) llvm.Value {
        return self.builder.buildAdd(sp, self.spConst(n), name);
    }

    /// SP sub: sp - N with correct type
    fn spSub(self: *ThinCodegenCtx, sp: llvm.Value, n: u64, name: [*:0]const u8) llvm.Value {
        return self.builder.buildSub(sp, self.spConst(n), name);
    }

    // =========================================================================
    // CV-type-aware helpers (native: {i64,i64}, WASM32: i64)
    // =========================================================================

    /// Get the LLVM type for a CV stack/local slot
    fn cvTy(self: *ThinCodegenCtx) llvm.Type {
        return self.target.cvType();
    }

    /// GEP into stack array with CV-sized slots
    fn stackSlot(self: *ThinCodegenCtx, index: llvm.Value) llvm.Value {
        return self.builder.buildInBoundsGEP(self.cvTy(), self.stack_ptr, &.{index}, "slot");
    }

    /// GEP into locals array with CV-sized slots
    fn localSlot(self: *ThinCodegenCtx, idx: u32) llvm.Value {
        return self.builder.buildInBoundsGEP(self.cvTy(), self.locals_ptr, &.{llvm.constInt(llvm.i64Type(), idx, false)}, "lslot");
    }

    /// Load a CV value from a pointer
    fn loadCv(self: *ThinCodegenCtx, ptr: llvm.Value, name: [*:0]const u8) llvm.Value {
        return self.builder.buildLoad(self.cvTy(), ptr, name);
    }

    /// Store a CV value to a pointer
    fn storeCv(self: *ThinCodegenCtx, val: llvm.Value, ptr: llvm.Value) void {
        _ = self.builder.buildStore(val, ptr);
    }

    /// Create a CV integer constant: on native = {zext(i32), tag=0}, on WASM32 = NaN-boxed
    fn makeIntCv(self: *ThinCodegenCtx, i32_val: llvm.Value) llvm.Value {
        if (self.target.is_wasm32) {
            const ext = self.builder.buildZExt(i32_val, llvm.i64Type(), "zext");
            return self.builder.buildOr(ext, llvm.constInt(llvm.i64Type(), CV_QNAN_INT, false), "newcv");
        }
        // Native: CV = JSValue = {u: i64, tag: i64}
        const b = self.builder;
        const cv_ty = self.cvTy();
        const ext = b.buildZExt(i32_val, llvm.i64Type(), "zext");
        var val = c.LLVMGetUndef(cv_ty);
        val = c.LLVMBuildInsertValue(b.ref, val, ext, 0, "cv_u");
        val = c.LLVMBuildInsertValue(b.ref, val, llvm.constInt64(0), 1, "cv_tag"); // JS_TAG_INT = 0
        return val;
    }

    /// Create a CV constant from u and tag fields (native only — WASM32 uses NaN-boxed bits)
    /// Create a CV constant from u and tag values.
    /// On WASM32: u_val is used as NaN-boxed bits (caller provides correct encoding).
    /// On native: builds {u, tag} struct.
    fn makeCvConst(self: *ThinCodegenCtx, u_val: i64, tag_val: i64) llvm.Value {
        if (self.target.is_wasm32) {
            return llvm.constInt(llvm.i64Type(), @bitCast(u_val), false);
        }
        const b = self.builder;
        const cv_ty = self.cvTy();
        var val = c.LLVMGetUndef(cv_ty);
        val = c.LLVMBuildInsertValue(b.ref, val, llvm.constInt64(u_val), 0, "cv_u");
        val = c.LLVMBuildInsertValue(b.ref, val, llvm.constInt64(tag_val), 1, "cv_tag");
        return val;
    }

    /// Check if a CV value is an integer
    fn isIntCv(self: *ThinCodegenCtx, val: llvm.Value) llvm.Value {
        if (self.target.is_wasm32) {
            const tag = self.builder.buildAnd(val, llvm.constInt(llvm.i64Type(), CV_TAG_MASK, false), "tag");
            return self.builder.buildICmp(c.LLVMIntEQ, tag, llvm.constInt(llvm.i64Type(), CV_QNAN_INT, false), "isint");
        }
        // Native: extract tag field (element 1), compare with 0 (JS_TAG_INT)
        const tag = c.LLVMBuildExtractValue(self.builder.ref, val, 1, "tag");
        return self.builder.buildICmp(c.LLVMIntEQ, tag, llvm.constInt64(0), "isint");
    }

    /// Extract i32 from a CV integer
    fn getIntCv(self: *ThinCodegenCtx, val: llvm.Value) llvm.Value {
        if (self.target.is_wasm32) {
            return self.builder.buildTrunc(val, llvm.i32Type(), "ival");
        }
        // Native: extract union field (element 0), trunc to i32
        const u_field = c.LLVMBuildExtractValue(self.builder.ref, val, 0, "ufield");
        return self.builder.buildTrunc(u_field, llvm.i32Type(), "ival");
    }

    /// Extract the raw union field (i64) from a CV value
    fn getCvUnion(self: *ThinCodegenCtx, val: llvm.Value) llvm.Value {
        if (self.target.is_wasm32) return val; // CV is i64 on WASM32
        return c.LLVMBuildExtractValue(self.builder.ref, val, 0, "cv_u");
    }

    /// Extract the tag field (i64) from a CV value
    fn getCvTag(self: *ThinCodegenCtx, val: llvm.Value) llvm.Value {
        if (self.target.is_wasm32) {
            return self.builder.buildAnd(val, llvm.constInt(llvm.i64Type(), CV_TAG_MASK, false), "tag");
        }
        return c.LLVMBuildExtractValue(self.builder.ref, val, 1, "cv_tag");
    }

    /// Push a CV constant onto the virtual stack or physical stack
    fn pushCvConst(self: *ThinCodegenCtx, u_val: u64, tag_val: i64) void {
        const val = self.makeCvConst(u_val, tag_val);
        self.vstackPush(val, true);
    }

    // =========================================================================
    // Named CV constants (correct for both native and WASM32)
    // Native: {u, tag} struct. WASM32: NaN-boxed u64.
    // =========================================================================

    fn cvUndefined(self: *ThinCodegenCtx) llvm.Value {
        if (self.target.is_wasm32) return llvm.constInt(llvm.i64Type(), CV_UNDEFINED, false);
        return self.nativeCvStruct(0, 3); // JS_TAG_UNDEFINED = 3
    }

    fn cvNull(self: *ThinCodegenCtx) llvm.Value {
        if (self.target.is_wasm32) return llvm.constInt(llvm.i64Type(), CV_NULL, false);
        return self.nativeCvStruct(0, 2); // JS_TAG_NULL = 2
    }

    fn cvTrue(self: *ThinCodegenCtx) llvm.Value {
        if (self.target.is_wasm32) return llvm.constInt(llvm.i64Type(), CV_TRUE, false);
        return self.nativeCvStruct(1, 1); // JS_TAG_BOOL = 1, u = 1
    }

    fn cvFalse(self: *ThinCodegenCtx) llvm.Value {
        if (self.target.is_wasm32) return llvm.constInt(llvm.i64Type(), CV_FALSE, false);
        return self.nativeCvStruct(0, 1); // JS_TAG_BOOL = 1, u = 0
    }

    fn cvUninitialized(self: *ThinCodegenCtx) llvm.Value {
        if (self.target.is_wasm32) return llvm.constInt(llvm.i64Type(), CV_UNINITIALIZED, false);
        return self.nativeCvStruct(0, 4); // JS_TAG_UNINITIALIZED = 4
    }

    /// Check if a CV value equals a specific constant (both fields on native, bits on WASM32)
    fn isCvEqual(self: *ThinCodegenCtx, val: llvm.Value, expected: llvm.Value, name: [*:0]const u8) llvm.Value {
        if (self.target.is_wasm32) {
            return self.builder.buildICmp(c.LLVMIntEQ, val, expected, name);
        }
        // Native: both u and tag must match
        const val_u = c.LLVMBuildExtractValue(self.builder.ref, val, 0, "cmp_u");
        const val_tag = c.LLVMBuildExtractValue(self.builder.ref, val, 1, "cmp_tag");
        const exp_u = c.LLVMBuildExtractValue(self.builder.ref, expected, 0, "exp_u");
        const exp_tag = c.LLVMBuildExtractValue(self.builder.ref, expected, 1, "exp_tag");
        const u_eq = self.builder.buildICmp(c.LLVMIntEQ, val_u, exp_u, "ueq");
        const tag_eq = self.builder.buildICmp(c.LLVMIntEQ, val_tag, exp_tag, "teq");
        return self.builder.buildAnd(u_eq, tag_eq, name);
    }

    /// Check if a CV value is undefined
    fn isUndefinedCv(self: *ThinCodegenCtx, val: llvm.Value) llvm.Value {
        if (self.target.is_wasm32) {
            return self.builder.buildICmp(c.LLVMIntEQ, val, llvm.constInt(llvm.i64Type(), CV_UNDEFINED, false), "isundef");
        }
        const tag = c.LLVMBuildExtractValue(self.builder.ref, val, 1, "tag");
        return self.builder.buildICmp(c.LLVMIntEQ, tag, llvm.constInt64(3), "isundef");
    }

    fn isNullCv(self: *ThinCodegenCtx, val: llvm.Value) llvm.Value {
        if (self.target.is_wasm32) {
            return self.builder.buildICmp(c.LLVMIntEQ, val, llvm.constInt(llvm.i64Type(), CV_NULL, false), "isnull");
        }
        const tag = c.LLVMBuildExtractValue(self.builder.ref, val, 1, "tag");
        return self.builder.buildICmp(c.LLVMIntEQ, tag, llvm.constInt64(2), "isnull");
    }

    fn isUninitializedCv(self: *ThinCodegenCtx, val: llvm.Value) llvm.Value {
        if (self.target.is_wasm32) {
            return self.builder.buildICmp(c.LLVMIntEQ, val, llvm.constInt(llvm.i64Type(), CV_UNINITIALIZED, false), "isuninit");
        }
        const tag = c.LLVMBuildExtractValue(self.builder.ref, val, 1, "tag");
        return self.builder.buildICmp(c.LLVMIntEQ, tag, llvm.constInt64(4), "isuninit");
    }

    /// Check if a CV is an object (for inline array/field access)
    fn isObjectCv(self: *ThinCodegenCtx, val: llvm.Value) llvm.Value {
        if (self.target.is_wasm32) {
            const tag = self.builder.buildAnd(val, llvm.constInt(llvm.i64Type(), CV_TAG_MASK, false), "tag");
            return self.builder.buildICmp(c.LLVMIntEQ, tag, llvm.constInt(llvm.i64Type(), CV_QNAN_PTR, false), "isobj");
        }
        // Native: tag == JS_TAG_OBJECT (-1)
        const tag = c.LLVMBuildExtractValue(self.builder.ref, val, 1, "tag");
        return self.builder.buildICmp(c.LLVMIntEQ, tag, llvm.constInt(llvm.i64Type(), @bitCast(@as(i64, -1)), false), "isobj");
    }

    /// Extract pointer from a CV value (for inline object access)
    fn getCvPtr(self: *ThinCodegenCtx, val: llvm.Value) llvm.Value {
        if (self.target.is_wasm32) {
            // Extract lower 48 bits
            return self.builder.buildAnd(val, llvm.constInt(llvm.i64Type(), CV_PAYLOAD_MASK, false), "pint");
        }
        // Native: the u field IS the pointer (as i64)
        return c.LLVMBuildExtractValue(self.builder.ref, val, 0, "pint");
    }

    /// Build a native CV struct constant from u and tag
    fn nativeCvStruct(self: *ThinCodegenCtx, u_val: i64, tag_val: i64) llvm.Value {
        const b = self.builder;
        const cv_ty_val = self.cvTy();
        var val = c.LLVMGetUndef(cv_ty_val);
        val = c.LLVMBuildInsertValue(b.ref, val, llvm.constInt64(u_val), 0, "cv_u");
        val = c.LLVMBuildInsertValue(b.ref, val, llvm.constInt64(tag_val), 1, "cv_tag");
        return val;
    }

    /// Create a CV integer from an i32 constant value (compile-time known)
    fn makeIntCvConst(self: *ThinCodegenCtx, val: i32) llvm.Value {
        if (self.target.is_wasm32) {
            return llvm.constInt(llvm.i64Type(), cvConstInt(val), false);
        }
        const b = self.builder;
        const cv_ty_val = self.cvTy();
        var cv = c.LLVMGetUndef(cv_ty_val);
        cv = c.LLVMBuildInsertValue(b.ref, cv, llvm.constInt64(@as(i64, @as(u32, @bitCast(val)))), 0, "cv_u");
        cv = c.LLVMBuildInsertValue(b.ref, cv, llvm.constInt64(0), 1, "cv_tag"); // JS_TAG_INT = 0
        return cv;
    }

    /// Check if a CV is a boolean
    fn isBoolCv(self: *ThinCodegenCtx, val: llvm.Value) llvm.Value {
        if (self.target.is_wasm32) {
            const CV_QNAN_BOOL: u64 = 0x7FFA_0000_0000_0000;
            const tag = self.builder.buildAnd(val, llvm.constInt(llvm.i64Type(), CV_TAG_MASK, false), "btag");
            return self.builder.buildICmp(c.LLVMIntEQ, tag, llvm.constInt(llvm.i64Type(), CV_QNAN_BOOL, false), "isbool");
        }
        // Native: tag == JS_TAG_BOOL (1)
        const tag = c.LLVMBuildExtractValue(self.builder.ref, val, 1, "btag");
        return self.builder.buildICmp(c.LLVMIntEQ, tag, llvm.constInt64(1), "isbool");
    }

    /// Extract bool truthiness from a CV known to be bool
    fn getBoolTruthy(self: *ThinCodegenCtx, val: llvm.Value) llvm.Value {
        if (self.target.is_wasm32) {
            const payload = self.builder.buildAnd(val, llvm.constInt(llvm.i64Type(), 1, false), "bpay");
            return self.builder.buildICmp(c.LLVMIntNE, payload, llvm.constInt64(0), "btruthy");
        }
        // Native: u field holds 0 (false) or 1 (true)
        const u_val = c.LLVMBuildExtractValue(self.builder.ref, val, 0, "bu");
        return self.builder.buildICmp(c.LLVMIntNE, u_val, llvm.constInt64(0), "btruthy");
    }

    /// Build a CV from a float (f64 bits as i64)
    fn makeFloatCv(self: *ThinCodegenCtx, f64_bits: llvm.Value) llvm.Value {
        if (self.target.is_wasm32) {
            // NaN-boxed: raw f64 bits are the CV
            return f64_bits;
        }
        // Native: {f64_bits, JS_TAG_FLOAT64 (8)}
        return self.makeDynCvStruct(f64_bits, llvm.constInt(llvm.i64Type(), 8, false));
    }

    /// Build a native CV struct from dynamic LLVM values (not compile-time constants)
    fn makeDynCvStruct(self: *ThinCodegenCtx, u_val: llvm.Value, tag_val: llvm.Value) llvm.Value {
        const b = self.builder;
        var cv = c.LLVMGetUndef(self.cvTy());
        cv = c.LLVMBuildInsertValue(b.ref, cv, u_val, 0, "cv_u");
        cv = c.LLVMBuildInsertValue(b.ref, cv, tag_val, 1, "cv_tag");
        return cv;
    }
};

/// Cached JSObject fast-array probe result (stored in function-level allocas).
/// The cache is valid as long as the local variable hasn't been overwritten.
const ArrayCacheEntry = struct {
    /// Alloca holding the cached values pointer ([*]JSValue)
    values_ptr_alloca: llvm.Value,
    /// Alloca holding the cached element count (u32)
    count_alloca: llvm.Value,
    /// Alloca holding the CV value of the array when cache was populated
    /// Used for staleness check: if local's current CV != cached CV, re-probe
    cached_cv_alloca: llvm.Value,
    /// Whether the cache has been populated at least once
    /// (stored as alloca i32: 0=unpopulated, 1=populated)
    populated_alloca: llvm.Value,
};

// ============================================================================
// Array cache helpers (loop-invariant probe hoisting)
// ============================================================================

/// Get or create a cache entry for a local variable's array probe.
/// Allocas are placed in the function entry block so they persist across loop iterations.
fn getOrCreateArrayCache(tctx: *ThinCodegenCtx, local_idx: u32) ArrayCacheEntry {
    if (tctx.array_cache.get(local_idx)) |entry| {
        return entry;
    }

    const b = tctx.builder;
    const ptr_t = llvm.ptrType();
    const i32t = llvm.i32Type();
    // Save current insertion point
    const saved_bb = c.LLVMGetInsertBlock(b.ref);

    // Position at the END of entry block (before its terminator, if any)
    // Actually, entry_bb may have a terminator. We need to insert BEFORE it.
    const term = c.LLVMGetBasicBlockTerminator(tctx.entry_bb);
    if (term != null) {
        c.LLVMPositionBuilderBefore(b.ref, term);
    } else {
        b.positionAtEnd(tctx.entry_bb);
    }

    // Create allocas in entry block
    const values_ptr_alloca = b.buildAlloca(ptr_t, "acvp");
    const count_alloca = b.buildAlloca(i32t, "acnt");
    const cached_cv_alloca = b.buildAlloca(tctx.cvTy(), "accv");
    const populated_alloca = b.buildAlloca(i32t, "acpop");

    // Initialize populated flag to 0 (not populated)
    _ = b.buildStore(llvm.constInt32(0), populated_alloca);
    // Initialize cached_cv to zero (impossible CV for objects)
    _ = b.buildStore(tctx.makeIntCvConst(0), cached_cv_alloca);

    // Restore insertion point
    b.positionAtEnd(saved_bb);

    const entry = ArrayCacheEntry{
        .values_ptr_alloca = values_ptr_alloca,
        .count_alloca = count_alloca,
        .cached_cv_alloca = cached_cv_alloca,
        .populated_alloca = populated_alloca,
    };

    tctx.array_cache.put(tctx.allocator, local_idx, entry) catch unreachable;
    return entry;
}

/// Invalidate array cache for a local variable (called when the local is written).
fn arrayCacheInvalidate(tctx: *ThinCodegenCtx, local_idx: u32) void {
    if (tctx.array_cache.get(local_idx)) |entry| {
        // Set populated flag to 0
        _ = tctx.builder.buildStore(llvm.constInt32(0), entry.populated_alloca);
    }
}

/// Invalidate ALL array cache entries. Called after any function call (op_call,
/// call_method, call_constructor, etc.) because the callee could modify any array's
/// internal storage (e.g., array.push() may reallocate the values buffer), making
/// cached values_ptr pointers dangling.
fn arrayCacheInvalidateAll(tctx: *ThinCodegenCtx) void {
    var it = tctx.array_cache.iterator();
    while (it.next()) |kv| {
        _ = tctx.builder.buildStore(llvm.constInt32(0), kv.value_ptr.populated_alloca);
    }
}

// ============================================================================
// Virtual stack (VStack) helpers
// ============================================================================

/// Push a value onto the virtual stack.
fn vstackPush(tctx: *ThinCodegenCtx, value: llvm.Value, owned: bool) void {
    tctx.vstack.append(tctx.allocator, .{ .value = value, .owned = owned }) catch unreachable;
}

/// Push a value with explicit local_idx tracking (for borrowed-from-local entries).
fn vstackPushLocal(tctx: *ThinCodegenCtx, value: llvm.Value, local_idx: u32) void {
    tctx.vstack.append(tctx.allocator, .{
        .value = value,
        .owned = false,
        .local_idx = @as(i32, @intCast(local_idx)),
    }) catch unreachable;
}

/// Pop one value from the virtual stack. If empty, loads from physical stack (underflow).
fn vstackPop(tctx: *ThinCodegenCtx) VStackEntry {
    if (tctx.vstack.items.len > 0) {
        return tctx.vstack.pop().?;
    }
    // Underflow: load from physical stack (sp-1), decrement sp
    const sp_val = tctx.loadSp();
    const new_sp = tctx.spSub(sp_val, 1, "usp");
    const slot = tctx.stackSlot(new_sp);
    const val = tctx.loadCv(slot, "upop");
    tctx.storeSp(new_sp);
    return .{ .value = val, .owned = true };
}

/// Peek the top value without removing it. If empty, pops from physical stack into vstack.
fn vstackPeek(tctx: *ThinCodegenCtx) VStackEntry {
    if (tctx.vstack.items.len > 0) {
        return tctx.vstack.items[tctx.vstack.items.len - 1];
    }
    // Underflow: pop from physical stack into vstack, then peek
    const sp_val = tctx.loadSp();
    const new_sp = tctx.spSub(sp_val, 1, "usp");
    const slot = tctx.stackSlot(new_sp);
    const val = tctx.loadCv(slot, "upeek");
    tctx.storeSp(new_sp);
    const entry = VStackEntry{ .value = val, .owned = true };
    tctx.vstack.append(tctx.allocator, entry) catch unreachable;
    return entry;
}

/// Flush all virtual stack entries to the physical stack array and update sp.
fn vstackFlush(tctx: *ThinCodegenCtx) void {
    const b = tctx.builder;
    const count = tctx.vstack.items.len;
    if (count == 0) return;

    const sp_val = tctx.loadSp();

    for (tctx.vstack.items, 0..) |entry, i| {
        const idx = b.buildAdd(sp_val, tctx.spConst(@intCast(i)), "fidx");
        const slot = tctx.stackSlot(idx);
        if (entry.owned) {
            _ = b.buildStore(entry.value, slot);
        } else {
            // Borrowed: need to dupRef before writing to physical stack
            const duped = inlineDupRef(tctx, entry.value);
            _ = b.buildStore(duped, slot);
        }
    }

    const new_sp = b.buildAdd(sp_val, tctx.spConst(@intCast(count)), "fsp");
    tctx.storeSp(new_sp);

    tctx.vstack.clearRetainingCapacity();
}

/// Free a virtual stack entry if owned (for drop operations).
/// Borrowed entries are no-ops since the source still holds the reference.
fn vstackFreeEntry(tctx: *ThinCodegenCtx, entry: VStackEntry) void {
    if (entry.owned) {
        inlineFreeRef(tctx, entry.value);
    }
}

/// Invalidate any vstack entries borrowed from a specific local.
/// Called before a local is overwritten to prevent dangling references.
/// Promotes borrowed entries to owned by calling dupRef.
fn vstackInvalidateLocal(tctx: *ThinCodegenCtx, local_idx: u32) void {
    const target: i32 = @intCast(local_idx);
    for (tctx.vstack.items) |*entry| {
        if (!entry.owned and entry.local_idx == target) {
            entry.value = inlineDupRef(tctx, entry.value);
            entry.owned = true;
            entry.local_idx = -1;
        }
    }
    // Also invalidate array cache for this local
    arrayCacheInvalidate(tctx, local_idx);
}

/// Free all owned vstack entries without writing to physical stack.
/// Used before return_undef where the stack is expected to be empty.
fn vstackDiscardAll(tctx: *ThinCodegenCtx) void {
    for (tctx.vstack.items) |entry| {
        if (entry.owned) {
            inlineFreeRef(tctx, entry.value);
        }
    }
    tctx.vstack.clearRetainingCapacity();
}

/// Generate a single thin-codegen function as LLVM IR.
/// Creates a state-machine function calling llvm_rt_* runtime helpers.
/// Returns true if the function has unsafe fallback opcodes (should not be registered).
fn generateThinFunction(
    allocator: Allocator,
    module: llvm.Module,
    builder: llvm.Builder,
    sf: ThinShardFunction,
    rt: *ThinRuntimeDecls,
    target: TargetInfo,
) CodegenError!bool {
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
    // JSValue @__frozen_N_name(ptr %ctx, JSValue %this, i32 %argc, ptr %argv, ptr %var_refs, i32 %closure_cnt, ptr %cpool)
    // Native: JSValue = {i64, i64} (16 bytes), WASM32: JSValue = i64 (8 bytes, NaN-boxed)
    var wrapper_name_buf: [512]u8 = undefined;
    const wrapper_name = std.fmt.bufPrintZ(&wrapper_name_buf, "{s}", .{sf.llvm_func_name}) catch
        return CodegenError.FormatError;

    const jsv_ty = target.jsvalueType();
    const wrapper_params = [_]llvm.Type{
        llvm.ptrType(), // ctx
        jsv_ty, // this_val: JSValue
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
    // closure_var_count is param 5, accessed via tctx.closure_var_count_param
    const cpool_param = c.LLVMGetParam(wrapper_fn, 6);

    // Stack overflow check: if (check_stack()) return throwRangeError
    {
        const overflow = builder.buildCall(
            llvm.functionType(llvm.i1Type(), &.{}, false),
            rt.check_stack, &.{}, "stk_chk",
        );
        const no_overflow_bb = llvm.appendBasicBlock(wrapper_fn, "no_overflow");
        const overflow_bb = llvm.appendBasicBlock(wrapper_fn, "overflow");
        _ = builder.buildCondBr(overflow, overflow_bb, no_overflow_bb);

        // Overflow: return EXCEPTION value
        builder.positionAtEnd(overflow_bb);
        _ = builder.buildRet(target.makeException(builder));

        builder.positionAtEnd(no_overflow_bb);
    }

    // Allocate locals: var locals: [N]CV
    // CV type: {i64,i64} on native (= JSValue), i64 on WASM32 (NaN-boxed)
    const cv_ty = target.cvType();
    const local_count: u32 = @max(func.var_count, 1);
    const locals_ty = llvm.arrayType(cv_ty, local_count);
    const locals_ptr = builder.buildAlloca(locals_ty, "locals");
    // Initialize to UNDEFINED
    const cv_undefined = if (target.is_wasm32) blk: {
        break :blk llvm.constInt64(0x7FFC_0000_0000_0000); // NaN-boxed UNDEFINED
    } else blk: {
        // Native: CV = JSValue = {u=0, tag=JS_TAG_UNDEFINED=3}
        var undef_val = c.LLVMGetUndef(cv_ty);
        undef_val = c.LLVMBuildInsertValue(builder.ref, undef_val, llvm.constInt64(0), 0, "undef_u");
        undef_val = c.LLVMBuildInsertValue(builder.ref, undef_val, llvm.constInt64(3), 1, "undef_tag");
        break :blk undef_val;
    };
    for (0..local_count) |i| {
        var indices = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(@intCast(i)) };
        const elem = builder.buildInBoundsGEP(locals_ty, locals_ptr, &indices, "loc_init");
        _ = builder.buildStore(cv_undefined, elem);
    }

    // Allocate stack
    const stack_array_size: u32 = @max(func.var_count + func.stack_size, 4);
    const stack_ty = llvm.arrayType(cv_ty, stack_array_size);
    const stack_alloc = builder.buildAlloca(stack_ty, "stack");

    // Stack pointer (usize: i64 on native, i32 on WASM32)
    const sp_ptr = builder.buildAlloca(target.usizeType(), "sp");
    _ = builder.buildStore(target.constUsize(0), sp_ptr);

    // Arg shadow (if has_put_arg)
    var arg_shadow_ptr: ?llvm.Value = null;
    if (has_put_arg and func.arg_count > 0) {
        const shadow_ty = llvm.arrayType(cv_ty, func.arg_count);
        arg_shadow_ptr = builder.buildAlloca(shadow_ty, "arg_shadow");
        // Initialize: arg_shadow[i] = i < argc ? dup(cv_from_jsvalue(argv[i])) : UNDEFINED
        // CRITICAL: Must use conditional branching, NOT select. LLVM select evaluates
        // both operands unconditionally, which causes OOB reads from argv when i >= argc.
        // Those OOB reads hit stale stack data (freed JSValues), causing phantom dupRef
        // on freed objects and corrupting GC refcounts.
        const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(builder.ref));
        for (0..func.arg_count) |i| {
            const idx_val = llvm.constInt32(@intCast(i));
            const in_bounds = builder.buildICmp(c.LLVMIntSLT, idx_val, argc_param, "inb");

            const arg_load_bb = llvm.appendBasicBlock(current_fn, "arg_load");
            const arg_undef_bb = llvm.appendBasicBlock(current_fn, "arg_undef");
            const arg_merge_bb = llvm.appendBasicBlock(current_fn, "arg_merge");
            _ = builder.buildCondBr(in_bounds, arg_load_bb, arg_undef_bb);

            // In-bounds path: load argv[i], convert to CV, dupRef
            builder.positionAtEnd(arg_load_bb);
            const jsv_el_ty = target.jsvalueType();
            var argv_idx = [_]llvm.Value{llvm.constInt(llvm.i64Type(), i, false)};
            const argv_elem_ptr = builder.buildInBoundsGEP(jsv_el_ty, argv_param, &argv_idx, "avp");
            const argv_val = builder.buildLoad(jsv_el_ty, argv_elem_ptr, "av");
            const cv_val = builder.buildCall(
                llvm.functionType(cv_ty, &.{jsv_el_ty}, false),
                rt.cv_from_jsvalue, &.{argv_val}, "cv",
            );
            // DupRef: frozen functions are dispatched BEFORE JS_CALL_FLAG_COPY_ARGV,
            // so argv[] contains borrowed references. We must dup to own them.
            const cv_duped = builder.buildCall(
                llvm.functionType(cv_ty, &.{cv_ty}, false),
                rt.cv_dup_ref, &.{cv_val}, "cvdup",
            );
            const load_final_bb = c.LLVMGetInsertBlock(builder.ref);
            _ = builder.buildBr(arg_merge_bb);

            // Out-of-bounds path: use UNDEFINED
            builder.positionAtEnd(arg_undef_bb);
            _ = builder.buildBr(arg_merge_bb);

            // Merge with phi
            builder.positionAtEnd(arg_merge_bb);
            const shadow_val = builder.buildPhi(cv_ty, "shv");
            var phi_vals = [_]llvm.Value{ cv_duped, cv_undefined };
            var phi_bbs = [_]llvm.BasicBlock{ load_final_bb, arg_undef_bb };
            llvm.addIncoming(shadow_val, &phi_vals, &phi_bbs);

            var shadow_idx = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(@intCast(i)) };
            const shadow_elem = builder.buildInBoundsGEP(
                llvm.arrayType(cv_ty, func.arg_count),
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

    // Allocate locals_jsv and var_ref_list for closure sync (v2 shared var_refs)
    // Only allocated for functions that create closures (fclosure/fclosure8).
    var locals_jsv_ptr_val: ?llvm.Value = null;
    var var_ref_list_ptr_val: ?llvm.Value = null;
    if (has_fclosure and local_count > 0) {
        // locals_jsv: array of JSValue. Native: 16 bytes = 2 x i64. WASM32: 8 bytes = 1 x i64.
        const jsv_slots = if (target.is_wasm32) local_count else local_count * 2;
        const jsv_array_ty = llvm.arrayType(llvm.i64Type(), jsv_slots);
        const jsv_alloca = builder.buildAlloca(jsv_array_ty, "locals_jsv");
        var jsv_gep_idx = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
        locals_jsv_ptr_val = builder.buildInBoundsGEP(jsv_array_ty, jsv_alloca, &jsv_gep_idx, "ljv");

        // var_ref_list: ListHead = { prev, next } = 2 pointers
        // Initialize as empty list (prev = next = &self)
        const list_ty = llvm.arrayType(llvm.ptrType(), 2);
        const list_alloca = builder.buildAlloca(list_ty, "var_ref_list");
        var list_gep_idx = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
        const list_flat = builder.buildInBoundsGEP(list_ty, list_alloca, &list_gep_idx, "vrl");
        var_ref_list_ptr_val = list_flat;
        // Initialize: prev = &self, next = &self (empty list)
        _ = builder.buildStore(list_flat, list_flat);
        var next_idx = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(1) };
        const next_ptr = builder.buildInBoundsGEP(list_ty, list_alloca, &next_idx, "vrl_next");
        _ = builder.buildStore(list_flat, next_ptr);
    }

    // for_of iter tracking: allocate stack for nested for...of depth tracking
    var for_of_iter_stack_ptr: ?llvm.Value = null;
    var for_of_depth_ptr: ?llvm.Value = null;
    if (has_for_of) {
        // Max nesting depth of 8 for...of loops (matches Zig runtime)
        const iter_stack_ty = llvm.arrayType(target.usizeType(), 8);
        const iter_stack_alloca = builder.buildAlloca(iter_stack_ty, "fo_iters");
        var iter_gep = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
        for_of_iter_stack_ptr = builder.buildInBoundsGEP(iter_stack_ty, iter_stack_alloca, &iter_gep, "fo_is");

        for_of_depth_ptr = builder.buildAlloca(target.usizeType(), "fo_depth");
        _ = builder.buildStore(target.constUsize(0), for_of_depth_ptr.?);
    }

    // Save the no_overflow insertion point
    const no_overflow_bb = c.LLVMGetInsertBlock(builder.ref);

    // Build the cleanup block (exception path)
    builder.positionAtEnd(cleanup_bb);
    {
        // Call exit_stack, then exceptionCleanup
        _ = builder.buildCall(llvm.functionType(llvm.voidType(), &.{}, false), rt.exit_stack, &.{}, "");
        const usize_t = target.usizeType();
        const cleanup_fn_ty = llvm.functionType(target.jsvalueType(), &.{ llvm.ptrType(), llvm.ptrType(), usize_t, llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), usize_t }, false);
        const cleanup_result = builder.buildCall(
            cleanup_fn_ty,
            rt.exceptionCleanup,
            &.{
                ctx_param,
                locals_flat,
                target.constUsize(local_count),
                locals_jsv_ptr_val orelse llvm.constNull(llvm.ptrType()),
                var_ref_list_ptr_val orelse llvm.constNull(llvm.ptrType()),
                if (arg_shadow_ptr) |sp2| blk: {
                    var si = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
                    break :blk builder.buildInBoundsGEP(llvm.arrayType(cv_ty, func.arg_count), sp2, &si, "asp");
                } else llvm.constNull(llvm.ptrType()),
                target.constUsize(if (has_put_arg) func.arg_count else 0),
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
        .target = target,
        .ctx_param = ctx_param,
        .this_param = this_param,
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
        .for_of_iter_stack_ptr = for_of_iter_stack_ptr,
        .for_of_depth_ptr = for_of_depth_ptr,
        .arg_shadow_ptr = arg_shadow_ptr,
        .locals_jsv_ptr = locals_jsv_ptr_val,
        .var_ref_list_ptr = var_ref_list_ptr_val,
        .cleanup_bb = cleanup_bb,
        .entry_bb = no_overflow_bb,
        .stack_array_size = stack_array_size,
        .local_count = local_count,
    };

    // Pre-allocate vstack capacity (never exceeds physical stack size)
    tctx.vstack.ensureTotalCapacity(allocator, stack_array_size) catch return CodegenError.OutOfMemory;
    defer tctx.vstack.deinit(allocator);
    defer tctx.array_cache.deinit(allocator);

    // At this point, builder is positioned at the end of the "no_overflow" block.
    // We saved our position there after the stack overflow check.

    // Detect counted loops for fast-path specialization
    const counted_loops = cfg_mod.detectCountedLoops(cfg, allocator) catch &[_]CountedLoop{};
    defer if (counted_loops.len > 0) allocator.free(counted_loops);

    // Build header_block_id → CountedLoop map for O(1) lookup during block emission
    var counted_loop_map = std.AutoHashMapUnmanaged(u32, CountedLoop){};
    defer counted_loop_map.deinit(allocator);
    for (counted_loops) |cl| {
        // Specialize array_sum loops bounded by array_length or arg_length
        if (cl.body_pattern == .array_sum and
            (cl.bound_type == .array_length or cl.bound_type == .arg_length) and
            cl.accumulator_local != null)
        {
            counted_loop_map.put(allocator, cl.header_block, cl) catch {};
        }
    }

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

        // Default return: flush vstack, exit_stack + returnUndef
        vstackFlush(&tctx);
        if (!isBlockTerminated(blocks[0])) {
            _ = builder.buildCall(llvm.functionType(llvm.voidType(), &.{}, false), rt.exit_stack, &.{}, "");
            const ret = builder.buildCall(
                llvm.functionType(tctx.target.jsvalueType(), &.{ llvm.ptrType(), llvm.ptrType(), tctx.target.usizeType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), tctx.target.usizeType() }, false),
                rt.returnUndef,
                &.{
                    ctx_param, locals_flat, tctx.target.constUsize(local_count),
                    llvm.constNull(llvm.ptrType()), llvm.constNull(llvm.ptrType()),
                    llvm.constNull(llvm.ptrType()), tctx.target.constUsize(0),
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

            // SSA values can't cross LLVM basic blocks without phis — reset vstack
            tctx.vstack.clearRetainingCapacity();
            tctx.pending_method = .none;
            tctx.pending_math_obj = false;

            // Check if this block is a counted loop header → emit fast-path preheader
            if (counted_loop_map.get(block.id)) |cl| {
                emitCountedLoopFastPath(&tctx, &cl, block_id_ptr, dispatch_bb, llvm_blocks);
            }

            for (block.instructions) |instr| {
                // Skip control flow ops (handled by terminator)
                switch (instr.opcode) {
                    .if_false, .if_false8, .if_true, .if_true8, .goto, .goto8, .goto16 => continue,
                    else => {},
                }

                {
                    emitThinInstruction(&tctx, instr) catch |err| {
                        switch (err) {
                            CodegenError.UnsupportedOpcode => emitFallbackOpcode(&tctx, instr),
                            else => return err,
                        }
                    };
                }
            }

            // Block terminator (vstackFlush handled inside for if_true/if_false optimization)
            emitThinBlockTerminator(&tctx, block, block_id_ptr, dispatch_bb, llvm_blocks);
        }
    }

    return tctx.has_unsafe_fallback;
}

fn isBlockTerminated(block: CFGBasicBlock) bool {
    if (block.instructions.len == 0) return false;
    const last = block.instructions[block.instructions.len - 1].opcode;
    return last == .@"return" or last == .return_undef or last == .throw or
        last == .tail_call or last == .tail_call_method;
}

/// Emit a single thin-codegen instruction as LLVM IR calls to llvm_rt_* functions.
/// Debug: emit ALL opcodes through runtime wrapper calls (no inline LLVM IR).
/// This is the "all-runtime" path used to diagnose codegen bugs in specific functions.
/// If an opcode doesn't have a runtime wrapper, returns UnsupportedOpcode to fall through.
fn emitAllRuntimeInstruction(
    tctx: *ThinCodegenCtx,
    instr: Instruction,
) CodegenError!void {
    // Flush vstack before every runtime call
    vstackFlush(tctx);
    const b = tctx.builder;
    const rt = tctx.rt;
    const ctx_p = tctx.ctx_param;
    const stk = tctx.stack_ptr;
    const sp = tctx.sp_ptr;
    const locs = tctx.locals_ptr;
    const i32t = llvm.i32Type();

    switch (instr.opcode) {
        // Local variables — all through runtime
        .get_loc0, .get_loc1, .get_loc2, .get_loc3, .get_loc, .get_loc8,
        .get_loc_check,
        => {
            const idx: u32 = switch (instr.opcode) {
                .get_loc0 => 0, .get_loc1 => 1, .get_loc2 => 2, .get_loc3 => 3,
                else => instr.operand.loc,
            };
            // For get_loc_check: inline TDZ check then runtime get_loc
            if (instr.opcode == .get_loc_check) {
                const loc_ptr = tctx.localSlot(idx);
                const val = tctx.loadCv(loc_ptr, "locv");
                const is_uninit = tctx.isUninitializedCv(val);
                const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
                const tdz_bb = llvm.appendBasicBlock(current_fn, "dtdz_err");
                const ok_bb = llvm.appendBasicBlock(current_fn, "dtdz_ok");
                _ = b.buildCondBr(is_uninit, tdz_bb, ok_bb);
                b.positionAtEnd(tdz_bb);
                _ = b.buildCall(llvm.functionType(i32t, &.{llvm.ptrType()}, false), rt.throw_tdz, &.{ctx_p}, "");
                _ = b.buildBr(tctx.cleanup_bb);
                b.positionAtEnd(ok_bb);
            }
            _ = b.buildCall(
                llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), i32t }, false),
                rt.get_loc, &.{ ctx_p, stk, sp, locs, llvm.constInt32(@intCast(idx)) }, "",
            );
        },
        .put_loc0, .put_loc1, .put_loc2, .put_loc3, .put_loc, .put_loc8 => {
            const idx: u32 = switch (instr.opcode) {
                .put_loc0 => 0, .put_loc1 => 1, .put_loc2 => 2, .put_loc3 => 3,
                else => instr.operand.loc,
            };
            _ = b.buildCall(
                llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), i32t }, false),
                rt.put_loc, &.{ ctx_p, stk, sp, locs, llvm.constInt32(@intCast(idx)) }, "",
            );
        },
        .put_loc_check, .put_loc_check_init => {
            const idx: u32 = instr.operand.loc;
            // Inline TDZ check for put_loc_check (not put_loc_check_init)
            if (instr.opcode == .put_loc_check) {
                const loc_ptr = tctx.localSlot(idx);
                const old_val = tctx.loadCv(loc_ptr, "oldv");
                const is_uninit = tctx.isUninitializedCv(old_val);
                const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
                const tdz_bb = llvm.appendBasicBlock(current_fn, "dtdz_err2");
                const ok_bb = llvm.appendBasicBlock(current_fn, "dtdz_ok2");
                _ = b.buildCondBr(is_uninit, tdz_bb, ok_bb);
                b.positionAtEnd(tdz_bb);
                _ = b.buildCall(llvm.functionType(i32t, &.{llvm.ptrType()}, false), rt.throw_tdz, &.{ctx_p}, "");
                _ = b.buildBr(tctx.cleanup_bb);
                b.positionAtEnd(ok_bb);
            }
            _ = b.buildCall(
                llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), i32t }, false),
                rt.put_loc, &.{ ctx_p, stk, sp, locs, llvm.constInt32(@intCast(idx)) }, "",
            );
        },
        .set_loc0, .set_loc1, .set_loc2, .set_loc3, .set_loc, .set_loc8 => {
            const idx: u32 = switch (instr.opcode) {
                .set_loc0 => 0, .set_loc1 => 1, .set_loc2 => 2, .set_loc3 => 3,
                else => instr.operand.loc,
            };
            _ = b.buildCall(
                llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), i32t }, false),
                rt.set_loc, &.{ ctx_p, stk, sp, locs, llvm.constInt32(@intCast(idx)) }, "",
            );
        },
        .set_loc_uninitialized => {
            // Inline: free old value, store UNINITIALIZED (no runtime wrapper exists)
            const idx = instr.operand.loc;
            const loc_ptr = tctx.localSlot(idx);
            const old_val = tctx.loadCv(loc_ptr, "oldv");
            inlineFreeRef(tctx, old_val);
            _ = b.buildStore(tctx.cvUninitialized(), loc_ptr);
        },

        // Stack operations — all through runtime
        .dup => { callVoid3(b, rt.dup_top, ctx_p, stk, sp); },
        .dup1 => { callVoid3(b, rt.dup1, ctx_p, stk, sp); },
        .dup2 => { callVoid3(b, rt.dup2, ctx_p, stk, sp); },
        .dup3 => { callVoid3(b, rt.dup3, ctx_p, stk, sp); },
        .drop => { callVoid3(b, rt.drop, ctx_p, stk, sp); },
        .nip => { callVoid3(b, rt.nip, ctx_p, stk, sp); },
        .nip1 => { callVoid3(b, rt.nip1, ctx_p, stk, sp); },
        .swap => { callVoid3(b, rt.swap, ctx_p, stk, sp); },
        .insert2 => { callVoid3(b, rt.insert2, ctx_p, stk, sp); },
        .insert3 => { callVoid3(b, rt.insert3, ctx_p, stk, sp); },
        .insert4 => { callVoid3(b, rt.insert4, ctx_p, stk, sp); },

        // Constants — through runtime
        .push_i8 => {
            const val: i32 = @intCast(instr.operand.i8);
            _ = b.buildCall(
                llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), i32t }, false),
                rt.push_i32, &.{ stk, sp, llvm.constInt32(@bitCast(val)) }, "",
            );
        },
        .push_i16 => {
            const val: i32 = @intCast(instr.operand.i16);
            _ = b.buildCall(
                llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), i32t }, false),
                rt.push_i32, &.{ stk, sp, llvm.constInt32(@bitCast(val)) }, "",
            );
        },
        .push_i32 => {
            _ = b.buildCall(
                llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), i32t }, false),
                rt.push_i32, &.{ stk, sp, llvm.constInt32(@bitCast(instr.operand.i32)) }, "",
            );
        },
        .push_0 => { _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), i32t }, false), rt.push_i32, &.{ stk, sp, llvm.constInt32(0) }, ""); },
        .push_1 => { _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), i32t }, false), rt.push_i32, &.{ stk, sp, llvm.constInt32(1) }, ""); },
        .push_2 => { _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), i32t }, false), rt.push_i32, &.{ stk, sp, llvm.constInt32(2) }, ""); },
        .push_3 => { _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), i32t }, false), rt.push_i32, &.{ stk, sp, llvm.constInt32(3) }, ""); },
        .push_4 => { _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), i32t }, false), rt.push_i32, &.{ stk, sp, llvm.constInt32(4) }, ""); },
        .push_5 => { _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), i32t }, false), rt.push_i32, &.{ stk, sp, llvm.constInt32(5) }, ""); },
        .push_6 => { _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), i32t }, false), rt.push_i32, &.{ stk, sp, llvm.constInt32(6) }, ""); },
        .push_7 => { _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), i32t }, false), rt.push_i32, &.{ stk, sp, llvm.constInt32(7) }, ""); },
        .push_minus1 => { _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), i32t }, false), rt.push_i32, &.{ stk, sp, llvm.constInt32(@bitCast(@as(i32, -1))) }, ""); },
        .push_true => { callVoid2(b, rt.push_true, stk, sp); },
        .push_false => { callVoid2(b, rt.push_false, stk, sp); },
        .null => { callVoid2(b, rt.push_null, stk, sp); },
        .undefined => { callVoid2(b, rt.push_undefined, stk, sp); },
        .push_this => {
            _ = b.buildCall(
                llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), tctx.target.jsvalueType() }, false),
                rt.push_this, &.{ ctx_p, stk, sp, tctx.this_param }, "",
            );
        },
        .push_empty_string => { callVoid3(b, rt.push_empty_string, ctx_p, stk, sp); },
        .push_const8, .push_const => {
            // Constant pool access through exec_opcode fallback
            return CodegenError.UnsupportedOpcode;
        },

        // Arithmetic — through runtime
        .add => { callVoid3(b, rt.op_add, ctx_p, stk, sp); },
        .sub => { callVoid3(b, rt.op_sub, ctx_p, stk, sp); },
        .mul => { callVoid3(b, rt.op_mul, ctx_p, stk, sp); },
        .div => { callVoid3(b, rt.op_div, ctx_p, stk, sp); },
        .mod => { callVoid3(b, rt.op_mod, ctx_p, stk, sp); },
        .neg => { callVoid3(b, rt.op_neg, ctx_p, stk, sp); },
        .plus => { callVoid3(b, rt.op_plus, ctx_p, stk, sp); },

        // Bitwise — through runtime
        .@"and" => { callVoid3(b, rt.op_band, ctx_p, stk, sp); },
        .@"or" => { callVoid3(b, rt.op_bor, ctx_p, stk, sp); },
        .xor => { callVoid3(b, rt.op_bxor, ctx_p, stk, sp); },
        .not => { callVoid3(b, rt.op_bnot, ctx_p, stk, sp); },
        .shl => { callVoid3(b, rt.op_shl, ctx_p, stk, sp); },
        .sar => { callVoid3(b, rt.op_sar, ctx_p, stk, sp); },
        .shr => { callVoid3(b, rt.op_shr, ctx_p, stk, sp); },

        // Comparison — through runtime
        .lt => { callVoid3(b, rt.op_lt, ctx_p, stk, sp); },
        .lte => { callVoid3(b, rt.op_lte, ctx_p, stk, sp); },
        .gt => { callVoid3(b, rt.op_gt, ctx_p, stk, sp); },
        .gte => { callVoid3(b, rt.op_gte, ctx_p, stk, sp); },
        .eq => { callVoid3(b, rt.op_eq, ctx_p, stk, sp); },
        .neq => { callVoid3(b, rt.op_neq, ctx_p, stk, sp); },
        .strict_eq => { callVoid3(b, rt.op_strict_eq, ctx_p, stk, sp); },
        .strict_neq => { callVoid3(b, rt.op_strict_neq, ctx_p, stk, sp); },

        // Logical/type
        .lnot => { callVoid3(b, rt.op_lnot, ctx_p, stk, sp); },
        .typeof => { callVoid3(b, rt.op_typeof, ctx_p, stk, sp); },
        .is_undefined => { callVoid3(b, rt.is_undefined, ctx_p, stk, sp); },
        .is_null => { callVoid3(b, rt.is_null, ctx_p, stk, sp); },
        .is_undefined_or_null => { callVoid3(b, rt.is_undefined_or_null, ctx_p, stk, sp); },

        // Increment/decrement
        .inc => { callVoid3(b, rt.inc, ctx_p, stk, sp); },
        .dec => { callVoid3(b, rt.dec, ctx_p, stk, sp); },
        .post_inc => { callVoid3(b, rt.post_inc, ctx_p, stk, sp); },
        .post_dec => { callVoid3(b, rt.post_dec, ctx_p, stk, sp); },
        .inc_loc => { callVoid4u(b, rt.inc_loc, ctx_p, stk, sp, locs, instr.operand.loc); },
        .dec_loc => { callVoid4u(b, rt.dec_loc, ctx_p, stk, sp, locs, instr.operand.loc); },

        // Arguments — through runtime
        .get_arg0, .get_arg1, .get_arg2, .get_arg3, .get_arg => {
            const idx: u32 = switch (instr.opcode) {
                .get_arg0 => 0, .get_arg1 => 1, .get_arg2 => 2, .get_arg3 => 3,
                else => instr.operand.arg,
            };
            if (tctx.has_put_arg) {
                if (tctx.arg_shadow_ptr) |asp| {
                    var si = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
                    const asp_flat = b.buildInBoundsGEP(
                        llvm.arrayType(tctx.cvTy(), tctx.func.arg_count),
                        asp, &si, "asp",
                    );
                    // get_arg_shadow(stack, sp, arg_shadow, idx) — no ctx param
                    _ = b.buildCall(
                        llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), i32t }, false),
                        rt.get_arg_shadow, &.{ stk, sp, asp_flat, llvm.constInt32(@intCast(idx)) }, "",
                    );
                }
            } else {
                // get_arg(ctx, stack, sp, argc, argv, idx)
                _ = b.buildCall(
                    llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), i32t, llvm.ptrType(), i32t }, false),
                    rt.get_arg, &.{ ctx_p, stk, sp, tctx.argc_param, tctx.argv_param, llvm.constInt32(@intCast(idx)) }, "",
                );
            }
        },

        // ===== Return (must emit exit_stack + returnFromStack/returnUndef + buildRet) =====
        .@"return" => {
            _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{}, false), rt.exit_stack, &.{}, "");
            const ret = b.buildCall(
                llvm.functionType(tctx.target.jsvalueType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), tctx.target.usizeType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), tctx.target.usizeType() }, false),
                rt.returnFromStack,
                &.{
                    ctx_p, stk, sp, locs, tctx.target.constUsize(tctx.local_count),
                    tctx.locals_jsv_ptr orelse llvm.constNull(llvm.ptrType()),
                    tctx.var_ref_list_ptr orelse llvm.constNull(llvm.ptrType()),
                    if (tctx.arg_shadow_ptr) |asp| blk: {
                        var si = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
                        break :blk b.buildInBoundsGEP(llvm.arrayType(tctx.cvTy(), tctx.func.arg_count), asp, &si, "asp");
                    } else llvm.constNull(llvm.ptrType()),
                    tctx.target.constUsize(if (tctx.has_put_arg) tctx.func.arg_count else 0),
                },
                "ret",
            );
            _ = b.buildRet(ret);
        },
        .return_undef => {
            _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{}, false), rt.exit_stack, &.{}, "");
            const ret = b.buildCall(
                llvm.functionType(tctx.target.jsvalueType(), &.{ llvm.ptrType(), llvm.ptrType(), tctx.target.usizeType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), tctx.target.usizeType() }, false),
                rt.returnUndef,
                &.{
                    ctx_p, locs, tctx.target.constUsize(tctx.local_count),
                    tctx.locals_jsv_ptr orelse llvm.constNull(llvm.ptrType()),
                    tctx.var_ref_list_ptr orelse llvm.constNull(llvm.ptrType()),
                    if (tctx.arg_shadow_ptr) |asp| blk: {
                        var si = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
                        break :blk b.buildInBoundsGEP(llvm.arrayType(tctx.cvTy(), tctx.func.arg_count), asp, &si, "asp");
                    } else llvm.constNull(llvm.ptrType()),
                    tctx.target.constUsize(if (tctx.has_put_arg) tctx.func.arg_count else 0),
                },
                "ret",
            );
            _ = b.buildRet(ret);
        },

        // ===== Calls (through runtime, no inline optimizations) =====
        .call0, .call1, .call2, .call3, .call => {
            const argc: u32 = switch (instr.opcode) {
                .call0 => 0, .call1 => 1, .call2 => 2, .call3 => 3,
                else => instr.operand.u16,
            };
            const err_code = b.buildCall(
                llvm.functionType(i32t, &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), i32t }, false),
                rt.op_call, &.{ ctx_p, stk, sp, llvm.constInt32(@intCast(argc)) }, "err",
            );
            emitErrorCheck(tctx, err_code);
        },
        .call_method => {
            const argc: u32 = instr.operand.u16;
            const err_code = b.buildCall(
                llvm.functionType(i32t, &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), i32t }, false),
                rt.op_call_method, &.{ ctx_p, stk, sp, llvm.constInt32(@intCast(argc)) }, "err",
            );
            emitErrorCheck(tctx, err_code);
            tctx.pending_method = .none;
        },
        .call_constructor => {
            const argc: u32 = instr.operand.u16;
            const err_code = b.buildCall(
                llvm.functionType(i32t, &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), i32t }, false),
                rt.op_call_constructor, &.{ ctx_p, stk, sp, llvm.constInt32(@intCast(argc)) }, "err",
            );
            emitErrorCheck(tctx, err_code);
        },

        // ===== Field ops (through runtime, no inline IC) =====
        .get_field => {
            emitThinFieldOp(tctx, rt.op_get_field_ic, instr);
        },
        .get_field2 => {
            emitThinFieldOp(tctx, rt.op_get_field2_ic, instr);
            tctx.pending_math_obj = false;
        },
        .put_field => {
            emitThinPutFieldOp(tctx, instr);
        },

        // ===== Closure vars (through runtime, no inline) =====
        .get_var_ref, .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3 => {
            const idx: u32 = switch (instr.opcode) {
                .get_var_ref0 => 0, .get_var_ref1 => 1, .get_var_ref2 => 2, .get_var_ref3 => 3,
                else => instr.operand.var_ref,
            };
            _ = b.buildCall(
                llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), i32t, i32t }, false),
                rt.get_var_ref, &.{ ctx_p, stk, sp, tctx.var_refs_param, llvm.constInt32(@intCast(idx)), tctx.closure_var_count_param }, "",
            );
        },

        // ===== put_arg (through runtime, needs arg_shadow) =====
        .put_arg0, .put_arg1, .put_arg2, .put_arg3, .put_arg => {
            const idx: u32 = switch (instr.opcode) {
                .put_arg0 => 0, .put_arg1 => 1, .put_arg2 => 2, .put_arg3 => 3,
                else => instr.operand.arg,
            };
            if (tctx.arg_shadow_ptr) |asp| {
                var si = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
                const asp_flat = b.buildInBoundsGEP(
                    llvm.arrayType(tctx.cvTy(), tctx.func.arg_count),
                    asp, &si, "asp",
                );
                callVoid4u(b, rt.put_arg, ctx_p, stk, sp, asp_flat, idx);
            }
        },

        // ===== array_from (dedicated handler, bypasses exec_opcode) =====
        .array_from => {
            const count: u32 = instr.operand.u16;
            const err_code = b.buildCall(
                llvm.functionType(i32t, &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), i32t }, false),
                rt.op_array_from, &.{ ctx_p, stk, sp, llvm.constInt32(@intCast(count)) }, "err",
            );
            emitErrorCheck(tctx, err_code);
        },

        // ===== for...of iterator ops (use dedicated runtime, not exec_opcode) =====
        .for_of_start => {
            if (tctx.for_of_iter_stack_ptr) |isp| {
                const err_code = b.buildCall(
                    llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                    rt.op_for_of_start, &.{ ctx_p, stk, sp, isp, tctx.for_of_depth_ptr.? }, "err",
                );
                emitErrorCheck(tctx, err_code);
            } else return CodegenError.UnsupportedOpcode;
        },
        .for_of_next => {
            if (tctx.for_of_iter_stack_ptr) |isp| {
                const err_code = b.buildCall(
                    llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                    rt.op_for_of_next, &.{ ctx_p, stk, sp, isp, tctx.for_of_depth_ptr.? }, "err",
                );
                emitErrorCheck(tctx, err_code);
            } else return CodegenError.UnsupportedOpcode;
        },
        .iterator_close => {
            if (tctx.for_of_iter_stack_ptr) |isp| {
                _ = b.buildCall(
                    llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                    rt.op_iterator_close, &.{ ctx_p, stk, sp, isp, tctx.for_of_depth_ptr.? }, "",
                );
            } else return CodegenError.UnsupportedOpcode;
        },

        // ===== No-op / handled by terminator =====
        .nop, .set_name, .set_home_object => {},

        // Everything else — fall through to emitFallbackOpcode (exec_opcode)
        else => return CodegenError.UnsupportedOpcode,
    }
}

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
        // ========== Constants (pushed to vstack as owned SSA values) ==========
        .push_0 => vstackPush(tctx, tctx.makeIntCvConst(0), true),
        .push_1 => vstackPush(tctx, tctx.makeIntCvConst(1), true),
        .push_2 => vstackPush(tctx, tctx.makeIntCvConst(2), true),
        .push_3 => vstackPush(tctx, tctx.makeIntCvConst(3), true),
        .push_4 => vstackPush(tctx, tctx.makeIntCvConst(4), true),
        .push_5 => vstackPush(tctx, tctx.makeIntCvConst(5), true),
        .push_6 => vstackPush(tctx, tctx.makeIntCvConst(6), true),
        .push_7 => vstackPush(tctx, tctx.makeIntCvConst(7), true),
        .push_minus1 => vstackPush(tctx, tctx.makeIntCvConst(-1), true),
        .push_i8 => vstackPush(tctx, tctx.makeIntCvConst(instr.operand.i8), true),
        .push_i16 => vstackPush(tctx, tctx.makeIntCvConst(instr.operand.i16), true),
        .push_i32 => vstackPush(tctx, tctx.makeIntCvConst(instr.operand.i32), true),
        .push_true => vstackPush(tctx, tctx.cvTrue(), true),
        .push_false => vstackPush(tctx, tctx.cvFalse(), true),
        .null => vstackPush(tctx, tctx.cvNull(), true),
        .undefined => vstackPush(tctx, tctx.cvUndefined(), true),

        .push_this => {
            vstackFlush(tctx);
            _ = b.buildCall(
                llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), tctx.target.jsvalueType() }, false),
                rt.push_this,
                &.{ ctx_p, stk, sp, tctx.this_param },
                "",
            );
        },
        .push_empty_string => {
            vstackFlush(tctx);
            callVoid3(b, rt.push_empty_string, ctx_p, stk, sp);
        },

        .push_const8, .push_const => {
            emitVstackPushConst(tctx, instr.operand.const_idx);
        },

        // ========== Local Variables (vstack-optimized) ==========
        .get_loc0 => emitVstackGetLoc(tctx, 0),
        .get_loc1 => emitVstackGetLoc(tctx, 1),
        .get_loc2 => emitVstackGetLoc(tctx, 2),
        .get_loc3 => emitVstackGetLoc(tctx, 3),
        .get_loc, .get_loc8 => emitVstackGetLoc(tctx, instr.operand.loc),
        .get_loc0_loc1 => {
            emitVstackGetLoc(tctx, 0);
            emitVstackGetLoc(tctx, 1);
        },

        .put_loc0 => emitVstackPutLoc(tctx, 0),
        .put_loc1 => emitVstackPutLoc(tctx, 1),
        .put_loc2 => emitVstackPutLoc(tctx, 2),
        .put_loc3 => emitVstackPutLoc(tctx, 3),
        .put_loc, .put_loc8 => emitVstackPutLoc(tctx, instr.operand.loc),

        .set_loc0 => emitVstackSetLoc(tctx, 0),
        .set_loc1 => emitVstackSetLoc(tctx, 1),
        .set_loc2 => emitVstackSetLoc(tctx, 2),
        .set_loc3 => emitVstackSetLoc(tctx, 3),
        .set_loc, .set_loc8 => emitVstackSetLoc(tctx, instr.operand.loc),

        // ========== Arguments (flush vstack before runtime calls) ==========
        .get_arg0, .get_arg1, .get_arg2, .get_arg3, .get_arg => {
            const idx: u32 = switch (instr.opcode) {
                .get_arg0 => 0,
                .get_arg1 => 1,
                .get_arg2 => 2,
                .get_arg3 => 3,
                else => instr.operand.arg,
            };
            if (tctx.has_put_arg) {
                // Inline arg_shadow read: load CV, dupRef, push to vstack
                if (tctx.arg_shadow_ptr) |asp| {
                    var si = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(@intCast(idx)) };
                    const elem_ptr = b.buildInBoundsGEP(
                        llvm.arrayType(tctx.cvTy(), tctx.func.arg_count),
                        asp, &si, "aselem",
                    );
                    const cv_val = tctx.loadCv(elem_ptr, "ascv");
                    const duped = inlineDupRef(tctx, cv_val);
                    vstackPush(tctx, duped, true);
                }
            } else {
                emitVstackGetArg(tctx, idx);
            }
        },

        .put_arg0, .put_arg1, .put_arg2, .put_arg3, .put_arg => {
            vstackFlush(tctx);
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
                    llvm.arrayType(tctx.cvTy(), tctx.func.arg_count),
                    asp, &si, "asp",
                );
                callVoid4u(b, rt.put_arg, ctx_p, stk, sp, asp_flat, idx);
            }
        },

        .set_arg0, .set_arg1, .set_arg2, .set_arg3, .set_arg => {
            vstackFlush(tctx);
            const idx: u32 = switch (instr.opcode) {
                .set_arg0 => 0,
                .set_arg1 => 1,
                .set_arg2 => 2,
                .set_arg3 => 3,
                else => instr.operand.arg,
            };
            if (tctx.arg_shadow_ptr) |asp| {
                var si = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
                const asp_flat = b.buildInBoundsGEP(
                    llvm.arrayType(tctx.cvTy(), tctx.func.arg_count),
                    asp, &si, "asp",
                );
                callVoid4u(b, rt.set_arg, ctx_p, stk, sp, asp_flat, idx);
            }
        },

        // ========== Closure Variables ==========
        .get_var_ref_check => {
            vstackFlush(tctx);
            // get_var_ref with TDZ check — returns error code (0=ok, 1=exception)
            const idx: u32 = instr.operand.var_ref;
            const err_code = b.buildCall(
                llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type(), llvm.i32Type() }, false),
                rt.get_var_ref_check, &.{ ctx_p, stk, sp, tctx.var_refs_param, llvm.constInt32(@intCast(idx)), tctx.closure_var_count_param }, "err",
            );
            emitErrorCheck(tctx, err_code);
        },
        // get_var_ref: vstack-optimized inline with int fast path
        .get_var_ref0, .get_var_ref1, .get_var_ref2, .get_var_ref3, .get_var_ref => {
            const idx: u32 = switch (instr.opcode) {
                .get_var_ref0 => 0,
                .get_var_ref1 => 1,
                .get_var_ref2 => 2,
                .get_var_ref3 => 3,
                else => instr.operand.var_ref,
            };
            if (tctx.target.is_wasm32) {
                vstackFlush(tctx);
                _ = b.buildCall(
                    llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type(), llvm.i32Type() }, false),
                    rt.get_var_ref, &.{ ctx_p, stk, sp, tctx.var_refs_param, llvm.constInt32(@intCast(idx)), tctx.closure_var_count_param }, "",
                );
            } else {
                emitVstackGetVarRef(tctx, idx);
            }
        },
        // put_var_ref / set_var_ref: flush + runtime (write ops need old-value free)
        .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3, .put_var_ref,
        .set_var_ref0, .set_var_ref1, .set_var_ref2, .set_var_ref3, .set_var_ref,
        => {
            vstackFlush(tctx);
            const idx: u32 = switch (instr.opcode) {
                .put_var_ref0, .set_var_ref0 => 0,
                .put_var_ref1, .set_var_ref1 => 1,
                .put_var_ref2, .set_var_ref2 => 2,
                .put_var_ref3, .set_var_ref3 => 3,
                else => instr.operand.var_ref,
            };
            const rt_fn = switch (instr.opcode) {
                .put_var_ref0, .put_var_ref1, .put_var_ref2, .put_var_ref3, .put_var_ref => rt.put_var_ref,
                else => rt.set_var_ref,
            };
            _ = b.buildCall(
                llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type(), llvm.i32Type() }, false),
                rt_fn, &.{ ctx_p, stk, sp, tctx.var_refs_param, llvm.constInt32(@intCast(idx)), tctx.closure_var_count_param }, "",
            );
        },

        // ========== Stack Operations (vstack-optimized) ==========
        .dup => {
            // Duplicate top: peek + dupRef + push copy
            const entry = vstackPeek(tctx);
            const duped = inlineDupRef(tctx, entry.value);
            vstackPush(tctx, duped, true);
        },
        .dup1, .dup2, .dup3 => {
            vstackFlush(tctx);
            const rt_fn = switch (instr.opcode) {
                .dup1 => rt.dup1,
                .dup2 => rt.dup2,
                .dup3 => rt.dup3,
                else => unreachable,
            };
            callVoid3(b, rt_fn, ctx_p, stk, sp);
        },
        .drop => {
            // Pop top and free if owned; borrowed entries are no-op
            const entry = vstackPop(tctx);
            vstackFreeEntry(tctx, entry);
        },
        .nip => {
            // Remove second-from-top: pop top, pop second (free it), push top back
            const top = vstackPop(tctx);
            const second = vstackPop(tctx);
            vstackFreeEntry(tctx, second);
            // Preserve local_idx when re-pushing (critical for borrowed-entry tracking)
            tctx.vstack.appendAssumeCapacity(.{ .value = top.value, .owned = top.owned, .local_idx = top.local_idx });
        },
        .swap => {
            // Swap top two entries
            if (tctx.vstack.items.len >= 2) {
                // Both on vstack: just swap entries (no memory, no refs)
                const len = tctx.vstack.items.len;
                const tmp = tctx.vstack.items[len - 1];
                tctx.vstack.items[len - 1] = tctx.vstack.items[len - 2];
                tctx.vstack.items[len - 2] = tmp;
            } else {
                // Pop both (may underflow to physical stack), push in reverse order
                // Preserve local_idx when re-pushing (critical for borrowed-entry tracking)
                const top = vstackPop(tctx);
                const second = vstackPop(tctx);
                tctx.vstack.appendAssumeCapacity(.{ .value = top.value, .owned = top.owned, .local_idx = top.local_idx });
                tctx.vstack.appendAssumeCapacity(.{ .value = second.value, .owned = second.owned, .local_idx = second.local_idx });
            }
        },
        .insert2, .insert3, .insert4 => {
            vstackFlush(tctx);
            const rt_fn = switch (instr.opcode) {
                .insert2 => rt.insert2,
                .insert3 => rt.insert3,
                .insert4 => rt.insert4,
                else => unreachable,
            };
            callVoid3(b, rt_fn, ctx_p, stk, sp);
        },
        .perm3, .perm4, .perm5 => {
            vstackFlush(tctx);
            const rt_fn = switch (instr.opcode) {
                .perm3 => rt.perm3,
                .perm4 => rt.perm4,
                .perm5 => rt.perm5,
                else => unreachable,
            };
            callVoid2(b, rt_fn, stk, sp);
        },
        .rot3l, .rot3r => {
            vstackFlush(tctx);
            callVoid2(b, if (instr.opcode == .rot3l) rt.rot3l else rt.rot3r, stk, sp);
        },
        .nip1 => {
            vstackFlush(tctx);
            callVoid3(b, rt.nip1, ctx_p, stk, sp);
        },

        // ========== Arithmetic (vstack: int fast path on SSA, slow path flushes) ==========
        .add => emitVstackArith(tctx, .add, rt.op_add, true), // add needs ctx for string concat
        .sub => emitVstackArith(tctx, .sub, rt.op_sub, true),
        .mul => emitVstackArith(tctx, .mul, rt.op_mul, true),
        .div => emitVstackDivMod(tctx, true, rt.op_div),
        .mod => emitVstackDivMod(tctx, false, rt.op_mod),
        .pow => { vstackFlush(tctx); callVoid3(b, rt.op_pow, ctx_p, stk, sp); },
        .neg => { vstackFlush(tctx); callVoid3(b, rt.op_neg, ctx_p, stk, sp); },
        .plus => { vstackFlush(tctx); callVoid3(b, rt.op_plus, ctx_p, stk, sp); },

        // ========== Bitwise (vstack: int fast path on SSA) ==========
        .@"and" => emitVstackBitwise(tctx, .@"and", rt.op_band),
        .@"or" => emitVstackBitwise(tctx, .@"or", rt.op_bor),
        .xor => emitVstackBitwise(tctx, .xor, rt.op_bxor),
        .shl => emitVstackBitwise(tctx, .shl, rt.op_shl),
        .sar => emitVstackBitwise(tctx, .sar, rt.op_sar),
        .shr => emitVstackBitwise(tctx, .shr, rt.op_shr),
        .not => { vstackFlush(tctx); callVoid3(b, rt.op_bnot, ctx_p, stk, sp); },

        // ========== Comparison (vstack: int fast path on SSA) ==========
        .lt => emitVstackCmp(tctx, c.LLVMIntSLT, rt.op_lt),
        .lte => emitVstackCmp(tctx, c.LLVMIntSLE, rt.op_lte),
        .gt => emitVstackCmp(tctx, c.LLVMIntSGT, rt.op_gt),
        .gte => emitVstackCmp(tctx, c.LLVMIntSGE, rt.op_gte),
        .eq => emitVstackEq(tctx, true, rt.op_eq),
        .neq => emitVstackEq(tctx, false, rt.op_neq),
        .strict_eq => emitVstackEq(tctx, true, rt.op_strict_eq),
        .strict_neq => emitVstackEq(tctx, false, rt.op_strict_neq),

        // ========== Logical / Type (flush vstack) ==========
        .lnot => { vstackFlush(tctx); callVoid3(b, rt.op_lnot, ctx_p, stk, sp); },
        .typeof => { vstackFlush(tctx); callVoid3(b, rt.op_typeof, ctx_p, stk, sp); },

        // ========== Type Checks (flush vstack) ==========
        .is_undefined => {
            const entry = vstackPop(tctx);
            vstackFreeEntry(tctx, entry);
            const is_undef = tctx.isUndefinedCv(entry.value);
            const result = b.buildSelect(is_undef, tctx.cvTrue(), tctx.cvFalse(), "isundefv");
            vstackPush(tctx, result, true);
        },
        .is_null => {
            const entry = vstackPop(tctx);
            vstackFreeEntry(tctx, entry);
            const is_null_v = tctx.isNullCv(entry.value);
            const result = b.buildSelect(is_null_v, tctx.cvTrue(), tctx.cvFalse(), "isnullv");
            vstackPush(tctx, result, true);
        },
        .is_undefined_or_null => {
            const entry = vstackPop(tctx);
            vstackFreeEntry(tctx, entry);
            const is_undef = tctx.isUndefinedCv(entry.value);
            const is_null_v = tctx.isNullCv(entry.value);
            const either = b.buildOr(is_undef, is_null_v, "isun");
            const result = b.buildSelect(either, tctx.cvTrue(), tctx.cvFalse(), "isunv");
            vstackPush(tctx, result, true);
        },

        // ========== Stack-based Increment / Decrement (flush vstack) ==========
        .inc => { vstackFlush(tctx); callVoid2(b, rt.inc, stk, sp); },
        .dec => { vstackFlush(tctx); callVoid2(b, rt.dec, stk, sp); },
        .post_inc => { vstackFlush(tctx); callVoid2(b, rt.post_inc, stk, sp); },
        .post_dec => { vstackFlush(tctx); callVoid2(b, rt.post_dec, stk, sp); },

        // ========== Object Creation (flush vstack) ==========
        .object => { vstackFlush(tctx); callVoid3(b, rt.object, ctx_p, stk, sp); },

        // ========== Increment / Decrement (local, inlined with int fast path) ==========
        .inc_loc => emitInlineIncDecLoc(tctx, instr.operand.loc, true),
        .dec_loc => emitInlineIncDecLoc(tctx, instr.operand.loc, false),

        // ========== Function Calls (with error check) ==========
        .call0 => emitThinCallOp(tctx, rt.op_call, 0),
        .call1 => emitThinCallOp(tctx, rt.op_call, 1),
        .call2 => emitThinCallOp(tctx, rt.op_call, 2),
        .call3 => emitThinCallOp(tctx, rt.op_call, 3),
        .call => emitThinCallOp(tctx, rt.op_call, instr.operand.u16),
        .call_method => {
            const argc = instr.operand.u16;
            const method_sig = llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false);
            const method_args = &[_]llvm.Value{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr };
            if (argc == 1 and tctx.pending_method == .char_code_at) {
                // charCodeAt fast path: str.charCodeAt(idx) via C helper
                tctx.pending_method = .none;
                vstackFlush(tctx);
                const err_code = b.buildCall(method_sig, rt.call_method_char_code_at, method_args, "err");
                emitErrorCheck(tctx, err_code);
                arrayCacheInvalidateAll(tctx);
            } else if (argc == 1 and tctx.pending_method == .array_push) {
                // Array.push fast path: arr.push(val) via add_fast_array_element
                tctx.pending_method = .none;
                vstackFlush(tctx);
                const err_code = b.buildCall(method_sig, rt.call_method_array_push, method_args, "err");
                emitErrorCheck(tctx, err_code);
                arrayCacheInvalidateAll(tctx);
            } else if (argc == 1 and @intFromEnum(tctx.pending_method) >= @intFromEnum(PendingMethodKind.math_abs) and
                @intFromEnum(tctx.pending_method) <= @intFromEnum(PendingMethodKind.math_round))
            {
                // Math.abs/sqrt/floor/ceil/round fast path
                const op_id: i32 = @as(i32, @intCast(@intFromEnum(tctx.pending_method))) - @as(i32, @intCast(@intFromEnum(PendingMethodKind.math_abs)));
                tctx.pending_method = .none;
                vstackFlush(tctx);
                const math_sig = llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false);
                const err_code = b.buildCall(math_sig, rt.call_method_math_unary, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, llvm.constInt32(@intCast(op_id)) }, "err");
                emitErrorCheck(tctx, err_code);
                arrayCacheInvalidateAll(tctx);
            } else {
                tctx.pending_method = .none;
                emitThinCallOp(tctx, rt.op_call_method, argc);
            }
        },
        .tail_call => {
            vstackFlush(tctx);
            // Sync locals and detach var_refs before tail call (closures must be detached)
            if (tctx.locals_jsv_ptr) |ljv| {
                const sync_fn_ty = llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false);
                _ = b.buildCall(sync_fn_ty, rt.sync_locals_to, &.{ locs, ljv, llvm.constInt32(@intCast(tctx.local_count)) }, "");
                const detach_fn_ty = llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType() }, false);
                _ = b.buildCall(detach_fn_ty, rt.var_ref_list_detach, &.{ ctx_p, tctx.var_ref_list_ptr.? }, "");
            }
            // Combined: call + cleanup + return in one runtime function
            const tc_fn_ty = llvm.functionType(tctx.target.jsvalueType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type(), llvm.ptrType(), tctx.target.usizeType(), llvm.ptrType(), tctx.target.usizeType() }, false);
            const ret = b.buildCall(
                tc_fn_ty,
                rt.tail_call_return,
                &.{
                    ctx_p, stk, sp, llvm.constInt32(@intCast(instr.operand.u16)),
                    locs, tctx.target.constUsize(tctx.local_count),
                    if (tctx.arg_shadow_ptr) |asp| blk: {
                        var si = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
                        break :blk b.buildInBoundsGEP(llvm.arrayType(tctx.cvTy(), tctx.func.arg_count), asp, &si, "asp");
                    } else llvm.constNull(llvm.ptrType()),
                    tctx.target.constUsize(if (tctx.has_put_arg) tctx.func.arg_count else 0),
                },
                "ret",
            );
            _ = b.buildRet(ret);
        },
        .tail_call_method => {
            vstackFlush(tctx);
            // Sync locals and detach var_refs before tail call (closures must be detached)
            if (tctx.locals_jsv_ptr) |ljv| {
                const sync_fn_ty = llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false);
                _ = b.buildCall(sync_fn_ty, rt.sync_locals_to, &.{ locs, ljv, llvm.constInt32(@intCast(tctx.local_count)) }, "");
                const detach_fn_ty = llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType() }, false);
                _ = b.buildCall(detach_fn_ty, rt.var_ref_list_detach, &.{ ctx_p, tctx.var_ref_list_ptr.? }, "");
            }
            // Combined: call_method + cleanup + return in one runtime function
            const tc_fn_ty = llvm.functionType(tctx.target.jsvalueType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type(), llvm.ptrType(), tctx.target.usizeType(), llvm.ptrType(), tctx.target.usizeType() }, false);
            const ret = b.buildCall(
                tc_fn_ty,
                rt.tail_call_method_return,
                &.{
                    ctx_p, stk, sp, llvm.constInt32(@intCast(instr.operand.u16)),
                    locs, tctx.target.constUsize(tctx.local_count),
                    if (tctx.arg_shadow_ptr) |asp| blk: {
                        var si = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
                        break :blk b.buildInBoundsGEP(llvm.arrayType(tctx.cvTy(), tctx.func.arg_count), asp, &si, "asp");
                    } else llvm.constNull(llvm.ptrType()),
                    tctx.target.constUsize(if (tctx.has_put_arg) tctx.func.arg_count else 0),
                },
                "ret",
            );
            _ = b.buildRet(ret);
        },
        .call_constructor => emitThinCallOp(tctx, rt.op_call_constructor, instr.operand.u16),

        // ========== Property Access (with error check, flush vstack) ==========
        .get_field => {
            if (tctx.target.is_wasm32) {
                emitThinFieldOp(tctx, rt.op_get_field_ic, instr);
            } else {
                emitVstackGetField(tctx, instr);
            }
        },
        .get_field2 => {
            if (tctx.target.is_wasm32) {
                emitThinFieldOp(tctx, rt.op_get_field2_ic, instr);
            } else {
                emitVstackGetField2(tctx, instr);
            }
            // Detect method patterns: get_field2("name") → ... → call_method(argc)
            const atom_idx = instr.operand.atom;
            const name = getAtomStringStatic(tctx.func, atom_idx);
            if (name) |n| {
                const span = std.mem.span(n);
                if (std.mem.eql(u8, span, "charCodeAt")) {
                    tctx.pending_method = .char_code_at;
                } else if (std.mem.eql(u8, span, "push")) {
                    tctx.pending_method = .array_push;
                } else if (tctx.pending_math_obj) {
                    // Phase 2 of Math detection: get_var("Math") was seen, now check method name
                    if (std.mem.eql(u8, span, "abs")) {
                        tctx.pending_method = .math_abs;
                    } else if (std.mem.eql(u8, span, "sqrt")) {
                        tctx.pending_method = .math_sqrt;
                    } else if (std.mem.eql(u8, span, "floor")) {
                        tctx.pending_method = .math_floor;
                    } else if (std.mem.eql(u8, span, "ceil")) {
                        tctx.pending_method = .math_ceil;
                    } else if (std.mem.eql(u8, span, "round")) {
                        tctx.pending_method = .math_round;
                    }
                }
            }
            tctx.pending_math_obj = false;
        },
        .put_field => {
            emitThinPutFieldOp(tctx, instr);
        },

        // ========== Apply (flush vstack) ==========
        .apply => {
            vstackFlush(tctx);
            const err_code = b.buildCall(
                llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                rt.op_apply, &.{ ctx_p, stk, sp }, "err",
            );
            emitErrorCheck(tctx, err_code);
            arrayCacheInvalidateAll(tctx);
        },

        // ========== Return (flush vstack first) ==========
        .@"return" => {
            vstackFlush(tctx);
            _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{}, false), rt.exit_stack, &.{}, "");
            const ret = b.buildCall(
                llvm.functionType(tctx.target.jsvalueType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), tctx.target.usizeType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), tctx.target.usizeType() }, false),
                rt.returnFromStack,
                &.{
                    ctx_p, stk, sp, locs, tctx.target.constUsize(tctx.local_count),
                    tctx.locals_jsv_ptr orelse llvm.constNull(llvm.ptrType()),
                    tctx.var_ref_list_ptr orelse llvm.constNull(llvm.ptrType()),
                    if (tctx.arg_shadow_ptr) |asp| blk: {
                        var si = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
                        break :blk b.buildInBoundsGEP(llvm.arrayType(tctx.cvTy(), tctx.func.arg_count), asp, &si, "asp");
                    } else llvm.constNull(llvm.ptrType()),
                    tctx.target.constUsize(if (tctx.has_put_arg) tctx.func.arg_count else 0),
                },
                "ret",
            );
            _ = b.buildRet(ret);
        },

        .return_undef => {
            vstackDiscardAll(tctx);
            _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{}, false), rt.exit_stack, &.{}, "");
            const ret = b.buildCall(
                llvm.functionType(tctx.target.jsvalueType(), &.{ llvm.ptrType(), llvm.ptrType(), tctx.target.usizeType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), tctx.target.usizeType() }, false),
                rt.returnUndef,
                &.{
                    ctx_p, locs, tctx.target.constUsize(tctx.local_count),
                    tctx.locals_jsv_ptr orelse llvm.constNull(llvm.ptrType()),
                    tctx.var_ref_list_ptr orelse llvm.constNull(llvm.ptrType()),
                    if (tctx.arg_shadow_ptr) |asp| blk: {
                        var si = [_]llvm.Value{ llvm.constInt32(0), llvm.constInt32(0) };
                        break :blk b.buildInBoundsGEP(llvm.arrayType(tctx.cvTy(), tctx.func.arg_count), asp, &si, "asp");
                    } else llvm.constNull(llvm.ptrType()),
                    tctx.target.constUsize(if (tctx.has_put_arg) tctx.func.arg_count else 0),
                },
                "ret",
            );
            _ = b.buildRet(ret);
        },

        // ========== No-op / Control flow handled by terminator ==========
        .nop, .set_name, .set_home_object => {},

        // close_loc: detach var_refs pointing to this local, then free value + set undefined.
        // This is critical for correct closure capture semantics — without it, closures
        // that capture a closed local may read overwritten/stale values instead of the
        // value at the point where close_loc was executed.
        .close_loc => {
            const idx = instr.operand.loc;
            if (tctx.var_ref_list_ptr != null and tctx.locals_jsv_ptr != null) {
                // Full close_loc: detach var_refs + free value + set undefined
                const close_fn_ty = llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false);
                _ = b.buildCall(close_fn_ty, rt.close_loc, &.{
                    ctx_p, locs,
                    tctx.locals_jsv_ptr.?,
                    tctx.var_ref_list_ptr.?,
                    llvm.constInt32(@intCast(idx)),
                }, "");
                // Invalidate vstack entries referencing this local
                vstackInvalidateLocal(tctx, idx);
            }
            // For non-closure functions (no var_ref_list), close_loc is a nop:
            // no var_refs to detach, and cleanupLocals frees everything at return.
        },

        // TDZ: mark local as uninitialized (required for get_loc_check/put_loc_check)
        // CRITICAL: must free the old value before overwriting, otherwise block-scoped
        // variables (let/const) inside loops leak their value on every iteration.
        .set_loc_uninitialized => {
            const idx = instr.operand.loc;
            vstackInvalidateLocal(tctx, idx);
            const loc_ptr = tctx.localSlot(idx);
            const old_val = tctx.loadCv(loc_ptr, "oldv");
            inlineFreeRef(tctx, old_val);
            _ = b.buildStore(tctx.cvUninitialized(), loc_ptr);
        },
        .if_false, .if_false8, .if_true, .if_true8, .goto, .goto8, .goto16 => {},

        // ========== Throw — flush vstack, delegate to interpreter, then cleanup ==========
        .throw => {
            vstackFlush(tctx);
            // Execute throw opcode via interpreter (sets pending exception)
            emitFallbackOpcode(tctx, instr);
            // After error check, the cont_bb is never reached for throw,
            // but LLVM requires a terminator. Branch to cleanup.
            _ = b.buildBr(tctx.cleanup_bb);
        },

        // ========== for_in / to_propkey — dedicated CV-native handlers (bypass exec_opcode) ==========
        .for_in_start => {
            vstackFlush(tctx);
            const err_code = b.buildCall(
                llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                rt.op_for_in_start, &.{ ctx_p, stk, sp }, "err",
            );
            emitErrorCheck(tctx, err_code);
        },
        .for_in_next => {
            vstackFlush(tctx);
            const err_code = b.buildCall(
                llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                rt.op_for_in_next, &.{ ctx_p, stk, sp }, "err",
            );
            emitErrorCheck(tctx, err_code);
        },
        .to_propkey => {
            vstackFlush(tctx);
            const err_code = b.buildCall(
                llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                rt.op_to_propkey, &.{ ctx_p, stk, sp }, "err",
            );
            emitErrorCheck(tctx, err_code);
        },
        .to_propkey2 => {
            vstackFlush(tctx);
            const err_code = b.buildCall(
                llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                rt.op_to_propkey2, &.{ ctx_p, stk, sp }, "err",
            );
            emitErrorCheck(tctx, err_code);
        },

        // ========== Array operations ==========
        .array_from => {
            vstackFlush(tctx);
            const count: u32 = instr.operand.u16;
            emitErrorCheck(tctx, b.buildCall(
                llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false),
                rt.op_array_from, &.{ ctx_p, stk, sp, llvm.constInt32(@intCast(count)) }, "e",
            ));
        },
        .append => { vstackFlush(tctx); callVoid3(b, rt.op_append, ctx_p, stk, sp); arrayCacheInvalidateAll(tctx); },
        .put_array_el => { vstackFlush(tctx); callVoid3(b, rt.op_put_array_el, ctx_p, stk, sp); arrayCacheInvalidateAll(tctx); },
        .get_array_el => if (tctx.target.is_wasm32) {
            vstackFlush(tctx);
            emitErrorCheck(tctx, b.buildCall(llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false), rt.get_array_el, &.{ ctx_p, stk, sp, locs, llvm.constInt32(0) }, "e"));
        } else emitVstackGetArrayEl(tctx, false),
        .get_array_el2 => if (tctx.target.is_wasm32) {
            vstackFlush(tctx);
            emitErrorCheck(tctx, b.buildCall(llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false), rt.get_array_el2, &.{ ctx_p, stk, sp, locs, llvm.constInt32(0) }, "e"));
        } else emitVstackGetArrayEl(tctx, true),
        .get_length => if (tctx.target.is_wasm32) {
            vstackFlush(tctx);
            emitErrorCheck(tctx, b.buildCall(llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false), rt.get_length, &.{ ctx_p, stk, sp, locs, llvm.constInt32(0) }, "e"));
        } else emitVstackGetLength(tctx),

        // ========== add_loc (vstack: int fast path) ==========
        .add_loc => emitVstackAddLoc(tctx, instr.operand.loc),

        // ========== TDZ (Temporal Dead Zone) checks ==========
        .get_loc_check => emitVstackGetLocCheck(tctx, instr.operand.loc),
        .put_loc_check => emitVstackPutLocCheck(tctx, instr.operand.loc),
        .put_loc_check_init => {
            // First initialization of a TDZ variable — no check needed, just put_loc
            emitVstackPutLoc(tctx, instr.operand.loc);
        },

        // ========== Global variable lookup ==========
        .get_var => {
            vstackFlush(tctx);
            const atom_idx = instr.operand.atom;
            const name = getAtomStringStatic(tctx.func, atom_idx) orelse return CodegenError.UnsupportedOpcode;
            const str_ptr = c.LLVMBuildGlobalStringPtr(b.ref, name, ".gv");
            const err_code = b.buildCall(
                llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                rt.op_get_var, &.{ ctx_p, stk, sp, str_ptr }, "err",
            );
            emitErrorCheck(tctx, err_code);
            // Phase 1 of Math detection: track get_var("Math")
            tctx.pending_math_obj = std.mem.eql(u8, std.mem.span(name), "Math");
        },
        .get_var_undef => {
            vstackFlush(tctx);
            const atom_idx = instr.operand.atom;
            const name = getAtomStringStatic(tctx.func, atom_idx) orelse return CodegenError.UnsupportedOpcode;
            const str_ptr = c.LLVMBuildGlobalStringPtr(b.ref, name, ".gvu");
            const err_code = b.buildCall(
                llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                rt.op_get_var_undef, &.{ ctx_p, stk, sp, str_ptr }, "err",
            );
            emitErrorCheck(tctx, err_code);
            // Phase 1 of Math detection: track get_var_undef("Math")
            tctx.pending_math_obj = std.mem.eql(u8, std.mem.span(name), "Math");
        },

        // ========== Push atom as string value ==========
        .push_atom_value => {
            vstackFlush(tctx);
            const atom_idx = instr.operand.atom;
            const name = getAtomStringStatic(tctx.func, atom_idx) orelse return CodegenError.UnsupportedOpcode;
            const str_ptr = c.LLVMBuildGlobalStringPtr(b.ref, name, ".atom");
            _ = b.buildCall(
                llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                rt.push_atom_value, &.{ ctx_p, stk, sp, str_ptr }, "",
            );
        },

        // ========== Define field (property definition) ==========
        .define_field => {
            vstackFlush(tctx);
            const atom_idx = instr.operand.atom;
            const name = getAtomStringStatic(tctx.func, atom_idx) orelse return CodegenError.UnsupportedOpcode;
            const str_ptr = c.LLVMBuildGlobalStringPtr(b.ref, name, ".df");
            const err_code = b.buildCall(
                llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                rt.define_field, &.{ ctx_p, stk, sp, str_ptr }, "err",
            );
            emitErrorCheck(tctx, err_code);
        },

        // ========== typeof_is_function ==========
        .typeof_is_function => {
            vstackFlush(tctx);
            callVoid3(b, rt.typeof_is_function, ctx_p, stk, sp);
        },

        // ========== Closure creation ==========
        .fclosure8, .fclosure => {
            vstackFlush(tctx);
            const func_idx: u32 = instr.operand.const_idx;
            if (tctx.locals_jsv_ptr) |ljv| {
                // V2: shared var_refs pointing into locals_jsv array
                // Pass arg_shadow (if exists) so is_arg captures read post-put_arg values
                // (e.g., rest parameter array stored via put_arg after rest opcode)
                const null_ptr = llvm.constNull(llvm.ptrType());
                const shadow_ptr = tctx.arg_shadow_ptr orelse null_ptr;
                const err_code = b.buildCall(
                    llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type(), llvm.ptrType(), llvm.i32Type(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false),
                    rt.fclosure_v2,
                    &.{
                        ctx_p, stk, sp, tctx.cpool_param,
                        llvm.constInt32(@intCast(func_idx)),
                        tctx.var_refs_param,
                        locs, llvm.constInt32(@intCast(tctx.local_count)),
                        tctx.argv_param, tctx.argc_param,
                        ljv, tctx.var_ref_list_ptr.?,
                        shadow_ptr, llvm.constInt32(@intCast(tctx.func.arg_count)),
                    },
                    "err",
                );
                emitErrorCheck(tctx, err_code);
            } else {
                // V1 fallback: detached var_refs (snapshot at creation time)
                const err_code = b.buildCall(
                    llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type(), llvm.ptrType(), llvm.i32Type() }, false),
                    rt.fclosure,
                    &.{
                        ctx_p, stk, sp, tctx.cpool_param,
                        llvm.constInt32(@intCast(func_idx)),
                        tctx.var_refs_param,
                        locs, llvm.constInt32(@intCast(tctx.local_count)),
                        tctx.argv_param, tctx.argc_param,
                    },
                    "err",
                );
                emitErrorCheck(tctx, err_code);
            }
        },

        // ========== for...of iterator ops (dedicated runtime, avoids CV↔JSValue round-trip) ==========
        .for_of_start => {
            vstackFlush(tctx);
            if (tctx.for_of_iter_stack_ptr) |isp| {
                const err_code = b.buildCall(
                    llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                    rt.op_for_of_start, &.{ ctx_p, stk, sp, isp, tctx.for_of_depth_ptr.? }, "err",
                );
                emitErrorCheck(tctx, err_code);
            } else return CodegenError.UnsupportedOpcode;
        },
        .for_of_next => {
            vstackFlush(tctx);
            if (tctx.for_of_iter_stack_ptr) |isp| {
                const err_code = b.buildCall(
                    llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                    rt.op_for_of_next, &.{ ctx_p, stk, sp, isp, tctx.for_of_depth_ptr.? }, "err",
                );
                emitErrorCheck(tctx, err_code);
            } else return CodegenError.UnsupportedOpcode;
        },
        .iterator_close => {
            vstackFlush(tctx);
            if (tctx.for_of_iter_stack_ptr) |isp| {
                _ = b.buildCall(
                    llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                    rt.op_iterator_close, &.{ ctx_p, stk, sp, isp, tctx.for_of_depth_ptr.? }, "",
                );
            } else return CodegenError.UnsupportedOpcode;
        },

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
    vstackFlush(tctx);
    const b = tctx.builder;
    const sync_fn_ty = llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false);

    // Sync locals → locals_jsv before call (closures see current values)
    if (tctx.locals_jsv_ptr) |ljv| {
        _ = b.buildCall(sync_fn_ty, tctx.rt.sync_locals_to, &.{ tctx.locals_ptr, ljv, llvm.constInt32(@intCast(tctx.local_count)) }, "");
    }

    const err_code = b.buildCall(
        llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false),
        func, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, llvm.constInt32(@intCast(argc)) }, "err",
    );
    emitErrorCheck(tctx, err_code);

    // Sync locals_jsv → locals after call (parent sees closure modifications)
    if (tctx.locals_jsv_ptr) |ljv| {
        _ = b.buildCall(sync_fn_ty, tctx.rt.sync_locals_from, &.{ tctx.locals_ptr, ljv, llvm.constInt32(@intCast(tctx.local_count)) }, "");
    }

    // Invalidate array caches — callee could have modified any array's internal storage
    arrayCacheInvalidateAll(tctx);

}

/// Create a zeroed poly IC global variable (4-way IC slot).
/// Layout: {shapes[4]ptr, offsets[4]u32, atom u32, count u32} = 56 bytes
/// Matches ICSlot in zig_runtime.zig.
fn createPolyIcGlobal(tctx: *ThinCodegenCtx, ic_idx: u32) llvm.Value {
    const i32t = llvm.i32Type();
    var ic_fields = [_]llvm.Type{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), i32t, i32t, i32t, i32t, i32t, i32t };
    const ic_ty = c.LLVMStructType(&ic_fields, ic_fields.len, 0);
    var ic_name_buf: [64]u8 = undefined;
    const ic_name = std.fmt.bufPrintZ(&ic_name_buf, ".ic_{d}", .{ic_idx}) catch ".ic";
    const ic_global = c.LLVMAddGlobal(tctx.module.ref, ic_ty, ic_name.ptr);
    c.LLVMSetInitializer(ic_global, c.LLVMConstNull(ic_ty));
    c.LLVMSetLinkage(ic_global, c.LLVMInternalLinkage);
    return ic_global;
}

/// Emit get_field_ic / get_field2_ic with error check
fn emitThinFieldOp(tctx: *ThinCodegenCtx, func: llvm.Value, instr: Instruction) void {
    vstackFlush(tctx);
    const b = tctx.builder;
    const atom_idx = instr.operand.atom;

    // Get atom string for this field access
    const name = getAtomStringStatic(tctx.func, atom_idx) orelse {
        std.debug.print("[CODEGEN_WARN] get_field: atom {d} unresolved in '{s}' — opcode SKIPPED!\n", .{ atom_idx, tctx.func.name });
        return;
    };

    // Create global string for field name
    const current_bb = c.LLVMGetInsertBlock(b.ref);
    const current_fn = c.LLVMGetBasicBlockParent(current_bb);
    _ = current_fn;

    var str_label_buf: [64]u8 = undefined;
    const ic_idx = tctx.nextIcSlot();
    const str_label = std.fmt.bufPrintZ(&str_label_buf, ".field_{d}", .{ic_idx}) catch ".field";

    // Create the string constant
    const str_ptr = c.LLVMBuildGlobalStringPtr(b.ref, name, str_label.ptr);

    // IC slot: allocate a zeroed global — poly IC (4-way)
    // Layout: {shapes[4]ptr, offsets[4]u32, atom u32, count u32} = 56 bytes
    // Matches ICSlot in zig_runtime.zig
    const ic_global = createPolyIcGlobal(tctx, ic_idx);

    const err_code = b.buildCall(
        llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
        func, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, ic_global, str_ptr }, "err",
    );
    emitErrorCheck(tctx, err_code);
}

/// Emit put_field with error check
fn emitThinPutFieldOp(tctx: *ThinCodegenCtx, instr: Instruction) void {
    vstackFlush(tctx);
    const b = tctx.builder;
    const atom_idx = instr.operand.atom;
    const name = getAtomStringStatic(tctx.func, atom_idx) orelse {
        std.debug.print("[CODEGEN_WARN] put_field: atom {d} unresolved in '{s}' — opcode SKIPPED!\n", .{ atom_idx, tctx.func.name });
        return;
    };

    var str_label_buf: [64]u8 = undefined;
    const str_label = std.fmt.bufPrintZ(&str_label_buf, ".put_field_{d}", .{tctx.ic_count}) catch ".pf";

    const str_ptr = c.LLVMBuildGlobalStringPtr(b.ref, name, str_label.ptr);
    const err_code = b.buildCall(
        llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
        tctx.rt.op_put_field, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, str_ptr }, "err",
    );
    emitErrorCheck(tctx, err_code);
    // put_field("length") on an array could resize internal storage
    arrayCacheInvalidateAll(tctx);
}

/// Emit fallback: delegate to interpreter for unsupported opcodes.
/// Opcodes handled by js_frozen_exec_opcode in C are safe; others are flagged.
fn emitFallbackOpcode(tctx: *ThinCodegenCtx, instr: Instruction) void {
    vstackFlush(tctx);
    // Track unsafe fallbacks: only opcodes implemented in js_frozen_exec_opcode are safe.
    // If an opcode is NOT in this list, the function falls back to interpreter.
    switch (instr.opcode) {
        .set_proto,
        // exec_opcode-handled opcodes (safe to delegate to C):
        .to_propkey, .to_propkey2, .to_object,
        .instanceof, .in, .delete, .typeof_is_undefined,
        .get_super, .define_array_el, .set_name_computed, .copy_data_properties,
        .put_var_ref_check,
        .rest, .regexp, .push_bigint_i32,
        // check_ctor, init_ctor, special_object need func_obj/new_target — not safe
        .define_method, .define_method_computed,
        .put_ref_value,
        .check_define_var, .define_var, .define_func,
        .get_var, .get_var_undef,
        .put_var, .put_var_init, .put_var_strict,
        .check_var,
        .for_of_start, .for_of_next, .for_in_start, .for_in_next,
        .iterator_close,
        .array_from,
        .get_field, .get_field2, .put_field, .get_array_el, .put_array_el,
        => {}, // Handled by js_frozen_exec_opcode
        else => tctx.has_unsafe_fallback = true,
    }

    const b = tctx.builder;
    const opcode_val = llvm.constInt(llvm.i8Type(), @intFromEnum(instr.opcode), false);
    const operand_val = llvm.constInt32(switch (instr.operand) {
        .none => 0,
        .i8 => |v| @as(i32, v),
        .i16 => |v| @as(i32, v),
        .i32 => |v| v,
        .u8 => |v| @as(i32, @intCast(v)),
        .u16 => |v| @as(i32, @intCast(v)),
        .atom => |v| @as(i32, @bitCast(v)),
        .loc => |v| @as(i32, @intCast(v)),
        .arg => |v| @as(i32, @intCast(v)),
        .var_ref => |v| @as(i32, @intCast(v)),
        .const_idx => |v| @as(i32, @bitCast(v)),
        .label => |v| v,
        .atom_u8 => |v| @as(i32, @bitCast(v.atom)), // atom only; flags in operand2
        else => 0,
    });
    // Second operand for atom_u8 format: the flags byte
    const operand2_val = llvm.constInt32(switch (instr.operand) {
        .atom_u8 => |v| @as(i32, @intCast(v.value)),
        else => 0,
    });

    // Build JS_UNDEFINED for func_obj and new_target
    const jsv_ty = tctx.target.jsvalueType();
    const undef_jsv = if (tctx.target.is_wasm32)
        // NaN-boxed: JS_MKVAL(JS_TAG_UNDEFINED, 0) = (3 << 32)
        llvm.constInt64(@as(u64, 3) << 32)
    else blk: {
        var undef_val = c.LLVMGetUndef(jsv_ty);
        undef_val = c.LLVMBuildInsertValue(b.ref, undef_val, llvm.constInt64(0), 0, "undef0");
        undef_val = c.LLVMBuildInsertValue(b.ref, undef_val, llvm.constInt64(3), 1, "undef1");
        break :blk undef_val;
    };

    // arg_buf: use arg_shadow if available, otherwise null
    const null_ptr = llvm.constNull(llvm.ptrType());
    const arg_buf_val = tctx.arg_shadow_ptr orelse null_ptr;

    // Atom remapping: serialized bytecodes use module-local atom indices which
    // differ from QuickJS's runtime global atom table. For atom-format operands,
    // resolve the atom string at compile time and pass it so the runtime wrapper
    // can call JS_NewAtom to get the correct runtime atom index.
    const atom_name_val: llvm.Value = switch (instr.operand) {
        .atom => |v| blk: {
            const name = getAtomStringStatic(tctx.func, v);
            if (name) |n| {
                break :blk c.LLVMBuildGlobalStringPtr(b.ref, n, ".atom_name");
            }
            break :blk null_ptr;
        },
        .atom_u8 => |v| blk: {
            const name = getAtomStringStatic(tctx.func, v.atom);
            if (name) |n| {
                break :blk c.LLVMBuildGlobalStringPtr(b.ref, n, ".atom_name");
            }
            break :blk null_ptr;
        },
        else => null_ptr,
    };

    const exec_fn_ty = llvm.functionType(llvm.i32Type(), &.{
        llvm.ptrType(), llvm.i8Type(), llvm.i32Type(), llvm.ptrType(), llvm.ptrType(),
        llvm.ptrType(), llvm.i32Type(), llvm.ptrType(), llvm.i32Type(), llvm.ptrType(),
        llvm.i32Type(), llvm.ptrType(), jsv_ty, jsv_ty, llvm.ptrType(), llvm.i32Type(),
        llvm.ptrType(),
    }, false);

    const sync_fn_ty = llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false);

    // Sync locals → locals_jsv before fallback (closures see current values)
    if (tctx.locals_jsv_ptr) |ljv| {
        _ = b.buildCall(sync_fn_ty, tctx.rt.sync_locals_to, &.{ tctx.locals_ptr, ljv, llvm.constInt32(@intCast(tctx.local_count)) }, "");
    }

    const err_code = b.buildCall(
        exec_fn_ty,
        tctx.rt.exec_opcode,
        &.{
            tctx.ctx_param,                                         // ctx
            opcode_val,                                             // op
            operand_val,                                            // operand
            tctx.stack_ptr,                                         // stack
            tctx.sp_ptr,                                            // sp
            tctx.locals_ptr,                                        // locals
            llvm.constInt32(@intCast(tctx.func.var_count)),         // var_count
            tctx.var_refs_param,                                    // var_refs
            tctx.closure_var_count_param,                           // closure_var_count
            tctx.argv_param,                                        // argv
            tctx.argc_param,                                        // argc
            arg_buf_val,                                            // arg_buf
            undef_jsv,                                              // func_obj (JS_UNDEFINED)
            undef_jsv,                                              // new_target (JS_UNDEFINED)
            tctx.cpool_param,                                       // cpool
            operand2_val,                                           // operand2 (flags for atom_u8)
            atom_name_val,                                          // atom_name (for runtime remapping)
        },
        "err",
    );
    emitErrorCheck(tctx, err_code);

    // Sync locals_jsv → locals after fallback (parent sees closure modifications)
    if (tctx.locals_jsv_ptr) |ljv| {
        _ = b.buildCall(sync_fn_ty, tctx.rt.sync_locals_from, &.{ tctx.locals_ptr, ljv, llvm.constInt32(@intCast(tctx.local_count)) }, "");
    }

    // Fallback opcodes could modify any array's internal storage
    arrayCacheInvalidateAll(tctx);
}

/// Emit block terminator for thin codegen (branch/return).
/// For if_true/if_false: pops condition from vstack, flushes remaining entries,
/// then inlines integer/boolean truthiness check before falling back to cv_to_bool.
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

    // Return/tail_call already handled by instruction emitter (they emit ret)
    if (last_op == .@"return" or last_op == .return_undef or last_op == .throw or
        last_op == .tail_call or last_op == .tail_call_method) return;

    // Conditional branches: inline int/bool check, fallback to cv_to_bool runtime
    if (last_op == .if_false or last_op == .if_false8 or last_op == .if_true or last_op == .if_true8) {
        if (successors.len >= 2) {
            const if_false_op = (last_op == .if_false or last_op == .if_false8);
            const true_target = if (if_false_op) successors[1] else successors[0];
            const false_target = if (if_false_op) successors[0] else successors[1];

            if (true_target < llvm_blocks.len and false_target < llvm_blocks.len) {
                // Pop condition from vstack
                const cond = vstackPop(tctx);
                // Flush remaining vstack entries to physical stack
                vstackFlush(tctx);

                _ = block_id_ptr;
                _ = dispatch_bb;

                // Inline integer truthiness check (hot path for loops)
                const is_int = tctx.isIntCv(cond.value);

                const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
                const int_check_bb = llvm.appendBasicBlock(current_fn, "tb_int");
                const bool_check_bb = llvm.appendBasicBlock(current_fn, "tb_bool");
                const slow_check_bb = llvm.appendBasicBlock(current_fn, "tb_slow");
                _ = b.buildCondBr(is_int, int_check_bb, bool_check_bb);

                // Int path: truthy if payload != 0
                b.positionAtEnd(int_check_bb);
                const i32_val = tctx.getIntCv(cond.value);
                const int_truthy = b.buildICmp(c.LLVMIntNE, i32_val, llvm.constInt32(0), "itruthy");
                _ = b.buildCondBr(int_truthy, llvm_blocks[true_target], llvm_blocks[false_target]);

                // Bool path: check if bool CV
                b.positionAtEnd(bool_check_bb);
                const is_bool = tctx.isBoolCv(cond.value);
                const bool_cond_bb = llvm.appendBasicBlock(current_fn, "tb_bcond");
                _ = b.buildCondBr(is_bool, bool_cond_bb, slow_check_bb);

                // Bool condition: extract truthiness
                b.positionAtEnd(bool_cond_bb);
                const bool_truthy = tctx.getBoolTruthy(cond.value);
                _ = b.buildCondBr(bool_truthy, llvm_blocks[true_target], llvm_blocks[false_target]);

                // Slow path: push cond to physical stack, call cv_to_bool runtime
                b.positionAtEnd(slow_check_bb);
                {
                    const v = if (cond.owned) cond.value else inlineDupRef(tctx, cond.value);
                    const sp_val = tctx.loadSp();
                    const slot = tctx.stackSlot(sp_val);
                    _ = b.buildStore(v, slot);
                    const sp_plus1 = tctx.spAdd(sp_val, 1, "sp1");
                    tctx.storeSp(sp_plus1);
                }
                const bool_val = b.buildCall(
                    llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType() }, false),
                    tctx.rt.cv_to_bool,
                    &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr },
                    "bval",
                );
                const is_truthy = b.buildICmp(c.LLVMIntNE, bool_val, llvm.constInt32(0), "truthy");
                _ = b.buildCondBr(is_truthy, llvm_blocks[true_target], llvm_blocks[false_target]);
                return;
            }
        }
        // Fallthrough if successors don't match
        vstackFlush(tctx);
        if (successors.len >= 1 and successors[0] < llvm_blocks.len) {
            _ = b.buildBr(llvm_blocks[successors[0]]);
            return;
        }
    }

    // Flush vstack before unconditional terminators
    vstackFlush(tctx);

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
        vstackDiscardAll(tctx);
        _ = b.buildCall(llvm.functionType(llvm.voidType(), &.{}, false), tctx.rt.exit_stack, &.{}, "");
        const ret = b.buildCall(
            llvm.functionType(tctx.target.jsvalueType(), &.{ llvm.ptrType(), llvm.ptrType(), tctx.target.usizeType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), tctx.target.usizeType() }, false),
            tctx.rt.returnUndef,
            &.{
                tctx.ctx_param, tctx.locals_ptr, tctx.target.constUsize(tctx.local_count),
                llvm.constNull(llvm.ptrType()), llvm.constNull(llvm.ptrType()),
                llvm.constNull(llvm.ptrType()), tctx.target.constUsize(0),
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
            if (name.len > 0 and name[0] != '<') {
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
        // User atom with empty string — log for debugging
        std.debug.print("[ATOM_WARN] User atom {d} (adjusted {d}) has empty string in func '{s}'\n", .{ atom_idx, adjusted_idx, func.name });
    } else {
        std.debug.print("[ATOM_WARN] User atom {d} (adjusted {d}) out of range (atom_strings.len={d}) in func '{s}'\n", .{ atom_idx, adjusted_idx, func.atom_strings.len, func.name });
    }
    return null;
}

// ============================================================================
// VStack-optimized local variable access helpers
// ============================================================================

/// get_loc: load locals[idx], push as borrowed (no dupRef!).
/// The local still holds the reference. DupRef deferred until flush or consumption.
/// Inline get_arg: load argv[idx] (JSValue) → CV, push onto vstack.
/// Bounds check: if idx >= argc, push CV_UNDEFINED.
/// Int fast path: tag==0 → pack as CV int (no dup needed for ints).
fn emitVstackGetArg(tctx: *ThinCodegenCtx, idx: u32) void {
    const b = tctx.builder;
    const i64t = llvm.i64Type();
    const i32t = llvm.i32Type();
    const ptr = llvm.ptrType();

    // Bounds check: idx < argc
    const idx_val = llvm.constInt(i32t, idx, false);
    const in_bounds = b.buildICmp(c.LLVMIntULT, idx_val, tctx.argc_param, "argib");

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const ok_bb = llvm.appendBasicBlock(current_fn, "ga_ok");
    const undef_bb = llvm.appendBasicBlock(current_fn, "ga_undef");
    const merge_bb = llvm.appendBasicBlock(current_fn, "ga_merge");
    _ = b.buildCondBr(in_bounds, ok_bb, undef_bb);

    // In bounds: load argv[idx] (JSValue is 16 bytes: {payload i64, tag i64})
    b.positionAtEnd(ok_bb);
    const byte_offset = @as(i64, @intCast(@as(u64, idx) * 16));
    const argv_addr = b.buildGEP(llvm.i8Type(), tctx.argv_param, &.{llvm.constInt64(byte_offset)}, "avaddr");
    // Load tag at offset 8
    const tag_addr = b.buildGEP(llvm.i8Type(), argv_addr, &.{llvm.constInt64(8)}, "avtaddr");
    const tag = b.buildLoad(i64t, tag_addr, "avtag");
    const is_int = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt64(0), "avint");

    const int_bb = llvm.appendBasicBlock(current_fn, "ga_int");
    const nonint_bb = llvm.appendBasicBlock(current_fn, "ga_ni");
    _ = b.buildCondBr(is_int, int_bb, nonint_bb);

    // Int path: load i32 payload, pack as CV int
    b.positionAtEnd(int_bb);
    const payload = b.buildLoad(i32t, argv_addr, "avpay");
    const int_cv = tctx.makeIntCv(payload);
    const int_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // Non-int path: call jsvalue_to_cv for dup + conversion
    b.positionAtEnd(nonint_bb);
    const ni_cv = b.buildCall(
        llvm.functionType(tctx.cvTy(), &.{ ptr, ptr, i32t }, false),
        tctx.rt.jsvalue_to_cv, &.{ tctx.ctx_param, argv_addr, llvm.constInt(i32t, 0, false) }, "avcv",
    );
    const ni_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // Out of bounds: push CV_UNDEFINED
    b.positionAtEnd(undef_bb);
    const undef_cv = tctx.cvUndefined();
    const undef_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // Merge
    b.positionAtEnd(merge_bb);
    const phi = b.buildPhi(tctx.cvTy(), "gares");
    var phi_vals = [_]llvm.Value{ int_cv, ni_cv, undef_cv };
    var phi_bbs = [_]llvm.BasicBlock{ int_final_bb, ni_final_bb, undef_final_bb };
    llvm.addIncoming(phi, &phi_vals, &phi_bbs);
    vstackPush(tctx, phi, true);
}

/// Inline push_const: load cpool[idx] (JSValue) → CV, push onto vstack.
/// Int fast path: tag==0 → pack as CV int (no dup needed).
/// Non-int: call jsvalue_to_cv for dup + conversion.
fn emitVstackPushConst(tctx: *ThinCodegenCtx, idx: u32) void {
    const b = tctx.builder;
    const i64t = llvm.i64Type();
    const i32t = llvm.i32Type();
    const ptr = llvm.ptrType();

    // cpool is a JSValue* array. Each JSValue is 16 bytes on native: {payload i64, tag i64}
    // GEP to &cpool[idx]: cpool_ptr + idx * 16
    const byte_offset = @as(i64, @intCast(@as(u64, idx) * 16));
    const cpool_addr = b.buildGEP(llvm.i8Type(), tctx.cpool_param, &.{llvm.constInt64(byte_offset)}, "cpaddr");

    // Load tag at offset 8 from cpool entry
    const tag_addr = b.buildGEP(llvm.i8Type(), cpool_addr, &.{llvm.constInt64(8)}, "cptaddr");
    const tag = b.buildLoad(i64t, tag_addr, "cptag");
    const is_int = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt64(0), "cpint");

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const int_bb = llvm.appendBasicBlock(current_fn, "cp_int");
    const nonint_bb = llvm.appendBasicBlock(current_fn, "cp_ni");
    const merge_bb = llvm.appendBasicBlock(current_fn, "cp_merge");
    _ = b.buildCondBr(is_int, int_bb, nonint_bb);

    // Int path: load i32 payload, pack as CV int (no refcount needed)
    b.positionAtEnd(int_bb);
    const payload = b.buildLoad(i32t, cpool_addr, "cppay");
    const int_cv = tctx.makeIntCv(payload);
    const int_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // Non-int path: call jsvalue_to_cv(ctx, &cpool[idx], 0) for dup + conversion
    b.positionAtEnd(nonint_bb);
    const ni_cv = b.buildCall(
        llvm.functionType(tctx.cvTy(), &.{ ptr, ptr, i32t }, false),
        tctx.rt.jsvalue_to_cv, &.{ tctx.ctx_param, cpool_addr, llvm.constInt(i32t, 0, false) }, "cpcv",
    );
    const ni_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // Merge
    b.positionAtEnd(merge_bb);
    const phi = b.buildPhi(tctx.cvTy(), "cpres");
    var phi_vals = [_]llvm.Value{ int_cv, ni_cv };
    var phi_bbs = [_]llvm.BasicBlock{ int_final_bb, ni_final_bb };
    llvm.addIncoming(phi, &phi_vals, &phi_bbs);
    vstackPush(tctx, phi, true);
}

// DEBUG: force specific opcode categories through runtime to isolate inline handler bugs
const FORCE_RUNTIME_LOCALS = false;
const FORCE_RUNTIME_STACK_OPS = false;

fn emitVstackGetLoc(tctx: *ThinCodegenCtx, idx: u32) void {
    if (FORCE_RUNTIME_LOCALS) {
        vstackFlush(tctx);
        const b = tctx.builder;
        _ = b.buildCall(
            llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false),
            tctx.rt.get_loc, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, tctx.locals_ptr, llvm.constInt32(@intCast(idx)) }, "",
        );
        return;
    }
    const loc_ptr = tctx.localSlot(idx);
    const val = tctx.loadCv(loc_ptr, "locv");
    vstackPushLocal(tctx, val, idx);
}

/// put_loc: pop from vstack, store to local, freeRef(old).
/// If owned, stores directly (ownership transfers). If borrowed, dupRef first.
fn emitVstackPutLoc(tctx: *ThinCodegenCtx, idx: u32) void {
    if (FORCE_RUNTIME_LOCALS) {
        vstackFlush(tctx);
        const b = tctx.builder;
        _ = b.buildCall(
            llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false),
            tctx.rt.put_loc, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, tctx.locals_ptr, llvm.constInt32(@intCast(idx)) }, "",
        );
        vstackInvalidateLocal(tctx, idx);
        return;
    }
    const b = tctx.builder;
    const entry = vstackPop(tctx);

    const loc_ptr = tctx.localSlot(idx);
    const old_val = tctx.loadCv(loc_ptr, "oldv");

    if (entry.owned) {
        _ = b.buildStore(entry.value, loc_ptr);
    } else {
        const duped = inlineDupRef(tctx, entry.value);
        _ = b.buildStore(duped, loc_ptr);
    }

    // Invalidate any remaining vstack entries borrowed from this local
    // before freeing the old value (prevents dangling references)
    vstackInvalidateLocal(tctx, idx);
    inlineFreeRef(tctx, old_val);
}

/// set_loc: peek vstack top (don't pop), always dupRef (value stays on vstack AND goes to local).
fn emitVstackSetLoc(tctx: *ThinCodegenCtx, idx: u32) void {
    if (FORCE_RUNTIME_LOCALS) {
        vstackFlush(tctx);
        const b = tctx.builder;
        _ = b.buildCall(
            llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false),
            tctx.rt.set_loc, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, tctx.locals_ptr, llvm.constInt32(@intCast(idx)) }, "",
        );
        vstackInvalidateLocal(tctx, idx);
        return;
    }
    const b = tctx.builder;
    const entry = vstackPeek(tctx);
    const duped = inlineDupRef(tctx, entry.value);

    const loc_ptr = tctx.localSlot(idx);
    const old_val = tctx.loadCv(loc_ptr, "oldv");
    _ = b.buildStore(duped, loc_ptr);

    // Invalidate before freeing old
    vstackInvalidateLocal(tctx, idx);
    inlineFreeRef(tctx, old_val);
}

// ============================================================================
// TDZ (Temporal Dead Zone) check helpers
// ============================================================================

/// get_loc_check: load locals[idx], check for UNINITIALIZED, throw TDZ error if so.
/// Otherwise push to vstack as borrowed (same as get_loc).
fn emitVstackGetLocCheck(tctx: *ThinCodegenCtx, idx: u32) void {
    if (FORCE_RUNTIME_LOCALS) {
        vstackFlush(tctx);
        const b = tctx.builder;
        // Inline TDZ check then runtime get_loc (same as emitAllRuntimeInstruction)
        const loc_ptr = tctx.localSlot(idx);
        const val = tctx.loadCv(loc_ptr, "locv");
        const is_uninit = tctx.isUninitializedCv(val);
        const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
        const tdz_bb = llvm.appendBasicBlock(current_fn, "dtdz_err");
        const ok_bb = llvm.appendBasicBlock(current_fn, "dtdz_ok");
        _ = b.buildCondBr(is_uninit, tdz_bb, ok_bb);
        b.positionAtEnd(tdz_bb);
        _ = b.buildCall(llvm.functionType(llvm.i32Type(), &.{llvm.ptrType()}, false), tctx.rt.throw_tdz, &.{tctx.ctx_param}, "");
        _ = b.buildBr(tctx.cleanup_bb);
        b.positionAtEnd(ok_bb);
        _ = b.buildCall(
            llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false),
            tctx.rt.get_loc, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, tctx.locals_ptr, llvm.constInt32(@intCast(idx)) }, "",
        );
        return;
    }
    const b = tctx.builder;
    const loc_ptr = tctx.localSlot(idx);
    const val = tctx.loadCv(loc_ptr, "locv");

    // Compare with CV_UNINITIALIZED
    const is_uninit = tctx.isUninitializedCv(val);

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const tdz_bb = llvm.appendBasicBlock(current_fn, "tdz_err");
    const ok_bb = llvm.appendBasicBlock(current_fn, "tdz_ok");
    _ = b.buildCondBr(is_uninit, tdz_bb, ok_bb);

    // TDZ error path: throw ReferenceError, branch to cleanup
    b.positionAtEnd(tdz_bb);
    _ = b.buildCall(
        llvm.functionType(llvm.i32Type(), &.{llvm.ptrType()}, false),
        tctx.rt.throw_tdz, &.{tctx.ctx_param}, "",
    );
    _ = b.buildBr(tctx.cleanup_bb);

    // OK path: push value as borrowed (same as get_loc)
    b.positionAtEnd(ok_bb);
    vstackPushLocal(tctx, val, idx);
}

/// put_loc_check: check locals[idx] for UNINITIALIZED before storing.
/// If uninitialized, throw TDZ error. Otherwise, normal put_loc via runtime.
/// Must flush vstack first since the check is runtime-branching.
fn emitVstackPutLocCheck(tctx: *ThinCodegenCtx, idx: u32) void {
    vstackFlush(tctx);
    const b = tctx.builder;
    const loc_ptr = tctx.localSlot(idx);
    const old_val = tctx.loadCv(loc_ptr, "oldv");

    // Compare with CV_UNINITIALIZED
    const is_uninit = tctx.isUninitializedCv(old_val);

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const tdz_bb = llvm.appendBasicBlock(current_fn, "tdz_err");
    const ok_bb = llvm.appendBasicBlock(current_fn, "tdz_ok");
    _ = b.buildCondBr(is_uninit, tdz_bb, ok_bb);

    // TDZ error path: throw and branch to cleanup
    b.positionAtEnd(tdz_bb);
    _ = b.buildCall(
        llvm.functionType(llvm.i32Type(), &.{llvm.ptrType()}, false),
        tctx.rt.throw_tdz, &.{tctx.ctx_param}, "",
    );
    _ = b.buildBr(tctx.cleanup_bb);

    // OK path: call runtime put_loc (vstack already flushed, value is on physical stack)
    b.positionAtEnd(ok_bb);
    _ = b.buildCall(
        llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false),
        tctx.rt.put_loc, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, tctx.locals_ptr, llvm.constInt32(@intCast(idx)) }, "",
    );
}

// ============================================================================
// Inline arithmetic/comparison helpers
// ============================================================================

/// Emit vstack arithmetic (add/sub/mul) with int fast path on SSA values.
/// Pop two entries from vstack, do pure SSA arithmetic if both int.
/// Slow path: write entries to physical stack, call runtime, pop result.
fn emitVstackArith(tctx: *ThinCodegenCtx, comptime op: enum { add, sub, mul }, rt_slow: llvm.Value, needs_ctx: bool) void {
    const b = tctx.builder;
    const rhs = vstackPop(tctx);
    const lhs = vstackPop(tctx);

    // Check both are int
    const a_is_int = tctx.isIntCv(lhs.value);
    const b_is_int = tctx.isIntCv(rhs.value);
    const both_int = b.buildAnd(a_is_int, b_is_int, "bothint");

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const int_bb = llvm.appendBasicBlock(current_fn, "va_int");
    const slow_bb = llvm.appendBasicBlock(current_fn, "va_slow");
    const merge_bb = llvm.appendBasicBlock(current_fn, "va_merge");
    _ = b.buildCondBr(both_int, int_bb, slow_bb);

    // === Int fast path: pure SSA arithmetic, no memory traffic ===
    b.positionAtEnd(int_bb);
    const a_i32 = tctx.getIntCv(lhs.value);
    const b_i32 = tctx.getIntCv(rhs.value);
    const a_i64 = b.buildSExt(a_i32, llvm.i64Type(), "a64");
    const b_i64 = b.buildSExt(b_i32, llvm.i64Type(), "b64");
    const result_i64 = switch (op) {
        .add => b.buildAdd(a_i64, b_i64, "r64"),
        .sub => b.buildSub(a_i64, b_i64, "r64"),
        .mul => b.buildMul(a_i64, b_i64, "r64"),
    };

    // Check if result fits in i32
    const min_i32 = llvm.constInt64(std.math.minInt(i32));
    const max_i32 = llvm.constInt64(std.math.maxInt(i32));
    const ge_min = b.buildICmp(c.LLVMIntSGE, result_i64, min_i32, "gemin");
    const le_max = b.buildICmp(c.LLVMIntSLE, result_i64, max_i32, "lemax");
    const in_range = b.buildAnd(ge_min, le_max, "inrng");

    const int_ok_bb = llvm.appendBasicBlock(current_fn, "va_ok");
    const int_ovf_bb = llvm.appendBasicBlock(current_fn, "va_ovf");
    _ = b.buildCondBr(in_range, int_ok_bb, int_ovf_bb);

    // Int OK: newInt(trunc(result)) — stays as SSA value
    b.positionAtEnd(int_ok_bb);
    const trunc_val = b.buildTrunc(result_i64, llvm.i32Type(), "trunc");
    const int_cv = tctx.makeIntCv(trunc_val);
    _ = b.buildBr(merge_bb);

    // Int overflow: convert to f64 CV
    b.positionAtEnd(int_ovf_bb);
    const f64_val = b.buildSIToFP(result_i64, llvm.doubleType(), "f64v");
    const f64_bits = tctx.makeFloatCv(b.buildBitCast(f64_val, llvm.i64Type(), "f64b"));
    _ = b.buildBr(merge_bb);

    // === Slow path: write operands to physical stack, call runtime, pop result ===
    b.positionAtEnd(slow_bb);
    {
        // Write entries to physical stack with proper ref handling
        const lhs_val = if (!lhs.owned) inlineDupRef(tctx, lhs.value) else lhs.value;
        const rhs_val = if (!rhs.owned) inlineDupRef(tctx, rhs.value) else rhs.value;
        const sp_val = tctx.loadSp();
        const a_slot = tctx.stackSlot(sp_val);
        _ = b.buildStore(lhs_val, a_slot);
        const b_slot_idx = tctx.spAdd(sp_val, 1, "bsi");
        const b_slot = tctx.stackSlot(b_slot_idx);
        _ = b.buildStore(rhs_val, b_slot);
        const sp_plus2 = tctx.spAdd(sp_val, 2, "sp2");
        tctx.storeSp(sp_plus2);
    }
    // Call runtime (consumes the two stack values, pushes result)
    if (needs_ctx) {
        callVoid3(b, rt_slow, tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr);
    } else {
        callVoid2(b, rt_slow, tctx.stack_ptr, tctx.sp_ptr);
    }
    // Pop result from physical stack back to SSA
    const slow_sp = tctx.loadSp();
    const slow_nsp = tctx.spSub(slow_sp, 1, "snsp");
    const slow_slot = tctx.stackSlot(slow_nsp);
    const slow_result = tctx.loadCv(slow_slot, "sres");
    tctx.storeSp(slow_nsp);
    const slow_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // Merge: phi result from int/overflow/slow paths → push to vstack
    b.positionAtEnd(merge_bb);
    const phi = b.buildPhi(tctx.cvTy(), "ares");
    var phi_vals = [_]llvm.Value{ int_cv, f64_bits, slow_result };
    var phi_bbs = [_]llvm.BasicBlock{ int_ok_bb, int_ovf_bb, slow_final_bb };
    llvm.addIncoming(phi, &phi_vals, &phi_bbs);

    vstackPush(tctx, phi, true);
}

/// Emit vstack comparison (lt/lte/gt/gte) with int fast path on SSA values.
/// Int path: icmp + select TRUE/FALSE as SSA value. Slow path: flush to physical stack.
fn emitVstackCmp(tctx: *ThinCodegenCtx, pred: c.LLVMIntPredicate, rt_slow: llvm.Value) void {
    const b = tctx.builder;
    const rhs = vstackPop(tctx);
    const lhs = vstackPop(tctx);

    // Check both are int
    const a_is_int = tctx.isIntCv(lhs.value);
    const b_is_int = tctx.isIntCv(rhs.value);
    const both_int = b.buildAnd(a_is_int, b_is_int, "bothint");

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const int_bb = llvm.appendBasicBlock(current_fn, "vc_int");
    const slow_bb = llvm.appendBasicBlock(current_fn, "vc_slow");
    const merge_bb = llvm.appendBasicBlock(current_fn, "vc_merge");
    _ = b.buildCondBr(both_int, int_bb, slow_bb);

    // === Int fast path: pure SSA comparison ===
    b.positionAtEnd(int_bb);
    const a_i32 = tctx.getIntCv(lhs.value);
    const b_i32 = tctx.getIntCv(rhs.value);
    const cmp_result = b.buildICmp(pred, a_i32, b_i32, "cmp");
    const int_result = b.buildSelect(
        cmp_result,
        tctx.cvTrue(),
        tctx.cvFalse(),
        "cmpv",
    );
    _ = b.buildBr(merge_bb);

    // === Slow path: write operands to physical stack, call runtime, pop result ===
    b.positionAtEnd(slow_bb);
    {
        const lhs_val = if (!lhs.owned) inlineDupRef(tctx, lhs.value) else lhs.value;
        const rhs_val = if (!rhs.owned) inlineDupRef(tctx, rhs.value) else rhs.value;
        const sp_val = tctx.loadSp();
        const a_slot = tctx.stackSlot(sp_val);
        _ = b.buildStore(lhs_val, a_slot);
        const b_slot_idx = tctx.spAdd(sp_val, 1, "bsi");
        const b_slot = tctx.stackSlot(b_slot_idx);
        _ = b.buildStore(rhs_val, b_slot);
        const sp_plus2 = tctx.spAdd(sp_val, 2, "sp2");
        tctx.storeSp(sp_plus2);
    }
    callVoid3(b, rt_slow, tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr);
    // Pop result from physical stack
    const slow_sp = tctx.loadSp();
    const slow_nsp = tctx.spSub(slow_sp, 1, "snsp");
    const slow_slot = tctx.stackSlot(slow_nsp);
    const slow_result = tctx.loadCv(slow_slot, "sres");
    tctx.storeSp(slow_nsp);
    const slow_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // Merge
    b.positionAtEnd(merge_bb);
    const phi = b.buildPhi(tctx.cvTy(), "cres");
    var phi_vals = [_]llvm.Value{ int_result, slow_result };
    var phi_bbs = [_]llvm.BasicBlock{ int_bb, slow_final_bb };
    llvm.addIncoming(phi, &phi_vals, &phi_bbs);

    vstackPush(tctx, phi, true);
}

/// Emit inline strict_eq/strict_neq/eq/neq with int fast path.
/// For two int operands, === and == are identical (no type coercion needed).
/// Slow path calls runtime with ctx (needed for object/string comparison).
fn emitVstackEq(tctx: *ThinCodegenCtx, comptime is_eq: bool, rt_slow: llvm.Value) void {
    const b = tctx.builder;
    const rhs = vstackPop(tctx);
    const lhs = vstackPop(tctx);

    // Check both are int
    const a_is_int = tctx.isIntCv(lhs.value);
    const b_is_int = tctx.isIntCv(rhs.value);
    const both_int = b.buildAnd(a_is_int, b_is_int, "bothint");

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const int_bb = llvm.appendBasicBlock(current_fn, "veq_int");
    const slow_bb = llvm.appendBasicBlock(current_fn, "veq_slow");
    const merge_bb = llvm.appendBasicBlock(current_fn, "veq_merge");
    _ = b.buildCondBr(both_int, int_bb, slow_bb);

    // === Int fast path: pure SSA equality ===
    b.positionAtEnd(int_bb);
    const a_i32 = tctx.getIntCv(lhs.value);
    const b_i32 = tctx.getIntCv(rhs.value);
    const pred: c.LLVMIntPredicate = if (is_eq) c.LLVMIntEQ else c.LLVMIntNE;
    const cmp_result = b.buildICmp(pred, a_i32, b_i32, "eq");
    const int_result = b.buildSelect(
        cmp_result,
        tctx.cvTrue(),
        tctx.cvFalse(),
        "eqv",
    );
    _ = b.buildBr(merge_bb);

    // === Slow path: write operands to physical stack, call runtime (needs ctx) ===
    b.positionAtEnd(slow_bb);
    {
        const lhs_val = if (!lhs.owned) inlineDupRef(tctx, lhs.value) else lhs.value;
        const rhs_val = if (!rhs.owned) inlineDupRef(tctx, rhs.value) else rhs.value;
        const sp_val = tctx.loadSp();
        const a_slot = tctx.stackSlot(sp_val);
        _ = b.buildStore(lhs_val, a_slot);
        const b_slot_idx = tctx.spAdd(sp_val, 1, "bsi");
        const b_slot = tctx.stackSlot(b_slot_idx);
        _ = b.buildStore(rhs_val, b_slot);
        const sp_plus2 = tctx.spAdd(sp_val, 2, "sp2");
        tctx.storeSp(sp_plus2);
    }
    callVoid3(b, rt_slow, tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr);
    // Pop result from physical stack
    const slow_sp = tctx.loadSp();
    const slow_nsp = tctx.spSub(slow_sp, 1, "snsp");
    const slow_slot = tctx.stackSlot(slow_nsp);
    const slow_result = tctx.loadCv(slow_slot, "sres");
    tctx.storeSp(slow_nsp);
    const slow_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // Merge
    b.positionAtEnd(merge_bb);
    const phi = b.buildPhi(tctx.cvTy(), "eqres");
    var phi_vals = [_]llvm.Value{ int_result, slow_result };
    var phi_bbs = [_]llvm.BasicBlock{ int_bb, slow_final_bb };
    llvm.addIncoming(phi, &phi_vals, &phi_bbs);

    vstackPush(tctx, phi, true);
}

/// Emit inline bitwise op with int fast path.
/// JS bitwise ops convert operands to i32, so for int CVs we extract the i32,
/// apply the native op, and pack back as CV int. Result always fits i32.
/// shr (>>>) is unsigned: result is u32, may exceed i32 range → use i64.
fn emitVstackBitwise(tctx: *ThinCodegenCtx, comptime op: enum { @"and", @"or", xor, shl, sar, shr }, rt_slow: llvm.Value) void {
    const b = tctx.builder;
    const rhs = vstackPop(tctx);
    const lhs = vstackPop(tctx);

    // Check both are int
    const a_is_int = tctx.isIntCv(lhs.value);
    const b_is_int = tctx.isIntCv(rhs.value);
    const both_int = b.buildAnd(a_is_int, b_is_int, "bothint");

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const int_bb = llvm.appendBasicBlock(current_fn, "vbw_int");
    const slow_bb = llvm.appendBasicBlock(current_fn, "vbw_slow");
    const merge_bb = llvm.appendBasicBlock(current_fn, "vbw_merge");
    _ = b.buildCondBr(both_int, int_bb, slow_bb);

    // === Int fast path ===
    b.positionAtEnd(int_bb);
    const a_i32 = tctx.getIntCv(lhs.value);
    const b_i32 = tctx.getIntCv(rhs.value);
    const int_result = switch (op) {
        .@"and" => tctx.makeIntCv(b.buildAnd(a_i32, b_i32, "band")),
        .@"or" => tctx.makeIntCv(b.buildOr(a_i32, b_i32, "bor")),
        .xor => tctx.makeIntCv(b.buildXor(a_i32, b_i32, "bxor")),
        .shl => blk: {
            // JS: (a << (b & 31)) — result is i32
            const shift = b.buildAnd(b_i32, llvm.constInt(llvm.i32Type(), 31, false), "shamt");
            break :blk tctx.makeIntCv(b.buildShl(a_i32, shift, "bshl"));
        },
        .sar => blk: {
            // JS: (a >> (b & 31)) — arithmetic right shift, result is i32
            const shift = b.buildAnd(b_i32, llvm.constInt(llvm.i32Type(), 31, false), "shamt");
            break :blk tctx.makeIntCv(b.buildAShr(a_i32, shift, "bsar"));
        },
        .shr => blk: {
            // JS: (a >>> (b & 31)) — unsigned right shift, result is u32 (may not fit i32)
            // Convert to u32, shift, result may be > 0x7fffffff → use float
            const shift = b.buildAnd(b_i32, llvm.constInt(llvm.i32Type(), 31, false), "shamt");
            const result_u32 = b.buildLShr(a_i32, shift, "bshr");
            // Check if result fits in i32 (bit 31 clear)
            const is_neg = b.buildICmp(c.LLVMIntSLT, result_u32, llvm.constInt(llvm.i32Type(), 0, false), "isneg");
            const int_val = tctx.makeIntCv(result_u32);
            // If bit 31 set, need to represent as positive float
            const as_u64 = b.buildZExt(result_u32, llvm.i64Type(), "u64");
            const as_f64 = b.buildUIToFP(as_u64, llvm.doubleType(), "f64");
            const float_cv = tctx.makeFloatCv(b.buildBitCast(as_f64, llvm.i64Type(), "fbits"));
            break :blk b.buildSelect(is_neg, float_cv, int_val, "shrv");
        },
    };
    _ = b.buildBr(merge_bb);

    // === Slow path ===
    b.positionAtEnd(slow_bb);
    {
        const lhs_val = if (!lhs.owned) inlineDupRef(tctx, lhs.value) else lhs.value;
        const rhs_val = if (!rhs.owned) inlineDupRef(tctx, rhs.value) else rhs.value;
        const sp_val = tctx.loadSp();
        const a_slot = tctx.stackSlot(sp_val);
        _ = b.buildStore(lhs_val, a_slot);
        const b_slot_idx = tctx.spAdd(sp_val, 1, "bsi");
        const b_slot = tctx.stackSlot(b_slot_idx);
        _ = b.buildStore(rhs_val, b_slot);
        const sp_plus2 = tctx.spAdd(sp_val, 2, "sp2");
        tctx.storeSp(sp_plus2);
    }
    callVoid3(b, rt_slow, tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr);
    const slow_sp = tctx.loadSp();
    const slow_nsp = tctx.spSub(slow_sp, 1, "snsp");
    const slow_slot = tctx.stackSlot(slow_nsp);
    const slow_result = tctx.loadCv(slow_slot, "sres");
    tctx.storeSp(slow_nsp);
    const slow_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // Merge
    b.positionAtEnd(merge_bb);
    const phi = b.buildPhi(tctx.cvTy(), "bwres");
    var phi_vals = [_]llvm.Value{ int_result, slow_result };
    var phi_bbs = [_]llvm.BasicBlock{ int_bb, slow_final_bb };
    llvm.addIncoming(phi, &phi_vals, &phi_bbs);

    vstackPush(tctx, phi, true);
}

/// Emit vstack div/mod with int fast path on SSA values.
/// div: if both int and result is exact integer (a % b == 0), return int; otherwise float.
/// mod: if both int and b != 0, return a % b as int.
/// Slow path flushes to physical stack and calls runtime.
fn emitVstackDivMod(tctx: *ThinCodegenCtx, comptime is_div: bool, rt_slow: llvm.Value) void {
    const b = tctx.builder;
    const rhs = vstackPop(tctx);
    const lhs = vstackPop(tctx);

    // Check both are int
    const a_is_int = tctx.isIntCv(lhs.value);
    const b_is_int = tctx.isIntCv(rhs.value);
    const both_int = b.buildAnd(a_is_int, b_is_int, "bothint");

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const int_bb = llvm.appendBasicBlock(current_fn, "vdm_int");
    const slow_bb = llvm.appendBasicBlock(current_fn, "vdm_slow");
    const merge_bb = llvm.appendBasicBlock(current_fn, "vdm_merge");
    _ = b.buildCondBr(both_int, int_bb, slow_bb);

    // === Int fast path ===
    b.positionAtEnd(int_bb);
    const a_i32 = tctx.getIntCv(lhs.value);
    const b_i32 = tctx.getIntCv(rhs.value);
    const zero = llvm.constInt(llvm.i32Type(), 0, false);

    // Check divisor != 0
    const not_zero = b.buildICmp(c.LLVMIntNE, b_i32, zero, "nz");
    const int_nz_bb = llvm.appendBasicBlock(current_fn, "vdm_nz");
    _ = b.buildCondBr(not_zero, int_nz_bb, slow_bb);

    b.positionAtEnd(int_nz_bb);
    if (is_div) {
        // div: check if exact (a % b == 0) → return int, else float
        const rem = b.buildSRem(a_i32, b_i32, "rem");
        const is_exact = b.buildICmp(c.LLVMIntEQ, rem, zero, "exact");
        const int_exact_bb = llvm.appendBasicBlock(current_fn, "vdm_exact");
        const float_bb = llvm.appendBasicBlock(current_fn, "vdm_float");
        _ = b.buildCondBr(is_exact, int_exact_bb, float_bb);

        // Exact integer division
        b.positionAtEnd(int_exact_bb);
        const quot = b.buildSDiv(a_i32, b_i32, "quot");
        const int_cv = tctx.makeIntCv(quot);
        _ = b.buildBr(merge_bb);

        // Float division
        b.positionAtEnd(float_bb);
        const a_f64 = b.buildSIToFP(a_i32, llvm.doubleType(), "af");
        const b_f64 = b.buildSIToFP(b_i32, llvm.doubleType(), "bf");
        const div_f64 = b.buildFDiv(a_f64, b_f64, "divf");
        const float_bits = tctx.makeFloatCv(b.buildBitCast(div_f64, llvm.i64Type(), "fbits"));
        _ = b.buildBr(merge_bb);

        // Slow path
        b.positionAtEnd(slow_bb);
        {
            const lhs_val = if (!lhs.owned) inlineDupRef(tctx, lhs.value) else lhs.value;
            const rhs_val = if (!rhs.owned) inlineDupRef(tctx, rhs.value) else rhs.value;
            const sp_val = tctx.loadSp();
            const a_slot = tctx.stackSlot(sp_val);
            _ = b.buildStore(lhs_val, a_slot);
            const b_slot_idx = tctx.spAdd(sp_val, 1, "bsi");
            const b_slot = tctx.stackSlot(b_slot_idx);
            _ = b.buildStore(rhs_val, b_slot);
            const sp_plus2 = tctx.spAdd(sp_val, 2, "sp2");
            tctx.storeSp(sp_plus2);
        }
        callVoid3(b, rt_slow, tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr);
        const slow_sp = tctx.loadSp();
        const slow_nsp = tctx.spSub(slow_sp, 1, "snsp");
        const slow_slot = tctx.stackSlot(slow_nsp);
        const slow_result = tctx.loadCv(slow_slot, "sres");
        tctx.storeSp(slow_nsp);
        const slow_final_bb = c.LLVMGetInsertBlock(b.ref);
        _ = b.buildBr(merge_bb);

        // Merge: phi from exact-int / float / slow paths
        b.positionAtEnd(merge_bb);
        const phi = b.buildPhi(tctx.cvTy(), "dmres");
        var phi_vals = [_]llvm.Value{ int_cv, float_bits, slow_result };
        var phi_bbs = [_]llvm.BasicBlock{ int_exact_bb, float_bb, slow_final_bb };
        llvm.addIncoming(phi, &phi_vals, &phi_bbs);
        vstackPush(tctx, phi, true);
    } else {
        // mod: a % b is always int when both operands are int and b != 0
        const rem = b.buildSRem(a_i32, b_i32, "rem");
        const int_cv = tctx.makeIntCv(rem);
        _ = b.buildBr(merge_bb);

        // Slow path
        b.positionAtEnd(slow_bb);
        {
            const lhs_val = if (!lhs.owned) inlineDupRef(tctx, lhs.value) else lhs.value;
            const rhs_val = if (!rhs.owned) inlineDupRef(tctx, rhs.value) else rhs.value;
            const sp_val = tctx.loadSp();
            const a_slot = tctx.stackSlot(sp_val);
            _ = b.buildStore(lhs_val, a_slot);
            const b_slot_idx = tctx.spAdd(sp_val, 1, "bsi");
            const b_slot = tctx.stackSlot(b_slot_idx);
            _ = b.buildStore(rhs_val, b_slot);
            const sp_plus2 = tctx.spAdd(sp_val, 2, "sp2");
            tctx.storeSp(sp_plus2);
        }
        callVoid3(b, rt_slow, tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr);
        const slow_sp = tctx.loadSp();
        const slow_nsp = tctx.spSub(slow_sp, 1, "snsp");
        const slow_slot = tctx.stackSlot(slow_nsp);
        const slow_result = tctx.loadCv(slow_slot, "sres");
        tctx.storeSp(slow_nsp);
        const slow_final_bb = c.LLVMGetInsertBlock(b.ref);
        _ = b.buildBr(merge_bb);

        // Merge: phi from int / slow paths
        b.positionAtEnd(merge_bb);
        const phi = b.buildPhi(tctx.cvTy(), "dmres");
        var phi_vals = [_]llvm.Value{ int_cv, slow_result };
        var phi_bbs = [_]llvm.BasicBlock{ int_nz_bb, slow_final_bb };
        llvm.addIncoming(phi, &phi_vals, &phi_bbs);
        vstackPush(tctx, phi, true);
    }
}

/// Inline get_var_ref: load closure variable with int fast path.
/// JSVarRef layout: offset 24 = pvalue (*JSValue).
/// Native JSValue layout: offset 0 = payload (i64), offset 8 = tag (i64).
/// Int fast path: tag == 0 (JS_TAG_INT) → extract i32 payload, make CV int (no dupRef needed).
/// Non-int path: call llvm_rt_jsvalue_to_cv for proper dupRef + CV conversion.
fn emitVstackGetVarRef(tctx: *ThinCodegenCtx, idx: u32) void {
    const b = tctx.builder;
    const i64t = llvm.i64Type();
    const i32t = llvm.i32Type();
    const ptr = llvm.ptrType();

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const bounds_ok_bb = llvm.appendBasicBlock(current_fn, "vr_bok");
    const int_bb = llvm.appendBasicBlock(current_fn, "vr_int");
    const nonint_bb = llvm.appendBasicBlock(current_fn, "vr_ni");
    const undef_bb = llvm.appendBasicBlock(current_fn, "vr_undef");
    const merge_bb = llvm.appendBasicBlock(current_fn, "vr_merge");

    // Bounds check: var_refs != null && idx < closure_var_count
    const var_refs = tctx.var_refs_param;
    const null_ptr = llvm.constNull(ptr);
    const not_null = b.buildICmp(c.LLVMIntNE, var_refs, null_ptr, "vrnull");
    const idx_val = llvm.constInt(i32t, idx, false);
    const idx_ok = b.buildICmp(c.LLVMIntULT, idx_val, tctx.closure_var_count_param, "vridx");
    const both_ok = b.buildAnd(not_null, idx_ok, "vrbok");
    _ = b.buildCondBr(both_ok, bounds_ok_bb, undef_bb);

    // Load var_refs[idx] → *JSVarRef
    b.positionAtEnd(bounds_ok_bb);
    const idx_i64 = b.buildZExt(idx_val, i64t, "vri64");
    const var_ref_slot = b.buildGEP(ptr, var_refs, &.{idx_i64}, "vrslot");
    const var_ref = b.buildLoad(ptr, var_ref_slot, "vref");

    // Load pvalue at offset 24 of JSVarRef
    const pvalue_addr = b.buildGEP(llvm.i8Type(), var_ref, &.{llvm.constInt64(24)}, "pvaddr");
    const pvalue = b.buildLoad(ptr, pvalue_addr, "pval");

    // Load JSValue tag at offset 8 from pvalue (native: {payload i64, tag i64})
    const tag_addr = b.buildGEP(llvm.i8Type(), pvalue, &.{llvm.constInt64(8)}, "taddr");
    const tag = b.buildLoad(i64t, tag_addr, "vtag");
    const is_int = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt64(0), "vrint"); // JS_TAG_INT = 0
    _ = b.buildCondBr(is_int, int_bb, nonint_bb);

    // Int fast path: load i32 payload, make CV int (no dupRef for ints)
    b.positionAtEnd(int_bb);
    const payload = b.buildLoad(i32t, pvalue, "vpay");
    const int_cv = tctx.makeIntCv(payload);
    _ = b.buildBr(merge_bb);

    // Non-int path: call llvm_rt_jsvalue_to_cv(ctx, pvalue, 0) for dupRef + conversion
    b.positionAtEnd(nonint_bb);
    const nonint_cv = b.buildCall(
        llvm.functionType(tctx.cvTy(), &.{ ptr, ptr, i32t }, false),
        tctx.rt.jsvalue_to_cv, &.{ tctx.ctx_param, pvalue, llvm.constInt(i32t, 0, false) }, "nicv",
    );
    const nonint_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // Bounds-fail path: return CV_UNDEFINED
    b.positionAtEnd(undef_bb);
    const undef_cv = tctx.cvUndefined();
    _ = b.buildBr(merge_bb);

    // Merge all paths
    b.positionAtEnd(merge_bb);
    const phi = b.buildPhi(tctx.cvTy(), "vrres");
    var phi_vals = [_]llvm.Value{ int_cv, nonint_cv, undef_cv };
    var phi_bbs = [_]llvm.BasicBlock{ int_bb, nonint_final_bb, undef_bb };
    llvm.addIncoming(phi, &phi_vals, &phi_bbs);

    vstackPush(tctx, phi, true);
}

/// Inline get_field with IC (Inline Cache) hit path.
/// On IC hit: load shape from object, compare to cached shape → direct property GEP.
/// On IC miss: fall back to runtime op_get_field_ic (which populates the cache).
///
/// CV object encoding (native x86_64):
///   QNAN|TAG_PTR|addr = 0x7FFD_xxxx_xxxx_xxxx, ptr in lower 48 bits
/// JSObject layout: shape at offset 24, prop base at offset 32
/// IC slot layout: {shape_ptr, offset_u32, atom_u32} (16 bytes)
/// JSProperty layout: 16 bytes each, JSValue at offset 0
fn emitVstackGetField(tctx: *ThinCodegenCtx, instr: Instruction) void {
    const b = tctx.builder;
    const i64t = llvm.i64Type();
    const i32t = llvm.i32Type();
    const ptr = llvm.ptrType();

    // Get atom string for this field access
    const atom_idx = instr.operand.atom;
    const name = getAtomStringStatic(tctx.func, atom_idx) orelse return;

    // Create global string constant for field name
    const ic_idx = tctx.nextIcSlot();
    var str_label_buf: [64]u8 = undefined;
    const str_label = std.fmt.bufPrintZ(&str_label_buf, ".field_{d}", .{ic_idx}) catch ".field";
    const str_ptr = c.LLVMBuildGlobalStringPtr(b.ref, name, str_label.ptr);

    // IC slot: poly IC (4-way), 56 bytes — matches ICSlot in zig_runtime.zig
    const ic_global = createPolyIcGlobal(tctx, ic_idx);

    // Pop obj from vstack
    const obj_entry = vstackPop(tctx);
    const obj_cv = obj_entry.value;

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const shape_bb = llvm.appendBasicBlock(current_fn, "gf_shape");
    // 4 shape check BBs + 1 common hit BB
    var check_bbs: [4]llvm.BasicBlock = undefined;
    for (0..4) |i| {
        var buf: [16]u8 = undefined;
        const lbl = std.fmt.bufPrintZ(&buf, "gf_chk{d}", .{i}) catch "gf_chk";
        check_bbs[i] = llvm.appendBasicBlock(current_fn, lbl);
    }
    const hit_bb = llvm.appendBasicBlock(current_fn, "gf_hit");
    const hit_int_bb = llvm.appendBasicBlock(current_fn, "gf_hint");
    const hit_ni_bb = llvm.appendBasicBlock(current_fn, "gf_hni");
    const miss_bb = llvm.appendBasicBlock(current_fn, "gf_miss");
    const merge_bb = llvm.appendBasicBlock(current_fn, "gf_merge");

    // Check if obj is a CV object
    const is_obj = tctx.isObjectCv(obj_cv);
    _ = b.buildCondBr(is_obj, shape_bb, miss_bb);

    // Extract object pointer and load shape
    b.positionAtEnd(shape_bb);
    const obj_addr = tctx.getCvPtr(obj_cv);
    const obj_ptr = b.buildIntToPtr(obj_addr, ptr, "optr");
    const shape_addr = b.buildGEP(llvm.i8Type(), obj_ptr, &.{llvm.constInt64(24)}, "saddr");
    const shape = b.buildLoad(ptr, shape_addr, "shape");
    _ = b.buildBr(check_bbs[0]);

    // Emit 4 shape checks in cascade: check[i] → hit or check[i+1] (last → miss)
    // IC layout: shapes at byte offsets 0,8,16,24; offsets at byte offsets 32,36,40,44
    const shape_offsets = [4]i64{ 0, 8, 16, 24 };
    const offset_byte_offsets = [4]i64{ 32, 36, 40, 44 };
    var hit_from_bbs: [4]llvm.BasicBlock = undefined;
    var hit_offsets: [4]llvm.Value = undefined;

    for (0..4) |i| {
        b.positionAtEnd(check_bbs[i]);
        // Load shapes[i] from IC global
        const cs_addr = b.buildGEP(llvm.i8Type(), ic_global, &.{llvm.constInt64(shape_offsets[i])}, "csaddr");
        const cached_shape = b.buildLoad(ptr, cs_addr, "cshp");
        // Pre-load offsets[i] for phi in hit_bb (MUST be before terminator)
        const off_addr_i = b.buildGEP(llvm.i8Type(), ic_global, &.{llvm.constInt64(offset_byte_offsets[i])}, "offaddr");
        hit_offsets[i] = b.buildLoad(i32t, off_addr_i, "off");
        // Branch: on shape match → hit; on miss → next check or miss_bb
        const shape_match = b.buildICmp(c.LLVMIntEQ, shape, cached_shape, "smatch");
        const next_bb = if (i < 3) check_bbs[i + 1] else miss_bb;
        _ = b.buildCondBr(shape_match, hit_bb, next_bb);
        hit_from_bbs[i] = check_bbs[i];
    }

    // IC HIT: merge offset from whichever shape matched
    b.positionAtEnd(hit_bb);
    const offset_phi = b.buildPhi(i32t, "offphi");
    llvm.addIncoming(offset_phi, &hit_offsets, &hit_from_bbs);

    // ABA validation: check offset < shape->prop_count (at shape+40) to prevent
    // stale IC offsets from reading beyond valid properties when shape pointers are reused.
    const prop_count_addr = b.buildGEP(llvm.i8Type(), shape, &.{llvm.constInt64(40)}, "pcaddr");
    const prop_count_val = b.buildLoad(i32t, prop_count_addr, "pcount");
    const in_bounds = b.buildICmp(c.LLVMIntULT, offset_phi, prop_count_val, "inbnd");
    const bounds_ok_bb = llvm.appendBasicBlock(current_fn, "gf_bok");
    _ = b.buildCondBr(in_bounds, bounds_ok_bb, miss_bb);

    // Also verify shape->prop[offset].atom matches IC's cached atom (double safety).
    b.positionAtEnd(bounds_ok_bb);
    const ic_atom_addr = b.buildGEP(llvm.i8Type(), ic_global, &.{llvm.constInt64(48)}, "icaaddr");
    const ic_atom = b.buildLoad(i32t, ic_atom_addr, "icatom");
    const off_ext_v = b.buildZExt(offset_phi, i64t, "offextv");
    const shape_prop_byte = b.buildMul(off_ext_v, llvm.constInt64(8), "spbyte");
    const shape_prop_base = b.buildGEP(llvm.i8Type(), shape, &.{llvm.constInt64(64)}, "spbase");
    const shape_prop_ptr = b.buildGEP(llvm.i8Type(), shape_prop_base, &.{shape_prop_byte}, "spptr");
    const shape_atom_addr = b.buildGEP(llvm.i8Type(), shape_prop_ptr, &.{llvm.constInt64(4)}, "saaddr");
    const shape_atom = b.buildLoad(i32t, shape_atom_addr, "satom");
    const atom_ok = b.buildICmp(c.LLVMIntEQ, ic_atom, shape_atom, "atok");
    const hit_ok_bb = llvm.appendBasicBlock(current_fn, "gf_hitok");
    _ = b.buildCondBr(atom_ok, hit_ok_bb, miss_bb);

    // Hit path (atom validated): load property value
    b.positionAtEnd(hit_ok_bb);
    // Load prop_base at offset 32
    const pbase_addr = b.buildGEP(llvm.i8Type(), obj_ptr, &.{llvm.constInt64(32)}, "pbaddr");
    const prop_base = b.buildLoad(ptr, pbase_addr, "pbase");
    // GEP to property: prop_base + offset * 16 (each JSProperty is 16 bytes)
    const off_ext = b.buildZExt(offset_phi, i64t, "offext");
    const byte_off = b.buildMul(off_ext, llvm.constInt64(16), "boff");
    const prop_addr = b.buildGEP(llvm.i8Type(), prop_base, &.{byte_off}, "paddr");
    // Load JSValue tag at prop_addr + 8 (native: {payload i64, tag i64})
    const tag_addr = b.buildGEP(llvm.i8Type(), prop_addr, &.{llvm.constInt64(8)}, "taddr");
    const tag = b.buildLoad(i64t, tag_addr, "ptag");
    const is_int = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt64(0), "pint");
    _ = b.buildCondBr(is_int, hit_int_bb, hit_ni_bb);

    // Hit + int: load i32 payload, make CV int, free obj
    b.positionAtEnd(hit_int_bb);
    const payload = b.buildLoad(i32t, prop_addr, "ppay");
    const int_cv = tctx.makeIntCv(payload);

    if (obj_entry.owned) inlineFreeRef(tctx, obj_cv);
    const int_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // Hit + non-int: call jsvalue_to_cv for dup + conversion, free obj
    b.positionAtEnd(hit_ni_bb);
    const ni_cv = b.buildCall(
        llvm.functionType(tctx.cvTy(), &.{ ptr, ptr, i32t }, false),
        tctx.rt.jsvalue_to_cv, &.{ tctx.ctx_param, prop_addr, llvm.constInt(i32t, 0, false) }, "nicv",
    );
    if (obj_entry.owned) inlineFreeRef(tctx, obj_cv);
    const ni_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // IC MISS: write obj to physical stack, call runtime, read result back
    b.positionAtEnd(miss_bb);
    {
        const obj_to_store = if (!obj_entry.owned) inlineDupRef(tctx, obj_cv) else obj_cv;
        const sp_val = tctx.loadSp();
        const slot = tctx.stackSlot(sp_val);
        _ = b.buildStore(obj_to_store, slot);
        const sp_plus1 = tctx.spAdd(sp_val, 1, "sp1");
        tctx.storeSp(sp_plus1);
    }
    const err_code = b.buildCall(
        llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr, ptr }, false),
        tctx.rt.op_get_field_ic, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, ic_global, str_ptr }, "gferr",
    );
    const is_err = b.buildICmp(c.LLVMIntNE, err_code, llvm.constInt(i32t, 0, false), "gfe");
    const miss_ok_bb = llvm.appendBasicBlock(current_fn, "gf_mok");
    _ = b.buildCondBr(is_err, tctx.cleanup_bb, miss_ok_bb);

    b.positionAtEnd(miss_ok_bb);
    const miss_sp = tctx.loadSp();
    const miss_nsp = tctx.spSub(miss_sp, 1, "mnsp");
    const miss_slot = tctx.stackSlot(miss_nsp);
    const miss_result = tctx.loadCv(miss_slot, "mres");
    tctx.storeSp(miss_nsp);
    const miss_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // Merge all paths
    b.positionAtEnd(merge_bb);
    const phi = b.buildPhi(tctx.cvTy(), "gfres");
    var phi_vals = [_]llvm.Value{ int_cv, ni_cv, miss_result };
    var phi_bbs = [_]llvm.BasicBlock{ int_final_bb, ni_final_bb, miss_final_bb };
    llvm.addIncoming(phi, &phi_vals, &phi_bbs);

    vstackPush(tctx, phi, true);
}

/// Emit inline get_field2 with poly IC (4-way shape cache).
/// Flushes vstack first so obj is on physical stack, then does inline IC check.
/// obj stays at stack[sp-1], result is pushed on top.
/// Used for method call patterns: obj.method(args) where obj stays as `this`.
fn emitVstackGetField2(tctx: *ThinCodegenCtx, instr: Instruction) void {
    const b = tctx.builder;
    const i64t = llvm.i64Type();
    const i32t = llvm.i32Type();
    const ptr = llvm.ptrType();

    // Get atom string for this field access
    const atom_idx = instr.operand.atom;
    const name = getAtomStringStatic(tctx.func, atom_idx) orelse return;

    // Create global string constant for field name
    const ic_idx = tctx.nextIcSlot();
    var str_label_buf: [64]u8 = undefined;
    const str_label = std.fmt.bufPrintZ(&str_label_buf, ".field2_{d}", .{ic_idx}) catch ".field2";
    const str_ptr = c.LLVMBuildGlobalStringPtr(b.ref, name, str_label.ptr);

    // IC slot: poly IC (4-way), 56 bytes — matches ICSlot in zig_runtime.zig
    const ic_global = createPolyIcGlobal(tctx, ic_idx);

    // Flush vstack so obj is on physical stack — avoids hit/miss path inconsistency
    vstackFlush(tctx);

    // Load obj from physical stack[sp-1] (peek, don't pop)
    const sp_val = tctx.loadSp();
    const obj_sp = tctx.spSub(sp_val, 1, "objsp");
    const obj_slot = tctx.stackSlot(obj_sp);
    const obj_cv = tctx.loadCv(obj_slot, "objcv");

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const shape_bb = llvm.appendBasicBlock(current_fn, "gf2_shape");
    var check_bbs: [4]llvm.BasicBlock = undefined;
    for (0..4) |i| {
        var buf: [16]u8 = undefined;
        const lbl = std.fmt.bufPrintZ(&buf, "gf2_chk{d}", .{i}) catch "gf2_chk";
        check_bbs[i] = llvm.appendBasicBlock(current_fn, lbl);
    }
    const hit_bb = llvm.appendBasicBlock(current_fn, "gf2_hit");
    const hit_int_bb = llvm.appendBasicBlock(current_fn, "gf2_hint");
    const hit_ni_bb = llvm.appendBasicBlock(current_fn, "gf2_hni");
    const miss_bb = llvm.appendBasicBlock(current_fn, "gf2_miss");
    const merge_bb = llvm.appendBasicBlock(current_fn, "gf2_merge");

    // Check if obj is a CV object
    const is_obj = tctx.isObjectCv(obj_cv);
    _ = b.buildCondBr(is_obj, shape_bb, miss_bb);

    // Extract object pointer and load shape
    b.positionAtEnd(shape_bb);
    const obj_addr = tctx.getCvPtr(obj_cv);
    const obj_ptr = b.buildIntToPtr(obj_addr, ptr, "o2ptr");
    const shape_addr = b.buildGEP(llvm.i8Type(), obj_ptr, &.{llvm.constInt64(24)}, "s2addr");
    const shape = b.buildLoad(ptr, shape_addr, "shape2");
    _ = b.buildBr(check_bbs[0]);

    // 4-way shape check cascade
    const shape_offsets = [4]i64{ 0, 8, 16, 24 };
    const offset_byte_offsets = [4]i64{ 32, 36, 40, 44 };
    var hit_from_bbs: [4]llvm.BasicBlock = undefined;
    var hit_offsets: [4]llvm.Value = undefined;

    for (0..4) |i| {
        b.positionAtEnd(check_bbs[i]);
        const cs_addr = b.buildGEP(llvm.i8Type(), ic_global, &.{llvm.constInt64(shape_offsets[i])}, "cs2addr");
        const cached_shape = b.buildLoad(ptr, cs_addr, "cs2hp");
        const off_addr_i = b.buildGEP(llvm.i8Type(), ic_global, &.{llvm.constInt64(offset_byte_offsets[i])}, "off2addr");
        hit_offsets[i] = b.buildLoad(i32t, off_addr_i, "off2");
        const shape_match = b.buildICmp(c.LLVMIntEQ, shape, cached_shape, "sm2atch");
        const next_bb = if (i < 3) check_bbs[i + 1] else miss_bb;
        _ = b.buildCondBr(shape_match, hit_bb, next_bb);
        hit_from_bbs[i] = check_bbs[i];
    }

    // IC HIT: merge offset from whichever shape matched
    b.positionAtEnd(hit_bb);
    const offset_phi = b.buildPhi(i32t, "off2phi");
    llvm.addIncoming(offset_phi, &hit_offsets, &hit_from_bbs);

    // ABA validation: bounds check offset < shape->prop_count (at shape+40)
    const prop_count_addr2 = b.buildGEP(llvm.i8Type(), shape, &.{llvm.constInt64(40)}, "pc2addr");
    const prop_count_val2 = b.buildLoad(i32t, prop_count_addr2, "pc2ount");
    const in_bounds2 = b.buildICmp(c.LLVMIntULT, offset_phi, prop_count_val2, "inb2nd");
    const bounds_ok_bb2 = llvm.appendBasicBlock(current_fn, "gf2_bok");
    _ = b.buildCondBr(in_bounds2, bounds_ok_bb2, miss_bb);

    // Also verify shape->prop[offset].atom matches IC's cached atom (double safety).
    b.positionAtEnd(bounds_ok_bb2);
    const ic_atom_addr2 = b.buildGEP(llvm.i8Type(), ic_global, &.{llvm.constInt64(48)}, "ica2addr");
    const ic_atom2 = b.buildLoad(i32t, ic_atom_addr2, "ica2tom");
    const off_ext_v2 = b.buildZExt(offset_phi, i64t, "oev2");
    const sp_byte2 = b.buildMul(off_ext_v2, llvm.constInt64(8), "spb2");
    const sp_base2 = b.buildGEP(llvm.i8Type(), shape, &.{llvm.constInt64(64)}, "spb2ase");
    const sp_ptr2 = b.buildGEP(llvm.i8Type(), sp_base2, &.{sp_byte2}, "spp2tr");
    const sa_addr2 = b.buildGEP(llvm.i8Type(), sp_ptr2, &.{llvm.constInt64(4)}, "saa2dr");
    const sa2 = b.buildLoad(i32t, sa_addr2, "sa2");
    const atom_ok2 = b.buildICmp(c.LLVMIntEQ, ic_atom2, sa2, "atok2");
    const hit_ok_bb2 = llvm.appendBasicBlock(current_fn, "gf2_hitok");
    _ = b.buildCondBr(atom_ok2, hit_ok_bb2, miss_bb);

    // Hit path (atom validated): load property value
    b.positionAtEnd(hit_ok_bb2);
    // Load prop_base at offset 32 and GEP to property
    const pbase_addr = b.buildGEP(llvm.i8Type(), obj_ptr, &.{llvm.constInt64(32)}, "pb2addr");
    const prop_base = b.buildLoad(ptr, pbase_addr, "pbase2");
    const off_ext = b.buildZExt(offset_phi, i64t, "off2ext");
    const byte_off = b.buildMul(off_ext, llvm.constInt64(16), "b2off");
    const prop_addr = b.buildGEP(llvm.i8Type(), prop_base, &.{byte_off}, "p2addr");
    // Load JSValue tag at prop_addr + 8
    const tag_addr = b.buildGEP(llvm.i8Type(), prop_addr, &.{llvm.constInt64(8)}, "t2addr");
    const tag = b.buildLoad(i64t, tag_addr, "p2tag");
    const is_int = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt64(0), "p2int");
    _ = b.buildCondBr(is_int, hit_int_bb, hit_ni_bb);

    // Hit + int: load i32 payload, make CV int
    b.positionAtEnd(hit_int_bb);
    const payload = b.buildLoad(i32t, prop_addr, "p2pay");
    const int_cv = tctx.makeIntCv(payload);
    const int_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // Hit + non-int: call jsvalue_to_cv for dup + conversion
    b.positionAtEnd(hit_ni_bb);
    const ni_cv = b.buildCall(
        llvm.functionType(tctx.cvTy(), &.{ ptr, ptr, i32t }, false),
        tctx.rt.jsvalue_to_cv, &.{ tctx.ctx_param, prop_addr, llvm.constInt(i32t, 0, false) }, "ni2cv",
    );
    const ni_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // IC MISS: call runtime op_get_field2_ic (vstack already flushed, obj on physical stack)
    b.positionAtEnd(miss_bb);
    const err_code = b.buildCall(
        llvm.functionType(i32t, &.{ ptr, ptr, ptr, ptr, ptr }, false),
        tctx.rt.op_get_field2_ic, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, ic_global, str_ptr }, "gf2err",
    );
    const is_err = b.buildICmp(c.LLVMIntNE, err_code, llvm.constInt(i32t, 0, false), "gf2e");
    const miss_ok_bb = llvm.appendBasicBlock(current_fn, "gf2_mok");
    _ = b.buildCondBr(is_err, tctx.cleanup_bb, miss_ok_bb);

    // Miss OK: read result from stack[sp-1] (op_get_field2_ic pushed it), pop back to vstack
    b.positionAtEnd(miss_ok_bb);
    const miss_sp = tctx.loadSp();
    const miss_nsp = tctx.spSub(miss_sp, 1, "m2nsp");
    const miss_slot = tctx.stackSlot(miss_nsp);
    const miss_result = tctx.loadCv(miss_slot, "m2res");
    tctx.storeSp(miss_nsp);
    const miss_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // Merge all paths — push result onto vstack
    b.positionAtEnd(merge_bb);
    const phi = b.buildPhi(tctx.cvTy(), "gf2res");
    var phi_vals = [_]llvm.Value{ int_cv, ni_cv, miss_result };
    var phi_bbs = [_]llvm.BasicBlock{ int_final_bb, ni_final_bb, miss_final_bb };
    llvm.addIncoming(phi, &phi_vals, &phi_bbs);
    vstackPush(tctx, phi, true);
}

/// Emit inline inc_loc/dec_loc with int fast path.
/// Invalidates any vstack entries borrowed from this local.
fn emitInlineIncDecLoc(tctx: *ThinCodegenCtx, idx: u32, comptime is_inc: bool) void {
    // Invalidate before modifying the local in-place
    vstackInvalidateLocal(tctx, idx);

    const b = tctx.builder;

    const loc_ptr = tctx.localSlot(idx);
    const val = tctx.loadCv(loc_ptr, "locv");

    const is_int = tctx.isIntCv(val);

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const int_bb = llvm.appendBasicBlock(current_fn, "idl_int");
    const slow_bb = llvm.appendBasicBlock(current_fn, "idl_slow");
    const merge_bb = llvm.appendBasicBlock(current_fn, "idl_merge");
    _ = b.buildCondBr(is_int, int_bb, slow_bb);

    // === Int fast path ===
    b.positionAtEnd(int_bb);
    const i32_val = tctx.getIntCv(val);
    // Check for overflow: i32 MAX for inc, i32 MIN for dec
    const at_limit = if (is_inc)
        b.buildICmp(c.LLVMIntEQ, i32_val, llvm.constInt32(std.math.maxInt(i32)), "atlim")
    else
        b.buildICmp(c.LLVMIntEQ, i32_val, llvm.constInt32(std.math.minInt(i32)), "atlim");

    const int_ok_bb = llvm.appendBasicBlock(current_fn, "idl_ok");
    _ = b.buildCondBr(at_limit, slow_bb, int_ok_bb);

    b.positionAtEnd(int_ok_bb);
    const new_i32 = if (is_inc)
        b.buildAdd(i32_val, llvm.constInt32(1), "inc")
    else
        b.buildSub(i32_val, llvm.constInt32(1), "dec");
    const new_cv = tctx.makeIntCv(new_i32);
    _ = b.buildStore(new_cv, loc_ptr);
    _ = b.buildBr(merge_bb);

    // === Slow path ===
    b.positionAtEnd(slow_bb);
    const rt_fn = if (is_inc) tctx.rt.inc_loc else tctx.rt.dec_loc;
    _ = b.buildCall(
        llvm.functionType(llvm.voidType(), &.{ llvm.ptrType(), llvm.i32Type() }, false),
        rt_fn, &.{ tctx.locals_ptr, llvm.constInt32(@intCast(idx)) }, "",
    );
    _ = b.buildBr(merge_bb);

    b.positionAtEnd(merge_bb);
}

// ============================================================================
// Fast array access / add_loc inline helpers
// ============================================================================

/// Emit fully-inlined get_array_el with cached JSObject probe.
///
/// When the array comes from a local variable (get_loc), uses a per-local cache
/// (stored in function-level allocas) to avoid re-probing the JSObject on every
/// loop iteration. The cache stores: values_ptr, count, and the array CV for validation.
///
/// Fast path (cached or inline probe → GEP → tag check → pack):
///   - Cached: load values_ptr + count from alloca (2 loads vs 6+ for full probe)
///   - Uncached: full JSObject64 probe (flags@5, class_id@6, values@56, count@64)
///   - Element load: GEP to JSValue[idx], check tag==0 (int) → pack as CV, no refcount
///
/// Slow path: flush to physical stack, call llvm_rt_get_array_el.
fn emitVstackGetArrayEl(tctx: *ThinCodegenCtx, keep_array: bool) void {
    // get_array_el2 is rare (obj[key]++ patterns) — use slow path only
    if (keep_array) {
        vstackFlush(tctx);
        const b = tctx.builder;
        const err_code = b.buildCall(
            llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false),
            tctx.rt.get_array_el2, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, tctx.locals_ptr, llvm.constInt32(@intCast(tctx.func.var_count)) }, "err",
        );
        emitErrorCheck(tctx, err_code);
        return;
    }

    const b = tctx.builder;
    const rt = tctx.rt;
    const i64t = llvm.i64Type();
    const i32t = llvm.i32Type();
    const i16t = llvm.i16Type();
    const i8t = llvm.i8Type();
    const ptr_t = llvm.ptrType();

    // Pop index and array from vstack
    const idx_entry = vstackPop(tctx);
    const arr_entry = vstackPop(tctx);

    // Check if array comes from a local variable (cacheable)
    const arr_local_idx: ?u32 = if (arr_entry.local_idx >= 0) @intCast(arr_entry.local_idx) else null;

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const slow_bb = llvm.appendBasicBlock(current_fn, "gae_slow");
    const merge_bb = llvm.appendBasicBlock(current_fn, "gae_merge");

    // We'll build the fast path with cache support
    const cache_check_bb = if (arr_local_idx != null) llvm.appendBasicBlock(current_fn, "gae_cchk") else null;
    const probe_bb = llvm.appendBasicBlock(current_fn, "gae_probe");
    const idx_check_bb = llvm.appendBasicBlock(current_fn, "gae_idxck");
    const bounds_bb = llvm.appendBasicBlock(current_fn, "gae_bounds");
    const load_bb = llvm.appendBasicBlock(current_fn, "gae_load");
    const int_bb = llvm.appendBasicBlock(current_fn, "gae_int");
    const nonint_bb = llvm.appendBasicBlock(current_fn, "gae_nonint");
    const fast_merge_bb = llvm.appendBasicBlock(current_fn, "gae_fmerge");

    // --- Step 1: Check arr_cv is object ---
    const is_obj = tctx.isObjectCv(arr_entry.value);
    const first_target = if (cache_check_bb) |cc| cc else probe_bb;
    _ = b.buildCondBr(is_obj, first_target, slow_bb);

    // --- Step 2 (cached path): Check if cache is valid ---
    // values_ptr and count come from either cache or probe, merged via phi
    var cached_vptr: llvm.Value = undefined;
    var cached_count: llvm.Value = undefined;
    var cached_obj_ptr: llvm.Value = undefined;
    var cache_hit_bb: llvm.BasicBlock = undefined;

    if (arr_local_idx) |local_idx| {
        b.positionAtEnd(cache_check_bb.?);
        const cache = getOrCreateArrayCache(tctx, local_idx);

        // Check: populated == 1 AND cached_cv == arr_cv
        const pop_val = b.buildLoad(i32t, cache.populated_alloca, "cpop");
        const is_pop = b.buildICmp(c.LLVMIntEQ, pop_val, llvm.constInt32(1), "ispop");
        const old_cv = tctx.loadCv(cache.cached_cv_alloca, "ocv");
        const cv_match = tctx.isCvEqual(old_cv, arr_entry.value, "cvmatch");
        const cache_valid = b.buildAnd(is_pop, cv_match, "cvalid");

        // If cache valid → load cached values and go to idx_check
        const cache_load_bb = llvm.appendBasicBlock(current_fn, "gae_cload");
        _ = b.buildCondBr(cache_valid, cache_load_bb, probe_bb);

        b.positionAtEnd(cache_load_bb);
        cached_vptr = b.buildLoad(ptr_t, cache.values_ptr_alloca, "cvptr");
        cached_count = b.buildLoad(i32t, cache.count_alloca, "ccnt");
        // We still need obj_ptr for non-int element fallback — extract it
        cached_obj_ptr = b.buildIntToPtr(tctx.getCvPtr(arr_entry.value), ptr_t, "coptr");
        cache_hit_bb = c.LLVMGetInsertBlock(b.ref);
        _ = b.buildBr(idx_check_bb);
    }

    // --- Step 3: Full JSObject probe (uncached or cache miss) ---
    b.positionAtEnd(probe_bb);
    const obj_ptr = b.buildIntToPtr(tctx.getCvPtr(arr_entry.value), ptr_t, "optr");

    // Load flags byte at offset 5
    const flags_ptr = b.buildInBoundsGEP(i8t, obj_ptr, &.{llvm.constInt64(5)}, "fptr");
    const flags_byte = b.buildLoad(i8t, flags_ptr, "flags");
    const flags_and = b.buildAnd(flags_byte, llvm.constInt(i8t, 0x08, false), "fand");
    const is_fast = b.buildICmp(c.LLVMIntNE, flags_and, llvm.constInt(i8t, 0, false), "isfa");

    // Load class_id (u16) at offset 6
    const cid_ptr = b.buildInBoundsGEP(i8t, obj_ptr, &.{llvm.constInt64(6)}, "cidp");
    const cid = b.buildLoad(i16t, cid_ptr, "cid");
    const is_arr = b.buildICmp(c.LLVMIntEQ, cid, llvm.constInt(i16t, 2, false), "isa");
    const is_args = b.buildICmp(c.LLVMIntEQ, cid, llvm.constInt(i16t, 8, false), "isarg");
    const is_arr_type = b.buildOr(is_arr, is_args, "isat");
    const obj_ok = b.buildAnd(is_fast, is_arr_type, "objok");

    // If probe OK, read values_ptr and count, update cache if applicable
    const probe_ok_bb = llvm.appendBasicBlock(current_fn, "gae_pok");
    _ = b.buildCondBr(obj_ok, probe_ok_bb, slow_bb);

    b.positionAtEnd(probe_ok_bb);
    // Load values pointer (ptr) at offset 56 and count (u32) at offset 64
    const vp_ptr = b.buildInBoundsGEP(i8t, obj_ptr, &.{llvm.constInt64(56)}, "vpp");
    const probed_vptr = b.buildLoad(ptr_t, vp_ptr, "pvptr");
    const cp_ptr = b.buildInBoundsGEP(i8t, obj_ptr, &.{llvm.constInt64(64)}, "cpp");
    const probed_count = b.buildLoad(i32t, cp_ptr, "pcnt");

    // Update cache if array is from a local
    if (arr_local_idx) |local_idx| {
        const cache = getOrCreateArrayCache(tctx, local_idx);
        _ = b.buildStore(probed_vptr, cache.values_ptr_alloca);
        _ = b.buildStore(probed_count, cache.count_alloca);
        _ = b.buildStore(arr_entry.value, cache.cached_cv_alloca);
        _ = b.buildStore(llvm.constInt32(1), cache.populated_alloca);
    }
    const probe_ok_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(idx_check_bb);

    // --- Step 4: Index check (with phi for values_ptr, count, obj_ptr) ---
    b.positionAtEnd(idx_check_bb);

    // Phi nodes merge cached and probed values
    const vptr_phi = b.buildPhi(ptr_t, "vptr");
    const count_phi = b.buildPhi(i32t, "cnt");
    const optr_phi = b.buildPhi(ptr_t, "optr");

    if (arr_local_idx != null) {
        var vptr_vals = [_]llvm.Value{ cached_vptr, probed_vptr };
        var vptr_bbs = [_]llvm.BasicBlock{ cache_hit_bb, probe_ok_final_bb };
        llvm.addIncoming(vptr_phi, &vptr_vals, &vptr_bbs);
        var cnt_vals = [_]llvm.Value{ cached_count, probed_count };
        llvm.addIncoming(count_phi, &cnt_vals, &vptr_bbs);
        var optr_vals = [_]llvm.Value{ cached_obj_ptr, obj_ptr };
        llvm.addIncoming(optr_phi, &optr_vals, &vptr_bbs);
    } else {
        var vptr_vals = [_]llvm.Value{probed_vptr};
        var vptr_bbs = [_]llvm.BasicBlock{probe_ok_final_bb};
        llvm.addIncoming(vptr_phi, &vptr_vals, &vptr_bbs);
        var cnt_vals = [_]llvm.Value{probed_count};
        llvm.addIncoming(count_phi, &cnt_vals, &vptr_bbs);
        var optr_vals = [_]llvm.Value{obj_ptr};
        llvm.addIncoming(optr_phi, &optr_vals, &vptr_bbs);
    }
    // optr_phi available for future use

    const idx_is_int = tctx.isIntCv(idx_entry.value);
    _ = b.buildCondBr(idx_is_int, bounds_bb, slow_bb);

    // --- Step 5: Bounds check ---
    b.positionAtEnd(bounds_bb);
    const idx_i32 = tctx.getIntCv(idx_entry.value);
    const idx_nonneg = b.buildICmp(c.LLVMIntSGE, idx_i32, llvm.constInt32(0), "nonneg");
    const in_bounds = b.buildICmp(c.LLVMIntULT, idx_i32, count_phi, "inb");
    const bounds_ok = b.buildAnd(idx_nonneg, in_bounds, "bok");
    _ = b.buildCondBr(bounds_ok, load_bb, slow_bb);

    // --- Step 6: Inline element load ---
    b.positionAtEnd(load_bb);
    const jsv_struct_ty = jsvalueType();
    const idx_i64 = b.buildSExt(idx_i32, i64t, "idx64");

    const payload_ptr = b.buildInBoundsGEP(jsv_struct_ty, vptr_phi, &.{ idx_i64, llvm.constInt32(0) }, "ppay");
    const tag_ptr = b.buildInBoundsGEP(jsv_struct_ty, vptr_phi, &.{ idx_i64, llvm.constInt32(1) }, "ptag");
    const payload = b.buildLoad(i64t, payload_ptr, "pay");
    const tag = b.buildLoad(i64t, tag_ptr, "etag");

    const is_int_tag = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt64(0), "isitag");
    _ = b.buildCondBr(is_int_tag, int_bb, nonint_bb);

    // --- Step 7a: Integer fast path (ZERO function calls!) ---
    b.positionAtEnd(int_bb);
    const int_cv = tctx.makeIntCv(b.buildTrunc(payload, i32t, "payi32"));
    if (idx_entry.owned) {
        inlineFreeRef(tctx, idx_entry.value);
    }
    if (arr_entry.owned) {
        inlineFreeRef(tctx, arr_entry.value);
    }
    const int_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(fast_merge_bb);

    // --- Step 7b: Non-integer element → call jsvalue_to_cv ---
    b.positionAtEnd(nonint_bb);
    const nonint_cv = b.buildCall(
        llvm.functionType(tctx.cvTy(), &.{ ptr_t, ptr_t, i32t }, false),
        rt.jsvalue_to_cv, &.{ tctx.ctx_param, vptr_phi, idx_i32 }, "nicv",
    );
    if (idx_entry.owned) {
        inlineFreeRef(tctx, idx_entry.value);
    }
    if (arr_entry.owned) {
        inlineFreeRef(tctx, arr_entry.value);
    }
    const nonint_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(fast_merge_bb);

    // --- Fast merge ---
    b.positionAtEnd(fast_merge_bb);
    const fast_phi = b.buildPhi(tctx.cvTy(), "fres");
    var fast_vals = [_]llvm.Value{ int_cv, nonint_cv };
    var fast_bbs = [_]llvm.BasicBlock{ int_final_bb, nonint_final_bb };
    llvm.addIncoming(fast_phi, &fast_vals, &fast_bbs);
    const fast_merge_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // === Slow path: flush both to physical stack, call runtime ===
    b.positionAtEnd(slow_bb);
    {
        const arr_val = if (!arr_entry.owned) inlineDupRef(tctx, arr_entry.value) else arr_entry.value;
        const idx_val = if (!idx_entry.owned) inlineDupRef(tctx, idx_entry.value) else idx_entry.value;
        const sp_val = tctx.loadSp();
        const arr_slot = tctx.stackSlot(sp_val);
        _ = b.buildStore(arr_val, arr_slot);
        const idx_slot_pos = tctx.spAdd(sp_val, 1, "isi");
        const idx_slot = tctx.stackSlot(idx_slot_pos);
        _ = b.buildStore(idx_val, idx_slot);
        const sp_plus2 = tctx.spAdd(sp_val, 2, "sp2");
        tctx.storeSp(sp_plus2);
    }
    const err_code = b.buildCall(
        llvm.functionType(i32t, &.{ ptr_t, ptr_t, ptr_t, ptr_t, i32t }, false),
        rt.get_array_el, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, tctx.locals_ptr, llvm.constInt32(@intCast(tctx.func.var_count)) }, "err",
    );
    emitErrorCheck(tctx, err_code);
    // Pop result from physical stack
    const slow_sp = tctx.loadSp();
    const slow_nsp = tctx.spSub(slow_sp, 1, "snsp");
    const slow_slot = tctx.stackSlot(slow_nsp);
    const slow_result = tctx.loadCv(slow_slot, "sres");
    tctx.storeSp(slow_nsp);
    const slow_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // === Final merge ===
    b.positionAtEnd(merge_bb);
    const phi = b.buildPhi(tctx.cvTy(), "gaeres");
    var phi_vals = [_]llvm.Value{ fast_phi, slow_result };
    var phi_bbs = [_]llvm.BasicBlock{ fast_merge_final_bb, slow_final_bb };
    llvm.addIncoming(phi, &phi_vals, &phi_bbs);

    vstackPush(tctx, phi, true);
}

/// Emit fully-inlined get_length with cached JSObject probe.
/// When the array comes from a local variable, checks the array cache first.
/// Cache hit: load count from alloca (1 load). Cache miss: full JSObject probe.
fn emitVstackGetLength(tctx: *ThinCodegenCtx) void {
    const b = tctx.builder;
    const rt = tctx.rt;
    const i32t = llvm.i32Type();
    const i16t = llvm.i16Type();
    const i8t = llvm.i8Type();
    const ptr_t = llvm.ptrType();

    // Pop array from vstack
    const arr_entry = vstackPop(tctx);
    const arr_local_idx: ?u32 = if (arr_entry.local_idx >= 0) @intCast(arr_entry.local_idx) else null;

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const slow_bb = llvm.appendBasicBlock(current_fn, "gl_slow");
    const merge_bb = llvm.appendBasicBlock(current_fn, "gl_merge");

    const cache_check_bb = if (arr_local_idx != null) llvm.appendBasicBlock(current_fn, "gl_cchk") else null;
    const probe_bb = llvm.appendBasicBlock(current_fn, "gl_probe");
    const fast_bb = llvm.appendBasicBlock(current_fn, "gl_fast");

    // Check arr_cv is object
    const is_obj = tctx.isObjectCv(arr_entry.value);
    const first_target = if (cache_check_bb) |cc| cc else probe_bb;
    _ = b.buildCondBr(is_obj, first_target, slow_bb);

    // Cache check (if array from local)
    var cached_count_val: llvm.Value = undefined;
    var cache_hit_final_bb: llvm.BasicBlock = undefined;

    if (arr_local_idx) |local_idx| {
        b.positionAtEnd(cache_check_bb.?);
        const cache = getOrCreateArrayCache(tctx, local_idx);

        const pop_val = b.buildLoad(i32t, cache.populated_alloca, "cpop");
        const is_pop = b.buildICmp(c.LLVMIntEQ, pop_val, llvm.constInt32(1), "ispop");
        const old_cv = tctx.loadCv(cache.cached_cv_alloca, "ocv");
        const cv_match = tctx.isCvEqual(old_cv, arr_entry.value, "cvmatch");
        const cache_valid = b.buildAnd(is_pop, cv_match, "cvalid");

        const cache_fast_bb = llvm.appendBasicBlock(current_fn, "gl_cfast");
        _ = b.buildCondBr(cache_valid, cache_fast_bb, probe_bb);

        // Cache hit: load count from cache, pack as CV int
        b.positionAtEnd(cache_fast_bb);
        cached_count_val = b.buildLoad(i32t, cache.count_alloca, "ccnt");
        cache_hit_final_bb = c.LLVMGetInsertBlock(b.ref);
        _ = b.buildBr(fast_bb);
    }

    // Full probe (cache miss or no cache)
    b.positionAtEnd(probe_bb);
    const obj_ptr = b.buildIntToPtr(tctx.getCvPtr(arr_entry.value), ptr_t, "optr");

    const flags_ptr = b.buildInBoundsGEP(i8t, obj_ptr, &.{llvm.constInt64(5)}, "fptr");
    const flags_byte = b.buildLoad(i8t, flags_ptr, "flags");
    const flags_and = b.buildAnd(flags_byte, llvm.constInt(i8t, 0x08, false), "fand");
    const is_fast = b.buildICmp(c.LLVMIntNE, flags_and, llvm.constInt(i8t, 0, false), "isfa");

    const cid_ptr = b.buildInBoundsGEP(i8t, obj_ptr, &.{llvm.constInt64(6)}, "cidp");
    const cid = b.buildLoad(i16t, cid_ptr, "cid");
    const is_arr = b.buildICmp(c.LLVMIntEQ, cid, llvm.constInt(i16t, 2, false), "isa");
    const is_args = b.buildICmp(c.LLVMIntEQ, cid, llvm.constInt(i16t, 8, false), "isarg");
    const is_arr_type = b.buildOr(is_arr, is_args, "isat");
    const obj_ok = b.buildAnd(is_fast, is_arr_type, "objok");

    const probe_ok_bb = llvm.appendBasicBlock(current_fn, "gl_pok");
    _ = b.buildCondBr(obj_ok, probe_ok_bb, slow_bb);

    b.positionAtEnd(probe_ok_bb);
    const count_ptr = b.buildInBoundsGEP(i8t, obj_ptr, &.{llvm.constInt64(64)}, "cntp");
    const probed_count = b.buildLoad(i32t, count_ptr, "pcnt");

    // Update cache if array from local
    if (arr_local_idx) |local_idx| {
        const cache = getOrCreateArrayCache(tctx, local_idx);
        // Also cache values_ptr for get_array_el's benefit
        const vp_ptr = b.buildInBoundsGEP(i8t, obj_ptr, &.{llvm.constInt64(56)}, "vpp");
        const probed_vptr = b.buildLoad(ptr_t, vp_ptr, "pvptr");
        _ = b.buildStore(probed_vptr, cache.values_ptr_alloca);
        _ = b.buildStore(probed_count, cache.count_alloca);
        _ = b.buildStore(arr_entry.value, cache.cached_cv_alloca);
        _ = b.buildStore(llvm.constInt32(1), cache.populated_alloca);
    }
    const probe_ok_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(fast_bb);

    // Fast path: phi merge of cached and probed count
    b.positionAtEnd(fast_bb);
    const count_phi = b.buildPhi(i32t, "cnt");
    if (arr_local_idx != null) {
        var cnt_vals = [_]llvm.Value{ cached_count_val, probed_count };
        var cnt_bbs = [_]llvm.BasicBlock{ cache_hit_final_bb, probe_ok_final_bb };
        llvm.addIncoming(count_phi, &cnt_vals, &cnt_bbs);
    } else {
        var cnt_vals = [_]llvm.Value{probed_count};
        var cnt_bbs = [_]llvm.BasicBlock{probe_ok_final_bb};
        llvm.addIncoming(count_phi, &cnt_vals, &cnt_bbs);
    }
    const count_cv = tctx.makeIntCv(count_phi);
    if (arr_entry.owned) {
        inlineFreeRef(tctx, arr_entry.value);
    }
    const fast_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // Slow path
    b.positionAtEnd(slow_bb);
    {
        const arr_val = if (!arr_entry.owned) inlineDupRef(tctx, arr_entry.value) else arr_entry.value;
        const sp_val = tctx.loadSp();
        const arr_slot = tctx.stackSlot(sp_val);
        _ = b.buildStore(arr_val, arr_slot);
        const sp_plus1 = tctx.spAdd(sp_val, 1, "sp1");
        tctx.storeSp(sp_plus1);
    }
    const err_code = b.buildCall(
        llvm.functionType(i32t, &.{ ptr_t, ptr_t, ptr_t, ptr_t, i32t }, false),
        rt.get_length, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, tctx.locals_ptr, llvm.constInt32(@intCast(tctx.func.var_count)) }, "err",
    );
    emitErrorCheck(tctx, err_code);
    const slow_sp = tctx.loadSp();
    const slow_nsp = tctx.spSub(slow_sp, 1, "snsp");
    const slow_slot = tctx.stackSlot(slow_nsp);
    const slow_result = tctx.loadCv(slow_slot, "sres");
    tctx.storeSp(slow_nsp);
    const slow_final_bb = c.LLVMGetInsertBlock(b.ref);
    _ = b.buildBr(merge_bb);

    // Merge
    b.positionAtEnd(merge_bb);
    const phi = b.buildPhi(tctx.cvTy(), "glres");
    var phi_vals = [_]llvm.Value{ count_cv, slow_result };
    var phi_bbs = [_]llvm.BasicBlock{ fast_final_bb, slow_final_bb };
    llvm.addIncoming(phi, &phi_vals, &phi_bbs);

    vstackPush(tctx, phi, true);
}

/// Emit inline add_loc with int fast path.
/// Pops value from vstack, adds to locals[idx].
/// Int fast path: if both old local and value are ints and result fits i32 → inline.
/// Slow path: flush to physical stack, call runtime.
fn emitVstackAddLoc(tctx: *ThinCodegenCtx, idx: u32) void {
    const b = tctx.builder;
    const rt = tctx.rt;

    // Pop value from vstack
    const val_entry = vstackPop(tctx);

    // Invalidate vstack entries borrowed from this local BEFORE modifying it
    // (must happen before the branch since it modifies vstack metadata)
    vstackInvalidateLocal(tctx, idx);

    // Load current local value
    const loc_ptr = tctx.localSlot(idx);
    const old_val = tctx.loadCv(loc_ptr, "oldv");

    // Check if both are int
    const val_is_int = tctx.isIntCv(val_entry.value);
    const old_is_int = tctx.isIntCv(old_val);
    const both_int = b.buildAnd(val_is_int, old_is_int, "bothint");

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));
    const int_bb = llvm.appendBasicBlock(current_fn, "al_int");
    const slow_bb = llvm.appendBasicBlock(current_fn, "al_slow");
    const merge_bb = llvm.appendBasicBlock(current_fn, "al_merge");
    _ = b.buildCondBr(both_int, int_bb, slow_bb);

    // === Int fast path: pure SSA add ===
    b.positionAtEnd(int_bb);
    const a_i32 = tctx.getIntCv(old_val);
    const b_i32 = tctx.getIntCv(val_entry.value);
    const a_i64 = b.buildSExt(a_i32, llvm.i64Type(), "a64");
    const b_i64 = b.buildSExt(b_i32, llvm.i64Type(), "b64");
    const sum_i64 = b.buildAdd(a_i64, b_i64, "sum64");

    // Check if result fits in i32
    const min_i32 = llvm.constInt64(std.math.minInt(i32));
    const max_i32 = llvm.constInt64(std.math.maxInt(i32));
    const ge_min = b.buildICmp(c.LLVMIntSGE, sum_i64, min_i32, "gemin");
    const le_max = b.buildICmp(c.LLVMIntSLE, sum_i64, max_i32, "lemax");
    const in_range = b.buildAnd(ge_min, le_max, "inrng");

    const int_ok_bb = llvm.appendBasicBlock(current_fn, "al_ok");
    const int_ovf_bb = llvm.appendBasicBlock(current_fn, "al_ovf");
    _ = b.buildCondBr(in_range, int_ok_bb, int_ovf_bb);

    // Int OK: store newInt result to local
    b.positionAtEnd(int_ok_bb);
    const trunc_val = b.buildTrunc(sum_i64, llvm.i32Type(), "trunc");
    const new_cv = tctx.makeIntCv(trunc_val);
    _ = b.buildStore(new_cv, loc_ptr);
    _ = b.buildBr(merge_bb);

    // Int overflow: convert to f64 CV
    b.positionAtEnd(int_ovf_bb);
    const f64_val = b.buildSIToFP(sum_i64, llvm.doubleType(), "f64v");
    const f64_cv = tctx.makeFloatCv(b.buildBitCast(f64_val, llvm.i64Type(), "f64b"));
    _ = b.buildStore(f64_cv, loc_ptr);
    _ = b.buildBr(merge_bb);

    // === Slow path: flush value to physical stack, call runtime ===
    b.positionAtEnd(slow_bb);
    {
        const v = if (!val_entry.owned) inlineDupRef(tctx, val_entry.value) else val_entry.value;
        const sp_val = tctx.loadSp();
        const slot = tctx.stackSlot(sp_val);
        _ = b.buildStore(v, slot);
        const sp_plus1 = tctx.spAdd(sp_val, 1, "sp1");
        tctx.storeSp(sp_plus1);
    }
    const err_code = b.buildCall(
        llvm.functionType(llvm.i32Type(), &.{ llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.ptrType(), llvm.i32Type() }, false),
        rt.add_loc, &.{ tctx.ctx_param, tctx.stack_ptr, tctx.sp_ptr, tctx.locals_ptr, llvm.constInt32(@intCast(idx)) }, "err",
    );
    emitErrorCheck(tctx, err_code);
    _ = b.buildBr(merge_bb);

    b.positionAtEnd(merge_bb);
}

// ============================================================================
// Counted loop fast-path specialization
// ============================================================================

/// Emit a specialized fast loop for counted loops with array_sum pattern.
///
/// When the CFG builder detects `for (i = 0; i < arr.length; i++) acc += arr[i]`,
/// this emits a tight native loop that:
/// 1. Validates the array once (is_fast_array check)
/// 2. Validates counter and accumulator are both int
/// 3. Runs a tight loop with only 2 branches per iteration (loop cond + int tag check)
/// 4. On completion → jumps to exit block
/// 5. On failure → falls through to normal opcode-by-opcode header block
fn emitCountedLoopFastPath(
    tctx: *ThinCodegenCtx,
    cl: *const CountedLoop,
    block_id_ptr: llvm.Value,
    dispatch_bb: llvm.BasicBlock,
    _: []llvm.BasicBlock,
) void {
    const b = tctx.builder;
    const i64t = llvm.i64Type();
    const i32t = llvm.i32Type();
    const i16t = llvm.i16Type();
    const i8t = llvm.i8Type();
    const ptr_t = llvm.ptrType();

    const acc_local = cl.accumulator_local orelse return;
    const ctr_local = cl.counter_local;

    const current_fn = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(b.ref));

    // Create all basic blocks for the fast path
    const normal_header_bb = llvm.appendBasicBlock(current_fn, "cl_normal");
    const validate_ctr_bb = llvm.appendBasicBlock(current_fn, "cl_vctr");
    const validate_acc_bb = llvm.appendBasicBlock(current_fn, "cl_vacc");
    const fast_loop_bb = llvm.appendBasicBlock(current_fn, "cl_floop");
    const fast_body_bb = llvm.appendBasicBlock(current_fn, "cl_fbody");
    const fast_add_bb = llvm.appendBasicBlock(current_fn, "cl_fadd");
    const loop_done_bb = llvm.appendBasicBlock(current_fn, "cl_done");
    const loop_bailout_bb = llvm.appendBasicBlock(current_fn, "cl_bail");

    // --- Step 1: Load array object pointer ---
    // Array can come from a local (array_length) or an argument (arg_length)
    var obj_ptr: llvm.Value = undefined;

    if (cl.array_local) |arr_local| {
        // Array from local variable — load CV, check tag, extract pointer
        const arr_ptr = tctx.localSlot(arr_local);
        const arr_cv = tctx.loadCv(arr_ptr, "arrcv");
        const is_obj = tctx.isObjectCv(arr_cv);
        _ = b.buildCondBr(is_obj, validate_ctr_bb, normal_header_bb);

        b.positionAtEnd(validate_ctr_bb);
        obj_ptr = b.buildIntToPtr(tctx.getCvPtr(arr_cv), ptr_t, "optr");
    } else if (cl.bound_type == .arg_length) {
        // Array from function argument — load JSValue from argv[bound_value]
        // Native JSValue: {i64 payload, i64 tag} — 16 bytes
        const jsv_struct_ty = jsvalueType();
        const arg_idx = llvm.constInt64(cl.bound_value);
        const pay_gep = b.buildInBoundsGEP(jsv_struct_ty, tctx.argv_param, &.{ arg_idx, llvm.constInt32(0) }, "apay");
        const tag_gep = b.buildInBoundsGEP(jsv_struct_ty, tctx.argv_param, &.{ arg_idx, llvm.constInt32(1) }, "atag");
        const payload = b.buildLoad(i64t, pay_gep, "apayv");
        const tag = b.buildLoad(i64t, tag_gep, "atagv");

        // Check tag == JS_TAG_OBJECT (-1 on native)
        const is_obj_tag = b.buildICmp(c.LLVMIntEQ, tag, llvm.constInt(i64t, @bitCast(@as(i64, -1)), false), "isobj");
        _ = b.buildCondBr(is_obj_tag, validate_ctr_bb, normal_header_bb);

        b.positionAtEnd(validate_ctr_bb);
        obj_ptr = b.buildIntToPtr(payload, ptr_t, "optr");
    } else {
        // Unsupported bound type — skip fast path
        _ = b.buildBr(normal_header_bb);
        b.positionAtEnd(normal_header_bb);
        return;
    }

    // Load flags byte at offset 5 — bit 3 = fast_array
    const flags_ptr = b.buildInBoundsGEP(i8t, obj_ptr, &.{llvm.constInt64(5)}, "fptr");
    const flags_byte = b.buildLoad(i8t, flags_ptr, "flags");
    const flags_and = b.buildAnd(flags_byte, llvm.constInt(i8t, 0x08, false), "fand");
    const is_fast = b.buildICmp(c.LLVMIntNE, flags_and, llvm.constInt(i8t, 0, false), "isfa");

    // Load class_id (u16) at offset 6
    // Regular arrays: JS_CLASS_ARRAY=2, JS_CLASS_ARGUMENTS=8
    // Typed arrays: JS_CLASS_INT32_ARRAY=27 (raw int32_t elements, 4 bytes each)
    const cid_ptr = b.buildInBoundsGEP(i8t, obj_ptr, &.{llvm.constInt64(6)}, "cidp");
    const cid = b.buildLoad(i16t, cid_ptr, "cid");
    const is_arr = b.buildICmp(c.LLVMIntEQ, cid, llvm.constInt(i16t, 2, false), "isa");
    const is_args = b.buildICmp(c.LLVMIntEQ, cid, llvm.constInt(i16t, 8, false), "isarg");
    const is_regular_arr = b.buildOr(is_arr, is_args, "isreg");
    const is_int32_arr = b.buildICmp(c.LLVMIntEQ, cid, llvm.constInt(i16t, 27, false), "isi32a");
    const is_supported = b.buildOr(is_regular_arr, is_int32_arr, "issup");
    const obj_ok = b.buildAnd(is_fast, is_supported, "objok");

    // Load values/data pointer (ptr) at offset 56 and count (u32) at offset 64
    // For regular arrays: values → JSValue[] (16 bytes per element)
    // For Int32Array: values → int32_t[] (4 bytes per element)
    const vp_ptr = b.buildInBoundsGEP(i8t, obj_ptr, &.{llvm.constInt64(56)}, "vpp");
    const values_ptr = b.buildLoad(ptr_t, vp_ptr, "vptr");
    const cp_ptr = b.buildInBoundsGEP(i8t, obj_ptr, &.{llvm.constInt64(64)}, "cpp");
    const arr_count = b.buildLoad(i32t, cp_ptr, "acnt");

    // Also verify obj check passed
    _ = b.buildCondBr(obj_ok, validate_acc_bb, normal_header_bb);

    // --- Step 2: Validate counter and accumulator are both int ---
    b.positionAtEnd(validate_acc_bb);
    const ctr_ptr = tctx.localSlot(ctr_local);
    const ctr_cv = tctx.loadCv(ctr_ptr, "ctrcv");
    const ctr_is_int = tctx.isIntCv(ctr_cv);
    const acc_ptr_val = tctx.localSlot(acc_local);
    const acc_cv = tctx.loadCv(acc_ptr_val, "acccv");
    const acc_is_int = tctx.isIntCv(acc_cv);
    const both_int = b.buildAnd(ctr_is_int, acc_is_int, "bint");

    // Extract initial counter and accumulator i32 values
    const init_ctr_i32 = tctx.getIntCv(ctr_cv);
    const init_acc_i32 = tctx.getIntCv(acc_cv);
    const init_acc_i64_ext = b.buildSExt(init_acc_i32, i64t, "a64ext");

    // Three-way branch: both_int → (is_int32_arr → typed_loop, else → jsv_loop), else → normal
    const type_check_bb = llvm.appendBasicBlock(current_fn, "cl_tychk");
    _ = b.buildCondBr(both_int, type_check_bb, normal_header_bb);

    b.positionAtEnd(type_check_bb);
    // Create separate loops for typed arrays and regular JSValue arrays
    const typed_loop_bb = llvm.appendBasicBlock(current_fn, "cl_tloop");
    _ = b.buildCondBr(is_int32_arr, typed_loop_bb, fast_loop_bb);

    // ===================================================================
    // Path A: Int32Array typed loop — raw int32_t[], no tag check needed
    // ===================================================================
    b.positionAtEnd(typed_loop_bb);
    const t_ctr_phi = b.buildPhi(i32t, "tctr");
    const t_acc_phi = b.buildPhi(i64t, "tacc");
    {
        var vals = [_]llvm.Value{init_ctr_i32};
        var bbs = [_]llvm.BasicBlock{type_check_bb};
        llvm.addIncoming(t_ctr_phi, &vals, &bbs);
    }
    {
        var vals = [_]llvm.Value{init_acc_i64_ext};
        var bbs = [_]llvm.BasicBlock{type_check_bb};
        llvm.addIncoming(t_acc_phi, &vals, &bbs);
    }
    const t_in_bounds = b.buildICmp(c.LLVMIntSLT, t_ctr_phi, arr_count, "tinb");
    const typed_body_bb = llvm.appendBasicBlock(current_fn, "cl_tbody");
    const typed_done_bb = llvm.appendBasicBlock(current_fn, "cl_tdone");
    _ = b.buildCondBr(t_in_bounds, typed_body_bb, typed_done_bb);

    // Typed loop body: load int32, add — NO per-iteration overflow check!
    // Int32Array elements are guaranteed int32. Accumulating in i64 is safe
    // (i64 can hold sum of ~4 billion int32 values without overflow).
    // Overflow check deferred to loop exit.
    b.positionAtEnd(typed_body_bb);
    const t_ctr_i64 = b.buildSExt(t_ctr_phi, i64t, "tci64");
    const t_elem_ptr = b.buildInBoundsGEP(i32t, values_ptr, &.{t_ctr_i64}, "tep");
    const t_elem_i32 = b.buildLoad(i32t, t_elem_ptr, "tei32");
    const t_elem_i64 = b.buildSExt(t_elem_i32, i64t, "tei64");
    const t_new_acc = b.buildNSWAdd(t_acc_phi, t_elem_i64, "tnacc");
    const t_next_ctr = b.buildNSWAdd(t_ctr_phi, llvm.constInt32(1), "tnctr");
    _ = b.buildBr(typed_loop_bb);

    // Typed loop back-edge phis
    {
        var vals = [_]llvm.Value{t_next_ctr};
        var bbs = [_]llvm.BasicBlock{typed_body_bb};
        llvm.addIncoming(t_ctr_phi, &vals, &bbs);
    }
    {
        var vals = [_]llvm.Value{t_new_acc};
        var bbs = [_]llvm.BasicBlock{typed_body_bb};
        llvm.addIncoming(t_acc_phi, &vals, &bbs);
    }

    // Typed loop done — check if final sum fits i32, store and exit
    b.positionAtEnd(typed_done_bb);
    _ = b.buildStore(tctx.makeIntCv(t_ctr_phi), ctr_ptr);
    // Deferred overflow check: if sum fits i32, store as int CV; else store as f64 CV
    const t_fits_min = b.buildICmp(c.LLVMIntSGE, t_acc_phi, llvm.constInt64(std.math.minInt(i32)), "tfmin");
    const t_fits_max = b.buildICmp(c.LLVMIntSLE, t_acc_phi, llvm.constInt64(std.math.maxInt(i32)), "tfmax");
    const t_fits_i32 = b.buildAnd(t_fits_min, t_fits_max, "tfi32");
    const t_int_done_bb = llvm.appendBasicBlock(current_fn, "cl_tid");
    const t_f64_done_bb = llvm.appendBasicBlock(current_fn, "cl_tf64");
    _ = b.buildCondBr(t_fits_i32, t_int_done_bb, t_f64_done_bb);

    // Fits i32: store as CV int
    b.positionAtEnd(t_int_done_bb);
    _ = b.buildStore(tctx.makeIntCv(b.buildTrunc(t_acc_phi, i32t, "tdai32")), acc_ptr_val);
    _ = b.buildStore(llvm.constInt32(@intCast(cl.exit_block)), block_id_ptr);
    _ = b.buildBr(dispatch_bb);

    // Doesn't fit i32: store as f64 CV
    b.positionAtEnd(t_f64_done_bb);
    const t_f64_val = b.buildSIToFP(t_acc_phi, llvm.doubleType(), "tf64");
    const t_f64_bits = b.buildBitCast(t_f64_val, i64t, "tf64b");
    _ = b.buildStore(tctx.makeFloatCv(t_f64_bits), acc_ptr_val);
    _ = b.buildStore(llvm.constInt32(@intCast(cl.exit_block)), block_id_ptr);
    _ = b.buildBr(dispatch_bb);

    // ===================================================================
    // Path B: Regular JSValue array loop — 16-byte elements with tag check
    // ===================================================================
    b.positionAtEnd(fast_loop_bb);
    const ctr_phi = b.buildPhi(i32t, "ctr");
    const acc_phi = b.buildPhi(i64t, "acc");
    {
        var vals = [_]llvm.Value{init_ctr_i32};
        var bbs = [_]llvm.BasicBlock{type_check_bb};
        llvm.addIncoming(ctr_phi, &vals, &bbs);
    }
    {
        var vals = [_]llvm.Value{init_acc_i64_ext};
        var bbs = [_]llvm.BasicBlock{type_check_bb};
        llvm.addIncoming(acc_phi, &vals, &bbs);
    }

    const ctr_in_bounds = b.buildICmp(c.LLVMIntSLT, ctr_phi, arr_count, "cltcnt");
    _ = b.buildCondBr(ctr_in_bounds, fast_body_bb, loop_done_bb);

    // JSValue loop body: load JSValue, check int tag, add
    b.positionAtEnd(fast_body_bb);
    const jsv_struct_ty = jsvalueType();
    const ctr_i64 = b.buildSExt(ctr_phi, i64t, "ci64");
    const pay_ptr = b.buildInBoundsGEP(jsv_struct_ty, values_ptr, &.{ ctr_i64, llvm.constInt32(0) }, "ppay");
    const tag_ptr = b.buildInBoundsGEP(jsv_struct_ty, values_ptr, &.{ ctr_i64, llvm.constInt32(1) }, "ptag");
    const elem_payload = b.buildLoad(i64t, pay_ptr, "epay");
    const elem_tag = b.buildLoad(i64t, tag_ptr, "etag");
    const elem_is_int = b.buildICmp(c.LLVMIntEQ, elem_tag, llvm.constInt64(0), "eisint");
    _ = b.buildCondBr(elem_is_int, fast_add_bb, loop_bailout_bb);

    // JSValue int add — NO per-iteration overflow check (deferred to loop exit)
    // i64 accumulator safely holds sum of any practical number of i32 values
    b.positionAtEnd(fast_add_bb);
    const elem_i32 = b.buildTrunc(elem_payload, i32t, "ei32");
    const elem_i64 = b.buildSExt(elem_i32, i64t, "ei64");
    const new_acc = b.buildNSWAdd(acc_phi, elem_i64, "nacc");
    const next_ctr = b.buildNSWAdd(ctr_phi, llvm.constInt32(1), "nctr");
    _ = b.buildBr(fast_loop_bb);

    // JSValue loop back-edge phis
    {
        var vals = [_]llvm.Value{next_ctr};
        var bbs = [_]llvm.BasicBlock{fast_add_bb};
        llvm.addIncoming(ctr_phi, &vals, &bbs);
    }
    {
        var vals = [_]llvm.Value{new_acc};
        var bbs = [_]llvm.BasicBlock{fast_add_bb};
        llvm.addIncoming(acc_phi, &vals, &bbs);
    }

    // --- Loop done — deferred overflow check, store results, jump to exit ---
    b.positionAtEnd(loop_done_bb);
    _ = b.buildStore(tctx.makeIntCv(ctr_phi), ctr_ptr);
    const j_fits_min = b.buildICmp(c.LLVMIntSGE, acc_phi, llvm.constInt64(std.math.minInt(i32)), "jfmin");
    const j_fits_max = b.buildICmp(c.LLVMIntSLE, acc_phi, llvm.constInt64(std.math.maxInt(i32)), "jfmax");
    const j_fits_i32 = b.buildAnd(j_fits_min, j_fits_max, "jfi32");
    const j_int_done_bb = llvm.appendBasicBlock(current_fn, "cl_jid");
    const j_f64_done_bb = llvm.appendBasicBlock(current_fn, "cl_jf64");
    _ = b.buildCondBr(j_fits_i32, j_int_done_bb, j_f64_done_bb);

    b.positionAtEnd(j_int_done_bb);
    _ = b.buildStore(tctx.makeIntCv(b.buildTrunc(acc_phi, i32t, "dai32")), acc_ptr_val);
    _ = b.buildStore(llvm.constInt32(@intCast(cl.exit_block)), block_id_ptr);
    _ = b.buildBr(dispatch_bb);

    b.positionAtEnd(j_f64_done_bb);
    const j_f64_val = b.buildSIToFP(acc_phi, llvm.doubleType(), "jf64");
    const j_f64_bits = b.buildBitCast(j_f64_val, i64t, "jf64b");
    _ = b.buildStore(tctx.makeFloatCv(j_f64_bits), acc_ptr_val);
    _ = b.buildStore(llvm.constInt32(@intCast(cl.exit_block)), block_id_ptr);
    _ = b.buildBr(dispatch_bb);

    // --- Bailout (non-int element) — store partial results, fall to normal header ---
    b.positionAtEnd(loop_bailout_bb);
    // Only source: fast_body_bb (JSValue element tag != 0)
    _ = b.buildStore(tctx.makeIntCv(ctr_phi), ctr_ptr);
    // Deferred overflow check for partial accumulator too
    const b_fits_min = b.buildICmp(c.LLVMIntSGE, acc_phi, llvm.constInt64(std.math.minInt(i32)), "bfmin");
    const b_fits_max = b.buildICmp(c.LLVMIntSLE, acc_phi, llvm.constInt64(std.math.maxInt(i32)), "bfmax");
    const b_fits_i32 = b.buildAnd(b_fits_min, b_fits_max, "bfi32");
    const bail_int_bb = llvm.appendBasicBlock(current_fn, "cl_bint");
    const bail_f64_bb = llvm.appendBasicBlock(current_fn, "cl_bf64");
    _ = b.buildCondBr(b_fits_i32, bail_int_bb, bail_f64_bb);

    b.positionAtEnd(bail_int_bb);
    _ = b.buildStore(tctx.makeIntCv(b.buildTrunc(acc_phi, i32t, "bai32")), acc_ptr_val);
    _ = b.buildBr(normal_header_bb);

    b.positionAtEnd(bail_f64_bb);
    const b_f64_val = b.buildSIToFP(acc_phi, llvm.doubleType(), "bf64");
    const b_f64_bits = b.buildBitCast(b_f64_val, i64t, "bf64b");
    _ = b.buildStore(tctx.makeFloatCv(b_f64_bits), acc_ptr_val);
    _ = b.buildBr(normal_header_bb);

    // --- Position builder at normal_header_bb for the regular block codegen ---
    b.positionAtEnd(normal_header_bb);
    // The normal header block instructions will be emitted here by the caller
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

