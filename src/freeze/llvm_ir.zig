//! LLVM IR Builder Wrapper
//!
//! Provides Zig-ergonomic wrappers around the LLVM C API for generating LLVM IR
//! in-memory, verifying modules, and compiling to native object files.
//!
//! Used by llvm_codegen.zig to emit frozen function implementations directly as
//! LLVM IR, skipping the Zig parser/type-checker stage entirely.

const std = @import("std");

pub const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/BitWriter.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/Transforms/PassBuilder.h");
});

// Re-export raw LLVM types for use by codegen
pub const Value = c.LLVMValueRef;
pub const Type = c.LLVMTypeRef;
pub const BasicBlock = c.LLVMBasicBlockRef;

pub const Error = error{
    InitFailed,
    TargetLookupFailed,
    TargetMachineCreateFailed,
    VerifyFailed,
    EmitFailed,
    WriteBitcodeFailed,
};

// ============================================================================
// Context
// ============================================================================

pub const Context = struct {
    ref: c.LLVMContextRef,

    pub fn create() Context {
        return .{ .ref = c.LLVMContextCreate() };
    }

    pub fn dispose(self: Context) void {
        c.LLVMContextDispose(self.ref);
    }
};

// ============================================================================
// Module
// ============================================================================

pub const Module = struct {
    ref: c.LLVMModuleRef,

    pub fn create(name: [*:0]const u8, ctx: Context) Module {
        return .{ .ref = c.LLVMModuleCreateWithNameInContext(name, ctx.ref) };
    }

    pub fn dispose(self: Module) void {
        c.LLVMDisposeModule(self.ref);
    }

    pub fn setTarget(self: Module, triple: [*:0]const u8) void {
        c.LLVMSetTarget(self.ref, triple);
    }

    pub fn setDataLayout(self: Module, layout: [*:0]const u8) void {
        c.LLVMSetDataLayout(self.ref, layout);
    }

    pub fn addFunction(self: Module, name: [*:0]const u8, func_ty: Type) Value {
        return c.LLVMAddFunction(self.ref, name, func_ty);
    }

    pub fn getNamedFunction(self: Module, name: [*:0]const u8) ?Value {
        const f = c.LLVMGetNamedFunction(self.ref, name);
        return if (f) |v| v else null;
    }

    pub fn addGlobal(self: Module, ty: Type, name: [*:0]const u8) Value {
        return c.LLVMAddGlobal(self.ref, ty, name);
    }

    pub fn verify(self: Module) Error!void {
        var err_msg: [*c]u8 = null;
        if (c.LLVMVerifyModule(self.ref, c.LLVMReturnStatusAction, &err_msg) != 0) {
            if (err_msg) |msg| {
                std.debug.print("[llvm] Module verification failed: {s}\n", .{msg});
                c.LLVMDisposeMessage(msg);
            }
            return Error.VerifyFailed;
        }
        if (err_msg) |msg| c.LLVMDisposeMessage(msg);
    }

    pub fn writeBitcodeToFile(self: Module, path: [*:0]const u8) Error!void {
        if (c.LLVMWriteBitcodeToFile(self.ref, path) != 0) {
            return Error.WriteBitcodeFailed;
        }
    }

    pub fn printToString(self: Module) [*:0]u8 {
        return c.LLVMPrintModuleToString(self.ref);
    }
};

// ============================================================================
// Builder
// ============================================================================

pub const Builder = struct {
    ref: c.LLVMBuilderRef,

    pub fn create(ctx: Context) Builder {
        return .{ .ref = c.LLVMCreateBuilderInContext(ctx.ref) };
    }

    pub fn dispose(self: Builder) void {
        c.LLVMDisposeBuilder(self.ref);
    }

    pub fn positionAtEnd(self: Builder, bb: BasicBlock) void {
        c.LLVMPositionBuilderAtEnd(self.ref, bb);
    }

    // Arithmetic
    pub fn buildAdd(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildAdd(self.ref, lhs, rhs, name);
    }

    pub fn buildNSWAdd(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildNSWAdd(self.ref, lhs, rhs, name);
    }

    pub fn buildSub(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildSub(self.ref, lhs, rhs, name);
    }

    pub fn buildMul(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildMul(self.ref, lhs, rhs, name);
    }

    pub fn buildSDiv(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildSDiv(self.ref, lhs, rhs, name);
    }

    pub fn buildSRem(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildSRem(self.ref, lhs, rhs, name);
    }

    pub fn buildNeg(self: Builder, val: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildNeg(self.ref, val, name);
    }

    pub fn buildNot(self: Builder, val: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildNot(self.ref, val, name);
    }

    // Bitwise
    pub fn buildShl(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildShl(self.ref, lhs, rhs, name);
    }

    pub fn buildAShr(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildAShr(self.ref, lhs, rhs, name);
    }

    pub fn buildAnd(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildAnd(self.ref, lhs, rhs, name);
    }

    pub fn buildOr(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildOr(self.ref, lhs, rhs, name);
    }

    pub fn buildXor(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildXor(self.ref, lhs, rhs, name);
    }

    // Comparisons
    pub fn buildICmp(self: Builder, op: c.LLVMIntPredicate, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildICmp(self.ref, op, lhs, rhs, name);
    }

    // Memory
    pub fn buildAlloca(self: Builder, ty: Type, name: [*:0]const u8) Value {
        return c.LLVMBuildAlloca(self.ref, ty, name);
    }

    pub fn buildLoad(self: Builder, ty: Type, ptr: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildLoad2(self.ref, ty, ptr, name);
    }

    pub fn buildStore(self: Builder, val: Value, ptr: Value) Value {
        return c.LLVMBuildStore(self.ref, val, ptr);
    }

    pub fn buildGEP(self: Builder, ty: Type, ptr: Value, indices: []const Value, name: [*:0]const u8) Value {
        return c.LLVMBuildGEP2(self.ref, ty, ptr, @constCast(indices.ptr), @intCast(indices.len), name);
    }

    pub fn buildInBoundsGEP(self: Builder, ty: Type, ptr: Value, indices: []const Value, name: [*:0]const u8) Value {
        return c.LLVMBuildInBoundsGEP2(self.ref, ty, ptr, @constCast(indices.ptr), @intCast(indices.len), name);
    }

    // Control flow
    pub fn buildBr(self: Builder, dest: BasicBlock) Value {
        return c.LLVMBuildBr(self.ref, dest);
    }

    pub fn buildCondBr(self: Builder, cond: Value, then_bb: BasicBlock, else_bb: BasicBlock) Value {
        return c.LLVMBuildCondBr(self.ref, cond, then_bb, else_bb);
    }

    pub fn buildSwitch(self: Builder, val: Value, default_bb: BasicBlock, num_cases: u32) Value {
        return c.LLVMBuildSwitch(self.ref, val, default_bb, num_cases);
    }

    pub fn buildRet(self: Builder, val: Value) Value {
        return c.LLVMBuildRet(self.ref, val);
    }

    pub fn buildRetVoid(self: Builder) Value {
        return c.LLVMBuildRetVoid(self.ref);
    }

    pub fn buildUnreachable(self: Builder) Value {
        return c.LLVMBuildUnreachable(self.ref);
    }

    // Calls
    pub fn buildCall(self: Builder, fn_ty: Type, func: Value, args: []const Value, name: [*:0]const u8) Value {
        return c.LLVMBuildCall2(self.ref, fn_ty, func, @constCast(args.ptr), @intCast(args.len), name);
    }

    // Casts
    pub fn buildZExt(self: Builder, val: Value, dest_ty: Type, name: [*:0]const u8) Value {
        return c.LLVMBuildZExt(self.ref, val, dest_ty, name);
    }

    pub fn buildSExt(self: Builder, val: Value, dest_ty: Type, name: [*:0]const u8) Value {
        return c.LLVMBuildSExt(self.ref, val, dest_ty, name);
    }

    pub fn buildTrunc(self: Builder, val: Value, dest_ty: Type, name: [*:0]const u8) Value {
        return c.LLVMBuildTrunc(self.ref, val, dest_ty, name);
    }

    pub fn buildIntToPtr(self: Builder, val: Value, dest_ty: Type, name: [*:0]const u8) Value {
        return c.LLVMBuildIntToPtr(self.ref, val, dest_ty, name);
    }

    pub fn buildPtrToInt(self: Builder, val: Value, dest_ty: Type, name: [*:0]const u8) Value {
        return c.LLVMBuildPtrToInt(self.ref, val, dest_ty, name);
    }

    pub fn buildBitCast(self: Builder, val: Value, dest_ty: Type, name: [*:0]const u8) Value {
        return c.LLVMBuildBitCast(self.ref, val, dest_ty, name);
    }

    pub fn buildSIToFP(self: Builder, val: Value, dest_ty: Type, name: [*:0]const u8) Value {
        return c.LLVMBuildSIToFP(self.ref, val, dest_ty, name);
    }

    // Phi nodes
    pub fn buildPhi(self: Builder, ty: Type, name: [*:0]const u8) Value {
        return c.LLVMBuildPhi(self.ref, ty, name);
    }

    // Select
    pub fn buildSelect(self: Builder, cond: Value, then_val: Value, else_val: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildSelect(self.ref, cond, then_val, else_val, name);
    }
};

// ============================================================================
// Type helpers
// ============================================================================

pub fn i1Type() Type {
    return c.LLVMInt1Type();
}

pub fn i8Type() Type {
    return c.LLVMInt8Type();
}

pub fn i16Type() Type {
    return c.LLVMInt16Type();
}

pub fn i32Type() Type {
    return c.LLVMInt32Type();
}

pub fn i64Type() Type {
    return c.LLVMInt64Type();
}

pub fn ptrType() Type {
    // LLVM opaque pointer type (since LLVM 15+)
    return c.LLVMPointerType(c.LLVMInt8Type(), 0);
}

pub fn doubleType() Type {
    return c.LLVMDoubleType();
}

pub fn voidType() Type {
    return c.LLVMVoidType();
}

pub fn arrayType(element_ty: Type, count: u32) Type {
    return c.LLVMArrayType(element_ty, count);
}

pub fn functionType(ret: Type, params: []const Type, is_var_arg: bool) Type {
    return c.LLVMFunctionType(ret, @constCast(params.ptr), @intCast(params.len), if (is_var_arg) 1 else 0);
}

// ============================================================================
// Constant helpers
// ============================================================================

pub fn constInt(ty: Type, val: u64, sign_extend: bool) Value {
    return c.LLVMConstInt(ty, val, if (sign_extend) 1 else 0);
}

pub fn constInt32(val: i32) Value {
    return c.LLVMConstInt(i32Type(), @as(u64, @bitCast(@as(i64, val))), 1);
}

pub fn constInt64(val: i64) Value {
    return c.LLVMConstInt(i64Type(), @bitCast(val), 1);
}

pub fn constNull(ty: Type) Value {
    return c.LLVMConstNull(ty);
}

pub fn constString(str: [*:0]const u8, len: u32, null_terminate: bool) Value {
    return c.LLVMConstString(str, len, if (null_terminate) 0 else 1);
}

// ============================================================================
// BasicBlock helpers
// ============================================================================

pub fn appendBasicBlock(func: Value, name: [*:0]const u8) BasicBlock {
    return c.LLVMAppendBasicBlock(func, name);
}

// ============================================================================
// Value helpers
// ============================================================================

pub fn setLinkage(val: Value, linkage: c.LLVMLinkage) void {
    c.LLVMSetLinkage(val, linkage);
}

pub fn setGlobalConstant(val: Value, is_const: bool) void {
    c.LLVMSetGlobalConstant(val, if (is_const) 1 else 0);
}

pub fn setInitializer(global: Value, init: Value) void {
    c.LLVMSetInitializer(global, init);
}

pub fn setFunctionCallConv(func: Value, cc: c.LLVMCallConv) void {
    c.LLVMSetFunctionCallConv(func, cc);
}

pub fn addFunctionAttr(func: Value, attr_kind: [*:0]const u8, ctx: Context) void {
    const kind = c.LLVMGetEnumAttributeKindForName(attr_kind, std.mem.len(attr_kind));
    if (kind != 0) {
        const attr = c.LLVMCreateEnumAttribute(ctx.ref, kind, 0);
        c.LLVMAddAttributeAtIndex(func, c.LLVMAttributeFunctionIndex, attr);
    }
}

pub fn addSwitchCase(switch_val: Value, on_val: Value, dest: BasicBlock) void {
    c.LLVMAddCase(switch_val, on_val, dest);
}

pub fn addIncoming(phi: Value, vals: []Value, blocks: []BasicBlock) void {
    c.LLVMAddIncoming(phi, vals.ptr, blocks.ptr, @intCast(vals.len));
}

pub fn disposeMessage(msg: [*c]u8) void {
    c.LLVMDisposeMessage(msg);
}

// ============================================================================
// Target Machine
// ============================================================================

pub const TargetMachine = struct {
    ref: c.LLVMTargetMachineRef,

    pub fn createWithOptLevel(opt_level: c.LLVMCodeGenOptLevel) Error!TargetMachine {
        _ = c.LLVMInitializeNativeTarget();
        _ = c.LLVMInitializeNativeAsmPrinter();
        _ = c.LLVMInitializeNativeAsmParser();

        const triple = c.LLVMGetDefaultTargetTriple();
        defer c.LLVMDisposeMessage(triple);

        var target: c.LLVMTargetRef = null;
        var err_msg: [*c]u8 = null;
        if (c.LLVMGetTargetFromTriple(triple, &target, &err_msg) != 0) {
            if (err_msg) |msg| {
                std.debug.print("[llvm] Target lookup failed: {s}\n", .{msg});
                c.LLVMDisposeMessage(msg);
            }
            return Error.TargetLookupFailed;
        }

        const tm = c.LLVMCreateTargetMachine(
            target,
            triple,
            "generic",
            "",
            opt_level,
            c.LLVMRelocPIC,
            c.LLVMCodeModelDefault,
        );
        if (tm == null) return Error.TargetMachineCreateFailed;

        return .{ .ref = tm };
    }

    pub fn dispose(self: TargetMachine) void {
        c.LLVMDisposeTargetMachine(self.ref);
    }

    pub fn getTriple(self: TargetMachine) [*c]u8 {
        return c.LLVMGetTargetMachineTriple(self.ref);
    }

    pub fn getDataLayout(self: TargetMachine) [*c]u8 {
        const dl = c.LLVMCreateTargetDataLayout(self.ref);
        defer c.LLVMDisposeTargetData(dl);
        return c.LLVMCopyStringRepOfTargetData(dl);
    }

    /// Compile module to object file
    pub fn emitToFile(self: TargetMachine, module: Module, path: [*:0]const u8) Error!void {
        var err_msg: [*c]u8 = null;
        if (c.LLVMTargetMachineEmitToFile(
            self.ref,
            module.ref,
            @constCast(path),
            c.LLVMObjectFile,
            &err_msg,
        ) != 0) {
            if (err_msg) |msg| {
                std.debug.print("[llvm] Emit to file failed: {s}\n", .{msg});
                c.LLVMDisposeMessage(msg);
            }
            return Error.EmitFailed;
        }
    }
};

// ============================================================================
// Convenience: create module with native target configured
// ============================================================================

pub const NativeModule = struct { ctx: Context, module: Module, tm: TargetMachine };

pub fn createNativeModule(name: [*:0]const u8) Error!NativeModule {
    return createNativeModuleWithOpt(name, c.LLVMCodeGenLevelAggressive);
}

pub fn createNativeModuleWithOpt(name: [*:0]const u8, opt_level: c.LLVMCodeGenOptLevel) Error!NativeModule {
    // Use the global context so that types from i32Type()/i64Type()/etc.
    // (which use LLVMInt32Type() etc. — global context) match the module.
    const ctx = Context{ .ref = c.LLVMGetGlobalContext() };
    const module = Module.create(name, ctx);
    const tm = try TargetMachine.createWithOptLevel(opt_level);

    // Set target triple and data layout
    const triple = tm.getTriple();
    defer c.LLVMDisposeMessage(triple);
    module.setTarget(triple);

    const layout = tm.getDataLayout();
    defer c.LLVMDisposeMessage(layout);
    module.setDataLayout(layout);

    return .{ .ctx = ctx, .module = module, .tm = tm };
}
