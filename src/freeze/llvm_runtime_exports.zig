//! C-ABI exports of zig_runtime thin helpers for LLVM IR codegen.
//!
//! The LLVM IR codegen generates native code that calls these functions directly.
//! Simple operations (push, pop, arithmetic) are inlined in LLVM IR.
//! Complex operations (calls, property access, iterators) delegate here.
//!
//! All functions use callconv(.c) with raw pointer types (no Zig slices/optionals).

const std = @import("std");
const zig_runtime = @import("zig_runtime");
const JSContext = zig_runtime.JSContext;
const JSValue = zig_runtime.JSValue;
const CV = zig_runtime.CompressedValue;
const ICSlot = zig_runtime.ICSlot;
const JSVarRef = zig_runtime.JSVarRef;
const thin = zig_runtime.thin;

// ============================================================================
// Stack operations (simple — inlined in LLVM IR, but exported as fallback)
// ============================================================================

export fn llvm_rt_push_i32(stack: [*]CV, sp: *usize, val: i32) callconv(.c) void {
    thin.push_i32(stack, sp, val);
}

export fn llvm_rt_push_true(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.push_true(stack, sp);
}

export fn llvm_rt_push_false(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.push_false(stack, sp);
}

export fn llvm_rt_push_null(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.push_null(stack, sp);
}

export fn llvm_rt_push_undefined(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.push_undefined(stack, sp);
}

export fn llvm_rt_push_cv(stack: [*]CV, sp: *usize, val: CV) callconv(.c) void {
    thin.push_cv(stack, sp, val);
}

// ============================================================================
// Local variable access
// ============================================================================

export fn llvm_rt_get_loc(ctx: *JSContext, stack: [*]CV, sp: *usize, locals: [*]CV, idx: u32) callconv(.c) void {
    thin.get_loc(ctx, stack, sp, locals, idx);
}

export fn llvm_rt_put_loc(ctx: *JSContext, stack: [*]CV, sp: *usize, locals: [*]CV, idx: u32) callconv(.c) void {
    thin.put_loc(ctx, stack, sp, locals, idx);
}

export fn llvm_rt_set_loc(ctx: *JSContext, stack: [*]CV, sp: *usize, locals: [*]CV, idx: u32) callconv(.c) void {
    thin.set_loc(ctx, stack, sp, locals, idx);
}

// ============================================================================
// Argument access
// ============================================================================

export fn llvm_rt_get_arg(ctx: *JSContext, stack: [*]CV, sp: *usize, argc: c_int, argv: [*]JSValue, idx: u32) callconv(.c) void {
    thin.get_arg(ctx, stack, sp, argc, argv, idx);
}

export fn llvm_rt_get_arg_shadow(stack: [*]CV, sp: *usize, arg_shadow: [*]CV, idx: u32) callconv(.c) void {
    thin.get_arg_shadow(stack, sp, arg_shadow, idx);
}

export fn llvm_rt_put_arg(ctx: *JSContext, stack: [*]CV, sp: *usize, arg_shadow: [*]CV, idx: u32) callconv(.c) void {
    thin.put_arg(ctx, stack, sp, arg_shadow, idx);
}

// ============================================================================
// Closure variable access
// ============================================================================

export fn llvm_rt_get_var_ref(ctx: *JSContext, stack: [*]CV, sp: *usize, var_refs: ?[*]*JSVarRef, idx: u32, closure_var_count: c_int) callconv(.c) void {
    thin.get_var_ref(ctx, stack, sp, var_refs, idx, closure_var_count);
}

export fn llvm_rt_put_var_ref(ctx: *JSContext, stack: [*]CV, sp: *usize, var_refs: ?[*]*JSVarRef, idx: u32, closure_var_count: c_int) callconv(.c) void {
    thin.put_var_ref(ctx, stack, sp, var_refs, idx, closure_var_count);
}

export fn llvm_rt_set_var_ref(ctx: *JSContext, stack: [*]CV, sp: *usize, var_refs: ?[*]*JSVarRef, idx: u32, closure_var_count: c_int) callconv(.c) void {
    thin.set_var_ref(ctx, stack, sp, var_refs, idx, closure_var_count);
}

// ============================================================================
// Stack manipulation
// ============================================================================

export fn llvm_rt_dup_top(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.dup_top(ctx, stack, sp);
}

export fn llvm_rt_dup1(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.dup1(ctx, stack, sp);
}

export fn llvm_rt_dup2(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.dup2(ctx, stack, sp);
}

export fn llvm_rt_dup3(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.dup3(ctx, stack, sp);
}

export fn llvm_rt_drop(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.drop(ctx, stack, sp);
}

export fn llvm_rt_nip(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.nip(ctx, stack, sp);
}

export fn llvm_rt_swap(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.swap(stack, sp);
}

// ============================================================================
// Arithmetic operations
// ============================================================================

export fn llvm_rt_op_add(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_add(ctx, stack, sp);
}

export fn llvm_rt_op_sub(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_sub(stack, sp);
}

export fn llvm_rt_op_mul(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_mul(stack, sp);
}

export fn llvm_rt_op_div(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_div(stack, sp);
}

export fn llvm_rt_op_mod(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_mod(stack, sp);
}

export fn llvm_rt_op_pow(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_pow(stack, sp);
}

export fn llvm_rt_op_neg(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_neg(stack, sp);
}

export fn llvm_rt_op_plus(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_plus(stack, sp);
}

// ============================================================================
// Bitwise operations
// ============================================================================

export fn llvm_rt_op_band(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_band(stack, sp);
}

export fn llvm_rt_op_bor(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_bor(stack, sp);
}

export fn llvm_rt_op_bxor(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_bxor(stack, sp);
}

export fn llvm_rt_op_bnot(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_bnot(stack, sp);
}

export fn llvm_rt_op_shl(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_shl(stack, sp);
}

export fn llvm_rt_op_sar(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_sar(stack, sp);
}

export fn llvm_rt_op_shr(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_shr(stack, sp);
}

// ============================================================================
// Comparison operations
// ============================================================================

export fn llvm_rt_op_lt(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_lt(stack, sp);
}

export fn llvm_rt_op_lte(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_lte(stack, sp);
}

export fn llvm_rt_op_gt(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_gt(stack, sp);
}

export fn llvm_rt_op_gte(stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_gte(stack, sp);
}

export fn llvm_rt_op_eq(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_eq(ctx, stack, sp);
}

export fn llvm_rt_op_neq(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_neq(ctx, stack, sp);
}

export fn llvm_rt_op_strict_eq(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_strict_eq(ctx, stack, sp);
}

export fn llvm_rt_op_strict_neq(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_strict_neq(ctx, stack, sp);
}

// ============================================================================
// Logical / type operations
// ============================================================================

export fn llvm_rt_op_lnot(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_lnot(ctx, stack, sp);
}

export fn llvm_rt_op_typeof(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_typeof(ctx, stack, sp);
}

// ============================================================================
// Increment / Decrement
// ============================================================================

export fn llvm_rt_inc_loc(locals: [*]CV, idx: u32) callconv(.c) void {
    thin.inc_loc(locals, idx);
}

export fn llvm_rt_dec_loc(locals: [*]CV, idx: u32) callconv(.c) void {
    thin.dec_loc(locals, idx);
}

// ============================================================================
// Complex operations (noinline — call through to thin helpers)
// These use simplified C-ABI signatures (no Zig slices or optionals).
// ============================================================================

/// Call function: stack has [func, arg0..argN-1] → [result]
/// Returns 0 on success, non-zero on exception.
export fn llvm_rt_op_call(
    ctx: *JSContext,
    stack: [*]CV,
    sp: *usize,
    argc: u16,
) callconv(.c) c_int {
    // Simplified version: no closure sync (for thin codegen)
    thin.op_call(ctx, stack, sp, argc, null, null, 0, null, &.{}) catch return 1;
    return 0;
}

/// Call method: stack has [obj, method, arg0..argN-1] → [result]
/// Returns 0 on success, non-zero on exception.
export fn llvm_rt_op_call_method(
    ctx: *JSContext,
    stack: [*]CV,
    sp: *usize,
    argc: u16,
) callconv(.c) c_int {
    thin.op_call_method(ctx, stack, sp, argc, null, null, 0, null, &.{}) catch return 1;
    return 0;
}

/// Call constructor: stack has [constructor, arg0..argN-1] → [result]
/// Returns 0 on success, non-zero on exception.
export fn llvm_rt_op_call_constructor(
    ctx: *JSContext,
    stack: [*]CV,
    sp: *usize,
    argc: u16,
) callconv(.c) c_int {
    thin.op_call_constructor(ctx, stack, sp, argc) catch return 1;
    return 0;
}

/// Property access with inline cache
/// Returns 0 on success, non-zero on exception.
export fn llvm_rt_op_get_field_ic(ctx: *JSContext, stack: [*]CV, sp: *usize, ic: *ICSlot, name: [*:0]const u8) callconv(.c) c_int {
    thin.op_get_field_ic(ctx, stack, sp, ic, name) catch return 1;
    return 0;
}

/// Property access with inline cache (keeps object on stack)
/// Returns 0 on success, non-zero on exception.
export fn llvm_rt_op_get_field2_ic(ctx: *JSContext, stack: [*]CV, sp: *usize, ic: *ICSlot, name: [*:0]const u8) callconv(.c) c_int {
    thin.op_get_field2_ic(ctx, stack, sp, ic, name) catch return 1;
    return 0;
}

/// Put property
/// Returns 0 on success, non-zero on exception.
export fn llvm_rt_op_put_field(ctx: *JSContext, stack: [*]CV, sp: *usize, name: [*:0]const u8) callconv(.c) c_int {
    thin.op_put_field(ctx, stack, sp, name) catch return 1;
    return 0;
}

/// Get global variable
/// Returns 0 on success, non-zero on exception.
export fn llvm_rt_op_get_var(ctx: *JSContext, stack: [*]CV, sp: *usize, name: [*:0]const u8) callconv(.c) c_int {
    thin.op_get_var(ctx, stack, sp, name) catch return 1;
    return 0;
}

// ============================================================================
// Return helpers
// ============================================================================

/// Return value from stack top. Cleans up locals.
export fn llvm_rt_returnFromStack(
    ctx: *JSContext,
    stack: [*]CV,
    sp: *usize,
    locals: [*]CV,
    local_count: usize,
    locals_jsv: ?[*]JSValue,
    var_ref_list: ?*zig_runtime.ListHead,
    arg_shadow: ?[*]CV,
    arg_count: usize,
) callconv(.c) JSValue {
    return thin.returnFromStack(ctx, stack, sp.*, locals, local_count, locals_jsv, var_ref_list, arg_shadow, arg_count);
}

/// Return undefined. Cleans up locals.
export fn llvm_rt_returnUndef(
    ctx: *JSContext,
    locals: [*]CV,
    local_count: usize,
    locals_jsv: ?[*]JSValue,
    var_ref_list: ?*zig_runtime.ListHead,
    arg_shadow: ?[*]CV,
    arg_count: usize,
) callconv(.c) JSValue {
    return thin.returnUndef(ctx, locals, local_count, locals_jsv, var_ref_list, arg_shadow, arg_count);
}

/// Exception cleanup. Frees locals, returns EXCEPTION.
export fn llvm_rt_exceptionCleanup(
    ctx: *JSContext,
    locals: [*]CV,
    local_count: usize,
    locals_jsv: ?[*]JSValue,
    var_ref_list: ?*zig_runtime.ListHead,
    arg_shadow: ?[*]CV,
    arg_count: usize,
) callconv(.c) JSValue {
    return thin.exceptionCleanup(ctx, locals, local_count, locals_jsv, var_ref_list, arg_shadow, arg_count);
}

// ============================================================================
// Iterator operations
// ============================================================================

export fn llvm_rt_op_for_of_start(ctx: *JSContext, stack: [*]CV, sp: *usize, for_of_iter_stack: [*]usize, for_of_depth: *usize) callconv(.c) c_int {
    thin.op_for_of_start(ctx, stack, sp, for_of_iter_stack, for_of_depth) catch return 1;
    return 0;
}

export fn llvm_rt_op_for_of_next(ctx: *JSContext, stack: [*]CV, sp: *usize, for_of_iter_stack: [*]usize, for_of_depth: *usize) callconv(.c) c_int {
    thin.op_for_of_next(ctx, stack, sp, for_of_iter_stack, for_of_depth) catch return 1;
    return 0;
}

export fn llvm_rt_op_iterator_close(ctx: *JSContext, stack: [*]CV, sp: *usize, for_of_iter_stack: [*]usize, for_of_depth: *usize) callconv(.c) void {
    thin.op_iterator_close(ctx, stack, sp, for_of_iter_stack, for_of_depth);
}

export fn llvm_rt_op_iterator_get_value_done(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_iterator_get_value_done(ctx, stack, sp);
}

export fn llvm_rt_op_for_in_start(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) c_int {
    thin.op_for_in_start(ctx, stack, sp) catch return 1;
    return 0;
}

export fn llvm_rt_op_for_in_next(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) c_int {
    thin.op_for_in_next(ctx, stack, sp) catch return 1;
    return 0;
}

export fn llvm_rt_op_in(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) c_int {
    thin.op_in(ctx, stack, sp) catch return 1;
    return 0;
}

// ============================================================================
// Array operations
// ============================================================================

export fn llvm_rt_op_append(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_append(ctx, stack, sp);
}

export fn llvm_rt_op_put_array_el(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    thin.op_put_array_el(ctx, stack, sp);
}

// ============================================================================
// CompressedValue operations (for JSValue ↔ CV conversion)
// ============================================================================

export fn llvm_rt_cv_from_jsvalue(val: JSValue) callconv(.c) CV {
    return CV.fromJSValue(val);
}

export fn llvm_rt_cv_to_jsvalue_with_ctx(ctx: *JSContext, cv: *CV) callconv(.c) JSValue {
    return cv.toJSValueWithCtx(ctx);
}

export fn llvm_rt_cv_dup_ref(cv: CV) callconv(.c) CV {
    return CV.dupRef(cv);
}

export fn llvm_rt_cv_free_ref(ctx: *JSContext, cv: CV) callconv(.c) void {
    CV.freeRef(ctx, cv);
}

// ============================================================================
// Fallback: execute single opcode via interpreter
// ============================================================================

export fn llvm_rt_exec_opcode(
    ctx: *JSContext,
    op: u8,
    operand: i32,
    stack: [*]CV,
    sp: *usize,
    locals: [*]CV,
    var_count: u32,
) callconv(.c) c_int {
    const quickjs = zig_runtime.quickjs;
    // Convert usize SP to c_int for QuickJS C API, then write back
    var sp_i: c_int = @intCast(sp.*);
    const result = quickjs.js_frozen_exec_opcode(
        ctx,
        op,
        @bitCast(operand),
        @ptrCast(stack),
        &sp_i,
        @ptrCast(locals),
        @intCast(var_count),
    );
    sp.* = @intCast(sp_i);
    // js_frozen_exec_opcode returns JSValue; check for exception
    if (result.isException()) return -1;
    return 0;
}

// ============================================================================
// QuickJS FFI helpers
// ============================================================================

export fn llvm_rt_js_get_global_object(ctx: *JSContext) callconv(.c) JSValue {
    return zig_runtime.quickjs.JS_GetGlobalObject(ctx);
}

export fn llvm_rt_js_free_value(ctx: *JSContext, val: JSValue) callconv(.c) void {
    zig_runtime.quickjs.JS_FreeValue(ctx, val);
}

export fn llvm_rt_js_dup_value(ctx: *JSContext, val: JSValue) callconv(.c) JSValue {
    return zig_runtime.quickjs.JS_DupValue(ctx, val);
}

// ============================================================================
// Higher-level frozen helpers
// ============================================================================

export fn llvm_rt_check_stack() callconv(.c) bool {
    return zig_runtime.checkStack();
}

export fn llvm_rt_exit_stack() callconv(.c) void {
    zig_runtime.exitStack();
}

export fn llvm_rt_get_closure_var_safe(ctx: *JSContext, var_refs: ?[*]*JSVarRef, idx: u32, closure_var_count: c_int) callconv(.c) JSValue {
    return zig_runtime.getClosureVarSafe(ctx, var_refs, idx, closure_var_count);
}

export fn llvm_rt_set_closure_var_safe(ctx: *JSContext, var_refs: ?[*]*JSVarRef, idx: u32, closure_var_count: c_int, val: JSValue) callconv(.c) void {
    zig_runtime.setClosureVarSafe(ctx, var_refs, idx, closure_var_count, val);
}

export fn llvm_rt_ic_load(ctx: *JSContext, obj: JSValue, ic: *ICSlot, name: [*:0]const u8) callconv(.c) JSValue {
    return zig_runtime.icLoad(ctx, obj, ic, name) catch JSValue.EXCEPTION;
}

export fn llvm_rt_get_field_checked(ctx: *JSContext, obj: JSValue, name: [*:0]const u8) callconv(.c) JSValue {
    return zig_runtime.getFieldChecked(ctx, obj, name) catch JSValue.EXCEPTION;
}

export fn llvm_rt_make_rest_array(ctx: *JSContext, argc: c_int, argv: [*]JSValue, first_arg_index: u32) callconv(.c) JSValue {
    return zig_runtime.makeRestArray(ctx, argc, argv, first_arg_index);
}

export fn llvm_rt_cleanup_locals(ctx: *JSContext, locals_ptr: [*]CV, count: usize) callconv(.c) void {
    zig_runtime.cleanupLocals(ctx, locals_ptr, count);
}

// ============================================================================
// Apply operation
// ============================================================================

export fn llvm_rt_op_apply(
    ctx: *JSContext,
    stack: [*]CV,
    sp: *usize,
) callconv(.c) c_int {
    thin.op_apply(ctx, stack, sp, null, null, 0, null, &.{}) catch return 1;
    return 0;
}
