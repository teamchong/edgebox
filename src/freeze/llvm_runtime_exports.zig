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

export fn llvm_rt_push_const(ctx: *JSContext, stack: [*]CV, sp: *usize, cpool: ?[*]JSValue, idx: u32) callconv(.c) void {
    if (cpool) |cp| {
        const val = JSValue.dup(ctx, cp[idx]);
        stack[sp.*] = CV.fromJSValue(val);
    } else {
        stack[sp.*] = CV.UNDEFINED;
    }
    sp.* += 1;
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

export fn llvm_rt_set_arg(ctx: *JSContext, stack: [*]CV, sp: *usize, arg_shadow: [*]CV, idx: u32) callconv(.c) void {
    thin.set_arg(ctx, stack, sp, arg_shadow, idx);
}

// ============================================================================
// Closure variable access
// ============================================================================

export fn llvm_rt_get_var_ref(ctx: *JSContext, stack: [*]CV, sp: *usize, var_refs: ?[*]*JSVarRef, idx: u32, closure_var_count: c_int) callconv(.c) void {
    thin.get_var_ref(ctx, stack, sp, var_refs, idx, closure_var_count);
}

/// get_var_ref with TDZ (Temporal Dead Zone) check.
/// Returns 0 on success, 1 on exception (uninitialized variable).
export fn llvm_rt_get_var_ref_check(ctx: *JSContext, stack: [*]CV, sp: *usize, var_refs: ?[*]*JSVarRef, idx: u32, closure_var_count: c_int) callconv(.c) c_int {
    const val = zig_runtime.getClosureVarCheckSafe(ctx, var_refs, idx, closure_var_count);
    stack[sp.*] = CV.fromJSValue(val);
    sp.* += 1;
    if (stack[sp.* - 1].isException()) return 1;
    return 0;
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

/// Specialized charCodeAt: stack has [str, method, idx] → [charCode]
/// Fast path for str.charCodeAt(idx) — avoids full JS method dispatch.
/// The str CV is converted to JSValue and passed to js_frozen_char_code_at (C helper)
/// which handles both 8-bit and 16-bit strings, normal and slice strings.
/// Falls back to normal call_method(1) on non-string or non-int index.
/// Returns 0 on success, non-zero on exception.
export fn llvm_rt_call_method_char_code_at(
    ctx: *JSContext,
    stack: [*]CV,
    sp: *usize,
) callconv(.c) c_int {
    const s = sp.*;
    if (s >= 3) {
        const str_cv = stack[s - 3];
        const idx_cv = stack[s - 1];

        // Fast path: string CV + int index
        if (str_cv.isStr() and idx_cv.isInt()) {
            const str_jsv = str_cv.toJSValueWithCtx(ctx);
            const idx = idx_cv.getInt();
            const char_code = zig_runtime.quickjs.js_frozen_char_code_at(str_jsv, idx);
            if (char_code >= 0) {
                // Success: free method (sp-2) and str (sp-3), idx is int (no refcount)
                CV.freeRef(ctx, stack[s - 2]); // free the charCodeAt function ref
                CV.freeRef(ctx, stack[s - 3]); // free the string ref
                sp.* = s - 3;
                stack[sp.*] = @bitCast(CV.newInt(char_code));
                sp.* += 1;
                return 0;
            }
            // char_code == -1: out of bounds → return NaN (standard charCodeAt behavior)
            // But we can't distinguish "not a string" from "out of bounds" with -1.
            // For out-of-bounds, charCodeAt returns NaN. Fall through to slow path
            // which handles this correctly via the actual JS method call.
        }
    }

    // Slow path: normal call_method(1)
    thin.op_call_method(ctx, stack, sp, 1, null, null, 0, null, &.{}) catch return 1;
    return 0;
}

/// Specialized Array.push(val): stack has [arr, method, val] → [new_length]
/// Fast path for arr.push(val) on fast arrays — directly appends via add_fast_array_element.
/// Falls back to normal call_method(1) on non-fast-arrays.
/// Returns 0 on success, non-zero on exception.
export fn llvm_rt_call_method_array_push(
    ctx: *JSContext,
    stack: [*]CV,
    sp: *usize,
) callconv(.c) c_int {
    const s = sp.*;
    if (s >= 3) {
        const arr_cv = stack[s - 3];

        // Fast path: arr is an object (ptr CV)
        if (arr_cv.isPtr()) {
            // Use toJSValue (no dup) for arr — we just read from it
            const arr_jsv = arr_cv.toJSValue();
            // Convert val to JSValue and DUP it — js_frozen_array_push CONSUMES its argument
            // (stores directly in array without dup). toJSValue is a bitwise conversion that
            // does NOT dup, so we must explicitly dup to create a separate reference for the array.
            const val_jsv = stack[s - 1].toJSValue();
            const val_duped = zig_runtime.quickjs.JS_DupValue(ctx, val_jsv);
            // js_frozen_array_push CONSUMES val_duped on success (array takes ownership)
            // On failure (-1), val_duped is NOT consumed — we must free it
            const new_len = zig_runtime.quickjs.js_frozen_array_push(ctx, arr_jsv, val_duped);
            if (new_len >= 0) {
                // Success: val_duped was consumed by array. Free stack CVs.
                CV.freeRef(ctx, stack[s - 1]); // val CV (stack's reference)
                CV.freeRef(ctx, stack[s - 2]); // method (push function)
                CV.freeRef(ctx, stack[s - 3]); // arr
                sp.* = s - 3;
                stack[sp.*] = @bitCast(CV.newInt(new_len));
                sp.* += 1;
                return 0;
            }
            // new_len == -1: not a fast array. val_duped was NOT consumed — free the dup
            zig_runtime.quickjs.JS_FreeValue(ctx, val_duped);
            // Fall through to slow path (stack CVs remain valid)
        }
    }

    // Slow path: normal call_method(1)
    thin.op_call_method(ctx, stack, sp, 1, null, null, 0, null, &.{}) catch return 1;
    return 0;
}

/// Specialized Math.abs/sqrt/floor/ceil/round: stack has [Math, method, arg] → [result]
/// op: 0=abs, 1=sqrt, 2=floor, 3=ceil, 4=round
/// Fast path for numeric args — applies the math operation directly.
/// Falls back to normal call_method(1) for non-numeric args.
/// Returns 0 on success, non-zero on exception.
export fn llvm_rt_call_method_math_unary(
    ctx: *JSContext,
    stack: [*]CV,
    sp: *usize,
    op: i32,
) callconv(.c) c_int {
    const s = sp.*;
    if (s >= 3) {
        const arg_cv = stack[s - 1];
        // Fast path: arg is int or float → extract f64, apply math op
        var f: f64 = undefined;
        var is_numeric = false;
        if (arg_cv.isFloat()) {
            f = arg_cv.getFloat();
            is_numeric = true;
        } else if (arg_cv.isInt()) {
            f = @floatFromInt(arg_cv.getInt());
            is_numeric = true;
        }
        if (is_numeric) {
            const result: f64 = switch (op) {
                0 => @abs(f),
                1 => @sqrt(f),
                2 => @floor(f),
                3 => @ceil(f),
                4 => blk: {
                    // JS Math.round: round half towards +Infinity (not banker's rounding)
                    break :blk @floor(f + 0.5);
                },
                else => unreachable,
            };
            // No freeRef needed for int/float args (they're not ref-counted)
            // But the method (stack[s-2]) and Math object (stack[s-3]) may be ref-counted
            CV.freeRef(ctx, stack[s - 1]); // arg (no-op for int/float)
            CV.freeRef(ctx, stack[s - 2]); // method function
            CV.freeRef(ctx, stack[s - 3]); // Math object
            sp.* = s - 3;
            stack[sp.*] = @bitCast(CV.newFloat(result));
            sp.* += 1;
            return 0;
        }
    }

    // Slow path: normal call_method(1)
    thin.op_call_method(ctx, stack, sp, 1, null, null, 0, null, &.{}) catch return 1;
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
// Tail call + return (combined to match Zig full codegen semantics)
// ============================================================================


/// Combined tail_call: call function + free consumed CVs + cleanup + return.
/// This matches the Zig full codegen's emitCall+return sequence exactly.
/// Unlike op_call + returnFromStack, this properly frees func+arg CVs.
export fn llvm_rt_tail_call_return(
    ctx: *JSContext,
    stack: [*]CV,
    sp: *usize,
    argc: u16,
    locals: [*]CV,
    local_count: usize,
    arg_shadow: ?[*]CV,
    arg_count: usize,
) callconv(.c) JSValue {
    const n: usize = @intCast(argc);

    // Extract function from stack
    const fn_val = CV.toJSValuePtr(&stack[sp.* - n - 1]);

    // Build args array from stack
    var args: [256]JSValue = undefined;
    for (0..n) |i| {
        args[i] = CV.toJSValuePtr(&stack[sp.* - n + i]);
    }

    // Call the function
    const result = JSValue.call(ctx, fn_val, JSValue.UNDEFINED, @intCast(argc), @ptrCast(&args));

    // Free consumed CVs: args then func (matching Zig full codegen order)
    for (0..n) |i| {
        CV.freeRef(ctx, stack[sp.* - n + i]);
    }
    CV.freeRef(ctx, stack[sp.* - n - 1]); // func

    // Exit frozen call depth
    zig_runtime.exitStack();

    // Cleanup locals
    zig_runtime.cleanupLocals(ctx, locals, local_count);

    // Free arg shadow
    if (arg_shadow) |shadow| {
        for (0..arg_count) |i| {
            CV.freeRef(ctx, shadow[i]);
        }
    }

    if (result.isException()) {
        return result;
    }

    return result;
}

/// Combined tail_call_method: call method + free consumed CVs + cleanup + return.
export fn llvm_rt_tail_call_method_return(
    ctx: *JSContext,
    stack: [*]CV,
    sp: *usize,
    argc: u16,
    locals: [*]CV,
    local_count: usize,
    arg_shadow: ?[*]CV,
    arg_count: usize,
) callconv(.c) JSValue {
    const n: usize = @intCast(argc);

    // Extract obj and method from stack
    const obj = CV.toJSValuePtr(&stack[sp.* - n - 2]);
    const method = CV.toJSValuePtr(&stack[sp.* - n - 1]);

    // Build args
    var args: [256]JSValue = undefined;
    for (0..n) |i| {
        args[i] = CV.toJSValuePtr(&stack[sp.* - n + i]);
    }

    // Call
    const result = JSValue.call(ctx, method, obj, @intCast(argc), @ptrCast(&args));

    // Free consumed CVs: args, method, obj
    for (0..n) |i| {
        CV.freeRef(ctx, stack[sp.* - n + i]);
    }
    CV.freeRef(ctx, stack[sp.* - n - 1]); // method
    CV.freeRef(ctx, stack[sp.* - n - 2]); // obj

    // Exit frozen call depth
    zig_runtime.exitStack();

    // Cleanup
    zig_runtime.cleanupLocals(ctx, locals, local_count);
    if (arg_shadow) |shadow| {
        for (0..arg_count) |i| {
            CV.freeRef(ctx, shadow[i]);
        }
    }

    if (result.isException()) {
        return result;
    }

    return result;
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
// Fast array access helpers (zero-FFI JSArray fast path)
// ============================================================================

/// Probe a CV array value: extract raw JSValue[] pointer and element count.
/// Returns 1 if fast array (out_values_ptr and out_count are set), 0 otherwise.
/// The returned pointer is valid as long as the array is not modified.
/// This is designed to be called ONCE before a loop, then use the pointer
/// for direct element access inside the loop (avoiding per-element getFastArrayDirect).
export fn llvm_rt_fast_array_probe(arr_cv: u64, out_values_ptr: *?[*]u8, out_count: *u32) callconv(.c) c_int {
    const cv: CV = @bitCast(arr_cv);
    if (!cv.isObject()) {
        out_values_ptr.* = null;
        out_count.* = 0;
        return 0;
    }
    const jsv = cv.toJSValue();
    const result = zig_runtime.getFastArrayDirect(jsv);
    if (!result.success) {
        out_values_ptr.* = null;
        out_count.* = 0;
        return 0;
    }
    if (result.values) |vals| {
        out_values_ptr.* = @ptrCast(vals);
        out_count.* = result.count;
        return 1;
    }
    out_values_ptr.* = null;
    out_count.* = 0;
    return 0;
}

/// Check if a CV value is a fast array (contiguous JSValue[] backing store).
/// Returns 1 if fast array, 0 otherwise.
export fn llvm_rt_fast_array_is_fast(cv_bits: u64) callconv(.c) c_int {
    const cv: CV = @bitCast(cv_bits);
    if (!cv.isObject()) return 0;
    const jsv = cv.toJSValue();
    const result = zig_runtime.getFastArrayDirect(jsv);
    return if (result.success) @as(c_int, 1) else @as(c_int, 0);
}

/// Get the raw JSValue[] pointer from a fast array CV.
/// Caller must check is_fast first. Returns null if not a fast array.
export fn llvm_rt_fast_array_get_values(cv_bits: u64) callconv(.c) ?[*]u8 {
    const cv: CV = @bitCast(cv_bits);
    const jsv = cv.toJSValue();
    const result = zig_runtime.getFastArrayDirect(jsv);
    if (!result.success) return null;
    if (result.values) |vals| {
        return @ptrCast(vals);
    }
    return null;
}

/// Get the element count from a fast array CV.
/// Caller must check is_fast first. Returns 0 if not a fast array.
export fn llvm_rt_fast_array_get_count(cv_bits: u64) callconv(.c) c_int {
    const cv: CV = @bitCast(cv_bits);
    const jsv = cv.toJSValue();
    const result = zig_runtime.getFastArrayDirect(jsv);
    if (!result.success) return 0;
    return @intCast(result.count);
}

/// Load a JSValue at index from a raw values pointer, convert to CV (with refcount dup).
/// The pointer must come from llvm_rt_fast_array_get_values.
/// Each JSValue is 16 bytes on x86_64 (JSValueUnion + tag).
export fn llvm_rt_jsvalue_to_cv(ctx: *JSContext, values_ptr: [*]const u8, index: c_int) callconv(.c) u64 {
    const jsv_ptr: [*]const JSValue = @ptrCast(@alignCast(values_ptr));
    const jsv = jsv_ptr[@intCast(index)];
    // Dup the JSValue (increment refcount) since we're creating a new owned CV reference
    const duped = JSValue.dup(ctx, jsv);
    return @bitCast(CV.fromJSValue(duped));
}

/// Combined fast array element access: check if fast array, load element, return as CV.
/// Returns the element CV on success, or CV_SENTINEL (0) on failure (not a fast array,
/// index out of bounds, or index not an integer).
/// This avoids 3 separate runtime calls per element access in the hot loop.
const CV_SENTINEL: u64 = 0; // 0.0 as f64 — not a valid NaN-boxed value for any JS type
export fn llvm_rt_fast_array_get_el(ctx: *JSContext, arr_cv: u64, idx_cv: u64) callconv(.c) u64 {
    const arr: CV = @bitCast(arr_cv);
    const idx: CV = @bitCast(idx_cv);

    // Index must be an integer
    if (!idx.isInt()) return CV_SENTINEL;

    // Array must be an object
    if (!arr.isObject()) return CV_SENTINEL;

    // Get JSValue and check fast array
    const jsv = arr.toJSValue();
    const result = zig_runtime.getFastArrayDirect(jsv);
    if (!result.success) return CV_SENTINEL;

    const i_signed = idx.getInt();
    if (i_signed < 0) return CV_SENTINEL;
    const i: u32 = @intCast(i_signed);
    if (i >= result.count) return CV_SENTINEL;

    if (result.values) |vals| {
        const elem = vals[i];
        const duped = JSValue.dup(ctx, elem);
        return @bitCast(CV.fromJSValue(duped));
    }
    return CV_SENTINEL;
}

/// Combined fast array length access: check if fast array, return count as CV int.
/// Returns CV int on success, or CV_SENTINEL (0) on failure.
export fn llvm_rt_fast_array_get_len(arr_cv: u64) callconv(.c) u64 {
    const arr: CV = @bitCast(arr_cv);
    if (!arr.isObject()) return CV_SENTINEL;
    const jsv = arr.toJSValue();
    const result = zig_runtime.getFastArrayDirect(jsv);
    if (!result.success) return CV_SENTINEL;
    return @bitCast(CV.newInt(@intCast(result.count)));
}

/// Stack-based get_array_el: pop index and array, push result.
/// Implements: val = arr[idx] using JS_GetPropertyUint32.
/// Returns 0 on success, non-zero on exception.
export fn llvm_rt_get_array_el(ctx: *JSContext, stack: [*]CV, sp: *usize, locals: [*]CV, var_count: u32) callconv(.c) c_int {
    _ = locals;
    _ = var_count;
    const quickjs = zig_runtime.quickjs;
    const s = sp.*;
    if (s < 2) return -1;
    sp.* = s - 2;

    const arr_cv = stack[s - 2];
    const idx_cv = stack[s - 1];

    // Get the index as u32
    const idx_val: u32 = if (idx_cv.isInt()) @as(u32, @intCast(idx_cv.getInt())) else 0;

    // Convert array to JSValue and get property by integer index
    const arr_jsv = arr_cv.toJSValue();
    const result = quickjs.JS_GetPropertyUint32(ctx, arr_jsv, idx_val);

    // Free operands
    CV.freeRef(ctx, arr_cv);
    CV.freeRef(ctx, idx_cv);

    if (result.isException()) return -1;

    // Push result
    stack[sp.*] = CV.fromJSValue(result);
    sp.* += 1;
    return 0;
}

/// Stack-based get_array_el2: pop index and array, push array and result.
/// Returns 0 on success, non-zero on exception.
export fn llvm_rt_get_array_el2(ctx: *JSContext, stack: [*]CV, sp: *usize, locals: [*]CV, var_count: u32) callconv(.c) c_int {
    _ = locals;
    _ = var_count;
    const quickjs = zig_runtime.quickjs;
    const s = sp.*;
    if (s < 2) return -1;
    sp.* = s - 2;

    const arr_cv = stack[s - 2];
    const idx_cv = stack[s - 1];

    const idx_val: u32 = if (idx_cv.isInt()) @as(u32, @intCast(idx_cv.getInt())) else 0;
    const arr_jsv = arr_cv.toJSValue();
    const result = quickjs.JS_GetPropertyUint32(ctx, arr_jsv, idx_val);

    CV.freeRef(ctx, idx_cv);

    if (result.isException()) {
        CV.freeRef(ctx, arr_cv);
        return -1;
    }

    // Push array back, then result
    stack[sp.*] = arr_cv; // keep array (don't free)
    sp.* += 1;
    stack[sp.*] = CV.fromJSValue(result);
    sp.* += 1;
    return 0;
}

/// Stack-based get_length: pop object, push .length property.
/// Implements: val = obj.length using JS_GetPropertyStr.
/// Returns 0 on success, non-zero on exception.
export fn llvm_rt_get_length(ctx: *JSContext, stack: [*]CV, sp: *usize, locals: [*]CV, var_count: u32) callconv(.c) c_int {
    _ = locals;
    _ = var_count;
    const quickjs = zig_runtime.quickjs;
    const s = sp.*;
    if (s < 1) return -1;
    sp.* = s - 1;

    const obj_cv = stack[s - 1];
    const obj_jsv = obj_cv.toJSValue();
    const result = quickjs.JS_GetPropertyStr(ctx, obj_jsv, "length");
    CV.freeRef(ctx, obj_cv);

    if (result.isException()) return -1;

    stack[sp.*] = CV.fromJSValue(result);
    sp.* += 1;
    return 0;
}

/// add_loc: pop value from stack, add to locals[idx].
/// Int fast path: both int → newInt(getInt(old) + getInt(value))
/// Returns 0 on success, non-zero on exception.
export fn llvm_rt_add_loc(ctx: *JSContext, stack: [*]CV, sp: *usize, locals: [*]CV, idx: u32) callconv(.c) c_int {
    const s = sp.*;
    if (s == 0) return 1;
    sp.* = s - 1;
    const val = stack[s - 1];
    const old = locals[idx];

    // Int fast path
    if (old.isInt() and val.isInt()) {
        const a: i64 = old.getInt();
        const b: i64 = val.getInt();
        const r = a + b;
        if (r >= std.math.minInt(i32) and r <= std.math.maxInt(i32)) {
            locals[idx] = CV.newInt(@intCast(r));
            return 0;
        }
        // Overflow to float
        locals[idx] = CV.newFloat(@floatFromInt(r));
        return 0;
    }

    // Slow path: use full addWithCtx (handles strings, floats, etc.)
    const result = CV.addWithCtx(ctx, old, val);
    CV.freeRef(ctx, old);
    CV.freeRef(ctx, val);
    locals[idx] = result;
    return 0;
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

/// Pop CV from stack, evaluate JS truthiness (handles empty string, NaN, 0.0, etc.)
/// Returns 1 for truthy, 0 for falsy — matches JS semantics exactly.
export fn llvm_rt_cv_to_bool(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) i32 {
    const s = sp.*;
    if (s == 0) return 0;
    sp.* = s - 1;
    const cv = stack[s - 1];
    const result: i32 = if (cv.toBoolWithCtx(ctx)) 1 else 0;
    // Free the popped value (must happen AFTER toBoolWithCtx reads it)
    CV.freeRef(ctx, cv);
    return result;
}

// ============================================================================
// Push operations (this, empty string, atom value)
// ============================================================================

export fn llvm_rt_push_this(ctx: *JSContext, stack: [*]CV, sp: *usize, this_val: JSValue) callconv(.c) void {
    // Non-strict mode: coerce undefined/null this to globalThis
    // Most bundled code behaves as non-strict (bundlers strip "use strict")
    if (this_val.isUndefined() or this_val.isNull()) {
        stack[sp.*] = CV.fromJSValue(zig_runtime.quickjs.JS_GetGlobalObject(ctx));
    } else {
        stack[sp.*] = CV.fromJSValue(JSValue.dup(ctx, this_val));
    }
    sp.* += 1;
}

export fn llvm_rt_push_empty_string(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    stack[sp.*] = CV.fromJSValue(JSValue.newString(ctx, ""));
    sp.* += 1;
}

// ============================================================================
// Stack manipulation (insert, perm, rot)
// ============================================================================

/// insert2: [obj, val] -> [val, obj, val] (dup_x1)
export fn llvm_rt_insert2(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    const val = stack[sp.* - 1];
    const obj = stack[sp.* - 2];
    stack[sp.*] = val;
    stack[sp.* - 1] = obj;
    stack[sp.* - 2] = CV.dupRef(val);
    sp.* += 1;
    _ = ctx;
}

/// insert3: [obj, prop, val] -> [val, obj, prop, val] (dup_x2)
export fn llvm_rt_insert3(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    const val = stack[sp.* - 1];
    const c_val = stack[sp.* - 2];
    const b_val = stack[sp.* - 3];
    stack[sp.*] = val;
    stack[sp.* - 1] = c_val;
    stack[sp.* - 2] = b_val;
    stack[sp.* - 3] = CV.dupRef(val);
    sp.* += 1;
    _ = ctx;
}

/// insert4: [this, obj, prop, val] -> [val, this, obj, prop, val]
export fn llvm_rt_insert4(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    const val = stack[sp.* - 1];
    const d = stack[sp.* - 2];
    const c_val = stack[sp.* - 3];
    const b_val = stack[sp.* - 4];
    stack[sp.*] = val;
    stack[sp.* - 1] = d;
    stack[sp.* - 2] = c_val;
    stack[sp.* - 3] = b_val;
    stack[sp.* - 4] = CV.dupRef(val);
    sp.* += 1;
    _ = ctx;
}

/// perm3: [a, b, c] -> [b, a, c]
export fn llvm_rt_perm3(stack: [*]CV, sp: *usize) callconv(.c) void {
    const c_val = stack[sp.* - 1];
    const b_val = stack[sp.* - 2];
    const a_val = stack[sp.* - 3];
    stack[sp.* - 3] = b_val;
    stack[sp.* - 2] = a_val;
    stack[sp.* - 1] = c_val;
}

/// perm4: [a, b, c, d] -> [c, a, b, d]
export fn llvm_rt_perm4(stack: [*]CV, sp: *usize) callconv(.c) void {
    const d = stack[sp.* - 1];
    const c_val = stack[sp.* - 2];
    const b_val = stack[sp.* - 3];
    const a_val = stack[sp.* - 4];
    stack[sp.* - 4] = c_val;
    stack[sp.* - 3] = a_val;
    stack[sp.* - 2] = b_val;
    stack[sp.* - 1] = d;
}

/// perm5: [a, b, c, d, e] -> [d, a, b, c, e]
export fn llvm_rt_perm5(stack: [*]CV, sp: *usize) callconv(.c) void {
    const e = stack[sp.* - 1];
    const d = stack[sp.* - 2];
    const c_val = stack[sp.* - 3];
    const b_val = stack[sp.* - 4];
    const a_val = stack[sp.* - 5];
    stack[sp.* - 5] = d;
    stack[sp.* - 4] = a_val;
    stack[sp.* - 3] = b_val;
    stack[sp.* - 2] = c_val;
    stack[sp.* - 1] = e;
}

/// rot3l: [a, b, c] -> [c, a, b]
export fn llvm_rt_rot3l(stack: [*]CV, sp: *usize) callconv(.c) void {
    const c_val = stack[sp.* - 1];
    const b_val = stack[sp.* - 2];
    const a_val = stack[sp.* - 3];
    stack[sp.* - 3] = c_val;
    stack[sp.* - 2] = a_val;
    stack[sp.* - 1] = b_val;
}

/// rot3r: [a, b, c] -> [b, c, a] (this is the standard right rotation semantics)
export fn llvm_rt_rot3r(stack: [*]CV, sp: *usize) callconv(.c) void {
    const c_val = stack[sp.* - 1];
    const b_val = stack[sp.* - 2];
    const a_val = stack[sp.* - 3];
    stack[sp.* - 3] = b_val;
    stack[sp.* - 2] = c_val;
    stack[sp.* - 1] = a_val;
}

/// nip1: [a, b] -> [b] but free a
export fn llvm_rt_nip1(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    const b_val = stack[sp.* - 1];
    const a_val = stack[sp.* - 2];
    CV.freeRef(ctx, a_val);
    stack[sp.* - 2] = b_val;
    sp.* -= 1;
}

// ============================================================================
// Stack-based increment / decrement
// ============================================================================

export fn llvm_rt_inc(stack: [*]CV, sp: *usize) callconv(.c) void {
    const v = stack[sp.* - 1];
    if (v.isInt()) {
        const n = v.getInt();
        if (n < 0x7fffffff) {
            stack[sp.* - 1] = CV.newInt(n + 1);
        } else {
            stack[sp.* - 1] = CV.newFloat(@as(f64, @floatFromInt(n)) + 1.0);
        }
    } else if (v.isFloat()) {
        stack[sp.* - 1] = CV.newFloat(v.getFloat() + 1.0);
    }
}

export fn llvm_rt_dec(stack: [*]CV, sp: *usize) callconv(.c) void {
    const v = stack[sp.* - 1];
    if (v.isInt()) {
        const n = v.getInt();
        if (n > -0x7fffffff) {
            stack[sp.* - 1] = CV.newInt(n - 1);
        } else {
            stack[sp.* - 1] = CV.newFloat(@as(f64, @floatFromInt(n)) - 1.0);
        }
    } else if (v.isFloat()) {
        stack[sp.* - 1] = CV.newFloat(v.getFloat() - 1.0);
    }
}

export fn llvm_rt_post_inc(stack: [*]CV, sp: *usize) callconv(.c) void {
    const v = stack[sp.* - 1];
    if (v.isInt()) {
        const n = v.getInt();
        // QuickJS post_inc: sp[-1] = old value, sp[0] = incremented value, sp++
        // After: stack has [old, new] with new on top
        if (n < 0x7fffffff) {
            stack[sp.*] = CV.newInt(n + 1);
        } else {
            stack[sp.*] = CV.newFloat(@as(f64, @floatFromInt(n)) + 1.0);
        }
        // sp[-1] keeps old value (v) unchanged
        sp.* += 1;
    } else if (v.isFloat()) {
        stack[sp.*] = CV.newFloat(v.getFloat() + 1.0);
        // sp[-1] keeps old value (v) unchanged
        sp.* += 1;
    }
}

export fn llvm_rt_post_dec(stack: [*]CV, sp: *usize) callconv(.c) void {
    const v = stack[sp.* - 1];
    if (v.isInt()) {
        const n = v.getInt();
        // QuickJS post_dec: sp[-1] = old value, sp[0] = decremented value, sp++
        if (n > -0x7fffffff) {
            stack[sp.*] = CV.newInt(n - 1);
        } else {
            stack[sp.*] = CV.newFloat(@as(f64, @floatFromInt(n)) - 1.0);
        }
        sp.* += 1;
    } else if (v.isFloat()) {
        stack[sp.*] = CV.newFloat(v.getFloat() - 1.0);
        sp.* += 1;
    }
}

// ============================================================================
// Type check operations
// ============================================================================

export fn llvm_rt_is_undefined(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    const v = stack[sp.* - 1];
    CV.freeRef(ctx, v);
    stack[sp.* - 1] = if (v.isUndefined()) CV.TRUE else CV.FALSE;
}

export fn llvm_rt_is_null(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    const v = stack[sp.* - 1];
    CV.freeRef(ctx, v);
    stack[sp.* - 1] = if (v.isNull()) CV.TRUE else CV.FALSE;
}

export fn llvm_rt_is_undefined_or_null(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    const v = stack[sp.* - 1];
    CV.freeRef(ctx, v);
    stack[sp.* - 1] = if (v.isUndefined() or v.isNull()) CV.TRUE else CV.FALSE;
}

// ============================================================================
// Object/array operations
// ============================================================================

export fn llvm_rt_object(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    stack[sp.*] = CV.fromJSValue(zig_runtime.quickjs.JS_NewObject(ctx));
    sp.* += 1;
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
    var_refs: ?[*]*JSVarRef,
    closure_var_count: i32,
    argv: ?[*]const JSValue,
    argc: i32,
    arg_buf: ?[*]JSValue,
    func_obj: JSValue,
    new_target: JSValue,
    cpool: ?[*]JSValue,
    operand2: i32,
) callconv(.c) c_int {
    const quickjs = zig_runtime.quickjs;
    // CVs are 8 bytes (NaN-boxed i64), JSValues are 16 bytes ({i64, i64}).
    // We must convert between them — @ptrCast is wrong since element sizes differ.
    const old_sp = sp.*;
    // Convert stack CVs to JSValues. toJSValuePtr does NOT dup — transfers
    // ownership temporarily. The C function operates on JSValues and manages
    // refs internally (frees popped values, creates refs for pushed values).
    var jsv_stack: [512]JSValue = undefined;
    for (0..old_sp) |i| {
        jsv_stack[i] = CV.toJSValuePtr(&stack[i]);
    }
    // Convert locals CVs to JSValues (some opcodes may read locals)
    const lc: usize = @intCast(var_count);
    var jsv_locals: [256]JSValue = undefined;
    const local_max = if (lc < 256) lc else 256;
    for (0..local_max) |i| {
        jsv_locals[i] = CV.toJSValuePtr(&locals[i]);
    }

    var sp_i: c_int = @intCast(old_sp);
    const result = quickjs.js_frozen_exec_opcode(
        ctx,
        op,
        @bitCast(operand),
        @ptrCast(&jsv_stack),
        &sp_i,
        @ptrCast(&jsv_locals),
        @intCast(var_count),
        var_refs,
        closure_var_count,
        argv,
        argc,
        arg_buf,
        func_obj,
        new_target,
        cpool,
        operand2,
    );
    const new_sp: usize = @intCast(sp_i);

    // Write back JSValue stack to CV stack.
    // toJSValuePtr does NOT dup — ownership was transferred to JSValue space.
    // The C function manages refs for consumed (popped) values internally,
    // so we must NOT call CV.freeRef on old CVs at popped positions (that
    // would double-free since C already freed them).
    // For untouched positions, the roundtrip CV→JSValue→CV is ref-neutral.
    // For new positions (pushed by C), we take ownership of the new refs.
    for (0..new_sp) |i| {
        stack[i] = CV.fromJSValue(jsv_stack[i]);
    }
    sp.* = new_sp;

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

// ============================================================================
// TDZ (Temporal Dead Zone) helpers
// ============================================================================

/// Throw a ReferenceError for accessing an uninitialized variable (TDZ).
/// Returns 1 always (signals exception to caller).
export fn llvm_rt_throw_tdz(ctx: *JSContext) callconv(.c) c_int {
    _ = zig_runtime.quickjs.JS_ThrowReferenceError(ctx, "Cannot access variable before initialization");
    return 1;
}

// ============================================================================
// Push atom value (string creation)
// ============================================================================

/// Push a string value created from a property name onto the stack.
/// Used by push_atom_value opcode.
export fn llvm_rt_push_atom_value(ctx: *JSContext, stack: [*]CV, sp: *usize, name: [*:0]const u8) callconv(.c) void {
    const str = JSValue.newString(ctx, name);
    stack[sp.*] = CV.fromJSValue(str);
    sp.* += 1;
}

// ============================================================================
// Define field
// ============================================================================

/// Define a property on an object: stack has [obj, val], after: [obj] (val consumed).
/// Returns 0 on success, non-zero on exception.
export fn llvm_rt_define_field(ctx: *JSContext, stack: [*]CV, sp: *usize, name: [*:0]const u8) callconv(.c) c_int {
    const quickjs = zig_runtime.quickjs;
    const s = sp.*;
    if (s < 2) return 1;

    const val_cv = stack[s - 1];

    // Convert to JSValue: obj borrows from stack (stays), val dup'd for definePropertyStr
    const obj_jsv = stack[s - 2].toJSValue();
    const val_jsv = val_cv.toJSValue();
    const val_duped = JSValue.dup(ctx, val_jsv);

    // definePropertyStr takes ownership of val_duped
    const rc = quickjs.JS_DefinePropertyValueStr(ctx, obj_jsv, name, val_duped, quickjs.JS_PROP_C_W_E);

    // Free the val CV (consumed from stack)
    CV.freeRef(ctx, val_cv);

    // Pop val, keep obj
    sp.* = s - 1;

    if (rc < 0) return 1;
    return 0;
}

// ============================================================================
// typeof_is_function check
// ============================================================================

/// Check if TOS typeof == "function". Replaces TOS with boolean result.
export fn llvm_rt_typeof_is_function(ctx: *JSContext, stack: [*]CV, sp: *usize) callconv(.c) void {
    const quickjs = zig_runtime.quickjs;
    const s = sp.*;
    if (s == 0) return;

    const v = stack[s - 1];
    const jsv = v.toJSValueWithCtx(ctx);
    const is_func: bool = quickjs.JS_IsFunction(ctx, jsv) != 0;
    CV.freeRef(ctx, v);
    stack[s - 1] = if (is_func) CV.TRUE else CV.FALSE;
}

// ============================================================================
// get_var_undef (global lookup, returns undefined for missing, no throw)
// ============================================================================

/// Get global variable, returning undefined for missing (no ReferenceError).
/// Returns 0 on success, non-zero on exception.
export fn llvm_rt_op_get_var_undef(ctx: *JSContext, stack: [*]CV, sp: *usize, name: [*:0]const u8) callconv(.c) c_int {
    // In frozen mode, get_var_undef is the same as get_var —
    // JS_GetPropertyStr returns undefined for missing properties, no throw.
    thin.op_get_var(ctx, stack, sp, name) catch return 1;
    return 0;
}

// ============================================================================
// Closure creation (fclosure / fclosure8)
// ============================================================================

/// Create a closure from a constant pool function.
/// Stack effect: pushes the closure onto the stack.
/// Returns 0 on success, non-zero on exception.
const ListHead = zig_runtime.ListHead;

export fn llvm_rt_fclosure(
    ctx: *JSContext,
    stack: [*]CV,
    sp: *usize,
    cpool: ?[*]JSValue,
    func_idx: u32,
    var_refs: ?[*]*JSVarRef,
    locals: [*]CV,
    local_count: u32,
    argv: ?[*]JSValue,
    argc: c_int,
) callconv(.c) c_int {
    const quickjs = zig_runtime.quickjs;

    // Get the bytecode function from cpool
    const bfunc = if (cpool) |cp| cp[func_idx] else JSValue.UNDEFINED;

    // Convert locals CVs to JSValues for closure capture
    var locals_jsv: [256]JSValue = undefined;
    const lc: usize = @min(local_count, 256);
    for (0..lc) |i| {
        locals_jsv[i] = CV.toJSValuePtr(&locals[i]);
    }

    // Create the closure using QuickJS API
    const closure = quickjs.js_frozen_create_closure(
        ctx,
        bfunc,
        var_refs,
        if (lc > 0) @ptrCast(&locals_jsv) else null,
        @intCast(lc),
        if (argv != null and argc > 0) argv else null,
        argc,
    );

    if (closure.isException()) return 1;

    // Push closure onto stack
    stack[sp.*] = CV.fromJSValue(closure);
    sp.* += 1;
    return 0;
}

/// Create a closure with shared var_refs (v2 API).
/// Unlike v1, var_refs POINT INTO the locals_jsv array so closures see live local values.
/// Requires syncLocalsTo before calls and js_frozen_var_ref_list_detach at function return.
export fn llvm_rt_fclosure_v2(
    ctx: *JSContext,
    stack: [*]CV,
    sp: *usize,
    cpool: ?[*]JSValue,
    func_idx: u32,
    var_refs: ?[*]*JSVarRef,
    locals: [*]CV,
    local_count: u32,
    argv: ?[*]JSValue,
    argc: c_int,
    locals_jsv: [*]JSValue,
    var_ref_list: *ListHead,
    arg_shadow: ?[*]CV,
    arg_count: u32,
) callconv(.c) c_int {
    const quickjs = zig_runtime.quickjs;
    const lc: usize = @intCast(local_count);

    // Sync CV locals → JSValue locals_jsv so closure sees current values
    for (0..lc) |i| {
        locals_jsv[i] = CV.toJSValuePtr(&locals[i]);
    }

    // If arg_shadow exists, convert to JSValues for is_arg closure captures.
    // arg_shadow reflects put_arg mutations (e.g., rest opcode stores array there),
    // while argv is the original arguments before any modifications.
    const ac: usize = @intCast(arg_count);
    var args_jsv_buf: [256]JSValue = undefined;
    const args_ptr: ?[*]JSValue = if (arg_shadow) |shadow| blk: {
        for (0..ac) |i| {
            args_jsv_buf[i] = CV.toJSValuePtr(&shadow[i]);
        }
        break :blk @ptrCast(&args_jsv_buf);
    } else if (argv != null and argc > 0) argv else null;
    const args_count: c_int = if (arg_shadow != null) @intCast(arg_count) else argc;

    const bfunc = if (cpool) |cp| cp[func_idx] else JSValue.UNDEFINED;
    const closure = quickjs.js_frozen_create_closure_v2(
        ctx,
        bfunc,
        var_refs,
        var_ref_list,
        @ptrCast(locals_jsv),
        @intCast(lc),
        args_ptr,
        args_count,
    );

    if (closure.isException()) return 1;

    // Sync back: closure creation may have modified locals_jsv (e.g. for captured vars)
    for (0..lc) |i| {
        locals[i] = CV.fromJSValue(locals_jsv[i]);
    }

    stack[sp.*] = CV.fromJSValue(closure);
    sp.* += 1;
    return 0;
}

/// Sync CV locals → JSValue locals_jsv (before calls, so closures see current values).
export fn llvm_rt_sync_locals_to(locals: [*]CV, locals_jsv: [*]JSValue, local_count: u32) callconv(.c) void {
    const lc: usize = @intCast(local_count);
    for (0..lc) |i| {
        locals_jsv[i] = CV.toJSValuePtr(&locals[i]);
    }
}

/// Sync JSValue locals_jsv → CV locals (after calls, so parent sees closure modifications).
export fn llvm_rt_sync_locals_from(locals: [*]CV, locals_jsv: [*]JSValue, local_count: u32) callconv(.c) void {
    const lc: usize = @intCast(local_count);
    for (0..lc) |i| {
        locals[i] = CV.fromJSValue(locals_jsv[i]);
    }
}

/// Detach shared var_refs before function return or tail_call.
export fn llvm_rt_var_ref_list_detach(ctx: *JSContext, var_ref_list: ?*ListHead) callconv(.c) void {
    if (var_ref_list) |vrl| {
        zig_runtime.quickjs.js_frozen_var_ref_list_detach(ctx, vrl);
    }
}
