//! Zig Runtime for Frozen Functions
//!
//! This module provides:
//! - JSValue wrapper type matching QuickJS layout
//! - Inline SMI (Small Integer) arithmetic helpers
//! - FFI bindings to QuickJS for non-primitive operations
//!
//! Design goals:
//! - Zero overhead for SMI operations (inline, no function calls)
//! - Type-safe JSValue handling in Zig
//! - Compatible with QuickJS ABI for FFI calls
//!
//! Module structure (split for maintainability):
//! - js_types.zig: Core types and tag constants
//! - js_value.zig: JSValue, CompressedValue, QuickJS FFI
//! - js_ops.zig: Arithmetic and comparison operations
//! - frozen_helpers.zig: Stack management, iterators, fallback, atoms
//! - fast_array.zig: TypedArray and fast array access
//! - string_pool.zig: String interning

const std = @import("std");
const builtin = @import("builtin");

// ============================================================================
// Module Imports and Re-exports
// ============================================================================

// Core types
pub const types = @import("js_types.zig");
pub const is_wasm32 = types.is_wasm32;
pub const heap_base = &types.heap_base;
pub const compressPtr = types.compressPtr;
pub const decompressPtr = types.decompressPtr;

// Tag constants
pub const JS_TAG_FIRST = types.JS_TAG_FIRST;
pub const JS_TAG_BIG_INT = types.JS_TAG_BIG_INT;
pub const JS_TAG_SYMBOL = types.JS_TAG_SYMBOL;
pub const JS_TAG_STRING = types.JS_TAG_STRING;
pub const JS_TAG_MODULE = types.JS_TAG_MODULE;
pub const JS_TAG_FUNCTION_BYTECODE = types.JS_TAG_FUNCTION_BYTECODE;
pub const JS_TAG_OBJECT = types.JS_TAG_OBJECT;
pub const JS_TAG_INT = types.JS_TAG_INT;
pub const JS_TAG_BOOL = types.JS_TAG_BOOL;
pub const JS_TAG_NULL = types.JS_TAG_NULL;
pub const JS_TAG_UNDEFINED = types.JS_TAG_UNDEFINED;
pub const JS_TAG_UNINITIALIZED = types.JS_TAG_UNINITIALIZED;
pub const JS_TAG_CATCH_OFFSET = types.JS_TAG_CATCH_OFFSET;
pub const JS_TAG_EXCEPTION = types.JS_TAG_EXCEPTION;
pub const JS_TAG_SHORT_BIG_INT = types.JS_TAG_SHORT_BIG_INT;
pub const JS_TAG_FLOAT64 = types.JS_TAG_FLOAT64;

// Opaque types
pub const JSContext = types.JSContext;
pub const JSRuntime = types.JSRuntime;
pub const JSValueUnion = types.JSValueUnion;

// JSValue and related types
pub const js_value = @import("js_value.zig");
pub const JSValue = js_value.JSValue;
pub const JSVarRef = js_value.JSVarRef;
pub const ListHead = js_value.ListHead;
pub const CompressedValue = js_value.CompressedValue;
pub const quickjs = js_value.quickjs;
pub const compressed_heap_base = &js_value.compressed_heap_base;
pub const initCompressedHeap = js_value.initCompressedHeap;
// Promise helpers for async/await support
pub const isThenable = js_value.isThenable;
pub const toPromise = js_value.toPromise;
pub const promiseThen = js_value.promiseThen;
pub const promiseReject = js_value.promiseReject;
pub const FrozenAsyncState = js_value.FrozenAsyncState;

/// Create an async resume callback that calls the target function with state pointer and resolved value
/// data[0] = target function to call
/// data[1] = state pointer (as JSValue int64)
/// When the callback is invoked with (resolved_value), it calls: target_func(state_ptr, resolved_value)
pub fn createAsyncResumeCallback(ctx: *JSContext, target_func: JSValue, state_ptr: JSValue) JSValue {
    var data = [_]JSValue{ target_func, state_ptr };
    return quickjs.JS_NewCFunctionData(
        ctx,
        &asyncResumeCallbackImpl,
        1, // length (expects 1 argument: resolved value)
        0, // magic (unused)
        2, // data_len
        &data,
    );
}

/// Implementation of the async resume callback
fn asyncResumeCallbackImpl(
    ctx: *JSContext,
    this_val: JSValue,
    argc: c_int,
    argv: [*]JSValue,
    magic: c_int,
    func_data: [*]JSValue,
) callconv(.c) JSValue {
    _ = this_val;
    _ = magic;

    // Extract target function and state pointer from closure data
    const target_func = func_data[0];
    const state_ptr = func_data[1];

    // Get the resolved value from arguments (or undefined if not provided)
    const resolved_value = if (argc > 0) argv[0] else JSValue.UNDEFINED;

    // Call target function with (state_ptr, resolved_value)
    var call_args = [_]JSValue{ state_ptr, resolved_value };
    return quickjs.JS_Call(ctx, target_func, JSValue.UNDEFINED, 2, &call_args);
}
// Global return slot for WASM32 - exported so native_dispatch can read from it
pub const g_return_slot = &js_value.g_return_slot;
// Split return slots for WASM32 - reading two u32s avoids any u64 operations
pub const g_return_slot_lo = &js_value.g_return_slot_lo;
pub const g_return_slot_hi = &js_value.g_return_slot_hi;

// Short alias for CompressedValue (used by generated code)
pub const CV = CompressedValue;

// Arithmetic and comparison operations
const js_ops = @import("js_ops.zig");
pub const add = js_ops.add;
pub const sub = js_ops.sub;
pub const mul = js_ops.mul;
pub const div = js_ops.div;
pub const mod = js_ops.mod;
pub const neg = js_ops.neg;
pub const lt = js_ops.lt;
pub const lte = js_ops.lte;
pub const gt = js_ops.gt;
pub const gte = js_ops.gte;
pub const eq = js_ops.eq;
pub const neq = js_ops.neq;
pub const bitAnd = js_ops.bitAnd;
pub const bitOr = js_ops.bitOr;
pub const bitXor = js_ops.bitXor;
pub const bitNot = js_ops.bitNot;
pub const shl = js_ops.shl;
pub const sar = js_ops.sar;
pub const shr = js_ops.shr;

// FFI helpers
pub const jsCall = js_ops.jsCall;
pub const jsGetPropertyStr = js_ops.jsGetPropertyStr;
pub const jsSetPropertyStr = js_ops.jsSetPropertyStr;
pub const jsGetPropertyUint32 = js_ops.jsGetPropertyUint32;
pub const jsSetPropertyUint32 = js_ops.jsSetPropertyUint32;
pub const jsToBool = js_ops.jsToBool;
pub const jsToInt32 = js_ops.jsToInt32;
pub const jsToFloat64 = js_ops.jsToFloat64;
pub const jsNewObject = js_ops.jsNewObject;
pub const jsNewArray = js_ops.jsNewArray;
pub const jsNewString = js_ops.jsNewString;
pub const jsThrowTypeError = js_ops.jsThrowTypeError;
pub const jsThrowRangeError = js_ops.jsThrowRangeError;
pub const jsThrowReferenceError = js_ops.jsThrowReferenceError;
pub const jsGetGlobal = js_ops.jsGetGlobal;
pub const jsDefinePropertyStr = js_ops.jsDefinePropertyStr;
pub const jsCallConstructor = js_ops.jsCallConstructor;
pub const jsThrow = js_ops.jsThrow;
pub const jsIsInstanceOf = js_ops.jsIsInstanceOf;
pub const jsFreeValue = js_ops.jsFreeValue;
pub const jsDupValue = js_ops.jsDupValue;
pub const getVarRef = js_ops.getVarRef;

// ============================================================================
// Constructor Call Detection
// ============================================================================

/// Check if function was called as constructor (with 'new').
/// In QuickJS, when called as constructor, this_val is new.target (a function).
/// When called normally, this_val is the actual this value (undefined/object).
pub inline fn isConstructorCall(ctx: *JSContext, this_val: JSValue) bool {
    // When called as constructor, this_val is new.target which is a function/constructor
    return quickjs.JS_IsFunction(ctx, this_val) != 0;
}

// ============================================================================
// Strict Equality with FFI for Reference Types
// ============================================================================

/// Strict equality comparison that properly handles strings and objects through FFI.
/// This is needed because CompressedValue.strictEq only compares bits, which doesn't
/// work for two different string objects with the same content.
pub inline fn strictEqWithCtx(ctx: *JSContext, a: CompressedValue, b: CompressedValue) CompressedValue {
    // Fast path: identical bits (platform-independent comparison)
    const a_eq_b = if (comptime is_wasm32)
        (a.lo == b.lo and a.hi == b.hi)
    else
        (a.bits == b.bits);

    if (a_eq_b) {
        // Exception: NaN !== NaN
        if (a.isFloat()) {
            const f = a.getFloat();
            if (std.math.isNan(f)) return CompressedValue.FALSE;
        }
        return CompressedValue.TRUE;
    }

    // For reference types (strings, objects), use FFI for proper comparison
    if (a.isRefType() or b.isRefType()) {
        return if (quickjs.JS_IsStrictEqual(ctx, a.toJSValue(), b.toJSValue()))
            CompressedValue.TRUE
        else
            CompressedValue.FALSE;
    }

    // Numeric comparison: int and float with same value should be equal
    // In JavaScript, 14 === 14.0 is true because both are Number type
    if (a.isInt() and b.isInt()) {
        return if (a.getInt() == b.getInt()) CompressedValue.TRUE else CompressedValue.FALSE;
    }
    if (a.isFloat() and b.isFloat()) {
        const fa = a.getFloat();
        const fb = b.getFloat();
        // NaN !== NaN
        if (std.math.isNan(fa) or std.math.isNan(fb)) return CompressedValue.FALSE;
        return if (fa == fb) CompressedValue.TRUE else CompressedValue.FALSE;
    }
    if (a.isInt() and b.isFloat()) {
        const fb = b.getFloat();
        if (std.math.isNan(fb)) return CompressedValue.FALSE;
        return if (@as(f64, @floatFromInt(a.getInt())) == fb) CompressedValue.TRUE else CompressedValue.FALSE;
    }
    if (a.isFloat() and b.isInt()) {
        const fa = a.getFloat();
        if (std.math.isNan(fa)) return CompressedValue.FALSE;
        return if (fa == @as(f64, @floatFromInt(b.getInt()))) CompressedValue.TRUE else CompressedValue.FALSE;
    }

    // Value types: different bits and not numeric means not equal
    return CompressedValue.FALSE;
}

/// Strict inequality with proper FFI for reference types
pub inline fn strictNeqWithCtx(ctx: *JSContext, a: CompressedValue, b: CompressedValue) CompressedValue {
    const result = strictEqWithCtx(ctx, a, b);
    const is_true = if (comptime is_wasm32)
        (result.lo == CompressedValue.TRUE.lo and result.hi == CompressedValue.TRUE.hi)
    else
        (result.bits == CompressedValue.TRUE.bits);
    return if (is_true) CompressedValue.FALSE else CompressedValue.TRUE;
}

// Frozen function helpers
const frozen_helpers = @import("frozen_helpers.zig");
pub const FROZEN_MAX_CALL_DEPTH = frozen_helpers.FROZEN_MAX_CALL_DEPTH;
pub const frozen_call_depth = &frozen_helpers.frozen_call_depth;
pub const checkStack = frozen_helpers.checkStack;
pub const exitStack = frozen_helpers.exitStack;
pub const frozen_reset_call_depth_zig = frozen_helpers.frozen_reset_call_depth_zig;

// Closure variable access
pub const getClosureVar = frozen_helpers.getClosureVar;
pub const setClosureVar = frozen_helpers.setClosureVar;
pub const getClosureVarCheck = frozen_helpers.getClosureVarCheck;
pub const setClosureVarCheck = frozen_helpers.setClosureVarCheck;
pub const getClosureVarSafe = frozen_helpers.getClosureVarSafe;
pub const setClosureVarSafe = frozen_helpers.setClosureVarSafe;
pub const setClosureVarDupSafe = frozen_helpers.setClosureVarDupSafe;
pub const getClosureVarCheckSafe = frozen_helpers.getClosureVarCheckSafe;
pub const setClosureVarCheckSafe = frozen_helpers.setClosureVarCheckSafe;

// Iterator helpers
pub const newCatchOffset = frozen_helpers.newCatchOffset;
pub const forOfStart = frozen_helpers.forOfStart;
pub const forOfNext = frozen_helpers.forOfNext;
pub const iteratorClose = frozen_helpers.iteratorClose;
pub const iteratorIsDone = frozen_helpers.iteratorIsDone;
pub const iteratorGetValue = frozen_helpers.iteratorGetValue;

// Object operations
pub const copyDataProperties = frozen_helpers.copyDataProperties;

// ============================================================================
// Rest Parameter Helper
// ============================================================================

/// Create an array from function arguments starting at first_arg_index
/// Used for rest parameters: function foo(a, b, ...rest) { }
/// When called with foo(1, 2, 3, 4, 5), rest = [3, 4, 5]
pub fn makeRestArray(ctx: *JSContext, argc: c_int, argv: [*]JSValue, first_arg_index: u32) JSValue {
    const rest_count = if (argc > @as(c_int, @intCast(first_arg_index)))
        @as(usize, @intCast(argc - @as(c_int, @intCast(first_arg_index))))
    else
        0;

    // Create a new array with the rest elements
    const arr = quickjs.JS_NewArray(ctx);
    if (arr.isException()) return arr;

    // Copy arguments starting from first_arg_index
    for (0..rest_count) |i| {
        const arg_idx = first_arg_index + @as(u32, @intCast(i));
        const val = argv[arg_idx];
        // Dup the value since array will own it
        const duped = quickjs.JS_DupValue(ctx, val);
        _ = quickjs.JS_SetPropertyUint32(ctx, arr, @intCast(i), duped);
    }

    return arr;
}

// Block fallback
pub const frozen_block_fallback = frozen_helpers.frozen_block_fallback;
pub const blockFallback = frozen_helpers.blockFallback;
pub const blockFallbackCV = frozen_helpers.blockFallbackCV;

// Native shape access
pub const NativeAstNode = frozen_helpers.NativeAstNode;
pub const jsvalueToAddr = frozen_helpers.jsvalueToAddr;
pub const native_node_lookup = frozen_helpers.native_node_lookup;
pub const nativeGetKind = frozen_helpers.nativeGetKind;
pub const nativeGetFlags = frozen_helpers.nativeGetFlags;
pub const nativeGetPos = frozen_helpers.nativeGetPos;
pub const nativeGetEnd = frozen_helpers.nativeGetEnd;
pub const nativeGetParent = frozen_helpers.nativeGetParent;
pub const isNativeProperty = frozen_helpers.isNativeProperty;
pub const nativeGetLength = frozen_helpers.nativeGetLength;
pub const nativeGetLengthCV = frozen_helpers.nativeGetLengthCV;

// Cached atoms
pub const cached_atoms = &frozen_helpers.cached_atoms;
pub const CachedAtoms = frozen_helpers.CachedAtoms;
pub const initCachedAtoms = frozen_helpers.initCachedAtoms;
pub const nativeGetSymbol = frozen_helpers.nativeGetSymbol;
pub const nativeGetEscapedName = frozen_helpers.nativeGetEscapedName;
pub const nativeGetDeclarations = frozen_helpers.nativeGetDeclarations;
pub const nativeGetValueDeclaration = frozen_helpers.nativeGetValueDeclaration;
pub const nativeGetMembers = frozen_helpers.nativeGetMembers;
pub const nativeGetProperties = frozen_helpers.nativeGetProperties;
pub const nativeGetTarget = frozen_helpers.nativeGetTarget;
pub const nativeGetConstraint = frozen_helpers.nativeGetConstraint;
pub const nativeGetModifiers = frozen_helpers.nativeGetModifiers;
pub const nativeGetName = frozen_helpers.nativeGetName;
pub const nativeGetText = frozen_helpers.nativeGetText;
pub const nativeGetType = frozen_helpers.nativeGetType;
pub const nativeGetChecker = frozen_helpers.nativeGetChecker;
pub const nativeGetTypeArguments = frozen_helpers.nativeGetTypeArguments;
pub const nativeGetArguments = frozen_helpers.nativeGetArguments;

// Fast array access
const fast_array = @import("fast_array.zig");
pub const TypedArraySumResult = fast_array.TypedArraySumResult;
pub const sumTypedArrayFast = fast_array.sumTypedArrayFast;
pub const JS_CLASS_ARRAY = fast_array.JS_CLASS_ARRAY;
pub const JS_CLASS_ARGUMENTS = fast_array.JS_CLASS_ARGUMENTS;
pub const JSObject = fast_array.JSObject;
pub const FastArrayResult = fast_array.FastArrayResult;
pub const getFastArrayDirect = fast_array.getFastArrayDirect;
pub const jsValueToInt32Inline = fast_array.jsValueToInt32Inline;
pub const jsValueToInt64Inline = fast_array.jsValueToInt64Inline;

// String pool
const string_pool = @import("string_pool.zig");
pub const StringPool = string_pool.StringPool;
pub const getGlobalStringPool = string_pool.getGlobalStringPool;
pub const deinitGlobalStringPool = string_pool.deinitGlobalStringPool;

// ============================================================================
// Tests
// ============================================================================

test "JSValue size" {
    // WASM32 uses 8-byte NaN-boxing, native uses 16-byte struct
    if (is_wasm32) {
        try std.testing.expectEqual(@sizeOf(JSValue), 8);
    } else {
        try std.testing.expectEqual(@sizeOf(JSValue), 16);
        try std.testing.expectEqual(@sizeOf(JSValueUnion), 8);
    }
}

test "JSValue constants" {
    try std.testing.expect(JSValue.UNDEFINED.isUndefined());
    try std.testing.expect(JSValue.NULL.isNull());
    try std.testing.expect(JSValue.TRUE.isBool());
    try std.testing.expect(JSValue.FALSE.isBool());
    try std.testing.expect(JSValue.TRUE.getBool());
    try std.testing.expect(!JSValue.FALSE.getBool());
}

test "JSValue int operations" {
    const a = JSValue.newInt(42);
    try std.testing.expect(a.isInt());
    try std.testing.expectEqual(a.getInt(), 42);
    try std.testing.expect(!a.hasRefCount());
}

test "JSValue refcount" {
    // Primitives don't have refcount
    try std.testing.expect(!JSValue.newInt(1).hasRefCount());
    try std.testing.expect(!JSValue.newBool(true).hasRefCount());
    try std.testing.expect(!JSValue.UNDEFINED.hasRefCount());
    try std.testing.expect(!JSValue.NULL.hasRefCount());

    // Objects/strings would have refcount (tag < 0)
    const obj_like = JSValue{ .u = .{ .ptr = null }, .tag = JS_TAG_OBJECT };
    try std.testing.expect(obj_like.hasRefCount());

    const str_like = JSValue{ .u = .{ .ptr = null }, .tag = JS_TAG_STRING };
    try std.testing.expect(str_like.hasRefCount());
}

test "StringPool intern" {
    var pool = StringPool.init(std.testing.allocator);
    defer pool.deinit();

    const s1 = try pool.intern("hello");
    const s2 = try pool.intern("hello");
    const s3 = try pool.intern("world");

    // Same string returns same pointer
    try std.testing.expectEqual(s1.ptr, s2.ptr);
    // Different strings are different
    try std.testing.expect(s1.ptr != s3.ptr);
    // Content is correct
    try std.testing.expectEqualStrings("hello", s1);
    try std.testing.expectEqualStrings("world", s3);
}
