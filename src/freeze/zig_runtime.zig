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
    return quickjs.JS_IsFunction(ctx, this_val);
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
        (a.u == b.u and a.tag == b.tag);

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
        (result.u == CompressedValue.TRUE.u and result.tag == CompressedValue.TRUE.tag);
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
// Profiling Counters (shared with js_value.zig via profile.zig)
// ============================================================================

const profile = @import("profile.zig");
pub const PROFILE = profile.PROFILE;
pub const prof = profile.prof;
pub const cycles = profile.cycles;
pub const vinc = profile.vinc;

extern fn js_frozen_try_call_stats() void;
extern fn js_frozen_dump_offsets() void;

pub fn printProfile() void {
    profile.prof.print();
    js_frozen_try_call_stats();
    js_frozen_dump_offsets();
}

// ============================================================================
// Property Access Helper (with exception propagation)
// ============================================================================

pub const GetFieldError = error{JsException};

/// Get a property by name, returning an error if the property access throws.
/// Used by frozen codegen to propagate exceptions from get_field operations
/// (e.g., accessing a property on undefined/null).
pub inline fn getFieldChecked(ctx: *JSContext, obj: JSValue, name: [*:0]const u8) GetFieldError!JSValue {
    if (PROFILE) vinc(prof.getfield_calls(), 1);
    const t0 = if (PROFILE) cycles() else 0;
    const result = JSValue.getPropertyStr(ctx, obj, name);
    if (PROFILE) vinc(prof.getfield_cycles(), cycles() - t0);
    if (result.isException()) return error.JsException;
    return result;
}

// ============================================================================
// Inline Cache for Property Access
// ============================================================================

/// Polymorphic inline cache slot (4-way) for a single property access site.
/// Stores up to 4 cached {shape, offset} pairs plus a resolved atom.
/// Must be initialized to zeroes (all shapes null, count 0).
///
/// Layout (56 bytes): {shapes[4]ptr, offsets[4]u32, atom u32, count u32}
/// Byte offsets: shapes=0,8,16,24  offsets=32,36,40,44  atom=48  count=52
pub const ICSlot = extern struct {
    shape0: ?*anyopaque = null,
    shape1: ?*anyopaque = null,
    shape2: ?*anyopaque = null,
    shape3: ?*anyopaque = null,
    offset0: u32 = 0,
    offset1: u32 = 0,
    offset2: u32 = 0,
    offset3: u32 = 0,
    atom: u32 = 0,
    count: u32 = 0,

    pub const ZERO = ICSlot{};
};

/// Load a property using a polymorphic inline cache slot (4-way).
/// Fast path (IC hit): O(1) direct property read entirely in Zig — no C FFI call.
/// Checks all 4 cached shapes before falling back to C slow path.
///
/// JSObject layout on 64-bit native (from quickjs.c):
///   offset 24: JSShape *shape
///   offset 32: JSProperty *prop (array, each entry = 16 bytes = sizeof(JSValue))
pub inline fn icLoad(ctx: *JSContext, obj: JSValue, ic: *ICSlot, name: [*:0]const u8) GetFieldError!JSValue {
    if (PROFILE) vinc(prof.getfield_calls(), 1);
    const t0 = if (PROFILE) cycles() else 0;
    // IC fast path: check all 4 shapes for a match
    if (!is_wasm32 and obj.tag == JS_TAG_OBJECT) {
        const obj_bytes: [*]const u8 = @ptrCast(obj.u.ptr.?);
        const shape_ptr: *const ?*anyopaque = @ptrCast(@alignCast(obj_bytes + 24));
        const shape = shape_ptr.*;
        if (shape != null) {
            const shapes = [4]?*anyopaque{ ic.shape0, ic.shape1, ic.shape2, ic.shape3 };
            const offsets = [4]u32{ ic.offset0, ic.offset1, ic.offset2, ic.offset3 };
            // Read shape->prop_count at offset 40 for bounds validation
            const shape_bytes_for_count: [*]const u8 = @ptrCast(shape.?);
            const shape_prop_count: u32 = @as(*const u32, @ptrCast(@alignCast(shape_bytes_for_count + 40))).*;
            inline for (0..4) |i| {
                if (shapes[i] == shape and
                    offsets[i] < shape_prop_count)
                {
                    const shape_bytes: [*]const u8 = @ptrCast(shape.?);
                    const shape_prop_atom: *const u32 = @ptrCast(@alignCast(shape_bytes + 64 + @as(usize, offsets[i]) * 8 + 4));
                    if (ic.atom == 0 or shape_prop_atom.* == ic.atom) {
                        if (PROFILE) vinc(prof.ic_hits(), 1);
                        if (PROFILE) vinc(prof.getfield_cycles(), cycles() - t0);
                        const prop_base: *const [*]const u8 = @ptrCast(@alignCast(obj_bytes + 32));
                        const prop_value: *const JSValue = @ptrCast(@alignCast(prop_base.* + @as(usize, offsets[i]) * 16));
                        return JSValue.dup(ctx, prop_value.*);
                    }
                }
            }
        }
    }
    if (PROFILE) vinc(prof.ic_misses(), 1);
    // Slow path: C function resolves atom, populates next IC slot, returns value
    const result = quickjs.js_frozen_ic_load(ctx, obj, @ptrCast(ic), name);
    if (PROFILE) vinc(prof.getfield_cycles(), cycles() - t0);
    if (result.isException()) return error.JsException;
    return result;
}

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
pub const nativeSetFlags = frozen_helpers.nativeSetFlags;
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

// ============================================================================
// Exception cleanup: free ref-type locals (noinline to reduce code size)
// ============================================================================

pub noinline fn cleanupLocals(ctx: *JSContext, locals_ptr: [*]CompressedValue, count: usize) void {
    for (0..count) |i| {
        const v = locals_ptr[i];
        if (v.isRefType()) {
            quickjs.JS_FreeValue(ctx, v.toJSValue());
        }
    }
}

// ============================================================================
// Thin Codegen Helpers: Stack-Direct Operations for Cold Functions
// DEBUG: JSValue tag corruption detection helpers
pub inline fn jsvalueTagCorrupt(jsv: *const JSValue) bool {
    const tag = jsvalueRawTag(jsv);
    // JS_TAG_FIRST = -9 (BIG_INT), JS_TAG_FLOAT64 = 8 (max valid tag)
    return tag > 8 or tag < -9;
}

pub inline fn jsvalueRawTag(jsv: *const JSValue) i64 {
    const ptr: [*]const u8 = @ptrCast(jsv);
    // JSValue layout: { value: u64 (bytes 0-7), tag: i64 (bytes 8-15) }
    return @as(*align(1) const i64, @ptrCast(ptr + 8)).*;
}

// ============================================================================
//
// These helpers operate directly on a [*]CV stack + *usize sp pointer.
// Used by ThinCodeGen for the "cold" tier — one call per opcode, no vstack.
//
// Design:
//   - `inline` helpers: simple ops (2-10 machine instructions, no JS FFI)
//     → compiler inlines them, so no actual call overhead
//   - `noinline` helpers: complex ops at JS FFI boundary (call, get_field, etc.)
//     → kept out-of-line to minimize code size per frozen function

pub const thin = struct {
    // Uses module-level CV (= CompressedValue) alias — no redeclaration needed

    // ================================================================
    // Inline Helpers — Constants
    // ================================================================

    pub inline fn push_i32(stack: [*]CV, sp: *usize, val: i32) void {
        stack[sp.*] = CV.newInt(val);
        sp.* += 1;
    }

    pub inline fn push_true(stack: [*]CV, sp: *usize) void {
        stack[sp.*] = CV.TRUE;
        sp.* += 1;
    }

    pub inline fn push_false(stack: [*]CV, sp: *usize) void {
        stack[sp.*] = CV.FALSE;
        sp.* += 1;
    }

    pub inline fn push_null(stack: [*]CV, sp: *usize) void {
        stack[sp.*] = CV.NULL;
        sp.* += 1;
    }

    pub inline fn push_undefined(stack: [*]CV, sp: *usize) void {
        stack[sp.*] = CV.UNDEFINED;
        sp.* += 1;
    }

    pub inline fn push_cv(stack: [*]CV, sp: *usize, val: CV) void {
        stack[sp.*] = val;
        sp.* += 1;
    }

    // ================================================================
    // Inline Helpers — Local Variable Access
    // ================================================================

    /// get_loc: push a copy of local onto stack (dup ref-types for ownership)
    pub inline fn get_loc(_: *JSContext, stack: [*]CV, sp: *usize, locals: [*]CV, idx: u32) void {
        const v = locals[idx];
        stack[sp.*] = CV.dupRef(v);
        sp.* += 1;
    }

    /// put_loc: transfer ownership from stack → local, free old local
    pub inline fn put_loc(ctx: *JSContext, stack: [*]CV, sp: *usize, locals: [*]CV, idx: u32) void {
        sp.* -= 1;
        const old = locals[idx];
        locals[idx] = stack[sp.*];
        CV.freeRef(ctx, old);
    }

    /// set_loc: copy stack top → local (keep value on stack), free old local
    pub inline fn set_loc(ctx: *JSContext, stack: [*]CV, sp: *usize, locals: [*]CV, idx: u32) void {
        const v = stack[sp.* - 1];
        const old = locals[idx];
        locals[idx] = CV.dupRef(v);
        CV.freeRef(ctx, old);
    }

    // ================================================================
    // Inline Helpers — Argument Access
    // ================================================================

    /// get_arg: push argument value from argv (with dup for ownership)
    pub inline fn get_arg(ctx: *JSContext, stack: [*]CV, sp: *usize, argc: c_int, argv: [*]JSValue, idx: u32) void {
        stack[sp.*] = CV.fromJSValue(if (idx < argc) JSValue.dup(ctx, argv[idx]) else JSValue.UNDEFINED);
        sp.* += 1;
    }

    /// get_arg from arg_shadow (when function modifies arguments)
    pub inline fn get_arg_shadow(stack: [*]CV, sp: *usize, arg_shadow: [*]CV, idx: u32) void {
        stack[sp.*] = CV.dupRef(arg_shadow[idx]);
        sp.* += 1;
    }

    /// put_arg: transfer ownership from stack → arg_shadow
    pub inline fn put_arg(ctx: *JSContext, stack: [*]CV, sp: *usize, arg_shadow: [*]CV, idx: u32) void {
        sp.* -= 1;
        const old = arg_shadow[idx];
        arg_shadow[idx] = stack[sp.*];
        CV.freeRef(ctx, old);
    }

    /// set_arg: copy stack top → arg_shadow (keep value on stack), free old entry
    pub inline fn set_arg(ctx: *JSContext, stack: [*]CV, sp: *usize, arg_shadow: [*]CV, idx: u32) void {
        const v = stack[sp.* - 1];
        const old = arg_shadow[idx];
        arg_shadow[idx] = CV.dupRef(v);
        CV.freeRef(ctx, old);
    }

    // ================================================================
    // Inline Helpers — Closure Variable Access
    // ================================================================

    pub inline fn get_var_ref(ctx: *JSContext, stack: [*]CV, sp: *usize, var_refs: ?[*]*JSVarRef, idx: u32, closure_var_count: c_int) void {
        stack[sp.*] = CV.fromJSValue(frozen_helpers.getClosureVarSafe(ctx, var_refs, idx, closure_var_count));
        sp.* += 1;
    }

    pub inline fn put_var_ref(ctx: *JSContext, stack: [*]CV, sp: *usize, var_refs: ?[*]*JSVarRef, idx: u32, closure_var_count: c_int) void {
        sp.* -= 1;
        frozen_helpers.setClosureVarSafe(ctx, var_refs, idx, closure_var_count, stack[sp.*].toJSValueWithCtx(ctx));
    }

    pub inline fn set_var_ref(ctx: *JSContext, stack: [*]CV, sp: *usize, var_refs: ?[*]*JSVarRef, idx: u32, closure_var_count: c_int) void {
        // set_var_ref: keep value on stack AND store in var_ref (dup needed)
        frozen_helpers.setClosureVarDupSafe(ctx, var_refs, idx, closure_var_count, stack[sp.* - 1].toJSValueWithCtx(ctx));
    }

    // ================================================================
    // Inline Helpers — Stack Operations
    // ================================================================

    pub inline fn dup_top(_: *JSContext, stack: [*]CV, sp: *usize) void {
        const v = stack[sp.* - 1];
        stack[sp.*] = CV.dupRef(v);
        sp.* += 1;
    }

    pub inline fn dup1(_: *JSContext, stack: [*]CV, sp: *usize) void {
        // [a, b] -> [a, b, a]
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 1] = CV.dupRef(a);
        stack[sp.*] = b;
        sp.* += 1;
    }

    pub inline fn dup2(_: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.*] = CV.dupRef(a);
        stack[sp.* + 1] = CV.dupRef(b);
        sp.* += 2;
    }

    pub inline fn dup3(_: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 3];
        const b = stack[sp.* - 2];
        const c = stack[sp.* - 1];
        stack[sp.*] = CV.dupRef(a);
        stack[sp.* + 1] = CV.dupRef(b);
        stack[sp.* + 2] = CV.dupRef(c);
        sp.* += 3;
    }

    pub inline fn drop(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        sp.* -= 1;
        CV.freeRef(ctx, stack[sp.*]);
    }

    pub inline fn nip(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        // [a, b] -> [b]  (remove second-from-top, keep top)
        const old = stack[sp.* - 2];
        stack[sp.* - 2] = stack[sp.* - 1];
        sp.* -= 1;
        CV.freeRef(ctx, old);
    }

    pub inline fn swap(stack: [*]CV, sp: *usize) void {
        const tmp = stack[sp.* - 1];
        stack[sp.* - 1] = stack[sp.* - 2];
        stack[sp.* - 2] = tmp;
    }

    // ================================================================
    // Inline Helpers — Arithmetic (delegate to CV methods)
    // ================================================================

    pub inline fn op_add(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.addWithCtx(ctx, a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_sub(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.sub(a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_mul(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.mul(a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_div(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.div(a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_mod(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.mod(a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_pow(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.pow(a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_neg(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const old = stack[sp.* - 1];
        stack[sp.* - 1] = CV.sub(CV.newInt(0), old);
        CV.freeRef(ctx, old);
    }

    pub inline fn op_plus(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        stack[sp.* - 1] = stack[sp.* - 1].toNumberWithCtx(ctx);
    }

    // ================================================================
    // Inline Helpers — Bitwise
    // ================================================================

    pub inline fn op_band(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.band(a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_bor(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.bor(a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_bxor(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.bxor(a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_bnot(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const old = stack[sp.* - 1];
        stack[sp.* - 1] = CV.bnot(old);
        CV.freeRef(ctx, old);
    }

    pub inline fn op_shl(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.shl(a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_sar(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.sar(a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_shr(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.ushr(a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    // ================================================================
    // Inline Helpers — Comparison
    // ================================================================

    pub inline fn op_lt(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.lt(a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_lte(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.lte(a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_gt(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.gt(a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_gte(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.gte(a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_eq(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.eqWithCtx(ctx, a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_neq(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = CV.neqWithCtx(ctx, a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_strict_eq(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = strictEqWithCtx(ctx, a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    pub inline fn op_strict_neq(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const a = stack[sp.* - 2];
        const b = stack[sp.* - 1];
        stack[sp.* - 2] = strictNeqWithCtx(ctx, a, b);
        CV.freeRef(ctx, a);
        CV.freeRef(ctx, b);
        sp.* -= 1;
    }

    // ================================================================
    // Inline Helpers — Logical / Type
    // ================================================================

    pub inline fn op_lnot(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const v = stack[sp.* - 1];
        stack[sp.* - 1] = if (v.toBoolWithCtx(ctx)) CV.FALSE else CV.TRUE;
        CV.freeRef(ctx, v); // free old value (may be ref-type object/string)
    }

    pub inline fn op_typeof(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const v = stack[sp.* - 1];
        stack[sp.* - 1] = CV.fromJSValue(JSValue.typeOf(ctx, v.toJSValueWithCtx(ctx)));
        CV.freeRef(ctx, v); // free old value (typeof consumes it)
    }

    // ================================================================
    // Inline Helpers — Increment/Decrement Locals
    // ================================================================

    pub inline fn inc_loc(locals: [*]CV, idx: u32) void {
        locals[idx] = CV.add(locals[idx], CV.newInt(1));
    }

    pub inline fn dec_loc(locals: [*]CV, idx: u32) void {
        locals[idx] = CV.sub(locals[idx], CV.newInt(1));
    }

    // ================================================================
    // Noinline Helpers — Function Calls (JS FFI boundary)
    // ================================================================

    /// Call a function: stack has [func, arg0, arg1, ...argN-1] → [result]
    /// Handles closure sync TO/FROM internally.
    pub noinline fn op_call(
        ctx: *JSContext,
        stack: [*]CV,
        sp: *usize,
        argc: u16,
        locals: ?[*]CV,
        locals_jsv: ?[*]JSValue,
        var_count: usize,
        closure_alive: ?*bool,
        captured_indices: []const u16,
    ) GetFieldError!void {
        const n: usize = @intCast(argc);
        const func_idx = sp.* - n - 1;
        const fn_val = stack[func_idx].toJSValue();

        // Sync locals before call (noop if no closures)
        syncLocalsTo(locals, locals_jsv, var_count);

        // On native, CV=JSValue — pass stack args directly to JS_Call (zero copy).
        // JS_Call takes JSValueConst* (does NOT modify or consume argv).
        const result = if (comptime !is_wasm32) blk: {
            const argv_ptr: [*]JSValue = @ptrCast(&stack[sp.* - n]);
            break :blk JSValue.call(ctx, fn_val, JSValue.UNDEFINED, @intCast(argc), argv_ptr);
        } else blk: {
            // WASM32: CV != JSValue, must convert
            if (n > 0) {
                var args: [256]JSValue = undefined;
                for (0..n) |i| {
                    args[i] = CV.toJSValuePtr(&stack[sp.* - n + i]);
                }
                break :blk JSValue.call(ctx, fn_val, JSValue.UNDEFINED, @intCast(argc), @ptrCast(&args));
            } else {
                break :blk JSValue.call(ctx, fn_val, JSValue.UNDEFINED, 0, @as([*]JSValue, @ptrCast(@constCast(&[0]JSValue{}))));
            }
        };

        // FreeRef all consumed CVs (func + args) directly from stack — no copy needed
        for (func_idx..sp.*) |i| {
            CV.freeRef(ctx, stack[i]);
        }

        if (result.isException()) return error.JsException;
        // Pop func + args, push result
        sp.* = func_idx;
        const result_cv = CV.fromJSValue(result);
        // Debug: detect null ref-counted return value from JS_Call
        if (result_cv.tag < 0 and result_cv.u == 0) {
            std.debug.print("[BUG-CALL] null ref return tag={d} argc={d} sp={d} ra=0x{x}\n", .{ result_cv.tag, argc, sp.*, @returnAddress() });
        }
        stack[sp.*] = result_cv;
        sp.* += 1;
        // Sync closure variables back
        syncLocalsFrom(locals, locals_jsv, var_count, closure_alive, captured_indices);
    }

    /// Call a method: stack has [obj, method, arg0, ...argN-1] → [result]
    pub noinline fn op_call_method(
        ctx: *JSContext,
        stack: [*]CV,
        sp: *usize,
        argc: u16,
        locals: ?[*]CV,
        locals_jsv: ?[*]JSValue,
        var_count: usize,
        closure_alive: ?*bool,
        captured_indices: []const u16,
    ) GetFieldError!void {
        const n: usize = @intCast(argc);
        const obj_idx = sp.* - n - 2;
        const obj = stack[obj_idx].toJSValue();
        const method = stack[obj_idx + 1].toJSValue();

        // Sync locals before call (noop if no closures)
        syncLocalsTo(locals, locals_jsv, var_count);

        // On native, CV=JSValue — pass stack args directly (zero copy).
        const result = if (comptime !is_wasm32) blk: {
            const argv_ptr: [*]JSValue = @ptrCast(&stack[sp.* - n]);
            break :blk JSValue.call(ctx, method, obj, @intCast(argc), argv_ptr);
        } else blk: {
            if (n > 0) {
                var args: [256]JSValue = undefined;
                for (0..n) |i| {
                    args[i] = CV.toJSValuePtr(&stack[sp.* - n + i]);
                }
                break :blk JSValue.call(ctx, method, obj, @intCast(argc), @ptrCast(&args));
            } else {
                break :blk JSValue.call(ctx, method, obj, 0, @as([*]JSValue, @ptrCast(@constCast(&[0]JSValue{}))));
            }
        };

        // FreeRef all consumed CVs (obj + method + args) directly from stack
        for (obj_idx..sp.*) |i| {
            CV.freeRef(ctx, stack[i]);
        }

        if (result.isException()) return error.JsException;
        sp.* = obj_idx;
        stack[sp.*] = CV.fromJSValue(result);
        sp.* += 1;
        syncLocalsFrom(locals, locals_jsv, var_count, closure_alive, captured_indices);
    }

    /// IC-accelerated property load: pop obj, push property value
    pub noinline fn op_get_field_ic(ctx: *JSContext, stack: [*]CV, sp: *usize, ic: *ICSlot, name: [*:0]const u8) GetFieldError!void {
        const obj = stack[sp.* - 1].toJSValueWithCtx(ctx);
        const result = icLoad(ctx, obj, ic, name) catch return error.JsException;
        CV.freeRef(ctx, stack[sp.* - 1]);
        stack[sp.* - 1] = CV.fromJSValue(result);
    }

    /// get_field2: pop obj, push obj AND method (for call_method)
    pub noinline fn op_get_field2_ic(ctx: *JSContext, stack: [*]CV, sp: *usize, ic: *ICSlot, name: [*:0]const u8) GetFieldError!void {
        const obj = stack[sp.* - 1].toJSValueWithCtx(ctx);
        const result = icLoad(ctx, obj, ic, name) catch return error.JsException;
        // obj stays at sp-1, method goes at sp
        stack[sp.*] = CV.fromJSValue(result);
        sp.* += 1;
    }

    /// Put property by name: stack has [obj, value] → [] (pops both)
    pub noinline fn op_put_field(ctx: *JSContext, stack: [*]CV, sp: *usize, name: [*:0]const u8) GetFieldError!void {
        const val = stack[sp.* - 1].toJSValueWithCtx(ctx);
        const obj = stack[sp.* - 2].toJSValueWithCtx(ctx);
        const rc = quickjs.JS_SetPropertyStr(ctx, obj, name, val);
        CV.freeRef(ctx, stack[sp.* - 2]); // free obj
        sp.* -= 2;
        if (rc < 0) return error.JsException;
    }

    /// Get global variable by name
    pub noinline fn op_get_var(ctx: *JSContext, stack: [*]CV, sp: *usize, name: [*:0]const u8) GetFieldError!void {
        const global = quickjs.JS_GetGlobalObject(ctx);
        const result = quickjs.JS_GetPropertyStr(ctx, global, name);
        quickjs.JS_FreeValue(ctx, global);
        if (result.isException()) return error.JsException;
        stack[sp.*] = CV.fromJSValue(result);
        sp.* += 1;
    }

    // ================================================================
    // Noinline Helper — Exception Cleanup (ONE call replaces ~40% code)
    // ================================================================

    /// Combined exception cleanup: sync closures, detach var_refs, free locals, free arg_shadow.
    /// Returns JSValue.EXCEPTION for easy `return rt.exceptionCleanup(...)` pattern.
    pub noinline fn exceptionCleanup(
        ctx: *JSContext,
        locals: [*]CV,
        local_count: usize,
        locals_jsv: ?[*]JSValue,
        var_ref_list: ?*ListHead,
        arg_shadow: ?[*]CV,
        arg_count: usize,
    ) JSValue {
        // 1. Sync locals → _locals_jsv and detach var_refs (if closures active)
        if (locals_jsv) |jsv| {
            for (0..local_count) |i| {
                jsv[i] = CV.toJSValuePtr(&locals[i]);
            }
            if (var_ref_list) |vrl| {
                quickjs.js_frozen_var_ref_list_detach(ctx, vrl);
            }
        }
        // 2. Free ref-type locals
        cleanupLocals(ctx, locals, local_count);
        // 3. Free arg_shadow (if function modifies arguments)
        if (arg_shadow) |shadow| {
            for (0..arg_count) |i| {
                CV.freeRef(ctx, shadow[i]);
            }
        }
        return JSValue.EXCEPTION;
    }

    // ================================================================
    // Internal: Closure Sync Helpers (used by op_call/op_call_method)
    // ================================================================

    inline fn syncLocalsTo(locals: ?[*]CV, locals_jsv: ?[*]JSValue, var_count: usize) void {
        if (locals_jsv) |jsv| {
            if (locals) |locs| {
                for (0..var_count) |i| {
                    jsv[i] = CV.toJSValuePtr(&locs[i]);
                }
            }
        }
    }

    inline fn syncLocalsFrom(locals: ?[*]CV, locals_jsv: ?[*]JSValue, var_count: usize, closure_alive: ?*bool, captured_indices: []const u16) void {
        _ = var_count;
        if (closure_alive) |alive| {
            if (alive.*) {
                if (locals_jsv) |jsv| {
                    if (locals) |locs| {
                        if (captured_indices.len > 0) {
                            for (captured_indices) |idx| {
                                locs[idx] = CV.fromJSValue(jsv[idx]);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Sync a single local to _locals_jsv after put_loc/set_loc (for closure tracking)
    pub inline fn sync_local_jsv(locals: [*]CV, locals_jsv: [*]JSValue, idx: u32) void {
        locals_jsv[idx] = CV.toJSValuePtr(&locals[idx]);
    }

    // ================================================================
    // Noinline Helpers — Return Cleanup
    // ================================================================

    /// Return the top-of-stack value with full cleanup:
    /// dup return value, free CV, detach closures, free locals, free arg_shadow.
    pub noinline fn returnFromStack(
        ctx: *JSContext,
        stack: [*]CV,
        sp: usize,
        locals: [*]CV,
        local_count: usize,
        locals_jsv: ?[*]JSValue,
        var_ref_list: ?*ListHead,
        arg_shadow: ?[*]CV,
        arg_count: usize,
    ) JSValue {
        // Transfer ownership: stack CV → returned JSValue.
        // On native (CV=JSValue), dup+freeRef cancel out — just bitcast.
        // On WASM32, must dup before free (different representations).
        const ret_cv = stack[sp - 1];
        const ret_val = if (comptime !is_wasm32)
            ret_cv.toJSValue()
        else blk: {
            const v = if (ret_cv.isRefType())
                JSValue.dup(ctx, ret_cv.toJSValueWithCtx(ctx))
            else
                ret_cv.toJSValueWithCtx(ctx);
            CV.freeRef(ctx, ret_cv);
            break :blk v;
        };
        // Sync locals → _locals_jsv and detach var_refs
        if (locals_jsv) |jsv| {
            for (0..local_count) |i| {
                jsv[i] = CV.toJSValuePtr(&locals[i]);
            }
            if (var_ref_list) |vrl| {
                quickjs.js_frozen_var_ref_list_detach(ctx, vrl);
            }
        }
        cleanupLocals(ctx, locals, local_count);
        if (arg_shadow) |shadow| {
            for (0..arg_count) |i| {
                CV.freeRef(ctx, shadow[i]);
            }
        }
        return ret_val;
    }

    /// Return UNDEFINED with full cleanup.
    pub noinline fn returnUndef(
        ctx: *JSContext,
        locals: [*]CV,
        local_count: usize,
        locals_jsv: ?[*]JSValue,
        var_ref_list: ?*ListHead,
        arg_shadow: ?[*]CV,
        arg_count: usize,
    ) JSValue {
        if (locals_jsv) |jsv| {
            for (0..local_count) |i| {
                jsv[i] = CV.toJSValuePtr(&locals[i]);
            }
            if (var_ref_list) |vrl| {
                quickjs.js_frozen_var_ref_list_detach(ctx, vrl);
            }
        }
        cleanupLocals(ctx, locals, local_count);
        if (arg_shadow) |shadow| {
            for (0..arg_count) |i| {
                CV.freeRef(ctx, shadow[i]);
            }
        }
        return JSValue.UNDEFINED;
    }

    // ================================================================
    // Noinline Helpers — Constructor Call
    // ================================================================

    /// Call a constructor: stack has [new_target, func, arg0, ...argN-1] → [result]
    pub noinline fn op_call_constructor(
        ctx: *JSContext,
        stack: [*]CV,
        sp: *usize,
        argc: u16,
    ) GetFieldError!void {
        const n: usize = @intCast(argc);
        // Stack: [func, new_target, arg0, ..., argN-1]
        // QuickJS: call_argv[-2] = func, call_argv[-1] = new_target
        const func_idx = sp.* - n - 2;
        const fn_val = stack[func_idx].toJSValue();
        const new_target_val = stack[func_idx + 1].toJSValue();

        // Use JS_CallConstructor2 to pass the correct new_target (needed for super() calls)
        const result = if (comptime !is_wasm32) blk: {
            const argv_ptr: [*]JSValue = @ptrCast(&stack[sp.* - n]);
            break :blk quickjs.JS_CallConstructor2(ctx, fn_val, new_target_val, @intCast(argc), argv_ptr);
        } else blk: {
            if (n > 0) {
                var args: [256]JSValue = undefined;
                for (0..n) |i| {
                    args[i] = CV.toJSValuePtr(&stack[sp.* - n + i]);
                }
                break :blk quickjs.JS_CallConstructor2(ctx, fn_val, new_target_val, @intCast(argc), @ptrCast(&args));
            } else {
                var no_args: [0]JSValue = .{};
                break :blk quickjs.JS_CallConstructor2(ctx, fn_val, new_target_val, 0, &no_args);
            }
        };

        // FreeRef all consumed CVs (func + new_target + args) directly from stack
        for (func_idx..sp.*) |i| {
            CV.freeRef(ctx, stack[i]);
        }

        if (result.isException()) return error.JsException;
        sp.* = func_idx;
        stack[sp.*] = CV.fromJSValue(result);
        sp.* += 1;
    }

    // ================================================================
    // Noinline Helpers — Apply
    // ================================================================

    /// Function.prototype.apply: stack has [func, this, args_array] → [result]
    pub noinline fn op_apply(
        ctx: *JSContext,
        stack: [*]CV,
        sp: *usize,
        locals: ?[*]CV,
        locals_jsv: ?[*]JSValue,
        var_count: usize,
        closure_alive: ?*bool,
        captured_indices: []const u16,
    ) GetFieldError!void {
        const args_array_cv = stack[sp.* - 1];
        const this_arg_cv = stack[sp.* - 2];
        const func_cv = stack[sp.* - 3];
        const args_array = args_array_cv.toJSValueWithCtx(ctx);
        const this_arg = this_arg_cv.toJSValueWithCtx(ctx);
        const func = func_cv.toJSValueWithCtx(ctx);
        var len: i64 = 0;
        _ = JSValue.getLength(ctx, &len, args_array);
        const argc_val: u32 = @intCast(@min(len, 32));
        var args: [32]JSValue = undefined;
        for (0..argc_val) |i| {
            args[i] = JSValue.getPropertyUint32(ctx, args_array, @intCast(i));
        }
        const result = JSValue.call(ctx, func, this_arg, @intCast(argc_val), &args);
        // Free args elements (getPropertyUint32 returns owned refs, JS_Call is non-consuming)
        for (0..argc_val) |i| {
            JSValue.free(ctx, args[i]);
        }
        // Free the three stack CVs (JS_Call is non-consuming for func/this)
        CV.freeRef(ctx, args_array_cv);
        CV.freeRef(ctx, this_arg_cv);
        CV.freeRef(ctx, func_cv);
        if (result.isException()) return error.JsException;
        sp.* -= 3;
        stack[sp.*] = CV.fromJSValue(result);
        sp.* += 1;
        syncLocalsFrom(locals, locals_jsv, var_count, closure_alive, captured_indices);
    }

    // ================================================================
    // Noinline Helpers — For-Of / For-In / Iterator
    // ================================================================

    pub noinline fn op_for_of_start(ctx: *JSContext, stack: [*]CV, sp: *usize, for_of_iter_stack: [*]usize, for_of_depth: *usize) GetFieldError!void {
        for_of_iter_stack[for_of_depth.*] = sp.* - 1;
        for_of_depth.* += 1;
        var buf: [2]JSValue = undefined;
        buf[0] = stack[sp.* - 1].toJSValueWithCtx(ctx);
        buf[1] = JSValue.UNDEFINED;
        const rc = quickjs.js_frozen_for_of_start(ctx, @ptrCast(&buf[1]), 0);
        if (rc != 0) return error.JsException;
        stack[sp.* - 1] = CV.fromJSValue(buf[0]);
        stack[sp.*] = CV.fromJSValue(buf[1]);
        sp.* += 1;
        stack[sp.*] = CV.fromJSValue(frozen_helpers.newCatchOffset(0));
        sp.* += 1;
    }

    pub noinline fn op_for_of_next(ctx: *JSContext, stack: [*]CV, sp: *usize, for_of_iter_stack: [*]usize, for_of_depth: *usize) GetFieldError!void {
        const iter_idx = for_of_iter_stack[for_of_depth.* - 1];
        var buf: [5]JSValue = undefined;
        buf[0] = stack[iter_idx].toJSValueWithCtx(ctx);
        buf[1] = stack[iter_idx + 1].toJSValueWithCtx(ctx);
        buf[2] = JSValue.UNDEFINED;
        buf[3] = JSValue.UNDEFINED;
        buf[4] = JSValue.UNDEFINED;
        const rc = quickjs.js_frozen_for_of_next(ctx, @ptrCast(&buf[3]), -3);
        if (rc != 0) return error.JsException;
        stack[iter_idx] = CV.fromJSValue(buf[0]);
        stack[sp.*] = CV.fromJSValue(buf[3]);
        sp.* += 1;
        stack[sp.*] = CV.fromJSValue(buf[4]);
        sp.* += 1;
    }

    pub noinline fn op_iterator_close(ctx: *JSContext, stack: [*]CV, sp: *usize, for_of_iter_stack: [*]usize, for_of_depth: *usize) void {
        const iter_base = for_of_iter_stack[for_of_depth.* - 1];
        CV.freeRef(ctx, stack[iter_base]);
        CV.freeRef(ctx, stack[iter_base + 1]);
        sp.* = iter_base;
        for_of_depth.* -= 1;
    }

    pub noinline fn op_iterator_get_value_done(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const old = stack[sp.* - 1];
        const result = old.toJSValueWithCtx(ctx);
        var done: i32 = 0;
        const val = quickjs.js_frozen_iterator_get_value_done(ctx, result, &done);
        CV.freeRef(ctx, old); // free the iterator result object {value, done}
        stack[sp.* - 1] = CV.fromJSValue(val);
        stack[sp.*] = if (done != 0) CV.TRUE else CV.FALSE;
        sp.* += 1;
    }

    pub noinline fn op_for_in_start(ctx: *JSContext, stack: [*]CV, sp: *usize) GetFieldError!void {
        var buf: [2]JSValue = .{ stack[sp.* - 1].toJSValueWithCtx(ctx), JSValue.UNDEFINED };
        const rc = quickjs.js_frozen_for_in_start(ctx, @ptrCast(&buf[1]));
        if (rc < 0) return error.JsException;
        stack[sp.* - 1] = CV.fromJSValue(buf[0]);
    }

    pub noinline fn op_for_in_next(ctx: *JSContext, stack: [*]CV, sp: *usize) GetFieldError!void {
        var buf: [3]JSValue = .{ stack[sp.* - 1].toJSValueWithCtx(ctx), JSValue.UNDEFINED, JSValue.UNDEFINED };
        const rc = quickjs.js_frozen_for_in_next(ctx, @ptrCast(&buf[1]));
        if (rc < 0) return error.JsException;
        stack[sp.* - 1] = CV.fromJSValue(buf[0]);
        stack[sp.*] = CV.fromJSValue(buf[1]);
        sp.* += 1;
        stack[sp.*] = CV.fromJSValue(buf[2]);
        sp.* += 1;
    }

    pub noinline fn op_in(ctx: *JSContext, stack: [*]CV, sp: *usize) GetFieldError!void {
        const obj_cv = stack[sp.* - 1];
        const prop_cv = stack[sp.* - 2];
        const obj = obj_cv.toJSValueWithCtx(ctx);
        const prop = prop_cv.toJSValueWithCtx(ctx);
        const atom = quickjs.JS_ValueToAtom(ctx, prop);
        const result = quickjs.JS_HasProperty(ctx, obj, atom);
        quickjs.JS_FreeAtom(ctx, atom);
        CV.freeRef(ctx, prop_cv); // JS_ValueToAtom is non-consuming
        CV.freeRef(ctx, obj_cv); // JS_HasProperty is non-consuming
        if (result < 0) return error.JsException;
        stack[sp.* - 2] = CV.fromJSValue(JSValue.newBool(result > 0));
        sp.* -= 1;
    }

    // ================================================================
    // Noinline Helpers — Array Operations
    // ================================================================

    pub noinline fn op_append(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const enumobj = stack[sp.* - 1].toJSValueWithCtx(ctx);
        var pos: i32 = stack[sp.* - 2].getInt();
        const arr = stack[sp.* - 3].toJSValueWithCtx(ctx);
        if (!enumobj.isUndefined() and !enumobj.isNull()) {
            const src_len_val = JSValue.getPropertyStr(ctx, enumobj, "length");
            var src_len: i32 = 0;
            _ = JSValue.toInt32(ctx, &src_len, src_len_val);
            JSValue.free(ctx, src_len_val);
            var i: i32 = 0;
            while (i < src_len) : (i += 1) {
                const elem = JSValue.getPropertyUint32(ctx, enumobj, @intCast(i));
                _ = JSValue.setPropertyUint32(ctx, arr, @intCast(pos), elem);
                pos += 1;
            }
        }
        stack[sp.* - 2] = CV.newInt(pos);
        if (CV.fromJSValue(enumobj).isRefType()) JSValue.free(ctx, enumobj);
        sp.* -= 1;
    }

    pub noinline fn op_put_array_el(ctx: *JSContext, stack: [*]CV, sp: *usize) void {
        const val = stack[sp.* - 1];
        const idx = stack[sp.* - 2];
        const arr = stack[sp.* - 3];
        const arr_jsv = arr.toJSValueWithCtx(ctx);
        const idx_jsv = idx.toJSValueWithCtx(ctx);
        const atom = quickjs.JS_ValueToAtom(ctx, idx_jsv);
        // JS_SetProperty CONSUMES val (takes ownership). Pass the stack's ref directly.
        _ = quickjs.JS_SetProperty(ctx, arr_jsv, atom, val.toJSValueWithCtx(ctx));
        quickjs.JS_FreeAtom(ctx, atom);
        // Free arr and idx (not consumed by JS_SetProperty). Val was consumed.
        CV.freeRef(ctx, arr);
        CV.freeRef(ctx, idx);
        sp.* -= 3;
    }
};

