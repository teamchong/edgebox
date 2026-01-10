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

const std = @import("std");

// ============================================================================
// QuickJS Tag Constants
// Matches quickjs.h enum values exactly
// ============================================================================

pub const JS_TAG_FIRST: i64 = -9; // First negative tag (has refcount)
pub const JS_TAG_BIG_INT: i64 = -9;
pub const JS_TAG_SYMBOL: i64 = -8;
pub const JS_TAG_STRING: i64 = -7;
pub const JS_TAG_MODULE: i64 = -3;
pub const JS_TAG_FUNCTION_BYTECODE: i64 = -2;
pub const JS_TAG_OBJECT: i64 = -1;
pub const JS_TAG_INT: i64 = 0;
pub const JS_TAG_BOOL: i64 = 1;
pub const JS_TAG_NULL: i64 = 2;
pub const JS_TAG_UNDEFINED: i64 = 3;
pub const JS_TAG_UNINITIALIZED: i64 = 4;
pub const JS_TAG_CATCH_OFFSET: i64 = 5;
pub const JS_TAG_EXCEPTION: i64 = 6;
pub const JS_TAG_SHORT_BIG_INT: i64 = 7;
pub const JS_TAG_FLOAT64: i64 = 8;

// ============================================================================
// JSContext - Opaque pointer to QuickJS context
// ============================================================================

pub const JSContext = opaque {};
pub const JSRuntime = opaque {};

// ============================================================================
// JSValue - Matches QuickJS 64-bit (non-NAN-boxing) layout
// Total size: 16 bytes (8-byte union + 8-byte tag)
// ============================================================================

pub const JSValueUnion = extern union {
    int32: i32,
    float64: f64,
    ptr: ?*anyopaque,
    short_big_int: i32,
};

pub const JSValue = extern struct {
    u: JSValueUnion,
    tag: i64,

    // ========================================================================
    // Constants
    // ========================================================================

    pub const UNDEFINED = JSValue{ .u = .{ .int32 = 0 }, .tag = JS_TAG_UNDEFINED };
    pub const NULL = JSValue{ .u = .{ .int32 = 0 }, .tag = JS_TAG_NULL };
    pub const TRUE = JSValue{ .u = .{ .int32 = 1 }, .tag = JS_TAG_BOOL };
    pub const FALSE = JSValue{ .u = .{ .int32 = 0 }, .tag = JS_TAG_BOOL };
    pub const EXCEPTION = JSValue{ .u = .{ .int32 = 0 }, .tag = JS_TAG_EXCEPTION };
    pub const UNINITIALIZED = JSValue{ .u = .{ .int32 = 0 }, .tag = JS_TAG_UNINITIALIZED };

    // ========================================================================
    // Tag Checks
    // ========================================================================

    pub inline fn isInt(self: JSValue) bool {
        return self.tag == JS_TAG_INT;
    }

    pub inline fn isBool(self: JSValue) bool {
        return self.tag == JS_TAG_BOOL;
    }

    pub inline fn isNull(self: JSValue) bool {
        return self.tag == JS_TAG_NULL;
    }

    pub inline fn isUndefined(self: JSValue) bool {
        return self.tag == JS_TAG_UNDEFINED;
    }

    pub inline fn isUninitialized(self: JSValue) bool {
        return self.tag == JS_TAG_UNINITIALIZED;
    }

    pub inline fn isException(self: JSValue) bool {
        return self.tag == JS_TAG_EXCEPTION;
    }

    pub inline fn isFunction(self: JSValue) bool {
        // Functions have tag OBJECT or FUNCTION_BYTECODE
        return self.tag == JS_TAG_OBJECT or self.tag == JS_TAG_FUNCTION_BYTECODE;
    }

    pub inline fn isString(self: JSValue) bool {
        return self.tag == JS_TAG_STRING;
    }

    pub inline fn isObject(self: JSValue) bool {
        return self.tag == JS_TAG_OBJECT;
    }

    pub inline fn isNumber(self: JSValue) bool {
        return self.tag == JS_TAG_INT or self.tag == JS_TAG_FLOAT64;
    }

    /// Negative tags have reference counts (objects, strings, symbols, etc.)
    pub inline fn hasRefCount(self: JSValue) bool {
        return self.tag < 0;
    }

    // ========================================================================
    // Value Extraction
    // ========================================================================

    pub inline fn getInt(self: JSValue) i32 {
        return self.u.int32;
    }

    pub inline fn getBool(self: JSValue) bool {
        return self.u.int32 != 0;
    }

    pub inline fn getFloat64(self: JSValue) f64 {
        return self.u.float64;
    }

    pub inline fn getPtr(self: JSValue) ?*anyopaque {
        return self.u.ptr;
    }

    // ========================================================================
    // Value Creation
    // ========================================================================

    pub inline fn newInt(val: i32) JSValue {
        return .{ .u = .{ .int32 = val }, .tag = JS_TAG_INT };
    }

    pub inline fn newBool(val: bool) JSValue {
        return .{ .u = .{ .int32 = if (val) 1 else 0 }, .tag = JS_TAG_BOOL };
    }

    pub inline fn newFloat64(val: f64) JSValue {
        return .{ .u = .{ .float64 = val }, .tag = JS_TAG_FLOAT64 };
    }

    // ========================================================================
    // Reference Counting
    // ========================================================================

    /// Duplicate a value (increment refcount if needed)
    pub inline fn dup(ctx: *JSContext, val: JSValue) JSValue {
        if (val.hasRefCount()) {
            return quickjs.JS_DupValue(ctx, val);
        }
        return val;
    }

    /// Free a value (decrement refcount if needed)
    pub inline fn free(ctx: *JSContext, val: JSValue) void {
        if (val.hasRefCount()) {
            quickjs.JS_FreeValue(ctx, val);
        }
    }

    // ========================================================================
    // FFI Calls to QuickJS (for non-primitive operations)
    // ========================================================================

    /// Call a JavaScript function
    pub fn call(ctx: *JSContext, func: JSValue, this: JSValue, args: []const JSValue) JSValue {
        return quickjs.JS_Call(ctx, func, this, @intCast(args.len), args.ptr);
    }

    /// Get property by string name
    pub fn getPropertyStr(ctx: *JSContext, obj: JSValue, name: [*:0]const u8) JSValue {
        return quickjs.JS_GetPropertyStr(ctx, obj, name);
    }

    /// Set property by string name
    pub fn setPropertyStr(ctx: *JSContext, obj: JSValue, name: [*:0]const u8, val: JSValue) c_int {
        return quickjs.JS_SetPropertyStr(ctx, obj, name, val);
    }

    /// Get array element by index
    pub fn getPropertyUint32(ctx: *JSContext, obj: JSValue, idx: u32) JSValue {
        return quickjs.JS_GetPropertyUint32(ctx, obj, idx);
    }

    /// Set array element by index
    pub fn setPropertyUint32(ctx: *JSContext, obj: JSValue, idx: u32, val: JSValue) c_int {
        return quickjs.JS_SetPropertyUint32(ctx, obj, idx, val);
    }

    /// Convert to boolean
    pub fn toBool(ctx: *JSContext, val: JSValue) c_int {
        return quickjs.JS_ToBool(ctx, val);
    }

    /// Convert to int32
    pub fn toInt32(ctx: *JSContext, pres: *i32, val: JSValue) c_int {
        return quickjs.JS_ToInt32(ctx, pres, val);
    }

    /// Convert to float64
    pub fn toFloat64(ctx: *JSContext, pres: *f64, val: JSValue) c_int {
        return quickjs.JS_ToFloat64(ctx, pres, val);
    }

    /// Create a new object
    pub fn newObject(ctx: *JSContext) JSValue {
        return quickjs.JS_NewObject(ctx);
    }

    /// Create a new array
    pub fn newArray(ctx: *JSContext) JSValue {
        return quickjs.JS_NewArray(ctx);
    }

    /// Create a new string
    pub fn newString(ctx: *JSContext, str: [*:0]const u8) JSValue {
        return quickjs.JS_NewString(ctx, str);
    }

    /// Throw a type error
    pub fn throwTypeError(ctx: *JSContext, fmt: [*:0]const u8) JSValue {
        return quickjs.JS_ThrowTypeError(ctx, fmt);
    }

    /// Throw a range error
    pub fn throwRangeError(ctx: *JSContext, fmt: [*:0]const u8) JSValue {
        return quickjs.JS_ThrowRangeError(ctx, fmt);
    }

    /// Throw a reference error
    pub fn throwReferenceError(ctx: *JSContext, fmt: [*:0]const u8) JSValue {
        return quickjs.JS_ThrowReferenceError(ctx, fmt);
    }

    /// Get a global variable by name
    pub fn getGlobal(ctx: *JSContext, name: [*:0]const u8) JSValue {
        const global = quickjs.JS_GetGlobalObject(ctx);
        defer free(ctx, global);
        return quickjs.JS_GetPropertyStr(ctx, global, name);
    }

    /// Define property on object (for object literal field definition)
    pub fn definePropertyStr(ctx: *JSContext, obj: JSValue, name: [*:0]const u8, val: JSValue) c_int {
        return quickjs.JS_DefinePropertyValueStr(ctx, obj, name, val, quickjs.JS_PROP_C_W_E);
    }

    /// Call function as constructor (new X(...args))
    pub fn callConstructor(ctx: *JSContext, func: JSValue, args: []const JSValue) JSValue {
        return quickjs.JS_CallConstructor(ctx, func, @intCast(args.len), args.ptr);
    }

    /// Strict equality check (===)
    pub fn strictEq(a: JSValue, b: JSValue) bool {
        // Fast path for same tag and same value
        if (a.tag != b.tag) return false;
        switch (a.tag) {
            JS_TAG_INT, JS_TAG_BOOL => return a.u.int32 == b.u.int32,
            JS_TAG_NULL, JS_TAG_UNDEFINED => return true,
            JS_TAG_FLOAT64 => return a.u.float64 == b.u.float64,
            else => return a.u.ptr == b.u.ptr, // Reference equality for objects
        }
    }

    /// Throw an exception value
    pub fn throw(ctx: *JSContext, val: JSValue) JSValue {
        return quickjs.JS_Throw(ctx, val);
    }

    /// Get typeof result as JSValue string
    pub fn typeOf(ctx: *JSContext, val: JSValue) JSValue {
        const type_str = switch (val.tag) {
            JS_TAG_UNDEFINED => "undefined",
            JS_TAG_NULL => "object", // typeof null === "object" (quirk)
            JS_TAG_BOOL => "boolean",
            JS_TAG_INT, JS_TAG_FLOAT64, JS_TAG_SHORT_BIG_INT => "number",
            JS_TAG_STRING => "string",
            JS_TAG_SYMBOL => "symbol",
            JS_TAG_BIG_INT => "bigint",
            JS_TAG_OBJECT, JS_TAG_FUNCTION_BYTECODE => blk: {
                // Need to check if it's a function
                if (quickjs.JS_IsFunction(ctx, val) != 0) {
                    break :blk "function";
                }
                break :blk "object";
            },
            else => "object",
        };
        return quickjs.JS_NewString(ctx, type_str);
    }

    /// Check instanceof
    pub fn isInstanceOf(ctx: *JSContext, obj: JSValue, ctor: JSValue) bool {
        return quickjs.JS_IsInstanceOf(ctx, obj, ctor) != 0;
    }

    /// Get global variable (returns undefined if not found)
    pub fn getGlobalUndef(ctx: *JSContext, name: [*:0]const u8) JSValue {
        const global = quickjs.JS_GetGlobalObject(ctx);
        defer free(ctx, global);
        const result = quickjs.JS_GetPropertyStr(ctx, global, name);
        // If exception (not found), return undefined
        if (result.isException()) {
            return UNDEFINED;
        }
        return result;
    }

    /// Delete property from object
    pub fn deleteProperty(ctx: *JSContext, obj: JSValue, prop: JSValue) c_int {
        return quickjs.JS_DeleteProperty(ctx, obj, prop, 0);
    }

    /// Append value to array
    pub fn appendArray(ctx: *JSContext, arr: JSValue, val: JSValue) c_int {
        // Get current length
        const len_val = quickjs.JS_GetPropertyStr(ctx, arr, "length");
        var len: i32 = 0;
        _ = toInt32(ctx, &len, len_val);
        free(ctx, len_val);
        // Set at length index
        return quickjs.JS_SetPropertyUint32(ctx, arr, @intCast(len), val);
    }
};

// ============================================================================
// SMI (Small Integer) Arithmetic - Inline Fast Paths
// Zero allocation for int32 operands, fallback to float64 on overflow
// ============================================================================

/// Add two JSValues with SMI fast path
pub inline fn add(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    // Fast path: int + int
    if (a.isInt() and b.isInt()) {
        const ia: i64 = a.getInt();
        const ib: i64 = b.getInt();
        const r = ia + ib;
        // Check for overflow
        if (r >= std.math.minInt(i32) and r <= std.math.maxInt(i32)) {
            return JSValue.newInt(@intCast(r));
        }
        return JSValue.newFloat64(@floatFromInt(r));
    }
    // String concatenation handled by slow path
    if (a.isString() or b.isString()) {
        return addSlow(ctx, a, b);
    }
    // Slow path: numeric conversion
    return addSlow(ctx, a, b);
}

/// Subtract two JSValues with SMI fast path
pub inline fn sub(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        const ia: i64 = a.getInt();
        const ib: i64 = b.getInt();
        const r = ia - ib;
        if (r >= std.math.minInt(i32) and r <= std.math.maxInt(i32)) {
            return JSValue.newInt(@intCast(r));
        }
        return JSValue.newFloat64(@floatFromInt(r));
    }
    return subSlow(ctx, a, b);
}

/// Multiply two JSValues with SMI fast path
pub inline fn mul(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        const ia: i64 = a.getInt();
        const ib: i64 = b.getInt();
        const r = ia * ib;
        if (r >= std.math.minInt(i32) and r <= std.math.maxInt(i32)) {
            return JSValue.newInt(@intCast(r));
        }
        return JSValue.newFloat64(@floatFromInt(r));
    }
    return mulSlow(ctx, a, b);
}

/// Divide two JSValues (always returns float64 per JS semantics)
pub inline fn div(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    // Division always produces float in JS (even 4/2 = 2.0 semantically)
    // But we can return int if result is exact integer
    if (a.isInt() and b.isInt()) {
        const ia = a.getInt();
        const ib = b.getInt();
        if (ib != 0 and @rem(ia, ib) == 0) {
            const result = @divExact(ia, ib);
            return JSValue.newInt(result);
        }
    }
    return divSlow(ctx, a, b);
}

/// Modulo two JSValues with SMI fast path
pub inline fn mod(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        const ia = a.getInt();
        const ib = b.getInt();
        if (ib == 0) return JSValue.newFloat64(std.math.nan(f64));
        if (ib == -1 and ia == std.math.minInt(i32)) return JSValue.newInt(0);
        return JSValue.newInt(@rem(ia, ib));
    }
    return modSlow(ctx, a, b);
}

/// Negate a JSValue with SMI fast path
pub inline fn neg(ctx: *JSContext, a: JSValue) JSValue {
    if (a.isInt()) {
        const ia = a.getInt();
        // Special case: -0 must return float -0.0
        if (ia == 0) return JSValue.newFloat64(-0.0);
        // Special case: -INT32_MIN overflows
        if (ia == std.math.minInt(i32)) return JSValue.newFloat64(-@as(f64, @floatFromInt(ia)));
        return JSValue.newInt(-ia);
    }
    return negSlow(ctx, a);
}

// ============================================================================
// Comparison Helpers - Return JSValue bool
// ============================================================================

pub inline fn lt(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        return JSValue.newBool(a.getInt() < b.getInt());
    }
    return ltSlow(ctx, a, b);
}

pub inline fn lte(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        return JSValue.newBool(a.getInt() <= b.getInt());
    }
    return lteSlow(ctx, a, b);
}

pub inline fn gt(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        return JSValue.newBool(a.getInt() > b.getInt());
    }
    return gtSlow(ctx, a, b);
}

pub inline fn gte(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        return JSValue.newBool(a.getInt() >= b.getInt());
    }
    return gteSlow(ctx, a, b);
}

pub inline fn eq(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        return JSValue.newBool(a.getInt() == b.getInt());
    }
    return eqSlow(ctx, a, b);
}

pub inline fn neq(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        return JSValue.newBool(a.getInt() != b.getInt());
    }
    return neqSlow(ctx, a, b);
}

// ============================================================================
// Bitwise Operations - SMI Fast Paths
// ============================================================================

pub inline fn bitAnd(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        return JSValue.newInt(a.getInt() & b.getInt());
    }
    return bitAndSlow(ctx, a, b);
}

pub inline fn bitOr(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        return JSValue.newInt(a.getInt() | b.getInt());
    }
    return bitOrSlow(ctx, a, b);
}

pub inline fn bitXor(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        return JSValue.newInt(a.getInt() ^ b.getInt());
    }
    return bitXorSlow(ctx, a, b);
}

pub inline fn bitNot(ctx: *JSContext, a: JSValue) JSValue {
    if (a.isInt()) {
        return JSValue.newInt(~a.getInt());
    }
    return bitNotSlow(ctx, a);
}

pub inline fn shl(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        const shift: u5 = @truncate(@as(u32, @bitCast(b.getInt())));
        return JSValue.newInt(a.getInt() << shift);
    }
    return shlSlow(ctx, a, b);
}

pub inline fn sar(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        const shift: u5 = @truncate(@as(u32, @bitCast(b.getInt())));
        return JSValue.newInt(a.getInt() >> shift);
    }
    return sarSlow(ctx, a, b);
}

pub inline fn shr(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        const ua: u32 = @bitCast(a.getInt());
        const shift: u5 = @truncate(@as(u32, @bitCast(b.getInt())));
        const result = ua >> shift;
        // Result might not fit in i32 if high bit was set
        if (result <= std.math.maxInt(i32)) {
            return JSValue.newInt(@bitCast(result));
        }
        return JSValue.newFloat64(@floatFromInt(result));
    }
    return shrSlow(ctx, a, b);
}

// ============================================================================
// Slow Paths - Call QuickJS C runtime
// These are called when operands are not both SMI integers
// ============================================================================

extern fn frozen_add(ctx: *JSContext, a: JSValue, b: JSValue) JSValue;
extern fn frozen_sub(ctx: *JSContext, a: JSValue, b: JSValue) JSValue;
extern fn frozen_mul(ctx: *JSContext, a: JSValue, b: JSValue) JSValue;
extern fn frozen_div(ctx: *JSContext, a: JSValue, b: JSValue) JSValue;
extern fn frozen_mod(ctx: *JSContext, a: JSValue, b: JSValue) JSValue;
extern fn frozen_neg(ctx: *JSContext, a: JSValue) JSValue;

extern fn frozen_lt(ctx: *JSContext, a: JSValue, b: JSValue) c_int;
extern fn frozen_lte(ctx: *JSContext, a: JSValue, b: JSValue) c_int;
extern fn frozen_gt(ctx: *JSContext, a: JSValue, b: JSValue) c_int;
extern fn frozen_gte(ctx: *JSContext, a: JSValue, b: JSValue) c_int;
extern fn frozen_eq(ctx: *JSContext, a: JSValue, b: JSValue) c_int;
extern fn frozen_neq(ctx: *JSContext, a: JSValue, b: JSValue) c_int;

extern fn frozen_and(ctx: *JSContext, a: JSValue, b: JSValue) JSValue;
extern fn frozen_or(ctx: *JSContext, a: JSValue, b: JSValue) JSValue;
extern fn frozen_xor(ctx: *JSContext, a: JSValue, b: JSValue) JSValue;
extern fn frozen_not(ctx: *JSContext, a: JSValue) JSValue;
extern fn frozen_shl(ctx: *JSContext, a: JSValue, b: JSValue) JSValue;
extern fn frozen_sar(ctx: *JSContext, a: JSValue, b: JSValue) JSValue;
extern fn frozen_shr(ctx: *JSContext, a: JSValue, b: JSValue) JSValue;

fn addSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return frozen_add(ctx, a, b);
}

fn subSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return frozen_sub(ctx, a, b);
}

fn mulSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return frozen_mul(ctx, a, b);
}

fn divSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return frozen_div(ctx, a, b);
}

fn modSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return frozen_mod(ctx, a, b);
}

fn negSlow(ctx: *JSContext, a: JSValue) JSValue {
    return frozen_neg(ctx, a);
}

fn ltSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return JSValue.newBool(frozen_lt(ctx, a, b) != 0);
}

fn lteSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return JSValue.newBool(frozen_lte(ctx, a, b) != 0);
}

fn gtSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return JSValue.newBool(frozen_gt(ctx, a, b) != 0);
}

fn gteSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return JSValue.newBool(frozen_gte(ctx, a, b) != 0);
}

fn eqSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return JSValue.newBool(frozen_eq(ctx, a, b) != 0);
}

fn neqSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return JSValue.newBool(frozen_neq(ctx, a, b) != 0);
}

fn bitAndSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return frozen_and(ctx, a, b);
}

fn bitOrSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return frozen_or(ctx, a, b);
}

fn bitXorSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return frozen_xor(ctx, a, b);
}

fn bitNotSlow(ctx: *JSContext, a: JSValue) JSValue {
    return frozen_not(ctx, a);
}

fn shlSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return frozen_shl(ctx, a, b);
}

fn sarSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return frozen_sar(ctx, a, b);
}

fn shrSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    return frozen_shr(ctx, a, b);
}

// ============================================================================
// QuickJS FFI Bindings
// ============================================================================

pub const quickjs = struct {
    // Reference counting
    pub extern fn JS_DupValue(ctx: *JSContext, val: JSValue) JSValue;
    pub extern fn JS_FreeValue(ctx: *JSContext, val: JSValue) void;

    // Function calls
    pub extern fn JS_Call(ctx: *JSContext, func: JSValue, this: JSValue, argc: c_int, argv: [*]const JSValue) JSValue;

    // Property access
    pub extern fn JS_GetPropertyStr(ctx: *JSContext, obj: JSValue, prop: [*:0]const u8) JSValue;
    pub extern fn JS_SetPropertyStr(ctx: *JSContext, obj: JSValue, prop: [*:0]const u8, val: JSValue) c_int;
    pub extern fn JS_GetPropertyUint32(ctx: *JSContext, obj: JSValue, idx: u32) JSValue;
    pub extern fn JS_SetPropertyUint32(ctx: *JSContext, obj: JSValue, idx: u32, val: JSValue) c_int;

    // Type conversion
    pub extern fn JS_ToBool(ctx: *JSContext, val: JSValue) c_int;
    pub extern fn JS_ToInt32(ctx: *JSContext, pres: *i32, val: JSValue) c_int;
    pub extern fn JS_ToFloat64(ctx: *JSContext, pres: *f64, val: JSValue) c_int;

    // Object creation
    pub extern fn JS_NewObject(ctx: *JSContext) JSValue;
    pub extern fn JS_NewArray(ctx: *JSContext) JSValue;
    pub extern fn JS_NewString(ctx: *JSContext, str: [*:0]const u8) JSValue;
    pub extern fn JS_NewFloat64(ctx: *JSContext, val: f64) JSValue;

    // Error handling
    pub extern fn JS_ThrowTypeError(ctx: *JSContext, fmt: [*:0]const u8, ...) JSValue;
    pub extern fn JS_ThrowRangeError(ctx: *JSContext, fmt: [*:0]const u8, ...) JSValue;
    pub extern fn JS_ThrowReferenceError(ctx: *JSContext, fmt: [*:0]const u8, ...) JSValue;
    pub extern fn JS_Throw(ctx: *JSContext, val: JSValue) JSValue;

    // Global object
    pub extern fn JS_GetGlobalObject(ctx: *JSContext) JSValue;

    // Function creation
    pub extern fn JS_NewCFunction(ctx: *JSContext, func: *const anyopaque, name: [*:0]const u8, length: c_int) JSValue;

    // Constructor calls
    pub extern fn JS_CallConstructor(ctx: *JSContext, func: JSValue, argc: c_int, argv: [*]const JSValue) JSValue;

    // Property definition
    pub extern fn JS_DefinePropertyValueStr(ctx: *JSContext, this_obj: JSValue, prop: [*:0]const u8, val: JSValue, flags: c_int) c_int;
    pub const JS_PROP_C_W_E: c_int = (1 << 0) | (1 << 1) | (1 << 2); // configurable, writable, enumerable

    // Type checks
    pub extern fn JS_IsFunction(ctx: *JSContext, val: JSValue) c_int;
    pub extern fn JS_IsInstanceOf(ctx: *JSContext, val: JSValue, obj: JSValue) c_int;

    // Property deletion
    pub extern fn JS_DeleteProperty(ctx: *JSContext, obj: JSValue, prop: JSValue, flags: c_int) c_int;

    // Iterator protocol (frozen functions)
    pub extern fn js_frozen_for_of_start(ctx: *JSContext, sp: [*]JSValue, is_async: c_int) c_int;
    pub extern fn js_frozen_for_of_next(ctx: *JSContext, sp: [*]JSValue, offset: c_int) c_int;
};

// ============================================================================
// Stack Management
// ============================================================================

pub const FROZEN_MAX_CALL_DEPTH: usize = 10000;

/// Global call depth counter - tracks frozen function recursion
pub var frozen_call_depth: usize = 0;

/// Check and increment call depth, return true if stack overflow
pub inline fn checkStack() bool {
    if (frozen_call_depth >= FROZEN_MAX_CALL_DEPTH) {
        return true; // Stack overflow
    }
    frozen_call_depth += 1;
    return false;
}

/// Decrement call depth on function exit
pub inline fn exitStack() void {
    if (frozen_call_depth > 0) {
        frozen_call_depth -= 1;
    }
}

/// Reset call depth (call at start of each request)
pub export fn frozen_reset_call_depth_zig() void {
    frozen_call_depth = 0;
}

// ============================================================================
// Closure Variable Access
// Closure variables are passed as an extra array at argv[argc]
// The closure_var_indices map bytecode indices to positions in this array
// ============================================================================

/// Get closure variable from argv[argc] array
/// @param ctx - JSContext pointer
/// @param argv - argument array (closure vars at argv[argc])
/// @param argc - argument count (closure array is at this index)
/// @param position - position in closure vars array (from closure_var_indices)
pub inline fn getClosureVar(ctx: *JSContext, argv: [*]JSValue, argc: c_int, position: u32) JSValue {
    // argv[argc] contains the closure vars array
    const closure_array = argv[@intCast(argc)];
    const val = JSValue.getPropertyUint32(ctx, closure_array, position);
    // Duplicate value since getProperty returns owned reference
    return val;
}

/// Set closure variable in argv[argc] array
/// @param ctx - JSContext pointer
/// @param argv - argument array (closure vars at argv[argc])
/// @param argc - argument count (closure array is at this index)
/// @param position - position in closure vars array (from closure_var_indices)
/// @param val - value to set (ownership transferred)
pub inline fn setClosureVar(ctx: *JSContext, argv: [*]JSValue, argc: c_int, position: u32, val: JSValue) void {
    // argv[argc] contains the closure vars array
    const closure_array = argv[@intCast(argc)];
    _ = JSValue.setPropertyUint32(ctx, closure_array, position, val);
}

/// Get closure variable with TDZ (Temporal Dead Zone) check
/// Returns EXCEPTION if variable is uninitialized
pub inline fn getClosureVarCheck(ctx: *JSContext, argv: [*]JSValue, argc: c_int, position: u32) JSValue {
    const val = getClosureVar(ctx, argv, argc, position);
    if (val.isUninitialized()) {
        return JSValue.throwReferenceError(ctx, "Cannot access '%s' before initialization");
    }
    return val;
}

/// Set closure variable with TDZ check
/// Returns true if variable was uninitialized (error condition)
pub inline fn setClosureVarCheck(ctx: *JSContext, argv: [*]JSValue, argc: c_int, position: u32, val: JSValue) bool {
    const existing = getClosureVar(ctx, argv, argc, position);
    JSValue.free(ctx, existing);
    if (existing.isUninitialized()) {
        JSValue.free(ctx, val);
        _ = JSValue.throwReferenceError(ctx, "Cannot access '%s' before initialization");
        return true; // Error
    }
    setClosureVar(ctx, argv, argc, position, val);
    return false;
}

// ============================================================================
// Iterator Protocol Helpers
// ============================================================================

/// Create a catch offset value (for try/catch/finally and iterator cleanup)
/// This is a pure Zig implementation matching JS_NewCatchOffset macro
pub inline fn newCatchOffset(val: i32) JSValue {
    return JSValue{ .u = .{ .int32 = val }, .tag = JS_TAG_CATCH_OFFSET };
}

/// Initialize for-of iterator
/// Stack before: [obj] at sp-1
/// Stack after: [iterator, next_method, catch_offset] (sp increases by 2)
/// Returns 0 on success, non-zero on error
pub inline fn forOfStart(ctx: *JSContext, stack: [*]JSValue, sp: *usize) c_int {
    // js_frozen_for_of_start expects pointer to current stack position
    // It reads obj from sp[-1], writes iterator to sp[-1], next_method to sp[0]
    const result = quickjs.js_frozen_for_of_start(ctx, stack + sp.*, 0);
    if (result != 0) return result;
    sp.* += 1; // next_method is now at sp
    // Add catch_offset at new sp position
    stack[sp.*] = newCatchOffset(0);
    sp.* += 1;
    return 0;
}

/// Get next value from for-of iterator
/// Stack before: [iterator, next_method, catch_offset, ...] with offset indicating iterator position
/// Stack after: [..., value, done] (sp increases by 2)
/// Returns 0 on success, non-zero on error
pub inline fn forOfNext(ctx: *JSContext, stack: [*]JSValue, sp: *usize, offset: i32) c_int {
    // js_frozen_for_of_next takes negative offset from current sp to iterator
    // offset 0 from bytecode means iterator is at sp-3 (offset -3)
    const result = quickjs.js_frozen_for_of_next(ctx, stack + sp.*, -(offset + 3));
    if (result != 0) return result;
    sp.* += 2; // value at sp-2, done at sp-1
    return 0;
}

/// Close iterator (cleanup after for-of loop)
/// Stack before: [iterator, next_method, catch_offset] at positions sp-3, sp-2, sp-1
/// Stack after: [] (sp decreases by 3)
/// Calls iterator.return() if it exists
pub fn iteratorClose(ctx: *JSContext, stack: [*]JSValue, sp: *usize) void {
    // Pop catch_offset (no need to free, it's a primitive)
    sp.* -= 1;

    // Pop next_method
    sp.* -= 1;
    const next_method = stack[sp.*];
    JSValue.free(ctx, next_method);

    // Pop iterator
    sp.* -= 1;
    const iter = stack[sp.*];

    // Call iterator.return() if it exists
    if (!iter.isUndefined()) {
        const ret_method = JSValue.getPropertyStr(ctx, iter, "return");
        if (!ret_method.isUndefined() and !ret_method.isNull()) {
            const ret = quickjs.JS_Call(ctx, ret_method, iter, 0, @ptrFromInt(0));
            JSValue.free(ctx, ret_method);
            // Don't propagate exception from return(), just clean up
            if (!ret.isException()) {
                JSValue.free(ctx, ret);
            }
        } else {
            JSValue.free(ctx, ret_method);
        }
        JSValue.free(ctx, iter);
    }
}

/// Check if iterator is done (top of stack is the done flag)
pub inline fn iteratorIsDone(stack: [*]JSValue, sp: usize) bool {
    const done = stack[sp - 1];
    // done is either true or false
    return done.isBool() and done.getBool();
}

/// Get iterator value (second from top after for_of_next)
pub inline fn iteratorGetValue(stack: [*]JSValue, sp: usize) JSValue {
    return stack[sp - 2];
}

// ============================================================================
// Partial Freeze Block Fallback
// ============================================================================

/// FFI declaration for frozen_block_fallback (defined in frozen_runtime.c)
/// Called when execution enters a contaminated block (one with never_freeze opcodes)
/// Transfers control to the interpreter for that block, then returns control
pub extern fn frozen_block_fallback(
    ctx: *JSContext,
    func_name: [*:0]const u8,
    this_val: JSValue,
    argc: c_int,
    argv: [*]JSValue,
    locals: [*]JSValue,
    num_locals: c_int,
    stack: [*]JSValue,
    sp: *c_int,
    block_id: c_int,
    next_block_out: *c_int,
) JSValue;

/// High-level wrapper for block fallback
/// Executes a contaminated block in the interpreter and returns control to native code
/// Returns: result if function completed, or UNDEFINED to continue to next_block
pub fn blockFallback(
    ctx: *JSContext,
    func_name: [*:0]const u8,
    this_val: JSValue,
    argc: c_int,
    argv: [*]JSValue,
    locals: [*]JSValue,
    num_locals: c_int,
    stack: [*]JSValue,
    sp: *usize,
    block_id: usize,
    next_block: *usize,
) JSValue {
    var sp_int: c_int = @intCast(sp.*);
    var next_block_int: c_int = -1;

    const result = frozen_block_fallback(
        ctx,
        func_name,
        this_val,
        argc,
        argv,
        locals,
        num_locals,
        stack,
        &sp_int,
        @intCast(block_id),
        &next_block_int,
    );

    // Update sp from C value
    sp.* = @intCast(sp_int);

    // Update next_block (-1 means function completed, otherwise block id)
    if (next_block_int >= 0) {
        next_block.* = @intCast(next_block_int);
    }

    return result;
}

// ============================================================================
// Native Shape Access
// Zero-overhead property access for known shapes (like TypeScript AST nodes)
// ============================================================================

/// Native AstNode structure - mirrors TypeScript AST node (defined in native_shapes.zig)
pub const NativeAstNode = extern struct {
    kind: i32,
    flags: i32,
    pos: i32,
    end: i32,
    parent: ?*NativeAstNode,
    js_value: u64,
    modifier_flags_cache: i32,
    transform_flags: i32,
};

/// Convert JSValue to address for registry lookup
/// Uses the raw 128-bit representation as a unique identifier
pub inline fn jsvalueToAddr(val: JSValue) u64 {
    // Use both tag and value parts for unique address
    // This matches the C macro: union { JSValue v; uint64_t u; } u; u.u = val;
    // For 16-byte JSValue, we use tag as high bits, value.ptr as low bits
    const ptr_bits: u64 = @intFromPtr(val.u.ptr);
    const tag_bits: u64 = @bitCast(val.tag);
    return ptr_bits ^ (tag_bits << 32);
}

/// Fast lookup for native node - returns null if not registered
pub extern fn native_node_lookup(js_addr: u64) ?*NativeAstNode;

/// Get node.kind with native fast path
/// Falls back to JSValue.getPropertyStr if not a native node
pub inline fn nativeGetKind(ctx: *JSContext, obj: JSValue) JSValue {
    const native = native_node_lookup(jsvalueToAddr(obj));
    if (native) |node| {
        return JSValue.newInt(node.kind);
    }
    return JSValue.getPropertyStr(ctx, obj, "kind");
}

/// Get node.flags with native fast path
pub inline fn nativeGetFlags(ctx: *JSContext, obj: JSValue) JSValue {
    const native = native_node_lookup(jsvalueToAddr(obj));
    if (native) |node| {
        return JSValue.newInt(node.flags);
    }
    return JSValue.getPropertyStr(ctx, obj, "flags");
}

/// Get node.pos with native fast path
pub inline fn nativeGetPos(ctx: *JSContext, obj: JSValue) JSValue {
    const native = native_node_lookup(jsvalueToAddr(obj));
    if (native) |node| {
        return JSValue.newInt(node.pos);
    }
    return JSValue.getPropertyStr(ctx, obj, "pos");
}

/// Get node.end with native fast path
pub inline fn nativeGetEnd(ctx: *JSContext, obj: JSValue) JSValue {
    const native = native_node_lookup(jsvalueToAddr(obj));
    if (native) |node| {
        return JSValue.newInt(node.end);
    }
    return JSValue.getPropertyStr(ctx, obj, "end");
}

/// Get node.parent with native fast path
/// Returns the original JSValue of the parent if found, otherwise falls back
pub inline fn nativeGetParent(ctx: *JSContext, obj: JSValue) JSValue {
    const native = native_node_lookup(jsvalueToAddr(obj));
    if (native) |node| {
        if (node.parent) |parent| {
            // Reconstruct JSValue from stored js_value bits
            // The parent's js_value contains the original JSValue bits
            const parent_val = JSValue{
                .u = .{ .ptr = @ptrFromInt(@as(usize, @truncate(parent.js_value))) },
                .tag = @bitCast(@as(i64, @intCast(parent.js_value >> 32))),
            };
            return JSValue.dup(ctx, parent_val);
        }
    }
    return JSValue.getPropertyStr(ctx, obj, "parent");
}

/// Check if a property name is optimizable with native shapes
pub fn isNativeProperty(name: []const u8) bool {
    return std.mem.eql(u8, name, "kind") or
        std.mem.eql(u8, name, "flags") or
        std.mem.eql(u8, name, "pos") or
        std.mem.eql(u8, name, "end") or
        std.mem.eql(u8, name, "parent");
}

// ============================================================================
// Tests
// ============================================================================

test "JSValue size" {
    // Must match QuickJS 64-bit non-NAN-boxing layout
    try std.testing.expectEqual(@sizeOf(JSValue), 16);
    try std.testing.expectEqual(@sizeOf(JSValueUnion), 8);
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
