//! Numeric Arithmetic and Comparison Operations
//!
//! Provides inline fast paths for SMI (Small Integer) operations
//! with fallback to QuickJS for complex cases (strings, BigInt, etc.)

const std = @import("std");
const js_value = @import("js_value.zig");

const JSContext = js_value.JSContext;
const JSValue = js_value.JSValue;
const quickjs = js_value.quickjs;

// ============================================================================
// Shared FFI methods (work for both WASM32 and native via quickjs extern)
// ============================================================================

/// Call a JavaScript function
pub fn jsCall(ctx: *JSContext, func: JSValue, this: JSValue, args: []const JSValue) JSValue {
    return quickjs.JS_Call(ctx, func, this, @intCast(args.len), @ptrCast(args.ptr));
}

/// Get property by string name
pub fn jsGetPropertyStr(ctx: *JSContext, obj: JSValue, name: [*:0]const u8) JSValue {
    return quickjs.JS_GetPropertyStr(ctx, obj, name);
}

/// Set property by string name
pub fn jsSetPropertyStr(ctx: *JSContext, obj: JSValue, name: [*:0]const u8, val: JSValue) c_int {
    return quickjs.JS_SetPropertyStr(ctx, obj, name, val);
}

/// Get array element by index
pub fn jsGetPropertyUint32(ctx: *JSContext, obj: JSValue, idx: u32) JSValue {
    return quickjs.JS_GetPropertyUint32(ctx, obj, idx);
}

/// Set array element by index
pub fn jsSetPropertyUint32(ctx: *JSContext, obj: JSValue, idx: u32, val: JSValue) c_int {
    return quickjs.JS_SetPropertyUint32(ctx, obj, idx, val);
}

/// Convert to boolean
pub fn jsToBool(ctx: *JSContext, val: JSValue) c_int {
    return quickjs.JS_ToBool(ctx, val);
}

/// Convert to int32
pub fn jsToInt32(ctx: *JSContext, pres: *i32, val: JSValue) c_int {
    return quickjs.JS_ToInt32(ctx, pres, val);
}

/// Convert to float64
pub fn jsToFloat64(ctx: *JSContext, pres: *f64, val: JSValue) c_int {
    return quickjs.JS_ToFloat64(ctx, pres, val);
}

/// Create a new object
pub fn jsNewObject(ctx: *JSContext) JSValue {
    return quickjs.JS_NewObject(ctx);
}

/// Create a new array
pub fn jsNewArray(ctx: *JSContext) JSValue {
    return quickjs.JS_NewArray(ctx);
}

/// Create a new string
pub fn jsNewString(ctx: *JSContext, str: [*:0]const u8) JSValue {
    return quickjs.JS_NewString(ctx, str);
}

/// Throw a type error
pub fn jsThrowTypeError(ctx: *JSContext, fmt: [*:0]const u8) JSValue {
    return quickjs.JS_ThrowTypeError(ctx, fmt);
}

/// Throw a range error
pub fn jsThrowRangeError(ctx: *JSContext, fmt: [*:0]const u8) JSValue {
    return quickjs.JS_ThrowRangeError(ctx, fmt);
}

/// Throw a reference error
pub fn jsThrowReferenceError(ctx: *JSContext, fmt: [*:0]const u8) JSValue {
    return quickjs.JS_ThrowReferenceError(ctx, fmt);
}

/// Get a global variable by name
pub fn jsGetGlobal(ctx: *JSContext, name: [*:0]const u8) JSValue {
    const global = quickjs.JS_GetGlobalObject(ctx);
    defer jsFreeValue(ctx, global);
    return quickjs.JS_GetPropertyStr(ctx, global, name);
}

/// Define property on object
pub fn jsDefinePropertyStr(ctx: *JSContext, obj: JSValue, name: [*:0]const u8, val: JSValue) c_int {
    return quickjs.JS_DefinePropertyValueStr(ctx, obj, name, val, quickjs.JS_PROP_C_W_E);
}

/// Call function as constructor
pub fn jsCallConstructor(ctx: *JSContext, func: JSValue, args: []const JSValue) JSValue {
    return quickjs.JS_CallConstructor(ctx, func, @intCast(args.len), @ptrCast(args.ptr));
}

/// Throw an exception value
pub fn jsThrow(ctx: *JSContext, val: JSValue) JSValue {
    return quickjs.JS_Throw(ctx, val);
}

/// Check instanceof
pub fn jsIsInstanceOf(ctx: *JSContext, obj: JSValue, ctor: JSValue) bool {
    return quickjs.JS_IsInstanceOf(ctx, obj, ctor) != 0;
}

/// Free a value (wrapper for external use)
pub fn jsFreeValue(ctx: *JSContext, val: JSValue) void {
    if (val.hasRefCount()) {
        quickjs.JS_FreeValue(ctx, val);
    }
}

/// Duplicate a value (wrapper for external use)
pub fn jsDupValue(ctx: *JSContext, val: JSValue) JSValue {
    if (val.hasRefCount()) {
        return quickjs.JS_DupValue(ctx, val);
    }
    return val;
}

/// Get value from a closure variable reference
/// Used by codegen for accessing closure variables
pub inline fn getVarRef(var_ref: *js_value.JSVarRef) JSValue {
    // Read the value through pvalue pointer (points to the actual value location)
    return var_ref.pvalue.*;
}

// ============================================================================
// Numeric Arithmetic - Inline Fast Paths
// Zero allocation for int32 operands, native f64 for floats
// ============================================================================

/// Add two JSValues with int and float fast paths
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
    // Fast path: float operations (float+float, int+float, float+int)
    if (a.isNumber() and b.isNumber()) {
        return JSValue.newFloat64(a.getNumberAsFloat() + b.getNumberAsFloat());
    }
    // String concatenation or other types - slow path
    return addSlow(ctx, a, b);
}

/// Subtract two JSValues with int and float fast paths
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
    // Fast path: float operations
    if (a.isNumber() and b.isNumber()) {
        return JSValue.newFloat64(a.getNumberAsFloat() - b.getNumberAsFloat());
    }
    return subSlow(ctx, a, b);
}

/// Multiply two JSValues with int and float fast paths
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
    // Fast path: float operations
    if (a.isNumber() and b.isNumber()) {
        return JSValue.newFloat64(a.getNumberAsFloat() * b.getNumberAsFloat());
    }
    return mulSlow(ctx, a, b);
}

/// Divide two JSValues with int and float fast paths
pub inline fn div(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    // Fast path: int / int with exact result
    if (a.isInt() and b.isInt()) {
        const ia = a.getInt();
        const ib = b.getInt();
        if (ib != 0 and @rem(ia, ib) == 0) {
            const result = @divExact(ia, ib);
            return JSValue.newInt(result);
        }
    }
    // Fast path: any numeric division
    if (a.isNumber() and b.isNumber()) {
        return JSValue.newFloat64(a.getNumberAsFloat() / b.getNumberAsFloat());
    }
    return divSlow(ctx, a, b);
}

/// Modulo two JSValues with int and float fast paths
pub inline fn mod(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        const ia = a.getInt();
        const ib = b.getInt();
        if (ib == 0) return JSValue.newFloat64(std.math.nan(f64));
        if (ib == -1 and ia == std.math.minInt(i32)) return JSValue.newInt(0);
        return JSValue.newInt(@rem(ia, ib));
    }
    // Fast path: float modulo
    if (a.isNumber() and b.isNumber()) {
        return JSValue.newFloat64(@mod(a.getNumberAsFloat(), b.getNumberAsFloat()));
    }
    return modSlow(ctx, a, b);
}

/// Negate a JSValue with int and float fast paths
pub inline fn neg(ctx: *JSContext, a: JSValue) JSValue {
    if (a.isInt()) {
        const ia = a.getInt();
        // Special case: -0 must return float -0.0
        if (ia == 0) return JSValue.newFloat64(-0.0);
        // Special case: -INT32_MIN overflows
        if (ia == std.math.minInt(i32)) return JSValue.newFloat64(-@as(f64, @floatFromInt(ia)));
        return JSValue.newInt(-ia);
    }
    // Fast path: float negation
    if (a.isFloat64()) {
        return JSValue.newFloat64(-a.getFloat64());
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
    // Fast path: float comparisons
    if (a.isNumber() and b.isNumber()) {
        return JSValue.newBool(a.getNumberAsFloat() < b.getNumberAsFloat());
    }
    return ltSlow(ctx, a, b);
}

pub inline fn lte(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        return JSValue.newBool(a.getInt() <= b.getInt());
    }
    // Fast path: float comparisons
    if (a.isNumber() and b.isNumber()) {
        return JSValue.newBool(a.getNumberAsFloat() <= b.getNumberAsFloat());
    }
    return lteSlow(ctx, a, b);
}

pub inline fn gt(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        return JSValue.newBool(a.getInt() > b.getInt());
    }
    // Fast path: float comparisons
    if (a.isNumber() and b.isNumber()) {
        return JSValue.newBool(a.getNumberAsFloat() > b.getNumberAsFloat());
    }
    return gtSlow(ctx, a, b);
}

pub inline fn gte(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        return JSValue.newBool(a.getInt() >= b.getInt());
    }
    // Fast path: float comparisons
    if (a.isNumber() and b.isNumber()) {
        return JSValue.newBool(a.getNumberAsFloat() >= b.getNumberAsFloat());
    }
    return gteSlow(ctx, a, b);
}

pub inline fn eq(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        return JSValue.newBool(a.getInt() == b.getInt());
    }
    // Fast path: float comparisons
    if (a.isNumber() and b.isNumber()) {
        return JSValue.newBool(a.getNumberAsFloat() == b.getNumberAsFloat());
    }
    return eqSlow(ctx, a, b);
}

pub inline fn neq(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    if (a.isInt() and b.isInt()) {
        return JSValue.newBool(a.getInt() != b.getInt());
    }
    // Fast path: float comparisons
    if (a.isNumber() and b.isNumber()) {
        return JSValue.newBool(a.getNumberAsFloat() != b.getNumberAsFloat());
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
// Slow Paths - Pure Zig implementations calling QuickJS directly
// These are called when operands are not both SMI integers
// ============================================================================

/// String concatenation helper
fn stringConcat(ctx: *JSContext, str_a: JSValue, str_b: JSValue) JSValue {
    const cstr_a = quickjs.JS_ToCString(ctx, str_a) orelse return JSValue.EXCEPTION;
    const cstr_b = quickjs.JS_ToCString(ctx, str_b) orelse {
        quickjs.JS_FreeCString(ctx, cstr_a);
        return JSValue.EXCEPTION;
    };

    // Get lengths
    var len_a: usize = 0;
    while (cstr_a[len_a] != 0) : (len_a += 1) {}
    var len_b: usize = 0;
    while (cstr_b[len_b] != 0) : (len_b += 1) {}

    // Allocate and copy
    const buf = @as([*]u8, @ptrCast(quickjs.js_malloc(ctx, len_a + len_b + 1) orelse {
        quickjs.JS_FreeCString(ctx, cstr_a);
        quickjs.JS_FreeCString(ctx, cstr_b);
        return JSValue.EXCEPTION;
    }));
    @memcpy(buf[0..len_a], cstr_a[0..len_a]);
    @memcpy(buf[len_a..][0..len_b], cstr_b[0..len_b]);
    buf[len_a + len_b] = 0;

    quickjs.JS_FreeCString(ctx, cstr_a);
    quickjs.JS_FreeCString(ctx, cstr_b);

    // Use JS_NewStringLen directly since buf is not sentinel-terminated in Zig type
    const result = quickjs.JS_NewStringLen(ctx, buf, len_a + len_b);
    quickjs.js_free(ctx, buf);
    return result;
}

fn addSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    // String concatenation path
    if (a.isString() or b.isString()) {
        const str_a = quickjs.JS_ToString(ctx, a);
        if (str_a.isException()) return str_a;
        const str_b = quickjs.JS_ToString(ctx, b);
        if (str_b.isException()) {
            JSValue.free(ctx, str_a);
            return str_b;
        }
        const result = stringConcat(ctx, str_a, str_b);
        JSValue.free(ctx, str_a);
        JSValue.free(ctx, str_b);
        return result;
    }
    // Numeric path
    var da: f64 = 0;
    var db: f64 = 0;
    if (quickjs.JS_ToFloat64(ctx, &da, a) != 0) return JSValue.EXCEPTION;
    if (quickjs.JS_ToFloat64(ctx, &db, b) != 0) return JSValue.EXCEPTION;
    return JSValue.newFloat64(da + db);
}

fn subSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    var da: f64 = 0;
    var db: f64 = 0;
    if (quickjs.JS_ToFloat64(ctx, &da, a) != 0) return JSValue.EXCEPTION;
    if (quickjs.JS_ToFloat64(ctx, &db, b) != 0) return JSValue.EXCEPTION;
    return JSValue.newFloat64(da - db);
}

fn mulSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    var da: f64 = 0;
    var db: f64 = 0;
    if (quickjs.JS_ToFloat64(ctx, &da, a) != 0) return JSValue.EXCEPTION;
    if (quickjs.JS_ToFloat64(ctx, &db, b) != 0) return JSValue.EXCEPTION;
    return JSValue.newFloat64(da * db);
}

fn divSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    var da: f64 = 0;
    var db: f64 = 0;
    if (quickjs.JS_ToFloat64(ctx, &da, a) != 0) return JSValue.EXCEPTION;
    if (quickjs.JS_ToFloat64(ctx, &db, b) != 0) return JSValue.EXCEPTION;
    return JSValue.newFloat64(da / db);
}

fn modSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    var da: f64 = 0;
    var db: f64 = 0;
    if (quickjs.JS_ToFloat64(ctx, &da, a) != 0) return JSValue.EXCEPTION;
    if (quickjs.JS_ToFloat64(ctx, &db, b) != 0) return JSValue.EXCEPTION;
    return JSValue.newFloat64(@mod(da, db));
}

fn negSlow(ctx: *JSContext, a: JSValue) JSValue {
    var da: f64 = 0;
    if (quickjs.JS_ToFloat64(ctx, &da, a) != 0) return JSValue.EXCEPTION;
    return JSValue.newFloat64(-da);
}

fn ltSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    var da: f64 = 0;
    var db: f64 = 0;
    if (quickjs.JS_ToFloat64(ctx, &da, a) != 0) return JSValue.EXCEPTION;
    if (quickjs.JS_ToFloat64(ctx, &db, b) != 0) return JSValue.EXCEPTION;
    return JSValue.newBool(da < db);
}

fn lteSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    var da: f64 = 0;
    var db: f64 = 0;
    if (quickjs.JS_ToFloat64(ctx, &da, a) != 0) return JSValue.EXCEPTION;
    if (quickjs.JS_ToFloat64(ctx, &db, b) != 0) return JSValue.EXCEPTION;
    return JSValue.newBool(da <= db);
}

fn gtSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    var da: f64 = 0;
    var db: f64 = 0;
    if (quickjs.JS_ToFloat64(ctx, &da, a) != 0) return JSValue.EXCEPTION;
    if (quickjs.JS_ToFloat64(ctx, &db, b) != 0) return JSValue.EXCEPTION;
    return JSValue.newBool(da > db);
}

fn gteSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    var da: f64 = 0;
    var db: f64 = 0;
    if (quickjs.JS_ToFloat64(ctx, &da, a) != 0) return JSValue.EXCEPTION;
    if (quickjs.JS_ToFloat64(ctx, &db, b) != 0) return JSValue.EXCEPTION;
    return JSValue.newBool(da >= db);
}

fn eqSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    // Use QuickJS loose equality for full semantics
    const result = quickjs.JS_IsEqual(ctx, a, b);
    if (result < 0) return JSValue.EXCEPTION;
    return JSValue.newBool(result != 0);
}

fn neqSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    const result = quickjs.JS_IsEqual(ctx, a, b);
    if (result < 0) return JSValue.EXCEPTION;
    return JSValue.newBool(result == 0);
}

fn bitAndSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    var ia: i32 = 0;
    var ib: i32 = 0;
    if (quickjs.JS_ToInt32(ctx, &ia, a) != 0) return JSValue.EXCEPTION;
    if (quickjs.JS_ToInt32(ctx, &ib, b) != 0) return JSValue.EXCEPTION;
    return JSValue.newInt(ia & ib);
}

fn bitOrSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    var ia: i32 = 0;
    var ib: i32 = 0;
    if (quickjs.JS_ToInt32(ctx, &ia, a) != 0) return JSValue.EXCEPTION;
    if (quickjs.JS_ToInt32(ctx, &ib, b) != 0) return JSValue.EXCEPTION;
    return JSValue.newInt(ia | ib);
}

fn bitXorSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    var ia: i32 = 0;
    var ib: i32 = 0;
    if (quickjs.JS_ToInt32(ctx, &ia, a) != 0) return JSValue.EXCEPTION;
    if (quickjs.JS_ToInt32(ctx, &ib, b) != 0) return JSValue.EXCEPTION;
    return JSValue.newInt(ia ^ ib);
}

fn bitNotSlow(ctx: *JSContext, a: JSValue) JSValue {
    var ia: i32 = 0;
    if (quickjs.JS_ToInt32(ctx, &ia, a) != 0) return JSValue.EXCEPTION;
    return JSValue.newInt(~ia);
}

fn shlSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    var ia: i32 = 0;
    var ib: u32 = 0;
    if (quickjs.JS_ToInt32(ctx, &ia, a) != 0) return JSValue.EXCEPTION;
    if (quickjs.JS_ToUint32(ctx, &ib, b) != 0) return JSValue.EXCEPTION;
    const shift: u5 = @truncate(ib & 0x1f);
    return JSValue.newInt(ia << shift);
}

fn sarSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    var ia: i32 = 0;
    var ib: u32 = 0;
    if (quickjs.JS_ToInt32(ctx, &ia, a) != 0) return JSValue.EXCEPTION;
    if (quickjs.JS_ToUint32(ctx, &ib, b) != 0) return JSValue.EXCEPTION;
    const shift: u5 = @truncate(ib & 0x1f);
    return JSValue.newInt(ia >> shift);
}

fn shrSlow(ctx: *JSContext, a: JSValue, b: JSValue) JSValue {
    var ia: u32 = 0;
    var ib: u32 = 0;
    if (quickjs.JS_ToUint32(ctx, &ia, a) != 0) return JSValue.EXCEPTION;
    if (quickjs.JS_ToUint32(ctx, &ib, b) != 0) return JSValue.EXCEPTION;
    const shift: u5 = @truncate(ib & 0x1f);
    return JSValue.newInt(@bitCast(ia >> shift));
}
