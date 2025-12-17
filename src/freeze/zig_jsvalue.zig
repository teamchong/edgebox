//! Zig JSValue Type
//!
//! ABI-compatible JSValue for frozen function execution.
//! Matches QuickJS's non-NaN-boxing layout exactly.
//!
//! Layout (16 bytes):
//!   - u: JSValueUnion (8 bytes) - int32, float64, or ptr
//!   - tag: i64 (8 bytes) - type tag

const std = @import("std");

/// JSValue union - matches QuickJS JSValueUnion exactly
pub const JSValueUnion = extern union {
    int32: i32,
    float64: f64,
    ptr: ?*anyopaque,
    short_big_int: i32,
};

/// JSValue struct - matches QuickJS JSValue exactly (16 bytes)
pub const JSValue = extern struct {
    u: JSValueUnion,
    tag: i64,

    // ========================================================================
    // Tag Constants (from quickjs.h)
    // ========================================================================

    // Negative tags (reference counted - objects, strings, etc.)
    pub const TAG_FIRST: i64 = -9;
    pub const TAG_BIG_INT: i64 = -9;
    pub const TAG_SYMBOL: i64 = -8;
    pub const TAG_STRING: i64 = -7;
    pub const TAG_MODULE: i64 = -3;
    pub const TAG_FUNCTION_BYTECODE: i64 = -2;
    pub const TAG_OBJECT: i64 = -1;

    // Non-negative tags (immediate values - no refcount)
    pub const TAG_INT: i64 = 0;
    pub const TAG_BOOL: i64 = 1;
    pub const TAG_NULL: i64 = 2;
    pub const TAG_UNDEFINED: i64 = 3;
    pub const TAG_UNINITIALIZED: i64 = 4;
    pub const TAG_CATCH_OFFSET: i64 = 5;
    pub const TAG_EXCEPTION: i64 = 6;
    pub const TAG_SHORT_BIG_INT: i64 = 7;
    pub const TAG_FLOAT64: i64 = 8;

    // ========================================================================
    // Constructors
    // ========================================================================

    /// Create int32 JSValue (SINT - no refcount)
    pub inline fn initInt(val: i32) JSValue {
        return .{ .u = .{ .int32 = val }, .tag = TAG_INT };
    }

    /// Create bool JSValue (SINT - no refcount)
    pub inline fn initBool(val: bool) JSValue {
        return .{ .u = .{ .int32 = if (val) 1 else 0 }, .tag = TAG_BOOL };
    }

    /// Create undefined JSValue (SINT - no refcount)
    pub inline fn initUndefined() JSValue {
        return .{ .u = .{ .int32 = 0 }, .tag = TAG_UNDEFINED };
    }

    /// Create null JSValue (SINT - no refcount)
    pub inline fn initNull() JSValue {
        return .{ .u = .{ .int32 = 0 }, .tag = TAG_NULL };
    }

    /// Create float64 JSValue
    pub inline fn initFloat64(val: f64) JSValue {
        return .{ .u = .{ .float64 = val }, .tag = TAG_FLOAT64 };
    }

    /// Create exception JSValue
    pub inline fn initException() JSValue {
        return .{ .u = .{ .int32 = 0 }, .tag = TAG_EXCEPTION };
    }

    // ========================================================================
    // Type Checks (inlined for SINT fast path)
    // ========================================================================

    /// Check if value is int32 (SINT fast path)
    pub inline fn isInt(self: JSValue) bool {
        return self.tag == TAG_INT;
    }

    /// Check if value is bool
    pub inline fn isBool(self: JSValue) bool {
        return self.tag == TAG_BOOL;
    }

    /// Check if value is null
    pub inline fn isNull(self: JSValue) bool {
        return self.tag == TAG_NULL;
    }

    /// Check if value is undefined
    pub inline fn isUndefined(self: JSValue) bool {
        return self.tag == TAG_UNDEFINED;
    }

    /// Check if value is float64
    pub inline fn isFloat64(self: JSValue) bool {
        return self.tag == TAG_FLOAT64;
    }

    /// Check if value is exception
    pub inline fn isException(self: JSValue) bool {
        return self.tag == TAG_EXCEPTION;
    }

    /// Check if value is object (negative tag)
    pub inline fn isObject(self: JSValue) bool {
        return self.tag == TAG_OBJECT;
    }

    /// Check if value is string
    pub inline fn isString(self: JSValue) bool {
        return self.tag == TAG_STRING;
    }

    /// Check if value is number (int32 or float64)
    pub inline fn isNumber(self: JSValue) bool {
        return self.tag == TAG_INT or self.tag == TAG_FLOAT64;
    }

    /// Check if value needs reference counting (negative tags)
    /// This is the key SINT optimization - skip refcount for immediates
    pub inline fn needsRefCount(self: JSValue) bool {
        return self.tag < 0;
    }

    /// Check if value is falsy (false, 0, null, undefined, NaN, "")
    pub inline fn isFalsy(self: JSValue) bool {
        return switch (self.tag) {
            TAG_BOOL => self.u.int32 == 0,
            TAG_INT => self.u.int32 == 0,
            TAG_NULL, TAG_UNDEFINED => true,
            TAG_FLOAT64 => self.u.float64 == 0.0 or std.math.isNan(self.u.float64),
            else => false, // Objects, strings need runtime check
        };
    }

    /// Check if two JSValues point to the same object (V8-style caching)
    /// For reference types (objects, arrays, strings), compares pointers
    /// For immediates (int, bool), compares values
    pub inline fn isSameObject(self: JSValue, other: JSValue) bool {
        if (self.tag != other.tag) return false;
        if (self.tag < 0) {
            // Reference types: compare pointers
            return self.u.ptr == other.u.ptr;
        } else {
            // Immediate types: compare int32 values
            return self.u.int32 == other.u.int32;
        }
    }

    // ========================================================================
    // Value Extraction
    // ========================================================================

    /// Get int32 value (assumes isInt() is true)
    pub inline fn toInt(self: JSValue) i32 {
        return self.u.int32;
    }

    /// Get bool value (assumes isBool() is true)
    pub inline fn toBool(self: JSValue) bool {
        return self.u.int32 != 0;
    }

    /// Get float64 value (assumes isFloat64() is true)
    pub inline fn toFloat64(self: JSValue) f64 {
        return self.u.float64;
    }

    /// Get pointer value (for objects, strings, etc.)
    pub inline fn toPtr(self: JSValue) ?*anyopaque {
        return self.u.ptr;
    }

    /// Convert to f64 (int32 or float64)
    pub inline fn toNumber(self: JSValue) f64 {
        return if (self.tag == TAG_INT)
            @floatFromInt(self.u.int32)
        else
            self.u.float64;
    }

    // ========================================================================
    // SINT Arithmetic (no boxing overhead in hot path)
    // ========================================================================

    /// Add two JSValues (SINT fast path)
    /// Returns null if fallback to runtime needed
    pub inline fn addSint(a: JSValue, b: JSValue) ?JSValue {
        if (a.tag == TAG_INT and b.tag == TAG_INT) {
            // Wrapping add to match JS semantics for int32
            const result = a.u.int32 +% b.u.int32;
            return initInt(result);
        }
        return null; // Fallback needed
    }

    /// Subtract two JSValues (SINT fast path)
    pub inline fn subSint(a: JSValue, b: JSValue) ?JSValue {
        if (a.tag == TAG_INT and b.tag == TAG_INT) {
            const result = a.u.int32 -% b.u.int32;
            return initInt(result);
        }
        return null;
    }

    /// Multiply two JSValues (SINT fast path)
    pub inline fn mulSint(a: JSValue, b: JSValue) ?JSValue {
        if (a.tag == TAG_INT and b.tag == TAG_INT) {
            const result = a.u.int32 *% b.u.int32;
            return initInt(result);
        }
        return null;
    }

    /// Divide two JSValues (returns float64 for precision)
    pub inline fn divSint(a: JSValue, b: JSValue) ?JSValue {
        if (a.tag == TAG_INT and b.tag == TAG_INT) {
            if (b.u.int32 == 0) return null; // Division by zero
            // JS division always returns float
            const af: f64 = @floatFromInt(a.u.int32);
            const bf: f64 = @floatFromInt(b.u.int32);
            return initFloat64(af / bf);
        }
        return null;
    }

    /// Modulo two JSValues (SINT fast path)
    pub inline fn modSint(a: JSValue, b: JSValue) ?JSValue {
        if (a.tag == TAG_INT and b.tag == TAG_INT) {
            if (b.u.int32 == 0) return null;
            const result = @rem(a.u.int32, b.u.int32);
            return initInt(result);
        }
        return null;
    }

    /// Bitwise AND (SINT fast path)
    pub inline fn andSint(a: JSValue, b: JSValue) ?JSValue {
        if (a.tag == TAG_INT and b.tag == TAG_INT) {
            return initInt(a.u.int32 & b.u.int32);
        }
        return null;
    }

    /// Bitwise OR (SINT fast path)
    pub inline fn orSint(a: JSValue, b: JSValue) ?JSValue {
        if (a.tag == TAG_INT and b.tag == TAG_INT) {
            return initInt(a.u.int32 | b.u.int32);
        }
        return null;
    }

    /// Bitwise XOR (SINT fast path)
    pub inline fn xorSint(a: JSValue, b: JSValue) ?JSValue {
        if (a.tag == TAG_INT and b.tag == TAG_INT) {
            return initInt(a.u.int32 ^ b.u.int32);
        }
        return null;
    }

    /// Left shift (SINT fast path)
    pub inline fn shlSint(a: JSValue, b: JSValue) ?JSValue {
        if (a.tag == TAG_INT and b.tag == TAG_INT) {
            const shift: u5 = @intCast(@as(u32, @bitCast(b.u.int32)) & 0x1f);
            return initInt(a.u.int32 << shift);
        }
        return null;
    }

    /// Right shift signed (SINT fast path)
    pub inline fn shrSint(a: JSValue, b: JSValue) ?JSValue {
        if (a.tag == TAG_INT and b.tag == TAG_INT) {
            const shift: u5 = @intCast(@as(u32, @bitCast(b.u.int32)) & 0x1f);
            return initInt(a.u.int32 >> shift);
        }
        return null;
    }

    /// Right shift unsigned (SINT fast path)
    pub inline fn ushrSint(a: JSValue, b: JSValue) ?JSValue {
        if (a.tag == TAG_INT and b.tag == TAG_INT) {
            const shift: u5 = @intCast(@as(u32, @bitCast(b.u.int32)) & 0x1f);
            const ua: u32 = @bitCast(a.u.int32);
            return initInt(@bitCast(ua >> shift));
        }
        return null;
    }

    // ========================================================================
    // SINT Comparisons (no boxing overhead)
    // ========================================================================

    /// Less than (SINT fast path)
    pub inline fn ltSint(a: JSValue, b: JSValue) ?JSValue {
        if (a.tag == TAG_INT and b.tag == TAG_INT) {
            return initBool(a.u.int32 < b.u.int32);
        }
        return null;
    }

    /// Less than or equal (SINT fast path)
    pub inline fn leSint(a: JSValue, b: JSValue) ?JSValue {
        if (a.tag == TAG_INT and b.tag == TAG_INT) {
            return initBool(a.u.int32 <= b.u.int32);
        }
        return null;
    }

    /// Greater than (SINT fast path)
    pub inline fn gtSint(a: JSValue, b: JSValue) ?JSValue {
        if (a.tag == TAG_INT and b.tag == TAG_INT) {
            return initBool(a.u.int32 > b.u.int32);
        }
        return null;
    }

    /// Greater than or equal (SINT fast path)
    pub inline fn geSint(a: JSValue, b: JSValue) ?JSValue {
        if (a.tag == TAG_INT and b.tag == TAG_INT) {
            return initBool(a.u.int32 >= b.u.int32);
        }
        return null;
    }

    /// Strict equality (SINT fast path)
    pub inline fn eqSint(a: JSValue, b: JSValue) ?JSValue {
        if (a.tag == TAG_INT and b.tag == TAG_INT) {
            return initBool(a.u.int32 == b.u.int32);
        }
        // Same tag types can be compared directly
        if (a.tag == b.tag) {
            return switch (a.tag) {
                TAG_BOOL => initBool(a.u.int32 == b.u.int32),
                TAG_NULL, TAG_UNDEFINED => initBool(true),
                TAG_FLOAT64 => initBool(a.u.float64 == b.u.float64),
                else => null, // Objects, strings need runtime
            };
        }
        // Different tags - check null/undefined
        if ((a.tag == TAG_NULL and b.tag == TAG_UNDEFINED) or
            (a.tag == TAG_UNDEFINED and b.tag == TAG_NULL))
        {
            return initBool(false); // null !== undefined
        }
        return null;
    }

    /// Not equal (SINT fast path)
    pub inline fn neSint(a: JSValue, b: JSValue) ?JSValue {
        if (eqSint(a, b)) |eq| {
            return initBool(!eq.toBool());
        }
        return null;
    }

    // ========================================================================
    // Unary Operations
    // ========================================================================

    /// Negate (SINT fast path)
    pub inline fn negSint(a: JSValue) ?JSValue {
        if (a.tag == TAG_INT) {
            return initInt(-%a.u.int32);
        }
        return null;
    }

    /// Bitwise NOT (SINT fast path)
    pub inline fn notSint(a: JSValue) ?JSValue {
        if (a.tag == TAG_INT) {
            return initInt(~a.u.int32);
        }
        return null;
    }

    /// Logical NOT
    pub inline fn lnotSint(a: JSValue) ?JSValue {
        return initBool(a.isFalsy());
    }

    /// Increment (SINT fast path)
    pub inline fn incSint(a: JSValue) ?JSValue {
        if (a.tag == TAG_INT) {
            return initInt(a.u.int32 +% 1);
        }
        return null;
    }

    /// Decrement (SINT fast path)
    pub inline fn decSint(a: JSValue) ?JSValue {
        if (a.tag == TAG_INT) {
            return initInt(a.u.int32 -% 1);
        }
        return null;
    }

    // ========================================================================
    // Type Conversion
    // ========================================================================

    /// Convert to int32 (for bitwise ops)
    pub inline fn toInt32(self: JSValue) ?i32 {
        return switch (self.tag) {
            TAG_INT => self.u.int32,
            TAG_BOOL => self.u.int32,
            TAG_FLOAT64 => @intFromFloat(self.u.float64),
            TAG_NULL, TAG_UNDEFINED => 0,
            else => null,
        };
    }
};

/// Opaque JSContext pointer (for ABI compatibility)
pub const JSContext = opaque {};

/// Frozen function signature (matches JS_CFunction)
pub const FrozenFn = *const fn (*JSContext, JSValue, c_int, [*]JSValue) callconv(.c) JSValue;

/// Frozen function entry for registration
pub const FrozenEntry = struct {
    name: [*:0]const u8,
    func: FrozenFn,
    argc: c_int,
};

// ============================================================================
// External QuickJS Functions (imported via C ABI)
// ============================================================================

/// Get global object
pub extern fn JS_GetGlobalObject(ctx: *JSContext) JSValue;

/// Free a JSValue
pub extern fn JS_FreeValue(ctx: *JSContext, val: JSValue) void;

/// Duplicate a JSValue (increment refcount)
pub extern fn JS_DupValue(ctx: *JSContext, val: JSValue) JSValue;

/// Create a new C function
pub extern fn JS_NewCFunction2(
    ctx: *JSContext,
    func: FrozenFn,
    name: [*:0]const u8,
    length: c_int,
    cproto: c_int,
    magic: c_int,
) JSValue;

/// Set property on object by string name
pub extern fn JS_SetPropertyStr(ctx: *JSContext, this_obj: JSValue, prop: [*:0]const u8, val: JSValue) c_int;

/// Throw a RangeError
pub extern fn JS_ThrowRangeError(ctx: *JSContext, fmt: [*:0]const u8, ...) JSValue;

/// Throw a TypeError
pub extern fn JS_ThrowTypeError(ctx: *JSContext, fmt: [*:0]const u8, ...) JSValue;

/// Runtime arithmetic (fallback)
pub extern fn JS_Add(ctx: *JSContext, a: JSValue, b: JSValue) JSValue;
pub extern fn JS_Sub(ctx: *JSContext, a: JSValue, b: JSValue) JSValue;
pub extern fn JS_Mul(ctx: *JSContext, a: JSValue, b: JSValue) JSValue;
pub extern fn JS_Div(ctx: *JSContext, a: JSValue, b: JSValue) JSValue;

/// Runtime comparison (fallback)
pub extern fn JS_LT(ctx: *JSContext, a: JSValue, b: JSValue) c_int;
pub extern fn JS_LE(ctx: *JSContext, a: JSValue, b: JSValue) c_int;
pub extern fn JS_EQ(ctx: *JSContext, a: JSValue, b: JSValue) c_int;

/// Call a JS function
pub extern fn JS_Call(ctx: *JSContext, func: JSValue, this: JSValue, argc: c_int, argv: [*]const JSValue) JSValue;

/// Throw OutOfMemory error
pub extern fn JS_ThrowOutOfMemory(ctx: *JSContext) JSValue;

// ============================================================================
// Memory Allocation (via QuickJS's allocator)
// ============================================================================

/// Allocate memory using QuickJS's context allocator
pub extern fn js_malloc(ctx: *JSContext, size: usize) ?*anyopaque;

/// Free memory allocated with js_malloc
pub extern fn js_free(ctx: *JSContext, ptr: ?*anyopaque) void;

/// Reallocate memory using QuickJS's context allocator
pub extern fn js_realloc(ctx: *JSContext, ptr: ?*anyopaque, size: usize) ?*anyopaque;

// ============================================================================
// Property Access (for get_field, put_field, get_array_el, etc.)
// ============================================================================

/// Get property by string name
pub extern fn JS_GetPropertyStr(ctx: *JSContext, this_obj: JSValue, prop: [*:0]const u8) JSValue;

/// Get property by uint32 index (array access)
pub extern fn JS_GetPropertyUint32(ctx: *JSContext, this_obj: JSValue, idx: u32) JSValue;

/// Set property by uint32 index (array access)
pub extern fn JS_SetPropertyUint32(ctx: *JSContext, this_obj: JSValue, idx: u32, val: JSValue) c_int;

/// Get property by int64 index (large array access)
pub extern fn JS_GetPropertyInt64(ctx: *JSContext, this_obj: JSValue, idx: i64) JSValue;

/// Set property by int64 index (large array access)
pub extern fn JS_SetPropertyInt64(ctx: *JSContext, this_obj: JSValue, idx: i64, val: JSValue) c_int;

/// Get array/string length
pub extern fn JS_GetLength(ctx: *JSContext, obj: JSValue, plen: *i64) c_int;

/// Delete property by atom
pub extern fn JS_DeleteProperty(ctx: *JSContext, this_obj: JSValue, prop: i32, flags: c_int) c_int;

/// Check if property exists (for 'in' operator)
pub extern fn JS_HasProperty(ctx: *JSContext, this_obj: JSValue, prop: i32) c_int;

/// Get property by atom
pub extern fn JS_GetProperty(ctx: *JSContext, this_obj: JSValue, prop: i32) JSValue;

/// Set property by atom
pub extern fn JS_SetProperty(ctx: *JSContext, this_obj: JSValue, prop: i32, val: JSValue) c_int;

/// Define property with getter/setter
pub extern fn JS_DefinePropertyValue(ctx: *JSContext, this_obj: JSValue, prop: i32, val: JSValue, flags: c_int) c_int;

// ============================================================================
// Object Creation
// ============================================================================

/// Create a new empty object
pub extern fn JS_NewObject(ctx: *JSContext) JSValue;

/// Create a new array
pub extern fn JS_NewArray(ctx: *JSContext) JSValue;

/// Create arguments object
pub extern fn JS_NewArguments(ctx: *JSContext, argc: c_int, argv: [*]const JSValue) JSValue;

// ============================================================================
// Type Checking and Conversion (runtime)
// ============================================================================

/// Check if value is an array
pub extern fn JS_IsArray(ctx: *JSContext, val: JSValue) c_int;

/// Check if value is a function
pub extern fn JS_IsFunction(ctx: *JSContext, val: JSValue) c_int;

/// Check if value is a constructor
pub extern fn JS_IsConstructor(ctx: *JSContext, val: JSValue) c_int;

/// Get typeof string
pub extern fn JS_TypeOf(ctx: *JSContext, val: JSValue) JSValue;

/// instanceof operator
pub extern fn JS_IsInstanceOf(ctx: *JSContext, val: JSValue, obj: JSValue) c_int;

/// Convert to string
pub extern fn JS_ToString(ctx: *JSContext, val: JSValue) JSValue;

/// Convert to number
pub extern fn JS_ToNumber(ctx: *JSContext, val: JSValue) JSValue;

/// Convert to int32
pub extern fn JS_ToInt32(ctx: *JSContext, pres: *i32, val: JSValue) c_int;

/// Convert to int64
pub extern fn JS_ToInt64(ctx: *JSContext, pres: *i64, val: JSValue) c_int;

/// Convert to float64
pub extern fn JS_ToFloat64(ctx: *JSContext, pres: *f64, val: JSValue) c_int;

// ============================================================================
// Frozen Interpreter Helpers (from patches/002-frozen-interpreter-helpers.patch)
// ============================================================================

// --- Iterator Helpers ---

/// Start a for-in iteration
/// sp[0] = object to iterate, sp[-1] = undefined
/// Returns: 0 on success, <0 on error
pub extern fn js_frozen_for_in_start(ctx: *JSContext, sp: [*]JSValue) c_int;

/// Get next for-in key
/// sp[-1] = iterator, sp[-2] = object
/// Returns: 0 = done, 1 = has next, <0 = error
pub extern fn js_frozen_for_in_next(ctx: *JSContext, sp: [*]JSValue) c_int;

/// Start a for-of iteration
/// sp[0] = iterable
/// is_async: 0 = sync, 1 = async
/// Returns: 0 on success, <0 on error
pub extern fn js_frozen_for_of_start(ctx: *JSContext, sp: [*]JSValue, is_async: c_int) c_int;

/// Get next for-of value
/// offset: stack offset adjustment
/// Returns: 0 = done, 1 = has next, <0 = error
pub extern fn js_frozen_for_of_next(ctx: *JSContext, sp: [*]JSValue, offset: c_int) c_int;

// --- Closure Variable Access ---

/// Opaque type for closure variable references
// --- Home Object (for super keyword) ---

/// Register home object for a frozen function (needed for super.method())
/// func_name: name of the frozen function
/// home_object: the class prototype object
pub extern fn JS_SetFrozenHomeObject(ctx: *JSContext, func_name: [*:0]const u8, home_object: JSValue) void;

/// Get home object for a frozen function
pub extern fn JS_GetFrozenHomeObject(ctx: *JSContext, func_name: [*:0]const u8) JSValue;

/// Get current import.meta object
pub extern fn JS_GetImportMetaCurrent(ctx: *JSContext) JSValue;

/// Create mapped arguments object with callee property
pub extern fn JS_NewMappedArgumentsSimple(ctx: *JSContext, argc: c_int, argv: [*]const JSValue, callee: JSValue) JSValue;

// --- Private Fields / Class Brands ---

/// Create a new private symbol from atom
pub extern fn JS_NewPrivateSymbol(ctx: *JSContext, atom: i32) JSValue;

/// Check if object has the brand of the class
/// Returns: 1 = has brand, 0 = no brand, <0 = error
pub extern fn JS_FrozenCheckBrand(ctx: *JSContext, obj: JSValue, func: JSValue) c_int;

/// Add brand to object (call in constructor)
/// Returns: 0 on success, <0 on error
pub extern fn JS_FrozenAddBrand(ctx: *JSContext, obj: JSValue, func: JSValue) c_int;

/// Check if private name is in object (for 'in' operator with private fields)
/// sp[0] = object, sp[-1] = private name
pub extern fn js_frozen_private_in(ctx: *JSContext, sp: [*]JSValue) c_int;

/// Get private field value
pub extern fn JS_FrozenGetPrivateField(ctx: *JSContext, obj: JSValue, name: JSValue) JSValue;

/// Set private field value
/// Returns: 0 on success, <0 on error
pub extern fn JS_FrozenSetPrivateField(ctx: *JSContext, obj: JSValue, name: JSValue, val: JSValue) c_int;

/// Define (create) private field on object
/// Returns: 0 on success, <0 on error
pub extern fn JS_FrozenDefinePrivateField(ctx: *JSContext, obj: JSValue, name: JSValue, val: JSValue) c_int;

// ============================================================================
// SINT Helper: Dup/Free with tag check
// ============================================================================

/// Duplicate value only if it needs refcount (SINT optimization)
pub inline fn dupValue(ctx: *JSContext, v: JSValue) JSValue {
    if (v.needsRefCount()) {
        return JS_DupValue(ctx, v);
    }
    return v;
}

/// Free value only if it needs refcount (SINT optimization)
pub inline fn freeValue(ctx: *JSContext, v: JSValue) void {
    if (v.needsRefCount()) {
        JS_FreeValue(ctx, v);
    }
}

// ============================================================================
// SIMD-Accelerated Array Operations
// ============================================================================

const builtin = @import("builtin");

/// SIMD-accelerated int32 array sum (4 elements per iteration)
/// Returns null if type guard fails (non-int32 elements) or unsupported platform
/// For WASM32, uses @Vector which maps to SIMD instructions
pub fn sumInt32ArraySimd(ctx: *JSContext, arr: JSValue, len: i64) ?i64 {
    // Only enable SIMD on wasm32 where it provides benefit
    if (builtin.cpu.arch != .wasm32) return null;
    if (len <= 0) return 0;

    // Use Zig's @Vector for SIMD - compiler generates v128 ops on WASM
    var sum_vec: @Vector(4, i32) = @splat(0);
    var scalar_sum: i64 = 0;
    var i: usize = 0;
    const arr_len: usize = @intCast(len);

    // SIMD loop: process 4 int32s at once
    while (i + 4 <= arr_len) : (i += 4) {
        var vals: [4]i32 = undefined;
        var all_int32 = true;

        // Load and check 4 values
        inline for (0..4) |j| {
            const val = JS_GetPropertyUint32(ctx, arr, @intCast(i + j));
            if (val.tag == JSValue.TAG_INT) {
                vals[j] = val.u.int32;
            } else {
                // Non-int32 found - free and bail
                freeValue(ctx, val);
                all_int32 = false;
                break;
            }
        }

        if (!all_int32) return null; // Type guard failed

        // Vectorized add
        sum_vec = sum_vec + @as(@Vector(4, i32), vals);
    }

    // Horizontal sum: reduce 4 lanes to single value
    scalar_sum = @reduce(.Add, sum_vec);

    // Scalar remainder for leftover elements
    while (i < arr_len) : (i += 1) {
        const val = JS_GetPropertyUint32(ctx, arr, @intCast(i));
        if (val.tag == JSValue.TAG_INT) {
            scalar_sum += val.u.int32;
        } else {
            freeValue(ctx, val);
            return null; // Type guard failed
        }
    }

    return scalar_sum;
}

/// Scalar fallback for int32 array sum (for non-WASM or mixed types)
pub fn sumInt32ArrayScalar(ctx: *JSContext, arr: JSValue, len: i64) ?i64 {
    if (len <= 0) return 0;

    var sum: i64 = 0;
    var i: usize = 0;
    const arr_len: usize = @intCast(len);

    while (i < arr_len) : (i += 1) {
        const val = JS_GetPropertyUint32(ctx, arr, @intCast(i));
        if (val.tag == JSValue.TAG_INT) {
            sum += val.u.int32;
        } else {
            freeValue(ctx, val);
            return null; // Type guard failed
        }
    }

    return sum;
}

/// Auto-select best array sum implementation
pub fn sumInt32Array(ctx: *JSContext, arr: JSValue, len: i64) ?i64 {
    // Try SIMD first (on supported platforms)
    if (sumInt32ArraySimd(ctx, arr, len)) |sum| {
        return sum;
    }
    // Fallback to scalar
    return sumInt32ArrayScalar(ctx, arr, len);
}

// ============================================================================
// Tests
// ============================================================================

test "JSValue size" {
    // Must be 16 bytes for ABI compatibility
    try std.testing.expectEqual(@as(usize, 16), @sizeOf(JSValue));
    try std.testing.expectEqual(@as(usize, 8), @sizeOf(JSValueUnion));
}

test "JSValue constructors" {
    const i = JSValue.initInt(42);
    try std.testing.expect(i.isInt());
    try std.testing.expectEqual(@as(i32, 42), i.toInt());

    const b = JSValue.initBool(true);
    try std.testing.expect(b.isBool());
    try std.testing.expect(b.toBool());

    const u = JSValue.initUndefined();
    try std.testing.expect(u.isUndefined());

    const n = JSValue.initNull();
    try std.testing.expect(n.isNull());
}

test "SINT arithmetic" {
    const a = JSValue.initInt(10);
    const b = JSValue.initInt(3);

    // Add
    const sum = JSValue.addSint(a, b).?;
    try std.testing.expectEqual(@as(i32, 13), sum.toInt());

    // Sub
    const diff = JSValue.subSint(a, b).?;
    try std.testing.expectEqual(@as(i32, 7), diff.toInt());

    // Mul
    const prod = JSValue.mulSint(a, b).?;
    try std.testing.expectEqual(@as(i32, 30), prod.toInt());

    // Compare
    const lt = JSValue.ltSint(b, a).?;
    try std.testing.expect(lt.toBool());
}

test "needsRefCount" {
    // Immediate values don't need refcount (SINT optimization)
    try std.testing.expect(!JSValue.initInt(42).needsRefCount());
    try std.testing.expect(!JSValue.initBool(true).needsRefCount());
    try std.testing.expect(!JSValue.initNull().needsRefCount());
    try std.testing.expect(!JSValue.initUndefined().needsRefCount());
    try std.testing.expect(!JSValue.initFloat64(3.14).needsRefCount());

    // Objects/strings would need refcount (negative tags)
    const obj = JSValue{ .u = .{ .ptr = null }, .tag = JSValue.TAG_OBJECT };
    try std.testing.expect(obj.needsRefCount());
}
