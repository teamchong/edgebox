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
const builtin = @import("builtin");

// WASM32 uses NaN-boxing (8-byte JSValue), native uses struct (16-byte JSValue)
pub const is_wasm32 = builtin.cpu.arch == .wasm32;

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

/// JSVarRef - QuickJS closure variable reference
/// We only need access to pvalue (pointer to JSValue) for reading closure variables
pub const JSVarRef = extern struct {
    header: u64, // GC header (8 bytes) - union simplified
    pvalue: *JSValue, // Pointer to the closure variable value
    value: JSValue, // Used when variable is no longer on stack
};

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

// ============================================================================
// Compressed JSValue for frozen functions (8 bytes via NaN-boxing)
// Encoding: f64 where NaN bits encode type and payload
// ============================================================================

pub const CompressedValue = packed struct {
    bits: u64,

    // NaN-boxing constants (V8/JSC style)
    const QNAN: u64 = 0x7FF8000000000000; // Quiet NaN
    const TAG_MASK: u64 = 0xFFFF000000000000;
    const PAYLOAD_MASK: u64 = 0x0000FFFFFFFFFFFF;

    // Type tags (stored in upper 16 bits above NaN)
    const TAG_INT: u64 = 0x0001000000000000;
    const TAG_BOOL: u64 = 0x0002000000000000;
    const TAG_NULL: u64 = 0x0003000000000000;
    const TAG_UNDEF: u64 = 0x0004000000000000;
    const TAG_PTR: u64 = 0x0005000000000000; // Object pointer
    const TAG_UNINIT: u64 = 0x0006000000000000;
    const TAG_STR: u64 = 0x0007000000000000; // String pointer
    const TAG_SYMBOL: u64 = 0x0008000000000000; // Symbol pointer
    const TAG_BIGINT: u64 = 0x0009000000000000; // BigInt pointer
    const TAG_FUNC: u64 = 0x000A000000000000; // Function bytecode pointer
    const TAG_EXCEPTION: u64 = 0x000B000000000000; // Exception marker

    pub const UNDEFINED = CompressedValue{ .bits = QNAN | TAG_UNDEF };
    pub const NULL = CompressedValue{ .bits = QNAN | TAG_NULL };
    pub const TRUE = CompressedValue{ .bits = QNAN | TAG_BOOL | 1 };
    pub const FALSE = CompressedValue{ .bits = QNAN | TAG_BOOL | 0 };
    pub const UNINITIALIZED = CompressedValue{ .bits = QNAN | TAG_UNINIT };
    pub const EXCEPTION = CompressedValue{ .bits = QNAN | TAG_EXCEPTION };

    pub inline fn isFloat(self: CompressedValue) bool {
        // If not a NaN, it's a regular float64
        return (self.bits & 0x7FF0000000000000) != 0x7FF0000000000000;
    }

    pub inline fn isInt(self: CompressedValue) bool {
        return (self.bits & TAG_MASK) == (QNAN | TAG_INT);
    }

    pub inline fn isUninitialized(self: CompressedValue) bool {
        return (self.bits & TAG_MASK) == (QNAN | TAG_UNINIT);
    }

    pub inline fn isUndefined(self: CompressedValue) bool {
        return self.bits == UNDEFINED.bits;
    }

    pub inline fn isNull(self: CompressedValue) bool {
        return self.bits == NULL.bits;
    }

    pub inline fn isException(self: CompressedValue) bool {
        return self.bits == EXCEPTION.bits;
    }

    pub inline fn getFloat(self: CompressedValue) f64 {
        return @bitCast(self.bits);
    }

    pub inline fn getInt(self: CompressedValue) i32 {
        return @truncate(@as(i64, @bitCast(self.bits & PAYLOAD_MASK)));
    }

    pub inline fn newFloat(val: f64) CompressedValue {
        return .{ .bits = @bitCast(val) };
    }

    pub inline fn newInt(val: i32) CompressedValue {
        const payload: u64 = @bitCast(@as(i64, val) & 0xFFFFFFFF);
        return .{ .bits = QNAN | TAG_INT | payload };
    }

    // Convert to/from full JSValue for FFI boundary
    pub inline fn toJSValue(self: CompressedValue) JSValue {
        @setEvalBranchQuota(100000);
        if (self.isFloat()) {
            return JSValue.newFloat64(self.getFloat());
        } else if (self.isInt()) {
            return JSValue.newInt(self.getInt());
        } else if (self.bits == UNDEFINED.bits) {
            return JSValue.UNDEFINED;
        } else if (self.bits == NULL.bits) {
            return JSValue.NULL;
        } else if (self.bits == TRUE.bits) {
            return JSValue.TRUE;
        } else if (self.bits == FALSE.bits) {
            return JSValue.FALSE;
        } else if (self.isRefType()) {
            // Reconstruct JSValue from compressed pointer with correct tag
            const ptr = self.decompressPtr();
            const tag: i64 = if (self.isPtr()) JS_TAG_OBJECT else if (self.isStr()) JS_TAG_STRING else if (self.isSymbol()) JS_TAG_SYMBOL else if (self.isBigInt()) JS_TAG_BIG_INT else if (self.isFunc()) JS_TAG_FUNCTION_BYTECODE else JS_TAG_OBJECT;
            // Platform-specific: WASM32 stores ptr in payload, native uses struct
            if (comptime is_wasm32) {
                const ptr_addr: u32 = @truncate(@intFromPtr(ptr));
                // Tag values fit in 32 bits, truncate i64 to i32 then bitcast to u32
                const tag32: i32 = @truncate(tag);
                return .{ .bits = (@as(u64, @as(u32, @bitCast(tag32))) << 32) | @as(u64, ptr_addr) };
            } else {
                return .{ .u = .{ .ptr = ptr }, .tag = tag };
            }
        }
        return JSValue.UNDEFINED;
    }

    pub inline fn fromJSValue(val: JSValue) CompressedValue {
        @setEvalBranchQuota(100000);
        if (val.isException()) {
            return EXCEPTION;
        } else if (val.isInt()) {
            return newInt(val.getInt());
        } else if (val.isFloat64()) {
            return newFloat(val.getFloat64());
        } else if (val.isUndefined()) {
            return UNDEFINED;
        } else if (val.isNull()) {
            return NULL;
        } else if (val.isBool()) {
            return if (val.getBool()) TRUE else FALSE;
        } else if (val.isString()) {
            // Compress string pointer with string tag
            return compressPtrWithTag(val.getPtr(), TAG_STR);
        } else if (val.isSymbol()) {
            // Compress symbol pointer with symbol tag
            return compressPtrWithTag(val.getPtr(), TAG_SYMBOL);
        } else if (val.isBigInt()) {
            // Compress BigInt pointer with BigInt tag
            return compressPtrWithTag(val.getPtr(), TAG_BIGINT);
        } else if (val.isFunctionBytecode()) {
            // Compress function bytecode pointer with func tag
            return compressPtrWithTag(val.getPtr(), TAG_FUNC);
        } else if (val.isObject()) {
            // Compress object pointer for storage on CV stack
            return compressPtr(val.getPtr(), 0);
        }
        return UNDEFINED;
    }

    // Compressed arithmetic - all inline, no function calls
    pub inline fn add(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            const ia: i64 = a.getInt();
            const ib: i64 = b.getInt();
            const r = ia + ib;
            if (r >= std.math.minInt(i32) and r <= std.math.maxInt(i32)) {
                return newInt(@intCast(r));
            }
            return newFloat(@floatFromInt(r));
        }
        // Float path
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return newFloat(fa + fb);
    }

    pub inline fn sub(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            const ia: i64 = a.getInt();
            const ib: i64 = b.getInt();
            const r = ia - ib;
            if (r >= std.math.minInt(i32) and r <= std.math.maxInt(i32)) {
                return newInt(@intCast(r));
            }
            return newFloat(@floatFromInt(r));
        }
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return newFloat(fa - fb);
    }

    pub inline fn mul(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            const ia: i64 = a.getInt();
            const ib: i64 = b.getInt();
            const r = ia * ib;
            if (r >= std.math.minInt(i32) and r <= std.math.maxInt(i32)) {
                return newInt(@intCast(r));
            }
            return newFloat(@floatFromInt(r));
        }
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return newFloat(fa * fb);
    }

    pub inline fn div(a: CompressedValue, b: CompressedValue) CompressedValue {
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return newFloat(fa / fb);
    }

    pub inline fn lt(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            return if (a.getInt() < b.getInt()) TRUE else FALSE;
        }
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return if (fa < fb) TRUE else FALSE;
    }

    pub inline fn gt(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            return if (a.getInt() > b.getInt()) TRUE else FALSE;
        }
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return if (fa > fb) TRUE else FALSE;
    }

    pub inline fn lte(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            return if (a.getInt() <= b.getInt()) TRUE else FALSE;
        }
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return if (fa <= fb) TRUE else FALSE;
    }

    pub inline fn gte(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            return if (a.getInt() >= b.getInt()) TRUE else FALSE;
        }
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return if (fa >= fb) TRUE else FALSE;
    }

    pub inline fn toInt32(self: CompressedValue) i32 {
        if (self.isInt()) {
            return self.getInt();
        }
        if (self.isFloat()) {
            return @intFromFloat(self.getFloat());
        }
        return 0;
    }

    pub inline fn strictEq(a: CompressedValue, b: CompressedValue) CompressedValue {
        // Fast path: same bits means same value
        if (a.bits == b.bits) return TRUE;
        // Type mismatch means not equal (strict equality)
        if (a.isInt() and b.isInt()) {
            return if (a.getInt() == b.getInt()) TRUE else FALSE;
        }
        if (a.isFloat() and b.isFloat()) {
            return if (a.getFloat() == b.getFloat()) TRUE else FALSE;
        }
        // Different types are never strictly equal
        return FALSE;
    }

    pub inline fn strictNeq(a: CompressedValue, b: CompressedValue) CompressedValue {
        return if (strictEq(a, b).bits == TRUE.bits) FALSE else TRUE;
    }

    // Bitwise operations
    pub inline fn band(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ia = a.toInt32();
        const ib = b.toInt32();
        return newInt(ia & ib);
    }

    pub inline fn bor(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ia = a.toInt32();
        const ib = b.toInt32();
        return newInt(ia | ib);
    }

    pub inline fn bxor(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ia = a.toInt32();
        const ib = b.toInt32();
        return newInt(ia ^ ib);
    }

    pub inline fn bnot(a: CompressedValue) CompressedValue {
        return newInt(~a.toInt32());
    }

    pub inline fn toBool(self: CompressedValue) bool {
        if (self.bits == FALSE.bits or self.bits == NULL.bits or self.bits == UNDEFINED.bits) {
            return false;
        }
        if (self.isInt()) {
            return self.getInt() != 0;
        }
        if (self.isFloat()) {
            const f = self.getFloat();
            return f != 0.0 and !std.math.isNan(f);
        }
        return true;
    }

    /// Convert value to number (returns CV with numeric value)
    /// For primitives, converts inline. For objects/strings, returns NaN or requires JSContext.
    pub inline fn toNumber(self: CompressedValue) CompressedValue {
        // Already a number
        if (self.isInt() or self.isFloat()) return self;
        // Null -> 0
        if (self.bits == NULL.bits) return newInt(0);
        // Undefined -> NaN
        if (self.bits == UNDEFINED.bits) return newFloat(std.math.nan(f64));
        // Boolean -> 0 or 1
        if (self.bits == TRUE.bits) return newInt(1);
        if (self.bits == FALSE.bits) return newInt(0);
        // For objects/strings, return NaN (proper conversion would need JSContext)
        return newFloat(std.math.nan(f64));
    }

    // Compressed pointer support - 32-bit offset from heap base
    pub inline fn isPtr(self: CompressedValue) bool {
        return (self.bits & TAG_MASK) == (QNAN | TAG_PTR);
    }

    pub inline fn isStr(self: CompressedValue) bool {
        return (self.bits & TAG_MASK) == (QNAN | TAG_STR);
    }

    pub inline fn isSymbol(self: CompressedValue) bool {
        return (self.bits & TAG_MASK) == (QNAN | TAG_SYMBOL);
    }

    pub inline fn isBigInt(self: CompressedValue) bool {
        return (self.bits & TAG_MASK) == (QNAN | TAG_BIGINT);
    }

    pub inline fn isFunc(self: CompressedValue) bool {
        return (self.bits & TAG_MASK) == (QNAN | TAG_FUNC);
    }

    // Check if this is any reference type (has a pointer)
    pub inline fn isRefType(self: CompressedValue) bool {
        const tag = self.bits & TAG_MASK;
        return tag == (QNAN | TAG_PTR) or tag == (QNAN | TAG_STR) or
            tag == (QNAN | TAG_SYMBOL) or tag == (QNAN | TAG_BIGINT) or
            tag == (QNAN | TAG_FUNC);
    }

    pub inline fn getPtr32(self: CompressedValue) u32 {
        return @truncate(self.bits & 0xFFFFFFFF);
    }

    pub inline fn newPtr32(offset: u32) CompressedValue {
        return .{ .bits = QNAN | TAG_PTR | offset };
    }

    // Convert 64-bit pointer to 48-bit compressed with type tag
    pub inline fn compressPtrWithTag(ptr: ?*anyopaque, tag: u64) CompressedValue {
        if (ptr == null) return NULL;
        const addr = @intFromPtr(ptr);
        return .{ .bits = QNAN | tag | (addr & PAYLOAD_MASK) };
    }

    // Convert 64-bit pointer to 48-bit compressed (no heap base needed)
    // ARM64 macOS uses 47-bit canonical addresses, so 48 bits is sufficient
    pub inline fn compressPtr(ptr: ?*anyopaque, _: usize) CompressedValue {
        if (ptr == null) return NULL;
        const addr = @intFromPtr(ptr);
        // Store lower 48 bits of pointer directly
        return .{ .bits = QNAN | TAG_PTR | (addr & PAYLOAD_MASK) };
    }

    // Convert 48-bit compressed back to pointer
    // On WASM32: just truncate to 32 bits
    // On native 64-bit: sign-extend from bit 47 for canonical address reconstruction
    pub inline fn decompressPtr(self: CompressedValue) ?*anyopaque {
        if (!self.isRefType()) return null;
        const low48 = self.bits & PAYLOAD_MASK;
        // WASM32: just use lower 32 bits directly
        if (@sizeOf(usize) == 4) {
            const addr: u32 = @truncate(low48);
            return @ptrFromInt(addr);
        }
        // Native 64-bit: sign-extend from bit 47 (canonical address format)
        const addr: u64 = if ((low48 & 0x800000000000) != 0)
            low48 | 0xFFFF000000000000 // Set upper 16 bits for kernel/high addresses
        else
            low48; // User-space address, upper bits already 0
        return @ptrFromInt(addr);
    }

    // Equality comparison (JavaScript loose equality ==)
    // Implements key parts of Abstract Equality Comparison:
    // - Same type: compare values
    // - null == undefined and undefined == null: true
    // - Number coercion for numeric comparisons
    pub inline fn eq(a: CompressedValue, b: CompressedValue) CompressedValue {
        // Same bits = same value
        if (a.bits == b.bits) return TRUE;
        // JavaScript: null == undefined and undefined == null
        const a_is_nullish = (a.bits == NULL.bits or a.bits == UNDEFINED.bits);
        const b_is_nullish = (b.bits == NULL.bits or b.bits == UNDEFINED.bits);
        if (a_is_nullish and b_is_nullish) return TRUE;
        // Compare numeric values
        if ((a.isInt() or a.isFloat()) and (b.isInt() or b.isFloat())) {
            const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
            const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
            return if (fa == fb) TRUE else FALSE;
        }
        return FALSE;
    }

    // Bitwise operations - all inline, 32-bit int path
    // JS semantics: undefined/null/NaN become 0 in bitwise ops
    fn toInt32ForBitwise(v: CompressedValue) i32 {
        if (v.isInt()) return v.getInt();
        if (v.isFloat()) {
            const f = v.getFloat();
            // NaN, Inf, etc. become 0 in JS bitwise ops
            if (std.math.isNan(f) or std.math.isInf(f)) return 0;
            // Truncate to i32 range per JS ToInt32
            const truncated = @trunc(f);
            if (truncated < -2147483648.0 or truncated > 2147483647.0) return 0;
            return @intFromFloat(truncated);
        }
        // undefined, null, bool false, etc. become 0
        if (v.bits == TRUE.bits) return 1;
        return 0;
    }

    pub inline fn bitAnd(a: CompressedValue, b: CompressedValue) CompressedValue {
        return newInt(toInt32ForBitwise(a) & toInt32ForBitwise(b));
    }

    pub inline fn bitOr(a: CompressedValue, b: CompressedValue) CompressedValue {
        return newInt(toInt32ForBitwise(a) | toInt32ForBitwise(b));
    }

    pub inline fn bitXor(a: CompressedValue, b: CompressedValue) CompressedValue {
        return newInt(toInt32ForBitwise(a) ^ toInt32ForBitwise(b));
    }

    pub inline fn bitNot(a: CompressedValue) CompressedValue {
        return newInt(~toInt32ForBitwise(a));
    }

    pub inline fn shl(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ia: i32 = toInt32ForBitwise(a);
        const ib: u5 = @truncate(@as(u32, @bitCast(toInt32ForBitwise(b))) & 0x1f);
        return newInt(ia << ib);
    }

    pub inline fn sar(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ia: i32 = toInt32ForBitwise(a);
        const ib: u5 = @truncate(@as(u32, @bitCast(toInt32ForBitwise(b))) & 0x1f);
        return newInt(ia >> ib);
    }

    pub inline fn shr(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ua: u32 = @bitCast(toInt32ForBitwise(a));
        const ib: u5 = @truncate(@as(u32, @bitCast(toInt32ForBitwise(b))) & 0x1f);
        return newInt(@bitCast(ua >> ib));
    }

    // Modulo for completeness
    pub inline fn mod(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            const ia = a.getInt();
            const ib = b.getInt();
            if (ib == 0) return newFloat(std.math.nan(f64));
            return newInt(@rem(ia, ib));
        }
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return newFloat(@mod(fa, fb));
    }
};

// Global heap base for pointer compression (set during init)
pub var compressed_heap_base: usize = 0;

pub fn initCompressedHeap(base: usize) void {
    compressed_heap_base = base;
}

// ============================================================================
// JSValue - Platform-dependent representation
// WASM32: 8-byte NaN-boxed u64 (tag in upper 32 bits, payload in lower 32 bits)
// Native: 16-byte struct (8-byte union + 8-byte tag)
// ============================================================================

pub const JSValue = if (is_wasm32) JSValueWasm32 else JSValueNative;

// WASM32 NaN-boxing: JS_MKVAL(tag, val) = ((uint64_t)(tag) << 32) | (uint32_t)(val)
const JSValueWasm32 = extern struct {
    bits: u64,

    // Constants
    pub const UNDEFINED: JSValueWasm32 = .{ .bits = (@as(u64, @intCast(@as(u32, @bitCast(@as(i32, JS_TAG_UNDEFINED))))) << 32) | 0 };
    pub const NULL: JSValueWasm32 = .{ .bits = (@as(u64, @intCast(@as(u32, @bitCast(@as(i32, JS_TAG_NULL))))) << 32) | 0 };
    pub const TRUE: JSValueWasm32 = .{ .bits = (@as(u64, @intCast(@as(u32, @bitCast(@as(i32, JS_TAG_BOOL))))) << 32) | 1 };
    pub const FALSE: JSValueWasm32 = .{ .bits = (@as(u64, @intCast(@as(u32, @bitCast(@as(i32, JS_TAG_BOOL))))) << 32) | 0 };
    pub const EXCEPTION: JSValueWasm32 = .{ .bits = (@as(u64, @intCast(@as(u32, @bitCast(@as(i32, JS_TAG_EXCEPTION))))) << 32) | 0 };
    pub const UNINITIALIZED: JSValueWasm32 = .{ .bits = (@as(u64, @intCast(@as(u32, @bitCast(@as(i32, JS_TAG_UNINITIALIZED))))) << 32) | 0 };

    inline fn getTag(self: JSValueWasm32) i32 {
        return @bitCast(@as(u32, @truncate(self.bits >> 32)));
    }

    inline fn getPayload(self: JSValueWasm32) u32 {
        return @truncate(self.bits);
    }

    pub inline fn isInt(self: JSValueWasm32) bool {
        return self.getTag() == JS_TAG_INT;
    }

    pub inline fn isBool(self: JSValueWasm32) bool {
        return self.getTag() == JS_TAG_BOOL;
    }

    pub inline fn isNull(self: JSValueWasm32) bool {
        return self.getTag() == JS_TAG_NULL;
    }

    pub inline fn isUndefined(self: JSValueWasm32) bool {
        return self.getTag() == JS_TAG_UNDEFINED;
    }

    pub inline fn isUninitialized(self: JSValueWasm32) bool {
        return self.getTag() == JS_TAG_UNINITIALIZED;
    }

    pub inline fn isException(self: JSValueWasm32) bool {
        return self.getTag() == JS_TAG_EXCEPTION;
    }

    pub inline fn isFunction(self: JSValueWasm32) bool {
        const tag = self.getTag();
        return tag == JS_TAG_OBJECT or tag == JS_TAG_FUNCTION_BYTECODE;
    }

    pub inline fn isString(self: JSValueWasm32) bool {
        return self.getTag() == JS_TAG_STRING;
    }

    pub inline fn isObject(self: JSValueWasm32) bool {
        return self.getTag() == JS_TAG_OBJECT;
    }

    pub inline fn isSymbol(self: JSValueWasm32) bool {
        return self.getTag() == JS_TAG_SYMBOL;
    }

    pub inline fn isBigInt(self: JSValueWasm32) bool {
        return self.getTag() == JS_TAG_BIG_INT;
    }

    pub inline fn isFunctionBytecode(self: JSValueWasm32) bool {
        return self.getTag() == JS_TAG_FUNCTION_BYTECODE;
    }

    pub inline fn isNumber(self: JSValueWasm32) bool {
        const tag = self.getTag();
        return tag == JS_TAG_INT or tag == JS_TAG_FLOAT64;
    }

    pub inline fn isFloat64(self: JSValueWasm32) bool {
        return self.getTag() == JS_TAG_FLOAT64;
    }

    pub inline fn hasRefCount(self: JSValueWasm32) bool {
        return self.getTag() < 0;
    }

    pub inline fn getInt(self: JSValueWasm32) i32 {
        return @bitCast(self.getPayload());
    }

    pub inline fn getBool(self: JSValueWasm32) bool {
        return self.getPayload() != 0;
    }

    pub inline fn getFloat64(self: JSValueWasm32) f64 {
        return @bitCast(self.bits);
    }

    pub inline fn getPtr(self: JSValueWasm32) ?*anyopaque {
        return @ptrFromInt(self.getPayload());
    }

    pub inline fn getNumberAsFloat(self: JSValueWasm32) f64 {
        if (self.getTag() == JS_TAG_INT) {
            return @floatFromInt(self.getInt());
        }
        return self.getFloat64();
    }

    pub inline fn newInt(val: i32) JSValueWasm32 {
        return .{ .bits = (@as(u64, @intCast(@as(u32, @bitCast(@as(i32, JS_TAG_INT))))) << 32) | @as(u64, @intCast(@as(u32, @bitCast(val)))) };
    }

    pub inline fn newBool(val: bool) JSValueWasm32 {
        return .{ .bits = (@as(u64, @intCast(@as(u32, @bitCast(@as(i32, JS_TAG_BOOL))))) << 32) | (if (val) @as(u64, 1) else @as(u64, 0)) };
    }

    pub inline fn newFloat64(val: f64) JSValueWasm32 {
        return .{ .bits = @bitCast(val) };
    }

    pub inline fn newInt64(ctx: *JSContext, val: i64) JSValueWasm32 {
        _ = ctx;
        if (val >= std.math.minInt(i32) and val <= std.math.maxInt(i32)) {
            return newInt(@intCast(val));
        }
        return newFloat64(@floatFromInt(val));
    }

    pub inline fn dup(ctx: *JSContext, val: JSValueWasm32) JSValueWasm32 {
        if (val.hasRefCount()) {
            return quickjs.JS_DupValue(ctx, val);
        }
        return val;
    }

    pub inline fn free(ctx: *JSContext, val: JSValueWasm32) void {
        if (val.hasRefCount()) {
            quickjs.JS_FreeValue(ctx, val);
        }
    }

    // FFI wrappers
    pub inline fn call(ctx: *JSContext, func: JSValueWasm32, this: JSValueWasm32, argc: c_int, argv: [*]JSValueWasm32) JSValueWasm32 {
        return quickjs.JS_Call(ctx, func, this, argc, argv);
    }

    pub inline fn setPropertyStr(ctx: *JSContext, this: JSValueWasm32, name: [*:0]const u8, val: JSValueWasm32) c_int {
        return quickjs.JS_SetPropertyStr(ctx, this, name, val);
    }

    pub inline fn getPropertyStr(ctx: *JSContext, this: JSValueWasm32, name: [*:0]const u8) JSValueWasm32 {
        return quickjs.JS_GetPropertyStr(ctx, this, name);
    }

    /// Get field by name (convenience wrapper for getPropertyStr)
    pub inline fn getField(ctx: *JSContext, this: JSValueWasm32, name: [*:0]const u8) JSValueWasm32 {
        return quickjs.JS_GetPropertyStr(ctx, this, name);
    }

    pub inline fn throwTypeError(ctx: *JSContext, msg: [*:0]const u8) JSValueWasm32 {
        return quickjs.JS_ThrowTypeError(ctx, msg);
    }

    pub inline fn throwRangeError(ctx: *JSContext, msg: [*:0]const u8) JSValueWasm32 {
        return quickjs.JS_ThrowRangeError(ctx, msg);
    }

    pub inline fn throwReferenceError(ctx: *JSContext, msg: [*:0]const u8) JSValueWasm32 {
        return quickjs.JS_ThrowReferenceError(ctx, msg);
    }

    /// Strict equality check (===)
    /// Uses QuickJS JS_IsStrictEqual for correct string comparison by value
    pub fn strictEq(ctx: *JSContext, a: JSValueWasm32, b: JSValueWasm32) bool {
        return quickjs.JS_IsStrictEqual(ctx, a, b);
    }

    /// Get typeof result as JSValue string
    pub fn typeOf(ctx: *JSContext, val: JSValueWasm32) JSValueWasm32 {
        const type_str = switch (val.getTag()) {
            JS_TAG_UNDEFINED => "undefined",
            JS_TAG_NULL => "object", // typeof null === "object" (quirk)
            JS_TAG_BOOL => "boolean",
            JS_TAG_INT, JS_TAG_FLOAT64, JS_TAG_SHORT_BIG_INT => "number",
            JS_TAG_STRING => "string",
            JS_TAG_SYMBOL => "symbol",
            JS_TAG_BIG_INT => "bigint",
            JS_TAG_OBJECT, JS_TAG_FUNCTION_BYTECODE => blk: {
                if (quickjs.JS_IsFunction(ctx, val) != 0) {
                    break :blk "function";
                }
                break :blk "object";
            },
            else => "object",
        };
        return quickjs.JS_NewString(ctx, type_str);
    }

    pub fn newObject(ctx: *JSContext) JSValueWasm32 {
        return quickjs.JS_NewObject(ctx);
    }

    pub fn newArray(ctx: *JSContext) JSValueWasm32 {
        return quickjs.JS_NewArray(ctx);
    }

    pub fn newString(ctx: *JSContext, str: [*:0]const u8) JSValueWasm32 {
        return quickjs.JS_NewString(ctx, str);
    }

    pub fn newStringLen(ctx: *JSContext, str: [*]const u8, len: usize) JSValueWasm32 {
        return quickjs.JS_NewStringLen(ctx, str, len);
    }

    pub fn getPropertyUint32(ctx: *JSContext, obj: JSValueWasm32, idx: u32) JSValueWasm32 {
        return quickjs.JS_GetPropertyUint32(ctx, obj, idx);
    }

    pub fn setPropertyUint32(ctx: *JSContext, obj: JSValueWasm32, idx: u32, val: JSValueWasm32) c_int {
        return quickjs.JS_SetPropertyUint32(ctx, obj, idx, val);
    }

    pub fn toInt32(ctx: *JSContext, pres: *i32, val: JSValueWasm32) c_int {
        return quickjs.JS_ToInt32(ctx, pres, val);
    }

    pub fn toFloat64(ctx: *JSContext, pres: *f64, val: JSValueWasm32) c_int {
        return quickjs.JS_ToFloat64(ctx, pres, val);
    }

    pub fn toBool(ctx: *JSContext, val: JSValueWasm32) c_int {
        return quickjs.JS_ToBool(ctx, val);
    }

    pub fn definePropertyStr(ctx: *JSContext, obj: JSValueWasm32, name: [*:0]const u8, val: JSValueWasm32) c_int {
        return quickjs.JS_DefinePropertyValueStr(ctx, obj, name, val, quickjs.JS_PROP_C_W_E);
    }

    pub fn callConstructor(ctx: *JSContext, func: JSValueWasm32, args: []const JSValueWasm32) JSValueWasm32 {
        return quickjs.JS_CallConstructor(ctx, func, @intCast(args.len), @ptrCast(args.ptr));
    }

    pub fn throw(ctx: *JSContext, val: JSValueWasm32) JSValueWasm32 {
        return quickjs.JS_Throw(ctx, val);
    }

    pub fn isInstanceOf(ctx: *JSContext, obj: JSValueWasm32, ctor: JSValueWasm32) bool {
        return quickjs.JS_IsInstanceOf(ctx, obj, ctor) != 0;
    }

    pub fn getGlobal(ctx: *JSContext, name: [*:0]const u8) JSValueWasm32 {
        const global = quickjs.JS_GetGlobalObject(ctx);
        defer free(ctx, global);
        return quickjs.JS_GetPropertyStr(ctx, global, name);
    }

    pub fn getGlobalUndef(ctx: *JSContext, name: [*:0]const u8) JSValueWasm32 {
        const global = quickjs.JS_GetGlobalObject(ctx);
        defer free(ctx, global);
        const result = quickjs.JS_GetPropertyStr(ctx, global, name);
        if (result.isException()) {
            return UNDEFINED;
        }
        return result;
    }

    pub fn deleteProperty(ctx: *JSContext, obj: JSValueWasm32, prop: JSValueWasm32) c_int {
        return quickjs.JS_DeleteProperty(ctx, obj, prop, 0);
    }

    pub fn appendArray(ctx: *JSContext, arr: JSValueWasm32, val: JSValueWasm32) c_int {
        const len_val = quickjs.JS_GetPropertyStr(ctx, arr, "length");
        var len: i32 = 0;
        _ = toInt32(ctx, &len, len_val);
        free(ctx, len_val);
        return quickjs.JS_SetPropertyUint32(ctx, arr, @intCast(len), val);
    }
};

// Native 64-bit: 16-byte struct (matches QuickJS non-NaN-boxing layout)
const JSValueNative = extern struct {
    u: JSValueUnion,
    tag: i64,

    pub const UNDEFINED = JSValueNative{ .u = .{ .int32 = 0 }, .tag = JS_TAG_UNDEFINED };
    pub const NULL = JSValueNative{ .u = .{ .int32 = 0 }, .tag = JS_TAG_NULL };
    pub const TRUE = JSValueNative{ .u = .{ .int32 = 1 }, .tag = JS_TAG_BOOL };
    pub const FALSE = JSValueNative{ .u = .{ .int32 = 0 }, .tag = JS_TAG_BOOL };
    pub const EXCEPTION = JSValueNative{ .u = .{ .int32 = 0 }, .tag = JS_TAG_EXCEPTION };
    pub const UNINITIALIZED = JSValueNative{ .u = .{ .int32 = 0 }, .tag = JS_TAG_UNINITIALIZED };

    pub inline fn isInt(self: JSValueNative) bool {
        return self.tag == JS_TAG_INT;
    }

    pub inline fn isBool(self: JSValueNative) bool {
        return self.tag == JS_TAG_BOOL;
    }

    pub inline fn isNull(self: JSValueNative) bool {
        return self.tag == JS_TAG_NULL;
    }

    pub inline fn isUndefined(self: JSValueNative) bool {
        return self.tag == JS_TAG_UNDEFINED;
    }

    pub inline fn isUninitialized(self: JSValueNative) bool {
        return self.tag == JS_TAG_UNINITIALIZED;
    }

    pub inline fn isException(self: JSValueNative) bool {
        return self.tag == JS_TAG_EXCEPTION;
    }

    pub inline fn isFunction(self: JSValueNative) bool {
        return self.tag == JS_TAG_OBJECT or self.tag == JS_TAG_FUNCTION_BYTECODE;
    }

    pub inline fn isString(self: JSValueNative) bool {
        return self.tag == JS_TAG_STRING;
    }

    pub inline fn isObject(self: JSValueNative) bool {
        return self.tag == JS_TAG_OBJECT;
    }

    pub inline fn isSymbol(self: JSValueNative) bool {
        return self.tag == JS_TAG_SYMBOL;
    }

    pub inline fn isBigInt(self: JSValueNative) bool {
        return self.tag == JS_TAG_BIG_INT;
    }

    pub inline fn isFunctionBytecode(self: JSValueNative) bool {
        return self.tag == JS_TAG_FUNCTION_BYTECODE;
    }

    pub inline fn isNumber(self: JSValueNative) bool {
        return self.tag == JS_TAG_INT or self.tag == JS_TAG_FLOAT64;
    }

    pub inline fn isFloat64(self: JSValueNative) bool {
        return self.tag == JS_TAG_FLOAT64;
    }

    pub inline fn hasRefCount(self: JSValueNative) bool {
        return self.tag < 0;
    }

    pub inline fn getInt(self: JSValueNative) i32 {
        return self.u.int32;
    }

    pub inline fn getBool(self: JSValueNative) bool {
        return self.u.int32 != 0;
    }

    pub inline fn getFloat64(self: JSValueNative) f64 {
        return self.u.float64;
    }

    pub inline fn getPtr(self: JSValueNative) ?*anyopaque {
        return self.u.ptr;
    }

    pub inline fn getNumberAsFloat(self: JSValueNative) f64 {
        if (self.tag == JS_TAG_INT) {
            return @floatFromInt(self.u.int32);
        }
        return self.u.float64;
    }

    pub inline fn newInt(val: i32) JSValueNative {
        return .{ .u = .{ .int32 = val }, .tag = JS_TAG_INT };
    }

    pub inline fn newBool(val: bool) JSValueNative {
        return .{ .u = .{ .int32 = if (val) 1 else 0 }, .tag = JS_TAG_BOOL };
    }

    pub inline fn newFloat64(val: f64) JSValueNative {
        return .{ .u = .{ .float64 = val }, .tag = JS_TAG_FLOAT64 };
    }

    pub inline fn newInt64(ctx: *JSContext, val: i64) JSValueNative {
        _ = ctx;
        if (val >= std.math.minInt(i32) and val <= std.math.maxInt(i32)) {
            return newInt(@intCast(val));
        }
        return newFloat64(@floatFromInt(val));
    }

    pub inline fn dup(ctx: *JSContext, val: JSValueNative) JSValueNative {
        if (val.hasRefCount()) {
            return quickjs.JS_DupValue(ctx, val);
        }
        return val;
    }

    pub inline fn free(ctx: *JSContext, val: JSValueNative) void {
        if (val.hasRefCount()) {
            quickjs.JS_FreeValue(ctx, val);
        }
    }

    // FFI wrappers
    pub inline fn call(ctx: *JSContext, func: JSValueNative, this: JSValueNative, argc: c_int, argv: [*]JSValueNative) JSValueNative {
        return quickjs.JS_Call(ctx, func, this, argc, argv);
    }

    pub inline fn setPropertyStr(ctx: *JSContext, this: JSValueNative, name: [*:0]const u8, val: JSValueNative) c_int {
        return quickjs.JS_SetPropertyStr(ctx, this, name, val);
    }

    pub inline fn getPropertyStr(ctx: *JSContext, this: JSValueNative, name: [*:0]const u8) JSValueNative {
        return quickjs.JS_GetPropertyStr(ctx, this, name);
    }

    /// Get field by name (convenience wrapper for getPropertyStr)
    pub inline fn getField(ctx: *JSContext, this: JSValueNative, name: [*:0]const u8) JSValueNative {
        return quickjs.JS_GetPropertyStr(ctx, this, name);
    }

    /// Get property by integer index
    pub inline fn getIndex(ctx: *JSContext, this: JSValueNative, idx: u32) JSValueNative {
        return quickjs.JS_GetPropertyUint32(ctx, this, idx);
    }

    /// Convert to boolean (FFI function form - ctx, val -> c_int)
    pub inline fn toBool(ctx: *JSContext, val: JSValueNative) c_int {
        return quickjs.JS_ToBool(ctx, val);
    }

    pub inline fn throwTypeError(ctx: *JSContext, msg: [*:0]const u8) JSValueNative {
        return quickjs.JS_ThrowTypeError(ctx, msg);
    }

    pub inline fn throwRangeError(ctx: *JSContext, msg: [*:0]const u8) JSValueNative {
        return quickjs.JS_ThrowRangeError(ctx, msg);
    }

    pub inline fn throwReferenceError(ctx: *JSContext, msg: [*:0]const u8) JSValueNative {
        return quickjs.JS_ThrowReferenceError(ctx, msg);
    }

    /// Strict equality check (===)
    /// Uses QuickJS JS_IsStrictEqual for correct string comparison by value
    pub fn strictEq(ctx: *JSContext, a: JSValueNative, b: JSValueNative) bool {
        return quickjs.JS_IsStrictEqual(ctx, a, b);
    }

    /// Get typeof result as JSValue string
    pub fn typeOf(ctx: *JSContext, val: JSValueNative) JSValueNative {
        const type_str = switch (val.tag) {
            JS_TAG_UNDEFINED => "undefined",
            JS_TAG_NULL => "object", // typeof null === "object" (quirk)
            JS_TAG_BOOL => "boolean",
            JS_TAG_INT, JS_TAG_FLOAT64, JS_TAG_SHORT_BIG_INT => "number",
            JS_TAG_STRING => "string",
            JS_TAG_SYMBOL => "symbol",
            JS_TAG_BIG_INT => "bigint",
            JS_TAG_OBJECT, JS_TAG_FUNCTION_BYTECODE => blk: {
                if (quickjs.JS_IsFunction(ctx, val) != 0) {
                    break :blk "function";
                }
                break :blk "object";
            },
            else => "object",
        };
        return quickjs.JS_NewString(ctx, type_str);
    }

    pub fn newObject(ctx: *JSContext) JSValueNative {
        return quickjs.JS_NewObject(ctx);
    }

    pub fn newArray(ctx: *JSContext) JSValueNative {
        return quickjs.JS_NewArray(ctx);
    }

    pub fn newString(ctx: *JSContext, str: [*:0]const u8) JSValueNative {
        return quickjs.JS_NewString(ctx, str);
    }

    pub fn newStringLen(ctx: *JSContext, str: [*]const u8, len: usize) JSValueNative {
        return quickjs.JS_NewStringLen(ctx, str, len);
    }

    pub fn getPropertyUint32(ctx: *JSContext, obj: JSValueNative, idx: u32) JSValueNative {
        return quickjs.JS_GetPropertyUint32(ctx, obj, idx);
    }

    pub fn setPropertyUint32(ctx: *JSContext, obj: JSValueNative, idx: u32, val: JSValueNative) c_int {
        return quickjs.JS_SetPropertyUint32(ctx, obj, idx, val);
    }

    pub fn toInt32(ctx: *JSContext, pres: *i32, val: JSValueNative) c_int {
        return quickjs.JS_ToInt32(ctx, pres, val);
    }

    pub fn toFloat64(ctx: *JSContext, pres: *f64, val: JSValueNative) c_int {
        return quickjs.JS_ToFloat64(ctx, pres, val);
    }

    pub fn jsToBool(ctx: *JSContext, val: JSValueNative) c_int {
        return quickjs.JS_ToBool(ctx, val);
    }

    pub fn definePropertyStr(ctx: *JSContext, obj: JSValueNative, name: [*:0]const u8, val: JSValueNative) c_int {
        return quickjs.JS_DefinePropertyValueStr(ctx, obj, name, val, quickjs.JS_PROP_C_W_E);
    }

    pub fn callConstructor(ctx: *JSContext, func: JSValueNative, args: []const JSValueNative) JSValueNative {
        return quickjs.JS_CallConstructor(ctx, func, @intCast(args.len), @ptrCast(args.ptr));
    }

    pub fn throw(ctx: *JSContext, val: JSValueNative) JSValueNative {
        return quickjs.JS_Throw(ctx, val);
    }

    pub fn isInstanceOf(ctx: *JSContext, obj: JSValueNative, ctor: JSValueNative) bool {
        return quickjs.JS_IsInstanceOf(ctx, obj, ctor) != 0;
    }

    pub fn getGlobal(ctx: *JSContext, name: [*:0]const u8) JSValueNative {
        const global = quickjs.JS_GetGlobalObject(ctx);
        defer free(ctx, global);
        return quickjs.JS_GetPropertyStr(ctx, global, name);
    }

    pub fn getGlobalUndef(ctx: *JSContext, name: [*:0]const u8) JSValueNative {
        const global = quickjs.JS_GetGlobalObject(ctx);
        defer free(ctx, global);
        const result = quickjs.JS_GetPropertyStr(ctx, global, name);
        if (result.isException()) {
            return UNDEFINED;
        }
        return result;
    }

    pub fn deleteProperty(ctx: *JSContext, obj: JSValueNative, prop: JSValueNative) c_int {
        return quickjs.JS_DeleteProperty(ctx, obj, prop, 0);
    }

    pub fn appendArray(ctx: *JSContext, arr: JSValueNative, val: JSValueNative) c_int {
        const len_val = quickjs.JS_GetPropertyStr(ctx, arr, "length");
        var len: i32 = 0;
        _ = toInt32(ctx, &len, len_val);
        free(ctx, len_val);
        return quickjs.JS_SetPropertyUint32(ctx, arr, @intCast(len), val);
    }

    // ============================================================================
    // Iterator methods (for for-of loops)
    // ============================================================================

    /// Get iterator from object
    pub fn getIterator(ctx: *JSContext, obj: JSValueNative, is_async: c_int) JSValueNative {
        return quickjs.js_frozen_get_iterator(ctx, obj, is_async);
    }

    /// Get next value from iterator, sets done flag
    pub fn iteratorNext(ctx: *JSContext, iter: JSValueNative, done: *c_int) JSValueNative {
        return quickjs.js_frozen_iterator_next(ctx, iter, done);
    }

    /// Close iterator (call return method if exists)
    pub fn iteratorClose(ctx: *JSContext, iter: JSValueNative, completion_type: c_int) c_int {
        return quickjs.js_frozen_iterator_close(ctx, iter, completion_type);
    }

    /// Get value and done from iterator result object
    pub fn iteratorGetValueDone(ctx: *JSContext, result: JSValueNative, done: *c_int) JSValueNative {
        return quickjs.js_frozen_iterator_get_value_done(ctx, result, done);
    }

    // ============================================================================
    // Object operations
    // ============================================================================

    /// Set field by name (like setPropertyStr but matches codegen pattern)
    pub fn setField(ctx: *JSContext, obj: JSValueNative, prop: [*:0]const u8, val: JSValueNative) c_int {
        return quickjs.js_frozen_set_field(ctx, obj, prop, val);
    }

    /// Set array element by index (alias for setPropertyUint32)
    pub fn setIndex(ctx: *JSContext, obj: JSValueNative, idx: u32, val: JSValueNative) c_int {
        return quickjs.JS_SetPropertyUint32(ctx, obj, idx, val);
    }

    /// Convert value to object
    pub fn toObject(ctx: *JSContext, val: JSValueNative) JSValueNative {
        return quickjs.js_frozen_to_object(ctx, val);
    }

    /// Get array/object length as JSValue
    pub fn getLengthVal(ctx: *JSContext, obj: JSValueNative) JSValueNative {
        return quickjs.js_frozen_get_length(ctx, obj);
    }

    /// Get array/object length into pointer (codegen pattern: getLength(ctx, &len, arr))
    pub fn getLength(ctx: *JSContext, plen: *i64, obj: JSValueNative) c_int {
        return quickjs.JS_GetLength(ctx, obj, plen);
    }

    /// Convert value to property key (string or symbol)
    pub fn toPropKey(ctx: *JSContext, val: JSValueNative) JSValueNative {
        return quickjs.js_frozen_to_prop_key(ctx, val);
    }

    /// instanceof operator (alias for isInstanceOf matching codegen pattern)
    pub fn instanceof(ctx: *JSContext, obj: JSValueNative, ctor: JSValueNative) bool {
        return quickjs.JS_IsInstanceOf(ctx, obj, ctor) != 0;
    }

    /// typeof operation returning JSValue string
    pub fn typeofValue(ctx: *JSContext, val: JSValueNative) JSValueNative {
        return typeOf(ctx, val);
    }

    /// Define property on Uint32 index
    pub fn definePropertyUint32(ctx: *JSContext, obj: JSValueNative, idx: u32, val: JSValueNative) c_int {
        return quickjs.JS_SetPropertyUint32(ctx, obj, idx, val);
    }
};

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
pub inline fn getVarRef(var_ref: *JSVarRef) JSValue {
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

// ============================================================================
// QuickJS FFI Bindings
// ============================================================================

pub const quickjs = struct {
    // Reference counting
    pub extern fn JS_DupValue(ctx: *JSContext, val: JSValue) JSValue;
    pub extern fn JS_FreeValue(ctx: *JSContext, val: JSValue) void;

    // Closure variable access (C implementation for correct struct layout)
    pub extern fn js_frozen_get_var_ref(ctx: *JSContext, var_refs: ?*anyopaque, idx: c_int) JSValue;
    pub extern fn js_frozen_set_var_ref(ctx: *JSContext, var_refs: ?*anyopaque, idx: c_int, val: JSValue) void;

    // Function calls
    pub extern fn JS_Call(ctx: *JSContext, func: JSValue, this: JSValue, argc: c_int, argv: [*]const JSValue) JSValue;

    // Atom management
    pub extern fn JS_NewAtom(ctx: *JSContext, str: [*:0]const u8) u32;
    pub extern fn JS_FreeAtom(ctx: *JSContext, atom: u32) void;

    // Property access
    pub extern fn JS_GetPropertyStr(ctx: *JSContext, obj: JSValue, prop: [*:0]const u8) JSValue;
    pub extern fn JS_SetPropertyStr(ctx: *JSContext, obj: JSValue, prop: [*:0]const u8, val: JSValue) c_int;
    pub extern fn JS_GetPropertyUint32(ctx: *JSContext, obj: JSValue, idx: u32) JSValue;
    pub extern fn JS_SetPropertyUint32(ctx: *JSContext, obj: JSValue, idx: u32, val: JSValue) c_int;
    // Atom-based property access (faster than string-based)
    pub extern fn JS_GetProperty(ctx: *JSContext, obj: JSValue, atom: u32) JSValue;

    // Type conversion
    pub extern fn JS_ToBool(ctx: *JSContext, val: JSValue) c_int;
    pub extern fn JS_ToInt32(ctx: *JSContext, pres: *i32, val: JSValue) c_int;
    // JS_ToUint32 is inline in quickjs.h - implement via JS_ToInt32
    pub fn JS_ToUint32(ctx: *JSContext, pres: *u32, val: JSValue) c_int {
        var i32_val: i32 = 0;
        const ret = JS_ToInt32(ctx, &i32_val, val);
        pres.* = @bitCast(i32_val);
        return ret;
    }
    pub extern fn JS_ToFloat64(ctx: *JSContext, pres: *f64, val: JSValue) c_int;
    pub extern fn JS_ToString(ctx: *JSContext, val: JSValue) JSValue;
    pub extern fn JS_ToCStringLen2(ctx: *JSContext, plen: ?*usize, val: JSValue, cesu8: bool) ?[*:0]const u8;
    pub fn JS_ToCString(ctx: *JSContext, val: JSValue) ?[*:0]const u8 {
        return JS_ToCStringLen2(ctx, null, val, false);
    }
    /// Alias for JS_ToCStringLen2 - used by codegen for charCodeAt/slice inlines
    pub fn JS_ToCStringLen(ctx: *JSContext, plen: *usize, val: JSValue) ?[*:0]const u8 {
        return JS_ToCStringLen2(ctx, plen, val, false);
    }
    pub extern fn JS_FreeCString(ctx: *JSContext, ptr: [*:0]const u8) void;

    /// Zero-copy string access - returns slice to QuickJS internal storage
    /// IMPORTANT: The returned slice is only valid while val is live and not modified.
    /// Caller must NOT free this - it's borrowed from QuickJS.
    /// Use this for read-only access (comparisons, hashing) to avoid allocation.
    pub const BorrowedString = struct {
        ptr: [*:0]const u8,
        len: usize,
        ctx: *JSContext,

        pub inline fn slice(self: BorrowedString) []const u8 {
            return self.ptr[0..self.len];
        }

        pub inline fn deinit(self: BorrowedString) void {
            JS_FreeCString(self.ctx, self.ptr);
        }
    };

    /// Borrow string data without copying. Returns null if not a string.
    /// Caller MUST call deinit() when done (unlike true zero-copy).
    pub fn borrowString(ctx: *JSContext, val: JSValue) ?BorrowedString {
        var len: usize = 0;
        const ptr = JS_ToCStringLen2(ctx, &len, val, false);
        if (ptr == null) return null;
        return BorrowedString{ .ptr = ptr, .len = len, .ctx = ctx };
    }

    // Object creation
    pub extern fn JS_NewObject(ctx: *JSContext) JSValue;
    pub extern fn JS_NewArray(ctx: *JSContext) JSValue;
    // Use exported JS_NewStringLen directly (JS_NewString is inline wrapper)
    pub extern fn JS_NewStringLen(ctx: *JSContext, str: [*]const u8, len: usize) JSValue;
    pub fn JS_NewString(ctx: *JSContext, str: [*:0]const u8) JSValue {
        // Calculate length inline in Zig (no C call)
        var len: usize = 0;
        while (str[len] != 0) : (len += 1) {}
        return JS_NewStringLen(ctx, str, len);
    }
    pub extern fn JS_NewFloat64(ctx: *JSContext, val: f64) JSValue;
    // JS_NewInt64 is inline in quickjs.h - use JSValue.newInt64() which handles it

    // Error handling
    pub extern fn JS_ThrowTypeError(ctx: *JSContext, fmt: [*:0]const u8, ...) JSValue;
    pub extern fn JS_ThrowRangeError(ctx: *JSContext, fmt: [*:0]const u8, ...) JSValue;
    pub extern fn JS_ThrowReferenceError(ctx: *JSContext, fmt: [*:0]const u8, ...) JSValue;
    pub extern fn JS_Throw(ctx: *JSContext, val: JSValue) JSValue;
    pub extern fn JS_GetException(ctx: *JSContext) JSValue; // Retrieves and clears pending exception

    // Global object
    pub extern fn JS_GetGlobalObject(ctx: *JSContext) JSValue;

    // Function creation - use exported JS_NewCFunction2 directly
    const JS_CFUNC_generic: c_int = 0;
    pub extern fn JS_NewCFunction2(ctx: *JSContext, func: *const anyopaque, name: [*:0]const u8, length: c_int, cproto: c_int, magic: c_int) JSValue;
    pub fn JS_NewCFunction(ctx: *JSContext, func: *const anyopaque, name: [*:0]const u8, length: c_int) JSValue {
        return JS_NewCFunction2(ctx, func, name, length, JS_CFUNC_generic, 0);
    }

    // Constructor calls
    pub extern fn JS_CallConstructor(ctx: *JSContext, func: JSValue, argc: c_int, argv: [*]const JSValue) JSValue;

    // Property definition
    pub extern fn JS_DefinePropertyValueStr(ctx: *JSContext, this_obj: JSValue, prop: [*:0]const u8, val: JSValue, flags: c_int) c_int;
    pub const JS_PROP_C_W_E: c_int = (1 << 0) | (1 << 1) | (1 << 2); // configurable, writable, enumerable

    // Type checks
    pub extern fn JS_IsFunction(ctx: *JSContext, val: JSValue) c_int;
    pub extern fn JS_IsInstanceOf(ctx: *JSContext, val: JSValue, obj: JSValue) c_int;
    pub extern fn JS_IsEqual(ctx: *JSContext, op1: JSValue, op2: JSValue) c_int;
    pub extern fn JS_IsStrictEqual(ctx: *JSContext, op1: JSValue, op2: JSValue) bool;

    // Array/String length - O(1) access without property lookup
    pub extern fn JS_GetLength(ctx: *JSContext, obj: JSValue, plen: *i64) c_int;

    // Property deletion
    pub extern fn JS_DeleteProperty(ctx: *JSContext, obj: JSValue, prop: JSValue, flags: c_int) c_int;

    // Iterator protocol (frozen functions)
    pub extern fn js_frozen_for_of_start(ctx: *JSContext, sp: [*]JSValue, is_async: c_int) c_int;
    pub extern fn js_frozen_for_of_next(ctx: *JSContext, sp: [*]JSValue, offset: c_int) c_int;
    pub extern fn js_frozen_get_iterator(ctx: *JSContext, obj: JSValue, is_async: c_int) JSValue;
    pub extern fn js_frozen_iterator_next(ctx: *JSContext, iter: JSValue, done: *c_int) JSValue;
    pub extern fn js_frozen_iterator_close(ctx: *JSContext, iter: JSValue, completion_type: c_int) c_int;
    pub extern fn js_frozen_iterator_get_value_done(ctx: *JSContext, result: JSValue, done: *c_int) JSValue;

    // Object operations (frozen functions)
    pub extern fn js_frozen_set_field(ctx: *JSContext, obj: JSValue, prop: [*:0]const u8, val: JSValue) c_int;
    pub extern fn js_frozen_to_object(ctx: *JSContext, val: JSValue) JSValue;
    pub extern fn js_frozen_get_length(ctx: *JSContext, obj: JSValue) JSValue;
    pub extern fn js_frozen_to_prop_key(ctx: *JSContext, val: JSValue) JSValue;
    pub extern fn js_frozen_copy_data_properties(ctx: *JSContext, dst: JSValue, src: JSValue, exclude_flags: c_int) c_int;

    // Memory allocation (QuickJS exported)
    pub extern fn js_malloc(ctx: *JSContext, size: usize) ?*anyopaque;
    pub extern fn js_free(ctx: *JSContext, ptr: *anyopaque) void;

    // TypedArray/ArrayBuffer access
    pub extern fn JS_GetTypedArrayBuffer(ctx: *JSContext, obj: JSValue, pbyte_offset: *usize, pbyte_length: *usize, pbytes_per_element: *usize) JSValue;
    pub extern fn JS_GetArrayBuffer(ctx: *JSContext, psize: *usize, obj: JSValue) ?[*]u8;
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
// Closure variables are accessed through var_refs from the function's closure
// The closure_var_indices map bytecode indices to var_refs array positions
// ============================================================================

/// Get closure variable from var_refs array
/// @param ctx - JSContext for memory management
/// @param var_refs - array of closure variable references from QuickJS
/// @param position - index into var_refs array
pub inline fn getClosureVar(ctx: *JSContext, var_refs: ?[*]*JSVarRef, position: u32) JSValue {
    // Use C FFI for correct struct layout handling
    // The JSVarRef struct layout in C doesn't match Zig's extern struct due to
    // GC header union alignment differences
    if (var_refs != null) {
        return quickjs.js_frozen_get_var_ref(ctx, @ptrCast(var_refs), @intCast(position));
    }
    return JSValue.UNDEFINED;
}

/// Set closure variable in var_refs array
/// @param ctx - JSContext for memory management
/// @param var_refs - array of closure variable references from QuickJS
/// @param position - index into var_refs array
/// @param val - value to set (ownership transferred)
pub inline fn setClosureVar(ctx: *JSContext, var_refs: ?[*]*JSVarRef, position: u32, val: JSValue) void {
    // Use C FFI for correct struct layout handling
    if (var_refs != null) {
        quickjs.js_frozen_set_var_ref(ctx, @ptrCast(var_refs), @intCast(position), val);
    }
}

/// Get closure variable with TDZ (Temporal Dead Zone) check
/// Returns EXCEPTION if variable is uninitialized
pub inline fn getClosureVarCheck(ctx: *JSContext, var_refs: ?[*]*JSVarRef, position: u32) JSValue {
    const val = getClosureVar(ctx, var_refs, position);
    if (val.isUninitialized()) {
        return JSValue.throwReferenceError(ctx, "Cannot access '%s' before initialization");
    }
    return val;
}

/// Set closure variable with TDZ check
/// Returns true if variable was uninitialized (error condition)
pub inline fn setClosureVarCheck(ctx: *JSContext, var_refs: ?[*]*JSVarRef, position: u32, val: JSValue) bool {
    const existing = getClosureVar(ctx, var_refs, position);
    JSValue.free(ctx, existing);
    if (existing.isUninitialized()) {
        JSValue.free(ctx, val);
        _ = JSValue.throwReferenceError(ctx, "Cannot access '%s' before initialization");
        return true; // Error
    }
    setClosureVar(ctx, var_refs, position, val);
    return false;
}

// ============================================================================
// Iterator Protocol Helpers
// ============================================================================

/// Create a catch offset value (for try/catch/finally and iterator cleanup)
/// This is a pure Zig implementation matching JS_NewCatchOffset macro
pub inline fn newCatchOffset(val: i32) JSValue {
    if (is_wasm32) {
        // WASM32: NaN-boxed u64 (tag in upper 32 bits, payload in lower 32 bits)
        return JSValue{ .bits = (@as(u64, @intCast(@as(u32, @bitCast(@as(i32, JS_TAG_CATCH_OFFSET))))) << 32) | @as(u64, @intCast(@as(u32, @bitCast(val)))) };
    } else {
        // Native: 16-byte struct
        return JSValue{ .u = .{ .int32 = val }, .tag = JS_TAG_CATCH_OFFSET };
    }
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
// Object Operations
// ============================================================================

/// Copy data properties from source to destination (for spread operator)
/// excludeFlags: bitmask of properties to exclude (0 for none)
pub fn copyDataProperties(ctx: *JSContext, dst: JSValue, src: JSValue, excludeFlags: c_int) c_int {
    return quickjs.js_frozen_copy_data_properties(ctx, dst, src, excludeFlags);
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

/// CV-aware block fallback for partial_freeze functions
/// Converts CV arrays to JSValue, calls fallback, converts back
pub fn blockFallbackCV(
    ctx: *JSContext,
    func_name: [*:0]const u8,
    this_val: JSValue,
    argc: c_int,
    argv: [*]JSValue,
    cv_locals: [*]CompressedValue,
    num_locals: c_int,
    cv_stack: [*]CompressedValue,
    sp: *usize,
    block_id: usize,
    next_block: *usize,
) JSValue {
    // Convert CV locals to JSValue (temporary arrays)
    var js_locals: [256]JSValue = .{JSValue.UNDEFINED} ** 256;
    var js_stack: [256]JSValue = .{JSValue.UNDEFINED} ** 256;

    const local_count: usize = @intCast(num_locals);
    for (0..local_count) |i| {
        js_locals[i] = cv_locals[i].toJSValue();
    }

    const stack_count = sp.*;
    for (0..stack_count) |i| {
        js_stack[i] = cv_stack[i].toJSValue();
    }

    // Call the JSValue-based fallback
    const result = blockFallback(
        ctx,
        func_name,
        this_val,
        argc,
        argv,
        &js_locals,
        num_locals,
        &js_stack,
        sp,
        block_id,
        next_block,
    );

    // Convert JSValue results back to CV
    for (0..local_count) |i| {
        cv_locals[i] = CompressedValue.fromJSValue(js_locals[i]);
    }

    const new_stack_count = sp.*;
    for (0..new_stack_count) |i| {
        cv_stack[i] = CompressedValue.fromJSValue(js_stack[i]);
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
/// Uses the raw representation as a unique identifier
pub inline fn jsvalueToAddr(val: JSValue) u64 {
    if (is_wasm32) {
        // WASM32: 64-bit NaN-boxed value, use bits directly
        return val.bits;
    } else {
        // Native 64-bit: Use both tag and value parts for unique address
        // This matches the C macro: union { JSValue v; uint64_t u; } u; u.u = val;
        // For 16-byte JSValue, we use tag as high bits, value.ptr as low bits
        const ptr_bits: u64 = @intFromPtr(val.u.ptr);
        const tag_bits: u64 = @bitCast(val.tag);
        return ptr_bits ^ (tag_bits << 32);
    }
}

/// Import native registry lookup from native_shapes.zig
/// The native_shapes module provides a SIMD-optimized columnar registry
extern fn native_shapes_lookup(js_addr: u64) ?*NativeAstNode;

/// Fast lookup for native node - delegates to native_shapes.zig implementation
/// Returns null if not registered
pub inline fn native_node_lookup(js_addr: u64) ?*NativeAstNode {
    if (js_addr == 0) return null;
    return native_shapes_lookup(js_addr);
}

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
            const parent_val: JSValue = if (is_wasm32)
                // WASM32: 64-bit NaN-boxed, bits stored directly
                .{ .bits = parent.js_value }
            else
                // Native 64-bit: Separate tag and pointer
                .{
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
        std.mem.eql(u8, name, "parent") or
        std.mem.eql(u8, name, "length");
}

/// Get array/string length with native fast path
/// Uses JS_GetLength for arrays (O(1)) instead of property lookup
pub inline fn nativeGetLength(ctx: *JSContext, obj: JSValue) JSValue {
    // JS_GetLength is O(1) for arrays and strings - much faster than getPropertyStr
    var len: i64 = 0;
    if (quickjs.JS_GetLength(ctx, obj, &len) < 0) {
        // Not an array/string, fall back to property access
        return JSValue.getPropertyStr(ctx, obj, "length");
    }
    return JSValue.newInt64(ctx, len);
}

// ============================================================================
// Cached Atoms for Fast Property Access
// ============================================================================
// Pre-computed atoms skip string hashing on every access (~10 cycles saved)

/// Cached atoms for hot TSC properties
pub var cached_atoms: CachedAtoms = .{};

pub const CachedAtoms = struct {
    initialized: bool = false,
    // TSC hot properties (beyond kind/flags/pos/end/parent which use native shapes)
    symbol: u32 = 0,
    escapedName: u32 = 0,
    declarations: u32 = 0,
    valueDeclaration: u32 = 0,
    members: u32 = 0,
    properties: u32 = 0,
    target: u32 = 0,
    constraint: u32 = 0,
    modifiers: u32 = 0,
    name: u32 = 0,
    text: u32 = 0,
    type_: u32 = 0, // "type" is reserved in Zig
    checker: u32 = 0,
    typeArguments: u32 = 0,
    arguments: u32 = 0,
};

/// Initialize cached atoms (call once at module init)
pub fn initCachedAtoms(ctx: *JSContext) void {
    if (cached_atoms.initialized) return;

    cached_atoms.symbol = quickjs.JS_NewAtom(ctx, "symbol");
    cached_atoms.escapedName = quickjs.JS_NewAtom(ctx, "escapedName");
    cached_atoms.declarations = quickjs.JS_NewAtom(ctx, "declarations");
    cached_atoms.valueDeclaration = quickjs.JS_NewAtom(ctx, "valueDeclaration");
    cached_atoms.members = quickjs.JS_NewAtom(ctx, "members");
    cached_atoms.properties = quickjs.JS_NewAtom(ctx, "properties");
    cached_atoms.target = quickjs.JS_NewAtom(ctx, "target");
    cached_atoms.constraint = quickjs.JS_NewAtom(ctx, "constraint");
    cached_atoms.modifiers = quickjs.JS_NewAtom(ctx, "modifiers");
    cached_atoms.name = quickjs.JS_NewAtom(ctx, "name");
    cached_atoms.text = quickjs.JS_NewAtom(ctx, "text");
    cached_atoms.type_ = quickjs.JS_NewAtom(ctx, "type");
    cached_atoms.checker = quickjs.JS_NewAtom(ctx, "checker");
    cached_atoms.typeArguments = quickjs.JS_NewAtom(ctx, "typeArguments");
    cached_atoms.arguments = quickjs.JS_NewAtom(ctx, "arguments");
    cached_atoms.initialized = true;
}

// Native getters using cached atoms (faster than getPropertyStr)
pub inline fn nativeGetSymbol(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.symbol);
}

pub inline fn nativeGetEscapedName(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.escapedName);
}

pub inline fn nativeGetDeclarations(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.declarations);
}

pub inline fn nativeGetValueDeclaration(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.valueDeclaration);
}

pub inline fn nativeGetMembers(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.members);
}

pub inline fn nativeGetProperties(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.properties);
}

pub inline fn nativeGetTarget(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.target);
}

pub inline fn nativeGetConstraint(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.constraint);
}

pub inline fn nativeGetModifiers(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.modifiers);
}

pub inline fn nativeGetName(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.name);
}

pub inline fn nativeGetText(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.text);
}

pub inline fn nativeGetType(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.type_);
}

pub inline fn nativeGetChecker(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.checker);
}

pub inline fn nativeGetTypeArguments(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.typeArguments);
}

pub inline fn nativeGetArguments(ctx: *JSContext, obj: JSValue) JSValue {
    if (!cached_atoms.initialized) initCachedAtoms(ctx);
    return quickjs.JS_GetProperty(ctx, obj, cached_atoms.arguments);
}

// ============================================================================
// TypedArray Fast Path
// ============================================================================

/// Result of TypedArray fast path sum operation
pub const TypedArraySumResult = struct {
    success: bool,
    sum: i64,
};

/// Try to sum a TypedArray using direct buffer access
/// Returns success=true if the input is an Int32Array and we can sum it directly
/// Otherwise returns success=false and caller should use regular loop
pub fn sumTypedArrayFast(ctx: *JSContext, arr: JSValue) TypedArraySumResult {
    var byte_offset: usize = 0;
    var byte_length: usize = 0;
    var bytes_per_element: usize = 0;

    // DEBUG: Verify function is called (uncomment for debugging)
    // std.debug.print("[TypedArray fast path] called\n", .{});

    // Try to get typed array buffer
    const buffer = quickjs.JS_GetTypedArrayBuffer(ctx, arr, &byte_offset, &byte_length, &bytes_per_element);

    // Not a TypedArray - check if exception
    if (buffer.isException()) {
        // Clear the pending exception so it doesn't propagate
        const exc = quickjs.JS_GetException(ctx);
        JSValue.free(ctx, exc);
        return .{ .success = false, .sum = 0 };
    }

    // Only optimize Int32Array (4 bytes per element)
    if (bytes_per_element != 4) {
        JSValue.free(ctx, buffer);
        return .{ .success = false, .sum = 0 };
    }

    // Get the raw array buffer pointer
    var buf_size: usize = 0;
    const buf_ptr = quickjs.JS_GetArrayBuffer(ctx, &buf_size, buffer);
    JSValue.free(ctx, buffer);

    if (buf_ptr == null) {
        return .{ .success = false, .sum = 0 };
    }

    // Calculate data pointer and length
    const data: [*]const i32 = @ptrCast(@alignCast(buf_ptr.? + byte_offset));
    const length = byte_length / bytes_per_element;

    // Hot loop - direct memory access, no JSValue boxing
    var sum: i64 = 0;
    for (0..length) |i| {
        sum += data[i];
    }

    return .{ .success = true, .sum = sum };
}

// ============================================================================
// JSObject - Direct Access to QuickJS Internals (Zero-FFI Array Access)
// ============================================================================

/// QuickJS class IDs for array types
pub const JS_CLASS_ARRAY: u16 = 2; // from quickjs.c enum
pub const JS_CLASS_ARGUMENTS: u16 = 8;

/// JSObject internal structure matching QuickJS layout
/// Used for direct fast_array access without FFI
///
/// QuickJS JSObject layout (64-bit):
///   Offset 0:  ref_count (4 bytes)
///   Offset 4:  gc_mark (1 byte)
///   Offset 5:  flags (1 byte) - fast_array is bit 3
///   Offset 6:  class_id (2 bytes)
///   Offset 8:  list_head link (16 bytes - two pointers)
///   Offset 24: shape (8 bytes)
///   Offset 32: prop (8 bytes)
///   Offset 40: first_weak_ref (8 bytes)
///   Offset 48: union u (contains array struct for arrays)
pub const JSObject = extern struct {
    // Header (ref_count + gc bits + flags + class_id) - 8 bytes
    ref_count: i32,
    gc_mark: u8,
    flags: u8, // extensible:1, free_mark:1, is_exotic:1, fast_array:1, ...
    class_id: u16,

    // list_head link (two pointers) - 16 bytes on 64-bit
    link_next: ?*anyopaque,
    link_prev: ?*anyopaque,

    // Pointers after list_head
    shape: ?*anyopaque,
    prop: ?*anyopaque,
    first_weak_ref: ?*anyopaque,

    // Union - we only care about array case
    u: extern union {
        opaque_ptr: ?*anyopaque,
        array: extern struct {
            u1: extern union {
                size: u32,
                typed_array: ?*anyopaque,
            },
            values: ?[*]JSValue, // Direct pointer to JSValue array for fast_array
            count: u32,
        },
    },

    /// Check if this object has fast_array flag set
    pub inline fn isFastArray(self: *const JSObject) bool {
        return (self.flags & 0x08) != 0; // fast_array is bit 3
    }

    /// Check if this is a regular JS array (not typed array)
    pub inline fn isRegularArray(self: *const JSObject) bool {
        return self.class_id == JS_CLASS_ARRAY or self.class_id == JS_CLASS_ARGUMENTS;
    }
};

/// Result of fast array access attempt
pub const FastArrayResult = struct {
    values: ?[*]JSValue,
    count: u32,
    success: bool,
};

/// Try to get direct pointer to array values (zero-FFI)
/// Returns success=true and values pointer for fast arrays
/// Returns success=false for non-arrays or sparse arrays
pub inline fn getFastArrayDirect(val: JSValue) FastArrayResult {
    // Must be an object
    if (!val.isObject()) {
        return .{ .values = null, .count = 0, .success = false };
    }

    // Get JSObject pointer
    const obj: *const JSObject = @ptrCast(@alignCast(val.getPtr()));

    // Check if it's a fast array
    if (!obj.isFastArray() or !obj.isRegularArray()) {
        return .{ .values = null, .count = 0, .success = false };
    }

    // Return direct access to values
    return .{
        .values = obj.u.array.values,
        .count = obj.u.array.count,
        .success = true,
    };
}

/// Extract int32 from JSValue inline (no FFI)
pub inline fn jsValueToInt32Inline(val: JSValue) i32 {
    if (val.isInt()) {
        return val.getInt();
    } else if (val.isFloat64()) {
        return @intFromFloat(val.getFloat64());
    }
    return 0;
}

/// Extract int64 from JSValue inline (no FFI)
pub inline fn jsValueToInt64Inline(val: JSValue) i64 {
    if (val.isInt()) {
        return val.getInt();
    } else if (val.isFloat64()) {
        return @intFromFloat(val.getFloat64());
    }
    return 0;
}

// ============================================================================
// String Intern Pool - Cache frequently accessed strings
// ============================================================================

/// String intern pool for caching frequently accessed strings.
/// Uses FNV-1a hash for O(1) lookup. Useful for property names, identifiers.
pub const StringPool = struct {
    const POOL_SIZE = 256; // Must be power of 2
    const Entry = struct {
        hash: u32,
        str: []const u8,
        valid: bool,
    };

    entries: [POOL_SIZE]Entry,
    data: std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) StringPool {
        return StringPool{
            .entries = [_]Entry{.{ .hash = 0, .str = &.{}, .valid = false }} ** POOL_SIZE,
            .data = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *StringPool) void {
        self.data.deinit(self.allocator);
    }

    /// FNV-1a hash
    fn hash(str: []const u8) u32 {
        var h: u32 = 2166136261;
        for (str) |c| {
            h ^= c;
            h *%= 16777619;
        }
        return h;
    }

    /// Intern a string - returns pointer to pooled copy
    pub fn intern(self: *StringPool, str: []const u8) ![]const u8 {
        const h = hash(str);
        const idx = h & (POOL_SIZE - 1);

        // Check if already interned
        if (self.entries[idx].valid and self.entries[idx].hash == h) {
            if (std.mem.eql(u8, self.entries[idx].str, str)) {
                return self.entries[idx].str;
            }
        }

        // Add to pool
        const start = self.data.items.len;
        try self.data.appendSlice(self.allocator, str);
        const interned = self.data.items[start..];

        self.entries[idx] = .{
            .hash = h,
            .str = interned,
            .valid = true,
        };

        return interned;
    }

    /// Check if string is in pool without adding
    pub fn lookup(self: *const StringPool, str: []const u8) ?[]const u8 {
        const h = hash(str);
        const idx = h & (POOL_SIZE - 1);

        if (self.entries[idx].valid and self.entries[idx].hash == h) {
            if (std.mem.eql(u8, self.entries[idx].str, str)) {
                return self.entries[idx].str;
            }
        }
        return null;
    }
};

/// Global string pool for runtime use (initialized on first use)
var global_string_pool: ?StringPool = null;
var global_pool_allocator: ?std.mem.Allocator = null;

pub fn getGlobalStringPool(allocator: std.mem.Allocator) *StringPool {
    if (global_string_pool == null) {
        global_string_pool = StringPool.init(allocator);
        global_pool_allocator = allocator;
    }
    return &global_string_pool.?;
}

pub fn deinitGlobalStringPool() void {
    if (global_string_pool) |*pool| {
        pool.deinit();
        global_string_pool = null;
    }
}

// ============================================================================
// Tests
// ============================================================================

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
