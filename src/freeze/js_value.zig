//! JSValue Types and QuickJS FFI Bindings
//!
//! This module provides:
//! - JSValue wrapper type matching QuickJS layout (WASM32 and Native)
//! - CompressedValue (8-byte NaN-boxed) for stack efficiency
//! - JSVarRef for closure variable access
//! - QuickJS FFI bindings (extern declarations)
//!
//! Design goals:
//! - Zero overhead for SMI operations (inline, no function calls)
//! - Type-safe JSValue handling in Zig
//! - Compatible with QuickJS ABI for FFI calls

const std = @import("std");
const types = @import("js_types.zig");

// Re-export types for convenience
pub const JSContext = types.JSContext;
pub const JSRuntime = types.JSRuntime;
pub const JSValueUnion = types.JSValueUnion;
pub const is_wasm32 = types.is_wasm32;

// Import tag constants
const JS_TAG_FIRST = types.JS_TAG_FIRST;
const JS_TAG_BIG_INT = types.JS_TAG_BIG_INT;
const JS_TAG_SYMBOL = types.JS_TAG_SYMBOL;
const JS_TAG_STRING = types.JS_TAG_STRING;
const JS_TAG_MODULE = types.JS_TAG_MODULE;
const JS_TAG_FUNCTION_BYTECODE = types.JS_TAG_FUNCTION_BYTECODE;
const JS_TAG_OBJECT = types.JS_TAG_OBJECT;
const JS_TAG_INT = types.JS_TAG_INT;
const JS_TAG_BOOL = types.JS_TAG_BOOL;
const JS_TAG_NULL = types.JS_TAG_NULL;
const JS_TAG_UNDEFINED = types.JS_TAG_UNDEFINED;
const JS_TAG_UNINITIALIZED = types.JS_TAG_UNINITIALIZED;
const JS_TAG_CATCH_OFFSET = types.JS_TAG_CATCH_OFFSET;
const JS_TAG_EXCEPTION = types.JS_TAG_EXCEPTION;
const JS_TAG_SHORT_BIG_INT = types.JS_TAG_SHORT_BIG_INT;
const JS_TAG_FLOAT64 = types.JS_TAG_FLOAT64;

// ============================================================================
// CompressedValue - 8-byte NaN-boxed JSValue for frozen function stacks
// ============================================================================

pub var compressed_heap_base: usize = 0;

pub fn initCompressedHeap(base: usize) void {
    compressed_heap_base = base;
}

/// Compressed JSValue for frozen functions (8 bytes via NaN-boxing)
/// Encoding: f64 where NaN bits encode type and payload
pub const CompressedValue = packed struct {
    bits: u64,

    // NaN-boxing constants (V8/JSC style)
    const QNAN: u64 = 0x7FF8000000000000; // Quiet NaN
    const TAG_MASK: u64 = 0xFFFF000000000000;
    const PAYLOAD_MASK: u64 = 0x0000FFFFFFFFFFFF;

    // Type tags (stored in upper 16 bits above NaN)
    // IMPORTANT: QNAN (0x7FF8) has bits 3-14 set, so we can only use bits 0-2 for tags
    // This gives us 8 unique tags (0-7). Tags 0x0008+ collide with 0x0000+!
    const TAG_INT: u64 = 0x0001000000000000; // Integer value
    const TAG_BOOL: u64 = 0x0002000000000000; // Boolean value
    const TAG_NULL: u64 = 0x0003000000000000; // null
    const TAG_UNDEF: u64 = 0x0004000000000000; // undefined
    const TAG_PTR: u64 = 0x0005000000000000; // Object/Symbol/BigInt/Func pointer (generic ref type)
    const TAG_UNINIT: u64 = 0x0006000000000000; // Uninitialized slot
    const TAG_STR: u64 = 0x0007000000000000; // String pointer
    const TAG_EXCEPTION: u64 = 0x0003000000000001; // Exception marker (uses NULL tag + bit 0)

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

    /// Convert to i32 (alias for getInt, used by codegen)
    pub inline fn toInt32(self: CompressedValue) i32 {
        return self.getInt();
    }

    // Reference type checks - only PTR and STR are reference types
    // (Symbol, BigInt, Func are stored under TAG_PTR)
    pub inline fn isRefType(self: CompressedValue) bool {
        const tag = self.bits & TAG_MASK;
        return tag == (QNAN | TAG_PTR) or tag == (QNAN | TAG_STR);
    }

    pub inline fn isPtr(self: CompressedValue) bool {
        return (self.bits & TAG_MASK) == (QNAN | TAG_PTR);
    }

    pub inline fn isStr(self: CompressedValue) bool {
        return (self.bits & TAG_MASK) == (QNAN | TAG_STR);
    }

    // Note: Symbol, BigInt, Func are stored under TAG_PTR - use isPtr() for them

    // Pointer compression/decompression
    pub inline fn compressPtr(ptr: ?*anyopaque, extra_tag: u64) CompressedValue {
        const addr = @intFromPtr(ptr);
        if (compressed_heap_base != 0 and addr >= compressed_heap_base) {
            const offset = addr - compressed_heap_base;
            if (offset <= PAYLOAD_MASK) {
                return .{ .bits = QNAN | TAG_PTR | extra_tag | offset };
            }
        }
        // Fallback: store low bits (works for most heap layouts)
        return .{ .bits = QNAN | TAG_PTR | extra_tag | (addr & PAYLOAD_MASK) };
    }

    pub inline fn compressPtrWithTag(ptr: ?*anyopaque, tag: u64) CompressedValue {
        const addr = @intFromPtr(ptr);
        if (compressed_heap_base != 0 and addr >= compressed_heap_base) {
            const offset = addr - compressed_heap_base;
            if (offset <= PAYLOAD_MASK) {
                return .{ .bits = QNAN | tag | offset };
            }
        }
        return .{ .bits = QNAN | tag | (addr & PAYLOAD_MASK) };
    }

    pub inline fn decompressPtr(self: CompressedValue) ?*anyopaque {
        const offset: usize = @truncate(self.bits & PAYLOAD_MASK);
        if (compressed_heap_base != 0) {
            return @ptrFromInt(compressed_heap_base + offset);
        }
        return @ptrFromInt(offset);
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
            // Note: Symbols, BigInts, and Funcs are stored under TAG_PTR and will be
            // decompressed as JS_TAG_OBJECT. This works for most cases.
            const ptr = self.decompressPtr();
            const tag: i64 = if (self.isStr()) JS_TAG_STRING else JS_TAG_OBJECT;
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
        } else if (val.isSymbol() or val.isBigInt() or val.isFunctionBytecode() or val.isObject()) {
            // Compress all reference types (object, symbol, bigint, func) with generic PTR tag
            // They'll be decompressed as JS_TAG_OBJECT which works for most operations
            return CompressedValue.compressPtr(val.getPtr(), 0);
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

    pub noinline fn div(a: CompressedValue, b: CompressedValue) CompressedValue {
        // Use explicit branches to avoid WASM32 codegen issues with inline conditionals
        var fa: f64 = undefined;
        var fb: f64 = undefined;
        if (a.isInt()) {
            fa = @floatFromInt(a.getInt());
        } else {
            fa = a.getFloat();
        }
        if (b.isInt()) {
            fb = @floatFromInt(b.getInt());
        } else {
            fb = b.getFloat();
        }
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

    pub inline fn lte(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            return if (a.getInt() <= b.getInt()) TRUE else FALSE;
        }
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return if (fa <= fb) TRUE else FALSE;
    }

    pub inline fn gt(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            return if (a.getInt() > b.getInt()) TRUE else FALSE;
        }
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return if (fa > fb) TRUE else FALSE;
    }

    pub inline fn gte(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            return if (a.getInt() >= b.getInt()) TRUE else FALSE;
        }
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return if (fa >= fb) TRUE else FALSE;
    }

    pub inline fn eq(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            return if (a.getInt() == b.getInt()) TRUE else FALSE;
        }
        if (a.isFloat() and b.isFloat()) {
            return if (a.getFloat() == b.getFloat()) TRUE else FALSE;
        }
        // Mixed int/float
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return if (fa == fb) TRUE else FALSE;
    }

    pub inline fn neq(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            return if (a.getInt() != b.getInt()) TRUE else FALSE;
        }
        if (a.isFloat() and b.isFloat()) {
            return if (a.getFloat() != b.getFloat()) TRUE else FALSE;
        }
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return if (fa != fb) TRUE else FALSE;
    }

    // Bitwise operations
    pub inline fn bitAnd(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            return newInt(a.getInt() & b.getInt());
        }
        return UNDEFINED;
    }

    pub inline fn bitOr(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            return newInt(a.getInt() | b.getInt());
        }
        return UNDEFINED;
    }

    pub inline fn bitXor(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            return newInt(a.getInt() ^ b.getInt());
        }
        return UNDEFINED;
    }

    pub inline fn bitNot(a: CompressedValue) CompressedValue {
        if (a.isInt()) {
            return newInt(~a.getInt());
        }
        return UNDEFINED;
    }

    pub inline fn shl(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            const shift: u5 = @truncate(@as(u32, @bitCast(b.getInt())));
            return newInt(a.getInt() << shift);
        }
        return UNDEFINED;
    }

    pub inline fn sar(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            const shift: u5 = @truncate(@as(u32, @bitCast(b.getInt())));
            return newInt(a.getInt() >> shift);
        }
        return UNDEFINED;
    }

    pub inline fn shr(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            const ua: u32 = @bitCast(a.getInt());
            const shift: u5 = @truncate(@as(u32, @bitCast(b.getInt())));
            const result = ua >> shift;
            if (result <= std.math.maxInt(i32)) {
                return newInt(@bitCast(result));
            }
            return newFloat(@floatFromInt(result));
        }
        return UNDEFINED;
    }

    pub inline fn neg(a: CompressedValue) CompressedValue {
        if (a.isInt()) {
            const ia = a.getInt();
            if (ia == 0) return newFloat(-0.0);
            if (ia == std.math.minInt(i32)) return newFloat(-@as(f64, @floatFromInt(ia)));
            return newInt(-ia);
        }
        if (a.isFloat()) {
            return newFloat(-a.getFloat());
        }
        return UNDEFINED;
    }

    pub inline fn mod(a: CompressedValue, b: CompressedValue) CompressedValue {
        if (a.isInt() and b.isInt()) {
            const ia = a.getInt();
            const ib = b.getInt();
            if (ib == 0) return newFloat(std.math.nan(f64));
            if (ib == -1 and ia == std.math.minInt(i32)) return newInt(0);
            return newInt(@rem(ia, ib));
        }
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return newFloat(@mod(fa, fb));
    }

    /// JavaScript truthy/falsy conversion
    /// Returns false for: undefined, null, false, 0, -0, NaN, ""
    /// Returns true for everything else
    pub inline fn toBool(self: CompressedValue) bool {
        // Check falsy primitives first (most common cases)
        if (self.bits == FALSE.bits) return false;
        if (self.bits == UNDEFINED.bits) return false;
        if (self.bits == NULL.bits) return false;

        // Check for integer 0
        if (self.isInt()) {
            return self.getInt() != 0;
        }

        // Check for float 0 or NaN
        if (self.isFloat()) {
            const f = self.getFloat();
            return f != 0.0 and !std.math.isNan(f);
        }

        // TRUE and all reference types (objects, strings, symbols, etc.) are truthy
        // Note: Empty string would need FFI to check length, but compressed strings
        // are pointers to QuickJS strings - we treat all string pointers as truthy
        // for performance (matches V8/JSC fast path behavior)
        return true;
    }

    /// Strict equality (===) - same type AND same value
    pub inline fn strictEq(a: CompressedValue, b: CompressedValue) CompressedValue {
        // Fast path: identical bits means equal
        if (a.bits == b.bits) {
            // Exception: NaN !== NaN
            if (a.isFloat()) {
                const f = a.getFloat();
                if (std.math.isNan(f)) return FALSE;
            }
            return TRUE;
        }

        // Different types are never strictly equal (except int/float comparison)
        const a_is_int = a.isInt();
        const b_is_int = b.isInt();
        const a_is_float = a.isFloat();
        const b_is_float = b.isFloat();

        // int === int with different bits means different values
        if (a_is_int and b_is_int) return FALSE;

        // float === float with different bits: check for -0 === 0
        if (a_is_float and b_is_float) {
            const fa = a.getFloat();
            const fb = b.getFloat();
            return if (fa == fb) TRUE else FALSE;
        }

        // int === float or float === int: compare as numbers
        if ((a_is_int and b_is_float) or (a_is_float and b_is_int)) {
            const fa: f64 = if (a_is_float) a.getFloat() else @floatFromInt(a.getInt());
            const fb: f64 = if (b_is_float) b.getFloat() else @floatFromInt(b.getInt());
            return if (fa == fb) TRUE else FALSE;
        }

        // Different types (bool, null, undefined, object, string, etc.)
        return FALSE;
    }

    /// Strict inequality (!==) - inverse of strictEq
    pub inline fn strictNeq(a: CompressedValue, b: CompressedValue) CompressedValue {
        const result = strictEq(a, b);
        return if (result.bits == TRUE.bits) FALSE else TRUE;
    }

    /// Convert to number (for unary + operator and arithmetic coercion)
    pub inline fn toNumber(self: CompressedValue) CompressedValue {
        if (self.isInt() or self.isFloat()) {
            return self;
        }
        if (self.bits == TRUE.bits) {
            return newInt(1);
        }
        if (self.bits == FALSE.bits or self.bits == NULL.bits) {
            return newInt(0);
        }
        if (self.bits == UNDEFINED.bits) {
            return newFloat(std.math.nan(f64));
        }
        // For reference types (object, string), return NaN
        // Full conversion would require FFI (toString -> parseFloat)
        return newFloat(std.math.nan(f64));
    }

    /// Check if value is a boolean
    pub inline fn isBool(self: CompressedValue) bool {
        return self.bits == TRUE.bits or self.bits == FALSE.bits;
    }

    /// Get boolean value (assumes isBool() is true)
    pub inline fn getBool(self: CompressedValue) bool {
        return self.bits == TRUE.bits;
    }
};

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
        return self.isInt() or self.isFloat64();
    }

    pub inline fn isFloat64(self: JSValueWasm32) bool {
        const tag = self.getTag();
        // On WASM32, floats are stored raw (not tagged), so their "tag" (upper 32 bits)
        // falls outside the valid tag range of [-9, 7] (JS_TAG_FIRST to JS_TAG_SHORT_BIG_INT)
        return tag < @as(i32, @intCast(JS_TAG_FIRST)) or tag > @as(i32, @intCast(JS_TAG_SHORT_BIG_INT));
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

    pub inline fn newArray(ctx: *JSContext) JSValueWasm32 {
        return quickjs.JS_NewArray(ctx);
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
    pub fn strictEq(ctx: *JSContext, a: JSValueWasm32, b: JSValueWasm32) bool {
        return quickjs.JS_IsStrictEqual(ctx, a, b);
    }

    /// Get typeof result as JSValue string
    pub fn typeOf(ctx: *JSContext, val: JSValueWasm32) JSValueWasm32 {
        const type_str = switch (val.getTag()) {
            JS_TAG_UNDEFINED => "undefined",
            JS_TAG_NULL => "object",
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
            else => "undefined",
        };
        return quickjs.JS_NewString(ctx, type_str);
    }

    pub inline fn getPropertyUint32(ctx: *JSContext, this: JSValueWasm32, idx: u32) JSValueWasm32 {
        return quickjs.JS_GetPropertyUint32(ctx, this, idx);
    }

    pub inline fn setPropertyUint32(ctx: *JSContext, this: JSValueWasm32, idx: u32, val: JSValueWasm32) c_int {
        return quickjs.JS_SetPropertyUint32(ctx, this, idx, val);
    }

    pub inline fn definePropertyUint32(ctx: *JSContext, obj: JSValueWasm32, idx: u32, val: JSValueWasm32) c_int {
        return quickjs.JS_SetPropertyUint32(ctx, obj, idx, val);
    }

    /// Dynamic property access (prop can be string or number)
    /// Note: prop is consumed (ownership transferred)
    pub inline fn getPropertyValue(ctx: *JSContext, obj: JSValueWasm32, prop: JSValueWasm32) JSValueWasm32 {
        // Fast path for integer indices (like original JS_GetPropertyValue)
        if (prop.isInt()) {
            const idx = prop.getInt();
            if (idx >= 0) {
                // No need to free - integers don't have refcount
                return quickjs.JS_GetPropertyUint32(ctx, obj, @intCast(idx));
            }
        }
        // Slow path: JS_ValueToAtom + JS_GetProperty + JS_FreeAtom
        // Note: JS_ValueToAtom takes ownership of prop for strings, so we free it after
        const atom = quickjs.JS_ValueToAtom(ctx, prop);
        // JS_ValueToAtom returns a new atom, doesn't consume prop - we must free it
        quickjs.JS_FreeValue(ctx, prop);
        if (atom == 0) return EXCEPTION; // JS_ATOM_NULL means error
        const result = quickjs.JS_GetProperty(ctx, obj, atom);
        quickjs.JS_FreeAtom(ctx, atom);
        return result;
    }

    /// Get global variable by name
    pub inline fn getGlobal(ctx: *JSContext, name: [*:0]const u8) JSValueWasm32 {
        const global = quickjs.JS_GetGlobalObject(ctx);
        const val = quickjs.JS_GetPropertyStr(ctx, global, name);
        quickjs.JS_FreeValue(ctx, global);
        return val;
    }

    /// Create new string from null-terminated string
    pub inline fn newString(ctx: *JSContext, str: [*:0]const u8) JSValueWasm32 {
        return quickjs.JS_NewString(ctx, str);
    }

    /// Convert to bool (JS truthy/falsy) - uses QuickJS JS_ToBool
    pub inline fn toBool(ctx: *JSContext, val: JSValueWasm32) c_int {
        return quickjs.JS_ToBool(ctx, val);
    }

    /// Convert to int32 - uses QuickJS JS_ToInt32
    pub inline fn toInt32(ctx: *JSContext, pres: *i32, val: JSValueWasm32) c_int {
        return quickjs.JS_ToInt32(ctx, pres, val);
    }

    // ============================================================
    // Additional methods needed by frozen codegen
    // ============================================================

    /// Call constructor (takes pointer to array, derives argc from array length)
    pub inline fn callConstructor(ctx: *JSContext, func: JSValueWasm32, argv: []const JSValueWasm32) JSValueWasm32 {
        return quickjs.JS_CallConstructor(ctx, func, @intCast(argv.len), argv.ptr);
    }

    /// Define property with string key
    pub inline fn definePropertyStr(ctx: *JSContext, obj: JSValueWasm32, prop: [*:0]const u8, val: JSValueWasm32) c_int {
        return quickjs.JS_DefinePropertyValueStr(ctx, obj, prop, val, quickjs.JS_PROP_C_W_E);
    }

    /// Delete property
    pub inline fn deleteProperty(ctx: *JSContext, obj: JSValueWasm32, prop: [*:0]const u8) c_int {
        const atom = quickjs.JS_NewAtom(ctx, prop);
        const ret = quickjs.JS_DeleteProperty(ctx, obj, atom, 0);
        quickjs.JS_FreeAtom(ctx, atom);
        return ret;
    }

    /// Get global variable, return undefined if not found
    pub inline fn getGlobalUndef(ctx: *JSContext, name: [*:0]const u8) JSValueWasm32 {
        const global = quickjs.JS_GetGlobalObject(ctx);
        const val = quickjs.JS_GetPropertyStr(ctx, global, name);
        quickjs.JS_FreeValue(ctx, global);
        return val;
    }

    /// Get property by index
    pub inline fn getIndex(ctx: *JSContext, obj: JSValueWasm32, idx: u32) JSValueWasm32 {
        return quickjs.JS_GetPropertyUint32(ctx, obj, idx);
    }

    /// Get iterator from object
    pub inline fn getIterator(ctx: *JSContext, obj: JSValueWasm32) JSValueWasm32 {
        return quickjs.JS_GetIterator(ctx, obj, 0);
    }

    /// Check if value is instance of constructor
    pub inline fn isInstanceOf(ctx: *JSContext, val: JSValueWasm32, ctor: JSValueWasm32) c_int {
        return quickjs.JS_IsInstanceOf(ctx, val, ctor);
    }

    /// Close iterator
    pub inline fn iteratorClose(ctx: *JSContext, iter: JSValueWasm32) JSValueWasm32 {
        return quickjs.JS_IteratorClose(ctx, iter, 0);
    }

    /// Get next value from iterator
    pub inline fn iteratorNext(ctx: *JSContext, iter: JSValueWasm32, done: *c_int) JSValueWasm32 {
        return quickjs.JS_IteratorNext(ctx, iter, done);
    }

    /// Set field by name (alias for setPropertyStr)
    pub inline fn setField(ctx: *JSContext, obj: JSValueWasm32, name: [*:0]const u8, val: JSValueWasm32) c_int {
        return quickjs.JS_SetPropertyStr(ctx, obj, name, val);
    }

    /// Throw a value
    pub inline fn throw(ctx: *JSContext, val: JSValueWasm32) JSValueWasm32 {
        return quickjs.JS_Throw(ctx, val);
    }

    /// Convert to object
    pub inline fn toObject(ctx: *JSContext, val: JSValueWasm32) JSValueWasm32 {
        return quickjs.JS_ToObject(ctx, val);
    }

    /// Convert to property key
    pub inline fn toPropKey(ctx: *JSContext, val: JSValueWasm32) JSValueWasm32 {
        return quickjs.js_frozen_to_prop_key(ctx, val);
    }

    /// Alias for setPropertyUint32 (used by generated code)
    pub inline fn setIndex(ctx: *JSContext, this: JSValueWasm32, idx: u32, val: JSValueWasm32) c_int {
        return quickjs.JS_SetPropertyUint32(ctx, this, idx, val);
    }
};

// Native 64-bit: 16-byte struct
const JSValueNative = extern struct {
    u: JSValueUnion,
    tag: i64,

    // Constants
    pub const UNDEFINED: JSValueNative = .{ .u = .{ .int32 = 0 }, .tag = JS_TAG_UNDEFINED };
    pub const NULL: JSValueNative = .{ .u = .{ .int32 = 0 }, .tag = JS_TAG_NULL };
    pub const TRUE: JSValueNative = .{ .u = .{ .int32 = 1 }, .tag = JS_TAG_BOOL };
    pub const FALSE: JSValueNative = .{ .u = .{ .int32 = 0 }, .tag = JS_TAG_BOOL };
    pub const EXCEPTION: JSValueNative = .{ .u = .{ .int32 = 0 }, .tag = JS_TAG_EXCEPTION };
    pub const UNINITIALIZED: JSValueNative = .{ .u = .{ .int32 = 0 }, .tag = JS_TAG_UNINITIALIZED };

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

    pub inline fn newArray(ctx: *JSContext) JSValueNative {
        return quickjs.JS_NewArray(ctx);
    }

    pub inline fn newObject(ctx: *JSContext) JSValueNative {
        return quickjs.JS_NewObject(ctx);
    }

    pub inline fn newString(ctx: *JSContext, str: [*:0]const u8) JSValueNative {
        return quickjs.JS_NewString(ctx, str);
    }

    pub inline fn newStringLen(ctx: *JSContext, str: [*]const u8, len: usize) JSValueNative {
        return quickjs.JS_NewStringLen(ctx, str, len);
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
    pub fn strictEq(ctx: *JSContext, a: JSValueNative, b: JSValueNative) bool {
        return quickjs.JS_IsStrictEqual(ctx, a, b);
    }

    /// Get typeof result as JSValue string
    pub fn typeOf(ctx: *JSContext, val: JSValueNative) JSValueNative {
        const type_str: [*:0]const u8 = switch (val.tag) {
            JS_TAG_UNDEFINED => "undefined",
            JS_TAG_NULL => "object",
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
            else => "undefined",
        };
        return quickjs.JS_NewString(ctx, type_str);
    }

    pub inline fn getPropertyUint32(ctx: *JSContext, this: JSValueNative, idx: u32) JSValueNative {
        return quickjs.JS_GetPropertyUint32(ctx, this, idx);
    }

    pub inline fn setPropertyUint32(ctx: *JSContext, this: JSValueNative, idx: u32, val: JSValueNative) c_int {
        return quickjs.JS_SetPropertyUint32(ctx, this, idx, val);
    }

    /// Alias for setPropertyUint32 (used by generated code)
    pub inline fn setIndex(ctx: *JSContext, this: JSValueNative, idx: u32, val: JSValueNative) c_int {
        return quickjs.JS_SetPropertyUint32(ctx, this, idx, val);
    }

    /// Define property on Uint32 index
    pub fn definePropertyUint32(ctx: *JSContext, obj: JSValueNative, idx: u32, val: JSValueNative) c_int {
        return quickjs.JS_SetPropertyUint32(ctx, obj, idx, val);
    }

    /// Dynamic property access (prop can be string or number)
    /// Note: prop is consumed (ownership transferred)
    pub inline fn getPropertyValue(ctx: *JSContext, obj: JSValueNative, prop: JSValueNative) JSValueNative {
        // Fast path for integer indices (like original JS_GetPropertyValue)
        if (prop.isInt()) {
            const idx = prop.getInt();
            if (idx >= 0) {
                // No need to free - integers don't have refcount
                return quickjs.JS_GetPropertyUint32(ctx, obj, @intCast(idx));
            }
        }
        // Slow path: JS_ValueToAtom + JS_GetProperty + JS_FreeAtom
        // Note: JS_ValueToAtom takes ownership of prop for strings, so we free it after
        const atom = quickjs.JS_ValueToAtom(ctx, prop);
        // JS_ValueToAtom returns a new atom, doesn't consume prop - we must free it
        quickjs.JS_FreeValue(ctx, prop);
        if (atom == 0) return EXCEPTION; // JS_ATOM_NULL means error
        const result = quickjs.JS_GetProperty(ctx, obj, atom);
        quickjs.JS_FreeAtom(ctx, atom);
        return result;
    }

    /// Get global variable by name
    pub inline fn getGlobal(ctx: *JSContext, name: [*:0]const u8) JSValueNative {
        const global = quickjs.JS_GetGlobalObject(ctx);
        const val = quickjs.JS_GetPropertyStr(ctx, global, name);
        quickjs.JS_FreeValue(ctx, global);
        return val;
    }

    /// Convert to bool (JS truthy/falsy) - uses QuickJS JS_ToBool
    pub inline fn toBool(ctx: *JSContext, val: JSValueNative) c_int {
        return quickjs.JS_ToBool(ctx, val);
    }

    /// Convert to int32 - uses QuickJS JS_ToInt32
    pub inline fn toInt32(ctx: *JSContext, pres: *i32, val: JSValueNative) c_int {
        return quickjs.JS_ToInt32(ctx, pres, val);
    }

    // ============================================================
    // Additional methods needed by frozen codegen
    // ============================================================

    /// Call constructor (takes pointer to array, derives argc from array length)
    pub inline fn callConstructor(ctx: *JSContext, func: JSValueNative, argv: []const JSValueNative) JSValueNative {
        return quickjs.JS_CallConstructor(ctx, func, @intCast(argv.len), argv.ptr);
    }

    /// Define property with string key
    pub inline fn definePropertyStr(ctx: *JSContext, obj: JSValueNative, prop: [*:0]const u8, val: JSValueNative) c_int {
        return quickjs.JS_DefinePropertyValueStr(ctx, obj, prop, val, quickjs.JS_PROP_C_W_E);
    }

    /// Delete property by JSValue key (converts to property key)
    pub inline fn deleteProperty(ctx: *JSContext, obj: JSValueNative, prop: JSValueNative) c_int {
        const key = quickjs.js_frozen_to_prop_key(ctx, prop);
        const key_str = quickjs.JS_ToCString(ctx, key);
        defer if (key_str) |s| quickjs.JS_FreeCString(ctx, s);
        defer quickjs.JS_FreeValue(ctx, key);
        if (key_str) |s| {
            const atom = quickjs.JS_NewAtom(ctx, s);
            defer quickjs.JS_FreeAtom(ctx, atom);
            return quickjs.JS_DeleteProperty(ctx, obj, atom, 0);
        }
        return -1;
    }

    /// Get global variable, return undefined if not found
    pub inline fn getGlobalUndef(ctx: *JSContext, name: [*:0]const u8) JSValueNative {
        const global = quickjs.JS_GetGlobalObject(ctx);
        const val = quickjs.JS_GetPropertyStr(ctx, global, name);
        quickjs.JS_FreeValue(ctx, global);
        return val;
    }

    /// Get property by index
    pub inline fn getIndex(ctx: *JSContext, obj: JSValueNative, idx: u32) JSValueNative {
        return quickjs.JS_GetPropertyUint32(ctx, obj, idx);
    }

    /// Get iterator from object
    pub inline fn getIterator(ctx: *JSContext, obj: JSValueNative, is_async: c_int) JSValueNative {
        return quickjs.js_frozen_get_iterator(ctx, obj, is_async);
    }

    /// Check if value is instance of constructor
    pub inline fn isInstanceOf(ctx: *JSContext, val: JSValueNative, ctor: JSValueNative) bool {
        return quickjs.JS_IsInstanceOf(ctx, val, ctor) != 0;
    }

    /// Close iterator
    pub inline fn iteratorClose(ctx: *JSContext, iter: JSValueNative, completion_type: c_int) JSValueNative {
        _ = quickjs.js_frozen_iterator_close(ctx, iter, completion_type);
        return UNDEFINED;
    }

    /// Get next value from iterator
    pub inline fn iteratorNext(ctx: *JSContext, iter: JSValueNative, done: *c_int) JSValueNative {
        return quickjs.js_frozen_iterator_next(ctx, iter, done);
    }

    /// Set field by name (alias for setPropertyStr)
    pub inline fn setField(ctx: *JSContext, obj: JSValueNative, name: [*:0]const u8, val: JSValueNative) c_int {
        return quickjs.JS_SetPropertyStr(ctx, obj, name, val);
    }

    /// Throw a value
    pub inline fn throw(ctx: *JSContext, val: JSValueNative) JSValueNative {
        return quickjs.JS_Throw(ctx, val);
    }

    /// Convert to object
    pub inline fn toObject(ctx: *JSContext, val: JSValueNative) JSValueNative {
        return quickjs.JS_ToObject(ctx, val);
    }

    /// Convert to property key
    pub inline fn toPropKey(ctx: *JSContext, val: JSValueNative) JSValueNative {
        return quickjs.js_frozen_to_prop_key(ctx, val);
    }
};

// ============================================================================
// JSVarRef - QuickJS closure variable reference
// ============================================================================

/// JSVarRef - QuickJS closure variable reference
/// We only need access to pvalue (pointer to JSValue) for reading closure variables
pub const JSVarRef = extern struct {
    header: u64, // GC header (8 bytes) - contains ref_count and gc flags
    pvalue: *JSValue, // Pointer to the closure variable value
    value: JSValue, // Used when variable is no longer on stack
};

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
    pub extern fn JS_ValueToAtom(ctx: *JSContext, val: JSValue) u32;
    pub extern fn JS_HasProperty(ctx: *JSContext, obj: JSValue, atom: u32) c_int;

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
        return BorrowedString{ .ptr = ptr.?, .len = len, .ctx = ctx };
    }

    // Object creation
    pub extern fn JS_NewObject(ctx: *JSContext) JSValue;
    pub extern fn JS_NewArray(ctx: *JSContext) JSValue;
    pub extern fn JS_NewObjectProtoClass(ctx: *JSContext, proto: JSValue, class_id: u32) JSValue;
    pub extern fn JS_GetPrototype(ctx: *JSContext, val: JSValue) JSValue;
    pub const JS_CLASS_OBJECT: u32 = 1;
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

    // Property deletion (prop is JSAtom which is u32)
    pub extern fn JS_DeleteProperty(ctx: *JSContext, obj: JSValue, prop: u32, flags: c_int) c_int;

    // Iterator protocol (frozen functions)
    pub extern fn js_frozen_for_in_start(ctx: *JSContext, sp: [*]JSValue) c_int;
    pub extern fn js_frozen_for_in_next(ctx: *JSContext, sp: [*]JSValue) c_int;
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

    // Closure creation
    pub extern fn js_frozen_create_closure(ctx: *JSContext, bfunc: JSValue, cur_var_refs: ?[*]*JSVarRef, locals: ?[*]JSValue, num_locals: c_int, args: ?[*]JSValue, num_args: c_int) JSValue;
    pub extern fn JS_GetFunctionConstantPool(ctx: *JSContext, func_obj: JSValue, pcount: ?*c_int) ?[*]JSValue;

    // Runtime access
    pub extern fn JS_GetRuntime(ctx: *JSContext) *JSRuntime;
    pub extern fn JS_NewContext(rt: *JSRuntime) ?*JSContext;
    pub extern fn JS_FreeContext(ctx: *JSContext) void;
};

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
