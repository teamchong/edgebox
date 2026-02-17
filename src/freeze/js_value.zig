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
const profile = @import("profile.zig");

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

// Global return slots for WASM32 to avoid LLVM u64 return corruption
// Writing to stable memory locations before returning helps avoid
// the LLVM FastISel bug that corrupts u64 return values
// Exported so native_dispatch can read from it after frozen function calls
pub var g_return_slot: JSValue = undefined;
// Split return slot - two u32 words to avoid any u64 operations when reading
// Accessed via frozen_dispatch_get_return_lo/hi functions from C
pub var g_return_slot_lo: u32 = 0;
pub var g_return_slot_hi: u32 = 0;
// CV return slot as aligned bytes - written by newFloatToGlobal, read as CompressedValue
var g_cv_return_bytes: [8]u8 align(4) = undefined;

/// Compressed JSValue for frozen functions (8 bytes via NaN-boxing)
/// Encoding: f64 where NaN bits encode type and payload
/// Using [2]u32 on WASM32 to completely avoid u64 operations
pub const CompressedValue = if (is_wasm32) extern struct {
    /// Store as two u32s to avoid any u64 operations on WASM32
    lo: u32,
    hi: u32,

    // Helper to get bits as u64 (for comparisons with native code)
    pub inline fn getBits(self: CompressedValue) u64 {
        return @as(u64, self.hi) << 32 | @as(u64, self.lo);
    }

    // NaN-boxing constants as u32 pairs (little-endian: lo, hi)
    // QNAN = 0x7FF8_0000_0000_0000 -> lo=0x00000000, hi=0x7FF80000
    // TAG_INT = 0x0001_0000_0000_0000 -> adds 0x00010000 to hi
    // So QNAN | TAG_INT = hi=0x7FF90000, lo=0x00000000
    const QNAN_HI: u32 = 0x7FF80000;
    const TAG_INT_HI: u32 = 0x00010000;
    const TAG_BOOL_HI: u32 = 0x00020000;
    const TAG_NULL_HI: u32 = 0x00030000;
    const TAG_UNDEF_HI: u32 = 0x00040000;
    const TAG_PTR_HI: u32 = 0x00050000;
    const TAG_UNINIT_HI: u32 = 0x00060000;
    const TAG_STR_HI: u32 = 0x00070000;
    const TAG_MASK_HI: u32 = 0xFFFF0000;

    // Legacy u64 constants for compatibility (used by native packed struct)
    const QNAN: u64 = 0x7FF8000000000000;
    const TAG_MASK: u64 = 0xFFFF000000000000;
    const PAYLOAD_MASK: u64 = 0x0000FFFFFFFFFFFF;
    const TAG_INT: u64 = 0x0001000000000000;
    const TAG_BOOL: u64 = 0x0002000000000000;
    const TAG_NULL: u64 = 0x0003000000000000;
    const TAG_UNDEF: u64 = 0x0004000000000000;
    const TAG_PTR: u64 = 0x0005000000000000;
    const TAG_UNINIT: u64 = 0x0006000000000000;
    const TAG_STR: u64 = 0x0007000000000000;
    const TAG_EXCEPTION: u64 = 0x0003000000000001;

    // Sub-type bits for TAG_PTR to distinguish Object/Symbol/BigInt/Func
    // Uses bits 44-47 of the payload (above typical heap addresses)
    const PTR_SUBTYPE_MASK: u64 = 0x0000F00000000000;
    const PTR_SUBTYPE_OBJECT: u64 = 0x0000000000000000; // JS_TAG_OBJECT = -1
    const PTR_SUBTYPE_SYMBOL: u64 = 0x0000100000000000; // JS_TAG_SYMBOL = -8
    const PTR_SUBTYPE_BIGINT: u64 = 0x0000200000000000; // JS_TAG_BIG_INT = -9
    const PTR_SUBTYPE_FUNCBC: u64 = 0x0000300000000000; // JS_TAG_FUNCTION_BYTECODE = -10
    const PTR_ADDR_MASK: u64 = 0x00000FFFFFFFFFFF; // Lower 44 bits for pointer

    pub const UNDEFINED: CompressedValue = .{ .lo = 0, .hi = QNAN_HI | TAG_UNDEF_HI };
    pub const NULL: CompressedValue = .{ .lo = 0, .hi = QNAN_HI | TAG_NULL_HI };
    pub const TRUE: CompressedValue = .{ .lo = 1, .hi = QNAN_HI | TAG_BOOL_HI };
    pub const FALSE: CompressedValue = .{ .lo = 0, .hi = QNAN_HI | TAG_BOOL_HI };
    pub const UNINITIALIZED: CompressedValue = .{ .lo = 0, .hi = QNAN_HI | TAG_UNINIT_HI };
    pub const EXCEPTION: CompressedValue = .{ .lo = 1, .hi = QNAN_HI | TAG_NULL_HI }; // NULL + 1 in lo

    pub inline fn isFloat(self: CompressedValue) bool {
        // NaN-boxing scheme: tagged values use QNaN space (exponent 0x7FF with quiet bit set)
        // Normal floats have exponent != 0x7FF
        // +/-Infinity has exponent 0x7FF but mantissa = 0 (quiet bit NOT set)
        // Actual IEEE 754 NaN has exponent 0x7FF, quiet bit set, no tag bits
        const exponent_bits = self.hi & 0x7FF00000;
        if (exponent_bits != 0x7FF00000) return true; // Normal float

        // Exponent is all 1s - could be Infinity, NaN, or NaN-boxed tag
        // Infinity: mantissa bits all zero (hi & 0x000FFFFF == 0 and lo == 0)
        const mantissa_hi = self.hi & 0x000FFFFF;
        if (mantissa_hi == 0 and self.lo == 0) return true; // +/- Infinity

        // Check if it's an actual IEEE 754 NaN (QNAN with no tag bits)
        // Our tags: hi & 0xFFFF0000 in {0x7FF90000, 0x7FFA0000, ..., 0x7FFF0000}
        // Actual NaN: hi & 0xFFFF0000 == 0x7FF80000
        const tag_bits = self.hi & 0xFFFF0000;
        return tag_bits == 0x7FF80000; // Actual NaN (not our tagged value)
    }

    pub inline fn isInt(self: CompressedValue) bool {
        return (self.hi & TAG_MASK_HI) == (QNAN_HI | TAG_INT_HI);
    }

    pub inline fn isUninitialized(self: CompressedValue) bool {
        return self.lo == UNINITIALIZED.lo and self.hi == UNINITIALIZED.hi;
    }

    pub inline fn isUndefined(self: CompressedValue) bool {
        return self.lo == UNDEFINED.lo and self.hi == UNDEFINED.hi;
    }

    pub inline fn isNull(self: CompressedValue) bool {
        return self.lo == NULL.lo and self.hi == NULL.hi;
    }

    pub inline fn isException(self: CompressedValue) bool {
        return self.lo == EXCEPTION.lo and self.hi == EXCEPTION.hi;
    }

    pub inline fn getFloat(self: CompressedValue) f64 {
        // Inline to avoid LLVM WASM32 parameter passing issues with 8-byte structs
        var result: f64 = undefined;
        const src_bytes: *const [8]u8 = @ptrCast(&self);
        const dst_bytes: *[8]u8 = @ptrCast(&result);
        @memcpy(dst_bytes, src_bytes);
        return result;
    }

    pub inline fn getInt(self: CompressedValue) i32 {
        return @bitCast(self.lo);
    }

    /// Write float to global slot - returns void to avoid u64 return corruption
    pub noinline fn newFloatToGlobal(val: f64) void {
        const src_bytes: *const [8]u8 = @ptrCast(&val);
        const dst_bytes: *volatile [8]u8 = @ptrCast(&g_cv_return_bytes);
        @memcpy(dst_bytes, src_bytes);
    }

    /// Inline wrapper for newFloat - calls void function and reads from global
    pub inline fn newFloat(val: f64) CompressedValue {
        newFloatToGlobal(val);
        return @as(*const CompressedValue, @ptrCast(&g_cv_return_bytes)).*;
    }

    pub inline fn newInt(val: i32) CompressedValue {
        return .{ .lo = @bitCast(val), .hi = QNAN_HI | TAG_INT_HI };
    }

    pub inline fn toInt32(self: CompressedValue) i32 {
        return self.getInt();
    }

    pub inline fn isRefType(self: CompressedValue) bool {
        const tag_hi = self.hi & TAG_MASK_HI;
        return tag_hi == (QNAN_HI | TAG_PTR_HI) or tag_hi == (QNAN_HI | TAG_STR_HI);
    }

    pub inline fn isPtr(self: CompressedValue) bool {
        return (self.hi & TAG_MASK_HI) == (QNAN_HI | TAG_PTR_HI);
    }

    pub inline fn isStr(self: CompressedValue) bool {
        return (self.hi & TAG_MASK_HI) == (QNAN_HI | TAG_STR_HI);
    }

    /// Check if this value is an object (same as isPtr in NaN-boxing)
    pub inline fn isObject(self: CompressedValue) bool {
        return self.isPtr();
    }

    pub inline fn toBool(self: CompressedValue) bool {
        if ((self.lo == FALSE.lo and self.hi == FALSE.hi) or
            (self.lo == NULL.lo and self.hi == NULL.hi) or
            (self.lo == UNDEFINED.lo and self.hi == UNDEFINED.hi)) return false;
        if (self.lo == TRUE.lo and self.hi == TRUE.hi) return true;
        if (self.isInt()) return self.getInt() != 0;
        if (self.isFloat()) return self.getFloat() != 0.0;
        return true;
    }

    /// Context-aware toBool that properly handles strings (empty string is falsy)
    pub inline fn toBoolWithCtx(self: CompressedValue, ctx: *JSContext) bool {
        // Fast path for common primitives
        if ((self.lo == FALSE.lo and self.hi == FALSE.hi) or
            (self.lo == NULL.lo and self.hi == NULL.hi) or
            (self.lo == UNDEFINED.lo and self.hi == UNDEFINED.hi)) return false;
        if (self.lo == TRUE.lo and self.hi == TRUE.hi) return true;
        if (self.isInt()) return self.getInt() != 0;
        if (self.isFloat()) {
            const f = self.getFloat();
            return f != 0.0 and !std.math.isNan(f);
        }
        // For reference types (strings, objects, etc.), use QuickJS
        // This properly handles empty string "" which is falsy
        const jsval = self.toJSValueWithCtx(ctx);
        return quickjs.JS_ToBool(ctx, jsval) != 0;
    }

    pub inline fn compressPtr(ptr: ?*anyopaque, extra_tag: u64) CompressedValue {
        const addr = @intFromPtr(ptr);
        const lo: u32 = @truncate(addr);
        // Extra tag goes into lower 16 bits of hi (above the ptr's upper bits)
        const extra_hi: u32 = @truncate(extra_tag >> 32);
        return .{ .lo = lo, .hi = QNAN_HI | TAG_PTR_HI | extra_hi };
    }

    pub inline fn compressPtrWithTag(ptr: ?*anyopaque, tag: u64) CompressedValue {
        const addr = @intFromPtr(ptr);
        const lo: u32 = @truncate(addr);
        const tag_hi: u32 = @truncate(tag >> 32);
        return .{ .lo = lo, .hi = QNAN_HI | tag_hi };
    }

    pub inline fn decompressPtr(self: CompressedValue) ?*anyopaque {
        // On WASM32, pointers fit in 32 bits, so lo contains the full pointer
        return @ptrFromInt(self.lo);
    }

    /// Get the PTR subtype bits (for Symbol/BigInt/Object discrimination)
    /// On WASM32, the subtype is stored in hi word bits
    pub inline fn getPtrSubtype(self: CompressedValue) u64 {
        // Extract the subtype from hi word and reconstruct the 64-bit pattern
        const subtype_hi: u32 = self.hi & 0x0000F000; // Extract bits 12-15 of hi
        return @as(u64, subtype_hi) << 32;
    }

    pub inline fn toJSValue(self: CompressedValue) JSValue {
        toJSValueToGlobal(&self);
        // Explicitly read lo and hi separately to avoid LLVM FastISel issues
        const words: *volatile [2]u32 = @ptrCast(&g_return_slot);
        var result: JSValue = undefined;
        const result_words: *[2]u32 = @ptrCast(&result);
        result_words[0] = words[0];
        result_words[1] = words[1];
        return result;
    }

    /// Convert CompressedValue to JSValue - uses global slot to avoid LLVM return bug
    pub inline fn toJSValueWithCtx(self: CompressedValue, ctx: *JSContext) JSValue {
        _ = ctx;
        toJSValueToGlobal(&self);
        return g_return_slot;
    }

    /// Write JSValue to global slot - returns void to avoid u64 return corruption
    /// Also writes to split globals (g_return_slot_lo/hi) for safe reading on WASM32
    pub noinline fn toJSValueToGlobal(self_ptr: *const CompressedValue) void {
        // Read lo and hi directly via pointer - avoid dereferencing entire struct
        const words: *const [2]u32 = @ptrCast(@alignCast(self_ptr));
        const lo = words[0];
        const hi = words[1];

        // Target is global return slot
        const out_words: *volatile [2]u32 = @ptrCast(&g_return_slot);

        const is_nan = (hi & 0x7FF00000) == 0x7FF00000;

        if (!is_nan) {
            // It's a float - SUBTRACT NaN-boxing addend for QuickJS encoding
            // QuickJS: __JS_NewFloat64 does: encoded = raw - addend
            // QuickJS: JS_VALUE_GET_FLOAT64 does: raw = encoded + addend
            // JS_FLOAT64_TAG_ADDEND = 0x7ff80000 - JS_TAG_FIRST + 1 = 0x7ff8000A
            const JS_FLOAT64_TAG_ADDEND: u32 = 0x7ff8000A;
            const encoded_hi = hi -% JS_FLOAT64_TAG_ADDEND;
            out_words[0] = lo;
            out_words[1] = encoded_hi;
            // Also write to split globals for WASM32 safe reading
            g_return_slot_lo = lo;
            g_return_slot_hi = encoded_hi;
            return;
        }

        const tag_bits = hi & TAG_MASK_HI;

        // Integer: QNAN_HI | TAG_INT_HI = 0x7FF90000
        if (tag_bits == (QNAN_HI | TAG_INT_HI)) {
            // QuickJS integer: tag=0 (JS_TAG_INT), payload=value
            out_words[0] = lo;
            out_words[1] = 0; // JS_TAG_INT = 0
            // Also write to split globals
            g_return_slot_lo = lo;
            g_return_slot_hi = 0;
            return;
        }

        // Object/Function/Symbol/BigInt pointer: QNAN_HI | TAG_PTR_HI = 0x7FFD0000
        if (tag_bits == (QNAN_HI | TAG_PTR_HI)) {
            // Check subtype bits (bits 12-15 of hi) to determine correct JS tag
            const subtype_hi = hi & 0x0000F000;
            const js_tag: u32 = if (subtype_hi == 0x1000)
                @bitCast(@as(i32, -8)) // JS_TAG_SYMBOL
            else if (subtype_hi == 0x2000)
                @bitCast(@as(i32, -9)) // JS_TAG_BIG_INT
            else if (subtype_hi == 0x3000)
                @bitCast(@as(i32, -10)) // JS_TAG_FUNCTION_BYTECODE
            else
                0xFFFFFFFF; // JS_TAG_OBJECT = -1
            out_words[0] = lo;
            out_words[1] = js_tag;
            g_return_slot_lo = lo;
            g_return_slot_hi = js_tag;
            return;
        }

        // String pointer: QNAN_HI | TAG_STR_HI = 0x7FFF0000
        if (tag_bits == (QNAN_HI | TAG_STR_HI)) {
            // QuickJS string: tag=-7 (JS_TAG_STRING), payload=ptr
            const str_tag: u32 = @bitCast(@as(i32, -7));
            out_words[0] = lo;
            out_words[1] = str_tag;
            g_return_slot_lo = lo;
            g_return_slot_hi = str_tag;
            return;
        }

        // Special values - write correct QuickJS representation
        if (hi == (QNAN_HI | TAG_UNDEF_HI) and lo == 0) {
            const undef_tag: u32 = @bitCast(@as(i32, 3));
            out_words[0] = 0;
            out_words[1] = undef_tag;
            g_return_slot_lo = 0;
            g_return_slot_hi = undef_tag;
            return;
        }
        if (hi == (QNAN_HI | TAG_NULL_HI) and lo == 0) {
            const null_tag: u32 = @bitCast(@as(i32, 2));
            out_words[0] = 0;
            out_words[1] = null_tag;
            g_return_slot_lo = 0;
            g_return_slot_hi = null_tag;
            return;
        }
        if (hi == (QNAN_HI | TAG_BOOL_HI) and lo == 1) {
            const bool_tag: u32 = @bitCast(@as(i32, 1));
            out_words[0] = 1;
            out_words[1] = bool_tag;
            g_return_slot_lo = 1;
            g_return_slot_hi = bool_tag;
            return;
        }
        if (hi == (QNAN_HI | TAG_BOOL_HI) and lo == 0) {
            const bool_tag: u32 = @bitCast(@as(i32, 1));
            out_words[0] = 0;
            out_words[1] = bool_tag;
            g_return_slot_lo = 0;
            g_return_slot_hi = bool_tag;
            return;
        }

        // Unknown tagged value - write undefined
        const undef_tag: u32 = @bitCast(@as(i32, 3));
        out_words[0] = 0;
        out_words[1] = undef_tag;
        g_return_slot_lo = 0;
        g_return_slot_hi = undef_tag;
    }

    /// Inline wrapper for code generators - calls void function and reads from global
    /// Being inline means the `return g_return_slot` is just a load, not a function return
    pub inline fn toJSValuePtr(self_ptr: *const CompressedValue) JSValue {
        toJSValueToGlobal(self_ptr);
        return g_return_slot;
    }

    pub inline fn fromJSValue(val: JSValue) CompressedValue {
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
            return compressPtrWithTag(val.getPtr(), TAG_STR);
        } else if (val.isSymbol()) {
            return CompressedValue.compressPtr(val.getPtr(), PTR_SUBTYPE_SYMBOL);
        } else if (val.isBigInt()) {
            return CompressedValue.compressPtr(val.getPtr(), PTR_SUBTYPE_BIGINT);
        } else if (val.isFunctionBytecode()) {
            return CompressedValue.compressPtr(val.getPtr(), PTR_SUBTYPE_FUNCBC);
        } else if (val.isObject()) {
            return CompressedValue.compressPtr(val.getPtr(), PTR_SUBTYPE_OBJECT);
        }
        return UNDEFINED;
    }

    /// Convert JSValue from global return slots to CompressedValue (WASM32 only)
    /// Use this after any FFI call that returns JSValue to avoid LLVM FastISel corruption
    /// The globals contain JSValue in QuickJS format: hi=tag, lo=payload
    pub inline fn fromJSValueFromGlobal() CompressedValue {
        const lo = g_return_slot_lo;
        const hi = g_return_slot_hi;

        // hi contains the tag (JS_TAG_INT = 0, JS_TAG_BOOL = 1, etc.)
        const tag: i32 = @bitCast(hi);

        if (tag == types.JS_TAG_INT) {
            return newInt(@bitCast(lo));
        } else if (tag == types.JS_TAG_BOOL) {
            return if (lo != 0) TRUE else FALSE;
        } else if (tag == types.JS_TAG_UNDEFINED) {
            return UNDEFINED;
        } else if (tag == types.JS_TAG_NULL) {
            return NULL;
        } else if (tag <= types.JS_TAG_FLOAT64) {
            // Float: QuickJS encoding is raw - addend, so decode by adding
            const JS_FLOAT64_TAG_ADDEND: u32 = 0x7ff8000A;
            const decoded_hi = hi +% JS_FLOAT64_TAG_ADDEND;
            // CompressedValue for floats uses raw IEEE 754 bits
            return .{ .lo = lo, .hi = decoded_hi };
        } else {
            // Object, string, symbol, etc. - lo contains the pointer
            return compressPtr(@ptrFromInt(@as(usize, lo)), 0);
        }
    }

    // Arithmetic - use simple f64 operations (no u64)
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

    /// Noinline division that writes result to global - avoids u64 return corruption
    /// Reads operands via pointer casts to avoid 8-byte struct copies on WASM32
    pub noinline fn divToGlobal(a_ptr: *const CompressedValue, b_ptr: *const CompressedValue) void {
        // Read as [2]u32 to avoid struct copy
        const a_words: *const [2]u32 = @ptrCast(@alignCast(a_ptr));
        const b_words: *const [2]u32 = @ptrCast(@alignCast(b_ptr));
        const a_lo = a_words[0];
        const a_hi = a_words[1];
        const b_lo = b_words[0];
        const b_hi = b_words[1];

        // Constants for int detection
        const INT_TAG_HI: u32 = 0x7FF90000; // QNAN_HI | TAG_INT_HI
        const TAG_MASK_32: u32 = 0xFFFF0000;

        var fa: f64 = undefined;
        var fb: f64 = undefined;

        // Check if a is int
        if ((a_hi & TAG_MASK_32) == INT_TAG_HI) {
            const int_val: i32 = @bitCast(a_lo);
            fa = @floatFromInt(int_val);
        } else {
            // Copy f64 bytes directly
            @memcpy(@as(*[8]u8, @ptrCast(&fa)), @as(*const [8]u8, @ptrCast(a_ptr)));
        }

        // Check if b is int
        if ((b_hi & TAG_MASK_32) == INT_TAG_HI) {
            const int_val: i32 = @bitCast(b_lo);
            fb = @floatFromInt(int_val);
        } else {
            @memcpy(@as(*[8]u8, @ptrCast(&fb)), @as(*const [8]u8, @ptrCast(b_ptr)));
        }

        newFloatToGlobal(fa / fb);
    }

    /// Division - uses pointer-based API internally to avoid struct corruption
    pub inline fn div(a: CompressedValue, b: CompressedValue) CompressedValue {
        // Write operands to local vars and pass their addresses
        var a_local = a;
        var b_local = b;
        var result: CompressedValue = undefined;
        // Use the pointer-based version to avoid any struct return issues
        divWasm32Ptr(&a_local, &b_local, &result);
        return result;
    }

    pub noinline fn divWasm32Ptr(a_ptr: *const CompressedValue, b_ptr: *const CompressedValue, out: *CompressedValue) void {
        // Access via pointer cast to [2]u32 - avoid any struct field access
        const a_words: *const [2]u32 = @ptrCast(@alignCast(a_ptr));
        const b_words: *const [2]u32 = @ptrCast(@alignCast(b_ptr));
        const a_lo = a_words[0];
        const a_hi = a_words[1];
        const b_lo = b_words[0];
        const b_hi = b_words[1];

        // QNAN_HI | TAG_INT_HI = 0x7FF90000
        const QNAN_INT_HI_LOCAL: u32 = 0x7FF90000;
        const TAG_MASK_HI_LOCAL: u32 = 0xFFFF0000;

        const a_is_int = (a_hi & TAG_MASK_HI_LOCAL) == QNAN_INT_HI_LOCAL;
        const b_is_int = (b_hi & TAG_MASK_HI_LOCAL) == QNAN_INT_HI_LOCAL;

        var fa: f64 = undefined;
        var fb: f64 = undefined;

        if (a_is_int) {
            const int_val: i32 = @bitCast(a_lo);
            fa = @floatFromInt(int_val);
        } else {
            @memcpy(@as(*[8]u8, @ptrCast(&fa)), @as(*const [8]u8, @ptrCast(a_ptr)));
        }

        if (b_is_int) {
            const int_val: i32 = @bitCast(b_lo);
            fb = @floatFromInt(int_val);
        } else {
            @memcpy(@as(*[8]u8, @ptrCast(&fb)), @as(*const [8]u8, @ptrCast(b_ptr)));
        }

        const result: f64 = fa / fb;
        // Store f64 directly - cast CompressedValue* to f64* and store
        const out_f64: *f64 = @ptrCast(@alignCast(out));
        out_f64.* = result;
    }

    // Helper to read operand as f64 (writes to output to avoid f64 return issues)
    noinline fn readAsF64(ptr: *const CompressedValue, out_f: *f64) void {
        const words: *const [2]u32 = @ptrCast(@alignCast(ptr));
        const hi = words[1];
        const QNAN_INT_HI_LOCAL: u32 = 0x7FF90000;
        const TAG_MASK_HI_LOCAL: u32 = 0xFFFF0000;
        if ((hi & TAG_MASK_HI_LOCAL) == QNAN_INT_HI_LOCAL) {
            const int_val: i32 = @bitCast(words[0]);
            out_f.* = @floatFromInt(int_val);
        } else {
            @memcpy(@as(*[8]u8, @ptrCast(out_f)), @as(*const [8]u8, @ptrCast(ptr)));
        }
    }

    // Helper to write f64 result (noinline to avoid optimizer issues on WASM32)
    noinline fn writeF64(out: *CompressedValue, val_ptr: *const f64) void {
        const out_f64: *f64 = @ptrCast(@alignCast(out));
        out_f64.* = val_ptr.*;
    }

    pub noinline fn addWasm32Ptr(a_ptr: *const CompressedValue, b_ptr: *const CompressedValue, out: *CompressedValue) void {
        var fa: f64 = undefined;
        var fb: f64 = undefined;
        readAsF64(a_ptr, &fa);
        readAsF64(b_ptr, &fb);
        var result = fa + fb;
        writeF64(out, &result);
    }

    pub noinline fn subWasm32Ptr(a_ptr: *const CompressedValue, b_ptr: *const CompressedValue, out: *CompressedValue) void {
        var fa: f64 = undefined;
        var fb: f64 = undefined;
        readAsF64(a_ptr, &fa);
        readAsF64(b_ptr, &fb);
        var result = fa - fb;
        writeF64(out, &result);
    }

    pub noinline fn mulWasm32Ptr(a_ptr: *const CompressedValue, b_ptr: *const CompressedValue, out: *CompressedValue) void {
        var fa: f64 = undefined;
        var fb: f64 = undefined;
        readAsF64(a_ptr, &fa);
        readAsF64(b_ptr, &fb);
        var result = fa * fb;
        writeF64(out, &result);
    }

    pub noinline fn modWasm32Ptr(a_ptr: *const CompressedValue, b_ptr: *const CompressedValue, out: *CompressedValue) void {
        var fa: f64 = undefined;
        var fb: f64 = undefined;
        readAsF64(a_ptr, &fa);
        readAsF64(b_ptr, &fb);
        var result = @mod(fa, fb);
        writeF64(out, &result);
    }

    /// Stack-based division - uses u32 word manipulation to avoid LLVM f64 issues
    pub noinline fn divOnStack(stack: [*]CompressedValue, sp: usize) void {
        const a_idx = sp - 2;
        const b_idx = sp - 1;

        // Read a and b values as u32 pairs
        const a_words: *const [2]u32 = @ptrCast(@alignCast(&stack[a_idx]));
        const b_words: *const [2]u32 = @ptrCast(@alignCast(&stack[b_idx]));

        var fa: f64 = undefined;
        var fb: f64 = undefined;

        // Convert a to f64
        const QNAN_INT_HI_LOCAL: u32 = 0x7FF90000;
        const TAG_MASK_HI_LOCAL: u32 = 0xFFFF0000;

        if ((a_words[1] & TAG_MASK_HI_LOCAL) == QNAN_INT_HI_LOCAL) {
            const int_val: i32 = @bitCast(a_words[0]);
            fa = @floatFromInt(int_val);
        } else {
            const fa_words: *[2]u32 = @ptrCast(@alignCast(&fa));
            fa_words[0] = a_words[0];
            fa_words[1] = a_words[1];
        }

        // Convert b to f64
        if ((b_words[1] & TAG_MASK_HI_LOCAL) == QNAN_INT_HI_LOCAL) {
            const int_val: i32 = @bitCast(b_words[0]);
            fb = @floatFromInt(int_val);
        } else {
            const fb_words: *[2]u32 = @ptrCast(@alignCast(&fb));
            fb_words[0] = b_words[0];
            fb_words[1] = b_words[1];
        }

        // Perform division
        const result = fa / fb;

        // Write result back as u32 pairs
        const result_words: *const [2]u32 = @ptrCast(@alignCast(&result));
        const out_words: *[2]u32 = @ptrCast(@alignCast(&stack[a_idx]));
        out_words[0] = result_words[0];
        out_words[1] = result_words[1];
    }

    pub noinline fn addOnStack(stack: [*]CompressedValue, sp: usize) void {
        const a_ptr = &stack[sp - 2];
        const b_ptr = &stack[sp - 1];
        var fa: f64 = undefined;
        var fb: f64 = undefined;
        readAsF64(a_ptr, &fa);
        readAsF64(b_ptr, &fb);
        var result = fa + fb;
        writeF64(a_ptr, &result);
    }

    pub noinline fn subOnStack(stack: [*]CompressedValue, sp: usize) void {
        const a_ptr = &stack[sp - 2];
        const b_ptr = &stack[sp - 1];
        var fa: f64 = undefined;
        var fb: f64 = undefined;
        readAsF64(a_ptr, &fa);
        readAsF64(b_ptr, &fb);
        var result = fa - fb;
        writeF64(a_ptr, &result);
    }

    pub noinline fn mulOnStack(stack: [*]CompressedValue, sp: usize) void {
        const a_ptr = &stack[sp - 2];
        const b_ptr = &stack[sp - 1];
        var fa: f64 = undefined;
        var fb: f64 = undefined;
        readAsF64(a_ptr, &fa);
        readAsF64(b_ptr, &fb);
        var result = fa * fb;
        writeF64(a_ptr, &result);
    }

    pub noinline fn modOnStack(stack: [*]CompressedValue, sp: usize) void {
        const a_ptr = &stack[sp - 2];
        const b_ptr = &stack[sp - 1];
        var fa: f64 = undefined;
        var fb: f64 = undefined;
        readAsF64(a_ptr, &fa);
        readAsF64(b_ptr, &fb);
        var result = @mod(fa, fb);
        writeF64(a_ptr, &result);
    }

    pub inline fn mod(a: CompressedValue, b: CompressedValue) CompressedValue {
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return newFloat(@mod(fa, fb));
    }

    pub inline fn pow(a: CompressedValue, b: CompressedValue) CompressedValue {
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return newFloat(std.math.pow(f64, fa, fb));
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
        // Fast path: identical bits means equal
        if (a.lo == b.lo and a.hi == b.hi) return TRUE;

        if (a.isInt() and b.isInt()) {
            return if (a.getInt() == b.getInt()) TRUE else FALSE;
        }
        if (a.isFloat() and b.isFloat()) {
            return if (a.getFloat() == b.getFloat()) TRUE else FALSE;
        }
        if ((a.isInt() or a.isFloat()) and (b.isInt() or b.isFloat())) {
            const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
            const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
            return if (fa == fb) TRUE else FALSE;
        }
        // For non-numeric types, need context - fall through to eqWithCtx
        return FALSE;
    }

    /// Context-aware equality comparison for strings and other ref types
    pub inline fn eqWithCtx(ctx: *JSContext, a: CompressedValue, b: CompressedValue) CompressedValue {
        // Fast path: identical bits means equal
        if (a.lo == b.lo and a.hi == b.hi) return TRUE;

        // Fast path for numbers
        if (a.isInt() and b.isInt()) {
            return if (a.getInt() == b.getInt()) TRUE else FALSE;
        }
        if ((a.isInt() or a.isFloat()) and (b.isInt() or b.isFloat())) {
            const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
            const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
            return if (fa == fb) TRUE else FALSE;
        }

        // Use QuickJS for full JS equality semantics (handles strings, objects, type coercion)
        const js_a = a.toJSValueWithCtx(ctx);
        const js_b = b.toJSValueWithCtx(ctx);
        const result = quickjs.JS_IsEqual(ctx, js_a, js_b);
        return if (result > 0) TRUE else FALSE;
    }

    pub inline fn neq(a: CompressedValue, b: CompressedValue) CompressedValue {
        // Fast path: identical bits means equal
        if (a.lo == b.lo and a.hi == b.hi) return FALSE;

        if (a.isInt() and b.isInt()) {
            return if (a.getInt() != b.getInt()) TRUE else FALSE;
        }
        if ((a.isInt() or a.isFloat()) and (b.isInt() or b.isFloat())) {
            const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
            const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
            return if (fa != fb) TRUE else FALSE;
        }
        // For non-numeric types, default to not equal
        return TRUE;
    }

    /// Context-aware inequality comparison
    pub inline fn neqWithCtx(ctx: *JSContext, a: CompressedValue, b: CompressedValue) CompressedValue {
        const eq_result = eqWithCtx(ctx, a, b);
        return if (eq_result.lo == TRUE.lo and eq_result.hi == TRUE.hi) FALSE else TRUE;
    }

    pub inline fn band(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ia: i32 = if (a.isInt()) a.getInt() else floatToInt32(a.getFloat());
        const ib: i32 = if (b.isInt()) b.getInt() else floatToInt32(b.getFloat());
        return newInt(ia & ib);
    }

    pub inline fn bor(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ia: i32 = if (a.isInt()) a.getInt() else floatToInt32(a.getFloat());
        const ib: i32 = if (b.isInt()) b.getInt() else floatToInt32(b.getFloat());
        return newInt(ia | ib);
    }

    pub inline fn bxor(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ia: i32 = if (a.isInt()) a.getInt() else floatToInt32(a.getFloat());
        const ib: i32 = if (b.isInt()) b.getInt() else floatToInt32(b.getFloat());
        return newInt(ia ^ ib);
    }

    // Keep old xor name for compatibility
    pub inline fn xor(a: CompressedValue, b: CompressedValue) CompressedValue {
        return bxor(a, b);
    }

    pub inline fn shl(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ia: i32 = if (a.isInt()) a.getInt() else floatToInt32(a.getFloat());
        const shift: u5 = @truncate(if (b.isInt()) @as(u32, @bitCast(b.getInt())) else floatToUint32(b.getFloat()));
        return newInt(ia << shift);
    }

    pub inline fn shr(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ia: i32 = if (a.isInt()) a.getInt() else floatToInt32(a.getFloat());
        const shift: u5 = @truncate(if (b.isInt()) @as(u32, @bitCast(b.getInt())) else floatToUint32(b.getFloat()));
        return newInt(ia >> shift);
    }

    // Alias for shr (arithmetic shift right) - native version uses this name
    pub inline fn sar(a: CompressedValue, b: CompressedValue) CompressedValue {
        return shr(a, b);
    }

    pub inline fn ushr(a: CompressedValue, b: CompressedValue) CompressedValue {
        // JavaScript's >>> operator: ToUint32(a) >>> (ToUint32(b) & 0x1f)
        const ua: u32 = if (a.isInt()) @bitCast(a.getInt()) else floatToUint32(a.getFloat());
        const shift: u5 = @truncate(if (b.isInt()) @as(u32, @bitCast(b.getInt())) else floatToUint32(b.getFloat()));
        const result = ua >> shift;
        // Result might not fit in i32 if high bit was set
        if (result <= @as(u32, @intCast(std.math.maxInt(i32)))) {
            return newInt(@bitCast(result));
        }
        return newFloat(@floatFromInt(result));
    }

    /// JavaScript's ToInt32 for float values
    /// Handles values > 2^31 by computing modulo 2^32 then converting to signed
    inline fn floatToInt32(f: f64) i32 {
        if (std.math.isNan(f) or std.math.isInf(f) or f == 0.0) return 0;
        // Truncate towards zero and take modulo 2^32
        const truncated = @trunc(f);
        const TWO_POW_32: f64 = 4294967296.0;
        const mod_val = @mod(truncated, TWO_POW_32);
        // Convert to signed: if >= 2^31, subtract 2^32
        const uint_val: u32 = @intFromFloat(mod_val);
        return @bitCast(uint_val);
    }

    /// JavaScript's ToUint32 for float values
    /// Handles values > 2^31 by computing modulo 2^32
    inline fn floatToUint32(f: f64) u32 {
        if (std.math.isNan(f) or std.math.isInf(f) or f == 0.0) return 0;
        // Truncate towards zero and take modulo 2^32
        const truncated = @trunc(f);
        const TWO_POW_32: f64 = 4294967296.0;
        // @mod gives non-negative remainder for positive dividend
        const mod_val = @mod(truncated, TWO_POW_32);
        return @intFromFloat(mod_val);
    }

    pub inline fn neg(a: CompressedValue) CompressedValue {
        if (a.isInt()) {
            return newInt(-a.getInt());
        }
        return newFloat(-a.getFloat());
    }

    pub inline fn not(a: CompressedValue) CompressedValue {
        return if (a.toBool()) FALSE else TRUE;
    }

    pub inline fn bnot(a: CompressedValue) CompressedValue {
        const ia: i32 = if (a.isInt()) a.getInt() else floatToInt32(a.getFloat());
        return newInt(~ia);
    }

    pub inline fn bitAnd(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ia: i32 = if (a.isInt()) a.getInt() else floatToInt32(a.getFloat());
        const ib: i32 = if (b.isInt()) b.getInt() else floatToInt32(b.getFloat());
        return newInt(ia & ib);
    }

    pub inline fn bitOr(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ia: i32 = if (a.isInt()) a.getInt() else floatToInt32(a.getFloat());
        const ib: i32 = if (b.isInt()) b.getInt() else floatToInt32(b.getFloat());
        return newInt(ia | ib);
    }

    pub inline fn bitXor(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ia: i32 = if (a.isInt()) a.getInt() else floatToInt32(a.getFloat());
        const ib: i32 = if (b.isInt()) b.getInt() else floatToInt32(b.getFloat());
        return newInt(ia ^ ib);
    }

    /// Add with context - handles strings via FFI (WASM32 version)
    pub inline fn addWithCtx(ctx: *JSContext, a: CompressedValue, b: CompressedValue) CompressedValue {
        // Fast path: both integers
        if (a.isInt() and b.isInt()) {
            const ia: i64 = a.getInt();
            const ib: i64 = b.getInt();
            const r = ia + ib;
            if (r >= std.math.minInt(i32) and r <= std.math.maxInt(i32)) {
                return newInt(@intCast(r));
            }
            return newFloat(@floatFromInt(r));
        }
        // Fast path: both numeric (int or float)
        if ((a.isInt() or a.isFloat()) and (b.isInt() or b.isFloat())) {
            const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
            const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
            return newFloat(fa + fb);
        }
        // String concatenation: if either operand is a string, concat as strings
        if (a.isStr() or b.isStr()) {
            const js_a = a.toJSValueWithCtx(ctx);
            const js_b = b.toJSValueWithCtx(ctx);
            const str_a = quickjs.JS_ToString(ctx, js_a);
            const str_b = quickjs.JS_ToString(ctx, js_b);
            // Free intermediate ToString results but NOT the original inputs.
            // addWithCtx is non-consuming: callers are responsible for freeing inputs.
            defer {
                quickjs.JS_FreeValue(ctx, str_a);
                quickjs.JS_FreeValue(ctx, str_b);
            }
            var len_a: usize = 0;
            var len_b: usize = 0;
            const cstr_a = quickjs.JS_ToCStringLen2(ctx, &len_a, str_a, false);
            const cstr_b = quickjs.JS_ToCStringLen2(ctx, &len_b, str_b, false);
            if (cstr_a == null or cstr_b == null) {
                if (cstr_a) |p| quickjs.JS_FreeCString(ctx, p);
                if (cstr_b) |p| quickjs.JS_FreeCString(ctx, p);
                return UNDEFINED;
            }
            defer {
                quickjs.JS_FreeCString(ctx, cstr_a.?);
                quickjs.JS_FreeCString(ctx, cstr_b.?);
            }
            const total_len = len_a + len_b;
            const buf = quickjs.js_malloc(ctx, total_len + 1) orelse return UNDEFINED;
            const buf_ptr: [*]u8 = @ptrCast(buf);
            @memcpy(buf_ptr[0..len_a], cstr_a.?[0..len_a]);
            @memcpy(buf_ptr[len_a..total_len], cstr_b.?[0..len_b]);
            const result = quickjs.JS_NewStringLen(ctx, buf_ptr, total_len);
            quickjs.js_free(ctx, buf);
            return fromJSValue(result);
        }
        // For other types, convert to primitive and retry
        // Non-consuming: do NOT free js_a/js_b - callers handle that.
        const js_a = a.toJSValueWithCtx(ctx);
        const js_b = b.toJSValueWithCtx(ctx);
        var fa: f64 = 0;
        var fb: f64 = 0;
        if (quickjs.JS_ToFloat64(ctx, &fa, js_a) == 0 and quickjs.JS_ToFloat64(ctx, &fb, js_b) == 0) {
            return newFloat(fa + fb);
        }
        return newFloat(std.math.nan(f64));
    }
} else packed struct {
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

    // Sub-type bits for TAG_PTR to distinguish Object/Symbol/BigInt/Func
    // Uses bits 44-47 of the payload (above typical heap addresses)
    const PTR_SUBTYPE_MASK: u64 = 0x0000F00000000000;
    const PTR_SUBTYPE_OBJECT: u64 = 0x0000000000000000; // JS_TAG_OBJECT = -1
    const PTR_SUBTYPE_SYMBOL: u64 = 0x0000100000000000; // JS_TAG_SYMBOL = -8
    const PTR_SUBTYPE_BIGINT: u64 = 0x0000200000000000; // JS_TAG_BIG_INT = -9
    const PTR_SUBTYPE_FUNCBC: u64 = 0x0000300000000000; // JS_TAG_FUNCTION_BYTECODE = -10
    const PTR_ADDR_MASK: u64 = 0x00000FFFFFFFFFFF; // Lower 44 bits for pointer

    pub const UNDEFINED = CompressedValue{ .bits = QNAN | TAG_UNDEF };
    pub const NULL = CompressedValue{ .bits = QNAN | TAG_NULL };
    pub const TRUE = CompressedValue{ .bits = QNAN | TAG_BOOL | 1 };
    pub const FALSE = CompressedValue{ .bits = QNAN | TAG_BOOL | 0 };
    pub const UNINITIALIZED = CompressedValue{ .bits = QNAN | TAG_UNINIT };
    pub const EXCEPTION = CompressedValue{ .bits = QNAN | TAG_EXCEPTION };

    pub inline fn isFloat(self: CompressedValue) bool {
        if (comptime is_wasm32) {
            // Check if high word indicates NaN (if not NaN, it's a float)
            const words: *const [2]u32 = @ptrCast(@alignCast(&self));
            const hi = words[1];
            const exponent_bits = hi & 0x7FF00000;
            if (exponent_bits != 0x7FF00000) return true; // Normal float

            // Exponent is all 1s - could be Infinity, NaN, or NaN-boxed tag
            // Infinity: mantissa bits all zero (hi & 0x000FFFFF == 0 and lo == 0)
            // Actual NaN: hi & 0xFFFF0000 == 0x7FF80000 (QNAN with no tag)
            // Our tags: hi & 0xFFFF0000 in {0x7FF90000, 0x7FFA0000, ..., 0x7FFF0000}
            const mantissa_hi = hi & 0x000FFFFF;
            const lo = words[0];
            if (mantissa_hi == 0 and lo == 0) return true; // +/- Infinity

            // Check if it's an actual IEEE 754 NaN (QNAN with no tag bits)
            const tag_bits = hi & 0xFFFF0000;
            return tag_bits == 0x7FF80000; // Actual NaN (not our tagged value)
        }
        // Native path: NaN-boxing scheme uses QNaN space (exponent 0x7FF with quiet bit set)
        const exponent_bits = self.bits & 0x7FF0000000000000;
        if (exponent_bits != 0x7FF0000000000000) return true; // Normal float

        // Exponent is all 1s - could be Infinity, NaN, or NaN-boxed tag
        // Infinity: mantissa bits all zero (bits 0-51 = 0)
        const mantissa = self.bits & 0x000FFFFFFFFFFFFF;
        if (mantissa == 0) return true; // +/- Infinity

        // Check if it's an actual IEEE 754 NaN (QNAN with no tag bits)
        // Our tags have bits in the 48-51 range; actual NaN has tag_bits == 0x7FF80000
        const tag_bits = (self.bits >> 48) & 0xFFFF;
        return tag_bits == 0x7FF8; // Actual NaN (not our tagged value)
    }

    pub inline fn isInt(self: CompressedValue) bool {
        if (comptime is_wasm32) {
            // Use u32 operations to avoid LLVM FastISel bug with u64
            const words: *const [2]u32 = @ptrCast(@alignCast(&self));
            const hi = words[1];
            const QNAN_INT_HI: u32 = 0x7FF90000;
            const TAG_MASK_HI: u32 = 0xFFFF0000;
            return (hi & TAG_MASK_HI) == QNAN_INT_HI;
        }
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
        if (comptime is_wasm32) {
            // Use byte-level copy to avoid LLVM FastISel bug with @bitCast and u64
            // Take address of entire struct, not the bits field (alignment issues)
            var result: f64 = undefined;
            const src: *const [8]u8 = @ptrCast(&self);
            const dst: *[8]u8 = @ptrCast(&result);
            inline for (0..8) |i| {
                dst[i] = src[i];
            }
            return result;
        }
        return @bitCast(self.bits);
    }

    pub inline fn getInt(self: CompressedValue) i32 {
        if (comptime is_wasm32) {
            // On WASM32, read low u32 directly to avoid u64 field access
            const words: *const [2]u32 = @ptrCast(@alignCast(&self));
            return @bitCast(words[0]);
        }
        return @truncate(@as(i64, @bitCast(self.bits & PAYLOAD_MASK)));
    }

    pub inline fn newFloat(val: f64) CompressedValue {
        if (comptime is_wasm32) {
            // Use byte-level copy to avoid packed struct initialization issues on WASM32
            var result: CompressedValue = undefined;
            const src: *const [8]u8 = @ptrCast(&val);
            const dst: *[8]u8 = @ptrCast(&result);
            inline for (0..8) |i| {
                dst[i] = src[i];
            }
            return result;
        }
        return .{ .bits = @bitCast(val) };
    }

    pub inline fn newInt(val: i32) CompressedValue {
        if (comptime is_wasm32) {
            // On WASM32, avoid u64 operations due to LLVM FastISel bug
            // Build the value using two u32s and byte copy
            const QNAN_HI: u32 = 0x7FF80000;
            const TAG_INT_HI: u32 = 0x00010000; // TAG_INT in high word
            const hi: u32 = QNAN_HI | TAG_INT_HI;
            const lo: u32 = @bitCast(val);
            var result: CompressedValue = undefined;
            const result_words: *[2]u32 = @ptrCast(@alignCast(&result));
            result_words[0] = lo;
            result_words[1] = hi;
            return result;
        }
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

    /// Fast ref count increment via direct pointer arithmetic.
    /// Avoids the CV→JSValue→JS_DupValue→JSValue→CV roundtrip.
    /// Returns the same CV unchanged (ref count on underlying object is incremented).
    /// Noinline to keep frozen shard code size small (better I-cache).
    pub noinline fn dupRef(v: CompressedValue) CompressedValue {
        if (v.isRefType()) {
            if (profile.PROFILE) profile.vinc(profile.prof.ref_dups(), 1);
            const ptr = v.decompressPtr() orelse return v;
            // JSRefCountHeader.ref_count is an i32 at offset 0 of JSObject/JSString
            const ref_count: *i32 = @ptrCast(@alignCast(ptr));
            ref_count.* += 1;
        }
        return v;
    }

    /// Fast ref count decrement via direct pointer arithmetic.
    /// Fast path (ref_count > 1): pure Zig pointer decrement, no C FFI.
    /// Slow path (ref_count <= 1): falls back to JS_FreeValue for actual object destruction.
    /// Noinline to keep frozen shard code size small (better I-cache).
    pub noinline fn freeRef(ctx: *JSContext, v: CompressedValue) void {
        if (v.isRefType()) {
            if (profile.PROFILE) profile.vinc(profile.prof.ref_frees(), 1);
            const ptr = v.decompressPtr() orelse return;
            const ref_count: *i32 = @ptrCast(@alignCast(ptr));
            if (ref_count.* > 1) {
                // Fast path: just decrement, no freeing needed
                ref_count.* -= 1;
            } else {
                // Slow path: object may need destruction, delegate to C
                quickjs.JS_FreeValue(ctx, v.toJSValue());
            }
        }
    }

    pub inline fn isPtr(self: CompressedValue) bool {
        return (self.bits & TAG_MASK) == (QNAN | TAG_PTR);
    }

    pub inline fn isStr(self: CompressedValue) bool {
        return (self.bits & TAG_MASK) == (QNAN | TAG_STR);
    }

    /// Check if this value is an object (same as isPtr in NaN-boxing)
    pub inline fn isObject(self: CompressedValue) bool {
        return self.isPtr();
    }

    // Note: Symbol, BigInt, Func are stored under TAG_PTR - use isPtr() for them

    // Pointer compression/decompression
    // Note: extra_tag is used for PTR_SUBTYPE_* values to distinguish Symbol/BigInt/etc
    pub inline fn compressPtr(ptr: ?*anyopaque, extra_tag: u64) CompressedValue {
        const addr = @intFromPtr(ptr);
        if (compressed_heap_base != 0 and addr >= compressed_heap_base) {
            const offset = addr - compressed_heap_base;
            if (offset <= PTR_ADDR_MASK) {
                return .{ .bits = QNAN | TAG_PTR | extra_tag | offset };
            }
        }
        // Fallback: store low bits (works for most heap layouts)
        return .{ .bits = QNAN | TAG_PTR | extra_tag | (addr & PTR_ADDR_MASK) };
    }

    pub inline fn compressPtrWithTag(ptr: ?*anyopaque, tag: u64) CompressedValue {
        const addr = @intFromPtr(ptr);
        if (compressed_heap_base != 0 and addr >= compressed_heap_base) {
            const offset = addr - compressed_heap_base;
            if (offset <= PTR_ADDR_MASK) {
                return .{ .bits = QNAN | tag | offset };
            }
        }
        return .{ .bits = QNAN | tag | (addr & PTR_ADDR_MASK) };
    }

    pub inline fn decompressPtr(self: CompressedValue) ?*anyopaque {
        // Mask out subtype bits (44-47) when extracting pointer
        const offset: usize = @truncate(self.bits & PTR_ADDR_MASK);
        if (compressed_heap_base != 0) {
            return @ptrFromInt(compressed_heap_base + offset);
        }
        return @ptrFromInt(offset);
    }

    /// Get the PTR subtype bits (for Symbol/BigInt/Object discrimination)
    pub inline fn getPtrSubtype(self: CompressedValue) u64 {
        return self.bits & PTR_SUBTYPE_MASK;
    }

    // Convert to/from full JSValue for FFI boundary
    pub inline fn toJSValue(self: CompressedValue) JSValue {
        @setEvalBranchQuota(1000000); // Increased for large codebases (date-fns, etc.)
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
            var tag: i64 = undefined;
            if (self.isStr()) {
                tag = JS_TAG_STRING;
            } else {
                // Decode PTR subtype to get correct JS tag
                const subtype = self.getPtrSubtype();
                tag = if (subtype == PTR_SUBTYPE_SYMBOL)
                    JS_TAG_SYMBOL
                else if (subtype == PTR_SUBTYPE_BIGINT)
                    JS_TAG_BIG_INT
                else if (subtype == PTR_SUBTYPE_FUNCBC)
                    JS_TAG_FUNCTION_BYTECODE
                else
                    JS_TAG_OBJECT;
            }
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

    /// Convert CompressedValue to JSValue - native version just calls toJSValue
    /// (ctx parameter exists for API compatibility with WASM32 version)
    pub inline fn toJSValueWithCtx(self: CompressedValue, ctx: *JSContext) JSValue {
        _ = ctx; // Not needed on native, just use standard conversion
        if (profile.PROFILE) profile.vinc(profile.prof.cv_to_jsvalue(), 1);
        return self.toJSValue();
    }

    /// Pointer-based toJSValue for generated code - avoids pass-by-value corruption on wasm32
    /// noinline: inline caused +4% regression due to icache pressure across 881 shards
    pub noinline fn toJSValuePtr(self_ptr: *const CompressedValue) JSValue {
        // Read bits as two u32s
        const words: *const [2]u32 = @ptrCast(@alignCast(self_ptr));
        const lo = words[0];
        const hi = words[1];

        // Check exponent bits (upper 11 bits of hi, after sign bit)
        const exponent_bits = hi & 0x7FF00000;

        if (exponent_bits != 0x7FF00000) {
            // Normal float (exponent not all 1s) - copy bytes to f64
            var f: f64 = undefined;
            const src: *const [8]u8 = @ptrCast(self_ptr);
            const dst: *[8]u8 = @ptrCast(&f);
            inline for (0..8) |i| {
                dst[i] = src[i];
            }
            return JSValue.newFloat64(f);
        }

        // Exponent is all 1s - could be Infinity or NaN-boxed tag
        // +/- Infinity: mantissa bits all zero (hi & 0x000FFFFF == 0 and lo == 0)
        // Our tags: have QNAN quiet bit set (0x00080000) in mantissa
        const mantissa_hi = hi & 0x000FFFFF;
        if (mantissa_hi == 0 and lo == 0) {
            // It's +/- Infinity - copy bytes to f64
            var f: f64 = undefined;
            const src: *const [8]u8 = @ptrCast(self_ptr);
            const dst: *[8]u8 = @ptrCast(&f);
            inline for (0..8) |i| {
                dst[i] = src[i];
            }
            return JSValue.newFloat64(f);
        }

        // It's NaN-boxed - check tag in upper 16 bits of hi
        const tag_bits = hi & 0xFFFF0000;

        // Check if it's an actual IEEE 754 NaN (QNAN with no tag bits)
        // tag_bits == 0x7FF80000 means it's a real NaN, not our tagged value
        if (tag_bits == 0x7FF80000) {
            // It's an actual NaN - copy bytes to f64
            var f: f64 = undefined;
            const src: *const [8]u8 = @ptrCast(self_ptr);
            const dst: *[8]u8 = @ptrCast(&f);
            inline for (0..8) |i| {
                dst[i] = src[i];
            }
            return JSValue.newFloat64(f);
        }

        // TAG_INT: QNAN_HI (0x7FF80000) | TAG_INT_HI (0x00010000) = 0x7FF90000
        if (tag_bits == 0x7FF90000) {
            const int_val: i32 = @bitCast(lo);
            return JSValue.newInt(int_val);
        }

        // Check for special values - compare both words
        // UNDEFINED = QNAN | TAG_UNDEF = 0x7FFC000000000000
        // hi = 0x7FFC0000, lo = 0
        if (hi == 0x7FFC0000 and lo == 0) {
            return JSValue.UNDEFINED;
        }

        // NULL = QNAN | TAG_NULL = 0x7FFB000000000000
        // hi = 0x7FFB0000, lo = 0
        if (hi == 0x7FFB0000 and lo == 0) {
            return JSValue.NULL;
        }

        // TRUE = QNAN | TAG_BOOL | 1 = 0x7FFA000000000001
        if (hi == 0x7FFA0000 and lo == 1) {
            return JSValue.TRUE;
        }

        // FALSE = QNAN | TAG_BOOL | 0 = 0x7FFA000000000000
        if (hi == 0x7FFA0000 and lo == 0) {
            return JSValue.FALSE;
        }

        // For reference types, we need the pointer from the payload (lower 44 bits, excluding subtype)
        // Reconstruct bits as u64 and extract with PTR_ADDR_MASK
        const bits: u64 = @as(u64, hi) << 32 | @as(u64, lo);
        const ptr_offset: usize = @truncate(bits & PTR_ADDR_MASK);
        const ptr_addr: usize = if (compressed_heap_base != 0) compressed_heap_base + ptr_offset else ptr_offset;

        // Object/Function/Symbol/BigInt pointer: QNAN_HI | TAG_PTR_HI = 0x7FFD0000
        if (tag_bits == 0x7FFD0000) {
            // Check subtype bits (bits 12-15 of hi) to determine correct JS tag
            const subtype_hi = hi & 0x0000F000;
            const js_tag: i64 = if (subtype_hi == 0x1000)
                JS_TAG_SYMBOL
            else if (subtype_hi == 0x2000)
                JS_TAG_BIG_INT
            else if (subtype_hi == 0x3000)
                JS_TAG_FUNCTION_BYTECODE
            else
                JS_TAG_OBJECT;
            return .{ .u = .{ .ptr = @ptrFromInt(ptr_addr) }, .tag = js_tag };
        }

        // String pointer: QNAN_HI | TAG_STR_HI = 0x7FFF0000
        if (tag_bits == 0x7FFF0000) {
            // Reconstruct JSValue with JS_TAG_STRING tag (-7)
            return .{ .u = .{ .ptr = @ptrFromInt(ptr_addr) }, .tag = -7 };
        }

        // Unknown tagged value - return undefined as fallback
        return JSValue.UNDEFINED;
    }

    pub inline fn fromJSValue(val: JSValue) CompressedValue {
        @setEvalBranchQuota(1000000); // Increased for large codebases (date-fns, etc.)
        if (profile.PROFILE) profile.vinc(profile.prof.jsvalue_to_cv(), 1);
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
            // Compress symbol with subtype marker so it decompresses with correct tag
            return CompressedValue.compressPtr(val.getPtr(), PTR_SUBTYPE_SYMBOL);
        } else if (val.isBigInt()) {
            return CompressedValue.compressPtr(val.getPtr(), PTR_SUBTYPE_BIGINT);
        } else if (val.isFunctionBytecode()) {
            return CompressedValue.compressPtr(val.getPtr(), PTR_SUBTYPE_FUNCBC);
        } else if (val.isObject()) {
            return CompressedValue.compressPtr(val.getPtr(), PTR_SUBTYPE_OBJECT);
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
        // Float path - only if both are numeric (int or float)
        if ((a.isInt() or a.isFloat()) and (b.isInt() or b.isFloat())) {
            const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
            const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
            return newFloat(fa + fb);
        }
        // Non-numeric types require FFI - return sentinel value
        // This should not happen in well-generated code that uses addWithCtx
        return UNDEFINED;
    }

    /// Add with context - handles strings via FFI
    pub inline fn addWithCtx(ctx: *JSContext, a: CompressedValue, b: CompressedValue) CompressedValue {
        // Fast path: both integers
        if (a.isInt() and b.isInt()) {
            const ia: i64 = a.getInt();
            const ib: i64 = b.getInt();
            const r = ia + ib;
            if (r >= std.math.minInt(i32) and r <= std.math.maxInt(i32)) {
                return newInt(@intCast(r));
            }
            return newFloat(@floatFromInt(r));
        }
        // Fast path: both numeric (int or float)
        if ((a.isInt() or a.isFloat()) and (b.isInt() or b.isFloat())) {
            const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
            const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
            return newFloat(fa + fb);
        }
        // String concatenation: if either operand is a string, concat as strings
        if (a.isStr() or b.isStr()) {
            // Convert both operands to JSValue for string conversion
            const js_a = a.toJSValueWithCtx(ctx);
            const js_b = b.toJSValueWithCtx(ctx);
            // Convert both to strings
            const str_a = quickjs.JS_ToString(ctx, js_a);
            const str_b = quickjs.JS_ToString(ctx, js_b);
            // Free intermediate ToString results but NOT the original inputs.
            // addWithCtx is non-consuming: callers are responsible for freeing inputs.
            defer {
                quickjs.JS_FreeValue(ctx, str_a);
                quickjs.JS_FreeValue(ctx, str_b);
            }
            // Get C strings
            var len_a: usize = 0;
            var len_b: usize = 0;
            const cstr_a = quickjs.JS_ToCStringLen2(ctx, &len_a, str_a, false);
            const cstr_b = quickjs.JS_ToCStringLen2(ctx, &len_b, str_b, false);
            if (cstr_a == null or cstr_b == null) {
                if (cstr_a) |p| quickjs.JS_FreeCString(ctx, p);
                if (cstr_b) |p| quickjs.JS_FreeCString(ctx, p);
                return UNDEFINED;
            }
            defer {
                quickjs.JS_FreeCString(ctx, cstr_a.?);
                quickjs.JS_FreeCString(ctx, cstr_b.?);
            }
            // Allocate buffer for concatenated result
            const total_len = len_a + len_b;
            const buf = quickjs.js_malloc(ctx, total_len + 1) orelse return UNDEFINED;
            const buf_ptr: [*]u8 = @ptrCast(buf);
            // Copy strings
            @memcpy(buf_ptr[0..len_a], cstr_a.?[0..len_a]);
            @memcpy(buf_ptr[len_a..total_len], cstr_b.?[0..len_b]);
            // Create new JS string
            const result = quickjs.JS_NewStringLen(ctx, buf_ptr, total_len);
            quickjs.js_free(ctx, buf);
            return fromJSValue(result);
        }
        // For other types (objects, etc.), convert to primitive and retry
        // Non-consuming: do NOT free js_a/js_b - callers handle that.
        const js_a = a.toJSValueWithCtx(ctx);
        const js_b = b.toJSValueWithCtx(ctx);
        // Try to convert to numbers
        var fa: f64 = 0;
        var fb: f64 = 0;
        if (quickjs.JS_ToFloat64(ctx, &fa, js_a) == 0 and quickjs.JS_ToFloat64(ctx, &fb, js_b) == 0) {
            return newFloat(fa + fb);
        }
        // Last resort: return NaN
        return newFloat(std.math.nan(f64));
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

    /// WASM32-specific division using all pointers and avoiding u64 entirely
    /// On WASM32, u64 operations are broken (LLVM FastISel bug)
    /// Uses [2]u32 for all bit manipulation
    pub noinline fn divWasm32Ptr(a_ptr: *const CompressedValue, b_ptr: *const CompressedValue, out: *CompressedValue) void {
        // Read operand bits as two u32s (little-endian: lo=bits[0:4], hi=bits[4:8])
        const a_words: *const [2]u32 = @ptrCast(@alignCast(a_ptr));
        const b_words: *const [2]u32 = @ptrCast(@alignCast(b_ptr));

        const a_lo = a_words[0];
        const a_hi = a_words[1];
        const b_lo = b_words[0];
        const b_hi = b_words[1];

        // TAG_MASK high word: 0xFFFF0000 (upper 16 bits of high u32)
        // QNAN = 0x7FF8_0000_0000_0000, TAG_INT = 0x0001_0000_0000_0000
        // QNAN | TAG_INT = 0x7FF9_0000_0000_0000 -> high word = 0x7FF90000
        // Integer payload is in low 48 bits (low word + lower 16 bits of high word)
        const QNAN_INT_HI: u32 = 0x7FF90000;
        const TAG_MASK_HI: u32 = 0xFFFF0000;

        // Check only the high word's upper 16 bits for the tag
        const a_is_int = (a_hi & TAG_MASK_HI) == QNAN_INT_HI;
        const b_is_int = (b_hi & TAG_MASK_HI) == QNAN_INT_HI;

        var fa: f64 = undefined;
        var fb: f64 = undefined;

        if (a_is_int) {
            // Integer: low 32 bits contain the value (but we only use low word for small ints)
            // For NaN-boxed int, payload is in low bits. With our encoding, a_lo is the int value
            // Actually TAG_INT uses lower 48 bits for payload, but ints are stored in low 32 bits
            const int_val: i32 = @bitCast(a_lo);
            fa = @floatFromInt(int_val);
        } else {
            // Float: direct reinterpret
            const src: *const [8]u8 = @ptrCast(a_ptr);
            const dst: *[8]u8 = @ptrCast(&fa);
            inline for (0..8) |i| {
                dst[i] = src[i];
            }
        }

        if (b_is_int) {
            const int_val: i32 = @bitCast(b_lo);
            fb = @floatFromInt(int_val);
        } else {
            const src: *const [8]u8 = @ptrCast(b_ptr);
            const dst: *[8]u8 = @ptrCast(&fb);
            inline for (0..8) |i| {
                dst[i] = src[i];
            }
        }

        const result: f64 = fa / fb;
        // Write result via byte-level copy
        const src: *const [8]u8 = @ptrCast(&result);
        const dst: *[8]u8 = @ptrCast(out);
        inline for (0..8) |i| {
            dst[i] = src[i];
        }
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
        // Fast path: identical bits means equal (for primitives)
        if (a.bits == b.bits) return TRUE;

        if (a.isInt() and b.isInt()) {
            return if (a.getInt() == b.getInt()) TRUE else FALSE;
        }
        if (a.isFloat() and b.isFloat()) {
            return if (a.getFloat() == b.getFloat()) TRUE else FALSE;
        }
        // Mixed int/float
        if ((a.isInt() or a.isFloat()) and (b.isInt() or b.isFloat())) {
            const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
            const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
            return if (fa == fb) TRUE else FALSE;
        }
        // For non-numeric types (strings, objects, etc.), need context - fall through to eqWithCtx
        // This should not be reached if codegen properly uses eqWithCtx for non-numeric comparisons
        return FALSE;
    }

    /// Context-aware equality comparison for strings and other ref types
    pub inline fn eqWithCtx(ctx: *JSContext, a: CompressedValue, b: CompressedValue) CompressedValue {
        // Fast path: identical bits means equal
        if (a.bits == b.bits) return TRUE;

        // Fast path for numbers
        if (a.isInt() and b.isInt()) {
            return if (a.getInt() == b.getInt()) TRUE else FALSE;
        }
        if ((a.isInt() or a.isFloat()) and (b.isInt() or b.isFloat())) {
            const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
            const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
            return if (fa == fb) TRUE else FALSE;
        }

        // Use QuickJS for full JS equality semantics (handles strings, objects, type coercion)
        const js_a = a.toJSValueWithCtx(ctx);
        const js_b = b.toJSValueWithCtx(ctx);
        const result = quickjs.JS_IsEqual(ctx, js_a, js_b);
        return if (result > 0) TRUE else FALSE;
    }

    pub inline fn neq(a: CompressedValue, b: CompressedValue) CompressedValue {
        // Fast path: identical bits means equal
        if (a.bits == b.bits) return FALSE;

        if (a.isInt() and b.isInt()) {
            return if (a.getInt() != b.getInt()) TRUE else FALSE;
        }
        if (a.isFloat() and b.isFloat()) {
            return if (a.getFloat() != b.getFloat()) TRUE else FALSE;
        }
        if ((a.isInt() or a.isFloat()) and (b.isInt() or b.isFloat())) {
            const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
            const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
            return if (fa != fb) TRUE else FALSE;
        }
        // For non-numeric types, default to not equal (proper handling needs context)
        return TRUE;
    }

    /// Context-aware inequality comparison
    pub inline fn neqWithCtx(ctx: *JSContext, a: CompressedValue, b: CompressedValue) CompressedValue {
        const eq_result = eqWithCtx(ctx, a, b);
        return if (eq_result.bits == TRUE.bits) FALSE else TRUE;
    }

    // Bitwise operations - handle both int and float inputs
    pub inline fn bitAnd(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ia: i32 = if (a.isInt()) a.getInt() else if (a.isFloat()) floatToInt32(a.getFloat()) else return UNDEFINED;
        const ib: i32 = if (b.isInt()) b.getInt() else if (b.isFloat()) floatToInt32(b.getFloat()) else return UNDEFINED;
        return newInt(ia & ib);
    }

    pub inline fn bitOr(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ia: i32 = if (a.isInt()) a.getInt() else if (a.isFloat()) floatToInt32(a.getFloat()) else return UNDEFINED;
        const ib: i32 = if (b.isInt()) b.getInt() else if (b.isFloat()) floatToInt32(b.getFloat()) else return UNDEFINED;
        return newInt(ia | ib);
    }

    // Aliases for opcode_emitter compatibility
    pub const band = bitAnd;
    pub const bor = bitOr;

    pub inline fn bitXor(a: CompressedValue, b: CompressedValue) CompressedValue {
        const ia: i32 = if (a.isInt()) a.getInt() else if (a.isFloat()) floatToInt32(a.getFloat()) else return UNDEFINED;
        const ib: i32 = if (b.isInt()) b.getInt() else if (b.isFloat()) floatToInt32(b.getFloat()) else return UNDEFINED;
        return newInt(ia ^ ib);
    }

    // Alias for bitXor
    pub const bxor = bitXor;

    pub inline fn bitNot(a: CompressedValue) CompressedValue {
        const ia: i32 = if (a.isInt()) a.getInt() else if (a.isFloat()) floatToInt32(a.getFloat()) else return UNDEFINED;
        return newInt(~ia);
    }

    // Alias for bitNot
    pub const bnot = bitNot;

    pub inline fn shl(a: CompressedValue, b: CompressedValue) CompressedValue {
        // Handle both int and float inputs - convert floats to i32 first
        const ia: i32 = if (a.isInt()) a.getInt() else if (a.isFloat()) floatToInt32(a.getFloat()) else return UNDEFINED;
        const shift: u5 = @truncate(if (b.isInt()) @as(u32, @bitCast(b.getInt())) else if (b.isFloat()) floatToUint32(b.getFloat()) else return UNDEFINED);
        return newInt(ia << shift);
    }

    pub inline fn sar(a: CompressedValue, b: CompressedValue) CompressedValue {
        // Handle both int and float inputs - convert floats to i32 first
        const ia: i32 = if (a.isInt()) a.getInt() else if (a.isFloat()) floatToInt32(a.getFloat()) else return UNDEFINED;
        const shift: u5 = @truncate(if (b.isInt()) @as(u32, @bitCast(b.getInt())) else if (b.isFloat()) floatToUint32(b.getFloat()) else return UNDEFINED);
        return newInt(ia >> shift);
    }

    pub inline fn shr(a: CompressedValue, b: CompressedValue) CompressedValue {
        // Handle both int and float inputs - convert floats to i32 first
        const ia: i32 = if (a.isInt()) a.getInt() else if (a.isFloat()) floatToInt32(a.getFloat()) else return UNDEFINED;
        const shift: u5 = @truncate(if (b.isInt()) @as(u32, @bitCast(b.getInt())) else if (b.isFloat()) floatToUint32(b.getFloat()) else return UNDEFINED);
        return newInt(ia >> shift);
    }

    pub inline fn ushr(a: CompressedValue, b: CompressedValue) CompressedValue {
        // JavaScript's >>> operator: ToUint32(a) >>> (ToUint32(b) & 0x1f)
        const ua: u32 = if (a.isInt()) @bitCast(a.getInt()) else if (a.isFloat()) floatToUint32(a.getFloat()) else return UNDEFINED;
        const shift: u5 = @truncate(if (b.isInt()) @as(u32, @bitCast(b.getInt())) else if (b.isFloat()) floatToUint32(b.getFloat()) else return UNDEFINED);
        const result = ua >> shift;
        // Result might not fit in i32 if high bit was set
        if (result <= @as(u32, @intCast(std.math.maxInt(i32)))) {
            return newInt(@bitCast(result));
        }
        return newFloat(@floatFromInt(result));
    }

    /// JavaScript's ToInt32 for float values
    /// Handles values > 2^31 by computing modulo 2^32 then converting to signed
    inline fn floatToInt32(f: f64) i32 {
        if (std.math.isNan(f) or std.math.isInf(f) or f == 0.0) return 0;
        // Truncate towards zero and take modulo 2^32
        const truncated = @trunc(f);
        const TWO_POW_32: f64 = 4294967296.0;
        const mod_val = @mod(truncated, TWO_POW_32);
        // Convert to signed: if >= 2^31, subtract 2^32
        const uint_val: u32 = @intFromFloat(mod_val);
        return @bitCast(uint_val);
    }

    /// JavaScript's ToUint32 for float values
    /// Handles values > 2^31 by computing modulo 2^32
    inline fn floatToUint32(f: f64) u32 {
        if (std.math.isNan(f) or std.math.isInf(f) or f == 0.0) return 0;
        // Truncate towards zero and take modulo 2^32
        const truncated = @trunc(f);
        const TWO_POW_32: f64 = 4294967296.0;
        // @mod gives non-negative remainder for positive dividend
        const mod_val = @mod(truncated, TWO_POW_32);
        return @intFromFloat(mod_val);
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

    pub inline fn pow(a: CompressedValue, b: CompressedValue) CompressedValue {
        const fa: f64 = if (a.isFloat()) a.getFloat() else @floatFromInt(a.getInt());
        const fb: f64 = if (b.isFloat()) b.getFloat() else @floatFromInt(b.getInt());
        return newFloat(std.math.pow(f64, fa, fb));
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

        // TRUE is truthy, reference types need context check (for empty string handling)
        return true;
    }

    /// Context-aware toBool that properly handles strings (empty string is falsy)
    pub inline fn toBoolWithCtx(self: CompressedValue, ctx: *JSContext) bool {
        // Fast path for common primitives
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

        // For reference types (strings, objects, etc.), use QuickJS
        // This properly handles empty string "" which is falsy
        const jsval = self.toJSValueWithCtx(ctx);
        return quickjs.JS_ToBool(ctx, jsval) != 0;
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
// MUST use u64 for ABI compatibility with QuickJS C code, but access via pointer casts internally
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
        // Read hi word via pointer to avoid u64 shift
        const words: *const [2]u32 = @ptrCast(&self);
        return @bitCast(words[1]);
    }

    inline fn getPayload(self: JSValueWasm32) u32 {
        // Read lo word via pointer
        const words: *const [2]u32 = @ptrCast(&self);
        return words[0];
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
        @setEvalBranchQuota(10000);
        return self.getTag() < 0;
    }

    pub inline fn getInt(self: JSValueWasm32) i32 {
        return @bitCast(self.getPayload());
    }

    pub inline fn getBool(self: JSValueWasm32) bool {
        return self.getPayload() != 0;
    }

    pub noinline fn getFloat64(self: JSValueWasm32) f64 {
        // QuickJS NaN-boxing: encoding does raw - addend, decoding does encoded + addend
        // JS_FLOAT64_TAG_ADDEND = 0x7ff80000 - JS_TAG_FIRST + 1 = 0x7ff8000A
        const JS_FLOAT64_TAG_ADDEND: u32 = 0x7ff8000A;
        var result: f64 = undefined;
        const src_words: *const [2]u32 = @ptrCast(&self);
        const dst_words: *[2]u32 = @ptrCast(&result);
        dst_words[0] = src_words[0];
        // ADD the addend to the high word to decode (wrapping)
        dst_words[1] = src_words[1] +% JS_FLOAT64_TAG_ADDEND;
        return result;
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

    pub noinline fn newInt(val: i32) JSValueWasm32 {
        // Write lo/hi separately via pointer to avoid u64 operations
        var result: JSValueWasm32 = undefined;
        const words: *[2]u32 = @ptrCast(&result);
        words[0] = @bitCast(val);
        words[1] = @bitCast(@as(i32, JS_TAG_INT));
        // Write to split globals for native_dispatch to read (avoids LLVM FastISel return bug)
        g_return_slot_lo = words[0];
        g_return_slot_hi = words[1];
        return result;
    }

    pub noinline fn newBool(val: bool) JSValueWasm32 {
        var result: JSValueWasm32 = undefined;
        const words: *[2]u32 = @ptrCast(&result);
        words[0] = if (val) 1 else 0;
        words[1] = @bitCast(@as(i32, JS_TAG_BOOL));
        // Write to split globals for native_dispatch to read (avoids LLVM FastISel return bug)
        g_return_slot_lo = words[0];
        g_return_slot_hi = words[1];
        return result;
    }

    pub noinline fn newFloat64(val: f64) JSValueWasm32 {
        // QuickJS NaN-boxing: encoding does raw - addend, decoding does encoded + addend
        // JS_FLOAT64_TAG_ADDEND = 0x7ff80000 - JS_TAG_FIRST + 1 = 0x7ff8000A
        const JS_FLOAT64_TAG_ADDEND: u32 = 0x7ff8000A;
        var result: JSValueWasm32 = undefined;
        const val_bytes: *const [2]u32 = @ptrCast(&val);
        const result_words: *[2]u32 = @ptrCast(&result);
        result_words[0] = val_bytes[0];
        // SUBTRACT the NaN-boxing addend from the high word to encode
        result_words[1] = val_bytes[1] -% JS_FLOAT64_TAG_ADDEND;
        // Write to split globals for native_dispatch to read (avoids LLVM FastISel return bug)
        g_return_slot_lo = result_words[0];
        g_return_slot_hi = result_words[1];
        return result;
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

    pub inline fn throwOutOfMemory(ctx: *JSContext) JSValueWasm32 {
        return quickjs.JS_ThrowOutOfMemory(ctx);
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

    /// Convert to float64 - uses QuickJS JS_ToFloat64
    pub inline fn toFloat64(ctx: *JSContext, pres: *f64, val: JSValueWasm32) c_int {
        return quickjs.JS_ToFloat64(ctx, pres, val);
    }

    /// Convert to string - uses QuickJS JS_ToString
    pub inline fn toString(ctx: *JSContext, val: JSValueWasm32) JSValueWasm32 {
        return quickjs.JS_ToString(ctx, val);
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

    /// Define property with string key and flags
    pub inline fn definePropertyValueStr(ctx: *JSContext, obj: JSValueWasm32, prop: [*:0]const u8, val: JSValueWasm32, flags: c_int) c_int {
        return quickjs.JS_DefinePropertyValueStr(ctx, obj, prop, val, flags);
    }

    /// Define property with atom key and flags
    pub inline fn definePropertyValueAtom(ctx: *JSContext, obj: JSValueWasm32, atom: u32, val: JSValueWasm32, flags: c_int) c_int {
        return quickjs.JS_DefinePropertyValue(ctx, obj, atom, val, flags);
    }

    /// Define getter/setter property with atom key
    pub inline fn definePropertyGetSet(ctx: *JSContext, obj: JSValueWasm32, atom: u32, getter: JSValueWasm32, setter: JSValueWasm32, flags: c_int) c_int {
        return quickjs.JS_DefinePropertyGetSet(ctx, obj, atom, getter, setter, flags);
    }

    /// JS_PROP_C_W_E constant (configurable, writable, enumerable)
    pub const JS_PROP_C_W_E: c_int = quickjs.JS_PROP_C_W_E;

    /// JS_PROP_CONFIGURABLE constant (configurable only)
    pub const JS_PROP_CONFIGURABLE: c_int = quickjs.JS_PROP_CONFIGURABLE;

    /// JS_PROP_ENUMERABLE constant
    pub const JS_PROP_ENUMERABLE: c_int = quickjs.JS_PROP_ENUMERABLE;

    /// JS_PROP_HAS_GET constant (for getter properties)
    pub const JS_PROP_HAS_GET: c_int = quickjs.JS_PROP_HAS_GET;

    /// JS_PROP_HAS_SET constant (for setter properties)
    pub const JS_PROP_HAS_SET: c_int = quickjs.JS_PROP_HAS_SET;

    /// Create a string from an atom
    pub inline fn newAtomString(ctx: *JSContext, atom: u32) JSValueWasm32 {
        return quickjs.JS_AtomToString(ctx, atom);
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

    /// Get length of array/string (O(1) operation)
    pub inline fn getLength(ctx: *JSContext, len: *i64, obj: JSValueWasm32) c_int {
        return quickjs.JS_GetLength(ctx, obj, len);
    }

    /// Create new empty object
    pub inline fn newObject(ctx: *JSContext) JSValueWasm32 {
        return quickjs.JS_NewObject(ctx);
    }

    /// Create closure from bytecode function
    /// Creates a new closure that captures the specified variables.
    /// @param bfunc - Bytecode function to wrap (from cpool)
    /// @param cur_var_refs - Current var_refs chain (may be null)
    /// @param locals_js - Array of local variables as JSValues (for capturing locals)
    /// @param args - Slice of argument JSValues (for capturing args)
    pub fn createClosure(
        ctx: *JSContext,
        bfunc: JSValueWasm32,
        cur_var_refs: ?[*]*JSVarRef,
        locals_js: ?[*]const JSValueWasm32,
        num_locals: usize,
        args: []const JSValueWasm32,
    ) JSValueWasm32 {
        return quickjs.js_frozen_create_closure(
            ctx,
            bfunc,
            cur_var_refs,
            if (locals_js) |l| @constCast(l) else null,
            @intCast(num_locals),
            if (args.len > 0) @constCast(args.ptr) else null,
            @intCast(args.len),
        );
    }

    /// Wrap a value in Promise.resolve() for async function returns
    pub fn promiseResolve(ctx: *JSContext, val: JSValueWasm32) JSValueWasm32 {
        // Use Promise.resolve(val)
        const global = quickjs.JS_GetGlobalObject(ctx);
        defer quickjs.JS_FreeValue(ctx, global);

        const promise_ctor = quickjs.JS_GetPropertyStr(ctx, global, "Promise");
        defer quickjs.JS_FreeValue(ctx, promise_ctor);

        const resolve_fn = quickjs.JS_GetPropertyStr(ctx, promise_ctor, "resolve");
        defer quickjs.JS_FreeValue(ctx, resolve_fn);

        var args = [_]JSValueWasm32{val};
        return quickjs.JS_Call(ctx, resolve_fn, promise_ctor, 1, &args);
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
        if (profile.PROFILE) profile.vinc(profile.prof.ref_dups(), 1);
        if (val.hasRefCount()) {
            return quickjs.JS_DupValue(ctx, val);
        }
        return val;
    }

    pub inline fn free(ctx: *JSContext, val: JSValueNative) void {
        if (profile.PROFILE) profile.vinc(profile.prof.ref_frees(), 1);
        if (val.hasRefCount()) {
            quickjs.JS_FreeValue(ctx, val);
        }
    }

    // FFI wrappers
    pub inline fn call(ctx: *JSContext, func: JSValueNative, this: JSValueNative, argc: c_int, argv: [*]JSValueNative) JSValueNative {
        if (profile.PROFILE) profile.vinc(profile.prof.call_count(), 1);
        const t0 = if (profile.PROFILE) profile.cycles() else 0;
        const result = quickjs.JS_Call(ctx, func, this, argc, argv);
        if (profile.PROFILE) profile.vinc(profile.prof.call_cycles(), profile.cycles() - t0);
        return result;
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

    pub inline fn throwOutOfMemory(ctx: *JSContext) JSValueNative {
        return quickjs.JS_ThrowOutOfMemory(ctx);
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

    /// Convert to float64 - uses QuickJS JS_ToFloat64
    pub inline fn toFloat64(ctx: *JSContext, pres: *f64, val: JSValueNative) c_int {
        return quickjs.JS_ToFloat64(ctx, pres, val);
    }

    /// Convert to string - uses QuickJS JS_ToString
    pub inline fn toString(ctx: *JSContext, val: JSValueNative) JSValueNative {
        return quickjs.JS_ToString(ctx, val);
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

    /// Define property with string key and flags
    pub inline fn definePropertyValueStr(ctx: *JSContext, obj: JSValueNative, prop: [*:0]const u8, val: JSValueNative, flags: c_int) c_int {
        return quickjs.JS_DefinePropertyValueStr(ctx, obj, prop, val, flags);
    }

    /// Define property with atom key and flags
    pub inline fn definePropertyValueAtom(ctx: *JSContext, obj: JSValueNative, atom: u32, val: JSValueNative, flags: c_int) c_int {
        return quickjs.JS_DefinePropertyValue(ctx, obj, atom, val, flags);
    }

    /// Define getter/setter property with atom key
    pub inline fn definePropertyGetSet(ctx: *JSContext, obj: JSValueNative, atom: u32, getter: JSValueNative, setter: JSValueNative, flags: c_int) c_int {
        return quickjs.JS_DefinePropertyGetSet(ctx, obj, atom, getter, setter, flags);
    }

    /// JS_PROP_C_W_E constant (configurable, writable, enumerable)
    pub const JS_PROP_C_W_E: c_int = quickjs.JS_PROP_C_W_E;

    /// JS_PROP_CONFIGURABLE constant (configurable only)
    pub const JS_PROP_CONFIGURABLE: c_int = quickjs.JS_PROP_CONFIGURABLE;

    /// JS_PROP_ENUMERABLE constant
    pub const JS_PROP_ENUMERABLE: c_int = quickjs.JS_PROP_ENUMERABLE;

    /// JS_PROP_HAS_GET constant (for getter properties)
    pub const JS_PROP_HAS_GET: c_int = quickjs.JS_PROP_HAS_GET;

    /// JS_PROP_HAS_SET constant (for setter properties)
    pub const JS_PROP_HAS_SET: c_int = quickjs.JS_PROP_HAS_SET;

    /// Create a string from an atom
    pub inline fn newAtomString(ctx: *JSContext, atom: u32) JSValueNative {
        return quickjs.JS_AtomToString(ctx, atom);
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

    /// Get length of array/string (O(1) operation)
    pub inline fn getLength(ctx: *JSContext, len: *i64, obj: JSValueNative) c_int {
        return quickjs.JS_GetLength(ctx, obj, len);
    }

    /// Create a closure from a function bytecode object.
    /// Wraps js_frozen_create_closure for use in frozen codegen.
    /// @param ctx - JSContext pointer
    /// @param bfunc - Function bytecode JSValue from constant pool
    /// @param cur_var_refs - Current var_refs chain (may be null)
    /// @param locals_js - Array of local variables as JSValues (for capturing locals)
    /// @param args - Slice of argument JSValues (for capturing args)
    pub fn createClosure(
        ctx: *JSContext,
        bfunc: JSValueNative,
        cur_var_refs: ?[*]*JSVarRef,
        locals_js: ?[*]const JSValueNative,
        num_locals: usize,
        args: []const JSValueNative,
    ) JSValueNative {
        return quickjs.js_frozen_create_closure(
            ctx,
            bfunc,
            cur_var_refs,
            if (locals_js) |l| @constCast(l) else null,
            @intCast(num_locals),
            if (args.len > 0) @constCast(args.ptr) else null,
            @intCast(args.len),
        );
    }

    /// Create a closure with shared var_refs for function hoisting support.
    /// V2 version that uses a tracking list so closures see updates to captured locals.
    /// @param ctx - JSContext pointer
    /// @param bfunc - Function bytecode JSValue from constant pool
    /// @param cur_var_refs - Current var_refs chain (may be null)
    /// @param local_var_ref_list - List for tracking shared var_refs (may be null for immediate detach)
    /// @param locals_js - Array of local variables as JSValues (for capturing locals)
    /// @param args - Slice of argument JSValues (for capturing args)
    pub fn createClosureV2(
        ctx: *JSContext,
        bfunc: JSValueNative,
        cur_var_refs: ?[*]*JSVarRef,
        local_var_ref_list: ?*ListHead,
        locals_js: ?[*]const JSValueNative,
        num_locals: usize,
        args: []const JSValueNative,
    ) JSValueNative {
        return quickjs.js_frozen_create_closure_v2(
            ctx,
            bfunc,
            cur_var_refs,
            local_var_ref_list,
            if (locals_js) |l| @constCast(l) else null,
            @intCast(num_locals),
            if (args.len > 0) @constCast(args.ptr) else null,
            @intCast(args.len),
        );
    }

    /// Wrap a value in Promise.resolve() for async function returns
    pub fn promiseResolve(ctx: *JSContext, val: JSValueNative) JSValueNative {
        // Use Promise.resolve(val)
        const global = quickjs.JS_GetGlobalObject(ctx);
        defer quickjs.JS_FreeValue(ctx, global);

        const promise_ctor = quickjs.JS_GetPropertyStr(ctx, global, "Promise");
        defer quickjs.JS_FreeValue(ctx, promise_ctor);

        const resolve_fn = quickjs.JS_GetPropertyStr(ctx, promise_ctor, "resolve");
        defer quickjs.JS_FreeValue(ctx, resolve_fn);

        var args = [_]JSValueNative{val};
        return quickjs.JS_Call(ctx, resolve_fn, promise_ctor, 1, &args);
    }
};

// ============================================================================
// JSVarRef - QuickJS closure variable reference
// ============================================================================

/// JSVarRef - QuickJS closure variable reference
/// Layout must match C: union{JSGCObjectHeader(24 bytes)}(24) + pvalue(8) + value(16) = 48 bytes
/// JSGCObjectHeader = ref_count(4) + gc_bits(1) + dummy1(1) + dummy2(2) + list_head(16) = 24 bytes
pub const JSVarRef = extern struct {
    ref_count: i32, // offset 0: GC ref count
    gc_bits: u8, // offset 4: gc_obj_type:4 + mark:4
    is_detached: u8, // offset 5
    dummy2: u16, // offset 6
    link_next: ?*anyopaque, // offset 8: list_head.next
    link_prev: ?*anyopaque, // offset 16: list_head.prev
    pvalue: *JSValue, // offset 24: pointer to the closure variable value
    value: JSValue, // offset 32: used when variable is no longer on stack
};

// ============================================================================
// ListHead - QuickJS doubly-linked list head (for var_ref tracking)
// ============================================================================

/// ListHead - QuickJS doubly-linked list structure
/// Used for tracking var_refs in frozen functions that need shared closure variables
pub const ListHead = extern struct {
    prev: ?*ListHead,
    next: ?*ListHead,

    /// Initialize a list head to point to itself (empty list)
    pub fn init(self: *ListHead) void {
        quickjs.js_frozen_var_ref_list_init(self);
    }
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
    pub extern fn js_frozen_get_var_ref_safe(ctx: *JSContext, var_refs: ?*anyopaque, idx: c_int, var_refs_count: c_int) JSValue;
    pub extern fn js_frozen_set_var_ref(ctx: *JSContext, var_refs: ?*anyopaque, idx: c_int, val: JSValue) void;
    pub extern fn js_frozen_set_var_ref_safe(ctx: *JSContext, var_refs: ?*anyopaque, idx: c_int, var_refs_count: c_int, val: JSValue) void;

    // Function calls
    pub extern fn JS_Call(ctx: *JSContext, func: JSValue, this: JSValue, argc: c_int, argv: [*]const JSValue) JSValue;
    pub extern fn js_frozen_try_call(ctx: *JSContext, func: JSValue, this: JSValue, argc: c_int, argv: [*]const JSValue, result_out: *JSValue) c_int;

    // Atom management
    pub extern fn JS_NewAtom(ctx: *JSContext, str: [*:0]const u8) u32;
    pub extern fn JS_FreeAtom(ctx: *JSContext, atom: u32) void;
    pub extern fn JS_ValueToAtom(ctx: *JSContext, val: JSValue) u32;
    pub extern fn JS_HasProperty(ctx: *JSContext, obj: JSValue, atom: u32) c_int;

    // Property access
    pub extern fn JS_GetPropertyStr(ctx: *JSContext, obj: JSValue, prop: [*:0]const u8) JSValue;
    pub extern fn JS_SetPropertyStr(ctx: *JSContext, obj: JSValue, prop: [*:0]const u8, val: JSValue) c_int;
    pub extern fn JS_GetPropertyUint32(ctx: *JSContext, obj: JSValue, idx: u32) JSValue;
    // Out-parameter version for WASM32 to avoid LLVM FastISel return corruption
    pub extern fn JS_GetPropertyUint32_OutParam(ctx: *JSContext, obj: JSValue, idx: u32, out: *JSValue) void;
    pub extern fn JS_SetPropertyUint32(ctx: *JSContext, obj: JSValue, idx: u32, val: JSValue) c_int;
    // Atom-based property access (faster than string-based)
    pub extern fn JS_GetProperty(ctx: *JSContext, obj: JSValue, atom: u32) JSValue;
    pub extern fn JS_SetProperty(ctx: *JSContext, obj: JSValue, atom: u32, val: JSValue) c_int;

    // Type conversion
    pub extern fn JS_ToBool(ctx: *JSContext, val: JSValue) c_int;
    pub extern fn JS_ToInt32(ctx: *JSContext, pres: *i32, val: JSValue) c_int;
    pub extern fn JS_ToObject(ctx: *JSContext, val: JSValue) JSValue;
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
    pub extern fn JS_NewArguments(ctx: *JSContext, argc: c_int, argv: [*]const JSValue) JSValue;
    pub extern fn JS_NewObjectProto(ctx: *JSContext, proto: JSValue) JSValue;
    pub extern fn JS_NewObjectProtoClass(ctx: *JSContext, proto: JSValue, class_id: u32) JSValue;
    pub extern fn JS_GetPrototype(ctx: *JSContext, val: JSValue) JSValue;
    pub extern fn JS_SetHomeObject(ctx: *JSContext, func_obj: JSValue, home_obj: JSValue) void;
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
    pub extern fn JS_ThrowOutOfMemory(ctx: *JSContext) JSValue;
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
    pub extern fn JS_DefinePropertyValue(ctx: *JSContext, this_obj: JSValue, prop: u32, val: JSValue, flags: c_int) c_int;
    pub extern fn JS_DefinePropertyGetSet(ctx: *JSContext, this_obj: JSValue, prop: u32, getter: JSValue, setter: JSValue, flags: c_int) c_int;
    pub const JS_PROP_C_W_E: c_int = (1 << 0) | (1 << 1) | (1 << 2); // configurable, writable, enumerable
    pub const JS_PROP_CONFIGURABLE: c_int = (1 << 0);
    pub const JS_PROP_ENUMERABLE: c_int = (1 << 2);
    pub const JS_PROP_HAS_GET: c_int = (1 << 11);
    pub const JS_PROP_HAS_SET: c_int = (1 << 12);

    // Atom to string conversion
    pub extern fn JS_AtomToString(ctx: *JSContext, atom: u32) JSValue;

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
    pub extern fn js_frozen_set_proto(ctx: *JSContext, obj: JSValue, proto: JSValue) c_int;
    pub extern fn js_frozen_exec_opcode(ctx: *JSContext, opcode: c_int, operand: u32, stack: [*]JSValue, sp: *c_int, locals: [*]JSValue, local_count: c_int) JSValue;
    pub extern fn js_frozen_copy_data_properties(ctx: *JSContext, dst: JSValue, src: JSValue, excluded: JSValue) c_int;

    // Module/import support
    pub extern fn JS_GetImportMetaCurrent(ctx: *JSContext) JSValue;

    // Memory allocation (QuickJS exported)
    pub extern fn js_malloc(ctx: *JSContext, size: usize) ?*anyopaque;
    pub extern fn js_free(ctx: *JSContext, ptr: *anyopaque) void;

    // TypedArray/ArrayBuffer access
    pub extern fn JS_GetTypedArrayBuffer(ctx: *JSContext, obj: JSValue, pbyte_offset: *usize, pbyte_length: *usize, pbytes_per_element: *usize) JSValue;
    pub extern fn JS_GetArrayBuffer(ctx: *JSContext, psize: *usize, obj: JSValue) ?[*]u8;

    // Closure creation
    pub extern fn js_frozen_create_closure(ctx: *JSContext, bfunc: JSValue, cur_var_refs: ?[*]*JSVarRef, locals: ?[*]JSValue, num_locals: c_int, args: ?[*]JSValue, num_args: c_int) JSValue;
    pub extern fn JS_GetFunctionConstantPool(ctx: *JSContext, func_obj: JSValue, pcount: ?*c_int) ?[*]JSValue;

    // V2 closure creation with shared var_refs for function hoisting support
    // ListHead is an opaque struct - we just need a pointer to pass to C
    pub extern fn js_frozen_var_ref_list_init(list: *ListHead) void;
    pub extern fn js_frozen_var_ref_list_detach(ctx: *JSContext, list: *ListHead) void;
    pub extern fn js_frozen_create_closure_v2(ctx: *JSContext, bfunc: JSValue, cur_var_refs: ?[*]*JSVarRef, local_var_ref_list: ?*ListHead, locals: ?[*]JSValue, num_locals: c_int, args: ?[*]JSValue, num_args: c_int) JSValue;

    // Class constructor creation (for define_class opcode)
    pub extern fn js_frozen_define_class(ctx: *JSContext, bfunc: JSValue, parent_class: JSValue, class_flags: c_int, class_name: u32, cur_var_refs: ?[*]*JSVarRef, local_var_ref_list: ?*ListHead, locals: ?[*]JSValue, num_locals: c_int, args: ?[*]JSValue, num_args: c_int, out_ctor: *JSValue, out_proto: *JSValue) c_int;

    // Inline cache for property access
    pub extern fn js_frozen_ic_load(ctx: *JSContext, obj: JSValue, ic_shape: *?*anyopaque, ic_offset: *u32, ic_atom: *u32, name: [*:0]const u8) JSValue;

    // Runtime access
    pub extern fn JS_GetRuntime(ctx: *JSContext) *JSRuntime;
    pub extern fn JS_NewContext(rt: *JSRuntime) ?*JSContext;
    pub extern fn JS_FreeContext(ctx: *JSContext) void;

    // Promise support for async/await
    /// Create a new Promise with resolve/reject capability
    /// Returns the promise, and fills resolving_funcs with [resolve, reject] functions
    pub extern fn JS_NewPromiseCapability(ctx: *JSContext, resolving_funcs: *[2]JSValue) JSValue;

    /// Get the currently active/executing function
    pub extern fn JS_GetActiveFunction(ctx: *JSContext) JSValue;

    /// Create a C function with associated data (closure-like)
    /// data_len is the number of JSValue elements in data
    /// magic is passed to the function
    pub extern fn JS_NewCFunctionData(
        ctx: *JSContext,
        func: *const fn (*JSContext, JSValue, c_int, [*]JSValue, c_int, [*]JSValue) callconv(.c) JSValue,
        length: c_int,
        magic: c_int,
        data_len: c_int,
        data: [*]JSValue,
    ) JSValue;

    /// Get bytecode pointer from a function JSValue
    /// Used for registering closure bytecode with frozen dispatch
    pub extern fn js_get_function_bytecode_ptr(val: JSValue) ?*anyopaque;
};

// ============================================================================
// Promise helpers for async/await support
// ============================================================================

/// Check if a value is a thenable (has a .then method that's a function)
/// This uses duck-typing which matches JavaScript's Promise.resolve behavior
pub fn isThenable(ctx: *JSContext, val: JSValue) bool {
    if (!val.isObject()) return false;
    const then_prop = quickjs.JS_GetPropertyStr(ctx, val, "then");
    defer quickjs.JS_FreeValue(ctx, then_prop);
    return quickjs.JS_IsFunction(ctx, then_prop) != 0;
}

/// Convert a value to a Promise (wrap non-promises in Promise.resolve)
pub fn toPromise(ctx: *JSContext, val: JSValue) JSValue {
    if (isThenable(ctx, val)) {
        return quickjs.JS_DupValue(ctx, val);
    }
    // Use Promise.resolve(val)
    const global = quickjs.JS_GetGlobalObject(ctx);
    defer quickjs.JS_FreeValue(ctx, global);

    const promise_ctor = quickjs.JS_GetPropertyStr(ctx, global, "Promise");
    defer quickjs.JS_FreeValue(ctx, promise_ctor);

    const resolve_fn = quickjs.JS_GetPropertyStr(ctx, promise_ctor, "resolve");
    defer quickjs.JS_FreeValue(ctx, resolve_fn);

    var args = [_]JSValue{val};
    return quickjs.JS_Call(ctx, resolve_fn, promise_ctor, 1, &args);
}

/// Attach .then() and .catch() handlers to a promise
/// Returns a new promise that resolves/rejects based on the handlers
pub fn promiseThen(
    ctx: *JSContext,
    promise: JSValue,
    on_resolve: JSValue,
    on_reject: JSValue,
) JSValue {
    const then_fn = quickjs.JS_GetPropertyStr(ctx, promise, "then");
    defer quickjs.JS_FreeValue(ctx, then_fn);

    var args = [_]JSValue{ on_resolve, on_reject };
    return quickjs.JS_Call(ctx, then_fn, promise, 2, &args);
}

/// Create a rejected promise with the given reason
pub fn promiseReject(ctx: *JSContext, reason: JSValue) JSValue {
    const global = quickjs.JS_GetGlobalObject(ctx);
    defer quickjs.JS_FreeValue(ctx, global);

    const promise_ctor = quickjs.JS_GetPropertyStr(ctx, global, "Promise");
    defer quickjs.JS_FreeValue(ctx, promise_ctor);

    const reject_fn = quickjs.JS_GetPropertyStr(ctx, promise_ctor, "reject");
    defer quickjs.JS_FreeValue(ctx, reject_fn);

    var args = [_]JSValue{reason};
    return quickjs.JS_Call(ctx, reject_fn, promise_ctor, 1, &args);
}

/// Wrap a value in Promise.resolve() - alias for toPromise
/// Used by async functions to ensure return value is always a Promise
pub fn promiseResolve(ctx: *JSContext, val: JSValue) JSValue {
    return toPromise(ctx, val);
}

// ============================================================================
// Frozen Async State - saved execution state for await resumption
// ============================================================================

/// Maximum inline storage for stack+locals in FrozenAsyncState
/// States larger than this use heap-allocated overflow storage
const ASYNC_STATE_INLINE_SIZE = 64;

/// State saved when a frozen async function hits an await
/// Allocated on the heap, freed when the promise resolves
pub const FrozenAsyncState = extern struct {
    /// Magic number for validation
    magic: u32 = 0xAF57A7E1, // "AFSTATE" in hex-ish

    /// Function identifier (for dispatch back to correct function)
    func_id: u32,

    /// Current block/state index in the Relooper state machine
    block_id: u32,

    /// Stack pointer (number of values on stack)
    sp: u32,

    /// Number of local variables
    locals_count: u32,

    /// The JSContext (needed for resume)
    ctx: ?*JSContext,

    /// Pointer to overflow data if state exceeds inline capacity, null otherwise
    overflow_data: ?[*]CompressedValue,

    /// Inline storage for small stacks (avoids extra allocation)
    /// Layout: [stack values...][local values...]
    /// Total size = sp + locals_count CompressedValues
    inline_data: [ASYNC_STATE_INLINE_SIZE]CompressedValue,

    /// Create a new async state, copying stack and locals
    pub fn create(
        allocator: std.mem.Allocator,
        ctx: *JSContext,
        func_id: u32,
        block_id: u32,
        stack: []const CompressedValue,
        locals: []const CompressedValue,
    ) !*FrozenAsyncState {
        _ = allocator;
        const total_size = stack.len + locals.len;

        // Allocate via QuickJS allocator (so it can be freed from C)
        const state_ptr = quickjs.js_malloc(ctx, @sizeOf(FrozenAsyncState)) orelse return error.OutOfMemory;
        const state: *FrozenAsyncState = @ptrCast(@alignCast(state_ptr));

        state.* = .{
            .func_id = func_id,
            .block_id = block_id,
            .sp = @intCast(stack.len),
            .locals_count = @intCast(locals.len),
            .ctx = ctx,
            .overflow_data = null,
            .inline_data = undefined,
        };

        if (total_size <= ASYNC_STATE_INLINE_SIZE) {
            // Use inline storage
            @memcpy(state.inline_data[0..stack.len], stack);
            @memcpy(state.inline_data[stack.len..][0..locals.len], locals);
        } else {
            // Allocate overflow storage
            const overflow_ptr = quickjs.js_malloc(ctx, total_size * @sizeOf(CompressedValue)) orelse {
                quickjs.js_free(ctx, state_ptr);
                return error.OutOfMemory;
            };
            state.overflow_data = @ptrCast(@alignCast(overflow_ptr));
            @memcpy(state.overflow_data.?[0..stack.len], stack);
            @memcpy(state.overflow_data.?[stack.len..][0..locals.len], locals);
        }

        return state;
    }

    /// Get pointer to the data storage (inline or overflow)
    fn getData(self: *const FrozenAsyncState) [*]const CompressedValue {
        if (self.overflow_data) |overflow| {
            return overflow;
        }
        return &self.inline_data;
    }

    /// Restore stack and locals from saved state
    pub fn restore(self: *const FrozenAsyncState, stack: []CompressedValue, locals: []CompressedValue) void {
        const data = self.getData();
        // Restore stack
        @memcpy(stack[0..self.sp], data[0..self.sp]);
        // Restore locals
        @memcpy(locals[0..self.locals_count], data[self.sp..][0..self.locals_count]);
    }

    /// Free the async state and any overflow storage
    pub fn destroy(self: *FrozenAsyncState) void {
        if (self.ctx) |ctx| {
            if (self.overflow_data) |overflow| {
                quickjs.js_free(ctx, @ptrCast(@alignCast(overflow)));
            }
            quickjs.js_free(ctx, self);
        }
    }
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
