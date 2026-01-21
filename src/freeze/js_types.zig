//! Core JavaScript Types for Frozen Functions
//!
//! This module provides:
//! - Tag constants matching QuickJS
//! - Opaque types for JSContext and JSRuntime
//! - Pointer compression utilities for heap management
//! - Basic struct layouts (JSVarRef, JSValueUnion)

const std = @import("std");
const builtin = @import("builtin");

// ============================================================================
// Platform Detection
// ============================================================================

/// Detect target architecture for correct JSValue ABI
/// QuickJS uses NaN-boxing (8-byte u64) on 32-bit platforms
/// QuickJS uses struct (16-byte) on 64-bit platforms
/// This MUST match the QuickJS build to avoid ABI mismatch!
pub const is_wasm32 = builtin.cpu.arch == .wasm32;

// ============================================================================
// Pointer Compression
// ============================================================================

/// Heap base for pointer compression on native builds
/// QuickJS pointers are compressed to 32-bit offsets from heap base
pub var heap_base: usize = 0;

pub inline fn compressPtr(ptr: usize) u32 {
    if (builtin.cpu.arch == .wasm32) {
        return @truncate(ptr);
    }
    // For native: store offset from heap base (or full ptr if base not set)
    if (heap_base == 0) {
        // Fallback: truncate (works for low addresses)
        return @truncate(ptr);
    }
    return @truncate(ptr - heap_base);
}

pub inline fn decompressPtr(offset: u32) usize {
    if (builtin.cpu.arch == .wasm32) {
        return offset;
    }
    // For native: add heap base to get full pointer
    if (heap_base == 0) {
        return offset;
    }
    return heap_base + offset;
}

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
// JSContext and JSRuntime - Opaque pointers to QuickJS
// ============================================================================

pub const JSContext = opaque {};
pub const JSRuntime = opaque {};

// ============================================================================
// JSValueUnion - Platform-specific value union
// ============================================================================

pub const JSValueUnion = extern union {
    int32: i32,
    float64: f64,
    ptr: ?*anyopaque,
    short_big_int: i32,
};
