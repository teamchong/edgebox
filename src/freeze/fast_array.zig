//! Fast Array Access - Zero-FFI optimizations for TypedArray and regular arrays
//!
//! Provides direct memory access to array data when possible,
//! bypassing QuickJS property lookups for hot paths.

const std = @import("std");
const js_value = @import("js_value.zig");

const JSContext = js_value.JSContext;
const JSValue = js_value.JSValue;
const quickjs = js_value.quickjs;

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
/// The layout differs between 32-bit and 64-bit due to pointer sizes:
/// - 64-bit: list_head is 16 bytes, each pointer is 8 bytes
/// - 32-bit: list_head is 8 bytes, each pointer is 4 bytes
pub const JSObject = if (@sizeOf(*anyopaque) == 4) JSObject32 else JSObject64;

/// JSObject layout for 32-bit (WASM32)
/// Offsets: header(8) + link(8) + shape(4) + prop(4) + weak_ref(4) = union at offset 28
const JSObject32 = extern struct {
    // Header (ref_count + gc bits + flags + class_id) - 8 bytes
    ref_count: i32,
    gc_mark: u8,
    flags: u8, // extensible:1, free_mark:1, is_exotic:1, fast_array:1, ...
    class_id: u16,

    // list_head link (two pointers) - 8 bytes on 32-bit
    link_next: u32, // Use u32 instead of pointer to ensure correct size
    link_prev: u32,

    // Pointers after list_head - 4 bytes each on 32-bit
    shape: u32,
    prop: u32,
    first_weak_ref: u32,

    // Union - we only care about array case
    u: extern union {
        opaque_ptr: u32,
        array: extern struct {
            u1: extern union {
                size: u32,
                typed_array: u32,
            },
            values: u32, // Pointer stored as u32, cast to [*]JSValue when needed
            count: u32,
        },
    },

    /// Check if this object has fast_array flag set
    pub inline fn isFastArray(self: *const JSObject32) bool {
        return (self.flags & 0x08) != 0; // fast_array is bit 3
    }

    /// Check if this is a regular JS array (not typed array)
    pub inline fn isRegularArray(self: *const JSObject32) bool {
        return self.class_id == JS_CLASS_ARRAY or self.class_id == JS_CLASS_ARGUMENTS;
    }

    /// Get values pointer (cast from u32)
    pub inline fn getValues(self: *const JSObject32) ?[*]JSValue {
        if (self.u.array.values == 0) return null;
        return @ptrFromInt(self.u.array.values);
    }

    /// Get count
    pub inline fn getCount(self: *const JSObject32) u32 {
        return self.u.array.count;
    }
};

/// JSObject layout for 64-bit (native)
/// Offsets: header(8) + link(16) + shape(8) + prop(8) + weak_ref(8) = union at offset 48
const JSObject64 = extern struct {
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
    pub inline fn isFastArray(self: *const JSObject64) bool {
        return (self.flags & 0x08) != 0; // fast_array is bit 3
    }

    /// Check if this is a regular JS array (not typed array)
    pub inline fn isRegularArray(self: *const JSObject64) bool {
        return self.class_id == JS_CLASS_ARRAY or self.class_id == JS_CLASS_ARGUMENTS;
    }

    /// Get values pointer
    pub inline fn getValues(self: *const JSObject64) ?[*]JSValue {
        return self.u.array.values;
    }

    /// Get count
    pub inline fn getCount(self: *const JSObject64) u32 {
        return self.u.array.count;
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

    // Return direct access to values (uses platform-specific getter)
    return .{
        .values = obj.getValues(),
        .count = obj.getCount(),
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
