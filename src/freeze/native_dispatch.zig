//! Native Frozen Function Dispatch
//!
//! Provides O(1) index-based lookup for frozen functions, called directly
//! from the QuickJS bytecode interpreter (JS_CallInternal) via C FFI.
//!
//! Each frozen function is assigned a stable index at compile time (its position
//! in the module parser's function list). During bytecode deserialization,
//! JS_ReadFunctionTag stores the same counter as frozen_func_index in each
//! JSFunctionBytecode. At call time, a flat array lookup replaces the old
//! string hash table, eliminating hashing overhead and duplicate-key issues.

const std = @import("std");
const zig_runtime = @import("zig_runtime");

const JSContext = zig_runtime.JSContext;
const JSValue = zig_runtime.JSValue;
const JSVarRef = zig_runtime.JSVarRef;
const qjs = zig_runtime.quickjs;

// WASM32 detection - same as in js_value.zig
const is_wasm32 = @sizeOf(*anyopaque) == 4;

/// C function pointer type for frozen functions (includes var_refs, cpool)
pub const FrozenFnPtr = *const fn (*JSContext, JSValue, c_int, [*]JSValue, ?[*]*JSVarRef, c_int, ?[*]JSValue) callconv(.c) JSValue;

// ============================================================================
// Index-based registry: flat array indexed by parser function ordinal
// ============================================================================

/// Arena allocator for registry allocations - freed all at once on clear()
var arena: ?std.heap.ArenaAllocator = null;

/// Flat array: index → FrozenFnPtr (null = not frozen)
var index_array: ?[]?FrozenFnPtr = null;

/// Current capacity of index_array
var index_capacity: usize = 0;

/// Initialize the registry
fn ensureInit() void {
    if (arena == null) {
        arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    }
}

/// Ensure index_array can hold at least `idx + 1` entries
fn ensureCapacity(idx: u32) void {
    ensureInit();
    const needed = @as(usize, idx) + 1;
    if (index_array != null and needed <= index_capacity) return;

    const alloc = arena.?.allocator();
    const new_cap = @max(needed, index_capacity * 2, 256);

    if (index_array) |old| {
        const new_arr = alloc.alloc(?FrozenFnPtr, new_cap) catch return;
        @memcpy(new_arr[0..old.len], old);
        @memset(new_arr[old.len..], null);
        index_array = new_arr;
    } else {
        const new_arr = alloc.alloc(?FrozenFnPtr, new_cap) catch return;
        @memset(new_arr, null);
        index_array = new_arr;
    }
    index_capacity = new_cap;
}

/// Register a frozen function by its parser index
/// Called from LLVM shard init functions during startup
pub fn registerByIndex(idx: u32, func: FrozenFnPtr) callconv(.c) void {
    ensureCapacity(idx);
    if (index_array) |arr| {
        if (idx < arr.len) {
            arr[idx] = func;
        }
    }
}

/// Lookup a frozen function by parser index
/// Returns the function pointer or null if not frozen
fn lookupByIndex(idx: u32) ?FrozenFnPtr {
    if (index_array) |arr| {
        if (idx < arr.len) {
            return arr[idx];
        }
    }
    return null;
}

// Export for LLVM shards to call via extern linkage
comptime {
    @export(&registerByIndex, .{ .name = "native_dispatch_register_by_index" });
}

/// Flag to enable/disable frozen dispatch (set after init complete)
var dispatch_enabled: bool = false;

/// Enable frozen dispatch (called after frozen_init completes)
pub fn enableDispatch() void {
    dispatch_enabled = true;
    // Debug: print registration stats
    if (index_array) |arr| {
        var count: usize = 0;
        var max_idx: usize = 0;
        for (arr, 0..) |entry, i| {
            if (entry != null) {
                count += 1;
                max_idx = i;
            }
        }
        std.debug.print("[dispatch] Enabled: {d} functions registered, max_index={d}, capacity={d}\n", .{ count, max_idx, index_capacity });
    } else {
        std.debug.print("[dispatch] Enabled: no functions registered\n", .{});
    }
}

/// Check if frozen dispatch is enabled (exported for C to avoid caching misses during init)
pub export fn frozen_dispatch_is_enabled() callconv(.c) c_int {
    return if (dispatch_enabled) 1 else 0;
}

/// Disable frozen dispatch (for fallback mode)
pub fn disableDispatch() void {
    dispatch_enabled = false;
}

// ============================================================================
// C FFI - Called from QuickJS JS_CallInternal
// ============================================================================

/// Get frozen function pointer by parser index
/// Called from JS_CallInternal on first call to a function
/// Returns: function pointer if frozen, NULL if not
pub export fn frozen_dispatch_get_by_index(idx: c_int) callconv(.c) ?*anyopaque {
    if (idx < 0) return null;
    const func = lookupByIndex(@intCast(idx)) orelse return null;
    return @ptrCast(@constCast(func));
}

/// Get the number of registered frozen functions
pub export fn frozen_dispatch_count() callconv(.c) c_int {
    if (index_array) |arr| {
        var count: c_int = 0;
        for (arr) |entry| {
            if (entry != null) count += 1;
        }
        return count;
    }
    return 0;
}

/// Clear the registry (for testing)
pub fn clear() void {
    if (arena) |*a| {
        a.deinit();
    }
    arena = null;
    index_array = null;
    index_capacity = 0;
    dispatch_enabled = false;
}

// ============================================================================
// Tests
// ============================================================================

test "register and lookup by index" {
    clear();

    // Test function
    const testFn = struct {
        fn call(_: *JSContext, _: JSValue, _: c_int, _: [*]JSValue, _: ?[*]*JSVarRef, _: c_int, _: ?[*]JSValue) callconv(.c) JSValue {
            return JSValue.newInt(42);
        }
    }.call;

    registerByIndex(5, @ptrCast(&testFn));
    enableDispatch();

    const found = lookupByIndex(5);
    try std.testing.expect(found != null);

    const not_found = lookupByIndex(3);
    try std.testing.expect(not_found == null);

    const out_of_range = lookupByIndex(999);
    try std.testing.expect(out_of_range == null);

    clear();
}

test "sparse index registration" {
    clear();

    const fn1 = struct {
        fn call(_: *JSContext, _: JSValue, _: c_int, _: [*]JSValue, _: ?[*]*JSVarRef, _: c_int, _: ?[*]JSValue) callconv(.c) JSValue {
            return JSValue.newInt(1);
        }
    }.call;

    const fn2 = struct {
        fn call(_: *JSContext, _: JSValue, _: c_int, _: [*]JSValue, _: ?[*]*JSVarRef, _: c_int, _: ?[*]JSValue) callconv(.c) JSValue {
            return JSValue.newInt(2);
        }
    }.call;

    // Register sparse indices
    registerByIndex(0, @ptrCast(&fn1));
    registerByIndex(100, @ptrCast(&fn2));
    registerByIndex(5000, @ptrCast(&fn1));

    enableDispatch();

    try std.testing.expect(lookupByIndex(0) != null);
    try std.testing.expect(lookupByIndex(1) == null);
    try std.testing.expect(lookupByIndex(100) != null);
    try std.testing.expect(lookupByIndex(5000) != null);
    try std.testing.expect(lookupByIndex(5001) == null);

    clear();
}
