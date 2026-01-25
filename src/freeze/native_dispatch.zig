//! Native Frozen Function Dispatch
//!
//! Provides a fast hash table lookup for frozen functions, called directly
//! from the QuickJS bytecode interpreter (JS_CallInternal) via C FFI.
//!
//! This eliminates the overhead of JS property lookups (globalThis.__frozen_NAME)
//! by doing the dispatch in native code before bytecode execution.
//!
//! Uses arena allocator + wyhash for efficient dynamic allocation and fast lookups.

const std = @import("std");
const zig_runtime = @import("zig_runtime");
const hashmap_helper = @import("../utils/hashmap_helper.zig");

const JSContext = zig_runtime.JSContext;
const JSValue = zig_runtime.JSValue;
const JSVarRef = zig_runtime.JSVarRef;
const qjs = zig_runtime.quickjs;

// WASM32 detection - same as in js_value.zig
const is_wasm32 = @sizeOf(*anyopaque) == 4;

// FFI for cpool access
extern fn js_frozen_get_cpool_info(bytecode_ptr: ?*anyopaque, cpool_count_out: *c_int, cpool_out: *?*anyopaque) c_int;
extern fn js_frozen_get_cpool_func_bytecode(cpool: *anyopaque, idx: c_int) ?*anyopaque;

// FFI for bytecode info access (name written to caller buffer)
extern fn js_frozen_get_bytecode_name_line(ctx: *JSContext, bytecode_ptr: ?*anyopaque, name_buf: [*]u8, name_buf_size: c_int, line_out: *c_int) c_int;

/// C function pointer type for frozen functions (includes var_refs and cpool for closure/fclosure support)
pub const FrozenFnPtr = *const fn (*JSContext, JSValue, c_int, [*]JSValue, ?[*]*JSVarRef, c_int, ?[*]JSValue) callconv(.c) JSValue;

// ============================================================================
// Arena-backed registries using wyhash for fast lookup
// ============================================================================

/// Arena allocator for all registry allocations - freed all at once on clear()
var arena: ?std.heap.ArenaAllocator = null;

/// Name-based registry: name@line -> FrozenFnPtr (uses wyhash)
var name_registry: ?hashmap_helper.StringHashMap(FrozenFnPtr) = null;

/// Bytecode pointer registry: *anyopaque -> FrozenFnPtr
var bytecode_registry: ?std.AutoArrayHashMap(*anyopaque, FrozenFnPtr) = null;

/// Initialize the registries with arena allocator
fn ensureInit() void {
    if (arena == null) {
        arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        const alloc = arena.?.allocator();
        name_registry = hashmap_helper.StringHashMap(FrozenFnPtr).init(alloc);
        bytecode_registry = std.AutoArrayHashMap(*anyopaque, FrozenFnPtr).init(alloc);
    }
}

/// Recursively register a function and all its cpool entries by bytecode pointer
/// This ensures nested/anonymous functions can be dispatched when called via closures
fn registerFunctionAndCpool(ctx: *JSContext, bytecode_ptr: *anyopaque, parent_func: FrozenFnPtr, depth: usize) void {
    // Prevent infinite recursion
    if (depth > 10) return;

    // Register the main function
    if (lookupByBytecode(bytecode_ptr) == null) {
        registerByBytecode(bytecode_ptr, parent_func);
    }

    // Get cpool info
    var cpool_count: c_int = 0;
    var cpool_ptr: ?*anyopaque = null;
    if (js_frozen_get_cpool_info(bytecode_ptr, &cpool_count, &cpool_ptr) == 0) return;
    if (cpool_ptr == null or cpool_count <= 0) return;

    // Iterate cpool and register any function bytecodes found
    var i: c_int = 0;
    while (i < cpool_count) : (i += 1) {
        if (js_frozen_get_cpool_func_bytecode(cpool_ptr.?, i)) |nested_bc| {
            // Only register if not already registered
            if (lookupByBytecode(nested_bc) == null) {
                // Try to find the frozen function for this nested bytecode by name@line
                var name_buf: [128]u8 = undefined;
                var line_num: c_int = 0;
                if (js_frozen_get_bytecode_name_line(ctx, nested_bc, &name_buf, 128, &line_num) != 0 and line_num > 0) {
                    // Format name@line_num and look up in name registry
                    // Use "anonymous" for empty function names (e.g., getters/setters in object literals)
                    const name_slice = if (name_buf[0] != 0)
                        name_buf[0..(std.mem.indexOfScalar(u8, &name_buf, 0) orelse name_buf.len)]
                    else
                        "anonymous";
                    var key_buf: [192]u8 = undefined;
                    const key = std.fmt.bufPrintZ(&key_buf, "{s}@{d}", .{ name_slice, line_num }) catch continue;
                    if (lookup(key)) |nested_func| {
                        registerByBytecode(nested_bc, nested_func);
                        // Recursively register this function's cpool
                        registerFunctionAndCpool(ctx, nested_bc, nested_func, depth + 1);
                        continue;
                    }
                }
                // If no name or not found by name, do NOT register with parent's func
                // This would cause the nested function to be dispatched with wrong cpool
                // Instead, let it fall through to normal bytecode execution
                // (Skip registration - the function will use standard QuickJS dispatch)
            }
        }
    }
}

// Flag to indicate frozen dispatch just happened - allows callers to fix return value
var g_frozen_dispatch_occurred: bool = false;

/// Check if frozen dispatch just occurred (and reset the flag)
pub export fn frozen_dispatch_check_and_reset() callconv(.c) c_int {
    const result = g_frozen_dispatch_occurred;
    g_frozen_dispatch_occurred = false;
    return if (result) 1 else 0;
}

/// Register a frozen function by its bytecode pointer
/// Called after bytecode is loaded when we know the actual bytecode address
pub fn registerByBytecode(bytecode_ptr: *anyopaque, func: FrozenFnPtr) void {
    ensureInit();
    bytecode_registry.?.put(bytecode_ptr, func) catch {
        std.debug.print("[native_dispatch] Failed to register bytecode function\n", .{});
    };
}

/// Lookup a frozen function by bytecode pointer
fn lookupByBytecode(bytecode_ptr: *anyopaque) ?FrozenFnPtr {
    if (bytecode_registry) |*reg| {
        return reg.get(bytecode_ptr);
    }
    return null;
}

/// Register a frozen function in the native registry
/// Called from frozen_init during startup
pub fn register(name: [*:0]const u8, func: FrozenFnPtr) void {
    ensureInit();
    const alloc = arena.?.allocator();

    // Convert null-terminated string to slice and dupe for arena ownership
    var len: usize = 0;
    while (name[len] != 0) : (len += 1) {}
    const name_slice = name[0..len];

    const owned_name = alloc.dupe(u8, name_slice) catch {
        std.debug.print("[native_dispatch] Failed to allocate name: {s}\n", .{name_slice});
        return;
    };

    name_registry.?.put(owned_name, func) catch {
        std.debug.print("[native_dispatch] Failed to register: {s}\n", .{name_slice});
    };
}

/// Lookup a frozen function by name
/// Returns null if not found
fn lookup(name: [*:0]const u8) ?FrozenFnPtr {
    if (name_registry) |*reg| {
        var len: usize = 0;
        while (name[len] != 0) : (len += 1) {}
        return reg.get(name[0..len]);
    }
    return null;
}

/// Flag to enable/disable frozen dispatch (set after init complete)
var dispatch_enabled: bool = false;

/// Enable frozen dispatch (called after frozen_init completes)
pub fn enableDispatch() void {
    dispatch_enabled = true;
}

/// Disable frozen dispatch (for fallback mode)
pub fn disableDispatch() void {
    dispatch_enabled = false;
}

// ============================================================================
// C FFI - Called from QuickJS JS_CallInternal
// ============================================================================

/// Lookup and call a frozen function if one exists for the given name
/// Called from JS_CallInternal before executing bytecode
/// Returns: 1 if frozen function was called (result in *result_out), 0 if not found
///
/// This is the hot path - must be as fast as possible
/// Uses name@line_num format to disambiguate functions with the same name in different scopes
// Debug counters for frozen dispatch (remove after debugging)
var dispatch_hits: usize = 0;
var dispatch_misses: usize = 0;

pub export fn frozen_dispatch_lookup(
    ctx: *JSContext,
    func_name: [*:0]const u8,
    this_val: JSValue,
    argc: c_int,
    argv: [*]JSValue,
    var_refs: ?[*]*JSVarRef,
    closure_var_count: c_int,
    cpool: ?[*]JSValue,
    bytecode_ptr: ?*anyopaque,
    result_out: *JSValue,
) callconv(.c) c_int {
    // Skip if dispatch is not enabled yet (during initialization)
    if (!dispatch_enabled) return 0;

    // Quick check: if registry not initialized, skip lookup entirely
    if (name_registry == null) return 0;

    // Lookup frozen function by name@line_num key
    const func = lookup(func_name) orelse {
        dispatch_misses += 1;
        return 0;
    };

    dispatch_hits += 1;

    // Register this function by bytecode pointer for future closure calls
    // This is crucial for fclosure - nested functions created from cpool need
    // to be dispatchable by bytecode pointer when called via callbacks
    // Recursively registers all nested functions from cpool as well
    if (bytecode_ptr) |bptr| {
        registerFunctionAndCpool(ctx, bptr, func, 0);
    }

    // Call the frozen function with var_refs and cpool for closure/fclosure support
    if (is_wasm32) {
        // On WASM32, LLVM FastISel corrupts u64 returns. The frozen function writes
        // its result to split globals before returning. We read the two u32s separately
        // and reconstruct the JSValue to avoid any u64 operations.
        _ = func(ctx, this_val, argc, argv, var_refs, closure_var_count, cpool);
        // Read split globals and reconstruct JSValue
        const lo = zig_runtime.g_return_slot_lo.*;
        const hi = zig_runtime.g_return_slot_hi.*;
        const result_words: *[2]u32 = @ptrCast(@alignCast(result_out));
        result_words[0] = lo;
        result_words[1] = hi;
        // Set flag so callers can fix the return value
        g_frozen_dispatch_occurred = true;
    } else {
        result_out.* = func(ctx, this_val, argc, argv, var_refs, closure_var_count, cpool);
    }
    return 1;
}

/// Lookup and call a frozen function by bytecode pointer
/// Called from JS_CallInternal when executing a bytecode function
/// This is the closure-aware dispatch path - var_refs is extracted from the function object
/// Returns: 1 if frozen function was called (result in *result_out), 0 if not found
var bytecode_dispatch_hits: usize = 0;
var bytecode_dispatch_misses: usize = 0;

pub export fn frozen_dispatch_lookup_bytecode(
    ctx: *JSContext,
    bytecode_ptr: *anyopaque,
    this_val: JSValue,
    argc: c_int,
    argv: [*]JSValue,
    var_refs: ?[*]*JSVarRef, // Extracted from JSFunctionBytecode in QuickJS
    closure_var_count: c_int, // Number of closure vars for bounds checking
    cpool: ?[*]JSValue, // Constant pool for fclosure support
    result_out: *JSValue,
) callconv(.c) c_int {
    // Skip if dispatch is not enabled yet (during initialization)
    if (!dispatch_enabled) return 0;

    // Quick check: if registry not initialized, skip lookup entirely
    if (bytecode_registry == null) return 0;

    // Lookup frozen function by bytecode pointer
    const func = lookupByBytecode(bytecode_ptr) orelse {
        bytecode_dispatch_misses += 1;
        return 0;
    };

    bytecode_dispatch_hits += 1;

    // Call the frozen function with var_refs, closure_var_count, and cpool for closure/fclosure support
    if (is_wasm32) {
        // On WASM32, LLVM FastISel corrupts u64 returns. The frozen function writes
        // its result to split globals before returning. We read the two u32s separately
        // and reconstruct the JSValue to avoid any u64 operations.
        _ = func(ctx, this_val, argc, argv, var_refs, closure_var_count, cpool);
        // Read split globals and reconstruct JSValue
        const lo = zig_runtime.g_return_slot_lo.*;
        const hi = zig_runtime.g_return_slot_hi.*;
        const result_words: *[2]u32 = @ptrCast(@alignCast(result_out));
        result_words[0] = lo;
        result_words[1] = hi;
        // Set flag so callers can fix the return value
        g_frozen_dispatch_occurred = true;
    } else {
        result_out.* = func(ctx, this_val, argc, argv, var_refs, closure_var_count, cpool);
    }
    return 1;
}

/// Get the number of registered frozen functions
pub export fn frozen_dispatch_count() callconv(.c) c_int {
    if (name_registry) |reg| {
        return @intCast(reg.count());
    }
    return 0;
}

/// Debug: print dispatch stats (remove after debugging)
export fn frozen_dispatch_stats() callconv(.c) void {
    const name_count = if (name_registry) |reg| reg.count() else 0;
    const bc_count = if (bytecode_registry) |reg| reg.count() else 0;
    std.debug.print("[frozen] Name dispatch: {d} hits, {d} misses, {d} registered\n", .{ dispatch_hits, dispatch_misses, name_count });
    std.debug.print("[frozen] Bytecode dispatch: {d} hits, {d} misses, {d} registered\n", .{ bytecode_dispatch_hits, bytecode_dispatch_misses, bc_count });
}

/// Debug: get dispatch hits count (useful when debug print doesn't work)
export fn frozen_dispatch_get_hits() callconv(.c) c_int {
    return @intCast(dispatch_hits + bytecode_dispatch_hits);
}

/// Debug: get the last return slot values
export fn frozen_dispatch_get_return_lo() callconv(.c) u32 {
    return zig_runtime.g_return_slot_lo.*;
}

export fn frozen_dispatch_get_return_hi() callconv(.c) u32 {
    return zig_runtime.g_return_slot_hi.*;
}

/// Get the number of bytecode-registered frozen functions
export fn frozen_dispatch_bytecode_count() callconv(.c) c_int {
    if (bytecode_registry) |reg| {
        return @intCast(reg.count());
    }
    return 0;
}

/// Clear the registry (for testing)
pub fn clear() void {
    if (arena) |*a| {
        a.deinit();
    }
    arena = null;
    name_registry = null;
    bytecode_registry = null;
    dispatch_enabled = false;
    dispatch_hits = 0;
    dispatch_misses = 0;
    bytecode_dispatch_hits = 0;
    bytecode_dispatch_misses = 0;
}

// ============================================================================
// Tests
// ============================================================================

test "register and lookup" {
    clear();

    // Test function
    const testFn = struct {
        fn call(_: *JSContext, _: JSValue, _: c_int, _: [*]JSValue, _: ?[*]*JSVarRef, _: c_int, _: ?[*]JSValue) callconv(.c) JSValue {
            return JSValue.newInt(42);
        }
    }.call;

    register("testFunc", @ptrCast(&testFn));
    enableDispatch();

    const found = lookup("testFunc");
    try std.testing.expect(found != null);

    const not_found = lookup("nonexistent");
    try std.testing.expect(not_found == null);

    clear();
}

test "hash collision handling" {
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

    // Register many functions to test probing
    register("func1", @ptrCast(&fn1));
    register("func2", @ptrCast(&fn2));
    register("func3", @ptrCast(&fn1));
    register("func4", @ptrCast(&fn2));

    enableDispatch();

    try std.testing.expect(lookup("func1") != null);
    try std.testing.expect(lookup("func2") != null);
    try std.testing.expect(lookup("func3") != null);
    try std.testing.expect(lookup("func4") != null);
    try std.testing.expect(lookup("func5") == null);

    clear();
}
