//! Native Frozen Function Dispatch
//!
//! Provides a fast hash table lookup for frozen functions, called directly
//! from the QuickJS bytecode interpreter (JS_CallInternal) via C FFI.
//!
//! This eliminates the overhead of JS property lookups (globalThis.__frozen_NAME)
//! by doing the dispatch in native code before bytecode execution.

const std = @import("std");
const zig_runtime = @import("zig_runtime");

const JSContext = zig_runtime.JSContext;
const JSValue = zig_runtime.JSValue;
const JSVarRef = zig_runtime.JSVarRef;
const qjs = zig_runtime.quickjs;

/// C function pointer type for frozen functions (includes var_refs for closure access)
pub const FrozenFnPtr = *const fn (*JSContext, JSValue, c_int, [*]JSValue, ?[*]*JSVarRef) callconv(.c) JSValue;

/// Maximum number of frozen functions we can register
const MAX_FROZEN_FUNCTIONS = 16384;

/// Registry entry
const RegistryEntry = struct {
    name_hash: u64,
    name: [*:0]const u8,
    func: FrozenFnPtr,
};

/// Global registry - simple array with linear probing
/// Using a fixed array avoids allocation issues and is fast enough for our use case
var registry: [MAX_FROZEN_FUNCTIONS]?RegistryEntry = [_]?RegistryEntry{null} ** MAX_FROZEN_FUNCTIONS;
var registry_count: usize = 0;

/// Hash function for function names (FNV-1a)
fn hashName(name: [*:0]const u8) u64 {
    var hash: u64 = 0xcbf29ce484222325; // FNV offset basis
    var i: usize = 0;
    while (name[i] != 0) : (i += 1) {
        hash ^= @as(u64, name[i]);
        hash *%= 0x100000001b3; // FNV prime
    }
    return hash;
}

/// Register a frozen function in the native registry
/// Called from frozen_init during startup
pub fn register(name: [*:0]const u8, func: FrozenFnPtr) void {
    if (registry_count >= MAX_FROZEN_FUNCTIONS) {
        std.debug.print("[native_dispatch] Registry full, cannot register {s}\n", .{name});
        return;
    }

    const hash = hashName(name);
    var idx: usize = @intCast(hash % MAX_FROZEN_FUNCTIONS);

    // Linear probing to find empty slot
    var probes: usize = 0;
    while (registry[idx] != null and probes < MAX_FROZEN_FUNCTIONS) {
        idx = (idx + 1) % MAX_FROZEN_FUNCTIONS;
        probes += 1;
    }

    if (probes >= MAX_FROZEN_FUNCTIONS) {
        std.debug.print("[native_dispatch] Registry full (probing exhausted)\n", .{});
        return;
    }

    registry[idx] = .{
        .name_hash = hash,
        .name = name,
        .func = func,
    };
    registry_count += 1;
}

/// Lookup a frozen function by name
/// Returns null if not found
fn lookup(name: [*:0]const u8) ?FrozenFnPtr {
    const hash = hashName(name);
    var idx: usize = @intCast(hash % MAX_FROZEN_FUNCTIONS);

    var probes: usize = 0;
    while (probes < MAX_FROZEN_FUNCTIONS) {
        const entry = registry[idx];
        if (entry == null) {
            return null; // Empty slot means not found
        }
        if (entry.?.name_hash == hash) {
            // Verify name matches (handle hash collisions)
            var i: usize = 0;
            var matches = true;
            while (entry.?.name[i] != 0 or name[i] != 0) {
                if (entry.?.name[i] != name[i]) {
                    matches = false;
                    break;
                }
                i += 1;
            }
            if (matches) {
                return entry.?.func;
            }
        }
        idx = (idx + 1) % MAX_FROZEN_FUNCTIONS;
        probes += 1;
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
    result_out: *JSValue,
) callconv(.c) c_int {
    // Skip if dispatch is not enabled yet (during initialization)
    if (!dispatch_enabled) return 0;

    // Quick check: if no functions registered, skip lookup entirely
    if (registry_count == 0) return 0;

    // Lookup frozen function by name@line_num key
    const func = lookup(func_name) orelse {
        dispatch_misses += 1;
        return 0;
    };

    dispatch_hits += 1;
    // Debug: print first few hits (disabled for production)
    // if (dispatch_hits <= 5) {
    //     std.debug.print("[frozen] HIT #{d}: {s}\n", .{ dispatch_hits, func_name });
    // }

    // Call the frozen function with var_refs for closure access
    result_out.* = func(ctx, this_val, argc, argv, var_refs);
    return 1;
}

/// Get the number of registered frozen functions
export fn frozen_dispatch_count() callconv(.c) c_int {
    return @intCast(registry_count);
}

/// Debug: print dispatch stats (remove after debugging)
export fn frozen_dispatch_stats() callconv(.c) void {
    std.debug.print("[frozen] Dispatch stats: {d} hits, {d} misses, {d} registered\n", .{ dispatch_hits, dispatch_misses, registry_count });
}

/// Clear the registry (for testing)
pub fn clear() void {
    registry = [_]?RegistryEntry{null} ** MAX_FROZEN_FUNCTIONS;
    registry_count = 0;
    dispatch_enabled = false;
}

// ============================================================================
// Tests
// ============================================================================

test "register and lookup" {
    clear();

    // Test function
    const testFn = struct {
        fn call(_: *JSContext, _: JSValue, _: c_int, _: [*]JSValue, _: ?[*]*JSVarRef) callconv(.c) JSValue {
            return JSValue.newInt(42);
        }
    }.call;

    register("testFunc", &testFn);
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
        fn call(_: *JSContext, _: JSValue, _: c_int, _: [*]JSValue, _: ?[*]*JSVarRef) callconv(.c) JSValue {
            return JSValue.newInt(1);
        }
    }.call;

    const fn2 = struct {
        fn call(_: *JSContext, _: JSValue, _: c_int, _: [*]JSValue, _: ?[*]*JSVarRef) callconv(.c) JSValue {
            return JSValue.newInt(2);
        }
    }.call;

    // Register many functions to test probing
    register("func1", &fn1);
    register("func2", &fn2);
    register("func3", &fn1);
    register("func4", &fn2);

    enableDispatch();

    try std.testing.expect(lookup("func1") != null);
    try std.testing.expect(lookup("func2") != null);
    try std.testing.expect(lookup("func3") != null);
    try std.testing.expect(lookup("func4") != null);
    try std.testing.expect(lookup("func5") == null);

    clear();
}
