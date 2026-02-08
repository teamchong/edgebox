/// Native Shapes Polyfill - JavaScript binding for native node registration
/// Exposes __edgebox_register_node() to JavaScript for populating the native registry
///
/// Usage from JavaScript:
///   globalThis.__edgebox_register_node(nodeObj, kind, flags, pos, end)
///
/// This allows TypeScript's factory to register nodes as they're created,
/// enabling the NATIVE_GET_* macros to bypass QuickJS property access.
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// Import the native registry functions from native_shapes.zig
extern fn native_registry_init() void;
extern fn native_registry_deinit() void;
extern fn native_node_register(js_addr: u64, kind: i32, flags: i32, pos: i32, end: i32) ?*anyopaque;
extern fn native_node_register32(js_addr32: u32, kind: i32, flags: i32, pos: i32, end: i32) ?*anyopaque;
extern fn native_registry_count() c_int;
extern fn native_debug_get_last_lookup() u64;
extern fn native_debug_get_found() c_int;
extern fn native_debug_get_call_count() c_int;
extern fn native_debug_get_registered_addr() u64;
extern fn native_debug_get_register_success() c_int;
extern fn native_debug_get_register32_addr() u32;
extern fn native_debug_get_register32_called() c_int;
extern fn native_shapes_init_atoms(atom_kind: u32, atom_flags: u32, atom_pos: u32, atom_end: u32) void;


/// Extract object pointer from JSValue for registry lookup
/// Handles both NaN-boxing (WASM/32-bit) and native struct (64-bit) modes
fn jsvalueToAddr(val: qjs.JSValue) u64 {
    // Check if we're using NaN-boxed representation (u64) or struct representation
    const js_is_nan_boxed = @sizeOf(qjs.JSValue) == 8;
    if (comptime js_is_nan_boxed) {
        // NaN boxing mode (WASM): JSValue is uint64_t with pointer in low 32 bits
        const as_u64: u64 = @bitCast(val);
        const low32: u32 = @truncate(as_u64);
        return @as(u64, low32);
    } else {
        // Native struct mode: JSValue has u.ptr field
        return @intFromPtr(val.u.ptr);
    }
}

/// __edgebox_register_node(nodeObj, kind, flags, pos, end)
/// Registers a JavaScript object in the native registry for fast property access
fn registerNode(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 5) {
        return qjs.JS_ThrowTypeError(ctx, "registerNode requires 5 arguments: obj, kind, flags, pos, end");
    }

    const obj = argv[0];

    // Only register objects
    if (qjs.JS_VALUE_GET_TAG(obj) != qjs.JS_TAG_OBJECT) {
        return quickjs.jsUndefined();
    }

    // Extract int arguments
    var kind: i32 = 0;
    var flags: i32 = 0;
    var pos: i32 = 0;
    var end: i32 = 0;

    if (qjs.JS_ToInt32(ctx, &kind, argv[1]) < 0) return quickjs.jsException();
    if (qjs.JS_ToInt32(ctx, &flags, argv[2]) < 0) return quickjs.jsException();
    if (qjs.JS_ToInt32(ctx, &pos, argv[3]) < 0) return quickjs.jsException();
    if (qjs.JS_ToInt32(ctx, &end, argv[4]) < 0) return quickjs.jsException();

    // Get the JSValue address for registry key
    const js_addr = jsvalueToAddr(obj);

    // Use 64-bit address for native builds, 32-bit for WASM
    const js_is_wasm = @sizeOf(usize) == 4;
    const node = if (comptime js_is_wasm) blk: {
        const js_addr32: u32 = @truncate(js_addr);
        break :blk native_node_register32(js_addr32, kind, flags, pos, end);
    } else native_node_register(js_addr, kind, flags, pos, end);

    // Return true if registered, false if failed
    return if (node != null) quickjs.jsTrue() else quickjs.jsFalse();
}

/// __edgebox_registry_count()
/// Returns the number of registered nodes (for debugging)
fn getRegistryCount(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const count = native_registry_count();
    return qjs.JS_NewInt32(ctx, count);
}

/// __edgebox_get_addr(obj) - Debug: Get JSValue address used for registry key
fn getAddr(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "getAddr requires 1 argument");
    }

    const obj = argv[0];
    const addr = jsvalueToAddr(obj);

    // Return as Float64 since BigInt is disabled in WASM builds
    return qjs.JS_NewFloat64(ctx, @floatFromInt(addr));
}

/// __edgebox_debug_last_lookup() - Get last lookup info from C frozen code
fn getLastLookup(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const obj = qjs.JS_NewObject(ctx);
    // Use Float64 for addr since BigInt is disabled in WASM builds
    const addr = native_debug_get_last_lookup();
    _ = qjs.JS_SetPropertyStr(ctx, obj, "addr", qjs.JS_NewFloat64(ctx, @floatFromInt(addr)));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "found", quickjs.jsNewBool( native_debug_get_found() != 0));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "calls", qjs.JS_NewInt32(ctx, native_debug_get_call_count()));

    // Also include registration debug info
    const reg_addr = native_debug_get_registered_addr();
    _ = qjs.JS_SetPropertyStr(ctx, obj, "registeredAddr", qjs.JS_NewFloat64(ctx, @floatFromInt(reg_addr)));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "registerSuccess", quickjs.jsNewBool( native_debug_get_register_success() != 0));

    // Also include 32-bit registration debug info
    _ = qjs.JS_SetPropertyStr(ctx, obj, "register32Addr", qjs.JS_NewInt32(ctx, @bitCast(native_debug_get_register32_addr())));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "register32Called", qjs.JS_NewInt32(ctx, native_debug_get_register32_called()));
    return obj;
}



/// Initialize the native registry (call once at startup)
pub fn init() void {
    native_registry_init();
}

/// Clean up the native registry
pub fn deinit() void {
    native_registry_deinit();
}

/// Initialize atoms for fast property lookup
/// MUST be called AFTER bytecode is loaded to get correct atom values
pub fn initAtoms(ctx: *qjs.JSContext) void {
    // These atoms are looked up once here and cached in native_shapes.zig
    // Must be called after bytecode loading so atoms match those in bytecode
    const atom_kind = qjs.JS_NewAtom(ctx, "kind");
    const atom_flags = qjs.JS_NewAtom(ctx, "flags");
    const atom_pos = qjs.JS_NewAtom(ctx, "pos");
    const atom_end = qjs.JS_NewAtom(ctx, "end");
    native_shapes_init_atoms(atom_kind, atom_flags, atom_pos, atom_end);
    // Don't free these atoms - they're cached and used throughout execution
}

/// Register native shape functions to globalThis
/// Called ONCE at WASM initialization
pub fn register(ctx: *qjs.JSContext) void {
    // Initialize the registry
    native_registry_init();

    // NOTE: Don't init atoms here - must be done after bytecode loading
    // Call initAtoms(ctx) separately after JS_ReadObject

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Register __edgebox_register_node(obj, kind, flags, pos, end)
    const register_fn = qjs.JS_NewCFunction(ctx, registerNode, "__edgebox_register_node", 5);
    _ = qjs.JS_SetPropertyStr(ctx, global, "__edgebox_register_node", register_fn);

    // Register __edgebox_registry_count() for debugging
    const count_fn = qjs.JS_NewCFunction(ctx, getRegistryCount, "__edgebox_registry_count", 0);
    _ = qjs.JS_SetPropertyStr(ctx, global, "__edgebox_registry_count", count_fn);

    // Register __edgebox_get_addr(obj) for debugging
    const addr_fn = qjs.JS_NewCFunction(ctx, getAddr, "__edgebox_get_addr", 1);
    _ = qjs.JS_SetPropertyStr(ctx, global, "__edgebox_get_addr", addr_fn);

    // Register __edgebox_debug_last_lookup() for debugging
    const lookup_fn = qjs.JS_NewCFunction(ctx, getLastLookup, "__edgebox_debug_last_lookup", 0);
    _ = qjs.JS_SetPropertyStr(ctx, global, "__edgebox_debug_last_lookup", lookup_fn);
}
