//! WASM Component Host Functions
//!
//! Host bindings for WASM Component Model operations.
//! These functions are registered with WAMR and called from WASM code
//! to load, inspect, and call WASM components.

const std = @import("std");
const wasm_helpers = @import("../wasm_helpers.zig");
const c = wasm_helpers.c;
const wasm_component = @import("wasm_component.zig");

// Type alias for native symbols
const NativeSymbol = c.NativeSymbol;

// =============================================================================
// WASM Component Host Functions
// =============================================================================

/// Host function: Load WASM component and return component ID
/// Args: path (offset, len in WASM memory)
/// Returns: component ID (>0) or 0 on error
fn load(
    exec_env: c.wasm_exec_env_t,
    path_offset: i32,
    path_len: i32,
) callconv(.c) i32 {
    // SECURITY: Use safe bounds-checked memory access
    const path_slice = wasm_helpers.safeWasmSlice(exec_env, path_offset, path_len) orelse return 0;

    const registry = wasm_component.getGlobalRegistry() orelse {
        std.debug.print("[WASM Component] Registry not initialized\n", .{});
        return 0;
    };

    const component = registry.loadComponent(path_slice) catch |err| {
        // SECURITY: Sanitize path in error message
        std.debug.print("[WASM Component] Failed to load module: {}\n", .{err});
        return 0;
    };

    // Return component pointer as ID (validated via registry lookup on use)
    return @intCast(@intFromPtr(component));
}

/// Host function: Get export count from loaded component
/// Args: component_id
/// Returns: export count
fn exportCount(
    _: c.wasm_exec_env_t,
    component_id: i32,
) callconv(.c) i32 {
    if (component_id <= 0) return 0;

    // SECURITY: Validate component_id is a registered component pointer
    const registry = wasm_component.getGlobalRegistry() orelse return 0;
    const component = registry.getComponentById(@intCast(component_id)) orelse return 0;

    return @intCast(component.exports.count());
}

/// Host function: Get export name by index
/// Args: component_id, index, name_buf_offset, name_buf_len
/// Returns: actual name length (0 if out of bounds)
fn exportName(
    exec_env: c.wasm_exec_env_t,
    component_id: i32,
    index: i32,
    name_buf_offset: i32,
    name_buf_len: i32,
) callconv(.c) i32 {
    if (component_id <= 0 or index < 0) return 0;

    // SECURITY: Use safe bounds-checked memory access (mutable for writing)
    const name_buf = wasm_helpers.safeWasmSlice(exec_env, name_buf_offset, name_buf_len) orelse return 0;

    // SECURITY: Validate component_id is a registered component pointer
    const registry = wasm_component.getGlobalRegistry() orelse return 0;
    const component = registry.getComponentById(@intCast(component_id)) orelse return 0;

    // Iterate to find the export at this index
    var iter = component.exports.iterator();
    var i: usize = 0;
    while (iter.next()) |entry| : (i += 1) {
        if (i == @as(usize, @intCast(index))) {
            const name = entry.key_ptr.*;
            const copy_len = @min(name.len, name_buf.len);
            @memcpy(name_buf[0..copy_len], name[0..copy_len]);
            return @intCast(name.len);
        }
    }

    return 0;
}

/// Host function: Call WASM component export
/// Args: component_id, func_name (offset, len), args (offset, count)
/// Returns: i32 result
fn call(
    exec_env: c.wasm_exec_env_t,
    component_id: i32,
    func_name_offset: i32,
    func_name_len: i32,
    args_offset: i32,
    args_count: i32,
) callconv(.c) i32 {
    if (component_id <= 0 or args_count < 0) return 0;

    // SECURITY: Use safe bounds-checked memory access for function name
    const func_name = wasm_helpers.safeWasmSlice(exec_env, func_name_offset, func_name_len) orelse return 0;

    // SECURITY: Validate component_id is a registered component pointer
    const registry = wasm_component.getGlobalRegistry() orelse return 0;
    const component = registry.getComponentById(@intCast(component_id)) orelse return 0;

    // Get args array with safe memory access
    var args: [16]i32 = undefined;
    const arg_count = @min(@as(usize, @intCast(args_count)), 16);
    if (arg_count > 0) {
        // SECURITY: Safe bounds-checked memory access for args (i32 array = 4 bytes per element)
        const args_bytes: i32 = @intCast(arg_count * 4);
        const args_mem = wasm_helpers.safeWasmSlice(exec_env, args_offset, args_bytes) orelse return 0;
        // Read i32 values safely (handles unaligned access)
        for (0..arg_count) |i| {
            const offset = i * 4;
            args[i] = std.mem.readInt(i32, args_mem[offset..][0..4], .little);
        }
    }

    const result = registry.callExport(component, func_name, args[0..arg_count]) catch |err| {
        std.debug.print("[WASM Component] Call failed: {}\n", .{err});
        return 0;
    };

    return result;
}

// =============================================================================
// Symbol Table
// =============================================================================

/// WASM Component host function symbols for WAMR registration
var g_symbols = [_]NativeSymbol{
    .{ .symbol = "wasm_component_load", .func_ptr = @ptrCast(@constCast(&load)), .signature = "(ii)i", .attachment = null },
    .{ .symbol = "wasm_component_export_count", .func_ptr = @ptrCast(@constCast(&exportCount)), .signature = "(i)i", .attachment = null },
    .{ .symbol = "wasm_component_export_name", .func_ptr = @ptrCast(@constCast(&exportName)), .signature = "(iiii)i", .attachment = null },
    .{ .symbol = "wasm_component_call", .func_ptr = @ptrCast(@constCast(&call)), .signature = "(iiiii)i", .attachment = null },
};

// =============================================================================
// Public API
// =============================================================================

/// Get the symbol table for WAMR registration
pub fn getSymbols() []NativeSymbol {
    return &g_symbols;
}

/// Register symbols with WAMR
pub fn registerAll() void {
    _ = c.wasm_runtime_register_natives("edgebox_wasm_component", &g_symbols, g_symbols.len);
}
