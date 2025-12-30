//! Unified WASM Memory Helpers for EdgeBox
//!
//! Provides safe, bounds-checked access to WASM linear memory.
//! Consolidated from 3 duplicate implementations:
//! - host/stdlib.zig
//! - edgebox_wamr.zig
//! - edgeboxd_wamr.zig
//!
//! IMPORTANT: This module exports `c` (the WAMR C API types) so that all
//! files using WAMR share the same type definitions.

const std = @import("std");

// Import WAMR C API - exported so all files share the same types
pub const c = @cImport({
    @cInclude("wasm_export.h");
    @cInclude("lib_export.h");
});

/// Read bytes from WASM linear memory with bounds checking.
/// Returns null if ptr+len is out of bounds.
pub fn readWasmMemory(exec_env: c.wasm_exec_env_t, ptr: u32, len: u32) ?[]const u8 {
    if (len == 0) return "";
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return null;

    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, len)) {
        return null; // Out of bounds
    }
    const native_addr: ?*u8 = @ptrCast(c.wasm_runtime_addr_app_to_native(module_inst, ptr));
    if (native_addr == null) return null;
    return @as([*]const u8, @ptrCast(native_addr.?))[0..len];
}

/// Write bytes to WASM linear memory with bounds checking.
/// Returns false if ptr+len is out of bounds or data too large.
pub fn writeWasmMemory(exec_env: c.wasm_exec_env_t, ptr: u32, data: []const u8) bool {
    if (data.len == 0) return true;
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return false;

    // Security: Reject data larger than u32 max to prevent truncation in WAMR API
    if (data.len > 0xFFFFFFFF) return false;

    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, @intCast(data.len))) {
        return false; // Out of bounds
    }
    const native_addr: ?*u8 = @ptrCast(c.wasm_runtime_addr_app_to_native(module_inst, ptr));
    if (native_addr == null) return false;
    const dest = @as([*]u8, @ptrCast(native_addr.?))[0..data.len];
    @memcpy(dest, data);
    return true;
}

/// Write bytes to WASM linear memory (void return version).
/// Use when you don't need to check for errors (e.g., already validated).
pub fn writeWasmBuffer(exec_env: c.wasm_exec_env_t, ptr: u32, data: []const u8) void {
    _ = writeWasmMemory(exec_env, ptr, data);
}

/// SECURITY: Safe WASM memory slice access with bounds checking.
/// Handles i32 inputs safely: rejects negative values and integer overflow.
/// Uses WAMR's validate_app_addr for proper bounds checking.
pub fn safeWasmSlice(exec_env: c.wasm_exec_env_t, offset: i32, len: i32) ?[]u8 {
    // Reject negative values
    if (offset < 0 or len < 0) return null;

    const uoffset: u32 = @intCast(offset);
    const ulen: u32 = @intCast(len);

    // Check for integer overflow
    const end = @as(u64, uoffset) + @as(u64, ulen);
    if (end > 0xFFFFFFFF) return null;

    if (ulen == 0) return &[_]u8{};

    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return null;

    if (!c.wasm_runtime_validate_app_addr(module_inst, uoffset, ulen)) {
        return null;
    }

    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, uoffset);
    if (native_ptr == null) return null;

    const bytes: [*]u8 = @ptrCast(native_ptr);
    return bytes[0..ulen];
}

/// Get module instance from exec_env (common operation).
pub fn getModuleInst(exec_env: c.wasm_exec_env_t) ?c.wasm_module_inst_t {
    return c.wasm_runtime_get_module_inst(exec_env);
}

// Aliases for compatibility with different naming conventions
pub const readWasmString = readWasmMemory;

test "wasm_helpers basic" {
    // Note: actual testing requires a WAMR runtime
}
