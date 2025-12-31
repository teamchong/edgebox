//! Crypto Dispatch - Component Model Integration
//!
//! Handles cryptographic operations via the Component Model registry:
//! - Hash (SHA256, SHA384, SHA512, SHA1, MD5)
//! - HMAC
//! - Random bytes generation

const std = @import("std");
const wasm_helpers = @import("../wasm_helpers.zig");
const c = wasm_helpers.c;
const NativeRegistry = @import("../component/native_registry.zig").NativeRegistry;
const Value = @import("../component/native_registry.zig").Value;

// Crypto dispatch opcodes
pub const OP_HASH: i32 = 0;
pub const OP_HMAC: i32 = 1;
pub const OP_RANDOM_BYTES: i32 = 2;
pub const OP_GET_RESULT_LEN: i32 = 3;
pub const OP_GET_RESULT: i32 = 4;

// Crypto result buffer (single-threaded, reused between calls)
var g_result: ?[]const u8 = null;
var g_result_allocator: std.mem.Allocator = std.heap.page_allocator;

/// Set the allocator for crypto results
pub fn setAllocator(allocator: std.mem.Allocator) void {
    g_result_allocator = allocator;
}

/// Free any stored crypto result
pub fn deinit() void {
    if (g_result) |old| {
        g_result_allocator.free(old);
        g_result = null;
    }
}

/// Main dispatch function for crypto operations
pub fn dispatch(
    exec_env: c.wasm_exec_env_t,
    registry: *NativeRegistry,
    opcode: i32,
    a1: i32,
    a2: i32,
    a3: i32,
    a4: i32,
    a5: i32,
) i32 {
    switch (opcode) {
        OP_HASH => {
            // a1=algorithm (0=sha256, 1=sha384, 2=sha512, 3=sha1, 4=md5)
            // a2=data_ptr, a3=data_len
            // Returns: 0=success (result in buffer), -1=error
            const data = readStringFromWasm(exec_env, @bitCast(a2), @bitCast(a3)) orelse return -1;

            const args = [_]Value{
                Value{ .u32 = @bitCast(a1) },
                Value{ .string = data },
            };

            const result = registry.call("crypto", "hash", &args) catch return -1;

            if (result.isOk()) {
                const hex_string = result.asOkString() catch return -1;
                storeResult(hex_string) catch return -1;
                return 0;
            } else {
                return -1;
            }
        },
        OP_HMAC => {
            // a1=algorithm (0=sha256, 1=sha384, 2=sha512)
            // a2=key_ptr, a3=key_len, a4=data_ptr, a5=data_len
            // Returns: 0=success (result in buffer), -1=error
            const key = readStringFromWasm(exec_env, @bitCast(a2), @bitCast(a3)) orelse return -1;
            const data = readStringFromWasm(exec_env, @bitCast(a4), @bitCast(a5)) orelse return -1;

            const args = [_]Value{
                Value{ .u32 = @bitCast(a1) },
                Value{ .string = key },
                Value{ .string = data },
            };

            const result = registry.call("crypto", "hmac", &args) catch return -1;

            if (result.isOk()) {
                const hex_string = result.asOkString() catch return -1;
                storeResult(hex_string) catch return -1;
                return 0;
            } else {
                return -1;
            }
        },
        OP_RANDOM_BYTES => {
            // a1=size
            // Returns: 0=success (result in buffer), -1=error
            const args = [_]Value{
                Value{ .u32 = @bitCast(a1) },
            };

            const result = registry.call("crypto", "random-bytes", &args) catch return -1;

            if (result.isOk()) {
                const bytes = result.asOkListU8() catch return -1;
                storeResult(bytes) catch return -1;
                return 0;
            } else {
                return -1;
            }
        },
        OP_GET_RESULT_LEN => {
            // Returns: length of last result, or -1 if no result
            if (g_result) |crypto_result| {
                return @intCast(crypto_result.len);
            }
            return -1;
        },
        OP_GET_RESULT => {
            // a1=out_ptr (WASM memory address to write result)
            // Returns: bytes written, or -1 if error
            if (g_result) |crypto_result| {
                if (writeStringToWasm(exec_env, @bitCast(a1), crypto_result)) {
                    return @intCast(crypto_result.len);
                }
            }
            return -1;
        },
        else => return -1,
    }
}

// ============================================================================
// Internal helpers
// ============================================================================

fn storeResult(data: []const u8) !void {
    if (g_result) |old| {
        g_result_allocator.free(old);
    }
    g_result = try g_result_allocator.dupe(u8, data);
}

fn readStringFromWasm(exec_env: c.wasm_exec_env_t, ptr: u32, len: u32) ?[]const u8 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return null;
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, len)) return null;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return null;
    const bytes: [*]const u8 = @ptrCast(native_ptr);
    return bytes[0..len];
}

fn writeStringToWasm(exec_env: c.wasm_exec_env_t, ptr: u32, data: []const u8) bool {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return false;
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, @intCast(data.len))) return false;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return false;
    const dest: [*]u8 = @ptrCast(native_ptr);
    @memcpy(dest[0..data.len], data);
    return true;
}
