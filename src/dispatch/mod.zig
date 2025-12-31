//! Dispatch Module - Unified Host Function Registration
//!
//! This module coordinates all dispatch handlers for the EdgeBox runtime:
//! - Crypto: Hash, HMAC, random bytes
//! - File: Async/sync file operations
//! - Socket: TCP socket operations
//! - HTTP: Client requests and server handling
//! - Spawn: Process execution with security controls
//! - GPU: WebGPU compute operations
//!
//! Usage:
//!   const dispatch = @import("dispatch/mod.zig");
//!   dispatch.init(allocator, config);
//!   dispatch.registerAll();

const std = @import("std");
const wasm_helpers = @import("../wasm_helpers.zig");
const c = wasm_helpers.c;

// Re-export dispatch modules
pub const crypto = @import("crypto.zig");
pub const file = @import("file.zig");
pub const socket = @import("socket.zig");
pub const http = @import("http.zig");
pub const spawn = @import("spawn.zig");
pub const gpu = @import("gpu.zig");

// Type alias for native symbols
const NativeSymbol = c.NativeSymbol;

// ============================================================================
// Symbol Tables
// ============================================================================

/// HTTP dispatch symbols
var g_http_symbols = [_]NativeSymbol{
    .{
        .symbol = "http_dispatch",
        .func_ptr = @ptrCast(@constCast(&http.__edgebox_http_dispatch)),
        .signature = "(iiiiiiiii)i",
        .attachment = null,
    },
};

/// Spawn dispatch symbols
var g_spawn_symbols = [_]NativeSymbol{
    .{
        .symbol = "spawn_dispatch",
        .func_ptr = @ptrCast(@constCast(&spawn.__edgebox_spawn_dispatch)),
        .signature = "(iiiii)i",
        .attachment = null,
    },
};

/// File dispatch symbols
var g_file_symbols = [_]NativeSymbol{
    .{
        .symbol = "file_dispatch",
        .func_ptr = @ptrCast(@constCast(&fileDispatchWrapper)),
        .signature = "(iiiii)i",
        .attachment = null,
    },
};

/// Crypto dispatch symbols
var g_crypto_symbols = [_]NativeSymbol{
    .{
        .symbol = "crypto_dispatch",
        .func_ptr = @ptrCast(@constCast(&cryptoDispatchWrapper)),
        .signature = "(iiiiiii)i",
        .attachment = null,
    },
};

/// Socket dispatch symbols
var g_socket_symbols = [_]NativeSymbol{
    .{
        .symbol = "socket_dispatch",
        .func_ptr = @ptrCast(@constCast(&socketDispatchWrapper)),
        .signature = "(iiii)i",
        .attachment = null,
    },
};

/// GPU dispatch symbols (from gpu.zig)
var g_gpu_symbols = [_]NativeSymbol{
    .{
        .symbol = "gpu_dispatch",
        .func_ptr = @ptrCast(@constCast(&gpuDispatchWrapper)),
        .signature = "(iiiiiii)i",
        .attachment = null,
    },
};

// ============================================================================
// Dispatch Wrappers
// These adapt the module dispatch functions to the WASM calling convention
// ============================================================================

/// Global registry reference (set by init)
var g_registry: ?*@import("../component/native_registry.zig").NativeRegistry = null;

fn fileDispatchWrapper(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32, a4: i32) callconv(.c) i32 {
    return file.dispatch(exec_env, g_registry, opcode, a1, a2, a3, a4);
}

fn cryptoDispatchWrapper(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32, a4: i32, a5: i32) callconv(.c) i32 {
    const registry = g_registry orelse return -1;
    return crypto.dispatch(exec_env, registry, opcode, a1, a2, a3, a4, a5);
}

fn socketDispatchWrapper(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32) callconv(.c) i32 {
    return socket.dispatch(exec_env, opcode, a1, a2, a3);
}

fn gpuDispatchWrapper(exec_env: c.wasm_exec_env_t, op: i32, arg1: i32, arg2: i32, arg3: i32, arg4: i32, result_ptr: i32, result_len: i32) callconv(.c) i32 {
    // GPU dispatch is handled by the gpu.zig module's internal symbols
    _ = exec_env;
    _ = op;
    _ = arg1;
    _ = arg2;
    _ = arg3;
    _ = arg4;
    _ = result_ptr;
    _ = result_len;
    // The actual GPU dispatch is registered via gpu.getSymbols()
    return -1;
}

// ============================================================================
// Public API
// ============================================================================

/// Initialize all dispatch modules with common dependencies
pub fn init(
    allocator: std.mem.Allocator,
    registry: *@import("../component/native_registry.zig").NativeRegistry,
) void {
    g_registry = registry;

    // Initialize individual modules
    crypto.setAllocator(allocator);
    file.setAllocator(allocator);
    socket.setAllocator(allocator);
}

/// Set config for modules that need it
pub fn setConfig(config: anytype) void {
    // File dispatch config
    if (@hasDecl(@TypeOf(config.*), "remapPath")) {
        file.setConfig(config);
    }

    // Socket dispatch config (port permissions)
    // spawn.setConfig expects spawn.Config type
}

/// Register all host function symbols with WAMR
pub fn registerAll() void {
    _ = c.wasm_runtime_register_natives("edgebox_http", &g_http_symbols, g_http_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_spawn", &g_spawn_symbols, g_spawn_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_file", &g_file_symbols, g_file_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_crypto", &g_crypto_symbols, g_crypto_symbols.len);
    _ = c.wasm_runtime_register_natives("edgebox_socket", &g_socket_symbols, g_socket_symbols.len);

    // GPU uses its own symbol table from gpu.zig
    const gpu_syms = gpu.getSymbols();
    _ = c.wasm_runtime_register_natives("edgebox_gpu", @ptrCast(gpu_syms.ptr), @intCast(gpu_syms.len));
}

/// Deinitialize all dispatch modules
pub fn deinit() void {
    crypto.deinit();
    file.deinit();
    gpu.deinitGpuSandbox();
    g_registry = null;
}

/// Reset dispatch state for new execution
pub fn reset() void {
    socket.reset();
    // Other modules may need reset functions added
}

// ============================================================================
// Direct Symbol Access (for advanced usage)
// ============================================================================

pub fn getHttpSymbols() []NativeSymbol {
    return &g_http_symbols;
}

pub fn getSpawnSymbols() []NativeSymbol {
    return &g_spawn_symbols;
}

pub fn getFileSymbols() []NativeSymbol {
    return &g_file_symbols;
}

pub fn getCryptoSymbols() []NativeSymbol {
    return &g_crypto_symbols;
}

pub fn getSocketSymbols() []NativeSymbol {
    return &g_socket_symbols;
}

pub fn getGpuSymbols() []const NativeSymbol {
    return gpu.getSymbols();
}
