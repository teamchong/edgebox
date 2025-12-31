// =============================================================================
// WebGPU host functions - GPU sandbox dispatch
// =============================================================================

const std = @import("std");
const wasm_helpers = @import("../wasm_helpers.zig");
const gpu_sandbox = @import("../gpu_sandbox.zig");

const c = wasm_helpers.c;
const NativeSymbol = c.NativeSymbol;

// Global state for GPU sandbox
var g_gpu_sandbox: ?*gpu_sandbox.GpuSandbox = null;
var g_gpu_allocator: ?std.mem.Allocator = null;

// Debug logging
const DAEMON_DEBUG = true;
fn daemonLog(comptime fmt: []const u8, args: anytype) void {
    if (DAEMON_DEBUG) {
        std.debug.print(fmt, args);
    }
}

// =============================================================================
// Public API
// =============================================================================

/// Initialize GPU sandbox with given config
/// Called from daemon startup if GPU is enabled
pub fn initGpuSandbox(alloc: std.mem.Allocator, config: gpu_sandbox.GpuConfig) !void {
    if (g_gpu_sandbox != null) {
        return; // Already initialized
    }

    g_gpu_allocator = alloc;
    const sandbox = try alloc.create(gpu_sandbox.GpuSandbox);
    sandbox.* = try gpu_sandbox.GpuSandbox.init(alloc, config);
    g_gpu_sandbox = sandbox;

    daemonLog("[gpu] GPU sandbox initialized (enabled={})\n", .{config.enabled});
}

/// Deinitialize GPU sandbox
pub fn deinitGpuSandbox() void {
    if (g_gpu_sandbox) |sandbox| {
        sandbox.deinit();
        if (g_gpu_allocator) |alloc| {
            alloc.destroy(sandbox);
        }
        g_gpu_sandbox = null;
        g_gpu_allocator = null;
        daemonLog("[gpu] GPU sandbox deinitialized\n", .{});
    }
}

/// Get native symbols for GPU dispatch
pub fn getSymbols() []const NativeSymbol {
    return &g_gpu_symbols;
}

// =============================================================================
// Native Symbol Registration
// =============================================================================

var g_gpu_symbols = [_]NativeSymbol{
    // GPU dispatch: (op, arg1, arg2, arg3, arg4, result_ptr, result_len) -> status
    // op: operation code (see GpuOp enum)
    // args: operation-specific parameters
    // result_ptr/len: where to write result data
    .{ .symbol = "gpu_dispatch", .func_ptr = @ptrCast(@constCast(&gpuDispatch)), .signature = "(iiiiiii)i", .attachment = null },
};

// =============================================================================
// GPU Operations
// =============================================================================

const GpuOp = enum(i32) {
    // Status
    is_available = 0,
    get_error = 1,

    // Device
    request_adapter = 10,
    request_device = 11,

    // Buffers
    create_buffer = 20,
    destroy_buffer = 21,
    write_buffer = 22,
    read_buffer = 23,

    // Shaders
    create_shader_module = 30,
    destroy_shader_module = 31,

    // Pipelines
    create_compute_pipeline = 40,
    create_bind_group = 41,

    // Execution
    dispatch_workgroups = 50,
    queue_submit = 51,
};

// GPU error codes (matches gpu_sandbox.ResponseCode)
const GPU_ERROR_NOT_AVAILABLE: i32 = -1;
const GPU_ERROR_INVALID_ARGS: i32 = -2;
const GPU_ERROR_DISPATCH_LIMIT: i32 = -4;
const GPU_ERROR_MEMORY_LIMIT: i32 = -6;
const GPU_ERROR_SHADER_INVALID: i32 = -8;

// =============================================================================
// Memory Helpers
// =============================================================================

/// Read slice from WASM memory for GPU operations
fn gpuReadWasmSlice(exec_env: c.wasm_exec_env_t, ptr: u32, len: u32) ?[]const u8 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return null;
    if (len == 0) return &[_]u8{};
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, len)) return null;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return null;
    const bytes: [*]const u8 = @ptrCast(native_ptr);
    return bytes[0..len];
}

/// Write slice to WASM memory for GPU operations
fn gpuWriteWasmSlice(exec_env: c.wasm_exec_env_t, ptr: u32, data: []const u8) bool {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return false;
    if (data.len == 0) return true;
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, @intCast(data.len))) return false;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return false;
    const dest: [*]u8 = @ptrCast(native_ptr);
    @memcpy(dest[0..data.len], data);
    return true;
}

// =============================================================================
// GPU Dispatch Function
// =============================================================================

/// GPU dispatch - routes WebGPU calls to GPU sandbox
/// Returns: handle/status on success (>=0), negative error code on failure
fn gpuDispatch(
    exec_env: c.wasm_exec_env_t,
    op: i32,
    arg1: i32,
    arg2: i32,
    arg3: i32,
    arg4: i32,
    result_ptr: i32,
    result_len: i32,
) i32 {
    const operation: GpuOp = @enumFromInt(op);

    // Get sandbox (may be null if GPU disabled)
    const sandbox = g_gpu_sandbox orelse {
        // GPU not available - return 0 for is_available, -1 for others
        return if (operation == .is_available) 0 else GPU_ERROR_NOT_AVAILABLE;
    };

    switch (operation) {
        .is_available => {
            return if (sandbox.isAvailable()) 1 else 0;
        },

        .get_error => {
            // Error message retrieval (not implemented yet - would need error buffer)
            return 0;
        },

        .request_adapter, .request_device => {
            // GPU initialization is done automatically by sandbox.init()
            // These are no-ops if already initialized
            return if (sandbox.device_ready) 0 else GPU_ERROR_NOT_AVAILABLE;
        },

        .create_buffer => {
            // arg1 = size (low 32 bits), arg2 = size (high 32 bits for u64)
            // arg3 = usage flags
            const size: u64 = @as(u64, @intCast(@as(u32, @bitCast(arg1)))) |
                (@as(u64, @intCast(@as(u32, @bitCast(arg2)))) << 32);
            const usage: gpu_sandbox.GpuSandbox.BufferUsage = @enumFromInt(@as(u32, @bitCast(arg3)));

            const handle = sandbox.createBuffer(size, usage) catch |err| {
                daemonLog("[gpu] createBuffer failed: {}\n", .{err});
                return GPU_ERROR_MEMORY_LIMIT;
            };
            return @intCast(handle);
        },

        .destroy_buffer => {
            const handle: u32 = @intCast(@as(u32, @bitCast(arg1)));
            sandbox.destroyBuffer(handle) catch {};
            return 0;
        },

        .write_buffer => {
            // arg1 = buffer handle, arg2 = wasm_ptr, arg3 = len, arg4 = offset (low bits)
            const handle: u32 = @intCast(@as(u32, @bitCast(arg1)));
            const wasm_ptr: u32 = @intCast(@as(u32, @bitCast(arg2)));
            const len: u32 = @intCast(@as(u32, @bitCast(arg3)));
            const offset: u64 = @intCast(@as(u32, @bitCast(arg4)));

            const data = gpuReadWasmSlice(exec_env, wasm_ptr, len) orelse {
                return GPU_ERROR_INVALID_ARGS;
            };

            sandbox.writeBuffer(handle, offset, data) catch |err| {
                daemonLog("[gpu] writeBuffer failed: {}\n", .{err});
                return GPU_ERROR_MEMORY_LIMIT;
            };
            return 0;
        },

        .read_buffer => {
            // arg1 = buffer handle, arg2 = offset (low), arg3 = size, result_ptr = destination
            const handle: u32 = @intCast(@as(u32, @bitCast(arg1)));
            const offset: u64 = @intCast(@as(u32, @bitCast(arg2)));
            const size: u64 = @intCast(@as(u32, @bitCast(arg3)));

            const data = sandbox.readBuffer(handle, offset, size) catch |err| {
                daemonLog("[gpu] readBuffer failed: {}\n", .{err});
                return GPU_ERROR_MEMORY_LIMIT;
            };
            defer if (g_gpu_allocator) |alloc| alloc.free(data);

            const dest_ptr: u32 = @intCast(@as(u32, @bitCast(result_ptr)));
            if (!gpuWriteWasmSlice(exec_env, dest_ptr, data)) {
                return GPU_ERROR_INVALID_ARGS;
            }
            return @intCast(data.len);
        },

        .create_shader_module => {
            // arg1 = wgsl_ptr, arg2 = wgsl_len
            const wgsl_ptr: u32 = @intCast(@as(u32, @bitCast(arg1)));
            const wgsl_len: u32 = @intCast(@as(u32, @bitCast(arg2)));

            const wgsl = gpuReadWasmSlice(exec_env, wgsl_ptr, wgsl_len) orelse {
                return GPU_ERROR_INVALID_ARGS;
            };

            const handle = sandbox.createShaderModule(wgsl) catch |err| {
                daemonLog("[gpu] createShaderModule failed: {}\n", .{err});
                return GPU_ERROR_SHADER_INVALID;
            };
            return @intCast(handle);
        },

        .destroy_shader_module => {
            // Not exposed in current gpu_sandbox API - no-op
            return 0;
        },

        .create_compute_pipeline => {
            // arg1 = shader_handle, arg2 = entry_ptr, arg3 = entry_len
            const shader_handle: u32 = @intCast(@as(u32, @bitCast(arg1)));
            const entry_ptr: u32 = @intCast(@as(u32, @bitCast(arg2)));
            const entry_len: u32 = @intCast(@as(u32, @bitCast(arg3)));

            const entry_point = gpuReadWasmSlice(exec_env, entry_ptr, entry_len) orelse {
                return GPU_ERROR_INVALID_ARGS;
            };

            const handle = sandbox.createComputePipeline(shader_handle, entry_point) catch |err| {
                daemonLog("[gpu] createComputePipeline failed: {}\n", .{err});
                return GPU_ERROR_SHADER_INVALID;
            };
            return @intCast(handle);
        },

        .create_bind_group => {
            // Bind group creation not directly exposed - handled internally
            return GPU_ERROR_NOT_AVAILABLE;
        },

        .dispatch_workgroups => {
            // arg1 = x, arg2 = y, arg3 = z workgroups
            const x: u32 = @intCast(@as(u32, @bitCast(arg1)));
            const y: u32 = @intCast(@as(u32, @bitCast(arg2)));
            const z: u32 = @intCast(@as(u32, @bitCast(arg3)));

            sandbox.dispatch(x, y, z) catch |err| {
                daemonLog("[gpu] dispatch failed: {}\n", .{err});
                return GPU_ERROR_DISPATCH_LIMIT;
            };
            return 0;
        },

        .queue_submit => {
            // Queue submit is implicit in dispatch - no-op
            return 0;
        },
    }

    _ = result_len; // Unused in most operations
}
