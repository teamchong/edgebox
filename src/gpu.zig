// GPU Module - Re-exports from metal0/packages/gpu
//
// This file provides backward compatibility for edgebox code that
// previously used gpu_sandbox.zig and gpu_worker.zig directly.
//
// The GPU implementation now lives in metal0/packages/gpu as the
// source of truth for cross-platform GPU compute.

const metal0_gpu = @import("metal0").gpu;

// Re-export all public types and functions
pub const Context = metal0_gpu.Context;
pub const Config = metal0_gpu.Config;
pub const Backend = metal0_gpu.Backend;
pub const Buffer = metal0_gpu.Buffer;
pub const BufferUsage = metal0_gpu.BufferUsage;
pub const ShaderModule = metal0_gpu.ShaderModule;
pub const ComputePipeline = metal0_gpu.ComputePipeline;
pub const BindGroup = metal0_gpu.BindGroup;

// Backend implementations
pub const wgpu = metal0_gpu.wgpu;
pub const sandbox = metal0_gpu.sandbox;
pub const validator = metal0_gpu.validator;

// Convenience functions
pub const vectorAdd = metal0_gpu.vectorAdd;

// Legacy compatibility aliases (for code using old gpu_sandbox.zig API)
pub const GpuSandbox = sandbox.SandboxedContext;
pub const GpuConfig = struct {
    enabled: bool = false,
    max_memory_bytes: u64 = 256 * 1024 * 1024,
    max_dispatch_calls: i32 = 100,
    max_workgroups: u32 = 65536,
    timeout_seconds: u32 = 10,
    process_isolation: bool = true,
    shader_validation: bool = true,
    max_shader_instructions: u32 = 10000,
    allowed_features: []const Feature = &.{.compute},

    pub const Feature = enum {
        compute,
        vertex,
        fragment,
        storage_textures,
    };

    /// Convert to new Config format
    pub fn toConfig(self: GpuConfig) Config {
        return .{
            .backend = if (self.process_isolation) .sandboxed else .wgpu,
            .enabled = self.enabled,
            .max_memory_bytes = self.max_memory_bytes,
            .max_dispatch_calls = self.max_dispatch_calls,
            .max_workgroups = self.max_workgroups,
            .shader_validation = self.shader_validation,
            .max_shader_instructions = self.max_shader_instructions,
            .process_isolation = self.process_isolation,
            .timeout_seconds = self.timeout_seconds,
        };
    }
};

// IPC types for compatibility with gpu_worker.zig
pub const CommandType = sandbox.CommandType;
pub const ResponseCode = sandbox.ResponseCode;

// Tests
test "gpu module re-exports" {
    const std = @import("std");
    _ = Context;
    _ = Config;
    _ = GpuSandbox;
    try std.testing.expect(true);
}
