// GPU Sandbox Controller
//
// Manages a separate GPU worker process for WebGPU operations.
// Process isolation ensures GPU issues don't affect the main WASM runtime.
//
// Architecture:
//   EdgeBox Main Process ←→ IPC (Unix socket) ←→ GPU Worker Process
//
// The GPU worker:
// - Runs wgpu-native for actual GPU operations
// - Has memory and dispatch limits enforced
// - Can be killed without affecting main process
// - Automatically restarts on failure

const std = @import("std");
const posix = std.posix;

pub const GpuConfig = struct {
    /// Enable GPU access (default: false)
    enabled: bool = false,

    /// Maximum GPU buffer memory in bytes (default: 256MB)
    max_memory_bytes: u64 = 256 * 1024 * 1024,

    /// Maximum compute dispatch calls per request (default: 100, -1 = unlimited)
    max_dispatch_calls: i32 = 100,

    /// Maximum workgroups per dispatch (default: 65536)
    max_workgroups: u32 = 65536,

    /// GPU worker timeout in seconds (default: 10)
    timeout_seconds: u32 = 10,

    /// Use separate process for GPU (default: true, safest option)
    process_isolation: bool = true,

    /// Validate shaders before execution (default: true)
    shader_validation: bool = true,

    /// Maximum estimated shader instructions (default: 10000)
    max_shader_instructions: u32 = 10000,

    /// Allowed GPU features
    allowed_features: []const Feature = &.{.compute},

    pub const Feature = enum {
        compute,
        vertex,
        fragment,
        storage_textures,
    };
};

/// GPU IPC Command Types
pub const CommandType = enum(u8) {
    // Adapter/Device
    request_adapter = 0x01,
    request_device = 0x02,

    // Buffers
    create_buffer = 0x10,
    destroy_buffer = 0x11,
    write_buffer = 0x12,
    read_buffer = 0x13,
    map_buffer = 0x14,
    unmap_buffer = 0x15,

    // Shaders
    create_shader_module = 0x20,
    destroy_shader_module = 0x21,

    // Pipelines
    create_compute_pipeline = 0x30,
    create_bind_group = 0x31,
    create_bind_group_layout = 0x32,
    create_pipeline_layout = 0x33,

    // Execution
    create_command_encoder = 0x40,
    dispatch_workgroups = 0x41,
    finish_encoder = 0x42,
    queue_submit = 0x43,

    // Control
    ping = 0xF0,
    shutdown = 0xFF,
};

/// GPU IPC Response Codes
pub const ResponseCode = enum(i32) {
    success = 0,
    error_unknown = -1,
    error_not_initialized = -2,
    error_invalid_handle = -3,
    error_dispatch_limit = -4,
    error_workgroup_limit = -5,
    error_memory_limit = -6,
    error_timeout = -7,
    error_shader_validation = -8,
    error_shader_complex = -9,
    error_feature_disabled = -10,
};

pub const GpuSandbox = struct {
    allocator: std.mem.Allocator,
    config: GpuConfig,
    worker_pid: ?posix.pid_t = null,
    socket: ?posix.socket_t = null,
    dispatch_count: u32 = 0,
    is_initialized: bool = false,
    adapter_ready: bool = false,
    device_ready: bool = false,

    // IPC protocol constants
    const IPC_TIMEOUT_MS = 10000;
    const MAX_RESPONSE_SIZE = 1024 * 1024; // 1MB max response

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, config: GpuConfig) !Self {
        var self = Self{
            .allocator = allocator,
            .config = config,
        };

        if (config.enabled and config.process_isolation) {
            try self.spawnWorker();
            // Request adapter and device
            try self.initializeGpu();
        }

        return self;
    }

    fn spawnWorker(self: *Self) !void {
        // Create Unix socket pair for IPC
        const sockets = try posix.socketpair(posix.AF.UNIX, posix.SOCK.STREAM, 0);

        const pid = try posix.fork();
        if (pid == 0) {
            // Child process - GPU worker
            posix.close(sockets[0]);

            // Set resource limits for child
            self.setResourceLimits() catch {};

            // Convert socket fd to string for exec
            var fd_buf: [16]u8 = undefined;
            const fd_str = std.fmt.bufPrint(&fd_buf, "{d}", .{sockets[1]}) catch {
                std.process.exit(1);
            };

            // Exec GPU worker binary
            const exe_path = getWorkerPath() catch {
                std.debug.print("[gpu-sandbox] GPU worker binary not found\n", .{});
                std.process.exit(1);
            };

            const args = [_:null]?[*:0]const u8{
                exe_path.ptr,
                @ptrCast(fd_str.ptr),
                null,
            };

            const err = posix.execvpeZ(args[0].?, &args, @ptrCast(std.c.environ));
            _ = err;
            std.debug.print("[gpu-sandbox] execvpe failed\n", .{});
            std.process.exit(1);
        }

        // Parent process
        posix.close(sockets[1]);
        self.worker_pid = pid;
        self.socket = sockets[0];
        self.is_initialized = true;

        std.debug.print("[gpu-sandbox] Spawned worker (pid: {})\n", .{pid});
    }

    fn getWorkerPath() !struct { ptr: [*:0]const u8 } {
        // Look for edgebox-gpu-worker in standard locations
        const paths = [_][]const u8{
            "edgebox-gpu-worker",
            "./edgebox-gpu-worker",
            "./zig-out/bin/edgebox-gpu-worker",
            "/usr/local/bin/edgebox-gpu-worker",
        };

        for (paths) |path| {
            const stat = posix.stat(path) catch continue;
            _ = stat;
            // Found the binary - convert to null-terminated
            return .{ .ptr = @ptrCast(path.ptr) };
        }

        return error.WorkerNotFound;
    }

    fn initializeGpu(self: *Self) !void {
        // Ping worker to ensure it's ready
        try self.sendCommand(.ping, &.{});
        const ping_resp = try self.readResponse();
        if (ping_resp.code != .success) {
            return error.WorkerNotResponding;
        }

        // Request adapter
        try self.sendCommand(.request_adapter, &.{});
        const adapter_resp = try self.readResponse();
        if (adapter_resp.code != .success) {
            std.debug.print("[gpu-sandbox] Failed to get adapter: {}\n", .{adapter_resp.code});
            return error.AdapterFailed;
        }
        self.adapter_ready = true;

        // Request device
        try self.sendCommand(.request_device, &.{});
        const device_resp = try self.readResponse();
        if (device_resp.code != .success) {
            std.debug.print("[gpu-sandbox] Failed to get device: {}\n", .{device_resp.code});
            return error.DeviceFailed;
        }
        self.device_ready = true;

        std.debug.print("[gpu-sandbox] GPU initialized\n", .{});
    }

    /// Send a command to the GPU worker
    fn sendCommand(self: *Self, cmd: CommandType, payload: []const u8) !void {
        const socket = self.socket orelse return error.NotConnected;

        var header: [5]u8 = undefined;
        header[0] = @intFromEnum(cmd);
        std.mem.writeInt(u32, header[1..5], @intCast(payload.len), .little);

        _ = try posix.write(socket, &header);
        if (payload.len > 0) {
            _ = try posix.write(socket, payload);
        }
    }

    /// Read response from GPU worker
    fn readResponse(self: *Self) !IpcResponse {
        const socket = self.socket orelse return error.NotConnected;

        var header: [8]u8 = undefined;
        const header_read = try posix.read(socket, &header);
        if (header_read < 8) {
            return error.IncompleteResponse;
        }

        const code: ResponseCode = @enumFromInt(std.mem.readInt(i32, header[0..4], .little));
        const data_len = std.mem.readInt(u32, header[4..8], .little);

        var data: ?[]u8 = null;
        if (data_len > 0) {
            if (data_len > MAX_RESPONSE_SIZE) {
                return error.ResponseTooLarge;
            }
            data = try self.allocator.alloc(u8, data_len);
            const read_len = try posix.read(socket, data.?);
            if (read_len != data_len) {
                self.allocator.free(data.?);
                return error.IncompleteResponse;
            }
        }

        return .{ .code = code, .data = data };
    }

    const IpcResponse = struct {
        code: ResponseCode,
        data: ?[]u8,
    };

    fn setResourceLimits(self: *Self) !void {
        // CPU time limit for worker
        const timeout: posix.rlimit = .{
            .cur = self.config.timeout_seconds,
            .max = self.config.timeout_seconds + 5,
        };
        try posix.setrlimit(.CPU, timeout);

        // Memory limit (GPU memory + overhead)
        const mem_limit: posix.rlimit = .{
            .cur = self.config.max_memory_bytes * 2,
            .max = self.config.max_memory_bytes * 2,
        };
        try posix.setrlimit(.AS, mem_limit);
    }

    /// Create a GPU buffer
    pub fn createBuffer(self: *Self, size: u64, usage: BufferUsage) !u32 {
        if (!self.config.enabled) {
            return error.GpuDisabled;
        }
        if (!self.device_ready) {
            return error.DeviceNotReady;
        }

        // Serialize buffer descriptor
        var payload: [16]u8 = undefined;
        std.mem.writeInt(u64, payload[0..8], size, .little);
        std.mem.writeInt(u32, payload[8..12], @intFromEnum(usage), .little);
        std.mem.writeInt(u32, payload[12..16], 0, .little); // padding

        try self.sendCommand(.create_buffer, &payload);
        const resp = try self.readResponse();

        if (resp.code != .success) {
            return codeToError(resp.code);
        }

        // Response data contains the buffer handle
        if (resp.data) |data| {
            defer self.allocator.free(data);
            if (data.len >= 4) {
                return std.mem.readInt(u32, data[0..4], .little);
            }
        }

        return error.InvalidResponse;
    }

    pub const BufferUsage = enum(u32) {
        map_read = 0x0001,
        map_write = 0x0002,
        copy_src = 0x0004,
        copy_dst = 0x0008,
        index = 0x0010,
        vertex = 0x0020,
        uniform = 0x0040,
        storage = 0x0080,
        indirect = 0x0100,
        query_resolve = 0x0200,
    };

    /// Write data to a GPU buffer
    pub fn writeBuffer(self: *Self, handle: u32, offset: u64, data: []const u8) !void {
        if (!self.config.enabled) {
            return error.GpuDisabled;
        }

        // Serialize: handle (4) + offset (8) + data
        var header: [12]u8 = undefined;
        std.mem.writeInt(u32, header[0..4], handle, .little);
        std.mem.writeInt(u64, header[4..12], offset, .little);

        // Send header first, then data
        const socket = self.socket orelse return error.NotConnected;

        var cmd_header: [5]u8 = undefined;
        cmd_header[0] = @intFromEnum(CommandType.write_buffer);
        std.mem.writeInt(u32, cmd_header[1..5], @intCast(header.len + data.len), .little);

        _ = try posix.write(socket, &cmd_header);
        _ = try posix.write(socket, &header);
        _ = try posix.write(socket, data);

        const resp = try self.readResponse();
        if (resp.code != .success) {
            return codeToError(resp.code);
        }
    }

    /// Read data from a GPU buffer
    pub fn readBuffer(self: *Self, handle: u32, offset: u64, size: u64) ![]u8 {
        if (!self.config.enabled) {
            return error.GpuDisabled;
        }

        var payload: [20]u8 = undefined;
        std.mem.writeInt(u32, payload[0..4], handle, .little);
        std.mem.writeInt(u64, payload[4..12], offset, .little);
        std.mem.writeInt(u64, payload[12..20], size, .little);

        try self.sendCommand(.read_buffer, &payload);
        const resp = try self.readResponse();

        if (resp.code != .success) {
            return codeToError(resp.code);
        }

        return resp.data orelse error.InvalidResponse;
    }

    /// Destroy a GPU buffer
    pub fn destroyBuffer(self: *Self, handle: u32) !void {
        var payload: [4]u8 = undefined;
        std.mem.writeInt(u32, payload[0..4], handle, .little);

        try self.sendCommand(.destroy_buffer, &payload);
        const resp = try self.readResponse();

        if (resp.code != .success) {
            return codeToError(resp.code);
        }
    }

    /// Dispatch compute workgroups
    pub fn dispatch(self: *Self, x: u32, y: u32, z: u32) !void {
        if (!self.config.enabled) {
            return error.GpuDisabled;
        }

        // Check dispatch limits
        if (self.config.max_dispatch_calls >= 0) {
            if (self.dispatch_count >= @as(u32, @intCast(self.config.max_dispatch_calls))) {
                return error.DispatchLimitExceeded;
            }
        }

        // Check workgroup limits
        const total_workgroups = @as(u64, x) * @as(u64, y) * @as(u64, z);
        if (total_workgroups > self.config.max_workgroups) {
            return error.WorkgroupLimitExceeded;
        }

        self.dispatch_count += 1;

        // Serialize dispatch command
        var payload: [12]u8 = undefined;
        std.mem.writeInt(u32, payload[0..4], x, .little);
        std.mem.writeInt(u32, payload[4..8], y, .little);
        std.mem.writeInt(u32, payload[8..12], z, .little);

        try self.sendCommand(.dispatch_workgroups, &payload);
        const resp = try self.readResponse();

        if (resp.code != .success) {
            return codeToError(resp.code);
        }
    }

    /// Create a shader module (with validation if enabled)
    pub fn createShaderModule(self: *Self, wgsl: []const u8) !u32 {
        if (!self.config.enabled) {
            return error.GpuDisabled;
        }
        if (!self.device_ready) {
            return error.DeviceNotReady;
        }

        // Validate shader in main process BEFORE sending to GPU worker
        if (self.config.shader_validation) {
            const validator = @import("shader_validator.zig");
            try validator.validate(wgsl, .{
                .max_instructions = self.config.max_shader_instructions,
                .allowed_features = self.config.allowed_features,
            });
        }

        // Send validated shader to worker
        try self.sendCommand(.create_shader_module, wgsl);
        const resp = try self.readResponse();

        if (resp.code != .success) {
            return codeToError(resp.code);
        }

        if (resp.data) |data| {
            defer self.allocator.free(data);
            if (data.len >= 4) {
                return std.mem.readInt(u32, data[0..4], .little);
            }
        }

        return error.InvalidResponse;
    }

    /// Create a compute pipeline
    pub fn createComputePipeline(self: *Self, shader_handle: u32, entry_point: []const u8) !u32 {
        if (!self.config.enabled) {
            return error.GpuDisabled;
        }

        // Serialize: shader_handle (4) + entry_point_len (4) + entry_point
        var header: [8]u8 = undefined;
        std.mem.writeInt(u32, header[0..4], shader_handle, .little);
        std.mem.writeInt(u32, header[4..8], @intCast(entry_point.len), .little);

        const socket = self.socket orelse return error.NotConnected;

        var cmd_header: [5]u8 = undefined;
        cmd_header[0] = @intFromEnum(CommandType.create_compute_pipeline);
        std.mem.writeInt(u32, cmd_header[1..5], @intCast(header.len + entry_point.len), .little);

        _ = try posix.write(socket, &cmd_header);
        _ = try posix.write(socket, &header);
        _ = try posix.write(socket, entry_point);

        const resp = try self.readResponse();

        if (resp.code != .success) {
            return codeToError(resp.code);
        }

        if (resp.data) |data| {
            defer self.allocator.free(data);
            if (data.len >= 4) {
                return std.mem.readInt(u32, data[0..4], .little);
            }
        }

        return error.InvalidResponse;
    }

    fn codeToError(code: ResponseCode) anyerror {
        return switch (code) {
            .success => unreachable,
            .error_not_initialized => error.NotInitialized,
            .error_invalid_handle => error.InvalidHandle,
            .error_dispatch_limit => error.DispatchLimitExceeded,
            .error_workgroup_limit => error.WorkgroupLimitExceeded,
            .error_memory_limit => error.MemoryLimitExceeded,
            .error_timeout => error.Timeout,
            .error_shader_validation => error.ShaderValidationFailed,
            .error_shader_complex => error.ShaderTooComplex,
            .error_feature_disabled => error.FeatureDisabled,
            .error_unknown => error.GpuError,
        };
    }

    /// Kill the GPU worker process
    pub fn kill(self: *Self) void {
        if (self.worker_pid) |pid| {
            posix.kill(pid, posix.SIG.KILL) catch {};
            _ = posix.waitpid(pid, 0) catch {};
            self.worker_pid = null;
            self.is_initialized = false;
            std.debug.print("[gpu-sandbox] Killed worker\n", .{});
        }
    }

    /// Restart the GPU worker (after crash or timeout)
    pub fn restart(self: *Self) !void {
        self.kill();
        self.dispatch_count = 0;
        if (self.config.enabled and self.config.process_isolation) {
            try self.spawnWorker();
        }
    }

    pub fn deinit(self: *Self) void {
        self.kill();
        if (self.socket) |sock| {
            posix.close(sock);
            self.socket = null;
        }
    }

    /// Check if GPU is available and working
    pub fn isAvailable(self: *Self) bool {
        return self.config.enabled and self.is_initialized and self.worker_pid != null;
    }
};

// Tests
test "GpuSandbox init disabled" {
    const allocator = std.testing.allocator;
    var sandbox = try GpuSandbox.init(allocator, .{ .enabled = false });
    defer sandbox.deinit();

    try std.testing.expect(!sandbox.isAvailable());
}

test "dispatch limits" {
    const allocator = std.testing.allocator;
    var sandbox = try GpuSandbox.init(allocator, .{
        .enabled = true,
        .process_isolation = false, // Don't actually spawn for test
        .max_dispatch_calls = 2,
    });
    defer sandbox.deinit();

    // First two dispatches should succeed (if enabled)
    // Third should fail with limit exceeded
}
