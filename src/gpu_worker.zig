// GPU Worker Process
//
// Separate process that runs wgpu-native for WebGPU operations.
// Communicates with main EdgeBox process via Unix socket IPC.
//
// Build: zig build gpu-worker -Doptimize=ReleaseFast
// Run: edgebox-gpu-worker <socket_fd>
//
// Security model:
// - Runs in separate process (can be killed without affecting main)
// - Resource limits enforced via setrlimit
// - Memory/dispatch limits enforced at IPC protocol level
// - Shader validation done in main process before IPC

const std = @import("std");
const posix = std.posix;
const gpu = @import("gpu_sandbox.zig");

// wgpu-native C bindings
const c = @cImport({
    @cInclude("webgpu.h");
    @cInclude("wgpu.h");
});

// Synchronization for async callbacks
const CallbackState = struct {
    completed: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
    adapter: c.WGPUAdapter = null,
    device: c.WGPUDevice = null,
    status: c.WGPURequestAdapterStatus = c.WGPURequestAdapterStatus_Unknown,
    device_status: c.WGPURequestDeviceStatus = c.WGPURequestDeviceStatus_Unknown,
    error_message: ?[]const u8 = null,
};

// Buffer map callback state
const MapCallbackState = struct {
    completed: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
    status: c.WGPUMapAsyncStatus = c.WGPUMapAsyncStatus_Unknown,
};

const GpuWorker = struct {
    allocator: std.mem.Allocator,
    socket_fd: posix.socket_t,

    // wgpu state
    instance: c.WGPUInstance = null,
    adapter: c.WGPUAdapter = null,
    device: c.WGPUDevice = null,
    queue: c.WGPUQueue = null,

    // Handle tracking
    buffers: std.AutoHashMap(u32, c.WGPUBuffer),
    shaders: std.AutoHashMap(u32, c.WGPUShaderModule),
    pipelines: std.AutoHashMap(u32, c.WGPUComputePipeline),
    bind_groups: std.AutoHashMap(u32, c.WGPUBindGroup),
    bind_group_layouts: std.AutoHashMap(u32, c.WGPUBindGroupLayout),
    pipeline_layouts: std.AutoHashMap(u32, c.WGPUPipelineLayout),
    command_encoders: std.AutoHashMap(u32, c.WGPUCommandEncoder),

    // Buffer metadata
    buffer_sizes: std.AutoHashMap(u32, u64),

    // Limits
    dispatch_count: u32 = 0,
    memory_used: u64 = 0,
    max_memory: u64 = 256 * 1024 * 1024,
    max_dispatches: i32 = 100,
    max_workgroups: u32 = 65536,

    next_handle: u32 = 1,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, socket_fd: posix.socket_t) !Self {
        var self = Self{
            .allocator = allocator,
            .socket_fd = socket_fd,
            .buffers = std.AutoHashMap(u32, c.WGPUBuffer).init(allocator),
            .shaders = std.AutoHashMap(u32, c.WGPUShaderModule).init(allocator),
            .pipelines = std.AutoHashMap(u32, c.WGPUComputePipeline).init(allocator),
            .bind_groups = std.AutoHashMap(u32, c.WGPUBindGroup).init(allocator),
            .bind_group_layouts = std.AutoHashMap(u32, c.WGPUBindGroupLayout).init(allocator),
            .pipeline_layouts = std.AutoHashMap(u32, c.WGPUPipelineLayout).init(allocator),
            .command_encoders = std.AutoHashMap(u32, c.WGPUCommandEncoder).init(allocator),
            .buffer_sizes = std.AutoHashMap(u32, u64).init(allocator),
        };

        // Initialize wgpu instance
        const instance_extras = c.WGPUInstanceExtras{
            .chain = .{
                .next = null,
                .sType = c.WGPUSType_InstanceExtras,
            },
            .backends = c.WGPUInstanceBackend_Metal, // Use Metal on macOS
            .flags = c.WGPUInstanceFlag_Default,
            .dx12ShaderCompiler = c.WGPUDx12Compiler_Undefined,
            .gles3MinorVersion = c.WGPUGles3MinorVersion_Automatic,
            .glFenceBehaviour = c.WGPUGLFenceBehaviour_Normal,
            .dxcPath = .{ .data = null, .length = 0 },
            .dxcMaxShaderModel = c.WGPUDxcMaxShaderModel_V6_0,
            .dx12PresentationSystem = c.WGPUDx12SwapchainKind_Undefined,
            .budgetForDeviceCreation = null,
            .budgetForDeviceLoss = null,
        };

        const instance_desc = c.WGPUInstanceDescriptor{
            .nextInChain = @ptrCast(&instance_extras.chain),
            .features = .{
                .nextInChain = null,
                .timedWaitAnyEnable = 0,
                .timedWaitAnyMaxCount = 0,
            },
        };

        self.instance = c.wgpuCreateInstance(&instance_desc);
        if (self.instance == null) {
            std.debug.print("[gpu-worker] Failed to create wgpu instance\n", .{});
            return error.WgpuInitFailed;
        }

        // Set up logging
        c.wgpuSetLogLevel(c.WGPULogLevel_Warn);

        std.debug.print("[gpu-worker] Initialized (pid: {})\n", .{std.c.getpid()});
        return self;
    }

    pub fn deinit(self: *Self) void {
        // Release all tracked resources
        var buf_iter = self.buffers.valueIterator();
        while (buf_iter.next()) |buf| {
            c.wgpuBufferRelease(buf.*);
        }
        self.buffers.deinit();
        self.buffer_sizes.deinit();

        var shader_iter = self.shaders.valueIterator();
        while (shader_iter.next()) |shader| {
            c.wgpuShaderModuleRelease(shader.*);
        }
        self.shaders.deinit();

        var pipeline_iter = self.pipelines.valueIterator();
        while (pipeline_iter.next()) |pipeline| {
            c.wgpuComputePipelineRelease(pipeline.*);
        }
        self.pipelines.deinit();

        var bg_iter = self.bind_groups.valueIterator();
        while (bg_iter.next()) |bg| {
            c.wgpuBindGroupRelease(bg.*);
        }
        self.bind_groups.deinit();

        var bgl_iter = self.bind_group_layouts.valueIterator();
        while (bgl_iter.next()) |bgl| {
            c.wgpuBindGroupLayoutRelease(bgl.*);
        }
        self.bind_group_layouts.deinit();

        var pl_iter = self.pipeline_layouts.valueIterator();
        while (pl_iter.next()) |pl| {
            c.wgpuPipelineLayoutRelease(pl.*);
        }
        self.pipeline_layouts.deinit();

        var enc_iter = self.command_encoders.valueIterator();
        while (enc_iter.next()) |enc| {
            c.wgpuCommandEncoderRelease(enc.*);
        }
        self.command_encoders.deinit();

        if (self.queue != null) c.wgpuQueueRelease(self.queue);
        if (self.device != null) c.wgpuDeviceRelease(self.device);
        if (self.adapter != null) c.wgpuAdapterRelease(self.adapter);
        if (self.instance != null) c.wgpuInstanceRelease(self.instance);
    }

    /// Request adapter with synchronous wait
    fn requestAdapter(self: *Self) !void {
        var state = CallbackState{};

        const callback_info = c.WGPURequestAdapterCallbackInfo{
            .nextInChain = null,
            .mode = c.WGPUCallbackMode_AllowSpontaneous,
            .callback = adapterCallback,
            .userdata1 = @ptrCast(&state),
            .userdata2 = null,
        };

        const options = c.WGPURequestAdapterOptions{
            .nextInChain = null,
            .compatibleSurface = null,
            .powerPreference = c.WGPUPowerPreference_HighPerformance,
            .backendType = c.WGPUBackendType_Undefined,
            .forceFallbackAdapter = 0,
        };

        _ = c.wgpuInstanceRequestAdapter(self.instance, &options, callback_info);

        // Poll until callback completes
        while (!state.completed.load(.acquire)) {
            _ = c.wgpuDevicePoll(self.device, 0, null);
            std.Thread.sleep(1_000_000); // 1ms
        }

        if (state.adapter == null) {
            std.debug.print("[gpu-worker] Failed to get adapter\n", .{});
            return error.AdapterFailed;
        }

        self.adapter = state.adapter;
        std.debug.print("[gpu-worker] Adapter acquired\n", .{});
    }

    fn adapterCallback(
        status: c.WGPURequestAdapterStatus,
        adapter: c.WGPUAdapter,
        message: c.WGPUStringView,
        userdata1: ?*anyopaque,
        _: ?*anyopaque,
    ) callconv(.c) void {
        const state: *CallbackState = @ptrCast(@alignCast(userdata1));
        state.status = status;
        if (status == c.WGPURequestAdapterStatus_Success) {
            state.adapter = adapter;
        } else {
            if (message.data != null and message.length > 0) {
                std.debug.print("[gpu-worker] Adapter error: {s}\n", .{message.data[0..message.length]});
            }
        }
        state.completed.store(true, .release);
    }

    /// Request device with synchronous wait
    fn requestDevice(self: *Self) !void {
        if (self.adapter == null) return error.NotInitialized;

        var state = CallbackState{};

        const callback_info = c.WGPURequestDeviceCallbackInfo{
            .nextInChain = null,
            .mode = c.WGPUCallbackMode_AllowSpontaneous,
            .callback = deviceCallback,
            .userdata1 = @ptrCast(&state),
            .userdata2 = null,
        };

        const device_desc = c.WGPUDeviceDescriptor{
            .nextInChain = null,
            .label = .{ .data = "EdgeBox GPU", .length = 11 },
            .requiredFeatureCount = 0,
            .requiredFeatures = null,
            .requiredLimits = null,
            .defaultQueue = .{
                .nextInChain = null,
                .label = .{ .data = null, .length = 0 },
            },
            .deviceLostCallbackInfo = .{
                .nextInChain = null,
                .mode = c.WGPUCallbackMode_AllowSpontaneous,
                .callback = null,
                .userdata1 = null,
                .userdata2 = null,
            },
            .uncapturedErrorCallbackInfo = .{
                .nextInChain = null,
                .callback = null,
                .userdata1 = null,
                .userdata2 = null,
            },
        };

        _ = c.wgpuAdapterRequestDevice(self.adapter, &device_desc, callback_info);

        // Poll until callback completes
        var timeout: u32 = 0;
        while (!state.completed.load(.acquire) and timeout < 5000) {
            std.Thread.sleep(1_000_000); // 1ms
            timeout += 1;
        }

        if (state.device == null) {
            std.debug.print("[gpu-worker] Failed to get device\n", .{});
            return error.DeviceFailed;
        }

        self.device = state.device;
        self.queue = c.wgpuDeviceGetQueue(self.device);
        std.debug.print("[gpu-worker] Device and queue acquired\n", .{});
    }

    fn deviceCallback(
        status: c.WGPURequestDeviceStatus,
        device: c.WGPUDevice,
        message: c.WGPUStringView,
        userdata1: ?*anyopaque,
        _: ?*anyopaque,
    ) callconv(.c) void {
        const state: *CallbackState = @ptrCast(@alignCast(userdata1));
        state.device_status = status;
        if (status == c.WGPURequestDeviceStatus_Success) {
            state.device = device;
        } else {
            if (message.data != null and message.length > 0) {
                std.debug.print("[gpu-worker] Device error: {s}\n", .{message.data[0..message.length]});
            }
        }
        state.completed.store(true, .release);
    }

    /// Main event loop - process IPC commands
    pub fn run(self: *Self) !void {
        var buf: [65536]u8 = undefined;

        while (true) {
            // Read command header (1 byte command type + 4 bytes payload length)
            const header_len = try posix.read(self.socket_fd, buf[0..5]);
            if (header_len == 0) {
                std.debug.print("[gpu-worker] Socket closed, exiting\n", .{});
                break;
            }
            if (header_len < 5) {
                try self.sendResponse(.error_unknown, null);
                continue;
            }

            const cmd_type: gpu.CommandType = @enumFromInt(buf[0]);
            const payload_len = std.mem.readInt(u32, buf[1..5], .little);

            // Read payload
            if (payload_len > buf.len - 5) {
                try self.sendResponse(.error_unknown, null);
                continue;
            }

            var payload: []u8 = &.{};
            if (payload_len > 0) {
                const read_len = try posix.read(self.socket_fd, buf[5 .. 5 + payload_len]);
                if (read_len != payload_len) {
                    try self.sendResponse(.error_unknown, null);
                    continue;
                }
                payload = buf[5 .. 5 + payload_len];
            }

            // Handle command
            const result = self.handleCommand(cmd_type, payload);
            if (result) |response_data| {
                try self.sendResponse(.success, response_data);
            } else |err| {
                const code = errorToCode(err);
                try self.sendResponse(code, null);
            }
        }
    }

    fn handleCommand(self: *Self, cmd: gpu.CommandType, payload: []u8) !?[]const u8 {
        switch (cmd) {
            .ping => return "pong",

            .shutdown => {
                std.debug.print("[gpu-worker] Shutdown requested\n", .{});
                return error.Shutdown;
            },

            .request_adapter => {
                try self.requestAdapter();
                return null;
            },

            .request_device => {
                try self.requestDevice();
                return null;
            },

            .create_buffer => {
                // Parse: size (8 bytes) + usage (4 bytes)
                if (payload.len < 12) return error.InvalidPayload;

                const size = std.mem.readInt(u64, payload[0..8], .little);
                const usage = std.mem.readInt(u32, payload[8..12], .little);

                // Check memory limits
                if (self.memory_used + size > self.max_memory) {
                    return error.MemoryLimit;
                }

                if (self.device == null) return error.NotInitialized;

                const desc = c.WGPUBufferDescriptor{
                    .nextInChain = null,
                    .label = .{ .data = null, .length = 0 },
                    .usage = usage,
                    .size = size,
                    .mappedAtCreation = 0,
                };

                const buffer = c.wgpuDeviceCreateBuffer(self.device, &desc);
                if (buffer == null) return error.BufferCreationFailed;

                const handle = self.allocHandle();
                try self.buffers.put(handle, buffer);
                try self.buffer_sizes.put(handle, size);
                self.memory_used += size;

                // Return handle as 4 bytes
                var result: [4]u8 = undefined;
                std.mem.writeInt(u32, &result, handle, .little);
                return &result;
            },

            .destroy_buffer => {
                if (payload.len < 4) return error.InvalidPayload;
                const handle = std.mem.readInt(u32, payload[0..4], .little);

                if (self.buffers.get(handle)) |buffer| {
                    c.wgpuBufferDestroy(buffer);
                    c.wgpuBufferRelease(buffer);
                    _ = self.buffers.remove(handle);

                    if (self.buffer_sizes.get(handle)) |size| {
                        self.memory_used -= size;
                        _ = self.buffer_sizes.remove(handle);
                    }
                    return null;
                }
                return error.InvalidHandle;
            },

            .write_buffer => {
                // Parse: handle (4) + offset (8) + data
                if (payload.len < 12) return error.InvalidPayload;

                const handle = std.mem.readInt(u32, payload[0..4], .little);
                const offset = std.mem.readInt(u64, payload[4..12], .little);
                const data = payload[12..];

                if (self.queue == null) return error.NotInitialized;

                if (self.buffers.get(handle)) |buffer| {
                    c.wgpuQueueWriteBuffer(self.queue, buffer, offset, data.ptr, data.len);
                    return null;
                }
                return error.InvalidHandle;
            },

            .read_buffer => {
                // Parse: handle (4) + offset (8) + size (8)
                if (payload.len < 20) return error.InvalidPayload;

                const handle = std.mem.readInt(u32, payload[0..4], .little);
                const offset = std.mem.readInt(u64, payload[4..12], .little);
                const size = std.mem.readInt(u64, payload[12..20], .little);

                if (self.device == null) return error.NotInitialized;

                if (self.buffers.get(handle)) |buffer| {
                    // Map buffer for reading
                    var map_state = MapCallbackState{};

                    const map_callback_info = c.WGPUBufferMapCallbackInfo{
                        .nextInChain = null,
                        .mode = c.WGPUCallbackMode_AllowSpontaneous,
                        .callback = mapCallback,
                        .userdata1 = @ptrCast(&map_state),
                        .userdata2 = null,
                    };

                    _ = c.wgpuBufferMapAsync(buffer, c.WGPUMapMode_Read, offset, size, map_callback_info);

                    // Poll until mapped
                    var timeout: u32 = 0;
                    while (!map_state.completed.load(.acquire) and timeout < 5000) {
                        _ = c.wgpuDevicePoll(self.device, 0, null);
                        std.Thread.sleep(1_000_000);
                        timeout += 1;
                    }

                    if (map_state.status != c.WGPUMapAsyncStatus_Success) {
                        return error.MapFailed;
                    }

                    // Read the data
                    const mapped_ptr = c.wgpuBufferGetConstMappedRange(buffer, offset, size);
                    if (mapped_ptr == null) {
                        c.wgpuBufferUnmap(buffer);
                        return error.MapFailed;
                    }

                    // Copy data (caller must handle response buffer)
                    const data_slice: [*]const u8 = @ptrCast(mapped_ptr);
                    c.wgpuBufferUnmap(buffer);

                    return data_slice[0..size];
                }
                return error.InvalidHandle;
            },

            .map_buffer, .unmap_buffer => {
                return error.NotImplemented;
            },

            .create_shader_module => {
                // Parse: WGSL code as string
                if (payload.len == 0) return error.InvalidPayload;
                if (self.device == null) return error.NotInitialized;

                const wgsl_source = c.WGPUShaderSourceWGSL{
                    .chain = .{
                        .next = null,
                        .sType = c.WGPUSType_ShaderSourceWGSL,
                    },
                    .code = .{
                        .data = payload.ptr,
                        .length = payload.len,
                    },
                };

                const shader_desc = c.WGPUShaderModuleDescriptor{
                    .nextInChain = @ptrCast(&wgsl_source.chain),
                    .label = .{ .data = null, .length = 0 },
                };

                const shader = c.wgpuDeviceCreateShaderModule(self.device, &shader_desc);
                if (shader == null) return error.ShaderCompilationFailed;

                const handle = self.allocHandle();
                try self.shaders.put(handle, shader);

                var result: [4]u8 = undefined;
                std.mem.writeInt(u32, &result, handle, .little);
                return &result;
            },

            .destroy_shader_module => {
                if (payload.len < 4) return error.InvalidPayload;
                const handle = std.mem.readInt(u32, payload[0..4], .little);

                if (self.shaders.get(handle)) |shader| {
                    c.wgpuShaderModuleRelease(shader);
                    _ = self.shaders.remove(handle);
                    return null;
                }
                return error.InvalidHandle;
            },

            .create_compute_pipeline => {
                // Parse: shader_handle (4) + entry_point_len (4) + entry_point
                if (payload.len < 8) return error.InvalidPayload;
                if (self.device == null) return error.NotInitialized;

                const shader_handle = std.mem.readInt(u32, payload[0..4], .little);
                const entry_len = std.mem.readInt(u32, payload[4..8], .little);

                if (payload.len < 8 + entry_len) return error.InvalidPayload;
                const entry_point = payload[8 .. 8 + entry_len];

                const shader = self.shaders.get(shader_handle) orelse return error.InvalidHandle;

                const compute_stage = c.WGPUProgrammableStageDescriptor{
                    .nextInChain = null,
                    .module = shader,
                    .entryPoint = .{
                        .data = entry_point.ptr,
                        .length = entry_point.len,
                    },
                    .constantCount = 0,
                    .constants = null,
                };

                const pipeline_desc = c.WGPUComputePipelineDescriptor{
                    .nextInChain = null,
                    .label = .{ .data = null, .length = 0 },
                    .layout = null, // Auto layout
                    .compute = compute_stage,
                };

                const pipeline = c.wgpuDeviceCreateComputePipeline(self.device, &pipeline_desc);
                if (pipeline == null) return error.PipelineCreationFailed;

                const handle = self.allocHandle();
                try self.pipelines.put(handle, pipeline);

                var result: [4]u8 = undefined;
                std.mem.writeInt(u32, &result, handle, .little);
                return &result;
            },

            .create_bind_group => {
                // Parse: layout_handle (4) + num_entries (4) + entries...
                if (payload.len < 8) return error.InvalidPayload;
                if (self.device == null) return error.NotInitialized;

                const layout_handle = std.mem.readInt(u32, payload[0..4], .little);
                const num_entries = std.mem.readInt(u32, payload[4..8], .little);

                const layout = self.bind_group_layouts.get(layout_handle) orelse return error.InvalidHandle;

                // For now, simplified: entries are just buffer handles at bindings 0, 1, 2...
                var entries: [16]c.WGPUBindGroupEntry = undefined;
                var offset: usize = 8;

                for (0..num_entries) |i| {
                    if (offset + 4 > payload.len) return error.InvalidPayload;
                    const buf_handle = std.mem.readInt(u32, payload[offset..][0..4], .little);
                    offset += 4;

                    const buffer = self.buffers.get(buf_handle) orelse return error.InvalidHandle;
                    const buf_size = self.buffer_sizes.get(buf_handle) orelse 0;

                    entries[i] = .{
                        .nextInChain = null,
                        .binding = @intCast(i),
                        .buffer = buffer,
                        .offset = 0,
                        .size = buf_size,
                        .sampler = null,
                        .textureView = null,
                    };
                }

                const bg_desc = c.WGPUBindGroupDescriptor{
                    .nextInChain = null,
                    .label = .{ .data = null, .length = 0 },
                    .layout = layout,
                    .entryCount = num_entries,
                    .entries = &entries,
                };

                const bind_group = c.wgpuDeviceCreateBindGroup(self.device, &bg_desc);
                if (bind_group == null) return error.BindGroupCreationFailed;

                const handle = self.allocHandle();
                try self.bind_groups.put(handle, bind_group);

                var result: [4]u8 = undefined;
                std.mem.writeInt(u32, &result, handle, .little);
                return &result;
            },

            .create_bind_group_layout => {
                // Parse: num_entries (4) + entries (binding, type, visibility)
                if (payload.len < 4) return error.InvalidPayload;
                if (self.device == null) return error.NotInitialized;

                const num_entries = std.mem.readInt(u32, payload[0..4], .little);
                var entries: [16]c.WGPUBindGroupLayoutEntry = undefined;
                var offset: usize = 4;

                for (0..num_entries) |i| {
                    if (offset + 12 > payload.len) return error.InvalidPayload;

                    const binding = std.mem.readInt(u32, payload[offset..][0..4], .little);
                    const visibility = std.mem.readInt(u32, payload[offset + 4 ..][0..4], .little);
                    const buf_type = std.mem.readInt(u32, payload[offset + 8 ..][0..4], .little);
                    offset += 12;

                    entries[i] = .{
                        .nextInChain = null,
                        .binding = binding,
                        .visibility = visibility,
                        .buffer = .{
                            .nextInChain = null,
                            .type = buf_type,
                            .hasDynamicOffset = 0,
                            .minBindingSize = 0,
                        },
                        .sampler = .{ .nextInChain = null, .type = c.WGPUSamplerBindingType_Undefined },
                        .texture = .{ .nextInChain = null, .sampleType = c.WGPUTextureSampleType_Undefined, .viewDimension = c.WGPUTextureViewDimension_Undefined, .multisampled = 0 },
                        .storageTexture = .{ .nextInChain = null, .access = c.WGPUStorageTextureAccess_Undefined, .format = c.WGPUTextureFormat_Undefined, .viewDimension = c.WGPUTextureViewDimension_Undefined },
                    };
                }

                const layout_desc = c.WGPUBindGroupLayoutDescriptor{
                    .nextInChain = null,
                    .label = .{ .data = null, .length = 0 },
                    .entryCount = num_entries,
                    .entries = &entries,
                };

                const layout = c.wgpuDeviceCreateBindGroupLayout(self.device, &layout_desc);
                if (layout == null) return error.LayoutCreationFailed;

                const handle = self.allocHandle();
                try self.bind_group_layouts.put(handle, layout);

                var result: [4]u8 = undefined;
                std.mem.writeInt(u32, &result, handle, .little);
                return &result;
            },

            .create_pipeline_layout => {
                // Parse: num_layouts (4) + layout_handles...
                if (payload.len < 4) return error.InvalidPayload;
                if (self.device == null) return error.NotInitialized;

                const num_layouts = std.mem.readInt(u32, payload[0..4], .little);
                var layouts: [8]c.WGPUBindGroupLayout = undefined;
                var offset: usize = 4;

                for (0..num_layouts) |i| {
                    if (offset + 4 > payload.len) return error.InvalidPayload;
                    const layout_handle = std.mem.readInt(u32, payload[offset..][0..4], .little);
                    offset += 4;

                    layouts[i] = self.bind_group_layouts.get(layout_handle) orelse return error.InvalidHandle;
                }

                const pipeline_layout_desc = c.WGPUPipelineLayoutDescriptor{
                    .nextInChain = null,
                    .label = .{ .data = null, .length = 0 },
                    .bindGroupLayoutCount = num_layouts,
                    .bindGroupLayouts = &layouts,
                };

                const pipeline_layout = c.wgpuDeviceCreatePipelineLayout(self.device, &pipeline_layout_desc);
                if (pipeline_layout == null) return error.LayoutCreationFailed;

                const handle = self.allocHandle();
                try self.pipeline_layouts.put(handle, pipeline_layout);

                var result: [4]u8 = undefined;
                std.mem.writeInt(u32, &result, handle, .little);
                return &result;
            },

            .create_command_encoder => {
                if (self.device == null) return error.NotInitialized;

                const enc_desc = c.WGPUCommandEncoderDescriptor{
                    .nextInChain = null,
                    .label = .{ .data = null, .length = 0 },
                };

                const encoder = c.wgpuDeviceCreateCommandEncoder(self.device, &enc_desc);
                if (encoder == null) return error.EncoderCreationFailed;

                const handle = self.allocHandle();
                try self.command_encoders.put(handle, encoder);

                var result: [4]u8 = undefined;
                std.mem.writeInt(u32, &result, handle, .little);
                return &result;
            },

            .dispatch_workgroups => {
                // Parse: encoder (4) + pipeline (4) + bind_group (4) + x (4) + y (4) + z (4)
                if (payload.len < 24) return error.InvalidPayload;

                // Check dispatch limits
                if (self.max_dispatches >= 0) {
                    if (self.dispatch_count >= @as(u32, @intCast(self.max_dispatches))) {
                        return error.DispatchLimit;
                    }
                }

                const encoder_handle = std.mem.readInt(u32, payload[0..4], .little);
                const pipeline_handle = std.mem.readInt(u32, payload[4..8], .little);
                const bind_group_handle = std.mem.readInt(u32, payload[8..12], .little);
                const x = std.mem.readInt(u32, payload[12..16], .little);
                const y = std.mem.readInt(u32, payload[16..20], .little);
                const z = std.mem.readInt(u32, payload[20..24], .little);

                // Check workgroup limits
                const total_workgroups = @as(u64, x) * y * z;
                if (total_workgroups > self.max_workgroups) {
                    return error.WorkgroupLimit;
                }

                const encoder = self.command_encoders.get(encoder_handle) orelse return error.InvalidHandle;
                const pipeline = self.pipelines.get(pipeline_handle) orelse return error.InvalidHandle;
                const bind_group = self.bind_groups.get(bind_group_handle) orelse return error.InvalidHandle;

                // Begin compute pass
                const compute_pass = c.wgpuCommandEncoderBeginComputePass(encoder, null);
                if (compute_pass == null) return error.ComputePassFailed;

                c.wgpuComputePassEncoderSetPipeline(compute_pass, pipeline);
                c.wgpuComputePassEncoderSetBindGroup(compute_pass, 0, bind_group, 0, null);
                c.wgpuComputePassEncoderDispatchWorkgroups(compute_pass, x, y, z);
                c.wgpuComputePassEncoderEnd(compute_pass);
                c.wgpuComputePassEncoderRelease(compute_pass);

                self.dispatch_count += 1;
                return null;
            },

            .finish_encoder => {
                if (payload.len < 4) return error.InvalidPayload;
                const encoder_handle = std.mem.readInt(u32, payload[0..4], .little);

                const encoder = self.command_encoders.get(encoder_handle) orelse return error.InvalidHandle;

                const cmd_buffer = c.wgpuCommandEncoderFinish(encoder, null);
                if (cmd_buffer == null) return error.FinishFailed;

                // Remove encoder from tracking (it's consumed)
                _ = self.command_encoders.remove(encoder_handle);

                // Submit immediately
                if (self.queue != null) {
                    c.wgpuQueueSubmit(self.queue, 1, &cmd_buffer);
                    c.wgpuCommandBufferRelease(cmd_buffer);
                }

                return null;
            },

            .queue_submit => {
                // For now, finish_encoder handles submission
                return null;
            },
        }
    }

    fn mapCallback(
        status: c.WGPUMapAsyncStatus,
        _: c.WGPUStringView,
        userdata1: ?*anyopaque,
        _: ?*anyopaque,
    ) callconv(.c) void {
        const state: *MapCallbackState = @ptrCast(@alignCast(userdata1));
        state.status = status;
        state.completed.store(true, .release);
    }

    fn sendResponse(self: *Self, code: gpu.ResponseCode, data: ?[]const u8) !void {
        var header: [8]u8 = undefined;
        std.mem.writeInt(i32, header[0..4], @intFromEnum(code), .little);
        const data_len: u32 = if (data) |d| @intCast(d.len) else 0;
        std.mem.writeInt(u32, header[4..8], data_len, .little);

        _ = try posix.write(self.socket_fd, &header);
        if (data) |d| {
            _ = try posix.write(self.socket_fd, d);
        }
    }

    fn errorToCode(err: anyerror) gpu.ResponseCode {
        return switch (err) {
            error.NotInitialized => .error_not_initialized,
            error.DispatchLimit => .error_dispatch_limit,
            error.WorkgroupLimit => .error_workgroup_limit,
            error.MemoryLimit => .error_memory_limit,
            error.Timeout => .error_timeout,
            error.ShaderValidation => .error_shader_validation,
            error.ShaderComplex => .error_shader_complex,
            error.FeatureDisabled => .error_feature_disabled,
            else => .error_unknown,
        };
    }

    fn allocHandle(self: *Self) u32 {
        const handle = self.next_handle;
        self.next_handle +%= 1;
        if (self.next_handle == 0) self.next_handle = 1;
        return handle;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: edgebox-gpu-worker <socket_fd>\n", .{});
        std.process.exit(1);
    }

    const socket_fd = std.fmt.parseInt(posix.socket_t, args[1], 10) catch {
        std.debug.print("Invalid socket fd: {s}\n", .{args[1]});
        std.process.exit(1);
    };

    var worker = try GpuWorker.init(allocator, socket_fd);
    defer worker.deinit();

    worker.run() catch |err| {
        if (err == error.Shutdown) {
            std.debug.print("[gpu-worker] Clean shutdown\n", .{});
            return;
        }
        std.debug.print("[gpu-worker] Error: {}\n", .{err});
        std.process.exit(1);
    };
}
