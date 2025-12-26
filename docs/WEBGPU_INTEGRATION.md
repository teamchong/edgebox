# WebGPU Integration Plan

EdgeBox provides sandboxed WebGPU access via a process-isolated GPU worker.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    EdgeBox Main Process                          │
│  ┌─────────────────┐    ┌──────────────────────────────────┐    │
│  │   WASM Runtime  │    │      GPU Sandbox Controller       │    │
│  │   (WAMR/AOT)    │←──→│        (gpu_sandbox.zig)          │    │
│  └─────────────────┘    └──────────────┬───────────────────┘    │
└──────────────────────────────────────────────────────────────────┘
                                         │
                              Unix Socket IPC
                                         │
                                         ▼
┌─────────────────────────────────────────────────────────────────┐
│                    GPU Worker Process                            │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │                 wgpu-native (Rust → C API)                │   │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐                │   │
│  │  │  Vulkan  │  │   Metal  │  │   D3D12  │  (platform)    │   │
│  │  └──────────┘  └──────────┘  └──────────┘                │   │
│  └──────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

## Security Model

1. **Process Isolation**: GPU worker runs in separate process
   - Can be killed without affecting main WASM runtime
   - Resource limits via setrlimit (CPU, memory)
   - Automatic restart on crash/timeout

2. **Shader Validation**: Before any shader reaches GPU
   - Instruction complexity estimation
   - Loop depth limits
   - Feature allowlist (compute-only by default)

3. **Resource Limits**: Enforced at multiple levels
   - Dispatch call limits
   - Workgroup limits
   - Memory limits

## Files

| File | Purpose |
|------|---------|
| `src/gpu_sandbox.zig` | Main process controller, IPC client |
| `src/gpu_worker.zig` | GPU worker process, IPC server |
| `src/shader_validator.zig` | WGSL validation before execution |

## wgpu-native Integration

### Download Pre-built Binaries

Use the download script:

```bash
./scripts/download-wgpu.sh
```

This downloads wgpu-native v27.0.4.0 to `vendor/wgpu-native/`:
- Headers: `include/webgpu/webgpu.h`, `include/webgpu/wgpu.h`
- Library: `lib/libwgpu_native.a` (33MB static library)

### Build GPU Worker

```bash
zig build gpu-worker
```

This produces `zig-out/bin/edgebox-gpu-worker` (6.6MB) which:
- Links wgpu-native statically
- Links Metal framework on macOS
- Links Vulkan on Linux

## IPC Protocol

### Command Format

```
┌──────────────────────────────────────┐
│ Command Type (1 byte)                │
├──────────────────────────────────────┤
│ Payload Length (4 bytes, LE)         │
├──────────────────────────────────────┤
│ Payload (variable)                   │
└──────────────────────────────────────┘
```

### Response Format

```
┌──────────────────────────────────────┐
│ Response Code (4 bytes, LE, i32)     │
├──────────────────────────────────────┤
│ Data Length (4 bytes, LE)            │
├──────────────────────────────────────┤
│ Data (variable)                      │
└──────────────────────────────────────┘
```

### Command Types

| Code | Name | Payload |
|------|------|---------|
| 0x01 | request_adapter | (none) |
| 0x02 | request_device | (none) |
| 0x10 | create_buffer | size(8) + usage(4) |
| 0x11 | destroy_buffer | handle(4) |
| 0x12 | write_buffer | handle(4) + offset(8) + data |
| 0x13 | read_buffer | handle(4) + offset(8) + size(8) |
| 0x20 | create_shader_module | wgsl_source |
| 0x30 | create_compute_pipeline | shader(4) + entry_len(4) + entry |
| 0x41 | dispatch_workgroups | x(4) + y(4) + z(4) |
| 0xF0 | ping | (none) |
| 0xFF | shutdown | (none) |

## Configuration (.edgebox.json)

```json
{
    "gpu": {
        "enabled": true,
        "max_memory_bytes": 268435456,
        "max_dispatch_calls": 100,
        "max_workgroups": 65536,
        "timeout_seconds": 10,
        "shader_validation": true,
        "max_shader_instructions": 10000,
        "allowed_features": ["compute"]
    }
}
```

## WebGPU WASM Imports (Phase 3)

When implementing WASM imports for WebGPU API:

```zig
// Example WASM import signatures
extern fn wgpuDeviceCreateBuffer(device: u32, desc: [*]const u8) u32;
extern fn wgpuQueueWriteBuffer(queue: u32, buffer: u32, offset: u64, data: [*]const u8, size: u32) void;
extern fn wgpuComputePassEncoderDispatchWorkgroups(encoder: u32, x: u32, y: u32, z: u32) void;
```

These imports will be provided by EdgeBox and forwarded to the GPU sandbox.

## Testing

```bash
# Unit tests
zig build test

# Integration test (requires GPU)
./zig-out/bin/edgebox test/webgpu_compute.js
```

## References

- [wgpu-native](https://github.com/gfx-rs/wgpu-native) - Native WebGPU implementation
- [bronter/wgpu_native_zig](https://github.com/bronter/wgpu_native_zig) - Zig bindings
- [WebGPU Spec](https://www.w3.org/TR/webgpu/) - W3C specification
- [WGSL Spec](https://www.w3.org/TR/WGSL/) - WebGPU Shading Language
