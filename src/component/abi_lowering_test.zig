/// Unit tests for ABI lowering helper functions
/// Tests the core memory management functions in import_resolver.zig

const std = @import("std");
const testing = std.testing;

// WAMR C API
const c = @cImport({
    @cInclude("wasm_export.h");
});

// Simple test WASM module that exports memory
// This module is compiled from:
//   (module (memory (export "memory") 1))
const TEST_WASM_MODULE = [_]u8{
    0x00, 0x61, 0x73, 0x6d, // magic: \0asm
    0x01, 0x00, 0x00, 0x00, // version: 1
    0x05, 0x03, 0x01, 0x00, 0x01, // memory section: 1 page (64KB)
    0x07, 0x0a, 0x01, 0x06, 0x6d, 0x65, 0x6d, 0x6f, 0x72, 0x79, 0x02, 0x00, // export "memory"
};

/// Test fixture for WAMR runtime
const WamrTestFixture = struct {
    module: c.wasm_module_t,
    module_inst: c.wasm_module_inst_t,
    exec_env: c.wasm_exec_env_t,
    allocator: std.mem.Allocator,

    /// Initialize WAMR runtime with test module
    fn init(allocator: std.mem.Allocator) !WamrTestFixture {
        // Initialize WAMR runtime
        _ = c.wasm_runtime_init();

        // Load test WASM module
        var error_buf: [128]u8 = undefined;
        const module = c.wasm_runtime_load(
            &TEST_WASM_MODULE,
            TEST_WASM_MODULE.len,
            &error_buf,
            error_buf.len,
        );
        if (module == null) {
            std.debug.print("Failed to load test WASM module: {s}\n", .{error_buf});
            return error.WasmLoadFailed;
        }

        // Instantiate module
        const stack_size: u32 = 8192;
        const heap_size: u32 = 8192;
        const module_inst = c.wasm_runtime_instantiate(
            module,
            stack_size,
            heap_size,
            &error_buf,
            error_buf.len,
        );
        if (module_inst == null) {
            std.debug.print("Failed to instantiate WASM module: {s}\n", .{error_buf});
            c.wasm_runtime_unload(module);
            return error.WasmInstantiateFailed;
        }

        // Create execution environment
        const exec_env = c.wasm_runtime_create_exec_env(module_inst, stack_size);
        if (exec_env == null) {
            c.wasm_runtime_deinstantiate(module_inst);
            c.wasm_runtime_unload(module);
            return error.ExecEnvCreateFailed;
        }

        return WamrTestFixture{
            .module = module,
            .module_inst = module_inst,
            .exec_env = exec_env,
            .allocator = allocator,
        };
    }

    /// Cleanup WAMR runtime
    fn deinit(self: *WamrTestFixture) void {
        c.wasm_runtime_destroy_exec_env(self.exec_env);
        c.wasm_runtime_deinstantiate(self.module_inst);
        c.wasm_runtime_unload(self.module);
        c.wasm_runtime_destroy();
    }

    /// Get WASM memory size
    fn getMemorySize(self: *WamrTestFixture) u32 {
        const memory = c.wasm_runtime_get_default_memory(self.module_inst);
        if (memory == null) return 0;
        return c.wasm_runtime_get_memory_data_size(self.module_inst);
    }
};

// Import the helper functions we're testing
// Note: These are private functions in import_resolver.zig, so we test them indirectly
// through the public bridge functions. For now, we test the WAMR APIs directly.

test "WAMR allocation and memory access" {
    var fixture = try WamrTestFixture.init(testing.allocator);
    defer fixture.deinit();

    // Test allocation
    var native_ptr: ?*anyopaque = null;
    const size: u32 = 100;
    const app_offset = c.wasm_runtime_module_malloc(
        fixture.module_inst,
        size,
        @ptrCast(&native_ptr),
    );

    try testing.expect(app_offset != 0);
    try testing.expect(native_ptr != null);

    // Write test data
    const test_data = "Hello, WASM!";
    const dest: [*]u8 = @ptrCast(native_ptr.?);
    @memcpy(dest[0..test_data.len], test_data);

    // Validate and read back
    const module_inst = c.wasm_runtime_get_module_inst(fixture.exec_env);
    try testing.expect(module_inst != null);

    const is_valid = c.wasm_runtime_validate_app_addr(
        module_inst,
        app_offset,
        @intCast(test_data.len),
    );
    try testing.expect(is_valid);

    // Convert back to native pointer and verify
    const read_ptr = c.wasm_runtime_addr_app_to_native(module_inst, app_offset);
    try testing.expect(read_ptr != null);

    const read_bytes: [*]const u8 = @ptrCast(read_ptr.?);
    const read_slice = read_bytes[0..test_data.len];
    try testing.expectEqualStrings(test_data, read_slice);

    // Free memory
    c.wasm_runtime_module_free(fixture.module_inst, app_offset);
}

test "WAMR memory bounds validation" {
    var fixture = try WamrTestFixture.init(testing.allocator);
    defer fixture.deinit();

    const module_inst = c.wasm_runtime_get_module_inst(fixture.exec_env);
    try testing.expect(module_inst != null);

    // Valid address (start of memory)
    const valid = c.wasm_runtime_validate_app_addr(module_inst, 0, 100);
    try testing.expect(valid);

    // Invalid address (beyond memory size)
    const memory_size = fixture.getMemorySize();
    const invalid = c.wasm_runtime_validate_app_addr(
        module_inst,
        memory_size + 1000,
        100,
    );
    try testing.expect(!invalid);
}

test "WAMR address conversion round-trip" {
    var fixture = try WamrTestFixture.init(testing.allocator);
    defer fixture.deinit();

    const module_inst = c.wasm_runtime_get_module_inst(fixture.exec_env);
    try testing.expect(module_inst != null);

    // Allocate memory
    var native_ptr: ?*anyopaque = null;
    const app_offset = c.wasm_runtime_module_malloc(
        fixture.module_inst,
        64,
        @ptrCast(&native_ptr),
    );
    try testing.expect(app_offset != 0);

    // Convert WASM offset to native pointer
    const converted_ptr = c.wasm_runtime_addr_app_to_native(module_inst, app_offset);
    try testing.expect(converted_ptr != null);

    // Verify it's the same pointer
    try testing.expectEqual(native_ptr, converted_ptr);

    // Convert back to WASM offset
    const converted_offset = c.wasm_runtime_addr_native_to_app(module_inst, native_ptr);
    try testing.expectEqual(app_offset, converted_offset);

    // Cleanup
    c.wasm_runtime_module_free(fixture.module_inst, app_offset);
}

test "WAMR multiple allocations" {
    var fixture = try WamrTestFixture.init(testing.allocator);
    defer fixture.deinit();

    // Allocate multiple blocks
    const allocations = [_]u32{ 10, 100, 1000 };
    var offsets: [allocations.len]u32 = undefined;

    for (allocations, 0..) |size, i| {
        var native_ptr: ?*anyopaque = null;
        const offset = c.wasm_runtime_module_malloc(
            fixture.module_inst,
            size,
            @ptrCast(&native_ptr),
        );
        try testing.expect(offset != 0);
        offsets[i] = offset;
    }

    // Verify all allocations are valid
    const module_inst = c.wasm_runtime_get_module_inst(fixture.exec_env);
    for (allocations, offsets) |size, offset| {
        const is_valid = c.wasm_runtime_validate_app_addr(module_inst, offset, size);
        try testing.expect(is_valid);
    }

    // Free all allocations
    for (offsets) |offset| {
        c.wasm_runtime_module_free(fixture.module_inst, offset);
    }
}

test "WAMR zero-length allocation" {
    var fixture = try WamrTestFixture.init(testing.allocator);
    defer fixture.deinit();

    // Allocate zero bytes (should succeed but return null or special value)
    var native_ptr: ?*anyopaque = null;
    const offset = c.wasm_runtime_module_malloc(
        fixture.module_inst,
        0,
        @ptrCast(&native_ptr),
    );

    // Different WAMR versions may handle this differently
    // Just verify it doesn't crash
    _ = offset;
}

test "WAMR string write and read" {
    var fixture = try WamrTestFixture.init(testing.allocator);
    defer fixture.deinit();

    const test_string = "The quick brown fox jumps over the lazy dog";

    // Allocate memory for string
    var native_ptr: ?*anyopaque = null;
    const app_offset = c.wasm_runtime_module_malloc(
        fixture.module_inst,
        @intCast(test_string.len),
        @ptrCast(&native_ptr),
    );
    try testing.expect(app_offset != 0);
    try testing.expect(native_ptr != null);

    // Write string
    const dest: [*]u8 = @ptrCast(native_ptr.?);
    @memcpy(dest[0..test_string.len], test_string);

    // Read string back using WAMR APIs
    const module_inst = c.wasm_runtime_get_module_inst(fixture.exec_env);
    const read_ptr = c.wasm_runtime_addr_app_to_native(module_inst, app_offset);
    try testing.expect(read_ptr != null);

    const read_bytes: [*]const u8 = @ptrCast(read_ptr.?);
    const read_slice = read_bytes[0..test_string.len];
    try testing.expectEqualStrings(test_string, read_slice);

    // Cleanup
    c.wasm_runtime_module_free(fixture.module_inst, app_offset);
}
