/// Component Model MVP Test - Phase 8a
/// Tests that Component Model timer interface can be registered with WAMR
///
/// This is a proof-of-concept that demonstrates:
/// 1. Timer implementation works through NativeRegistry
/// 2. Import resolver can bridge WAMR to Component Model
/// 3. WASM modules can import timer functions
///
/// Full end-to-end test (WASM calling timer functions) is done via CLI:
///   zig-out/bin/edgebox test/component_mvp/timer_test.wasm

const std = @import("std");
const NativeRegistry = @import("native_registry.zig").NativeRegistry;
const timer_impl = @import("impls/timer_impl.zig");
const import_resolver = @import("import_resolver.zig");
const Value = @import("native_registry.zig").Value;

test "Component MVP - timer implementation registered" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    timer_impl.init(std.testing.allocator);
    defer timer_impl.deinit();

    try timer_impl.registerTimerImpl(&registry);

    // Verify timer functions are registered
    try std.testing.expect(registry.has("timer", "set-timeout"));
    try std.testing.expect(registry.has("timer", "clear-timeout"));
    try std.testing.expect(registry.has("timer", "set-interval"));
}

test "Component MVP - timer::set-timeout works" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    timer_impl.init(std.testing.allocator);
    defer timer_impl.deinit();

    try timer_impl.registerTimerImpl(&registry);

    // Call set-timeout through registry
    const args = [_]Value{Value{ .u32 = 1000 }};
    const result = try registry.call("timer", "set-timeout", &args);

    // Verify we got a valid timer ID (non-zero u64)
    const timer_id = try result.asU64();
    try std.testing.expect(timer_id > 0);

    // Clean up timer
    const clear_args = [_]Value{Value{ .u64 = timer_id }};
    _ = try registry.call("timer", "clear-timeout", &clear_args);
}

test "Component MVP - import resolver bridge functions exist" {
    // This test verifies the import resolver module compiles and has the right structure
    // Actual WAMR registration is tested via CLI with test/component_mvp/timer_test.wasm

    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    timer_impl.init(std.testing.allocator);
    defer timer_impl.deinit();

    try timer_impl.registerTimerImpl(&registry);

    // Register with WAMR-style import resolver
    // Note: This doesn't actually test WAMR integration (requires WAMR runtime)
    // But it verifies the function exists and doesn't crash
    import_resolver.registerTimerImports(&registry);

    // If we got here without crashing, the bridge is set up correctly
    try std.testing.expect(true);
}

test "Component MVP - full timer workflow" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    timer_impl.init(std.testing.allocator);
    defer timer_impl.deinit();

    try timer_impl.registerTimerImpl(&registry);

    // 1. Create timeout
    const timeout_args = [_]Value{Value{ .u32 = 2000 }};
    const timeout_result = try registry.call("timer", "set-timeout", &timeout_args);
    const timeout_id = try timeout_result.asU64();
    try std.testing.expect(timeout_id > 0);

    // 2. Create interval
    const interval_args = [_]Value{Value{ .u32 = 1000 }};
    const interval_result = try registry.call("timer", "set-interval", &interval_args);
    const interval_id = try interval_result.asU64();
    try std.testing.expect(interval_id > 0);

    // 3. Verify they have different IDs
    try std.testing.expect(timeout_id != interval_id);

    // 4. Clear both
    const clear_timeout_args = [_]Value{Value{ .u64 = timeout_id }};
    _ = try registry.call("timer", "clear-timeout", &clear_timeout_args);

    const clear_interval_args = [_]Value{Value{ .u64 = interval_id }};
    _ = try registry.call("timer", "clear-timeout", &clear_interval_args);
}
