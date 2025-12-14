/// Test runner for timer implementation
/// This file sets up the proper imports for testing

const std = @import("std");

// Import modules
const native_registry_module = @import("../native_registry.zig");
const timer_module = @import("timer_impl.zig");

const Value = native_registry_module.Value;
const NativeRegistry = native_registry_module.NativeRegistry;

test "Timer - set and clear timeout" {
    timer_module.init(std.testing.allocator);
    defer timer_module.deinit();

    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    try timer_module.registerTimerImpl(&registry);

    // Set timeout
    const set_args = [_]Value{
        Value{ .u32 = 42 }, // callback_id
        Value{ .u64 = 1000 }, // delay_ms
    };
    const result = try registry.call("timer", "set-timeout", &set_args);
    const timer_id = try result.asU32();

    try std.testing.expect(timer_id > 0);

    // Verify timer was stored
    const info = timer_module.getTimerInfo(timer_id).?;
    try std.testing.expectEqual(@as(u32, 42), info.callback_id);
    try std.testing.expectEqual(@as(u64, 1000), info.delay_or_interval);
    try std.testing.expect(!info.is_interval);

    // Clear timeout
    const clear_args = [_]Value{
        Value{ .u32 = timer_id },
    };
    _ = try registry.call("timer", "clear-timeout", &clear_args);

    // Verify timer was removed
    try std.testing.expect(timer_module.getTimerInfo(timer_id) == null);
}

test "Timer - set and clear interval" {
    timer_module.init(std.testing.allocator);
    defer timer_module.deinit();

    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    try timer_module.registerTimerImpl(&registry);

    // Set interval
    const set_args = [_]Value{
        Value{ .u32 = 99 }, // callback_id
        Value{ .u64 = 500 }, // interval_ms
    };
    const result = try registry.call("timer", "set-interval", &set_args);
    const timer_id = try result.asU32();

    try std.testing.expect(timer_id > 0);

    // Verify timer was stored
    const info = timer_module.getTimerInfo(timer_id).?;
    try std.testing.expectEqual(@as(u32, 99), info.callback_id);
    try std.testing.expectEqual(@as(u64, 500), info.delay_or_interval);
    try std.testing.expect(info.is_interval);

    // Clear interval
    const clear_args = [_]Value{
        Value{ .u32 = timer_id },
    };
    _ = try registry.call("timer", "clear-interval", &clear_args);

    // Verify timer was removed
    try std.testing.expect(timer_module.getTimerInfo(timer_id) == null);
}

test "Timer - multiple timers" {
    timer_module.init(std.testing.allocator);
    defer timer_module.deinit();

    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    try timer_module.registerTimerImpl(&registry);

    // Create 3 timers
    const timer1 = try (try registry.call("timer", "set-timeout", &[_]Value{
        Value{ .u32 = 1 },
        Value{ .u64 = 100 },
    })).asU32();

    const timer2 = try (try registry.call("timer", "set-timeout", &[_]Value{
        Value{ .u32 = 2 },
        Value{ .u64 = 200 },
    })).asU32();

    const timer3 = try (try registry.call("timer", "set-interval", &[_]Value{
        Value{ .u32 = 3 },
        Value{ .u64 = 300 },
    })).asU32();

    // All should have unique IDs
    try std.testing.expect(timer1 != timer2);
    try std.testing.expect(timer2 != timer3);
    try std.testing.expect(timer1 != timer3);

    // All should exist
    try std.testing.expect(timer_module.getTimerInfo(timer1) != null);
    try std.testing.expect(timer_module.getTimerInfo(timer2) != null);
    try std.testing.expect(timer_module.getTimerInfo(timer3) != null);

    // Clear timer2
    _ = try registry.call("timer", "clear-timeout", &[_]Value{Value{ .u32 = timer2 }});

    // timer1 and timer3 should still exist
    try std.testing.expect(timer_module.getTimerInfo(timer1) != null);
    try std.testing.expect(timer_module.getTimerInfo(timer2) == null);
    try std.testing.expect(timer_module.getTimerInfo(timer3) != null);
}
