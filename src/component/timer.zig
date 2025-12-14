/// Timer interface implementation for Component Model
/// Implements the WIT interface defined in wit/edgebox-timer.wit

const std = @import("std");
const native_registry = @import("native_registry.zig");
const Value = native_registry.Value;
const NativeRegistry = native_registry.NativeRegistry;

/// Timer state (for PoC - in production this would integrate with event loop)
var next_timer_id: u32 = 1;
var timers: std.AutoHashMap(u32, TimerInfo) = undefined;
var timers_initialized = false;

pub const TimerInfo = struct {
    callback_id: u32,
    delay_or_interval: u64,
    is_interval: bool,
};

/// Initialize timer subsystem
pub fn init(allocator: std.mem.Allocator) void {
    if (!timers_initialized) {
        timers = std.AutoHashMap(u32, TimerInfo).init(allocator);
        timers_initialized = true;
    }
}

/// Cleanup timer subsystem
pub fn deinit() void {
    if (timers_initialized) {
        timers.deinit();
        timers_initialized = false;
    }
}

/// Register timer implementations with native registry
pub fn registerTimerImpl(registry: *NativeRegistry) !void {
    try registry.register("timer", "set-timeout", setTimeout);
    try registry.register("timer", "clear-timeout", clearTimeout);
    try registry.register("timer", "set-interval", setInterval);
    try registry.register("timer", "clear-interval", clearInterval);
}

/// Implementation of set-timeout
fn setTimeout(args: []const Value) !Value {
    const callback_id = try args[0].asU32();
    const delay_ms = try args[1].asU64();

    const timer_id = next_timer_id;
    next_timer_id += 1;

    // Store timer info
    try timers.put(timer_id, TimerInfo{
        .callback_id = callback_id,
        .delay_or_interval = delay_ms,
        .is_interval = false,
    });

    return Value{ .u32 = timer_id };
}

/// Implementation of clear-timeout
fn clearTimeout(args: []const Value) !Value {
    const timer_id = try args[0].asU32();

    _ = timers.remove(timer_id);

    return Value{ .void = {} };
}

/// Implementation of set-interval
fn setInterval(args: []const Value) !Value {
    const callback_id = try args[0].asU32();
    const interval_ms = try args[1].asU64();

    const timer_id = next_timer_id;
    next_timer_id += 1;

    // Store timer info
    try timers.put(timer_id, TimerInfo{
        .callback_id = callback_id,
        .delay_or_interval = interval_ms,
        .is_interval = true,
    });

    return Value{ .u32 = timer_id };
}

/// Implementation of clear-interval
fn clearInterval(args: []const Value) !Value {
    const timer_id = try args[0].asU32();

    _ = timers.remove(timer_id);

    return Value{ .void = {} };
}

/// Get timer info (for testing)
pub fn getTimerInfo(timer_id: u32) ?TimerInfo {
    return timers.get(timer_id);
}

// Tests
test "Timer - set and clear timeout" {
    init(std.testing.allocator);
    defer deinit();

    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    try registerTimerImpl(&registry);

    // Set timeout
    const set_args = [_]Value{
        Value{ .u32 = 42 }, // callback_id
        Value{ .u64 = 1000 }, // delay_ms
    };
    const result = try registry.call("timer", "set-timeout", &set_args);
    const timer_id = try result.asU32();

    try std.testing.expect(timer_id > 0);

    // Verify timer was stored
    const info = getTimerInfo(timer_id).?;
    try std.testing.expectEqual(@as(u32, 42), info.callback_id);
    try std.testing.expectEqual(@as(u64, 1000), info.delay_or_interval);
    try std.testing.expect(!info.is_interval);

    // Clear timeout
    const clear_args = [_]Value{
        Value{ .u32 = timer_id },
    };
    _ = try registry.call("timer", "clear-timeout", &clear_args);

    // Verify timer was removed
    try std.testing.expect(getTimerInfo(timer_id) == null);
}

test "Timer - set and clear interval" {
    init(std.testing.allocator);
    defer deinit();

    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    try registerTimerImpl(&registry);

    // Set interval
    const set_args = [_]Value{
        Value{ .u32 = 99 }, // callback_id
        Value{ .u64 = 500 }, // interval_ms
    };
    const result = try registry.call("timer", "set-interval", &set_args);
    const timer_id = try result.asU32();

    try std.testing.expect(timer_id > 0);

    // Verify timer was stored
    const info = getTimerInfo(timer_id).?;
    try std.testing.expectEqual(@as(u32, 99), info.callback_id);
    try std.testing.expectEqual(@as(u64, 500), info.delay_or_interval);
    try std.testing.expect(info.is_interval);

    // Clear interval
    const clear_args = [_]Value{
        Value{ .u32 = timer_id },
    };
    _ = try registry.call("timer", "clear-interval", &clear_args);

    // Verify timer was removed
    try std.testing.expect(getTimerInfo(timer_id) == null);
}

test "Timer - multiple timers" {
    init(std.testing.allocator);
    defer deinit();

    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    try registerTimerImpl(&registry);

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
    try std.testing.expect(getTimerInfo(timer1) != null);
    try std.testing.expect(getTimerInfo(timer2) != null);
    try std.testing.expect(getTimerInfo(timer3) != null);

    // Clear timer2
    _ = try registry.call("timer", "clear-timeout", &[_]Value{Value{ .u32 = timer2 }});

    // timer1 and timer3 should still exist
    try std.testing.expect(getTimerInfo(timer1) != null);
    try std.testing.expect(getTimerInfo(timer2) == null);
    try std.testing.expect(getTimerInfo(timer3) != null);
}
