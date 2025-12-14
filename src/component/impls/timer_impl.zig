/// Timer interface implementation for Component Model
/// Implements the WIT interface defined in wit/edgebox-timer.wit

const std = @import("std");

// These will be provided by the importer
pub const Value = @import("../native_registry.zig").Value;
pub const NativeRegistry = @import("../native_registry.zig").NativeRegistry;

/// Timer state (for PoC - in production this would integrate with event loop)
var next_timer_id: u32 = 1;
var timers: std.AutoHashMap(u32, TimerInfo) = undefined;
var timers_initialized = false;

const TimerInfo = struct {
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
