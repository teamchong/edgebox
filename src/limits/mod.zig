/// CPU and Execution Limits Module
///
/// Provides CPU time and wall-clock timeout enforcement for sandboxed execution.
/// Uses setrlimit-based CPU limits and a watchdog thread for wall-clock timeouts.
///
/// Note: Instruction metering only works in interpreter mode (not AOT).
const std = @import("std");
const builtin = @import("builtin");

// =============================================================================
// CPU/Execution Limit Helpers (setrlimit-based, works in both modes)
// =============================================================================

/// Global flag set by signal handlers to indicate timeout
pub var g_cpu_limit_exceeded: std.atomic.Value(bool) = std.atomic.Value(bool).init(false);

/// Set CPU time limit using setrlimit (Unix only)
/// Returns true if limit was set successfully
pub fn setCpuTimeLimit(seconds: u32) bool {
    if (builtin.os.tag == .windows) return false;
    if (seconds == 0) return false;

    const rlim = std.posix.rlimit{
        .cur = seconds,
        .max = seconds,
    };
    std.posix.setrlimit(.CPU, rlim) catch return false;
    return true;
}

/// Restore unlimited CPU time (for daemon mode reuse)
pub fn restoreCpuTimeLimit() void {
    if (builtin.os.tag == .windows) return;

    const unlimited = std.posix.rlimit{
        .cur = std.posix.RLIM.INFINITY,
        .max = std.posix.RLIM.INFINITY,
    };
    std.posix.setrlimit(.CPU, unlimited) catch {};
}

/// Setup signal handlers for CPU limit exceeded (SIGXCPU) and termination (SIGTERM)
pub fn setupCpuLimitSignalHandlers() void {
    if (builtin.os.tag == .windows) return;

    // SIGXCPU handler - sent when CPU limit is exceeded
    const xcpu_handler = std.posix.Sigaction{
        .handler = .{ .handler = cpuLimitExceededHandler },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    };
    _ = std.posix.sigaction(std.posix.SIG.XCPU, &xcpu_handler, null);
}

fn cpuLimitExceededHandler(sig: i32) callconv(.c) void {
    _ = sig;
    g_cpu_limit_exceeded.store(true, .release);
    // Exit with standard timeout exit code (124 matches GNU timeout)
    std.debug.print("[EDGEBOX] CPU time limit exceeded\n", .{});
    std.process.exit(124);
}

/// Watchdog context for wall-clock timeout enforcement
pub const WatchdogContext = struct {
    deadline_ns: i128,
    cancelled: std.atomic.Value(bool),
    main_pid: if (builtin.os.tag != .windows) std.posix.pid_t else u32,
};

/// Get current process ID (Unix only)
pub fn getPid() if (builtin.os.tag != .windows) std.posix.pid_t else u32 {
    if (builtin.os.tag == .windows) return 0;
    // Use the C library getpid
    return @import("std").c.getpid();
}

/// Watchdog thread function - monitors wall-clock time and kills process on timeout
pub fn watchdogThread(ctx: *WatchdogContext) void {
    while (std.time.nanoTimestamp() < ctx.deadline_ns) {
        if (ctx.cancelled.load(.acquire)) return;
        std.Thread.sleep(50 * std.time.ns_per_ms); // Check every 50ms
    }

    // Timeout reached - atomically check and set to prevent race with cancellation
    // Use cmpxchgStrong: if cancelled is false (0), set it to true (1) and proceed with kill
    // If it was already true, someone else cancelled - don't kill
    if (ctx.cancelled.cmpxchgStrong(false, true, .acq_rel, .acquire) == null) {
        // We won the race - we set cancelled=true, so we should send the signal
        std.debug.print("[EDGEBOX] Execution timeout exceeded (wall-clock)\n", .{});
        // Send SIGTERM to self
        if (builtin.os.tag != .windows) {
            std.posix.kill(ctx.main_pid, std.posix.SIG.TERM) catch {};
        }
    }
    // else: cmpxchg returned the old value (true), meaning it was already cancelled
}
