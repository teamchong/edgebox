/// EdgeBox Async Runtime
/// Provides true async execution using OS threads for HTTP and process operations.
///
/// Architecture:
/// - Uses std.Thread for background execution
/// - Tasks are spawned with spawn() and complete in background
/// - Poll with isComplete() to check status (non-blocking)
/// - Thread-safe with mutex protection

const std = @import("std");

/// Async task state (u8 backing for atomic compatibility)
pub const TaskState = enum(u8) {
    pending = 0, // Created but not started
    running = 1, // Executing in background thread
    complete = 2, // Finished successfully
    failed = 3, // Finished with error
};

/// Async task handle
pub const Task = struct {
    id: u32,
    state: std.atomic.Value(TaskState),
    thread: ?std.Thread,
    context: ?*anyopaque,
    func: *const fn (?*anyopaque) void,

    pub fn isComplete(self: *const Task) bool {
        const state = self.state.load(.acquire);
        return state == .complete or state == .failed;
    }
};

/// Async runtime manages background task execution
pub const AsyncRuntime = struct {
    allocator: std.mem.Allocator,
    tasks: [64]?*Task,
    next_id: u32,
    mutex: std.Thread.Mutex,

    pub fn init(allocator: std.mem.Allocator) AsyncRuntime {
        return .{
            .allocator = allocator,
            .tasks = [_]?*Task{null} ** 64,
            .next_id = 0,
            .mutex = .{},
        };
    }

    pub fn deinit(self: *AsyncRuntime) void {
        // Wait for all running tasks and clean up
        for (&self.tasks) |*slot| {
            if (slot.*) |task| {
                // Wait for thread to finish if still running
                if (task.thread) |thread| {
                    thread.join();
                }
                self.allocator.destroy(task);
                slot.* = null;
            }
        }
    }

    /// Spawn a task to run in background thread
    /// Returns task ID that can be used to poll for completion
    pub fn spawn(
        self: *AsyncRuntime,
        func: *const fn (?*anyopaque) void,
        context: ?*anyopaque,
    ) !u32 {
        self.mutex.lock();
        defer self.mutex.unlock();

        // Allocate task
        const task = try self.allocator.create(Task);
        errdefer self.allocator.destroy(task);

        const id = self.next_id;
        self.next_id = (self.next_id + 1) % 64;

        // Clean up old task in this slot if exists
        if (self.tasks[id]) |old_task| {
            if (old_task.thread) |thread| {
                thread.join();
            }
            self.allocator.destroy(old_task);
        }

        task.* = .{
            .id = id,
            .state = std.atomic.Value(TaskState).init(.pending),
            .thread = null,
            .context = context,
            .func = func,
        };

        self.tasks[id] = task;

        // Spawn OS thread for background execution
        task.state.store(.running, .release);
        task.thread = std.Thread.spawn(.{}, taskRunner, .{task}) catch {
            // If thread spawn fails, execute synchronously
            task.func(task.context);
            task.state.store(.complete, .release);
            return id;
        };

        return id;
    }

    /// Check if task is complete (non-blocking)
    pub fn isComplete(self: *AsyncRuntime, id: u32) bool {
        if (id >= 64) return true;
        const task = self.tasks[id] orelse return true;
        return task.isComplete();
    }

    /// Get task state
    pub fn getState(self: *AsyncRuntime, id: u32) TaskState {
        if (id >= 64) return .failed;
        const task = self.tasks[id] orelse return .failed;
        return task.state.load(.acquire);
    }

    /// Free a completed task
    pub fn free(self: *AsyncRuntime, id: u32) void {
        if (id >= 64) return;

        self.mutex.lock();
        defer self.mutex.unlock();

        if (self.tasks[id]) |task| {
            if (task.thread) |thread| {
                thread.join();
            }
            self.allocator.destroy(task);
            self.tasks[id] = null;
        }
    }
};

/// Thread runner function - executes task and updates state
fn taskRunner(task: *Task) void {
    // Execute the user function
    task.func(task.context);

    // Mark as complete
    task.state.store(.complete, .release);
}

// Global runtime instance
var g_runtime: ?AsyncRuntime = null;
var g_runtime_mutex: std.Thread.Mutex = .{};

/// Initialize the global async runtime
pub fn init(allocator: std.mem.Allocator) void {
    g_runtime_mutex.lock();
    defer g_runtime_mutex.unlock();

    if (g_runtime == null) {
        g_runtime = AsyncRuntime.init(allocator);
    }
}

/// Deinitialize the global async runtime
pub fn deinit() void {
    g_runtime_mutex.lock();
    defer g_runtime_mutex.unlock();

    if (g_runtime) |*rt| {
        rt.deinit();
        g_runtime = null;
    }
}

/// Get the global runtime (returns null if not initialized)
pub fn getRuntime() ?*AsyncRuntime {
    return if (g_runtime) |*rt| rt else null;
}

// Tests
test "AsyncRuntime - basic spawn and complete" {
    const allocator = std.testing.allocator;

    var runtime = AsyncRuntime.init(allocator);
    defer runtime.deinit();

    const TestCtx = struct {
        value: std.atomic.Value(u32),
    };

    var ctx = TestCtx{ .value = std.atomic.Value(u32).init(0) };

    const TestFn = struct {
        fn run(c: ?*anyopaque) void {
            const test_ctx: *TestCtx = @ptrCast(@alignCast(c.?));
            test_ctx.value.store(42, .release);
        }
    };

    const id = try runtime.spawn(TestFn.run, &ctx);

    // Poll until complete
    while (!runtime.isComplete(id)) {
        std.Thread.yield();
    }

    try std.testing.expectEqual(@as(u32, 42), ctx.value.load(.acquire));
    try std.testing.expectEqual(TaskState.complete, runtime.getState(id));

    runtime.free(id);
}

test "AsyncRuntime - multiple concurrent tasks" {
    const allocator = std.testing.allocator;

    var runtime = AsyncRuntime.init(allocator);
    defer runtime.deinit();

    const Counter = struct {
        value: std.atomic.Value(u32),
    };

    var counter = Counter{ .value = std.atomic.Value(u32).init(0) };

    const IncrementFn = struct {
        fn run(c: ?*anyopaque) void {
            const cnt: *Counter = @ptrCast(@alignCast(c.?));
            _ = cnt.value.fetchAdd(1, .acq_rel);
        }
    };

    // Spawn 10 tasks
    var ids: [10]u32 = undefined;
    for (&ids) |*id| {
        id.* = try runtime.spawn(IncrementFn.run, &counter);
    }

    // Wait for all to complete
    for (ids) |id| {
        while (!runtime.isComplete(id)) {
            std.Thread.yield();
        }
    }

    try std.testing.expectEqual(@as(u32, 10), counter.value.load(.acquire));
}
