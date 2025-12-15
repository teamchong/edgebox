/// Process Implementation for Component Model
/// Uses std.process.Child directly for process spawning
///
/// Architecture:
/// This implementation directly uses Zig's std.process.Child
/// without going through dispatch layers. Component Model impl
/// calls std library directly.

const std = @import("std");
const async_runtime = @import("../async_runtime.zig");
const native_registry = @import("../native_registry.zig");
const Value = native_registry.Value;
const NativeRegistry = native_registry.NativeRegistry;
const ProcessOutput = native_registry.ProcessOutput;
const SpawnOptions = native_registry.SpawnOptions;

/// Process error enum (matches WIT process-error)
pub const ProcessError = enum(u32) {
    permission_denied = 0,
    command_not_found = 1,
    timeout = 2,
    invalid_input = 3,
    spawn_failed = 4,
    operation_failed = 5,
};

/// Global allocator for process operations
var process_allocator: ?std.mem.Allocator = null;

/// Max output size for process output (10MB)
const MAX_OUTPUT_SIZE = 10 * 1024 * 1024;

/// Async spawn state (u8 backing for atomic compatibility)
const AsyncState = enum(u8) {
    pending = 0, // Created, child not spawned
    running = 1, // Child process running, waiting for output
    complete = 2, // Finished successfully
    failed = 3, // Finished with error
};

/// Async spawn storage
const AsyncSpawn = struct {
    child: ?std.process.Child,
    stdout_data: ?[]u8,
    stderr_data: ?[]u8,
    exit_code: ?i32,
    state: std.atomic.Value(AsyncState) = std.atomic.Value(AsyncState).init(.pending),
    thread_id: ?u32 = null, // Async runtime task ID
    allocator: std.mem.Allocator,

    fn deinit(self: *AsyncSpawn) void {
        if (self.stdout_data) |d| self.allocator.free(d);
        if (self.stderr_data) |d| self.allocator.free(d);
        if (self.child) |*c| {
            // Close any remaining handles
            if (c.stdout) |*stdout| stdout.close();
            if (c.stderr) |*stderr| stderr.close();
            if (c.stdin) |*stdin| stdin.close();
        }
    }

    fn isComplete(self: *const AsyncSpawn) bool {
        const s = self.state.load(.acquire);
        return s == .complete or s == .failed;
    }
};

var g_async_spawns: [64]?*AsyncSpawn = [_]?*AsyncSpawn{null} ** 64;
var g_next_spawn_id: u32 = 0;
var g_spawn_mutex: std.Thread.Mutex = .{};

/// Initialize process implementation
pub fn init(allocator: std.mem.Allocator) void {
    process_allocator = allocator;
}

/// Clean up process implementation
pub fn deinit() void {
    // Free any pending async spawns
    if (process_allocator) |alloc| {
        for (&g_async_spawns) |*maybe_spawn| {
            if (maybe_spawn.*) |spawn| {
                spawn.deinit();
                alloc.destroy(spawn);
                maybe_spawn.* = null;
            }
        }
    }
    process_allocator = null;
}

/// Register process implementations with native registry
pub fn registerProcessImpl(registry: *NativeRegistry) !void {
    try registry.register("process", "spawn-sync", spawnSyncImpl);
    try registry.register("process", "exec-sync", execSyncImpl);
    try registry.register("process", "spawn-start", spawnStartImpl);
    try registry.register("process", "spawn-poll", spawnPollImpl);
    try registry.register("process", "spawn-output", spawnOutputImpl);
    try registry.register("process", "spawn-free", spawnFreeImpl);
}

/// spawn-sync: func(command: string, args: list<string>, options: spawn-options)
///   -> result<process-output, process-error>
fn spawnSyncImpl(args: []const Value) !Value {
    const allocator = process_allocator orelse return error.NotInitialized;
    const command = try args[0].asString();
    const arg_list = try args[1].asListString();
    const options = try args[2].asSpawnOptions();
    _ = options; // TODO: implement timeout

    // Build argv array
    var argv = std.ArrayListUnmanaged([]const u8){};
    defer argv.deinit(allocator);

    try argv.append(allocator, command);
    for (arg_list) |arg| {
        try argv.append(allocator, arg);
    }

    // Spawn child process
    var child = std.process.Child.init(argv.items, allocator);
    child.stdin_behavior = .Pipe;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    child.spawn() catch {
        return Value{ .err = @intFromEnum(ProcessError.spawn_failed) };
    };

    // Close stdin (we're not writing to it for sync)
    if (child.stdin) |*stdin| {
        stdin.close();
        child.stdin = null;
    }

    // Read stdout and stderr using readToEndAlloc (Zig 0.15 style)
    const stdout = if (child.stdout) |f| f.readToEndAlloc(allocator, MAX_OUTPUT_SIZE) catch &[_]u8{} else &[_]u8{};
    const stderr = if (child.stderr) |f| f.readToEndAlloc(allocator, MAX_OUTPUT_SIZE) catch &[_]u8{} else &[_]u8{};

    // Wait for process to complete
    const term = child.wait() catch {
        return Value{ .err = @intFromEnum(ProcessError.operation_failed) };
    };

    const exit_code: i32 = switch (term) {
        .Exited => |code| @intCast(code),
        .Signal => |sig| -@as(i32, @intCast(sig)),
        .Stopped => |sig| -@as(i32, @intCast(sig)),
        .Unknown => -1,
    };

    return Value{ .ok_process_output = .{
        .exit_code = exit_code,
        .stdout = stdout,
        .stderr = stderr,
    } };
}

/// exec-sync: func(command: string, options: spawn-options)
///   -> result<process-output, process-error>
fn execSyncImpl(args: []const Value) !Value {
    const allocator = process_allocator orelse return error.NotInitialized;
    const command = try args[0].asString();
    const options = try args[1].asSpawnOptions();

    // Parse command into program and args using shell
    // Use /bin/sh -c for shell command execution
    const shell_args = [_][]const u8{ "/bin/sh", "-c", command };

    // Build argv
    var argv = std.ArrayListUnmanaged([]const u8){};
    defer argv.deinit(allocator);

    for (shell_args) |arg| {
        try argv.append(allocator, arg);
    }

    // Call spawn-sync internally with shell command
    return try spawnSyncImpl(&[_]Value{
        Value{ .string = "/bin/sh" },
        Value{ .list_string = argv.items[1..] }, // -c and command
        Value{ .spawn_options = options },
    });
}

/// Worker function for background process execution
fn spawnWorker(ctx: ?*anyopaque) void {
    const async_spawn: *AsyncSpawn = @ptrCast(@alignCast(ctx.?));
    async_spawn.state.store(.running, .release);

    const allocator = async_spawn.allocator;

    if (async_spawn.child) |*child| {
        // Read stdout and stderr (blocks until process completes)
        if (child.stdout) |f| {
            async_spawn.stdout_data = f.readToEndAlloc(allocator, MAX_OUTPUT_SIZE) catch null;
        }
        if (child.stderr) |f| {
            async_spawn.stderr_data = f.readToEndAlloc(allocator, MAX_OUTPUT_SIZE) catch null;
        }

        // Wait for process to complete
        if (child.wait()) |term| {
            async_spawn.exit_code = switch (term) {
                .Exited => |code| @intCast(code),
                .Signal => |sig| -@as(i32, @intCast(sig)),
                .Stopped => |sig| -@as(i32, @intCast(sig)),
                .Unknown => -1,
            };
            async_spawn.state.store(.complete, .release);
        } else |_| {
            async_spawn.state.store(.failed, .release);
        }
    } else {
        async_spawn.state.store(.failed, .release);
    }
}

/// spawn-start: func(command: string, args: list<string>, options: spawn-options)
///   -> result<u32, process-error>
fn spawnStartImpl(args: []const Value) !Value {
    const allocator = process_allocator orelse return error.NotInitialized;
    const command = try args[0].asString();
    const arg_list = try args[1].asListString();
    const options = try args[2].asSpawnOptions();
    _ = options; // TODO: implement timeout

    // Build argv array
    var argv = std.ArrayListUnmanaged([]const u8){};
    defer argv.deinit(allocator);

    try argv.append(allocator, command);
    for (arg_list) |arg| {
        try argv.append(allocator, arg);
    }

    // Create async spawn record
    const async_spawn = allocator.create(AsyncSpawn) catch {
        return Value{ .err = @intFromEnum(ProcessError.spawn_failed) };
    };
    errdefer allocator.destroy(async_spawn);

    async_spawn.* = .{
        .child = null,
        .stdout_data = null,
        .stderr_data = null,
        .exit_code = null,
        .allocator = allocator,
    };

    // Spawn child process
    var child = std.process.Child.init(argv.items, allocator);
    child.stdin_behavior = .Pipe;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    child.spawn() catch {
        allocator.destroy(async_spawn);
        return Value{ .err = @intFromEnum(ProcessError.spawn_failed) };
    };

    // Close stdin
    if (child.stdin) |*stdin| {
        stdin.close();
        child.stdin = null;
    }

    async_spawn.child = child;

    // Store in global array (protected by mutex for thread safety)
    g_spawn_mutex.lock();
    const spawn_id = g_next_spawn_id;
    g_next_spawn_id = (g_next_spawn_id + 1) % 64;

    if (g_async_spawns[spawn_id]) |old| {
        old.deinit();
        allocator.destroy(old);
    }
    g_async_spawns[spawn_id] = async_spawn;
    g_spawn_mutex.unlock();

    // Spawn background thread for TRUE ASYNC execution
    if (async_runtime.getRuntime()) |rt| {
        async_spawn.thread_id = rt.spawn(spawnWorker, async_spawn) catch null;
        if (async_spawn.thread_id == null) {
            // Fallback: execute synchronously if spawn fails
            spawnWorker(async_spawn);
        }
    } else {
        // Fallback: execute synchronously if runtime not available
        spawnWorker(async_spawn);
    }

    return Value{ .ok_spawn_id = spawn_id };
}

/// spawn-poll: func(spawn-id: u32) -> result<u32, process-error>
/// Returns: 0=pending, 1=complete
fn spawnPollImpl(args: []const Value) !Value {
    const spawn_id = try args[0].asU32();

    if (spawn_id >= 64) {
        return Value{ .err = @intFromEnum(ProcessError.invalid_input) };
    }

    const async_spawn = g_async_spawns[spawn_id] orelse {
        return Value{ .err = @intFromEnum(ProcessError.invalid_input) };
    };

    // Check state atomically (non-blocking)
    return Value{ .ok_spawn_id = if (async_spawn.isComplete()) @as(u32, 1) else @as(u32, 0) };
}

/// spawn-output: func(spawn-id: u32) -> result<process-output, process-error>
fn spawnOutputImpl(args: []const Value) !Value {
    const allocator = process_allocator orelse return error.NotInitialized;
    const spawn_id = try args[0].asU32();

    if (spawn_id >= 64) {
        return Value{ .err = @intFromEnum(ProcessError.invalid_input) };
    }

    const async_spawn = g_async_spawns[spawn_id] orelse {
        return Value{ .err = @intFromEnum(ProcessError.invalid_input) };
    };

    // Check state
    const state = async_spawn.state.load(.acquire);
    if (state == .pending or state == .running) {
        return Value{ .err = @intFromEnum(ProcessError.operation_failed) };
    }

    if (state == .failed) {
        return Value{ .err = @intFromEnum(ProcessError.operation_failed) };
    }

    // Copy output (caller owns memory)
    const stdout = if (async_spawn.stdout_data) |d| try allocator.dupe(u8, d) else &[_]u8{};
    const stderr = if (async_spawn.stderr_data) |d| try allocator.dupe(u8, d) else &[_]u8{};

    return Value{ .ok_process_output = .{
        .exit_code = async_spawn.exit_code orelse 0,
        .stdout = stdout,
        .stderr = stderr,
    } };
}

/// spawn-free: func(spawn-id: u32) -> result<_, process-error>
fn spawnFreeImpl(args: []const Value) !Value {
    const allocator = process_allocator orelse return error.NotInitialized;
    const spawn_id = try args[0].asU32();

    if (spawn_id >= 64) {
        return Value{ .err = @intFromEnum(ProcessError.invalid_input) };
    }

    if (g_async_spawns[spawn_id]) |async_spawn| {
        async_spawn.deinit();
        allocator.destroy(async_spawn);
        g_async_spawns[spawn_id] = null;
    }

    return Value{ .ok_void = {} };
}

// Tests
test "Process - ProcessError enum values" {
    try std.testing.expectEqual(@as(u32, 0), @intFromEnum(ProcessError.permission_denied));
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(ProcessError.command_not_found));
    try std.testing.expectEqual(@as(u32, 2), @intFromEnum(ProcessError.timeout));
    try std.testing.expectEqual(@as(u32, 3), @intFromEnum(ProcessError.invalid_input));
    try std.testing.expectEqual(@as(u32, 4), @intFromEnum(ProcessError.spawn_failed));
    try std.testing.expectEqual(@as(u32, 5), @intFromEnum(ProcessError.operation_failed));
}

test "Process - ProcessOutput construction" {
    const output = ProcessOutput{
        .exit_code = 0,
        .stdout = "hello",
        .stderr = "",
    };

    try std.testing.expectEqual(@as(i32, 0), output.exit_code);
    try std.testing.expectEqualStrings("hello", output.stdout);
    try std.testing.expectEqualStrings("", output.stderr);
}

test "Process - SpawnOptions construction" {
    const options = SpawnOptions{
        .stdin_data = "input",
        .timeout_seconds = 30,
        .capture_output = true,
    };

    try std.testing.expectEqualStrings("input", options.stdin_data);
    try std.testing.expectEqual(@as(u32, 30), options.timeout_seconds);
}
