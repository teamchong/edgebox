/// Process Implementation for Component Model
/// Wraps process execution operations as Component Model functions

const std = @import("std");
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

/// External process functions from wasi_process.zig
extern "edgebox_process" fn edgebox_process_set_prog_name(name: [*]const u8, len: u32) void;
extern "edgebox_process" fn edgebox_process_add_arg(arg: [*]const u8, len: u32) void;
extern "edgebox_process" fn edgebox_process_add_env(env: [*]const u8, env_len: u32, val: [*]const u8, val_len: u32) void;
extern "edgebox_process" fn edgebox_process_add_stdin(buf: [*]const u8, len: u32) void;
extern "edgebox_process" fn edgebox_process_set_timeout(time_ms: u32) void;
extern "edgebox_process" fn edgebox_process_run() i32;
extern "edgebox_process" fn edgebox_process_get_exit_code() i32;
extern "edgebox_process" fn edgebox_process_get_stdout_len() u32;
extern "edgebox_process" fn edgebox_process_get_stdout(buf: [*]u8) void;
extern "edgebox_process" fn edgebox_process_get_stderr_len() u32;
extern "edgebox_process" fn edgebox_process_get_stderr(buf: [*]u8) void;

/// External spawn dispatcher for async operations
extern "edgebox" fn __edgebox_spawn_dispatch(opcode: i32, a1: i32, a2: i32, a3: i32, a4: i32) i32;

/// Spawn opcodes
const SPAWN_OP_START: i32 = 0;
const SPAWN_OP_POLL: i32 = 1;
const SPAWN_OP_OUTPUT_LEN: i32 = 2;
const SPAWN_OP_OUTPUT: i32 = 3;
const SPAWN_OP_FREE: i32 = 4;

/// Global allocator for process operations
var process_allocator: ?std.mem.Allocator = null;

/// Initialize process implementation
pub fn init(allocator: std.mem.Allocator) void {
    process_allocator = allocator;
}

/// Clean up process implementation
pub fn deinit() void {
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

/// Map process error codes to ProcessError enum
fn mapProcessError(code: i32) u32 {
    return switch (code) {
        -10, -11, -12 => @intFromEnum(ProcessError.permission_denied),
        -2 => @intFromEnum(ProcessError.command_not_found),
        -3 => @intFromEnum(ProcessError.timeout),
        -4 => @intFromEnum(ProcessError.spawn_failed),
        else => @intFromEnum(ProcessError.operation_failed),
    };
}

/// Unescape JSON string (handle \n, \r, \t, \", \\, etc.)
fn unescapeJSON(allocator: std.mem.Allocator, escaped: []const u8) ![]const u8 {
    var result = std.ArrayList(u8).init(allocator);
    defer result.deinit();

    var i: usize = 0;
    while (i < escaped.len) : (i += 1) {
        if (escaped[i] == '\\' and i + 1 < escaped.len) {
            switch (escaped[i + 1]) {
                'n' => {
                    try result.append('\n');
                    i += 1;
                },
                'r' => {
                    try result.append('\r');
                    i += 1;
                },
                't' => {
                    try result.append('\t');
                    i += 1;
                },
                '"' => {
                    try result.append('"');
                    i += 1;
                },
                '\\' => {
                    try result.append('\\');
                    i += 1;
                },
                else => try result.append(escaped[i]),
            }
        } else {
            try result.append(escaped[i]);
        }
    }

    return result.toOwnedSlice();
}

/// Parse JSON output to ProcessOutput struct
/// JSON format: {"exitCode":N,"stdout":"...","stderr":"..."}
fn parseJSONOutput(allocator: std.mem.Allocator, json: []const u8) !ProcessOutput {
    // Simple JSON parsing for fixed structure
    var exit_code: i32 = 0;
    var stdout: []const u8 = "";
    var stderr: []const u8 = "";

    // Find exitCode value
    if (std.mem.indexOf(u8, json, "\"exitCode\":")) |idx| {
        const value_start = idx + 11; // Skip "exitCode":
        if (std.mem.indexOfScalarPos(u8, json, value_start, ',')) |comma| {
            const exit_str = std.mem.trim(u8, json[value_start..comma], " \t\r\n");
            exit_code = std.fmt.parseInt(i32, exit_str, 10) catch 0;
        }
    }

    // Find stdout value
    if (std.mem.indexOf(u8, json, "\"stdout\":\"")) |idx| {
        const value_start = idx + 10; // Skip "stdout":"
        // Find closing quote (handling escaped quotes)
        var end_idx = value_start;
        while (end_idx < json.len) : (end_idx += 1) {
            if (json[end_idx] == '"' and (end_idx == value_start or json[end_idx - 1] != '\\')) {
                break;
            }
        }
        if (end_idx < json.len) {
            stdout = try unescapeJSON(allocator, json[value_start..end_idx]);
        }
    }

    // Find stderr value
    if (std.mem.indexOf(u8, json, "\"stderr\":\"")) |idx| {
        const value_start = idx + 10; // Skip "stderr":"
        // Find closing quote (handling escaped quotes)
        var end_idx = value_start;
        while (end_idx < json.len) : (end_idx += 1) {
            if (json[end_idx] == '"' and (end_idx == value_start or json[end_idx - 1] != '\\')) {
                break;
            }
        }
        if (end_idx < json.len) {
            stderr = try unescapeJSON(allocator, json[value_start..end_idx]);
        }
    }

    return ProcessOutput{
        .exit_code = exit_code,
        .stdout = stdout,
        .stderr = stderr,
    };
}

/// spawn-sync: func(command: string, args: list<string>, options: spawn-options)
///   -> result<process-output, process-error>
fn spawnSyncImpl(args: []const Value) !Value {
    const allocator = process_allocator orelse return error.NotInitialized;
    const command = try args[0].asString();
    const arg_list = try args[1].asListString();
    const options = try args[2].asSpawnOptions();

    // Build process using wasi_process API
    edgebox_process_set_prog_name(command.ptr, @intCast(command.len));

    for (arg_list) |arg| {
        edgebox_process_add_arg(arg.ptr, @intCast(arg.len));
    }

    if (options.stdin_data.len > 0) {
        edgebox_process_add_stdin(options.stdin_data.ptr, @intCast(options.stdin_data.len));
    }

    if (options.timeout_seconds > 0) {
        // Convert seconds to milliseconds
        edgebox_process_set_timeout(options.timeout_seconds * 1000);
    }

    // Run process
    const run_result = edgebox_process_run();
    if (run_result != 0) {
        return Value{ .err = mapProcessError(run_result) };
    }

    // Capture output
    const exit_code = edgebox_process_get_exit_code();

    const stdout_len = edgebox_process_get_stdout_len();
    const stdout = if (stdout_len > 0) blk: {
        const buf = try allocator.alloc(u8, stdout_len);
        edgebox_process_get_stdout(buf.ptr);
        break :blk buf;
    } else try allocator.dupe(u8, "");

    const stderr_len = edgebox_process_get_stderr_len();
    const stderr = if (stderr_len > 0) blk: {
        const buf = try allocator.alloc(u8, stderr_len);
        edgebox_process_get_stderr(buf.ptr);
        break :blk buf;
    } else try allocator.dupe(u8, "");

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

    // Parse command into program and args (simple whitespace split)
    var tokens = std.mem.tokenizeScalar(u8, command, ' ');
    const program = tokens.next() orelse return Value{ .err = @intFromEnum(ProcessError.invalid_input) };

    // Collect remaining args
    var arg_list = std.ArrayList([]const u8).init(allocator);
    defer arg_list.deinit();

    while (tokens.next()) |token| {
        try arg_list.append(token);
    }

    // Call spawn-sync internally
    return try spawnSyncImpl(&[_]Value{
        Value{ .string = program },
        Value{ .list_string = arg_list.items },
        Value{ .spawn_options = options },
    });
}

/// spawn-start: func(command: string, args: list<string>, options: spawn-options)
///   -> result<u32, process-error>
fn spawnStartImpl(args: []const Value) !Value {
    const allocator = process_allocator orelse return error.NotInitialized;
    const command = try args[0].asString();
    const arg_list = try args[1].asListString();
    const options = try args[2].asSpawnOptions();

    // Build full command string with args
    var cmd_buf = std.ArrayList(u8).init(allocator);
    defer cmd_buf.deinit();

    try cmd_buf.appendSlice(command);
    for (arg_list) |arg| {
        try cmd_buf.append(' ');
        try cmd_buf.appendSlice(arg);
    }

    const full_cmd = cmd_buf.items;

    // Call spawn dispatcher with SPAWN_OP_START
    // For now, we pass empty args_json (args are already in command string)
    const spawn_id = __edgebox_spawn_dispatch(
        SPAWN_OP_START,
        @intCast(@intFromPtr(full_cmd.ptr)),
        @intCast(full_cmd.len),
        0, // args_ptr (unused)
        0, // args_len (unused)
    );

    if (spawn_id < 0) {
        return Value{ .err = mapProcessError(spawn_id) };
    }

    return Value{ .ok_spawn_id = @intCast(spawn_id) };
}

/// spawn-poll: func(spawn-id: u32) -> result<u32, process-error>
fn spawnPollImpl(args: []const Value) !Value {
    const spawn_id = try args[0].asU32();

    const status = __edgebox_spawn_dispatch(
        SPAWN_OP_POLL,
        @intCast(spawn_id),
        0,
        0,
        0,
    );

    if (status < 0) {
        return Value{ .err = mapProcessError(status) };
    }

    return Value{ .ok_spawn_id = @intCast(status) };
}

/// spawn-output: func(spawn-id: u32) -> result<process-output, process-error>
fn spawnOutputImpl(args: []const Value) !Value {
    const allocator = process_allocator orelse return error.NotInitialized;
    const spawn_id = try args[0].asU32();

    // Get JSON output length
    const output_len = __edgebox_spawn_dispatch(
        SPAWN_OP_OUTPUT_LEN,
        @intCast(spawn_id),
        0,
        0,
        0,
    );

    if (output_len < 0) {
        return Value{ .err = mapProcessError(output_len) };
    }

    // Allocate buffer and get output
    const json_buf = try allocator.alloc(u8, @intCast(output_len));
    defer allocator.free(json_buf);

    const result = __edgebox_spawn_dispatch(
        SPAWN_OP_OUTPUT,
        @intCast(spawn_id),
        @intCast(@intFromPtr(json_buf.ptr)),
        0,
        0,
    );

    if (result < 0) {
        return Value{ .err = mapProcessError(result) };
    }

    // Parse JSON to ProcessOutput
    const output = try parseJSONOutput(allocator, json_buf);

    return Value{ .ok_process_output = output };
}

/// spawn-free: func(spawn-id: u32) -> result<_, process-error>
fn spawnFreeImpl(args: []const Value) !Value {
    const spawn_id = try args[0].asU32();

    const result = __edgebox_spawn_dispatch(
        SPAWN_OP_FREE,
        @intCast(spawn_id),
        0,
        0,
        0,
    );

    if (result < 0) {
        return Value{ .err = mapProcessError(result) };
    }

    return Value{ .ok_void = {} };
}
