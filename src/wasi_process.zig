/// WasmEdge Process Interface
/// Allows spawning system commands from WASM via WasmEdge's process plugin
///
/// These are WasmEdge-specific extensions (not standard WASI)
/// Requires: wasmedge --enable-all or wasmedge --enable-process
/// Based on: https://github.com/second-state/wasmedge_process_interface
const std = @import("std");

// WasmEdge process host functions
extern "wasmedge_process" fn wasmedge_process_set_prog_name(name: [*]const u8, len: u32) void;
extern "wasmedge_process" fn wasmedge_process_add_arg(arg: [*]const u8, len: u32) void;
extern "wasmedge_process" fn wasmedge_process_add_env(env: [*]const u8, env_len: u32, val: [*]const u8, val_len: u32) void;
extern "wasmedge_process" fn wasmedge_process_add_stdin(buf: [*]const u8, len: u32) void;
extern "wasmedge_process" fn wasmedge_process_set_timeout(time_ms: u32) void;
extern "wasmedge_process" fn wasmedge_process_run() i32;
extern "wasmedge_process" fn wasmedge_process_get_exit_code() i32;
extern "wasmedge_process" fn wasmedge_process_get_stdout_len() u32;
extern "wasmedge_process" fn wasmedge_process_get_stdout(buf: [*]u8) void;
extern "wasmedge_process" fn wasmedge_process_get_stderr_len() u32;
extern "wasmedge_process" fn wasmedge_process_get_stderr(buf: [*]u8) void;

pub const ProcessError = error{
    CommandFailed,
    TimedOut,
    OutOfMemory,
    InvalidCommand,
};

/// Result of a spawned process
pub const ProcessResult = struct {
    exit_code: i32,
    stdout: []u8,
    stderr: []u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *ProcessResult) void {
        if (self.stdout.len > 0) self.allocator.free(self.stdout);
        if (self.stderr.len > 0) self.allocator.free(self.stderr);
    }
};

/// Environment variable key-value pair
pub const EnvVar = struct {
    key: []const u8,
    value: []const u8,
};

/// Command builder for spawning processes
pub const Command = struct {
    program: []const u8,
    cmd_args: std.ArrayList([]const u8),
    env_vars: std.ArrayList(EnvVar),
    stdin_data: ?[]const u8,
    timeout_ms: u32,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, program: []const u8) Self {
        return .{
            .program = program,
            .cmd_args = std.ArrayList([]const u8){},
            .env_vars = std.ArrayList(EnvVar){},
            .stdin_data = null,
            .timeout_ms = 30000, // 30 second default timeout
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.cmd_args.deinit(self.allocator);
        self.env_vars.deinit(self.allocator);
    }

    /// Add a command line argument
    pub fn arg(self: *Self, value: []const u8) !*Self {
        try self.cmd_args.append(self.allocator, value);
        return self;
    }

    /// Add multiple arguments
    pub fn addArgs(self: *Self, values: []const []const u8) !*Self {
        for (values) |v| {
            try self.cmd_args.append(self.allocator, v);
        }
        return self;
    }

    /// Set an environment variable
    pub fn setEnv(self: *Self, key: []const u8, value: []const u8) !*Self {
        try self.env_vars.append(self.allocator, .{ .key = key, .value = value });
        return self;
    }

    /// Set stdin data
    pub fn setStdin(self: *Self, data: []const u8) *Self {
        self.stdin_data = data;
        return self;
    }

    /// Set timeout in milliseconds (0 = no timeout)
    pub fn setTimeout(self: *Self, ms: u32) *Self {
        self.timeout_ms = ms;
        return self;
    }

    /// Execute the command and capture output
    pub fn output(self: *Self) ProcessError!ProcessResult {
        // Set program name
        wasmedge_process_set_prog_name(self.program.ptr, @intCast(self.program.len));

        // Add arguments
        for (self.cmd_args.items) |a| {
            wasmedge_process_add_arg(a.ptr, @intCast(a.len));
        }

        // Add environment variables
        for (self.env_vars.items) |e| {
            wasmedge_process_add_env(e.key.ptr, @intCast(e.key.len), e.value.ptr, @intCast(e.value.len));
        }

        // Set stdin
        if (self.stdin_data) |data| {
            wasmedge_process_add_stdin(data.ptr, @intCast(data.len));
        }

        // Set timeout
        wasmedge_process_set_timeout(self.timeout_ms);

        // Run the process
        const run_result = wasmedge_process_run();
        if (run_result != 0) {
            return ProcessError.CommandFailed;
        }

        // Get exit code
        const exit_code = wasmedge_process_get_exit_code();

        // Get stdout
        const stdout_len = wasmedge_process_get_stdout_len();
        var stdout: []u8 = &[_]u8{};
        if (stdout_len > 0) {
            stdout = self.allocator.alloc(u8, stdout_len) catch return ProcessError.OutOfMemory;
            wasmedge_process_get_stdout(stdout.ptr);
        }

        // Get stderr
        const stderr_len = wasmedge_process_get_stderr_len();
        var stderr: []u8 = &[_]u8{};
        if (stderr_len > 0) {
            stderr = self.allocator.alloc(u8, stderr_len) catch {
                if (stdout.len > 0) self.allocator.free(stdout);
                return ProcessError.OutOfMemory;
            };
            wasmedge_process_get_stderr(stderr.ptr);
        }

        return ProcessResult{
            .exit_code = exit_code,
            .stdout = stdout,
            .stderr = stderr,
            .allocator = self.allocator,
        };
    }

    /// Execute and return just the exit code (discards output)
    pub fn status(self: *Self) ProcessError!i32 {
        var result = try self.output();
        defer result.deinit();
        return result.exit_code;
    }
};

/// Convenience function to spawn a simple command
pub fn spawn(allocator: std.mem.Allocator, program: []const u8, cmd_args: []const []const u8) ProcessError!ProcessResult {
    var cmd = Command.init(allocator, program);
    defer cmd.deinit();

    for (cmd_args) |a| {
        _ = cmd.arg(a) catch return ProcessError.OutOfMemory;
    }

    return cmd.output();
}

/// Check if process spawning is available (WasmEdge process plugin loaded)
pub fn isAvailable() bool {
    // Try to run a simple "true" command
    // If the process plugin isn't loaded, this will fail
    var cmd = Command.init(std.heap.page_allocator, "true");
    defer cmd.deinit();
    _ = cmd.setTimeout(1000);

    const result = cmd.output() catch return false;
    defer @constCast(&result).deinit();
    return true;
}
