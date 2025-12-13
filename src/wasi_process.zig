/// EdgeBox Process Interface
/// Allows spawning system commands from WASM via EdgeBox's process API
///
/// These are EdgeBox-specific extensions (not standard WASI)
/// The API design is inspired by WasmEdge's process interface
///
/// Security: Commands are validated against __EDGEBOX_COMMANDS env var
/// Format: {"git":["clone","status"],"node":true}
///   - Array = allowed subcommands (first arg must match)
///   - true = all arguments allowed
///   - Missing binary = denied
const std = @import("std");

// EdgeBox process host functions
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

pub const ProcessError = error{
    CommandFailed,
    TimedOut,
    OutOfMemory,
    InvalidCommand,
    PermissionDenied,
};

// ============================================================================
// Command Permission Validation
// ============================================================================

/// Cached command permissions parsed from __EDGEBOX_COMMANDS
var permissions_cache: ?CommandPermissions = null;
var permissions_initialized: bool = false;

const CommandPermissions = struct {
    /// Map of binary name -> allowed subcommands (null = all allowed)
    binaries: std.StringHashMap(?[]const []const u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) CommandPermissions {
        return .{
            .binaries = std.StringHashMap(?[]const []const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *CommandPermissions) void {
        var iter = self.binaries.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            if (entry.value_ptr.*) |subs| {
                for (subs) |sub| {
                    self.allocator.free(sub);
                }
                self.allocator.free(subs);
            }
        }
        self.binaries.deinit();
    }

    /// Check if command is allowed
    pub fn isAllowed(self: *const CommandPermissions, binary: []const u8, args: []const []const u8) bool {
        const entry = self.binaries.get(binary) orelse return false;

        // null = all args allowed
        if (entry == null) return true;

        // Empty args = binary-only call, allowed if binary is in list
        if (args.len == 0) return true;

        // Check if first arg is in allowed list
        for (entry.?) |allowed| {
            if (std.mem.eql(u8, args[0], allowed)) {
                return true;
            }
        }

        return false;
    }
};

/// Parse __EDGEBOX_COMMANDS env var and cache permissions
fn getPermissions(allocator: std.mem.Allocator) ?*const CommandPermissions {
    if (permissions_initialized) {
        return if (permissions_cache != null) &permissions_cache.? else null;
    }
    permissions_initialized = true;

    // Read __EDGEBOX_COMMANDS from environment
    const env_value = std.posix.getenv("__EDGEBOX_COMMANDS") orelse {
        // No permissions configured = deny all
        return null;
    };

    // Parse JSON: {"git":["clone","status"],"node":true}
    var perms = CommandPermissions.init(allocator);

    const parsed = std.json.parseFromSlice(std.json.Value, allocator, env_value, .{}) catch {
        return null;
    };
    defer parsed.deinit();

    if (parsed.value != .object) return null;

    var iter = parsed.value.object.iterator();
    while (iter.next()) |entry| {
        const binary = allocator.dupe(u8, entry.key_ptr.*) catch continue;

        if (entry.value_ptr.* == .bool and entry.value_ptr.bool) {
            // true = allow all
            perms.binaries.put(binary, null) catch {
                allocator.free(binary);
                continue;
            };
        } else if (entry.value_ptr.* == .array) {
            var subs: std.ArrayListUnmanaged([]const u8) = .{};
            for (entry.value_ptr.array.items) |item| {
                if (item == .string) {
                    subs.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
                }
            }
            perms.binaries.put(binary, subs.toOwnedSlice(allocator) catch null) catch {
                allocator.free(binary);
                continue;
            };
        }
    }

    permissions_cache = perms;
    return &permissions_cache.?;
}

/// Check if a command is allowed by permissions
pub fn checkPermission(allocator: std.mem.Allocator, binary: []const u8, args: []const []const u8) bool {
    // If no __EDGEBOX_COMMANDS is set, allow all (backwards compatible)
    if (std.posix.getenv("__EDGEBOX_COMMANDS") == null) {
        return true;
    }

    const perms = getPermissions(allocator) orelse return false;
    return perms.isAllowed(binary, args);
}

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
        // Check command permissions
        if (!checkPermission(self.allocator, self.program, self.cmd_args.items)) {
            return ProcessError.PermissionDenied;
        }

        // Check if OS-level sandbox is enabled via __EDGEBOX_DIRS
        const use_sandbox = std.posix.getenv("__EDGEBOX_DIRS") != null;

        if (use_sandbox) {
            // Wrap command with edgebox-sandbox for OS-level filesystem isolation
            // edgebox-sandbox reads __EDGEBOX_DIRS from env to set allowed directories
            const sandbox_cmd = "edgebox-sandbox";
            edgebox_process_set_prog_name(sandbox_cmd.ptr, @intCast(sandbox_cmd.len));

            // Original program becomes first argument to sandbox
            edgebox_process_add_arg(self.program.ptr, @intCast(self.program.len));

            // Original arguments follow
            for (self.cmd_args.items) |a| {
                edgebox_process_add_arg(a.ptr, @intCast(a.len));
            }
        } else {
            // No sandbox - run command directly
            edgebox_process_set_prog_name(self.program.ptr, @intCast(self.program.len));

            // Add arguments
            for (self.cmd_args.items) |a| {
                edgebox_process_add_arg(a.ptr, @intCast(a.len));
            }
        }

        // Add environment variables
        for (self.env_vars.items) |e| {
            edgebox_process_add_env(e.key.ptr, @intCast(e.key.len), e.value.ptr, @intCast(e.value.len));
        }

        // Set stdin
        if (self.stdin_data) |data| {
            edgebox_process_add_stdin(data.ptr, @intCast(data.len));
        }

        // Set timeout
        edgebox_process_set_timeout(self.timeout_ms);

        // Run the process
        const run_result = edgebox_process_run();
        if (run_result != 0) {
            return ProcessError.CommandFailed;
        }

        // Get exit code
        const exit_code = edgebox_process_get_exit_code();

        // Get stdout
        const stdout_len = edgebox_process_get_stdout_len();
        var stdout: []u8 = &[_]u8{};
        if (stdout_len > 0) {
            stdout = self.allocator.alloc(u8, stdout_len) catch return ProcessError.OutOfMemory;
            edgebox_process_get_stdout(stdout.ptr);
        }

        // Get stderr
        const stderr_len = edgebox_process_get_stderr_len();
        var stderr: []u8 = &[_]u8{};
        if (stderr_len > 0) {
            stderr = self.allocator.alloc(u8, stderr_len) catch {
                if (stdout.len > 0) self.allocator.free(stdout);
                return ProcessError.OutOfMemory;
            };
            edgebox_process_get_stderr(stderr.ptr);
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

/// Check if process spawning is available (EdgeBox process API loaded)
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
