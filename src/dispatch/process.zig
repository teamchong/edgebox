//! Process Dispatch - edgebox_process.* host functions
//!
//! Host bindings for process spawning API.
//! These functions are registered with WAMR and called from WASM code
//! to spawn and manage child processes.

const std = @import("std");
const wasm_helpers = @import("../wasm_helpers.zig");
const c = wasm_helpers.c;

// Type alias for native symbols
const NativeSymbol = c.NativeSymbol;

// Use shared WASM memory helpers
const readWasmString = wasm_helpers.readWasmMemory;
const writeWasmBuffer = wasm_helpers.writeWasmBuffer;

// =============================================================================
// Process State
// =============================================================================

const allocator = std.heap.page_allocator;

const ProcessState = struct {
    program: ?[]const u8 = null,
    args: std.ArrayListUnmanaged([]const u8) = .{},
    env_vars: std.ArrayListUnmanaged(EnvVar) = .{},
    stdin_data: ?[]const u8 = null,
    timeout_ms: u32 = 30000,

    // Result state
    exit_code: i32 = 0,
    stdout_buf: ?[]u8 = null,
    stderr_buf: ?[]u8 = null,

    const EnvVar = struct { key: []const u8, value: []const u8 };

    fn reset(self: *ProcessState) void {
        self.program = null;
        self.args.clearRetainingCapacity();
        self.env_vars.clearRetainingCapacity();
        self.stdin_data = null;
        self.timeout_ms = 30000;
        self.exit_code = 0;
        if (self.stdout_buf) |buf| allocator.free(buf);
        if (self.stderr_buf) |buf| allocator.free(buf);
        self.stdout_buf = null;
        self.stderr_buf = null;
    }
};

/// Global process builder state (edgebox_process uses global state)
/// Exported for edgebox_wamr.zig to use during refactoring transition
pub var process_state: ProcessState = .{};

// =============================================================================
// Host Functions
// =============================================================================

/// edgebox_process_set_prog_name(name_ptr, name_len)
fn processSetProgName(exec_env: c.wasm_exec_env_t, name_ptr: u32, name_len: u32) callconv(.c) void {
    const name = readWasmString(exec_env, name_ptr, name_len) orelse return;
    // Duplicate to ensure we own the memory
    process_state.program = allocator.dupe(u8, name) catch return;
}

/// edgebox_process_add_arg(arg_ptr, arg_len)
fn processAddArg(exec_env: c.wasm_exec_env_t, arg_ptr: u32, arg_len: u32) callconv(.c) void {
    const arg = readWasmString(exec_env, arg_ptr, arg_len) orelse return;
    const duped = allocator.dupe(u8, arg) catch return;
    process_state.args.append(allocator, duped) catch return;
}

/// edgebox_process_add_env(key_ptr, key_len, val_ptr, val_len)
fn processAddEnv(exec_env: c.wasm_exec_env_t, key_ptr: u32, key_len: u32, val_ptr: u32, val_len: u32) callconv(.c) void {
    const key = readWasmString(exec_env, key_ptr, key_len) orelse return;
    const val = readWasmString(exec_env, val_ptr, val_len) orelse return;
    const key_duped = allocator.dupe(u8, key) catch return;
    const val_duped = allocator.dupe(u8, val) catch return;
    process_state.env_vars.append(allocator, .{ .key = key_duped, .value = val_duped }) catch return;
}

/// edgebox_process_add_stdin(buf_ptr, buf_len)
fn processAddStdin(exec_env: c.wasm_exec_env_t, buf_ptr: u32, buf_len: u32) callconv(.c) void {
    const data = readWasmString(exec_env, buf_ptr, buf_len) orelse return;
    process_state.stdin_data = allocator.dupe(u8, data) catch return;
}

/// edgebox_process_set_timeout(time_ms)
fn processSetTimeout(_: c.wasm_exec_env_t, time_ms: u32) callconv(.c) void {
    process_state.timeout_ms = time_ms;
}

/// edgebox_process_run() -> i32 (0 = success, -1 = error)
fn processRun(_: c.wasm_exec_env_t) callconv(.c) i32 {
    const program = process_state.program orelse return -1;

    // Check if OS-level sandbox is enabled
    const use_sandbox = std.posix.getenv("__EDGEBOX_DIRS") != null;

    // Build argv
    var argv = std.ArrayListUnmanaged([]const u8){};
    defer argv.deinit(allocator);

    if (use_sandbox) {
        // Wrap with edgebox-sandbox
        argv.append(allocator, "edgebox-sandbox") catch return -1;
    }

    // Add program
    argv.append(allocator, program) catch return -1;

    // Add args
    for (process_state.args.items) |arg| {
        argv.append(allocator, arg) catch return -1;
    }

    // Spawn child process using std.process.Child
    var child = std.process.Child.init(argv.items, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    // Apply explicit environment variables from process_state
    if (process_state.env_vars.items.len > 0) {
        var env_map = std.process.getEnvMap(allocator) catch null;
        if (env_map) |*em| {
            for (process_state.env_vars.items) |env| {
                em.put(env.key, env.value) catch {};
            }
            child.env_map = em;
        }
    }

    child.spawn() catch |err| {
        std.debug.print("Process spawn failed: {}\n", .{err});
        return -1;
    };

    // Read stdout/stderr before wait (pipes must be drained first)
    var stdout_data: ?[]u8 = null;
    var stderr_data: ?[]u8 = null;
    var read_buf: [4096]u8 = undefined;

    if (child.stdout) |*stdout_file| {
        var stdout_list = std.ArrayListUnmanaged(u8){};
        while (true) {
            const n = stdout_file.read(&read_buf) catch break;
            if (n == 0) break;
            stdout_list.appendSlice(allocator, read_buf[0..n]) catch break;
        }
        if (stdout_list.items.len > 0) {
            // Return empty slice on allocation failure instead of null to prevent use-after-free
            stdout_data = stdout_list.toOwnedSlice(allocator) catch &.{};
        }
    }
    if (child.stderr) |*stderr_file| {
        var stderr_list = std.ArrayListUnmanaged(u8){};
        while (true) {
            const n = stderr_file.read(&read_buf) catch break;
            if (n == 0) break;
            stderr_list.appendSlice(allocator, read_buf[0..n]) catch break;
        }
        if (stderr_list.items.len > 0) {
            // Return empty slice on allocation failure instead of null to prevent use-after-free
            stderr_data = stderr_list.toOwnedSlice(allocator) catch &.{};
        }
    }

    // Wait for process to terminate
    const term = child.wait() catch |err| {
        std.debug.print("Process wait failed: {}\n", .{err});
        return -1;
    };

    // Store results
    process_state.exit_code = switch (term) {
        .Exited => |code| @intCast(code),
        .Signal => -1,
        .Stopped => -1,
        .Unknown => -1,
    };
    process_state.stdout_buf = stdout_data;
    process_state.stderr_buf = stderr_data;

    return 0;
}

/// edgebox_process_get_exit_code() -> i32
fn processGetExitCode(_: c.wasm_exec_env_t) callconv(.c) i32 {
    return process_state.exit_code;
}

/// edgebox_process_get_stdout_len() -> u32
fn processGetStdoutLen(_: c.wasm_exec_env_t) callconv(.c) u32 {
    return if (process_state.stdout_buf) |buf| @intCast(buf.len) else 0;
}

/// edgebox_process_get_stdout(buf_ptr)
fn processGetStdout(exec_env: c.wasm_exec_env_t, buf_ptr: u32) callconv(.c) void {
    if (process_state.stdout_buf) |buf| {
        writeWasmBuffer(exec_env, buf_ptr, buf);
    }
}

/// edgebox_process_get_stderr_len() -> u32
fn processGetStderrLen(_: c.wasm_exec_env_t) callconv(.c) u32 {
    return if (process_state.stderr_buf) |buf| @intCast(buf.len) else 0;
}

/// edgebox_process_get_stderr(buf_ptr)
fn processGetStderr(exec_env: c.wasm_exec_env_t, buf_ptr: u32) callconv(.c) void {
    if (process_state.stderr_buf) |buf| {
        writeWasmBuffer(exec_env, buf_ptr, buf);
    }
}

// =============================================================================
// Symbol Table
// =============================================================================

// IMPORTANT: These must be global/static because WAMR retains references to them
var g_symbols = [_]NativeSymbol{
    .{ .symbol = "edgebox_process_set_prog_name", .func_ptr = @ptrCast(@constCast(&processSetProgName)), .signature = "(ii)", .attachment = null },
    .{ .symbol = "edgebox_process_add_arg", .func_ptr = @ptrCast(@constCast(&processAddArg)), .signature = "(ii)", .attachment = null },
    .{ .symbol = "edgebox_process_add_env", .func_ptr = @ptrCast(@constCast(&processAddEnv)), .signature = "(iiii)", .attachment = null },
    .{ .symbol = "edgebox_process_add_stdin", .func_ptr = @ptrCast(@constCast(&processAddStdin)), .signature = "(ii)", .attachment = null },
    .{ .symbol = "edgebox_process_set_timeout", .func_ptr = @ptrCast(@constCast(&processSetTimeout)), .signature = "(i)", .attachment = null },
    .{ .symbol = "edgebox_process_run", .func_ptr = @ptrCast(@constCast(&processRun)), .signature = "()i", .attachment = null },
    .{ .symbol = "edgebox_process_get_exit_code", .func_ptr = @ptrCast(@constCast(&processGetExitCode)), .signature = "()i", .attachment = null },
    .{ .symbol = "edgebox_process_get_stdout_len", .func_ptr = @ptrCast(@constCast(&processGetStdoutLen)), .signature = "()i", .attachment = null },
    .{ .symbol = "edgebox_process_get_stdout", .func_ptr = @ptrCast(@constCast(&processGetStdout)), .signature = "(i)", .attachment = null },
    .{ .symbol = "edgebox_process_get_stderr_len", .func_ptr = @ptrCast(@constCast(&processGetStderrLen)), .signature = "()i", .attachment = null },
    .{ .symbol = "edgebox_process_get_stderr", .func_ptr = @ptrCast(@constCast(&processGetStderr)), .signature = "(i)", .attachment = null },
};

// =============================================================================
// Public API
// =============================================================================

/// Get the symbol table for WAMR registration
pub fn getSymbols() []NativeSymbol {
    return &g_symbols;
}

/// Register symbols with WAMR
pub fn registerAll() void {
    _ = c.wasm_runtime_register_natives("edgebox_process", &g_symbols, g_symbols.len);
}

/// Reset process state between runs
pub fn reset() void {
    process_state.reset();
}
