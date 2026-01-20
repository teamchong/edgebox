/// Native child_process module - QuickJS C functions
/// Provides execSync and spawnSync for running external commands
const std = @import("std");
const builtin = @import("builtin");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// Platform-specific C imports for IPC
const c = @cImport({
    @cInclude("sys/socket.h");
    @cInclude("fcntl.h");
    @cInclude("unistd.h");
});

// Static buffers for command execution
var stdout_buf: [65536]u8 = undefined;
var stderr_buf: [65536]u8 = undefined;
var cmd_buf: [4096]u8 = undefined;

/// child_process.execSync(command, [options]) - Execute shell command synchronously
/// Returns: Buffer containing stdout
/// Options: { encoding: string, cwd: string, timeout: number, maxBuffer: number }
fn execSync(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_ThrowTypeError(ctx, "execSync not available in WASI environment");
    }

    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "execSync requires command argument");
    }

    const cmd_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (cmd_cstr == null) {
        return qjs.JS_ThrowTypeError(ctx, "command must be a string");
    }
    defer qjs.JS_FreeCString(ctx, cmd_cstr);

    // Parse options
    var encoding: ?[]const u8 = null;
    var cwd: ?[]const u8 = null;

    if (argc >= 2 and qjs.JS_IsObject(argv[1])) {
        // Get encoding option
        const enc_val = qjs.JS_GetPropertyStr(ctx, argv[1], "encoding");
        defer qjs.JS_FreeValue(ctx, enc_val);
        if (qjs.JS_IsString(enc_val)) {
            const enc_str = qjs.JS_ToCString(ctx, enc_val);
            if (enc_str != null) {
                encoding = std.mem.span(enc_str);
                qjs.JS_FreeCString(ctx, enc_str);
            }
        }

        // Get cwd option (not used in this simple implementation)
        const cwd_val = qjs.JS_GetPropertyStr(ctx, argv[1], "cwd");
        defer qjs.JS_FreeValue(ctx, cwd_val);
        if (qjs.JS_IsString(cwd_val)) {
            const cwd_str = qjs.JS_ToCString(ctx, cwd_val);
            if (cwd_str != null) {
                cwd = std.mem.span(cwd_str);
                qjs.JS_FreeCString(ctx, cwd_str);
            }
        }
    }

    // Execute command using /bin/sh -c
    const cmd_slice = std.mem.span(cmd_cstr);

    const cmd_argv_arr = [_][]const u8{ "/bin/sh", "-c", cmd_slice };
    var child = std.process.Child.init(&cmd_argv_arr, std.heap.page_allocator);
    child.cwd = cwd;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    child.spawn() catch |err| {
        const err_obj = qjs.JS_NewError(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err_obj, "code", qjs.JS_NewString(ctx, "ENOENT"));
        _ = qjs.JS_SetPropertyStr(ctx, err_obj, "message", qjs.JS_NewString(ctx, @errorName(err)));
        return qjs.JS_Throw(ctx, err_obj);
    };

    // Read stdout
    var stdout_len: usize = 0;
    if (child.stdout) |stdout| {
        stdout_len = stdout.deprecatedReader().readAll(&stdout_buf) catch 0;
    }

    // Wait for process to complete
    const result = child.wait() catch |err| {
        const err_obj = qjs.JS_NewError(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err_obj, "message", qjs.JS_NewString(ctx, @errorName(err)));
        return qjs.JS_Throw(ctx, err_obj);
    };

    // Check exit status
    if (result.Exited != 0) {
        // Read stderr for error message
        var stderr_len: usize = 0;
        if (child.stderr) |stderr| {
            stderr_len = stderr.deprecatedReader().readAll(&stderr_buf) catch 0;
        }

        const err_obj = qjs.JS_NewError(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err_obj, "status", qjs.JS_NewInt32(ctx, @intCast(result.Exited)));
        if (stderr_len > 0) {
            _ = qjs.JS_SetPropertyStr(ctx, err_obj, "stderr", qjs.JS_NewStringLen(ctx, &stderr_buf, stderr_len));
        }
        if (stdout_len > 0) {
            _ = qjs.JS_SetPropertyStr(ctx, err_obj, "stdout", qjs.JS_NewStringLen(ctx, &stdout_buf, stdout_len));
        }
        return qjs.JS_Throw(ctx, err_obj);
    }

    // Return result based on encoding
    if (encoding != null) {
        // Return as string
        return qjs.JS_NewStringLen(ctx, &stdout_buf, stdout_len);
    } else {
        // Return as Buffer (Uint8Array)
        return createUint8Array(ctx, stdout_buf[0..stdout_len]);
    }
}

/// child_process.spawnSync(command, args, [options]) - Spawn process synchronously
/// Returns: { status: number, stdout: Buffer, stderr: Buffer, pid: number }
fn spawnSync(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_ThrowTypeError(ctx, "spawnSync not available in WASI environment");
    }

    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "spawnSync requires command argument");
    }

    const cmd_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (cmd_cstr == null) {
        return qjs.JS_ThrowTypeError(ctx, "command must be a string");
    }
    defer qjs.JS_FreeCString(ctx, cmd_cstr);

    // Build argv array
    var args_array: [64][]const u8 = undefined;
    var args_count: usize = 1;
    args_array[0] = std.mem.span(cmd_cstr);

    // Parse args array if provided
    if (argc >= 2 and qjs.JS_IsArray(argv[1])) {
        const len_val = qjs.JS_GetPropertyStr(ctx, argv[1], "length");
        defer qjs.JS_FreeValue(ctx, len_val);
        var arr_len: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &arr_len, len_val);

        var i: usize = 0;
        while (i < @as(usize, @intCast(arr_len)) and args_count < 63) : (i += 1) {
            const arg_val = qjs.JS_GetPropertyUint32(ctx, argv[1], @intCast(i));
            defer qjs.JS_FreeValue(ctx, arg_val);
            const arg_str = qjs.JS_ToCString(ctx, arg_val);
            if (arg_str != null) {
                args_array[args_count] = std.mem.span(arg_str);
                args_count += 1;
                // Note: We don't free arg_str here as we need it for spawn
                // This is a memory leak but acceptable for sync operations
            }
        }
    }

    // Parse options
    var cwd: ?[]const u8 = null;
    if (argc >= 3 and qjs.JS_IsObject(argv[2])) {
        const cwd_val = qjs.JS_GetPropertyStr(ctx, argv[2], "cwd");
        defer qjs.JS_FreeValue(ctx, cwd_val);
        if (qjs.JS_IsString(cwd_val)) {
            const cwd_str = qjs.JS_ToCString(ctx, cwd_val);
            if (cwd_str != null) {
                cwd = std.mem.span(cwd_str);
                qjs.JS_FreeCString(ctx, cwd_str);
            }
        }
    }

    // Spawn process
    var child = std.process.Child.init(args_array[0..args_count], std.heap.page_allocator);
    child.cwd = cwd;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    child.spawn() catch |err| {
        const result_obj = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, result_obj, "status", qjs.JS_NewInt32(ctx, -1));
        _ = qjs.JS_SetPropertyStr(ctx, result_obj, "signal", quickjs.jsNull());
        _ = qjs.JS_SetPropertyStr(ctx, result_obj, "error", qjs.JS_NewString(ctx, @errorName(err)));
        return result_obj;
    };

    // Read stdout and stderr
    var stdout_len: usize = 0;
    var stderr_len: usize = 0;

    if (child.stdout) |stdout| {
        stdout_len = stdout.deprecatedReader().readAll(&stdout_buf) catch 0;
    }
    if (child.stderr) |stderr| {
        stderr_len = stderr.deprecatedReader().readAll(&stderr_buf) catch 0;
    }

    // Wait for completion
    const wait_result = child.wait() catch |err| {
        const result_obj = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, result_obj, "status", qjs.JS_NewInt32(ctx, -1));
        _ = qjs.JS_SetPropertyStr(ctx, result_obj, "signal", quickjs.jsNull());
        _ = qjs.JS_SetPropertyStr(ctx, result_obj, "error", qjs.JS_NewString(ctx, @errorName(err)));
        return result_obj;
    };

    // Build result object
    const result_obj = qjs.JS_NewObject(ctx);

    const status: i32 = switch (wait_result) {
        .Exited => |code| @intCast(code),
        .Signal => -1,
        .Stopped => -1,
        else => -1,
    };

    _ = qjs.JS_SetPropertyStr(ctx, result_obj, "status", qjs.JS_NewInt32(ctx, status));
    _ = qjs.JS_SetPropertyStr(ctx, result_obj, "signal", quickjs.jsNull());
    _ = qjs.JS_SetPropertyStr(ctx, result_obj, "pid", qjs.JS_NewInt32(ctx, @intCast(child.id)));
    _ = qjs.JS_SetPropertyStr(ctx, result_obj, "stdout", createUint8Array(ctx, stdout_buf[0..stdout_len]));
    _ = qjs.JS_SetPropertyStr(ctx, result_obj, "stderr", createUint8Array(ctx, stderr_buf[0..stderr_len]));

    return result_obj;
}

/// Helper to create Uint8Array from bytes
fn createUint8Array(ctx: ?*qjs.JSContext, data: []const u8) qjs.JSValue {
    if (data.len == 0) {
        const global = qjs.JS_GetGlobalObject(ctx);
        defer qjs.JS_FreeValue(ctx, global);
        const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
        defer qjs.JS_FreeValue(ctx, uint8array_ctor);
        var zero_arg = [1]qjs.JSValue{qjs.JS_NewInt32(ctx, 0)};
        return qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &zero_arg);
    }

    // Create ArrayBuffer with data
    const array_buf = qjs.JS_NewArrayBufferCopy(ctx, data.ptr, data.len);
    if (qjs.JS_IsException(array_buf)) return array_buf;

    // Wrap in Uint8Array
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);
    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);
    var ctor_args = [1]qjs.JSValue{array_buf};
    const result = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    qjs.JS_FreeValue(ctx, array_buf); // Free after constructor call (constructor dups args)
    return result;
}

// ============================================================
// Async child process support
// ============================================================

const MAX_ASYNC_PROCESSES = 32;

const AsyncProcessEntry = struct {
    active: bool = false,
    child: ?std.process.Child = null,
    pid: i32 = 0,
    stdout_buf: [65536]u8 = undefined,
    stderr_buf: [65536]u8 = undefined,
    stdout_pos: usize = 0,
    stderr_pos: usize = 0,
    completed: bool = false,
    exit_code: i32 = 0,
};

var async_processes: [MAX_ASYNC_PROCESSES]AsyncProcessEntry = [_]AsyncProcessEntry{.{}} ** MAX_ASYNC_PROCESSES;

/// Allocate a slot for async process
fn allocateAsyncProcess() ?usize {
    for (&async_processes, 0..) |*entry, i| {
        if (!entry.active) {
            entry.* = .{ .active = true };
            return i;
        }
    }
    return null;
}

/// __edgebox_spawn_async(command, args_json) - Spawn process asynchronously
/// Returns: process slot ID (>=0) or error code (<0)
fn spawnAsync(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    if (argc < 1) {
        return qjs.JS_NewInt32(ctx, -2);
    }

    const cmd_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (cmd_cstr == null) {
        return qjs.JS_NewInt32(ctx, -3);
    }
    defer qjs.JS_FreeCString(ctx, cmd_cstr);

    // Allocate process slot
    const idx = allocateAsyncProcess() orelse {
        return qjs.JS_NewInt32(ctx, -4); // No slots available
    };

    // Build argv array (command is run through shell)
    const cmd_slice = std.mem.span(cmd_cstr);
    const shell_args = [_][]const u8{ "/bin/sh", "-c", cmd_slice };

    var child = std.process.Child.init(&shell_args, std.heap.page_allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    child.spawn() catch {
        async_processes[idx].active = false;
        return qjs.JS_NewInt32(ctx, -5); // Spawn failed
    };

    // Store process info
    async_processes[idx].child = child;
    async_processes[idx].pid = @intCast(child.id);
    async_processes[idx].completed = false;

    return qjs.JS_NewInt32(ctx, @intCast(idx));
}

/// __edgebox_poll_process(processId) - Poll async process for output/completion
/// Returns: { stdout: string|null, stderr: string|null, done: bool, code: number }
fn pollProcess(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return quickjs.jsNull();
    }

    var proc_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &proc_id, argv[0]);

    if (proc_id < 0 or proc_id >= MAX_ASYNC_PROCESSES) {
        return quickjs.jsNull();
    }

    const idx: usize = @intCast(proc_id);
    var entry = &async_processes[idx];

    if (!entry.active) {
        return quickjs.jsNull();
    }

    const result_obj = qjs.JS_NewObject(ctx);

    // If already completed, return final result
    if (entry.completed) {
        _ = qjs.JS_SetPropertyStr(ctx, result_obj, "done", quickjs.jsTrue());
        _ = qjs.JS_SetPropertyStr(ctx, result_obj, "code", qjs.JS_NewInt32(ctx, entry.exit_code));
        _ = qjs.JS_SetPropertyStr(ctx, result_obj, "stdout", quickjs.jsNull());
        _ = qjs.JS_SetPropertyStr(ctx, result_obj, "stderr", quickjs.jsNull());
        return result_obj;
    }

    // Try to read available stdout/stderr without blocking
    var new_stdout: ?[]const u8 = null;
    var new_stderr: ?[]const u8 = null;

    if (entry.child) |*child| {
        // Non-blocking read from stdout
        if (child.stdout) |stdout| {
            const fd = stdout.handle;
            // Use poll to check if data available
            var poll_fds = [1]std.posix.pollfd{
                .{ .fd = fd, .events = std.posix.POLL.IN, .revents = 0 },
            };
            const poll_result = std.posix.poll(&poll_fds, 0) catch 0;
            if (poll_result > 0 and (poll_fds[0].revents & std.posix.POLL.IN) != 0) {
                const space_left = entry.stdout_buf.len - entry.stdout_pos;
                if (space_left > 0) {
                    const bytes_read = std.posix.read(fd, entry.stdout_buf[entry.stdout_pos..]) catch 0;
                    if (bytes_read > 0) {
                        new_stdout = entry.stdout_buf[entry.stdout_pos .. entry.stdout_pos + bytes_read];
                        entry.stdout_pos += bytes_read;
                    }
                }
            }
        }

        // Non-blocking read from stderr
        if (child.stderr) |stderr| {
            const fd = stderr.handle;
            var poll_fds = [1]std.posix.pollfd{
                .{ .fd = fd, .events = std.posix.POLL.IN, .revents = 0 },
            };
            const poll_result = std.posix.poll(&poll_fds, 0) catch 0;
            if (poll_result > 0 and (poll_fds[0].revents & std.posix.POLL.IN) != 0) {
                const space_left = entry.stderr_buf.len - entry.stderr_pos;
                if (space_left > 0) {
                    const bytes_read = std.posix.read(fd, entry.stderr_buf[entry.stderr_pos..]) catch 0;
                    if (bytes_read > 0) {
                        new_stderr = entry.stderr_buf[entry.stderr_pos .. entry.stderr_pos + bytes_read];
                        entry.stderr_pos += bytes_read;
                    }
                }
            }
        }

        // Check if process completed (non-blocking)
        const wait_result = child.wait() catch null;
        if (wait_result) |wr| {
            entry.completed = true;
            entry.exit_code = switch (wr) {
                .Exited => |code| @intCast(code),
                .Signal => -1,
                .Stopped => -1,
                else => -1,
            };
            entry.child = null;
        }
    }

    // Build result
    if (new_stdout) |data| {
        _ = qjs.JS_SetPropertyStr(ctx, result_obj, "stdout", qjs.JS_NewStringLen(ctx, data.ptr, data.len));
    } else {
        _ = qjs.JS_SetPropertyStr(ctx, result_obj, "stdout", quickjs.jsNull());
    }

    if (new_stderr) |data| {
        _ = qjs.JS_SetPropertyStr(ctx, result_obj, "stderr", qjs.JS_NewStringLen(ctx, data.ptr, data.len));
    } else {
        _ = qjs.JS_SetPropertyStr(ctx, result_obj, "stderr", quickjs.jsNull());
    }

    _ = qjs.JS_SetPropertyStr(ctx, result_obj, "done", if (entry.completed) quickjs.jsTrue() else quickjs.jsFalse());
    _ = qjs.JS_SetPropertyStr(ctx, result_obj, "code", qjs.JS_NewInt32(ctx, entry.exit_code));

    return result_obj;
}

/// __edgebox_kill_process(processId, signal) - Kill async process
fn killProcess(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return quickjs.jsFalse();
    }

    var proc_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &proc_id, argv[0]);

    if (proc_id < 0 or proc_id >= MAX_ASYNC_PROCESSES) {
        return quickjs.jsFalse();
    }

    const idx: usize = @intCast(proc_id);
    const entry = &async_processes[idx];

    if (!entry.active or entry.child == null) {
        return quickjs.jsFalse();
    }

    // Get signal (default SIGTERM = 15)
    var signal: i32 = 15;
    if (argc >= 2) {
        _ = qjs.JS_ToInt32(ctx, &signal, argv[1]);
    }

    // Kill the process
    const result = std.c.kill(entry.pid, signal);
    return if (result == 0) quickjs.jsTrue() else quickjs.jsFalse();
}

/// __edgebox_cleanup_process(processId) - Clean up completed process slot
fn cleanupProcess(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return quickjs.jsFalse();
    }

    var proc_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &proc_id, argv[0]);

    if (proc_id < 0 or proc_id >= MAX_ASYNC_PROCESSES) {
        return quickjs.jsFalse();
    }

    const idx: usize = @intCast(proc_id);
    async_processes[idx] = .{};

    return quickjs.jsTrue();
}

/// __edgebox_socketpair() - Create a Unix domain socket pair for IPC
/// Returns: { parentFd: number, childFd: number } or null on error
fn socketpair(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        return quickjs.jsNull();
    }

    // Create Unix domain socket pair
    var fds: [2]c_int = undefined;
    const result = c.socketpair(c.AF_UNIX, c.SOCK_STREAM, 0, &fds);
    if (result < 0) {
        return quickjs.jsNull();
    }

    // Return object with both file descriptors
    const obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, obj, "parentFd", qjs.JS_NewInt32(ctx, fds[0]));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "childFd", qjs.JS_NewInt32(ctx, fds[1]));
    return obj;
}

/// __edgebox_ipc_write(fd, data) - Write data to IPC channel
/// Returns: bytes written or -1 on error
fn ipcWrite(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_NewInt32(ctx, -1);

    var fd: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &fd, argv[0]);

    // Get data as string
    var len: usize = undefined;
    const data_ptr = qjs.JS_ToCStringLen(ctx, &len, argv[1]);
    if (data_ptr == null) return qjs.JS_NewInt32(ctx, -1);
    defer qjs.JS_FreeCString(ctx, data_ptr);

    // Write to fd
    const written = c.write(fd, data_ptr, len);
    return qjs.JS_NewInt32(ctx, @intCast(written));
}

/// __edgebox_ipc_read(fd) - Read data from IPC channel (non-blocking)
/// Returns: string data or null if no data/error
fn ipcRead(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsNull();

    var fd: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &fd, argv[0]);

    // Set non-blocking mode temporarily
    const old_flags = c.fcntl(fd, c.F_GETFL, @as(c_int, 0));
    if (old_flags < 0) return quickjs.jsNull();

    _ = c.fcntl(fd, c.F_SETFL, old_flags | c.O_NONBLOCK);
    defer _ = c.fcntl(fd, c.F_SETFL, old_flags);

    // Try to read
    var buf: [8192]u8 = undefined;
    const bytes_read = c.read(fd, &buf, buf.len);

    if (bytes_read <= 0) {
        return quickjs.jsNull();
    }

    return qjs.JS_NewStringLen(ctx, &buf, @intCast(bytes_read));
}

/// __edgebox_close_fd(fd) - Close a file descriptor
fn closeFd(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    var fd: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &fd, argv[0]);

    _ = c.close(fd);
    return quickjs.jsTrue();
}

/// Register child_process module functions
pub fn register(ctx: ?*qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Create child_process object
    const cp_obj = qjs.JS_NewObject(ctx);

    // Register functions on child_process module
    inline for (.{
        .{ "execSync", execSync, 2 },
        .{ "spawnSync", spawnSync, 3 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, cp_obj, binding[0], func);
    }

    // Register async functions on global object (for JS polyfill access)
    inline for (.{
        .{ "__edgebox_spawn_async", spawnAsync, 1 },
        .{ "__edgebox_poll_process", pollProcess, 1 },
        .{ "__edgebox_kill_process", killProcess, 2 },
        .{ "__edgebox_cleanup_process", cleanupProcess, 1 },
        .{ "__edgebox_socketpair", socketpair, 0 },
        .{ "__edgebox_ipc_write", ipcWrite, 2 },
        .{ "__edgebox_ipc_read", ipcRead, 1 },
        .{ "__edgebox_close_fd", closeFd, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, global, binding[0], func);
    }

    // Set in _modules for require('child_process')
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "child_process", qjs.JS_DupValue(ctx, cp_obj));
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "node:child_process", cp_obj);
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, cp_obj);
    }
}
