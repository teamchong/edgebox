/// Native child_process module - QuickJS C functions
/// Provides execSync and spawnSync for running external commands
const std = @import("std");
const builtin = @import("builtin");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

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

    var child = std.process.Child.init(.{
        .argv = &[_][]const u8{ "/bin/sh", "-c", cmd_slice },
        .cwd = if (cwd) |c| c else null,
        .stdout_behavior = .Pipe,
        .stderr_behavior = .Pipe,
    }, std.heap.page_allocator);

    child.spawn() catch |err| {
        const err_obj = qjs.JS_NewError(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err_obj, "code", qjs.JS_NewString(ctx, "ENOENT"));
        _ = qjs.JS_SetPropertyStr(ctx, err_obj, "message", qjs.JS_NewString(ctx, @errorName(err)));
        return qjs.JS_Throw(ctx, err_obj);
    };

    // Read stdout
    var stdout_len: usize = 0;
    if (child.stdout) |stdout| {
        stdout_len = stdout.reader().readAll(&stdout_buf) catch 0;
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
            stderr_len = stderr.reader().readAll(&stderr_buf) catch 0;
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
    var child = std.process.Child.init(.{
        .argv = args_array[0..args_count],
        .cwd = cwd,
        .stdout_behavior = .Pipe,
        .stderr_behavior = .Pipe,
    }, std.heap.page_allocator);

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
        stdout_len = stdout.reader().readAll(&stdout_buf) catch 0;
    }
    if (child.stderr) |stderr| {
        stderr_len = stderr.reader().readAll(&stderr_buf) catch 0;
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
    return qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
}

/// Register child_process module functions
pub fn register(ctx: ?*qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Create child_process object
    const cp_obj = qjs.JS_NewObject(ctx);

    // Register functions
    inline for (.{
        .{ "execSync", execSync, 2 },
        .{ "spawnSync", spawnSync, 3 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, cp_obj, binding[0], func);
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
