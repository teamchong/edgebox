/// Native process module - QuickJS C functions
/// Properly sets up process like Node.js for native binaries
const std = @import("std");
const builtin = @import("builtin");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// Static buffer for cwd (avoids runtime allocation)
var cwd_buf: [4096]u8 = undefined;

// Process start time for uptime calculation
var process_start_ns: i128 = 0;

/// process.cwd() - Get actual current working directory
fn processCwd(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    // WASI doesn't support realpath, use fallback
    if (builtin.os.tag == .wasi) {
        const cwd = "/";
        return qjs.JS_NewStringLen(ctx, cwd.ptr, cwd.len);
    }
    const cwd = std.fs.cwd().realpath(".", &cwd_buf) catch "/";
    return qjs.JS_NewStringLen(ctx, cwd.ptr, cwd.len);
}

/// process.exit(code) - Exit with status code
fn processExit(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    var exit_code: u8 = 0;
    if (argc > 0) {
        var code: i32 = 0;
        _ = qjs.JS_ToInt32(null, &code, argv[0]);
        exit_code = if (code >= 0 and code <= 255) @intCast(code) else 1;
    }
    std.process.exit(exit_code);
}

/// process.nextTick(callback) - Schedule callback for next tick
fn processNextTick(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "nextTick requires a callback function");
    }

    // Use Promise.resolve().then(callback) for immediate execution
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const promise_ctor = qjs.JS_GetPropertyStr(ctx, global, "Promise");
    defer qjs.JS_FreeValue(ctx, promise_ctor);

    const resolve_func = qjs.JS_GetPropertyStr(ctx, promise_ctor, "resolve");
    defer qjs.JS_FreeValue(ctx, resolve_func);

    const resolved = qjs.JS_Call(ctx, resolve_func, promise_ctor, 0, null);
    defer qjs.JS_FreeValue(ctx, resolved);

    const then_func = qjs.JS_GetPropertyStr(ctx, resolved, "then");
    defer qjs.JS_FreeValue(ctx, then_func);

    var args = [1]qjs.JSValue{argv[0]};
    return qjs.JS_Call(ctx, then_func, resolved, 1, &args);
}

/// process.hrtime() - High-resolution time
fn processHrtime(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const now = std.time.nanoTimestamp();
    var secs: i128 = @divFloor(now, 1_000_000_000);
    var nanos: i128 = @mod(now, 1_000_000_000);

    // If previous time provided, compute difference
    if (argc > 0 and qjs.JS_IsArray(argv[0])) {
        var prev_secs: i32 = 0;
        var prev_nanos: i32 = 0;
        const prev_secs_val = qjs.JS_GetPropertyUint32(ctx, argv[0], 0);
        const prev_nanos_val = qjs.JS_GetPropertyUint32(ctx, argv[0], 1);
        _ = qjs.JS_ToInt32(ctx, &prev_secs, prev_secs_val);
        _ = qjs.JS_ToInt32(ctx, &prev_nanos, prev_nanos_val);
        qjs.JS_FreeValue(ctx, prev_secs_val);
        qjs.JS_FreeValue(ctx, prev_nanos_val);

        secs -= prev_secs;
        nanos -= prev_nanos;
        if (nanos < 0) {
            secs -= 1;
            nanos += 1_000_000_000;
        }
    }

    const result = qjs.JS_NewArray(ctx);
    _ = qjs.JS_SetPropertyUint32(ctx, result, 0, qjs.JS_NewInt64(ctx, @intCast(secs)));
    _ = qjs.JS_SetPropertyUint32(ctx, result, 1, qjs.JS_NewInt64(ctx, @intCast(nanos)));
    return result;
}

/// process.hrtime.bigint() - High-resolution time as BigInt
fn processHrtimeBigint(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const now = std.time.nanoTimestamp();
    const abs_now: u64 = if (now < 0) 0 else @intCast(now);

    // Call BigInt constructor: BigInt(nsString)
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const bigint_ctor = qjs.JS_GetPropertyStr(ctx, global, "BigInt");
    if (qjs.JS_IsUndefined(bigint_ctor)) {
        // BigInt not available, return number
        return qjs.JS_NewInt64(ctx, @intCast(abs_now));
    }
    defer qjs.JS_FreeValue(ctx, bigint_ctor);

    // Convert ns to string and call BigInt(str)
    var buf: [32]u8 = undefined;
    const ns_str = std.fmt.bufPrint(&buf, "{d}", .{abs_now}) catch "0";
    const js_str = qjs.JS_NewStringLen(ctx, ns_str.ptr, @intCast(ns_str.len));

    var args = [1]qjs.JSValue{js_str};
    const result = qjs.JS_Call(ctx, bigint_ctor, quickjs.jsUndefined(), 1, &args);
    qjs.JS_FreeValue(ctx, js_str);

    // Check if call failed
    if (qjs.JS_IsException(result)) {
        qjs.JS_FreeValue(ctx, result);
        return qjs.JS_NewInt64(ctx, @intCast(abs_now));
    }

    return result;
}

/// stdout.write(str) - Write to stdout
fn stdoutWrite(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const str = qjs.JS_ToCString(ctx, argv[0]);
    if (str == null) return quickjs.jsFalse();
    defer qjs.JS_FreeCString(ctx, str);

    const stdout = std.fs.File.stdout().deprecatedWriter();
    stdout.print("{s}", .{str}) catch return quickjs.jsFalse();

    return quickjs.jsTrue();
}

/// stderr.write(str) - Write to stderr
fn stderrWrite(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const str = qjs.JS_ToCString(ctx, argv[0]);
    if (str == null) return quickjs.jsFalse();
    defer qjs.JS_FreeCString(ctx, str);

    const stderr = std.fs.File.stderr().deprecatedWriter();
    stderr.print("{s}", .{str}) catch return quickjs.jsFalse();

    return quickjs.jsTrue();
}

/// Set process.argv from command-line arguments
/// Node.js convention: argv[0] = node, argv[1] = script, argv[2..] = args
pub fn setArgv(ctx: *qjs.JSContext, args: []const [:0]const u8) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const process_obj = qjs.JS_GetPropertyStr(ctx, global, "process");
    if (qjs.JS_IsUndefined(process_obj)) return;
    defer qjs.JS_FreeValue(ctx, process_obj);

    const argv_arr = qjs.JS_NewArray(ctx);
    var idx: u32 = 0;

    // argv[0] = 'node' (for Node.js compatibility)
    _ = qjs.JS_SetPropertyUint32(ctx, argv_arr, idx, qjs.JS_NewString(ctx, "node"));
    idx += 1;

    // argv[1] = script path placeholder (Node.js convention)
    // TypeScript and other tools use process.argv.slice(2) to get actual args
    _ = qjs.JS_SetPropertyUint32(ctx, argv_arr, idx, qjs.JS_NewString(ctx, "[embedded]"));
    idx += 1;

    // argv[2..] = actual command-line args
    for (args) |arg| {
        _ = qjs.JS_SetPropertyUint32(ctx, argv_arr, idx, qjs.JS_NewString(ctx, arg.ptr));
        idx += 1;
    }

    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "argv", argv_arr);
}

/// Get platform string
fn getPlatform() [*:0]const u8 {
    return switch (builtin.os.tag) {
        .macos => "darwin",
        .linux => "linux",
        .windows => "win32",
        .freebsd => "freebsd",
        else => "unknown",
    };
}

/// Get architecture string
fn getArch() [*:0]const u8 {
    return switch (builtin.cpu.arch) {
        .aarch64 => "arm64",
        .x86_64 => "x64",
        .x86 => "ia32",
        .arm => "arm",
        .wasm32 => "wasm32",
        else => "unknown",
    };
}

/// process.uptime() - Seconds since process started
fn processUptime(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const now = std.time.nanoTimestamp();
    const elapsed_ns = now - process_start_ns;
    const elapsed_sec = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000_000.0;
    return qjs.JS_NewFloat64(ctx, elapsed_sec);
}

/// Register all process functions and properties to globalThis.process
pub fn register(ctx: *qjs.JSContext) void {
    // Capture start time for uptime()
    if (process_start_ns == 0) {
        process_start_ns = std.time.nanoTimestamp();
    }

    const process_obj = qjs.JS_NewObject(ctx);

    // Platform info - detect actual platform
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "platform", qjs.JS_NewString(ctx, getPlatform()));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "arch", qjs.JS_NewString(ctx, getArch()));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "version", qjs.JS_NewString(ctx, "v20.0.0"));

    // versions object
    const versions_obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, versions_obj, "node", qjs.JS_NewString(ctx, "20.0.0"));
    _ = qjs.JS_SetPropertyStr(ctx, versions_obj, "v8", qjs.JS_NewString(ctx, "0.0.0"));
    _ = qjs.JS_SetPropertyStr(ctx, versions_obj, "quickjs", qjs.JS_NewString(ctx, "2024.1"));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "versions", versions_obj);

    // pid/ppid - use POSIX getpid/getppid for cross-platform
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "pid", qjs.JS_NewInt32(ctx, @intCast(std.c.getpid())));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "ppid", qjs.JS_NewInt32(ctx, @intCast(std.c.getppid())));

    // execPath - path to this binary
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "execPath", qjs.JS_NewString(ctx, "/usr/bin/node"));

    // execArgv - empty array (no Node.js flags)
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "execArgv", qjs.JS_NewArray(ctx));

    // argv - empty array (will be populated by setArgv)
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "argv", qjs.JS_NewArray(ctx));

    // env - Proxy-like object that reads from environment
    // For simplicity, just create an empty object; native_bindings will populate it
    const env_obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "env", env_obj);

    // stdout object
    const stdout_obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, stdout_obj, "write", qjs.JS_NewCFunction(ctx, stdoutWrite, "write", 1));
    _ = qjs.JS_SetPropertyStr(ctx, stdout_obj, "isTTY", quickjs.jsFalse());
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "stdout", stdout_obj);

    // stderr object
    const stderr_obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, stderr_obj, "write", qjs.JS_NewCFunction(ctx, stderrWrite, "write", 1));
    _ = qjs.JS_SetPropertyStr(ctx, stderr_obj, "isTTY", quickjs.jsFalse());
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "stderr", stderr_obj);

    // Register functions
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "cwd", qjs.JS_NewCFunction(ctx, processCwd, "cwd", 0));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "exit", qjs.JS_NewCFunction(ctx, processExit, "exit", 1));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "nextTick", qjs.JS_NewCFunction(ctx, processNextTick, "nextTick", 1));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "uptime", qjs.JS_NewCFunction(ctx, processUptime, "uptime", 0));

    // hrtime function with bigint method
    const hrtime_func = qjs.JS_NewCFunction(ctx, processHrtime, "hrtime", 1);
    _ = qjs.JS_SetPropertyStr(ctx, hrtime_func, "bigint", qjs.JS_NewCFunction(ctx, processHrtimeBigint, "bigint", 0));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "hrtime", hrtime_func);

    // Set as global.process
    const global = qjs.JS_GetGlobalObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, global, "process", qjs.JS_DupValue(ctx, process_obj));

    // Also add to _modules for require('process')
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "process", process_obj);
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, process_obj);
    }

    qjs.JS_FreeValue(ctx, global);
}
