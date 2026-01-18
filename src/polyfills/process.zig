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

/// process.memoryUsage() - Return memory usage stats
/// Returns { rss, heapTotal, heapUsed, external, arrayBuffers }
fn processMemoryUsage(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const obj = qjs.JS_NewObject(ctx);

    var rss: i64 = 0;
    var heap_total: i64 = 0;
    var heap_used: i64 = 0;

    // Platform-specific memory info
    if (builtin.os.tag == .linux) {
        // Linux: read from /proc/self/statm
        var buf: [256]u8 = undefined;
        if (std.fs.cwd().openFile("/proc/self/statm", .{})) |file| {
            defer file.close();
            const bytes_read = file.read(&buf) catch 0;
            if (bytes_read > 0) {
                var it = std.mem.splitScalar(u8, buf[0..bytes_read], ' ');
                // First value is total pages, second is resident pages
                if (it.next()) |_| {
                    if (it.next()) |resident_str| {
                        const resident = std.fmt.parseInt(i64, resident_str, 10) catch 0;
                        const page_size: i64 = 4096; // Common page size
                        rss = resident * page_size;
                        heap_used = rss;
                        heap_total = rss * 2; // Estimate
                    }
                }
            }
        } else |_| {}
    }
    // For macOS and other platforms, return 0 for now
    // TODO: implement mach_task_basic_info for macOS if needed

    _ = qjs.JS_SetPropertyStr(ctx, obj, "rss", qjs.JS_NewInt64(ctx, rss));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "heapTotal", qjs.JS_NewInt64(ctx, heap_total));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "heapUsed", qjs.JS_NewInt64(ctx, heap_used));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "external", qjs.JS_NewInt64(ctx, 0));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "arrayBuffers", qjs.JS_NewInt64(ctx, 0));

    return obj;
}

/// process.cpuUsage(previousValue) - Return CPU usage in microseconds
/// Returns { user, system } representing time spent in user/system mode
fn processCpuUsage(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const obj = qjs.JS_NewObject(ctx);

    var user_us: i64 = 0;
    var system_us: i64 = 0;

    // Get current CPU usage using getrusage (POSIX)
    if (builtin.os.tag != .wasi) {
        const usage = std.posix.getrusage(0); // RUSAGE_SELF = 0
        // Convert timeval to microseconds
        user_us = @as(i64, usage.utime.sec) * 1_000_000 + @as(i64, @intCast(usage.utime.usec));
        system_us = @as(i64, usage.stime.sec) * 1_000_000 + @as(i64, @intCast(usage.stime.usec));
    }

    // If previous value provided, calculate delta
    if (argc > 0 and qjs.JS_IsObject(argv[0])) {
        const prev_user_val = qjs.JS_GetPropertyStr(ctx, argv[0], "user");
        const prev_system_val = qjs.JS_GetPropertyStr(ctx, argv[0], "system");
        defer qjs.JS_FreeValue(ctx, prev_user_val);
        defer qjs.JS_FreeValue(ctx, prev_system_val);

        var prev_user: i64 = 0;
        var prev_system: i64 = 0;
        _ = qjs.JS_ToInt64(ctx, &prev_user, prev_user_val);
        _ = qjs.JS_ToInt64(ctx, &prev_system, prev_system_val);

        user_us -= prev_user;
        system_us -= prev_system;
    }

    _ = qjs.JS_SetPropertyStr(ctx, obj, "user", qjs.JS_NewInt64(ctx, user_us));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "system", qjs.JS_NewInt64(ctx, system_us));

    return obj;
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
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "memoryUsage", qjs.JS_NewCFunction(ctx, processMemoryUsage, "memoryUsage", 0));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "cpuUsage", qjs.JS_NewCFunction(ctx, processCpuUsage, "cpuUsage", 1));

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
