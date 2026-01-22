/// Native process module - QuickJS C functions
/// Properly sets up process like Node.js for native binaries
const std = @import("std");
const builtin = @import("builtin");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// POSIX functions not in std.c
const posix = struct {
    extern fn getgid() c_uint;
    extern fn getegid() c_uint;
    extern fn getgroups(size: c_int, list: [*]c_uint) c_int;
    extern fn umask(mask: c_uint) c_uint;
    // Round 12: setuid/setgid functions
    extern fn setuid(uid: c_uint) c_int;
    extern fn setgid(gid: c_uint) c_int;
    extern fn seteuid(uid: c_uint) c_int;
    extern fn setegid(gid: c_uint) c_int;
    // Round 13: setgroups, initgroups, getrusage
    extern fn setgroups(size: usize, list: [*]const c_uint) c_int;
    extern fn initgroups(user: [*:0]const u8, group: c_uint) c_int;
    extern fn getrusage(who: c_int, usage: *rusage) c_int;
    const RUSAGE_SELF: c_int = 0;
};

// rusage structure for getrusage (platform-specific)
const timeval = extern struct {
    tv_sec: i64,
    tv_usec: i64,
};

const rusage = extern struct {
    ru_utime: timeval, // user CPU time used
    ru_stime: timeval, // system CPU time used
    ru_maxrss: c_long, // maximum resident set size
    ru_ixrss: c_long, // integral shared memory size
    ru_idrss: c_long, // integral unshared data size
    ru_isrss: c_long, // integral unshared stack size
    ru_minflt: c_long, // page reclaims (soft page faults)
    ru_majflt: c_long, // page faults (hard page faults)
    ru_nswap: c_long, // swaps
    ru_inblock: c_long, // block input operations
    ru_oublock: c_long, // block output operations
    ru_msgsnd: c_long, // IPC messages sent
    ru_msgrcv: c_long, // IPC messages received
    ru_nsignals: c_long, // signals received
    ru_nvcsw: c_long, // voluntary context switches
    ru_nivcsw: c_long, // involuntary context switches
};

// macOS Mach API for memory usage
const mach = struct {
    // Mach port types
    const mach_port_t = c_uint;
    const kern_return_t = c_int;
    const task_flavor_t = c_uint;
    const mach_msg_type_number_t = c_uint;
    const natural_t = c_uint;

    const KERN_SUCCESS: kern_return_t = 0;
    const MACH_TASK_BASIC_INFO: task_flavor_t = 20;
    const MACH_TASK_BASIC_INFO_COUNT: mach_msg_type_number_t = @sizeOf(mach_task_basic_info) / @sizeOf(natural_t);

    // mach_task_basic_info structure (macOS 10.8+)
    const mach_task_basic_info = extern struct {
        virtual_size: u64, // virtual memory size (bytes)
        resident_size: u64, // resident memory size (bytes)
        resident_size_max: u64, // max resident size (bytes)
        user_time: time_value_t, // total user run time
        system_time: time_value_t, // total system run time
        policy: c_int, // scheduling policy
        suspend_count: c_int, // suspend count
    };

    const time_value_t = extern struct {
        seconds: c_int,
        microseconds: c_int,
    };

    extern fn mach_task_self() mach_port_t;
    extern fn task_info(target_task: mach_port_t, flavor: task_flavor_t, task_info_out: *mach_task_basic_info, task_info_outCnt: *mach_msg_type_number_t) kern_return_t;
};

// Static buffer for process.title
var process_title_buf: [256]u8 = undefined;
var process_title_len: usize = 0;

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
    if (builtin.os.tag == .macos) {
        // macOS: use mach_task_basic_info
        var info: mach.mach_task_basic_info = undefined;
        var count: mach.mach_msg_type_number_t = mach.MACH_TASK_BASIC_INFO_COUNT;

        const kr = mach.task_info(
            mach.mach_task_self(),
            mach.MACH_TASK_BASIC_INFO,
            &info,
            &count,
        );

        if (kr == mach.KERN_SUCCESS) {
            rss = @intCast(info.resident_size);
            heap_total = @intCast(info.virtual_size);
            // heapUsed is approximately the resident size
            heap_used = @intCast(info.resident_size);
        }
    } else if (builtin.os.tag == .linux) {
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
    // For other platforms (WASI, Windows), return 0

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

/// process.kill(pid, signal) - Send signal to a process
/// Returns true on success, false on failure
fn processKill(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "pid is required");
    }

    var pid: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &pid, argv[0]);

    // Default to SIGTERM (15)
    var sig: i32 = 15;
    if (argc > 1) {
        _ = qjs.JS_ToInt32(ctx, &sig, argv[1]);
    }

    // On WASI, return false (can't send signals)
    if (builtin.os.tag == .wasi) {
        return quickjs.jsFalse();
    }

    // Use POSIX kill()
    const result = std.c.kill(pid, sig);
    return if (result == 0) quickjs.jsTrue() else quickjs.jsFalse();
}

/// process.abort() - Abort the process immediately
fn processAbort(_: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag != .wasi) {
        std.c.abort();
    }
    // On WASI, just exit with error code
    std.process.exit(134); // 128 + 6 (SIGABRT)
}

/// process.chdir(path) - Change current working directory
fn processChdir(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "path required");
    }

    const path_str = qjs.JS_ToCString(ctx, argv[0]);
    if (path_str == null) {
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    }
    defer qjs.JS_FreeCString(ctx, path_str);

    // On WASI, chdir is not supported
    if (builtin.os.tag == .wasi) {
        return quickjs.jsUndefined();
    }

    // Use std.posix.chdir
    const path_slice = std.mem.span(path_str);
    std.posix.chdir(path_slice) catch |err| {
        const err_name = @errorName(err);
        return qjs.JS_ThrowTypeError(ctx, err_name.ptr);
    };

    return quickjs.jsUndefined();
}

/// process.getuid() - Get real user ID
fn processGetuid(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        return qjs.JS_NewInt32(ctx, 0);
    }
    return qjs.JS_NewInt32(ctx, @intCast(std.c.getuid()));
}

/// process.getgid() - Get real group ID
fn processGetgid(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        return qjs.JS_NewInt32(ctx, 0);
    }
    return qjs.JS_NewInt32(ctx, @intCast(posix.getgid()));
}

/// process.geteuid() - Get effective user ID
fn processGeteuid(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        return qjs.JS_NewInt32(ctx, 0);
    }
    return qjs.JS_NewInt32(ctx, @intCast(std.c.geteuid()));
}

/// process.getegid() - Get effective group ID
fn processGetegid(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        return qjs.JS_NewInt32(ctx, 0);
    }
    return qjs.JS_NewInt32(ctx, @intCast(posix.getegid()));
}

/// process.setuid(id) - Set real user ID
fn processSetuid(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "setuid requires 1 argument");
    }
    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        return qjs.JS_ThrowTypeError(ctx, "setuid is not supported on this platform");
    }
    var uid: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &uid, argv[0]);
    const result = posix.setuid(@intCast(uid));
    if (result < 0) {
        return qjs.JS_ThrowTypeError(ctx, "setuid failed: permission denied");
    }
    return quickjs.jsUndefined();
}

/// process.setgid(id) - Set real group ID
fn processSetgid(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "setgid requires 1 argument");
    }
    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        return qjs.JS_ThrowTypeError(ctx, "setgid is not supported on this platform");
    }
    var gid: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &gid, argv[0]);
    const result = posix.setgid(@intCast(gid));
    if (result < 0) {
        return qjs.JS_ThrowTypeError(ctx, "setgid failed: permission denied");
    }
    return quickjs.jsUndefined();
}

/// process.seteuid(id) - Set effective user ID
fn processSeteuid(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "seteuid requires 1 argument");
    }
    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        return qjs.JS_ThrowTypeError(ctx, "seteuid is not supported on this platform");
    }
    var uid: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &uid, argv[0]);
    const result = posix.seteuid(@intCast(uid));
    if (result < 0) {
        return qjs.JS_ThrowTypeError(ctx, "seteuid failed: permission denied");
    }
    return quickjs.jsUndefined();
}

/// process.setegid(id) - Set effective group ID
fn processSetegid(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "setegid requires 1 argument");
    }
    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        return qjs.JS_ThrowTypeError(ctx, "setegid is not supported on this platform");
    }
    var gid: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &gid, argv[0]);
    const result = posix.setegid(@intCast(gid));
    if (result < 0) {
        return qjs.JS_ThrowTypeError(ctx, "setegid failed: permission denied");
    }
    return quickjs.jsUndefined();
}

/// process.setgroups(groups) - Set supplementary group IDs
fn processSetgroups(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "setgroups requires an array argument");
    }
    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        return qjs.JS_ThrowTypeError(ctx, "setgroups is not supported on this platform");
    }

    // Get array length
    const length_val = qjs.JS_GetPropertyStr(ctx, argv[0], "length");
    defer qjs.JS_FreeValue(ctx, length_val);
    var length: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &length, length_val);

    if (length > 64) {
        return qjs.JS_ThrowRangeError(ctx, "too many groups");
    }

    // Extract group IDs from array
    var groups_buf: [64]c_uint = undefined;
    var i: u32 = 0;
    while (i < @as(u32, @intCast(length))) : (i += 1) {
        const elem = qjs.JS_GetPropertyUint32(ctx, argv[0], i);
        defer qjs.JS_FreeValue(ctx, elem);
        var gid: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &gid, elem);
        groups_buf[i] = @intCast(gid);
    }

    const result = posix.setgroups(@intCast(length), &groups_buf);
    if (result < 0) {
        return qjs.JS_ThrowTypeError(ctx, "setgroups failed: permission denied");
    }
    return quickjs.jsUndefined();
}

/// process.initgroups(user, extraGroup) - Initialize supplementary groups
fn processInitgroups(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) {
        return qjs.JS_ThrowTypeError(ctx, "initgroups requires user and extraGroup arguments");
    }
    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        return qjs.JS_ThrowTypeError(ctx, "initgroups is not supported on this platform");
    }

    const user_str = qjs.JS_ToCString(ctx, argv[0]);
    if (user_str == null) {
        return qjs.JS_ThrowTypeError(ctx, "user must be a string");
    }
    defer qjs.JS_FreeCString(ctx, user_str);

    var gid: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &gid, argv[1]);

    const result = posix.initgroups(user_str, @intCast(gid));
    if (result < 0) {
        return qjs.JS_ThrowTypeError(ctx, "initgroups failed: permission denied or user not found");
    }
    return quickjs.jsUndefined();
}

/// process.resourceUsage() - Get resource usage for current process
fn processResourceUsage(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        // Return stub object on unsupported platforms
        const obj = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, obj, "userCPUTime", qjs.JS_NewInt64(ctx, 0));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "systemCPUTime", qjs.JS_NewInt64(ctx, 0));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "maxRSS", qjs.JS_NewInt64(ctx, 0));
        return obj;
    }

    var usage: rusage = undefined;
    const result = posix.getrusage(posix.RUSAGE_SELF, &usage);
    if (result < 0) {
        const obj = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, obj, "userCPUTime", qjs.JS_NewInt64(ctx, 0));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "systemCPUTime", qjs.JS_NewInt64(ctx, 0));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "maxRSS", qjs.JS_NewInt64(ctx, 0));
        return obj;
    }

    const obj = qjs.JS_NewObject(ctx);
    // Convert timeval to microseconds
    const user_us = usage.ru_utime.tv_sec * 1_000_000 + usage.ru_utime.tv_usec;
    const system_us = usage.ru_stime.tv_sec * 1_000_000 + usage.ru_stime.tv_usec;

    _ = qjs.JS_SetPropertyStr(ctx, obj, "userCPUTime", qjs.JS_NewInt64(ctx, user_us));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "systemCPUTime", qjs.JS_NewInt64(ctx, system_us));
    // maxRSS is in bytes on Linux, kilobytes on macOS - Node.js returns bytes
    const max_rss = if (builtin.os.tag == .macos) usage.ru_maxrss * 1024 else usage.ru_maxrss;
    _ = qjs.JS_SetPropertyStr(ctx, obj, "maxRSS", qjs.JS_NewInt64(ctx, max_rss));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "sharedMemorySize", qjs.JS_NewInt64(ctx, usage.ru_ixrss));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "unsharedDataSize", qjs.JS_NewInt64(ctx, usage.ru_idrss));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "unsharedStackSize", qjs.JS_NewInt64(ctx, usage.ru_isrss));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "minorPageFault", qjs.JS_NewInt64(ctx, usage.ru_minflt));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "majorPageFault", qjs.JS_NewInt64(ctx, usage.ru_majflt));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "swappedOut", qjs.JS_NewInt64(ctx, usage.ru_nswap));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "fsRead", qjs.JS_NewInt64(ctx, usage.ru_inblock));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "fsWrite", qjs.JS_NewInt64(ctx, usage.ru_oublock));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "ipcSent", qjs.JS_NewInt64(ctx, usage.ru_msgsnd));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "ipcReceived", qjs.JS_NewInt64(ctx, usage.ru_msgrcv));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "signalsCount", qjs.JS_NewInt64(ctx, usage.ru_nsignals));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "voluntaryContextSwitches", qjs.JS_NewInt64(ctx, usage.ru_nvcsw));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "involuntaryContextSwitches", qjs.JS_NewInt64(ctx, usage.ru_nivcsw));

    return obj;
}

/// process.getgroups() - Get list of supplementary group IDs
fn processGetgroups(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    // Return empty array on WASI/Windows
    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        return qjs.JS_NewArray(ctx);
    }

    // Use POSIX getgroups()
    var groups_buf: [64]c_uint = undefined;
    const ngroups = posix.getgroups(64, &groups_buf);

    const arr = qjs.JS_NewArray(ctx);
    if (ngroups > 0) {
        var i: u32 = 0;
        while (i < @as(u32, @intCast(ngroups))) : (i += 1) {
            _ = qjs.JS_SetPropertyUint32(ctx, arr, i, qjs.JS_NewInt32(ctx, @intCast(groups_buf[i])));
        }
    }
    return arr;
}

/// process.umask() - Get file mode creation mask
/// process.umask(mask) - Set and return previous mask
fn processUmask(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    // On WASI/Windows, return default umask
    if (builtin.os.tag == .wasi or builtin.os.tag == .windows) {
        return qjs.JS_NewInt32(ctx, 0o022);
    }

    if (argc == 0) {
        // Get current umask: set 0, then restore
        const current = posix.umask(0);
        _ = posix.umask(current);
        return qjs.JS_NewInt32(ctx, @intCast(current));
    }

    // Set new umask, return old one
    var new_mask: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &new_mask, argv[0]);
    const old_mask = posix.umask(@intCast(new_mask & 0o777));
    return qjs.JS_NewInt32(ctx, @intCast(old_mask));
}

/// Getter for process.title
fn processTitleGetter(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (process_title_len == 0) {
        // Return default title
        return qjs.JS_NewString(ctx, "edgebox");
    }
    return qjs.JS_NewStringLen(ctx, &process_title_buf, process_title_len);
}

/// Setter for process.title (stored in buffer, actual process title may not change)
fn processTitleSetter(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsUndefined();

    var len: usize = undefined;
    const str = qjs.JS_ToCStringLen(ctx, &len, argv[0]);
    if (str == null) return quickjs.jsUndefined();
    defer qjs.JS_FreeCString(ctx, str);

    // Copy to buffer (max 255 chars)
    const copy_len = @min(len, 255);
    @memcpy(process_title_buf[0..copy_len], str[0..copy_len]);
    process_title_len = copy_len;

    return quickjs.jsUndefined();
}

/// Create process.config object (Node.js build configuration)
fn createConfigObject(ctx: *qjs.JSContext) qjs.JSValue {
    const config = qjs.JS_NewObject(ctx);
    const target_defaults = qjs.JS_NewObject(ctx);
    const variables = qjs.JS_NewObject(ctx);

    // target_defaults - simplified
    _ = qjs.JS_SetPropertyStr(ctx, target_defaults, "cflags", qjs.JS_NewArray(ctx));
    _ = qjs.JS_SetPropertyStr(ctx, target_defaults, "default_configuration", qjs.JS_NewString(ctx, "Release"));
    _ = qjs.JS_SetPropertyStr(ctx, target_defaults, "defines", qjs.JS_NewArray(ctx));

    // variables - EdgeBox-specific
    _ = qjs.JS_SetPropertyStr(ctx, variables, "asan", qjs.JS_NewInt32(ctx, 0));
    _ = qjs.JS_SetPropertyStr(ctx, variables, "coverage", quickjs.jsFalse());
    _ = qjs.JS_SetPropertyStr(ctx, variables, "debug_nghttp2", quickjs.jsFalse());
    _ = qjs.JS_SetPropertyStr(ctx, variables, "force_dynamic_crt", qjs.JS_NewInt32(ctx, 0));
    _ = qjs.JS_SetPropertyStr(ctx, variables, "host_arch", qjs.JS_NewString(ctx, getArch()));
    _ = qjs.JS_SetPropertyStr(ctx, variables, "icu_small", quickjs.jsFalse());
    _ = qjs.JS_SetPropertyStr(ctx, variables, "node_install_npm", quickjs.jsFalse());
    _ = qjs.JS_SetPropertyStr(ctx, variables, "node_module_version", qjs.JS_NewInt32(ctx, 115));
    _ = qjs.JS_SetPropertyStr(ctx, variables, "node_prefix", qjs.JS_NewString(ctx, "/usr/local"));
    _ = qjs.JS_SetPropertyStr(ctx, variables, "node_shared", quickjs.jsFalse());
    _ = qjs.JS_SetPropertyStr(ctx, variables, "openssl_fips", qjs.JS_NewString(ctx, ""));
    _ = qjs.JS_SetPropertyStr(ctx, variables, "shlib_suffix", qjs.JS_NewString(ctx, ".dylib"));
    _ = qjs.JS_SetPropertyStr(ctx, variables, "target_arch", qjs.JS_NewString(ctx, getArch()));
    _ = qjs.JS_SetPropertyStr(ctx, variables, "v8_enable_inspector", qjs.JS_NewInt32(ctx, 0));

    _ = qjs.JS_SetPropertyStr(ctx, config, "target_defaults", target_defaults);
    _ = qjs.JS_SetPropertyStr(ctx, config, "variables", variables);

    return config;
}

/// Create process.release object (Node.js release information)
fn createReleaseObject(ctx: *qjs.JSContext) qjs.JSValue {
    const release = qjs.JS_NewObject(ctx);

    _ = qjs.JS_SetPropertyStr(ctx, release, "name", qjs.JS_NewString(ctx, "edgebox"));
    _ = qjs.JS_SetPropertyStr(ctx, release, "sourceUrl", qjs.JS_NewString(ctx, "https://github.com/nickolasburr/edgebox/archive/v1.0.0.tar.gz"));
    _ = qjs.JS_SetPropertyStr(ctx, release, "headersUrl", qjs.JS_NewString(ctx, "https://github.com/nickolasburr/edgebox/archive/v1.0.0-headers.tar.gz"));
    _ = qjs.JS_SetPropertyStr(ctx, release, "libUrl", qjs.JS_NewString(ctx, ""));
    _ = qjs.JS_SetPropertyStr(ctx, release, "lts", quickjs.jsFalse());

    return release;
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

    // pid/ppid - use POSIX getpid/getppid for native, stubs for WASM
    const is_wasm = builtin.target.cpu.arch == .wasm32 or builtin.target.cpu.arch == .wasm64;
    const pid: i32 = if (is_wasm) 1 else @intCast(std.c.getpid());
    const ppid: i32 = if (is_wasm) 0 else @intCast(std.c.getppid());
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "pid", qjs.JS_NewInt32(ctx, pid));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "ppid", qjs.JS_NewInt32(ctx, ppid));

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

    // stdin object (minimal - just for type checking/compatibility)
    const stdin_obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, stdin_obj, "isTTY", quickjs.jsFalse());
    _ = qjs.JS_SetPropertyStr(ctx, stdin_obj, "fd", qjs.JS_NewInt32(ctx, 0));
    _ = qjs.JS_SetPropertyStr(ctx, stdin_obj, "isRaw", quickjs.jsFalse());
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "stdin", stdin_obj);

    // Register functions
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "cwd", qjs.JS_NewCFunction(ctx, processCwd, "cwd", 0));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "exit", qjs.JS_NewCFunction(ctx, processExit, "exit", 1));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "nextTick", qjs.JS_NewCFunction(ctx, processNextTick, "nextTick", 1));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "uptime", qjs.JS_NewCFunction(ctx, processUptime, "uptime", 0));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "memoryUsage", qjs.JS_NewCFunction(ctx, processMemoryUsage, "memoryUsage", 0));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "cpuUsage", qjs.JS_NewCFunction(ctx, processCpuUsage, "cpuUsage", 1));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "kill", qjs.JS_NewCFunction(ctx, processKill, "kill", 2));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "abort", qjs.JS_NewCFunction(ctx, processAbort, "abort", 0));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "chdir", qjs.JS_NewCFunction(ctx, processChdir, "chdir", 1));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "getuid", qjs.JS_NewCFunction(ctx, processGetuid, "getuid", 0));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "getgid", qjs.JS_NewCFunction(ctx, processGetgid, "getgid", 0));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "geteuid", qjs.JS_NewCFunction(ctx, processGeteuid, "geteuid", 0));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "getegid", qjs.JS_NewCFunction(ctx, processGetegid, "getegid", 0));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "getgroups", qjs.JS_NewCFunction(ctx, processGetgroups, "getgroups", 0));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "umask", qjs.JS_NewCFunction(ctx, processUmask, "umask", 1));
    // Round 12: setuid/setgid/seteuid/setegid
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "setuid", qjs.JS_NewCFunction(ctx, processSetuid, "setuid", 1));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "setgid", qjs.JS_NewCFunction(ctx, processSetgid, "setgid", 1));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "seteuid", qjs.JS_NewCFunction(ctx, processSeteuid, "seteuid", 1));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "setegid", qjs.JS_NewCFunction(ctx, processSetegid, "setegid", 1));
    // Round 13: setgroups, initgroups, resourceUsage, argv0
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "setgroups", qjs.JS_NewCFunction(ctx, processSetgroups, "setgroups", 1));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "initgroups", qjs.JS_NewCFunction(ctx, processInitgroups, "initgroups", 2));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "resourceUsage", qjs.JS_NewCFunction(ctx, processResourceUsage, "resourceUsage", 0));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "argv0", qjs.JS_NewString(ctx, "edgebox"));

    // process.title - use getter/setter functions for simplicity
    // Note: In a full implementation, we'd use JS_DefinePropertyGetSet for proper getter/setter
    // For now, expose as _getTitle/_setTitle and handle via JS proxy if needed
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "_getTitle", qjs.JS_NewCFunction(ctx, processTitleGetter, "_getTitle", 0));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "_setTitle", qjs.JS_NewCFunction(ctx, processTitleSetter, "_setTitle", 1));
    // Also set initial title property
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "title", qjs.JS_NewString(ctx, "edgebox"));

    // process.config - build configuration
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "config", createConfigObject(ctx));

    // process.release - release information
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "release", createReleaseObject(ctx));

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
