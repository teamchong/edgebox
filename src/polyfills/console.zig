/// Native console module - QuickJS C functions
/// Registered ONCE at WASM init via inline for loop
/// Zero runtime overhead, platform-native output
const std = @import("std");
const builtin = @import("builtin");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

const is_wasm = builtin.cpu.arch == .wasm32 or builtin.cpu.arch == .wasm64;

/// Helper to print to stdout (platform-aware)
fn printToStdout(text: []const u8) void {
    if (comptime is_wasm) {
        var nwritten: usize = undefined;
        const iov = [_]std.os.wasi.ciovec_t{.{ .base = text.ptr, .len = text.len }};
        _ = std.os.wasi.fd_write(1, &iov, 1, &nwritten);
    } else {
        _ = std.posix.write(std.posix.STDOUT_FILENO, text) catch {};
    }
}

/// Helper to print to stderr (platform-aware)
fn printToStderr(text: []const u8) void {
    if (comptime is_wasm) {
        var nwritten: usize = undefined;
        const iov = [_]std.os.wasi.ciovec_t{.{ .base = text.ptr, .len = text.len }};
        _ = std.os.wasi.fd_write(2, &iov, 1, &nwritten);
    } else {
        _ = std.posix.write(std.posix.STDERR_FILENO, text) catch {};
    }
}

/// Buffer for formatting console output
var console_buffer: [8192]u8 = undefined;

/// Timer storage for console.time/timeEnd/timeLog
/// Uses fixed-size array to avoid allocator - supports up to 32 concurrent timers
const MAX_TIMERS = 32;
const MAX_LABEL_LEN = 64;
var timer_labels: [MAX_TIMERS][MAX_LABEL_LEN]u8 = undefined;
var timer_label_lens: [MAX_TIMERS]usize = [_]usize{0} ** MAX_TIMERS;
var timer_starts: [MAX_TIMERS]i128 = [_]i128{0} ** MAX_TIMERS;
var timer_count: usize = 0;

/// Counter storage for console.count/countReset
const MAX_COUNTERS = 32;
var counter_labels: [MAX_COUNTERS][MAX_LABEL_LEN]u8 = undefined;
var counter_label_lens: [MAX_COUNTERS]usize = [_]usize{0} ** MAX_COUNTERS;
var counter_values: [MAX_COUNTERS]u32 = [_]u32{0} ** MAX_COUNTERS;
var counter_count: usize = 0;

/// Find timer by label, returns index or null
fn findTimer(label: []const u8) ?usize {
    for (0..timer_count) |i| {
        if (timer_label_lens[i] == label.len and
            std.mem.eql(u8, timer_labels[i][0..timer_label_lens[i]], label))
        {
            return i;
        }
    }
    return null;
}

/// Find counter by label, returns index or null
fn findCounter(label: []const u8) ?usize {
    for (0..counter_count) |i| {
        if (counter_label_lens[i] == label.len and
            std.mem.eql(u8, counter_labels[i][0..counter_label_lens[i]], label))
        {
            return i;
        }
    }
    return null;
}

/// console.log(...args) - Print to stdout
fn consoleLog(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    var pos: usize = 0;

    for (0..@intCast(argc)) |i| {
        if (i > 0 and pos < console_buffer.len) {
            console_buffer[pos] = ' ';
            pos += 1;
        }

        var len: usize = undefined;
        const str = qjs.JS_ToCStringLen(ctx, &len, argv[i]);
        if (str != null) {
            defer qjs.JS_FreeCString(ctx, str);
            const text = str[0..len];
            const copy_len = @min(text.len, console_buffer.len - pos);
            if (copy_len > 0) {
                @memcpy(console_buffer[pos..][0..copy_len], text[0..copy_len]);
                pos += copy_len;
            }
        }
    }

    if (pos < console_buffer.len) {
        console_buffer[pos] = '\n';
        pos += 1;
    }

    printToStdout(console_buffer[0..pos]);
    return quickjs.jsUndefined();
}

/// console.error(...args) - Print to stderr
fn consoleError(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    var pos: usize = 0;

    for (0..@intCast(argc)) |i| {
        if (i > 0 and pos < console_buffer.len) {
            console_buffer[pos] = ' ';
            pos += 1;
        }

        var len: usize = undefined;
        const str = qjs.JS_ToCStringLen(ctx, &len, argv[i]);
        if (str != null) {
            defer qjs.JS_FreeCString(ctx, str);
            const text = str[0..len];
            const copy_len = @min(text.len, console_buffer.len - pos);
            if (copy_len > 0) {
                @memcpy(console_buffer[pos..][0..copy_len], text[0..copy_len]);
                pos += copy_len;
            }
        }
    }

    if (pos < console_buffer.len) {
        console_buffer[pos] = '\n';
        pos += 1;
    }

    printToStderr(console_buffer[0..pos]);
    return quickjs.jsUndefined();
}

/// console.warn(...args) - Alias for console.error
fn consoleWarn(ctx: ?*qjs.JSContext, this: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return consoleError(ctx, this, argc, argv);
}

/// console.info(...args) - Alias for console.log
fn consoleInfo(ctx: ?*qjs.JSContext, this: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return consoleLog(ctx, this, argc, argv);
}

/// console.debug(...args) - Alias for console.log
fn consoleDebug(ctx: ?*qjs.JSContext, this: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return consoleLog(ctx, this, argc, argv);
}

/// console.assert(condition, ...args) - Assert condition
fn consoleAssert(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsUndefined();

    const cond = qjs.JS_ToBool(ctx, argv[0]);
    if (cond == 1) return quickjs.jsUndefined(); // Condition is true, nothing to do

    // Condition is false, print assertion error
    const prefix = "[ASSERT] ";
    var pos: usize = 0;
    @memcpy(console_buffer[pos..][0..prefix.len], prefix);
    pos += prefix.len;

    // Print remaining arguments starting from index 1
    for (1..@intCast(argc)) |i| {
        if (i > 1 and pos < console_buffer.len) {
            console_buffer[pos] = ' ';
            pos += 1;
        }

        var len: usize = undefined;
        const str = qjs.JS_ToCStringLen(ctx, &len, argv[i]);
        if (str != null) {
            defer qjs.JS_FreeCString(ctx, str);
            const text = str[0..len];
            const copy_len = @min(text.len, console_buffer.len - pos);
            if (copy_len > 0) {
                @memcpy(console_buffer[pos..][0..copy_len], text[0..copy_len]);
                pos += copy_len;
            }
        }
    }

    // If no message provided, use default
    if (argc == 1) {
        const default_msg = "Assertion failed";
        const copy_len = @min(default_msg.len, console_buffer.len - pos);
        if (copy_len > 0) {
            @memcpy(console_buffer[pos..][0..copy_len], default_msg[0..copy_len]);
            pos += copy_len;
        }
    }

    if (pos < console_buffer.len) {
        console_buffer[pos] = '\n';
        pos += 1;
    }

    printToStderr(console_buffer[0..pos]);
    return quickjs.jsUndefined();
}

/// console.time(label) - Start a timer
fn consoleTime(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    // Get label (default: "default")
    const default_label = "default";
    var label: []const u8 = default_label;
    var label_len: usize = default_label.len;

    if (argc > 0) {
        const str = qjs.JS_ToCStringLen(ctx, &label_len, argv[0]);
        if (str != null) {
            label = str[0..label_len];
            defer qjs.JS_FreeCString(ctx, str);

            // Check if timer already exists
            if (findTimer(label) != null) {
                return quickjs.jsUndefined(); // Timer already running
            }

            // Add new timer if space available
            if (timer_count < MAX_TIMERS and label_len <= MAX_LABEL_LEN) {
                @memcpy(timer_labels[timer_count][0..label_len], label);
                timer_label_lens[timer_count] = label_len;
                timer_starts[timer_count] = std.time.nanoTimestamp();
                timer_count += 1;
            }
            return quickjs.jsUndefined();
        }
    }

    // Default label case
    if (findTimer(default_label) == null and timer_count < MAX_TIMERS) {
        @memcpy(timer_labels[timer_count][0..default_label.len], default_label);
        timer_label_lens[timer_count] = default_label.len;
        timer_starts[timer_count] = std.time.nanoTimestamp();
        timer_count += 1;
    }
    return quickjs.jsUndefined();
}

/// console.timeEnd(label) - End timer and print elapsed time
fn consoleTimeEnd(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const default_label = "default";
    var label: []const u8 = default_label;
    var label_len: usize = default_label.len;

    if (argc > 0) {
        const str = qjs.JS_ToCStringLen(ctx, &label_len, argv[0]);
        if (str != null) {
            label = str[0..label_len];
            defer qjs.JS_FreeCString(ctx, str);

            if (findTimer(label)) |idx| {
                const elapsed_ns = std.time.nanoTimestamp() - timer_starts[idx];
                const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;

                // Format output: "label: X.XXXms"
                var pos: usize = 0;
                @memcpy(console_buffer[pos..][0..label_len], label);
                pos += label_len;
                @memcpy(console_buffer[pos..][0..2], ": ");
                pos += 2;
                const ms_str = std.fmt.bufPrint(console_buffer[pos..], "{d:.3}ms\n", .{elapsed_ms}) catch "";
                pos += ms_str.len;
                printToStdout(console_buffer[0..pos]);

                // Remove timer by swapping with last
                if (idx < timer_count - 1) {
                    timer_labels[idx] = timer_labels[timer_count - 1];
                    timer_label_lens[idx] = timer_label_lens[timer_count - 1];
                    timer_starts[idx] = timer_starts[timer_count - 1];
                }
                timer_count -= 1;
            } else {
                // Timer not found
                var pos: usize = 0;
                const prefix = "Timer '";
                @memcpy(console_buffer[pos..][0..prefix.len], prefix);
                pos += prefix.len;
                @memcpy(console_buffer[pos..][0..label_len], label);
                pos += label_len;
                const suffix = "' does not exist\n";
                @memcpy(console_buffer[pos..][0..suffix.len], suffix);
                pos += suffix.len;
                printToStdout(console_buffer[0..pos]);
            }
            return quickjs.jsUndefined();
        }
    }

    // Default label case
    if (findTimer(default_label)) |idx| {
        const elapsed_ns = std.time.nanoTimestamp() - timer_starts[idx];
        const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
        var pos: usize = 0;
        @memcpy(console_buffer[pos..][0..default_label.len], default_label);
        pos += default_label.len;
        @memcpy(console_buffer[pos..][0..2], ": ");
        pos += 2;
        const ms_str = std.fmt.bufPrint(console_buffer[pos..], "{d:.3}ms\n", .{elapsed_ms}) catch "";
        pos += ms_str.len;
        printToStdout(console_buffer[0..pos]);

        if (idx < timer_count - 1) {
            timer_labels[idx] = timer_labels[timer_count - 1];
            timer_label_lens[idx] = timer_label_lens[timer_count - 1];
            timer_starts[idx] = timer_starts[timer_count - 1];
        }
        timer_count -= 1;
    } else {
        printToStdout("Timer 'default' does not exist\n");
    }
    return quickjs.jsUndefined();
}

/// console.timeLog(label, ...args) - Log elapsed time without ending timer
fn consoleTimeLog(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const default_label = "default";
    var label: []const u8 = default_label;
    var label_len: usize = default_label.len;
    var start_arg: c_int = 0;

    if (argc > 0) {
        const str = qjs.JS_ToCStringLen(ctx, &label_len, argv[0]);
        if (str != null) {
            label = str[0..label_len];
            start_arg = 1;

            if (findTimer(label)) |idx| {
                const elapsed_ns = std.time.nanoTimestamp() - timer_starts[idx];
                const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;

                var pos: usize = 0;
                const copy_len = @min(label_len, console_buffer.len - pos);
                @memcpy(console_buffer[pos..][0..copy_len], label[0..copy_len]);
                pos += copy_len;
                @memcpy(console_buffer[pos..][0..2], ": ");
                pos += 2;
                const ms_str = std.fmt.bufPrint(console_buffer[pos..], "{d:.3}ms", .{elapsed_ms}) catch "";
                pos += ms_str.len;

                // Append additional args
                for (@intCast(start_arg)..@intCast(argc)) |i| {
                    if (pos < console_buffer.len) {
                        console_buffer[pos] = ' ';
                        pos += 1;
                    }
                    var arg_len: usize = undefined;
                    const arg_str = qjs.JS_ToCStringLen(ctx, &arg_len, argv[i]);
                    if (arg_str != null) {
                        defer qjs.JS_FreeCString(ctx, arg_str);
                        const arg_copy_len = @min(arg_len, console_buffer.len - pos);
                        if (arg_copy_len > 0) {
                            @memcpy(console_buffer[pos..][0..arg_copy_len], arg_str[0..arg_copy_len]);
                            pos += arg_copy_len;
                        }
                    }
                }
                if (pos < console_buffer.len) {
                    console_buffer[pos] = '\n';
                    pos += 1;
                }
                printToStdout(console_buffer[0..pos]);
            } else {
                var pos: usize = 0;
                const prefix = "Timer '";
                @memcpy(console_buffer[pos..][0..prefix.len], prefix);
                pos += prefix.len;
                @memcpy(console_buffer[pos..][0..label_len], label);
                pos += label_len;
                const suffix = "' does not exist\n";
                @memcpy(console_buffer[pos..][0..suffix.len], suffix);
                pos += suffix.len;
                printToStdout(console_buffer[0..pos]);
            }
            qjs.JS_FreeCString(ctx, str);
            return quickjs.jsUndefined();
        }
    }

    // Default label
    if (findTimer(default_label)) |idx| {
        const elapsed_ns = std.time.nanoTimestamp() - timer_starts[idx];
        const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
        var pos: usize = 0;
        @memcpy(console_buffer[pos..][0..default_label.len], default_label);
        pos += default_label.len;
        @memcpy(console_buffer[pos..][0..2], ": ");
        pos += 2;
        const ms_str = std.fmt.bufPrint(console_buffer[pos..], "{d:.3}ms\n", .{elapsed_ms}) catch "";
        pos += ms_str.len;
        printToStdout(console_buffer[0..pos]);
    } else {
        printToStdout("Timer 'default' does not exist\n");
    }
    return quickjs.jsUndefined();
}

/// console.count(label) - Increment and print counter
fn consoleCount(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const default_label = "default";
    var label: []const u8 = default_label;
    var label_len: usize = default_label.len;

    if (argc > 0) {
        const str = qjs.JS_ToCStringLen(ctx, &label_len, argv[0]);
        if (str != null) {
            label = str[0..label_len];
            defer qjs.JS_FreeCString(ctx, str);

            const idx = findCounter(label) orelse blk: {
                // Add new counter
                if (counter_count < MAX_COUNTERS and label_len <= MAX_LABEL_LEN) {
                    @memcpy(counter_labels[counter_count][0..label_len], label);
                    counter_label_lens[counter_count] = label_len;
                    counter_values[counter_count] = 0;
                    const new_idx = counter_count;
                    counter_count += 1;
                    break :blk new_idx;
                }
                return quickjs.jsUndefined();
            };

            counter_values[idx] += 1;

            // Print "label: count"
            var pos: usize = 0;
            @memcpy(console_buffer[pos..][0..label_len], label);
            pos += label_len;
            @memcpy(console_buffer[pos..][0..2], ": ");
            pos += 2;
            const count_str = std.fmt.bufPrint(console_buffer[pos..], "{d}\n", .{counter_values[idx]}) catch "";
            pos += count_str.len;
            printToStdout(console_buffer[0..pos]);
            return quickjs.jsUndefined();
        }
    }

    // Default label
    const idx = findCounter(default_label) orelse blk: {
        if (counter_count < MAX_COUNTERS) {
            @memcpy(counter_labels[counter_count][0..default_label.len], default_label);
            counter_label_lens[counter_count] = default_label.len;
            counter_values[counter_count] = 0;
            const new_idx = counter_count;
            counter_count += 1;
            break :blk new_idx;
        }
        return quickjs.jsUndefined();
    };

    counter_values[idx] += 1;
    var pos: usize = 0;
    @memcpy(console_buffer[pos..][0..default_label.len], default_label);
    pos += default_label.len;
    @memcpy(console_buffer[pos..][0..2], ": ");
    pos += 2;
    const count_str = std.fmt.bufPrint(console_buffer[pos..], "{d}\n", .{counter_values[idx]}) catch "";
    pos += count_str.len;
    printToStdout(console_buffer[0..pos]);
    return quickjs.jsUndefined();
}

/// console.countReset(label) - Reset counter to 0
fn consoleCountReset(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const default_label = "default";
    var label: []const u8 = default_label;
    var label_len: usize = default_label.len;

    if (argc > 0) {
        const str = qjs.JS_ToCStringLen(ctx, &label_len, argv[0]);
        if (str != null) {
            label = str[0..label_len];
            defer qjs.JS_FreeCString(ctx, str);

            if (findCounter(label)) |idx| {
                counter_values[idx] = 0;
            }
            return quickjs.jsUndefined();
        }
    }

    // Default label
    if (findCounter(default_label)) |idx| {
        counter_values[idx] = 0;
    }
    return quickjs.jsUndefined();
}

/// Register all console functions to globalThis.console
/// Called ONCE at WASM initialization
pub fn register(ctx: *qjs.JSContext) void {
    const console_obj = qjs.JS_NewObject(ctx);

    // Register all functions at once - zero runtime cost
    inline for (.{
        .{ "log", consoleLog, -1 },
        .{ "error", consoleError, -1 },
        .{ "warn", consoleWarn, -1 },
        .{ "info", consoleInfo, -1 },
        .{ "debug", consoleDebug, -1 },
        .{ "assert", consoleAssert, -1 },
        .{ "time", consoleTime, 1 },
        .{ "timeEnd", consoleTimeEnd, 1 },
        .{ "timeLog", consoleTimeLog, -1 },
        .{ "count", consoleCount, 1 },
        .{ "countReset", consoleCountReset, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, console_obj, binding[0], func);
    }

    // Set as global.console
    const global = qjs.JS_GetGlobalObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, global, "console", console_obj);

    // Also register global print() function (used by runtime polyfill as fallback)
    const print_func = qjs.JS_NewCFunction(ctx, consoleLog, "print", -1);
    _ = qjs.JS_SetPropertyStr(ctx, global, "print", print_func);

    qjs.JS_FreeValue(ctx, global);
}
