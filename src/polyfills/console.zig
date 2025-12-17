/// Native console module - QuickJS C functions
/// Registered ONCE at WASM init via inline for loop
/// Zero runtime overhead, WASI-based output
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

/// Helper to print to stdout via WASI
fn printToStdout(text: []const u8) void {
    var nwritten: usize = undefined;
    const iov = [_]std.os.wasi.ciovec_t{.{ .base = text.ptr, .len = text.len }};
    _ = std.os.wasi.fd_write(1, &iov, 1, &nwritten);
}

/// Helper to print to stderr via WASI
fn printToStderr(text: []const u8) void {
    var nwritten: usize = undefined;
    const iov = [_]std.os.wasi.ciovec_t{.{ .base = text.ptr, .len = text.len }};
    _ = std.os.wasi.fd_write(2, &iov, 1, &nwritten);
}

/// Buffer for formatting console output
var console_buffer: [8192]u8 = undefined;

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
    return qjs.JS_UNDEFINED;
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
    return qjs.JS_UNDEFINED;
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
    if (argc < 1) return qjs.JS_UNDEFINED;

    const cond = qjs.JS_ToBool(ctx, argv[0]);
    if (cond == 1) return qjs.JS_UNDEFINED; // Condition is true, nothing to do

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
    return qjs.JS_UNDEFINED;
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
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, console_obj, binding[0], func);
    }

    // Set as global.console
    const global = qjs.JS_GetGlobalObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, global, "console", console_obj);
    qjs.JS_FreeValue(ctx, global);
}
