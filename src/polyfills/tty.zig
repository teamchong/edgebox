/// Native tty module - QuickJS C functions
/// Provides TTY detection and terminal utilities
const std = @import("std");
const builtin = @import("builtin");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// POSIX isatty for native platforms
const posix = struct {
    extern fn isatty(fd: c_int) c_int;
};

// WASI platform detection
const is_wasi = builtin.os.tag == .wasi;

/// tty.isatty(fd) - Check if file descriptor is a TTY
fn ttyIsatty(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    var fd: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &fd, argv[0]) < 0) return quickjs.jsFalse();

    if (comptime is_wasi) {
        // On WASI, only stdin/stdout/stderr are considered TTYs
        // Real TTY detection would require WASI preview2
        const is_tty = (fd >= 0 and fd <= 2);
        return if (is_tty) quickjs.jsTrue() else quickjs.jsFalse();
    } else {
        // Native platforms use POSIX isatty
        const result = posix.isatty(fd);
        return if (result != 0) quickjs.jsTrue() else quickjs.jsFalse();
    }
}

/// Get terminal window size helper - returns (rows, cols) tuple
fn getTerminalSize() struct { rows: u16, cols: u16 } {
    var rows: u16 = 24; // Default
    var cols: u16 = 80; // Default

    if (comptime !is_wasi) {
        // Try to get actual terminal size on native platforms
        // Using POSIX ioctl with TIOCGWINSZ
        const Winsize = extern struct {
            ws_row: u16,
            ws_col: u16,
            ws_xpixel: u16,
            ws_ypixel: u16,
        };

        const TIOCGWINSZ: u32 = if (builtin.os.tag == .macos) 0x40087468 else 0x5413;

        const ioctl_fn = struct {
            extern fn ioctl(fd: c_int, request: c_ulong, ...) c_int;
        };

        var ws: Winsize = undefined;
        if (ioctl_fn.ioctl(1, TIOCGWINSZ, &ws) == 0) {
            if (ws.ws_row > 0) rows = ws.ws_row;
            if (ws.ws_col > 0) cols = ws.ws_col;
        }
    }

    return .{ .rows = rows, .cols = cols };
}

/// Get terminal window size (rows, columns) - returns [cols, rows] array
fn ttyGetWindowSize(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    _ = argc;
    _ = argv;

    const size = getTerminalSize();

    // Return [cols, rows] array (legacy format)
    const result = qjs.JS_NewArray(ctx);
    _ = qjs.JS_SetPropertyUint32(ctx, result, 0, qjs.JS_NewInt32(ctx, @intCast(size.cols)));
    _ = qjs.JS_SetPropertyUint32(ctx, result, 1, qjs.JS_NewInt32(ctx, @intCast(size.rows)));
    return result;
}

/// Get terminal window size - returns {rows, cols} object (test expects this format)
fn ttyGetTerminalSizeObject(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    _ = argc;
    _ = argv;

    const size = getTerminalSize();

    // Return {rows, cols} object
    const result = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, result, "rows", qjs.JS_NewInt32(ctx, @intCast(size.rows)));
    _ = qjs.JS_SetPropertyStr(ctx, result, "cols", qjs.JS_NewInt32(ctx, @intCast(size.cols)));
    return result;
}

/// ReadStream constructor - native implementation
fn readStreamConstructor(ctx: ?*qjs.JSContext, new_target: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    _ = new_target;

    const this = qjs.JS_NewObject(ctx);
    if (qjs.JS_IsException(this)) return this;

    // Set fd property (default 0 for stdin)
    var fd: i32 = 0;
    if (argc >= 1 and !qjs.JS_IsUndefined(argv[0])) {
        _ = qjs.JS_ToInt32(ctx, &fd, argv[0]);
    }
    _ = qjs.JS_SetPropertyStr(ctx, this, "fd", qjs.JS_NewInt32(ctx, fd));

    // Set isTTY based on fd
    if (comptime is_wasi) {
        const is_tty = (fd >= 0 and fd <= 2);
        _ = qjs.JS_SetPropertyStr(ctx, this, "isTTY", if (is_tty) quickjs.jsTrue() else quickjs.jsFalse());
    } else {
        const result = posix.isatty(fd);
        _ = qjs.JS_SetPropertyStr(ctx, this, "isTTY", if (result != 0) quickjs.jsTrue() else quickjs.jsFalse());
    }

    // Set isRaw property
    _ = qjs.JS_SetPropertyStr(ctx, this, "isRaw", quickjs.jsFalse());

    // Add setRawMode method
    _ = qjs.JS_SetPropertyStr(ctx, this, "setRawMode", qjs.JS_NewCFunction(ctx, readStreamSetRawMode, "setRawMode", 1));

    return this;
}

/// ReadStream.setRawMode method
fn readStreamSetRawMode(ctx: ?*qjs.JSContext, this_val: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    var mode: bool = false;
    if (argc >= 1) {
        mode = qjs.JS_ToBool(ctx, argv[0]) != 0;
    }
    _ = qjs.JS_SetPropertyStr(ctx, this_val, "isRaw", if (mode) quickjs.jsTrue() else quickjs.jsFalse());
    return qjs.JS_DupValue(ctx, this_val);
}

/// Create a ReadStream class for TTY input - native implementation without JS_Eval
fn createReadStreamClass(ctx: *qjs.JSContext) qjs.JSValue {
    // Create constructor function natively
    return qjs.JS_NewCFunction2(ctx, readStreamConstructor, "ReadStream", 1, qjs.JS_CFUNC_constructor, 0);
}

/// WriteStream constructor - native implementation
fn writeStreamConstructor(ctx: ?*qjs.JSContext, new_target: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    _ = new_target;

    const this = qjs.JS_NewObject(ctx);
    if (qjs.JS_IsException(this)) return this;

    // Set fd property (default 1 for stdout)
    var fd: i32 = 1;
    if (argc >= 1 and !qjs.JS_IsUndefined(argv[0])) {
        _ = qjs.JS_ToInt32(ctx, &fd, argv[0]);
    }
    _ = qjs.JS_SetPropertyStr(ctx, this, "fd", qjs.JS_NewInt32(ctx, fd));

    // Set isTTY based on fd
    if (comptime is_wasi) {
        const is_tty = (fd >= 0 and fd <= 2);
        _ = qjs.JS_SetPropertyStr(ctx, this, "isTTY", if (is_tty) quickjs.jsTrue() else quickjs.jsFalse());
    } else {
        const result = posix.isatty(fd);
        _ = qjs.JS_SetPropertyStr(ctx, this, "isTTY", if (result != 0) quickjs.jsTrue() else quickjs.jsFalse());
    }

    // Set terminal size
    const size = getTerminalSize();
    _ = qjs.JS_SetPropertyStr(ctx, this, "columns", qjs.JS_NewInt32(ctx, @intCast(size.cols)));
    _ = qjs.JS_SetPropertyStr(ctx, this, "rows", qjs.JS_NewInt32(ctx, @intCast(size.rows)));

    // Add methods
    _ = qjs.JS_SetPropertyStr(ctx, this, "getWindowSize", qjs.JS_NewCFunction(ctx, writeStreamGetWindowSize, "getWindowSize", 0));
    _ = qjs.JS_SetPropertyStr(ctx, this, "clearLine", qjs.JS_NewCFunction(ctx, writeStreamClearLine, "clearLine", 1));
    _ = qjs.JS_SetPropertyStr(ctx, this, "cursorTo", qjs.JS_NewCFunction(ctx, writeStreamCursorTo, "cursorTo", 2));
    _ = qjs.JS_SetPropertyStr(ctx, this, "moveCursor", qjs.JS_NewCFunction(ctx, writeStreamMoveCursor, "moveCursor", 2));

    return this;
}

/// WriteStream.getWindowSize method
fn writeStreamGetWindowSize(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const size = getTerminalSize();
    const result = qjs.JS_NewArray(ctx);
    _ = qjs.JS_SetPropertyUint32(ctx, result, 0, qjs.JS_NewInt32(ctx, @intCast(size.cols)));
    _ = qjs.JS_SetPropertyUint32(ctx, result, 1, qjs.JS_NewInt32(ctx, @intCast(size.rows)));
    return result;
}

/// WriteStream.clearLine method
fn writeStreamClearLine(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    var dir: i32 = 0;
    if (argc >= 1) {
        _ = qjs.JS_ToInt32(ctx, &dir, argv[0]);
    }

    // Write ANSI escape code based on direction
    const code = if (dir < 0) "\x1b[1K" else if (dir > 0) "\x1b[0K" else "\x1b[2K";
    _ = std.posix.write(1, code) catch {};
    return quickjs.jsUndefined();
}

/// WriteStream.cursorTo method
fn writeStreamCursorTo(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    var x: i32 = 0;
    if (argc >= 1) {
        _ = qjs.JS_ToInt32(ctx, &x, argv[0]);
    }

    var buf: [32]u8 = undefined;
    if (argc >= 2 and !qjs.JS_IsUndefined(argv[1])) {
        var y: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &y, argv[1]);
        // Move to absolute position (row, col)
        const len = std.fmt.bufPrint(&buf, "\x1b[{d};{d}H", .{ y + 1, x + 1 }) catch return quickjs.jsUndefined();
        _ = std.posix.write(1, len) catch {};
    } else {
        // Move to column
        const len = std.fmt.bufPrint(&buf, "\x1b[{d}G", .{x + 1}) catch return quickjs.jsUndefined();
        _ = std.posix.write(1, len) catch {};
    }
    return quickjs.jsUndefined();
}

/// WriteStream.moveCursor method
fn writeStreamMoveCursor(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    var dx: i32 = 0;
    var dy: i32 = 0;
    if (argc >= 1) {
        _ = qjs.JS_ToInt32(ctx, &dx, argv[0]);
    }
    if (argc >= 2) {
        _ = qjs.JS_ToInt32(ctx, &dy, argv[1]);
    }

    var buf: [32]u8 = undefined;

    // Horizontal movement
    if (dx > 0) {
        const len = std.fmt.bufPrint(&buf, "\x1b[{d}C", .{dx}) catch return quickjs.jsUndefined();
        _ = std.posix.write(1, len) catch {};
    } else if (dx < 0) {
        const len = std.fmt.bufPrint(&buf, "\x1b[{d}D", .{-dx}) catch return quickjs.jsUndefined();
        _ = std.posix.write(1, len) catch {};
    }

    // Vertical movement
    if (dy > 0) {
        const len = std.fmt.bufPrint(&buf, "\x1b[{d}B", .{dy}) catch return quickjs.jsUndefined();
        _ = std.posix.write(1, len) catch {};
    } else if (dy < 0) {
        const len = std.fmt.bufPrint(&buf, "\x1b[{d}A", .{-dy}) catch return quickjs.jsUndefined();
        _ = std.posix.write(1, len) catch {};
    }

    return quickjs.jsUndefined();
}

/// Create a WriteStream class for TTY output - native implementation without JS_Eval
fn createWriteStreamClass(ctx: *qjs.JSContext) qjs.JSValue {
    // Create constructor function natively
    return qjs.JS_NewCFunction2(ctx, writeStreamConstructor, "WriteStream", 1, qjs.JS_CFUNC_constructor, 0);
}

/// Register tty module
pub fn register(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Register helper functions on global (used by classes)
    _ = qjs.JS_SetPropertyStr(ctx, global, "_tty_isatty", qjs.JS_NewCFunction(ctx, ttyIsatty, "_tty_isatty", 1));
    _ = qjs.JS_SetPropertyStr(ctx, global, "_tty_getWindowSize", qjs.JS_NewCFunction(ctx, ttyGetWindowSize, "_tty_getWindowSize", 0));

    // Register __edgebox_* aliases (expected by tests)
    _ = qjs.JS_SetPropertyStr(ctx, global, "__edgebox_isatty", qjs.JS_NewCFunction(ctx, ttyIsatty, "__edgebox_isatty", 1));
    _ = qjs.JS_SetPropertyStr(ctx, global, "__edgebox_get_terminal_size", qjs.JS_NewCFunction(ctx, ttyGetTerminalSizeObject, "__edgebox_get_terminal_size", 0));

    // Create tty module object
    const tty_obj = qjs.JS_NewObject(ctx);

    // Add isatty function
    _ = qjs.JS_SetPropertyStr(ctx, tty_obj, "isatty", qjs.JS_NewCFunction(ctx, ttyIsatty, "isatty", 1));

    // Add ReadStream and WriteStream classes
    const read_stream_class = createReadStreamClass(ctx);
    if (!qjs.JS_IsException(read_stream_class)) {
        _ = qjs.JS_SetPropertyStr(ctx, tty_obj, "ReadStream", read_stream_class);
    }

    const write_stream_class = createWriteStreamClass(ctx);
    if (!qjs.JS_IsException(write_stream_class)) {
        _ = qjs.JS_SetPropertyStr(ctx, tty_obj, "WriteStream", write_stream_class);
    }

    // Set in _modules for require('tty')
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "tty", qjs.JS_DupValue(ctx, tty_obj));
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "node:tty", tty_obj);
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, tty_obj);
    }
}
