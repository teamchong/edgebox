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

/// Get terminal window size (rows, columns)
fn ttyGetWindowSize(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    _ = argc;
    _ = argv;

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

    // Return [rows, cols] array
    const result = qjs.JS_NewArray(ctx);
    _ = qjs.JS_SetPropertyUint32(ctx, result, 0, qjs.JS_NewInt32(ctx, @intCast(cols)));
    _ = qjs.JS_SetPropertyUint32(ctx, result, 1, qjs.JS_NewInt32(ctx, @intCast(rows)));
    return result;
}

/// Create a ReadStream class for TTY input
fn createReadStreamClass(ctx: *qjs.JSContext) qjs.JSValue {
    // Create a simple ReadStream class
    const class_code =
        \\(function() {
        \\    class ReadStream {
        \\        constructor(fd) {
        \\            this.fd = fd || 0;
        \\            this.isTTY = globalThis._tty_isatty(this.fd);
        \\            this.isRaw = false;
        \\        }
        \\        setRawMode(mode) {
        \\            this.isRaw = !!mode;
        \\            return this;
        \\        }
        \\    }
        \\    return ReadStream;
        \\})()
    ;

    const result = qjs.JS_Eval(ctx, class_code.ptr, class_code.len, "<tty>", qjs.JS_EVAL_TYPE_GLOBAL);
    return result;
}

/// Create a WriteStream class for TTY output
fn createWriteStreamClass(ctx: *qjs.JSContext) qjs.JSValue {
    // Create a simple WriteStream class
    const class_code =
        \\(function() {
        \\    class WriteStream {
        \\        constructor(fd) {
        \\            this.fd = fd || 1;
        \\            this.isTTY = globalThis._tty_isatty(this.fd);
        \\            this.columns = 80;
        \\            this.rows = 24;
        \\            var size = globalThis._tty_getWindowSize();
        \\            if (size) {
        \\                this.columns = size[0] || 80;
        \\                this.rows = size[1] || 24;
        \\            }
        \\        }
        \\        getWindowSize() {
        \\            return globalThis._tty_getWindowSize() || [this.columns, this.rows];
        \\        }
        \\        clearLine(dir) {
        \\            var code = dir < 0 ? '\x1b[1K' : dir > 0 ? '\x1b[0K' : '\x1b[2K';
        \\            process.stdout.write(code);
        \\        }
        \\        cursorTo(x, y) {
        \\            if (y === undefined) {
        \\                process.stdout.write('\x1b[' + (x + 1) + 'G');
        \\            } else {
        \\                process.stdout.write('\x1b[' + (y + 1) + ';' + (x + 1) + 'H');
        \\            }
        \\        }
        \\        moveCursor(dx, dy) {
        \\            if (dx > 0) process.stdout.write('\x1b[' + dx + 'C');
        \\            else if (dx < 0) process.stdout.write('\x1b[' + (-dx) + 'D');
        \\            if (dy > 0) process.stdout.write('\x1b[' + dy + 'B');
        \\            else if (dy < 0) process.stdout.write('\x1b[' + (-dy) + 'A');
        \\        }
        \\    }
        \\    return WriteStream;
        \\})()
    ;

    const result = qjs.JS_Eval(ctx, class_code.ptr, class_code.len, "<tty>", qjs.JS_EVAL_TYPE_GLOBAL);
    return result;
}

/// Register tty module
pub fn register(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Register helper functions on global (used by classes)
    _ = qjs.JS_SetPropertyStr(ctx, global, "_tty_isatty", qjs.JS_NewCFunction(ctx, ttyIsatty, "_tty_isatty", 1));
    _ = qjs.JS_SetPropertyStr(ctx, global, "_tty_getWindowSize", qjs.JS_NewCFunction(ctx, ttyGetWindowSize, "_tty_getWindowSize", 0));

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
