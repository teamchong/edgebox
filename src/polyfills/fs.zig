/// Native fs module - QuickJS C functions
/// Provides native filesystem functions that require system calls
const std = @import("std");
const builtin = @import("builtin");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// Static buffer for realpath (avoids runtime allocation)
var realpath_buf: [4096]u8 = undefined;

/// fs.realpathSync(path) - Resolve symlinks and return canonical path
fn fsRealpathSync(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "path required");
    }

    const path_str = qjs.JS_ToCString(ctx, argv[0]);
    if (path_str == null) {
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    }
    defer qjs.JS_FreeCString(ctx, path_str);

    // On WASI, just return the path as-is (no symlink resolution)
    if (builtin.os.tag == .wasi) {
        return qjs.JS_DupValue(ctx, argv[0]);
    }

    const path_slice = std.mem.span(path_str);

    // Use std.fs to resolve the path
    const resolved = std.fs.cwd().realpath(path_slice, &realpath_buf) catch {
        // If realpath fails, return the original path
        return qjs.JS_DupValue(ctx, argv[0]);
    };

    return qjs.JS_NewStringLen(ctx, resolved.ptr, resolved.len);
}

/// fs.realpathSync.native(path) - Same as realpathSync
fn fsRealpathSyncNative(ctx: ?*qjs.JSContext, this: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return fsRealpathSync(ctx, this, argc, argv);
}

/// fs.realpath(path, options, callback) - Async version
fn fsRealpath(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "path required");
    }

    // Find callback (last function argument)
    var callback_idx: i32 = -1;
    var i: usize = 1;
    while (i < @as(usize, @intCast(argc))) : (i += 1) {
        if (qjs.JS_IsFunction(ctx, argv[i])) {
            callback_idx = @intCast(i);
            break;
        }
    }

    // Get the resolved path synchronously
    const path_str = qjs.JS_ToCString(ctx, argv[0]);
    if (path_str == null) {
        if (callback_idx >= 0) {
            const err = qjs.JS_NewError(ctx);
            _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewString(ctx, "path must be a string"));
            var cb_args = [2]qjs.JSValue{ err, quickjs.jsNull() };
            const result = qjs.JS_Call(ctx, argv[@intCast(callback_idx)], quickjs.jsUndefined(), 2, &cb_args);
            qjs.JS_FreeValue(ctx, result);
            qjs.JS_FreeValue(ctx, err);
        }
        return quickjs.jsUndefined();
    }
    defer qjs.JS_FreeCString(ctx, path_str);

    // Resolve the path
    var resolved_path: []const u8 = std.mem.span(path_str);

    if (builtin.os.tag != .wasi) {
        const path_slice = std.mem.span(path_str);
        if (std.fs.cwd().realpath(path_slice, &realpath_buf)) |resolved| {
            resolved_path = resolved;
        } else |_| {
            // Keep original path on error
        }
    }

    // Call callback with result
    if (callback_idx >= 0) {
        const result_str = qjs.JS_NewStringLen(ctx, resolved_path.ptr, resolved_path.len);
        var cb_args = [2]qjs.JSValue{ quickjs.jsNull(), result_str };
        const result = qjs.JS_Call(ctx, argv[@intCast(callback_idx)], quickjs.jsUndefined(), 2, &cb_args);
        qjs.JS_FreeValue(ctx, result);
        qjs.JS_FreeValue(ctx, result_str);
    }

    return quickjs.jsUndefined();
}

// POSIX access constants
const F_OK: c_int = 0; // File exists
const R_OK: c_int = 4; // Read permission
const W_OK: c_int = 2; // Write permission
const X_OK: c_int = 1; // Execute permission

// POSIX open flags
const O_RDONLY: c_int = 0;
const O_WRONLY: c_int = 1;
const O_RDWR: c_int = 2;
const O_CREAT: c_int = if (builtin.os.tag == .macos) 0x0200 else 0x40;
const O_TRUNC: c_int = if (builtin.os.tag == .macos) 0x0400 else 0x200;
const O_APPEND: c_int = if (builtin.os.tag == .macos) 0x0008 else 0x400;
const O_EXCL: c_int = if (builtin.os.tag == .macos) 0x0800 else 0x80;

// POSIX lseek whence
const SEEK_SET: c_int = 0;
const SEEK_CUR: c_int = 1;
const SEEK_END: c_int = 2;

// POSIX functions
const posix = struct {
    extern fn access(path: [*:0]const u8, mode: c_int) c_int;
    extern fn open(path: [*:0]const u8, flags: c_int, mode: c_uint) c_int;
    extern fn close(fd: c_int) c_int;
    extern fn read(fd: c_int, buf: [*]u8, count: usize) isize;
    extern fn write(fd: c_int, buf: [*]const u8, count: usize) isize;
    extern fn lseek(fd: c_int, offset: i64, whence: c_int) i64;
    extern fn fsync(fd: c_int) c_int;
    extern fn ftruncate(fd: c_int, length: i64) c_int;
    extern fn fstat(fd: c_int, buf: *Stat) c_int;

    // Stat structure (platform-specific)
    const Stat = if (builtin.os.tag == .macos) extern struct {
        st_dev: i32,
        st_mode: u16,
        st_nlink: u16,
        st_ino: u64,
        st_uid: u32,
        st_gid: u32,
        st_rdev: i32,
        st_atimespec: Timespec,
        st_mtimespec: Timespec,
        st_ctimespec: Timespec,
        st_birthtimespec: Timespec,
        st_size: i64,
        st_blocks: i64,
        st_blksize: i32,
        st_flags: u32,
        st_gen: u32,
        st_lspare: i32,
        st_qspare: [2]i64,
    } else extern struct {
        st_dev: u64,
        st_ino: u64,
        st_nlink: u64,
        st_mode: u32,
        st_uid: u32,
        st_gid: u32,
        __pad0: u32,
        st_rdev: u64,
        st_size: i64,
        st_blksize: i64,
        st_blocks: i64,
        st_atim: Timespec,
        st_mtim: Timespec,
        st_ctim: Timespec,
        __unused: [3]i64,
    };

    const Timespec = extern struct {
        tv_sec: i64,
        tv_nsec: i64,
    };
};

/// fs.accessSync(path, mode) - Check file accessibility
/// mode: F_OK (0) = exists, R_OK (4) = readable, W_OK (2) = writable, X_OK (1) = executable
fn fsAccessSync(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "path required");
    }

    const path_str = qjs.JS_ToCString(ctx, argv[0]);
    if (path_str == null) {
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    }
    defer qjs.JS_FreeCString(ctx, path_str);

    // Get mode (default: F_OK)
    var mode: c_int = F_OK;
    if (argc >= 2 and !qjs.JS_IsUndefined(argv[1])) {
        var mode_val: i32 = 0;
        if (qjs.JS_ToInt32(ctx, &mode_val, argv[1]) >= 0) {
            mode = mode_val;
        }
    }

    // On WASI, use std.fs instead
    if (comptime builtin.os.tag == .wasi) {
        const path_slice = std.mem.span(path_str);
        _ = std.fs.cwd().statFile(path_slice) catch {
            // File doesn't exist or not accessible
            const err = qjs.JS_NewError(ctx);
            _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, "ENOENT"));
            _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewString(ctx, "no such file or directory"));
            _ = qjs.JS_SetPropertyStr(ctx, err, "path", qjs.JS_DupValue(ctx, argv[0]));
            return qjs.JS_Throw(ctx, err);
        };
        return quickjs.jsUndefined();
    }

    // Native POSIX access()
    const result = posix.access(path_str, mode);
    if (result != 0) {
        // Access denied or file not found
        const err = qjs.JS_NewError(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, if (mode == F_OK) "ENOENT" else "EACCES"));
        _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewString(ctx, if (mode == F_OK) "no such file or directory" else "permission denied"));
        _ = qjs.JS_SetPropertyStr(ctx, err, "path", qjs.JS_DupValue(ctx, argv[0]));
        return qjs.JS_Throw(ctx, err);
    }

    return quickjs.jsUndefined();
}

/// fs.access(path, mode, callback) - Async version
fn fsAccess(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "path required");
    }

    // Find callback (last function argument)
    var callback_idx: i32 = -1;
    var mode: c_int = F_OK;

    // Parse arguments: (path, mode?, callback)
    if (argc >= 2) {
        if (qjs.JS_IsFunction(ctx, argv[1])) {
            callback_idx = 1;
        } else if (!qjs.JS_IsUndefined(argv[1])) {
            var mode_val: i32 = 0;
            if (qjs.JS_ToInt32(ctx, &mode_val, argv[1]) >= 0) {
                mode = mode_val;
            }
        }
    }
    if (argc >= 3 and qjs.JS_IsFunction(ctx, argv[2])) {
        callback_idx = 2;
    }

    const path_str = qjs.JS_ToCString(ctx, argv[0]);
    if (path_str == null) {
        if (callback_idx >= 0) {
            const err = qjs.JS_NewError(ctx);
            _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewString(ctx, "path must be a string"));
            var cb_args = [1]qjs.JSValue{err};
            const result = qjs.JS_Call(ctx, argv[@intCast(callback_idx)], quickjs.jsUndefined(), 1, &cb_args);
            qjs.JS_FreeValue(ctx, result);
            qjs.JS_FreeValue(ctx, err);
        }
        return quickjs.jsUndefined();
    }
    defer qjs.JS_FreeCString(ctx, path_str);

    // Check access
    var access_ok = true;
    if (comptime builtin.os.tag == .wasi) {
        const path_slice = std.mem.span(path_str);
        _ = std.fs.cwd().statFile(path_slice) catch {
            access_ok = false;
        };
    } else {
        access_ok = (posix.access(path_str, mode) == 0);
    }

    // Call callback
    if (callback_idx >= 0) {
        if (access_ok) {
            var cb_args = [1]qjs.JSValue{quickjs.jsNull()};
            const result = qjs.JS_Call(ctx, argv[@intCast(callback_idx)], quickjs.jsUndefined(), 1, &cb_args);
            qjs.JS_FreeValue(ctx, result);
        } else {
            const err = qjs.JS_NewError(ctx);
            _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, if (mode == F_OK) "ENOENT" else "EACCES"));
            _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewString(ctx, if (mode == F_OK) "no such file or directory" else "permission denied"));
            var cb_args = [1]qjs.JSValue{err};
            const result = qjs.JS_Call(ctx, argv[@intCast(callback_idx)], quickjs.jsUndefined(), 1, &cb_args);
            qjs.JS_FreeValue(ctx, result);
            qjs.JS_FreeValue(ctx, err);
        }
    }

    return quickjs.jsUndefined();
}

// ============ File Descriptor Operations ============

/// Parse Node.js flags string to POSIX flags
fn parseFlags(flags_str: []const u8) c_int {
    if (std.mem.eql(u8, flags_str, "r")) return O_RDONLY;
    if (std.mem.eql(u8, flags_str, "r+")) return O_RDWR;
    if (std.mem.eql(u8, flags_str, "rs+") or std.mem.eql(u8, flags_str, "sr+")) return O_RDWR;
    if (std.mem.eql(u8, flags_str, "w")) return O_WRONLY | O_CREAT | O_TRUNC;
    if (std.mem.eql(u8, flags_str, "wx") or std.mem.eql(u8, flags_str, "xw")) return O_WRONLY | O_CREAT | O_TRUNC | O_EXCL;
    if (std.mem.eql(u8, flags_str, "w+")) return O_RDWR | O_CREAT | O_TRUNC;
    if (std.mem.eql(u8, flags_str, "wx+") or std.mem.eql(u8, flags_str, "xw+")) return O_RDWR | O_CREAT | O_TRUNC | O_EXCL;
    if (std.mem.eql(u8, flags_str, "a")) return O_WRONLY | O_CREAT | O_APPEND;
    if (std.mem.eql(u8, flags_str, "ax") or std.mem.eql(u8, flags_str, "xa")) return O_WRONLY | O_CREAT | O_APPEND | O_EXCL;
    if (std.mem.eql(u8, flags_str, "a+")) return O_RDWR | O_CREAT | O_APPEND;
    if (std.mem.eql(u8, flags_str, "ax+") or std.mem.eql(u8, flags_str, "xa+")) return O_RDWR | O_CREAT | O_APPEND | O_EXCL;
    return O_RDONLY; // Default
}

/// fs.openSync(path, flags, mode) - Open file and return file descriptor
fn fsOpenSync(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "path required");
    }

    const path_str = qjs.JS_ToCString(ctx, argv[0]);
    if (path_str == null) {
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    }
    defer qjs.JS_FreeCString(ctx, path_str);

    // Parse flags (string or number)
    var flags: c_int = O_RDONLY;
    if (argc >= 2 and !qjs.JS_IsUndefined(argv[1])) {
        if (qjs.JS_IsString(argv[1])) {
            const flags_cstr = qjs.JS_ToCString(ctx, argv[1]);
            if (flags_cstr != null) {
                defer qjs.JS_FreeCString(ctx, flags_cstr);
                flags = parseFlags(std.mem.span(flags_cstr));
            }
        } else {
            var flags_val: i32 = 0;
            if (qjs.JS_ToInt32(ctx, &flags_val, argv[1]) >= 0) {
                flags = flags_val;
            }
        }
    }

    // Parse mode (default 0o666)
    var mode: c_uint = 0o666;
    if (argc >= 3 and !qjs.JS_IsUndefined(argv[2])) {
        var mode_val: i32 = 0;
        if (qjs.JS_ToInt32(ctx, &mode_val, argv[2]) >= 0) {
            mode = @intCast(mode_val);
        }
    }

    // On WASI, use std.fs
    if (comptime builtin.os.tag == .wasi) {
        return qjs.JS_ThrowTypeError(ctx, "fs.openSync not supported on WASI");
    }

    const fd = posix.open(path_str, flags, mode);
    if (fd < 0) {
        const err = qjs.JS_NewError(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, "ENOENT"));
        _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewString(ctx, "failed to open file"));
        _ = qjs.JS_SetPropertyStr(ctx, err, "path", qjs.JS_DupValue(ctx, argv[0]));
        return qjs.JS_Throw(ctx, err);
    }

    return qjs.JS_NewInt32(ctx, fd);
}

/// fs.closeSync(fd) - Close file descriptor
fn fsCloseSync(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "fd required");
    }

    var fd: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &fd, argv[0]) < 0) {
        return qjs.JS_ThrowTypeError(ctx, "fd must be a number");
    }

    if (comptime builtin.os.tag == .wasi) {
        return qjs.JS_ThrowTypeError(ctx, "fs.closeSync not supported on WASI");
    }

    const result = posix.close(fd);
    if (result < 0) {
        const err = qjs.JS_NewError(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, "EBADF"));
        _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewString(ctx, "bad file descriptor"));
        return qjs.JS_Throw(ctx, err);
    }

    return quickjs.jsUndefined();
}

// Static buffer for read operations
var fd_read_buffer: [65536]u8 = undefined;

/// fs.readSync(fd, buffer, offset, length, position) - Read from file descriptor
fn fsReadSync(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) {
        return qjs.JS_ThrowTypeError(ctx, "fd and buffer required");
    }

    var fd: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &fd, argv[0]) < 0) {
        return qjs.JS_ThrowTypeError(ctx, "fd must be a number");
    }

    // Get buffer to read into
    var buf_size: usize = 0;
    const buf_ptr = qjs.JS_GetArrayBuffer(ctx, &buf_size, argv[1]);
    if (buf_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "buffer must be ArrayBuffer or TypedArray");
    }
    const buffer = @as([*]u8, @ptrCast(buf_ptr))[0..buf_size];

    // Get offset (default 0)
    var offset: usize = 0;
    if (argc >= 3 and !qjs.JS_IsUndefined(argv[2]) and !qjs.JS_IsNull(argv[2])) {
        var off_val: i32 = 0;
        if (qjs.JS_ToInt32(ctx, &off_val, argv[2]) >= 0) {
            offset = @intCast(off_val);
        }
    }

    // Get length (default buffer.length - offset)
    var length: usize = buf_size - offset;
    if (argc >= 4 and !qjs.JS_IsUndefined(argv[3]) and !qjs.JS_IsNull(argv[3])) {
        var len_val: i32 = 0;
        if (qjs.JS_ToInt32(ctx, &len_val, argv[3]) >= 0) {
            length = @intCast(len_val);
        }
    }

    // Get position (null = current position)
    var has_position = false;
    var position: i64 = 0;
    if (argc >= 5 and !qjs.JS_IsUndefined(argv[4]) and !qjs.JS_IsNull(argv[4])) {
        if (qjs.JS_ToInt64(ctx, &position, argv[4]) >= 0) {
            has_position = true;
        }
    }

    if (offset + length > buf_size) {
        return qjs.JS_ThrowRangeError(ctx, "offset + length exceeds buffer size");
    }

    if (comptime builtin.os.tag == .wasi) {
        return qjs.JS_ThrowTypeError(ctx, "fs.readSync not supported on WASI");
    }

    // Seek if position specified
    if (has_position) {
        _ = posix.lseek(fd, position, SEEK_SET);
    }

    const bytes_read = posix.read(fd, buffer[offset..].ptr, length);
    if (bytes_read < 0) {
        const err = qjs.JS_NewError(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, "EIO"));
        _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewString(ctx, "read error"));
        return qjs.JS_Throw(ctx, err);
    }

    return qjs.JS_NewInt32(ctx, @intCast(bytes_read));
}

/// fs.writeSync(fd, buffer, offset, length, position) - Write to file descriptor
fn fsWriteSync(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) {
        return qjs.JS_ThrowTypeError(ctx, "fd and buffer required");
    }

    var fd: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &fd, argv[0]) < 0) {
        return qjs.JS_ThrowTypeError(ctx, "fd must be a number");
    }

    // Get buffer to write from (can be ArrayBuffer or string)
    var data: []const u8 = undefined;
    var buf_size: usize = 0;
    const buf_ptr = qjs.JS_GetArrayBuffer(ctx, &buf_size, argv[1]);
    if (buf_ptr != null) {
        data = @as([*]const u8, @ptrCast(buf_ptr))[0..buf_size];
    } else if (qjs.JS_IsString(argv[1])) {
        const str = qjs.JS_ToCString(ctx, argv[1]);
        if (str == null) {
            return qjs.JS_ThrowTypeError(ctx, "invalid string");
        }
        defer qjs.JS_FreeCString(ctx, str);
        data = std.mem.span(str);
        buf_size = data.len;
    } else {
        return qjs.JS_ThrowTypeError(ctx, "buffer must be ArrayBuffer, TypedArray, or string");
    }

    // Get offset (default 0)
    var offset: usize = 0;
    if (argc >= 3 and !qjs.JS_IsUndefined(argv[2]) and !qjs.JS_IsNull(argv[2])) {
        var off_val: i32 = 0;
        if (qjs.JS_ToInt32(ctx, &off_val, argv[2]) >= 0) {
            offset = @intCast(off_val);
        }
    }

    // Get length (default buffer.length - offset)
    var length: usize = buf_size - offset;
    if (argc >= 4 and !qjs.JS_IsUndefined(argv[3]) and !qjs.JS_IsNull(argv[3])) {
        var len_val: i32 = 0;
        if (qjs.JS_ToInt32(ctx, &len_val, argv[3]) >= 0) {
            length = @intCast(len_val);
        }
    }

    // Get position (null = current position)
    var has_position = false;
    var position: i64 = 0;
    if (argc >= 5 and !qjs.JS_IsUndefined(argv[4]) and !qjs.JS_IsNull(argv[4])) {
        if (qjs.JS_ToInt64(ctx, &position, argv[4]) >= 0) {
            has_position = true;
        }
    }

    if (offset + length > buf_size) {
        return qjs.JS_ThrowRangeError(ctx, "offset + length exceeds buffer size");
    }

    if (comptime builtin.os.tag == .wasi) {
        return qjs.JS_ThrowTypeError(ctx, "fs.writeSync not supported on WASI");
    }

    // Seek if position specified
    if (has_position) {
        _ = posix.lseek(fd, position, SEEK_SET);
    }

    const bytes_written = posix.write(fd, data[offset..].ptr, length);
    if (bytes_written < 0) {
        const err = qjs.JS_NewError(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, "EIO"));
        _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewString(ctx, "write error"));
        return qjs.JS_Throw(ctx, err);
    }

    return qjs.JS_NewInt32(ctx, @intCast(bytes_written));
}

/// fs.fstatSync(fd) - Get file stats from file descriptor
fn fsFstatSync(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "fd required");
    }

    var fd: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &fd, argv[0]) < 0) {
        return qjs.JS_ThrowTypeError(ctx, "fd must be a number");
    }

    if (comptime builtin.os.tag == .wasi) {
        return qjs.JS_ThrowTypeError(ctx, "fs.fstatSync not supported on WASI");
    }

    var stat: posix.Stat = undefined;
    const result = posix.fstat(fd, &stat);
    if (result < 0) {
        const err = qjs.JS_NewError(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, "EBADF"));
        _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewString(ctx, "bad file descriptor"));
        return qjs.JS_Throw(ctx, err);
    }

    // Create Stats object
    const stats_obj = qjs.JS_NewObject(ctx);
    if (qjs.JS_IsException(stats_obj)) return stats_obj;

    // Add properties (platform-specific field access)
    if (comptime builtin.os.tag == .macos) {
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "dev", qjs.JS_NewInt64(ctx, stat.st_dev));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "ino", qjs.JS_NewInt64(ctx, @intCast(stat.st_ino)));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "mode", qjs.JS_NewInt32(ctx, stat.st_mode));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "nlink", qjs.JS_NewInt32(ctx, stat.st_nlink));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "uid", qjs.JS_NewInt32(ctx, @intCast(stat.st_uid)));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "gid", qjs.JS_NewInt32(ctx, @intCast(stat.st_gid)));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "rdev", qjs.JS_NewInt64(ctx, stat.st_rdev));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "size", qjs.JS_NewInt64(ctx, stat.st_size));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "blksize", qjs.JS_NewInt32(ctx, stat.st_blksize));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "blocks", qjs.JS_NewInt64(ctx, stat.st_blocks));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "atimeMs", qjs.JS_NewFloat64(ctx, @as(f64, @floatFromInt(stat.st_atimespec.tv_sec)) * 1000.0 + @as(f64, @floatFromInt(stat.st_atimespec.tv_nsec)) / 1000000.0));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "mtimeMs", qjs.JS_NewFloat64(ctx, @as(f64, @floatFromInt(stat.st_mtimespec.tv_sec)) * 1000.0 + @as(f64, @floatFromInt(stat.st_mtimespec.tv_nsec)) / 1000000.0));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "ctimeMs", qjs.JS_NewFloat64(ctx, @as(f64, @floatFromInt(stat.st_ctimespec.tv_sec)) * 1000.0 + @as(f64, @floatFromInt(stat.st_ctimespec.tv_nsec)) / 1000000.0));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "birthtimeMs", qjs.JS_NewFloat64(ctx, @as(f64, @floatFromInt(stat.st_birthtimespec.tv_sec)) * 1000.0 + @as(f64, @floatFromInt(stat.st_birthtimespec.tv_nsec)) / 1000000.0));
    } else {
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "dev", qjs.JS_NewInt64(ctx, @intCast(stat.st_dev)));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "ino", qjs.JS_NewInt64(ctx, @intCast(stat.st_ino)));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "mode", qjs.JS_NewInt32(ctx, @intCast(stat.st_mode)));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "nlink", qjs.JS_NewInt64(ctx, @intCast(stat.st_nlink)));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "uid", qjs.JS_NewInt32(ctx, @intCast(stat.st_uid)));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "gid", qjs.JS_NewInt32(ctx, @intCast(stat.st_gid)));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "rdev", qjs.JS_NewInt64(ctx, @intCast(stat.st_rdev)));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "size", qjs.JS_NewInt64(ctx, stat.st_size));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "blksize", qjs.JS_NewInt64(ctx, stat.st_blksize));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "blocks", qjs.JS_NewInt64(ctx, stat.st_blocks));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "atimeMs", qjs.JS_NewFloat64(ctx, @as(f64, @floatFromInt(stat.st_atim.tv_sec)) * 1000.0 + @as(f64, @floatFromInt(stat.st_atim.tv_nsec)) / 1000000.0));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "mtimeMs", qjs.JS_NewFloat64(ctx, @as(f64, @floatFromInt(stat.st_mtim.tv_sec)) * 1000.0 + @as(f64, @floatFromInt(stat.st_mtim.tv_nsec)) / 1000000.0));
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "ctimeMs", qjs.JS_NewFloat64(ctx, @as(f64, @floatFromInt(stat.st_ctim.tv_sec)) * 1000.0 + @as(f64, @floatFromInt(stat.st_ctim.tv_nsec)) / 1000000.0));
    }

    // Add isFile/isDirectory helper methods
    const mode_val = if (comptime builtin.os.tag == .macos) stat.st_mode else @as(u16, @truncate(stat.st_mode));
    const is_file = (mode_val & 0o170000) == 0o100000;
    const is_directory = (mode_val & 0o170000) == 0o040000;
    const is_symlink = (mode_val & 0o170000) == 0o120000;

    // Add isFile, isDirectory, isSymbolicLink as methods returning functions
    const file_code =
        \\(function(val) { return function() { return val; }; })
    ;
    const file_factory = qjs.JS_Eval(ctx, file_code.ptr, file_code.len, "<fs>", qjs.JS_EVAL_TYPE_GLOBAL);
    if (!qjs.JS_IsException(file_factory)) {
        var args1 = [1]qjs.JSValue{if (is_file) quickjs.jsTrue() else quickjs.jsFalse()};
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "isFile", qjs.JS_Call(ctx, file_factory, quickjs.jsUndefined(), 1, &args1));
        var args2 = [1]qjs.JSValue{if (is_directory) quickjs.jsTrue() else quickjs.jsFalse()};
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "isDirectory", qjs.JS_Call(ctx, file_factory, quickjs.jsUndefined(), 1, &args2));
        var args3 = [1]qjs.JSValue{if (is_symlink) quickjs.jsTrue() else quickjs.jsFalse()};
        _ = qjs.JS_SetPropertyStr(ctx, stats_obj, "isSymbolicLink", qjs.JS_Call(ctx, file_factory, quickjs.jsUndefined(), 1, &args3));
        qjs.JS_FreeValue(ctx, file_factory);
    }

    return stats_obj;
}

/// fs.fsyncSync(fd) - Flush file to disk
fn fsFsyncSync(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "fd required");
    }

    var fd: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &fd, argv[0]) < 0) {
        return qjs.JS_ThrowTypeError(ctx, "fd must be a number");
    }

    if (comptime builtin.os.tag == .wasi) {
        return qjs.JS_ThrowTypeError(ctx, "fs.fsyncSync not supported on WASI");
    }

    const result = posix.fsync(fd);
    if (result < 0) {
        const err = qjs.JS_NewError(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, "EIO"));
        _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewString(ctx, "fsync failed"));
        return qjs.JS_Throw(ctx, err);
    }

    return quickjs.jsUndefined();
}

/// fs.ftruncateSync(fd, len) - Truncate file to specified length
fn fsFtruncateSync(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "fd required");
    }

    var fd: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &fd, argv[0]) < 0) {
        return qjs.JS_ThrowTypeError(ctx, "fd must be a number");
    }

    var len: i64 = 0;
    if (argc >= 2 and !qjs.JS_IsUndefined(argv[1])) {
        _ = qjs.JS_ToInt64(ctx, &len, argv[1]);
    }

    if (comptime builtin.os.tag == .wasi) {
        return qjs.JS_ThrowTypeError(ctx, "fs.ftruncateSync not supported on WASI");
    }

    const result = posix.ftruncate(fd, len);
    if (result < 0) {
        const err = qjs.JS_NewError(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, err, "code", qjs.JS_NewString(ctx, "EIO"));
        _ = qjs.JS_SetPropertyStr(ctx, err, "message", qjs.JS_NewString(ctx, "ftruncate failed"));
        return qjs.JS_Throw(ctx, err);
    }

    return quickjs.jsUndefined();
}

/// Register fs native functions
/// This enhances the existing JS fs module with native implementations
pub fn register(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Get _modules
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (qjs.JS_IsUndefined(modules_val)) {
        return;
    }
    defer qjs.JS_FreeValue(ctx, modules_val);

    // FIRST: Register _nativeFs on _modules (this is needed by JS polyfills)
    // This must happen before the JS polyfills run so they can access native functions
    const native_fs = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, native_fs, "openSync", qjs.JS_NewCFunction(ctx, fsOpenSync, "openSync", 3));
    _ = qjs.JS_SetPropertyStr(ctx, native_fs, "closeSync", qjs.JS_NewCFunction(ctx, fsCloseSync, "closeSync", 1));
    _ = qjs.JS_SetPropertyStr(ctx, native_fs, "readSync", qjs.JS_NewCFunction(ctx, fsReadSync, "readSync", 5));
    _ = qjs.JS_SetPropertyStr(ctx, native_fs, "writeSync", qjs.JS_NewCFunction(ctx, fsWriteSync, "writeSync", 5));
    _ = qjs.JS_SetPropertyStr(ctx, native_fs, "fstatSync", qjs.JS_NewCFunction(ctx, fsFstatSync, "fstatSync", 1));
    _ = qjs.JS_SetPropertyStr(ctx, native_fs, "fsyncSync", qjs.JS_NewCFunction(ctx, fsFsyncSync, "fsyncSync", 1));
    _ = qjs.JS_SetPropertyStr(ctx, native_fs, "ftruncateSync", qjs.JS_NewCFunction(ctx, fsFtruncateSync, "ftruncateSync", 2));
    _ = qjs.JS_SetPropertyStr(ctx, modules_val, "_nativeFs", native_fs);

    // Then try to enhance _modules.fs if it exists
    const fs_mod = qjs.JS_GetPropertyStr(ctx, modules_val, "fs");
    if (qjs.JS_IsUndefined(fs_mod) or qjs.JS_IsNull(fs_mod)) {
        qjs.JS_FreeValue(ctx, fs_mod);
        return;
    }
    defer qjs.JS_FreeValue(ctx, fs_mod);

    // Create realpathSync function with native sub-property
    const realpath_sync_func = qjs.JS_NewCFunction(ctx, fsRealpathSync, "realpathSync", 1);
    _ = qjs.JS_SetPropertyStr(ctx, realpath_sync_func, "native", qjs.JS_NewCFunction(ctx, fsRealpathSyncNative, "native", 1));
    _ = qjs.JS_SetPropertyStr(ctx, fs_mod, "realpathSync", realpath_sync_func);

    // Create realpath function
    _ = qjs.JS_SetPropertyStr(ctx, fs_mod, "realpath", qjs.JS_NewCFunction(ctx, fsRealpath, "realpath", 3));

    // Also update fs.promises.realpath if it exists
    const promises = qjs.JS_GetPropertyStr(ctx, fs_mod, "promises");
    if (!qjs.JS_IsUndefined(promises) and !qjs.JS_IsNull(promises)) {
        // Create promise-based realpath using JS wrapper
        const wrapper_code =
            \\(function(syncFn) {
            \\    return function realpath(path, options) {
            \\        return new Promise(function(resolve, reject) {
            \\            try {
            \\                resolve(syncFn(path, options));
            \\            } catch (e) {
            \\                reject(e);
            \\            }
            \\        });
            \\    };
            \\})
        ;

        const factory = qjs.JS_Eval(ctx, wrapper_code.ptr, wrapper_code.len, "<fs>", qjs.JS_EVAL_TYPE_GLOBAL);
        if (!qjs.JS_IsException(factory)) {
            var factory_args = [1]qjs.JSValue{realpath_sync_func};
            const promise_realpath = qjs.JS_Call(ctx, factory, quickjs.jsUndefined(), 1, &factory_args);
            if (!qjs.JS_IsException(promise_realpath)) {
                _ = qjs.JS_SetPropertyStr(ctx, promises, "realpath", promise_realpath);
            } else {
                qjs.JS_FreeValue(ctx, promise_realpath);
            }
        }
        qjs.JS_FreeValue(ctx, factory);
    }
    qjs.JS_FreeValue(ctx, promises);

    // Register accessSync and access functions
    _ = qjs.JS_SetPropertyStr(ctx, fs_mod, "accessSync", qjs.JS_NewCFunction(ctx, fsAccessSync, "accessSync", 2));
    _ = qjs.JS_SetPropertyStr(ctx, fs_mod, "access", qjs.JS_NewCFunction(ctx, fsAccess, "access", 3));

    // Register file descriptor operations on fs module directly
    _ = qjs.JS_SetPropertyStr(ctx, fs_mod, "openSync", qjs.JS_NewCFunction(ctx, fsOpenSync, "openSync", 3));
    _ = qjs.JS_SetPropertyStr(ctx, fs_mod, "closeSync", qjs.JS_NewCFunction(ctx, fsCloseSync, "closeSync", 1));
    _ = qjs.JS_SetPropertyStr(ctx, fs_mod, "readSync", qjs.JS_NewCFunction(ctx, fsReadSync, "readSync", 5));
    _ = qjs.JS_SetPropertyStr(ctx, fs_mod, "writeSync", qjs.JS_NewCFunction(ctx, fsWriteSync, "writeSync", 5));
    _ = qjs.JS_SetPropertyStr(ctx, fs_mod, "fstatSync", qjs.JS_NewCFunction(ctx, fsFstatSync, "fstatSync", 1));
    _ = qjs.JS_SetPropertyStr(ctx, fs_mod, "fsyncSync", qjs.JS_NewCFunction(ctx, fsFsyncSync, "fsyncSync", 1));
    _ = qjs.JS_SetPropertyStr(ctx, fs_mod, "ftruncateSync", qjs.JS_NewCFunction(ctx, fsFtruncateSync, "ftruncateSync", 2));

    // Create fs.constants object
    const constants = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, constants, "F_OK", qjs.JS_NewInt32(ctx, F_OK));
    _ = qjs.JS_SetPropertyStr(ctx, constants, "R_OK", qjs.JS_NewInt32(ctx, R_OK));
    _ = qjs.JS_SetPropertyStr(ctx, constants, "W_OK", qjs.JS_NewInt32(ctx, W_OK));
    _ = qjs.JS_SetPropertyStr(ctx, constants, "X_OK", qjs.JS_NewInt32(ctx, X_OK));
    _ = qjs.JS_SetPropertyStr(ctx, fs_mod, "constants", constants);
}
