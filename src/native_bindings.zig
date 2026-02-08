/// Native Bindings for EdgeBox
///
/// Implements native functions exposed to JavaScript via QuickJS.
/// These functions bridge JS calls to WASI syscalls for fs, crypto, etc.
///
/// Strategy:
/// - JS polyfills in node_compat.zig call these __edgebox_* functions
/// - These functions are registered as C functions in QuickJS
/// - They use WASI syscalls internally for actual operations
const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

// QuickJS C API
const qjs = @cImport({
    @cDefine("CONFIG_VERSION", "\"2024-02-14\"");
    @cDefine("CONFIG_BIGNUM", "1");
    @cInclude("quickjs.h");
});

// Type definitions
const JSContext = qjs.JSContext;
const JSValue = qjs.JSValue;

// QuickJS callback type for C functions
const JSCFunction = *const fn (?*JSContext, JSValue, c_int, [*c]JSValue) callconv(.c) JSValue;

/// Global state for native bindings
var global_allocator: ?Allocator = null;
var global_cwd: []const u8 = "/";

/// Initialize native bindings
pub fn init(allocator: Allocator) void {
    global_allocator = allocator;
}

/// Set current working directory
pub fn setCwd(path: []const u8) void {
    global_cwd = path;
}

/// Get JS undefined value
inline fn jsUndefined() JSValue {
    // JS_TAG_UNDEFINED = 3 (see quickjs.h line 101)
    return .{ .tag = 3, .u = .{ .int32 = 0 } };
}

/// Get JS null value
inline fn jsNull() JSValue {
    // JS_TAG_NULL = 2 (see quickjs.h line 100)
    return .{ .tag = 2, .u = .{ .int32 = 0 } };
}

/// Get JS bool value (workaround for cimport bug)
inline fn jsBool(val: bool) JSValue {
    // JS_TAG_BOOL = 1, tag is stored as i64
    return .{
        .tag = 1, // JS_TAG_BOOL
        .u = .{ .int32 = if (val) 1 else 0 },
    };
}

/// Get JS false value
inline fn jsFalse(_: ?*JSContext) JSValue {
    return jsBool(false);
}

/// Get JS true value
inline fn jsTrue(_: ?*JSContext) JSValue {
    return jsBool(true);
}

/// Register all native bindings in QuickJS context
pub fn registerAll(ctx_ptr: *anyopaque) void {
    const ctx: ?*JSContext = @ptrCast(ctx_ptr);
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // File system bindings
    registerFunc(ctx, global, "__edgebox_fs_read", fsRead, 1);
    registerFunc(ctx, global, "__edgebox_fs_write", fsWrite, 2);
    registerFunc(ctx, global, "__edgebox_fs_exists", fsExists, 1);
    registerFunc(ctx, global, "__edgebox_fs_stat", fsStat, 1);
    registerFunc(ctx, global, "__edgebox_fs_readdir", fsReaddir, 1);
    registerFunc(ctx, global, "__edgebox_fs_mkdir", fsMkdir, 2);
    registerFunc(ctx, global, "__edgebox_fs_unlink", fsUnlink, 1);
    registerFunc(ctx, global, "__edgebox_fs_rmdir", fsRmdir, 2);
    registerFunc(ctx, global, "__edgebox_fs_rename", fsRename, 2);
    registerFunc(ctx, global, "__edgebox_fs_copy", fsCopy, 2);
    registerFunc(ctx, global, "__edgebox_fs_append", fsAppend, 2);

    // File descriptor-based operations (for TSC and other apps using open/write/close)
    registerFunc(ctx, global, "__edgebox_fs_open", fsOpen, 3);
    registerFunc(ctx, global, "__edgebox_fs_write_fd", fsWriteFd, 3);
    registerFunc(ctx, global, "__edgebox_fs_read_fd", fsReadFd, 4);
    registerFunc(ctx, global, "__edgebox_fs_close", fsClose, 1);

    // Native-only FS bindings (not available in WASI)
    registerFunc(ctx, global, "__edgebox_fs_chmod", fsChmod, 2);
    registerFunc(ctx, global, "__edgebox_fs_chown", fsChown, 3);
    registerFunc(ctx, global, "__edgebox_fs_symlink", fsSymlink, 2);
    registerFunc(ctx, global, "__edgebox_fs_link", fsLink, 2);
    registerFunc(ctx, global, "__edgebox_fs_readlink", fsReadlink, 1);
    registerFunc(ctx, global, "__edgebox_fs_lstat", fsLstat, 1);
    registerFunc(ctx, global, "__edgebox_fs_truncate", fsTruncate, 2);
    registerFunc(ctx, global, "__edgebox_fs_utimes", fsUtimes, 3);

    // Process bindings
    registerFunc(ctx, global, "__edgebox_cwd", getCwd, 0);

    // Crypto bindings
    registerFunc(ctx, global, "__edgebox_random_bytes", randomBytes, 1);
    registerFunc(ctx, global, "__edgebox_hash", nativeHash, 2);

    // Network bindings
    registerFunc(ctx, global, "__edgebox_fetch", nativeFetch, 4);
}

/// Helper to register a C function in QuickJS
fn registerFunc(
    ctx: ?*JSContext,
    global: JSValue,
    name: [:0]const u8,
    func: JSCFunction,
    arg_count: u8,
) void {
    const func_val = qjs.JS_NewCFunction(ctx, func, name.ptr, arg_count);
    _ = qjs.JS_SetPropertyStr(ctx, global, name.ptr, func_val);
}

// ============================================================================
// File System Bindings
// ============================================================================

/// Threshold for using mmap - files larger than 1MB use memory mapping
const MMAP_THRESHOLD: u64 = 1024 * 1024;

/// Read file contents as string
fn fsRead(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.readFileSync requires path argument");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve path
    const resolved = resolvePath(allocator, path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved);

    // Open file
    const file = std.fs.cwd().openFile(resolved, .{}) catch |err| {
        return throwErrno(ctx, "open", err);
    };
    defer file.close();

    // Get file size to decide read strategy
    const stat = file.stat() catch |err| {
        return throwErrno(ctx, "stat", err);
    };

    const file_size = stat.size;

    // For large files, try mmap for better performance
    if (file_size > MMAP_THRESHOLD) {
        if (fsReadMmap(ctx, file, file_size)) |result| {
            return result;
        }
        // Fallback to regular read if mmap fails
    }

    // Regular read for small files or mmap fallback
    const content = file.readToEndAlloc(allocator, 100 * 1024 * 1024) catch |err| {
        return throwErrno(ctx, "read", err);
    };
    defer allocator.free(content);

    return qjs.JS_NewStringLen(ctx, content.ptr, content.len);
}

/// Read file using mmap for better performance on large files
fn fsReadMmap(ctx: ?*JSContext, file: std.fs.File, size: u64) ?JSValue {
    // Only supported on Unix-like systems
    if (comptime builtin.os.tag != .macos and builtin.os.tag != .linux) {
        return null;
    }

    const fd = file.handle;

    // Platform-specific read optimizations
    switch (comptime builtin.os.tag) {
        .macos => {
            // Enable readahead on macOS (F_RDAHEAD = 45)
            _ = std.c.fcntl(fd, 45, @as(c_int, 1));
        },
        .linux => {
            // Sequential access hint for Linux
            _ = std.os.linux.fadvise(fd, 0, @intCast(size), std.os.linux.POSIX_FADV.SEQUENTIAL);
        },
        else => {},
    }

    // Memory map the file
    const mapped = std.posix.mmap(
        null,
        @intCast(size),
        std.posix.PROT.READ,
        .{ .TYPE = .PRIVATE },
        fd,
        0,
    ) catch {
        return null; // Fallback to regular read
    };
    defer std.posix.munmap(mapped);

    // Hint: we'll need all of this data (MADV_WILLNEED = 3 on most platforms)
    // Skip madvise - not critical for correctness

    // Create JS string from mapped memory
    return qjs.JS_NewStringLen(ctx, mapped.ptr, mapped.len);
}

/// Write data to file
fn fsWrite(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "fs.writeFileSync requires path and data arguments");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    const data = getStringArg(ctx, argv[1]) orelse
        return qjs.JS_ThrowTypeError(ctx, "data must be a string");
    defer freeStringArg(ctx, data);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve path
    const resolved = resolvePath(allocator, path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved);

    // Create parent directories if they don't exist
    if (std.fs.path.dirname(resolved)) |parent| {
        std.fs.cwd().makePath(parent) catch |err| {
            // Ignore PathAlreadyExists - directory already exists
            if (err != error.PathAlreadyExists) {
                // Don't fail on mkdir errors - let createFile handle it
            }
        };
    }

    // Create/truncate and write file
    const file = std.fs.cwd().createFile(resolved, .{}) catch |err| {
        return throwErrno(ctx, "create", err);
    };
    defer file.close();

    file.writeAll(data) catch |err| {
        return throwErrno(ctx, "write", err);
    };

    return jsUndefined();
}

/// Append data to file
fn fsAppend(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "fs.appendFileSync requires path and data arguments");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    const data = getStringArg(ctx, argv[1]) orelse
        return qjs.JS_ThrowTypeError(ctx, "data must be a string");
    defer freeStringArg(ctx, data);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve path
    const resolved = resolvePath(allocator, path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved);

    // Create parent directories if they don't exist
    if (std.fs.path.dirname(resolved)) |parent| {
        std.fs.cwd().makePath(parent) catch {};
    }

    // Open file for appending (create if doesn't exist)
    const file = std.fs.cwd().openFile(resolved, .{ .mode = .write_only }) catch |open_err| {
        // If file doesn't exist, create it
        if (open_err == error.FileNotFound) {
            const new_file = std.fs.cwd().createFile(resolved, .{}) catch |err| {
                return throwErrno(ctx, "create", err);
            };
            defer new_file.close();
            new_file.writeAll(data) catch |err| {
                return throwErrno(ctx, "write", err);
            };
            return jsUndefined();
        }
        return throwErrno(ctx, "open", open_err);
    };
    defer file.close();

    // Seek to end and append - get file size and seek there
    const stat = file.stat() catch |err| {
        return throwErrno(ctx, "stat", err);
    };
    file.seekTo(stat.size) catch |err| {
        return throwErrno(ctx, "seek", err);
    };

    file.writeAll(data) catch |err| {
        return throwErrno(ctx, "write", err);
    };

    return jsUndefined();
}

// ============================================================================
// File Descriptor-Based Operations (for TSC and similar apps)
// ============================================================================

/// Global file descriptor table - maps JS fd numbers to Zig File handles
const MAX_FDS = 256;
var fd_table: [MAX_FDS]?std.fs.File = [_]?std.fs.File{null} ** MAX_FDS;
var fd_paths: [MAX_FDS]?[]const u8 = [_]?[]const u8{null} ** MAX_FDS;
const FIRST_USER_FD: usize = 10; // After stdin/stdout/stderr

/// Open file and return file descriptor
fn fsOpen(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.openSync requires path argument");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    // Parse flags (string or number)
    var flags_str: ?[]const u8 = null;
    var flags_num: i32 = 0;
    if (argc >= 2) {
        flags_str = getStringArg(ctx, argv[1]);
        if (flags_str == null) {
            _ = qjs.JS_ToInt32(ctx, &flags_num, argv[1]);
        }
    }
    defer if (flags_str) |f| freeStringArg(ctx, f);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve path
    const resolved = resolvePath(allocator, path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");

    // Determine open mode from flags
    const is_write = if (flags_str) |f|
        std.mem.eql(u8, f, "w") or std.mem.eql(u8, f, "w+") or
            std.mem.eql(u8, f, "a") or std.mem.eql(u8, f, "a+") or
            std.mem.eql(u8, f, "wx") or std.mem.eql(u8, f, "xw")
    else
        (flags_num & 1) != 0 or (flags_num & 2) != 0; // O_WRONLY or O_RDWR

    const is_append = if (flags_str) |f|
        std.mem.eql(u8, f, "a") or std.mem.eql(u8, f, "a+")
    else
        (flags_num & 0o2000) != 0; // O_APPEND

    const is_create = if (flags_str) |f|
        std.mem.eql(u8, f, "w") or std.mem.eql(u8, f, "w+") or
            std.mem.eql(u8, f, "a") or std.mem.eql(u8, f, "a+") or
            std.mem.eql(u8, f, "wx") or std.mem.eql(u8, f, "xw")
    else
        (flags_num & 0o100) != 0; // O_CREAT

    // Create parent directories for write operations
    if (is_write or is_create) {
        if (std.fs.path.dirname(resolved)) |parent| {
            std.fs.cwd().makePath(parent) catch {};
        }
    }

    // Open or create the file
    const file = if (is_write and !is_append) blk: {
        // Write mode (truncate or create)
        break :blk std.fs.cwd().createFile(resolved, .{ .truncate = true }) catch |err| {
            allocator.free(resolved);
            return throwErrno(ctx, "open", err);
        };
    } else if (is_append) blk: {
        // Append mode - open existing or create new
        break :blk std.fs.cwd().openFile(resolved, .{ .mode = .write_only }) catch |open_err| {
            if (open_err == error.FileNotFound) {
                break :blk std.fs.cwd().createFile(resolved, .{}) catch |err| {
                    allocator.free(resolved);
                    return throwErrno(ctx, "open", err);
                };
            }
            allocator.free(resolved);
            return throwErrno(ctx, "open", open_err);
        };
    } else blk: {
        // Read mode
        break :blk std.fs.cwd().openFile(resolved, .{}) catch |err| {
            allocator.free(resolved);
            return throwErrno(ctx, "open", err);
        };
    };

    // Find free slot in fd table (linear search allows FD reuse)
    var fd_slot: usize = FIRST_USER_FD;
    while (fd_slot < MAX_FDS and fd_table[fd_slot] != null) {
        fd_slot += 1;
    }
    if (fd_slot >= MAX_FDS) {
        file.close();
        allocator.free(resolved);
        return qjs.JS_ThrowInternalError(ctx, "too many open files");
    }

    // Store in fd table
    fd_table[fd_slot] = file;
    fd_paths[fd_slot] = resolved; // Keep path for fstat

    // If append mode, seek to end
    if (is_append) {
        const stat = file.stat() catch {
            return qjs.JS_NewInt32(ctx, @intCast(fd_slot));
        };
        file.seekTo(stat.size) catch {};
    }

    return qjs.JS_NewInt32(ctx, @intCast(fd_slot));
}

/// Write data to file descriptor
fn fsWriteFd(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "fs.writeSync requires fd and data arguments");

    var fd: i32 = undefined;
    if (qjs.JS_ToInt32(ctx, &fd, argv[0]) != 0 or fd < 0) {
        return qjs.JS_ThrowTypeError(ctx, "fd must be a positive integer");
    }

    // Get data - can be string or ArrayBuffer/TypedArray
    var data_ptr: [*c]const u8 = null;
    var data_len: usize = 0;

    // Try string first
    const str_data = getStringArg(ctx, argv[1]);
    if (str_data) |s| {
        data_ptr = s.ptr;
        data_len = s.len;
    } else {
        // Try ArrayBuffer
        var psize: usize = undefined;
        data_ptr = qjs.JS_GetArrayBuffer(ctx, &psize, argv[1]);
        if (data_ptr != null) {
            data_len = psize;
        } else {
            return qjs.JS_ThrowTypeError(ctx, "data must be a string or ArrayBuffer");
        }
    }
    defer if (str_data != null) freeStringArg(ctx, str_data.?);

    const data = data_ptr[0..data_len];

    // Get file from fd table
    const fd_idx: usize = @intCast(fd);
    if (fd_idx >= MAX_FDS or fd_table[fd_idx] == null) {
        return qjs.JS_ThrowTypeError(ctx, "invalid file descriptor");
    }

    const file = fd_table[fd_idx].?;

    // Write data
    file.writeAll(data) catch |err| {
        return throwErrno(ctx, "write", err);
    };

    return qjs.JS_NewInt32(ctx, @intCast(data_len));
}

/// Read data from file descriptor
fn fsReadFd(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 4) return qjs.JS_ThrowTypeError(ctx, "fs.readSync requires fd, buffer, offset, length arguments");

    var fd: i32 = undefined;
    if (qjs.JS_ToInt32(ctx, &fd, argv[0]) != 0 or fd < 0) {
        return qjs.JS_ThrowTypeError(ctx, "fd must be a positive integer");
    }

    // Get buffer (ArrayBuffer or TypedArray)
    var buf_size: usize = undefined;
    const buf_ptr = qjs.JS_GetArrayBuffer(ctx, &buf_size, argv[1]);
    if (buf_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "buffer must be an ArrayBuffer");
    }

    var offset: i32 = 0;
    var length: i32 = @intCast(buf_size);
    _ = qjs.JS_ToInt32(ctx, &offset, argv[2]);
    _ = qjs.JS_ToInt32(ctx, &length, argv[3]);

    // Get file from fd table
    const fd_idx: usize = @intCast(fd);
    if (fd_idx >= MAX_FDS or fd_table[fd_idx] == null) {
        return qjs.JS_ThrowTypeError(ctx, "invalid file descriptor");
    }

    const file = fd_table[fd_idx].?;

    // Calculate actual read length
    const offset_u: usize = @intCast(@max(0, offset));
    const length_u: usize = @intCast(@max(0, length));
    const actual_len = @min(length_u, buf_size - offset_u);

    // Read data
    const bytes_read = file.read(buf_ptr[offset_u..][0..actual_len]) catch |err| {
        return throwErrno(ctx, "read", err);
    };

    return qjs.JS_NewInt32(ctx, @intCast(bytes_read));
}

/// Close file descriptor
fn fsClose(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.closeSync requires fd argument");

    var fd: i32 = undefined;
    if (qjs.JS_ToInt32(ctx, &fd, argv[0]) != 0 or fd < 0) {
        return qjs.JS_ThrowTypeError(ctx, "fd must be a positive integer");
    }

    const fd_idx: usize = @intCast(fd);
    if (fd_idx >= MAX_FDS or fd_table[fd_idx] == null) {
        return jsUndefined(); // Already closed, no error
    }

    // Close file
    fd_table[fd_idx].?.close();
    fd_table[fd_idx] = null;

    // Free stored path
    if (fd_paths[fd_idx]) |p| {
        if (global_allocator) |alloc| {
            alloc.free(p);
        }
    }
    fd_paths[fd_idx] = null;

    return jsUndefined();
}

/// Check if file exists
fn fsExists(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return jsFalse(ctx);

    const path = getStringArg(ctx, argv[0]) orelse return jsFalse(ctx);
    defer freeStringArg(ctx, path);

    const allocator = global_allocator orelse return jsFalse(ctx);

    // Resolve path
    const resolved = resolvePath(allocator, path) catch return jsFalse(ctx);
    defer allocator.free(resolved);

    // Check access
    std.fs.cwd().access(resolved, .{}) catch return jsFalse(ctx);
    return jsTrue(ctx);
}

/// Get file/directory stats
fn fsStat(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.statSync requires path argument");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve path
    const resolved = resolvePath(allocator, path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved);

    // Get stat
    const stat = std.fs.cwd().statFile(resolved) catch |err| {
        return throwErrno(ctx, "stat", err);
    };

    // Create stat object
    const obj = qjs.JS_NewObject(ctx);

    const size: i64 = @intCast(stat.size);
    _ = qjs.JS_SetPropertyStr(ctx, obj, "size", qjs.JS_NewInt64(ctx, size));

    // Mode: actual file mode with type bits
    const is_dir: bool = stat.kind == .directory;
    const is_symlink: bool = stat.kind == .sym_link;
    // Combine file type bits with permission bits from stat.mode
    const type_bits: u32 = if (is_symlink) 0o120000 else if (is_dir) 0o040000 else 0o100000;
    const perm_bits: u32 = @as(u32, @intCast(stat.mode)) & 0o7777;
    const mode: i32 = @intCast(type_bits | perm_bits);
    _ = qjs.JS_SetPropertyStr(ctx, obj, "mode", qjs.JS_NewInt32(ctx, mode));

    // isFile(), isDirectory() methods - use simple boolean properties
    _ = qjs.JS_SetPropertyStr(ctx, obj, "_isDir", jsBool(is_dir));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "_isFile", jsBool(!is_dir and !is_symlink));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "_isSymlink", jsBool(is_symlink));

    // Timestamps (as milliseconds since epoch) - MUST be set before adding methods
    const mtime_ms: i64 = @intCast(@divFloor(stat.mtime, std.time.ns_per_ms));
    const atime_ms: i64 = @intCast(@divFloor(stat.atime, std.time.ns_per_ms));
    const ctime_ms: i64 = @intCast(@divFloor(stat.ctime, std.time.ns_per_ms));

    _ = qjs.JS_SetPropertyStr(ctx, obj, "mtimeMs", qjs.JS_NewInt64(ctx, mtime_ms));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "atimeMs", qjs.JS_NewInt64(ctx, atime_ms));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "ctimeMs", qjs.JS_NewInt64(ctx, ctime_ms));

    // mtime/atime/ctime as Date objects (Node.js compatibility)
    _ = qjs.JS_SetPropertyStr(ctx, obj, "mtime", qjs.JS_NewInt64(ctx, mtime_ms));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "atime", qjs.JS_NewInt64(ctx, atime_ms));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "ctime", qjs.JS_NewInt64(ctx, ctime_ms));

    // Add isFile/isDirectory/isSymbolicLink as functions via eval
    const methods_code =
        \\(function(obj) {
        \\    obj.isFile = function() { return this._isFile; };
        \\    obj.isDirectory = function() { return this._isDir; };
        \\    obj.isSymbolicLink = function() { return this._isSymlink || false; };
        \\    obj.mtime = new Date(obj.mtimeMs);
        \\    obj.atime = new Date(obj.atimeMs);
        \\    obj.ctime = new Date(obj.ctimeMs);
        \\    return obj;
        \\})
    ;
    const methods_fn = qjs.JS_Eval(ctx, methods_code.ptr, methods_code.len, "<stat>", qjs.JS_EVAL_TYPE_GLOBAL);
    if (!qjs.JS_IsException(methods_fn)) {
        var args = [_]JSValue{obj};
        const result = qjs.JS_Call(ctx, methods_fn, jsUndefined(), 1, &args);
        qjs.JS_FreeValue(ctx, methods_fn);
        if (!qjs.JS_IsException(result)) {
            return result;
        }
        qjs.JS_FreeValue(ctx, result);
    } else {
        qjs.JS_FreeValue(ctx, methods_fn);
    }

    return obj;
}

/// Read directory entries
fn fsReaddir(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.readdirSync requires path argument");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve path
    const resolved = resolvePath(allocator, path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved);

    // Open directory
    var dir = std.fs.cwd().openDir(resolved, .{ .iterate = true }) catch |err| {
        return throwErrno(ctx, "opendir", err);
    };
    defer dir.close();

    // Create array of entries
    const arr = qjs.JS_NewArray(ctx);
    var idx: u32 = 0;

    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        const name_val = qjs.JS_NewStringLen(ctx, entry.name.ptr, entry.name.len);
        _ = qjs.JS_SetPropertyUint32(ctx, arr, idx, name_val);
        idx += 1;
    }

    return arr;
}

/// Create directory
fn fsMkdir(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.mkdirSync requires path argument");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    const recursive = if (argc >= 2) qjs.JS_ToBool(ctx, argv[1]) != 0 else false;

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve path
    const resolved = resolvePath(allocator, path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved);

    // Create directory
    if (recursive) {
        std.fs.cwd().makePath(resolved) catch |err| {
            return throwErrno(ctx, "mkdir", err);
        };
    } else {
        std.fs.cwd().makeDir(resolved) catch |err| {
            return throwErrno(ctx, "mkdir", err);
        };
    }

    return jsUndefined();
}

/// Delete file
fn fsUnlink(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.unlinkSync requires path argument");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve path
    const resolved = resolvePath(allocator, path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved);

    // Delete file
    std.fs.cwd().deleteFile(resolved) catch |err| {
        return throwErrno(ctx, "unlink", err);
    };

    return jsUndefined();
}

/// Delete directory
fn fsRmdir(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.rmdirSync requires path argument");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    const recursive = if (argc >= 2) qjs.JS_ToBool(ctx, argv[1]) != 0 else false;

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve path
    const resolved = resolvePath(allocator, path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved);

    // Delete directory
    if (recursive) {
        std.fs.cwd().deleteTree(resolved) catch |err| {
            return throwErrno(ctx, "rmdir", err);
        };
    } else {
        std.fs.cwd().deleteDir(resolved) catch |err| {
            return throwErrno(ctx, "rmdir", err);
        };
    }

    return jsUndefined();
}

/// Rename file/directory
fn fsRename(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "fs.renameSync requires oldPath and newPath arguments");

    const old_path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "oldPath must be a string");
    defer freeStringArg(ctx, old_path);

    const new_path = getStringArg(ctx, argv[1]) orelse
        return qjs.JS_ThrowTypeError(ctx, "newPath must be a string");
    defer freeStringArg(ctx, new_path);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve paths
    const resolved_old = resolvePath(allocator, old_path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved_old);

    const resolved_new = resolvePath(allocator, new_path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved_new);

    // Create parent directories for destination if they don't exist
    if (std.fs.path.dirname(resolved_new)) |parent| {
        std.fs.cwd().makePath(parent) catch {};
    }

    // Rename
    std.fs.cwd().rename(resolved_old, resolved_new) catch |err| {
        return throwErrno(ctx, "rename", err);
    };

    return jsUndefined();
}

/// Copy file
fn fsCopy(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "fs.copyFileSync requires src and dest arguments");

    const src = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "src must be a string");
    defer freeStringArg(ctx, src);

    const dest = getStringArg(ctx, argv[1]) orelse
        return qjs.JS_ThrowTypeError(ctx, "dest must be a string");
    defer freeStringArg(ctx, dest);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve paths
    const resolved_src = resolvePath(allocator, src) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved_src);

    const resolved_dest = resolvePath(allocator, dest) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved_dest);

    // Create parent directories for destination if they don't exist
    if (std.fs.path.dirname(resolved_dest)) |parent| {
        std.fs.cwd().makePath(parent) catch {};
    }

    // Copy file
    std.fs.cwd().copyFile(resolved_src, std.fs.cwd(), resolved_dest, .{}) catch |err| {
        return throwErrno(ctx, "copyfile", err);
    };

    return jsUndefined();
}

// ============================================================================
// Native-only FS Bindings (not available in WASI)
// ============================================================================

/// Change file permissions (chmod)
fn fsChmod(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "fs.chmodSync requires path and mode arguments");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    var mode: i32 = undefined;
    if (qjs.JS_ToInt32(ctx, &mode, argv[1]) != 0) {
        return qjs.JS_ThrowTypeError(ctx, "mode must be a number");
    }

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve path
    const resolved = resolvePath(allocator, path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved);

    // Open file to get fd for fchmod
    const file = std.fs.cwd().openFile(resolved, .{ .mode = .read_only }) catch |err| {
        return throwErrno(ctx, "open", err);
    };
    defer file.close();

    // Change mode
    file.chmod(@intCast(@as(u32, @bitCast(mode)) & 0o7777)) catch |err| {
        return throwErrno(ctx, "chmod", err);
    };

    return jsUndefined();
}

/// Change file ownership (chown)
fn fsChown(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 3) return qjs.JS_ThrowTypeError(ctx, "fs.chownSync requires path, uid, and gid arguments");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    var uid: i32 = undefined;
    if (qjs.JS_ToInt32(ctx, &uid, argv[1]) != 0) {
        return qjs.JS_ThrowTypeError(ctx, "uid must be a number");
    }

    var gid: i32 = undefined;
    if (qjs.JS_ToInt32(ctx, &gid, argv[2]) != 0) {
        return qjs.JS_ThrowTypeError(ctx, "gid must be a number");
    }

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve path
    const resolved = resolvePath(allocator, path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved);

    // Open file to get fd for fchown
    const file = std.fs.cwd().openFile(resolved, .{ .mode = .read_only }) catch |err| {
        return throwErrno(ctx, "open", err);
    };
    defer file.close();

    // Change ownership
    file.chown(if (uid >= 0) @intCast(uid) else null, if (gid >= 0) @intCast(gid) else null) catch |err| {
        return throwErrno(ctx, "chown", err);
    };

    return jsUndefined();
}

/// Create symbolic link
fn fsSymlink(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "fs.symlinkSync requires target and path arguments");

    const target = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "target must be a string");
    defer freeStringArg(ctx, target);

    const link_path = getStringArg(ctx, argv[1]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, link_path);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve link path (target stays as-is for symlinks)
    const resolved_link = resolvePath(allocator, link_path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved_link);

    // Create parent directories if they don't exist
    if (std.fs.path.dirname(resolved_link)) |parent| {
        std.fs.cwd().makePath(parent) catch {};
    }

    // Create symlink
    std.posix.symlink(target, resolved_link) catch |err| {
        return throwErrno(ctx, "symlink", err);
    };

    return jsUndefined();
}

/// Create hard link
fn fsLink(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "fs.linkSync requires existingPath and newPath arguments");

    const existing = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "existingPath must be a string");
    defer freeStringArg(ctx, existing);

    const new_path = getStringArg(ctx, argv[1]) orelse
        return qjs.JS_ThrowTypeError(ctx, "newPath must be a string");
    defer freeStringArg(ctx, new_path);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve paths
    const resolved_existing = resolvePath(allocator, existing) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved_existing);

    const resolved_new = resolvePath(allocator, new_path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved_new);

    // Create parent directories if they don't exist
    if (std.fs.path.dirname(resolved_new)) |parent| {
        std.fs.cwd().makePath(parent) catch {};
    }

    // Create hard link using POSIX link syscall
    const existing_z = std.posix.toPosixPath(resolved_existing) catch
        return qjs.JS_ThrowInternalError(ctx, "path too long");
    const new_z = std.posix.toPosixPath(resolved_new) catch
        return qjs.JS_ThrowInternalError(ctx, "path too long");
    std.posix.linkat(std.posix.AT.FDCWD, &existing_z, std.posix.AT.FDCWD, &new_z, 0) catch |err| {
        return throwErrno(ctx, "link", err);
    };

    return jsUndefined();
}

/// Read symbolic link target
fn fsReadlink(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.readlinkSync requires path argument");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve path
    const resolved = resolvePath(allocator, path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved);

    // Read link target
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const target = std.fs.cwd().readLink(resolved, &buf) catch |err| {
        return throwErrno(ctx, "readlink", err);
    };

    return qjs.JS_NewStringLen(ctx, target.ptr, target.len);
}

/// Get file stats without following symlinks (lstat)
fn fsLstat(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.lstatSync requires path argument");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve path
    const resolved = resolvePath(allocator, path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved);

    // Get stat without following symlinks using fstatat with AT_SYMLINK_NOFOLLOW
    const resolved_z = allocator.dupeZ(u8, resolved) catch
        return qjs.JS_ThrowInternalError(ctx, "out of memory");
    defer allocator.free(resolved_z);

    const stat = std.posix.fstatat(std.posix.AT.FDCWD, resolved_z, std.posix.AT.SYMLINK_NOFOLLOW) catch |err| {
        return throwErrno(ctx, "lstat", err);
    };

    // Create stat object
    const obj = qjs.JS_NewObject(ctx);

    const size: i64 = @intCast(stat.size);
    _ = qjs.JS_SetPropertyStr(ctx, obj, "size", qjs.JS_NewInt64(ctx, size));

    // Check file type from mode (S_IFMT mask)
    const S_IFMT: u32 = 0o170000;
    const S_IFLNK: u32 = 0o120000;
    const S_IFDIR: u32 = 0o040000;
    const S_IFREG: u32 = 0o100000;
    const file_type = stat.mode & S_IFMT;
    const is_symlink = file_type == S_IFLNK;
    const is_dir = file_type == S_IFDIR;
    const is_file = file_type == S_IFREG;

    // Mode includes type bits and permission bits
    const mode_i32: i32 = @intCast(stat.mode);
    _ = qjs.JS_SetPropertyStr(ctx, obj, "mode", qjs.JS_NewInt32(ctx, mode_i32));

    // Type flags
    _ = qjs.JS_SetPropertyStr(ctx, obj, "_isDir", jsBool(is_dir));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "_isFile", jsBool(is_file));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "_isSymlink", jsBool(is_symlink));

    // Timestamps from the stat structure (mtimespec/atimespec on macOS, mtim/atim on Linux)
    const mtime_ms: i64 = @as(i64, stat.mtimespec.sec) * 1000 + @divFloor(stat.mtimespec.nsec, 1_000_000);
    const atime_ms: i64 = @as(i64, stat.atimespec.sec) * 1000 + @divFloor(stat.atimespec.nsec, 1_000_000);
    const ctime_ms: i64 = @as(i64, stat.ctimespec.sec) * 1000 + @divFloor(stat.ctimespec.nsec, 1_000_000);

    _ = qjs.JS_SetPropertyStr(ctx, obj, "mtimeMs", qjs.JS_NewInt64(ctx, mtime_ms));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "atimeMs", qjs.JS_NewInt64(ctx, atime_ms));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "ctimeMs", qjs.JS_NewInt64(ctx, ctime_ms));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "mtime", qjs.JS_NewInt64(ctx, mtime_ms));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "atime", qjs.JS_NewInt64(ctx, atime_ms));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "ctime", qjs.JS_NewInt64(ctx, ctime_ms));

    // Add methods via eval
    const methods_code =
        \\(function(obj) {
        \\    obj.isFile = function() { return this._isFile; };
        \\    obj.isDirectory = function() { return this._isDir; };
        \\    obj.isSymbolicLink = function() { return this._isSymlink; };
        \\    obj.mtime = new Date(obj.mtimeMs);
        \\    obj.atime = new Date(obj.atimeMs);
        \\    obj.ctime = new Date(obj.ctimeMs);
        \\    return obj;
        \\})
    ;
    const methods_fn = qjs.JS_Eval(ctx, methods_code.ptr, methods_code.len, "<lstat>", qjs.JS_EVAL_TYPE_GLOBAL);
    if (!qjs.JS_IsException(methods_fn)) {
        var args = [_]JSValue{obj};
        const result = qjs.JS_Call(ctx, methods_fn, jsUndefined(), 1, &args);
        qjs.JS_FreeValue(ctx, methods_fn);
        if (!qjs.JS_IsException(result)) {
            return result;
        }
        qjs.JS_FreeValue(ctx, result);
    } else {
        qjs.JS_FreeValue(ctx, methods_fn);
    }

    return obj;
}

/// Truncate file to specified length
fn fsTruncate(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.truncateSync requires path argument");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    var len: i64 = 0;
    if (argc >= 2) {
        if (qjs.JS_ToInt64(ctx, &len, argv[1]) != 0) {
            return qjs.JS_ThrowTypeError(ctx, "length must be a number");
        }
    }

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve path
    const resolved = resolvePath(allocator, path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved);

    // Open file for writing
    const file = std.fs.cwd().openFile(resolved, .{ .mode = .write_only }) catch |err| {
        return throwErrno(ctx, "open", err);
    };
    defer file.close();

    // Truncate to specified length
    file.setEndPos(@intCast(if (len < 0) 0 else len)) catch |err| {
        return throwErrno(ctx, "truncate", err);
    };

    return jsUndefined();
}

/// Update file access and modification times
fn fsUtimes(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 3) return qjs.JS_ThrowTypeError(ctx, "fs.utimesSync requires path, atime, and mtime arguments");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    // atime and mtime can be numbers (seconds since epoch) or Date objects
    var atime_sec: f64 = 0;
    var mtime_sec: f64 = 0;
    _ = qjs.JS_ToFloat64(ctx, &atime_sec, argv[1]);
    _ = qjs.JS_ToFloat64(ctx, &mtime_sec, argv[2]);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Resolve path
    const resolved = resolvePath(allocator, path) catch
        return qjs.JS_ThrowInternalError(ctx, "failed to resolve path");
    defer allocator.free(resolved);

    // Open file
    const file = std.fs.cwd().openFile(resolved, .{ .mode = .read_only }) catch |err| {
        return throwErrno(ctx, "open", err);
    };
    defer file.close();

    // Convert seconds to nanoseconds (Zig's updateTimes uses nanoseconds)
    const atime_ns: i128 = @intFromFloat(atime_sec * @as(f64, std.time.ns_per_s));
    const mtime_ns: i128 = @intFromFloat(mtime_sec * @as(f64, std.time.ns_per_s));

    file.updateTimes(@intCast(atime_ns), @intCast(mtime_ns)) catch |err| {
        return throwErrno(ctx, "utimes", err);
    };

    return jsUndefined();
}

// ============================================================================
// Process Bindings
// ============================================================================

/// Get current working directory
fn getCwd(ctx: ?*JSContext, _: JSValue, _: c_int, _: [*c]JSValue) callconv(.c) JSValue {
    return qjs.JS_NewStringLen(ctx, global_cwd.ptr, global_cwd.len);
}

// ============================================================================
// Crypto Bindings
// ============================================================================

/// Generate random bytes
fn randomBytes(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "crypto.randomBytes requires size argument");

    var size: i32 = undefined;
    if (qjs.JS_ToInt32(ctx, &size, argv[0]) != 0 or size < 0) {
        return qjs.JS_ThrowTypeError(ctx, "size must be a positive integer");
    }

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Allocate buffer
    const buf = allocator.alloc(u8, @intCast(size)) catch {
        return qjs.JS_ThrowInternalError(ctx, "out of memory");
    };
    defer allocator.free(buf);

    // Fill with random bytes
    std.crypto.random.bytes(buf);

    // Create Uint8Array
    const array_buf = qjs.JS_NewArrayBufferCopy(ctx, buf.ptr, buf.len);
    if (qjs.JS_IsException(array_buf)) {
        return array_buf;
    }

    // Wrap in Uint8Array
    const uint8_code =
        \\(function(buf) { return new Uint8Array(buf); })
    ;
    const wrapper = qjs.JS_Eval(ctx, uint8_code.ptr, uint8_code.len, "<crypto>", qjs.JS_EVAL_TYPE_GLOBAL);
    if (qjs.JS_IsException(wrapper)) {
        qjs.JS_FreeValue(ctx, array_buf);
        return wrapper;
    }
    defer qjs.JS_FreeValue(ctx, wrapper);

    var args = [_]JSValue{array_buf};
    const result = qjs.JS_Call(ctx, wrapper, jsUndefined(), 1, &args);
    qjs.JS_FreeValue(ctx, array_buf);

    return result;
}

/// Hash data using specified algorithm (sha256, sha384, sha512, sha1, md5)
fn nativeHash(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "hash requires algorithm and data arguments");

    const algorithm = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "algorithm must be a string");
    defer freeStringArg(ctx, algorithm);

    const data = getStringArg(ctx, argv[1]) orelse
        return qjs.JS_ThrowTypeError(ctx, "data must be a string");
    defer freeStringArg(ctx, data);

    // Hash based on algorithm
    if (std.mem.eql(u8, algorithm, "sha256")) {
        var hash: [32]u8 = undefined;
        std.crypto.hash.sha2.Sha256.hash(data, &hash, .{});
        return hexEncode(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "sha384")) {
        var hash: [48]u8 = undefined;
        std.crypto.hash.sha2.Sha384.hash(data, &hash, .{});
        return hexEncode(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "sha512")) {
        var hash: [64]u8 = undefined;
        std.crypto.hash.sha2.Sha512.hash(data, &hash, .{});
        return hexEncode(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "sha1")) {
        var hash: [20]u8 = undefined;
        std.crypto.hash.Sha1.hash(data, &hash, .{});
        return hexEncode(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "md5")) {
        var hash: [16]u8 = undefined;
        std.crypto.hash.Md5.hash(data, &hash, .{});
        return hexEncode(ctx, &hash);
    } else {
        return qjs.JS_ThrowTypeError(ctx, "unsupported algorithm: use sha256, sha384, sha512, sha1, or md5");
    }
}

/// Encode bytes as hex string
fn hexEncode(ctx: ?*JSContext, bytes: []const u8) JSValue {
    const hex_chars = "0123456789abcdef";
    var buf: [128]u8 = undefined;
    if (bytes.len * 2 > buf.len) {
        return qjs.JS_ThrowInternalError(ctx, "hash too long");
    }
    for (bytes, 0..) |byte, i| {
        buf[i * 2] = hex_chars[byte >> 4];
        buf[i * 2 + 1] = hex_chars[byte & 0x0F];
    }
    return qjs.JS_NewStringLen(ctx, &buf, bytes.len * 2);
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Get string argument from JS value
fn getStringArg(ctx: ?*JSContext, val: JSValue) ?[]const u8 {
    var len: usize = undefined;
    const cstr = qjs.JS_ToCStringLen(ctx, &len, val);
    if (cstr == null) return null;
    return cstr[0..len];
}

/// Free string argument
fn freeStringArg(ctx: ?*JSContext, str: []const u8) void {
    qjs.JS_FreeCString(ctx, str.ptr);
}

/// Resolve path relative to cwd
fn resolvePath(allocator: Allocator, path: []const u8) ![]u8 {
    if (path.len == 0) return try allocator.dupe(u8, global_cwd);

    // Absolute path
    if (path[0] == '/') {
        return try allocator.dupe(u8, path);
    }

    // Relative path - join with cwd
    if (std.mem.endsWith(u8, global_cwd, "/")) {
        return try std.fmt.allocPrint(allocator, "{s}{s}", .{ global_cwd, path });
    } else {
        return try std.fmt.allocPrint(allocator, "{s}/{s}", .{ global_cwd, path });
    }
}

/// Throw JS error from Zig error
fn throwErrno(ctx: ?*JSContext, syscall: [:0]const u8, err: anyerror) JSValue {
    const code = errorToCode(err);
    const msg = errorToMessage(err);

    // Create Error object with code property
    var buf: [256]u8 = undefined;
    const formatted = std.fmt.bufPrint(&buf, "{s}: {s}, {s}", .{ code, msg, syscall }) catch
        return qjs.JS_ThrowInternalError(ctx, "error formatting failed");

    const err_val = qjs.JS_NewError(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, err_val, "message", qjs.JS_NewStringLen(ctx, formatted.ptr, formatted.len));
    _ = qjs.JS_SetPropertyStr(ctx, err_val, "code", qjs.JS_NewString(ctx, code.ptr));
    _ = qjs.JS_SetPropertyStr(ctx, err_val, "syscall", qjs.JS_NewString(ctx, syscall.ptr));

    return qjs.JS_Throw(ctx, err_val);
}

/// Convert Zig error to Node.js error code
fn errorToCode(err: anyerror) [:0]const u8 {
    return switch (err) {
        error.FileNotFound => "ENOENT",
        error.AccessDenied => "EACCES",
        error.IsDir => "EISDIR",
        error.NotDir => "ENOTDIR",
        error.FileTooBig => "EFBIG",
        error.NoSpaceLeft => "ENOSPC",
        error.PathAlreadyExists => "EEXIST",
        error.DirNotEmpty => "ENOTEMPTY",
        error.BrokenPipe => "EPIPE",
        error.ConnectionRefused => "ECONNREFUSED",
        error.ConnectionResetByPeer => "ECONNRESET",
        error.NetworkUnreachable => "ENETUNREACH",
        error.TimedOut => "ETIMEDOUT",
        else => "EIO",
    };
}

/// Convert Zig error to human-readable message
fn errorToMessage(err: anyerror) [:0]const u8 {
    return switch (err) {
        error.FileNotFound => "no such file or directory",
        error.AccessDenied => "permission denied",
        error.IsDir => "is a directory",
        error.NotDir => "not a directory",
        error.FileTooBig => "file too large",
        error.NoSpaceLeft => "no space left on device",
        error.PathAlreadyExists => "file already exists",
        error.DirNotEmpty => "directory not empty",
        error.BrokenPipe => "broken pipe",
        error.ConnectionRefused => "connection refused",
        error.ConnectionResetByPeer => "connection reset by peer",
        error.NetworkUnreachable => "network unreachable",
        error.TimedOut => "operation timed out",
        else => "input/output error",
    };
}

// ============================================================================
// Network Bindings
// ============================================================================

/// Native fetch implementation (DEPRECATED - use WAMR-based edgebox CLI instead)
/// This legacy binding is stubbed out. Use `zig build cli` for the full CLI with HTTP support.
fn nativeFetch(ctx: ?*JSContext, _: JSValue, _: c_int, _: [*c]JSValue) callconv(.c) JSValue {
    return qjs.JS_ThrowInternalError(ctx, "fetch not available in legacy CLI - use WAMR-based edgebox");
}
