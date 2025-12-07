/// Native Bindings for EdgeBox
///
/// Implements native functions exposed to JavaScript via QuickJS.
/// These functions bridge JS calls to WASI syscalls for fs, crypto, etc.
///
/// Strategy (like wasmedge-quickjs):
/// - JS polyfills in node_compat.zig call these __edgebox_* functions
/// - These functions are registered as C functions in QuickJS
/// - They use WASI syscalls internally for actual operations
const std = @import("std");
const Allocator = std.mem.Allocator;
const fetch_impl = @import("fetch.zig");

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

    // Process bindings
    registerFunc(ctx, global, "__edgebox_cwd", getCwd, 0);

    // Crypto bindings
    registerFunc(ctx, global, "__edgebox_random_bytes", randomBytes, 1);

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

    // Open and read file
    const file = std.fs.cwd().openFile(resolved, .{}) catch |err| {
        return throwErrno(ctx, "open", err);
    };
    defer file.close();

    const content = file.readToEndAlloc(allocator, 100 * 1024 * 1024) catch |err| {
        return throwErrno(ctx, "read", err);
    };
    defer allocator.free(content);

    return qjs.JS_NewStringLen(ctx, content.ptr, content.len);
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

    // Mode: file type bits
    const is_dir: bool = stat.kind == .directory;
    const mode: i32 = if (is_dir) 0o40755 else 0o100644;
    _ = qjs.JS_SetPropertyStr(ctx, obj, "mode", qjs.JS_NewInt32(ctx, mode));

    // isFile(), isDirectory() methods - use simple boolean properties
    _ = qjs.JS_SetPropertyStr(ctx, obj, "_isDir", jsBool(is_dir));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "_isFile", jsBool(!is_dir));

    // Add isFile/isDirectory as functions via eval
    const methods_code =
        \\(function(obj) {
        \\    obj.isFile = function() { return this._isFile; };
        \\    obj.isDirectory = function() { return this._isDir; };
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

    // Timestamps (as milliseconds since epoch)
    const mtime_ms: i64 = @intCast(@divFloor(stat.mtime, std.time.ns_per_ms));
    const atime_ms: i64 = @intCast(@divFloor(stat.atime, std.time.ns_per_ms));
    const ctime_ms: i64 = @intCast(@divFloor(stat.ctime, std.time.ns_per_ms));

    _ = qjs.JS_SetPropertyStr(ctx, obj, "mtimeMs", qjs.JS_NewInt64(ctx, mtime_ms));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "atimeMs", qjs.JS_NewInt64(ctx, atime_ms));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "ctimeMs", qjs.JS_NewInt64(ctx, ctime_ms));

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

    // Copy file
    std.fs.cwd().copyFile(resolved_src, std.fs.cwd(), resolved_dest, .{}) catch |err| {
        return throwErrno(ctx, "copyfile", err);
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

/// Native fetch implementation
/// Args: url, method, headers_json, body
fn nativeFetch(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fetch requires url argument");

    const url = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "url must be a string");
    defer freeStringArg(ctx, url);

    // Get method (default GET)
    const method = if (argc >= 2) getStringArg(ctx, argv[1]) orelse "GET" else "GET";
    const method_owned = argc >= 2 and getStringArg(ctx, argv[1]) != null;
    defer if (method_owned) freeStringArg(ctx, method);

    // Get headers JSON (optional)
    const headers_json = if (argc >= 3 and !qjs.JS_IsUndefined(argv[2]) and !qjs.JS_IsNull(argv[2]))
        getStringArg(ctx, argv[2])
    else
        null;
    defer if (headers_json) |h| freeStringArg(ctx, h);

    // Get body (optional)
    const body = if (argc >= 4 and !qjs.JS_IsUndefined(argv[3]) and !qjs.JS_IsNull(argv[3]))
        getStringArg(ctx, argv[3])
    else
        null;
    defer if (body) |b| freeStringArg(ctx, b);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Perform fetch
    var response = fetch_impl.jsFetch(allocator, url, method, headers_json, body) catch |err| {
        return switch (err) {
            fetch_impl.FetchError.InvalidUrl => qjs.JS_ThrowTypeError(ctx, "Invalid URL"),
            fetch_impl.FetchError.ConnectionFailed => qjs.JS_ThrowInternalError(ctx, "Connection failed"),
            fetch_impl.FetchError.TlsError => qjs.JS_ThrowInternalError(ctx, "TLS error"),
            fetch_impl.FetchError.Timeout => qjs.JS_ThrowInternalError(ctx, "Request timed out"),
            fetch_impl.FetchError.InvalidResponse => qjs.JS_ThrowInternalError(ctx, "Invalid HTTP response"),
            fetch_impl.FetchError.OutOfMemory => qjs.JS_ThrowInternalError(ctx, "Out of memory"),
        };
    };
    defer response.deinit();

    // Create response object
    const obj = qjs.JS_NewObject(ctx);

    // Set status
    _ = qjs.JS_SetPropertyStr(ctx, obj, "status", qjs.JS_NewInt32(ctx, @intCast(response.status)));

    // Set ok (status 200-299)
    _ = qjs.JS_SetPropertyStr(ctx, obj, "ok", jsBool(response.status >= 200 and response.status < 300));

    // Set body as string
    _ = qjs.JS_SetPropertyStr(ctx, obj, "body", qjs.JS_NewStringLen(ctx, response.body.ptr, response.body.len));

    // Set headers as object
    const headers_obj = qjs.JS_NewObject(ctx);
    var iter = response.headers.iterator();
    while (iter.next()) |entry| {
        _ = qjs.JS_SetPropertyStr(
            ctx,
            headers_obj,
            @ptrCast(entry.key_ptr.*.ptr),
            qjs.JS_NewStringLen(ctx, entry.value_ptr.*.ptr, entry.value_ptr.*.len),
        );
    }
    _ = qjs.JS_SetPropertyStr(ctx, obj, "headers", headers_obj);

    return obj;
}
