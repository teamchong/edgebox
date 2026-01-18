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

// POSIX access function
const posix = struct {
    extern fn access(path: [*:0]const u8, mode: c_int) c_int;
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

/// Register fs native functions
/// This enhances the existing JS fs module with native implementations
pub fn register(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Get _modules.fs
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (qjs.JS_IsUndefined(modules_val)) {
        return;
    }
    defer qjs.JS_FreeValue(ctx, modules_val);

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

    // Create fs.constants object
    const constants = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, constants, "F_OK", qjs.JS_NewInt32(ctx, F_OK));
    _ = qjs.JS_SetPropertyStr(ctx, constants, "R_OK", qjs.JS_NewInt32(ctx, R_OK));
    _ = qjs.JS_SetPropertyStr(ctx, constants, "W_OK", qjs.JS_NewInt32(ctx, W_OK));
    _ = qjs.JS_SetPropertyStr(ctx, constants, "X_OK", qjs.JS_NewInt32(ctx, X_OK));
    _ = qjs.JS_SetPropertyStr(ctx, fs_mod, "constants", constants);
}
