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
}
