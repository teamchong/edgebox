/// Native process module - QuickJS C functions
/// Registered ONCE at WASM init via inline for loop
/// Zero runtime overhead, allocation-free implementations
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

/// process.cwd() - Get current working directory
fn processCwd(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    // In WASM, always return root directory
    return qjs.JS_NewString(ctx, "/");
}

/// process.exit(code) - Exit with status code
fn processExit(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    var exit_code: i32 = 0;
    if (argc > 0) {
        _ = qjs.JS_ToInt32(ctx, &exit_code, argv[0]);
    }

    // In WASM, we can't actually exit, but we can throw an exception
    // that the host can catch to stop execution
    const msg_buf: [64]u8 = undefined;
    var msg = msg_buf;
    const msg_str = std.fmt.bufPrint(&msg, "process.exit({d})", .{exit_code}) catch "process.exit";

    return qjs.JS_ThrowInternalError(ctx, "%s", msg_str.ptr);
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

    // Call Promise.resolve()
    const resolved = qjs.JS_Call(ctx, resolve_func, promise_ctor, 0, null);
    defer qjs.JS_FreeValue(ctx, resolved);

    // Get .then method
    const then_func = qjs.JS_GetPropertyStr(ctx, resolved, "then");
    defer qjs.JS_FreeValue(ctx, then_func);

    // Call .then(callback)
    var args = [1]qjs.JSValue{argv[0]};
    const result = qjs.JS_Call(ctx, then_func, resolved, 1, &args);

    return result;
}

/// Register all process functions and properties to globalThis.process
/// Called ONCE at WASM initialization
pub fn register(ctx: *qjs.JSContext) void {
    const process_obj = qjs.JS_NewObject(ctx);

    // Properties
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "platform", qjs.JS_NewString(ctx, "wasm32"));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "arch", qjs.JS_NewString(ctx, "wasm32"));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "version", qjs.JS_NewString(ctx, "v20.0.0"));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "pid", qjs.JS_NewInt32(ctx, 1));
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "ppid", qjs.JS_NewInt32(ctx, 0));

    // Create env object - will be populated by runtime
    const env_obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "env", env_obj);

    // Create argv array
    const argv_obj = qjs.JS_NewArray(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, process_obj, "argv", argv_obj);

    // Register all functions at once - zero runtime cost
    inline for (.{
        .{ "cwd", processCwd, 0 },
        .{ "exit", processExit, 1 },
        .{ "nextTick", processNextTick, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, process_obj, binding[0], func);
    }

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
