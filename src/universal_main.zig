/// Universal Runtime Entry Point
/// Compiles to both native binaries and WASM modules
/// Embedded bytecode is executed on startup
const std = @import("std");
const builtin = @import("builtin");

// Use direct C import for QuickJS
const qjs = @cImport({
    @cInclude("quickjs.h");
    @cInclude("quickjs-libc.h");
});

// Embedded bytecode - provided by build system via -Dbytecode=...
const bytecode_data = @import("bytecode_data");
const bytecode: []const u8 = bytecode_data.data;

const is_wasm = builtin.cpu.arch == .wasm32;

pub fn main() !void {
    // Initialize QuickJS runtime
    const rt = qjs.JS_NewRuntime() orelse return error.RuntimeInitFailed;
    defer qjs.JS_FreeRuntime(rt);

    // Set memory limits
    qjs.JS_SetMemoryLimit(rt, 512 * 1024 * 1024); // 512MB max
    qjs.JS_SetMaxStackSize(rt, 8 * 1024 * 1024); // 8MB stack

    // Create context
    const ctx = qjs.JS_NewContext(rt) orelse return error.ContextInitFailed;
    defer qjs.JS_FreeContext(ctx);

    // Add QuickJS standard helpers (console, print, etc.)
    qjs.js_std_add_helpers(ctx, 0, null);

    // Register basic globals
    registerGlobals(ctx);

    // Load and execute embedded bytecode
    const func = qjs.JS_ReadObject(
        ctx,
        bytecode.ptr,
        bytecode.len,
        qjs.JS_READ_OBJ_BYTECODE,
    );

    if (qjs.JS_IsException(func)) {
        printException(ctx);
        return error.BytecodeLoadFailed;
    }

    const result = qjs.JS_EvalFunction(ctx, func);
    if (qjs.JS_IsException(result)) {
        printException(ctx);
        return error.EvalFailed;
    }
    qjs.JS_FreeValue(ctx, result);

    // Run any pending async jobs
    while (true) {
        var pctx: ?*qjs.JSContext = undefined;
        const pending = qjs.JS_ExecutePendingJob(rt, &pctx);
        if (pending <= 0) break;
    }
}

fn registerGlobals(ctx: *qjs.JSContext) void {
    const globals_js =
        \\// EdgeBox info
        \\globalThis.__edgebox = {
        \\    version: '0.1.0',
        \\    runtime: 'quickjs-native',
        \\    platform: 'native'
        \\};
        \\
        \\// setTimeout/setInterval stubs (synchronous)
        \\if (typeof setTimeout === 'undefined') {
        \\    let timerId = 0;
        \\    globalThis.setTimeout = function(fn, ms) {
        \\        Promise.resolve().then(fn);
        \\        return ++timerId;
        \\    };
        \\    globalThis.clearTimeout = function() {};
        \\    globalThis.setInterval = function(fn, ms) { return ++timerId; };
        \\    globalThis.clearInterval = function() {};
        \\}
        \\
        \\// performance.now() for timing
        \\if (typeof performance === 'undefined') {
        \\    globalThis.performance = {
        \\        now: function() { return Date.now(); }
        \\    };
        \\}
    ;

    const val = qjs.JS_Eval(
        ctx,
        globals_js.ptr,
        globals_js.len,
        "<globals>",
        qjs.JS_EVAL_TYPE_GLOBAL,
    );
    if (!qjs.JS_IsException(val)) {
        qjs.JS_FreeValue(ctx, val);
    }
}

fn printException(ctx: *qjs.JSContext) void {
    const exception = qjs.JS_GetException(ctx);
    defer qjs.JS_FreeValue(ctx, exception);

    const str = qjs.JS_ToCString(ctx, exception);
    if (str != null) {
        std.debug.print("Exception: {s}\n", .{str});
        qjs.JS_FreeCString(ctx, str);
    }

    // Print stack trace if available
    const stack = qjs.JS_GetPropertyStr(ctx, exception, "stack");
    if (!qjs.JS_IsUndefined(stack)) {
        const stack_str = qjs.JS_ToCString(ctx, stack);
        if (stack_str != null) {
            std.debug.print("{s}\n", .{stack_str});
            qjs.JS_FreeCString(ctx, stack_str);
        }
        qjs.JS_FreeValue(ctx, stack);
    }
}
