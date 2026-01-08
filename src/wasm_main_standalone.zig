/// Standalone WASM Entry Point
/// For WASI runtimes (wasmtime, wasmer, etc.) without WAMR host
///
/// Build: zig build wasm-standalone -Dbytecode=path/to/bytecode.bin
///
const std = @import("std");
const builtin = @import("builtin");

// QuickJS C API
const qjs = @cImport({
    @cDefine("CONFIG_VERSION", "\"2024-02-14\"");
    @cDefine("CONFIG_BIGNUM", "1");
    @cInclude("quickjs.h");
    @cInclude("quickjs-libc.h");
});

// Embedded bytecode - provided by build system
const bytecode_data = @import("bytecode_data");
const bytecode: []const u8 = bytecode_data.data;

// Import frozen_init_c from frozen_functions.c
extern fn frozen_init_c(ctx: *qjs.JSContext) callconv(.c) c_int;

pub fn main() !void {
    // Initialize QuickJS runtime
    const rt = qjs.JS_NewRuntime() orelse return error.RuntimeInitFailed;
    defer qjs.JS_FreeRuntime(rt);

    // Set memory limits
    qjs.JS_SetMemoryLimit(rt, 512 * 1024 * 1024); // 512MB max
    qjs.JS_SetMaxStackSize(rt, 64 * 1024 * 1024); // 64MB stack

    // Create context
    const ctx = qjs.JS_NewContext(rt) orelse return error.ContextInitFailed;
    defer qjs.JS_FreeContext(ctx);

    // Add QuickJS standard helpers (console, print, etc.)
    qjs.js_std_add_helpers(ctx, 0, null);

    // Initialize polyfills (performance.now, etc.)
    initPolyfills(ctx);

    // Initialize frozen functions BEFORE loading bytecode
    // This registers __frozen_NAME functions in globalThis
    _ = frozen_init_c(ctx);

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

fn initPolyfills(ctx: *qjs.JSContext) void {
    const polyfill_js =
        \\if (typeof performance === 'undefined') {
        \\    globalThis.performance = { now: function() { return Date.now(); } };
        \\}
        \\if (typeof process === 'undefined') {
        \\    globalThis.process = { exit: function(code) { std.exit(code); } };
        \\}
    ;

    const val = qjs.JS_Eval(
        ctx,
        polyfill_js.ptr,
        polyfill_js.len,
        "<polyfill>",
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
