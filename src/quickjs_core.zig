/// QuickJS Core Bindings (WASM compatible)
/// Includes quickjs-libc which has __wasi__ support
const std = @import("std");
const Allocator = std.mem.Allocator;

// QuickJS C API bindings
const qjs = @cImport({
    @cDefine("CONFIG_VERSION", "\"2024-02-14\"");
    @cDefine("CONFIG_BIGNUM", "1");
    @cInclude("quickjs.h");
    @cInclude("quickjs-libc.h");
});

pub const Error = error{
    RuntimeCreateFailed,
    ContextCreateFailed,
    EvalFailed,
    CompileFailed,
    ParseFailed,
    TypeError,
    ReferenceError,
    SyntaxError,
    RangeError,
    OutOfMemory,
};

/// QuickJS Runtime wrapper
pub const Runtime = struct {
    inner: *qjs.JSRuntime,
    allocator: Allocator,

    pub fn init(allocator: Allocator) !Runtime {
        const rt = qjs.JS_NewRuntime() orelse return Error.RuntimeCreateFailed;
        return .{ .inner = rt, .allocator = allocator };
    }

    pub fn deinit(self: *Runtime) void {
        qjs.JS_FreeRuntime(self.inner);
    }

    pub fn newContext(self: *Runtime) !Context {
        const ctx = qjs.JS_NewContext(self.inner) orelse return Error.ContextCreateFailed;
        return .{ .inner = ctx, .runtime = self };
    }

    /// Create context with std module (console, print, etc.)
    pub fn newStdContext(self: *Runtime) !Context {
        return self.newStdContextWithArgs(0, null);
    }

    /// Create context with std module and command line arguments
    pub fn newStdContextWithArgs(self: *Runtime, argc: c_int, argv: [*c][*c]u8) !Context {
        const ctx = qjs.JS_NewContext(self.inner) orelse return Error.ContextCreateFailed;
        // Add std and os modules with command line arguments
        qjs.js_std_add_helpers(ctx, argc, argv);
        _ = qjs.js_init_module_std(ctx, "std");
        _ = qjs.js_init_module_os(ctx, "os");
        return .{ .inner = ctx, .runtime = self };
    }

    pub fn setMemoryLimit(self: *Runtime, limit: usize) void {
        qjs.JS_SetMemoryLimit(self.inner, limit);
    }

    pub fn setMaxStackSize(self: *Runtime, size: usize) void {
        qjs.JS_SetMaxStackSize(self.inner, size);
    }

    pub fn runGC(self: *Runtime) void {
        qjs.JS_RunGC(self.inner);
    }
};

/// Re-export QuickJS types for native bindings
pub const JSContext = qjs.JSContext;
pub const JSValue = qjs.JSValue;
pub const c = qjs; // Full access to QuickJS C API

/// QuickJS C function callback type
pub const JSCFunction = *const fn (?*qjs.JSContext, qjs.JSValue, c_int, [*c]qjs.JSValue) callconv(.c) qjs.JSValue;

/// QuickJS Context wrapper
pub const Context = struct {
    inner: *qjs.JSContext,
    runtime: *Runtime,

    pub fn deinit(self: *Context) void {
        qjs.JS_FreeContext(self.inner);
    }

    /// Get the raw context pointer for native bindings
    pub fn getRaw(self: *Context) *qjs.JSContext {
        return self.inner;
    }

    /// Register a native C function as a global
    pub fn registerGlobalFunction(self: *Context, name: [:0]const u8, func: JSCFunction, arg_count: u8) void {
        const global = qjs.JS_GetGlobalObject(self.inner);
        defer qjs.JS_FreeValue(self.inner, global);

        const func_val = qjs.JS_NewCFunction(self.inner, func, name.ptr, arg_count);
        _ = qjs.JS_SetPropertyStr(self.inner, global, name.ptr, func_val);
    }

    pub fn eval(self: *Context, code: []const u8) !Value {
        const result = qjs.JS_Eval(
            self.inner,
            code.ptr,
            code.len,
            "<eval>",
            qjs.JS_EVAL_TYPE_GLOBAL,
        );

        if (qjs.JS_IsException(result)) {
            // Get and print exception
            const exc = qjs.JS_GetException(self.inner);
            defer qjs.JS_FreeValue(self.inner, exc);
            return Error.EvalFailed;
        }

        return .{ .inner = result, .context = self };
    }

    pub fn evalWithFilename(self: *Context, code: []const u8, filename: [:0]const u8) !Value {
        const result = qjs.JS_Eval(
            self.inner,
            code.ptr,
            code.len,
            filename.ptr,
            qjs.JS_EVAL_TYPE_GLOBAL,
        );

        if (qjs.JS_IsException(result)) {
            return Error.EvalFailed;
        }

        return .{ .inner = result, .context = self };
    }
};

/// JavaScript Value wrapper
pub const Value = struct {
    inner: qjs.JSValue,
    context: *Context,

    pub fn free(self: Value) void {
        qjs.JS_FreeValue(self.context.inner, self.inner);
    }

    pub fn isUndefined(self: Value) bool {
        return qjs.JS_IsUndefined(self.inner);
    }

    pub fn isNull(self: Value) bool {
        return qjs.JS_IsNull(self.inner);
    }

    pub fn isBool(self: Value) bool {
        return qjs.JS_IsBool(self.inner);
    }

    pub fn isNumber(self: Value) bool {
        return qjs.JS_IsNumber(self.inner);
    }

    pub fn isString(self: Value) bool {
        return qjs.JS_IsString(self.inner);
    }

    pub fn toInt32(self: Value) ?i32 {
        var val: i32 = 0;
        if (qjs.JS_ToInt32(self.context.inner, &val, self.inner) < 0) {
            return null;
        }
        return val;
    }

    pub fn toFloat64(self: Value) ?f64 {
        var val: f64 = 0;
        if (qjs.JS_ToFloat64(self.context.inner, &val, self.inner) < 0) {
            return null;
        }
        return val;
    }

    pub fn toStringSlice(self: Value) ?[]const u8 {
        var len: usize = 0;
        const ptr = qjs.JS_ToCStringLen(self.context.inner, &len, self.inner);
        if (ptr == null) return null;
        return ptr[0..len];
    }

    pub fn toString(self: Value, allocator: Allocator) ![]u8 {
        const slice = self.toStringSlice() orelse return Error.TypeError;
        return allocator.dupe(u8, slice);
    }
};
