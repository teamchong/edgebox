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
    SerializationFailed,
};

/// QuickJS Runtime wrapper
pub const Runtime = struct {
    inner: *qjs.JSRuntime,
    allocator: Allocator,
    use_pool_allocator: bool = false,

    /// Initialize with default allocator
    pub fn init(allocator: Allocator) !Runtime {
        const rt = qjs.JS_NewRuntime() orelse return Error.RuntimeCreateFailed;
        return .{ .inner = rt, .allocator = allocator, .use_pool_allocator = false };
    }

    /// Initialize with WASM bump allocator for O(1) malloc
    /// This is a fast bump allocator - free is NO-OP, memory reclaimed at exit
    pub fn initWithPoolAllocator(allocator: Allocator) !Runtime {
        const wasm_bump = @import("wasm_bump.zig");

        // Initialize the bump allocator
        wasm_bump.init();

        const malloc_funcs = qjs.JSMallocFunctions{
            .js_calloc = wasm_bump.js_calloc,
            .js_malloc = wasm_bump.js_malloc,
            .js_free = wasm_bump.js_free,
            .js_realloc = wasm_bump.js_realloc,
            .js_malloc_usable_size = wasm_bump.js_malloc_usable_size,
        };

        const rt = qjs.JS_NewRuntime2(&malloc_funcs, null) orelse return Error.RuntimeCreateFailed;
        return .{ .inner = rt, .allocator = allocator, .use_pool_allocator = true };
    }

    pub fn deinit(self: *Runtime) void {
        qjs.JS_FreeRuntime(self.inner);
    }

    /// Reset the pool allocator (call at request end for O(1) bulk cleanup)
    /// Only effective if initialized with initWithPoolAllocator()
    pub fn resetPoolAllocator(self: *Runtime) void {
        if (self.use_pool_allocator) {
            const pool_alloc = @import("wasm_pool_alloc.zig");
            pool_alloc.resetGlobalPool();
        }
    }

    /// Get pool allocator statistics (allocation count, reuse rate, etc.)
    pub fn getPoolStats(self: *const Runtime) ?@import("wasm_pool_alloc.zig").WasmPoolAllocator.Stats {
        if (self.use_pool_allocator) {
            const pool_alloc = @import("wasm_pool_alloc.zig");
            return pool_alloc.getGlobalPoolStats();
        }
        return null;
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
    // For contexts created from raw pointers (no runtime ownership)
    is_raw: bool = false,

    /// Create a Context from a raw JSContext pointer (for callbacks)
    pub fn fromRaw(ctx: ?*qjs.JSContext, allocator: std.mem.Allocator) Context {
        _ = allocator;
        return .{
            .inner = ctx.?,
            .runtime = undefined, // Not owned
            .is_raw = true,
        };
    }

    pub fn deinit(self: *Context) void {
        if (!self.is_raw) {
            qjs.JS_FreeContext(self.inner);
        }
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
        // Use JS_DefinePropertyValueStr with CONFIGURABLE | WRITABLE | ENUMERABLE flags
        // This ensures the property is visible and accessible
        const result = qjs.JS_DefinePropertyValueStr(
            self.inner,
            global,
            name.ptr,
            func_val,
            qjs.JS_PROP_C_W_E, // Configurable, Writable, Enumerable
        );
        // Returns -1 on error, 0 on failure, 1 on success
        if (result <= 0) {
            std.debug.print("[ZIG] Failed to register global function: {s}, result={d}\n", .{ name, result });
        }
    }

    /// Get the current exception (if any)
    pub fn getException(self: *Context) ?Value {
        const exc = qjs.JS_GetException(self.inner);
        if (qjs.JS_IsNull(exc) or qjs.JS_IsUndefined(exc)) {
            qjs.JS_FreeValue(self.inner, exc);
            return null;
        }
        return .{ .inner = exc, .context = self };
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

    /// Evaluate code as an ES module (allows import/export)
    pub fn evalModule(self: *Context, code: []const u8, filename: [:0]const u8) !Value {
        const result = qjs.JS_Eval(
            self.inner,
            code.ptr,
            code.len,
            filename.ptr,
            qjs.JS_EVAL_TYPE_MODULE,
        );

        if (qjs.JS_IsException(result)) {
            return Error.EvalFailed;
        }

        return .{ .inner = result, .context = self };
    }

    // ========================================================================
    // Snapshot/State Serialization Methods
    // ========================================================================

    /// Serialize the global object state (for snapshots)
    /// This captures the entire global scope including all defined variables,
    /// functions, and object graphs. Used for zero-latency cold starts.
    pub fn serializeGlobalState(self: *Context, allocator: Allocator) ![]u8 {
        const global = qjs.JS_GetGlobalObject(self.inner);
        defer qjs.JS_FreeValue(self.inner, global);

        var out_len: usize = undefined;
        const flags = SerializeFlags.BYTECODE | SerializeFlags.REFERENCE | SerializeFlags.STRIP_SOURCE;
        const data = qjs.JS_WriteObject(
            self.inner,
            &out_len,
            global,
            flags,
        );

        if (data == null) {
            return Error.CompileFailed;
        }

        // Copy to Zig-managed memory
        const result = try allocator.alloc(u8, out_len);
        @memcpy(result, data[0..out_len]);
        qjs.js_free(self.inner, @constCast(@ptrCast(data)));

        return result;
    }

    /// Deserialize an object from snapshot data
    pub fn deserializeValue(self: *Context, data: []const u8) !Value {
        const flags = SerializeFlags.BYTECODE | SerializeFlags.REFERENCE;
        const obj = qjs.JS_ReadObject(
            self.inner,
            data.ptr,
            data.len,
            flags,
        );

        if (qjs.JS_IsException(obj)) {
            return Error.ParseFailed;
        }

        return .{ .inner = obj, .context = self };
    }

    /// Restore global state from serialized data
    /// Merges the deserialized object properties into the current global object
    pub fn restoreGlobalState(self: *Context, data: []const u8) !void {
        // Deserialize the saved global object
        const restored = try self.deserializeValue(data);
        defer restored.free();

        // Copy all enumerable properties from restored to current global via JS
        const merge_script =
            \\(function(src) {
            \\    var names = Object.getOwnPropertyNames(src);
            \\    for (var i = 0; i < names.length; i++) {
            \\        var key = names[i];
            \\        if (key !== 'globalThis' && key !== 'global') {
            \\            try {
            \\                globalThis[key] = src[key];
            \\            } catch(e) {}
            \\        }
            \\    }
            \\})
        ;

        const merge_fn = self.eval(merge_script) catch return Error.EvalFailed;
        defer merge_fn.free();

        // Call the merge function with restored object
        var args = [_]qjs.JSValue{restored.inner};
        const call_result = qjs.JS_Call(
            self.inner,
            merge_fn.inner,
            qjs.JS_UNDEFINED,
            1,
            &args,
        );

        if (qjs.JS_IsException(call_result)) {
            return Error.EvalFailed;
        }
        qjs.JS_FreeValue(self.inner, call_result);
    }

    /// Compile JavaScript to bytecode
    pub fn compile(self: *Context, code: []const u8, filename: [:0]const u8, allocator: Allocator) ![]u8 {
        // First evaluate to get function
        const func = qjs.JS_Eval(
            self.inner,
            code.ptr,
            code.len,
            filename.ptr,
            qjs.JS_EVAL_FLAG_COMPILE_ONLY,
        );

        if (qjs.JS_IsException(func)) {
            return Error.CompileFailed;
        }
        defer qjs.JS_FreeValue(self.inner, func);

        // Write bytecode
        var out_len: usize = undefined;
        const bytecode = qjs.JS_WriteObject(
            self.inner,
            &out_len,
            func,
            qjs.JS_WRITE_OBJ_BYTECODE,
        );

        if (bytecode == null) {
            return Error.CompileFailed;
        }

        // Copy to Zig-managed memory
        const result = try allocator.alloc(u8, out_len);
        @memcpy(result, bytecode[0..out_len]);
        qjs.js_free(self.inner, @constCast(@ptrCast(bytecode)));

        return result;
    }

    /// Load and run bytecode
    pub fn loadBytecode(self: *Context, bytecode: []const u8) !Value {
        const func = qjs.JS_ReadObject(
            self.inner,
            bytecode.ptr,
            bytecode.len,
            qjs.JS_READ_OBJ_BYTECODE,
        );

        if (qjs.JS_IsException(func)) {
            return Error.ParseFailed;
        }

        const result = qjs.JS_EvalFunction(self.inner, func);
        if (qjs.JS_IsException(result)) {
            return Error.EvalFailed;
        }

        return .{ .inner = result, .context = self };
    }
};

// ============================================================================
// Snapshot/State Serialization (for Zero-Latency Cold Starts)
// ============================================================================

/// Flags for state serialization
pub const SerializeFlags = struct {
    pub const BYTECODE: c_int = 1 << 0; // JS_WRITE_OBJ_BYTECODE
    pub const SAB: c_int = 1 << 2; // SharedArrayBuffer
    pub const REFERENCE: c_int = 1 << 3; // Object references (graphs)
    pub const STRIP_SOURCE: c_int = 1 << 4; // No source code
    pub const STRIP_DEBUG: c_int = 1 << 5; // No debug info
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

// ============================================================================
// Zero-Copy ArrayBuffer Support
// ============================================================================

/// Global allocator for zero-copy ArrayBuffer free callbacks
/// Must be set before using newArrayBufferZeroCopy
var zero_copy_allocator: ?Allocator = null;

/// Set the allocator used for zero-copy ArrayBuffer deallocation
pub fn setZeroCopyAllocator(allocator: Allocator) void {
    zero_copy_allocator = allocator;
}

/// Callback for QuickJS to free zero-copy ArrayBuffer data
/// Called when the ArrayBuffer is garbage collected
fn zeroCopyFreeCallback(rt: ?*anyopaque, user_data: ?*anyopaque, ptr: ?*anyopaque) callconv(.c) void {
    _ = rt;
    if (ptr == null) return;

    // Get the length from user_data (we store it there)
    const len = @intFromPtr(user_data);
    if (len == 0) return; // No-op for bump allocator mode

    // Use global allocator to free
    if (zero_copy_allocator) |allocator| {
        const slice: []u8 = @as([*]u8, @ptrCast(ptr.?))[0..len];
        allocator.free(slice);
    }
}

/// No-op free callback for bump allocator (memory reclaimed at arena reset)
fn zeroCopyNoopFree(rt: ?*anyopaque, user_data: ?*anyopaque, ptr: ?*anyopaque) callconv(.c) void {
    _ = rt;
    _ = user_data;
    _ = ptr;
    // No-op: bump allocator reclaims all memory at once
}

/// Create a zero-copy ArrayBuffer from existing data
/// The buffer ownership is transferred to QuickJS - do NOT free it yourself
/// For WASM bump allocator, use newArrayBufferZeroCopyNoFree instead
pub fn newArrayBufferZeroCopy(ctx: ?*qjs.JSContext, data: []u8) qjs.JSValue {
    return qjs.JS_NewArrayBuffer(
        ctx,
        data.ptr,
        data.len,
        zeroCopyFreeCallback,
        @ptrFromInt(data.len), // Store length in opaque for free callback
        false, // not shared
    );
}

/// Create a zero-copy ArrayBuffer without free callback
/// Use this for bump allocator where memory is bulk-freed at request end
pub fn newArrayBufferZeroCopyNoFree(ctx: ?*qjs.JSContext, data: []u8) qjs.JSValue {
    return qjs.JS_NewArrayBuffer(
        ctx,
        data.ptr,
        data.len,
        zeroCopyNoopFree,
        null,
        false, // not shared
    );
}

/// Create an ArrayBuffer with copy (when zero-copy isn't safe)
pub fn newArrayBufferCopy(ctx: ?*qjs.JSContext, data: []const u8) qjs.JSValue {
    return qjs.JS_NewArrayBufferCopy(ctx, data.ptr, data.len);
}
