/// QuickJS Zig Bindings
///
/// Provides a Zig-friendly interface to the QuickJS JavaScript engine.
/// Supports ES2023, modules, async/await, and BigInt.
///
/// Usage:
/// ```zig
/// const quickjs = @import("quickjs");
///
/// var rt = try quickjs.Runtime.init(allocator);
/// defer rt.deinit();
///
/// var ctx = try rt.newContext();
/// defer ctx.deinit();
///
/// const result = try ctx.eval("1 + 2");
/// std.debug.print("Result: {}\n", .{result.toInt32()});
/// ```
const std = @import("std");
const Allocator = std.mem.Allocator;

// QuickJS C API bindings
const qjs = @cImport({
    @cDefine("CONFIG_VERSION", "\"2024-02-14\"");
    @cDefine("CONFIG_BIGNUM", "1");
    @cInclude("quickjs.h");
    @cInclude("quickjs-libc.h");
});

/// QuickJS error types
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
    ModuleNotFound,
    ExportNotFound,
};

/// JavaScript value types
pub const ValueType = enum {
    undefined,
    null,
    bool,
    int,
    float,
    string,
    symbol,
    object,
    function,
    array,
    exception,
    bigint,
    bigfloat,
    bigdecimal,
};

/// QuickJS Value wrapper
pub const Value = struct {
    inner: qjs.JSValue,
    ctx: *qjs.JSContext,

    const Self = @This();

    /// Check if value is undefined
    pub fn isUndefined(self: Self) bool {
        return qjs.JS_IsUndefined(self.inner);
    }

    /// Check if value is null
    pub fn isNull(self: Self) bool {
        return qjs.JS_IsNull(self.inner);
    }

    /// Check if value is boolean
    pub fn isBool(self: Self) bool {
        return qjs.JS_IsBool(self.inner);
    }

    /// Check if value is number
    pub fn isNumber(self: Self) bool {
        return qjs.JS_IsNumber(self.inner);
    }

    /// Check if value is string
    pub fn isString(self: Self) bool {
        return qjs.JS_IsString(self.inner);
    }

    /// Check if value is object
    pub fn isObject(self: Self) bool {
        return qjs.JS_IsObject(self.inner);
    }

    /// Check if value is function
    pub fn isFunction(self: Self) bool {
        return qjs.JS_IsFunction(self.ctx, self.inner);
    }

    /// Check if value is array
    pub fn isArray(self: Self) bool {
        return qjs.JS_IsArray(self.ctx, self.inner);
    }

    /// Check if value is exception
    pub fn isException(self: Self) bool {
        return qjs.JS_IsException(self.inner);
    }

    /// Get value type
    pub fn getType(self: Self) ValueType {
        if (self.isUndefined()) return .undefined;
        if (self.isNull()) return .null;
        if (self.isBool()) return .bool;
        if (self.isNumber()) {
            // Distinguish int vs float
            var d: f64 = undefined;
            if (qjs.JS_ToFloat64(self.ctx, &d, self.inner) == 0) {
                if (@floor(d) == d and d >= -2147483648 and d <= 2147483647) {
                    return .int;
                }
            }
            return .float;
        }
        if (self.isString()) return .string;
        if (self.isException()) return .exception;
        if (self.isArray()) return .array;
        if (self.isFunction()) return .function;
        if (self.isObject()) return .object;
        return .undefined;
    }

    /// Convert to i32
    pub fn toInt32(self: Self) ?i32 {
        var result: i32 = undefined;
        if (qjs.JS_ToInt32(self.ctx, &result, self.inner) != 0) {
            return null;
        }
        return result;
    }

    /// Convert to i64
    pub fn toInt64(self: Self) ?i64 {
        var result: i64 = undefined;
        if (qjs.JS_ToInt64(self.ctx, &result, self.inner) != 0) {
            return null;
        }
        return result;
    }

    /// Convert to f64
    pub fn toFloat64(self: Self) ?f64 {
        var result: f64 = undefined;
        if (qjs.JS_ToFloat64(self.ctx, &result, self.inner) != 0) {
            return null;
        }
        return result;
    }

    /// Convert to bool
    pub fn toBool(self: Self) bool {
        return qjs.JS_ToBool(self.ctx, self.inner) != 0;
    }

    /// Convert to string (caller must free)
    pub fn toString(self: Self, allocator: Allocator) ![]u8 {
        var len: usize = undefined;
        const cstr = qjs.JS_ToCStringLen(self.ctx, &len, self.inner);
        if (cstr == null) return Error.TypeError;
        defer qjs.JS_FreeCString(self.ctx, cstr);

        const result = try allocator.alloc(u8, len);
        @memcpy(result, cstr[0..len]);
        return result;
    }

    /// Get string as slice (valid until value is freed)
    pub fn toStringSlice(self: Self) ?[]const u8 {
        var len: usize = undefined;
        const cstr = qjs.JS_ToCStringLen(self.ctx, &len, self.inner);
        if (cstr == null) return null;
        return cstr[0..len];
    }

    /// Get object property
    pub fn getProperty(self: Self, name: [:0]const u8) Value {
        return .{
            .inner = qjs.JS_GetPropertyStr(self.ctx, self.inner, name.ptr),
            .ctx = self.ctx,
        };
    }

    /// Set object property
    pub fn setProperty(self: Self, name: [:0]const u8, value: Value) void {
        _ = qjs.JS_SetPropertyStr(self.ctx, self.inner, name.ptr, value.inner);
    }

    /// Get array element
    pub fn getIndex(self: Self, index: u32) Value {
        return .{
            .inner = qjs.JS_GetPropertyUint32(self.ctx, self.inner, index),
            .ctx = self.ctx,
        };
    }

    /// Get array length
    pub fn getLength(self: Self) ?u32 {
        const len_val = self.getProperty("length");
        return @intCast(len_val.toInt32() orelse return null);
    }

    /// Call as function
    pub fn call(self: Self, this: ?Value, args: []const Value) !Value {
        var c_args: [16]qjs.JSValue = undefined;
        for (args, 0..) |arg, i| {
            c_args[i] = arg.inner;
        }

        const this_val = if (this) |t| t.inner else qjs.JS_UNDEFINED;
        const result = qjs.JS_Call(
            self.ctx,
            self.inner,
            this_val,
            @intCast(args.len),
            &c_args,
        );

        if (qjs.JS_IsException(result)) {
            return Error.EvalFailed;
        }

        return .{ .inner = result, .ctx = self.ctx };
    }

    /// Free value (decrement refcount)
    pub fn free(self: Self) void {
        qjs.JS_FreeValue(self.ctx, self.inner);
    }

    /// Duplicate value (increment refcount)
    pub fn dup(self: Self) Value {
        return .{
            .inner = qjs.JS_DupValue(self.ctx, self.inner),
            .ctx = self.ctx,
        };
    }
};

/// QuickJS Context - JS execution environment
pub const Context = struct {
    inner: *qjs.JSContext,
    allocator: Allocator,

    const Self = @This();

    /// Evaluate JavaScript code
    pub fn eval(self: *Self, code: []const u8) !Value {
        return self.evalWithFilename(code, "<eval>");
    }

    /// Evaluate JavaScript code with filename for error messages
    pub fn evalWithFilename(self: *Self, code: []const u8, filename: [:0]const u8) !Value {
        const result = qjs.JS_Eval(
            self.inner,
            code.ptr,
            code.len,
            filename.ptr,
            qjs.JS_EVAL_TYPE_GLOBAL,
        );

        if (qjs.JS_IsException(result)) {
            // Get exception details
            const exc = qjs.JS_GetException(self.inner);
            defer qjs.JS_FreeValue(self.inner, exc);

            // Log exception for debugging
            const str = qjs.JS_ToCString(self.inner, exc);
            if (str != null) {
                defer qjs.JS_FreeCString(self.inner, str);
                std.log.err("JavaScript error: {s}", .{str});
            }

            return Error.EvalFailed;
        }

        return .{ .inner = result, .ctx = self.inner };
    }

    /// Evaluate JavaScript module
    pub fn evalModule(self: *Self, code: []const u8, filename: [:0]const u8) !Value {
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

        return .{ .inner = result, .ctx = self.inner };
    }

    /// Compile JavaScript to bytecode
    pub fn compile(self: *Self, code: []const u8, filename: [:0]const u8) ![]u8 {
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
        const result = try self.allocator.alloc(u8, out_len);
        @memcpy(result, bytecode[0..out_len]);
        qjs.js_free(self.inner, @constCast(@ptrCast(bytecode)));

        return result;
    }

    /// Load and run bytecode
    pub fn loadBytecode(self: *Self, bytecode: []const u8) !Value {
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

        return .{ .inner = result, .ctx = self.inner };
    }

    // ========================================================================
    // Snapshot/State Serialization (for Zero-Latency Cold Starts)
    // ========================================================================

    /// Flags for state serialization
    pub const SerializeFlags = struct {
        pub const BYTECODE: c_int = 1 << 0; // JS_WRITE_OBJ_BYTECODE
        pub const SAB: c_int = 1 << 2; // SharedArrayBuffer
        pub const REFERENCE: c_int = 1 << 3; // Object references (graphs)
        pub const STRIP_SOURCE: c_int = 1 << 4; // No source code
        pub const STRIP_DEBUG: c_int = 1 << 5; // No debug info
    };

    /// Serialize the global object state (for snapshots)
    /// This captures the entire global scope including all defined variables,
    /// functions, and object graphs. Used for zero-latency cold starts.
    pub fn serializeGlobalState(self: *Self) ![]u8 {
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
        const result = try self.allocator.alloc(u8, out_len);
        @memcpy(result, data[0..out_len]);
        qjs.js_free(self.inner, @constCast(@ptrCast(data)));

        return result;
    }

    /// Serialize a specific object/value (for partial snapshots)
    pub fn serializeValue(self: *Self, value: Value) ![]u8 {
        var out_len: usize = undefined;
        const flags = SerializeFlags.BYTECODE | SerializeFlags.REFERENCE | SerializeFlags.STRIP_SOURCE;
        const data = qjs.JS_WriteObject(
            self.inner,
            &out_len,
            value.inner,
            flags,
        );

        if (data == null) {
            return Error.CompileFailed;
        }

        // Copy to Zig-managed memory
        const result = try self.allocator.alloc(u8, out_len);
        @memcpy(result, data[0..out_len]);
        qjs.js_free(self.inner, @constCast(@ptrCast(data)));

        return result;
    }

    /// Deserialize an object from snapshot data
    pub fn deserializeValue(self: *Self, data: []const u8) !Value {
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

        return .{ .inner = obj, .ctx = self.inner };
    }

    /// Restore global state from serialized data
    /// Merges the deserialized object properties into the current global object
    pub fn restoreGlobalState(self: *Self, data: []const u8) !void {
        // Deserialize the saved global object
        const restored = try self.deserializeValue(data);
        defer restored.free();

        // Get current global
        const current_global = qjs.JS_GetGlobalObject(self.inner);
        defer qjs.JS_FreeValue(self.inner, current_global);

        // Copy all enumerable properties from restored to current global
        // This is done via JS to handle property iteration properly
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

    /// Get global object
    pub fn getGlobal(self: *Self) Value {
        return .{
            .inner = qjs.JS_GetGlobalObject(self.inner),
            .ctx = self.inner,
        };
    }

    /// Create undefined value
    pub fn @"undefined"(self: *Self) Value {
        return .{ .inner = qjs.JS_UNDEFINED, .ctx = self.inner };
    }

    /// Create null value
    pub fn null_(self: *Self) Value {
        return .{ .inner = qjs.JS_NULL, .ctx = self.inner };
    }

    /// Create boolean value
    pub fn newBool(self: *Self, val: bool) Value {
        return .{
            .inner = qjs.JS_NewBool(self.inner, if (val) 1 else 0),
            .ctx = self.inner,
        };
    }

    /// Create integer value
    pub fn newInt32(self: *Self, val: i32) Value {
        return .{
            .inner = qjs.JS_NewInt32(self.inner, val),
            .ctx = self.inner,
        };
    }

    /// Create integer value (64-bit)
    pub fn newInt64(self: *Self, val: i64) Value {
        return .{
            .inner = qjs.JS_NewInt64(self.inner, val),
            .ctx = self.inner,
        };
    }

    /// Create float value
    pub fn newFloat64(self: *Self, val: f64) Value {
        return .{
            .inner = qjs.JS_NewFloat64(self.inner, val),
            .ctx = self.inner,
        };
    }

    /// Create string value
    pub fn newString(self: *Self, str: []const u8) Value {
        return .{
            .inner = qjs.JS_NewStringLen(self.inner, str.ptr, str.len),
            .ctx = self.inner,
        };
    }

    /// Create object
    pub fn newObject(self: *Self) Value {
        return .{
            .inner = qjs.JS_NewObject(self.inner),
            .ctx = self.inner,
        };
    }

    /// Create array
    pub fn newArray(self: *Self) Value {
        return .{
            .inner = qjs.JS_NewArray(self.inner),
            .ctx = self.inner,
        };
    }

    /// Register native function
    pub fn registerFunction(
        self: *Self,
        name: [:0]const u8,
        comptime func: fn (*qjs.JSContext, qjs.JSValue, c_int, [*c]qjs.JSValue) callconv(.C) qjs.JSValue,
        arg_count: u8,
    ) void {
        const global = self.getGlobal();
        defer global.free();

        const func_val = qjs.JS_NewCFunction(self.inner, func, name.ptr, arg_count);
        _ = qjs.JS_SetPropertyStr(self.inner, global.inner, name.ptr, func_val);
    }

    /// Run pending jobs (async/promises)
    pub fn executePendingJobs(self: *Self) !bool {
        var ctx_ptr: ?*qjs.JSContext = null;
        const rt = qjs.JS_GetRuntime(self.inner);
        const ret = qjs.JS_ExecutePendingJob(rt, &ctx_ptr);
        if (ret < 0) {
            return Error.EvalFailed;
        }
        return ret > 0;
    }

    /// Deinitialize context
    pub fn deinit(self: *Self) void {
        qjs.JS_FreeContext(self.inner);
    }
};

/// QuickJS Runtime - manages memory and contexts
pub const Runtime = struct {
    inner: *qjs.JSRuntime,
    allocator: Allocator,

    const Self = @This();

    /// Initialize runtime
    pub fn init(allocator: Allocator) !Self {
        const rt = qjs.JS_NewRuntime();
        if (rt == null) return Error.RuntimeCreateFailed;

        // No memory limit by default - let WASM memory.grow handle it dynamically
        // This matches Node.js/Bun behavior where JS heap grows as needed
        // Memory is bounded by WASM max_memory (4GB) set at compile time

        // Set max stack size (1MB)
        qjs.JS_SetMaxStackSize(rt.?, 1024 * 1024);

        return .{
            .inner = rt.?,
            .allocator = allocator,
        };
    }

    /// Create new execution context
    pub fn newContext(self: *Self) !Context {
        const ctx = qjs.JS_NewContext(self.inner);
        if (ctx == null) return Error.ContextCreateFailed;

        // Add standard library
        qjs.js_std_add_helpers(ctx.?, 0, null);

        return .{
            .inner = ctx.?,
            .allocator = self.allocator,
        };
    }

    /// Set memory limit
    pub fn setMemoryLimit(self: *Self, limit: usize) void {
        qjs.JS_SetMemoryLimit(self.inner, limit);
    }

    /// Set max stack size
    pub fn setMaxStackSize(self: *Self, size: usize) void {
        qjs.JS_SetMaxStackSize(self.inner, size);
    }

    /// Get memory usage
    pub fn getMemoryUsage(self: *Self) MemoryUsage {
        var usage: qjs.JSMemoryUsage = undefined;
        qjs.JS_ComputeMemoryUsage(self.inner, &usage);
        return .{
            .malloc_size = @intCast(usage.malloc_size),
            .malloc_limit = @intCast(usage.malloc_limit),
            .memory_used_size = @intCast(usage.memory_used_size),
            .atom_count = @intCast(usage.atom_count),
            .str_count = @intCast(usage.str_count),
            .obj_count = @intCast(usage.obj_count),
            .prop_count = @intCast(usage.prop_count),
            .shape_count = @intCast(usage.shape_count),
            .js_func_count = @intCast(usage.js_func_count),
            .c_func_count = @intCast(usage.c_func_count),
        };
    }

    /// Run garbage collection
    pub fn runGC(self: *Self) void {
        qjs.JS_RunGC(self.inner);
    }

    /// Free standard library handlers
    pub fn freeStdHandlers(self: *Self) void {
        qjs.js_std_free_handlers(self.inner);
    }

    /// Deinitialize runtime
    pub fn deinit(self: *Self) void {
        qjs.JS_FreeRuntime(self.inner);
    }
};

/// Memory usage statistics
pub const MemoryUsage = struct {
    malloc_size: usize,
    malloc_limit: usize,
    memory_used_size: usize,
    atom_count: usize,
    str_count: usize,
    obj_count: usize,
    prop_count: usize,
    shape_count: usize,
    js_func_count: usize,
    c_func_count: usize,
};

// Helper: Create JS value from Zig value
pub fn toJSValue(ctx: *Context, value: anytype) Value {
    const T = @TypeOf(value);
    return switch (@typeInfo(T)) {
        .int, .comptime_int => ctx.newInt64(@intCast(value)),
        .float, .comptime_float => ctx.newFloat64(@floatCast(value)),
        .bool => ctx.newBool(value),
        .pointer => |ptr| {
            if (ptr.size == .Slice and ptr.child == u8) {
                return ctx.newString(value);
            }
            return ctx.undefined();
        },
        .null => ctx.null_(),
        .optional => if (value) |v| toJSValue(ctx, v) else ctx.null_(),
        else => ctx.undefined(),
    };
}

// Tests
test "create runtime and context" {
    var rt = try Runtime.init(std.testing.allocator);
    defer rt.deinit();

    var ctx = try rt.newContext();
    defer ctx.deinit();
}

test "eval simple expression" {
    var rt = try Runtime.init(std.testing.allocator);
    defer rt.deinit();

    var ctx = try rt.newContext();
    defer ctx.deinit();

    const result = try ctx.eval("1 + 2");
    defer result.free();

    try std.testing.expectEqual(@as(i32, 3), result.toInt32().?);
}

test "eval string" {
    var rt = try Runtime.init(std.testing.allocator);
    defer rt.deinit();

    var ctx = try rt.newContext();
    defer ctx.deinit();

    const result = try ctx.eval("'hello' + ' world'");
    defer result.free();

    const str = try result.toString(std.testing.allocator);
    defer std.testing.allocator.free(str);

    try std.testing.expectEqualStrings("hello world", str);
}
