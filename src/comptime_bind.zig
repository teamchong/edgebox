/// Comptime Binding Generation for QuickJS
///
/// Uses Zig's comptime to generate zero-cost wrappers for native functions.
/// This eliminates runtime type checking and enables inlining.
///
/// Example usage:
/// ```zig
/// fn add(a: i32, b: i32) i32 {
///     return a + b;
/// }
///
/// const js_add = comptimeBind.wrapSimple(add, 2);
/// context.registerGlobalFunction("add", js_add, 2);
/// ```
const std = @import("std");
const qjs = @import("quickjs_core.zig").c;

/// JSCFunction type for QuickJS callbacks
pub const JSCFunction = *const fn (?*qjs.JSContext, qjs.JSValue, c_int, [*c]qjs.JSValue) callconv(.c) qjs.JSValue;

/// Wrap a simple Zig function (i32, i32) -> i32 for QuickJS
/// This is the most common pattern for math functions
pub fn wrapI32Binary(comptime func: fn (i32, i32) i32) JSCFunction {
    return struct {
        pub fn call(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
            if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "requires 2 arguments");

            var a: i32 = 0;
            var b: i32 = 0;
            if (qjs.JS_ToInt32(ctx, &a, argv[0]) < 0) return qjs.JS_ThrowTypeError(ctx, "arg 0 must be number");
            if (qjs.JS_ToInt32(ctx, &b, argv[1]) < 0) return qjs.JS_ThrowTypeError(ctx, "arg 1 must be number");

            return qjs.JS_NewInt32(ctx, func(a, b));
        }
    }.call;
}

/// Wrap a Zig function (i32) -> i32 for QuickJS
pub fn wrapI32Unary(comptime func: fn (i32) i32) JSCFunction {
    return struct {
        pub fn call(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
            if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "requires 1 argument");

            var a: i32 = 0;
            if (qjs.JS_ToInt32(ctx, &a, argv[0]) < 0) return qjs.JS_ThrowTypeError(ctx, "arg must be number");

            return qjs.JS_NewInt32(ctx, func(a));
        }
    }.call;
}

/// Wrap a Zig function (f64, f64) -> f64 for QuickJS
pub fn wrapF64Binary(comptime func: fn (f64, f64) f64) JSCFunction {
    return struct {
        pub fn call(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
            if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "requires 2 arguments");

            var a: f64 = 0;
            var b: f64 = 0;
            if (qjs.JS_ToFloat64(ctx, &a, argv[0]) < 0) return qjs.JS_ThrowTypeError(ctx, "arg 0 must be number");
            if (qjs.JS_ToFloat64(ctx, &b, argv[1]) < 0) return qjs.JS_ThrowTypeError(ctx, "arg 1 must be number");

            return qjs.JS_NewFloat64(ctx, func(a, b));
        }
    }.call;
}

/// Wrap a Zig function () -> i32 for QuickJS (no args, returns int)
pub fn wrapNoArgsI32(comptime func: fn () i32) JSCFunction {
    return struct {
        pub fn call(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
            return qjs.JS_NewInt32(ctx, func());
        }
    }.call;
}

/// Wrap a Zig function () -> bool for QuickJS
pub fn wrapNoArgsBool(comptime func: fn () bool) JSCFunction {
    return struct {
        pub fn call(_: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
            return if (func()) qjs.JS_TRUE else qjs.JS_FALSE;
        }
    }.call;
}

/// Wrap a Zig function (i32) -> bool for QuickJS
pub fn wrapI32ToBool(comptime func: fn (i32) bool) JSCFunction {
    return struct {
        pub fn call(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
            if (argc < 1) return qjs.JS_FALSE;

            var a: i32 = 0;
            if (qjs.JS_ToInt32(ctx, &a, argv[0]) < 0) return qjs.JS_FALSE;

            return if (func(a)) qjs.JS_TRUE else qjs.JS_FALSE;
        }
    }.call;
}

// ============================================================================
// Helper: Convert JSValue to Zig types
// ============================================================================

/// Get string from JSValue (caller must free with freeString)
pub fn getString(ctx: ?*qjs.JSContext, val: qjs.JSValue) ?[]const u8 {
    var len: usize = undefined;
    const cstr = qjs.JS_ToCStringLen(ctx, &len, val);
    if (cstr == null) return null;
    return cstr[0..len];
}

/// Free string obtained from getString
pub fn freeString(ctx: ?*qjs.JSContext, str: []const u8) void {
    qjs.JS_FreeCString(ctx, str.ptr);
}

/// Create JS object from Zig struct
pub fn structToJS(ctx: ?*qjs.JSContext, value: anytype) qjs.JSValue {
    const T = @TypeOf(value);
    const obj = qjs.JS_NewObject(ctx);

    inline for (std.meta.fields(T)) |field| {
        const field_val = @field(value, field.name);
        const js_val = valueToJS(ctx, field_val);
        _ = qjs.JS_SetPropertyStr(ctx, obj, field.name ++ "", js_val);
    }

    return obj;
}

/// Convert any Zig value to JSValue
pub fn valueToJS(ctx: ?*qjs.JSContext, value: anytype) qjs.JSValue {
    const T = @TypeOf(value);

    return switch (@typeInfo(T)) {
        .Int => |int_info| blk: {
            if (int_info.bits <= 32) {
                break :blk qjs.JS_NewInt32(ctx, @intCast(value));
            } else {
                break :blk qjs.JS_NewInt64(ctx, @intCast(value));
            }
        },
        .Float => qjs.JS_NewFloat64(ctx, @floatCast(value)),
        .Bool => if (value) qjs.JS_TRUE else qjs.JS_FALSE,
        .Void => qjs.JS_UNDEFINED,
        .Pointer => |ptr_info| blk: {
            if (ptr_info.child == u8) {
                break :blk qjs.JS_NewStringLen(ctx, value.ptr, value.len);
            }
            break :blk qjs.JS_UNDEFINED;
        },
        .Optional => blk: {
            if (value) |v| {
                break :blk valueToJS(ctx, v);
            }
            break :blk qjs.JS_NULL;
        },
        .Struct => structToJS(ctx, value),
        else => qjs.JS_UNDEFINED,
    };
}
