/// Native Math module - replaces QuickJS Math entirely
/// All Math operations are pure Zig - no interpreter overhead
const std = @import("std");

// ============================================================================
// QuickJS FFI types and bindings (matches zig_runtime.zig pattern)
// ============================================================================

pub const JSContext = opaque {};

pub const JSValue = extern struct {
    u: extern union {
        int32: i32,
        float64: f64,
        ptr: *anyopaque,
    },
    tag: i64,

    pub const UNDEFINED = JSValue{ .u = .{ .int32 = 0 }, .tag = 3 };
    pub const NAN = JSValue{ .u = .{ .float64 = std.math.nan(f64) }, .tag = 7 };
};

// QuickJS C API externs (only functions that are actual symbols, not inlines)
extern fn JS_ToFloat64(ctx: ?*JSContext, pres: *f64, val: JSValue) c_int;
extern fn JS_ToInt32(ctx: ?*JSContext, pres: *i32, val: JSValue) c_int;
extern fn JS_NewObject(ctx: ?*JSContext) JSValue;
extern fn JS_GetGlobalObject(ctx: ?*JSContext) JSValue;
extern fn JS_FreeValue(ctx: ?*JSContext, val: JSValue) void;
extern fn JS_SetPropertyStr(ctx: ?*JSContext, this_obj: JSValue, prop: [*:0]const u8, val: JSValue) c_int;
extern fn JS_NewCFunction2(ctx: ?*JSContext, func: *const anyopaque, name: [*:0]const u8, length: c_int, cproto: c_int, magic: c_int) JSValue;

// Inline functions reimplemented in Zig (QuickJS declares these as static inline)
const JS_TAG_INT: i64 = 0;
const JS_TAG_FLOAT64: i64 = 7;

fn JS_NewFloat64(_: ?*JSContext, d: f64) JSValue {
    return JSValue{ .u = .{ .float64 = d }, .tag = JS_TAG_FLOAT64 };
}

fn JS_NewInt32(_: ?*JSContext, val: i32) JSValue {
    return JSValue{ .u = .{ .int32 = val }, .tag = JS_TAG_INT };
}

fn JS_NewCFunction(ctx: ?*JSContext, func: *const anyopaque, name: [*:0]const u8, length: c_int) JSValue {
    return JS_NewCFunction2(ctx, func, name, length, 0, 0);
}

// ============================================================================
// Helper: Convert JSValue to f64
// ============================================================================

inline fn toFloat64(ctx: ?*JSContext, val: JSValue) f64 {
    var f: f64 = 0;
    _ = JS_ToFloat64(ctx, &f, val);
    return f;
}

// ============================================================================
// Math Functions (single argument)
// ============================================================================

fn mathAbs(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, @abs(f));
}

fn mathSqrt(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, @sqrt(f));
}

fn mathFloor(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, @floor(f));
}

fn mathCeil(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, @ceil(f));
}

fn mathRound(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, @round(f));
}

fn mathTrunc(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, @trunc(f));
}

fn mathSign(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    if (std.math.isNan(f)) return JSValue.NAN;
    if (f > 0) return JS_NewFloat64(ctx, 1.0);
    if (f < 0) return JS_NewFloat64(ctx, -1.0);
    return JS_NewFloat64(ctx, f); // preserves +0 and -0
}

fn mathSin(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, @sin(f));
}

fn mathCos(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, @cos(f));
}

fn mathTan(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, @tan(f));
}

fn mathAsin(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, std.math.asin(f));
}

fn mathAcos(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, std.math.acos(f));
}

fn mathAtan(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, std.math.atan(f));
}

fn mathAsinh(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, std.math.asinh(f));
}

fn mathAcosh(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, std.math.acosh(f));
}

fn mathAtanh(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, std.math.atanh(f));
}

fn mathSinh(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, std.math.sinh(f));
}

fn mathCosh(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, std.math.cosh(f));
}

fn mathTanh(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, std.math.tanh(f));
}

fn mathExp(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, @exp(f));
}

fn mathExpm1(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, std.math.expm1(f));
}

fn mathLog(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, @log(f));
}

fn mathLog10(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, std.math.log10(f));
}

fn mathLog2(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, std.math.log2(f));
}

fn mathLog1p(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, std.math.log1p(f));
}

fn mathCbrt(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    return JS_NewFloat64(ctx, std.math.cbrt(f));
}

fn mathFround(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JSValue.NAN;
    const f = toFloat64(ctx, argv[0]);
    const f32_val: f32 = @floatCast(f);
    return JS_NewFloat64(ctx, @as(f64, f32_val));
}

fn mathClz32(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JS_NewInt32(ctx, 32);
    var i: i32 = 0;
    _ = JS_ToInt32(ctx, &i, argv[0]);
    const u: u32 = @bitCast(i);
    return JS_NewInt32(ctx, @intCast(@clz(u)));
}

// ============================================================================
// Math Functions (two arguments)
// ============================================================================

fn mathPow(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 2) return JSValue.NAN;
    const base = toFloat64(ctx, argv[0]);
    const exp_val = toFloat64(ctx, argv[1]);
    return JS_NewFloat64(ctx, std.math.pow(f64, base, exp_val));
}

fn mathAtan2(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 2) return JSValue.NAN;
    const y = toFloat64(ctx, argv[0]);
    const x = toFloat64(ctx, argv[1]);
    return JS_NewFloat64(ctx, std.math.atan2(y, x));
}

fn mathHypot(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc == 0) return JS_NewFloat64(ctx, 0.0);
    if (argc == 1) return JS_NewFloat64(ctx, @abs(toFloat64(ctx, argv[0])));

    // For 2 args, use hypot
    if (argc == 2) {
        const x = toFloat64(ctx, argv[0]);
        const y = toFloat64(ctx, argv[1]);
        return JS_NewFloat64(ctx, std.math.hypot(x, y));
    }

    // For more args, compute sqrt(sum of squares)
    var sum: f64 = 0;
    for (0..@intCast(argc)) |i| {
        const v = toFloat64(ctx, argv[i]);
        sum += v * v;
    }
    return JS_NewFloat64(ctx, @sqrt(sum));
}

fn mathImul(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 2) return JS_NewInt32(ctx, 0);
    var a: i32 = 0;
    var b: i32 = 0;
    _ = JS_ToInt32(ctx, &a, argv[0]);
    _ = JS_ToInt32(ctx, &b, argv[1]);
    // JavaScript imul: 32-bit integer multiply with wraparound
    const result = @as(i32, @truncate(@as(i64, a) * @as(i64, b)));
    return JS_NewInt32(ctx, result);
}

// ============================================================================
// Math Functions (variadic: min, max)
// ============================================================================

fn mathMin(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc == 0) return JS_NewFloat64(ctx, std.math.inf(f64));

    var result = toFloat64(ctx, argv[0]);
    if (std.math.isNan(result)) return JSValue.NAN;

    for (1..@intCast(argc)) |i| {
        const v = toFloat64(ctx, argv[i]);
        if (std.math.isNan(v)) return JSValue.NAN;
        if (v < result) result = v;
    }
    return JS_NewFloat64(ctx, result);
}

fn mathMax(ctx: ?*JSContext, _: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc == 0) return JS_NewFloat64(ctx, -std.math.inf(f64));

    var result = toFloat64(ctx, argv[0]);
    if (std.math.isNan(result)) return JSValue.NAN;

    for (1..@intCast(argc)) |i| {
        const v = toFloat64(ctx, argv[i]);
        if (std.math.isNan(v)) return JSValue.NAN;
        if (v > result) result = v;
    }
    return JS_NewFloat64(ctx, result);
}

// ============================================================================
// Math.random() - Fast PRNG
// ============================================================================

var prng_state: u64 = 0x853c49e6748fea9b; // Default seed

fn mathRandom(ctx: ?*JSContext, _: JSValue, _: c_int, _: [*c]JSValue) callconv(.c) JSValue {
    // xorshift64* PRNG - very fast
    var x = prng_state;
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    prng_state = x;
    const result = x *% 0x2545F4914F6CDD1D;
    // Convert to [0, 1) double
    const f: f64 = @as(f64, @floatFromInt(result >> 11)) * (1.0 / 9007199254740992.0);
    return JS_NewFloat64(ctx, f);
}

// ============================================================================
// Register Math object to globalThis
// ============================================================================

/// Register Math polyfill - accepts any JSContext pointer type via opaque cast
pub fn register(ctx_opaque: *anyopaque) void {
    const ctx: *JSContext = @ptrCast(ctx_opaque);
    const math_obj = JS_NewObject(ctx);

    // Helper to register a function
    const registerFn = struct {
        fn reg(c: *JSContext, obj: JSValue, name: [*:0]const u8, func: *const anyopaque, argc: c_int) void {
            const f = JS_NewCFunction(c, func, name, argc);
            _ = JS_SetPropertyStr(c, obj, name, f);
        }
    }.reg;

    // Single arg functions
    registerFn(ctx, math_obj, "abs", @ptrCast(&mathAbs), 1);
    registerFn(ctx, math_obj, "sqrt", @ptrCast(&mathSqrt), 1);
    registerFn(ctx, math_obj, "floor", @ptrCast(&mathFloor), 1);
    registerFn(ctx, math_obj, "ceil", @ptrCast(&mathCeil), 1);
    registerFn(ctx, math_obj, "round", @ptrCast(&mathRound), 1);
    registerFn(ctx, math_obj, "trunc", @ptrCast(&mathTrunc), 1);
    registerFn(ctx, math_obj, "sign", @ptrCast(&mathSign), 1);
    registerFn(ctx, math_obj, "sin", @ptrCast(&mathSin), 1);
    registerFn(ctx, math_obj, "cos", @ptrCast(&mathCos), 1);
    registerFn(ctx, math_obj, "tan", @ptrCast(&mathTan), 1);
    registerFn(ctx, math_obj, "asin", @ptrCast(&mathAsin), 1);
    registerFn(ctx, math_obj, "acos", @ptrCast(&mathAcos), 1);
    registerFn(ctx, math_obj, "atan", @ptrCast(&mathAtan), 1);
    registerFn(ctx, math_obj, "asinh", @ptrCast(&mathAsinh), 1);
    registerFn(ctx, math_obj, "acosh", @ptrCast(&mathAcosh), 1);
    registerFn(ctx, math_obj, "atanh", @ptrCast(&mathAtanh), 1);
    registerFn(ctx, math_obj, "sinh", @ptrCast(&mathSinh), 1);
    registerFn(ctx, math_obj, "cosh", @ptrCast(&mathCosh), 1);
    registerFn(ctx, math_obj, "tanh", @ptrCast(&mathTanh), 1);
    registerFn(ctx, math_obj, "exp", @ptrCast(&mathExp), 1);
    registerFn(ctx, math_obj, "expm1", @ptrCast(&mathExpm1), 1);
    registerFn(ctx, math_obj, "log", @ptrCast(&mathLog), 1);
    registerFn(ctx, math_obj, "log10", @ptrCast(&mathLog10), 1);
    registerFn(ctx, math_obj, "log2", @ptrCast(&mathLog2), 1);
    registerFn(ctx, math_obj, "log1p", @ptrCast(&mathLog1p), 1);
    registerFn(ctx, math_obj, "cbrt", @ptrCast(&mathCbrt), 1);
    registerFn(ctx, math_obj, "fround", @ptrCast(&mathFround), 1);
    registerFn(ctx, math_obj, "clz32", @ptrCast(&mathClz32), 1);

    // Two arg functions
    registerFn(ctx, math_obj, "pow", @ptrCast(&mathPow), 2);
    registerFn(ctx, math_obj, "atan2", @ptrCast(&mathAtan2), 2);
    registerFn(ctx, math_obj, "hypot", @ptrCast(&mathHypot), 2);
    registerFn(ctx, math_obj, "imul", @ptrCast(&mathImul), 2);

    // Variadic functions (declared as 2 args but accept any number)
    registerFn(ctx, math_obj, "min", @ptrCast(&mathMin), 2);
    registerFn(ctx, math_obj, "max", @ptrCast(&mathMax), 2);

    // Zero arg functions
    registerFn(ctx, math_obj, "random", @ptrCast(&mathRandom), 0);

    // Constants
    _ = JS_SetPropertyStr(ctx, math_obj, "PI", JS_NewFloat64(ctx, std.math.pi));
    _ = JS_SetPropertyStr(ctx, math_obj, "E", JS_NewFloat64(ctx, std.math.e));
    _ = JS_SetPropertyStr(ctx, math_obj, "LN2", JS_NewFloat64(ctx, std.math.ln2));
    _ = JS_SetPropertyStr(ctx, math_obj, "LN10", JS_NewFloat64(ctx, std.math.ln10));
    _ = JS_SetPropertyStr(ctx, math_obj, "LOG2E", JS_NewFloat64(ctx, std.math.log2e));
    _ = JS_SetPropertyStr(ctx, math_obj, "LOG10E", JS_NewFloat64(ctx, std.math.log10e));
    _ = JS_SetPropertyStr(ctx, math_obj, "SQRT2", JS_NewFloat64(ctx, std.math.sqrt2));
    _ = JS_SetPropertyStr(ctx, math_obj, "SQRT1_2", JS_NewFloat64(ctx, 1.0 / std.math.sqrt2));

    // Replace globalThis.Math
    const global = JS_GetGlobalObject(ctx);
    _ = JS_SetPropertyStr(ctx, global, "Math", math_obj);
    JS_FreeValue(ctx, global);
}
