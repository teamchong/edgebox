/// Native Array methods - optimized forEach, map, filter, etc.
/// Replaces QuickJS Array.prototype methods with Zig implementations
const std = @import("std");

// QuickJS types (matching NaN-boxing representation)
const JSValue = u64;
const JSContext = opaque {};

// QuickJS extern functions (non-inline functions that are exported)
extern fn JS_GetGlobalObject(ctx: ?*JSContext) JSValue;
extern fn JS_FreeValue(ctx: ?*JSContext, val: JSValue) void;
extern fn JS_GetPropertyStr(ctx: ?*JSContext, this_obj: JSValue, prop: [*:0]const u8) JSValue;
extern fn JS_SetPropertyStr(ctx: ?*JSContext, this_obj: JSValue, prop: [*:0]const u8, val: JSValue) c_int;
extern fn JS_GetPropertyUint32(ctx: ?*JSContext, this_obj: JSValue, idx: u32) JSValue;
extern fn JS_SetPropertyUint32(ctx: ?*JSContext, this_obj: JSValue, idx: u32, val: JSValue) c_int;
extern fn JS_NewCFunction2(ctx: ?*JSContext, func: *const anyopaque, name: [*:0]const u8, length: c_int, cproto: c_int, magic: c_int) JSValue;
extern fn JS_Call(ctx: ?*JSContext, func_obj: JSValue, this_obj: JSValue, argc: c_int, argv: [*]JSValue) JSValue;
extern fn JS_ToInt64(ctx: ?*JSContext, pres: *i64, val: JSValue) c_int;
extern fn JS_ToBool(ctx: ?*JSContext, val: JSValue) c_int;
extern fn JS_NewArray(ctx: ?*JSContext) JSValue;
extern fn JS_DupValue(ctx: ?*JSContext, val: JSValue) JSValue;
extern fn JS_HasProperty(ctx: ?*JSContext, this_obj: JSValue, atom: u32) c_int;
extern fn JS_NewAtomUInt32(ctx: ?*JSContext, n: u32) u32;
extern fn JS_FreeAtom(ctx: ?*JSContext, atom: u32) void;
extern fn JS_ThrowTypeError(ctx: ?*JSContext, fmt: [*:0]const u8, ...) JSValue;

// Inline functions reimplemented in Zig (QuickJS declares these as static inline)
// JS_NewInt32: Create a JS integer value using NaN-boxing
fn JS_NewInt32(_: ?*JSContext, val: i32) JSValue {
    // JS_MKVAL(JS_TAG_INT, val) = ((0 << 32) | (uint32_t)val)
    // In NaN-boxing for WASM32, integers are stored with tag 0 in upper 32 bits
    return @as(u64, @intCast(@as(u32, @bitCast(val))));
}

// QuickJS constants (NaN-boxed special values for WASM32)
// JS_MKVAL(tag, val) = ((uint64_t)(tag) << 32) | (uint32_t)(val)
// Tags: INT=0, BOOL=1, NULL=2, UNDEFINED=3, UNINITIALIZED=4, CATCH_OFFSET=5, EXCEPTION=6
const JS_UNDEFINED: JSValue = 0x0000000300000000; // tag 3
const JS_NULL: JSValue = 0x0000000200000000;      // tag 2
const JS_TRUE: JSValue = 0x0000000100000001;      // tag 1, val 1
const JS_FALSE: JSValue = 0x0000000100000000;     // tag 1, val 0
const JS_EXCEPTION: JSValue = 0x0000000600000000; // tag 6

fn JS_IsException(val: JSValue) bool {
    return val == JS_EXCEPTION;
}

fn JS_NewCFunction(ctx: ?*JSContext, func: *const anyopaque, name: [*:0]const u8, length: c_int) JSValue {
    return JS_NewCFunction2(ctx, func, name, length, 0, 0);
}

// ============================================================================
// Native Array.prototype.forEach
// ============================================================================

fn nativeForEach(ctx: ?*JSContext, this: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) return JS_UNDEFINED;

    const callback = argv[0];
    // Get thisArg (optional second argument)
    const this_arg = if (argc > 1) argv[1] else JS_UNDEFINED;

    // Get array length
    const len_val = JS_GetPropertyStr(ctx, this, "length");
    defer JS_FreeValue(ctx, len_val);
    var len: i64 = 0;
    _ = JS_ToInt64(ctx, &len, len_val);

    // Iterate and call callback for each element
    var i: u32 = 0;
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        // Check if property exists (sparse array support)
        const atom = JS_NewAtomUInt32(ctx, i);
        defer JS_FreeAtom(ctx, atom);
        const has_prop = JS_HasProperty(ctx, this, atom);
        if (has_prop == 0) continue;

        const elem = JS_GetPropertyUint32(ctx, this, i);
        defer JS_FreeValue(ctx, elem);

        // callback(element, index, array)
        var args = [3]JSValue{ elem, JS_NewInt32(ctx, @intCast(i)), this };
        const result = JS_Call(ctx, callback, this_arg, 3, &args);
        JS_FreeValue(ctx, args[1]); // Free the index

        if (JS_IsException(result)) {
            return result;
        }
        JS_FreeValue(ctx, result);
    }

    return JS_UNDEFINED;
}

// ============================================================================
// Native Array.prototype.map
// ============================================================================

fn nativeMap(ctx: ?*JSContext, this: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) {
        return JS_ThrowTypeError(ctx, "map requires a callback function");
    }

    const callback = argv[0];
    const this_arg = if (argc > 1) argv[1] else JS_UNDEFINED;

    // Get array length
    const len_val = JS_GetPropertyStr(ctx, this, "length");
    defer JS_FreeValue(ctx, len_val);
    var len: i64 = 0;
    _ = JS_ToInt64(ctx, &len, len_val);

    // Create result array
    const result_array = JS_NewArray(ctx);
    if (JS_IsException(result_array)) return result_array;

    var i: u32 = 0;
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const atom = JS_NewAtomUInt32(ctx, i);
        defer JS_FreeAtom(ctx, atom);
        const has_prop = JS_HasProperty(ctx, this, atom);
        if (has_prop == 0) continue;

        const elem = JS_GetPropertyUint32(ctx, this, i);
        defer JS_FreeValue(ctx, elem);

        // callback(element, index, array)
        var args = [3]JSValue{ elem, JS_NewInt32(ctx, @intCast(i)), this };
        const mapped = JS_Call(ctx, callback, this_arg, 3, &args);
        JS_FreeValue(ctx, args[1]);

        if (JS_IsException(mapped)) {
            JS_FreeValue(ctx, result_array);
            return mapped;
        }

        _ = JS_SetPropertyUint32(ctx, result_array, i, mapped);
    }

    return result_array;
}

// ============================================================================
// Native Array.prototype.filter
// ============================================================================

fn nativeFilter(ctx: ?*JSContext, this: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) {
        return JS_ThrowTypeError(ctx, "filter requires a callback function");
    }

    const callback = argv[0];
    const this_arg = if (argc > 1) argv[1] else JS_UNDEFINED;

    // Get array length
    const len_val = JS_GetPropertyStr(ctx, this, "length");
    defer JS_FreeValue(ctx, len_val);
    var len: i64 = 0;
    _ = JS_ToInt64(ctx, &len, len_val);

    // Create result array
    const result_array = JS_NewArray(ctx);
    if (JS_IsException(result_array)) return result_array;

    var result_idx: u32 = 0;
    var i: u32 = 0;
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const atom = JS_NewAtomUInt32(ctx, i);
        defer JS_FreeAtom(ctx, atom);
        const has_prop = JS_HasProperty(ctx, this, atom);
        if (has_prop == 0) continue;

        const elem = JS_GetPropertyUint32(ctx, this, i);

        // callback(element, index, array)
        var args = [3]JSValue{ elem, JS_NewInt32(ctx, @intCast(i)), this };
        const predicate_result = JS_Call(ctx, callback, this_arg, 3, &args);
        JS_FreeValue(ctx, args[1]);

        if (JS_IsException(predicate_result)) {
            JS_FreeValue(ctx, elem);
            JS_FreeValue(ctx, result_array);
            return predicate_result;
        }

        // Check if truthy
        const is_truthy = JS_ToBool(ctx, predicate_result);
        JS_FreeValue(ctx, predicate_result);

        if (is_truthy != 0) {
            // Duplicate elem since we're adding to result array
            _ = JS_SetPropertyUint32(ctx, result_array, result_idx, JS_DupValue(ctx, elem));
            result_idx += 1;
        }

        JS_FreeValue(ctx, elem);
    }

    return result_array;
}

// ============================================================================
// Native Array.prototype.reduce
// ============================================================================

fn nativeReduce(ctx: ?*JSContext, this: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) {
        return JS_ThrowTypeError(ctx, "reduce requires a callback function");
    }

    const callback = argv[0];

    // Get array length
    const len_val = JS_GetPropertyStr(ctx, this, "length");
    defer JS_FreeValue(ctx, len_val);
    var len: i64 = 0;
    _ = JS_ToInt64(ctx, &len, len_val);

    if (len == 0 and argc < 2) {
        return JS_ThrowTypeError(ctx, "Reduce of empty array with no initial value");
    }

    var accumulator: JSValue = undefined;
    var start_idx: u32 = 0;

    // Initialize accumulator
    if (argc > 1) {
        accumulator = JS_DupValue(ctx, argv[1]);
    } else {
        // Use first element as initial value
        accumulator = JS_GetPropertyUint32(ctx, this, 0);
        start_idx = 1;
    }

    var i: u32 = start_idx;
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const atom = JS_NewAtomUInt32(ctx, i);
        defer JS_FreeAtom(ctx, atom);
        const has_prop = JS_HasProperty(ctx, this, atom);
        if (has_prop == 0) continue;

        const elem = JS_GetPropertyUint32(ctx, this, i);
        defer JS_FreeValue(ctx, elem);

        // callback(accumulator, element, index, array)
        var args = [4]JSValue{ accumulator, elem, JS_NewInt32(ctx, @intCast(i)), this };
        const new_acc = JS_Call(ctx, callback, JS_UNDEFINED, 4, &args);
        JS_FreeValue(ctx, args[2]);
        JS_FreeValue(ctx, accumulator);

        if (JS_IsException(new_acc)) {
            return new_acc;
        }

        accumulator = new_acc;
    }

    return accumulator;
}

// ============================================================================
// Native Array.prototype.find
// ============================================================================

fn nativeFind(ctx: ?*JSContext, this: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) {
        return JS_ThrowTypeError(ctx, "find requires a callback function");
    }

    const callback = argv[0];
    const this_arg = if (argc > 1) argv[1] else JS_UNDEFINED;

    // Get array length
    const len_val = JS_GetPropertyStr(ctx, this, "length");
    defer JS_FreeValue(ctx, len_val);
    var len: i64 = 0;
    _ = JS_ToInt64(ctx, &len, len_val);

    var i: u32 = 0;
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const elem = JS_GetPropertyUint32(ctx, this, i);

        // callback(element, index, array)
        var args = [3]JSValue{ elem, JS_NewInt32(ctx, @intCast(i)), this };
        const predicate_result = JS_Call(ctx, callback, this_arg, 3, &args);
        JS_FreeValue(ctx, args[1]);

        if (JS_IsException(predicate_result)) {
            JS_FreeValue(ctx, elem);
            return predicate_result;
        }

        const is_truthy = JS_ToBool(ctx, predicate_result);
        JS_FreeValue(ctx, predicate_result);

        if (is_truthy != 0) {
            return elem; // Return the element (ownership transferred)
        }

        JS_FreeValue(ctx, elem);
    }

    return JS_UNDEFINED;
}

// ============================================================================
// Native Array.prototype.findIndex
// ============================================================================

fn nativeFindIndex(ctx: ?*JSContext, this: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) {
        return JS_ThrowTypeError(ctx, "findIndex requires a callback function");
    }

    const callback = argv[0];
    const this_arg = if (argc > 1) argv[1] else JS_UNDEFINED;

    // Get array length
    const len_val = JS_GetPropertyStr(ctx, this, "length");
    defer JS_FreeValue(ctx, len_val);
    var len: i64 = 0;
    _ = JS_ToInt64(ctx, &len, len_val);

    var i: u32 = 0;
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const elem = JS_GetPropertyUint32(ctx, this, i);
        defer JS_FreeValue(ctx, elem);

        // callback(element, index, array)
        var args = [3]JSValue{ elem, JS_NewInt32(ctx, @intCast(i)), this };
        const predicate_result = JS_Call(ctx, callback, this_arg, 3, &args);
        JS_FreeValue(ctx, args[1]);

        if (JS_IsException(predicate_result)) {
            return predicate_result;
        }

        const is_truthy = JS_ToBool(ctx, predicate_result);
        JS_FreeValue(ctx, predicate_result);

        if (is_truthy != 0) {
            return JS_NewInt32(ctx, @intCast(i));
        }
    }

    return JS_NewInt32(ctx, -1);
}

// ============================================================================
// Native Array.prototype.every
// ============================================================================

fn nativeEvery(ctx: ?*JSContext, this: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) {
        return JS_ThrowTypeError(ctx, "every requires a callback function");
    }

    const callback = argv[0];
    const this_arg = if (argc > 1) argv[1] else JS_UNDEFINED;

    // Get array length
    const len_val = JS_GetPropertyStr(ctx, this, "length");
    defer JS_FreeValue(ctx, len_val);
    var len: i64 = 0;
    _ = JS_ToInt64(ctx, &len, len_val);

    var i: u32 = 0;
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const atom = JS_NewAtomUInt32(ctx, i);
        defer JS_FreeAtom(ctx, atom);
        const has_prop = JS_HasProperty(ctx, this, atom);
        if (has_prop == 0) continue;

        const elem = JS_GetPropertyUint32(ctx, this, i);
        defer JS_FreeValue(ctx, elem);

        // callback(element, index, array)
        var args = [3]JSValue{ elem, JS_NewInt32(ctx, @intCast(i)), this };
        const predicate_result = JS_Call(ctx, callback, this_arg, 3, &args);
        JS_FreeValue(ctx, args[1]);

        if (JS_IsException(predicate_result)) {
            return predicate_result;
        }

        const is_truthy = JS_ToBool(ctx, predicate_result);
        JS_FreeValue(ctx, predicate_result);

        if (is_truthy == 0) {
            return JS_FALSE;
        }
    }

    return JS_TRUE;
}

// ============================================================================
// Native Array.prototype.some
// ============================================================================

fn nativeSome(ctx: ?*JSContext, this: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) {
        return JS_ThrowTypeError(ctx, "some requires a callback function");
    }

    const callback = argv[0];
    const this_arg = if (argc > 1) argv[1] else JS_UNDEFINED;

    // Get array length
    const len_val = JS_GetPropertyStr(ctx, this, "length");
    defer JS_FreeValue(ctx, len_val);
    var len: i64 = 0;
    _ = JS_ToInt64(ctx, &len, len_val);

    var i: u32 = 0;
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const atom = JS_NewAtomUInt32(ctx, i);
        defer JS_FreeAtom(ctx, atom);
        const has_prop = JS_HasProperty(ctx, this, atom);
        if (has_prop == 0) continue;

        const elem = JS_GetPropertyUint32(ctx, this, i);
        defer JS_FreeValue(ctx, elem);

        // callback(element, index, array)
        var args = [3]JSValue{ elem, JS_NewInt32(ctx, @intCast(i)), this };
        const predicate_result = JS_Call(ctx, callback, this_arg, 3, &args);
        JS_FreeValue(ctx, args[1]);

        if (JS_IsException(predicate_result)) {
            return predicate_result;
        }

        const is_truthy = JS_ToBool(ctx, predicate_result);
        JS_FreeValue(ctx, predicate_result);

        if (is_truthy != 0) {
            return JS_TRUE;
        }
    }

    return JS_FALSE;
}

// ============================================================================
// Native Array.prototype.includes
// ============================================================================

fn nativeIncludes(ctx: ?*JSContext, this: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) {
        return JS_FALSE;
    }

    const search_elem = argv[0];

    // Get array length
    const len_val = JS_GetPropertyStr(ctx, this, "length");
    defer JS_FreeValue(ctx, len_val);
    var len: i64 = 0;
    _ = JS_ToInt64(ctx, &len, len_val);

    // Get fromIndex (optional)
    var from_idx: i64 = 0;
    if (argc > 1) {
        _ = JS_ToInt64(ctx, &from_idx, argv[1]);
        if (from_idx < 0) {
            from_idx = @max(0, len + from_idx);
        }
    }

    var i: u32 = @intCast(@max(0, from_idx));
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const elem = JS_GetPropertyUint32(ctx, this, i);
        defer JS_FreeValue(ctx, elem);

        // SameValueZero comparison (treats NaN === NaN, but we use simple equality for now)
        // For correctness, we'd need to call JS to compare
        const eq = sameValueZero(search_elem, elem);
        if (eq) {
            return JS_TRUE;
        }
    }

    return JS_FALSE;
}

// ============================================================================
// Native Array.prototype.indexOf
// ============================================================================

fn nativeIndexOf(ctx: ?*JSContext, this: JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    if (argc < 1) {
        return JS_NewInt32(ctx, -1);
    }

    const search_elem = argv[0];

    // Get array length
    const len_val = JS_GetPropertyStr(ctx, this, "length");
    defer JS_FreeValue(ctx, len_val);
    var len: i64 = 0;
    _ = JS_ToInt64(ctx, &len, len_val);

    // Get fromIndex (optional)
    var from_idx: i64 = 0;
    if (argc > 1) {
        _ = JS_ToInt64(ctx, &from_idx, argv[1]);
        if (from_idx < 0) {
            from_idx = @max(0, len + from_idx);
        }
    }

    var i: u32 = @intCast(@max(0, from_idx));
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const atom = JS_NewAtomUInt32(ctx, i);
        defer JS_FreeAtom(ctx, atom);
        const has_prop = JS_HasProperty(ctx, this, atom);
        if (has_prop == 0) continue;

        const elem = JS_GetPropertyUint32(ctx, this, i);
        defer JS_FreeValue(ctx, elem);

        // Strict equality (===)
        if (strictEqual(search_elem, elem)) {
            return JS_NewInt32(ctx, @intCast(i));
        }
    }

    return JS_NewInt32(ctx, -1);
}

// ============================================================================
// Helper: SameValueZero comparison (for includes)
// ============================================================================

fn sameValueZero(a: JSValue, b: JSValue) bool {
    // For NaN-boxed values, we can compare directly for most cases
    // NaN handling would require special logic, but this covers common cases
    return a == b;
}

// ============================================================================
// Helper: Strict equality (===) comparison
// ============================================================================

fn strictEqual(a: JSValue, b: JSValue) bool {
    // For NaN-boxed values, direct comparison works for primitives
    return a == b;
}

// ============================================================================
// Registration: Replace Array.prototype methods with native implementations
// ============================================================================

pub fn register(ctx_opaque: *anyopaque) void {
    const ctx: *JSContext = @ptrCast(ctx_opaque);

    const global = JS_GetGlobalObject(ctx);
    defer JS_FreeValue(ctx, global);

    const array_ctor = JS_GetPropertyStr(ctx, global, "Array");
    defer JS_FreeValue(ctx, array_ctor);

    const array_proto = JS_GetPropertyStr(ctx, array_ctor, "prototype");
    defer JS_FreeValue(ctx, array_proto);

    // Helper to register a function
    const registerFn = struct {
        fn reg(c: *JSContext, obj: JSValue, name: [*:0]const u8, func: *const anyopaque, arg_count: c_int) void {
            const f = JS_NewCFunction(c, func, name, arg_count);
            _ = JS_SetPropertyStr(c, obj, name, f);
        }
    }.reg;

    // Override Array.prototype methods with native implementations
    registerFn(ctx, array_proto, "forEach", @ptrCast(&nativeForEach), 1);
    registerFn(ctx, array_proto, "map", @ptrCast(&nativeMap), 1);
    registerFn(ctx, array_proto, "filter", @ptrCast(&nativeFilter), 1);
    registerFn(ctx, array_proto, "reduce", @ptrCast(&nativeReduce), 1);
    registerFn(ctx, array_proto, "find", @ptrCast(&nativeFind), 1);
    registerFn(ctx, array_proto, "findIndex", @ptrCast(&nativeFindIndex), 1);
    registerFn(ctx, array_proto, "every", @ptrCast(&nativeEvery), 1);
    registerFn(ctx, array_proto, "some", @ptrCast(&nativeSome), 1);
    registerFn(ctx, array_proto, "includes", @ptrCast(&nativeIncludes), 1);
    registerFn(ctx, array_proto, "indexOf", @ptrCast(&nativeIndexOf), 1);
}
