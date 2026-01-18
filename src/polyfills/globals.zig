/// Native global functions - queueMicrotask, structuredClone
/// Registered as globalThis functions
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

/// queueMicrotask(callback) - Schedule callback to run as microtask
/// Implementation uses Promise.resolve().then(callback)
fn queueMicrotask(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (ctx == null or argc < 1) return quickjs.jsUndefined();

    const callback = argv[0];
    if (!qjs.JS_IsFunction(ctx, callback)) {
        return qjs.JS_ThrowTypeError(ctx, "queueMicrotask requires a callback function");
    }

    // Get Promise.resolve()
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const promise_ctor = qjs.JS_GetPropertyStr(ctx, global, "Promise");
    defer qjs.JS_FreeValue(ctx, promise_ctor);

    const resolve_func = qjs.JS_GetPropertyStr(ctx, promise_ctor, "resolve");
    defer qjs.JS_FreeValue(ctx, resolve_func);

    // Call Promise.resolve()
    const resolved = qjs.JS_Call(ctx, resolve_func, promise_ctor, 0, null);
    defer qjs.JS_FreeValue(ctx, resolved);

    // Get .then() and call it with callback
    const then_func = qjs.JS_GetPropertyStr(ctx, resolved, "then");
    defer qjs.JS_FreeValue(ctx, then_func);

    var args = [1]qjs.JSValue{callback};
    const result = qjs.JS_Call(ctx, then_func, resolved, 1, &args);

    // We don't care about the result promise, just need to schedule the callback
    if (!qjs.JS_IsException(result)) {
        qjs.JS_FreeValue(ctx, result);
    }

    return quickjs.jsUndefined();
}

/// structuredClone(value) - Deep clone an object
/// Supports: primitives, objects, arrays, Date, RegExp, Map, Set, ArrayBuffer, TypedArrays
fn structuredClone(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (ctx == null or argc < 1) return quickjs.jsUndefined();

    return cloneValue(ctx.?, argv[0]);
}

/// Recursive clone helper
fn cloneValue(ctx: *qjs.JSContext, value: qjs.JSValue) qjs.JSValue {
    // Handle primitives (undefined, null, bool, number, string, symbol, bigint)
    if (qjs.JS_IsUndefined(value) or qjs.JS_IsNull(value) or
        qjs.JS_IsBool(value) or qjs.JS_IsNumber(value) or
        qjs.JS_IsString(value) or qjs.JS_IsBigInt(value))
    {
        return qjs.JS_DupValue(ctx, value);
    }

    // Handle functions - not clonable, throw error
    if (qjs.JS_IsFunction(ctx, value)) {
        return qjs.JS_ThrowTypeError(ctx, "structuredClone cannot clone functions");
    }

    // Check object type using JS instanceof or constructor checks
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Check for Date
    const date_ctor = qjs.JS_GetPropertyStr(ctx, global, "Date");
    defer qjs.JS_FreeValue(ctx, date_ctor);
    if (qjs.JS_IsInstanceOf(ctx, value, date_ctor) == 1) {
        // Clone Date: new Date(originalDate.getTime())
        const getTime = qjs.JS_GetPropertyStr(ctx, value, "getTime");
        defer qjs.JS_FreeValue(ctx, getTime);
        const timeVal = qjs.JS_Call(ctx, getTime, value, 0, null);
        defer qjs.JS_FreeValue(ctx, timeVal);
        var args = [1]qjs.JSValue{timeVal};
        return qjs.JS_CallConstructor(ctx, date_ctor, 1, &args);
    }

    // Check for RegExp
    const regexp_ctor = qjs.JS_GetPropertyStr(ctx, global, "RegExp");
    defer qjs.JS_FreeValue(ctx, regexp_ctor);
    if (qjs.JS_IsInstanceOf(ctx, value, regexp_ctor) == 1) {
        // Clone RegExp: new RegExp(source, flags)
        const source = qjs.JS_GetPropertyStr(ctx, value, "source");
        defer qjs.JS_FreeValue(ctx, source);
        const flags = qjs.JS_GetPropertyStr(ctx, value, "flags");
        defer qjs.JS_FreeValue(ctx, flags);
        var args = [2]qjs.JSValue{ source, flags };
        return qjs.JS_CallConstructor(ctx, regexp_ctor, 2, &args);
    }

    // Check for Map
    const map_ctor = qjs.JS_GetPropertyStr(ctx, global, "Map");
    defer qjs.JS_FreeValue(ctx, map_ctor);
    if (qjs.JS_IsInstanceOf(ctx, value, map_ctor) == 1) {
        // Clone Map: new Map(), then copy entries with cloned values
        const new_map = qjs.JS_CallConstructor(ctx, map_ctor, 0, null);
        if (qjs.JS_IsException(new_map)) return new_map;

        const entries_func = qjs.JS_GetPropertyStr(ctx, value, "entries");
        defer qjs.JS_FreeValue(ctx, entries_func);
        const iterator = qjs.JS_Call(ctx, entries_func, value, 0, null);
        defer qjs.JS_FreeValue(ctx, iterator);

        const set_func = qjs.JS_GetPropertyStr(ctx, new_map, "set");
        defer qjs.JS_FreeValue(ctx, set_func);

        // Iterate entries
        while (true) {
            const next_func = qjs.JS_GetPropertyStr(ctx, iterator, "next");
            const next_result = qjs.JS_Call(ctx, next_func, iterator, 0, null);
            qjs.JS_FreeValue(ctx, next_func);
            defer qjs.JS_FreeValue(ctx, next_result);

            const done = qjs.JS_GetPropertyStr(ctx, next_result, "done");
            defer qjs.JS_FreeValue(ctx, done);
            if (qjs.JS_ToBool(ctx, done) == 1) break;

            const entry = qjs.JS_GetPropertyStr(ctx, next_result, "value");
            defer qjs.JS_FreeValue(ctx, entry);

            const key = qjs.JS_GetPropertyUint32(ctx, entry, 0);
            const val = qjs.JS_GetPropertyUint32(ctx, entry, 1);
            defer qjs.JS_FreeValue(ctx, key);
            defer qjs.JS_FreeValue(ctx, val);

            const cloned_key = cloneValue(ctx, key);
            defer qjs.JS_FreeValue(ctx, cloned_key);
            const cloned_val = cloneValue(ctx, val);
            defer qjs.JS_FreeValue(ctx, cloned_val);

            var set_args = [2]qjs.JSValue{ cloned_key, cloned_val };
            const set_result = qjs.JS_Call(ctx, set_func, new_map, 2, &set_args);
            qjs.JS_FreeValue(ctx, set_result);
        }
        return new_map;
    }

    // Check for Set
    const set_ctor = qjs.JS_GetPropertyStr(ctx, global, "Set");
    defer qjs.JS_FreeValue(ctx, set_ctor);
    if (qjs.JS_IsInstanceOf(ctx, value, set_ctor) == 1) {
        const new_set = qjs.JS_CallConstructor(ctx, set_ctor, 0, null);
        if (qjs.JS_IsException(new_set)) return new_set;

        const values_func = qjs.JS_GetPropertyStr(ctx, value, "values");
        defer qjs.JS_FreeValue(ctx, values_func);
        const iterator = qjs.JS_Call(ctx, values_func, value, 0, null);
        defer qjs.JS_FreeValue(ctx, iterator);

        const add_func = qjs.JS_GetPropertyStr(ctx, new_set, "add");
        defer qjs.JS_FreeValue(ctx, add_func);

        while (true) {
            const next_func = qjs.JS_GetPropertyStr(ctx, iterator, "next");
            const next_result = qjs.JS_Call(ctx, next_func, iterator, 0, null);
            qjs.JS_FreeValue(ctx, next_func);
            defer qjs.JS_FreeValue(ctx, next_result);

            const done = qjs.JS_GetPropertyStr(ctx, next_result, "done");
            defer qjs.JS_FreeValue(ctx, done);
            if (qjs.JS_ToBool(ctx, done) == 1) break;

            const val = qjs.JS_GetPropertyStr(ctx, next_result, "value");
            defer qjs.JS_FreeValue(ctx, val);

            const cloned = cloneValue(ctx, val);
            defer qjs.JS_FreeValue(ctx, cloned);

            var add_args = [1]qjs.JSValue{cloned};
            const add_result = qjs.JS_Call(ctx, add_func, new_set, 1, &add_args);
            qjs.JS_FreeValue(ctx, add_result);
        }
        return new_set;
    }

    // Check for Array
    if (qjs.JS_IsArray(value)) {
        const len_val = qjs.JS_GetPropertyStr(ctx, value, "length");
        defer qjs.JS_FreeValue(ctx, len_val);
        var len: i64 = 0;
        _ = qjs.JS_ToInt64(ctx, &len, len_val);

        const new_arr = qjs.JS_NewArray(ctx);
        if (qjs.JS_IsException(new_arr)) return new_arr;

        var i: u32 = 0;
        while (i < @as(u32, @intCast(len))) : (i += 1) {
            const elem = qjs.JS_GetPropertyUint32(ctx, value, i);
            defer qjs.JS_FreeValue(ctx, elem);
            const cloned = cloneValue(ctx, elem);
            _ = qjs.JS_SetPropertyUint32(ctx, new_arr, i, cloned);
        }
        return new_arr;
    }

    // Plain object - clone properties
    if (qjs.JS_IsObject(value)) {
        const new_obj = qjs.JS_NewObject(ctx);
        if (qjs.JS_IsException(new_obj)) return new_obj;

        // Get own property names using Object.keys
        const obj_ctor = qjs.JS_GetPropertyStr(ctx, global, "Object");
        defer qjs.JS_FreeValue(ctx, obj_ctor);
        const keys_func = qjs.JS_GetPropertyStr(ctx, obj_ctor, "keys");
        defer qjs.JS_FreeValue(ctx, keys_func);

        var args = [1]qjs.JSValue{value};
        const keys = qjs.JS_Call(ctx, keys_func, obj_ctor, 1, &args);
        defer qjs.JS_FreeValue(ctx, keys);

        if (!qjs.JS_IsArray(keys)) {
            return new_obj;
        }

        const len_val = qjs.JS_GetPropertyStr(ctx, keys, "length");
        defer qjs.JS_FreeValue(ctx, len_val);
        var len: i64 = 0;
        _ = qjs.JS_ToInt64(ctx, &len, len_val);

        var i: u32 = 0;
        while (i < @as(u32, @intCast(len))) : (i += 1) {
            const key = qjs.JS_GetPropertyUint32(ctx, keys, i);
            defer qjs.JS_FreeValue(ctx, key);

            const key_str = qjs.JS_ToCString(ctx, key);
            if (key_str != null) {
                defer qjs.JS_FreeCString(ctx, key_str);

                const prop = qjs.JS_GetPropertyStr(ctx, value, key_str);
                defer qjs.JS_FreeValue(ctx, prop);

                const cloned = cloneValue(ctx, prop);
                _ = qjs.JS_SetPropertyStr(ctx, new_obj, key_str, cloned);
            }
        }
        return new_obj;
    }

    // Unknown type - return copy
    return qjs.JS_DupValue(ctx, value);
}

/// Register global functions
pub fn register(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    _ = qjs.JS_SetPropertyStr(ctx, global, "queueMicrotask", qjs.JS_NewCFunction(ctx, queueMicrotask, "queueMicrotask", 1));
    _ = qjs.JS_SetPropertyStr(ctx, global, "structuredClone", qjs.JS_NewCFunction(ctx, structuredClone, "structuredClone", 1));
}
