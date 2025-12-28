/// Native util module - QuickJS C functions
/// Utility functions for Node.js compatibility
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

/// util.format(format, ...args) - String formatting with specifiers
/// Supports %s (string), %d (number), %j/%o/%O (JSON), %% (escape)
fn utilFormat(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (ctx == null) return qjs.JS_UNDEFINED;
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    // Get first arg as string (the format)
    const fmt_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (fmt_cstr == null) return qjs.JS_NewString(ctx, "");
    defer qjs.JS_FreeCString(ctx, fmt_cstr);

    const fmt = std.mem.span(fmt_cstr);
    if (fmt.len == 0) return qjs.JS_NewString(ctx, "");

    // Node.js behavior: if no extra args, return format string as-is
    if (argc == 1) {
        return qjs.JS_DupValue(ctx, argv[0]);
    }

    // Buffer for output
    var buffer: [8192]u8 = undefined;
    var pos: usize = 0;
    var arg_idx: usize = 1; // Start from second arg

    var i: usize = 0;
    while (i < fmt.len) : (i += 1) {
        if (i + 1 < fmt.len and fmt[i] == '%') {
            const spec = fmt[i + 1];
            if (spec == '%') {
                // Escape %% -> % (only when processing args)
                if (pos < buffer.len) {
                    buffer[pos] = '%';
                    pos += 1;
                }
                i += 1;
            } else if (spec == 's' and arg_idx < @as(usize, @intCast(argc))) {
                // String specifier
                const str = qjs.JS_ToCString(ctx, argv[arg_idx]);
                arg_idx += 1;
                if (str) |s| {
                    defer qjs.JS_FreeCString(ctx, s);
                    const text = std.mem.span(s);
                    const len = @min(text.len, buffer.len - pos);
                    @memcpy(buffer[pos..][0..len], text[0..len]);
                    pos += len;
                }
                i += 1;
            } else if (spec == 'd' and arg_idx < @as(usize, @intCast(argc))) {
                // Number specifier
                var num: f64 = 0;
                _ = qjs.JS_ToFloat64(ctx, &num, argv[arg_idx]);
                arg_idx += 1;
                // Format as integer if possible
                if (num == @trunc(num) and @abs(num) < 9007199254740992) {
                    const n = std.fmt.bufPrint(buffer[pos..], "{d}", .{@as(i64, @intFromFloat(num))}) catch "";
                    pos += n.len;
                } else {
                    const n = std.fmt.bufPrint(buffer[pos..], "{d}", .{num}) catch "";
                    pos += n.len;
                }
                i += 1;
            } else if ((spec == 'j' or spec == 'o' or spec == 'O') and arg_idx < @as(usize, @intCast(argc))) {
                // JSON specifier
                const global = qjs.JS_GetGlobalObject(ctx);
                const json = qjs.JS_GetPropertyStr(ctx, global, "JSON");
                const stringify = qjs.JS_GetPropertyStr(ctx, json, "stringify");
                var args_arr = [1]qjs.JSValue{argv[arg_idx]};
                const result = qjs.JS_Call(ctx, stringify, json, 1, &args_arr);
                arg_idx += 1;
                if (!qjs.JS_IsException(result)) {
                    const str = qjs.JS_ToCString(ctx, result);
                    if (str) |s| {
                        const text = std.mem.span(s);
                        const len = @min(text.len, buffer.len - pos);
                        @memcpy(buffer[pos..][0..len], text[0..len]);
                        pos += len;
                        qjs.JS_FreeCString(ctx, s);
                    }
                }
                qjs.JS_FreeValue(ctx, result);
                qjs.JS_FreeValue(ctx, stringify);
                qjs.JS_FreeValue(ctx, json);
                qjs.JS_FreeValue(ctx, global);
                i += 1;
            } else {
                // Keep unknown specifier or specifier without arg
                if (pos < buffer.len) {
                    buffer[pos] = fmt[i];
                    pos += 1;
                }
            }
        } else {
            // Regular character
            if (pos < buffer.len) {
                buffer[pos] = fmt[i];
                pos += 1;
            }
        }
    }

    // Append remaining args with space separator
    while (arg_idx < @as(usize, @intCast(argc))) : (arg_idx += 1) {
        if (pos < buffer.len) {
            buffer[pos] = ' ';
            pos += 1;
        }
        const str = qjs.JS_ToCString(ctx, argv[arg_idx]);
        if (str) |s| {
            defer qjs.JS_FreeCString(ctx, s);
            const text = std.mem.span(s);
            const len = @min(text.len, buffer.len - pos);
            @memcpy(buffer[pos..][0..len], text[0..len]);
            pos += len;
        }
    }

    return qjs.JS_NewStringLen(ctx, &buffer, @intCast(pos));
}

/// util.inspect(obj) - Return string representation using JSON.stringify
fn utilInspect(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "undefined");

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const json_obj = qjs.JS_GetPropertyStr(ctx, global, "JSON");
    defer qjs.JS_FreeValue(ctx, json_obj);

    const stringify = qjs.JS_GetPropertyStr(ctx, json_obj, "stringify");
    defer qjs.JS_FreeValue(ctx, stringify);

    // JSON.stringify(obj, null, 2) for pretty print
    var args = [3]qjs.JSValue{ argv[0], qjs.JS_NULL, qjs.JS_NewInt32(ctx, 2) };
    defer qjs.JS_FreeValue(ctx, args[2]);

    const result = qjs.JS_Call(ctx, stringify, json_obj, 3, &args);
    if (qjs.JS_IsException(result)) {
        qjs.JS_FreeValue(ctx, result);
        // Fallback to toString
        return qjs.JS_ToString(ctx, argv[0]);
    }

    return result;
}

/// util.deprecate(fn, msg) - Return function as-is (no-op for now)
fn utilDeprecate(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_UNDEFINED;
    return qjs.JS_DupValue(ctx, argv[0]);
}

/// util.inherits(ctor, superCtor) - Set up prototype chain
fn utilInherits(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_UNDEFINED;

    // Get Object.setPrototypeOf
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const object_ctor = qjs.JS_GetPropertyStr(ctx, global, "Object");
    defer qjs.JS_FreeValue(ctx, object_ctor);

    const set_proto_func = qjs.JS_GetPropertyStr(ctx, object_ctor, "setPrototypeOf");
    defer qjs.JS_FreeValue(ctx, set_proto_func);

    // Get ctor.prototype
    const ctor_proto = qjs.JS_GetPropertyStr(ctx, argv[0], "prototype");
    defer qjs.JS_FreeValue(ctx, ctor_proto);

    // Get superCtor.prototype
    const super_proto = qjs.JS_GetPropertyStr(ctx, argv[1], "prototype");
    defer qjs.JS_FreeValue(ctx, super_proto);

    // Object.setPrototypeOf(ctor.prototype, superCtor.prototype)
    var set_proto_args = [2]qjs.JSValue{ ctor_proto, super_proto };
    const result = qjs.JS_Call(ctx, set_proto_func, object_ctor, 2, &set_proto_args);
    qjs.JS_FreeValue(ctx, result);

    return qjs.JS_UNDEFINED;
}

/// util.isArray(obj) - Check if object is an array
fn utilIsArray(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    const is_array = qjs.JS_IsArray(argv[0]);
    if (is_array) {
        return qjs.JS_TRUE;
    }
    return qjs.JS_FALSE;
}

/// util.isBuffer(obj) - Check if object is a Buffer
fn utilIsBuffer(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;

    // Check if it's a Uint8Array
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], uint8array_ctor);
    if (result == 1) {
        return qjs.JS_TRUE;
    }
    return qjs.JS_FALSE;
}

/// util.isBoolean(obj) - Check if value is a boolean
fn utilIsBoolean(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    return if (qjs.JS_IsBool(argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.isNull(obj) - Check if value is null
fn utilIsNull(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    return if (qjs.JS_IsNull(argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.isNullOrUndefined(obj) - Check if value is null or undefined
fn utilIsNullOrUndefined(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_TRUE; // No arg is undefined
    return if (qjs.JS_IsNull(argv[0]) or qjs.JS_IsUndefined(argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.isNumber(obj) - Check if value is a number
fn utilIsNumber(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    return if (qjs.JS_IsNumber(argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.isString(obj) - Check if value is a string
fn utilIsString(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    return if (qjs.JS_IsString(argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.isSymbol(obj) - Check if value is a symbol
fn utilIsSymbol(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    return if (qjs.JS_IsSymbol(argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.isUndefined(obj) - Check if value is undefined
fn utilIsUndefined(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_TRUE; // No arg is undefined
    return if (qjs.JS_IsUndefined(argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.isRegExp(obj) - Check if value is a RegExp
fn utilIsRegExp(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const regexp_ctor = qjs.JS_GetPropertyStr(ctx, global, "RegExp");
    defer qjs.JS_FreeValue(ctx, regexp_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], regexp_ctor);
    return if (result == 1) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.isObject(obj) - Check if value is an object (not null)
fn utilIsObject(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    const val = argv[0];
    // Object means: is object AND not null
    return if (qjs.JS_IsObject(val) and !qjs.JS_IsNull(val)) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.isDate(obj) - Check if value is a Date
fn utilIsDate(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const date_ctor = qjs.JS_GetPropertyStr(ctx, global, "Date");
    defer qjs.JS_FreeValue(ctx, date_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], date_ctor);
    return if (result == 1) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.isError(obj) - Check if value is an Error
fn utilIsError(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const error_ctor = qjs.JS_GetPropertyStr(ctx, global, "Error");
    defer qjs.JS_FreeValue(ctx, error_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], error_ctor);
    return if (result == 1) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.isFunction(obj) - Check if value is a function
fn utilIsFunction(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    return if (qjs.JS_IsFunction(ctx, argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.isPrimitive(obj) - Check if value is a primitive (null, undefined, boolean, number, string, symbol)
fn utilIsPrimitive(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_TRUE; // undefined is primitive
    const val = argv[0];
    // Primitive: not an object, or is null
    if (qjs.JS_IsNull(val)) return qjs.JS_TRUE;
    if (qjs.JS_IsUndefined(val)) return qjs.JS_TRUE;
    if (qjs.JS_IsBool(val)) return qjs.JS_TRUE;
    if (qjs.JS_IsNumber(val)) return qjs.JS_TRUE;
    if (qjs.JS_IsString(val)) return qjs.JS_TRUE;
    if (qjs.JS_IsSymbol(val)) return qjs.JS_TRUE;
    return qjs.JS_FALSE;
}

/// util.debuglog(section) - Returns a no-op debug function (stub)
fn utilDebuglog(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    // Return a no-op function
    const noop_code = "(function(){})";
    return qjs.JS_Eval(ctx, noop_code.ptr, noop_code.len, "<debuglog>", qjs.JS_EVAL_TYPE_GLOBAL);
}

/// util.promisify(fn) - Convert callback-style function to Promise-style
/// The original function must follow Node.js callback convention: fn(...args, callback)
/// where callback is (err, result) => void
fn utilPromisify(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "promisify requires a function argument");

    const original_fn = argv[0];
    if (!qjs.JS_IsFunction(ctx, original_fn)) {
        return qjs.JS_ThrowTypeError(ctx, "argument must be a function");
    }

    // Create a wrapper function via JS eval that captures the original function
    // This is simpler and more reliable than manually constructing closures in C
    const wrapper_code =
        \\(function(originalFn) {
        \\    return function promisified(...args) {
        \\        return new Promise(function(resolve, reject) {
        \\            originalFn.apply(this, args.concat(function(err, result) {
        \\                if (err) reject(err);
        \\                else resolve(result);
        \\            }));
        \\        });
        \\    };
        \\})
    ;

    // Evaluate the wrapper factory function
    const factory = qjs.JS_Eval(ctx, wrapper_code.ptr, wrapper_code.len, "<promisify>", qjs.JS_EVAL_TYPE_GLOBAL);
    if (qjs.JS_IsException(factory)) {
        return factory;
    }
    defer qjs.JS_FreeValue(ctx, factory);

    // Call the factory with the original function to get the promisified version
    var call_args = [1]qjs.JSValue{original_fn};
    return qjs.JS_Call(ctx, factory, qjs.JS_UNDEFINED, 1, &call_args);
}

/// util.callbackify(fn) - Convert Promise-returning function to callback-style
fn utilCallbackify(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "callbackify requires a function argument");

    const original_fn = argv[0];
    if (!qjs.JS_IsFunction(ctx, original_fn)) {
        return qjs.JS_ThrowTypeError(ctx, "argument must be a function");
    }

    const wrapper_code =
        \\(function(originalFn) {
        \\    return function callbackified(...args) {
        \\        var callback = args.pop();
        \\        if (typeof callback !== 'function') {
        \\            throw new TypeError('The last argument must be a callback function');
        \\        }
        \\        originalFn.apply(this, args).then(
        \\            function(result) { callback(null, result); },
        \\            function(err) { callback(err); }
        \\        );
        \\    };
        \\})
    ;

    const factory = qjs.JS_Eval(ctx, wrapper_code.ptr, wrapper_code.len, "<callbackify>", qjs.JS_EVAL_TYPE_GLOBAL);
    if (qjs.JS_IsException(factory)) {
        return factory;
    }
    defer qjs.JS_FreeValue(ctx, factory);

    var call_args = [1]qjs.JSValue{original_fn};
    return qjs.JS_Call(ctx, factory, qjs.JS_UNDEFINED, 1, &call_args);
}

// ============================================================================
// util.types - Type checking functions
// ============================================================================

/// util.types.isDate(obj) - Check if object is a Date
fn utilTypesIsDate(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const date_ctor = qjs.JS_GetPropertyStr(ctx, global, "Date");
    defer qjs.JS_FreeValue(ctx, date_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], date_ctor);
    return if (result == 1) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.types.isRegExp(obj) - Check if object is a RegExp
fn utilTypesIsRegExp(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const regexp_ctor = qjs.JS_GetPropertyStr(ctx, global, "RegExp");
    defer qjs.JS_FreeValue(ctx, regexp_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], regexp_ctor);
    return if (result == 1) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.types.isPromise(obj) - Check if object is a Promise
fn utilTypesIsPromise(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const promise_ctor = qjs.JS_GetPropertyStr(ctx, global, "Promise");
    defer qjs.JS_FreeValue(ctx, promise_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], promise_ctor);
    return if (result == 1) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.types.isMap(obj) - Check if object is a Map
fn utilTypesIsMap(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const map_ctor = qjs.JS_GetPropertyStr(ctx, global, "Map");
    defer qjs.JS_FreeValue(ctx, map_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], map_ctor);
    return if (result == 1) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.types.isSet(obj) - Check if object is a Set
fn utilTypesIsSet(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const set_ctor = qjs.JS_GetPropertyStr(ctx, global, "Set");
    defer qjs.JS_FreeValue(ctx, set_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], set_ctor);
    return if (result == 1) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// util.types.isNativeError(obj) - Check if object is an Error
fn utilTypesIsNativeError(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const error_ctor = qjs.JS_GetPropertyStr(ctx, global, "Error");
    defer qjs.JS_FreeValue(ctx, error_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], error_ctor);
    return if (result == 1) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// Register util module
pub fn register(ctx: *qjs.JSContext) void {
    const util_obj = qjs.JS_NewObject(ctx);

    // Mark as native so JS polyfill doesn't override
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "__native", qjs.JS_TRUE);

    // Register format directly (debugging function pointer issue)
    const format_func_new = qjs.JS_NewCFunction(ctx, utilFormat, "format", -1);
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "format", format_func_new);

    // Register other util functions
    inline for (.{
        .{ "inspect", utilInspect, 1 },
        .{ "deprecate", utilDeprecate, 2 },
        .{ "inherits", utilInherits, 2 },
        .{ "isArray", utilIsArray, 1 },
        .{ "isBuffer", utilIsBuffer, 1 },
        .{ "promisify", utilPromisify, 1 },
        .{ "callbackify", utilCallbackify, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, util_obj, binding[0], func);
    }

    // Create util.types sub-object
    const types_obj = qjs.JS_NewObject(ctx);
    inline for (.{
        .{ "isDate", utilTypesIsDate, 1 },
        .{ "isRegExp", utilTypesIsRegExp, 1 },
        .{ "isPromise", utilTypesIsPromise, 1 },
        .{ "isMap", utilTypesIsMap, 1 },
        .{ "isSet", utilTypesIsSet, 1 },
        .{ "isNativeError", utilTypesIsNativeError, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, types_obj, binding[0], func);
    }
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "types", types_obj);

    // Add TextDecoder and TextEncoder references
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const text_decoder = qjs.JS_GetPropertyStr(ctx, global, "TextDecoder");
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "TextDecoder", text_decoder);

    const text_encoder = qjs.JS_GetPropertyStr(ctx, global, "TextEncoder");
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "TextEncoder", text_encoder);

    // Set in _modules for require('util') and require('node:util')
    // NOTE: We update the EXISTING _modules.util object in-place rather than replacing it
    // This ensures that any code that captured a reference to util still gets native functions
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        // Check if _modules.util already exists (set by JS polyfill)
        const existing_util = qjs.JS_GetPropertyStr(ctx, modules_val, "util");

        if (!qjs.JS_IsUndefined(existing_util) and !qjs.JS_IsNull(existing_util)) {
            // Update existing object in-place - register format directly (not via inline for)
            const format_func = qjs.JS_NewCFunction(ctx, utilFormat, "format", -1);
            _ = qjs.JS_SetPropertyStr(ctx, existing_util, "format", format_func);

            // Rest via inline for
            inline for (.{
                .{ "inspect", utilInspect, 1 },
                .{ "deprecate", utilDeprecate, 2 },
                .{ "inherits", utilInherits, 2 },
                .{ "isArray", utilIsArray, 1 },
                .{ "isBuffer", utilIsBuffer, 1 },
                .{ "promisify", utilPromisify, 1 },
                .{ "callbackify", utilCallbackify, 1 },
            }) |binding| {
                const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
                _ = qjs.JS_SetPropertyStr(ctx, existing_util, binding[0], func);
            }

            // Create/update util.types sub-object
            const existing_types = qjs.JS_GetPropertyStr(ctx, existing_util, "types");
            const types_target = if (qjs.JS_IsUndefined(existing_types) or qjs.JS_IsNull(existing_types))
                qjs.JS_NewObject(ctx)
            else
                existing_types;

            inline for (.{
                .{ "isDate", utilTypesIsDate, 1 },
                .{ "isRegExp", utilTypesIsRegExp, 1 },
                .{ "isPromise", utilTypesIsPromise, 1 },
                .{ "isMap", utilTypesIsMap, 1 },
                .{ "isSet", utilTypesIsSet, 1 },
                .{ "isNativeError", utilTypesIsNativeError, 1 },
            }) |binding| {
                const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
                _ = qjs.JS_SetPropertyStr(ctx, types_target, binding[0], func);
            }

            // If we created a new types object, set it on existing_util
            if (qjs.JS_IsUndefined(existing_types) or qjs.JS_IsNull(existing_types)) {
                _ = qjs.JS_SetPropertyStr(ctx, existing_util, "types", types_target);
            } else {
                qjs.JS_FreeValue(ctx, existing_types);
            }

            // Add TextEncoder/TextDecoder references
            const text_decoder2 = qjs.JS_GetPropertyStr(ctx, global, "TextDecoder");
            _ = qjs.JS_SetPropertyStr(ctx, existing_util, "TextDecoder", text_decoder2);
            const text_encoder2 = qjs.JS_GetPropertyStr(ctx, global, "TextEncoder");
            _ = qjs.JS_SetPropertyStr(ctx, existing_util, "TextEncoder", text_encoder2);

            // Mark as native
            _ = qjs.JS_SetPropertyStr(ctx, existing_util, "__native", qjs.JS_TRUE);

            qjs.JS_FreeValue(ctx, existing_util);
            qjs.JS_FreeValue(ctx, util_obj); // Don't need our new object
        } else {
            // No existing util - set our new object
            if (!qjs.JS_IsUndefined(existing_util)) qjs.JS_FreeValue(ctx, existing_util);
            _ = qjs.JS_SetPropertyStr(ctx, util_obj, "__native", qjs.JS_TRUE);
            _ = qjs.JS_SetPropertyStr(ctx, modules_val, "util", qjs.JS_DupValue(ctx, util_obj));
            _ = qjs.JS_SetPropertyStr(ctx, modules_val, "node:util", util_obj);
        }
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, util_obj);
    }
}
