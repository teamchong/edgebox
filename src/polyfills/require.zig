/// Native require() function - handles module resolution at native level
/// No JS fallback for known modules - native handles everything
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// Import native module implementations
const util_polyfill = @import("util.zig");
const path_polyfill = @import("path.zig");
const buffer_polyfill = @import("buffer.zig");
const process_polyfill = @import("process.zig");
const url_polyfill = @import("url.zig");
const querystring_polyfill = @import("querystring.zig");
const crypto_polyfill = @import("crypto.zig");

/// Native require(name) - module resolution without JS fallback for known modules
fn nativeRequire(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_ThrowTypeError(ctx, "require() needs a module name");
    }

    const name_str = qjs.JS_ToCString(ctx, argv[0]);
    if (name_str == null) {
        return qjs.JS_ThrowTypeError(ctx, "require() module name must be a string");
    }
    defer qjs.JS_FreeCString(ctx, name_str);

    const name = std.mem.span(name_str);

    // Strip 'node:' prefix if present
    const module_name = if (std.mem.startsWith(u8, name, "node:"))
        name[5..]
    else
        name;

    // Check native modules - NO FALLBACK for these
    if (std.mem.eql(u8, module_name, "util")) {
        return createUtilModule(ctx);
    }

    // For other modules, look up in _modules (JS polyfill territory)
    // This is the ONLY fallback - for modules we don't handle natively
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (qjs.JS_IsUndefined(modules_val)) {
        qjs.JS_FreeValue(ctx, modules_val);
        return qjs.JS_ThrowReferenceError(ctx, "Module not found: %s", name_str);
    }
    defer qjs.JS_FreeValue(ctx, modules_val);

    // Try exact name first
    var result = qjs.JS_GetPropertyStr(ctx, modules_val, name_str);
    if (!qjs.JS_IsUndefined(result)) {
        return result;
    }
    qjs.JS_FreeValue(ctx, result);

    // Try without node: prefix
    if (std.mem.startsWith(u8, name, "node:")) {
        const short_name = name[5..];
        result = qjs.JS_GetPropertyStr(ctx, modules_val, short_name.ptr);
        if (!qjs.JS_IsUndefined(result)) {
            return result;
        }
        qjs.JS_FreeValue(ctx, result);
    }

    return qjs.JS_ThrowReferenceError(ctx, "Module not found: %s", name_str);
}

/// Create native util module object - complete implementation, no JS involved
fn createUtilModule(ctx: ?*qjs.JSContext) qjs.JSValue {
    const util_obj = qjs.JS_NewObject(ctx);

    // Get global for TextEncoder/TextDecoder
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Main util functions - all native
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "format", qjs.JS_NewCFunction(ctx, utilFormat, "format", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "inspect", qjs.JS_NewCFunction(ctx, utilInspect, "inspect", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "promisify", qjs.JS_NewCFunction(ctx, utilPromisify, "promisify", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "callbackify", qjs.JS_NewCFunction(ctx, utilCallbackify, "callbackify", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "deprecate", qjs.JS_NewCFunction(ctx, utilDeprecate, "deprecate", 2));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "inherits", qjs.JS_NewCFunction(ctx, utilInherits, "inherits", 2));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "debuglog", qjs.JS_NewCFunction(ctx, utilDebuglog, "debuglog", 1));

    // Type checking functions
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "isArray", qjs.JS_NewCFunction(ctx, utilIsArray, "isArray", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "isBoolean", qjs.JS_NewCFunction(ctx, utilIsBoolean, "isBoolean", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "isNull", qjs.JS_NewCFunction(ctx, utilIsNull, "isNull", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "isNullOrUndefined", qjs.JS_NewCFunction(ctx, utilIsNullOrUndefined, "isNullOrUndefined", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "isNumber", qjs.JS_NewCFunction(ctx, utilIsNumber, "isNumber", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "isString", qjs.JS_NewCFunction(ctx, utilIsString, "isString", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "isSymbol", qjs.JS_NewCFunction(ctx, utilIsSymbol, "isSymbol", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "isUndefined", qjs.JS_NewCFunction(ctx, utilIsUndefined, "isUndefined", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "isRegExp", qjs.JS_NewCFunction(ctx, utilIsRegExp, "isRegExp", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "isObject", qjs.JS_NewCFunction(ctx, utilIsObject, "isObject", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "isDate", qjs.JS_NewCFunction(ctx, utilIsDate, "isDate", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "isError", qjs.JS_NewCFunction(ctx, utilIsError, "isError", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "isFunction", qjs.JS_NewCFunction(ctx, utilIsFunction, "isFunction", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "isBuffer", qjs.JS_NewCFunction(ctx, utilIsBuffer, "isBuffer", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "isPrimitive", qjs.JS_NewCFunction(ctx, utilIsPrimitive, "isPrimitive", 1));

    // util.types sub-object
    const types_obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, types_obj, "isDate", qjs.JS_NewCFunction(ctx, utilTypesIsDate, "isDate", 1));
    _ = qjs.JS_SetPropertyStr(ctx, types_obj, "isRegExp", qjs.JS_NewCFunction(ctx, utilTypesIsRegExp, "isRegExp", 1));
    _ = qjs.JS_SetPropertyStr(ctx, types_obj, "isPromise", qjs.JS_NewCFunction(ctx, utilTypesIsPromise, "isPromise", 1));
    _ = qjs.JS_SetPropertyStr(ctx, types_obj, "isMap", qjs.JS_NewCFunction(ctx, utilTypesIsMap, "isMap", 1));
    _ = qjs.JS_SetPropertyStr(ctx, types_obj, "isSet", qjs.JS_NewCFunction(ctx, utilTypesIsSet, "isSet", 1));
    _ = qjs.JS_SetPropertyStr(ctx, types_obj, "isNativeError", qjs.JS_NewCFunction(ctx, utilTypesIsNativeError, "isNativeError", 1));
    _ = qjs.JS_SetPropertyStr(ctx, types_obj, "isAsyncFunction", qjs.JS_NewCFunction(ctx, utilTypesIsAsyncFunction, "isAsyncFunction", 1));
    _ = qjs.JS_SetPropertyStr(ctx, types_obj, "isGeneratorFunction", qjs.JS_NewCFunction(ctx, utilTypesIsGeneratorFunction, "isGeneratorFunction", 1));
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "types", types_obj);

    // TextEncoder/TextDecoder references
    const text_encoder = qjs.JS_GetPropertyStr(ctx, global, "TextEncoder");
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "TextEncoder", text_encoder);
    const text_decoder = qjs.JS_GetPropertyStr(ctx, global, "TextDecoder");
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "TextDecoder", text_decoder);

    // Mark as native
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "__native", qjs.JS_TRUE);

    return util_obj;
}

// ============================================================================
// Util function implementations (all native, no JS eval)
// ============================================================================

fn utilFormat(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    var buffer: [4096]u8 = undefined;
    var pos: usize = 0;

    var i: usize = 0;
    while (i < @as(usize, @intCast(argc))) : (i += 1) {
        if (i > 0 and pos < buffer.len) {
            buffer[pos] = ' ';
            pos += 1;
        }

        const str = qjs.JS_ToCString(ctx, argv[i]);
        if (str != null) {
            defer qjs.JS_FreeCString(ctx, str);
            const text = std.mem.span(str);
            const len = @min(text.len, buffer.len - pos);
            @memcpy(buffer[pos..][0..len], text[0..len]);
            pos += len;
        }
    }

    return qjs.JS_NewStringLen(ctx, &buffer, @intCast(pos));
}

fn utilInspect(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "undefined");

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const json_obj = qjs.JS_GetPropertyStr(ctx, global, "JSON");
    defer qjs.JS_FreeValue(ctx, json_obj);

    const stringify_func = qjs.JS_GetPropertyStr(ctx, json_obj, "stringify");
    defer qjs.JS_FreeValue(ctx, stringify_func);

    var stringify_args = [1]qjs.JSValue{argv[0]};
    const result = qjs.JS_Call(ctx, stringify_func, json_obj, 1, &stringify_args);

    if (qjs.JS_IsException(result)) {
        qjs.JS_FreeValue(ctx, result);
        return qjs.JS_ToString(ctx, argv[0]);
    }

    return result;
}

fn utilPromisify(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "promisify requires a function argument");

    const original_fn = argv[0];
    if (!qjs.JS_IsFunction(ctx, original_fn)) {
        return qjs.JS_ThrowTypeError(ctx, "argument must be a function");
    }

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

    const factory = qjs.JS_Eval(ctx, wrapper_code.ptr, wrapper_code.len, "<promisify>", qjs.JS_EVAL_TYPE_GLOBAL);
    if (qjs.JS_IsException(factory)) {
        return factory;
    }
    defer qjs.JS_FreeValue(ctx, factory);

    var call_args = [1]qjs.JSValue{original_fn};
    return qjs.JS_Call(ctx, factory, qjs.JS_UNDEFINED, 1, &call_args);
}

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

fn utilDeprecate(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_UNDEFINED;
    return qjs.JS_DupValue(ctx, argv[0]);
}

fn utilInherits(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_UNDEFINED;

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const object_ctor = qjs.JS_GetPropertyStr(ctx, global, "Object");
    defer qjs.JS_FreeValue(ctx, object_ctor);

    const set_proto_func = qjs.JS_GetPropertyStr(ctx, object_ctor, "setPrototypeOf");
    defer qjs.JS_FreeValue(ctx, set_proto_func);

    const ctor_proto = qjs.JS_GetPropertyStr(ctx, argv[0], "prototype");
    defer qjs.JS_FreeValue(ctx, ctor_proto);

    const super_proto = qjs.JS_GetPropertyStr(ctx, argv[1], "prototype");
    defer qjs.JS_FreeValue(ctx, super_proto);

    var set_proto_args = [2]qjs.JSValue{ ctor_proto, super_proto };
    const result = qjs.JS_Call(ctx, set_proto_func, object_ctor, 2, &set_proto_args);
    qjs.JS_FreeValue(ctx, result);

    return qjs.JS_UNDEFINED;
}

fn utilDebuglog(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const noop_code = "(function(){})";
    return qjs.JS_Eval(ctx, noop_code.ptr, noop_code.len, "<debuglog>", qjs.JS_EVAL_TYPE_GLOBAL);
}

// Type checking functions
fn utilIsArray(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    return if (qjs.JS_IsArray(argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilIsBoolean(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    return if (qjs.JS_IsBool(argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilIsNull(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    return if (qjs.JS_IsNull(argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilIsNullOrUndefined(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_TRUE;
    return if (qjs.JS_IsNull(argv[0]) or qjs.JS_IsUndefined(argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilIsNumber(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    return if (qjs.JS_IsNumber(argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilIsString(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    return if (qjs.JS_IsString(argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilIsSymbol(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    return if (qjs.JS_IsSymbol(argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilIsUndefined(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_TRUE;
    return if (qjs.JS_IsUndefined(argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilIsRegExp(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);
    const regexp_ctor = qjs.JS_GetPropertyStr(ctx, global, "RegExp");
    defer qjs.JS_FreeValue(ctx, regexp_ctor);
    return if (qjs.JS_IsInstanceOf(ctx, argv[0], regexp_ctor) == 1) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilIsObject(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    return if (qjs.JS_IsObject(argv[0]) and !qjs.JS_IsNull(argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilIsDate(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);
    const date_ctor = qjs.JS_GetPropertyStr(ctx, global, "Date");
    defer qjs.JS_FreeValue(ctx, date_ctor);
    return if (qjs.JS_IsInstanceOf(ctx, argv[0], date_ctor) == 1) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilIsError(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);
    const error_ctor = qjs.JS_GetPropertyStr(ctx, global, "Error");
    defer qjs.JS_FreeValue(ctx, error_ctor);
    return if (qjs.JS_IsInstanceOf(ctx, argv[0], error_ctor) == 1) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilIsFunction(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    return if (qjs.JS_IsFunction(ctx, argv[0])) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilIsBuffer(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);
    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);
    return if (qjs.JS_IsInstanceOf(ctx, argv[0], uint8array_ctor) == 1) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilIsPrimitive(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_TRUE;
    const val = argv[0];
    if (qjs.JS_IsNull(val)) return qjs.JS_TRUE;
    if (qjs.JS_IsUndefined(val)) return qjs.JS_TRUE;
    if (qjs.JS_IsBool(val)) return qjs.JS_TRUE;
    if (qjs.JS_IsNumber(val)) return qjs.JS_TRUE;
    if (qjs.JS_IsString(val)) return qjs.JS_TRUE;
    if (qjs.JS_IsSymbol(val)) return qjs.JS_TRUE;
    return qjs.JS_FALSE;
}

// util.types functions
fn utilTypesIsDate(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return utilIsDate(ctx, qjs.JS_UNDEFINED, argc, argv);
}

fn utilTypesIsRegExp(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return utilIsRegExp(ctx, qjs.JS_UNDEFINED, argc, argv);
}

fn utilTypesIsPromise(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);
    const promise_ctor = qjs.JS_GetPropertyStr(ctx, global, "Promise");
    defer qjs.JS_FreeValue(ctx, promise_ctor);
    return if (qjs.JS_IsInstanceOf(ctx, argv[0], promise_ctor) == 1) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilTypesIsMap(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);
    const map_ctor = qjs.JS_GetPropertyStr(ctx, global, "Map");
    defer qjs.JS_FreeValue(ctx, map_ctor);
    return if (qjs.JS_IsInstanceOf(ctx, argv[0], map_ctor) == 1) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilTypesIsSet(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);
    const set_ctor = qjs.JS_GetPropertyStr(ctx, global, "Set");
    defer qjs.JS_FreeValue(ctx, set_ctor);
    return if (qjs.JS_IsInstanceOf(ctx, argv[0], set_ctor) == 1) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilTypesIsNativeError(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return utilIsError(ctx, qjs.JS_UNDEFINED, argc, argv);
}

fn utilTypesIsAsyncFunction(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    if (!qjs.JS_IsFunction(ctx, argv[0])) return qjs.JS_FALSE;

    // Check constructor.name === 'AsyncFunction'
    const ctor = qjs.JS_GetPropertyStr(ctx, argv[0], "constructor");
    if (qjs.JS_IsUndefined(ctor)) return qjs.JS_FALSE;
    defer qjs.JS_FreeValue(ctx, ctor);

    const name = qjs.JS_GetPropertyStr(ctx, ctor, "name");
    if (qjs.JS_IsUndefined(name)) return qjs.JS_FALSE;
    defer qjs.JS_FreeValue(ctx, name);

    const name_str = qjs.JS_ToCString(ctx, name);
    if (name_str == null) return qjs.JS_FALSE;
    defer qjs.JS_FreeCString(ctx, name_str);

    return if (std.mem.eql(u8, std.mem.span(name_str), "AsyncFunction")) qjs.JS_TRUE else qjs.JS_FALSE;
}

fn utilTypesIsGeneratorFunction(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_FALSE;
    if (!qjs.JS_IsFunction(ctx, argv[0])) return qjs.JS_FALSE;

    const ctor = qjs.JS_GetPropertyStr(ctx, argv[0], "constructor");
    if (qjs.JS_IsUndefined(ctor)) return qjs.JS_FALSE;
    defer qjs.JS_FreeValue(ctx, ctor);

    const name = qjs.JS_GetPropertyStr(ctx, ctor, "name");
    if (qjs.JS_IsUndefined(name)) return qjs.JS_FALSE;
    defer qjs.JS_FreeValue(ctx, name);

    const name_str = qjs.JS_ToCString(ctx, name);
    if (name_str == null) return qjs.JS_FALSE;
    defer qjs.JS_FreeCString(ctx, name_str);

    return if (std.mem.eql(u8, std.mem.span(name_str), "GeneratorFunction")) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// Register native require function - NOT overwritable by JS
pub fn register(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Create native require function
    const require_func = qjs.JS_NewCFunction(ctx, nativeRequire, "require", 1);

    // Add require.resolve stub
    const resolve_code = "(function(name) { return name; })";
    const resolve_func = qjs.JS_Eval(ctx, resolve_code.ptr, resolve_code.len, "<require.resolve>", qjs.JS_EVAL_TYPE_GLOBAL);
    _ = qjs.JS_SetPropertyStr(ctx, require_func, "resolve", resolve_func);

    // Add require.cache stub
    const cache_obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, require_func, "cache", cache_obj);

    // Set require on global - JS polyfill will try to overwrite but we'll check if native exists
    _ = qjs.JS_SetPropertyStr(ctx, global, "require", require_func);
}
