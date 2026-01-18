/// Native util module - QuickJS C functions
/// Utility functions for Node.js compatibility
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

/// util.format(format, ...args) - String formatting with specifiers
/// Supports %s (string), %d (number), %j/%o/%O (JSON), %% (escape)
fn utilFormat(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (ctx == null) return quickjs.jsUndefined();
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

    const value = argv[0];

    // Handle undefined specially - JSON.stringify returns undefined for undefined
    if (qjs.JS_IsUndefined(value)) {
        return qjs.JS_NewString(ctx, "undefined");
    }

    // Handle functions specially - JSON.stringify returns undefined for functions
    if (qjs.JS_IsFunction(ctx, value)) {
        // Get function name if available
        const name_val = qjs.JS_GetPropertyStr(ctx, value, "name");
        defer qjs.JS_FreeValue(ctx, name_val);

        if (!qjs.JS_IsUndefined(name_val) and !qjs.JS_IsNull(name_val)) {
            const name_cstr = qjs.JS_ToCString(ctx, name_val);
            if (name_cstr != null) {
                defer qjs.JS_FreeCString(ctx, name_cstr);
                const name = std.mem.span(name_cstr);
                if (name.len > 0) {
                    var buf: [256]u8 = undefined;
                    const result = std.fmt.bufPrint(&buf, "[Function: {s}]", .{name}) catch "[Function]";
                    return qjs.JS_NewStringLen(ctx, result.ptr, @intCast(result.len));
                }
            }
        }
        return qjs.JS_NewString(ctx, "[Function (anonymous)]");
    }

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const json_obj = qjs.JS_GetPropertyStr(ctx, global, "JSON");
    defer qjs.JS_FreeValue(ctx, json_obj);

    const stringify = qjs.JS_GetPropertyStr(ctx, json_obj, "stringify");
    defer qjs.JS_FreeValue(ctx, stringify);

    // JSON.stringify(obj, null, 2) for pretty print
    var args = [3]qjs.JSValue{ value, quickjs.jsNull(), qjs.JS_NewInt32(ctx, 2) };
    defer qjs.JS_FreeValue(ctx, args[2]);

    const result = qjs.JS_Call(ctx, stringify, json_obj, 3, &args);
    if (qjs.JS_IsException(result) or qjs.JS_IsUndefined(result)) {
        qjs.JS_FreeValue(ctx, result);
        // Fallback to toString
        return qjs.JS_ToString(ctx, value);
    }

    return result;
}

/// util.deprecate(fn, msg) - Return function as-is (no-op for now)
fn utilDeprecate(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsUndefined();
    return qjs.JS_DupValue(ctx, argv[0]);
}

/// util.inherits(ctor, superCtor) - Set up prototype chain
fn utilInherits(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return quickjs.jsUndefined();

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

    return quickjs.jsUndefined();
}

/// util.isArray(obj) - Check if object is an array
fn utilIsArray(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    const is_array = qjs.JS_IsArray(argv[0]);
    if (is_array) {
        return quickjs.jsTrue();
    }
    return quickjs.jsFalse();
}

/// util.isBuffer(obj) - Check if object is a Buffer
fn utilIsBuffer(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    // Check if it's a Uint8Array
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], uint8array_ctor);
    if (result == 1) {
        return quickjs.jsTrue();
    }
    return quickjs.jsFalse();
}

/// util.isBoolean(obj) - Check if value is a boolean
fn utilIsBoolean(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    return if (qjs.JS_IsBool(argv[0])) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.isNull(obj) - Check if value is null
fn utilIsNull(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    return if (qjs.JS_IsNull(argv[0])) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.isNullOrUndefined(obj) - Check if value is null or undefined
fn utilIsNullOrUndefined(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsTrue(); // No arg is undefined
    return if (qjs.JS_IsNull(argv[0]) or qjs.JS_IsUndefined(argv[0])) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.isNumber(obj) - Check if value is a number
fn utilIsNumber(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    return if (qjs.JS_IsNumber(argv[0])) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.isString(obj) - Check if value is a string
fn utilIsString(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    return if (qjs.JS_IsString(argv[0])) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.isSymbol(obj) - Check if value is a symbol
fn utilIsSymbol(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    return if (qjs.JS_IsSymbol(argv[0])) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.isUndefined(obj) - Check if value is undefined
fn utilIsUndefined(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsTrue(); // No arg is undefined
    return if (qjs.JS_IsUndefined(argv[0])) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.isRegExp(obj) - Check if value is a RegExp
fn utilIsRegExp(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const regexp_ctor = qjs.JS_GetPropertyStr(ctx, global, "RegExp");
    defer qjs.JS_FreeValue(ctx, regexp_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], regexp_ctor);
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.isObject(obj) - Check if value is an object (not null)
fn utilIsObject(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    const val = argv[0];
    // Object means: is object AND not null
    return if (qjs.JS_IsObject(val) and !qjs.JS_IsNull(val)) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.isDate(obj) - Check if value is a Date
fn utilIsDate(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const date_ctor = qjs.JS_GetPropertyStr(ctx, global, "Date");
    defer qjs.JS_FreeValue(ctx, date_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], date_ctor);
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.isError(obj) - Check if value is an Error
fn utilIsError(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const error_ctor = qjs.JS_GetPropertyStr(ctx, global, "Error");
    defer qjs.JS_FreeValue(ctx, error_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], error_ctor);
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.isFunction(obj) - Check if value is a function
fn utilIsFunction(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    return if (qjs.JS_IsFunction(ctx, argv[0])) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.isPrimitive(obj) - Check if value is a primitive (null, undefined, boolean, number, string, symbol)
fn utilIsPrimitive(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsTrue(); // undefined is primitive
    const val = argv[0];
    // Primitive: not an object, or is null
    if (qjs.JS_IsNull(val)) return quickjs.jsTrue();
    if (qjs.JS_IsUndefined(val)) return quickjs.jsTrue();
    if (qjs.JS_IsBool(val)) return quickjs.jsTrue();
    if (qjs.JS_IsNumber(val)) return quickjs.jsTrue();
    if (qjs.JS_IsString(val)) return quickjs.jsTrue();
    if (qjs.JS_IsSymbol(val)) return quickjs.jsTrue();
    return quickjs.jsFalse();
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
    return qjs.JS_Call(ctx, factory, quickjs.jsUndefined(), 1, &call_args);
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
    return qjs.JS_Call(ctx, factory, quickjs.jsUndefined(), 1, &call_args);
}

// ============================================================================
// util.styleText - ANSI terminal styling
// ============================================================================

/// ANSI escape codes for terminal styling
const AnsiCode = struct {
    open: []const u8,
    close: []const u8,
};

/// Map style names to ANSI escape codes
fn getStyleCode(style: []const u8) ?AnsiCode {
    // Color styles
    if (std.mem.eql(u8, style, "reset")) return .{ .open = "\x1b[0m", .close = "\x1b[0m" };
    if (std.mem.eql(u8, style, "bold")) return .{ .open = "\x1b[1m", .close = "\x1b[22m" };
    if (std.mem.eql(u8, style, "dim")) return .{ .open = "\x1b[2m", .close = "\x1b[22m" };
    if (std.mem.eql(u8, style, "italic")) return .{ .open = "\x1b[3m", .close = "\x1b[23m" };
    if (std.mem.eql(u8, style, "underline")) return .{ .open = "\x1b[4m", .close = "\x1b[24m" };
    if (std.mem.eql(u8, style, "inverse")) return .{ .open = "\x1b[7m", .close = "\x1b[27m" };
    if (std.mem.eql(u8, style, "hidden")) return .{ .open = "\x1b[8m", .close = "\x1b[28m" };
    if (std.mem.eql(u8, style, "strikethrough")) return .{ .open = "\x1b[9m", .close = "\x1b[29m" };

    // Foreground colors
    if (std.mem.eql(u8, style, "black")) return .{ .open = "\x1b[30m", .close = "\x1b[39m" };
    if (std.mem.eql(u8, style, "red")) return .{ .open = "\x1b[31m", .close = "\x1b[39m" };
    if (std.mem.eql(u8, style, "green")) return .{ .open = "\x1b[32m", .close = "\x1b[39m" };
    if (std.mem.eql(u8, style, "yellow")) return .{ .open = "\x1b[33m", .close = "\x1b[39m" };
    if (std.mem.eql(u8, style, "blue")) return .{ .open = "\x1b[34m", .close = "\x1b[39m" };
    if (std.mem.eql(u8, style, "magenta")) return .{ .open = "\x1b[35m", .close = "\x1b[39m" };
    if (std.mem.eql(u8, style, "cyan")) return .{ .open = "\x1b[36m", .close = "\x1b[39m" };
    if (std.mem.eql(u8, style, "white")) return .{ .open = "\x1b[37m", .close = "\x1b[39m" };
    if (std.mem.eql(u8, style, "gray") or std.mem.eql(u8, style, "grey")) return .{ .open = "\x1b[90m", .close = "\x1b[39m" };

    // Bright foreground colors
    if (std.mem.eql(u8, style, "redBright")) return .{ .open = "\x1b[91m", .close = "\x1b[39m" };
    if (std.mem.eql(u8, style, "greenBright")) return .{ .open = "\x1b[92m", .close = "\x1b[39m" };
    if (std.mem.eql(u8, style, "yellowBright")) return .{ .open = "\x1b[93m", .close = "\x1b[39m" };
    if (std.mem.eql(u8, style, "blueBright")) return .{ .open = "\x1b[94m", .close = "\x1b[39m" };
    if (std.mem.eql(u8, style, "magentaBright")) return .{ .open = "\x1b[95m", .close = "\x1b[39m" };
    if (std.mem.eql(u8, style, "cyanBright")) return .{ .open = "\x1b[96m", .close = "\x1b[39m" };
    if (std.mem.eql(u8, style, "whiteBright")) return .{ .open = "\x1b[97m", .close = "\x1b[39m" };

    // Background colors
    if (std.mem.eql(u8, style, "bgBlack")) return .{ .open = "\x1b[40m", .close = "\x1b[49m" };
    if (std.mem.eql(u8, style, "bgRed")) return .{ .open = "\x1b[41m", .close = "\x1b[49m" };
    if (std.mem.eql(u8, style, "bgGreen")) return .{ .open = "\x1b[42m", .close = "\x1b[49m" };
    if (std.mem.eql(u8, style, "bgYellow")) return .{ .open = "\x1b[43m", .close = "\x1b[49m" };
    if (std.mem.eql(u8, style, "bgBlue")) return .{ .open = "\x1b[44m", .close = "\x1b[49m" };
    if (std.mem.eql(u8, style, "bgMagenta")) return .{ .open = "\x1b[45m", .close = "\x1b[49m" };
    if (std.mem.eql(u8, style, "bgCyan")) return .{ .open = "\x1b[46m", .close = "\x1b[49m" };
    if (std.mem.eql(u8, style, "bgWhite")) return .{ .open = "\x1b[47m", .close = "\x1b[49m" };

    return null;
}

/// util.styleText(format, text) - Apply ANSI styling to text
/// format can be a string or array of strings
fn utilStyleText(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "styleText requires format and text arguments");

    // Get the text
    const text_cstr = qjs.JS_ToCString(ctx, argv[1]);
    if (text_cstr == null) return qjs.JS_NewString(ctx, "");
    defer qjs.JS_FreeCString(ctx, text_cstr);
    const text = std.mem.span(text_cstr);

    // Buffer for output
    var buffer: [8192]u8 = undefined;
    var pos: usize = 0;

    // Handle single style string
    if (qjs.JS_IsString(argv[0])) {
        const style_cstr = qjs.JS_ToCString(ctx, argv[0]);
        if (style_cstr == null) {
            return qjs.JS_NewStringLen(ctx, text.ptr, @intCast(text.len));
        }
        defer qjs.JS_FreeCString(ctx, style_cstr);
        const style = std.mem.span(style_cstr);

        if (getStyleCode(style)) |code| {
            // Copy open code
            const open_len = @min(code.open.len, buffer.len - pos);
            @memcpy(buffer[pos..][0..open_len], code.open[0..open_len]);
            pos += open_len;

            // Copy text
            const text_len = @min(text.len, buffer.len - pos);
            @memcpy(buffer[pos..][0..text_len], text[0..text_len]);
            pos += text_len;

            // Copy close code
            const close_len = @min(code.close.len, buffer.len - pos);
            @memcpy(buffer[pos..][0..close_len], code.close[0..close_len]);
            pos += close_len;

            return qjs.JS_NewStringLen(ctx, &buffer, @intCast(pos));
        }

        // Unknown style - return text unchanged
        return qjs.JS_NewStringLen(ctx, text.ptr, @intCast(text.len));
    }

    // Handle array of styles
    if (qjs.JS_IsArray(argv[0])) {
        const len_val = qjs.JS_GetPropertyStr(ctx, argv[0], "length");
        defer qjs.JS_FreeValue(ctx, len_val);

        var arr_len: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &arr_len, len_val);

        // Collect open codes
        var open_codes: [32]AnsiCode = undefined;
        var code_count: usize = 0;

        for (0..@intCast(arr_len)) |i| {
            const style_val = qjs.JS_GetPropertyUint32(ctx, argv[0], @intCast(i));
            defer qjs.JS_FreeValue(ctx, style_val);

            const style_cstr = qjs.JS_ToCString(ctx, style_val);
            if (style_cstr != null) {
                defer qjs.JS_FreeCString(ctx, style_cstr);
                const style = std.mem.span(style_cstr);

                if (getStyleCode(style)) |code| {
                    if (code_count < open_codes.len) {
                        open_codes[code_count] = code;
                        code_count += 1;
                    }
                }
            }
        }

        // Write open codes
        for (0..code_count) |i| {
            const open_len = @min(open_codes[i].open.len, buffer.len - pos);
            @memcpy(buffer[pos..][0..open_len], open_codes[i].open[0..open_len]);
            pos += open_len;
        }

        // Write text
        const text_len = @min(text.len, buffer.len - pos);
        @memcpy(buffer[pos..][0..text_len], text[0..text_len]);
        pos += text_len;

        // Write close codes in reverse order
        var j: usize = code_count;
        while (j > 0) {
            j -= 1;
            const close_len = @min(open_codes[j].close.len, buffer.len - pos);
            @memcpy(buffer[pos..][0..close_len], open_codes[j].close[0..close_len]);
            pos += close_len;
        }

        return qjs.JS_NewStringLen(ctx, &buffer, @intCast(pos));
    }

    // Invalid format - return text unchanged
    return qjs.JS_NewStringLen(ctx, text.ptr, @intCast(text.len));
}

// ============================================================================
// util.types - Type checking functions
// ============================================================================

/// util.types.isDate(obj) - Check if object is a Date
fn utilTypesIsDate(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const date_ctor = qjs.JS_GetPropertyStr(ctx, global, "Date");
    defer qjs.JS_FreeValue(ctx, date_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], date_ctor);
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.types.isRegExp(obj) - Check if object is a RegExp
fn utilTypesIsRegExp(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const regexp_ctor = qjs.JS_GetPropertyStr(ctx, global, "RegExp");
    defer qjs.JS_FreeValue(ctx, regexp_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], regexp_ctor);
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.types.isPromise(obj) - Check if object is a Promise
fn utilTypesIsPromise(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const promise_ctor = qjs.JS_GetPropertyStr(ctx, global, "Promise");
    defer qjs.JS_FreeValue(ctx, promise_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], promise_ctor);
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.types.isMap(obj) - Check if object is a Map
fn utilTypesIsMap(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const map_ctor = qjs.JS_GetPropertyStr(ctx, global, "Map");
    defer qjs.JS_FreeValue(ctx, map_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], map_ctor);
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.types.isSet(obj) - Check if object is a Set
fn utilTypesIsSet(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const set_ctor = qjs.JS_GetPropertyStr(ctx, global, "Set");
    defer qjs.JS_FreeValue(ctx, set_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], set_ctor);
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.types.isNativeError(obj) - Check if object is an Error
fn utilTypesIsNativeError(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const error_ctor = qjs.JS_GetPropertyStr(ctx, global, "Error");
    defer qjs.JS_FreeValue(ctx, error_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], error_ctor);
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.types.isArrayBuffer(obj) - Check if object is an ArrayBuffer
fn utilTypesIsArrayBuffer(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const ab_ctor = qjs.JS_GetPropertyStr(ctx, global, "ArrayBuffer");
    defer qjs.JS_FreeValue(ctx, ab_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], ab_ctor);
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.types.isDataView(obj) - Check if object is a DataView
fn utilTypesIsDataView(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const dv_ctor = qjs.JS_GetPropertyStr(ctx, global, "DataView");
    defer qjs.JS_FreeValue(ctx, dv_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], dv_ctor);
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.types.isWeakMap(obj) - Check if object is a WeakMap
fn utilTypesIsWeakMap(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const wm_ctor = qjs.JS_GetPropertyStr(ctx, global, "WeakMap");
    defer qjs.JS_FreeValue(ctx, wm_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], wm_ctor);
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.types.isWeakSet(obj) - Check if object is a WeakSet
fn utilTypesIsWeakSet(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const ws_ctor = qjs.JS_GetPropertyStr(ctx, global, "WeakSet");
    defer qjs.JS_FreeValue(ctx, ws_ctor);

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], ws_ctor);
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// Helper: Check instanceof a typed array constructor
fn checkTypedArrayInstance(ctx: ?*qjs.JSContext, val: qjs.JSValue, ctor_name: [*:0]const u8) qjs.JSValue {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const ctor = qjs.JS_GetPropertyStr(ctx, global, ctor_name);
    defer qjs.JS_FreeValue(ctx, ctor);

    if (qjs.JS_IsUndefined(ctor)) return quickjs.jsFalse();

    const result = qjs.JS_IsInstanceOf(ctx, val, ctor);
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.types.isUint8Array(obj)
fn utilTypesIsUint8Array(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    return checkTypedArrayInstance(ctx, argv[0], "Uint8Array");
}

/// util.types.isUint16Array(obj)
fn utilTypesIsUint16Array(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    return checkTypedArrayInstance(ctx, argv[0], "Uint16Array");
}

/// util.types.isUint32Array(obj)
fn utilTypesIsUint32Array(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    return checkTypedArrayInstance(ctx, argv[0], "Uint32Array");
}

/// util.types.isInt8Array(obj)
fn utilTypesIsInt8Array(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    return checkTypedArrayInstance(ctx, argv[0], "Int8Array");
}

/// util.types.isInt16Array(obj)
fn utilTypesIsInt16Array(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    return checkTypedArrayInstance(ctx, argv[0], "Int16Array");
}

/// util.types.isInt32Array(obj)
fn utilTypesIsInt32Array(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    return checkTypedArrayInstance(ctx, argv[0], "Int32Array");
}

/// util.types.isFloat32Array(obj)
fn utilTypesIsFloat32Array(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    return checkTypedArrayInstance(ctx, argv[0], "Float32Array");
}

/// util.types.isFloat64Array(obj)
fn utilTypesIsFloat64Array(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    return checkTypedArrayInstance(ctx, argv[0], "Float64Array");
}

/// util.types.isBigInt64Array(obj)
fn utilTypesIsBigInt64Array(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    return checkTypedArrayInstance(ctx, argv[0], "BigInt64Array");
}

/// util.types.isBigUint64Array(obj)
fn utilTypesIsBigUint64Array(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();
    return checkTypedArrayInstance(ctx, argv[0], "BigUint64Array");
}

/// util.types.isTypedArray(obj) - Check if any TypedArray
fn utilTypesIsTypedArray(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    // Check against all typed array types
    const typed_array_names = [_][*:0]const u8{
        "Uint8Array",      "Uint8ClampedArray", "Uint16Array",  "Uint32Array",
        "Int8Array",       "Int16Array",        "Int32Array",   "Float32Array",
        "Float64Array",    "BigInt64Array",     "BigUint64Array",
    };

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    for (typed_array_names) |name| {
        const ctor = qjs.JS_GetPropertyStr(ctx, global, name);
        defer qjs.JS_FreeValue(ctx, ctor);

        if (!qjs.JS_IsUndefined(ctor)) {
            const result = qjs.JS_IsInstanceOf(ctx, argv[0], ctor);
            if (result == 1) return quickjs.jsTrue();
        }
    }
    return quickjs.jsFalse();
}

/// util.types.isArrayBufferView(obj) - Check if TypedArray or DataView
fn utilTypesIsArrayBufferView(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    // Check DataView first
    const dv_result = utilTypesIsDataView(ctx, quickjs.jsUndefined(), argc, argv);
    if (qjs.JS_ToBool(ctx, dv_result) == 1) {
        return quickjs.jsTrue();
    }
    // Then check TypedArray
    return utilTypesIsTypedArray(ctx, quickjs.jsUndefined(), argc, argv);
}

/// util.types.isAsyncFunction(obj) - Check if async function
fn utilTypesIsAsyncFunction(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    // Check constructor name
    const ctor = qjs.JS_GetPropertyStr(ctx, argv[0], "constructor");
    defer qjs.JS_FreeValue(ctx, ctor);

    if (qjs.JS_IsUndefined(ctor)) return quickjs.jsFalse();

    const name = qjs.JS_GetPropertyStr(ctx, ctor, "name");
    defer qjs.JS_FreeValue(ctx, name);

    const name_str = qjs.JS_ToCString(ctx, name);
    if (name_str == null) return quickjs.jsFalse();
    defer qjs.JS_FreeCString(ctx, name_str);

    const name_slice = std.mem.span(name_str);
    return if (std.mem.eql(u8, name_slice, "AsyncFunction")) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.types.isGeneratorFunction(obj) - Check if generator function
fn utilTypesIsGeneratorFunction(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const ctor = qjs.JS_GetPropertyStr(ctx, argv[0], "constructor");
    defer qjs.JS_FreeValue(ctx, ctor);

    if (qjs.JS_IsUndefined(ctor)) return quickjs.jsFalse();

    const name = qjs.JS_GetPropertyStr(ctx, ctor, "name");
    defer qjs.JS_FreeValue(ctx, name);

    const name_str = qjs.JS_ToCString(ctx, name);
    if (name_str == null) return quickjs.jsFalse();
    defer qjs.JS_FreeCString(ctx, name_str);

    const name_slice = std.mem.span(name_str);
    return if (std.mem.eql(u8, name_slice, "GeneratorFunction")) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.types.isGeneratorObject(obj) - Check if generator object
fn utilTypesIsGeneratorObject(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    // Check for next and return methods (generator protocol)
    const next = qjs.JS_GetPropertyStr(ctx, argv[0], "next");
    defer qjs.JS_FreeValue(ctx, next);

    if (!qjs.JS_IsFunction(ctx, next)) return quickjs.jsFalse();

    const ret = qjs.JS_GetPropertyStr(ctx, argv[0], "return");
    defer qjs.JS_FreeValue(ctx, ret);

    if (!qjs.JS_IsFunction(ctx, ret)) return quickjs.jsFalse();

    return quickjs.jsTrue();
}

/// util.types.isSharedArrayBuffer(obj)
fn utilTypesIsSharedArrayBuffer(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsFalse();

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const sab_ctor = qjs.JS_GetPropertyStr(ctx, global, "SharedArrayBuffer");
    defer qjs.JS_FreeValue(ctx, sab_ctor);

    if (qjs.JS_IsUndefined(sab_ctor)) return quickjs.jsFalse();

    const result = qjs.JS_IsInstanceOf(ctx, argv[0], sab_ctor);
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// Register util module
pub fn register(ctx: *qjs.JSContext) void {
    const util_obj = qjs.JS_NewObject(ctx);

    // Mark as native so JS polyfill doesn't override
    _ = qjs.JS_SetPropertyStr(ctx, util_obj, "__native", quickjs.jsTrue());

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
        .{ "styleText", utilStyleText, 2 },
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
        .{ "isArrayBuffer", utilTypesIsArrayBuffer, 1 },
        .{ "isDataView", utilTypesIsDataView, 1 },
        .{ "isWeakMap", utilTypesIsWeakMap, 1 },
        .{ "isWeakSet", utilTypesIsWeakSet, 1 },
        .{ "isTypedArray", utilTypesIsTypedArray, 1 },
        .{ "isArrayBufferView", utilTypesIsArrayBufferView, 1 },
        .{ "isUint8Array", utilTypesIsUint8Array, 1 },
        .{ "isUint16Array", utilTypesIsUint16Array, 1 },
        .{ "isUint32Array", utilTypesIsUint32Array, 1 },
        .{ "isInt8Array", utilTypesIsInt8Array, 1 },
        .{ "isInt16Array", utilTypesIsInt16Array, 1 },
        .{ "isInt32Array", utilTypesIsInt32Array, 1 },
        .{ "isFloat32Array", utilTypesIsFloat32Array, 1 },
        .{ "isFloat64Array", utilTypesIsFloat64Array, 1 },
        .{ "isBigInt64Array", utilTypesIsBigInt64Array, 1 },
        .{ "isBigUint64Array", utilTypesIsBigUint64Array, 1 },
        .{ "isAsyncFunction", utilTypesIsAsyncFunction, 1 },
        .{ "isGeneratorFunction", utilTypesIsGeneratorFunction, 1 },
        .{ "isGeneratorObject", utilTypesIsGeneratorObject, 1 },
        .{ "isSharedArrayBuffer", utilTypesIsSharedArrayBuffer, 1 },
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
                .{ "styleText", utilStyleText, 2 },
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
            _ = qjs.JS_SetPropertyStr(ctx, existing_util, "__native", quickjs.jsTrue());

            qjs.JS_FreeValue(ctx, existing_util);
            qjs.JS_FreeValue(ctx, util_obj); // Don't need our new object
        } else {
            // No existing util - set our new object
            if (!qjs.JS_IsUndefined(existing_util)) qjs.JS_FreeValue(ctx, existing_util);
            _ = qjs.JS_SetPropertyStr(ctx, util_obj, "__native", quickjs.jsTrue());
            _ = qjs.JS_SetPropertyStr(ctx, modules_val, "util", qjs.JS_DupValue(ctx, util_obj));
            _ = qjs.JS_SetPropertyStr(ctx, modules_val, "node:util", util_obj);
        }
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, util_obj);
    }
}
