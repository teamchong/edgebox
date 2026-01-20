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

/// util.inspect(obj, options) - Return string representation using JSON.stringify
/// Options: { depth: number, colors: boolean, showHidden: boolean, compact: boolean }
fn utilInspect(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "undefined");

    const value = argv[0];

    // Parse options (second argument)
    var max_depth: i32 = 2;
    var use_colors: bool = false;
    var compact: bool = true;
    // showHidden is acknowledged but JSON.stringify can't show non-enumerable properties

    if (argc >= 2 and !qjs.JS_IsUndefined(argv[1]) and !qjs.JS_IsNull(argv[1])) {
        const opts = argv[1];

        // Parse depth
        const depth_val = qjs.JS_GetPropertyStr(ctx, opts, "depth");
        if (!qjs.JS_IsUndefined(depth_val)) {
            var depth_num: i32 = 0;
            _ = qjs.JS_ToInt32(ctx, &depth_num, depth_val);
            if (depth_num >= 0) max_depth = depth_num;
        }
        qjs.JS_FreeValue(ctx, depth_val);

        // Parse colors
        const colors_val = qjs.JS_GetPropertyStr(ctx, opts, "colors");
        if (!qjs.JS_IsUndefined(colors_val)) {
            use_colors = qjs.JS_ToBool(ctx, colors_val) == 1;
        }
        qjs.JS_FreeValue(ctx, colors_val);

        // Parse compact
        const compact_val = qjs.JS_GetPropertyStr(ctx, opts, "compact");
        if (!qjs.JS_IsUndefined(compact_val)) {
            compact = qjs.JS_ToBool(ctx, compact_val) == 1;
        }
        qjs.JS_FreeValue(ctx, compact_val);
    }

    // Handle undefined specially - JSON.stringify returns undefined for undefined
    if (qjs.JS_IsUndefined(value)) {
        if (use_colors) {
            return qjs.JS_NewString(ctx, "\x1b[90mundefined\x1b[39m");
        }
        return qjs.JS_NewString(ctx, "undefined");
    }

    // Handle null
    if (qjs.JS_IsNull(value)) {
        if (use_colors) {
            return qjs.JS_NewString(ctx, "\x1b[1mnull\x1b[22m");
        }
        return qjs.JS_NewString(ctx, "null");
    }

    // Handle booleans with colors
    if (qjs.JS_IsBool(value)) {
        const is_true = qjs.JS_ToBool(ctx, value) == 1;
        if (use_colors) {
            return qjs.JS_NewString(ctx, if (is_true) "\x1b[33mtrue\x1b[39m" else "\x1b[33mfalse\x1b[39m");
        }
        return qjs.JS_NewString(ctx, if (is_true) "true" else "false");
    }

    // Handle numbers with colors
    if (qjs.JS_IsNumber(value)) {
        var num: f64 = 0;
        _ = qjs.JS_ToFloat64(ctx, &num, value);
        var buf: [64]u8 = undefined;
        const num_str = if (num == @trunc(num) and @abs(num) < 9007199254740992)
            std.fmt.bufPrint(&buf, "{d}", .{@as(i64, @intFromFloat(num))}) catch "NaN"
        else
            std.fmt.bufPrint(&buf, "{d}", .{num}) catch "NaN";

        if (use_colors) {
            var color_buf: [80]u8 = undefined;
            const colored = std.fmt.bufPrint(&color_buf, "\x1b[33m{s}\x1b[39m", .{num_str}) catch num_str;
            return qjs.JS_NewStringLen(ctx, colored.ptr, @intCast(colored.len));
        }
        return qjs.JS_NewStringLen(ctx, num_str.ptr, @intCast(num_str.len));
    }

    // Handle strings with colors (show quotes)
    if (qjs.JS_IsString(value)) {
        const str_cstr = qjs.JS_ToCString(ctx, value);
        if (str_cstr != null) {
            defer qjs.JS_FreeCString(ctx, str_cstr);
            const str_slice = std.mem.span(str_cstr);
            var buf: [4096]u8 = undefined;
            if (use_colors) {
                const result = std.fmt.bufPrint(&buf, "\x1b[32m'{s}'\x1b[39m", .{str_slice}) catch return qjs.JS_DupValue(ctx, value);
                return qjs.JS_NewStringLen(ctx, result.ptr, @intCast(result.len));
            } else {
                const result = std.fmt.bufPrint(&buf, "'{s}'", .{str_slice}) catch return qjs.JS_DupValue(ctx, value);
                return qjs.JS_NewStringLen(ctx, result.ptr, @intCast(result.len));
            }
        }
    }

    // Handle functions specially - JSON.stringify returns undefined for functions
    if (qjs.JS_IsFunction(ctx, value)) {
        const name_val = qjs.JS_GetPropertyStr(ctx, value, "name");
        defer qjs.JS_FreeValue(ctx, name_val);

        if (!qjs.JS_IsUndefined(name_val) and !qjs.JS_IsNull(name_val)) {
            const name_cstr = qjs.JS_ToCString(ctx, name_val);
            if (name_cstr != null) {
                defer qjs.JS_FreeCString(ctx, name_cstr);
                const name = std.mem.span(name_cstr);
                if (name.len > 0) {
                    var buf: [256]u8 = undefined;
                    if (use_colors) {
                        const result = std.fmt.bufPrint(&buf, "\x1b[36m[Function: {s}]\x1b[39m", .{name}) catch "[Function]";
                        return qjs.JS_NewStringLen(ctx, result.ptr, @intCast(result.len));
                    }
                    const result = std.fmt.bufPrint(&buf, "[Function: {s}]", .{name}) catch "[Function]";
                    return qjs.JS_NewStringLen(ctx, result.ptr, @intCast(result.len));
                }
            }
        }
        if (use_colors) {
            return qjs.JS_NewString(ctx, "\x1b[36m[Function (anonymous)]\x1b[39m");
        }
        return qjs.JS_NewString(ctx, "[Function (anonymous)]");
    }

    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const json_obj = qjs.JS_GetPropertyStr(ctx, global, "JSON");
    defer qjs.JS_FreeValue(ctx, json_obj);

    const stringify = qjs.JS_GetPropertyStr(ctx, json_obj, "stringify");
    defer qjs.JS_FreeValue(ctx, stringify);

    // JSON.stringify(obj, null, indent) for pretty print
    const indent = if (compact) qjs.JS_NewInt32(ctx, 0) else qjs.JS_NewInt32(ctx, 2);
    var args = [3]qjs.JSValue{ value, quickjs.jsNull(), indent };
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

// ============================================================================
// util.getSystemErrorName - Map errno to error name
// ============================================================================

/// POSIX errno to name mapping (common error codes)
const errno_map = [_]struct { code: i32, name: []const u8 }{
    .{ .code = 1, .name = "EPERM" },
    .{ .code = 2, .name = "ENOENT" },
    .{ .code = 3, .name = "ESRCH" },
    .{ .code = 4, .name = "EINTR" },
    .{ .code = 5, .name = "EIO" },
    .{ .code = 6, .name = "ENXIO" },
    .{ .code = 7, .name = "E2BIG" },
    .{ .code = 8, .name = "ENOEXEC" },
    .{ .code = 9, .name = "EBADF" },
    .{ .code = 10, .name = "ECHILD" },
    .{ .code = 11, .name = "EAGAIN" },
    .{ .code = 12, .name = "ENOMEM" },
    .{ .code = 13, .name = "EACCES" },
    .{ .code = 14, .name = "EFAULT" },
    .{ .code = 15, .name = "ENOTBLK" },
    .{ .code = 16, .name = "EBUSY" },
    .{ .code = 17, .name = "EEXIST" },
    .{ .code = 18, .name = "EXDEV" },
    .{ .code = 19, .name = "ENODEV" },
    .{ .code = 20, .name = "ENOTDIR" },
    .{ .code = 21, .name = "EISDIR" },
    .{ .code = 22, .name = "EINVAL" },
    .{ .code = 23, .name = "ENFILE" },
    .{ .code = 24, .name = "EMFILE" },
    .{ .code = 25, .name = "ENOTTY" },
    .{ .code = 26, .name = "ETXTBSY" },
    .{ .code = 27, .name = "EFBIG" },
    .{ .code = 28, .name = "ENOSPC" },
    .{ .code = 29, .name = "ESPIPE" },
    .{ .code = 30, .name = "EROFS" },
    .{ .code = 31, .name = "EMLINK" },
    .{ .code = 32, .name = "EPIPE" },
    .{ .code = 33, .name = "EDOM" },
    .{ .code = 34, .name = "ERANGE" },
    .{ .code = 35, .name = "EDEADLK" },
    .{ .code = 36, .name = "ENAMETOOLONG" },
    .{ .code = 37, .name = "ENOLCK" },
    .{ .code = 38, .name = "ENOSYS" },
    .{ .code = 39, .name = "ENOTEMPTY" },
    .{ .code = 40, .name = "ELOOP" },
    .{ .code = 42, .name = "ENOMSG" },
    .{ .code = 43, .name = "EIDRM" },
    .{ .code = 60, .name = "ENOSTR" },
    .{ .code = 61, .name = "ENODATA" },
    .{ .code = 62, .name = "ETIME" },
    .{ .code = 63, .name = "ENOSR" },
    .{ .code = 66, .name = "EREMOTE" },
    .{ .code = 67, .name = "ENOLINK" },
    .{ .code = 71, .name = "EPROTO" },
    .{ .code = 72, .name = "EMULTIHOP" },
    .{ .code = 74, .name = "EBADMSG" },
    .{ .code = 75, .name = "EOVERFLOW" },
    .{ .code = 84, .name = "EILSEQ" },
    .{ .code = 88, .name = "ENOTSOCK" },
    .{ .code = 89, .name = "EDESTADDRREQ" },
    .{ .code = 90, .name = "EMSGSIZE" },
    .{ .code = 91, .name = "EPROTOTYPE" },
    .{ .code = 92, .name = "ENOPROTOOPT" },
    .{ .code = 93, .name = "EPROTONOSUPPORT" },
    .{ .code = 94, .name = "ESOCKTNOSUPPORT" },
    .{ .code = 95, .name = "ENOTSUP" },
    .{ .code = 97, .name = "EAFNOSUPPORT" },
    .{ .code = 98, .name = "EADDRINUSE" },
    .{ .code = 99, .name = "EADDRNOTAVAIL" },
    .{ .code = 100, .name = "ENETDOWN" },
    .{ .code = 101, .name = "ENETUNREACH" },
    .{ .code = 102, .name = "ENETRESET" },
    .{ .code = 103, .name = "ECONNABORTED" },
    .{ .code = 104, .name = "ECONNRESET" },
    .{ .code = 105, .name = "ENOBUFS" },
    .{ .code = 106, .name = "EISCONN" },
    .{ .code = 107, .name = "ENOTCONN" },
    .{ .code = 110, .name = "ETIMEDOUT" },
    .{ .code = 111, .name = "ECONNREFUSED" },
    .{ .code = 112, .name = "EHOSTDOWN" },
    .{ .code = 113, .name = "EHOSTUNREACH" },
    .{ .code = 114, .name = "EALREADY" },
    .{ .code = 115, .name = "EINPROGRESS" },
    .{ .code = 116, .name = "ESTALE" },
    .{ .code = 122, .name = "EDQUOT" },
    .{ .code = 125, .name = "ECANCELED" },
};

/// util.getSystemErrorName(errno) - Get system error name from errno
fn utilGetSystemErrorName(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsUndefined();

    var errno: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &errno, argv[0]);

    // Look up in our errno map
    for (errno_map) |entry| {
        if (entry.code == errno) {
            return qjs.JS_NewStringLen(ctx, entry.name.ptr, @intCast(entry.name.len));
        }
    }

    // Unknown errno - return undefined (Node.js behavior)
    return quickjs.jsUndefined();
}

/// util.getSystemErrorMap() - Get map of errno codes to names
fn utilGetSystemErrorMap(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const map_ctor = qjs.JS_GetPropertyStr(ctx, global, "Map");
    defer qjs.JS_FreeValue(ctx, map_ctor);

    const map_obj = qjs.JS_CallConstructor(ctx, map_ctor, 0, null);

    const set_func = qjs.JS_GetPropertyStr(ctx, map_obj, "set");
    defer qjs.JS_FreeValue(ctx, set_func);

    for (errno_map) |entry| {
        const key = qjs.JS_NewInt32(ctx, entry.code);
        const val = qjs.JS_NewStringLen(ctx, entry.name.ptr, @intCast(entry.name.len));
        var args = [2]qjs.JSValue{ key, val };
        const result = qjs.JS_Call(ctx, set_func, map_obj, 2, &args);
        qjs.JS_FreeValue(ctx, result);
        qjs.JS_FreeValue(ctx, key);
        qjs.JS_FreeValue(ctx, val);
    }

    return map_obj;
}

/// util.debuglog(section) - Returns a debug function that logs when NODE_DEBUG contains section
fn utilDebuglog(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        // Return a no-op function if no section provided
        const noop_code = "(function(){})";
        return qjs.JS_Eval(ctx, noop_code.ptr, noop_code.len, "<debuglog>", qjs.JS_EVAL_TYPE_GLOBAL);
    }

    // Get the section name
    const section_cstr = qjs.JS_ToCString(ctx, argv[0]);
    if (section_cstr == null) {
        const noop_code = "(function(){})";
        return qjs.JS_Eval(ctx, noop_code.ptr, noop_code.len, "<debuglog>", qjs.JS_EVAL_TYPE_GLOBAL);
    }
    defer qjs.JS_FreeCString(ctx, section_cstr);
    const section = std.mem.span(section_cstr);

    // Check if NODE_DEBUG env var contains this section
    const node_debug = std.posix.getenv("NODE_DEBUG") orelse "";
    var enabled = false;

    if (node_debug.len > 0) {
        // NODE_DEBUG can be comma or space separated
        var it = std.mem.tokenizeAny(u8, node_debug, ", ");
        while (it.next()) |token| {
            if (std.ascii.eqlIgnoreCase(token, section)) {
                enabled = true;
                break;
            }
            // Also check for wildcard *
            if (std.mem.eql(u8, token, "*")) {
                enabled = true;
                break;
            }
        }
    }

    if (enabled) {
        // Return a function that logs with section prefix
        var buf: [512]u8 = undefined;
        const logger_code = std.fmt.bufPrint(&buf,
            \\(function(section) {{
            \\    return function(...args) {{
            \\        console.error(section.toUpperCase() + ':', ...args);
            \\    }};
            \\}})('{s}')
        , .{section}) catch {
            const noop_code = "(function(){})";
            return qjs.JS_Eval(ctx, noop_code.ptr, noop_code.len, "<debuglog>", qjs.JS_EVAL_TYPE_GLOBAL);
        };

        return qjs.JS_Eval(ctx, logger_code.ptr, @intCast(logger_code.len), "<debuglog>", qjs.JS_EVAL_TYPE_GLOBAL);
    }

    // Not enabled - return no-op
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

// ============================================================================
// util.isDeepStrictEqual - Deep comparison
// ============================================================================

/// Recursively compare two JS values for deep strict equality
/// Returns 1 if equal, 0 if not equal, -1 on error
const is_wasm = @import("builtin").cpu.arch == .wasm32 or @import("builtin").cpu.arch == .wasm64;

fn deepEqual(ctx: ?*qjs.JSContext, val1: qjs.JSValue, val2: qjs.JSValue, depth: u32) i32 {
    // Prevent stack overflow
    if (depth > 100) return 0;

    // Same reference - handle platform differences
    // On WASM32, JSValue is u64 (NaN-boxed); on native, it's a struct
    if (comptime is_wasm) {
        if (val1 == val2) return 1;
    } else {
        if (val1.u.ptr == val2.u.ptr and val1.tag == val2.tag) return 1;
    }

    // Handle primitives
    if (qjs.JS_IsNull(val1) and qjs.JS_IsNull(val2)) return 1;
    if (qjs.JS_IsUndefined(val1) and qjs.JS_IsUndefined(val2)) return 1;
    if (qjs.JS_IsNull(val1) or qjs.JS_IsNull(val2)) return 0;
    if (qjs.JS_IsUndefined(val1) or qjs.JS_IsUndefined(val2)) return 0;

    // Boolean
    if (qjs.JS_IsBool(val1) and qjs.JS_IsBool(val2)) {
        const b1 = qjs.JS_ToBool(ctx, val1);
        const b2 = qjs.JS_ToBool(ctx, val2);
        return if (b1 == b2) 1 else 0;
    }

    // Number
    if (qjs.JS_IsNumber(val1) and qjs.JS_IsNumber(val2)) {
        var n1: f64 = 0;
        var n2: f64 = 0;
        _ = qjs.JS_ToFloat64(ctx, &n1, val1);
        _ = qjs.JS_ToFloat64(ctx, &n2, val2);
        // Handle NaN (NaN === NaN should be true in deep equal)
        if (std.math.isNan(n1) and std.math.isNan(n2)) return 1;
        // Handle -0 vs 0
        if (n1 == 0 and n2 == 0) {
            const neg1 = std.math.signbit(n1);
            const neg2 = std.math.signbit(n2);
            return if (neg1 == neg2) 1 else 0;
        }
        return if (n1 == n2) 1 else 0;
    }

    // String
    if (qjs.JS_IsString(val1) and qjs.JS_IsString(val2)) {
        var len1: usize = undefined;
        var len2: usize = undefined;
        const str1 = qjs.JS_ToCStringLen(ctx, &len1, val1);
        const str2 = qjs.JS_ToCStringLen(ctx, &len2, val2);
        defer {
            if (str1 != null) qjs.JS_FreeCString(ctx, str1);
            if (str2 != null) qjs.JS_FreeCString(ctx, str2);
        }
        if (str1 == null or str2 == null) return 0;
        if (len1 != len2) return 0;
        return if (std.mem.eql(u8, str1[0..len1], str2[0..len2])) 1 else 0;
    }

    // Symbol - compare by reference
    if (qjs.JS_IsSymbol(val1) and qjs.JS_IsSymbol(val2)) {
        if (comptime is_wasm) {
            return if (val1 == val2) 1 else 0;
        } else {
            return if (val1.u.ptr == val2.u.ptr) 1 else 0;
        }
    }

    // BigInt
    if (qjs.JS_IsBigInt(val1) and qjs.JS_IsBigInt(val2)) {
        // Compare via toString
        const str1 = qjs.JS_ToString(ctx, val1);
        const str2 = qjs.JS_ToString(ctx, val2);
        defer {
            qjs.JS_FreeValue(ctx, str1);
            qjs.JS_FreeValue(ctx, str2);
        }
        var len1: usize = undefined;
        var len2: usize = undefined;
        const cstr1 = qjs.JS_ToCStringLen(ctx, &len1, str1);
        const cstr2 = qjs.JS_ToCStringLen(ctx, &len2, str2);
        defer {
            if (cstr1 != null) qjs.JS_FreeCString(ctx, cstr1);
            if (cstr2 != null) qjs.JS_FreeCString(ctx, cstr2);
        }
        if (cstr1 == null or cstr2 == null) return 0;
        if (len1 != len2) return 0;
        return if (std.mem.eql(u8, cstr1[0..len1], cstr2[0..len2])) 1 else 0;
    }

    // Type mismatch for primitives
    if (!qjs.JS_IsObject(val1) or !qjs.JS_IsObject(val2)) return 0;

    // Both are objects
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Check Date
    const date_ctor = qjs.JS_GetPropertyStr(ctx, global, "Date");
    defer qjs.JS_FreeValue(ctx, date_ctor);
    const is_date1 = qjs.JS_IsInstanceOf(ctx, val1, date_ctor) == 1;
    const is_date2 = qjs.JS_IsInstanceOf(ctx, val2, date_ctor) == 1;
    if (is_date1 != is_date2) return 0;
    if (is_date1) {
        // Compare via getTime()
        const get_time = qjs.JS_GetPropertyStr(ctx, val1, "getTime");
        defer qjs.JS_FreeValue(ctx, get_time);
        const time1 = qjs.JS_Call(ctx, get_time, val1, 0, null);
        const time2 = qjs.JS_Call(ctx, get_time, val2, 0, null);
        defer qjs.JS_FreeValue(ctx, time1);
        defer qjs.JS_FreeValue(ctx, time2);
        var t1: f64 = 0;
        var t2: f64 = 0;
        _ = qjs.JS_ToFloat64(ctx, &t1, time1);
        _ = qjs.JS_ToFloat64(ctx, &t2, time2);
        // Handle invalid dates (NaN)
        if (std.math.isNan(t1) and std.math.isNan(t2)) return 1;
        return if (t1 == t2) 1 else 0;
    }

    // Check RegExp
    const regexp_ctor = qjs.JS_GetPropertyStr(ctx, global, "RegExp");
    defer qjs.JS_FreeValue(ctx, regexp_ctor);
    const is_regexp1 = qjs.JS_IsInstanceOf(ctx, val1, regexp_ctor) == 1;
    const is_regexp2 = qjs.JS_IsInstanceOf(ctx, val2, regexp_ctor) == 1;
    if (is_regexp1 != is_regexp2) return 0;
    if (is_regexp1) {
        // Compare source and flags
        const source1 = qjs.JS_GetPropertyStr(ctx, val1, "source");
        const source2 = qjs.JS_GetPropertyStr(ctx, val2, "source");
        defer qjs.JS_FreeValue(ctx, source1);
        defer qjs.JS_FreeValue(ctx, source2);
        const flags1 = qjs.JS_GetPropertyStr(ctx, val1, "flags");
        const flags2 = qjs.JS_GetPropertyStr(ctx, val2, "flags");
        defer qjs.JS_FreeValue(ctx, flags1);
        defer qjs.JS_FreeValue(ctx, flags2);

        if (deepEqual(ctx, source1, source2, depth + 1) != 1) return 0;
        return deepEqual(ctx, flags1, flags2, depth + 1);
    }

    // Check Error
    const error_ctor = qjs.JS_GetPropertyStr(ctx, global, "Error");
    defer qjs.JS_FreeValue(ctx, error_ctor);
    const is_error1 = qjs.JS_IsInstanceOf(ctx, val1, error_ctor) == 1;
    const is_error2 = qjs.JS_IsInstanceOf(ctx, val2, error_ctor) == 1;
    if (is_error1 != is_error2) return 0;
    if (is_error1) {
        // Compare message and name
        const msg1 = qjs.JS_GetPropertyStr(ctx, val1, "message");
        const msg2 = qjs.JS_GetPropertyStr(ctx, val2, "message");
        defer qjs.JS_FreeValue(ctx, msg1);
        defer qjs.JS_FreeValue(ctx, msg2);
        if (deepEqual(ctx, msg1, msg2, depth + 1) != 1) return 0;

        const name1 = qjs.JS_GetPropertyStr(ctx, val1, "name");
        const name2 = qjs.JS_GetPropertyStr(ctx, val2, "name");
        defer qjs.JS_FreeValue(ctx, name1);
        defer qjs.JS_FreeValue(ctx, name2);
        return deepEqual(ctx, name1, name2, depth + 1);
    }

    // Check Array
    const is_array1 = qjs.JS_IsArray(val1);
    const is_array2 = qjs.JS_IsArray(val2);
    if (is_array1 != is_array2) return 0;
    if (is_array1) {
        // Compare lengths
        const len1_val = qjs.JS_GetPropertyStr(ctx, val1, "length");
        const len2_val = qjs.JS_GetPropertyStr(ctx, val2, "length");
        defer qjs.JS_FreeValue(ctx, len1_val);
        defer qjs.JS_FreeValue(ctx, len2_val);
        var len1: i32 = 0;
        var len2: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &len1, len1_val);
        _ = qjs.JS_ToInt32(ctx, &len2, len2_val);
        if (len1 != len2) return 0;

        // Compare each element
        var i: u32 = 0;
        while (i < @as(u32, @intCast(len1))) : (i += 1) {
            const elem1 = qjs.JS_GetPropertyUint32(ctx, val1, i);
            const elem2 = qjs.JS_GetPropertyUint32(ctx, val2, i);
            defer qjs.JS_FreeValue(ctx, elem1);
            defer qjs.JS_FreeValue(ctx, elem2);
            if (deepEqual(ctx, elem1, elem2, depth + 1) != 1) return 0;
        }
        return 1;
    }

    // Check Map
    const map_ctor = qjs.JS_GetPropertyStr(ctx, global, "Map");
    defer qjs.JS_FreeValue(ctx, map_ctor);
    const is_map1 = qjs.JS_IsInstanceOf(ctx, val1, map_ctor) == 1;
    const is_map2 = qjs.JS_IsInstanceOf(ctx, val2, map_ctor) == 1;
    if (is_map1 != is_map2) return 0;
    if (is_map1) {
        // Compare sizes first
        const size1 = qjs.JS_GetPropertyStr(ctx, val1, "size");
        const size2 = qjs.JS_GetPropertyStr(ctx, val2, "size");
        defer qjs.JS_FreeValue(ctx, size1);
        defer qjs.JS_FreeValue(ctx, size2);
        var s1: i32 = 0;
        var s2: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &s1, size1);
        _ = qjs.JS_ToInt32(ctx, &s2, size2);
        if (s1 != s2) return 0;
        if (s1 == 0) return 1; // Both empty

        // Get entries and compare - call entries() to get iterator, then Array.from() to get array
        const array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Array");
        defer qjs.JS_FreeValue(ctx, array_ctor);
        const array_from = qjs.JS_GetPropertyStr(ctx, array_ctor, "from");
        defer qjs.JS_FreeValue(ctx, array_from);

        // Get entries from map1
        const entries1_fn = qjs.JS_GetPropertyStr(ctx, val1, "entries");
        defer qjs.JS_FreeValue(ctx, entries1_fn);
        const entries1_iter = qjs.JS_Call(ctx, entries1_fn, val1, 0, null);
        defer qjs.JS_FreeValue(ctx, entries1_iter);
        var entries1_args = [1]qjs.JSValue{entries1_iter};
        const entries1_arr = qjs.JS_Call(ctx, array_from, array_ctor, 1, &entries1_args);
        defer qjs.JS_FreeValue(ctx, entries1_arr);

        // For each entry in map1, check if map2 has the same key with the same value
        const get2_fn = qjs.JS_GetPropertyStr(ctx, val2, "get");
        defer qjs.JS_FreeValue(ctx, get2_fn);
        const has2_fn = qjs.JS_GetPropertyStr(ctx, val2, "has");
        defer qjs.JS_FreeValue(ctx, has2_fn);

        var i: u32 = 0;
        while (i < @as(u32, @intCast(s1))) : (i += 1) {
            const entry = qjs.JS_GetPropertyUint32(ctx, entries1_arr, i);
            defer qjs.JS_FreeValue(ctx, entry);

            // entry is [key, value]
            const key = qjs.JS_GetPropertyUint32(ctx, entry, 0);
            const val1_entry = qjs.JS_GetPropertyUint32(ctx, entry, 1);
            defer qjs.JS_FreeValue(ctx, key);
            defer qjs.JS_FreeValue(ctx, val1_entry);

            // Check if map2 has the key
            var has_args = [1]qjs.JSValue{key};
            const has_result = qjs.JS_Call(ctx, has2_fn, val2, 1, &has_args);
            defer qjs.JS_FreeValue(ctx, has_result);
            if (qjs.JS_ToBool(ctx, has_result) != 1) return 0;

            // Get value from map2 and compare
            var get_args = [1]qjs.JSValue{key};
            const val2_entry = qjs.JS_Call(ctx, get2_fn, val2, 1, &get_args);
            defer qjs.JS_FreeValue(ctx, val2_entry);

            if (deepEqual(ctx, val1_entry, val2_entry, depth + 1) != 1) return 0;
        }
        return 1;
    }

    // Check Set
    const set_ctor = qjs.JS_GetPropertyStr(ctx, global, "Set");
    defer qjs.JS_FreeValue(ctx, set_ctor);
    const is_set1 = qjs.JS_IsInstanceOf(ctx, val1, set_ctor) == 1;
    const is_set2 = qjs.JS_IsInstanceOf(ctx, val2, set_ctor) == 1;
    if (is_set1 != is_set2) return 0;
    if (is_set1) {
        // Compare sizes first
        const size1 = qjs.JS_GetPropertyStr(ctx, val1, "size");
        const size2 = qjs.JS_GetPropertyStr(ctx, val2, "size");
        defer qjs.JS_FreeValue(ctx, size1);
        defer qjs.JS_FreeValue(ctx, size2);
        var s1: i32 = 0;
        var s2: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &s1, size1);
        _ = qjs.JS_ToInt32(ctx, &s2, size2);
        if (s1 != s2) return 0;
        if (s1 == 0) return 1; // Both empty

        // Get values and compare - call values() to get iterator, then Array.from() to get array
        const array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Array");
        defer qjs.JS_FreeValue(ctx, array_ctor);
        const array_from = qjs.JS_GetPropertyStr(ctx, array_ctor, "from");
        defer qjs.JS_FreeValue(ctx, array_from);

        // Get values from set1
        const values1_fn = qjs.JS_GetPropertyStr(ctx, val1, "values");
        defer qjs.JS_FreeValue(ctx, values1_fn);
        const values1_iter = qjs.JS_Call(ctx, values1_fn, val1, 0, null);
        defer qjs.JS_FreeValue(ctx, values1_iter);
        var values1_args = [1]qjs.JSValue{values1_iter};
        const values1_arr = qjs.JS_Call(ctx, array_from, array_ctor, 1, &values1_args);
        defer qjs.JS_FreeValue(ctx, values1_arr);

        // For each value in set1, check if set2 has it
        const has2_fn = qjs.JS_GetPropertyStr(ctx, val2, "has");
        defer qjs.JS_FreeValue(ctx, has2_fn);

        var i: u32 = 0;
        while (i < @as(u32, @intCast(s1))) : (i += 1) {
            const set1_val = qjs.JS_GetPropertyUint32(ctx, values1_arr, i);
            defer qjs.JS_FreeValue(ctx, set1_val);

            // Check if set2 has this value
            // For primitives, has() works directly
            // For objects, we need deep comparison
            var has_args = [1]qjs.JSValue{set1_val};
            const has_result = qjs.JS_Call(ctx, has2_fn, val2, 1, &has_args);
            defer qjs.JS_FreeValue(ctx, has_result);

            if (qjs.JS_ToBool(ctx, has_result) != 1) {
                // has() didn't find it - for primitive values, this means not equal
                // For object values, Set.has() uses reference equality
                // So we need to iterate set2 and do deep comparison
                if (qjs.JS_IsObject(set1_val)) {
                    // Get values from set2
                    const values2_fn = qjs.JS_GetPropertyStr(ctx, val2, "values");
                    defer qjs.JS_FreeValue(ctx, values2_fn);
                    const values2_iter = qjs.JS_Call(ctx, values2_fn, val2, 0, null);
                    defer qjs.JS_FreeValue(ctx, values2_iter);
                    var values2_args = [1]qjs.JSValue{values2_iter};
                    const values2_arr = qjs.JS_Call(ctx, array_from, array_ctor, 1, &values2_args);
                    defer qjs.JS_FreeValue(ctx, values2_arr);

                    var found = false;
                    var j: u32 = 0;
                    while (j < @as(u32, @intCast(s2))) : (j += 1) {
                        const set2_val = qjs.JS_GetPropertyUint32(ctx, values2_arr, j);
                        defer qjs.JS_FreeValue(ctx, set2_val);
                        if (deepEqual(ctx, set1_val, set2_val, depth + 1) == 1) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) return 0;
                } else {
                    // Primitive value not found
                    return 0;
                }
            }
        }
        return 1;
    }

    // Plain object - compare own enumerable properties
    const object_ctor = qjs.JS_GetPropertyStr(ctx, global, "Object");
    defer qjs.JS_FreeValue(ctx, object_ctor);

    const keys_func = qjs.JS_GetPropertyStr(ctx, object_ctor, "keys");
    defer qjs.JS_FreeValue(ctx, keys_func);

    var args1 = [1]qjs.JSValue{val1};
    const keys1 = qjs.JS_Call(ctx, keys_func, object_ctor, 1, &args1);
    defer qjs.JS_FreeValue(ctx, keys1);

    var args2 = [1]qjs.JSValue{val2};
    const keys2 = qjs.JS_Call(ctx, keys_func, object_ctor, 1, &args2);
    defer qjs.JS_FreeValue(ctx, keys2);

    // Compare key counts
    const len1_val = qjs.JS_GetPropertyStr(ctx, keys1, "length");
    const len2_val = qjs.JS_GetPropertyStr(ctx, keys2, "length");
    defer qjs.JS_FreeValue(ctx, len1_val);
    defer qjs.JS_FreeValue(ctx, len2_val);
    var len1: i32 = 0;
    var len2: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &len1, len1_val);
    _ = qjs.JS_ToInt32(ctx, &len2, len2_val);
    if (len1 != len2) return 0;

    // Compare each property
    var i: u32 = 0;
    while (i < @as(u32, @intCast(len1))) : (i += 1) {
        const key = qjs.JS_GetPropertyUint32(ctx, keys1, i);
        defer qjs.JS_FreeValue(ctx, key);

        const key_str = qjs.JS_ToCString(ctx, key);
        if (key_str == null) return 0;
        defer qjs.JS_FreeCString(ctx, key_str);

        const prop1 = qjs.JS_GetPropertyStr(ctx, val1, key_str);
        const prop2 = qjs.JS_GetPropertyStr(ctx, val2, key_str);
        defer qjs.JS_FreeValue(ctx, prop1);
        defer qjs.JS_FreeValue(ctx, prop2);

        if (deepEqual(ctx, prop1, prop2, depth + 1) != 1) return 0;
    }

    return 1;
}

/// util.isDeepStrictEqual(val1, val2) - Deep strict comparison
fn utilIsDeepStrictEqual(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return quickjs.jsFalse();
    const result = deepEqual(ctx, argv[0], argv[1], 0);
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// util.parseArgs(config) - Parse command line arguments
/// config = { args: [], options: {}, strict: true, allowPositionals: false }
/// Returns { values: {}, positionals: [] }
fn utilParseArgs(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    // Use JS implementation for complex parsing logic
    const parse_args_code =
        \\(function(config) {
        \\    config = config || {};
        \\    var args = config.args || (typeof process !== 'undefined' ? process.argv.slice(2) : []);
        \\    var options = config.options || {};
        \\    var strict = config.strict !== false;
        \\    var allowPositionals = config.allowPositionals === true;
        \\    var allowNegative = config.allowNegative === true;
        \\
        \\    var values = {};
        \\    var positionals = [];
        \\    var tokens = [];
        \\
        \\    // Initialize default values
        \\    for (var name in options) {
        \\        if (options[name].default !== undefined) {
        \\            values[name] = options[name].default;
        \\        } else if (options[name].type === 'boolean') {
        \\            values[name] = false;
        \\        }
        \\    }
        \\
        \\    var i = 0;
        \\    while (i < args.length) {
        \\        var arg = args[i];
        \\
        \\        // Handle -- (end of options)
        \\        if (arg === '--') {
        \\            tokens.push({ kind: 'option-terminator', index: i });
        \\            i++;
        \\            while (i < args.length) {
        \\                if (allowPositionals) {
        \\                    positionals.push(args[i]);
        \\                    tokens.push({ kind: 'positional', index: i, value: args[i] });
        \\                } else if (strict) {
        \\                    throw new Error('Unexpected positional: ' + args[i]);
        \\                }
        \\                i++;
        \\            }
        \\            break;
        \\        }
        \\
        \\        // Long option (--name or --name=value)
        \\        if (arg.slice(0, 2) === '--') {
        \\            var eqIdx = arg.indexOf('=');
        \\            var optName, optValue;
        \\            if (eqIdx !== -1) {
        \\                optName = arg.slice(2, eqIdx);
        \\                optValue = arg.slice(eqIdx + 1);
        \\            } else {
        \\                optName = arg.slice(2);
        \\                optValue = undefined;
        \\            }
        \\
        \\            // Handle --no-* negation
        \\            var negated = false;
        \\            if (optName.slice(0, 3) === 'no-' && allowNegative) {
        \\                var baseName = optName.slice(3);
        \\                if (options[baseName] && options[baseName].type === 'boolean') {
        \\                    optName = baseName;
        \\                    negated = true;
        \\                }
        \\            }
        \\
        \\            var opt = options[optName];
        \\            if (!opt && strict) {
        \\                throw new Error('Unknown option: --' + optName);
        \\            }
        \\
        \\            if (opt) {
        \\                if (opt.type === 'boolean') {
        \\                    values[optName] = !negated;
        \\                    tokens.push({ kind: 'option', name: optName, value: !negated, index: i, rawName: arg.split('=')[0] });
        \\                } else {
        \\                    if (optValue === undefined) {
        \\                        i++;
        \\                        if (i >= args.length) {
        \\                            throw new Error('Option --' + optName + ' requires a value');
        \\                        }
        \\                        optValue = args[i];
        \\                    }
        \\                    if (opt.multiple) {
        \\                        values[optName] = values[optName] || [];
        \\                        values[optName].push(optValue);
        \\                    } else {
        \\                        values[optName] = optValue;
        \\                    }
        \\                    tokens.push({ kind: 'option', name: optName, value: optValue, index: i, rawName: '--' + optName });
        \\                }
        \\            }
        \\            i++;
        \\            continue;
        \\        }
        \\
        \\        // Short option (-x or -xyz bundled)
        \\        if (arg.length > 1 && arg[0] === '-' && arg[1] !== '-') {
        \\            var shorts = arg.slice(1);
        \\            for (var j = 0; j < shorts.length; j++) {
        \\                var shortName = shorts[j];
        \\                var foundOpt = null;
        \\                var foundName = null;
        \\                for (var name in options) {
        \\                    if (options[name].short === shortName) {
        \\                        foundOpt = options[name];
        \\                        foundName = name;
        \\                        break;
        \\                    }
        \\                }
        \\                if (!foundOpt && strict) {
        \\                    throw new Error('Unknown option: -' + shortName);
        \\                }
        \\                if (foundOpt) {
        \\                    if (foundOpt.type === 'boolean') {
        \\                        values[foundName] = true;
        \\                        tokens.push({ kind: 'option', name: foundName, value: true, index: i, rawName: '-' + shortName });
        \\                    } else {
        \\                        var shortValue;
        \\                        if (j < shorts.length - 1) {
        \\                            shortValue = shorts.slice(j + 1);
        \\                            j = shorts.length;
        \\                        } else {
        \\                            i++;
        \\                            if (i >= args.length) {
        \\                                throw new Error('Option -' + shortName + ' requires a value');
        \\                            }
        \\                            shortValue = args[i];
        \\                        }
        \\                        if (foundOpt.multiple) {
        \\                            values[foundName] = values[foundName] || [];
        \\                            values[foundName].push(shortValue);
        \\                        } else {
        \\                            values[foundName] = shortValue;
        \\                        }
        \\                        tokens.push({ kind: 'option', name: foundName, value: shortValue, index: i, rawName: '-' + shortName });
        \\                    }
        \\                }
        \\            }
        \\            i++;
        \\            continue;
        \\        }
        \\
        \\        // Positional argument
        \\        if (allowPositionals) {
        \\            positionals.push(arg);
        \\            tokens.push({ kind: 'positional', index: i, value: arg });
        \\        } else if (strict) {
        \\            throw new Error('Unexpected positional: ' + arg);
        \\        }
        \\        i++;
        \\    }
        \\
        \\    var result = { values: values, positionals: positionals };
        \\    if (config.tokens) result.tokens = tokens;
        \\    return result;
        \\})
    ;

    // Evaluate the parseArgs implementation
    const factory = qjs.JS_Eval(ctx, parse_args_code.ptr, parse_args_code.len, "<parseArgs>", qjs.JS_EVAL_TYPE_GLOBAL);
    if (qjs.JS_IsException(factory)) {
        return factory;
    }
    defer qjs.JS_FreeValue(ctx, factory);

    // Call with config argument
    if (argc >= 1) {
        var call_args = [1]qjs.JSValue{argv[0]};
        return qjs.JS_Call(ctx, factory, quickjs.jsUndefined(), 1, &call_args);
    } else {
        return qjs.JS_Call(ctx, factory, quickjs.jsUndefined(), 0, null);
    }
}

/// util.stripVTControlCharacters(str) - Remove ANSI escape codes from string
fn utilStripVTControlCharacters(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    var len: usize = undefined;
    const str = qjs.JS_ToCStringLen(ctx, &len, argv[0]);
    if (str == null) return qjs.JS_NewString(ctx, "");
    defer qjs.JS_FreeCString(ctx, str);

    const data: []const u8 = @ptrCast(str[0..len]);

    // Output buffer - escapes are removed so output <= input
    var buffer: [8192]u8 = undefined;
    var out_idx: usize = 0;
    var i: usize = 0;

    while (i < len and out_idx < buffer.len) {
        // Check for ESC (0x1B) followed by [ or ( or other CSI
        if (data[i] == 0x1B and i + 1 < len) {
            if (data[i + 1] == '[') {
                // CSI sequence: ESC [ ... final_byte (0x40-0x7E)
                i += 2;
                while (i < len and (data[i] < 0x40 or data[i] > 0x7E)) {
                    i += 1;
                }
                if (i < len) i += 1; // Skip final byte
                continue;
            } else if (data[i + 1] == ']') {
                // OSC sequence: ESC ] ... BEL or ESC \
                i += 2;
                while (i < len) {
                    if (data[i] == 0x07) { // BEL
                        i += 1;
                        break;
                    }
                    if (data[i] == 0x1B and i + 1 < len and data[i + 1] == '\\') {
                        i += 2;
                        break;
                    }
                    i += 1;
                }
                continue;
            } else if (data[i + 1] >= 0x40 and data[i + 1] <= 0x5F) {
                // Two-byte escape sequence
                i += 2;
                continue;
            }
        }

        // Regular character
        buffer[out_idx] = data[i];
        out_idx += 1;
        i += 1;
    }

    return qjs.JS_NewStringLen(ctx, &buffer, @intCast(out_idx));
}

/// util.toUSVString(str) - Convert string to well-formed UTF-16
fn utilToUSVString(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    // For well-formed UTF-8 input, just return the string as-is
    // QuickJS strings are internally UTF-8, and malformed surrogates are rare
    var len: usize = undefined;
    const str = qjs.JS_ToCStringLen(ctx, &len, argv[0]);
    if (str == null) return qjs.JS_NewString(ctx, "");
    defer qjs.JS_FreeCString(ctx, str);

    // Check for surrogate pairs and replace lone surrogates with U+FFFD
    const data: []const u8 = @ptrCast(str[0..len]);
    var buffer: [8192]u8 = undefined;
    var out_idx: usize = 0;
    var i: usize = 0;

    while (i < len and out_idx + 4 < buffer.len) {
        const byte = data[i];
        if (byte < 0x80) {
            // ASCII
            buffer[out_idx] = byte;
            out_idx += 1;
            i += 1;
        } else if (byte < 0xE0) {
            // 2-byte UTF-8
            if (i + 1 < len) {
                buffer[out_idx] = byte;
                buffer[out_idx + 1] = data[i + 1];
                out_idx += 2;
            }
            i += 2;
        } else if (byte < 0xF0) {
            // 3-byte UTF-8 - check for surrogate
            if (i + 2 < len) {
                // Decode to check if surrogate
                const cp: u32 = (@as(u32, byte & 0x0F) << 12) |
                    (@as(u32, data[i + 1] & 0x3F) << 6) |
                    @as(u32, data[i + 2] & 0x3F);

                if (cp >= 0xD800 and cp <= 0xDFFF) {
                    // Lone surrogate - replace with U+FFFD (EF BF BD in UTF-8)
                    buffer[out_idx] = 0xEF;
                    buffer[out_idx + 1] = 0xBF;
                    buffer[out_idx + 2] = 0xBD;
                    out_idx += 3;
                } else {
                    buffer[out_idx] = byte;
                    buffer[out_idx + 1] = data[i + 1];
                    buffer[out_idx + 2] = data[i + 2];
                    out_idx += 3;
                }
            }
            i += 3;
        } else {
            // 4-byte UTF-8
            if (i + 3 < len) {
                buffer[out_idx] = byte;
                buffer[out_idx + 1] = data[i + 1];
                buffer[out_idx + 2] = data[i + 2];
                buffer[out_idx + 3] = data[i + 3];
                out_idx += 4;
            }
            i += 4;
        }
    }

    return qjs.JS_NewStringLen(ctx, &buffer, @intCast(out_idx));
}

/// Public wrapper for isDeepStrictEqual - used by assert module
pub fn isDeepStrictEqualInternal(ctx: ?*qjs.JSContext, val1: qjs.JSValue, val2: qjs.JSValue) bool {
    return deepEqual(ctx, val1, val2, 0) == 1;
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
        .{ "inspect", utilInspect, 2 },
        .{ "deprecate", utilDeprecate, 2 },
        .{ "inherits", utilInherits, 2 },
        .{ "isArray", utilIsArray, 1 },
        .{ "isBuffer", utilIsBuffer, 1 },
        .{ "promisify", utilPromisify, 1 },
        .{ "callbackify", utilCallbackify, 1 },
        .{ "styleText", utilStyleText, 2 },
        .{ "debuglog", utilDebuglog, 1 },
        .{ "getSystemErrorName", utilGetSystemErrorName, 1 },
        .{ "getSystemErrorMap", utilGetSystemErrorMap, 0 },
        .{ "isDeepStrictEqual", utilIsDeepStrictEqual, 2 },
        .{ "parseArgs", utilParseArgs, 1 },
        .{ "stripVTControlCharacters", utilStripVTControlCharacters, 1 },
        .{ "toUSVString", utilToUSVString, 1 },
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
                .{ "inspect", utilInspect, 2 },
                .{ "deprecate", utilDeprecate, 2 },
                .{ "inherits", utilInherits, 2 },
                .{ "isArray", utilIsArray, 1 },
                .{ "isBuffer", utilIsBuffer, 1 },
                .{ "promisify", utilPromisify, 1 },
                .{ "callbackify", utilCallbackify, 1 },
                .{ "styleText", utilStyleText, 2 },
                .{ "debuglog", utilDebuglog, 1 },
                .{ "getSystemErrorName", utilGetSystemErrorName, 1 },
                .{ "getSystemErrorMap", utilGetSystemErrorMap, 0 },
                .{ "isDeepStrictEqual", utilIsDeepStrictEqual, 2 },
                .{ "parseArgs", utilParseArgs, 1 },
                .{ "stripVTControlCharacters", utilStripVTControlCharacters, 1 },
                .{ "toUSVString", utilToUSVString, 1 },
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
