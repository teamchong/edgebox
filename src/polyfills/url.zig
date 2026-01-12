/// Native URL module - QuickJS C functions
/// Implements basic URL parsing (uses browser URL API under the hood)
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

/// url.parse(urlString) - Parse URL into components
fn urlParse(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "url.parse requires urlString argument");

    const str = qjs.JS_ToCString(ctx, argv[0]);
    if (str == null) return quickjs.jsException();
    defer qjs.JS_FreeCString(ctx, str);

    // Use native URL constructor
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const url_ctor = qjs.JS_GetPropertyStr(ctx, global, "URL");
    defer qjs.JS_FreeValue(ctx, url_ctor);

    var url_args = [1]qjs.JSValue{argv[0]};
    const url_obj = qjs.JS_CallConstructor(ctx, url_ctor, 1, &url_args);
    if (qjs.JS_IsException(url_obj)) {
        // URL parse failed, return empty object
        qjs.JS_FreeValue(ctx, url_obj);
        return qjs.JS_NewObject(ctx);
    }
    defer qjs.JS_FreeValue(ctx, url_obj);

    // Extract components into legacy format
    const result = qjs.JS_NewObject(ctx);

    // href
    const href = qjs.JS_GetPropertyStr(ctx, url_obj, "href");
    _ = qjs.JS_SetPropertyStr(ctx, result, "href", href);

    // protocol
    const protocol = qjs.JS_GetPropertyStr(ctx, url_obj, "protocol");
    _ = qjs.JS_SetPropertyStr(ctx, result, "protocol", protocol);

    // host (hostname:port)
    const host = qjs.JS_GetPropertyStr(ctx, url_obj, "host");
    _ = qjs.JS_SetPropertyStr(ctx, result, "host", host);

    // hostname
    const hostname = qjs.JS_GetPropertyStr(ctx, url_obj, "hostname");
    _ = qjs.JS_SetPropertyStr(ctx, result, "hostname", hostname);

    // port
    const port = qjs.JS_GetPropertyStr(ctx, url_obj, "port");
    _ = qjs.JS_SetPropertyStr(ctx, result, "port", port);

    // pathname
    const pathname = qjs.JS_GetPropertyStr(ctx, url_obj, "pathname");
    _ = qjs.JS_SetPropertyStr(ctx, result, "pathname", pathname);

    // search (query with ?)
    const search = qjs.JS_GetPropertyStr(ctx, url_obj, "search");
    _ = qjs.JS_SetPropertyStr(ctx, result, "search", search);

    // query (search without ?)
    const search_str = qjs.JS_ToCString(ctx, search);
    if (search_str != null) {
        defer qjs.JS_FreeCString(ctx, search_str);
        const search_slice = std.mem.span(search_str);
        if (search_slice.len > 0 and search_slice[0] == '?') {
            const query_val = qjs.JS_NewString(ctx, search_str + 1);
            _ = qjs.JS_SetPropertyStr(ctx, result, "query", query_val);
        } else {
            _ = qjs.JS_SetPropertyStr(ctx, result, "query", qjs.JS_DupValue(ctx, search));
        }
    }

    // hash
    const hash = qjs.JS_GetPropertyStr(ctx, url_obj, "hash");
    _ = qjs.JS_SetPropertyStr(ctx, result, "hash", hash);

    return result;
}

/// url.format(urlObject) - Format URL object into string
fn urlFormat(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    // Check if it's a URL object with href
    const href = qjs.JS_GetPropertyStr(ctx, argv[0], "href");
    defer qjs.JS_FreeValue(ctx, href);

    if (!qjs.JS_IsUndefined(href)) {
        return qjs.JS_DupValue(ctx, href);
    }

    // Build URL string from components
    var buffer: [2048]u8 = undefined;
    var pos: usize = 0;

    // protocol
    const protocol = qjs.JS_GetPropertyStr(ctx, argv[0], "protocol");
    defer qjs.JS_FreeValue(ctx, protocol);
    if (!qjs.JS_IsUndefined(protocol)) {
        const protocol_str = qjs.JS_ToCString(ctx, protocol);
        if (protocol_str != null) {
            defer qjs.JS_FreeCString(ctx, protocol_str);
            const protocol_slice = std.mem.span(protocol_str);
            const len = @min(protocol_slice.len, buffer.len - pos);
            @memcpy(buffer[pos..][0..len], protocol_slice[0..len]);
            pos += len;

            // Add // if protocol doesn't end with :
            if (pos > 0 and buffer[pos-1] != ':') {
                if (pos + 1 < buffer.len) {
                    buffer[pos] = ':';
                    pos += 1;
                }
            }
            if (pos + 2 < buffer.len) {
                buffer[pos] = '/';
                buffer[pos+1] = '/';
                pos += 2;
            }
        }
    }

    // hostname
    const hostname = qjs.JS_GetPropertyStr(ctx, argv[0], "hostname");
    defer qjs.JS_FreeValue(ctx, hostname);
    if (!qjs.JS_IsUndefined(hostname)) {
        const hostname_str = qjs.JS_ToCString(ctx, hostname);
        if (hostname_str != null) {
            defer qjs.JS_FreeCString(ctx, hostname_str);
            const hostname_slice = std.mem.span(hostname_str);
            const len = @min(hostname_slice.len, buffer.len - pos);
            @memcpy(buffer[pos..][0..len], hostname_slice[0..len]);
            pos += len;
        }
    }

    // port
    const port = qjs.JS_GetPropertyStr(ctx, argv[0], "port");
    defer qjs.JS_FreeValue(ctx, port);
    if (!qjs.JS_IsUndefined(port)) {
        const port_str = qjs.JS_ToCString(ctx, port);
        if (port_str != null) {
            defer qjs.JS_FreeCString(ctx, port_str);
            const port_slice = std.mem.span(port_str);
            if (port_slice.len > 0 and pos < buffer.len) {
                buffer[pos] = ':';
                pos += 1;
                const len = @min(port_slice.len, buffer.len - pos);
                @memcpy(buffer[pos..][0..len], port_slice[0..len]);
                pos += len;
            }
        }
    }

    // pathname
    const pathname = qjs.JS_GetPropertyStr(ctx, argv[0], "pathname");
    defer qjs.JS_FreeValue(ctx, pathname);
    if (!qjs.JS_IsUndefined(pathname)) {
        const pathname_str = qjs.JS_ToCString(ctx, pathname);
        if (pathname_str != null) {
            defer qjs.JS_FreeCString(ctx, pathname_str);
            const pathname_slice = std.mem.span(pathname_str);
            const len = @min(pathname_slice.len, buffer.len - pos);
            @memcpy(buffer[pos..][0..len], pathname_slice[0..len]);
            pos += len;
        }
    }

    // search or query
    const search = qjs.JS_GetPropertyStr(ctx, argv[0], "search");
    defer qjs.JS_FreeValue(ctx, search);
    if (!qjs.JS_IsUndefined(search)) {
        const search_str = qjs.JS_ToCString(ctx, search);
        if (search_str != null) {
            defer qjs.JS_FreeCString(ctx, search_str);
            const search_slice = std.mem.span(search_str);
            const len = @min(search_slice.len, buffer.len - pos);
            @memcpy(buffer[pos..][0..len], search_slice[0..len]);
            pos += len;
        }
    }

    // hash
    const hash = qjs.JS_GetPropertyStr(ctx, argv[0], "hash");
    defer qjs.JS_FreeValue(ctx, hash);
    if (!qjs.JS_IsUndefined(hash)) {
        const hash_str = qjs.JS_ToCString(ctx, hash);
        if (hash_str != null) {
            defer qjs.JS_FreeCString(ctx, hash_str);
            const hash_slice = std.mem.span(hash_str);
            const len = @min(hash_slice.len, buffer.len - pos);
            @memcpy(buffer[pos..][0..len], hash_slice[0..len]);
            pos += len;
        }
    }

    return qjs.JS_NewStringLen(ctx, &buffer, @intCast(pos));
}

/// Register url module
pub fn register(ctx: *qjs.JSContext) void {
    const url_obj = qjs.JS_NewObject(ctx);

    // Register functions
    inline for (.{
        .{ "parse", urlParse, 1 },
        .{ "format", urlFormat, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, url_obj, binding[0], func);
    }

    // Set in _modules for require('url')
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "url", url_obj);
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, url_obj);
    }
}
