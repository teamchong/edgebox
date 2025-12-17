/// Native path module - QuickJS C functions
/// Registered ONCE at WASM init via inline for loop
/// Zero runtime overhead, allocation-free implementations
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// Stack buffers for path operations - no heap allocation needed
var path_buffer: [4096]u8 = undefined;

/// path.join(...parts) - Join all arguments with '/'
fn pathJoin(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    var pos: usize = 0;

    for (0..@intCast(argc)) |i| {
        const str = qjs.JS_ToCString(ctx, argv[i]);
        if (str == null) continue;
        defer qjs.JS_FreeCString(ctx, str);

        const part = std.mem.span(str);
        if (part.len == 0) continue;

        // Add separator if needed
        if (pos > 0 and path_buffer[pos - 1] != '/') {
            path_buffer[pos] = '/';
            pos += 1;
        }

        // Skip leading slash if not first part
        const start: usize = if (pos > 0 and part.len > 0 and part[0] == '/') 1 else 0;
        const to_copy = part[start..];

        if (pos + to_copy.len >= path_buffer.len) break;
        @memcpy(path_buffer[pos..][0..to_copy.len], to_copy);
        pos += to_copy.len;
    }

    // Remove trailing slash unless it's root
    if (pos > 1 and path_buffer[pos - 1] == '/') {
        pos -= 1;
    }

    if (pos == 0) {
        return qjs.JS_NewString(ctx, ".");
    }

    return qjs.JS_NewStringLen(ctx, &path_buffer, @intCast(pos));
}

/// path.dirname(p) - Get directory name
fn pathDirname(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, ".");

    const str = qjs.JS_ToCString(ctx, argv[0]);
    if (str == null) return qjs.JS_NewString(ctx, ".");
    defer qjs.JS_FreeCString(ctx, str);

    const path = std.mem.span(str);
    if (path.len == 0) return qjs.JS_NewString(ctx, ".");
    if (std.mem.eql(u8, path, "/")) return qjs.JS_NewString(ctx, "/");

    if (std.mem.lastIndexOfScalar(u8, path, '/')) |idx| {
        if (idx == 0) return qjs.JS_NewString(ctx, "/");
        return qjs.JS_NewStringLen(ctx, path.ptr, @intCast(idx));
    }

    return qjs.JS_NewString(ctx, ".");
}

/// path.basename(p, ext?) - Get base name
fn pathBasename(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    const str = qjs.JS_ToCString(ctx, argv[0]);
    if (str == null) return qjs.JS_NewString(ctx, "");
    defer qjs.JS_FreeCString(ctx, str);

    const path = std.mem.span(str);
    if (path.len == 0) return qjs.JS_NewString(ctx, "");

    const base_start = if (std.mem.lastIndexOfScalar(u8, path, '/')) |idx| idx + 1 else 0;
    const base = path[base_start..];

    // Remove extension if provided
    if (argc > 1) {
        const ext_str = qjs.JS_ToCString(ctx, argv[1]);
        if (ext_str != null) {
            defer qjs.JS_FreeCString(ctx, ext_str);
            const ext = std.mem.span(ext_str);
            if (ext.len > 0 and std.mem.endsWith(u8, base, ext)) {
                const len = base.len - ext.len;
                return qjs.JS_NewStringLen(ctx, base.ptr, @intCast(len));
            }
        }
    }

    return qjs.JS_NewStringLen(ctx, base.ptr, @intCast(base.len));
}

/// path.extname(p) - Get extension
fn pathExtname(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    const str = qjs.JS_ToCString(ctx, argv[0]);
    if (str == null) return qjs.JS_NewString(ctx, "");
    defer qjs.JS_FreeCString(ctx, str);

    const path = std.mem.span(str);
    if (path.len == 0) return qjs.JS_NewString(ctx, "");

    const base_start = if (std.mem.lastIndexOfScalar(u8, path, '/')) |idx| idx + 1 else 0;
    const base = path[base_start..];

    if (std.mem.lastIndexOfScalar(u8, base, '.')) |idx| {
        if (idx > 0) {
            const ext = base[idx..];
            return qjs.JS_NewStringLen(ctx, ext.ptr, @intCast(ext.len));
        }
    }

    return qjs.JS_NewString(ctx, "");
}

/// path.normalize(p) - Normalize path (resolve . and ..)
fn pathNormalize(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, ".");

    const str = qjs.JS_ToCString(ctx, argv[0]);
    if (str == null) return qjs.JS_NewString(ctx, ".");
    defer qjs.JS_FreeCString(ctx, str);

    const path = std.mem.span(str);
    if (path.len == 0) return qjs.JS_NewString(ctx, ".");

    const is_absolute = path[0] == '/';

    // Stack-based parts array
    var parts_buf: [128][]const u8 = undefined;
    var parts_len: usize = 0;

    var it = std.mem.splitScalar(u8, path, '/');
    while (it.next()) |part| {
        if (part.len == 0 or std.mem.eql(u8, part, ".")) continue;
        if (std.mem.eql(u8, part, "..")) {
            if (parts_len > 0 and !std.mem.eql(u8, parts_buf[parts_len - 1], "..")) {
                parts_len -= 1;
            } else if (!is_absolute and parts_len < parts_buf.len) {
                parts_buf[parts_len] = "..";
                parts_len += 1;
            }
        } else if (parts_len < parts_buf.len) {
            parts_buf[parts_len] = part;
            parts_len += 1;
        }
    }

    var pos: usize = 0;
    if (is_absolute) {
        path_buffer[0] = '/';
        pos = 1;
    }

    for (0..parts_len) |i| {
        const part = parts_buf[i];
        if (pos + part.len >= path_buffer.len) break;

        @memcpy(path_buffer[pos..][0..part.len], part);
        pos += part.len;

        if (i < parts_len - 1 and pos < path_buffer.len) {
            path_buffer[pos] = '/';
            pos += 1;
        }
    }

    if (pos == 0) {
        return qjs.JS_NewString(ctx, if (is_absolute) "/" else ".");
    }

    return qjs.JS_NewStringLen(ctx, &path_buffer, @intCast(pos));
}

/// Register all path functions to globalThis.path
/// Called ONCE at WASM initialization
pub fn register(ctx: *qjs.JSContext) void {
    const path_obj = qjs.JS_NewObject(ctx);

    // Properties
    _ = qjs.JS_SetPropertyStr(ctx, path_obj, "sep", qjs.JS_NewString(ctx, "/"));
    _ = qjs.JS_SetPropertyStr(ctx, path_obj, "delimiter", qjs.JS_NewString(ctx, ":"));

    // Register all functions at once - zero runtime cost
    inline for (.{
        .{ "join", pathJoin, -1 }, // -1 = variadic
        .{ "dirname", pathDirname, 1 },
        .{ "basename", pathBasename, 2 },
        .{ "extname", pathExtname, 1 },
        .{ "normalize", pathNormalize, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, path_obj, binding[0], func);
    }

    // Set as global.path
    const global = qjs.JS_GetGlobalObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, global, "path", qjs.JS_DupValue(ctx, path_obj));

    // Also add to _modules for require('path')
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "path", path_obj);
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, path_obj);
    }

    qjs.JS_FreeValue(ctx, global);
}
