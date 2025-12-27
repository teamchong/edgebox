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

/// path.resolve(...paths) - Resolve paths to absolute path
fn pathResolve(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    var pos: usize = 0;

    // Start with current directory (simplified - in real Node.js this would be process.cwd())
    // For now, assume we're at root if no absolute path given
    var is_absolute = false;

    // Process arguments right to left until we find an absolute path
    var i: usize = @intCast(argc);
    while (i > 0) {
        i -= 1;

        const str = qjs.JS_ToCString(ctx, argv[i]);
        if (str == null) continue;
        defer qjs.JS_FreeCString(ctx, str);

        const part = std.mem.span(str);
        if (part.len == 0) continue;

        // If this is an absolute path, start fresh
        if (part[0] == '/') {
            is_absolute = true;
            pos = 0;
            if (pos + part.len >= path_buffer.len) break;
            @memcpy(path_buffer[pos..][0..part.len], part);
            pos += part.len;
            break; // Stop once we hit an absolute path
        }

        // Prepend this part (we're going right to left)
        if (pos > 0) {
            // Shift existing content right to make room
            const shift_len = part.len + 1; // +1 for separator
            if (pos + shift_len >= path_buffer.len) continue;

            // Move existing content
            var j: usize = pos;
            while (j > 0) {
                j -= 1;
                path_buffer[j + shift_len] = path_buffer[j];
            }

            // Insert new part at beginning
            @memcpy(path_buffer[0..part.len], part);
            path_buffer[part.len] = '/';
            pos += shift_len;
        } else {
            // First part
            if (part.len >= path_buffer.len) break;
            @memcpy(path_buffer[0..part.len], part);
            pos = part.len;
        }
    }

    // If not absolute, prepend current directory (just "/" for now)
    if (!is_absolute) {
        if (pos > 0) {
            // Shift and prepend "/"
            var j: usize = pos;
            while (j > 0) {
                j -= 1;
                path_buffer[j + 1] = path_buffer[j];
            }
            path_buffer[0] = '/';
            pos += 1;
        } else {
            path_buffer[0] = '/';
            pos = 1;
        }
    }

    // Now normalize the result (remove . and ..)
    const temp_str = qjs.JS_NewStringLen(ctx, &path_buffer, @intCast(pos));
    var temp_arg = temp_str;
    const result = pathNormalize(ctx, qjs.JS_UNDEFINED, 1, @ptrCast(&temp_arg));
    qjs.JS_FreeValue(ctx, temp_str);
    return result;
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

/// path.parse(p) - Parse path into {root, dir, base, ext, name}
fn pathParse(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const obj = qjs.JS_NewObject(ctx);

    if (argc < 1) {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "root", qjs.JS_NewString(ctx, ""));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "dir", qjs.JS_NewString(ctx, ""));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "base", qjs.JS_NewString(ctx, ""));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "ext", qjs.JS_NewString(ctx, ""));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "name", qjs.JS_NewString(ctx, ""));
        return obj;
    }

    const str = qjs.JS_ToCString(ctx, argv[0]);
    if (str == null) {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "root", qjs.JS_NewString(ctx, ""));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "dir", qjs.JS_NewString(ctx, ""));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "base", qjs.JS_NewString(ctx, ""));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "ext", qjs.JS_NewString(ctx, ""));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "name", qjs.JS_NewString(ctx, ""));
        return obj;
    }
    defer qjs.JS_FreeCString(ctx, str);

    const path = std.mem.span(str);

    // root: "/" if absolute, "" otherwise
    const is_absolute = path.len > 0 and path[0] == '/';
    _ = qjs.JS_SetPropertyStr(ctx, obj, "root", qjs.JS_NewString(ctx, if (is_absolute) "/" else ""));

    // Find last slash for dirname
    const dir_end = std.mem.lastIndexOfScalar(u8, path, '/') orelse 0;

    // dir: directory part (without trailing slash, unless root)
    if (dir_end == 0 and is_absolute) {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "dir", qjs.JS_NewString(ctx, "/"));
    } else if (dir_end > 0) {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "dir", qjs.JS_NewStringLen(ctx, path.ptr, @intCast(dir_end)));
    } else {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "dir", qjs.JS_NewString(ctx, ""));
    }

    // base: filename with extension
    const base_start = if (dir_end > 0 or (dir_end == 0 and is_absolute)) dir_end + 1 else 0;
    const base = path[base_start..];
    _ = qjs.JS_SetPropertyStr(ctx, obj, "base", qjs.JS_NewStringLen(ctx, base.ptr, @intCast(base.len)));

    // Find extension
    if (std.mem.lastIndexOfScalar(u8, base, '.')) |dot_idx| {
        if (dot_idx > 0) { // Don't treat leading dot as extension
            const ext = base[dot_idx..];
            const name = base[0..dot_idx];
            _ = qjs.JS_SetPropertyStr(ctx, obj, "ext", qjs.JS_NewStringLen(ctx, ext.ptr, @intCast(ext.len)));
            _ = qjs.JS_SetPropertyStr(ctx, obj, "name", qjs.JS_NewStringLen(ctx, name.ptr, @intCast(name.len)));
        } else {
            _ = qjs.JS_SetPropertyStr(ctx, obj, "ext", qjs.JS_NewString(ctx, ""));
            _ = qjs.JS_SetPropertyStr(ctx, obj, "name", qjs.JS_NewStringLen(ctx, base.ptr, @intCast(base.len)));
        }
    } else {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "ext", qjs.JS_NewString(ctx, ""));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "name", qjs.JS_NewStringLen(ctx, base.ptr, @intCast(base.len)));
    }

    return obj;
}

/// path.format(pathObject) - Format path object to string
fn pathFormat(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewString(ctx, "");

    const obj = argv[0];
    var pos: usize = 0;

    // Get dir property
    const dir_val = qjs.JS_GetPropertyStr(ctx, obj, "dir");
    defer qjs.JS_FreeValue(ctx, dir_val);

    if (!qjs.JS_IsUndefined(dir_val) and !qjs.JS_IsNull(dir_val)) {
        const dir_str = qjs.JS_ToCString(ctx, dir_val);
        if (dir_str != null) {
            defer qjs.JS_FreeCString(ctx, dir_str);
            const dir = std.mem.span(dir_str);
            if (dir.len > 0 and pos + dir.len < path_buffer.len) {
                @memcpy(path_buffer[pos..][0..dir.len], dir);
                pos += dir.len;
            }
        }
    } else {
        // Try root property
        const root_val = qjs.JS_GetPropertyStr(ctx, obj, "root");
        defer qjs.JS_FreeValue(ctx, root_val);

        if (!qjs.JS_IsUndefined(root_val) and !qjs.JS_IsNull(root_val)) {
            const root_str = qjs.JS_ToCString(ctx, root_val);
            if (root_str != null) {
                defer qjs.JS_FreeCString(ctx, root_str);
                const root = std.mem.span(root_str);
                if (root.len > 0 and pos + root.len < path_buffer.len) {
                    @memcpy(path_buffer[pos..][0..root.len], root);
                    pos += root.len;
                }
            }
        }
    }

    // Add separator if we have a dir and it doesn't end with /
    if (pos > 0 and path_buffer[pos - 1] != '/') {
        if (pos < path_buffer.len) {
            path_buffer[pos] = '/';
            pos += 1;
        }
    }

    // Get base property
    const base_val = qjs.JS_GetPropertyStr(ctx, obj, "base");
    defer qjs.JS_FreeValue(ctx, base_val);

    if (!qjs.JS_IsUndefined(base_val) and !qjs.JS_IsNull(base_val)) {
        const base_str = qjs.JS_ToCString(ctx, base_val);
        if (base_str != null) {
            defer qjs.JS_FreeCString(ctx, base_str);
            const base = std.mem.span(base_str);
            if (base.len > 0 and pos + base.len < path_buffer.len) {
                @memcpy(path_buffer[pos..][0..base.len], base);
                pos += base.len;
            }
        }
    } else {
        // Try name + ext
        const name_val = qjs.JS_GetPropertyStr(ctx, obj, "name");
        defer qjs.JS_FreeValue(ctx, name_val);

        if (!qjs.JS_IsUndefined(name_val) and !qjs.JS_IsNull(name_val)) {
            const name_str = qjs.JS_ToCString(ctx, name_val);
            if (name_str != null) {
                defer qjs.JS_FreeCString(ctx, name_str);
                const name = std.mem.span(name_str);
                if (name.len > 0 and pos + name.len < path_buffer.len) {
                    @memcpy(path_buffer[pos..][0..name.len], name);
                    pos += name.len;
                }
            }
        }

        const ext_val = qjs.JS_GetPropertyStr(ctx, obj, "ext");
        defer qjs.JS_FreeValue(ctx, ext_val);

        if (!qjs.JS_IsUndefined(ext_val) and !qjs.JS_IsNull(ext_val)) {
            const ext_str = qjs.JS_ToCString(ctx, ext_val);
            if (ext_str != null) {
                defer qjs.JS_FreeCString(ctx, ext_str);
                const ext = std.mem.span(ext_str);
                if (ext.len > 0 and pos + ext.len < path_buffer.len) {
                    @memcpy(path_buffer[pos..][0..ext.len], ext);
                    pos += ext.len;
                }
            }
        }
    }

    if (pos == 0) return qjs.JS_NewString(ctx, "");
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
        .{ "resolve", pathResolve, -1 }, // -1 = variadic
        .{ "dirname", pathDirname, 1 },
        .{ "basename", pathBasename, 2 },
        .{ "extname", pathExtname, 1 },
        .{ "normalize", pathNormalize, 1 },
        .{ "parse", pathParse, 1 },
        .{ "format", pathFormat, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, path_obj, binding[0], func);
    }

    // Set as global.path
    const global = qjs.JS_GetGlobalObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, global, "path", qjs.JS_DupValue(ctx, path_obj));

    // Also add to _modules for require('path')
    // _modules is created by require.zig which is registered first
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    _ = qjs.JS_SetPropertyStr(ctx, modules_val, "path", path_obj);
    qjs.JS_FreeValue(ctx, modules_val);

    qjs.JS_FreeValue(ctx, global);
}
