/// Native Bindings for WASM
/// All native function implementations for QuickJS integration
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const wasm_fetch = @import("../wasm_fetch.zig");
const wasi_tty = @import("../wasi_tty.zig");
const wasi_process = @import("../wasi_process.zig");
const wasm_zlib = @import("../wasm_zlib.zig");
const snapshot = @import("../snapshot.zig");
const build_options = @import("build_options");
const dispatch = @import("dispatch.zig");
const errors = @import("../errors.zig");
const simd_json = @import("../abi/simd_json.zig");
const bigint = @import("../abi/bigint.zig");
const websocket = @import("../abi/websocket.zig");

// Re-export dispatch constants for internal use
const USE_COMPONENT_MODEL_CRYPTO = dispatch.USE_COMPONENT_MODEL_CRYPTO;
const USE_COMPONENT_MODEL_FS = dispatch.USE_COMPONENT_MODEL_FS;
const USE_COMPONENT_MODEL_PROCESS = dispatch.USE_COMPONENT_MODEL_PROCESS;
const file_dispatch = dispatch.file_dispatch;
const crypto_dispatch = dispatch.crypto_dispatch;
const process_cm_dispatch = dispatch.process_cm_dispatch;
const stdlib_dispatch = dispatch.stdlib_dispatch;
const socket_host = dispatch.socket_host;

// Dispatch opcodes
const FILE_CM_READ = dispatch.FILE_CM_READ;
const FILE_CM_WRITE = dispatch.FILE_CM_WRITE;
const FILE_CM_EXISTS = dispatch.FILE_CM_EXISTS;
const FILE_CM_STAT = dispatch.FILE_CM_STAT;
const FILE_CM_READDIR = dispatch.FILE_CM_READDIR;
const FILE_CM_MKDIR = dispatch.FILE_CM_MKDIR;
const FILE_CM_UNLINK = dispatch.FILE_CM_UNLINK;
const FILE_CM_RMDIR = dispatch.FILE_CM_RMDIR;
const FILE_CM_RENAME = dispatch.FILE_CM_RENAME;
const FILE_CM_COPY = dispatch.FILE_CM_COPY;
const FILE_CM_GET_RESULT_LEN = dispatch.FILE_CM_GET_RESULT_LEN;
const FILE_CM_GET_RESULT = dispatch.FILE_CM_GET_RESULT;
const CRYPTO_OP_HASH = dispatch.CRYPTO_OP_HASH;
const CRYPTO_OP_HMAC = dispatch.CRYPTO_OP_HMAC;
const CRYPTO_OP_GET_RESULT_LEN = dispatch.CRYPTO_OP_GET_RESULT_LEN;
const CRYPTO_OP_GET_RESULT = dispatch.CRYPTO_OP_GET_RESULT;
const PROCESS_CM_SPAWN_SYNC = dispatch.PROCESS_CM_SPAWN_SYNC;
const PROCESS_CM_GET_RESULT_LEN = dispatch.PROCESS_CM_GET_RESULT_LEN;
const PROCESS_CM_GET_RESULT = dispatch.PROCESS_CM_GET_RESULT;
const STDLIB_OP_ARRAY_NEW = dispatch.STDLIB_OP_ARRAY_NEW;
const STDLIB_OP_ARRAY_PUSH = dispatch.STDLIB_OP_ARRAY_PUSH;
const STDLIB_OP_ARRAY_POP = dispatch.STDLIB_OP_ARRAY_POP;
const STDLIB_OP_ARRAY_GET = dispatch.STDLIB_OP_ARRAY_GET;
const STDLIB_OP_ARRAY_SET = dispatch.STDLIB_OP_ARRAY_SET;
const STDLIB_OP_ARRAY_LEN = dispatch.STDLIB_OP_ARRAY_LEN;
const STDLIB_OP_ARRAY_SORT = dispatch.STDLIB_OP_ARRAY_SORT;
const STDLIB_OP_ARRAY_SORT_DESC = dispatch.STDLIB_OP_ARRAY_SORT_DESC;
const STDLIB_OP_ARRAY_REVERSE = dispatch.STDLIB_OP_ARRAY_REVERSE;
const STDLIB_OP_ARRAY_CLEAR = dispatch.STDLIB_OP_ARRAY_CLEAR;
const STDLIB_OP_ARRAY_INDEX_OF = dispatch.STDLIB_OP_ARRAY_INDEX_OF;
const STDLIB_OP_ARRAY_FREE = dispatch.STDLIB_OP_ARRAY_FREE;
const STDLIB_OP_MAP_NEW = dispatch.STDLIB_OP_MAP_NEW;
const STDLIB_OP_MAP_SET = dispatch.STDLIB_OP_MAP_SET;
const STDLIB_OP_MAP_GET = dispatch.STDLIB_OP_MAP_GET;
const STDLIB_OP_MAP_HAS = dispatch.STDLIB_OP_MAP_HAS;
const STDLIB_OP_MAP_DELETE = dispatch.STDLIB_OP_MAP_DELETE;
const STDLIB_OP_MAP_LEN = dispatch.STDLIB_OP_MAP_LEN;
const STDLIB_OP_MAP_CLEAR = dispatch.STDLIB_OP_MAP_CLEAR;
const STDLIB_OP_MAP_FREE = dispatch.STDLIB_OP_MAP_FREE;

const wasi_nn = if (build_options.enable_wasi_nn) @import("../wasi_nn.zig") else struct {
    pub fn chat(_: []const u8, _: []u8) ![]u8 {
        return error.NotEnabled;
    }
    pub fn isAvailable() bool {
        return false;
    }
};

// Global allocator - set by main
pub var global_allocator: ?std.mem.Allocator = null;

// BigInt state
var bigint_handles: ?std.AutoHashMap(u32, *bigint.BigIntHandle) = null;
var next_bigint_id: u32 = 1;

fn getBigIntHandles() *std.AutoHashMap(u32, *bigint.BigIntHandle) {
    if (bigint_handles == null) {
        const allocator = global_allocator orelse @panic("allocator not initialized");
        bigint_handles = std.AutoHashMap(u32, *bigint.BigIntHandle).init(allocator);
    }
    return &bigint_handles.?;
}

// WebSocket state
var ws_connections: [64]?*websocket.WebSocketClient = [_]?*websocket.WebSocketClient{null} ** 64;

fn allocWsId() ?usize {
    for (ws_connections, 0..) |conn, i| {
        if (conn == null) return i;
    }
    return null;
}

const qjs = quickjs.c;

/// Get JS undefined value
inline fn jsUndefined() qjs.JSValue {
    // JS_UNDEFINED in QuickJS - use the C macro via eval
    return quickjs.jsUndefined();
}

/// Get JS bool value
pub inline fn jsBool(val: bool) qjs.JSValue {
    return if (val) quickjs.jsTrue() else quickjs.jsFalse();
}

// Extern declaration for JS_ToCStringLen2 to avoid cImport issues
extern fn JS_ToCStringLen2(ctx: ?*qjs.JSContext, plen: *usize, val: qjs.JSValue, cesu8: bool) ?[*:0]const u8;

// Debug helper to write to stderr using WASI fd_write
pub fn debugPrint(comptime fmt: []const u8, args: anytype) void {
    var buf: [512]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, fmt, args) catch return;
    const iov = [_]std.os.wasi.ciovec_t{.{ .base = msg.ptr, .len = msg.len }};
    var nwritten: usize = 0;
    _ = std.os.wasi.fd_write(2, &iov, 1, &nwritten);
}

/// Get string argument from JS value
pub fn getStringArg(ctx: ?*qjs.JSContext, val: qjs.JSValue) ?[]const u8 {
    var len: usize = 0;
    const cstr_opt = JS_ToCStringLen2(ctx, &len, val, false);
    const cstr = cstr_opt orelse return null;
    // Debug: print the raw length and first few bytes
    debugPrint("[getStringArg] JS_ToCStringLen2 returned len={d}, first bytes: {d} {d} {d}\n", .{ len, cstr[0], if (len > 1) cstr[1] else 0, if (len > 2) cstr[2] else 0 });
    // Fallback to strlen if len wasn't set
    if (len == 0) {
        len = std.mem.len(cstr);
        debugPrint("[getStringArg] strlen fallback: len={d}\n", .{len});
    }
    return cstr[0..len];
}

/// Free string argument
pub fn freeStringArg(ctx: ?*qjs.JSContext, str: []const u8) void {
    qjs.JS_FreeCString(ctx, str.ptr);
}

/// Native fetch implementation for WASM
pub fn nativeFetch(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fetch requires url argument");

    const url = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "url must be a string");
    defer freeStringArg(ctx, url);

    // Get method (default GET)
    const method = if (argc >= 2) getStringArg(ctx, argv[1]) orelse "GET" else "GET";
    const method_owned = argc >= 2 and getStringArg(ctx, argv[1]) != null;
    defer if (method_owned) freeStringArg(ctx, method);

    // Get headers (optional - arg 2) - format: "Key: Value\r\nKey2: Value2"
    const headers = if (argc >= 3 and !qjs.JS_IsUndefined(argv[2]) and !qjs.JS_IsNull(argv[2]))
        getStringArg(ctx, argv[2])
    else
        null;
    defer if (headers) |h| freeStringArg(ctx, h);

    // Get body (optional - arg 3)
    // Just try to get string - JS_ToCStringLen2 returns null for undefined/null
    const body = if (argc >= 4) getStringArg(ctx, argv[3]) else null;
    defer if (body) |b| freeStringArg(ctx, b);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Perform fetch using host HTTP dispatch
    var response = wasm_fetch.jsFetch(allocator, url, method, headers, body) catch |err| {
        return switch (err) {
            wasm_fetch.FetchError.InvalidUrl => qjs.JS_ThrowTypeError(ctx, "Invalid URL"),
            wasm_fetch.FetchError.ConnectionFailed => qjs.JS_ThrowInternalError(ctx, "Connection failed"),
            wasm_fetch.FetchError.HostNotFound => qjs.JS_ThrowInternalError(ctx, "Host not found"),
            wasm_fetch.FetchError.Timeout => qjs.JS_ThrowInternalError(ctx, "Request timed out"),
            wasm_fetch.FetchError.InvalidResponse => qjs.JS_ThrowInternalError(ctx, "Invalid HTTP response"),
            wasm_fetch.FetchError.OutOfMemory => qjs.JS_ThrowInternalError(ctx, "Out of memory"),
            wasm_fetch.FetchError.TlsNotSupported => qjs.JS_ThrowInternalError(ctx, "HTTPS not supported yet (use HTTP)"),
        };
    };
    defer response.deinit();

    // Create response object
    const obj = qjs.JS_NewObject(ctx);

    // Set status
    _ = qjs.JS_SetPropertyStr(ctx, obj, "status", qjs.JS_NewInt32(ctx, @intCast(response.status)));

    // Set ok (status 200-299)
    _ = qjs.JS_SetPropertyStr(ctx, obj, "ok", jsBool(response.status >= 200 and response.status < 300));

    // Set body as string
    _ = qjs.JS_SetPropertyStr(ctx, obj, "body", qjs.JS_NewStringLen(ctx, response.body.ptr, response.body.len));

    // Set headers as object
    const headers_obj = qjs.JS_NewObject(ctx);
    for (response.headers.items) |h| {
        // Need null-terminated key for JS_SetPropertyStr
        var key_buf: [256]u8 = undefined;
        if (h.name.len < key_buf.len) {
            @memcpy(key_buf[0..h.name.len], h.name);
            key_buf[h.name.len] = 0;
            _ = qjs.JS_SetPropertyStr(
                ctx,
                headers_obj,
                &key_buf,
                qjs.JS_NewStringLen(ctx, h.value.ptr, h.value.len),
            );
        }
    }
    _ = qjs.JS_SetPropertyStr(ctx, obj, "headers", headers_obj);

    return obj;
}

/// Native isatty implementation
pub fn nativeIsatty(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return jsBool(false);

    var fd: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &fd, argv[0]) < 0) {
        return jsBool(false);
    }

    const is_tty = wasi_tty.isatty(fd);
    return jsBool(is_tty);
}

/// Native get terminal size implementation
pub fn nativeGetTerminalSize(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const size = wasi_tty.getTerminalSize() orelse {
        // Return default size
        const obj = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, obj, "rows", qjs.JS_NewInt32(ctx, 24));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "cols", qjs.JS_NewInt32(ctx, 80));
        return obj;
    };

    const obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, obj, "rows", qjs.JS_NewInt32(ctx, size.rows));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "cols", qjs.JS_NewInt32(ctx, size.cols));
    return obj;
}

/// Native read stdin implementation
pub fn nativeReadStdin(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    var max_size: i32 = 1024;
    if (argc >= 1) {
        _ = qjs.JS_ToInt32(ctx, &max_size, argv[0]);
    }
    if (max_size <= 0) max_size = 1024;

    const line = wasi_tty.readLine(allocator, @intCast(max_size)) catch |err| {
        return qjs.JS_ThrowInternalError(ctx, "read error: %d", @intFromError(err));
    } orelse {
        return quickjs.jsNull();
    };
    defer allocator.free(line);

    return qjs.JS_NewStringLen(ctx, line.ptr, line.len);
}

/// Check if stdin has data ready (non-blocking)
pub fn nativeStdinReady(_: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const ready = wasi_tty.stdinReady();
    return if (ready) quickjs.jsTrue() else quickjs.jsFalse();
}

/// Native spawn implementation for child_process
/// Args: command (string), args (array), stdin (string|null), timeout (number)
/// Phase 9b: Routes through Component Model when USE_COMPONENT_MODEL_PROCESS is true
pub fn nativeSpawn(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "spawn requires command argument");

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Get command
    const command = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "command must be a string");
    defer freeStringArg(ctx, command);

    // Component Model path (Phase 9b)
    if (USE_COMPONENT_MODEL_PROCESS) {
        // Build args JSON array: ["arg1","arg2",...]
        var args_json_buf: [4096]u8 = undefined;
        var args_json_len: usize = 0;
        args_json_buf[0] = '[';
        args_json_len = 1;

        if (argc >= 2 and qjs.JS_IsArray(argv[1])) {
            const arr_len_val = qjs.JS_GetPropertyStr(ctx, argv[1], "length");
            var arr_len: i32 = 0;
            _ = qjs.JS_ToInt32(ctx, &arr_len, arr_len_val);
            qjs.JS_FreeValue(ctx, arr_len_val);

            var i: u32 = 0;
            while (i < @as(u32, @intCast(arr_len))) : (i += 1) {
                const elem = qjs.JS_GetPropertyUint32(ctx, argv[1], i);
                defer qjs.JS_FreeValue(ctx, elem);

                if (getStringArg(ctx, elem)) |arg_str| {
                    defer freeStringArg(ctx, arg_str);

                    // Add comma if not first element
                    if (i > 0 and args_json_len < args_json_buf.len - 1) {
                        args_json_buf[args_json_len] = ',';
                        args_json_len += 1;
                    }

                    // Add quoted string with escaping
                    if (args_json_len < args_json_buf.len - 1) {
                        args_json_buf[args_json_len] = '"';
                        args_json_len += 1;
                    }
                    for (arg_str) |ch| {
                        if (args_json_len >= args_json_buf.len - 2) break;
                        if (ch == '"' or ch == '\\') {
                            args_json_buf[args_json_len] = '\\';
                            args_json_len += 1;
                        }
                        if (args_json_len < args_json_buf.len - 1) {
                            args_json_buf[args_json_len] = ch;
                            args_json_len += 1;
                        }
                    }
                    if (args_json_len < args_json_buf.len - 1) {
                        args_json_buf[args_json_len] = '"';
                        args_json_len += 1;
                    }
                }
            }
        }
        if (args_json_len < args_json_buf.len) {
            args_json_buf[args_json_len] = ']';
            args_json_len += 1;
        }

        // Get stdin data
        var stdin_ptr: [*]const u8 = "";
        var stdin_len: usize = 0;
        var stdin_to_free: ?[]const u8 = null;
        if (argc >= 3 and !qjs.JS_IsNull(argv[2]) and !qjs.JS_IsUndefined(argv[2])) {
            if (getStringArg(ctx, argv[2])) |stdin_data| {
                stdin_ptr = stdin_data.ptr;
                stdin_len = stdin_data.len;
                stdin_to_free = stdin_data;
            }
        }
        defer if (stdin_to_free) |s| freeStringArg(ctx, s);

        // Get timeout
        var timeout_ms: i32 = 30000;
        if (argc >= 4) {
            _ = qjs.JS_ToInt32(ctx, &timeout_ms, argv[3]);
        }

        // Call Component Model dispatch
        const result = process_cm_dispatch(
            PROCESS_CM_SPAWN_SYNC,
            @intFromPtr(command.ptr),
            @intCast(command.len),
            @intFromPtr(&args_json_buf),
            @intCast(args_json_len),
            @intFromPtr(stdin_ptr),
            @intCast(stdin_len),
            @intCast(timeout_ms),
        );

        if (result < 0) {
            return qjs.JS_ThrowInternalError(ctx, mapProcessErrorCodeToString(result));
        }

        // Get result JSON length
        const result_len = process_cm_dispatch(PROCESS_CM_GET_RESULT_LEN, 0, 0, 0, 0, 0, 0, 0);
        if (result_len <= 0) {
            return qjs.JS_ThrowInternalError(ctx, "failed to get process result");
        }

        // Get result JSON
        var result_buf: [65536]u8 = undefined;
        const copy_result = process_cm_dispatch(
            PROCESS_CM_GET_RESULT,
            @intFromPtr(&result_buf),
            @intCast(result_buf.len),
            0,
            0,
            0,
            0,
            0,
        );
        if (copy_result < 0) {
            return qjs.JS_ThrowInternalError(ctx, "failed to copy process result");
        }

        // Parse JSON result: {"exitCode":N,"stdout":"...","stderr":"..."}
        const json_str = result_buf[0..@intCast(result_len)];
        const parsed = qjs.JS_ParseJSON(ctx, json_str.ptr, json_str.len, "<process_result>");
        if (qjs.JS_IsException(parsed)) {
            return qjs.JS_ThrowInternalError(ctx, "failed to parse process result JSON");
        }
        return parsed;
    }

    // Legacy path - direct implementation via wasi_process
    // Build command
    var cmd = wasi_process.Command.init(allocator, command);
    defer cmd.deinit();

    // Parse args array (arg 1)
    if (argc >= 2 and qjs.JS_IsArray(argv[1])) {
        const arr_len_val = qjs.JS_GetPropertyStr(ctx, argv[1], "length");
        var arr_len: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &arr_len, arr_len_val);
        qjs.JS_FreeValue(ctx, arr_len_val);

        var i: u32 = 0;
        while (i < @as(u32, @intCast(arr_len))) : (i += 1) {
            const elem = qjs.JS_GetPropertyUint32(ctx, argv[1], i);
            defer qjs.JS_FreeValue(ctx, elem);

            if (getStringArg(ctx, elem)) |arg_str| {
                // Need to copy because arg_str is freed when elem is freed
                const arg_copy = allocator.dupe(u8, arg_str) catch {
                    return qjs.JS_ThrowInternalError(ctx, "out of memory");
                };
                freeStringArg(ctx, arg_str);
                _ = cmd.arg(arg_copy) catch {
                    allocator.free(arg_copy);
                    return qjs.JS_ThrowInternalError(ctx, "out of memory");
                };
            }
        }
    }

    // Set stdin data (arg 2)
    if (argc >= 3 and !qjs.JS_IsNull(argv[2]) and !qjs.JS_IsUndefined(argv[2])) {
        if (getStringArg(ctx, argv[2])) |stdin_data| {
            _ = cmd.setStdin(stdin_data);
            // Note: don't free stdin_data until after command runs
        }
    }

    // Set timeout (arg 3)
    if (argc >= 4) {
        var timeout: i32 = 30000;
        _ = qjs.JS_ToInt32(ctx, &timeout, argv[3]);
        if (timeout > 0) {
            _ = cmd.setTimeout(@intCast(timeout));
        }
    }

    // Run the command
    var result = cmd.output() catch |err| {
        return switch (err) {
            wasi_process.ProcessError.CommandFailed => qjs.JS_ThrowInternalError(ctx, "Command failed to execute (edgebox_process API not available?)"),
            wasi_process.ProcessError.TimedOut => qjs.JS_ThrowInternalError(ctx, "Command timed out"),
            wasi_process.ProcessError.OutOfMemory => qjs.JS_ThrowInternalError(ctx, "Out of memory"),
            wasi_process.ProcessError.InvalidCommand => qjs.JS_ThrowTypeError(ctx, "Invalid command"),
            wasi_process.ProcessError.PermissionDenied => qjs.JS_ThrowTypeError(ctx, "Permission denied: command not in allowed list"),
        };
    };
    defer result.deinit();

    // Create result object
    const obj = qjs.JS_NewObject(ctx);

    // Set exitCode
    _ = qjs.JS_SetPropertyStr(ctx, obj, "exitCode", qjs.JS_NewInt32(ctx, result.exit_code));

    // Set stdout
    if (result.stdout.len > 0) {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "stdout", qjs.JS_NewStringLen(ctx, result.stdout.ptr, result.stdout.len));
    } else {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "stdout", qjs.JS_NewString(ctx, ""));
    }

    // Set stderr
    if (result.stderr.len > 0) {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "stderr", qjs.JS_NewStringLen(ctx, result.stderr.ptr, result.stderr.len));
    } else {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "stderr", qjs.JS_NewString(ctx, ""));
    }

    return obj;
}

// ============================================================================
// Process Error Mapping (Phase 9b)
// ============================================================================

pub fn mapProcessErrorCodeToString(code: i32) [*:0]const u8 {
    // Use unified ErrorCode for all error messages
    const err = errors.ErrorCode.fromLegacyCode(code);
    return err.message().ptr;
}

// ============================================================================
// File System Native Bindings
// ============================================================================

// FS error code to string mapping (Phase 9b)
pub fn mapFsErrorCodeToString(code: i32) [*:0]const u8 {
    // Use unified ErrorCode for all error messages
    const err = errors.ErrorCode.fromFsLegacyCode(code);
    return err.message().ptr;
}

/// Read file contents
/// Phase 9b: Routes through Component Model when USE_COMPONENT_MODEL_FS is true
pub fn nativeFsRead(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.readFileSync requires path argument");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    // Component Model path (Phase 9b)
    if (USE_COMPONENT_MODEL_FS) {
        const result = file_dispatch(
            FILE_CM_READ,
            @intFromPtr(path.ptr),
            @intCast(path.len),
            0,
            0,
        );

        if (result < 0) {
            return qjs.JS_ThrowInternalError(ctx, mapFsErrorCodeToString(result));
        }

        // Get result length
        const result_len = file_dispatch(FILE_CM_GET_RESULT_LEN, 0, 0, 0, 0);
        if (result_len < 0) {
            return qjs.JS_ThrowInternalError(ctx, "failed to get file result length");
        }

        // Get result into buffer
        var result_buf: [65536]u8 = undefined;
        const get_result = file_dispatch(
            FILE_CM_GET_RESULT,
            @intCast(@intFromPtr(&result_buf)),
            @intCast(result_buf.len),
            0,
            0,
        );

        if (get_result < 0) {
            return qjs.JS_ThrowInternalError(ctx, "failed to get file result");
        }

        return qjs.JS_NewStringLen(ctx, &result_buf, @intCast(result_len));
    }

    // Legacy inline implementation
    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    const file = std.fs.cwd().openFile(path, .{}) catch {
        return qjs.JS_ThrowInternalError(ctx, "ENOENT: no such file or directory");
    };
    defer file.close();

    const content = file.readToEndAlloc(allocator, 100 * 1024 * 1024) catch {
        return qjs.JS_ThrowInternalError(ctx, "failed to read file");
    };
    defer allocator.free(content);

    return qjs.JS_NewStringLen(ctx, content.ptr, content.len);
}

/// Write data to file
pub fn nativeFsWrite(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "fs.writeFileSync requires path and data arguments");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    const data = getStringArg(ctx, argv[1]) orelse
        return qjs.JS_ThrowTypeError(ctx, "data must be a string");
    defer freeStringArg(ctx, data);

    // Component Model path
    if (USE_COMPONENT_MODEL_FS) {
        const result = file_dispatch(
            FILE_CM_WRITE,
            @intCast(@intFromPtr(path.ptr)),
            @intCast(path.len),
            @intCast(@intFromPtr(data.ptr)),
            @intCast(data.len),
        );

        if (result < 0) {
            return qjs.JS_ThrowInternalError(ctx, mapFsErrorCodeToString(result));
        }

        return quickjs.jsUndefined();
    }

    // Legacy inline implementation
    const file = std.fs.cwd().createFile(path, .{}) catch {
        return qjs.JS_ThrowInternalError(ctx, "failed to create file");
    };
    defer file.close();

    file.writeAll(data) catch {
        return qjs.JS_ThrowInternalError(ctx, "failed to write file");
    };

    return quickjs.jsUndefined();
}

/// Check if file exists
pub fn nativeFsExists(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return jsBool(false);

    const path = getStringArg(ctx, argv[0]) orelse return jsBool(false);
    defer freeStringArg(ctx, path);

    // Component Model path
    if (USE_COMPONENT_MODEL_FS) {
        const result = file_dispatch(
            FILE_CM_EXISTS,
            @intCast(@intFromPtr(path.ptr)),
            @intCast(path.len),
            0,
            0,
        );
        // result: 1 = exists, 0 = not exists
        return jsBool(result == 1);
    }

    // Legacy inline implementation
    std.fs.cwd().access(path, .{}) catch return jsBool(false);
    return jsBool(true);
}

/// Get file stats
pub fn nativeFsStat(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.statSync requires path argument");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    // Component Model path
    if (USE_COMPONENT_MODEL_FS) {
        const result = file_dispatch(
            FILE_CM_STAT,
            @intCast(@intFromPtr(path.ptr)),
            @intCast(path.len),
            0,
            0,
        );

        if (result < 0) {
            return qjs.JS_ThrowInternalError(ctx, mapFsErrorCodeToString(result));
        }

        // Get result - stat is serialized as: size(8) | mode(4) | is_file(1) | is_dir(1) | mtime(8) | ctime(8) | atime(8)
        const result_len = file_dispatch(FILE_CM_GET_RESULT_LEN, 0, 0, 0, 0);
        if (result_len < 38) {
            return qjs.JS_ThrowInternalError(ctx, "invalid stat result");
        }

        var result_buf: [64]u8 = undefined;
        _ = file_dispatch(
            FILE_CM_GET_RESULT,
            @intCast(@intFromPtr(&result_buf)),
            @intCast(result_buf.len),
            0,
            0,
        );

        // Parse stat data
        const size = std.mem.readInt(i64, result_buf[0..8], .little);
        const mode = std.mem.readInt(i32, result_buf[8..12], .little);
        const is_file = result_buf[12] != 0;
        const is_dir = result_buf[13] != 0;

        const obj = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, obj, "size", qjs.JS_NewInt64(ctx, size));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "mode", qjs.JS_NewInt32(ctx, mode));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "_isDir", jsBool(is_dir));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "_isFile", jsBool(is_file));

        // Add isFile/isDirectory methods via eval
        const methods_code =
            \\(function(obj) {
            \\    obj.isFile = function() { return this._isFile; };
            \\    obj.isDirectory = function() { return this._isDir; };
            \\    return obj;
            \\})
        ;
        const methods_fn = qjs.JS_Eval(ctx, methods_code.ptr, methods_code.len, "<stat>", qjs.JS_EVAL_TYPE_GLOBAL);
        if (!qjs.JS_IsException(methods_fn)) {
            var args_arr = [_]qjs.JSValue{obj};
            const eval_result = qjs.JS_Call(ctx, methods_fn, quickjs.jsUndefined(), 1, &args_arr);
            qjs.JS_FreeValue(ctx, methods_fn);
            if (!qjs.JS_IsException(eval_result)) {
                return eval_result;
            }
            qjs.JS_FreeValue(ctx, eval_result);
        } else {
            qjs.JS_FreeValue(ctx, methods_fn);
        }

        return obj;
    }

    // Legacy inline implementation
    const stat = std.fs.cwd().statFile(path) catch {
        return qjs.JS_ThrowInternalError(ctx, "ENOENT: no such file or directory");
    };

    const obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, obj, "size", qjs.JS_NewInt64(ctx, @intCast(stat.size)));

    const is_dir = stat.kind == .directory;
    const mode: i32 = if (is_dir) 0o40755 else 0o100644;
    _ = qjs.JS_SetPropertyStr(ctx, obj, "mode", qjs.JS_NewInt32(ctx, mode));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "_isDir", jsBool(is_dir));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "_isFile", jsBool(!is_dir));

    // Add isFile/isDirectory methods via eval
    const methods_code =
        \\(function(obj) {
        \\    obj.isFile = function() { return this._isFile; };
        \\    obj.isDirectory = function() { return this._isDir; };
        \\    return obj;
        \\})
    ;
    const methods_fn = qjs.JS_Eval(ctx, methods_code.ptr, methods_code.len, "<stat>", qjs.JS_EVAL_TYPE_GLOBAL);
    if (!qjs.JS_IsException(methods_fn)) {
        var args = [_]qjs.JSValue{obj};
        const result = qjs.JS_Call(ctx, methods_fn, quickjs.jsUndefined(), 1, &args);
        qjs.JS_FreeValue(ctx, methods_fn);
        if (!qjs.JS_IsException(result)) {
            return result;
        }
        qjs.JS_FreeValue(ctx, result);
    } else {
        qjs.JS_FreeValue(ctx, methods_fn);
    }

    return obj;
}

/// Read directory entries
pub fn nativeFsReaddir(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.readdirSync requires path argument");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    // Component Model path
    if (USE_COMPONENT_MODEL_FS) {
        const result = file_dispatch(
            FILE_CM_READDIR,
            @intCast(@intFromPtr(path.ptr)),
            @intCast(path.len),
            0,
            0,
        );

        if (result < 0) {
            return qjs.JS_ThrowInternalError(ctx, mapFsErrorCodeToString(result));
        }

        // Get result - entries are serialized as: count(4) | [len(4) | name(len)]...
        const result_len = file_dispatch(FILE_CM_GET_RESULT_LEN, 0, 0, 0, 0);
        if (result_len < 4) {
            return qjs.JS_NewArray(ctx); // Empty array
        }

        var result_buf: [65536]u8 = undefined;
        const actual_len: usize = @intCast(@min(result_len, @as(i32, @intCast(result_buf.len))));
        _ = file_dispatch(
            FILE_CM_GET_RESULT,
            @intCast(@intFromPtr(&result_buf)),
            @intCast(result_buf.len),
            0,
            0,
        );

        // Parse entries
        const arr = qjs.JS_NewArray(ctx);
        const count = std.mem.readInt(u32, result_buf[0..4], .little);
        var offset: usize = 4;
        var idx: u32 = 0;

        while (idx < count and offset + 4 <= actual_len) {
            const name_len = std.mem.readInt(u32, result_buf[offset..][0..4], .little);
            offset += 4;
            if (offset + name_len > actual_len) break;

            const name_val = qjs.JS_NewStringLen(ctx, result_buf[offset..].ptr, name_len);
            _ = qjs.JS_SetPropertyUint32(ctx, arr, idx, name_val);
            offset += name_len;
            idx += 1;
        }

        return arr;
    }

    // Legacy inline implementation
    var dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch {
        return qjs.JS_ThrowInternalError(ctx, "ENOENT: no such file or directory");
    };
    defer dir.close();

    const arr = qjs.JS_NewArray(ctx);
    var idx: u32 = 0;

    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        const name_val = qjs.JS_NewStringLen(ctx, entry.name.ptr, entry.name.len);
        _ = qjs.JS_SetPropertyUint32(ctx, arr, idx, name_val);
        idx += 1;
    }

    return arr;
}

/// Create directory
pub fn nativeFsMkdir(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.mkdirSync requires path argument");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    const recursive = if (argc >= 2) qjs.JS_ToBool(ctx, argv[1]) != 0 else false;

    // Component Model path
    if (USE_COMPONENT_MODEL_FS) {
        const result = file_dispatch(
            FILE_CM_MKDIR,
            @intCast(@intFromPtr(path.ptr)),
            @intCast(path.len),
            if (recursive) 1 else 0,
            0,
        );

        if (result < 0) {
            return qjs.JS_ThrowInternalError(ctx, mapFsErrorCodeToString(result));
        }

        return quickjs.jsUndefined();
    }

    // Legacy inline implementation
    if (recursive) {
        std.fs.cwd().makePath(path) catch {
            return qjs.JS_ThrowInternalError(ctx, "failed to create directory");
        };
    } else {
        std.fs.cwd().makeDir(path) catch {
            return qjs.JS_ThrowInternalError(ctx, "failed to create directory");
        };
    }

    return quickjs.jsUndefined();
}

/// Delete file
pub fn nativeFsUnlink(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.unlinkSync requires path argument");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    // Component Model path
    if (USE_COMPONENT_MODEL_FS) {
        const result = file_dispatch(
            FILE_CM_UNLINK,
            @intCast(@intFromPtr(path.ptr)),
            @intCast(path.len),
            0,
            0,
        );

        if (result < 0) {
            return qjs.JS_ThrowInternalError(ctx, mapFsErrorCodeToString(result));
        }

        return quickjs.jsUndefined();
    }

    // Legacy inline implementation
    std.fs.cwd().deleteFile(path) catch {
        return qjs.JS_ThrowInternalError(ctx, "ENOENT: no such file or directory");
    };

    return quickjs.jsUndefined();
}

/// Delete directory
pub fn nativeFsRmdir(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fs.rmdirSync requires path argument");

    const path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "path must be a string");
    defer freeStringArg(ctx, path);

    const recursive = if (argc >= 2) qjs.JS_ToBool(ctx, argv[1]) != 0 else false;

    // Component Model path
    if (USE_COMPONENT_MODEL_FS) {
        const result = file_dispatch(
            FILE_CM_RMDIR,
            @intCast(@intFromPtr(path.ptr)),
            @intCast(path.len),
            if (recursive) 1 else 0,
            0,
        );

        if (result < 0) {
            return qjs.JS_ThrowInternalError(ctx, mapFsErrorCodeToString(result));
        }

        return quickjs.jsUndefined();
    }

    // Legacy inline implementation
    if (recursive) {
        std.fs.cwd().deleteTree(path) catch {
            return qjs.JS_ThrowInternalError(ctx, "failed to delete directory");
        };
    } else {
        std.fs.cwd().deleteDir(path) catch {
            return qjs.JS_ThrowInternalError(ctx, "ENOTEMPTY: directory not empty");
        };
    }

    return quickjs.jsUndefined();
}

/// Rename file/directory
pub fn nativeFsRename(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "fs.renameSync requires oldPath and newPath arguments");

    const old_path = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "oldPath must be a string");
    defer freeStringArg(ctx, old_path);

    const new_path = getStringArg(ctx, argv[1]) orelse
        return qjs.JS_ThrowTypeError(ctx, "newPath must be a string");
    defer freeStringArg(ctx, new_path);

    // Component Model path - encode both paths: old_path_len(4) | old_path | new_path
    if (USE_COMPONENT_MODEL_FS) {
        const result = file_dispatch(
            FILE_CM_RENAME,
            @intCast(@intFromPtr(old_path.ptr)),
            @intCast(old_path.len),
            @intCast(@intFromPtr(new_path.ptr)),
            @intCast(new_path.len),
        );

        if (result < 0) {
            return qjs.JS_ThrowInternalError(ctx, mapFsErrorCodeToString(result));
        }

        return quickjs.jsUndefined();
    }

    // Legacy inline implementation
    std.fs.cwd().rename(old_path, new_path) catch {
        return qjs.JS_ThrowInternalError(ctx, "failed to rename");
    };

    return quickjs.jsUndefined();
}

/// Copy file
pub fn nativeFsCopy(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "fs.copyFileSync requires src and dest arguments");

    const src = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "src must be a string");
    defer freeStringArg(ctx, src);

    const dest = getStringArg(ctx, argv[1]) orelse
        return qjs.JS_ThrowTypeError(ctx, "dest must be a string");
    defer freeStringArg(ctx, dest);

    // Component Model path
    if (USE_COMPONENT_MODEL_FS) {
        const result = file_dispatch(
            FILE_CM_COPY,
            @intCast(@intFromPtr(src.ptr)),
            @intCast(src.len),
            @intCast(@intFromPtr(dest.ptr)),
            @intCast(dest.len),
        );

        if (result < 0) {
            return qjs.JS_ThrowInternalError(ctx, mapFsErrorCodeToString(result));
        }

        return quickjs.jsUndefined();
    }

    // Legacy inline implementation
    std.fs.cwd().copyFile(src, std.fs.cwd(), dest, .{}) catch {
        return qjs.JS_ThrowInternalError(ctx, "failed to copy file");
    };

    return quickjs.jsUndefined();
}

/// Get current working directory
pub fn nativeCwd(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    // Get PWD from environment via WASI
    const allocator = global_allocator orelse {
        return qjs.JS_NewString(ctx, "/");
    };

    // Get environ from WASI
    var environ_count: usize = 0;
    var environ_buf_size: usize = 0;
    _ = std.os.wasi.environ_sizes_get(&environ_count, &environ_buf_size);

    if (environ_count == 0) {
        return qjs.JS_NewString(ctx, "/");
    }

    const environ_ptrs = allocator.alloc([*:0]u8, environ_count) catch {
        return qjs.JS_NewString(ctx, "/");
    };
    defer allocator.free(environ_ptrs);

    const environ_buf = allocator.alloc(u8, environ_buf_size) catch {
        return qjs.JS_NewString(ctx, "/");
    };
    defer allocator.free(environ_buf);

    _ = std.os.wasi.environ_get(environ_ptrs.ptr, environ_buf.ptr);

    for (environ_ptrs) |env_ptr| {
        const env = std.mem.span(env_ptr);
        if (std.mem.startsWith(u8, env, "PWD=")) {
            const pwd = env[4..];
            return qjs.JS_NewStringLen(ctx, pwd.ptr, pwd.len);
        }
    }

    return qjs.JS_NewString(ctx, "/");
}

/// Get home directory
pub fn nativeHomedir(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const allocator = global_allocator orelse {
        return qjs.JS_NewString(ctx, "/home/user");
    };

    // Get environ from WASI
    var environ_count: usize = 0;
    var environ_buf_size: usize = 0;
    _ = std.os.wasi.environ_sizes_get(&environ_count, &environ_buf_size);

    if (environ_count == 0) {
        return qjs.JS_NewString(ctx, "/home/user");
    }

    const environ_ptrs = allocator.alloc([*:0]u8, environ_count) catch {
        return qjs.JS_NewString(ctx, "/home/user");
    };
    defer allocator.free(environ_ptrs);

    const environ_buf = allocator.alloc(u8, environ_buf_size) catch {
        return qjs.JS_NewString(ctx, "/home/user");
    };
    defer allocator.free(environ_buf);

    _ = std.os.wasi.environ_get(environ_ptrs.ptr, environ_buf.ptr);

    for (environ_ptrs) |env_ptr| {
        const env = std.mem.span(env_ptr);
        if (std.mem.startsWith(u8, env, "HOME=")) {
            const home = env[5..];
            return qjs.JS_NewStringLen(ctx, home.ptr, home.len);
        }
    }

    return qjs.JS_NewString(ctx, "/home/user");
}

// ============================================================================
// Zlib Native Bindings
// ============================================================================

/// Get binary data from JS value (supports String, Uint8Array, ArrayBuffer)
pub fn getBinaryArg(ctx: ?*qjs.JSContext, val: qjs.JSValue) ?[]const u8 {
    // Try typed array first (Uint8Array, etc.)
    var offset: usize = undefined;
    var byte_len: usize = undefined;
    var bytes_per_element: usize = undefined;
    const array_buf = qjs.JS_GetTypedArrayBuffer(ctx, val, &offset, &byte_len, &bytes_per_element);

    if (!qjs.JS_IsException(array_buf)) {
        var size: usize = undefined;
        const ptr = qjs.JS_GetArrayBuffer(ctx, &size, array_buf);
        qjs.JS_FreeValue(ctx, array_buf);
        if (ptr != null and byte_len > 0) {
            return (ptr + offset)[0..byte_len];
        }
    } else {
        // Clear exception from failed typed array check
        const exc = qjs.JS_GetException(ctx);
        qjs.JS_FreeValue(ctx, exc);
    }

    // Try raw ArrayBuffer
    var ab_size: usize = undefined;
    const ab_ptr = qjs.JS_GetArrayBuffer(ctx, &ab_size, val);
    if (ab_ptr != null and ab_size > 0) {
        return ab_ptr[0..ab_size];
    } else {
        // Clear exception from failed ArrayBuffer check
        const exc = qjs.JS_GetException(ctx);
        qjs.JS_FreeValue(ctx, exc);
    }

    // Fall back to string
    var len: usize = undefined;
    const cstr = qjs.JS_ToCStringLen(ctx, &len, val);
    if (cstr != null) {
        return cstr[0..len];
    }

    return null;
}

/// Free binary data if it was from a string
pub fn freeBinaryArg(ctx: ?*qjs.JSContext, data: []const u8, val: qjs.JSValue) void {
    // Check if it was a typed array or ArrayBuffer (no need to free)
    var offset: usize = undefined;
    var byte_len: usize = undefined;
    var bytes_per_element: usize = undefined;
    const array_buf = qjs.JS_GetTypedArrayBuffer(ctx, val, &offset, &byte_len, &bytes_per_element);
    if (!qjs.JS_IsException(array_buf)) {
        qjs.JS_FreeValue(ctx, array_buf);
        return; // Typed array - don't free
    } else {
        // Clear exception
        const exc = qjs.JS_GetException(ctx);
        qjs.JS_FreeValue(ctx, exc);
    }

    var ab_size: usize = undefined;
    const ab_ptr = qjs.JS_GetArrayBuffer(ctx, &ab_size, val);
    if (ab_ptr != null) {
        return; // ArrayBuffer - don't free
    } else {
        // Clear exception
        const exc = qjs.JS_GetException(ctx);
        qjs.JS_FreeValue(ctx, exc);
    }

    // String - needs to be freed
    qjs.JS_FreeCString(ctx, data.ptr);
}

/// Decompress gzip data
pub fn nativeGunzip(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "gunzip requires data argument");

    const data = getBinaryArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "data must be a string or buffer");
    defer freeBinaryArg(ctx, data, argv[0]);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    const decompressed = wasm_zlib.gunzip(allocator, data) catch |err| {
        return switch (err) {
            wasm_zlib.ZlibError.InvalidInput => qjs.JS_ThrowTypeError(ctx, "invalid gzip data"),
            wasm_zlib.ZlibError.DecompressionFailed => qjs.JS_ThrowInternalError(ctx, "gzip decompression failed"),
            else => qjs.JS_ThrowInternalError(ctx, "gunzip error"),
        };
    };
    defer allocator.free(decompressed);

    return qjs.JS_NewStringLen(ctx, decompressed.ptr, decompressed.len);
}

/// Decompress raw deflate data
pub fn nativeInflate(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "inflate requires data argument");

    const data = getBinaryArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "data must be a string or buffer");
    defer freeBinaryArg(ctx, data, argv[0]);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    const decompressed = wasm_zlib.inflate(allocator, data) catch |err| {
        return switch (err) {
            wasm_zlib.ZlibError.InvalidInput => qjs.JS_ThrowTypeError(ctx, "invalid deflate data"),
            wasm_zlib.ZlibError.DecompressionFailed => qjs.JS_ThrowInternalError(ctx, "deflate decompression failed"),
            else => qjs.JS_ThrowInternalError(ctx, "inflate error"),
        };
    };
    defer allocator.free(decompressed);

    return qjs.JS_NewStringLen(ctx, decompressed.ptr, decompressed.len);
}

/// Decompress zlib-wrapped deflate data
pub fn nativeInflateZlib(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "inflateZlib requires data argument");

    const data = getBinaryArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "data must be a string or buffer");
    defer freeBinaryArg(ctx, data, argv[0]);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    const decompressed = wasm_zlib.inflateZlib(allocator, data) catch |err| {
        return switch (err) {
            wasm_zlib.ZlibError.InvalidInput => qjs.JS_ThrowTypeError(ctx, "invalid zlib data"),
            wasm_zlib.ZlibError.DecompressionFailed => qjs.JS_ThrowInternalError(ctx, "zlib decompression failed"),
            else => qjs.JS_ThrowInternalError(ctx, "inflateZlib error"),
        };
    };
    defer allocator.free(decompressed);

    return qjs.JS_NewStringLen(ctx, decompressed.ptr, decompressed.len);
}

// ============================================================================
// Crypto Hash/HMAC Functions
// ============================================================================

const crypto = std.crypto;

/// Convert bytes to hex string
pub fn hexEncode(ctx: ?*qjs.JSContext, bytes: []const u8) qjs.JSValue {
    const hex_chars = "0123456789abcdef";
    const allocator = global_allocator orelse return quickjs.jsUndefined();

    const hex = allocator.alloc(u8, bytes.len * 2) catch return quickjs.jsUndefined();
    defer allocator.free(hex);

    for (bytes, 0..) |byte, i| {
        hex[i * 2] = hex_chars[byte >> 4];
        hex[i * 2 + 1] = hex_chars[byte & 0x0f];
    }

    return qjs.JS_NewStringLen(ctx, hex.ptr, hex.len);
}

/// Native hash function: __edgebox_hash(algorithm, data)
/// Returns hex-encoded hash string
/// Phase 9b: Routes through Component Model when USE_COMPONENT_MODEL_CRYPTO is true
pub fn nativeHash(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "hash requires algorithm and data arguments");

    // Get algorithm name
    var algo_len: usize = 0;
    const algo_ptr = qjs.JS_ToCStringLen(ctx, &algo_len, argv[0]);
    if (algo_ptr == null) return qjs.JS_ThrowTypeError(ctx, "algorithm must be a string");
    defer qjs.JS_FreeCString(ctx, algo_ptr);
    const algorithm = algo_ptr[0..algo_len];

    // Get data
    const data = getBinaryArg(ctx, argv[1]) orelse
        return qjs.JS_ThrowTypeError(ctx, "data must be a string or buffer");
    defer freeBinaryArg(ctx, data, argv[1]);

    // Phase 9b: Route through Component Model via crypto_dispatch
    if (USE_COMPONENT_MODEL_CRYPTO) {
        // Map algorithm name to enum (0=sha256, 1=sha384, 2=sha512, 3=sha1, 4=md5)
        const algo_enum: u32 = if (std.mem.eql(u8, algorithm, "sha256"))
            0
        else if (std.mem.eql(u8, algorithm, "sha384"))
            1
        else if (std.mem.eql(u8, algorithm, "sha512"))
            2
        else if (std.mem.eql(u8, algorithm, "sha1"))
            3
        else if (std.mem.eql(u8, algorithm, "md5"))
            4
        else
            return qjs.JS_ThrowTypeError(ctx, "unsupported hash algorithm");

        // Call Component Model via dispatch
        // crypto_dispatch(CRYPTO_OP_HASH, algo, data_ptr, data_len, 0, 0, 0)
        const result = crypto_dispatch(
            CRYPTO_OP_HASH,
            algo_enum,
            @intFromPtr(data.ptr),
            @intCast(data.len),
            0,
            0,
            0,
        );

        if (result != 0) {
            return qjs.JS_ThrowTypeError(ctx, "hash operation failed");
        }

        // Get result length
        const result_len = crypto_dispatch(CRYPTO_OP_GET_RESULT_LEN, 0, 0, 0, 0, 0, 0);
        if (result_len < 0) {
            return qjs.JS_ThrowTypeError(ctx, "failed to get hash result length");
        }

        // Get result into stack-allocated buffer
        var result_buf: [256]u8 = undefined;
        const get_result = crypto_dispatch(
            CRYPTO_OP_GET_RESULT,
            @intCast(@intFromPtr(&result_buf)),
            @intCast(result_buf.len),
            0,
            0,
            0,
            0,
        );

        if (get_result < 0) {
            return qjs.JS_ThrowTypeError(ctx, "failed to get hash result");
        }

        return qjs.JS_NewStringLen(ctx, &result_buf, @intCast(result_len));
    }

    // Legacy inline implementation (when USE_COMPONENT_MODEL_CRYPTO is false)
    if (std.mem.eql(u8, algorithm, "sha256")) {
        var hash: [32]u8 = undefined;
        crypto.hash.sha2.Sha256.hash(data, &hash, .{});
        return hexEncode(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "sha384")) {
        var hash: [48]u8 = undefined;
        crypto.hash.sha2.Sha384.hash(data, &hash, .{});
        return hexEncode(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "sha512")) {
        var hash: [64]u8 = undefined;
        crypto.hash.sha2.Sha512.hash(data, &hash, .{});
        return hexEncode(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "sha1")) {
        var hash: [20]u8 = undefined;
        crypto.hash.Sha1.hash(data, &hash, .{});
        return hexEncode(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "md5")) {
        var hash: [16]u8 = undefined;
        crypto.hash.Md5.hash(data, &hash, .{});
        return hexEncode(ctx, &hash);
    } else {
        return qjs.JS_ThrowTypeError(ctx, "unsupported hash algorithm");
    }
}

/// Native HMAC function: __edgebox_hmac(algorithm, key, data)
/// Returns hex-encoded HMAC string
pub fn nativeHmac(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) return qjs.JS_ThrowTypeError(ctx, "hmac requires algorithm, key, and data arguments");

    // Get algorithm name
    var algo_len: usize = 0;
    const algo_ptr = qjs.JS_ToCStringLen(ctx, &algo_len, argv[0]);
    if (algo_ptr == null) return qjs.JS_ThrowTypeError(ctx, "algorithm must be a string");
    defer qjs.JS_FreeCString(ctx, algo_ptr);
    const algorithm = algo_ptr[0..algo_len];

    // Get key
    const key = getBinaryArg(ctx, argv[1]) orelse
        return qjs.JS_ThrowTypeError(ctx, "key must be a string or buffer");
    defer freeBinaryArg(ctx, key, argv[1]);

    // Get data
    const data = getBinaryArg(ctx, argv[2]) orelse
        return qjs.JS_ThrowTypeError(ctx, "data must be a string or buffer");
    defer freeBinaryArg(ctx, data, argv[2]);

    // Component Model path (Phase 9b)
    if (USE_COMPONENT_MODEL_CRYPTO) {
        // Map algorithm name to enum value (matches Component Model crypto interface)
        const algo_enum: u32 = if (std.mem.eql(u8, algorithm, "sha256"))
            0 // sha256
        else if (std.mem.eql(u8, algorithm, "sha384"))
            1 // sha384
        else if (std.mem.eql(u8, algorithm, "sha512"))
            2 // sha512
        else
            return qjs.JS_ThrowTypeError(ctx, "unsupported hmac algorithm");

        // Call crypto_dispatch with HMAC opcode
        // Args: opcode, algo_enum, key_ptr, key_len, data_ptr, data_len, unused
        const result = crypto_dispatch(
            CRYPTO_OP_HMAC,
            @intCast(algo_enum),
            @intCast(@intFromPtr(key.ptr)),
            @intCast(key.len),
            @intCast(@intFromPtr(data.ptr)),
            @intCast(data.len),
            0,
        );

        if (result < 0) {
            return qjs.JS_ThrowTypeError(ctx, "hmac failed");
        }

        // Get result length
        const result_len = crypto_dispatch(CRYPTO_OP_GET_RESULT_LEN, 0, 0, 0, 0, 0, 0);
        if (result_len <= 0) {
            return qjs.JS_ThrowTypeError(ctx, "hmac failed: no result");
        }

        // Get result into buffer
        var result_buf: [256]u8 = undefined;
        const get_result = crypto_dispatch(
            CRYPTO_OP_GET_RESULT,
            @intCast(@intFromPtr(&result_buf)),
            @intCast(result_buf.len),
            0,
            0,
            0,
            0,
        );

        if (get_result < 0) {
            return qjs.JS_ThrowTypeError(ctx, "hmac failed: could not get result");
        }

        // Return hex string result
        return qjs.JS_NewStringLen(ctx, &result_buf, @intCast(result_len));
    }

    // Legacy inline implementation (fallback)
    if (std.mem.eql(u8, algorithm, "sha256")) {
        var mac: [32]u8 = undefined;
        crypto.auth.hmac.sha2.HmacSha256.create(&mac, data, key);
        return hexEncode(ctx, &mac);
    } else if (std.mem.eql(u8, algorithm, "sha384")) {
        var mac: [48]u8 = undefined;
        crypto.auth.hmac.sha2.HmacSha384.create(&mac, data, key);
        return hexEncode(ctx, &mac);
    } else if (std.mem.eql(u8, algorithm, "sha512")) {
        var mac: [64]u8 = undefined;
        crypto.auth.hmac.sha2.HmacSha512.create(&mac, data, key);
        return hexEncode(ctx, &mac);
    } else {
        return qjs.JS_ThrowTypeError(ctx, "unsupported hmac algorithm");
    }
}

/// WASI-NN AI chat function
/// Requires: Build with WASI-NN support: zig build wasm -Denable-wasi-nn=true
pub fn nativeAIChat(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "ai.chat requires a prompt argument");

    // Get prompt string
    var prompt_len: usize = 0;
    const prompt_ptr = qjs.JS_ToCStringLen(ctx, &prompt_len, argv[0]);
    if (prompt_ptr == null) return qjs.JS_ThrowTypeError(ctx, "prompt must be a string");
    defer qjs.JS_FreeCString(ctx, prompt_ptr);
    const prompt = prompt_ptr[0..prompt_len];

    // Output buffer for response
    var output_buf: [8192]u8 = undefined;

    // Call WASI-NN chat
    const response = wasi_nn.chat(prompt, &output_buf) catch {
        if (!build_options.enable_wasi_nn) {
            return qjs.JS_ThrowTypeError(ctx, "WASI-NN: Not enabled (build with -Denable-wasi-nn=true)");
        }
        return qjs.JS_ThrowTypeError(ctx, "WASI-NN: Inference failed (check model/prompt)");
    };

    return qjs.JS_NewStringLen(ctx, response.ptr, response.len);
}

/// Check if WASI-NN is available
pub fn nativeAIAvailable(_: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return if (wasi_nn.isAvailable()) quickjs.jsTrue() else quickjs.jsFalse();
}

// ============================================================================
// Stdlib Native Bindings (HostArray, HostMap)
// ============================================================================
// These provide ~10x faster data structure operations by using native Zig
// implementations instead of WASM. Values are i32 only (no JS objects).

/// Create a new native array, returns handle
pub fn nativeArrayNew(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const handle = stdlib_dispatch(STDLIB_OP_ARRAY_NEW, 0, 0, 0, 0);
    if (handle < 0) return qjs.JS_ThrowInternalError(ctx, "Failed to create array");
    return qjs.JS_NewInt32(ctx, handle);
}

/// Push value to array: __edgebox_array_push(handle, value)
pub fn nativeArrayPush(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "array_push requires handle and value");
    var handle: i32 = 0;
    var value: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);
    _ = qjs.JS_ToInt32(ctx, &value, argv[1]);
    const result = stdlib_dispatch(STDLIB_OP_ARRAY_PUSH, @intCast(handle), @intCast(value), 0, 0);
    return qjs.JS_NewInt32(ctx, result);
}

/// Pop value from array: __edgebox_array_pop(handle)
pub fn nativeArrayPop(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "array_pop requires handle");
    var handle: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);
    const result = stdlib_dispatch(STDLIB_OP_ARRAY_POP, @intCast(handle), 0, 0, 0);
    return qjs.JS_NewInt32(ctx, result);
}

/// Get value at index: __edgebox_array_get(handle, index)
pub fn nativeArrayGet(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "array_get requires handle and index");
    var handle: i32 = 0;
    var index: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);
    _ = qjs.JS_ToInt32(ctx, &index, argv[1]);
    const result = stdlib_dispatch(STDLIB_OP_ARRAY_GET, @intCast(handle), @intCast(index), 0, 0);
    return qjs.JS_NewInt32(ctx, result);
}

/// Set value at index: __edgebox_array_set(handle, index, value)
pub fn nativeArraySet(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) return qjs.JS_ThrowTypeError(ctx, "array_set requires handle, index, and value");
    var handle: i32 = 0;
    var index: i32 = 0;
    var value: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);
    _ = qjs.JS_ToInt32(ctx, &index, argv[1]);
    _ = qjs.JS_ToInt32(ctx, &value, argv[2]);
    const result = stdlib_dispatch(STDLIB_OP_ARRAY_SET, @intCast(handle), @intCast(index), @intCast(value), 0);
    return qjs.JS_NewInt32(ctx, result);
}

/// Get array length: __edgebox_array_len(handle)
pub fn nativeArrayLen(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "array_len requires handle");
    var handle: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);
    const result = stdlib_dispatch(STDLIB_OP_ARRAY_LEN, @intCast(handle), 0, 0, 0);
    return qjs.JS_NewInt32(ctx, result);
}

/// Sort array ascending: __edgebox_array_sort(handle)
pub fn nativeArraySort(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "array_sort requires handle");
    var handle: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);
    const result = stdlib_dispatch(STDLIB_OP_ARRAY_SORT, @intCast(handle), 0, 0, 0);
    return qjs.JS_NewInt32(ctx, result);
}

/// Sort array descending: __edgebox_array_sort_desc(handle)
pub fn nativeArraySortDesc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "array_sort_desc requires handle");
    var handle: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);
    const result = stdlib_dispatch(STDLIB_OP_ARRAY_SORT_DESC, @intCast(handle), 0, 0, 0);
    return qjs.JS_NewInt32(ctx, result);
}

/// Reverse array: __edgebox_array_reverse(handle)
pub fn nativeArrayReverse(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "array_reverse requires handle");
    var handle: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);
    const result = stdlib_dispatch(STDLIB_OP_ARRAY_REVERSE, @intCast(handle), 0, 0, 0);
    return qjs.JS_NewInt32(ctx, result);
}

/// Clear array: __edgebox_array_clear(handle)
pub fn nativeArrayClear(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "array_clear requires handle");
    var handle: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);
    const result = stdlib_dispatch(STDLIB_OP_ARRAY_CLEAR, @intCast(handle), 0, 0, 0);
    return qjs.JS_NewInt32(ctx, result);
}

/// Find index of value: __edgebox_array_index_of(handle, value)
pub fn nativeArrayIndexOf(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "array_index_of requires handle and value");
    var handle: i32 = 0;
    var value: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);
    _ = qjs.JS_ToInt32(ctx, &value, argv[1]);
    const result = stdlib_dispatch(STDLIB_OP_ARRAY_INDEX_OF, @intCast(handle), @intCast(value), 0, 0);
    return qjs.JS_NewInt32(ctx, result);
}

/// Free array: __edgebox_array_free(handle)
pub fn nativeArrayFree(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "array_free requires handle");
    var handle: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);
    const result = stdlib_dispatch(STDLIB_OP_ARRAY_FREE, @intCast(handle), 0, 0, 0);
    return qjs.JS_NewInt32(ctx, result);
}

/// Create a new native map, returns handle
pub fn nativeMapNew(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const handle = stdlib_dispatch(STDLIB_OP_MAP_NEW, 0, 0, 0, 0);
    if (handle < 0) return qjs.JS_ThrowInternalError(ctx, "Failed to create map");
    return qjs.JS_NewInt32(ctx, handle);
}

/// Set key-value in map: __edgebox_map_set(handle, key, value)
pub fn nativeMapSet(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) return qjs.JS_ThrowTypeError(ctx, "map_set requires handle, key, and value");
    var handle: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);

    // Get string key
    var key_len: usize = 0;
    const key_ptr = qjs.JS_ToCStringLen(ctx, &key_len, argv[1]);
    if (key_ptr == null) return qjs.JS_ThrowTypeError(ctx, "key must be a string");
    defer qjs.JS_FreeCString(ctx, key_ptr);

    var value: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &value, argv[2]);

    // Pass string pointer and length to host
    const result = stdlib_dispatch(
        STDLIB_OP_MAP_SET,
        @intCast(handle),
        @intFromPtr(key_ptr),
        @intCast(key_len),
        @intCast(value),
    );
    return qjs.JS_NewInt32(ctx, result);
}

/// Get value for key: __edgebox_map_get(handle, key)
pub fn nativeMapGet(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "map_get requires handle and key");
    var handle: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);

    // Get string key
    var key_len: usize = 0;
    const key_ptr = qjs.JS_ToCStringLen(ctx, &key_len, argv[1]);
    if (key_ptr == null) return qjs.JS_ThrowTypeError(ctx, "key must be a string");
    defer qjs.JS_FreeCString(ctx, key_ptr);

    const result = stdlib_dispatch(
        STDLIB_OP_MAP_GET,
        @intCast(handle),
        @intFromPtr(key_ptr),
        @intCast(key_len),
        0,
    );
    return qjs.JS_NewInt32(ctx, result);
}

/// Check if key exists: __edgebox_map_has(handle, key)
pub fn nativeMapHas(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "map_has requires handle and key");
    var handle: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);

    // Get string key
    var key_len: usize = 0;
    const key_ptr = qjs.JS_ToCStringLen(ctx, &key_len, argv[1]);
    if (key_ptr == null) return qjs.JS_ThrowTypeError(ctx, "key must be a string");
    defer qjs.JS_FreeCString(ctx, key_ptr);

    const result = stdlib_dispatch(
        STDLIB_OP_MAP_HAS,
        @intCast(handle),
        @intFromPtr(key_ptr),
        @intCast(key_len),
        0,
    );
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// Delete key from map: __edgebox_map_delete(handle, key)
pub fn nativeMapDelete(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "map_delete requires handle and key");
    var handle: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);

    // Get string key
    var key_len: usize = 0;
    const key_ptr = qjs.JS_ToCStringLen(ctx, &key_len, argv[1]);
    if (key_ptr == null) return qjs.JS_ThrowTypeError(ctx, "key must be a string");
    defer qjs.JS_FreeCString(ctx, key_ptr);

    const result = stdlib_dispatch(
        STDLIB_OP_MAP_DELETE,
        @intCast(handle),
        @intFromPtr(key_ptr),
        @intCast(key_len),
        0,
    );
    return if (result == 1) quickjs.jsTrue() else quickjs.jsFalse();
}

/// Get map size: __edgebox_map_len(handle)
pub fn nativeMapLen(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "map_len requires handle");
    var handle: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);
    const result = stdlib_dispatch(STDLIB_OP_MAP_LEN, @intCast(handle), 0, 0, 0);
    return qjs.JS_NewInt32(ctx, result);
}

/// Clear map: __edgebox_map_clear(handle)
pub fn nativeMapClear(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "map_clear requires handle");
    var handle: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);
    const result = stdlib_dispatch(STDLIB_OP_MAP_CLEAR, @intCast(handle), 0, 0, 0);
    return qjs.JS_NewInt32(ctx, result);
}

/// Free map: __edgebox_map_free(handle)
pub fn nativeMapFree(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "map_free requires handle");
    var handle: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle, argv[0]);
    const result = stdlib_dispatch(STDLIB_OP_MAP_FREE, @intCast(handle), 0, 0, 0);
    return qjs.JS_NewInt32(ctx, result);
}

// ============================================================================
// Bytecode Cache Helpers
// ============================================================================

/// Hash all polyfill sources for cache invalidation
pub fn computePolyfillsHash() u64 {
    const polyfills = @import("../polyfills/polyfills.zig");
    return snapshot.hashPolyfills(&polyfills.all_sources);
}

// ============================================================================
// Socket Native Bindings
// ============================================================================

/// Create a new socket, returns socket ID
pub fn nativeSocketCreate(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const result = socket_host.create();
    if (result < 0) {
        return qjs.JS_ThrowInternalError(ctx, "Failed to create socket");
    }
    return qjs.JS_NewInt32(ctx, result);
}

/// Bind socket to port, args: socket_id, port
pub fn nativeSocketBind(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "socket_bind requires socket_id and port");
    var socket_id: i32 = 0;
    var port: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);
    _ = qjs.JS_ToInt32(ctx, &port, argv[1]);
    const result = socket_host.bind(@intCast(socket_id), @intCast(port));
    return qjs.JS_NewInt32(ctx, result);
}

/// Start listening, args: socket_id, backlog
pub fn nativeSocketListen(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "socket_listen requires socket_id and backlog");
    var socket_id: i32 = 0;
    var backlog: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);
    _ = qjs.JS_ToInt32(ctx, &backlog, argv[1]);
    const result = socket_host.listen(@intCast(socket_id), @intCast(backlog));
    return qjs.JS_NewInt32(ctx, result);
}

/// Accept connection, args: socket_id, returns new socket ID or 0 if no connection
pub fn nativeSocketAccept(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "socket_accept requires socket_id");
    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);
    const result = socket_host.accept(@intCast(socket_id));
    return qjs.JS_NewInt32(ctx, result);
}

/// Connect to port, args: socket_id, port
pub fn nativeSocketConnect(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "socket_connect requires socket_id and port");
    var socket_id: i32 = 0;
    var port: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);
    _ = qjs.JS_ToInt32(ctx, &port, argv[1]);
    const result = socket_host.connect(@intCast(socket_id), @intCast(port));
    return qjs.JS_NewInt32(ctx, result);
}

/// Write data to socket, args: socket_id, data (string)
pub fn nativeSocketWrite(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "socket_write requires socket_id and data");
    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const data = getStringArg(ctx, argv[1]) orelse
        return qjs.JS_ThrowTypeError(ctx, "data must be a string");
    defer freeStringArg(ctx, data);

    const result = socket_host.write(@intCast(socket_id), data.ptr, @intCast(data.len));
    return qjs.JS_NewInt32(ctx, result);
}

/// Read data from socket, args: socket_id, max_len, returns string or null for EOF
pub fn nativeSocketRead(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "socket_read requires socket_id and max_len");
    var socket_id: i32 = 0;
    var max_len: i32 = 65536;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);
    _ = qjs.JS_ToInt32(ctx, &max_len, argv[1]);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // First call read to get data into host buffer
    const read_result = socket_host.read(@intCast(socket_id), @intCast(max_len));
    if (read_result < 0) {
        if (read_result == -4) {
            // EOF - return null
            return quickjs.jsNull();
        }
        return qjs.JS_ThrowInternalError(ctx, "socket read failed");
    }
    if (read_result == 0) {
        // No data available
        return qjs.JS_NewString(ctx, "");
    }

    // Allocate buffer and get data
    const buf = allocator.alloc(u8, @intCast(read_result)) catch
        return qjs.JS_ThrowInternalError(ctx, "out of memory");
    defer allocator.free(buf);

    const copied = socket_host.get_read_data(@intCast(socket_id), buf.ptr);
    if (copied < 0) {
        return qjs.JS_ThrowInternalError(ctx, "failed to get read data");
    }

    return qjs.JS_NewStringLen(ctx, buf.ptr, @intCast(copied));
}

/// Close socket, args: socket_id
pub fn nativeSocketClose(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "socket_close requires socket_id");
    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);
    const result = socket_host.close(@intCast(socket_id));
    return qjs.JS_NewInt32(ctx, result);
}

/// Get socket state, args: socket_id, returns state enum (0=created, 1=bound, 2=listening, 3=connected, 4=closed)
pub fn nativeSocketState(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "socket_state requires socket_id");
    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);
    const result = socket_host.state(@intCast(socket_id));
    return qjs.JS_NewInt32(ctx, result);
}

// ============================================================================
// SIMD-Accelerated JSON Functions
// ============================================================================

/// Native JSON parse: __edgebox_json_parse(str) -> JSValue
/// Uses SIMD-accelerated parsing for ~2-3x speedup over QuickJS JSON.parse
pub fn nativeJsonParse(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "JSON.parse requires a string argument");

    const str = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "argument must be a string");
    defer freeStringArg(ctx, str);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    var parsed = simd_json.parse(allocator, str) catch |err| {
        return switch (err) {
            simd_json.ParseError.UnexpectedToken => qjs.JS_ThrowSyntaxError(ctx, "Unexpected token in JSON"),
            simd_json.ParseError.UnexpectedEndOfInput => qjs.JS_ThrowSyntaxError(ctx, "Unexpected end of JSON input"),
            simd_json.ParseError.InvalidNumber => qjs.JS_ThrowSyntaxError(ctx, "Invalid number in JSON"),
            simd_json.ParseError.InvalidString, simd_json.ParseError.UnterminatedString => qjs.JS_ThrowSyntaxError(ctx, "Invalid string in JSON"),
            simd_json.ParseError.InvalidEscape, simd_json.ParseError.InvalidUnicode => qjs.JS_ThrowSyntaxError(ctx, "Invalid escape sequence in JSON"),
            simd_json.ParseError.TrailingData => qjs.JS_ThrowSyntaxError(ctx, "Unexpected data after JSON"),
            simd_json.ParseError.MaxDepthExceeded => qjs.JS_ThrowRangeError(ctx, "JSON nesting too deep"),
            simd_json.ParseError.OutOfMemory => qjs.JS_ThrowInternalError(ctx, "Out of memory"),
        };
    };
    defer parsed.deinit(allocator);

    // Convert simd_json.Value to JSValue
    return valueToJs(ctx, &parsed, allocator);
}

/// Convert simd_json.Value to QuickJS JSValue
fn valueToJs(ctx: ?*qjs.JSContext, value: *const simd_json.Value, allocator: std.mem.Allocator) qjs.JSValue {
    return switch (value.*) {
        .null_value => quickjs.jsNull(),
        .bool_value => |b| if (b) quickjs.jsTrue() else quickjs.jsFalse(),
        .number_int => |n| qjs.JS_NewInt64(ctx, n),
        .number_float => |f| qjs.JS_NewFloat64(ctx, f),
        .string => |s| qjs.JS_NewStringLen(ctx, s.ptr, s.len),
        .array => |arr| blk: {
            const js_arr = qjs.JS_NewArray(ctx);
            if (qjs.JS_IsException(js_arr)) break :blk js_arr;
            for (arr.items, 0..) |*item, i| {
                const js_item = valueToJs(ctx, item, allocator);
                _ = qjs.JS_SetPropertyUint32(ctx, js_arr, @intCast(i), js_item);
            }
            break :blk js_arr;
        },
        .object => |obj| blk: {
            const js_obj = qjs.JS_NewObject(ctx);
            if (qjs.JS_IsException(js_obj)) break :blk js_obj;
            var it = obj.iterator();
            while (it.next()) |entry| {
                const js_val = valueToJs(ctx, entry.value_ptr, allocator);
                var key_buf: [512:0]u8 = undefined;
                if (entry.key_ptr.len < key_buf.len) {
                    @memcpy(key_buf[0..entry.key_ptr.len], entry.key_ptr.*);
                    key_buf[entry.key_ptr.len] = 0;
                    _ = qjs.JS_SetPropertyStr(ctx, js_obj, @ptrCast(&key_buf), js_val);
                }
            }
            break :blk js_obj;
        },
    };
}

/// Native JSON stringify: __edgebox_json_stringify(val) -> string
pub fn nativeJsonStringify(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "JSON.stringify requires an argument");

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    var value = jsToValue(ctx, argv[0], allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => qjs.JS_ThrowInternalError(ctx, "Out of memory"),
            error.CircularReference => qjs.JS_ThrowTypeError(ctx, "Converting circular structure to JSON"),
            error.UnsupportedType => quickjs.jsUndefined(),
        };
    };
    defer value.deinit(allocator);

    const result = simd_json.stringify(allocator, value) catch
        return qjs.JS_ThrowInternalError(ctx, "Out of memory");
    defer allocator.free(result);

    return qjs.JS_NewStringLen(ctx, result.ptr, result.len);
}

const JsConvertError = error{
    OutOfMemory,
    CircularReference,
    UnsupportedType,
};

/// Convert QuickJS JSValue to simd_json.Value
fn jsToValue(ctx: ?*qjs.JSContext, val: qjs.JSValue, allocator: std.mem.Allocator) JsConvertError!simd_json.Value {
    if (qjs.JS_IsNull(val)) return .null_value;
    if (qjs.JS_IsUndefined(val)) return JsConvertError.UnsupportedType;
    if (qjs.JS_IsBool(val)) return .{ .bool_value = qjs.JS_ToBool(ctx, val) != 0 };
    if (qjs.JS_IsNumber(val)) {
        var int_val: i64 = 0;
        if (qjs.JS_ToInt64(ctx, &int_val, val) == 0) {
            var float_val: f64 = 0;
            _ = qjs.JS_ToFloat64(ctx, &float_val, val);
            if (@as(f64, @floatFromInt(int_val)) == float_val) {
                return .{ .number_int = int_val };
            }
            return .{ .number_float = float_val };
        }
        var float_val: f64 = 0;
        _ = qjs.JS_ToFloat64(ctx, &float_val, val);
        return .{ .number_float = float_val };
    }
    if (qjs.JS_IsString(val)) {
        var len: usize = 0;
        const ptr = JS_ToCStringLen2(ctx, &len, val, false);
        if (ptr == null) return JsConvertError.OutOfMemory;
        defer qjs.JS_FreeCString(ctx, ptr);
        const str = allocator.dupe(u8, ptr.?[0..len]) catch return JsConvertError.OutOfMemory;
        return .{ .string = str };
    }
    if (qjs.JS_IsArray(val)) {
        var arr = std.ArrayListUnmanaged(simd_json.Value){};
        errdefer {
            for (arr.items) |*item| item.deinit(allocator);
            arr.deinit(allocator);
        }
        const len_val = qjs.JS_GetPropertyStr(ctx, val, "length");
        defer qjs.JS_FreeValue(ctx, len_val);
        var len: i64 = 0;
        _ = qjs.JS_ToInt64(ctx, &len, len_val);
        for (0..@intCast(len)) |i| {
            const elem = qjs.JS_GetPropertyUint32(ctx, val, @intCast(i));
            defer qjs.JS_FreeValue(ctx, elem);
            const elem_val = jsToValue(ctx, elem, allocator) catch |err| {
                if (err == JsConvertError.UnsupportedType) continue;
                return err;
            };
            arr.append(allocator, elem_val) catch return JsConvertError.OutOfMemory;
        }
        return .{ .array = arr };
    }
    if (qjs.JS_IsObject(val)) {
        if (qjs.JS_IsFunction(ctx, val)) return JsConvertError.UnsupportedType;
        var obj = std.StringHashMap(simd_json.Value).init(allocator);
        errdefer {
            var it = obj.iterator();
            while (it.next()) |entry| {
                allocator.free(entry.key_ptr.*);
                entry.value_ptr.deinit(allocator);
            }
            obj.deinit();
        }
        var ptab: [*c]qjs.JSPropertyEnum = undefined;
        var plen: u32 = 0;
        if (qjs.JS_GetOwnPropertyNames(ctx, &ptab, &plen, val, qjs.JS_GPN_STRING_MASK | qjs.JS_GPN_ENUM_ONLY) != 0) {
            return .{ .object = obj };
        }
        defer {
            for (0..plen) |i| qjs.JS_FreeAtom(ctx, ptab[i].atom);
            qjs.js_free(ctx, ptab);
        }
        for (0..plen) |i| {
            const atom = ptab[i].atom;
            const key_val = qjs.JS_AtomToString(ctx, atom);
            defer qjs.JS_FreeValue(ctx, key_val);
            var key_len: usize = 0;
            const key_ptr = JS_ToCStringLen2(ctx, &key_len, key_val, false) orelse continue;
            defer qjs.JS_FreeCString(ctx, key_ptr);
            const prop_val = qjs.JS_GetProperty(ctx, val, atom);
            defer qjs.JS_FreeValue(ctx, prop_val);
            const converted = jsToValue(ctx, prop_val, allocator) catch |err| {
                if (err == JsConvertError.UnsupportedType) continue;
                return err;
            };
            const key = allocator.dupe(u8, key_ptr[0..key_len]) catch return JsConvertError.OutOfMemory;
            obj.put(key, converted) catch {
                allocator.free(key);
                return JsConvertError.OutOfMemory;
            };
        }
        return .{ .object = obj };
    }
    return JsConvertError.UnsupportedType;
}

// ============================================================================
// BigInt Native Functions
// ============================================================================

pub fn nativeBigIntFromString(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "BigInt requires a value");
    const allocator = global_allocator orelse return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");
    const str = getStringArg(ctx, argv[0]) orelse return qjs.JS_ThrowTypeError(ctx, "argument must be a string");
    defer freeStringArg(ctx, str);
    var base: u8 = 10;
    if (argc > 1) {
        var base_int: i32 = 10;
        _ = qjs.JS_ToInt32(ctx, &base_int, argv[1]);
        if (base_int < 2 or base_int > 36) return qjs.JS_ThrowRangeError(ctx, "base must be between 2 and 36");
        base = @intCast(base_int);
    }
    const handle_ptr = allocator.create(bigint.BigIntHandle) catch return qjs.JS_ThrowInternalError(ctx, "out of memory");
    handle_ptr.* = bigint.BigIntHandle.fromString(allocator, str, base) catch {
        allocator.destroy(handle_ptr);
        return qjs.JS_ThrowSyntaxError(ctx, "Cannot convert string to BigInt");
    };
    const handle_id = next_bigint_id;
    next_bigint_id += 1;
    const handles = getBigIntHandles();
    handles.put(handle_id, handle_ptr) catch {
        handle_ptr.deinit();
        allocator.destroy(handle_ptr);
        return qjs.JS_ThrowInternalError(ctx, "out of memory");
    };
    return qjs.JS_NewInt32(ctx, @intCast(handle_id));
}

pub fn nativeBigIntFromNumber(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "BigInt requires a value");
    const allocator = global_allocator orelse return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");
    const handle_ptr = allocator.create(bigint.BigIntHandle) catch return qjs.JS_ThrowInternalError(ctx, "out of memory");
    var int_val: i64 = 0;
    if (qjs.JS_ToInt64(ctx, &int_val, argv[0]) == 0) {
        handle_ptr.* = bigint.BigIntHandle.fromInt(allocator, int_val) catch {
            allocator.destroy(handle_ptr);
            return qjs.JS_ThrowInternalError(ctx, "BigInt creation failed");
        };
    } else {
        var float_val: f64 = 0;
        _ = qjs.JS_ToFloat64(ctx, &float_val, argv[0]);
        handle_ptr.* = bigint.BigIntHandle.fromFloat(allocator, float_val) catch {
            allocator.destroy(handle_ptr);
            return qjs.JS_ThrowRangeError(ctx, "Cannot convert to BigInt");
        };
    }
    const handle_id = next_bigint_id;
    next_bigint_id += 1;
    const handles = getBigIntHandles();
    handles.put(handle_id, handle_ptr) catch {
        handle_ptr.deinit();
        allocator.destroy(handle_ptr);
        return qjs.JS_ThrowInternalError(ctx, "out of memory");
    };
    return qjs.JS_NewInt32(ctx, @intCast(handle_id));
}

pub fn nativeBigIntToString(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "toString requires handle_id");
    const allocator = global_allocator orelse return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");
    var handle_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &handle_id, argv[0]);
    const handles = getBigIntHandles();
    const handle = handles.get(@intCast(handle_id)) orelse return qjs.JS_ThrowReferenceError(ctx, "invalid BigInt handle");
    var base: u8 = 10;
    if (argc > 1) {
        var base_int: i32 = 10;
        _ = qjs.JS_ToInt32(ctx, &base_int, argv[1]);
        if (base_int < 2 or base_int > 36) return qjs.JS_ThrowRangeError(ctx, "base must be between 2 and 36");
        base = @intCast(base_int);
    }
    const str = handle.toString(base) catch return qjs.JS_ThrowInternalError(ctx, "toString failed");
    defer allocator.free(str);
    return qjs.JS_NewStringLen(ctx, str.ptr, str.len);
}

fn bigintBinaryOp(ctx: ?*qjs.JSContext, argc: c_int, argv: [*c]qjs.JSValue, comptime op: []const u8) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "operation requires two BigInt handles");
    const allocator = global_allocator orelse return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");
    var a_id: i32 = 0;
    var b_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &a_id, argv[0]);
    _ = qjs.JS_ToInt32(ctx, &b_id, argv[1]);
    const handles = getBigIntHandles();
    const a = handles.get(@intCast(a_id)) orelse return qjs.JS_ThrowReferenceError(ctx, "invalid BigInt handle (a)");
    const b = handles.get(@intCast(b_id)) orelse return qjs.JS_ThrowReferenceError(ctx, "invalid BigInt handle (b)");
    const handle_ptr = allocator.create(bigint.BigIntHandle) catch return qjs.JS_ThrowInternalError(ctx, "out of memory");
    const result = if (comptime std.mem.eql(u8, op, "add")) a.add(b)
        else if (comptime std.mem.eql(u8, op, "sub")) a.sub(b)
        else if (comptime std.mem.eql(u8, op, "mul")) a.mul(b)
        else if (comptime std.mem.eql(u8, op, "div")) a.div(b)
        else if (comptime std.mem.eql(u8, op, "mod")) a.mod(b)
        else @compileError("unknown operation");
    handle_ptr.* = result catch |err| {
        allocator.destroy(handle_ptr);
        if (comptime (std.mem.eql(u8, op, "div") or std.mem.eql(u8, op, "mod"))) {
            if (err == error.DivisionByZero) return qjs.JS_ThrowRangeError(ctx, "Division by zero");
        }
        return qjs.JS_ThrowInternalError(ctx, "BigInt operation failed");
    };
    const handle_id = next_bigint_id;
    next_bigint_id += 1;
    handles.put(handle_id, handle_ptr) catch {
        handle_ptr.deinit();
        allocator.destroy(handle_ptr);
        return qjs.JS_ThrowInternalError(ctx, "out of memory");
    };
    return qjs.JS_NewInt32(ctx, @intCast(handle_id));
}

pub fn nativeBigIntAdd(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return bigintBinaryOp(ctx, argc, argv, "add");
}
pub fn nativeBigIntSub(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return bigintBinaryOp(ctx, argc, argv, "sub");
}
pub fn nativeBigIntMul(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return bigintBinaryOp(ctx, argc, argv, "mul");
}
pub fn nativeBigIntDiv(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return bigintBinaryOp(ctx, argc, argv, "div");
}
pub fn nativeBigIntMod(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    return bigintBinaryOp(ctx, argc, argv, "mod");
}
pub fn nativeBigIntCmp(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "compare requires two BigInt handles");
    var a_id: i32 = 0;
    var b_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &a_id, argv[0]);
    _ = qjs.JS_ToInt32(ctx, &b_id, argv[1]);
    const handles = getBigIntHandles();
    const a = handles.get(@intCast(a_id)) orelse return qjs.JS_ThrowReferenceError(ctx, "invalid BigInt handle (a)");
    const b = handles.get(@intCast(b_id)) orelse return qjs.JS_ThrowReferenceError(ctx, "invalid BigInt handle (b)");
    return qjs.JS_NewInt32(ctx, a.compare(b));
}
pub fn nativeBigIntFree(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    _ = ctx;
    if (argc < 1) return quickjs.jsUndefined();
    const allocator = global_allocator orelse return quickjs.jsUndefined();
    var handle_id: i32 = 0;
    _ = qjs.JS_ToInt32(null, &handle_id, argv[0]);
    const handles = getBigIntHandles();
    if (handles.fetchRemove(@intCast(handle_id))) |kv| {
        kv.value.deinit();
        allocator.destroy(kv.value);
    }
    return quickjs.jsUndefined();
}

// ============================================================================
// WebSocket Native Functions
// ============================================================================

pub fn nativeWsConnect(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewInt32(ctx, -1);
    var url_len: usize = 0;
    const url_ptr = qjs.JS_ToCStringLen(ctx, &url_len, argv[0]) orelse return qjs.JS_NewInt32(ctx, -1);
    defer qjs.JS_FreeCString(ctx, url_ptr);
    const id = allocWsId() orelse return qjs.JS_NewInt32(ctx, -1);
    const client = websocket.WebSocketClient.init(url_ptr[0..url_len]) catch return qjs.JS_NewInt32(ctx, -1);
    client.connect() catch { client.deinit(); return qjs.JS_NewInt32(ctx, -1); };
    ws_connections[id] = client;
    return qjs.JS_NewInt32(ctx, @intCast(id));
}

pub fn nativeWsSend(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_NewInt32(ctx, -1);
    const id = qjs.JS_VALUE_GET_INT(argv[0]);
    if (id < 0 or id >= 64) return qjs.JS_NewInt32(ctx, -1);
    const client = ws_connections[@intCast(id)] orelse return qjs.JS_NewInt32(ctx, -1);
    var data_len: usize = 0;
    const data_ptr = qjs.JS_ToCStringLen(ctx, &data_len, argv[1]) orelse return qjs.JS_NewInt32(ctx, -1);
    defer qjs.JS_FreeCString(ctx, data_ptr);
    client.sendText(data_ptr[0..data_len]) catch return qjs.JS_NewInt32(ctx, -1);
    return qjs.JS_NewInt32(ctx, 0);
}

pub fn nativeWsSendBinary(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_NewInt32(ctx, -1);
    const id = qjs.JS_VALUE_GET_INT(argv[0]);
    if (id < 0 or id >= 64) return qjs.JS_NewInt32(ctx, -1);
    const client = ws_connections[@intCast(id)] orelse return qjs.JS_NewInt32(ctx, -1);
    var data_len: usize = 0;
    const data_ptr = qjs.JS_ToCStringLen(ctx, &data_len, argv[1]) orelse return qjs.JS_NewInt32(ctx, -1);
    defer qjs.JS_FreeCString(ctx, data_ptr);
    client.sendBinary(data_ptr[0..data_len]) catch return qjs.JS_NewInt32(ctx, -1);
    return qjs.JS_NewInt32(ctx, 0);
}

pub fn nativeWsRecv(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsNull();
    const id = qjs.JS_VALUE_GET_INT(argv[0]);
    if (id < 0 or id >= 64) return quickjs.jsNull();
    const client = ws_connections[@intCast(id)] orelse return quickjs.jsNull();
    var msg = client.recv() catch return quickjs.jsNull();
    defer msg.deinit();
    const obj = qjs.JS_NewObject(ctx);
    if (qjs.JS_IsException(obj)) return quickjs.jsNull();
    const data_val = qjs.JS_NewStringLen(ctx, msg.data.ptr, msg.data.len);
    _ = qjs.JS_SetPropertyStr(ctx, obj, "data", data_val);
    _ = qjs.JS_SetPropertyStr(ctx, obj, "binary", if (msg.is_binary) quickjs.jsTrue() else quickjs.jsFalse());
    return obj;
}

pub fn nativeWsClose(_: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return quickjs.jsUndefined();
    const id = qjs.JS_VALUE_GET_INT(argv[0]);
    if (id < 0 or id >= 64) return quickjs.jsUndefined();
    if (ws_connections[@intCast(id)]) |client| { client.deinit(); ws_connections[@intCast(id)] = null; }
    return quickjs.jsUndefined();
}

pub fn nativeWsState(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewInt32(ctx, 3);
    const id = qjs.JS_VALUE_GET_INT(argv[0]);
    if (id < 0 or id >= 64) return qjs.JS_NewInt32(ctx, 3);
    const client = ws_connections[@intCast(id)] orelse return qjs.JS_NewInt32(ctx, 3);
    return qjs.JS_NewInt32(ctx, @intFromEnum(client.state));
}

pub const nativeWsSendText = nativeWsSend;

pub fn nativeWsPing(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_NewInt32(ctx, -1);
    const id = qjs.JS_VALUE_GET_INT(argv[0]);
    if (id < 0 or id >= 64) return qjs.JS_NewInt32(ctx, -1);
    const client = ws_connections[@intCast(id)] orelse return qjs.JS_NewInt32(ctx, -1);
    client.ping() catch return qjs.JS_NewInt32(ctx, -1);
    return qjs.JS_NewInt32(ctx, 0);
}

// ============================================================================
// Optimized zlib Native Functions
// ============================================================================

/// Compress data to gzip format
pub fn nativeGzip(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "gzip requires data argument");

    const data = getBinaryArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "data must be a string or buffer");
    defer freeBinaryArg(ctx, data, argv[0]);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    const compressed = wasm_zlib.gzip(allocator, data) catch |err| {
        return switch (err) {
            wasm_zlib.ZlibError.OutOfMemory => qjs.JS_ThrowInternalError(ctx, "out of memory"),
            wasm_zlib.ZlibError.CompressionFailed => qjs.JS_ThrowInternalError(ctx, "gzip compression failed"),
            else => qjs.JS_ThrowInternalError(ctx, "gzip error"),
        };
    };
    defer allocator.free(compressed);

    return qjs.JS_NewStringLen(ctx, compressed.ptr, compressed.len);
}

/// Compress data using raw deflate
pub fn nativeDeflate(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "deflate requires data argument");

    const data = getBinaryArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "data must be a string or buffer");
    defer freeBinaryArg(ctx, data, argv[0]);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    const compressed = wasm_zlib.deflate(allocator, data) catch |err| {
        return switch (err) {
            wasm_zlib.ZlibError.OutOfMemory => qjs.JS_ThrowInternalError(ctx, "out of memory"),
            wasm_zlib.ZlibError.CompressionFailed => qjs.JS_ThrowInternalError(ctx, "deflate compression failed"),
            else => qjs.JS_ThrowInternalError(ctx, "deflate error"),
        };
    };
    defer allocator.free(compressed);

    return qjs.JS_NewStringLen(ctx, compressed.ptr, compressed.len);
}

/// Compress data using deflate with zlib header
pub fn nativeDeflateZlib(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "deflateZlib requires data argument");

    const data = getBinaryArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "data must be a string or buffer");
    defer freeBinaryArg(ctx, data, argv[0]);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    const compressed = wasm_zlib.deflateZlib(allocator, data) catch |err| {
        return switch (err) {
            wasm_zlib.ZlibError.OutOfMemory => qjs.JS_ThrowInternalError(ctx, "out of memory"),
            wasm_zlib.ZlibError.CompressionFailed => qjs.JS_ThrowInternalError(ctx, "zlib compression failed"),
            else => qjs.JS_ThrowInternalError(ctx, "deflateZlib error"),
        };
    };
    defer allocator.free(compressed);

    return qjs.JS_NewStringLen(ctx, compressed.ptr, compressed.len);
}
