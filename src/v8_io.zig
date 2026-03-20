// v8_io.zig — IO dispatcher for V8 embed path
//
// Handles __edgebox_io_sync callbacks from JavaScript.
// Same JSON protocol as io_server.zig but without the socket layer.
// JS calls: __edgebox_io_sync('{"op":"readFile","path":"/foo.ts"}')
// Returns:  '{"ok":true,"data":"file contents..."}'

const std = @import("std");
const v8 = @import("v8.zig");

const alloc = std.heap.page_allocator;

/// Register __edgebox_io_sync and __edgebox_io_batch as global functions.
pub fn registerGlobals(isolate: *v8.Isolate, context: *const v8.Context) void {
    const global = v8.ContextApi.global(context);

    // __edgebox_io_sync(jsonString) → jsonString
    const tmpl = v8.FunctionTemplateApi.create(isolate, &ioSyncCallback) orelse return;
    const func = v8.FunctionTemplateApi.getFunction(tmpl, context) orelse return;
    const key = v8.StringApi.fromUtf8(isolate, "__edgebox_io_sync") orelse return;
    _ = v8.ObjectApi.set(global, context, @ptrCast(key), @ptrCast(func));

    // __edgebox_io_batch(jsonArrayString) → jsonArrayString
    const batch_tmpl = v8.FunctionTemplateApi.create(isolate, &ioBatchCallback) orelse return;
    const batch_func = v8.FunctionTemplateApi.getFunction(batch_tmpl, context) orelse return;
    const batch_key = v8.StringApi.fromUtf8(isolate, "__edgebox_io_batch") orelse return;
    _ = v8.ObjectApi.set(global, context, @ptrCast(batch_key), @ptrCast(batch_func));
}

/// The V8 callback for __edgebox_io_sync(jsonString) → jsonString
/// The V8 callback for __edgebox_io_sync. Public so v8_runner.zig can use
/// its address in the snapshot external_references array.
pub fn ioSyncCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate: *v8.Isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    // Get first argument (JSON string)
    if (v8.CallbackInfoApi.length(info) < 1) {
        rv.setUndefined();
        return;
    }
    const arg0 = v8.CallbackInfoApi.get(info, 0) orelse {
        rv.setUndefined();
        return;
    };

    // Convert V8 string to Zig slice
    if (!v8.ValueApi.isString(arg0)) {
        rv.setUndefined();
        return;
    }
    const str: *const v8.String = @ptrCast(arg0);
    const len: usize = @intCast(v8.StringApi.utf8Length(str, isolate));
    if (len == 0 or len > 16 * 1024 * 1024) { // 16MB max
        rv.setUndefined();
        return;
    }

    // Use stack buffer for small requests, heap for large
    var heap_buf: ?[]u8 = null;
    defer if (heap_buf) |hb| alloc.free(hb);

    var stack_buf: [4096]u8 = undefined;
    const buf: []u8 = if (len + 1 <= stack_buf.len)
        &stack_buf
    else blk: {
        heap_buf = alloc.alloc(u8, len + 1) catch {
            rv.setUndefined();
            return;
        };
        break :blk heap_buf.?;
    };

    const written = v8.StringApi.writeUtf8(str, isolate, buf);
    const request_json = buf[0..written];

    // Dispatch the IO request
    const response = handleRequest(request_json) catch {
        const err_str = v8.StringApi.fromUtf8(isolate, "{\"ok\":false,\"error\":\"internal error\"}") orelse return;
        rv.set(@ptrCast(err_str));
        return;
    };
    defer alloc.free(response);

    // Return response as V8 string
    const result_str = v8.StringApi.fromUtf8(isolate, response) orelse {
        rv.setUndefined();
        return;
    };
    rv.set(@ptrCast(result_str));
}

// ============================================================
// IO Request Dispatcher
// ============================================================

const Request = struct {
    op: []const u8,
    path: ?[]const u8 = null,
    data: ?[]const u8 = null,
    encoding: ?[]const u8 = null,
    recursive: ?bool = null,
    code: ?i32 = null,
};

fn handleRequest(msg: []const u8) ![]const u8 {
    const parsed = std.json.parseFromSlice(Request, alloc, msg, .{ .ignore_unknown_fields = true }) catch {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"invalid JSON\"}}", .{});
    };
    defer parsed.deinit();
    const req = parsed.value;

    if (std.mem.eql(u8, req.op, "readFileSync") or std.mem.eql(u8, req.op, "readFile")) {
        return opReadFile(req);
    } else if (std.mem.eql(u8, req.op, "writeFileSync") or std.mem.eql(u8, req.op, "writeFile")) {
        return opWriteFile(req);
    } else if (std.mem.eql(u8, req.op, "statSync") or std.mem.eql(u8, req.op, "stat")) {
        return opStat(req);
    } else if (std.mem.eql(u8, req.op, "readdirSync") or std.mem.eql(u8, req.op, "readdir")) {
        return opReaddir(req);
    } else if (std.mem.eql(u8, req.op, "realpathSync") or std.mem.eql(u8, req.op, "realpath")) {
        return opRealpath(req);
    } else if (std.mem.eql(u8, req.op, "mkdirSync") or std.mem.eql(u8, req.op, "mkdir")) {
        return opMkdir(req);
    } else if (std.mem.eql(u8, req.op, "existsSync") or std.mem.eql(u8, req.op, "exists")) {
        return opExists(req);
    } else if (std.mem.eql(u8, req.op, "unlinkSync") or std.mem.eql(u8, req.op, "unlink")) {
        return opUnlink(req);
    } else if (std.mem.eql(u8, req.op, "cwd")) {
        return opCwd();
    } else if (std.mem.eql(u8, req.op, "writeStdout")) {
        return opWriteStdout(req);
    } else if (std.mem.eql(u8, req.op, "writeStderr")) {
        return opWriteStderr(req);
    } else if (std.mem.eql(u8, req.op, "exit")) {
        const code = req.code orelse 0;
        std.process.exit(@intCast(code));
    } else if (std.mem.eql(u8, req.op, "argv")) {
        return opArgv();
    } else if (std.mem.eql(u8, req.op, "env")) {
        return opEnv();
    }

    return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"unknown op: {s}\"}}", .{req.op});
}

// ============================================================
// IO Operations — direct filesystem access (no socket/IPC)
// ============================================================

fn opReadFile(req: Request) ![]const u8 {
    const path = req.path orelse return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"{s}\",\"code\":\"ENOENT\"}}", .{@errorName(err)});
    };
    defer file.close();

    const content = file.readToEndAlloc(alloc, 64 * 1024 * 1024) catch |err| {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"{s}\"}}", .{@errorName(err)});
    };
    defer alloc.free(content);

    // JSON-escape the content
    var result: std.ArrayListUnmanaged(u8) = .{};
    try result.appendSlice(alloc, "{\"ok\":true,\"data\":\"");
    for (content) |byte| {
        switch (byte) {
            '"' => try result.appendSlice(alloc, "\\\""),
            '\\' => try result.appendSlice(alloc, "\\\\"),
            '\n' => try result.appendSlice(alloc, "\\n"),
            '\r' => try result.appendSlice(alloc, "\\r"),
            '\t' => try result.appendSlice(alloc, "\\t"),
            else => {
                if (byte < 0x20) {
                    var hex_buf: [6]u8 = undefined;
                    const hex = std.fmt.bufPrint(&hex_buf, "\\u{x:0>4}", .{byte}) catch unreachable;
                    try result.appendSlice(alloc, hex);
                } else {
                    try result.append(alloc, byte);
                }
            },
        }
    }
    try result.appendSlice(alloc, "\"}");
    return try result.toOwnedSlice(alloc);
}

fn opWriteFile(req: Request) ![]const u8 {
    const path = req.path orelse return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"path required\"}}", .{});
    const data = req.data orelse return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"data required\"}}", .{});

    const file = std.fs.cwd().createFile(path, .{}) catch |err| {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"{s}\"}}", .{@errorName(err)});
    };
    defer file.close();
    file.writeAll(data) catch |err| {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"{s}\"}}", .{@errorName(err)});
    };

    return try std.fmt.allocPrint(alloc, "{{\"ok\":true}}", .{});
}

fn opStat(req: Request) ![]const u8 {
    const path = req.path orelse return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

    const stat = std.fs.cwd().statFile(path) catch |err| {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"{s}\",\"code\":\"ENOENT\"}}", .{@errorName(err)});
    };

    const is_dir = stat.kind == .directory;
    const is_file = stat.kind == .file;
    return try std.fmt.allocPrint(alloc, "{{\"ok\":true,\"isFile\":{},\"isDirectory\":{},\"size\":{d}}}", .{ is_file, is_dir, stat.size });
}

fn opReaddir(req: Request) ![]const u8 {
    const path = req.path orelse return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

    var dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch |err| {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"{s}\",\"code\":\"ENOENT\"}}", .{@errorName(err)});
    };
    defer dir.close();

    var result: std.ArrayListUnmanaged(u8) = .{};
    try result.appendSlice(alloc, "{\"ok\":true,\"entries\":[");

    var first = true;
    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (!first) try result.append(alloc, ',');
        first = false;
        const is_dir = entry.kind == .directory;
        const item = try std.fmt.allocPrint(alloc, "{{\"name\":\"{s}\",\"isDirectory\":{}}}", .{ entry.name, is_dir });
        defer alloc.free(item);
        try result.appendSlice(alloc, item);
    }

    try result.appendSlice(alloc, "]}");
    return try result.toOwnedSlice(alloc);
}

fn opRealpath(req: Request) ![]const u8 {
    const path = req.path orelse return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const real = std.fs.cwd().realpath(path, &buf) catch |err| {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"{s}\"}}", .{@errorName(err)});
    };

    return try std.fmt.allocPrint(alloc, "{{\"ok\":true,\"data\":\"{s}\"}}", .{real});
}

fn opMkdir(req: Request) ![]const u8 {
    const path = req.path orelse return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

    if (req.recursive orelse false) {
        std.fs.cwd().makePath(path) catch |err| {
            return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"{s}\"}}", .{@errorName(err)});
        };
    } else {
        std.fs.cwd().makeDir(path) catch |err| {
            return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"{s}\"}}", .{@errorName(err)});
        };
    }
    return try std.fmt.allocPrint(alloc, "{{\"ok\":true}}", .{});
}

fn opExists(req: Request) ![]const u8 {
    const path = req.path orelse return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

    _ = std.fs.cwd().statFile(path) catch {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":true,\"exists\":false}}", .{});
    };
    return try std.fmt.allocPrint(alloc, "{{\"ok\":true,\"exists\":true}}", .{});
}

fn opUnlink(req: Request) ![]const u8 {
    const path = req.path orelse return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

    std.fs.cwd().deleteFile(path) catch |err| {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"{s}\"}}", .{@errorName(err)});
    };
    return try std.fmt.allocPrint(alloc, "{{\"ok\":true}}", .{});
}

fn opCwd() ![]const u8 {
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = std.fs.cwd().realpath(".", &buf) catch {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"cwd failed\"}}", .{});
    };
    return try std.fmt.allocPrint(alloc, "{{\"ok\":true,\"data\":\"{s}\"}}", .{cwd});
}

fn opWriteStdout(req: Request) ![]const u8 {
    const data = req.data orelse return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"data required\"}}", .{});
    const stdout = std.fs.File.stdout();
    stdout.writeAll(data) catch {};
    return try std.fmt.allocPrint(alloc, "{{\"ok\":true}}", .{});
}

fn opWriteStderr(req: Request) ![]const u8 {
    const data = req.data orelse return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"data required\"}}", .{});
    const stderr = std.fs.File.stderr();
    stderr.writeAll(data) catch {};
    return try std.fmt.allocPrint(alloc, "{{\"ok\":true}}", .{});
}

fn opArgv() ![]const u8 {
    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();

    var result: std.ArrayListUnmanaged(u8) = .{};
    try result.appendSlice(alloc, "{\"ok\":true,\"data\":[");

    var first = true;
    while (args.next()) |arg| {
        if (!first) try result.append(alloc, ',');
        first = false;
        try result.append(alloc, '"');
        try result.appendSlice(alloc, arg);
        try result.append(alloc, '"');
    }

    try result.appendSlice(alloc, "]}");
    return try result.toOwnedSlice(alloc);
}

// ============================================================
// Batch IO — parallel execution of multiple IO requests
// ============================================================

/// V8 callback for __edgebox_io_batch(jsonArrayString).
/// Takes a JSON array of requests, executes them in parallel using OS threads,
/// returns a JSON array of results in the same order.
///
/// JS usage:
///   const results = JSON.parse(__edgebox_io_batch(JSON.stringify([
///     {op:'readFile', path:'a.ts'},
///     {op:'stat', path:'b.ts'},
///     {op:'readFile', path:'c.ts'},
///   ])));
pub fn ioBatchCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate: *v8.Isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    if (v8.CallbackInfoApi.length(info) < 1) {
        rv.setUndefined();
        return;
    }
    const arg0 = v8.CallbackInfoApi.get(info, 0) orelse {
        rv.setUndefined();
        return;
    };
    if (!v8.ValueApi.isString(arg0)) {
        rv.setUndefined();
        return;
    }
    const str: *const v8.String = @ptrCast(arg0);
    const len: usize = @intCast(v8.StringApi.utf8Length(str, isolate));
    if (len == 0 or len > 64 * 1024 * 1024) { // 64MB max for batch
        rv.setUndefined();
        return;
    }

    var heap_buf: ?[]u8 = null;
    defer if (heap_buf) |hb| alloc.free(hb);

    var stack_buf: [8192]u8 = undefined;
    const buf: []u8 = if (len + 1 <= stack_buf.len)
        &stack_buf
    else blk: {
        heap_buf = alloc.alloc(u8, len + 1) catch {
            rv.setUndefined();
            return;
        };
        break :blk heap_buf.?;
    };

    const written = v8.StringApi.writeUtf8(str, isolate, buf);
    const request_json = buf[0..written];

    const response = handleBatchRequest(request_json) catch {
        const err_str = v8.StringApi.fromUtf8(isolate, "[]") orelse return;
        rv.set(@ptrCast(err_str));
        return;
    };
    defer alloc.free(response);

    const result_str = v8.StringApi.fromUtf8(isolate, response) orelse {
        rv.setUndefined();
        return;
    };
    rv.set(@ptrCast(result_str));
}

const BatchItem = struct {
    op: []const u8,
    path: ?[]const u8 = null,
    data: ?[]const u8 = null,
    encoding: ?[]const u8 = null,
    recursive: ?bool = null,
};

fn handleBatchRequest(msg: []const u8) ![]const u8 {
    const parsed = std.json.parseFromSlice([]const BatchItem, alloc, msg, .{
        .ignore_unknown_fields = true,
    }) catch {
        return try std.fmt.allocPrint(alloc, "[]", .{});
    };
    defer parsed.deinit();
    const items = parsed.value;

    if (items.len == 0) return try std.fmt.allocPrint(alloc, "[]", .{});
    if (items.len == 1) {
        // Single item — no threading overhead, just dispatch directly
        const req = Request{
            .op = items[0].op,
            .path = items[0].path,
            .data = items[0].data,
            .encoding = items[0].encoding,
            .recursive = items[0].recursive,
            .code = null,
        };
        const r = try handleRequest_noExit(req);
        defer alloc.free(r);
        return try std.fmt.allocPrint(alloc, "[{s}]", .{r});
    }

    // Parallel execution: spawn one thread per request (capped at 32)
    const max_threads = 32;
    const n = @min(items.len, max_threads);

    const results = alloc.alloc([]const u8, items.len) catch {
        return try std.fmt.allocPrint(alloc, "[]", .{});
    };
    defer alloc.free(results);
    for (results) |*r| r.* = "";

    // If batch is small enough, one thread per item; otherwise chunk
    if (items.len <= max_threads) {
        var threads: [max_threads]?std.Thread = undefined;
        var ctxs: [max_threads]BatchWorkerCtx = undefined;

        for (0..n) |i| {
            ctxs[i] = .{ .items = items, .results = results, .start = i, .end = i + 1 };
            threads[i] = std.Thread.spawn(.{}, batchWorker, .{&ctxs[i]}) catch null;
        }
        for (0..n) |i| {
            if (threads[i]) |t| t.join();
        }
    } else {
        // Chunk: distribute items across max_threads workers
        var threads: [max_threads]?std.Thread = undefined;
        var ctxs: [max_threads]BatchWorkerCtx = undefined;
        const chunk = (items.len + max_threads - 1) / max_threads;

        for (0..max_threads) |i| {
            const start = i * chunk;
            const end = @min(start + chunk, items.len);
            if (start >= end) {
                threads[i] = null;
                continue;
            }
            ctxs[i] = .{ .items = items, .results = results, .start = start, .end = end };
            threads[i] = std.Thread.spawn(.{}, batchWorker, .{&ctxs[i]}) catch null;
        }
        for (0..max_threads) |i| {
            if (threads[i]) |t| t.join();
        }
    }

    // Build JSON array response
    var out: std.ArrayListUnmanaged(u8) = .{};
    try out.append(alloc, '[');
    for (results, 0..) |r, i| {
        if (i > 0) try out.append(alloc, ',');
        if (r.len > 0) {
            try out.appendSlice(alloc, r);
            alloc.free(r);
        } else {
            try out.appendSlice(alloc, "{\"ok\":false,\"error\":\"thread failed\"}");
        }
    }
    try out.append(alloc, ']');
    return try out.toOwnedSlice(alloc);
}

fn batchWorker(ctx: *const BatchWorkerCtx) void {
    for (ctx.start..ctx.end) |i| {
        const item = ctx.items[i];
        const req = Request{
            .op = item.op,
            .path = item.path,
            .data = item.data,
            .encoding = item.encoding,
            .recursive = item.recursive,
            .code = null,
        };
        ctx.results[i] = handleRequest_noExit(req) catch
            std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"internal\"}}", .{}) catch "";
    }
}

const BatchWorkerCtx = struct {
    items: []const BatchItem,
    results: [][]const u8,
    start: usize,
    end: usize,
};

/// Like handleRequest but never calls process.exit (safe for batch threads).
fn handleRequest_noExit(req: Request) ![]const u8 {
    if (std.mem.eql(u8, req.op, "readFileSync") or std.mem.eql(u8, req.op, "readFile")) {
        return opReadFile(req);
    } else if (std.mem.eql(u8, req.op, "writeFileSync") or std.mem.eql(u8, req.op, "writeFile")) {
        return opWriteFile(req);
    } else if (std.mem.eql(u8, req.op, "statSync") or std.mem.eql(u8, req.op, "stat")) {
        return opStat(req);
    } else if (std.mem.eql(u8, req.op, "readdirSync") or std.mem.eql(u8, req.op, "readdir")) {
        return opReaddir(req);
    } else if (std.mem.eql(u8, req.op, "realpathSync") or std.mem.eql(u8, req.op, "realpath")) {
        return opRealpath(req);
    } else if (std.mem.eql(u8, req.op, "mkdirSync") or std.mem.eql(u8, req.op, "mkdir")) {
        return opMkdir(req);
    } else if (std.mem.eql(u8, req.op, "existsSync") or std.mem.eql(u8, req.op, "exists")) {
        return opExists(req);
    } else if (std.mem.eql(u8, req.op, "unlinkSync") or std.mem.eql(u8, req.op, "unlink")) {
        return opUnlink(req);
    } else if (std.mem.eql(u8, req.op, "cwd")) {
        return opCwd();
    } else if (std.mem.eql(u8, req.op, "argv")) {
        return opArgv();
    } else if (std.mem.eql(u8, req.op, "env")) {
        return opEnv();
    }
    return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"unknown op: {s}\"}}", .{req.op});
}

fn opEnv() ![]const u8 {
    var env_map = std.process.getEnvMap(alloc) catch {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":true,\"data\":{{}}}}", .{});
    };
    defer env_map.deinit();

    var result: std.ArrayListUnmanaged(u8) = .{};
    try result.appendSlice(alloc, "{\"ok\":true,\"data\":{");

    var first = true;
    var iter = env_map.iterator();
    while (iter.next()) |entry| {
        if (!first) try result.append(alloc, ',');
        first = false;
        try result.append(alloc, '"');
        for (entry.key_ptr.*) |byte| {
            if (byte == '"') {
                try result.appendSlice(alloc, "\\\"");
            } else if (byte == '\\') {
                try result.appendSlice(alloc, "\\\\");
            } else {
                try result.append(alloc, byte);
            }
        }
        try result.appendSlice(alloc, "\":\"");
        for (entry.value_ptr.*) |byte| {
            if (byte == '"') {
                try result.appendSlice(alloc, "\\\"");
            } else if (byte == '\\') {
                try result.appendSlice(alloc, "\\\\");
            } else if (byte == '\n') {
                try result.appendSlice(alloc, "\\n");
            } else {
                try result.append(alloc, byte);
            }
        }
        try result.append(alloc, '"');
    }

    try result.appendSlice(alloc, "}}");
    return try result.toOwnedSlice(alloc);
}
