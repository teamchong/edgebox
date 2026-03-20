// v8_io.zig — IO dispatcher for V8 embed path
//
// Handles __edgebox_io_sync callbacks from JavaScript.
// Same JSON protocol as io_server.zig but without the socket layer.
// JS calls: __edgebox_io_sync('{"op":"readFile","path":"/foo.ts"}')
// Returns:  '{"ok":true,"data":"file contents..."}'

const std = @import("std");
const v8 = @import("v8.zig");

const alloc = std.heap.page_allocator;

/// Deferred exit code — set by process.exit(), checked by runner after execution.
/// Allows code cache to be saved before termination.
pub var deferred_exit_code: ?u8 = null;

/// Register __edgebox_io_sync, __edgebox_io_batch, and fast-path callbacks.
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

    // __edgebox_read_file(path) → string|undefined (fast path, no JSON)
    const rf_tmpl = v8.FunctionTemplateApi.create(isolate, &readFileFastCallback) orelse return;
    const rf_func = v8.FunctionTemplateApi.getFunction(rf_tmpl, context) orelse return;
    const rf_key = v8.StringApi.fromUtf8(isolate, "__edgebox_read_file") orelse return;
    _ = v8.ObjectApi.set(global, context, @ptrCast(rf_key), @ptrCast(rf_func));

    // __edgebox_file_exists(path) → boolean (fast path, no JSON)
    const fe_tmpl = v8.FunctionTemplateApi.create(isolate, &fileExistsFastCallback) orelse return;
    const fe_func = v8.FunctionTemplateApi.getFunction(fe_tmpl, context) orelse return;
    const fe_key = v8.StringApi.fromUtf8(isolate, "__edgebox_file_exists") orelse return;
    _ = v8.ObjectApi.set(global, context, @ptrCast(fe_key), @ptrCast(fe_func));
}

/// Fast readFile: path → string (no JSON serialize/parse overhead)
/// Raw file content cache (no JSON escaping — for fast callback)
var raw_file_cache: std.StringHashMapUnmanaged([]const u8) = .{};

pub fn readFileFastCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    if (v8.global_platform) |platform| {
        while (v8.pumpMessageLoop(platform, isolate)) {}
    }

    if (v8.CallbackInfoApi.length(info) < 1) { rv.setUndefined(); return; }
    const arg0 = v8.CallbackInfoApi.get(info, 0) orelse { rv.setUndefined(); return; };
    if (!v8.ValueApi.isString(arg0)) { rv.setUndefined(); return; }

    const str: *const v8.String = @ptrCast(arg0);
    const len: usize = @intCast(v8.StringApi.utf8Length(str, isolate));
    if (len == 0 or len > 4096) { rv.setUndefined(); return; }

    var path_buf: [4096]u8 = undefined;
    const written = v8.StringApi.writeUtf8(str, isolate, &path_buf);
    const path = path_buf[0..written];

    // Check raw content cache first
    const cached_content = raw_file_cache.get(path);
    const content = cached_content orelse blk: {
        const file = std.fs.cwd().openFile(path, .{}) catch { rv.setUndefined(); return; };
        defer file.close();
        const data = file.readToEndAlloc(alloc, 64 * 1024 * 1024) catch { rv.setUndefined(); return; };
        // Cache the raw content
        const key = alloc.dupe(u8, path) catch path;
        raw_file_cache.put(alloc, key, data) catch {};
        break :blk data;
    };

    // Create V8 string directly — no JSON escaping needed!
    // Use external string (zero-copy) for all files > 128 bytes.
    // fromExternalOneByte is zero-copy: V8 reads directly from Zig's buffer.
    // fromUtf8 copies the string (only for tiny files where copy is free).
    const v8_str = if (content.len > 128)
        v8.StringApi.fromExternalOneByte(isolate, content)
    else
        v8.StringApi.fromUtf8(isolate, content);

    if (v8_str) |s| {
        rv.set(@ptrCast(s));
    } else {
        rv.setUndefined();
    }
}

/// Fast fileExists: path → boolean (no JSON)
pub fn fileExistsFastCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    if (v8.global_platform) |platform| {
        while (v8.pumpMessageLoop(platform, isolate)) {}
    }

    if (v8.CallbackInfoApi.length(info) < 1) { rv.setUndefined(); return; }
    const arg0 = v8.CallbackInfoApi.get(info, 0) orelse { rv.setUndefined(); return; };
    if (!v8.ValueApi.isString(arg0)) { rv.setUndefined(); return; }

    const str: *const v8.String = @ptrCast(arg0);
    const len: usize = @intCast(v8.StringApi.utf8Length(str, isolate));
    if (len == 0 or len > 4096) { rv.setUndefined(); return; }

    var path_buf: [4096]u8 = undefined;
    const written = v8.StringApi.writeUtf8(str, isolate, &path_buf);
    const path = path_buf[0..written];

    // Check cache first
    if (exists_cache.get(path)) |exists| {
        // Return 1 for true, 0 for false (JS treats both as truthy/falsy correctly)
        rv.setInt32(if (exists) 1 else 0);
        return;
    }

    const exists = blk: {
        _ = std.fs.cwd().statFile(path) catch break :blk false;
        break :blk true;
    };

    const key = alloc.dupe(u8, path) catch path;
    exists_cache.put(alloc, key, exists) catch {};

    rv.setInt32(if (exists) 1 else 0);
}

/// The V8 callback for __edgebox_io_sync(jsonString) → jsonString
/// The V8 callback for __edgebox_io_sync. Public so v8_runner.zig can use
/// its address in the snapshot external_references array.
pub fn ioSyncCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate: *v8.Isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    // Pump V8 message loop on every IO callback — processes pending
    // TurboFan background compilation tasks. Critical for JIT performance.
    if (v8.global_platform) |platform| {
        while (v8.pumpMessageLoop(platform, isolate)) {}
    }

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
    // Note: cached IO responses must NOT be freed — they're reused across calls.
    // Uncached responses (errors, dynamic results) are leaked but minimal.

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
        // Defer exit: save exit code, throw to unwind back to runner
        // so code cache can be saved before termination.
        deferred_exit_code = @intCast(code);
        return "{\"ok\":true,\"deferred_exit\":true}";
    } else if (std.mem.eql(u8, req.op, "argv")) {
        return opArgv();
    } else if (std.mem.eql(u8, req.op, "env")) {
        return opEnv();
    }

    return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"unknown op: {s}\"}}", .{req.op});
}

// ============================================================
// Parallel file prefetch — pre-read all source files into cache
// ============================================================

const max_prefetch_threads = 8;

/// Walk a directory tree and collect all .ts/.tsx/.js/.jsx/.d.ts files
fn collectSourceFiles(dir_path: []const u8, paths: *std.ArrayListUnmanaged([]const u8)) void {
    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch return;
    defer dir.close();

    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        const name = entry.name;
        if (entry.kind == .directory) {
            if (std.mem.eql(u8, name, "node_modules") or
                std.mem.eql(u8, name, ".git") or
                std.mem.eql(u8, name, "dist") or
                std.mem.eql(u8, name, "build"))
                continue;
            const sub_path = std.fmt.allocPrint(alloc, "{s}/{s}", .{ dir_path, name }) catch continue;
            collectSourceFiles(sub_path, paths);
        } else if (entry.kind == .file) {
            const is_source = std.mem.endsWith(u8, name, ".ts") or
                std.mem.endsWith(u8, name, ".tsx") or
                std.mem.endsWith(u8, name, ".js") or
                std.mem.endsWith(u8, name, ".jsx") or
                std.mem.endsWith(u8, name, ".d.ts") or
                std.mem.endsWith(u8, name, ".json");
            if (is_source) {
                const full = std.fmt.allocPrint(alloc, "{s}/{s}", .{ dir_path, name }) catch continue;
                paths.append(alloc, full) catch {};
            }
        }
    }
}

/// Read a single file and populate the cache (thread-safe: each path is unique)
fn prefetchWorker(paths: []const []const u8, start: usize, end: usize) void {
    for (start..end) |i| {
        const path = paths[i];

        // Read file
        const file = std.fs.cwd().openFile(path, .{}) catch continue;
        defer file.close();
        const content = file.readToEndAlloc(alloc, 64 * 1024 * 1024) catch continue;

        // JSON-escape (same logic as opReadFile)
        var escape_count: usize = 0;
        var control_count: usize = 0;
        for (content) |byte| {
            switch (byte) {
                '"', '\\' => escape_count += 1,
                '\n', '\r', '\t' => escape_count += 1,
                else => if (byte < 0x20) { control_count += 1; },
            }
        }

        const prefix = "{\"ok\":true,\"data\":\"";
        const suffix = "\"}";
        const total_len = prefix.len + content.len + escape_count + control_count * 5 + suffix.len;
        const result = alloc.alloc(u8, total_len) catch continue;

        @memcpy(result[0..prefix.len], prefix);
        var pw: usize = prefix.len;

        for (content) |byte| {
            switch (byte) {
                '"' => { result[pw] = '\\'; result[pw + 1] = '"'; pw += 2; },
                '\\' => { result[pw] = '\\'; result[pw + 1] = '\\'; pw += 2; },
                '\n' => { result[pw] = '\\'; result[pw + 1] = 'n'; pw += 2; },
                '\r' => { result[pw] = '\\'; result[pw + 1] = 'r'; pw += 2; },
                '\t' => { result[pw] = '\\'; result[pw + 1] = 't'; pw += 2; },
                else => {
                    if (byte < 0x20) {
                        const hex = std.fmt.bufPrint(result[pw..][0..6], "\\u{x:0>4}", .{byte}) catch unreachable;
                        pw += hex.len;
                    } else {
                        result[pw] = byte;
                        pw += 1;
                    }
                },
            }
        }

        @memcpy(result[pw..][0..suffix.len], suffix);
        pw += suffix.len;

        alloc.free(content);
        const final = result[0..pw];

        // Cache (using mutex for thread safety)
        prefetch_mutex.lock();
        defer prefetch_mutex.unlock();
        const key = alloc.dupe(u8, path) catch continue;
        file_cache.put(alloc, key, final) catch {};

        // Also cache in raw file cache (for fast readFile callback)
        // Re-read the file for raw content (the 'content' was freed after JSON escape)
        const raw_file = std.fs.cwd().openFile(path, .{}) catch continue;
        defer raw_file.close();
        const raw_content = raw_file.readToEndAlloc(alloc, 64 * 1024 * 1024) catch continue;
        raw_file_cache.put(alloc, key, raw_content) catch {};

        // Also cache exists=true
        exists_cache.put(alloc, key, true) catch {};
    }
}

var prefetch_mutex: std.Thread.Mutex = .{};

/// Prefetch all source files in a directory tree using parallel Zig threads.
/// Call before TSC starts to pre-populate the IO cache.
pub fn prefetchDirectory(dir_path: []const u8) void {
    var paths: std.ArrayListUnmanaged([]const u8) = .{};
    collectSourceFiles(dir_path, &paths);

    if (paths.items.len == 0) return;

    const cpu_count = std.Thread.getCpuCount() catch 4;
    const thread_count = @min(@min(paths.items.len, cpu_count), max_prefetch_threads);
    const per_thread = (paths.items.len + thread_count - 1) / thread_count;

    var threads: [max_prefetch_threads]?std.Thread = .{null} ** max_prefetch_threads;

    for (0..thread_count) |i| {
        const start = i * per_thread;
        const end = @min(start + per_thread, paths.items.len);
        if (start >= end) break;
        threads[i] = std.Thread.spawn(.{}, prefetchWorker, .{ paths.items, start, end }) catch null;
    }

    for (&threads) |*t| {
        if (t.*) |thread| {
            thread.join();
            t.* = null;
        }
    }
}

// ============================================================
// IO Operations — direct filesystem access (no socket/IPC)
// ============================================================

/// File read cache — avoids re-reading and re-escaping the same file.
/// TSC reads many files multiple times (program creation + type checking).
var file_cache: std.StringHashMapUnmanaged([]const u8) = .{};

fn opReadFile(req: Request) ![]const u8 {
    const path = req.path orelse return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

    // Check cache first
    if (file_cache.get(path)) |cached| {
        return cached;
    }

    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"{s}\",\"code\":\"ENOENT\"}}", .{@errorName(err)});
    };
    defer file.close();

    const content = file.readToEndAlloc(alloc, 64 * 1024 * 1024) catch |err| {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"{s}\"}}", .{@errorName(err)});
    };

    // Fast JSON-escape: pre-scan to count escape chars, single allocation
    var escape_count: usize = 0;
    var control_count: usize = 0;
    for (content) |byte| {
        switch (byte) {
            '"', '\\' => escape_count += 1,
            '\n', '\r', '\t' => escape_count += 1,
            else => if (byte < 0x20) { control_count += 1; },
        }
    }

    // Pre-allocate exact size: prefix + content + escapes + control chars + suffix
    const prefix = "{\"ok\":true,\"data\":\"";
    const suffix = "\"}";
    const total_len = prefix.len + content.len + escape_count + control_count * 5 + suffix.len;
    const result = try alloc.alloc(u8, total_len);

    @memcpy(result[0..prefix.len], prefix);
    var pw: usize = prefix.len;

    for (content) |byte| {
        switch (byte) {
            '"' => { result[pw] = '\\'; result[pw + 1] = '"'; pw += 2; },
            '\\' => { result[pw] = '\\'; result[pw + 1] = '\\'; pw += 2; },
            '\n' => { result[pw] = '\\'; result[pw + 1] = 'n'; pw += 2; },
            '\r' => { result[pw] = '\\'; result[pw + 1] = 'r'; pw += 2; },
            '\t' => { result[pw] = '\\'; result[pw + 1] = 't'; pw += 2; },
            else => {
                if (byte < 0x20) {
                    const hex = std.fmt.bufPrint(result[pw..][0..6], "\\u{x:0>4}", .{byte}) catch unreachable;
                    pw += hex.len;
                } else {
                    result[pw] = byte;
                    pw += 1;
                }
            },
        }
    }

    @memcpy(result[pw..][0..suffix.len], suffix);
    pw += suffix.len;

    alloc.free(content);
    const final = result[0..pw];

    // Cache the escaped result (path must be duped for map key)
    const key = try alloc.dupe(u8, path);
    try file_cache.put(alloc, key, final);

    return final;
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

/// Stat cache
var stat_cache: std.StringHashMapUnmanaged([]const u8) = .{};

fn opStat(req: Request) ![]const u8 {
    const path = req.path orelse return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

    if (stat_cache.get(path)) |cached| return cached;

    const stat = std.fs.cwd().statFile(path) catch |err| {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"{s}\",\"code\":\"ENOENT\"}}", .{@errorName(err)});
    };

    const is_dir = stat.kind == .directory;
    const is_file = stat.kind == .file;
    const result = try std.fmt.allocPrint(alloc, "{{\"ok\":true,\"isFile\":{},\"isDirectory\":{},\"size\":{d}}}", .{ is_file, is_dir, stat.size });

    const key = alloc.dupe(u8, path) catch path;
    stat_cache.put(alloc, key, result) catch {};

    return result;
}

/// Readdir cache
var readdir_cache: std.StringHashMapUnmanaged([]const u8) = .{};

fn opReaddir(req: Request) ![]const u8 {
    const path = req.path orelse return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

    if (readdir_cache.get(path)) |cached| return cached;

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
    const final = try result.toOwnedSlice(alloc);

    const key = alloc.dupe(u8, path) catch path;
    readdir_cache.put(alloc, key, final) catch {};

    return final;
}

/// Realpath cache
var realpath_cache: std.StringHashMapUnmanaged([]const u8) = .{};

fn opRealpath(req: Request) ![]const u8 {
    const path = req.path orelse return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

    if (realpath_cache.get(path)) |cached| return cached;

    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const real = std.fs.cwd().realpath(path, &buf) catch |err| {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"{s}\"}}", .{@errorName(err)});
    };

    const result = try std.fmt.allocPrint(alloc, "{{\"ok\":true,\"data\":\"{s}\"}}", .{real});
    const key = alloc.dupe(u8, path) catch path;
    realpath_cache.put(alloc, key, result) catch {};

    return result;
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

/// Exists cache — TSC checks fileExists/directoryExists thousands of times
var exists_cache: std.StringHashMapUnmanaged(bool) = .{};

fn opExists(req: Request) ![]const u8 {
    const path = req.path orelse return try std.fmt.allocPrint(alloc, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

    // Check cache
    if (exists_cache.get(path)) |exists| {
        return if (exists) "{\"ok\":true,\"exists\":true}" else "{\"ok\":true,\"exists\":false}";
    }

    const exists = blk: {
        _ = std.fs.cwd().statFile(path) catch {
            break :blk false;
        };
        break :blk true;
    };

    // Cache result
    const key = alloc.dupe(u8, path) catch path;
    exists_cache.put(alloc, key, exists) catch {};

    return if (exists) "{\"ok\":true,\"exists\":true}" else "{\"ok\":true,\"exists\":false}";
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
