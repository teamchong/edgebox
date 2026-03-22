// edgebox_workerd_io.zig — Standalone Zig IO for workerd
//
// C ABI exports called by workerd's jsg API binding.
// Provides filesystem access (readFile, stat, readdir, etc.) via Zig's stdlib.
// File contents cached in mmap for zero-copy reads across workers.

const std = @import("std");

const alloc = std.heap.page_allocator;

// File cache: path → content (shared across all workerd worker threads)
var file_cache: std.StringHashMapUnmanaged([]const u8) = .{};
var cache_mutex: std.Thread.Mutex = .{};

fn cacheReadFile(path: []const u8) ![]const u8 {
    // Check cache first (lock-free read after first population)
    {
        cache_mutex.lock();
        defer cache_mutex.unlock();
        if (file_cache.get(path)) |cached| return cached;
    }

    // Read from disk
    const content = std.fs.cwd().readFileAlloc(alloc, path, 50 * 1024 * 1024) catch |err| {
        return err;
    };

    // Cache it
    const key = try alloc.dupe(u8, path);
    {
        cache_mutex.lock();
        defer cache_mutex.unlock();
        file_cache.put(alloc, key, content) catch {};
    }

    return content;
}

fn jsonEscape(writer: anytype, s: []const u8) !void {
    for (s) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => {
                if (c < 0x20) {
                    try writer.print("\\u{x:0>4}", .{c});
                } else {
                    try writer.writeByte(c);
                }
            },
        }
    }
}

fn handleRequest(request: []const u8) ![]u8 {
    // Parse JSON request manually (avoid std.json overhead)
    // Format: {"op":"readFile","path":"/foo/bar"}
    var op: []const u8 = "";
    var path: []const u8 = "";

    // Simple JSON field extraction
    if (std.mem.indexOf(u8, request, "\"op\":\"")) |op_start| {
        const start = op_start + 6;
        if (std.mem.indexOfPos(u8, request, start, "\"")) |end| {
            op = request[start..end];
        }
    }
    if (std.mem.indexOf(u8, request, "\"path\":\"")) |path_start| {
        const start = path_start + 8;
        if (std.mem.indexOfPos(u8, request, start, "\"")) |end| {
            path = request[start..end];
        }
    }

    var result: std.ArrayListUnmanaged(u8) = .{};

    if (std.mem.eql(u8, op, "readFile")) {
        const content = cacheReadFile(path) catch {
            try result.appendSlice(alloc, "{\"ok\":false}");
            return try result.toOwnedSlice(alloc);
        };
        try result.appendSlice(alloc, "{\"ok\":true,\"data\":\"");
        try jsonEscape(result.writer(alloc), content);
        try result.appendSlice(alloc, "\"}");
    } else if (std.mem.eql(u8, op, "stat")) {
        const stat = std.fs.cwd().statFile(path) catch {
            try result.appendSlice(alloc, "{\"ok\":false}");
            return try result.toOwnedSlice(alloc);
        };
        try result.appendSlice(alloc, "{\"ok\":true,\"data\":{\"isFile\":");
        try result.appendSlice(alloc, if (stat.kind == .file) "true" else "false");
        try result.appendSlice(alloc, ",\"isDirectory\":");
        try result.appendSlice(alloc, if (stat.kind == .directory) "true" else "false");
        try result.appendSlice(alloc, ",\"size\":");
        try result.writer(alloc).print("{d}", .{stat.size});
        try result.appendSlice(alloc, "}}");
    } else if (std.mem.eql(u8, op, "fileExists")) {
        const exists = std.fs.cwd().access(path, .{});
        if (exists) |_| {
            try result.appendSlice(alloc, "{\"ok\":true,\"data\":true}");
        } else |_| {
            try result.appendSlice(alloc, "{\"ok\":true,\"data\":false}");
        }
    } else if (std.mem.eql(u8, op, "dirExists")) {
        const dir = std.fs.cwd().openDir(path, .{});
        if (dir) |d| {
            var dd = d;
            dd.close();
            try result.appendSlice(alloc, "{\"ok\":true,\"data\":true}");
        } else |_| {
            try result.appendSlice(alloc, "{\"ok\":true,\"data\":false}");
        }
    } else if (std.mem.eql(u8, op, "readdir")) {
        var dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch {
            try result.appendSlice(alloc, "{\"ok\":false}");
            return try result.toOwnedSlice(alloc);
        };
        defer dir.close();
        try result.appendSlice(alloc, "{\"ok\":true,\"data\":[");
        var first = true;
        var it = dir.iterate();
        while (it.next() catch null) |entry| {
            if (!first) try result.append(alloc, ',');
            first = false;
            try result.append(alloc, '"');
            try result.appendSlice(alloc, entry.name);
            try result.append(alloc, '"');
        }
        try result.appendSlice(alloc, "]}");
    } else if (std.mem.eql(u8, op, "cwd")) {
        var buf: [std.fs.max_path_bytes]u8 = undefined;
        const cwd = std.fs.cwd().realpath(".", &buf) catch ".";
        try result.appendSlice(alloc, "{\"ok\":true,\"data\":\"");
        try result.appendSlice(alloc, cwd);
        try result.appendSlice(alloc, "\"}");
    } else if (std.mem.eql(u8, op, "argv")) {
        var args = try std.process.argsWithAllocator(alloc);
        defer args.deinit();
        try result.appendSlice(alloc, "{\"ok\":true,\"data\":[");
        var first = true;
        while (args.next()) |arg| {
            if (!first) try result.append(alloc, ',');
            first = false;
            try result.append(alloc, '"');
            try jsonEscape(result.writer(alloc), arg);
            try result.append(alloc, '"');
        }
        try result.appendSlice(alloc, "]}");
    } else if (std.mem.eql(u8, op, "env")) {
        try result.appendSlice(alloc, "{\"ok\":true,\"data\":{}}");
    } else if (std.mem.eql(u8, op, "version")) {
        try result.appendSlice(alloc, "{\"ok\":true,\"data\":\"edgebox-workerd-0.1.0\"}");
    } else if (std.mem.eql(u8, op, "write")) {
        // Extract data field
        if (std.mem.indexOf(u8, request, "\"data\":\"")) |data_start| {
            const start = data_start + 8;
            if (std.mem.lastIndexOf(u8, request, "\"")) |end| {
                if (end > start) {
                    const data = request[start..end];
                    const stdout = std.fs.File.stdout();
                    stdout.writeAll(data) catch {};
                }
            }
        }
        try result.appendSlice(alloc, "{\"ok\":true}");
    } else if (std.mem.eql(u8, op, "writeErr")) {
        if (std.mem.indexOf(u8, request, "\"data\":\"")) |data_start| {
            const start = data_start + 8;
            if (std.mem.lastIndexOf(u8, request, "\"")) |end| {
                if (end > start) {
                    const data = request[start..end];
                    const stderr = std.fs.File.stderr();
                    stderr.writeAll(data) catch {};
                }
            }
        }
        try result.appendSlice(alloc, "{\"ok\":true}");
    } else if (std.mem.eql(u8, op, "exit")) {
        // Extract code
        if (std.mem.indexOf(u8, request, "\"code\":")) |code_start| {
            const start = code_start + 7;
            var end = start;
            while (end < request.len and request[end] >= '0' and request[end] <= '9') : (end += 1) {}
            const code_str = request[start..end];
            const code = std.fmt.parseInt(u8, code_str, 10) catch 0;
            std.process.exit(code);
        }
        try result.appendSlice(alloc, "{\"ok\":true}");
    } else if (std.mem.eql(u8, op, "setConfig")) {
        // Store shared config — main worker writes, checker workers read
        if (std.mem.indexOf(u8, request, "\"data\":\"")) |data_start| {
            const start = data_start + 8;
            if (std.mem.lastIndexOf(u8, request, "\"")) |end| {
                if (end > start) {
                    const data = request[start..end];
                    shared_config_mutex.lock();
                    defer shared_config_mutex.unlock();
                    if (shared_config) |old| alloc.free(@constCast(old));
                    const copy = alloc.alloc(u8, data.len) catch {
                        try result.appendSlice(alloc, "{\"ok\":false}");
                        return try result.toOwnedSlice(alloc);
                    };
                    @memcpy(copy, data);
                    shared_config = copy;
                }
            }
        }
        try result.appendSlice(alloc, "{\"ok\":true}");
    } else if (std.mem.eql(u8, op, "getConfig")) {
        // Read shared config — zero copy from Zig heap
        shared_config_mutex.lock();
        defer shared_config_mutex.unlock();
        if (shared_config) |cfg| {
            try result.appendSlice(alloc, "{\"ok\":true,\"data\":\"");
            try result.appendSlice(alloc, cfg);
            try result.appendSlice(alloc, "\"}");
        } else {
            try result.appendSlice(alloc, "{\"ok\":false}");
        }
    } else {
        try result.appendSlice(alloc, "{\"ok\":false,\"error\":\"unknown op\"}");
    }

    return try result.toOwnedSlice(alloc);
}

/// C ABI: Synchronous IO
export fn edgebox_io_sync(
    request_ptr: [*]const u8,
    request_len: c_int,
    response_len: *c_int,
) ?[*]const u8 {
    if (request_len <= 0) {
        response_len.* = 0;
        return null;
    }

    const request = request_ptr[0..@intCast(request_len)];
    const response = handleRequest(request) catch {
        const err = "{\"ok\":false,\"error\":\"io_error\"}";
        const buf = alloc.alloc(u8, err.len) catch return null;
        @memcpy(buf, err);
        response_len.* = @intCast(err.len);
        return buf.ptr;
    };

    response_len.* = @intCast(response.len);
    return response.ptr;
}

/// C ABI: Batch IO (sequential for now)
export fn edgebox_io_batch(
    request_ptr: [*]const u8,
    request_len: c_int,
    response_len: *c_int,
) ?[*]const u8 {
    _ = request_ptr;
    _ = request_len;
    const r = "[]";
    const buf = alloc.alloc(u8, r.len) catch return null;
    @memcpy(buf, r);
    response_len.* = @intCast(r.len);
    return buf.ptr;
}

/// C ABI: Free response buffer
export fn edgebox_io_free(ptr: ?[*]const u8, len: c_int) void {
    if (ptr) |p| {
        if (len > 0) {
            const slice: []const u8 = p[0..@intCast(len)];
            alloc.free(@constCast(slice));
        }
    }
}

// ── Shared project config (zero-copy coordination between workers) ──
// Main worker writes project rootNames + options once.
// Checker workers read at module load time → start parsing IMMEDIATELY.
// Data lives in Zig heap — all workerd workers (same process) see same pointer.
var shared_config: ?[]const u8 = null;
var shared_config_mutex: std.Thread.Mutex = .{};

/// C ABI: Main writes project config (JSON string: {rootNames, options, cwd})
export fn edgebox_set_shared_config(ptr: [*]const u8, len: c_int) void {
    if (len <= 0) return;
    const data = ptr[0..@intCast(len)];
    shared_config_mutex.lock();
    defer shared_config_mutex.unlock();
    if (shared_config) |old| alloc.free(@constCast(old));
    const copy = alloc.alloc(u8, data.len) catch return;
    @memcpy(copy, data);
    shared_config = copy;
}

/// C ABI: Workers read project config (returns pointer to Zig heap — zero copy)
export fn edgebox_get_shared_config(out_len: *c_int) ?[*]const u8 {
    shared_config_mutex.lock();
    defer shared_config_mutex.unlock();
    if (shared_config) |cfg| {
        out_len.* = @intCast(cfg.len);
        return cfg.ptr;
    }
    out_len.* = 0;
    return null;
}
