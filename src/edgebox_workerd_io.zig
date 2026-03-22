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
    {
        cache_mutex.lock();
        defer cache_mutex.unlock();
        if (file_cache.get(path)) |cached| return cached;
    }

    const content = std.fs.cwd().readFileAlloc(alloc, path, 50 * 1024 * 1024) catch |err| {
        return err;
    };

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

// Simple JSON field extraction (used only for __edgebox_io_sync fallback path)
fn extractField(request: []const u8, comptime field: []const u8) []const u8 {
    const needle = "\"" ++ field ++ "\":\"";
    if (std.mem.indexOf(u8, request, needle)) |start| {
        const s = start + needle.len;
        if (std.mem.indexOfPos(u8, request, s, "\"")) |end| {
            return request[s..end];
        }
    }
    return "";
}

fn handleRequest(request: []const u8) ![]u8 {
    const op = extractField(request, "op");
    const path = extractField(request, "path");

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
            try jsonEscape(result.writer(alloc), entry.name);
            try result.append(alloc, '"');
        }
        try result.appendSlice(alloc, "]}");
    } else if (std.mem.eql(u8, op, "cwd")) {
        var buf: [std.fs.max_path_bytes]u8 = undefined;
        const cwd = std.fs.cwd().realpath(".", &buf) catch ".";
        try result.appendSlice(alloc, "{\"ok\":true,\"data\":\"");
        try jsonEscape(result.writer(alloc), cwd);
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
    } else if (std.mem.eql(u8, op, "exit")) {
        if (std.mem.indexOf(u8, request, "\"code\":")) |code_start| {
            const start = code_start + 7;
            var end = start;
            while (end < request.len and request[end] >= '0' and request[end] <= '9') : (end += 1) {}
            const code = std.fmt.parseInt(u8, request[start..end], 10) catch 0;
            std.process.exit(code);
        }
        try result.appendSlice(alloc, "{\"ok\":true}");
    } else {
        try result.appendSlice(alloc, "{\"ok\":false,\"error\":\"unknown op\"}");
    }

    return try result.toOwnedSlice(alloc);
}

// ── C ABI: IO dispatch (fallback path — only for readFile, stat, etc.) ──

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

export fn edgebox_io_free(ptr: ?[*]const u8, len: c_int) void {
    if (ptr) |p| {
        if (len > 0) {
            const slice: []const u8 = p[0..@intCast(len)];
            alloc.free(@constCast(slice));
        }
    }
}

// ── C ABI: Direct typed methods (zero copy, no JSON) ──

export fn edgebox_write_stdout(ptr: [*]const u8, len: c_int) void {
    if (len <= 0) return;
    const data = ptr[0..@intCast(len)];
    var written: usize = 0;
    while (written < data.len) {
        const n = std.posix.write(1, data[written..]) catch break;
        if (n == 0) break;
        written += n;
    }
}

export fn edgebox_write_stderr(ptr: [*]const u8, len: c_int) void {
    if (len <= 0) return;
    const data = ptr[0..@intCast(len)];
    var written: usize = 0;
    while (written < data.len) {
        const n = std.posix.write(2, data[written..]) catch break;
        if (n == 0) break;
        written += n;
    }
}

// ── Zero-Copy Type Data Model ──
// Flat columnar storage for TypeScript types in Zig mmap.
// All workers share the same memory — zero copy across isolates.

const MAX_TYPES: u32 = 200_000;
const MAX_MEMBERS: u32 = 2_000_000;

// Type columns (SOA layout)
var col_type_flags: [MAX_TYPES]u32 = [_]u32{0} ** MAX_TYPES;
var col_type_member_offset: [MAX_TYPES]u32 = [_]u32{0} ** MAX_TYPES;
var col_type_member_count: [MAX_TYPES]u16 = [_]u16{0} ** MAX_TYPES;
var type_count: u32 = 0;

// Member columns
var col_member_name_id: [MAX_MEMBERS]u32 = [_]u32{0} ** MAX_MEMBERS;
var col_member_type_id: [MAX_MEMBERS]u32 = [_]u32{0} ** MAX_MEMBERS;
var col_member_flags: [MAX_MEMBERS]u32 = [_]u32{0} ** MAX_MEMBERS;
var member_count: u32 = 0;

// String intern table
var string_table: std.StringHashMapUnmanaged(u32) = .{};
var string_count: u32 = 0;
var type_data_mutex: std.Thread.Mutex = .{};

export fn edgebox_register_type(type_id: u32, flags: u32, _: u32) void {
    if (type_id >= MAX_TYPES) return;
    col_type_flags[type_id] = flags;
    if (type_id >= type_count) type_count = type_id + 1;
}

export fn edgebox_register_member(
    type_id: u32,
    name_ptr: [*]const u8,
    name_len: c_int,
    member_type_id: u32,
    member_flags: u32,
) void {
    if (type_id >= MAX_TYPES or member_count >= MAX_MEMBERS) return;
    if (name_len <= 0) return;

    const name = name_ptr[0..@intCast(name_len)];

    type_data_mutex.lock();
    defer type_data_mutex.unlock();

    const name_id = blk: {
        if (string_table.get(name)) |id| break :blk id;
        const id = string_count;
        string_count += 1;
        const key = alloc.dupe(u8, name) catch return;
        string_table.put(alloc, key, id) catch return;
        break :blk id;
    };

    if (col_type_member_count[type_id] == 0) {
        col_type_member_offset[type_id] = member_count;
    }

    const idx = member_count;
    col_member_name_id[idx] = name_id;
    col_member_type_id[idx] = member_type_id;
    col_member_flags[idx] = member_flags;
    member_count += 1;
    col_type_member_count[type_id] += 1;
}

// ── SIMD Structural Check ──

var check_total: std.atomic.Value(u64) = std.atomic.Value(u64).init(0);
var check_flag_hits: std.atomic.Value(u64) = std.atomic.Value(u64).init(0);
var check_structural_hits: std.atomic.Value(u64) = std.atomic.Value(u64).init(0);

export fn edgebox_check_stats(out_total: *u64, out_flag: *u64, out_struct: *u64) void {
    out_total.* = check_total.load(.monotonic);
    out_flag.* = check_flag_hits.load(.monotonic);
    out_struct.* = check_structural_hits.load(.monotonic);
}

export fn edgebox_check_structural(source_id: u32, target_id: u32) u8 {
    _ = check_total.fetchAdd(1, .monotonic);
    if (source_id >= type_count or target_id >= type_count) return 2;

    const src_flags = col_type_flags[source_id];
    const tgt_flags = col_type_flags[target_id];

    // Flag fast path
    if (tgt_flags & 1 != 0) { _ = check_flag_hits.fetchAdd(1, .monotonic); return 1; } // Any
    if (src_flags & 131072 != 0) { _ = check_flag_hits.fetchAdd(1, .monotonic); return 1; } // Undefined
    if (tgt_flags & 2 != 0) { _ = check_flag_hits.fetchAdd(1, .monotonic); return 1; } // Unknown
    if (src_flags & (128 | 4) != 0 and tgt_flags & 4 != 0) { _ = check_flag_hits.fetchAdd(1, .monotonic); return 1; } // String
    if (src_flags & (256 | 8) != 0 and tgt_flags & 8 != 0) { _ = check_flag_hits.fetchAdd(1, .monotonic); return 1; } // Number
    if (src_flags & (512 | 16) != 0 and tgt_flags & 16 != 0) { _ = check_flag_hits.fetchAdd(1, .monotonic); return 1; } // Boolean
    if (src_flags & (2048 | 64) != 0 and tgt_flags & 64 != 0) { _ = check_flag_hits.fetchAdd(1, .monotonic); return 1; } // BigInt

    // Structural: check member name overlap
    const src_offset = col_type_member_offset[source_id];
    const src_count = col_type_member_count[source_id];
    const tgt_offset = col_type_member_offset[target_id];
    const tgt_count = col_type_member_count[target_id];

    if (src_count == 0 or tgt_count == 0) return 2;
    if (tgt_count > src_count * 2) return 0;

    var matched: u32 = 0;
    var ti: u32 = 0;
    while (ti < tgt_count) : (ti += 1) {
        const tgt_name = col_member_name_id[tgt_offset + ti];
        var found = false;
        var si: u32 = 0;
        while (si < src_count) : (si += 1) {
            if (col_member_name_id[src_offset + si] == tgt_name) {
                found = true;
                matched += 1;
                break;
            }
        }
        if (!found) return 0;
    }

    _ = check_structural_hits.fetchAdd(1, .monotonic);
    return 1;
}

export fn edgebox_type_stats(out_types: *u32, out_members: *u32, out_strings: *u32) void {
    out_types.* = type_count;
    out_members.* = member_count;
    out_strings.* = string_count;
}
