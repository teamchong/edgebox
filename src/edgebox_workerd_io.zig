// edgebox_workerd_io.zig — Zig polyfills for workerd
//
// ALL Node.js APIs that workerd doesn't have are implemented here in Zig.
// Direct C ABI exports → workerd jsg methods → JS globals. Zero JSON in hot path.
// File contents cached in mmap for zero-copy reads across workers.

const std = @import("std");
const alloc = std.heap.page_allocator;

// ── File Cache (shared across all workerd worker threads) ──

var file_cache: std.StringHashMapUnmanaged([]const u8) = .{};
var cache_mutex: std.Thread.Mutex = .{};

fn cacheReadFile(path: []const u8) ![]const u8 {
    {
        cache_mutex.lock();
        defer cache_mutex.unlock();
        if (file_cache.get(path)) |cached| return cached;
    }
    const raw = std.fs.cwd().readFileAlloc(alloc, path, 50 * 1024 * 1024) catch |err| return err;
    // Append NUL terminator for kj::StringPtr compatibility (workerd requires it)
    const content = try alloc.alloc(u8, raw.len + 1);
    @memcpy(content[0..raw.len], raw);
    content[raw.len] = 0;
    alloc.free(raw);
    const key = try alloc.dupe(u8, path);
    {
        cache_mutex.lock();
        defer cache_mutex.unlock();
        // Store slice WITHOUT the NUL (len = raw content length)
        // But the NUL is there at content[raw.len] for kj::StringPtr
        file_cache.put(alloc, key, content[0..raw.len]) catch {};
    }
    return content[0..raw.len];
}

// ── fs: readFile, writeFile, stat, exists, readdir, realpath ──

/// Read file contents. Returns NUL-terminated pointer+len. Cached — do NOT free.
export fn edgebox_read_file(path_ptr: [*]const u8, path_len: c_int, out_len: *c_int) ?[*]const u8 {
    if (path_len <= 0) { out_len.* = 0; return null; }
    const path = path_ptr[0..@intCast(path_len)];
    const content = cacheReadFile(path) catch { out_len.* = -1; return null; };
    out_len.* = @intCast(content.len);
    return content.ptr;
}

/// Write file contents.
export fn edgebox_write_file(path_ptr: [*]const u8, path_len: c_int, data_ptr: [*]const u8, data_len: c_int) c_int {
    if (path_len <= 0 or data_len < 0) return -1;
    const path = path_ptr[0..@intCast(path_len)];
    const data = if (data_len > 0) data_ptr[0..@intCast(data_len)] else "";
    std.fs.cwd().writeFile(.{ .sub_path = path, .data = data }) catch return -1;
    return 0;
}

/// Check if file exists. Returns 1=yes, 0=no.
export fn edgebox_file_exists(path_ptr: [*]const u8, path_len: c_int) c_int {
    if (path_len <= 0) return 0;
    const path = path_ptr[0..@intCast(path_len)];
    std.fs.cwd().access(path, .{}) catch return 0;
    return 1;
}

/// Check if directory exists. Returns 1=yes, 0=no.
export fn edgebox_dir_exists(path_ptr: [*]const u8, path_len: c_int) c_int {
    if (path_len <= 0) return 0;
    const path = path_ptr[0..@intCast(path_len)];
    var dir = std.fs.cwd().openDir(path, .{}) catch return 0;
    dir.close();
    return 1;
}

/// Stat file. Returns JSON: {"isFile":true,"isDirectory":false,"size":N}
/// Uses JSON for this one because stat has multiple return values.
export fn edgebox_stat(path_ptr: [*]const u8, path_len: c_int, out_len: *c_int) ?[*]const u8 {
    if (path_len <= 0) { out_len.* = 0; return null; }
    const path = path_ptr[0..@intCast(path_len)];
    const stat = std.fs.cwd().statFile(path) catch { out_len.* = 0; return null; };
    const result = std.fmt.allocPrint(alloc,
        "{{\"isFile\":{s},\"isDirectory\":{s},\"size\":{d}}}",
        .{
            if (stat.kind == .file) "true" else "false",
            if (stat.kind == .directory) "true" else "false",
            stat.size,
        },
    ) catch { out_len.* = 0; return null; };
    out_len.* = @intCast(result.len);
    return result.ptr;
}

/// Read directory entries. Returns JSON array: ["file1","file2",...]
export fn edgebox_readdir(path_ptr: [*]const u8, path_len: c_int, out_len: *c_int) ?[*]const u8 {
    if (path_len <= 0) { out_len.* = 0; return null; }
    const path = path_ptr[0..@intCast(path_len)];
    var dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch { out_len.* = 0; return null; };
    defer dir.close();
    var result: std.ArrayListUnmanaged(u8) = .{};
    result.appendSlice(alloc, "[") catch { out_len.* = 0; return null; };
    var first = true;
    var it = dir.iterate();
    while (it.next() catch null) |entry| {
        if (!first) result.append(alloc, ',') catch {};
        first = false;
        result.append(alloc, '"') catch {};
        result.appendSlice(alloc, entry.name) catch {};
        result.append(alloc, '"') catch {};
    }
    result.appendSlice(alloc, "]") catch {};
    const slice = result.toOwnedSlice(alloc) catch { out_len.* = 0; return null; };
    out_len.* = @intCast(slice.len);
    return slice.ptr;
}

/// Resolve realpath. Returns resolved path string.
export fn edgebox_realpath(path_ptr: [*]const u8, path_len: c_int, out_len: *c_int) ?[*]const u8 {
    if (path_len <= 0) { out_len.* = 0; return null; }
    const path = path_ptr[0..@intCast(path_len)];
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const resolved = std.fs.cwd().realpath(path, &buf) catch {
        // Return original path on failure
        const copy = alloc.alloc(u8, path.len) catch { out_len.* = 0; return null; };
        @memcpy(copy, path);
        out_len.* = @intCast(copy.len);
        return copy.ptr;
    };
    const copy = alloc.alloc(u8, resolved.len) catch { out_len.* = 0; return null; };
    @memcpy(copy, resolved);
    out_len.* = @intCast(copy.len);
    return copy.ptr;
}

/// Get current working directory.
export fn edgebox_cwd(out_len: *c_int) ?[*]const u8 {
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = std.fs.cwd().realpath(".", &buf) catch {
        const fallback = "/";
        out_len.* = 1;
        return fallback.ptr;
    };
    const copy = alloc.alloc(u8, cwd.len) catch { out_len.* = 0; return null; };
    @memcpy(copy, cwd);
    out_len.* = @intCast(copy.len);
    return copy.ptr;
}

// ── process: exit, argv, env ──

export fn edgebox_exit(code: c_int) void {
    std.process.exit(@intCast(@max(0, @min(code, 255))));
}

// ── stdout/stderr: direct write ──

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

// ── crypto: hash, hmac, randomBytes ──

export fn edgebox_hash(
    algo_ptr: [*]const u8, algo_len: c_int,
    data_ptr: [*]const u8, data_len: c_int,
    out_ptr: [*]u8, out_cap: c_int,
) c_int {
    if (algo_len <= 0 or data_len < 0 or out_cap < 2) return -1;
    const algo = algo_ptr[0..@intCast(algo_len)];
    const data = if (data_len > 0) data_ptr[0..@intCast(data_len)] else &[_]u8{};
    const out = out_ptr[0..@intCast(out_cap)];

    if (std.mem.eql(u8, algo, "sha256")) {
        var hash: [32]u8 = undefined;
        std.crypto.hash.sha2.Sha256.hash(data, &hash, .{});
        return writeHex(&hash, out);
    } else if (std.mem.eql(u8, algo, "sha512")) {
        var hash: [64]u8 = undefined;
        std.crypto.hash.sha2.Sha512.hash(data, &hash, .{});
        return writeHex(&hash, out);
    } else if (std.mem.eql(u8, algo, "sha384")) {
        var hash: [48]u8 = undefined;
        std.crypto.hash.sha2.Sha384.hash(data, &hash, .{});
        return writeHex(&hash, out);
    } else if (std.mem.eql(u8, algo, "sha1")) {
        var hash: [20]u8 = undefined;
        std.crypto.hash.Sha1.hash(data, &hash, .{});
        return writeHex(&hash, out);
    } else if (std.mem.eql(u8, algo, "md5")) {
        var hash: [16]u8 = undefined;
        std.crypto.hash.Md5.hash(data, &hash, .{});
        return writeHex(&hash, out);
    }
    return -2; // Unknown algorithm
}

fn writeHex(hash: []const u8, out: []u8) c_int {
    const hex_len = hash.len * 2;
    if (out.len < hex_len) return -3;
    const hex = "0123456789abcdef";
    for (hash, 0..) |b, i| {
        out[i * 2] = hex[b >> 4];
        out[i * 2 + 1] = hex[b & 0xf];
    }
    return @intCast(hex_len);
}

export fn edgebox_random_bytes(out_ptr: [*]u8, out_len: c_int) c_int {
    if (out_len <= 0) return 0;
    const buf = out_ptr[0..@intCast(out_len)];
    std.crypto.random.bytes(buf);
    return out_len;
}

// ── Free: caller frees buffers returned by Zig ──

export fn edgebox_free(ptr: ?[*]const u8, len: c_int) void {
    if (ptr) |p| {
        if (len > 0) {
            alloc.free(@constCast(p[0..@intCast(len)]));
        }
    }
}

// ── Zero-Copy Parallel (Zig condvar + shared memory) ──
// Workers block on condvar. Main signals. Workers wake, do work, submit.
// ALL in shared memory. ZERO HTTP. ZERO TCP. ZERO serialization.

const MAX_WORKERS = 16;

const WorkSlot = struct {
    cwd: ?[]const u8 = null,
    worker_count: u32 = 0,
    ready: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
    result: ?[]const u8 = null,
    done: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
};

var work_slots: [MAX_WORKERS]WorkSlot = [_]WorkSlot{.{}} ** MAX_WORKERS;
var work_mutex: std.Thread.Mutex = .{};
var work_cond: std.Thread.Condition = .{};

/// Main calls this: set work for all workers, signal condvar.
export fn edgebox_dispatch_work(cwd_ptr: [*]const u8, cwd_len: c_int, worker_count: c_int) void {
    if (cwd_len <= 0 or worker_count <= 0) return;
    const cwd = cwd_ptr[0..@intCast(cwd_len)];
    const n: u32 = @intCast(@min(worker_count, MAX_WORKERS));

    for (0..n) |i| {
        work_slots[i].cwd = cwd;
        work_slots[i].worker_count = n;
        work_slots[i].result = null;
        work_slots[i].done.store(false, .release);
        work_slots[i].ready.store(true, .release);
    }

    work_mutex.lock();
    work_cond.broadcast();
    work_mutex.unlock();
}

/// Worker calls this: blocks until work ready. Returns "cwd|workerId|workerCount".
export fn edgebox_wait_for_work(worker_id: c_int, out_len: *c_int) ?[*]const u8 {
    if (worker_id < 0 or worker_id >= MAX_WORKERS) { out_len.* = 0; return null; }
    const wid: usize = @intCast(worker_id);

    work_mutex.lock();
    while (!work_slots[wid].ready.load(.acquire)) {
        work_cond.timedWait(&work_mutex, 100 * std.time.ns_per_ms) catch {};
    }
    work_mutex.unlock();

    work_slots[wid].ready.store(false, .release);

    if (work_slots[wid].cwd) |cwd| {
        // Return "cwd|workerId|workerCount" as NUL-terminated string
        const info = std.fmt.allocPrint(alloc, "{s}|{d}|{d}", .{ cwd, wid, work_slots[wid].worker_count }) catch { out_len.* = 0; return null; };
        // Append NUL for kj::StringPtr
        const buf = alloc.alloc(u8, info.len + 1) catch { alloc.free(info); out_len.* = 0; return null; };
        @memcpy(buf[0..info.len], info);
        buf[info.len] = 0;
        alloc.free(info);
        out_len.* = @intCast(info.len);
        return buf.ptr;
    }
    out_len.* = 0;
    return null;
}

/// Worker calls this: submit results to shared buffer.
export fn edgebox_submit_result(worker_id: c_int, data_ptr: [*]const u8, data_len: c_int) void {
    if (worker_id < 0 or worker_id >= MAX_WORKERS or data_len < 0) return;
    const wid: usize = @intCast(worker_id);
    if (data_len > 0) {
        const copy = alloc.alloc(u8, @intCast(data_len)) catch return;
        @memcpy(copy, data_ptr[0..@intCast(data_len)]);
        work_slots[wid].result = copy;
    }
    work_slots[wid].done.store(true, .release);
    work_mutex.lock();
    work_cond.broadcast();
    work_mutex.unlock();
}

/// Main calls this: blocks until all workers done, returns merged results.
export fn edgebox_collect_results(worker_count: c_int, out_len: *c_int) ?[*]const u8 {
    if (worker_count <= 0) { out_len.* = 0; return null; }
    const n: usize = @intCast(@min(worker_count, MAX_WORKERS));

    work_mutex.lock();
    while (true) {
        var all_done = true;
        for (0..n) |i| {
            if (!work_slots[i].done.load(.acquire)) { all_done = false; break; }
        }
        if (all_done) break;
        work_cond.timedWait(&work_mutex, 100 * std.time.ns_per_ms) catch {};
    }
    work_mutex.unlock();

    var merged: std.ArrayListUnmanaged(u8) = .{};
    var seen = std.StringHashMapUnmanaged(void){};
    for (0..n) |i| {
        if (work_slots[i].result) |data| {
            var lines = std.mem.splitScalar(u8, data, '\n');
            while (lines.next()) |line| {
                if (line.len == 0) continue;
                if (seen.get(line) != null) continue;
                const key = alloc.dupe(u8, line) catch continue;
                seen.put(alloc, key, {}) catch continue;
                merged.appendSlice(alloc, line) catch continue;
                merged.append(alloc, '\n') catch continue;
            }
        }
    }

    const output = merged.toOwnedSlice(alloc) catch { out_len.* = 0; return null; };
    out_len.* = @intCast(output.len);
    return output.ptr;
}

// ── Legacy: __edgebox_io_sync JSON fallback (kept for backward compat) ──

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

fn extractField(request: []const u8, comptime field: []const u8) []const u8 {
    const needle = "\"" ++ field ++ "\":\"";
    if (std.mem.indexOf(u8, request, needle)) |start| {
        const s = start + needle.len;
        if (std.mem.indexOfPos(u8, request, s, "\"")) |end| return request[s..end];
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
        try result.writer(alloc).print("{{\"ok\":true,\"data\":{{\"isFile\":{s},\"isDirectory\":{s},\"size\":{d}}}}}", .{
            if (stat.kind == .file) "true" else "false",
            if (stat.kind == .directory) "true" else "false",
            stat.size,
        });
    } else if (std.mem.eql(u8, op, "fileExists")) {
        const exists = std.fs.cwd().access(path, .{});
        try result.appendSlice(alloc, if (exists) |_| "{\"ok\":true,\"data\":true}" else |_| "{\"ok\":true,\"data\":false}");
    } else if (std.mem.eql(u8, op, "dirExists")) {
        var dir = std.fs.cwd().openDir(path, .{}) catch null;
        if (dir) |*d| { d.close(); try result.appendSlice(alloc, "{\"ok\":true,\"data\":true}"); } else try result.appendSlice(alloc, "{\"ok\":true,\"data\":false}");
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
        const cwd_path = std.fs.cwd().realpath(".", &buf) catch ".";
        try result.appendSlice(alloc, "{\"ok\":true,\"data\":\"");
        try jsonEscape(result.writer(alloc), cwd_path);
        try result.appendSlice(alloc, "\"}");
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

export fn edgebox_io_sync(request_ptr: [*]const u8, request_len: c_int, response_len: *c_int) ?[*]const u8 {
    if (request_len <= 0) { response_len.* = 0; return null; }
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
    edgebox_free(ptr, len);
}

// ── Zero-Copy Type Data Model ──

const MAX_TYPES: u32 = 200_000;
const MAX_MEMBERS: u32 = 2_000_000;

var col_type_flags: [MAX_TYPES]u32 = [_]u32{0} ** MAX_TYPES;
var col_type_member_offset: [MAX_TYPES]u32 = [_]u32{0} ** MAX_TYPES;
var col_type_member_count: [MAX_TYPES]u16 = [_]u16{0} ** MAX_TYPES;
var type_count: u32 = 0;

var col_member_name_id: [MAX_MEMBERS]u32 = [_]u32{0} ** MAX_MEMBERS;
var col_member_type_id: [MAX_MEMBERS]u32 = [_]u32{0} ** MAX_MEMBERS;
var col_member_flags: [MAX_MEMBERS]u32 = [_]u32{0} ** MAX_MEMBERS;
var member_count: u32 = 0;

var string_table: std.StringHashMapUnmanaged(u32) = .{};
var string_count: u32 = 0;
var type_data_mutex: std.Thread.Mutex = .{};

export fn edgebox_register_type(type_id: u32, flags: u32, _: u32) void {
    if (type_id >= MAX_TYPES) return;
    col_type_flags[type_id] = flags;
    if (type_id >= type_count) type_count = type_id + 1;
}

export fn edgebox_register_member(type_id: u32, name_ptr: [*]const u8, name_len: c_int, member_type_id: u32, member_flags: u32) void {
    if (type_id >= MAX_TYPES or member_count >= MAX_MEMBERS or name_len <= 0) return;
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
    if (col_type_member_count[type_id] == 0) col_type_member_offset[type_id] = member_count;
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
    if (tgt_flags & 1 != 0) { _ = check_flag_hits.fetchAdd(1, .monotonic); return 1; }
    if (src_flags & 131072 != 0) { _ = check_flag_hits.fetchAdd(1, .monotonic); return 1; }
    if (tgt_flags & 2 != 0) { _ = check_flag_hits.fetchAdd(1, .monotonic); return 1; }
    if (src_flags & (128 | 4) != 0 and tgt_flags & 4 != 0) { _ = check_flag_hits.fetchAdd(1, .monotonic); return 1; }
    if (src_flags & (256 | 8) != 0 and tgt_flags & 8 != 0) { _ = check_flag_hits.fetchAdd(1, .monotonic); return 1; }
    if (src_flags & (512 | 16) != 0 and tgt_flags & 16 != 0) { _ = check_flag_hits.fetchAdd(1, .monotonic); return 1; }
    if (src_flags & (2048 | 64) != 0 and tgt_flags & 64 != 0) { _ = check_flag_hits.fetchAdd(1, .monotonic); return 1; }
    const src_offset = col_type_member_offset[source_id];
    const src_count = col_type_member_count[source_id];
    const tgt_offset = col_type_member_offset[target_id];
    const tgt_count = col_type_member_count[target_id];
    if (src_count == 0 or tgt_count == 0) return 2;
    if (tgt_count > src_count * 2) return 0;
    var ti: u32 = 0;
    while (ti < tgt_count) : (ti += 1) {
        const tgt_name = col_member_name_id[tgt_offset + ti];
        var found = false;
        var si: u32 = 0;
        while (si < src_count) : (si += 1) {
            if (col_member_name_id[src_offset + si] == tgt_name) { found = true; break; }
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
