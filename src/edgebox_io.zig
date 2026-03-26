// edgebox_io.zig — Zig polyfills for V8 pool
//
// ALL Node.js APIs that V8 pool doesn't have are implemented here in Zig.
// Direct C ABI exports → V8 pool jsg methods → JS globals. Zero JSON in hot path.
// File contents cached in mmap for zero-copy reads across workers.

const std = @import("std");
const alloc = std.heap.page_allocator;

// Import shared AST pool — exports shared_ast_* C ABI functions for cross-worker parse splitting
comptime {
    _ = @import("shared_ast.zig");
}

// Import Zig parser — exports edgebox_zig_parse C ABI function
comptime {
    _ = @import("zig_parser_core.zig");
}

// ── File Cache (shared across all V8 pool worker threads) ──

var file_cache: std.StringHashMapUnmanaged([]const u8) = .{};
var cache_mutex: std.Thread.Mutex = .{};

/// Clear file content cache between requests — ensures fresh content on each type check.
/// Existence caches are kept (file/dir existence rarely changes within a daemon session).
/// Game-style frame reset: discard mutable state, keep structural state.
export fn edgebox_clear_file_cache() void {
    cache_mutex.lock();
    defer cache_mutex.unlock();
    // Don't free entries — they may be referenced by V8 strings.
    // Just clear the map so next reads go to disk.
    file_cache.clearRetainingCapacity();
}

fn cacheReadFile(path: []const u8) ![]const u8 {
    {
        cache_mutex.lock();
        defer cache_mutex.unlock();
        if (file_cache.get(path)) |cached| return cached;
    }
    const raw = std.fs.cwd().readFileAlloc(alloc, path, 50 * 1024 * 1024) catch |err| return err;
    // Append NUL terminator for kj::StringPtr compatibility (V8 pool requires it)
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
// Existence cache — HashMap for correctness (no hash collisions).
// Shared across workers. Populated on first access, reused by all.
// Key: path string, Value: true=exists, false=not exists
// Separate caches for files and directories.
var file_exist_cache: std.StringHashMapUnmanaged(bool) = .{};
var dir_exist_cache: std.StringHashMapUnmanaged(bool) = .{};
var exist_rwlock: std.Thread.RwLock = .{};

var io_file_exists_calls: std.atomic.Value(u64) = std.atomic.Value(u64).init(0);
var io_file_exists_cached: std.atomic.Value(u64) = std.atomic.Value(u64).init(0);
var io_dir_exists_calls: std.atomic.Value(u64) = std.atomic.Value(u64).init(0);
var io_dir_exists_cached: std.atomic.Value(u64) = std.atomic.Value(u64).init(0);

export fn edgebox_io_stats(fe_calls: *u64, fe_cached: *u64, de_calls: *u64, de_cached: *u64) void {
    fe_calls.* = io_file_exists_calls.load(.monotonic);
    fe_cached.* = io_file_exists_cached.load(.monotonic);
    de_calls.* = io_dir_exists_calls.load(.monotonic);
    de_cached.* = io_dir_exists_cached.load(.monotonic);
}

// ── Module Resolution Cache (shared across workers) ──
// Key: "importName\0containingFile" → Value: resolvedFilePath (or empty for failed)
// Workers 2+3 find cached results from worker 1's resolutions.
// Saves ~130ms per extra worker (191ms × 2/3 cache hit rate).
var resolve_cache: std.StringHashMapUnmanaged([]const u8) = .{};
var resolve_rwlock: std.Thread.RwLock = .{};

/// Look up a cached module resolution. Returns path length (0 = miss, -1 = cached failure).
export fn edgebox_resolve_cache_get(key_ptr: [*]const u8, key_len: c_int, out_ptr: *?[*]const u8, out_len: *c_int) c_int {
    if (key_len <= 0) return 0;
    const key = key_ptr[0..@intCast(key_len)];
    resolve_rwlock.lockShared();
    defer resolve_rwlock.unlockShared();
    if (resolve_cache.get(key)) |val| {
        if (val.len == 0) {
            // Cached failure (module not found)
            out_ptr.* = null;
            out_len.* = 0;
            return -1;
        }
        out_ptr.* = val.ptr;
        out_len.* = @intCast(val.len);
        return 1;
    }
    return 0; // miss
}

/// Resolve a relative import in Zig — native speed, no V8 overhead.
/// Uses edgebox_file_exists (SAME function TSC uses via ts.sys.fileExists)
/// to ensure identical behavior. Tries extensions: .ts, .tsx, /index.ts, etc.
/// Returns 1 on success (out_ptr/out_len set), 0 on failure.
export fn edgebox_resolve_relative(
    import_ptr: [*]const u8,
    import_len: c_int,
    dir_ptr: [*]const u8,
    dir_len: c_int,
    out_ptr: *?[*]const u8,
    out_len: *c_int,
) c_int {
    if (import_len <= 0 or dir_len <= 0) return 0;
    const import_name = import_ptr[0..@intCast(import_len)];
    const dir = dir_ptr[0..@intCast(dir_len)];

    const base = std.fmt.allocPrint(alloc, "{s}/{s}", .{ dir, import_name }) catch return 0;
    defer alloc.free(base);

    // Try extensions in TypeScript's bundler resolution order
    const extensions = [_][]const u8{ ".ts", ".tsx", "/index.ts", "/index.tsx", ".d.ts" };
    for (extensions) |ext| {
        const candidate = std.fmt.allocPrint(alloc, "{s}{s}", .{ base, ext }) catch continue;
        // Use edgebox_file_exists — SAME function as ts.sys.fileExists
        // This ensures identical behavior (same cache, same checks)
        const exists = edgebox_file_exists(candidate.ptr, @intCast(candidate.len));
        if (exists == 1) {
            out_ptr.* = candidate.ptr;
            out_len.* = @intCast(candidate.len);
            // Store in resolve cache for other workers
            const import_key = std.fmt.allocPrint(alloc, "{s}\x00{s}", .{ import_name, dir }) catch return 1;
            resolve_rwlock.lock();
            defer resolve_rwlock.unlock();
            resolve_cache.put(alloc, import_key, candidate) catch {};
            return 1;
        }
        alloc.free(candidate);
    }
    return 0;
}

/// Store a module resolution result. path_len=0 means resolution failed.
export fn edgebox_resolve_cache_set(key_ptr: [*]const u8, key_len: c_int, path_ptr: [*]const u8, path_len: c_int) void {
    if (key_len <= 0) return;
    const key_src = key_ptr[0..@intCast(key_len)];
    const key = alloc.dupe(u8, key_src) catch return;
    const val = if (path_len > 0)
        alloc.dupe(u8, path_ptr[0..@intCast(path_len)]) catch return
    else
        &[_]u8{};
    resolve_rwlock.lock();
    defer resolve_rwlock.unlock();
    resolve_cache.put(alloc, key, val) catch {};
}

export fn edgebox_file_exists(path_ptr: [*]const u8, path_len: c_int) c_int {
    if (path_len <= 0) return 0;
    const path = path_ptr[0..@intCast(path_len)];
    _ = io_file_exists_calls.fetchAdd(1, .monotonic);
    // Check existence cache (HashMap — no collisions)
    {
        exist_rwlock.lockShared();
        defer exist_rwlock.unlockShared();
        if (file_exist_cache.get(path)) |exists| {
            _ = io_file_exists_cached.fetchAdd(1, .monotonic);
            return if (exists) @as(c_int, 1) else @as(c_int, 0);
        }
    }
    // Also check file content cache — if file was read, it exists
    {
        cache_mutex.lock();
        defer cache_mutex.unlock();
        if (file_cache.get(path) != null) {
            _ = io_file_exists_cached.fetchAdd(1, .monotonic);
            return 1;
        }
    }
    const exists = blk: {
        std.fs.cwd().access(path, .{}) catch break :blk false;
        break :blk true;
    };
    {
        exist_rwlock.lock();
        defer exist_rwlock.unlock();
        const key = alloc.dupe(u8, path) catch return if (exists) @as(c_int, 1) else @as(c_int, 0);
        file_exist_cache.put(alloc, key, exists) catch {};
    }
    return if (exists) @as(c_int, 1) else @as(c_int, 0);
}

/// Check if directory exists. Returns 1=yes, 0=no. Cached via HashMap.
export fn edgebox_dir_exists(path_ptr: [*]const u8, path_len: c_int) c_int {
    if (path_len <= 0) return 0;
    const path = path_ptr[0..@intCast(path_len)];
    _ = io_dir_exists_calls.fetchAdd(1, .monotonic);
    {
        exist_rwlock.lockShared();
        defer exist_rwlock.unlockShared();
        if (dir_exist_cache.get(path)) |exists| {
            _ = io_dir_exists_cached.fetchAdd(1, .monotonic);
            return if (exists) @as(c_int, 1) else @as(c_int, 0);
        }
    }
    const exists = blk: {
        var dir = std.fs.cwd().openDir(path, .{}) catch break :blk false;
        dir.close();
        break :blk true;
    };
    {
        exist_rwlock.lock();
        defer exist_rwlock.unlock();
        const key = alloc.dupe(u8, path) catch return if (exists) @as(c_int, 1) else @as(c_int, 0);
        dir_exist_cache.put(alloc, key, exists) catch {};
    }
    return if (exists) @as(c_int, 1) else @as(c_int, 0);
}

/// Stat file. Returns JSON: {"isFile":true,"isDirectory":false,"size":N}
/// Uses thread-local buffer to avoid mmap per call.
threadlocal var stat_buf: [192]u8 = undefined;

export fn edgebox_stat(path_ptr: [*]const u8, path_len: c_int, out_len: *c_int) ?[*]const u8 {
    if (path_len <= 0) { out_len.* = 0; return null; }
    const path = path_ptr[0..@intCast(path_len)];
    const stat = std.fs.cwd().statFile(path) catch { out_len.* = 0; return null; };
    const mtime_ns: i128 = stat.mtime;
    const mtime_ms: i64 = if (mtime_ns != 0) @intCast(@divFloor(mtime_ns, std.time.ns_per_ms)) else 0;
    const result = std.fmt.bufPrint(&stat_buf,
        "{{\"isFile\":{s},\"isDirectory\":{s},\"size\":{d},\"mtimeMs\":{d}}}",
        .{
            if (stat.kind == .file) "true" else "false",
            if (stat.kind == .directory) "true" else "false",
            stat.size,
            mtime_ms,
        },
    ) catch { out_len.* = 0; return null; };
    out_len.* = @intCast(result.len);
    return result.ptr;
}

/// Read directory entries. Returns JSON: {"f":["file1"],"d":["dir1"]}
/// Pre-classifies entries as file or directory.
export fn edgebox_readdir(path_ptr: [*]const u8, path_len: c_int, out_len: *c_int) ?[*]const u8 {
    if (path_len <= 0) { out_len.* = 0; return null; }
    const path = path_ptr[0..@intCast(path_len)];
    var dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch { out_len.* = 0; return null; };
    defer dir.close();
    var result: std.ArrayListUnmanaged(u8) = .{};
    result.appendSlice(alloc, "{\"f\":[") catch { out_len.* = 0; return null; };
    var first_f = true;
    var dirs_buf: std.ArrayListUnmanaged(u8) = .{};
    var first_d = true;
    var it = dir.iterate();
    while (it.next() catch null) |entry| {
        if (entry.kind == .directory) {
            if (!first_d) dirs_buf.append(alloc, ',') catch {};
            first_d = false;
            dirs_buf.append(alloc, '"') catch {};
            dirs_buf.appendSlice(alloc, entry.name) catch {};
            dirs_buf.append(alloc, '"') catch {};
        } else {
            if (!first_f) result.append(alloc, ',') catch {};
            first_f = false;
            result.append(alloc, '"') catch {};
            result.appendSlice(alloc, entry.name) catch {};
            result.append(alloc, '"') catch {};
        }
    }
    result.appendSlice(alloc, "],\"d\":[") catch {};
    if (dirs_buf.items.len > 0) {
        result.appendSlice(alloc, dirs_buf.items) catch {};
    }
    dirs_buf.deinit(alloc);
    result.appendSlice(alloc, "]}") catch {};
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
// Edgebox root directory (set by v8_pool.zig at init, resolved from executable path)
var eb_root_ptr: ?[*]const u8 = null;
var eb_root_len: c_int = 0;

export fn edgebox_set_root(ptr: [*]const u8, len: c_int) void {
    const copy = alloc.alloc(u8, @intCast(len)) catch return;
    @memcpy(copy, ptr[0..@intCast(len)]);
    eb_root_ptr = copy.ptr;
    eb_root_len = @intCast(copy.len);
}

export fn edgebox_root(out_len: *c_int) ?[*]const u8 {
    if (eb_root_ptr) |p| {
        out_len.* = eb_root_len;
        return p;
    }
    // Fallback to CWD
    return edgebox_cwd(out_len);
}

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

// In daemon/pool mode, exit must NOT kill the process.
// Set to true when running as daemon.
var daemon_mode: bool = false;

export fn edgebox_set_daemon_mode(enabled: c_int) void {
    daemon_mode = enabled != 0;
}

export fn edgebox_exit(code: c_int) void {
    if (daemon_mode) return; // Don't kill daemon
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
    _ = std.posix.write(2, "[zig] dispatch_work\n") catch {};
    if (cwd_len <= 0 or worker_count <= 0) return;
    const cwd = cwd_ptr[0..@intCast(cwd_len)];
    const n: u32 = @intCast(@min(worker_count, MAX_WORKERS));

    for (0..n) |i| {
        work_slots[i].cwd = cwd;
        work_slots[i].worker_count = n;
        if (work_slots[i].result) |old| alloc.free(@constCast(old));
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
    _ = std.posix.write(2, "[zig] wait_for_work ENTER\n") catch {};
    if (worker_id < 0 or worker_id >= MAX_WORKERS) { out_len.* = 0; return null; }
    const wid: usize = @intCast(worker_id);

    work_mutex.lock();
    while (!work_slots[wid].ready.load(.acquire)) {
        work_cond.timedWait(&work_mutex, 100 * std.time.ns_per_ms) catch {};
    }
    work_mutex.unlock();
    _ = std.posix.write(2, "[zig] wait_for_work WOKE\n") catch {};

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

/// Worker calls this: APPEND results to shared buffer (can be called multiple times).
export fn edgebox_submit_result(worker_id: c_int, data_ptr: [*]const u8, data_len: c_int) void {
    if (worker_id < 0 or worker_id >= MAX_WORKERS or data_len <= 0) return;
    const wid: usize = @intCast(worker_id);
    const new_data = data_ptr[0..@intCast(data_len)];

    if (work_slots[wid].result) |existing| {
        // Append to existing
        const combined = alloc.alloc(u8, existing.len + new_data.len) catch return;
        @memcpy(combined[0..existing.len], existing);
        @memcpy(combined[existing.len..], new_data);
        alloc.free(@constCast(existing));
        work_slots[wid].result = combined;
    } else {
        const copy = alloc.alloc(u8, new_data.len) catch return;
        @memcpy(copy, new_data);
        work_slots[wid].result = copy;
    }
}

/// Worker calls this when done — signals main to collect.
export fn edgebox_worker_done(worker_id: c_int) void {
    if (worker_id < 0 or worker_id >= MAX_WORKERS) return;
    work_slots[@intCast(worker_id)].done.store(true, .release);
    work_mutex.lock();
    work_cond.broadcast();
    work_mutex.unlock();
}

/// Check if worker is done
export fn edgebox_is_worker_done(worker_id: c_int) c_int {
    if (worker_id < 0 or worker_id >= MAX_WORKERS) return 1;
    return if (work_slots[@intCast(worker_id)].done.load(.acquire)) 1 else 0;
}

/// Read submitted result for a worker (called by v8_pool collect)
export fn edgebox_get_result(worker_id: c_int, out_len: *c_int) ?[*]const u8 {
    if (worker_id < 0 or worker_id >= MAX_WORKERS) { out_len.* = 0; return null; }
    const wid: usize = @intCast(worker_id);
    if (work_slots[wid].result) |data| {
        out_len.* = @intCast(data.len);
        return data.ptr;
    }
    out_len.* = 0;
    return null;
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

// Type data model fully migrated to WasmGC (type_flags_gc.wasm, soa_gc.wasm).

// ── Work-Stealing File Counter ──
// Workers atomically claim the next file index instead of static sharding.
// Fast workers process more files; slow workers don't bottleneck.
var work_file_index: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);

/// Claim next file index for work-stealing.
/// Uses block allocation (4 files at a time) for better TSC cache locality.
/// Workers check consecutive files → dependency-ordered types stay in cache.
export fn edgebox_claim_file() u32 {
    return work_file_index.fetchAdd(1, .monotonic);
}

/// Claim a block of files. Returns first index; caller checks first..first+block_size.
export fn edgebox_claim_block(block_size: u32) u32 {
    return work_file_index.fetchAdd(block_size, .monotonic);
}

const shared_ast = @import("shared_ast.zig");

export fn edgebox_reset_work() void {
    work_file_index.store(0, .release);
    phase_channel.reset();
    shared_ast.resetAll();
}

// ── Phase Channel for Coordinated Cold Start ──
// Worker 0 sends on channel after createProgram finishes.
// Workers 1-N block on recv (proper condvar, no spin-wait).
// This lets worker 0 populate shared file/existence caches first.
const Channel = @import("channel.zig").Channel;
var phase_channel: Channel(u32) = Channel(u32).init();

export fn edgebox_signal_program_ready() void {
    // Send N-1 signals (one per waiting worker)
    var i: u32 = 0;
    while (i < 16) : (i += 1) { // send enough for max workers
        _ = phase_channel.send(1);
    }
}

export fn edgebox_wait_program_ready() void {
    _ = phase_channel.recv(); // blocks on condvar until signal
}

// ── Shared Cache (cross-worker, lock-free for reads) ──
// Generic key→value cache shared across all worker threads.
// EdgeBox provides the cache; recipes use it for cross-worker deduplication.
// Example: TSC recipe caches type relation results so worker 1 can reuse
// worker 0's computation of isTypeRelatedTo(TypeA, TypeB).

const SHARED_CACHE_SIZE: u32 = 262144; // 256K entries, power of 2

// Type relation cache: stores exact (source_id, target_id, relation) → result.
// Layout per u64 entry: [source_id:20 | target_id:20 | relation:4 | result:4 | tag:16]
// No hash collisions — exact ID comparison. Slot collisions = cache miss (safe).
// tag = 0xEB to distinguish populated entries from empty (0).
var shared_cache: [SHARED_CACHE_SIZE]std.atomic.Value(u64) = [_]std.atomic.Value(u64){std.atomic.Value(u64).init(0)} ** SHARED_CACHE_SIZE;

const TAG: u64 = 0xEB;

fn packEntry(src_id: u32, tgt_id: u32, rel: u32, result: u32) u64 {
    return (@as(u64, src_id & 0xFFFFF) << 44) |
           (@as(u64, tgt_id & 0xFFFFF) << 24) |
           (@as(u64, rel & 0xF) << 20) |
           (@as(u64, result & 0xF) << 16) |
           TAG;
}

fn slotIndex(src_id: u32, tgt_id: u32, rel: u32) u32 {
    return ((src_id *% 2654435761 +% tgt_id *% 2246822519 +% rel) & (SHARED_CACHE_SIZE - 1));
}

/// Get type relation from cache. Exact match on (src_id, tgt_id, rel).
/// Returns: 0 = miss, 1 = related, 2 = not related.
export fn edgebox_shared_cache_get(key: i32) callconv(.c) i32 {
    if (key == 0) return 0;
    const ukey = @as(u32, @bitCast(key));
    const idx = ukey & (SHARED_CACHE_SIZE - 1);
    const entry = shared_cache[idx].load(.acquire);
    if (entry == 0) return 0;
    const stored_key = @as(u32, @truncate(entry >> 32));
    if (stored_key == ukey) {
        return @as(i32, @bitCast(@as(u32, @truncate(entry & 0xFFFFFFFF))));
    }
    return 0;
}

/// Set type relation in cache.
export fn edgebox_shared_cache_set(key: i32, value: i32) callconv(.c) void {
    if (key == 0) return;
    const ukey = @as(u32, @bitCast(key));
    const uval = @as(u32, @bitCast(value));
    const idx = ukey & (SHARED_CACHE_SIZE - 1);
    const combined = (@as(u64, ukey) << 32) | @as(u64, uval);
    shared_cache[idx].store(combined, .release);
}

/// 3-param get: exact match on (src_id, tgt_id, rel). No hash collisions.
export fn edgebox_type_cache_get(src_id: i32, tgt_id: i32, rel: i32) callconv(.c) i32 {
    const s = @as(u32, @bitCast(src_id));
    const t = @as(u32, @bitCast(tgt_id));
    const r = @as(u32, @bitCast(rel));
    const idx = slotIndex(s, t, r);
    const entry = shared_cache[idx].load(.acquire);
    if (entry == 0) return 0;
    // Verify exact match: entry stores [src:20 | tgt:20 | rel:4 | result:4 | tag:16]
    if ((entry & 0xFF) != TAG) return 0;
    const e_src = @as(u32, @truncate(entry >> 44)) & 0xFFFFF;
    const e_tgt = @as(u32, @truncate(entry >> 24)) & 0xFFFFF;
    const e_rel = @as(u32, @truncate(entry >> 20)) & 0xF;
    if (e_src == (s & 0xFFFFF) and e_tgt == (t & 0xFFFFF) and e_rel == (r & 0xF)) {
        return @as(i32, @intCast((entry >> 16) & 0xF));
    }
    return 0;
}

/// 4-param set: stores exact (src_id, tgt_id, rel, result).
export fn edgebox_type_cache_set(src_id: i32, tgt_id: i32, rel: i32, result: i32) callconv(.c) void {
    const s = @as(u32, @bitCast(src_id));
    const t = @as(u32, @bitCast(tgt_id));
    const r = @as(u32, @bitCast(rel));
    const v = @as(u32, @bitCast(result));
    const idx = slotIndex(s, t, r);
    const entry = packEntry(s, t, r, v);
    shared_cache[idx].store(entry, .release);
}

/// Clear shared cache.
export fn edgebox_shared_cache_clear() callconv(.c) void {
    for (&shared_cache) |*e| e.store(0, .release);
}

// All type data lives in WasmGC modules:
//   type_flags_gc.wasm — GC array for type flags (TurboFan-inlinable array.get/set)
//   soa_gc.wasm — GC structs for auto SOA + columns (TurboFan-inlinable struct.get/set)
