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
pub threadlocal var deferred_exit_code: ?u8 = null;

/// Global IO mutex — protects all hash map caches for multi-isolate safety.
var io_mutex: std.Thread.Mutex = .{};

/// Set to true in worker threads to skip PumpMessageLoop
pub threadlocal var is_worker_thread: bool = false;

/// Pump throttle — only call pumpMessageLoop every N IO callbacks.
/// During Check phase, TurboFan has already compiled hot functions;
/// pumping on every IO call wastes ~100µs per call × thousands of calls.
var pump_counter: u32 = 0;
const PUMP_INTERVAL: u32 = 64; // pump every 64th IO call

inline fn maybePumpMessageLoop(isolate: *v8.Isolate) void {
    if (is_worker_thread) return;
    pump_counter +%= 1;
    if (pump_counter & (PUMP_INTERVAL - 1) != 0) return;
    if (v8.global_platform) |platform| {
        while (v8.pumpMessageLoop(platform, isolate)) {}
    }
}

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

    // __edgebox_write_stdout(str) — fast stdout write (no JSON)
    const ws_tmpl = v8.FunctionTemplateApi.create(isolate, &writeStdoutFastCallback) orelse return;
    const ws_func = v8.FunctionTemplateApi.getFunction(ws_tmpl, context) orelse return;
    const ws_key = v8.StringApi.fromUtf8(isolate, "__edgebox_write_stdout") orelse return;
    _ = v8.ObjectApi.set(global, context, @ptrCast(ws_key), @ptrCast(ws_func));

    // __edgebox_write_stderr(str) — fast stderr write (no JSON)
    const we_tmpl = v8.FunctionTemplateApi.create(isolate, &writeStderrFastCallback) orelse return;
    const we_func = v8.FunctionTemplateApi.getFunction(we_tmpl, context) orelse return;
    const we_key = v8.StringApi.fromUtf8(isolate, "__edgebox_write_stderr") orelse return;
    _ = v8.ObjectApi.set(global, context, @ptrCast(we_key), @ptrCast(we_func));

    // __edgebox_readdir(path) → string (JSON array of names, fast path)
    const rd_tmpl = v8.FunctionTemplateApi.create(isolate, &readdirFastCallback) orelse return;
    const rd_func = v8.FunctionTemplateApi.getFunction(rd_tmpl, context) orelse return;
    const rd_key = v8.StringApi.fromUtf8(isolate, "__edgebox_readdir") orelse return;
    _ = v8.ObjectApi.set(global, context, @ptrCast(rd_key), @ptrCast(rd_func));

    // __edgebox_dir_exists(path) → boolean (fast path for directoryExists)
    const de_tmpl = v8.FunctionTemplateApi.create(isolate, &dirExistsFastCallback) orelse return;
    const de_func = v8.FunctionTemplateApi.getFunction(de_tmpl, context) orelse return;
    const de_key = v8.StringApi.fromUtf8(isolate, "__edgebox_dir_exists") orelse return;
    _ = v8.ObjectApi.set(global, context, @ptrCast(de_key), @ptrCast(de_func));

    // __edgebox_realpath(path) → string|undefined (fast path for realpath)
    const rp_tmpl = v8.FunctionTemplateApi.create(isolate, &realpathFastCallback) orelse return;
    const rp_func = v8.FunctionTemplateApi.getFunction(rp_tmpl, context) orelse return;
    const rp_key = v8.StringApi.fromUtf8(isolate, "__edgebox_realpath") orelse return;
    _ = v8.ObjectApi.set(global, context, @ptrCast(rp_key), @ptrCast(rp_func));
}

/// Fast readFile: path → string (no JSON serialize/parse overhead)
/// Buffered stdout — reduce syscalls for TSC diagnostic output (14K+ lines)
/// Note: not thread-local because ArrayListUnmanaged uses page_allocator
/// which is thread-safe. Multiple isolates write to same stdout.
var stdout_buf: std.ArrayListUnmanaged(u8) = .{};
var stdout_mutex: std.Thread.Mutex = .{};
const STDOUT_BUF_SIZE: usize = 65536; // 64KB buffer, flush when full

/// Background stdout flush thread
var stdout_flush_thread: ?std.Thread = null;
var stdout_flush_data: ?[]u8 = null;
var stdout_flush_mutex: std.Thread.Mutex = .{};

fn flushStdoutWorker() void {
    if (stdout_flush_data) |data| {
        const stdout = std.fs.File.stdout();
        stdout.writeAll(data) catch {};
        alloc.free(data);
        stdout_flush_mutex.lock();
        stdout_flush_data = null;
        stdout_flush_mutex.unlock();
    }
}

fn flushStdout() void {
    if (stdout_buf.items.len == 0) return;

    // Wait for previous flush to complete
    if (stdout_flush_thread) |t| { t.join(); stdout_flush_thread = null; }

    // Move buffer to flush data (swap instead of copy)
    stdout_flush_mutex.lock();
    const data = alloc.dupe(u8, stdout_buf.items) catch {
        stdout_flush_mutex.unlock();
        // Fallback: sync write
        const stdout = std.fs.File.stdout();
        stdout.writeAll(stdout_buf.items) catch {};
        stdout_buf.clearRetainingCapacity();
        return;
    };
    stdout_flush_data = data;
    stdout_flush_mutex.unlock();
    stdout_buf.clearRetainingCapacity();

    // Flush on background thread
    stdout_flush_thread = std.Thread.spawn(.{}, flushStdoutWorker, .{}) catch {
        // Fallback: sync write
        const stdout = std.fs.File.stdout();
        stdout.writeAll(data) catch {};
        alloc.free(data);
        stdout_flush_data = null;
        return;
    };
}

/// Fast stdout write — buffered, no JSON
pub fn writeStdoutFastCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate = v8.CallbackInfoApi.getIsolate(info);
    if (v8.CallbackInfoApi.length(info) < 1) return;
    const arg0 = v8.CallbackInfoApi.get(info, 0) orelse return;
    if (!v8.ValueApi.isString(arg0)) return;

    const str: *const v8.String = @ptrCast(arg0);
    const len: usize = @intCast(v8.StringApi.utf8Length(str, isolate));
    if (len == 0) return;

    // Ensure capacity in the buffer
    var stack_buf: [4096]u8 = undefined;
    if (len <= stack_buf.len) {
        const written_count = v8.StringApi.writeUtf8(str, isolate, &stack_buf);
        stdout_mutex.lock();
        stdout_buf.ensureTotalCapacity(alloc, stdout_buf.items.len + written_count) catch {
            flushStdout();
            stdout_buf.ensureTotalCapacity(alloc, written_count) catch { stdout_mutex.unlock(); return; };
        };
        stdout_buf.appendSlice(alloc, stack_buf[0..written_count]) catch {};
        if (stdout_buf.items.len >= STDOUT_BUF_SIZE) flushStdout();
        stdout_mutex.unlock();
    } else {
        const heap_buf = alloc.alloc(u8, len) catch return;
        defer alloc.free(heap_buf);
        const written_count = v8.StringApi.writeUtf8(str, isolate, heap_buf);
        stdout_mutex.lock();
        stdout_buf.appendSlice(alloc, heap_buf[0..written_count]) catch {};
        if (stdout_buf.items.len >= STDOUT_BUF_SIZE) flushStdout();
        stdout_mutex.unlock();
    }
}

/// Buffered stderr — TSC outputs diagnostics to stderr
var stderr_buf: std.ArrayListUnmanaged(u8) = .{};

/// Flush all buffered output. Call at end of script execution.
pub fn flushAll() void {
    flushStdout();
    flushStderr();
    if (stdout_flush_thread) |t| { t.join(); stdout_flush_thread = null; }
    if (prefetch_thread) |t| { t.join(); prefetch_thread = null; }
    waitBatchPrefetch();
}

fn flushStderr() void {
    if (stderr_buf.items.len > 0) {
        const stderr = std.fs.File.stderr();
        stderr.writeAll(stderr_buf.items) catch {};
        stderr_buf.clearRetainingCapacity();
    }
}

/// Fast stderr write — buffered, no JSON
pub fn writeStderrFastCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate = v8.CallbackInfoApi.getIsolate(info);
    if (v8.CallbackInfoApi.length(info) < 1) return;
    const arg0 = v8.CallbackInfoApi.get(info, 0) orelse return;
    if (!v8.ValueApi.isString(arg0)) return;

    const str: *const v8.String = @ptrCast(arg0);
    const len: usize = @intCast(v8.StringApi.utf8Length(str, isolate));
    if (len == 0) return;

    stderr_buf.ensureTotalCapacity(alloc, stderr_buf.items.len + len) catch {
        flushStderr();
        stderr_buf.ensureTotalCapacity(alloc, len) catch return;
    };

    var stack_buf: [4096]u8 = undefined;
    if (len <= stack_buf.len) {
        const written = v8.StringApi.writeUtf8(str, isolate, &stack_buf);
        stderr_buf.appendSlice(alloc, stack_buf[0..written]) catch {};
    } else {
        const heap_buf = alloc.alloc(u8, len) catch return;
        defer alloc.free(heap_buf);
        const written = v8.StringApi.writeUtf8(str, isolate, heap_buf);
        stderr_buf.appendSlice(alloc, heap_buf[0..written]) catch {};
    }

    if (stderr_buf.items.len >= STDOUT_BUF_SIZE) {
        flushStderr();
    }
}

/// Fast readdir: returns JSON array string of {name, isDirectory} entries
/// Still uses JSON for the result (V8 needs to parse it) but avoids
/// the full JSON roundtrip of _ioSync.
pub fn readdirFastCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    if (v8.CallbackInfoApi.length(info) < 1) { rv.setUndefined(); return; }
    const arg0 = v8.CallbackInfoApi.get(info, 0) orelse { rv.setUndefined(); return; };
    if (!v8.ValueApi.isString(arg0)) { rv.setUndefined(); return; }

    const str: *const v8.String = @ptrCast(arg0);
    const len: usize = @intCast(v8.StringApi.utf8Length(str, isolate));
    if (len == 0 or len > 4096) { rv.setUndefined(); return; }

    var path_buf: [4096]u8 = undefined;
    const written = v8.StringApi.writeUtf8(str, isolate, &path_buf);
    const path = path_buf[0..written];

    // Check cache
    if (readdir_cache.get(path)) |cached| {
        const v8_str = v8.StringApi.fromUtf8(isolate, cached) orelse { rv.setUndefined(); return; };
        rv.set(@ptrCast(v8_str));
        return;
    }

    // Read directory
    var dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch { rv.setUndefined(); return; };
    defer dir.close();

    var result: std.ArrayListUnmanaged(u8) = .{};
    result.appendSlice(alloc, "[") catch { rv.setUndefined(); return; };

    var first = true;
    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (!first) result.append(alloc, ',') catch {};
        first = false;
        const is_dir = entry.kind == .directory;
        const item = std.fmt.allocPrint(alloc, "{{\"name\":\"{s}\",\"d\":{}}}", .{ entry.name, is_dir }) catch continue;
        defer alloc.free(item);
        result.appendSlice(alloc, item) catch {};
    }

    result.appendSlice(alloc, "]") catch {};
    const final = result.toOwnedSlice(alloc) catch { rv.setUndefined(); return; };

    // Cache
    const key = alloc.dupe(u8, path) catch path;
    readdir_cache.put(alloc, key, final) catch {};

    const v8_str = v8.StringApi.fromUtf8(isolate, final) orelse { rv.setUndefined(); return; };
    rv.set(@ptrCast(v8_str));
}

/// Raw realpath cache (for fast callback — stores resolved path strings)
var raw_realpath_cache: std.StringHashMapUnmanaged([]const u8) = .{};

/// Directory exists cache
var dir_exists_cache: std.StringHashMapUnmanaged(bool) = .{};

/// Fast directoryExists: path → boolean (no JSON)
pub fn dirExistsFastCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    if (v8.CallbackInfoApi.length(info) < 1) { rv.setInt32(0); return; }
    const arg0 = v8.CallbackInfoApi.get(info, 0) orelse { rv.setInt32(0); return; };
    if (!v8.ValueApi.isString(arg0)) { rv.setInt32(0); return; }

    const str: *const v8.String = @ptrCast(arg0);
    const len: usize = @intCast(v8.StringApi.utf8Length(str, isolate));
    if (len == 0 or len > 4096) { rv.setInt32(0); return; }

    var path_buf: [4096]u8 = undefined;
    const written = v8.StringApi.writeUtf8(str, isolate, &path_buf);
    const path = path_buf[0..written];

    io_mutex.lock();
    const dir_cached = dir_exists_cache.get(path);
    io_mutex.unlock();
    if (dir_cached) |exists| {
        rv.setInt32(if (exists) 1 else 0);
        return;
    }

    const exists = blk: {
        var dir = std.fs.cwd().openDir(path, .{}) catch break :blk false;
        dir.close();
        break :blk true;
    };

    const key = alloc.dupe(u8, path) catch path;
    io_mutex.lock();
    dir_exists_cache.put(alloc, key, exists) catch {};
    io_mutex.unlock();
    rv.setInt32(if (exists) 1 else 0);
}

/// Fast realpath: path → string|undefined (no JSON)
pub fn realpathFastCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    if (v8.CallbackInfoApi.length(info) < 1) { rv.setUndefined(); return; }
    const arg0 = v8.CallbackInfoApi.get(info, 0) orelse { rv.setUndefined(); return; };
    if (!v8.ValueApi.isString(arg0)) { rv.setUndefined(); return; }

    const str: *const v8.String = @ptrCast(arg0);
    const len: usize = @intCast(v8.StringApi.utf8Length(str, isolate));
    if (len == 0 or len > 4096) { rv.setUndefined(); return; }

    var path_buf: [4096]u8 = undefined;
    const written = v8.StringApi.writeUtf8(str, isolate, &path_buf);
    const path = path_buf[0..written];

    // Check raw realpath cache
    if (raw_realpath_cache.get(path)) |cached_real| {
        const v8_str = v8.StringApi.fromUtf8(isolate, cached_real) orelse { rv.setUndefined(); return; };
        rv.set(@ptrCast(v8_str));
        return;
    }

    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const real = std.fs.cwd().realpath(path, &buf) catch { rv.setUndefined(); return; };

    // Cache the raw result
    const key = alloc.dupe(u8, path) catch path;
    const val = alloc.dupe(u8, real) catch real;
    raw_realpath_cache.put(alloc, key, val) catch {};

    const v8_str = v8.StringApi.fromUtf8(isolate, real) orelse { rv.setUndefined(); return; };
    rv.set(@ptrCast(v8_str));
}

// ============================================================
// Speculative prefetch — predictive file read
// When fileExists returns true, spawn a Zig thread to pre-read
// the file content so readFile hits cache on the next call.
// ============================================================

var prefetch_thread: ?std.Thread = null;
var prefetch_path_buf: [4096]u8 = undefined;
var prefetch_path_len: usize = 0;

fn speculativePrefetchWorker() void {
    const path = prefetch_path_buf[0..prefetch_path_len];
    if (raw_file_cache.get(path) != null) return; // Already cached

    const file = std.fs.cwd().openFile(path, .{}) catch return;
    defer file.close();
    const data = file.readToEndAlloc(alloc, 64 * 1024 * 1024) catch return;

    prefetch_mutex.lock();
    defer prefetch_mutex.unlock();
    const key = alloc.dupe(u8, path) catch return;
    raw_file_cache.put(alloc, key, data) catch {};
}

fn triggerSpeculativePrefetch(path: []const u8) void {
    // Join previous prefetch if still running
    if (prefetch_thread) |t| { t.join(); prefetch_thread = null; }

    // Start new prefetch
    if (path.len <= prefetch_path_buf.len) {
        @memcpy(prefetch_path_buf[0..path.len], path);
        prefetch_path_len = path.len;
        prefetch_thread = std.Thread.spawn(.{}, speculativePrefetchWorker, .{}) catch null;
    }
}

/// Raw file content cache (no JSON escaping — for fast callback)
var raw_file_cache: std.StringHashMapUnmanaged([]const u8) = .{};

/// Collect file paths for batch prefetch.
var prefetch_file_list: std.ArrayListUnmanaged([]const u8) = .{};

/// Queue all source files in a directory for batch prefetch.
pub fn queueDirectoryPrefetch(dir_path: []const u8) void {
    collectSourceFiles(dir_path, &prefetch_file_list);
}

/// Queue a file for batch prefetch.
pub fn prefetchFile(path: []const u8) void {
    const key = alloc.dupe(u8, path) catch return;
    prefetch_file_list.append(alloc, key) catch {};
}

/// Background threads for parallel batch prefetch
const MAX_BATCH_THREADS = 4;
var batch_threads: [MAX_BATCH_THREADS]?std.Thread = .{null} ** MAX_BATCH_THREADS;
var batch_thread_count: usize = 0;

fn batchPrefetchShard(items: []const []const u8, start: usize, end: usize) void {
    for (start..end) |i| {
        const path = items[i];
        io_mutex.lock();
        const already = raw_file_cache.get(path) != null;
        io_mutex.unlock();
        if (already) continue;

        const file = std.fs.cwd().openFile(path, .{}) catch continue;
        defer file.close();
        const data = file.readToEndAlloc(alloc, 64 * 1024 * 1024) catch continue;

        io_mutex.lock();
        raw_file_cache.put(alloc, path, data) catch {};
        io_mutex.unlock();
    }
}

/// Start parallel batch prefetch. Non-blocking — V8 starts while files load.
pub fn startBatchPrefetch() void {
    const n = prefetch_file_list.items.len;
    if (n == 0) return;
    const tc = @min(n, MAX_BATCH_THREADS);
    batch_thread_count = tc;
    const per = (n + tc - 1) / tc;
    for (0..tc) |i| {
        const s = i * per;
        const e = @min(s + per, n);
        if (s >= e) break;
        batch_threads[i] = std.Thread.spawn(.{}, batchPrefetchShard, .{ prefetch_file_list.items, s, e }) catch null;
    }
}

/// Wait for batch prefetch to complete.
pub fn waitBatchPrefetch() void {
    for (0..batch_thread_count) |i| {
        if (batch_threads[i]) |t| { t.join(); batch_threads[i] = null; }
    }
    batch_thread_count = 0;
}

pub fn readFileFastCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    // Pump on readFile — but less frequently than before.
    // readFile is called ~400 times; pump every 128th call = ~3 pumps total.
    {
        pump_counter +%= 1;
        if (!is_worker_thread and pump_counter & 127 == 0) {
            if (v8.global_platform) |platform| {
                while (v8.pumpMessageLoop(platform, isolate)) {}
            }
        }
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

    // Join speculative prefetch only if one is pending
    if (prefetch_thread) |t| { t.join(); prefetch_thread = null; }

    // Check raw content cache (thread-safe via mutex)
    io_mutex.lock();
    const cached_content = raw_file_cache.get(path);
    io_mutex.unlock();
    const content = cached_content orelse blk: {
        const file = std.fs.cwd().openFile(path, .{}) catch { rv.setUndefined(); return; };
        defer file.close();
        const data = file.readToEndAlloc(alloc, 64 * 1024 * 1024) catch { rv.setUndefined(); return; };
        const key = alloc.dupe(u8, path) catch path;
        io_mutex.lock();
        raw_file_cache.put(alloc, key, data) catch {};
        io_mutex.unlock();
        break :blk data;
    };

    // Create V8 string directly — no JSON escaping needed!
    // Use external string (zero-copy) for pure-ASCII files.
    // Files with non-ASCII bytes MUST use fromUtf8 (copies, but handles UTF-8).
    // fromExternalOneByte treats bytes as Latin-1, corrupting multi-byte UTF-8.
    // Fast ASCII check: process 8 bytes at a time using bitwise OR.
    // Any byte >= 0x80 sets the high bit. OR accumulates → single check.
    const is_ascii = blk: {
        var i: usize = 0;
        // 8-byte fast path
        while (i + 8 <= content.len) : (i += 8) {
            const chunk = std.mem.readInt(u64, content[i..][0..8], .little);
            if (chunk & 0x8080808080808080 != 0) break :blk false;
        }
        // Remaining bytes
        while (i < content.len) : (i += 1) {
            if (content[i] >= 0x80) break :blk false;
        }
        break :blk true;
    };
    const v8_str = if (is_ascii and content.len > 128)
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
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    // No pump here — fileExists is called thousands of times during Program phase.
    // TurboFan pumping happens in readFile and at Check phase boundaries.

    if (v8.CallbackInfoApi.length(info) < 1) { rv.setUndefined(); return; }
    const arg0 = v8.CallbackInfoApi.get(info, 0) orelse { rv.setUndefined(); return; };
    if (!v8.ValueApi.isString(arg0)) { rv.setUndefined(); return; }

    const isolate = v8.CallbackInfoApi.getIsolate(info);
    const str: *const v8.String = @ptrCast(arg0);
    const len: usize = @intCast(v8.StringApi.utf8Length(str, isolate));
    if (len == 0 or len > 4096) { rv.setUndefined(); return; }

    var path_buf: [4096]u8 = undefined;
    const written = v8.StringApi.writeUtf8(str, isolate, &path_buf);
    const path = path_buf[0..written];

    // Hot path: cache lookup (no mutex needed in single-threaded TSC mode —
    // batch prefetch completed before TSC starts)
    io_mutex.lock();
    const cached = exists_cache.get(path);
    const dir_check = if (cached == null) blk: {
        // Fast negative: check if directory was scanned
        const dir = std.fs.path.dirname(path);
        if (dir) |d| {
            break :blk scanned_dirs.get(d) != null;
        }
        break :blk false;
    } else false;
    io_mutex.unlock();

    if (cached) |exists| {
        rv.setInt32(if (exists) 1 else 0);
        return;
    }
    if (dir_check) {
        // Directory was scanned — path not in exists_cache = doesn't exist
        const key = alloc.dupe(u8, path) catch path;
        io_mutex.lock();
        exists_cache.put(alloc, key, false) catch {};
        io_mutex.unlock();
        rv.setInt32(0);
        return;
    }

    const exists = blk: {
        _ = std.fs.cwd().statFile(path) catch break :blk false;
        break :blk true;
    };

    const key = alloc.dupe(u8, path) catch path;
    io_mutex.lock();
    exists_cache.put(alloc, key, exists) catch {};
    io_mutex.unlock();

    // Speculative prefetch: if file exists and is a source file,
    // start reading it on a background Zig thread.
    // TSC pattern: fileExists(path) → true → readFile(path)
    if (exists and (std.mem.endsWith(u8, path, ".ts") or
        std.mem.endsWith(u8, path, ".tsx") or
        std.mem.endsWith(u8, path, ".d.ts") or
        std.mem.endsWith(u8, path, ".js") or
        std.mem.endsWith(u8, path, ".json")))
    {
        triggerSpeculativePrefetch(path);
    }

    rv.setInt32(if (exists) 1 else 0);
}

/// The V8 callback for __edgebox_io_sync(jsonString) → jsonString
/// The V8 callback for __edgebox_io_sync. Public so v8_runner.zig can use
/// its address in the snapshot external_references array.
pub fn ioSyncCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate: *v8.Isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    // Throttled pump — TurboFan background tasks drain every 64th IO call
    maybePumpMessageLoop(isolate);

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
        flushStdout();
        flushStderr();
        // Wait for async stdout flush to complete before exit
        if (stdout_flush_thread) |t| { t.join(); stdout_flush_thread = null; }
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

/// Directories we've fully scanned — fileExists can skip stat() for paths in these dirs.
var scanned_dirs: std.StringHashMapUnmanaged(void) = .{};

/// Read files into raw_file_cache + exists_cache (for fast IO callbacks).
fn prefetchWorker(paths: []const []const u8, start: usize, end: usize) void {
    for (start..end) |i| {
        const path = paths[i];
        const file = std.fs.cwd().openFile(path, .{}) catch continue;
        defer file.close();
        const content = file.readToEndAlloc(alloc, 64 * 1024 * 1024) catch continue;

        const key = alloc.dupe(u8, path) catch continue;
        io_mutex.lock();
        raw_file_cache.put(alloc, key, content) catch {};
        exists_cache.put(alloc, key, true) catch {};
        io_mutex.unlock();

        // Mark directory as scanned — enables fast negative fileExists
        const dir = std.fs.path.dirname(path) orelse continue;
        io_mutex.lock();
        const dir_already = scanned_dirs.get(dir) != null;
        io_mutex.unlock();
        if (!dir_already) {
            // Scan directory listing — cache ALL entries (positive)
            // Any path not in the listing is negative (doesn't exist)
            var d = std.fs.cwd().openDir(dir, .{ .iterate = true }) catch continue;
            defer d.close();
            var iter = d.iterate();
            while (iter.next() catch null) |entry| {
                if (entry.kind != .file) continue;
                const full = std.fmt.allocPrint(alloc, "{s}/{s}", .{ dir, entry.name }) catch continue;
                io_mutex.lock();
                if (exists_cache.get(full) == null) {
                    exists_cache.put(alloc, full, true) catch {};
                }
                io_mutex.unlock();
            }
            const dir_key = alloc.dupe(u8, dir) catch continue;
            io_mutex.lock();
            scanned_dirs.put(alloc, dir_key, {}) catch {};
            io_mutex.unlock();
        }
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
