// v8_parallel_tsc.zig — Parallel type checking via multiple V8 isolates
//
// Architecture (phase-split):
// 1. Main isolate: createProgram (parse + bind all files) — populates Zig file cache
// 2. After createProgram: __edgebox_spawn_check_workers callback fires
// 3. Zig spawns N-1 worker threads with snapshot isolates
// 4. Workers: createProgram (instant — files already in Zig cache) + check assigned files
// 5. Main: checks its assigned files (worker 0)
// 6. All workers finish → diagnostics merged via shared stdout
//
// Key: workers start AFTER main's createProgram, so all file reads hit cache.
// No IO contention during workers' createProgram phase.

const std = @import("std");
const v8 = @import("v8.zig");
const v8_io = @import("v8_io.zig");

const alloc = std.heap.page_allocator;
const max_check_workers = 8;

const embedded_snapshot = @embedFile("v8_bootstrap.snapshot");

/// External references — MUST match snapshot_gen.zig exactly (same order, same count).
var external_refs: [11]usize = .{0} ** 11;
var ext_refs_initialized = false;

fn getExternalRefs() [*]const usize {
    if (!ext_refs_initialized) {
        external_refs[0] = @intFromPtr(&v8_io.ioSyncCallback);
        external_refs[1] = @intFromPtr(&v8_io.ioBatchCallback);
        external_refs[2] = @intFromPtr(&v8_io.readFileFastCallback);
        external_refs[3] = @intFromPtr(&v8_io.fileExistsFastCallback);
        external_refs[4] = @intFromPtr(&v8_io.writeStdoutFastCallback);
        external_refs[5] = @intFromPtr(&v8_io.writeStderrFastCallback);
        external_refs[6] = @intFromPtr(&v8_io.readdirFastCallback);
        external_refs[7] = @intFromPtr(&v8_io.dirExistsFastCallback);
        external_refs[8] = @intFromPtr(&v8_io.realpathFastCallback);
        external_refs[9] = 0; // null terminator
        ext_refs_initialized = true;
    }
    return &external_refs;
}

// Refresh process.argv/env after snapshot deserialization
const snapshot_init_js =
    \\(function() {
    \\  try { var r = __edgebox_io_sync('{"op":"argv"}'); if (r) { var d = JSON.parse(r); if (d.ok) process.argv = d.data; } } catch(e) {}
    \\  try { var r = __edgebox_io_sync('{"op":"env"}'); if (r) { var d = JSON.parse(r); if (d.ok) process.env = d.data; } } catch(e) {}
    \\})();
;

/// Shared state — set by main thread, read by workers
var g_worker_count: usize = 0;
var g_tsc_path: []const u8 = "";
var g_tsc_dir: []const u8 = "";
var g_tsc_fast_js: []const u8 = "";
var g_threads: [max_check_workers]?std.Thread = .{null} ** max_check_workers;

const CheckWorkerCtx = struct {
    worker_id: usize,
    worker_count: usize,
    tsc_path: []const u8,
    tsc_dir: []const u8,
    tsc_fast_js: []const u8,
};

var g_ctxs: [max_check_workers]CheckWorkerCtx = undefined;

fn checkWorkerFn(ctx: *CheckWorkerCtx) void {
    v8_io.is_worker_thread = true;

    // Snapshot isolate — TSC pre-compiled (~10ms startup)
    const iso = v8.SnapshotApi.createIsolateFromSnapshot(
        embedded_snapshot.ptr,
        @intCast(embedded_snapshot.len),
        getExternalRefs(),
    );
    defer v8.IsolateApi.dispose(iso);
    v8.IsolateApi.enter(iso);
    defer v8.IsolateApi.exit(iso);

    var handle_scope = v8.HandleScope.init(iso);
    defer handle_scope.deinit();
    const context = v8.ContextApi.create(iso);
    v8.ContextApi.enter(context);
    defer v8.ContextApi.exit(context);

    _ = v8.eval(iso, context, snapshot_init_js, "snapshot_init.js") catch {};

    var init_buf: [2048]u8 = undefined;
    const init_js = std.fmt.bufPrint(&init_buf,
        "globalThis.__edgebox_worker_id={d};" ++
            "globalThis.__edgebox_worker_count={d};" ++
            "globalThis.__filename=\"{s}\";" ++
            "globalThis.__dirname=\"{s}\";" ++
            "globalThis.__sfCache=Object.create(null);",
        .{ ctx.worker_id, ctx.worker_count, ctx.tsc_path, ctx.tsc_dir },
    ) catch return;
    _ = v8.eval(iso, context, init_js, "check_worker_init.js") catch {};

    // Run TSC — createProgram reads from Zig cache
    const t0 = std.time.milliTimestamp();
    std.debug.print("[worker-{d}] start (+{d}ms)\n", .{ ctx.worker_id, t0 - g_start_time });
    _ = v8.eval(iso, context, ctx.tsc_fast_js, "tsc_check_worker.js") catch {};
    const t1 = std.time.milliTimestamp();
    std.debug.print("[worker-{d}] done ({d}ms total, +{d}ms)\n", .{ ctx.worker_id, t1 - t0, t1 - g_start_time });

    v8_io.flushAll();
}

/// V8 callback: __edgebox_spawn_check_workers()
/// Called from TSC transform AFTER createProgram completes.
/// Spawns N-1 worker threads. Main continues as worker 0.
var g_start_time: i64 = 0;

pub fn spawnCheckWorkersCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    _ = info;
    const now = std.time.milliTimestamp();
    std.debug.print("[parallel-tsc] createProgram done ({d}ms), spawning workers\n", .{now - g_start_time});

    if (g_worker_count <= 1) return;

    std.debug.print("[parallel-tsc] spawning {d} workers (after createProgram)\n", .{g_worker_count - 1});

    // Freeze IO caches — workers can read lock-free
    v8_io.prefetch_complete = true;

    for (1..g_worker_count) |i| {
        g_ctxs[i] = .{
            .worker_id = i,
            .worker_count = g_worker_count,
            .tsc_path = g_tsc_path,
            .tsc_dir = g_tsc_dir,
            .tsc_fast_js = g_tsc_fast_js,
        };
        g_threads[i] = std.Thread.spawn(.{}, checkWorkerFn, .{&g_ctxs[i]}) catch null;
    }
}

/// Set up parallel checking. Called from v8_runner.zig before TSC starts.
/// Registers __edgebox_spawn_check_workers callback + sets worker sharding.
/// Workers are NOT spawned here — they start when createProgram completes.
pub fn setupParallelCheck(
    isolate: *v8.Isolate,
    context: *const v8.Context,
    tsc_path: []const u8,
    tsc_fast_js: []const u8,
) bool {
    const cpu_count = std.Thread.getCpuCount() catch 4;
    g_worker_count = @min(cpu_count, 2);

    if (g_worker_count <= 1 or embedded_snapshot.len == 0) return false;

    g_start_time = std.time.milliTimestamp();
    std.debug.print("[parallel-tsc] {d} cores, {d} workers (deferred spawn)\n", .{ cpu_count, g_worker_count });

    g_tsc_path = tsc_path;
    g_tsc_dir = std.fs.path.dirname(tsc_path) orelse ".";
    g_tsc_fast_js = tsc_fast_js;

    // Set worker 0 sharding on main isolate
    var main_buf: [256]u8 = undefined;
    const main_init = std.fmt.bufPrint(&main_buf,
        "globalThis.__edgebox_worker_id=0;globalThis.__edgebox_worker_count={d};",
        .{g_worker_count},
    ) catch return false;
    _ = v8.eval(isolate, context, main_init, "w0_init.js") catch return false;

    // Register __edgebox_spawn_check_workers callback
    const global_obj = v8.ContextApi.global(context);
    const tmpl = v8.FunctionTemplateApi.create(isolate, &spawnCheckWorkersCallback) orelse return false;
    const func = v8.FunctionTemplateApi.getFunction(tmpl, context) orelse return false;
    const key = v8.StringApi.fromUtf8(isolate, "__edgebox_spawn_check_workers") orelse return false;
    _ = v8.ObjectApi.set(global_obj, context, @ptrCast(key), @ptrCast(func));

    return true;
}

/// Wait for all parallel workers to finish. Call after main TSC eval completes.
pub fn waitForWorkers() void {
    for (&g_threads) |*t| {
        if (t.*) |thread| {
            thread.join();
            t.* = null;
        }
    }
}
