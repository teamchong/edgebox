// v8_parallel_tsc.zig — Parallel type checking via multiple V8 isolates
//
// Architecture (A/B V8 idea):
// - Main isolate (A): snapshot-based, acts as worker 0
// - Worker isolates (B1..BN): snapshot-based (TSC pre-compiled, ~10ms startup)
// - Zig: the channel connecting A↔B via shared memory (file cache, stdout)
// - Each worker creates its own program (re-parses from Zig file cache)
// - Check loop sharding: worker only checks files where fileIndex % N === workerId
// - MapReduce: diagnostics fan out to workers, reduce back via shared stdout

const std = @import("std");
const v8 = @import("v8.zig");
const v8_io = @import("v8_io.zig");

const alloc = std.heap.page_allocator;
const max_check_workers = 8;

const embedded_snapshot = @embedFile("v8_bootstrap.snapshot");

/// External references — MUST match snapshot_gen.zig exactly (same order, same count).
/// Missing refs cause V8 to map wrong function pointers → segfault.
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

const CheckWorkerCtx = struct {
    worker_id: usize,
    worker_count: usize,
    tsc_path: []const u8,
    tsc_dir: []const u8,
    tsc_fast_js: []const u8,
};

fn checkWorkerFn(ctx: *CheckWorkerCtx) void {
    // Mark as worker thread — prevents pumpMessageLoop and speculative prefetch
    v8_io.is_worker_thread = true;

    // Create isolate from snapshot — TSC already compiled (~10ms startup)
    // CRITICAL: external_refs must match snapshot_gen.zig exactly (all 9 callbacks)
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

    // Refresh process.argv and process.env from OS args
    _ = v8.eval(iso, context, snapshot_init_js, "snapshot_init.js") catch {};

    // Set worker sharding globals + __filename/__dirname
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

    // Run TSC — snapshot has ts global, check loop will shard by worker_id
    std.debug.print("[worker-{d}] running TSC (snapshot)\n", .{ctx.worker_id});
    _ = v8.eval(iso, context, ctx.tsc_fast_js, "tsc_check_worker.js") catch {};
    std.debug.print("[worker-{d}] done (exit={any})\n", .{ ctx.worker_id, v8_io.deferred_exit_code });

    v8_io.flushAll();
}

/// Run TSC type checking in parallel across N worker isolates.
/// Main thread acts as worker 0 (snapshot), spawns N-1 workers (also snapshot).
///
/// Returns true if parallel checking was used, false if single-threaded.
pub fn runParallelCheck(
    main_isolate: *v8.Isolate,
    main_context: *const v8.Context,
    tsc_path: []const u8,
    tsc_fast_js: []const u8,
) bool {
    const cpu_count = std.Thread.getCpuCount() catch 4;
    const worker_count: usize = @min(cpu_count, max_check_workers);

    std.debug.print("[parallel-tsc] {d} cores, {d} workers\n", .{ cpu_count, worker_count });

    if (worker_count <= 1 or embedded_snapshot.len == 0) return false;

    const tsc_dir = std.fs.path.dirname(tsc_path) orelse ".";

    // Set worker sharding on main isolate (worker 0)
    var main_buf: [256]u8 = undefined;
    const main_init = std.fmt.bufPrint(&main_buf,
        "globalThis.__edgebox_worker_id=0;globalThis.__edgebox_worker_count={d};",
        .{worker_count},
    ) catch return false;
    _ = v8.eval(main_isolate, main_context, main_init, "w0_init.js") catch return false;

    // Spawn N-1 worker threads — each uses snapshot (TSC pre-compiled, ~10ms)
    var threads: [max_check_workers]?std.Thread = .{null} ** max_check_workers;
    var ctxs: [max_check_workers]CheckWorkerCtx = undefined;

    for (1..worker_count) |i| {
        ctxs[i] = .{
            .worker_id = i,
            .worker_count = worker_count,
            .tsc_path = tsc_path,
            .tsc_dir = tsc_dir,
            .tsc_fast_js = tsc_fast_js,
        };
        threads[i] = std.Thread.spawn(.{}, checkWorkerFn, .{&ctxs[i]}) catch null;
    }

    // Main thread runs as worker 0 (snapshot — instant TSC load)
    var try_catch = v8.TryCatch.init(main_isolate);
    defer try_catch.deinit();
    _ = v8.eval(main_isolate, main_context, tsc_fast_js, "tsc_fast.js") catch {};
    v8_io.flushAll();

    // Wait for all workers
    for (&threads) |*t| {
        if (t.*) |thread| {
            thread.join();
            t.* = null;
        }
    }

    return true;
}
