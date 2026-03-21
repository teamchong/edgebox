// v8_parallel_tsc.zig — Parallel type checking via multiple V8 isolates
//
// Architecture (your A/B V8 idea):
// - Main isolate (A): snapshot-based, acts as worker 0
// - Worker isolates (B1..BN): fresh isolates with pre-transformed TSC source
// - Zig: the channel connecting A↔B via shared memory (file cache, stdout)
// - Each worker creates its own program (re-parses from Zig file cache)
// - Check loop sharding: worker only checks files where fileIndex % N === workerId
// - MapReduce: diagnostics fan out to workers, reduce back via shared stdout

const std = @import("std");
const v8 = @import("v8.zig");
const v8_io = @import("v8_io.zig");

const alloc = std.heap.page_allocator;
const max_check_workers = 8;

const CheckWorkerCtx = struct {
    worker_id: usize,
    worker_count: usize,
    tsc_path: []const u8,
    tsc_dir: []const u8,
    transformed_tsc: []const u8, // Pre-transformed _tsc.js (shared read-only)
};

fn checkWorkerFn(ctx: *CheckWorkerCtx) void {
    // Mark as worker thread — prevents pumpMessageLoop crashes
    v8_io.is_worker_thread = true;

    // Fresh isolate — V8 snapshot crashes during heavy TSC on worker threads.
    // Pre-transformed source shared via Zig (zero-copy, read-only pointer).
    const iso = v8.IsolateApi.create();
    defer v8.IsolateApi.dispose(iso);
    v8.IsolateApi.enter(iso);
    defer v8.IsolateApi.exit(iso);

    var handle_scope = v8.HandleScope.init(iso);
    defer handle_scope.deinit();
    const context = v8.ContextApi.create(iso);
    v8.ContextApi.enter(context);
    defer v8.ContextApi.exit(context);

    // Bootstrap from source
    v8_io.registerGlobals(iso, context);
    const bootstrap_code = @embedFile("v8_bootstrap.js");
    _ = v8.eval(iso, context, bootstrap_code, "v8_bootstrap.js") catch return;

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

    // Load and run pre-transformed TSC (shared memory, zero transform cost)
    std.debug.print("[worker-{d}] loading TSC ({d}MB)\n", .{ ctx.worker_id, ctx.transformed_tsc.len / 1024 / 1024 });
    _ = v8.eval(iso, context, ctx.transformed_tsc, "_tsc_worker.js") catch {};
    std.debug.print("[worker-{d}] done (exit={any})\n", .{ ctx.worker_id, v8_io.deferred_exit_code });

    v8_io.flushAll();
}

/// Run TSC type checking in parallel across N worker isolates.
/// Main thread acts as worker 0 (snapshot), spawns N-1 workers (fresh isolates).
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

    if (worker_count <= 1) return false;

    const tsc_dir = std.fs.path.dirname(tsc_path) orelse ".";

    // Pre-transform TSC source ONCE, share across all workers (read-only).
    const tsc_real_path = blk: {
        if (std.mem.endsWith(u8, tsc_path, "/tsc.js")) {
            break :blk std.fmt.allocPrint(alloc, "{s}/_tsc.js", .{tsc_dir}) catch return false;
        }
        break :blk @as([]const u8, tsc_path);
    };

    const tsc_source = blk: {
        const f = std.fs.cwd().openFile(tsc_real_path, .{}) catch return false;
        defer f.close();
        break :blk f.readToEndAlloc(alloc, 64 * 1024 * 1024) catch return false;
    };
    defer alloc.free(tsc_source);

    const v8_tsc_transforms = @import("v8_tsc_transforms.zig");
    const transformed = v8_tsc_transforms.apply(alloc, tsc_source) catch return false;
    // Don't free — workers hold shared read-only pointer

    // Set worker sharding on main isolate (worker 0)
    var main_buf: [256]u8 = undefined;
    const main_init = std.fmt.bufPrint(&main_buf,
        "globalThis.__edgebox_worker_id=0;globalThis.__edgebox_worker_count={d};",
        .{worker_count},
    ) catch return false;
    _ = v8.eval(main_isolate, main_context, main_init, "w0_init.js") catch return false;

    // Spawn N-1 worker threads
    var threads: [max_check_workers]?std.Thread = .{null} ** max_check_workers;
    var ctxs: [max_check_workers]CheckWorkerCtx = undefined;

    for (1..worker_count) |i| {
        ctxs[i] = .{
            .worker_id = i,
            .worker_count = worker_count,
            .tsc_path = tsc_path,
            .tsc_dir = tsc_dir,
            .transformed_tsc = transformed,
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
