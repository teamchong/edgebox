// v8_parallel.zig — Parallel V8 isolate pool for multi-core JS execution
//
// Architecture:
// - Pool of N V8 isolates, each on its own OS thread
// - Main isolate dispatches work items to worker isolates
// - Workers share code (snapshot) but have independent heaps
// - Results collected via shared memory (Zig-side aggregation)
// - Workers created LAZILY on first __edgebox_parallel call (no cold-start cost)
//
// JS API:
//   __edgebox_parallel(jsonArrayOfCode) → jsonArrayOfResults
//
// Use cases:
// - Promise.all with independent operations → true parallelism
// - Map/filter/reduce on large arrays → data parallelism
// - TSC checker sharding → parallel type-checking

const std = @import("std");
const v8 = @import("v8.zig");
const v8_io = @import("v8_io.zig");

const alloc = std.heap.page_allocator;

const max_workers = 8;
const max_work_items = 64;

/// A work item to be executed on a worker isolate
pub const WorkItem = struct {
    /// JS code to evaluate (function body or expression)
    code: []const u8,
    /// JSON-encoded argument to pass
    arg: []const u8,
    /// Result (filled by worker)
    result: ?[]const u8 = null,
    /// Error message (filled by worker)
    err: ?[]const u8 = null,
    /// Done flag
    done: bool = false,
};

/// Embedded snapshot for fast worker isolate creation (~10ms instead of ~100ms)
const embedded_snapshot = @embedFile("v8_bootstrap.snapshot");

/// External references for snapshot (must match snapshot_gen order)
var ext_refs: [3]usize = .{ 0, 0, 0 };
var ext_refs_initialized = false;

fn getExternalRefs() [*]const usize {
    if (!ext_refs_initialized) {
        ext_refs[0] = @intFromPtr(&v8_io.ioSyncCallback);
        ext_refs[1] = @intFromPtr(&v8_io.ioBatchCallback);
        ext_refs[2] = 0; // null terminator
        ext_refs_initialized = true;
    }
    return &ext_refs;
}

/// Worker thread context
const WorkerCtx = struct {
    items: []WorkItem,
    start_idx: usize,
    end_idx: usize,
    bootstrap_code: []const u8,
};

/// Execute work items in parallel across OS threads.
/// Each thread creates its own V8 isolate (no snapshot — lazy, no cold-start cost).
pub fn executeParallel(items: []WorkItem) void {
    const n = items.len;
    if (n == 0) return;

    // Determine thread count: min(items, cores, max_workers)
    const cpu_count = std.Thread.getCpuCount() catch 4;
    const thread_count = @min(@min(n, cpu_count), max_workers);

    const bootstrap_code = @embedFile("v8_bootstrap.js");

    if (thread_count <= 1) {
        var ctx = WorkerCtx{
            .items = items,
            .start_idx = 0,
            .end_idx = n,
            .bootstrap_code = bootstrap_code,
        };
        workerFn(&ctx);
        return;
    }

    var threads: [max_workers]?std.Thread = .{null} ** max_workers;
    var contexts: [max_workers]WorkerCtx = undefined;
    const items_per_thread = (n + thread_count - 1) / thread_count;

    for (0..thread_count) |i| {
        const start = i * items_per_thread;
        const end = @min(start + items_per_thread, n);
        if (start >= end) break;

        contexts[i] = .{
            .items = items,
            .start_idx = start,
            .end_idx = end,
            .bootstrap_code = bootstrap_code,
        };
        threads[i] = std.Thread.spawn(.{}, workerFn, .{&contexts[i]}) catch null;
    }

    for (&threads) |*t| {
        if (t.*) |thread| {
            thread.join();
            t.* = null;
        }
    }
}

fn workerFn(ctx: *WorkerCtx) void {
    // Create isolate from snapshot (fast ~10ms) or fresh (slower ~100ms)
    const isolate = if (embedded_snapshot.len > 0)
        v8.SnapshotApi.createIsolateFromSnapshot(
            embedded_snapshot.ptr,
            @intCast(embedded_snapshot.len),
            getExternalRefs(),
        )
    else
        v8.IsolateApi.create();
    defer v8.IsolateApi.dispose(isolate);
    v8.IsolateApi.enter(isolate);
    defer v8.IsolateApi.exit(isolate);

    var handle_scope = v8.HandleScope.init(isolate);
    defer handle_scope.deinit();
    const context = v8.ContextApi.create(isolate);
    v8.ContextApi.enter(context);
    defer v8.ContextApi.exit(context);

    if (embedded_snapshot.len > 0) {
        // Snapshot has IO bridge via external_refs — just refresh env
        _ = v8.eval(isolate, context,
            "process.argv = typeof __original_argv !== 'undefined' ? __original_argv.slice() : ['edgebox-worker'];",
            "worker_init.js",
        ) catch {};
    } else {
        // No snapshot — register IO and bootstrap from source
        v8_io.registerGlobals(isolate, context);
        _ = v8.eval(isolate, context, ctx.bootstrap_code, "v8_bootstrap.js") catch {};
    }

    // Register channel callbacks in worker (channels are Zig-global, shared across isolates)
    const v8_channel = @import("v8_channel.zig");
    v8_channel.registerGlobals(isolate, context);

    // Execute each work item
    for (ctx.start_idx..ctx.end_idx) |i| {
        const item = &ctx.items[i];

        // Eval the code and capture result as JSON string
        const code_to_eval = std.fmt.allocPrint(alloc,
            "JSON.stringify((function() {{ {s} }})())",
            .{item.code},
        ) catch {
            item.err = "OOM";
            item.done = true;
            continue;
        };
        defer alloc.free(code_to_eval);

        if (v8.eval(isolate, context, code_to_eval, "worker.js")) |result_val| {
            if (v8.ValueApi.isString(result_val)) {
                const str: *const v8.String = @ptrCast(result_val);
                const len: usize = @intCast(v8.StringApi.utf8Length(str, isolate));
                if (len > 0 and len < 64 * 1024 * 1024) {
                    const buf = alloc.alloc(u8, len + 1) catch null;
                    if (buf) |b| {
                        _ = v8.StringApi.writeUtf8(str, isolate, b);
                        item.result = b[0..len];
                    }
                }
            }
        } else |_| {
            item.err = "eval failed";
        }
        item.done = true;
    }
}

/// V8 callback for __edgebox_parallel(jsonArrayOfCodeStrings) → jsonArrayOfResults
pub fn parallelCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
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
    if (len == 0 or len > 16 * 1024 * 1024) {
        rv.setUndefined();
        return;
    }

    const buf = alloc.alloc(u8, len + 1) catch {
        rv.setUndefined();
        return;
    };
    defer alloc.free(buf);
    _ = v8.StringApi.writeUtf8(str, isolate, buf);
    const json_str = buf[0..len];

    // Parse JSON array of code strings
    var parsed = std.json.parseFromSlice(std.json.Value, alloc, json_str, .{}) catch {
        rv.setUndefined();
        return;
    };
    defer parsed.deinit();

    const arr = if (parsed.value == .array) &parsed.value.array else {
        rv.setUndefined();
        return;
    };
    if (arr.items.len == 0) {
        rv.setUndefined();
        return;
    }

    // Build work items
    var items: [max_work_items]WorkItem = undefined;
    const n = @min(arr.items.len, max_work_items);
    for (0..n) |i| {
        items[i] = .{
            .code = if (arr.items[i] == .string) arr.items[i].string else "",
            .arg = "",
        };
    }

    // Execute in parallel (lazy isolate creation — no cold-start cost)
    executeParallel(items[0..n]);

    // Build JSON result array
    var result_buf: std.ArrayListUnmanaged(u8) = .{};
    defer result_buf.deinit(alloc);
    result_buf.append(alloc, '[') catch {};
    for (0..n) |i| {
        if (i > 0) result_buf.append(alloc, ',') catch {};
        if (items[i].result) |r| {
            result_buf.appendSlice(alloc, r) catch {};
        } else if (items[i].err) |e| {
            result_buf.appendSlice(alloc, "{\"error\":\"") catch {};
            result_buf.appendSlice(alloc, e) catch {};
            result_buf.appendSlice(alloc, "\"}") catch {};
        } else {
            result_buf.appendSlice(alloc, "null") catch {};
        }
    }
    result_buf.append(alloc, ']') catch {};

    const result_str = v8.StringApi.fromUtf8(isolate, result_buf.items) orelse {
        rv.setUndefined();
        return;
    };
    rv.set(@ptrCast(result_str));
}

/// V8 callback: __edgebox_spawn(codeString) — non-blocking, fire-and-forget
/// Launches a single work item on a background thread.
/// Used with channels for producer-consumer patterns.
pub fn spawnCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate: *v8.Isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    if (v8.CallbackInfoApi.length(info) < 1) { rv.setUndefined(); return; }
    const arg0 = v8.CallbackInfoApi.get(info, 0) orelse { rv.setUndefined(); return; };
    if (!v8.ValueApi.isString(arg0)) { rv.setUndefined(); return; }

    const str: *const v8.String = @ptrCast(arg0);
    const len: usize = @intCast(v8.StringApi.utf8Length(str, isolate));
    if (len == 0 or len > 16 * 1024 * 1024) { rv.setUndefined(); return; }

    // Copy code to owned buffer (original may be freed when this callback returns)
    const code = alloc.alloc(u8, len + 1) catch { rv.setUndefined(); return; };
    _ = v8.StringApi.writeUtf8(str, isolate, code);

    // Create a persistent work item
    const item = alloc.create(WorkItem) catch { alloc.free(code); rv.setUndefined(); return; };
    item.* = .{ .code = code[0..len], .arg = "" };

    // Wrap in context
    const ctx = alloc.create(WorkerCtx) catch { alloc.destroy(item); alloc.free(code); rv.setUndefined(); return; };
    ctx.* = .{
        .items = @as([*]WorkItem, @ptrCast(item))[0..1],
        .start_idx = 0,
        .end_idx = 1,
        .bootstrap_code = @embedFile("v8_bootstrap.js"),
    };

    // Launch on background thread — fire and forget
    _ = std.Thread.spawn(.{}, workerFn, .{ctx}) catch {
        alloc.destroy(ctx);
        alloc.destroy(item);
        alloc.free(code);
        rv.setUndefined();
        return;
    };

    rv.set(@ptrCast(v8.StringApi.fromUtf8(isolate, "ok") orelse return));
}

/// Register __edgebox_parallel and __edgebox_spawn as global functions.
pub fn registerGlobals(isolate: *v8.Isolate, context: *const v8.Context) void {
    const global = v8.ContextApi.global(context);

    // __edgebox_parallel — blocking, returns results
    const tmpl = v8.FunctionTemplateApi.create(isolate, &parallelCallback) orelse return;
    const func = v8.FunctionTemplateApi.getFunction(tmpl, context) orelse return;
    const key = v8.StringApi.fromUtf8(isolate, "__edgebox_parallel") orelse return;
    _ = v8.ObjectApi.set(global, context, @ptrCast(key), @ptrCast(func));

    // __edgebox_spawn — non-blocking, fire-and-forget (for channel producers)
    const tmpl2 = v8.FunctionTemplateApi.create(isolate, &spawnCallback) orelse return;
    const func2 = v8.FunctionTemplateApi.getFunction(tmpl2, context) orelse return;
    const key2 = v8.StringApi.fromUtf8(isolate, "__edgebox_spawn") orelse return;
    _ = v8.ObjectApi.set(global, context, @ptrCast(key2), @ptrCast(func2));
}
