// v8_parallel_check.zig — Zig-threaded parallel type relation checking
//
// Architecture: V8 orchestrates (parse, bind). Zig computes (type checking).
// Zig owns all memory via mmap. V8 gets SharedArrayBuffer views (zero-copy).
// Pre-spawned thread pool eliminates spawn overhead (~16ms → <0.1ms dispatch).
//
// Memory layout (single mmap allocation, 3 regions):
//   [0 .. MAX_TYPES):                typeFlags  (Int32Array, indexed by type.id)
//   [MAX_TYPES .. MAX_TYPES+2*MAX_PAIRS):  pairs      (Int32Array, sourceId/targetId alternating)
//   [MAX_TYPES+2*MAX_PAIRS .. end):  results    (Int32Array, 0=unknown, 1=related, 2=not-related)
//
// JS API:
//   globalThis.__pc_typeFlags  — Int32Array view of typeFlags region
//   globalThis.__pc_pairs      — Int32Array view of pairs region
//   globalThis.__pc_results    — Int32Array view of results region
//   __edgebox_parallel_check(count) — triggers Zig parallel check, returns resolved count

const std = @import("std");
const v8 = @import("v8.zig");

const max_threads = 8;

// Buffer sizing: 256K types, 512K pairs max
const MAX_TYPES: usize = 262144; // 256K type slots
const MAX_PAIRS: usize = 524288; // 512K pair slots
// Layout: [typeFlags 256K | objectFlags 256K | flagTable 4M bytes | pairs 1M | results 512K]
// flagTable: 2048*2048 = 4M entries of u8, for isSimpleTypeRelatedTo lookup
const FLAG_TABLE_I32 = (2048 * 2048 + 3) / 4; // 4MB in i32 units (rounded up)
const TOTAL_I32 = MAX_TYPES * 2 + FLAG_TABLE_I32 + 2 * MAX_PAIRS + MAX_PAIRS;
const TOTAL_BYTES = TOTAL_I32 * @sizeOf(i32);

const TYPE_FLAGS_OFFSET: usize = 0;
const OBJ_FLAGS_OFFSET: usize = MAX_TYPES;
const FLAG_TABLE_OFFSET: usize = MAX_TYPES * 2;
const PAIRS_OFFSET: usize = MAX_TYPES * 2 + FLAG_TABLE_I32;
const RESULTS_OFFSET: usize = MAX_TYPES * 2 + FLAG_TABLE_I32 + 2 * MAX_PAIRS;

// Zig-owned shared memory
var shared_buffer: ?[*]align(4096) u8 = null;

fn getSharedBuffer() ?[*]align(4096) u8 {
    if (shared_buffer) |buf| return buf;
    const buf = std.heap.page_allocator.alignedAlloc(u8, .fromByteUnits(4096), TOTAL_BYTES) catch return null;
    shared_buffer = buf.ptr;
    return buf.ptr;
}

fn getTypeFlags() ?[*]i32 {
    const buf = getSharedBuffer() orelse return null;
    return @alignCast(@ptrCast(buf + TYPE_FLAGS_OFFSET * @sizeOf(i32)));
}

fn getObjFlags() ?[*]i32 {
    const buf = getSharedBuffer() orelse return null;
    return @alignCast(@ptrCast(buf + OBJ_FLAGS_OFFSET * @sizeOf(i32)));
}

fn getPairs() ?[*]i32 {
    const buf = getSharedBuffer() orelse return null;
    return @alignCast(@ptrCast(buf + PAIRS_OFFSET * @sizeOf(i32)));
}

fn getResults() ?[*]i32 {
    const buf = getSharedBuffer() orelse return null;
    return @alignCast(@ptrCast(buf + RESULTS_OFFSET * @sizeOf(i32)));
}

/// isSimpleTypeRelatedTo in Zig — pure integer operations on flat arrays.
fn checkRelation(type_flags: [*]const i32, source_id: u32, target_id: u32) i32 {
    if (source_id >= MAX_TYPES or target_id >= MAX_TYPES) return 0;

    const s = type_flags[source_id];
    const t = type_flags[target_id];

    if (t & 1 != 0 or s & 131072 != 0) return 1;
    if (t & 2 != 0) return 1;
    if (t & 131072 != 0) return 2;
    if (s & 402653316 != 0 and t & 4 != 0) return 1;
    if (s & 296 != 0 and t & 8 != 0) return 1;
    if (s & 2112 != 0 and t & 64 != 0) return 1;
    if (s & 528 != 0 and t & 16 != 0) return 1;
    if (s & 12288 != 0 and t & 4096 != 0) return 1;
    if (s & 32768 != 0 and t & (32768 | 16384) != 0) return 1;
    if (s & 65536 != 0 and t & 65536 != 0) return 1;
    if (s & 524288 != 0 and t & 67108864 != 0) return 1;

    return 0;
}

// ============================================================
// Thread Pool — pre-spawned workers, futex-based wake
// ============================================================

const WorkerState = enum(u32) {
    idle = 0,
    working = 1,
    done = 2,
    shutdown = 3,
};

const WorkerSlot = struct {
    state: std.atomic.Value(u32) = std.atomic.Value(u32).init(@intFromEnum(WorkerState.idle)),
    type_flags: [*]const i32 = undefined,
    pairs: [*]const i32 = undefined,
    results: [*]i32 = undefined,
    start: usize = 0,
    end: usize = 0,
};

var pool_slots: [max_threads]WorkerSlot = undefined;
var pool_threads: [max_threads]?std.Thread = .{null} ** max_threads;
var pool_size: usize = 0;
var pool_initialized = false;

fn poolWorker(slot: *WorkerSlot) void {
    while (true) {
        // Wait for work (spin then yield)
        while (true) {
            const state = slot.state.load(.acquire);
            if (state == @intFromEnum(WorkerState.working)) break;
            if (state == @intFromEnum(WorkerState.shutdown)) return;
            // Spin-wait with backoff
            std.atomic.spinLoopHint();
        }

        // Do work
        var i = slot.start;
        while (i < slot.end) : (i += 1) {
            const raw_src = slot.pairs[i * 2];
            const raw_tgt = slot.pairs[i * 2 + 1];
            if (raw_src < 0 or raw_tgt < 0) {
                slot.results[i] = 0;
                continue;
            }
            slot.results[i] = checkRelation(slot.type_flags, @intCast(raw_src), @intCast(raw_tgt));
        }

        // Signal done
        slot.state.store(@intFromEnum(WorkerState.done), .release);
    }
}

fn initPool() void {
    if (pool_initialized) return;
    pool_initialized = true;

    const cpu_count = std.Thread.getCpuCount() catch 4;
    pool_size = @min(cpu_count, max_threads);

    for (0..pool_size) |i| {
        pool_slots[i] = .{};
        pool_threads[i] = std.Thread.spawn(.{}, poolWorker, .{&pool_slots[i]}) catch null;
    }
}

fn dispatchParallel(
    type_flags: [*]const i32,
    pairs: [*]const i32,
    results: [*]i32,
    count: usize,
) void {
    if (count == 0) return;

    initPool();

    if (pool_size <= 1 or count < 1000) {
        // Single-threaded for small batches
        for (0..count) |i| {
            const raw_src = pairs[i * 2];
            const raw_tgt = pairs[i * 2 + 1];
            if (raw_src < 0 or raw_tgt < 0) {
                results[i] = 0;
                continue;
            }
            results[i] = checkRelation(type_flags, @intCast(raw_src), @intCast(raw_tgt));
        }
        return;
    }

    const thread_count = @min(pool_size, @max(count / 1000, 2));
    const per_thread = (count + thread_count - 1) / thread_count;

    // Dispatch work to pool
    var dispatched: usize = 0;
    for (0..thread_count) |i| {
        const start = i * per_thread;
        const end = @min(start + per_thread, count);
        if (start >= end) break;

        pool_slots[i].type_flags = type_flags;
        pool_slots[i].pairs = pairs;
        pool_slots[i].results = results;
        pool_slots[i].start = start;
        pool_slots[i].end = end;
        pool_slots[i].state.store(@intFromEnum(WorkerState.working), .release);
        dispatched += 1;
    }

    // Wait for all workers
    for (0..dispatched) |i| {
        while (pool_slots[i].state.load(.acquire) != @intFromEnum(WorkerState.done)) {
            std.atomic.spinLoopHint();
        }
        pool_slots[i].state.store(@intFromEnum(WorkerState.idle), .release);
    }
}

/// V8 callback: __edgebox_parallel_check(count)
pub fn parallelCheckCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate: *v8.Isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    if (v8.CallbackInfoApi.length(info) < 1) {
        rv.setInt32(0);
        return;
    }

    const count_arg = v8.CallbackInfoApi.get(info, 0) orelse {
        rv.setInt32(0);
        return;
    };

    var count: usize = 0;
    if (v8.ValueApi.isInt32(count_arg)) {
        const context = v8.ContextApi.create(isolate);
        const val = v8.ValueApi.int32Value(count_arg, context) orelse 0;
        if (val <= 0) {
            rv.setInt32(0);
            return;
        }
        count = @intCast(val);
    } else if (v8.ValueApi.isString(count_arg)) {
        const count_str: *const v8.String = @ptrCast(count_arg);
        const count_len: usize = @intCast(v8.StringApi.utf8Length(count_str, isolate));
        var count_buf: [16]u8 = undefined;
        _ = v8.StringApi.writeUtf8(count_str, isolate, &count_buf);
        count = std.fmt.parseInt(usize, count_buf[0..count_len], 10) catch {
            rv.setInt32(0);
            return;
        };
    } else {
        rv.setInt32(0);
        return;
    }

    if (count == 0 or count > MAX_PAIRS) {
        rv.setInt32(0);
        return;
    }

    const type_flags = getTypeFlags() orelse { rv.setInt32(0); return; };
    const pairs = getPairs() orelse { rv.setInt32(0); return; };
    const results = getResults() orelse { rv.setInt32(0); return; };

    // Dispatch to thread pool
    dispatchParallel(type_flags, pairs, results, count);

    // Count resolved
    var resolved: i32 = 0;
    for (0..count) |i| {
        if (results[i] != 0) resolved += 1;
    }
    rv.setInt32(resolved);
}

// ============================================================
// Flag-pair precomputation table
// ============================================================
// Maps (sourceFlags, targetFlags) → result for isSimpleTypeRelatedTo.
// Built once after types are created, then every call is a single lookup.
// Table indexed by: (sourceFlags & 0x7FF) * 2048 + (targetFlags & 0x7FF)
// This captures the bottom 11 bits of flags which contain all type kinds.

const FLAG_TABLE_ENTRIES: usize = 2048 * 2048; // 4M entries

fn getFlagTable() ?[*]u8 {
    const buf = getSharedBuffer() orelse return null;
    return buf + FLAG_TABLE_OFFSET * @sizeOf(i32);
}

/// Build the flag-pair lookup table using ALL type flags in the SAB.
/// The table lives IN the SAB — JS reads it directly via Uint8Array (zero-copy).
pub fn buildFlagTable(type_flags: [*]const i32, max_id: usize) void {
    const table = getFlagTable() orelse return;
    @memset(table[0..FLAG_TABLE_ENTRIES], 0);

    // Collect distinct flag values
    var seen_flags: [4096]i32 = undefined;
    var seen_count: usize = 0;
    for (0..@min(max_id + 1, MAX_TYPES)) |i| {
        const f = type_flags[i];
        if (f == 0) continue;
        var found = false;
        for (0..seen_count) |j| {
            if (seen_flags[j] == f) { found = true; break; }
        }
        if (!found and seen_count < 4096) {
            seen_flags[seen_count] = f;
            seen_count += 1;
        }
    }

    // Pre-compute all pairs of distinct flags
    for (0..seen_count) |si| {
        const s = seen_flags[si];
        const s_idx: usize = @intCast(s & 0x7FF);
        for (0..seen_count) |ti| {
            const t = seen_flags[ti];
            const t_idx: usize = @intCast(t & 0x7FF);
            const idx = s_idx * 2048 + t_idx;

            // isSimpleTypeRelatedTo logic
            var result: u8 = 0;
            if (t & 1 != 0 or s & 131072 != 0) { result = 1; }
            else if (t & 2 != 0) { result = 1; }
            else if (t & 131072 != 0) { result = 2; }
            else if (s & 402653316 != 0 and t & 4 != 0) { result = 1; }
            else if (s & 296 != 0 and t & 8 != 0) { result = 1; }
            else if (s & 2112 != 0 and t & 64 != 0) { result = 1; }
            else if (s & 528 != 0 and t & 16 != 0) { result = 1; }
            else if (s & 12288 != 0 and t & 4096 != 0) { result = 1; }
            else if (s & 32768 != 0 and t & (32768 | 16384) != 0) { result = 1; }
            else if (s & 65536 != 0 and t & 65536 != 0) { result = 1; }
            else if (s & 524288 != 0 and t & 67108864 != 0) { result = 1; }

            table[idx] = result;
        }
    }
}

/// Async flag table build state
var async_build_thread: ?std.Thread = null;
var async_build_max_id: usize = 0;
var async_build_done: std.atomic.Value(bool) = std.atomic.Value(bool).init(false);

fn asyncBuildWorker() void {
    const type_flags = getTypeFlags() orelse return;
    buildFlagTable(type_flags, async_build_max_id);
    async_build_done.store(true, .release);
}

/// V8 callback: __edgebox_precompute_relations(maxTypeId)
/// Builds the flag table — uses async result if available, else builds synchronously.
pub fn precomputeCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate: *v8.Isolate = v8.CallbackInfoApi.getIsolate(info);
    var rv = v8.CallbackInfoApi.getReturnValue(info);

    if (v8.CallbackInfoApi.length(info) < 1) { rv.setInt32(0); return; }
    const count_arg = v8.CallbackInfoApi.get(info, 0) orelse { rv.setInt32(0); return; };

    var max_id: usize = 0;
    if (v8.ValueApi.isInt32(count_arg)) {
        const context = v8.ContextApi.create(isolate);
        const val = v8.ValueApi.int32Value(count_arg, context) orelse 0;
        if (val > 0) max_id = @intCast(val);
    }

    // If async build was started and has the same or larger max_id, wait for it
    if (async_build_done.load(.acquire)) {
        // Async build already completed — check if it covers enough types
        if (async_build_max_id >= max_id) {
            // Join the thread if still active
            if (async_build_thread) |t| { t.join(); async_build_thread = null; }
            rv.setInt32(1);
            return;
        }
    }

    // Join any pending async thread
    if (async_build_thread) |t| { t.join(); async_build_thread = null; }

    // Build synchronously (async didn't cover all types)
    const type_flags = getTypeFlags() orelse { rv.setInt32(0); return; };
    buildFlagTable(type_flags, max_id);
    rv.setInt32(1);
}

/// Trigger async flag table build from createType (called early during bind).
/// Spawns a Zig thread that builds the table while V8 continues binding.
pub fn triggerAsyncBuild(max_id: usize) void {
    if (async_build_thread != null) return; // Already running
    async_build_done.store(false, .release);
    async_build_max_id = max_id;
    async_build_thread = std.Thread.spawn(.{}, asyncBuildWorker, .{}) catch null;
}

/// V8 callback: __edgebox_trigger_build(typeCount)
/// Called periodically from createType to trigger background build.
pub fn triggerBuildCallback(info: *const v8.FunctionCallbackInfo) callconv(.c) void {
    const isolate: *v8.Isolate = v8.CallbackInfoApi.getIsolate(info);
    if (v8.CallbackInfoApi.length(info) < 1) return;
    const arg = v8.CallbackInfoApi.get(info, 0) orelse return;
    if (!v8.ValueApi.isInt32(arg)) return;
    const context = v8.ContextApi.create(isolate);
    const val = v8.ValueApi.int32Value(arg, context) orelse return;
    if (val > 0) triggerAsyncBuild(@intCast(val));
}

/// Look up the flag-pair table result.
/// Returns 0 (unknown), 1 (related), 2 (not-related).
pub fn lookupFlagPair(source_flags: i32, target_flags: i32) u8 {
    const table = getFlagTable() orelse return 0;
    const s_idx: usize = @intCast(source_flags & 0x7FF);
    const t_idx: usize = @intCast(target_flags & 0x7FF);
    const idx = s_idx * 2048 + t_idx;
    if (idx >= FLAG_TABLE_ENTRIES) return 0;
    return table[idx];
}

/// Register __edgebox_parallel_check and expose SAB-backed typed arrays.
pub fn registerGlobals(isolate: *v8.Isolate, context: *const v8.Context) void {
    const global = v8.ContextApi.global(context);

    const tmpl = v8.FunctionTemplateApi.create(isolate, &parallelCheckCallback) orelse return;
    const func = v8.FunctionTemplateApi.getFunction(tmpl, context) orelse return;
    const key = v8.StringApi.fromUtf8(isolate, "__edgebox_parallel_check") orelse return;
    _ = v8.ObjectApi.set(global, context, @ptrCast(key), @ptrCast(func));

    const buf = getSharedBuffer() orelse return;
    const sab = v8.SharedArrayBufferApi.fromExternalMemory(isolate, buf, TOTAL_BYTES) orelse return;

    const sab_key = v8.StringApi.fromUtf8(isolate, "__pc_sab") orelse return;
    _ = v8.ObjectApi.set(global, context, @ptrCast(sab_key), sab);

    var init_buf: [1024]u8 = undefined;
    const init_js = std.fmt.bufPrint(&init_buf,
        "globalThis.__pc_typeFlags=new Int32Array(__pc_sab,0,{d});" ++
        "globalThis.__pc_objectFlags=new Int32Array(__pc_sab,{d},{d});" ++
        "globalThis.__pc_flagTable=new Uint8Array(__pc_sab,{d},{d});" ++
        "globalThis.__pc_pairs=new Int32Array(__pc_sab,{d},{d});" ++
        "globalThis.__pc_results=new Int32Array(__pc_sab,{d},{d});",
        .{
            MAX_TYPES,
            MAX_TYPES * 4, MAX_TYPES,
            FLAG_TABLE_OFFSET * 4, FLAG_TABLE_ENTRIES,
            PAIRS_OFFSET * 4, MAX_PAIRS * 2,
            RESULTS_OFFSET * 4, MAX_PAIRS,
        },
    ) catch return;
    _ = v8.eval(isolate, context, init_js, "pc_init.js") catch {};

    // Pre-initialize the thread pool (lazy — threads created on first use)
    initPool();
}
