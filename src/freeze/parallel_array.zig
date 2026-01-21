//! Parallel Array Operations for Frozen Functions
//!
//! Provides parallelization for array iteration methods (map, filter, forEach,
//! some, every, find) when arrays exceed a threshold size.
//!
//! Architecture:
//! - Persistent thread pool initialized once, reused across all operations
//! - Work queue with condition variable for efficient thread wakeup
//! - Each parallel operation creates temporary JSContexts (required by QuickJS)
//!
//! Key constraints:
//! - JSContext is NOT thread-safe - each worker gets its own JSContext
//! - JSRuntime IS thread-safe - shared across all workers
//! - Result ordering must be preserved for map/filter
//!
//! Thresholds:
//! - forEach: 500 elements
//! - map/filter/some/every/find: 1000 elements

const std = @import("std");
const js_value = @import("js_value.zig");

const JSContext = js_value.JSContext;
const JSRuntime = js_value.JSRuntime;
const JSValue = js_value.JSValue;
const quickjs = js_value.quickjs;

const Mutex = std.Thread.Mutex;
const Condition = std.Thread.Condition;

// ============================================================================
// Configuration
// ============================================================================

/// Number of parallel worker threads in the pool
pub const PARALLEL_WORKERS: usize = 4;

/// Threshold for forEach parallelization
pub const THRESHOLD_FOR_EACH: i64 = 500;

/// Threshold for map/filter/some/every/find parallelization
pub const THRESHOLD_MAP: i64 = 1000;

// ============================================================================
// Work Item Types
// ============================================================================

/// Type of parallel operation
const WorkType = enum {
    for_each,
    map,
    filter,
    some,
    every,
    find,
};

/// Work item submitted to the thread pool
const WorkItem = struct {
    /// Type of operation
    work_type: WorkType,
    /// Thread-local JSContext for this work item
    ctx: *JSContext,
    /// The array being iterated
    arr: JSValue,
    /// The callback function
    callback: JSValue,
    /// Start index (inclusive)
    start_idx: usize,
    /// End index (exclusive)
    end_idx: usize,
    /// Results buffer (for map/filter)
    results: []JSValue,
    /// Pointer to result count (for filter)
    result_count: *usize,
    /// Pointer to boolean result (for some/every)
    bool_result: *bool,
    /// Pointer to found value (for find)
    found_value: *JSValue,
    /// Pointer to found index (for find)
    found_idx: *i64,
    /// Pointer to error flag
    has_error: *bool,
    /// Completion signal
    done: *std.atomic.Value(u32),
};

// ============================================================================
// Thread Pool
// ============================================================================

/// Persistent thread pool for parallel array operations
pub const ThreadPool = struct {
    /// Worker threads
    threads: [PARALLEL_WORKERS]?std.Thread,
    /// Work queue
    queue: std.ArrayList(WorkItem),
    /// Mutex protecting the queue
    mutex: Mutex,
    /// Condition variable for work availability
    work_available: Condition,
    /// Shutdown flag
    shutdown: std.atomic.Value(bool),
    /// Allocator for the queue
    allocator: std.mem.Allocator,
    /// Initialization flag
    initialized: bool,

    const Self = @This();

    /// Initialize the thread pool
    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .threads = .{null} ** PARALLEL_WORKERS,
            .queue = std.ArrayList(WorkItem).init(allocator),
            .mutex = .{},
            .work_available = .{},
            .shutdown = std.atomic.Value(bool).init(false),
            .allocator = allocator,
            .initialized = false,
        };
    }

    /// Start the worker threads
    pub fn start(self: *Self) void {
        if (self.initialized) return;

        for (0..PARALLEL_WORKERS) |i| {
            self.threads[i] = std.Thread.spawn(.{}, workerLoop, .{self}) catch null;
        }
        self.initialized = true;
    }

    /// Worker thread main loop
    fn workerLoop(self: *Self) void {
        while (true) {
            var work: ?WorkItem = null;

            // Wait for work
            {
                self.mutex.lock();
                defer self.mutex.unlock();

                while (self.queue.items.len == 0 and !self.shutdown.load(.acquire)) {
                    self.work_available.wait(&self.mutex);
                }

                if (self.shutdown.load(.acquire) and self.queue.items.len == 0) {
                    return;
                }

                if (self.queue.items.len > 0) {
                    work = self.queue.orderedRemove(0);
                }
            }

            // Execute work outside the lock
            if (work) |w| {
                executeWork(w);
            }
        }
    }

    /// Submit work items and wait for completion
    pub fn submitAndWait(self: *Self, items: []const WorkItem) void {
        if (!self.initialized) {
            self.start();
        }

        // Create completion counter
        var done = std.atomic.Value(u32).init(0);
        const total: u32 = @intCast(items.len);

        // Submit all items
        {
            self.mutex.lock();
            defer self.mutex.unlock();

            for (items) |item| {
                var work = item;
                work.done = &done;
                self.queue.append(work) catch continue;
            }

            // Wake all workers
            self.work_available.broadcast();
        }

        // Wait for completion (spin with yield)
        while (done.load(.acquire) < total) {
            std.Thread.yield() catch {};
        }
    }

    /// Shutdown the thread pool
    pub fn deinit(self: *Self) void {
        if (!self.initialized) return;

        // Signal shutdown
        self.shutdown.store(true, .release);

        // Wake all threads
        {
            self.mutex.lock();
            defer self.mutex.unlock();
            self.work_available.broadcast();
        }

        // Join all threads
        for (&self.threads) |*t| {
            if (t.*) |thread| {
                thread.join();
                t.* = null;
            }
        }

        self.queue.deinit();
        self.initialized = false;
    }
};

/// Execute a single work item
fn executeWork(work: WorkItem) void {
    defer {
        // Signal completion
        _ = work.done.fetchAdd(1, .release);
    }

    switch (work.work_type) {
        .for_each => executeForEach(work),
        .map => executeMap(work),
        .filter => executeFilter(work),
        .some => executeSome(work),
        .every => executeEvery(work),
        .find => executeFind(work),
    }
}

// ============================================================================
// Global Thread Pool
// ============================================================================

var global_pool: ThreadPool = ThreadPool.init(std.heap.page_allocator);
var pool_initialized: bool = false;

/// Get or initialize the global thread pool
pub fn getPool() *ThreadPool {
    if (!pool_initialized) {
        global_pool.start();
        pool_initialized = true;
    }
    return &global_pool;
}

/// Shutdown the global thread pool (call at program exit)
pub fn shutdownPool() void {
    if (pool_initialized) {
        global_pool.deinit();
        pool_initialized = false;
    }
}

// ============================================================================
// Work Execution Functions
// ============================================================================

fn executeForEach(work: WorkItem) void {
    const ctx = work.ctx;
    for (work.start_idx..work.end_idx) |i| {
        const elem = quickjs.JS_GetPropertyUint32(ctx, work.arr, @intCast(i));
        if (elem.isException()) {
            work.has_error.* = true;
            return;
        }
        defer JSValue.free(ctx, elem);

        var args = [3]JSValue{ elem, JSValue.newInt(@intCast(i)), work.arr };
        const result = quickjs.JS_Call(ctx, work.callback, JSValue.UNDEFINED, 3, &args);
        if (result.isException()) {
            work.has_error.* = true;
            return;
        }
        JSValue.free(ctx, result);
    }
}

fn executeMap(work: WorkItem) void {
    const ctx = work.ctx;
    for (work.start_idx..work.end_idx) |i| {
        const elem = quickjs.JS_GetPropertyUint32(ctx, work.arr, @intCast(i));
        if (elem.isException()) {
            work.has_error.* = true;
            return;
        }
        defer JSValue.free(ctx, elem);

        var args = [3]JSValue{ elem, JSValue.newInt(@intCast(i)), work.arr };
        const result = quickjs.JS_Call(ctx, work.callback, JSValue.UNDEFINED, 3, &args);
        if (result.isException()) {
            work.has_error.* = true;
            return;
        }
        work.results[i - work.start_idx] = result;
    }
}

fn executeFilter(work: WorkItem) void {
    const ctx = work.ctx;
    var count: usize = 0;

    for (work.start_idx..work.end_idx) |i| {
        const elem = quickjs.JS_GetPropertyUint32(ctx, work.arr, @intCast(i));
        if (elem.isException()) {
            work.has_error.* = true;
            return;
        }

        var args = [3]JSValue{ elem, JSValue.newInt(@intCast(i)), work.arr };
        const result = quickjs.JS_Call(ctx, work.callback, JSValue.UNDEFINED, 3, &args);
        if (result.isException()) {
            JSValue.free(ctx, elem);
            work.has_error.* = true;
            return;
        }

        const is_truthy = quickjs.JS_ToBool(ctx, result) != 0;
        JSValue.free(ctx, result);

        if (is_truthy) {
            work.results[i - work.start_idx] = elem;
            count += 1;
        } else {
            work.results[i - work.start_idx] = JSValue.UNDEFINED;
            JSValue.free(ctx, elem);
        }
    }
    work.result_count.* = count;
}

fn executeSome(work: WorkItem) void {
    const ctx = work.ctx;

    for (work.start_idx..work.end_idx) |i| {
        // Early exit if another worker found a match
        if (work.bool_result.*) return;

        const elem = quickjs.JS_GetPropertyUint32(ctx, work.arr, @intCast(i));
        if (elem.isException()) {
            work.has_error.* = true;
            return;
        }
        defer JSValue.free(ctx, elem);

        var args = [3]JSValue{ elem, JSValue.newInt(@intCast(i)), work.arr };
        const result = quickjs.JS_Call(ctx, work.callback, JSValue.UNDEFINED, 3, &args);
        if (result.isException()) {
            work.has_error.* = true;
            return;
        }

        const is_truthy = quickjs.JS_ToBool(ctx, result) != 0;
        JSValue.free(ctx, result);

        if (is_truthy) {
            work.bool_result.* = true;
            return;
        }
    }
}

fn executeEvery(work: WorkItem) void {
    const ctx = work.ctx;

    for (work.start_idx..work.end_idx) |i| {
        const elem = quickjs.JS_GetPropertyUint32(ctx, work.arr, @intCast(i));
        if (elem.isException()) {
            work.has_error.* = true;
            return;
        }
        defer JSValue.free(ctx, elem);

        var args = [3]JSValue{ elem, JSValue.newInt(@intCast(i)), work.arr };
        const result = quickjs.JS_Call(ctx, work.callback, JSValue.UNDEFINED, 3, &args);
        if (result.isException()) {
            work.has_error.* = true;
            return;
        }

        const is_truthy = quickjs.JS_ToBool(ctx, result) != 0;
        JSValue.free(ctx, result);

        if (!is_truthy) {
            work.bool_result.* = false;
            return;
        }
    }
}

fn executeFind(work: WorkItem) void {
    const ctx = work.ctx;

    for (work.start_idx..work.end_idx) |i| {
        const elem = quickjs.JS_GetPropertyUint32(ctx, work.arr, @intCast(i));
        if (elem.isException()) {
            work.has_error.* = true;
            return;
        }

        var args = [3]JSValue{ elem, JSValue.newInt(@intCast(i)), work.arr };
        const result = quickjs.JS_Call(ctx, work.callback, JSValue.UNDEFINED, 3, &args);
        if (result.isException()) {
            JSValue.free(ctx, elem);
            work.has_error.* = true;
            return;
        }

        const is_truthy = quickjs.JS_ToBool(ctx, result) != 0;
        JSValue.free(ctx, result);

        if (is_truthy) {
            // Check if we found a better (earlier) match
            const current_found = work.found_idx.*;
            if (current_found < 0 or @as(i64, @intCast(i)) < current_found) {
                work.found_value.* = elem;
                work.found_idx.* = @intCast(i);
            } else {
                JSValue.free(ctx, elem);
            }
            return;
        }
        JSValue.free(ctx, elem);
    }
}

// ============================================================================
// Public Parallel Functions
// ============================================================================

/// Create a worker JSContext from the main context's runtime
fn createWorkerContext(main_ctx: *JSContext) !*JSContext {
    const rt = quickjs.JS_GetRuntime(main_ctx);
    const worker_ctx = quickjs.JS_NewContext(rt);
    if (worker_ctx == null) return error.ContextCreationFailed;
    return worker_ctx.?;
}

/// Free a worker context
fn freeWorkerContext(ctx: *JSContext) void {
    quickjs.JS_FreeContext(ctx);
}

/// Parallel forEach - executes callback on each element in parallel
pub fn parallelForEach(ctx: *JSContext, arr: JSValue, callback: JSValue, length: i64) JSValue {
    const len: usize = @intCast(length);
    const pool = getPool();

    // Create worker contexts and work items
    var contexts: [PARALLEL_WORKERS]*JSContext = undefined;
    var items: [PARALLEL_WORKERS]WorkItem = undefined;
    var has_errors: [PARALLEL_WORKERS]bool = .{false} ** PARALLEL_WORKERS;
    var result_counts: [PARALLEL_WORKERS]usize = .{0} ** PARALLEL_WORKERS;
    var bool_results: [PARALLEL_WORKERS]bool = .{false} ** PARALLEL_WORKERS;
    var found_values: [PARALLEL_WORKERS]JSValue = .{JSValue.UNDEFINED} ** PARALLEL_WORKERS;
    var found_indices: [PARALLEL_WORKERS]i64 = .{-1} ** PARALLEL_WORKERS;

    const chunk_size = len / PARALLEL_WORKERS;
    var num_workers: usize = 0;

    // Initialize workers
    for (0..PARALLEL_WORKERS) |i| {
        const start = i * chunk_size;
        const end = if (i == PARALLEL_WORKERS - 1) len else (i + 1) * chunk_size;
        if (start >= end) break;

        contexts[i] = createWorkerContext(ctx) catch {
            // Cleanup already created contexts
            for (0..i) |j| freeWorkerContext(contexts[j]);
            return JSValue.EXCEPTION;
        };

        items[i] = .{
            .work_type = .for_each,
            .ctx = contexts[i],
            .arr = quickjs.JS_DupValue(ctx, arr),
            .callback = quickjs.JS_DupValue(ctx, callback),
            .start_idx = start,
            .end_idx = end,
            .results = &.{},
            .result_count = &result_counts[i],
            .bool_result = &bool_results[i],
            .found_value = &found_values[i],
            .found_idx = &found_indices[i],
            .has_error = &has_errors[i],
            .done = undefined, // Set by submitAndWait
        };
        num_workers += 1;
    }

    // Submit and wait
    pool.submitAndWait(items[0..num_workers]);

    // Cleanup and check errors
    var any_error = false;
    for (0..num_workers) |i| {
        if (has_errors[i]) any_error = true;
        quickjs.JS_FreeValue(contexts[i], items[i].arr);
        quickjs.JS_FreeValue(contexts[i], items[i].callback);
        freeWorkerContext(contexts[i]);
    }

    if (any_error) return JSValue.EXCEPTION;
    return JSValue.UNDEFINED;
}

/// Parallel map - returns new array with callback results
pub fn parallelMap(ctx: *JSContext, arr: JSValue, callback: JSValue, length: i64) JSValue {
    const len: usize = @intCast(length);
    const allocator = std.heap.page_allocator;
    const pool = getPool();

    // Allocate results array
    const results = allocator.alloc(JSValue, len) catch return JSValue.EXCEPTION;
    defer allocator.free(results);
    for (results) |*r| r.* = JSValue.UNDEFINED;

    // Create worker contexts and work items
    var contexts: [PARALLEL_WORKERS]*JSContext = undefined;
    var items: [PARALLEL_WORKERS]WorkItem = undefined;
    var has_errors: [PARALLEL_WORKERS]bool = .{false} ** PARALLEL_WORKERS;
    var result_counts: [PARALLEL_WORKERS]usize = .{0} ** PARALLEL_WORKERS;
    var bool_results: [PARALLEL_WORKERS]bool = .{false} ** PARALLEL_WORKERS;
    var found_values: [PARALLEL_WORKERS]JSValue = .{JSValue.UNDEFINED} ** PARALLEL_WORKERS;
    var found_indices: [PARALLEL_WORKERS]i64 = .{-1} ** PARALLEL_WORKERS;

    const chunk_size = len / PARALLEL_WORKERS;
    var num_workers: usize = 0;

    for (0..PARALLEL_WORKERS) |i| {
        const start = i * chunk_size;
        const end = if (i == PARALLEL_WORKERS - 1) len else (i + 1) * chunk_size;
        if (start >= end) break;

        contexts[i] = createWorkerContext(ctx) catch {
            for (0..i) |j| freeWorkerContext(contexts[j]);
            return JSValue.EXCEPTION;
        };

        items[i] = .{
            .work_type = .map,
            .ctx = contexts[i],
            .arr = quickjs.JS_DupValue(ctx, arr),
            .callback = quickjs.JS_DupValue(ctx, callback),
            .start_idx = start,
            .end_idx = end,
            .results = results[start..end],
            .result_count = &result_counts[i],
            .bool_result = &bool_results[i],
            .found_value = &found_values[i],
            .found_idx = &found_indices[i],
            .has_error = &has_errors[i],
            .done = undefined,
        };
        num_workers += 1;
    }

    pool.submitAndWait(items[0..num_workers]);

    // Cleanup and check errors
    var any_error = false;
    for (0..num_workers) |i| {
        if (has_errors[i]) any_error = true;
        quickjs.JS_FreeValue(contexts[i], items[i].arr);
        quickjs.JS_FreeValue(contexts[i], items[i].callback);
        freeWorkerContext(contexts[i]);
    }

    if (any_error) {
        for (results) |r| {
            if (!r.isUndefined()) JSValue.free(ctx, r);
        }
        return JSValue.EXCEPTION;
    }

    // Assemble result array
    const result_arr = quickjs.JS_NewArray(ctx);
    if (result_arr.isException()) {
        for (results) |r| JSValue.free(ctx, r);
        return JSValue.EXCEPTION;
    }

    for (0..len) |i| {
        _ = quickjs.JS_SetPropertyUint32(ctx, result_arr, @intCast(i), results[i]);
    }

    return result_arr;
}

/// Parallel filter - returns new array with elements matching predicate
pub fn parallelFilter(ctx: *JSContext, arr: JSValue, callback: JSValue, length: i64) JSValue {
    const len: usize = @intCast(length);
    const allocator = std.heap.page_allocator;
    const pool = getPool();

    // Allocate results array (sparse)
    const results = allocator.alloc(JSValue, len) catch return JSValue.EXCEPTION;
    defer allocator.free(results);
    for (results) |*r| r.* = JSValue.UNDEFINED;

    // Create worker contexts and work items
    var contexts: [PARALLEL_WORKERS]*JSContext = undefined;
    var items: [PARALLEL_WORKERS]WorkItem = undefined;
    var has_errors: [PARALLEL_WORKERS]bool = .{false} ** PARALLEL_WORKERS;
    var result_counts: [PARALLEL_WORKERS]usize = .{0} ** PARALLEL_WORKERS;
    var bool_results: [PARALLEL_WORKERS]bool = .{false} ** PARALLEL_WORKERS;
    var found_values: [PARALLEL_WORKERS]JSValue = .{JSValue.UNDEFINED} ** PARALLEL_WORKERS;
    var found_indices: [PARALLEL_WORKERS]i64 = .{-1} ** PARALLEL_WORKERS;

    const chunk_size = len / PARALLEL_WORKERS;
    var num_workers: usize = 0;

    for (0..PARALLEL_WORKERS) |i| {
        const start = i * chunk_size;
        const end = if (i == PARALLEL_WORKERS - 1) len else (i + 1) * chunk_size;
        if (start >= end) break;

        contexts[i] = createWorkerContext(ctx) catch {
            for (0..i) |j| freeWorkerContext(contexts[j]);
            return JSValue.EXCEPTION;
        };

        items[i] = .{
            .work_type = .filter,
            .ctx = contexts[i],
            .arr = quickjs.JS_DupValue(ctx, arr),
            .callback = quickjs.JS_DupValue(ctx, callback),
            .start_idx = start,
            .end_idx = end,
            .results = results[start..end],
            .result_count = &result_counts[i],
            .bool_result = &bool_results[i],
            .found_value = &found_values[i],
            .found_idx = &found_indices[i],
            .has_error = &has_errors[i],
            .done = undefined,
        };
        num_workers += 1;
    }

    pool.submitAndWait(items[0..num_workers]);

    // Cleanup and check errors
    var any_error = false;
    for (0..num_workers) |i| {
        if (has_errors[i]) any_error = true;
        quickjs.JS_FreeValue(contexts[i], items[i].arr);
        quickjs.JS_FreeValue(contexts[i], items[i].callback);
        freeWorkerContext(contexts[i]);
    }

    if (any_error) {
        for (results) |r| {
            if (!r.isUndefined()) JSValue.free(ctx, r);
        }
        return JSValue.EXCEPTION;
    }

    // Assemble result array (compact)
    const result_arr = quickjs.JS_NewArray(ctx);
    if (result_arr.isException()) {
        for (results) |r| {
            if (!r.isUndefined()) JSValue.free(ctx, r);
        }
        return JSValue.EXCEPTION;
    }

    var out_idx: u32 = 0;
    for (results) |r| {
        if (!r.isUndefined()) {
            _ = quickjs.JS_SetPropertyUint32(ctx, result_arr, out_idx, r);
            out_idx += 1;
        }
    }

    return result_arr;
}

/// Parallel some - returns true if any element matches predicate
pub fn parallelSome(ctx: *JSContext, arr: JSValue, callback: JSValue, length: i64) JSValue {
    const len: usize = @intCast(length);
    const pool = getPool();

    // Shared result
    var found_any: bool = false;

    // Create worker contexts and work items
    var contexts: [PARALLEL_WORKERS]*JSContext = undefined;
    var items: [PARALLEL_WORKERS]WorkItem = undefined;
    var has_errors: [PARALLEL_WORKERS]bool = .{false} ** PARALLEL_WORKERS;
    var result_counts: [PARALLEL_WORKERS]usize = .{0} ** PARALLEL_WORKERS;
    var found_values: [PARALLEL_WORKERS]JSValue = .{JSValue.UNDEFINED} ** PARALLEL_WORKERS;
    var found_indices: [PARALLEL_WORKERS]i64 = .{-1} ** PARALLEL_WORKERS;

    const chunk_size = len / PARALLEL_WORKERS;
    var num_workers: usize = 0;

    for (0..PARALLEL_WORKERS) |i| {
        const start = i * chunk_size;
        const end = if (i == PARALLEL_WORKERS - 1) len else (i + 1) * chunk_size;
        if (start >= end) break;

        contexts[i] = createWorkerContext(ctx) catch {
            for (0..i) |j| freeWorkerContext(contexts[j]);
            return JSValue.EXCEPTION;
        };

        items[i] = .{
            .work_type = .some,
            .ctx = contexts[i],
            .arr = quickjs.JS_DupValue(ctx, arr),
            .callback = quickjs.JS_DupValue(ctx, callback),
            .start_idx = start,
            .end_idx = end,
            .results = &.{},
            .result_count = &result_counts[i],
            .bool_result = &found_any, // Shared!
            .found_value = &found_values[i],
            .found_idx = &found_indices[i],
            .has_error = &has_errors[i],
            .done = undefined,
        };
        num_workers += 1;
    }

    pool.submitAndWait(items[0..num_workers]);

    // Cleanup and check errors
    var any_error = false;
    for (0..num_workers) |i| {
        if (has_errors[i]) any_error = true;
        quickjs.JS_FreeValue(contexts[i], items[i].arr);
        quickjs.JS_FreeValue(contexts[i], items[i].callback);
        freeWorkerContext(contexts[i]);
    }

    if (any_error) return JSValue.EXCEPTION;
    return JSValue.newBool(found_any);
}

/// Parallel every - returns true if all elements match predicate
pub fn parallelEvery(ctx: *JSContext, arr: JSValue, callback: JSValue, length: i64) JSValue {
    const len: usize = @intCast(length);
    const pool = getPool();

    // Per-worker results (can't share for every - need all to be true)
    var all_match: [PARALLEL_WORKERS]bool = .{true} ** PARALLEL_WORKERS;

    // Create worker contexts and work items
    var contexts: [PARALLEL_WORKERS]*JSContext = undefined;
    var items: [PARALLEL_WORKERS]WorkItem = undefined;
    var has_errors: [PARALLEL_WORKERS]bool = .{false} ** PARALLEL_WORKERS;
    var result_counts: [PARALLEL_WORKERS]usize = .{0} ** PARALLEL_WORKERS;
    var found_values: [PARALLEL_WORKERS]JSValue = .{JSValue.UNDEFINED} ** PARALLEL_WORKERS;
    var found_indices: [PARALLEL_WORKERS]i64 = .{-1} ** PARALLEL_WORKERS;

    const chunk_size = len / PARALLEL_WORKERS;
    var num_workers: usize = 0;

    for (0..PARALLEL_WORKERS) |i| {
        const start = i * chunk_size;
        const end = if (i == PARALLEL_WORKERS - 1) len else (i + 1) * chunk_size;
        if (start >= end) break;

        contexts[i] = createWorkerContext(ctx) catch {
            for (0..i) |j| freeWorkerContext(contexts[j]);
            return JSValue.EXCEPTION;
        };

        items[i] = .{
            .work_type = .every,
            .ctx = contexts[i],
            .arr = quickjs.JS_DupValue(ctx, arr),
            .callback = quickjs.JS_DupValue(ctx, callback),
            .start_idx = start,
            .end_idx = end,
            .results = &.{},
            .result_count = &result_counts[i],
            .bool_result = &all_match[i],
            .found_value = &found_values[i],
            .found_idx = &found_indices[i],
            .has_error = &has_errors[i],
            .done = undefined,
        };
        num_workers += 1;
    }

    pool.submitAndWait(items[0..num_workers]);

    // Cleanup and collect results
    var any_error = false;
    var result = true;
    for (0..num_workers) |i| {
        if (has_errors[i]) any_error = true;
        if (!all_match[i]) result = false;
        quickjs.JS_FreeValue(contexts[i], items[i].arr);
        quickjs.JS_FreeValue(contexts[i], items[i].callback);
        freeWorkerContext(contexts[i]);
    }

    if (any_error) return JSValue.EXCEPTION;
    return JSValue.newBool(result);
}

/// Parallel find - returns first element matching predicate
pub fn parallelFind(ctx: *JSContext, arr: JSValue, callback: JSValue, length: i64) JSValue {
    const len: usize = @intCast(length);
    const pool = getPool();

    // Per-worker found results
    var found_values: [PARALLEL_WORKERS]JSValue = .{JSValue.UNDEFINED} ** PARALLEL_WORKERS;
    var found_indices: [PARALLEL_WORKERS]i64 = .{-1} ** PARALLEL_WORKERS;

    // Create worker contexts and work items
    var contexts: [PARALLEL_WORKERS]*JSContext = undefined;
    var items: [PARALLEL_WORKERS]WorkItem = undefined;
    var has_errors: [PARALLEL_WORKERS]bool = .{false} ** PARALLEL_WORKERS;
    var result_counts: [PARALLEL_WORKERS]usize = .{0} ** PARALLEL_WORKERS;
    var bool_results: [PARALLEL_WORKERS]bool = .{false} ** PARALLEL_WORKERS;

    const chunk_size = len / PARALLEL_WORKERS;
    var num_workers: usize = 0;

    for (0..PARALLEL_WORKERS) |i| {
        const start = i * chunk_size;
        const end = if (i == PARALLEL_WORKERS - 1) len else (i + 1) * chunk_size;
        if (start >= end) break;

        contexts[i] = createWorkerContext(ctx) catch {
            for (0..i) |j| freeWorkerContext(contexts[j]);
            return JSValue.EXCEPTION;
        };

        items[i] = .{
            .work_type = .find,
            .ctx = contexts[i],
            .arr = quickjs.JS_DupValue(ctx, arr),
            .callback = quickjs.JS_DupValue(ctx, callback),
            .start_idx = start,
            .end_idx = end,
            .results = &.{},
            .result_count = &result_counts[i],
            .bool_result = &bool_results[i],
            .found_value = &found_values[i],
            .found_idx = &found_indices[i],
            .has_error = &has_errors[i],
            .done = undefined,
        };
        num_workers += 1;
    }

    pool.submitAndWait(items[0..num_workers]);

    // Find earliest match and cleanup
    var any_error = false;
    var best_idx: i64 = -1;
    var best_value: JSValue = JSValue.UNDEFINED;
    var best_ctx: usize = 0;

    for (0..num_workers) |i| {
        if (has_errors[i]) any_error = true;
        if (found_indices[i] >= 0) {
            if (best_idx < 0 or found_indices[i] < best_idx) {
                // Free previous best if any
                if (!best_value.isUndefined()) {
                    JSValue.free(contexts[best_ctx], best_value);
                }
                best_idx = found_indices[i];
                best_value = found_values[i];
                best_ctx = i;
            } else {
                // Free this found value
                JSValue.free(contexts[i], found_values[i]);
            }
        }
        quickjs.JS_FreeValue(contexts[i], items[i].arr);
        quickjs.JS_FreeValue(contexts[i], items[i].callback);
    }

    // Dup best value to main context before freeing worker contexts
    var result = JSValue.UNDEFINED;
    if (!best_value.isUndefined()) {
        result = quickjs.JS_DupValue(ctx, best_value);
        JSValue.free(contexts[best_ctx], best_value);
    }

    // Free all worker contexts
    for (0..num_workers) |i| {
        freeWorkerContext(contexts[i]);
    }

    if (any_error) {
        if (!result.isUndefined()) JSValue.free(ctx, result);
        return JSValue.EXCEPTION;
    }

    return result;
}
