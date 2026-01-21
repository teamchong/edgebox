//! Parallel Array Operations for Frozen Functions
//!
//! Provides parallelization for array iteration methods (map, filter, forEach,
//! some, every, find) when arrays exceed a threshold size.
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

// ============================================================================
// Configuration
// ============================================================================

/// Number of parallel worker threads
pub const PARALLEL_WORKERS: usize = 4;

/// Threshold for forEach parallelization
pub const THRESHOLD_FOR_EACH: i64 = 500;

/// Threshold for map/filter/some/every/find parallelization
pub const THRESHOLD_MAP: i64 = 1000;

// ============================================================================
// Worker Context
// ============================================================================

/// Context passed to each worker thread
const WorkerContext = struct {
    /// Thread-local JSContext (created from shared runtime)
    ctx: *JSContext,
    /// The array being iterated
    arr: JSValue,
    /// The callback function
    callback: JSValue,
    /// Start index (inclusive)
    start_idx: usize,
    /// End index (exclusive)
    end_idx: usize,
    /// Results buffer (for map) - pre-allocated by caller
    results: []JSValue,
    /// Count of valid results (for filter)
    result_count: usize,
    /// Boolean result (for some/every)
    bool_result: bool,
    /// Found value (for find)
    found_value: JSValue,
    /// Index of found value (for find, -1 if not found)
    found_idx: i64,
    /// Error flag
    has_error: bool,
};

// ============================================================================
// Worker Thread Pool
// ============================================================================

/// Create a worker context with its own JSContext from the runtime
pub fn createWorkerContext(main_ctx: *JSContext) !*JSContext {
    const rt = quickjs.JS_GetRuntime(main_ctx);
    const worker_ctx = quickjs.JS_NewContext(rt);
    if (worker_ctx == null) return error.ContextCreationFailed;
    return worker_ctx.?;
}

/// Free a worker context
pub fn freeWorkerContext(ctx: *JSContext) void {
    quickjs.JS_FreeContext(ctx);
}

// ============================================================================
// Parallel forEach
// ============================================================================

/// Worker function for forEach - executes callback on each element
fn forEachWorker(worker: *WorkerContext) void {
    const ctx = worker.ctx;
    for (worker.start_idx..worker.end_idx) |i| {
        const elem = quickjs.JS_GetPropertyUint32(ctx, worker.arr, @intCast(i));
        if (elem.isException()) {
            worker.has_error = true;
            return;
        }
        defer JSValue.free(ctx, elem);

        var args = [3]JSValue{ elem, JSValue.newInt(@intCast(i)), worker.arr };
        const result = quickjs.JS_Call(ctx, worker.callback, JSValue.UNDEFINED, 3, &args);
        if (result.isException()) {
            worker.has_error = true;
            return;
        }
        JSValue.free(ctx, result);
    }
}

/// Parallel forEach - executes callback on each element in parallel
/// Does not return a value, just executes side effects
pub fn parallelForEach(ctx: *JSContext, arr: JSValue, callback: JSValue, length: i64) JSValue {
    const len: usize = @intCast(length);

    // Create worker contexts
    var workers: [PARALLEL_WORKERS]WorkerContext = undefined;
    var threads: [PARALLEL_WORKERS]?std.Thread = .{null} ** PARALLEL_WORKERS;
    const chunk_size = len / PARALLEL_WORKERS;
    var any_error = false;

    // Initialize and spawn workers
    for (0..PARALLEL_WORKERS) |i| {
        const start = i * chunk_size;
        const end = if (i == PARALLEL_WORKERS - 1) len else (i + 1) * chunk_size;

        const worker_ctx = createWorkerContext(ctx) catch {
            any_error = true;
            break;
        };

        workers[i] = .{
            .ctx = worker_ctx,
            .arr = quickjs.JS_DupValue(ctx, arr),
            .callback = quickjs.JS_DupValue(ctx, callback),
            .start_idx = start,
            .end_idx = end,
            .results = &.{},
            .result_count = 0,
            .bool_result = false,
            .found_value = JSValue.UNDEFINED,
            .found_idx = -1,
            .has_error = false,
        };

        threads[i] = std.Thread.spawn(.{}, forEachWorker, .{&workers[i]}) catch null;
    }

    // Wait for all threads to complete
    for (&threads) |*t| {
        if (t.*) |thread| thread.join();
    }

    // Check for errors and cleanup
    for (0..PARALLEL_WORKERS) |i| {
        if (workers[i].has_error) any_error = true;
        quickjs.JS_FreeValue(workers[i].ctx, workers[i].arr);
        quickjs.JS_FreeValue(workers[i].ctx, workers[i].callback);
        freeWorkerContext(workers[i].ctx);
    }

    if (any_error) {
        return JSValue.EXCEPTION;
    }
    return JSValue.UNDEFINED;
}

// ============================================================================
// Parallel map
// ============================================================================

/// Worker function for map - executes callback and stores results
fn mapWorker(worker: *WorkerContext) void {
    const ctx = worker.ctx;
    for (worker.start_idx..worker.end_idx) |i| {
        const elem = quickjs.JS_GetPropertyUint32(ctx, worker.arr, @intCast(i));
        if (elem.isException()) {
            worker.has_error = true;
            return;
        }
        defer JSValue.free(ctx, elem);

        var args = [3]JSValue{ elem, JSValue.newInt(@intCast(i)), worker.arr };
        const result = quickjs.JS_Call(ctx, worker.callback, JSValue.UNDEFINED, 3, &args);
        if (result.isException()) {
            worker.has_error = true;
            return;
        }
        // Store result at the correct index (results array is pre-allocated)
        worker.results[i - worker.start_idx] = result;
    }
}

/// Parallel map - returns new array with callback results
pub fn parallelMap(ctx: *JSContext, arr: JSValue, callback: JSValue, length: i64) JSValue {
    const len: usize = @intCast(length);
    const allocator = std.heap.page_allocator;

    // Allocate results array
    const results = allocator.alloc(JSValue, len) catch return JSValue.EXCEPTION;
    defer allocator.free(results);

    // Initialize to UNDEFINED
    for (results) |*r| r.* = JSValue.UNDEFINED;

    // Create worker contexts
    var workers: [PARALLEL_WORKERS]WorkerContext = undefined;
    var threads: [PARALLEL_WORKERS]?std.Thread = .{null} ** PARALLEL_WORKERS;
    const chunk_size = len / PARALLEL_WORKERS;
    var any_error = false;

    // Initialize and spawn workers
    for (0..PARALLEL_WORKERS) |i| {
        const start = i * chunk_size;
        const end = if (i == PARALLEL_WORKERS - 1) len else (i + 1) * chunk_size;

        const worker_ctx = createWorkerContext(ctx) catch {
            any_error = true;
            break;
        };

        workers[i] = .{
            .ctx = worker_ctx,
            .arr = quickjs.JS_DupValue(ctx, arr),
            .callback = quickjs.JS_DupValue(ctx, callback),
            .start_idx = start,
            .end_idx = end,
            .results = results[start..end],
            .result_count = 0,
            .bool_result = false,
            .found_value = JSValue.UNDEFINED,
            .found_idx = -1,
            .has_error = false,
        };

        threads[i] = std.Thread.spawn(.{}, mapWorker, .{&workers[i]}) catch null;
    }

    // Wait for all threads to complete
    for (&threads) |*t| {
        if (t.*) |thread| thread.join();
    }

    // Check for errors and cleanup workers
    for (0..PARALLEL_WORKERS) |i| {
        if (workers[i].has_error) any_error = true;
        quickjs.JS_FreeValue(workers[i].ctx, workers[i].arr);
        quickjs.JS_FreeValue(workers[i].ctx, workers[i].callback);
        freeWorkerContext(workers[i].ctx);
    }

    if (any_error) {
        // Free any results that were created before the error
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
        // Transfer ownership of results[i] to the array
        _ = quickjs.JS_SetPropertyUint32(ctx, result_arr, @intCast(i), results[i]);
    }

    return result_arr;
}

// ============================================================================
// Parallel filter
// ============================================================================

/// Worker function for filter - executes callback and marks matching elements
fn filterWorker(worker: *WorkerContext) void {
    const ctx = worker.ctx;
    var count: usize = 0;

    for (worker.start_idx..worker.end_idx) |i| {
        const elem = quickjs.JS_GetPropertyUint32(ctx, worker.arr, @intCast(i));
        if (elem.isException()) {
            worker.has_error = true;
            return;
        }

        var args = [3]JSValue{ elem, JSValue.newInt(@intCast(i)), worker.arr };
        const result = quickjs.JS_Call(ctx, worker.callback, JSValue.UNDEFINED, 3, &args);
        if (result.isException()) {
            JSValue.free(ctx, elem);
            worker.has_error = true;
            return;
        }

        // Check if result is truthy
        const is_truthy = quickjs.JS_ToBool(ctx, result) != 0;
        JSValue.free(ctx, result);

        if (is_truthy) {
            // Store element in results (sparse - may have gaps)
            worker.results[i - worker.start_idx] = elem;
            count += 1;
        } else {
            // Mark as not included (UNDEFINED) and free the element
            worker.results[i - worker.start_idx] = JSValue.UNDEFINED;
            JSValue.free(ctx, elem);
        }
    }
    worker.result_count = count;
}

/// Parallel filter - returns new array with elements matching predicate
pub fn parallelFilter(ctx: *JSContext, arr: JSValue, callback: JSValue, length: i64) JSValue {
    const len: usize = @intCast(length);
    const allocator = std.heap.page_allocator;

    // Allocate results array (sparse - some will be UNDEFINED)
    const results = allocator.alloc(JSValue, len) catch return JSValue.EXCEPTION;
    defer allocator.free(results);

    // Initialize to UNDEFINED
    for (results) |*r| r.* = JSValue.UNDEFINED;

    // Create worker contexts
    var workers: [PARALLEL_WORKERS]WorkerContext = undefined;
    var threads: [PARALLEL_WORKERS]?std.Thread = .{null} ** PARALLEL_WORKERS;
    const chunk_size = len / PARALLEL_WORKERS;
    var any_error = false;

    // Initialize and spawn workers
    for (0..PARALLEL_WORKERS) |i| {
        const start = i * chunk_size;
        const end = if (i == PARALLEL_WORKERS - 1) len else (i + 1) * chunk_size;

        const worker_ctx = createWorkerContext(ctx) catch {
            any_error = true;
            break;
        };

        workers[i] = .{
            .ctx = worker_ctx,
            .arr = quickjs.JS_DupValue(ctx, arr),
            .callback = quickjs.JS_DupValue(ctx, callback),
            .start_idx = start,
            .end_idx = end,
            .results = results[start..end],
            .result_count = 0,
            .bool_result = false,
            .found_value = JSValue.UNDEFINED,
            .found_idx = -1,
            .has_error = false,
        };

        threads[i] = std.Thread.spawn(.{}, filterWorker, .{&workers[i]}) catch null;
    }

    // Wait for all threads to complete
    for (&threads) |*t| {
        if (t.*) |thread| thread.join();
    }

    // Check for errors and cleanup workers
    for (0..PARALLEL_WORKERS) |i| {
        if (workers[i].has_error) any_error = true;
        quickjs.JS_FreeValue(workers[i].ctx, workers[i].arr);
        quickjs.JS_FreeValue(workers[i].ctx, workers[i].callback);
        freeWorkerContext(workers[i].ctx);
    }

    if (any_error) {
        // Free any elements that were kept
        for (results) |r| {
            if (!r.isUndefined()) JSValue.free(ctx, r);
        }
        return JSValue.EXCEPTION;
    }

    // Assemble result array (compact - no gaps)
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

// ============================================================================
// Parallel some
// ============================================================================

/// Worker function for some - returns true if any element matches
fn someWorker(worker: *WorkerContext) void {
    const ctx = worker.ctx;

    for (worker.start_idx..worker.end_idx) |i| {
        // Early exit if another worker already found a match
        if (worker.bool_result) return;

        const elem = quickjs.JS_GetPropertyUint32(ctx, worker.arr, @intCast(i));
        if (elem.isException()) {
            worker.has_error = true;
            return;
        }
        defer JSValue.free(ctx, elem);

        var args = [3]JSValue{ elem, JSValue.newInt(@intCast(i)), worker.arr };
        const result = quickjs.JS_Call(ctx, worker.callback, JSValue.UNDEFINED, 3, &args);
        if (result.isException()) {
            worker.has_error = true;
            return;
        }

        const is_truthy = quickjs.JS_ToBool(ctx, result) != 0;
        JSValue.free(ctx, result);

        if (is_truthy) {
            worker.bool_result = true;
            return;
        }
    }
}

/// Parallel some - returns true if any element matches predicate
pub fn parallelSome(ctx: *JSContext, arr: JSValue, callback: JSValue, length: i64) JSValue {
    const len: usize = @intCast(length);

    // Create worker contexts
    var workers: [PARALLEL_WORKERS]WorkerContext = undefined;
    var threads: [PARALLEL_WORKERS]?std.Thread = .{null} ** PARALLEL_WORKERS;
    const chunk_size = len / PARALLEL_WORKERS;
    var any_error = false;

    // Initialize and spawn workers
    for (0..PARALLEL_WORKERS) |i| {
        const start = i * chunk_size;
        const end = if (i == PARALLEL_WORKERS - 1) len else (i + 1) * chunk_size;

        const worker_ctx = createWorkerContext(ctx) catch {
            any_error = true;
            break;
        };

        workers[i] = .{
            .ctx = worker_ctx,
            .arr = quickjs.JS_DupValue(ctx, arr),
            .callback = quickjs.JS_DupValue(ctx, callback),
            .start_idx = start,
            .end_idx = end,
            .results = &.{},
            .result_count = 0,
            .bool_result = false,
            .found_value = JSValue.UNDEFINED,
            .found_idx = -1,
            .has_error = false,
        };

        threads[i] = std.Thread.spawn(.{}, someWorker, .{&workers[i]}) catch null;
    }

    // Wait for all threads to complete
    for (&threads) |*t| {
        if (t.*) |thread| thread.join();
    }

    // Collect results
    var found_any = false;
    for (0..PARALLEL_WORKERS) |i| {
        if (workers[i].has_error) any_error = true;
        if (workers[i].bool_result) found_any = true;
        quickjs.JS_FreeValue(workers[i].ctx, workers[i].arr);
        quickjs.JS_FreeValue(workers[i].ctx, workers[i].callback);
        freeWorkerContext(workers[i].ctx);
    }

    if (any_error) return JSValue.EXCEPTION;
    return JSValue.newBool(found_any);
}

// ============================================================================
// Parallel every
// ============================================================================

/// Worker function for every - returns false if any element doesn't match
fn everyWorker(worker: *WorkerContext) void {
    const ctx = worker.ctx;
    worker.bool_result = true; // Assume all match until proven otherwise

    for (worker.start_idx..worker.end_idx) |i| {
        const elem = quickjs.JS_GetPropertyUint32(ctx, worker.arr, @intCast(i));
        if (elem.isException()) {
            worker.has_error = true;
            return;
        }
        defer JSValue.free(ctx, elem);

        var args = [3]JSValue{ elem, JSValue.newInt(@intCast(i)), worker.arr };
        const result = quickjs.JS_Call(ctx, worker.callback, JSValue.UNDEFINED, 3, &args);
        if (result.isException()) {
            worker.has_error = true;
            return;
        }

        const is_truthy = quickjs.JS_ToBool(ctx, result) != 0;
        JSValue.free(ctx, result);

        if (!is_truthy) {
            worker.bool_result = false;
            return; // Early exit
        }
    }
}

/// Parallel every - returns true if all elements match predicate
pub fn parallelEvery(ctx: *JSContext, arr: JSValue, callback: JSValue, length: i64) JSValue {
    const len: usize = @intCast(length);

    // Create worker contexts
    var workers: [PARALLEL_WORKERS]WorkerContext = undefined;
    var threads: [PARALLEL_WORKERS]?std.Thread = .{null} ** PARALLEL_WORKERS;
    const chunk_size = len / PARALLEL_WORKERS;
    var any_error = false;

    // Initialize and spawn workers
    for (0..PARALLEL_WORKERS) |i| {
        const start = i * chunk_size;
        const end = if (i == PARALLEL_WORKERS - 1) len else (i + 1) * chunk_size;

        const worker_ctx = createWorkerContext(ctx) catch {
            any_error = true;
            break;
        };

        workers[i] = .{
            .ctx = worker_ctx,
            .arr = quickjs.JS_DupValue(ctx, arr),
            .callback = quickjs.JS_DupValue(ctx, callback),
            .start_idx = start,
            .end_idx = end,
            .results = &.{},
            .result_count = 0,
            .bool_result = true, // Assume true
            .found_value = JSValue.UNDEFINED,
            .found_idx = -1,
            .has_error = false,
        };

        threads[i] = std.Thread.spawn(.{}, everyWorker, .{&workers[i]}) catch null;
    }

    // Wait for all threads to complete
    for (&threads) |*t| {
        if (t.*) |thread| thread.join();
    }

    // Collect results
    var all_match = true;
    for (0..PARALLEL_WORKERS) |i| {
        if (workers[i].has_error) any_error = true;
        if (!workers[i].bool_result) all_match = false;
        quickjs.JS_FreeValue(workers[i].ctx, workers[i].arr);
        quickjs.JS_FreeValue(workers[i].ctx, workers[i].callback);
        freeWorkerContext(workers[i].ctx);
    }

    if (any_error) return JSValue.EXCEPTION;
    return JSValue.newBool(all_match);
}

// ============================================================================
// Parallel find
// ============================================================================

/// Worker function for find - finds first matching element in chunk
fn findWorker(worker: *WorkerContext) void {
    const ctx = worker.ctx;

    for (worker.start_idx..worker.end_idx) |i| {
        const elem = quickjs.JS_GetPropertyUint32(ctx, worker.arr, @intCast(i));
        if (elem.isException()) {
            worker.has_error = true;
            return;
        }

        var args = [3]JSValue{ elem, JSValue.newInt(@intCast(i)), worker.arr };
        const result = quickjs.JS_Call(ctx, worker.callback, JSValue.UNDEFINED, 3, &args);
        if (result.isException()) {
            JSValue.free(ctx, elem);
            worker.has_error = true;
            return;
        }

        const is_truthy = quickjs.JS_ToBool(ctx, result) != 0;
        JSValue.free(ctx, result);

        if (is_truthy) {
            worker.found_value = elem;
            worker.found_idx = @intCast(i);
            return; // Found - don't free elem, it's the result
        }
        JSValue.free(ctx, elem);
    }
}

/// Parallel find - returns first element matching predicate
pub fn parallelFind(ctx: *JSContext, arr: JSValue, callback: JSValue, length: i64) JSValue {
    const len: usize = @intCast(length);

    // Create worker contexts
    var workers: [PARALLEL_WORKERS]WorkerContext = undefined;
    var threads: [PARALLEL_WORKERS]?std.Thread = .{null} ** PARALLEL_WORKERS;
    const chunk_size = len / PARALLEL_WORKERS;
    var any_error = false;

    // Initialize and spawn workers
    for (0..PARALLEL_WORKERS) |i| {
        const start = i * chunk_size;
        const end = if (i == PARALLEL_WORKERS - 1) len else (i + 1) * chunk_size;

        const worker_ctx = createWorkerContext(ctx) catch {
            any_error = true;
            break;
        };

        workers[i] = .{
            .ctx = worker_ctx,
            .arr = quickjs.JS_DupValue(ctx, arr),
            .callback = quickjs.JS_DupValue(ctx, callback),
            .start_idx = start,
            .end_idx = end,
            .results = &.{},
            .result_count = 0,
            .bool_result = false,
            .found_value = JSValue.UNDEFINED,
            .found_idx = -1,
            .has_error = false,
        };

        threads[i] = std.Thread.spawn(.{}, findWorker, .{&workers[i]}) catch null;
    }

    // Wait for all threads to complete
    for (&threads) |*t| {
        if (t.*) |thread| thread.join();
    }

    // Find the result with lowest index (to match sequential behavior)
    var best_idx: i64 = -1;
    var best_value: JSValue = JSValue.UNDEFINED;
    var best_worker: usize = 0;

    for (0..PARALLEL_WORKERS) |i| {
        if (workers[i].has_error) any_error = true;
        if (workers[i].found_idx >= 0) {
            if (best_idx < 0 or workers[i].found_idx < best_idx) {
                // Free previous best if there was one
                if (!best_value.isUndefined()) {
                    JSValue.free(workers[best_worker].ctx, best_value);
                }
                best_idx = workers[i].found_idx;
                best_value = workers[i].found_value;
                best_worker = i;
            } else {
                // Free this found value since it's not the best
                JSValue.free(workers[i].ctx, workers[i].found_value);
            }
        }
    }

    // Cleanup workers (except best_value which is transferred)
    for (0..PARALLEL_WORKERS) |i| {
        quickjs.JS_FreeValue(workers[i].ctx, workers[i].arr);
        quickjs.JS_FreeValue(workers[i].ctx, workers[i].callback);
        freeWorkerContext(workers[i].ctx);
    }

    if (any_error) {
        if (!best_value.isUndefined()) JSValue.free(ctx, best_value);
        return JSValue.EXCEPTION;
    }

    // Need to dup the value to transfer to main context
    if (!best_value.isUndefined()) {
        return quickjs.JS_DupValue(ctx, best_value);
    }
    return JSValue.UNDEFINED;
}
