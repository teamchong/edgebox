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
    v8_io.worker_id = @intCast(ctx.worker_id);

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

    // Ensure ts.sys.write uses our fast callback (snapshot ref may be stale)
    _ = v8.eval(iso, context,
        "if(typeof ts!=='undefined'&&ts.sys){ts.sys.write=function(s){" ++
        "if(typeof __edgebox_write_stdout==='function')__edgebox_write_stdout(s);" ++
        "else process.stdout.write(s);};}",
        "fix_sys_write.js",
    ) catch {};

    // Workers use per-file getSemanticDiagnostics (not executeCommandLine).
    // Output format: "DIAG:file:line:col:code:message\n" for main to dedup.
    const worker_check_js =
        \\(function() {
        \\  if (typeof ts === 'undefined' || !ts.sys) return;
        \\  var args = process.argv.slice(2).filter(function(a){return a!=='--serve';});
        \\  var configPath = null;
        \\  for (var i = 0; i < args.length; i++) {
        \\    if (args[i] === '-p' && i+1 < args.length) { configPath = args[i+1]; break; }
        \\  }
        \\  if (!configPath) return;
        \\  var configFile = ts.readConfigFile(configPath, ts.sys.readFile);
        \\  if (!configFile.config) return;
        \\  var parsed = ts.parseJsonConfigFileContent(configFile.config, ts.sys,
        \\    require('path').dirname(configPath));
        \\  // Ensure lib.d.ts resolves correctly — use same path as main
        \\  var host = ts.createCompilerHost(parsed.options);
        \\  var libDir = require('path').dirname(__filename);
        \\  host.getDefaultLibLocation = function() { return libDir; };
        \\  host.getDefaultLibFileName = function(o) { return require('path').join(libDir, ts.getDefaultLibFileName(o)); };
        \\  var program = ts.createProgram(parsed.fileNames, parsed.options, host);
        \\  var files = program.getSourceFiles();
        \\  var wid = __edgebox_worker_id, wcnt = __edgebox_worker_count;
        \\  var seen = {};
        \\  for (var i = 0; i < files.length; i++) {
        \\    if (i % wcnt !== wid) continue;
        \\    var diags = program.getSemanticDiagnostics(files[i]);
        \\    for (var d = 0; d < diags.length; d++) {
        \\      var dg = diags[d];
        \\      if (!dg.file) continue;
        \\      var fn = dg.file.fileName;
        \\      var pos = dg.file.getLineAndCharacterOfPosition(dg.start || 0);
        \\      var key = fn + ':' + pos.line + ':' + pos.character + ':' + dg.code;
        \\      if (seen[key]) continue;
        \\      seen[key] = true;
        \\      var msg = ts.flattenDiagnosticMessageText(dg.messageText, ' ');
        \\      ts.sys.write(fn + '(' + (pos.line+1) + ',' + (pos.character+1) + '): error TS' + dg.code + ': ' + msg + '\n');
        \\    }
        \\  }
        \\})();
    ;
    const t0 = std.time.milliTimestamp();
    std.debug.print("[worker-{d}] start (+{d}ms)\n", .{ ctx.worker_id, t0 - g_start_time });
    {
        var tc = v8.TryCatch.init(iso);
        defer tc.deinit();
        _ = v8.eval(iso, context, worker_check_js, "tsc_check_worker.js") catch {};
    }
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

    std.debug.print("[parallel-tsc] spawning {d} workers (after createProgram)\n", .{g_worker_count});

    // Freeze IO caches — workers can read lock-free
    v8_io.prefetch_complete = true;

    // Spawn ALL workers (0..N). Main exits early, workers handle all checking.
    for (0..g_worker_count) |i| {
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
    // Workers now use per-file getSemanticDiagnostics (not executeCommandLine).
    // Each worker only outputs diagnostics for its assigned files.
    // Per-worker stdout buffers in v8_io.zig prevent diagnostic doubling.
    // TRUE parallel via Zig threads + V8 isolates.
    // Workers use per-file getSemanticDiagnostics, output to per-worker buffers.
    // Main deduplicates after all workers finish.
    const cpu_count = std.Thread.getCpuCount() catch 4;
    g_worker_count = @min(@max(cpu_count / 2, 2), max_check_workers);

    if (g_worker_count <= 1 or embedded_snapshot.len == 0) return false;

    g_start_time = std.time.milliTimestamp();
    std.debug.print("[parallel-tsc] {d} workers (deferred spawn)\n", .{g_worker_count});

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

/// Wait for all parallel workers to finish, then merge stdout.
/// Workers write to per-worker buffers. Main's stdout goes directly.
/// After join, write worker buffers to real stdout (no doubling).
pub fn waitForWorkers() void {
    for (&g_threads) |*t| {
        if (t.*) |thread| {
            thread.join();
            t.* = null;
        }
    }

    // Merge + dedup worker stdout buffers.
    // Workers may produce overlapping diagnostics (lazy type resolution).
    // Dedup by exact line content (file(line,col): error TScode: message).
    var seen = std.StringHashMapUnmanaged(void){};
    defer seen.deinit(alloc);
    var total_lines: usize = 0;
    var deduped_lines: usize = 0;

    for (0..g_worker_count) |w| {
        const output = v8_io.getWorkerStdout(@intCast(w));
        if (output.len == 0) continue;

        // Split output by newlines, dedup each line
        var start: usize = 0;
        for (0..output.len) |i| {
            if (output[i] == '\n' or i == output.len - 1) {
                const end = if (output[i] == '\n') i else i + 1;
                const line = output[start..end];
                if (line.len > 0) {
                    total_lines += 1;
                    const gop = seen.getOrPut(alloc, line) catch {
                        start = end + 1;
                        continue;
                    };
                    if (!gop.found_existing) {
                        deduped_lines += 1;
                        _ = std.posix.write(1, line) catch {};
                        _ = std.posix.write(1, "\n") catch {};
                    }
                }
                start = end + 1;
            }
        }
        v8_io.clearWorkerStdout(@intCast(w));
    }
    std.debug.print("[parallel-tsc] total={d} deduped={d} unique={d}\n", .{ total_lines, total_lines - deduped_lines, deduped_lines });
}
