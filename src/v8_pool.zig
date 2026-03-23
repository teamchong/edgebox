/// V8 Isolate Pool — pre-initialized V8 isolates on OS threads
///
/// Created once at daemon start. Each isolate loads TypeScript.
/// Workers park on condvar. Main signals to dispatch work.
/// Green threads coordinate only — they never create V8.
///
/// Architecture:
///   Daemon start → N OS threads → N V8 isolates → load TSC → park on condvar
///   Request → signal condvar → workers wake → check shard → submit → park
///   Next request → same isolates, already warm → instant

const std = @import("std");
const alloc = std.heap.page_allocator;

// V8 bridge (C ABI from v8_bridge_pool.cpp)
extern fn edgebox_v8_init() void;
extern fn edgebox_v8_create_isolate() ?*anyopaque;
extern fn edgebox_v8_enter_isolate(?*anyopaque) void;
extern fn edgebox_v8_exit_isolate(?*anyopaque) void;
extern fn edgebox_v8_dispose_isolate(?*anyopaque) void;
extern fn edgebox_v8_setup_context(?*anyopaque) ?*anyopaque;
extern fn edgebox_v8_eval_in_context(?*anyopaque, ?*anyopaque, [*]const u8, c_int, *c_int) ?[*]const u8;
extern fn edgebox_v8_free(?[*]const u8) void;
extern fn edgebox_v8_create_snapshot([*]const u8, c_int, [*]const u8, c_int, [*]const u8, c_int, [*]const u8, c_int) c_int;
extern fn edgebox_v8_create_isolate_from_snapshot() ?*anyopaque;
extern fn edgebox_set_daemon_mode(c_int) void;
extern fn edgebox_get_result(c_int, *c_int) ?[*]const u8;

// IO functions from edgebox_io.zig (shared across all workers)
extern fn edgebox_read_file([*]const u8, c_int, *c_int) ?[*]const u8;
extern fn edgebox_reset_work() void;
extern fn edgebox_set_root([*]const u8, c_int) void;

const MAX_WORKERS = 16;

const WorkerState = struct {
    thread: ?std.Thread = null,
    // Work assignment (set by main, read by worker)
    cwd: ?[]const u8 = null,
    worker_id: u32 = 0,
    worker_count: u32 = 0,
    // Synchronization
    work_ready: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
    result_ready: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
    // Result (set by worker, read by main)
    result: ?[]const u8 = null,
    // Shutdown
    shutdown: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
};

var workers: [MAX_WORKERS]WorkerState = [_]WorkerState{.{}} ** MAX_WORKERS;
var pool_size: u32 = 0;
var work_mutex: std.Thread.Mutex = .{};
var work_cond: std.Thread.Condition = .{};

// Edgebox root directory (resolved at init, used for recipe + TypeScript paths)
var eb_root: []const u8 = ".";
var eb_root_buf: [std.fs.max_path_bytes]u8 = undefined;

// Worker init JS — overrides snapshot stubs with real Zig-backed IO.
// Shared between snapshot creation and worker startup.
const worker_init_js =
    "globalThis.ts = globalThis.ts || (globalThis.module && globalThis.module.exports);" ++
    "globalThis.process = { argv:['edgebox'], env:{}, platform:'linux', " ++
    "cwd:function(){return __edgebox_cwd();}, " ++
    "exit:function(){}, " ++
    "stdout:{write:function(s){__edgebox_write_stdout(String(s));return true;},isTTY:false,columns:80}, " ++
    "stderr:{write:function(s){__edgebox_write_stderr(String(s));return true;},isTTY:false}, " ++
    "versions:{node:'20.0.0'}, nextTick:function(cb){Promise.resolve().then(cb);}, " ++
    "hrtime:Object.assign(function(){var t=Date.now();return[Math.floor(t/1000),(t%1000)*1e6];},{bigint:function(){return BigInt(Date.now())*1000000n;}}), " ++
    "on:function(){return process;}, once:function(){return process;}, removeListener:function(){return process;}, emit:function(){return false;}, binding:function(){return{};} };" ++
    "globalThis.require = function(name) {" ++
    "  name = String(name).replace(/^node:/, '');" ++
    "  if(name==='fs') return {" ++
    "    readFileSync:function(p){return __edgebox_read_file(String(p))||null;}," ++
    "    writeFileSync:function(){}," ++
    "    existsSync:function(p){return __edgebox_file_exists(String(p))===1||__edgebox_dir_exists(String(p))===1;}," ++
    "    statSync:function(p){var j=__edgebox_stat(String(p));if(!j){var e=new Error('ENOENT');e.code='ENOENT';throw e;}var s=JSON.parse(j);return{isFile:function(){return s.isFile;},isDirectory:function(){return s.isDirectory;},isSymbolicLink:function(){return false;},size:s.size,mtime:new Date()};}," ++
    "    lstatSync:function(p){return require('fs').statSync(p);}," ++
    "    readdirSync:function(p){return JSON.parse(__edgebox_readdir(String(p)));}," ++
    "    realpathSync:Object.assign(function(p){return __edgebox_realpath(String(p));},{native:function(p){return __edgebox_realpath(String(p));}})," ++
    "    openSync:function(){return -1;},closeSync:function(){},watchFile:function(){},unwatchFile:function(){},watch:function(){return{close:function(){}};}" ++
    "  };" ++
    "  if(name==='path') return {" ++
    "    join:function(){return Array.prototype.slice.call(arguments).join('/').replace(/\\/+/g,'/');}," ++
    "    dirname:function(p){var i=String(p).lastIndexOf('/');return i>=0?String(p).slice(0,i):'.';}," ++
    "    basename:function(p,e){p=String(p);var b=p.slice(p.lastIndexOf('/')+1);if(e&&b.endsWith(e))b=b.slice(0,-e.length);return b;}," ++
    "    resolve:function(){var a=Array.prototype.slice.call(arguments),r='';for(var i=a.length-1;i>=0;i--){r=String(a[i])+(r?'/'+r:'');if(String(a[i]).charAt(0)==='/')break;}if(r.charAt(0)!=='/'){r='/'+r;}return r.replace(/\\/+/g,'/');}," ++
    "    normalize:function(p){return String(p).replace(/\\/+/g,'/');}," ++
    "    isAbsolute:function(p){return String(p).charAt(0)==='/';}," ++
    "    extname:function(p){p=String(p);var i=p.lastIndexOf('.');return i>=0?p.slice(i):'';}," ++
    "    sep:'/',delimiter:':'" ++
    "  };" ++
    "  if(name==='os') return {EOL:'\\n',platform:function(){return'linux';},tmpdir:function(){return'/tmp';},homedir:function(){return'/tmp';},cpus:function(){return[{model:'edgebox'}];},arch:function(){return'x64';}};" ++
    "  if(name==='crypto') return {createHash:function(a){var d='';return{update:function(s){d+=String(s);return this;},digest:function(){return __edgebox_hash(a,d);}};},randomBytes:function(n){return{toString:function(){return'';}};}};" ++
    "  if(name==='perf_hooks') return {performance:{now:function(){return Date.now();},mark:function(){},measure:function(){}}};" ++
    "  if(name==='buffer') return {Buffer:{from:function(s){return s;},isBuffer:function(){return false;},alloc:function(n){return new Uint8Array(n);}}};" ++
    "  return {};" ++
    "};";

/// Initialize the V8 pool with N workers (formula-based).
/// Each worker creates a V8 isolate, loads TSC, parks on condvar.
/// Call once at daemon start.
pub fn init(worker_count: u32) !void {
    const n = @min(worker_count, MAX_WORKERS);
    pool_size = n;

    // Daemon mode: don't let process.exit kill the daemon
    edgebox_set_daemon_mode(1);

    // Initialize V8 platform (once, before any isolate creation)
    edgebox_v8_init();

    // Resolve edgebox root from executable path (binary is at <root>/zig-out/bin/edgebox)
    var exe_buf: [std.fs.max_path_bytes]u8 = undefined;
    const exe_path = std.fs.selfExePath(&exe_buf) catch "";
    if (exe_path.len > 0) {
        // Go up from zig-out/bin/edgebox → project root
        var dir = exe_path;
        var levels: u32 = 0;
        while (levels < 3 and dir.len > 1) : (levels += 1) {
            const idx = std.mem.lastIndexOfScalar(u8, dir, '/') orelse break;
            dir = dir[0..idx];
        }
        @memcpy(eb_root_buf[0..dir.len], dir);
        eb_root = eb_root_buf[0..dir.len];
    } else {
        const cwd_r = std.fs.cwd().realpath(".", &eb_root_buf) catch "/tmp";
        eb_root = eb_root_buf[0..cwd_r.len];
    }
    // Store root in edgebox_io.zig for __edgebox_root() export
    edgebox_set_root(eb_root.ptr, @intCast(eb_root.len));
    _ = std.posix.write(2, "[v8pool] root: ") catch {};
    _ = std.posix.write(2, eb_root) catch {};
    _ = std.posix.write(2, "\n") catch {};

    // Create V8 snapshot with TypeScript pre-loaded (eliminates 3s per worker)
    const eb_cwd = eb_root;
    const shim = std.fmt.allocPrint(alloc,
        "globalThis.module = {{ exports: {{}} }};" ++
        "globalThis.__filename = '{s}/node_modules/typescript/lib/typescript.js';" ++
        "globalThis.__dirname = '{s}/node_modules/typescript/lib';" ++
        "globalThis.process = {{ argv:['edgebox'], env:{{}}, platform:'linux', " ++
        "cwd:function(){{return '/';}}, exit:function(){{}}, " ++
        "stdout:{{write:function(){{return true;}},isTTY:false,columns:80}}, " ++
        "stderr:{{write:function(){{return true;}},isTTY:false}}, " ++
        "versions:{{node:'20.0.0'}}, nextTick:function(cb){{Promise.resolve().then(cb);}}, " ++
        "on:function(){{return process;}}, once:function(){{return process;}}, " ++
        "removeListener:function(){{return process;}}, emit:function(){{return false;}}, " ++
        "binding:function(){{return {{}};}} }};" ++
        "globalThis.setTimeout=function(f){{f();return 0;}};" ++
        "globalThis.clearTimeout=function(){{}};" ++
        "globalThis.setInterval=function(){{return 0;}};" ++
        "globalThis.clearInterval=function(){{}};" ++
        "globalThis.queueMicrotask=function(f){{Promise.resolve().then(f);}};" ++
        "globalThis.console={{log:function(){{}},warn:function(){{}},error:function(){{}},info:function(){{}},debug:function(){{}}}};" ++
        "globalThis.Buffer={{from:function(s){{return s;}},isBuffer:function(){{return false;}},alloc:function(n){{return new Uint8Array(n);}},byteLength:function(s){{return typeof s==='string'?s.length:0;}},isEncoding:function(){{return true;}}}};" ++
        "globalThis.require=function(n){{n=String(n).replace(/^node:/,'');" ++
        "if(n==='fs')return{{readFileSync:function(){{return null;}},writeFileSync:function(){{}},existsSync:function(){{return false;}},statSync:function(){{throw new Error('ENOENT');}},lstatSync:function(){{throw new Error('ENOENT');}},readdirSync:function(){{return[];}},realpathSync:Object.assign(function(p){{return p;}},{{native:function(p){{return p;}}}}),openSync:function(){{return-1;}},closeSync:function(){{}},watchFile:function(){{}},unwatchFile:function(){{}},watch:function(){{return{{close:function(){{}}}};}}}};" ++
        "if(n==='path')return{{join:function(){{return Array.prototype.slice.call(arguments).join('/').replace(/\\\\/+/g,'/');}},dirname:function(p){{var i=String(p).lastIndexOf('/');return i>=0?String(p).slice(0,i):'.';}},basename:function(p,e){{p=String(p);var b=p.slice(p.lastIndexOf('/')+1);if(e&&b.endsWith(e))b=b.slice(0,-e.length);return b;}},resolve:function(){{var a=Array.prototype.slice.call(arguments),r='';for(var i=a.length-1;i>=0;i--){{r=String(a[i])+(r?'/'+r:'');if(String(a[i]).charAt(0)==='/')break;}}if(r.charAt(0)!=='/');return r.replace(/\\\\/+/g,'/');}},normalize:function(p){{return String(p).replace(/\\\\/+/g,'/');}},isAbsolute:function(p){{return String(p).charAt(0)==='/';}},extname:function(p){{p=String(p);var i=p.lastIndexOf('.');return i>=0?p.slice(i):'';}},sep:'/',delimiter:':'}};" ++
        "if(n==='os')return{{EOL:'\\n',platform:function(){{return'linux';}},tmpdir:function(){{return'/tmp';}},homedir:function(){{return'/tmp';}},cpus:function(){{return[{{model:'edgebox'}}];}},arch:function(){{return'x64';}}}};" ++
        "if(n==='crypto')return{{}};" ++
        "if(n==='perf_hooks')return{{performance:{{now:function(){{return Date.now();}}}}}};" ++
        "if(n==='buffer')return{{Buffer:Buffer}};" ++
        "return{{}};}};"
    , .{ eb_cwd, eb_cwd }) catch null;

    if (shim) |s| {
        defer alloc.free(s);
        const ts_path = std.fmt.allocPrint(alloc, "{s}/node_modules/typescript/lib/typescript.js", .{eb_root}) catch null;
        if (ts_path == null) { _ = std.posix.write(2, "[v8pool] FATAL: cannot format ts path\n") catch {}; return error.OutOfMemory; }
        defer alloc.free(ts_path.?);
        var ts_len: c_int = 0;
        const ts_src = edgebox_read_file(ts_path.?.ptr, @intCast(ts_path.?.len), &ts_len);
        if (ts_src != null and ts_len > 0) {
            // Apply recipe transform: inject Zig structural check into isTypeRelatedTo
            const ts_data = ts_src.?[0..@intCast(ts_len)];
            const patched = applyRecipeTransform(ts_data) catch ts_data;
            const final_src: [*]const u8 = patched.ptr;
            const final_len: c_int = @intCast(patched.len);

            // Read recipe file for inclusion in snapshot
            const recipe_path = std.fmt.allocPrint(alloc, "{s}/src/tsc-recipe/checker-parallel.js", .{eb_root}) catch null;
            var r_len: c_int = 0;
            var r_src: ?[*]const u8 = null;
            if (recipe_path) |rp| {
                defer alloc.free(rp);
                r_src = edgebox_read_file(rp.ptr, @intCast(rp.len), &r_len);
            }

            _ = std.posix.write(2, "[v8pool] creating snapshot (with worker_init + recipe)...\n") catch {};
            const snap_size = edgebox_v8_create_snapshot(
                final_src, final_len,
                s.ptr, @intCast(s.len),
                worker_init_js.ptr, @intCast(worker_init_js.len),
                if (r_src) |rs| rs else "".ptr, r_len,
            );
            if (snap_size > 0) {
                var snap_msg: [64]u8 = undefined;
                const msg = std.fmt.bufPrint(&snap_msg, "[v8pool] snapshot: {d} bytes\n", .{snap_size}) catch "[v8pool] snapshot created\n";
                _ = std.posix.write(2, msg) catch {};
            }
        }
    }

    // Pre-warm TypeScript lib .d.ts files once at daemon start (same for all projects)
    const ts_lib = std.fmt.allocPrint(alloc, "{s}/node_modules/typescript/lib", .{eb_root}) catch null;
    if (ts_lib) |tl| {
        defer alloc.free(tl);
        prewarmDir(tl);
    }

    // Spawn worker threads (restore from snapshot — instant TypeScript)
    for (0..n) |i| {
        workers[i].worker_id = @intCast(i);
        workers[i].worker_count = n;
        workers[i].thread = try std.Thread.spawn(.{}, workerLoop, .{@as(u32, @intCast(i))});
    }
}

/// Recipe transform: inject Zig structural check into TSC's internal isTypeRelatedTo.
/// Finds the injection point in typescript.js and inserts the Zig fast path.
/// This is baked into the V8 snapshot — zero runtime overhead.
fn applyRecipeTransform(src: []const u8) ![]const u8 {
    // Find the expensive checkTypeRelatedTo call site inside isTypeRelatedTo
    const fn_marker = "function isTypeRelatedTo(source, target, relation) {";
    const fn_start = std.mem.indexOf(u8, src, fn_marker) orelse {
        _ = std.posix.write(2, "[recipe] WARNING: isTypeRelatedTo not found — no transform\n") catch {};
        return src;
    };

    // Find the expensive structural check: "if (source.flags & 469499904"
    const inject_marker = "if (source.flags & 469499904";
    const inject_pos = std.mem.indexOfPos(u8, src, fn_start, inject_marker) orelse {
        _ = std.posix.write(2, "[recipe] WARNING: injection point not found — no transform\n") catch {};
        return src;
    };

    // Zig structural fast path — injected BEFORE the expensive checkTypeRelatedTo.
    // If Zig says compatible → return true (skip expensive recursive check).
    // If Zig says incompatible → fall through to TSC's full check (TSC is authority).
    // Injection point marker — space for future optimizations.
    // TSC's checkTypeRelatedTo is context-dependent (not a pure function),
    // so caching its results externally is unsafe. TSC's own relation cache handles this.
    const zig_check = "/* edgebox: injection point */\n    ";

    // Second transform: inject WASM fast path into isSimpleTypeRelatedTo.
    // The Zig kernel handles pure flag comparisons (~80% of calls).
    // JS fallback handles value/symbol comparisons.
    const simple_marker = "function isSimpleTypeRelatedTo(source, target, relation, errorReporter) {";
    const simple_start = std.mem.indexOf(u8, src, simple_marker) orelse {
        // No isSimpleTypeRelatedTo — just do the first injection
        const result = try alloc.alloc(u8, src.len + zig_check.len);
        @memcpy(result[0..inject_pos], src[0..inject_pos]);
        @memcpy(result[inject_pos .. inject_pos + zig_check.len], zig_check);
        @memcpy(result[inject_pos + zig_check.len ..], src[inject_pos..]);
        _ = std.posix.write(2, "[recipe] transform: injection point only\n") catch {};
        return result;
    };

    // Inject WASM fast path at the START of isSimpleTypeRelatedTo
    const simple_body_start = simple_start + simple_marker.len;
    // Load WASM kernel bytes and build the injection code.
    // The WASM is loaded once at snapshot time, instantiated, and the function cached.
    const wasm_path = std.fmt.allocPrint(alloc, "{s}/src/tsc-recipe/type_kernel.wasm", .{eb_root}) catch null;
    var wasm_len: c_int = 0;
    var wasm_bytes: ?[*]const u8 = null;
    if (wasm_path) |wp| {
        defer alloc.free(wp);
        wasm_bytes = edgebox_read_file(wp.ptr, @intCast(wp.len), &wasm_len);
    }

    var wasm_fast_path: []const u8 = undefined;
    if (wasm_bytes != null and wasm_len > 0) {
        // Encode WASM bytes as comma-separated integers for JS
        const wb = wasm_bytes.?[0..@intCast(wasm_len)];
        var byte_str: std.ArrayListUnmanaged(u8) = .{};
        for (wb, 0..) |b, idx| {
            if (idx > 0) byte_str.append(alloc, ',') catch {};
            var num_buf: [4]u8 = undefined;
            const num = std.fmt.bufPrint(&num_buf, "{d}", .{b}) catch "0";
            byte_str.appendSlice(alloc, num) catch {};
        }
        const bytes_js = byte_str.toOwnedSlice(alloc) catch "";

        wasm_fast_path = std.fmt.allocPrint(alloc,
            "\n    /* edgebox: WASM kernel — TurboFan inlines this */\n" ++
            "    if (!globalThis.__ebWasmTypeKernel) {{\n" ++
            "      var __wb = new Uint8Array([{s}]);\n" ++
            "      var __wm = new WebAssembly.Module(__wb);\n" ++
            "      var __wi = new WebAssembly.Instance(__wm);\n" ++
            "      globalThis.__ebWasmTypeKernel = __wi.exports.isSimpleTypeRelated;\n" ++
            "    }}\n" ++
            "    var __rel = relation === assignableRelation ? 0 : relation === comparableRelation ? 1 : relation === strictSubtypeRelation ? 2 : 3;\n" ++
            "    var __r = globalThis.__ebWasmTypeKernel(source.flags, target.flags, __rel, strictNullChecks ? 1 : 0);\n" ++
            "    if (__r === 1) return true;\n"
        , .{bytes_js}) catch "";
        alloc.free(bytes_js);

        var msg_buf: [64]u8 = undefined;
        const msg = std.fmt.bufPrint(&msg_buf, "[recipe] WASM kernel: {d} bytes\n", .{wasm_len}) catch "";
        _ = std.posix.write(2, msg) catch {};
    } else {
        // Fallback to C ABI callback
        wasm_fast_path =
            "\n    /* edgebox: C ABI fallback (no WASM) */\n" ++
            "    var __rel = relation === assignableRelation ? 0 : relation === comparableRelation ? 1 : relation === strictSubtypeRelation ? 2 : 3;\n" ++
            "    var __r = __edgebox_is_simple_type_related(source.flags, target.flags, __rel, strictNullChecks ? 1 : 0);\n" ++
            "    if (__r === 1) return true;\n";
    }

    // isSimpleTypeRelatedTo (line ~69244) comes BEFORE inject_pos (line ~69304)
    // Order: src[0..simple_body_start] + wasm + src[simple_body_start..inject_pos] + zig_check + src[inject_pos..]
    const seg1 = src[0..simple_body_start]; // up to isSimpleTypeRelatedTo body
    const seg2 = src[simple_body_start..inject_pos]; // between the two injection points
    const seg3 = src[inject_pos..]; // rest (including checkTypeRelatedTo)

    const total = seg1.len + wasm_fast_path.len + seg2.len + zig_check.len + seg3.len;
    const result = try alloc.alloc(u8, total);
    var pos: usize = 0;
    @memcpy(result[pos .. pos + seg1.len], seg1); pos += seg1.len;
    @memcpy(result[pos .. pos + wasm_fast_path.len], wasm_fast_path); pos += wasm_fast_path.len;
    @memcpy(result[pos .. pos + seg2.len], seg2); pos += seg2.len;
    @memcpy(result[pos .. pos + zig_check.len], zig_check); pos += zig_check.len;
    @memcpy(result[pos .. pos + seg3.len], seg3);
    _ = std.posix.write(2, "[recipe] transform: WASM isSimpleTypeRelated kernel + injection point\n") catch {};
    return result;
}

/// Shutdown the pool. Signal all workers to exit, join threads.
pub fn deinit() void {
    for (0..pool_size) |i| {
        workers[i].shutdown.store(true, .release);
    }
    work_mutex.lock();
    work_cond.broadcast();
    work_mutex.unlock();

    for (0..pool_size) |i| {
        if (workers[i].thread) |t| {
            t.join();
            workers[i].thread = null;
        }
    }
}

/// Dispatch work to all workers. Workers wake from condvar.
/// Pre-warm file cache by reading all .ts/.d.ts files in project
fn prewarmDir(dir_path: []const u8) void {
    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch return;
    defer dir.close();
    var it = dir.iterate();
    while (it.next() catch null) |entry| {
        if (entry.kind == .file and (std.mem.endsWith(u8, entry.name, ".ts") or std.mem.endsWith(u8, entry.name, ".json") or std.mem.endsWith(u8, entry.name, ".d.ts"))) {
            const full = std.fmt.allocPrint(alloc, "{s}/{s}", .{ dir_path, entry.name }) catch continue;
            defer alloc.free(full);
            var fl: c_int = 0;
            _ = edgebox_read_file(full.ptr, @intCast(full.len), &fl);
        } else if (entry.kind == .directory and !std.mem.eql(u8, entry.name, "node_modules") and !std.mem.eql(u8, entry.name, ".git") and entry.name[0] != '.') {
            const sub = std.fmt.allocPrint(alloc, "{s}/{s}", .{ dir_path, entry.name }) catch continue;
            defer alloc.free(sub);
            prewarmDir(sub);
        }
    }
}

pub fn dispatch(cwd: []const u8) void {
    // Reset work counter for work-stealing
    edgebox_reset_work();
    // Pre-warm file cache (18ms — reads project files into Zig mmap cache)
    prewarmDir(cwd);
    for (0..pool_size) |i| {
        workers[i].cwd = cwd;
        workers[i].worker_count = pool_size;
        workers[i].result = null;
        workers[i].result_ready.store(false, .release);
        workers[i].work_ready.store(true, .release);
    }
    work_mutex.lock();
    work_cond.broadcast();
    work_mutex.unlock();
}

/// Collect results from all workers. Blocks until all done.
/// Returns merged, deduped diagnostics.
pub fn collect() ![]const u8 {
    // Wait for all workers (they set result_ready after eval)
    work_mutex.lock();
    while (true) {
        var all_done = true;
        for (0..pool_size) |i| {
            if (!workers[i].result_ready.load(.acquire)) { all_done = false; break; }
        }
        if (all_done) break;
        work_cond.timedWait(&work_mutex, 1 * std.time.ns_per_ms) catch {};
    }
    work_mutex.unlock();

    // Merge ALL workers' results (sharded — each has unique files, dedup not needed)
    var merged: std.ArrayListUnmanaged(u8) = .{};
    for (0..pool_size) |i| {
        if (workers[i].result) |data| {
            if (data.len > 0) {
                try merged.appendSlice(alloc, data);
                if (data[data.len - 1] != '\n') try merged.append(alloc, '\n');
            }
        }
    }
    return try merged.toOwnedSlice(alloc);
}

/// Worker thread: creates V8 isolate, loads TSC, loops waiting for work.
fn workerLoop(worker_id: u32) void {
    const wid: usize = @intCast(worker_id);

    // Create V8 isolate from snapshot (TypeScript pre-loaded, instant)
    const isolate = edgebox_v8_create_isolate_from_snapshot();
    if (isolate == null) return;
    const context = edgebox_v8_setup_context(isolate);
    if (context == null) { edgebox_v8_dispose_isolate(isolate); return; }

    // Snapshot includes worker_init + recipe — no post-restore eval needed.
    // IO callbacks are registered by setup_context (same functions as snapshot).
    _ = std.posix.write(2, "[v8pool] worker ready\n") catch {};

    while (!workers[wid].shutdown.load(.acquire)) {
        // Park on condvar
        work_mutex.lock();
        while (!workers[wid].work_ready.load(.acquire) and !workers[wid].shutdown.load(.acquire)) {
            work_cond.timedWait(&work_mutex, 1 * std.time.ns_per_ms) catch {};
        }
        work_mutex.unlock();

        if (workers[wid].shutdown.load(.acquire)) break;

        // Consume work
        workers[wid].work_ready.store(false, .release);

        if (workers[wid].cwd) |cwd| {
            // Call recipe function: __edgebox_check(cwd, workerId, workerCount)
            const check_code = std.fmt.allocPrint(alloc,
                "typeof __edgebox_check === 'function' ? __edgebox_check('{s}', {d}, {d}) : 'no __edgebox_check: ' + typeof __edgebox_check"
            , .{ cwd, wid, workers[wid].worker_count }) catch {
                workers[wid].result = null;
                continue;
            };
            defer alloc.free(check_code);


            var out_len: c_int = 0;
            const result = edgebox_v8_eval_in_context(isolate, context, check_code.ptr, @intCast(check_code.len), &out_len);
            if (result) |r| {
                if (out_len > 0) {
                    const copy = alloc.alloc(u8, @intCast(out_len)) catch null;
                    if (copy) |c| {
                        @memcpy(c, r[0..@intCast(out_len)]);
                        workers[wid].result = c;
                    }
                }
                edgebox_v8_free(r);
            }
        }

        // Signal done
        workers[wid].result_ready.store(true, .release);
        work_mutex.lock();
        work_cond.broadcast();
        work_mutex.unlock();
    }

    // Cleanup V8 isolate
    edgebox_v8_exit_isolate(isolate);
    edgebox_v8_dispose_isolate(isolate);
}
