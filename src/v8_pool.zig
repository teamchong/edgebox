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
extern fn edgebox_v8_create_snapshot([*]const u8, c_int, [*]const u8, c_int) c_int;
extern fn edgebox_v8_create_isolate_from_snapshot() ?*anyopaque;
extern fn edgebox_set_daemon_mode(c_int) void;
extern fn edgebox_get_result(c_int, *c_int) ?[*]const u8;

// IO functions from edgebox_io.zig (shared across all workers)
extern fn edgebox_read_file([*]const u8, c_int, *c_int) ?[*]const u8;

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

    // Create V8 snapshot with TypeScript pre-loaded (eliminates 3s per worker)
    var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
    const eb_cwd = std.fs.cwd().realpath(".", &cwd_buf) catch "/tmp";
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
        const ts_path = "node_modules/typescript/lib/typescript.js";
        var ts_len: c_int = 0;
        const ts_src = edgebox_read_file(ts_path.ptr, @intCast(ts_path.len), &ts_len);
        if (ts_src != null and ts_len > 0) {
            _ = std.posix.write(2, "[v8pool] creating snapshot...\n") catch {};
            const snap_size = edgebox_v8_create_snapshot(ts_src.?, ts_len, s.ptr, @intCast(s.len));
            if (snap_size > 0) {
                var snap_msg: [64]u8 = undefined;
                const msg = std.fmt.bufPrint(&snap_msg, "[v8pool] snapshot: {d} bytes\n", .{snap_size}) catch "[v8pool] snapshot created\n";
                _ = std.posix.write(2, msg) catch {};
            }
        }
    }

    // Spawn worker threads (restore from snapshot — instant TypeScript)
    for (0..n) |i| {
        workers[i].worker_id = @intCast(i);
        workers[i].worker_count = n;
        workers[i].thread = try std.Thread.spawn(.{}, workerLoop, .{@as(u32, @intCast(i))});
    }
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
        if (entry.kind == .file and (std.mem.endsWith(u8, entry.name, ".ts") or std.mem.endsWith(u8, entry.name, ".json"))) {
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
    // Pre-warm file cache (Zig reads all files into mmap before V8 touches them)
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
        work_cond.timedWait(&work_mutex, 100 * std.time.ns_per_ms) catch {};
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

    // Override stub require/process with real Zig-backed versions
    // (snapshot has stubs, we replace with real IO)
    const module_shim =
        "globalThis.module = { exports: {} };" ++
        "globalThis.__filename = '/edgebox/worker.js';" ++
        "globalThis.__dirname = '/edgebox';" ++
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
        "};" ++
        "if(typeof Buffer==='undefined') globalThis.Buffer={from:function(s){return s;},isBuffer:function(){return false;},alloc:function(n){return new Uint8Array(n);},concat:function(b){return b[0]||new Uint8Array(0);},byteLength:function(s){return typeof s==='string'?s.length:0;},isEncoding:function(){return true;}};" ++
        "if(typeof setTimeout==='undefined') globalThis.setTimeout=function(f,t){f();return 0;};" ++
        "if(typeof clearTimeout==='undefined') globalThis.clearTimeout=function(){};" ++
        "if(typeof setInterval==='undefined') globalThis.setInterval=function(){return 0;};" ++
        "if(typeof clearInterval==='undefined') globalThis.clearInterval=function(){};" ++
        "if(typeof queueMicrotask==='undefined') globalThis.queueMicrotask=function(f){Promise.resolve().then(f);};" ++
        "if(typeof console==='undefined') globalThis.console={log:function(){},warn:function(){},error:function(){},info:function(){},debug:function(){}};"
    ;
    {
        var el: c_int = 0;
        const r = edgebox_v8_eval_in_context(isolate, context, module_shim.ptr, @intCast(module_shim.len), &el);
        if (r) |rr| edgebox_v8_free(rr);
    }

    // Re-initialize ts.sys after snapshot restore (snapshot had stub require)
    // The module_shim above sets up real require with Zig IO
    const reinit_sys = "if(typeof ts !== 'undefined' && !ts.sys) { try { ts.setSys(ts.createSystem()); } catch(e) { /* createSystem may not exist */ } }";
    {
        var rl: c_int = 0;
        const rr = edgebox_v8_eval_in_context(isolate, context, reinit_sys.ptr, @intCast(reinit_sys.len), &rl);
        if (rr) |r| edgebox_v8_free(r);
    }

    // Check if TypeScript survived snapshot restore
    const ts_check = "typeof ts !== 'undefined' ? 'ts:OK' : (typeof module !== 'undefined' ? 'module:' + typeof module.exports : 'no ts, no module')";
    {
        var cl: c_int = 0;
        const cr = edgebox_v8_eval_in_context(isolate, context, ts_check.ptr, @intCast(ts_check.len), &cl);
        if (cr) |c| {
            _ = std.posix.write(2, "[v8pool] ") catch {};
            _ = std.posix.write(2, c[0..@intCast(cl)]) catch {};
            _ = std.posix.write(2, "\n") catch {};
            edgebox_v8_free(c);
        }
    }
    // Set ts alias
    const ts_alias = "globalThis.ts = globalThis.ts || globalThis.module.exports;";
    {
        var el2: c_int = 0;
        const r3 = edgebox_v8_eval_in_context(isolate, context, ts_alias.ptr, @intCast(ts_alias.len), &el2);
        if (r3) |rr| edgebox_v8_free(rr);
    }

    // Load TSC recipe (sets up globalThis.__edgebox_check)
    const recipe_path = "src/tsc-recipe/checker-parallel.js";
    var recipe_len: c_int = 0;
    const recipe_src = edgebox_read_file(recipe_path.ptr, @intCast(recipe_path.len), &recipe_len);
    if (recipe_src != null and recipe_len > 0) {
        var rel: c_int = 0;
        const rr = edgebox_v8_eval_in_context(isolate, context, recipe_src.?, recipe_len, &rel);
        if (rr) |r| edgebox_v8_free(r);
    }

    _ = std.posix.write(2, "[v8pool] worker ready\n") catch {};

    while (!workers[wid].shutdown.load(.acquire)) {
        // Park on condvar
        work_mutex.lock();
        while (!workers[wid].work_ready.load(.acquire) and !workers[wid].shutdown.load(.acquire)) {
            work_cond.timedWait(&work_mutex, 100 * std.time.ns_per_ms) catch {};
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
