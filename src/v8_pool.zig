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
extern fn edgebox_clear_file_cache() void;
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

            // Load Zig V8 bridge (creates __zigCreateSourceFile)
            const bridge_path = std.fmt.allocPrint(alloc, "{s}/src/tsc-recipe/zig_v8_bridge.js", .{eb_root}) catch null;
            var bridge_len: c_int = 0;
            var bridge_src: ?[*]const u8 = null;
            if (bridge_path) |bp| {
                defer alloc.free(bp);
                bridge_src = edgebox_read_file(bp.ptr, @intCast(bp.len), &bridge_len);
            }
            // Concatenate recipe + bridge
            var full_recipe: ?[*]const u8 = null;
            var full_recipe_len: c_int = 0;
            if (r_src != null and bridge_src != null and bridge_len > 0) {
                const total = @as(usize, @intCast(r_len)) + 1 + @as(usize, @intCast(bridge_len));
                const buf = alloc.alloc(u8, total) catch null;
                if (buf) |b| {
                    const rl: usize = @intCast(r_len);
                    const bl: usize = @intCast(bridge_len);
                    @memcpy(b[0..rl], r_src.?[0..rl]);
                    b[rl] = '\n';
                    @memcpy(b[rl + 1 .. rl + 1 + bl], bridge_src.?[0..bl]);
                    full_recipe = b.ptr;
                    full_recipe_len = @intCast(total);
                }
            }
            if (full_recipe == null) {
                full_recipe = if (r_src) |rs| rs else "".ptr;
                full_recipe_len = r_len;
            }

            _ = std.posix.write(2, "[v8pool] creating snapshot (with worker_init + recipe + bridge)...\n") catch {};
            const snap_size = edgebox_v8_create_snapshot(
                final_src, final_len,
                s.ptr, @intCast(s.len),
                worker_init_js.ptr, @intCast(worker_init_js.len),
                full_recipe.?, full_recipe_len,
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

/// Recipe transform: inject Zig WASM type registry into TSC.
/// Two patches:
///   1. createType — writes flags to WASM linear memory (zero-copy shared data model)
///   2. isTypeRelatedTo — uses WASM kernel with type IDs (avoids megamorphic IC)
/// Baked into V8 snapshot → TurboFan inlines WASM at callsite.
fn applyRecipeTransform(src: []const u8) ![]const u8 {
    var result = src;

    // Source injections: measured 50ms OVERHEAD on first cold start.
    // createType, setStructuredTypeMembers, isTypeRelatedTo injections duplicate
    // TSC's own fast-paths (isSimpleTypeRelatedTo does the same flag checks).
    // Skip injections when EDGEBOX_LEAN_COLD=1 for fastest first cold.
    // Keep them by default for warm mode where TurboFan inlines WasmGC.
    // Source injections add ~100ms overhead on first cold (duplicate TSC's isSimpleTypeRelatedTo).
    // Default: skip injections. Set EDGEBOX_PATCHES=1 to enable (useful for warm mode).
    const enable_patches = std.posix.getenv("EDGEBOX_PATCHES") != null;
    if (!enable_patches) {
        _ = std.posix.write(2, "[v8pool] clean cold: no source injections (fastest first cold)\n") catch {};
        return result;
    }

    // Patch 1: createType — write type.id→flags to WasmGC array.
    const ct_needle = "function createType(flags) {";
    const ct_inject = "function createType(flags) {" ++
        // typeCount++ happens AFTER this patch code. type.id = typeCount (after ++).
        // So type.id = typeCount + 1 at this point. Store at typeCount+1.
        "if(typeCount<65534&&globalThis.__gcFlagsArr)" ++
        "globalThis.__gcFlags.setFlag(globalThis.__gcFlagsArr,(typeCount+1)|0,flags|0);";

    const ct_idx = std.mem.indexOf(u8, result, ct_needle) orelse {
        _ = std.posix.write(2, "[v8pool] FATAL: createType needle not found in TSC source\n") catch {};
        return error.NeedleNotFound;
    };
    _ = std.posix.write(2, "[v8pool] patching createType → WASM flags\n") catch {};
    {
        const ct_len = result.len - ct_needle.len + ct_inject.len;
        const ct_result = try alloc.alloc(u8, ct_len);
        @memcpy(ct_result[0..ct_idx], result[0..ct_idx]);
        @memcpy(ct_result[ct_idx .. ct_idx + ct_inject.len], ct_inject);
        @memcpy(ct_result[ct_idx + ct_inject.len .. ct_len], result[ct_idx + ct_needle.len ..]);
        result = ct_result;
    }

    // Skip expensive patches — only createType + isSimpleTypeRelatedTo for minimal overhead
    const skip_expensive = std.posix.getenv("EDGEBOX_LEAN") != null;
    if (skip_expensive) {
        _ = std.posix.write(2, "[v8pool] lean mode: createType + isSimpleTypeRelatedTo (strictNull-aware)\n") catch {};
        // Patch isSimpleTypeRelatedTo with strictNullChecks-aware WASM
        const lean_needle = "function isTypeRelatedTo(source, target, relation) {";
        // Patch isTypeRelatedTo (not isSimpleTypeRelatedTo) — called once per comparison.
        // Skips fresh literal normalization + isSimpleTypeRelatedTo when WASM resolves.
        const lean_inject = "function isTypeRelatedTo(source, target, relation) {" ++
            "var _si=source.id,_ti=target.id;" ++
            "if(_si>0&&_si<65536&&_ti>0&&_ti<65536&&globalThis.__gcCheckRel){" ++
            "var _rel=relation===assignableRelation?0:relation===comparableRelation?1:" ++
            "relation===strictSubtypeRelation?3:relation===identityRelation?4:2;" ++
            "var _r=globalThis.__gcCheckRel(_si,_ti,_rel,strictNullChecks?1:0);" ++
            "if(_r===1)return true;" ++
            "if(_r===0)return false;" ++
            "}";
        if (std.mem.indexOf(u8, result, lean_needle)) |idx2| {
            _ = std.posix.write(2, "[v8pool] patching isSimpleTypeRelatedTo → frozen WASM\n") catch {};
            const len2 = result.len - lean_needle.len + lean_inject.len;
            const r2 = try alloc.alloc(u8, len2);
            @memcpy(r2[0..idx2], result[0..idx2]);
            @memcpy(r2[idx2 .. idx2 + lean_inject.len], lean_inject);
            @memcpy(r2[idx2 + lean_inject.len .. len2], result[idx2 + lean_needle.len ..]);
            result = r2;
        }
        return result;
    }

    // Patch 2: createSourceFile — cache .d.ts parsed ASTs.
    // TSC's internal createGetSourceFile calls the module-level createSourceFile
    // (NOT ts.createSourceFile), so we must patch the actual function.
    // Saves 573ms on cold start — lib .d.ts files parsed once, cached forever.
    const csf_full_needle = "function createSourceFile(fileName, sourceText, languageVersionOrOptions, setParentNodes = false, scriptKind) {";
    // Inject: cache check at start + store wrapper using a flag
    const csf_full_inject = "function createSourceFile(fileName, sourceText, languageVersionOrOptions, setParentNodes = false, scriptKind) {" ++
        "if(typeof fileName==='string'&&fileName.endsWith('.d.ts')&&globalThis.__sfCache){" ++
        "var _cv=globalThis.__sfCache.get(fileName);" ++
        "if(_cv&&_cv.text===sourceText)return _cv;}" ++
        // We can't easily store at the end without wrapping the whole function.
        // Instead, we'll rely on the pre-parse to populate the cache for lib files.
        // Non-pre-parsed .d.ts files will be parsed normally (only once per daemon lifetime).
        "";

    const csf_idx = std.mem.indexOf(u8, result, csf_full_needle) orelse {
        _ = std.posix.write(2, "[v8pool] FATAL: createSourceFile needle not found\n") catch {};
        return error.NeedleNotFound;
    };
    _ = std.posix.write(2, "[v8pool] patching createSourceFile → .d.ts cache\n") catch {};
    {
        const csf_len = result.len - csf_full_needle.len + csf_full_inject.len;
        const csf_result = try alloc.alloc(u8, csf_len);
        @memcpy(csf_result[0..csf_idx], result[0..csf_idx]);
        @memcpy(csf_result[csf_idx .. csf_idx + csf_full_inject.len], csf_full_inject);
        @memcpy(csf_result[csf_idx + csf_full_inject.len .. csf_len], result[csf_idx + csf_full_needle.len ..]);
        result = csf_result;
    }

    // Patch 3a: setStructuredTypeMembers — compute bloom filter of member names.
    // When a type's members are resolved, hash all member names into an i32 bloom filter.
    // Stored in WasmGC array. Used by __gcCheck for Object→Object fast-reject.
    const ssm_needle = "function setStructuredTypeMembers(type, members, callSignatures, constructSignatures, indexInfos) {";
    const ssm_inject = "function setStructuredTypeMembers(type, members, callSignatures, constructSignatures, indexInfos) {" ++
        "var _bid=type.id;" ++
        "if(_bid>0&&_bid<65536&&members&&members!==emptySymbols&&globalThis.__gcBloomArr&&globalThis.__gcSoa){" ++
        "var _bloom=0;" ++
        "members.forEach(function(_v,_k){" ++
        // Skip optional members — only REQUIRED members matter for structural reject.
        "if(_v.flags&16777216)return;" ++ // SymbolFlags.Optional = 16777216
        // Skip internal members (__index, __call, __new, __constructor) — these are
        // index/call/construct signatures, not named properties. A type with an index
        // signature ({[key:string]:string}) should NOT bloom "__index" — it would
        // falsely reject types without index signatures.
        "if(_k.charCodeAt(0)===95&&_k.charCodeAt(1)===95)return;" ++ // startsWith('__')
        // Each member name hashed INDEPENDENTLY — no carry-over
        "var _h=0;" ++
        "for(var _i=0;_i<_k.length;_i++){_h=((_h<<5)|(_h>>>27))^_k.charCodeAt(_i);}" ++
        // Set 3 bloom bits from this member's hash
        "_bloom|=(1<<(_h&31))|(1<<((_h>>>5)&31))|(1<<((_h>>>10)&31));" ++
        "});" ++
        "if(_bloom)globalThis.__gcFlags.setFlag(globalThis.__gcBloomArr,_bid|0,_bloom|0);" ++
        "}" ++
        // Also populate property hash/type arrays for structural checking
        // Structural property population disabled — overhead > savings on cold.
        // Property population adds ~100ms (forEach + setFlag per prop).
        // Structural WASM check saves ~0ms (too few Object→Object comparisons).
        "if(false&&_bid>0&&_bid<16384&&(type.flags&524288)&&globalThis.__gcPropHashArr){" ++
        "var _pSf=globalThis.__gcFlags.setFlag;" ++
        "var _pHA=globalThis.__gcPropHashArr;" ++
        "var _pTA=globalThis.__gcPropTypeArr;" ++
        "var _pCA=globalThis.__gcPropCountArr;" ++
        "var _pi=0,_pBase=_bid*8;" ++
        "members.forEach(function(_v,_k){" ++
        "if(_pi>=8)return;" ++
        "if(_k.charCodeAt(0)===95&&_k.charCodeAt(1)===95)return;" ++
        // FNV-1a hash of property name
        "var _h=2166136261;" ++
        "for(var _ci=0;_ci<_k.length;_ci++){_h=Math.imul(_h^_k.charCodeAt(_ci),16777619);}" ++
        "_pSf(_pHA,(_pBase+_pi)|0,_h|0);" ++
        // Property type ID (if available)
        "var _pTid=0;" ++
        "if(_v.links&&_v.links.type&&_v.links.type.id>0)_pTid=_v.links.type.id;" ++
        "_pSf(_pTA,(_pBase+_pi)|0,_pTid|0);" ++
        "_pi=_pi+1;" ++
        "});" ++
        "_pSf(_pCA,_bid|0,_pi|0);" ++
        "}";

    const ssm_idx = std.mem.indexOf(u8, result, ssm_needle) orelse {
        _ = std.posix.write(2, "[v8pool] FATAL: setStructuredTypeMembers needle not found\n") catch {};
        return error.NeedleNotFound;
    };
    _ = std.posix.write(2, "[v8pool] patching setStructuredTypeMembers → bloom filter\n") catch {};
    {
        const ssm_len = result.len - ssm_needle.len + ssm_inject.len;
        const ssm_result = try alloc.alloc(u8, ssm_len);
        @memcpy(ssm_result[0..ssm_idx], result[0..ssm_idx]);
        @memcpy(ssm_result[ssm_idx .. ssm_idx + ssm_inject.len], ssm_inject);
        @memcpy(ssm_result[ssm_idx + ssm_inject.len .. ssm_len], result[ssm_idx + ssm_needle.len ..]);
        result = ssm_result;
    }

    // Patch 3b: Store union member IDs when union types are created.
    // Flat layout: unionMembers[typeId * 4] = count, [+1..+3] = member IDs (up to 3)
    // For unions with >3 members, count is stored but we fall through to TSC.
    const union_needle = "type.types = types;\n      type.origin = origin;";
    const union_inject = "type.types = types;" ++
        "if(type.id>0&&type.id<16384&&types.length>0&&types.length<=3){" ++
        "var _ua=globalThis.__gcUnionArr,_uf=globalThis.__gcFlags;" ++
        "if(_ua&&_uf){" ++
        "var _ub=type.id*4;" ++
        "_uf.setFlag(_ua,_ub|0,types.length|0);" ++
        "for(var _ui=0;_ui<types.length;_ui++){" ++
        "if(types[_ui].id>0&&types[_ui].id<65536)" ++
        "_uf.setFlag(_ua,(_ub+_ui+1)|0,types[_ui].id|0);" ++
        "}}}" ++
        "\n      type.origin = origin;";

    const union_idx = std.mem.indexOf(u8, result, union_needle) orelse {
        _ = std.posix.write(2, "[v8pool] FATAL: union types needle not found\n") catch {};
        return error.NeedleNotFound;
    };
    _ = std.posix.write(2, "[v8pool] patching union type → store member IDs\n") catch {};
    {
        const union_len = result.len - union_needle.len + union_inject.len;
        const union_result = try alloc.alloc(u8, union_len);
        @memcpy(union_result[0..union_idx], result[0..union_idx]);
        @memcpy(union_result[union_idx .. union_idx + union_inject.len], union_inject);
        @memcpy(union_result[union_idx + union_inject.len .. union_len], result[union_idx + union_needle.len ..]);
        result = union_result;
    }

    // Patch 3c: isSimpleTypeRelatedTo — REPLACE flag checks with frozen WASM.
    // Uses freeze-compiled checkFlags (QuickJS→LLVM→WASM) that runs at native
    // speed from the FIRST call. No JIT warmup needed.
    // Relation mapping: assignable=0, comparable=1, subtype=2, strictSubtype=3, identity=4
    const simple_needle = "function isSimpleTypeRelatedTo(source, target, relation, errorReporter) {";
    const simple_inject = "function isSimpleTypeRelatedTo(source, target, relation, errorReporter) {" ++
        "if(globalThis.__frozenCheckFlags){" ++
        "var _rel=relation===assignableRelation?0:relation===comparableRelation?1:" ++
        "relation===strictSubtypeRelation?3:relation===identityRelation?4:2;" ++
        "var _r=globalThis.__frozenCheckFlags(source.flags|0,target.flags|0,_rel);" ++
        "if(_r===1)return true;" ++
        "if(_r===0)return false;" ++
        "}";

    const simple_idx = std.mem.indexOf(u8, result, simple_needle) orelse {
        _ = std.posix.write(2, "[v8pool] FATAL: isSimpleTypeRelatedTo needle not found\n") catch {};
        return error.NeedleNotFound;
    };
    _ = std.posix.write(2, "[v8pool] patching isSimpleTypeRelatedTo → WASM fast-path\n") catch {};
    {
        const simple_len = result.len - simple_needle.len + simple_inject.len;
        const simple_result = try alloc.alloc(u8, simple_len);
        @memcpy(simple_result[0..simple_idx], result[0..simple_idx]);
        @memcpy(simple_result[simple_idx .. simple_idx + simple_inject.len], simple_inject);
        @memcpy(simple_result[simple_idx + simple_inject.len .. simple_len], result[simple_idx + simple_needle.len ..]);
        result = simple_result;
    }

    // Patch 3d: getObjectFlags — replace with frozen WASM (flat trampoline).
    const gof_needle = "function getObjectFlags(type) {";
    const gof_inject = "function getObjectFlags(type) {" ++
        "if(globalThis.__frozenGetObjectFlags)return globalThis.__frozenGetObjectFlags(type);";
    if (std.mem.indexOf(u8, result, gof_needle)) |gof_idx| {
        _ = std.posix.write(2, "[v8pool] patching getObjectFlags → frozen WASM\n") catch {};
        const gof_len = result.len - gof_needle.len + gof_inject.len;
        const gof_result = try alloc.alloc(u8, gof_len);
        @memcpy(gof_result[0..gof_idx], result[0..gof_idx]);
        @memcpy(gof_result[gof_idx .. gof_idx + gof_inject.len], gof_inject);
        @memcpy(gof_result[gof_idx + gof_inject.len .. gof_len], result[gof_idx + gof_needle.len ..]);
        result = gof_result;
    }

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

var first_dispatch: bool = true;

pub fn dispatch(cwd: []const u8) void {
    edgebox_reset_work();
    if (first_dispatch) {
        first_dispatch = false;
    } else {
        edgebox_clear_file_cache();
    }
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
