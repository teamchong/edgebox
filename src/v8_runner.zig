// EdgeBox — V8 JavaScript runtime with AOT WASM kernels, snapshots, and parallel IO
//
// Primary entry point for running JS files through embedded V8.
//
// Features:
// - CJS module wrapping with __filename/__dirname
// - V8 code cache for instant script reloads (~0ms parse on warm runs)
// - Embedded bootstrap (no external files needed)
// - Single binary mode: JS + code cache appended to binary (edgebox pack)
//
// Usage:
//   edgebox <script.js> [args...]      Run a JS file
//   edgebox pack <script.js> [output]  Pack into single binary
//
// Build: zig build v8-run   (or: zig build cli)
// Run:   ./zig-out/bin/edgebox node_modules/typescript/lib/_tsc.js --version

const std = @import("std");
const v8 = @import("v8.zig");
const v8_io = @import("v8_io.zig");

// Trailer magic: "EDGEBOX\x00" (8 bytes)
const TRAILER_MAGIC = [8]u8{ 'E', 'D', 'G', 'E', 'B', 'O', 'X', 0 };
const TRAILER_SIZE = 8 + 8 + 8 + 8; // source_len + cache_len + scriptpath_len + magic

pub fn main() !void {
    const alloc = std.heap.page_allocator;

    // Parse args
    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();

    const bin_name = args.next() orelse "edgebox";

    // Check for embedded app first (single binary mode)
    const embedded = readEmbeddedApp(alloc) catch null;

    if (embedded) |app| {
        // Single binary mode — use original script path for __filename/__dirname
        // so TSC can find lib.d.ts files relative to its installation
        const script_filename = app.script_path orelse blk: {
            var exe_path_buf: [std.fs.max_path_bytes]u8 = undefined;
            break :blk std.fs.selfExePath(&exe_path_buf) catch "/usr/bin/edgebox-app";
        };
        return runScript(alloc, app.source, app.cache, script_filename, "edgebox-app", true);
    }

    // Normal mode — parse args for script path or pack subcommand
    const first_arg = args.next() orelse {
        const stderr = std.fs.File.stderr();
        stderr.writeAll("Usage: edgebox <script.js> [args...]\n       edgebox pack <script.js> [output]\n") catch {};
        std.process.exit(1);
    };

    // Check for pack subcommand
    if (std.mem.eql(u8, first_arg, "pack")) {
        const script_arg = args.next() orelse {
            const stderr = std.fs.File.stderr();
            stderr.writeAll("Usage: edgebox pack <script.js> [output-binary]\n") catch {};
            std.process.exit(1);
        };
        const output_name = args.next() orelse blk: {
            // Default output name: strip .js extension
            const base = std.fs.path.basename(script_arg);
            if (std.mem.endsWith(u8, base, ".js")) {
                break :blk base[0 .. base.len - 3];
            }
            break :blk base;
        };
        return packBinary(alloc, script_arg, output_name, bin_name);
    }

    // Normal script execution
    const script_path = first_arg;
    var abs_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_path = std.fs.cwd().realpath(script_path, &abs_buf) catch script_path;

    const script_code = std.fs.cwd().readFileAlloc(alloc, script_path, 64 * 1024 * 1024) catch |err| {
        std.debug.print("[v8] ERROR: could not read {s}: {}\n", .{ script_path, err });
        return err;
    };
    defer alloc.free(script_code);

    // Try to load code cache from disk
    const cache_path = getCachePath(alloc, script_code) catch null;
    var disk_cache: ?[]u8 = null;
    if (cache_path) |cp| {
        disk_cache = std.fs.cwd().readFileAlloc(alloc, cp, 128 * 1024 * 1024) catch null;
    }

    return runScript(alloc, script_code, disk_cache, abs_path, script_path, false);
}

/// Embedded V8 snapshot of the bootstrap context — generated at build time
/// by v8_snapshot_gen. Contains console, require, fs, Buffer, process, etc.
/// Using this skips ~200ms of bootstrap eval on every startup.
const embedded_snapshot = @embedFile("v8_bootstrap.snapshot");

/// External references for V8 snapshot deserialization — must match the array
/// used during snapshot creation (in v8_snapshot_gen.zig).
/// Order: [ioSyncCallback, ioBatchCallback, 0 (null terminator)]
var external_refs: [3]usize = .{ 0, 0, 0 };

fn getExternalRefs() *const [3]usize {
    if (external_refs[0] == 0) {
        external_refs[0] = @intFromPtr(&v8_io.ioSyncCallback);
        external_refs[1] = @intFromPtr(&v8_io.ioBatchCallback);
    }
    return &external_refs;
}

/// Core script execution: initialize V8, load bootstrap, compile + run script.
fn runScript(alloc: std.mem.Allocator, script_code: []const u8, cache_bytes: ?[]const u8, abs_path: []const u8, display_name: []const u8, is_embedded: bool) !void {
    _ = display_name;

    // Initialize V8
    _ = try v8.initPlatform();
    defer v8.disposePlatform();

    // Create isolate from embedded snapshot — bootstrap context is pre-compiled
    const isolate = if (embedded_snapshot.len > 0)
        v8.SnapshotApi.createIsolateFromSnapshot(embedded_snapshot.ptr, @intCast(embedded_snapshot.len), getExternalRefs())
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
        // Snapshot loaded — IO bridge callback restored via external_refs.
        // Refresh runtime-specific data (argv, env come from snapshot-gen time).
        _ = v8.eval(isolate, context, snapshot_init_js, "snapshot_init.js") catch {};
    } else {
        // Fallback: register IO bridge and eval bootstrap from source
        v8_io.registerGlobals(isolate, context);
        const bootstrap_code = @embedFile("v8_bootstrap.js");
        _ = v8.eval(isolate, context, bootstrap_code, "v8_bootstrap.js") catch |err| {
            std.debug.print("[v8] ERROR: bootstrap failed: {}\n", .{err});
            return err;
        };
    }

    // Register parallel execution API (lazy — no cold-start cost)
    const v8_parallel = @import("v8_parallel.zig");
    v8_parallel.registerGlobals(isolate, context);

    // Register channel API (Go-like channels for inter-worker communication)
    const v8_channel = @import("v8_channel.zig");
    v8_channel.registerGlobals(isolate, context);

    // Create JS-friendly wrappers: edgebox.parallel, edgebox.map, edgebox.reduce, edgebox.channel
    const parallel_init_js = @embedFile("v8_parallel_init.js");
    _ = v8.eval(isolate, context, parallel_init_js, "v8_parallel_init.js") catch {};

    // For packed binaries: ensure process.argv has [exe, script, ...args] format
    // TSC uses process.argv.slice(2), so we need argv[1] to be the script path.
    // In embedded mode, OS argv is [binary, ...args] — insert synthetic script path.
    if (is_embedded) {
        const argv_fix = try std.fmt.allocPrint(alloc,
            "process.argv.splice(1, 0, \"{s}\");",
            .{abs_path},
        );
        defer alloc.free(argv_fix);
        _ = v8.eval(isolate, context, argv_fix, "argv_fix.js") catch {};
    }

    // Auto-inject zero-copy relation cache for TSC
    // Patches TSC's 5 relation caches from Map → Int32Array hash table
    var patched_script: ?[]u8 = null;
    defer if (patched_script) |ps| alloc.free(ps);
    var final_code = script_code;

    if (std.mem.indexOf(u8, script_code, "var assignableRelation") != null and
        std.mem.indexOf(u8, script_code, "var identityRelation") != null)
    {
        const tsc_shim = @embedFile("v8_tsc_shim.js");
        _ = v8.eval(isolate, context, tsc_shim, "v8_tsc_shim.js") catch {};

        // Patch ALL Map caches: "/* @__PURE__ */ new Map()" → "new __FastRelationCache()"
        // FastMap is a full Map drop-in with adaptive Int32Array fast path for numeric keys.
        // Supports: get, set, has, delete, clear, forEach, entries, keys, values, size, [Symbol.iterator]
        const needle = "/* @__PURE__ */ new Map()";
        const replacement = "new __FastRelationCache()";
        var count: usize = 0;
        {
            var ci: usize = 0;
            while (ci + needle.len <= script_code.len) : (ci += 1) {
                if (std.mem.startsWith(u8, script_code[ci..], needle)) count += 1;
            }
        }
        if (count > 0) {
            var buf = try alloc.alloc(u8, script_code.len + count * replacement.len);
            var wi: usize = 0;
            var ri: usize = 0;
            while (ri < script_code.len) {
                if (ri + needle.len <= script_code.len and
                    std.mem.startsWith(u8, script_code[ri..], needle))
                {
                    @memcpy(buf[wi..][0..replacement.len], replacement);
                    wi += replacement.len;
                    ri += needle.len;
                } else {
                    buf[wi] = script_code[ri];
                    wi += 1;
                    ri += 1;
                }
            }
            patched_script = buf[0..wi];
            final_code = patched_script.?;
        }
    }

    // Patch getRelationKey to return packed integer for simple cases
    // Eliminates string allocation "N,N" on every type relation check
    if (patched_script != null or final_code.ptr != script_code.ptr) {
        const grk_needle = "return isTypeReferenceWithGenericArguments(source) && isTypeReferenceWithGenericArguments(target) ? getGenericTypeReferenceRelationKey(source, target, postFix, ignoreConstraints) : `${source.id},${target.id}${postFix}`;";
        const grk_replacement = "if(!postFix && !isTypeReferenceWithGenericArguments(source) && !isTypeReferenceWithGenericArguments(target) && source.id < 0x100000 && target.id < 0x100000) return (source.id << 20) | target.id; return isTypeReferenceWithGenericArguments(source) && isTypeReferenceWithGenericArguments(target) ? getGenericTypeReferenceRelationKey(source, target, postFix, ignoreConstraints) : `${source.id},${target.id}${postFix}`;";
        if (std.mem.indexOf(u8, final_code, grk_needle)) |grk_pos| {
            const extra = grk_replacement.len - grk_needle.len + 1;
            var grk_buf = try alloc.alloc(u8, final_code.len + extra);
            @memcpy(grk_buf[0..grk_pos], final_code[0..grk_pos]);
            @memcpy(grk_buf[grk_pos..][0..grk_replacement.len], grk_replacement);
            @memcpy(grk_buf[grk_pos + grk_replacement.len ..][0 .. final_code.len - grk_pos - grk_needle.len], final_code[grk_pos + grk_needle.len ..]);
            const grk_total = grk_pos + grk_replacement.len + (final_code.len - grk_pos - grk_needle.len);
            if (patched_script) |ps| alloc.free(ps);
            patched_script = grk_buf[0..grk_total];
            final_code = patched_script.?;
        }
    }

    // Additional TSC source patches: SOA columns for type flags
    // Patch "result.id = typeCount" → "result.id = typeCount; __type_flags[typeCount] = flags"
    // This stores type flags in a flat Int32Array for O(1) access in hot paths.
    if (patched_script != null or final_code.ptr != script_code.ptr) {
        // Already have a mutable buffer — check if we can add SOA patches
        const soa_needle = "result.id = typeCount;";
        const soa_replacement = "result.id = typeCount; if(typeof __type_flags !== 'undefined' && typeCount < 262144) __type_flags[typeCount] = flags;";
        if (std.mem.indexOf(u8, final_code, soa_needle)) |_| {
            // Need to re-patch with SOA additions
            var soa_count: usize = 0;
            {
                var si: usize = 0;
                while (si + soa_needle.len <= final_code.len) : (si += 1) {
                    if (std.mem.startsWith(u8, final_code[si..], soa_needle)) soa_count += 1;
                }
            }
            if (soa_count > 0) {
                const extra = soa_count * (soa_replacement.len - soa_needle.len + 1);
                var soa_buf = try alloc.alloc(u8, final_code.len + extra);
                var sw: usize = 0;
                var sr: usize = 0;
                while (sr < final_code.len) {
                    if (sr + soa_needle.len <= final_code.len and
                        std.mem.startsWith(u8, final_code[sr..], soa_needle))
                    {
                        @memcpy(soa_buf[sw..][0..soa_replacement.len], soa_replacement);
                        sw += soa_replacement.len;
                        sr += soa_needle.len;
                    } else {
                        soa_buf[sw] = final_code[sr];
                        sw += 1;
                        sr += 1;
                    }
                }
                // Free old patched buffer if we own it
                if (patched_script) |ps| alloc.free(ps);
                patched_script = soa_buf[0..sw];
                final_code = patched_script.?;
            }
        }
    }

    // Wrap script as CJS module with per-module require
    const dirname = std.fs.path.dirname(abs_path) orelse ".";
    const prefix = "(function(__filename, __dirname) { var module = globalThis.module; var exports = module.exports; var require = globalThis._loadModule ? function(id) { return globalThis._loadModule(id, __dirname); } : globalThis.require;\n";
    const suffix = try std.fmt.allocPrint(alloc,
        "\n}})(\"{s}\", \"{s}\");",
        .{ abs_path, dirname },
    );
    defer alloc.free(suffix);

    const total_len = prefix.len + final_code.len + suffix.len;
    const wrapped = try alloc.alloc(u8, total_len);
    defer alloc.free(wrapped);

    @memcpy(wrapped[0..prefix.len], prefix);
    @memcpy(wrapped[prefix.len..][0..final_code.len], final_code);
    @memcpy(wrapped[prefix.len + final_code.len ..][0..suffix.len], suffix);

    // Compile with code cache
    var try_catch = v8.TryCatch.init(isolate);
    defer try_catch.deinit();

    const source_str = v8.StringApi.fromUtf8(isolate, wrapped) orelse {
        std.debug.print("[v8] ERROR: failed to create source string\n", .{});
        std.process.exit(1);
    };
    const name_str = v8.StringApi.fromUtf8(isolate, abs_path) orelse {
        std.debug.print("[v8] ERROR: failed to create name string\n", .{});
        std.process.exit(1);
    };
    var origin = v8.ScriptOrigin.init(@ptrCast(name_str));

    // Load code cache into V8's CachedData if available
    var cached_data: ?*v8.CachedData = null;
    if (cache_bytes) |cb| {
        if (cb.len > 0) {
            // V8 requires the buffer to outlive compilation, and CachedData is
            // created with BufferNotOwned so we keep ownership of cache_bytes.
            cached_data = v8.ScriptCompilerApi.createCachedData(cb.ptr, @intCast(cb.len));
        }
    }

    // Construct Source in-place — avoids bitwise copy of C++ object with internal pointers.
    // Takes ownership of cached_data (freed by Source destructor on deinit).
    var compiler_source: v8.CompilerSource = .{};
    compiler_source.initInPlace(source_str, &origin, cached_data);
    defer compiler_source.deinit();

    const compile_options: c_int = if (cached_data != null)
        v8.ScriptCompilerApi.kConsumeCodeCache
    else
        v8.ScriptCompilerApi.kNoCompileOptions;

    const script = v8.ScriptCompilerApi.compile(context, &compiler_source, compile_options) orelse {
        reportException(&try_catch, isolate, context);
        std.process.exit(1);
    };

    // Save code cache if this is a fresh compile (not embedded mode)
    var cache_was_used = false;
    if (cached_data != null) {
        if (compiler_source.getCachedData()) |cd| {
            const info = v8.ScriptCompilerApi.getCachedDataBytes(cd);
            cache_was_used = !info.rejected;
        }
    }
    if (!cache_was_used) {
        const cache_path = getCachePath(alloc, script_code) catch null;
        if (cache_path) |cp| {
            const unbound = v8.ScriptExtApi.getUnboundScript(script);
            const new_cache = v8.UnboundScriptApi.createCodeCache(unbound);
            const cache_info = v8.ScriptCompilerApi.getCachedDataBytes(new_cache);
            if (cache_info.length > 0) {
                const cache_data_slice = cache_info.data[0..@intCast(cache_info.length)];
                if (std.fs.path.dirname(cp)) |dir| {
                    std.fs.cwd().makePath(dir) catch {};
                }
                if (std.fs.cwd().createFile(cp, .{})) |f| {
                    defer f.close();
                    f.writeAll(cache_data_slice) catch {};
                } else |_| {}
            }
        }
    }

    // Note: do NOT call deleteCachedData here — the Source destructor
    // (compiler_source.deinit) owns the CachedData and frees it.

    // Execute
    _ = v8.ScriptApi.run(script, context) orelse {
        reportException(&try_catch, isolate, context);
        std.process.exit(1);
    };
}

/// Pack a JS file into a single binary by appending source + code cache.
fn packBinary(alloc: std.mem.Allocator, script_path: []const u8, output_name: []const u8, self_exe_name: []const u8) !void {
    const stdout = std.fs.File.stdout();

    // Read the script source and resolve its absolute path
    const script_code = std.fs.cwd().readFileAlloc(alloc, script_path, 64 * 1024 * 1024) catch |err| {
        std.debug.print("[pack] ERROR: could not read {s}: {}\n", .{ script_path, err });
        return err;
    };
    defer alloc.free(script_code);

    var script_abs_buf: [std.fs.max_path_bytes]u8 = undefined;
    const script_abs_path = std.fs.cwd().realpath(script_path, &script_abs_buf) catch script_path;

    // Read our own executable as the template
    var self_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const self_path = std.fs.selfExePath(&self_path_buf) catch blk: {
        // Fallback: try to find via argv[0]
        break :blk std.fs.cwd().realpath(self_exe_name, &self_path_buf) catch {
            std.debug.print("[pack] ERROR: cannot find own executable\n", .{});
            return error.SelfExeNotFound;
        };
    };

    const self_exe = std.fs.openFileAbsolute(self_path, .{}) catch |err| {
        std.debug.print("[pack] ERROR: cannot open {s}: {}\n", .{ self_path, err });
        return err;
    };
    defer self_exe.close();

    const self_stat = self_exe.stat() catch |err| {
        std.debug.print("[pack] ERROR: cannot stat self: {}\n", .{err});
        return err;
    };
    const self_size = self_stat.size;

    // Check if self already has embedded data (don't double-pack)
    var actual_binary_size = self_size;
    if (self_size > TRAILER_SIZE) {
        self_exe.seekTo(self_size - TRAILER_SIZE) catch {};
        var trailer_buf: [TRAILER_SIZE]u8 = undefined;
        const read_len = self_exe.readAll(&trailer_buf) catch 0;
        if (read_len == TRAILER_SIZE) {
            if (std.mem.eql(u8, trailer_buf[24..32], &TRAILER_MAGIC)) {
                // Already packed — use original binary size
                const embedded_source_len = std.mem.readInt(u64, trailer_buf[0..8], .little);
                const embedded_cache_len = std.mem.readInt(u64, trailer_buf[8..16], .little);
                const embedded_path_len = std.mem.readInt(u64, trailer_buf[16..24], .little);
                actual_binary_size = self_size - embedded_source_len - embedded_cache_len - embedded_path_len - TRAILER_SIZE;
            }
        }
        self_exe.seekTo(0) catch {};
    }

    // Generate code cache by compiling with V8
    stdout.writeAll("[pack] Generating code cache...\n") catch {};

    var cache_data: []const u8 = &.{};
    const cache_alloc = generateCodeCache(alloc, script_code) catch null;
    if (cache_alloc) |ca| cache_data = ca;
    defer if (cache_alloc) |ca| alloc.free(ca);

    // Write output binary: [self binary] [JS source] [code cache] [trailer]
    const out_file = std.fs.cwd().createFile(output_name, .{}) catch |err| {
        std.debug.print("[pack] ERROR: cannot create {s}: {}\n", .{ output_name, err });
        return err;
    };
    defer out_file.close();

    // Copy template binary
    var copied: u64 = 0;
    var copy_buf: [64 * 1024]u8 = undefined;
    while (copied < actual_binary_size) {
        const to_read = @min(copy_buf.len, actual_binary_size - copied);
        const n = self_exe.read(copy_buf[0..to_read]) catch break;
        if (n == 0) break;
        out_file.writeAll(copy_buf[0..n]) catch |err| {
            std.debug.print("[pack] ERROR: write failed: {}\n", .{err});
            return err;
        };
        copied += n;
    }

    // Append JS source
    out_file.writeAll(script_code) catch |err| {
        std.debug.print("[pack] ERROR: write source failed: {}\n", .{err});
        return err;
    };

    // Append code cache
    out_file.writeAll(cache_data) catch |err| {
        std.debug.print("[pack] ERROR: write cache failed: {}\n", .{err});
        return err;
    };

    // Append original script path (for __filename/__dirname resolution)
    out_file.writeAll(script_abs_path) catch |err| {
        std.debug.print("[pack] ERROR: write path failed: {}\n", .{err});
        return err;
    };

    // Trailer: [source_len: u64] [cache_len: u64] [path_len: u64] [magic: 8]
    var trailer: [TRAILER_SIZE]u8 = undefined;
    std.mem.writeInt(u64, trailer[0..8], @intCast(script_code.len), .little);
    std.mem.writeInt(u64, trailer[8..16], @intCast(cache_data.len), .little);
    std.mem.writeInt(u64, trailer[16..24], @intCast(script_abs_path.len), .little);
    @memcpy(trailer[24..32], &TRAILER_MAGIC);
    out_file.writeAll(&trailer) catch |err| {
        std.debug.print("[pack] ERROR: write trailer failed: {}\n", .{err});
        return err;
    };

    // Make executable
    out_file.chmod(0o755) catch {};

    const total_size = actual_binary_size + script_code.len + cache_data.len + script_abs_path.len + TRAILER_SIZE;
    var msg_buf: [512]u8 = undefined;
    const msg = std.fmt.bufPrint(&msg_buf,
        "[pack] Created {s} ({d:.1} MB)\n  Binary: {d:.1} MB, Source: {d:.0} KB, Cache: {d:.0} KB\n",
        .{
            output_name,
            @as(f64, @floatFromInt(total_size)) / (1024 * 1024),
            @as(f64, @floatFromInt(actual_binary_size)) / (1024 * 1024),
            @as(f64, @floatFromInt(script_code.len)) / 1024,
            @as(f64, @floatFromInt(cache_data.len)) / 1024,
        },
    ) catch "pack done\n";
    stdout.writeAll(msg) catch {};
}

/// Read embedded app from the end of our own binary (single binary mode).
fn readEmbeddedApp(alloc: std.mem.Allocator) !struct { source: []const u8, cache: ?[]const u8, script_path: ?[]const u8 } {
    var self_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const self_path = try std.fs.selfExePath(&self_path_buf);

    const file = try std.fs.openFileAbsolute(self_path, .{});
    defer file.close();

    const stat = try file.stat();
    if (stat.size < TRAILER_SIZE) return error.NotPacked;

    // Read trailer: [source_len: u64] [cache_len: u64] [path_len: u64] [magic: 8]
    try file.seekTo(stat.size - TRAILER_SIZE);
    var trailer: [TRAILER_SIZE]u8 = undefined;
    const read_len = try file.readAll(&trailer);
    if (read_len != TRAILER_SIZE) return error.NotPacked;

    // Check magic
    if (!std.mem.eql(u8, trailer[24..32], &TRAILER_MAGIC)) return error.NotPacked;

    const source_len = std.mem.readInt(u64, trailer[0..8], .little);
    const cache_len = std.mem.readInt(u64, trailer[8..16], .little);
    const path_len = std.mem.readInt(u64, trailer[16..24], .little);

    if (source_len == 0 or source_len > 128 * 1024 * 1024) return error.NotPacked;

    // Read source
    const data_start = stat.size - TRAILER_SIZE - path_len - cache_len - source_len;
    try file.seekTo(data_start);
    const source = try alloc.alloc(u8, source_len);
    const src_read = try file.readAll(source);
    if (src_read != source_len) return error.NotPacked;

    // Read cache
    var cache: ?[]const u8 = null;
    if (cache_len > 0) {
        const cache_buf = try alloc.alloc(u8, cache_len);
        const cache_read = try file.readAll(cache_buf);
        if (cache_read == cache_len) {
            cache = cache_buf;
        }
    }

    // Read script path
    var script_path: ?[]const u8 = null;
    if (path_len > 0 and path_len < 4096) {
        const path_buf = try alloc.alloc(u8, path_len);
        const path_read = try file.readAll(path_buf);
        if (path_read == path_len) {
            script_path = path_buf;
        }
    }

    return .{ .source = source, .cache = cache, .script_path = script_path };
}

/// Generate V8 code cache for a script by compiling it.
fn generateCodeCache(alloc: std.mem.Allocator, script_code: []const u8) ![]const u8 {
    // We need V8 to compile the script to generate the cache.
    // Use a temporary V8 instance.
    _ = try v8.initPlatform();
    defer v8.disposePlatform();

    const isolate = v8.IsolateApi.create();
    defer v8.IsolateApi.dispose(isolate);
    v8.IsolateApi.enter(isolate);
    defer v8.IsolateApi.exit(isolate);

    var hs = v8.HandleScope.init(isolate);
    defer hs.deinit();
    const ctx = v8.ContextApi.create(isolate);
    v8.ContextApi.enter(ctx);
    defer v8.ContextApi.exit(ctx);

    // Wrap as CJS
    const prefix = "(function(exports, require, module, __filename, __dirname) {\n";
    const suffix_str = "\n})(globalThis.module.exports, require, globalThis.module, \"app.js\", \".\");";
    const total = prefix.len + script_code.len + suffix_str.len;
    const wrapped = try alloc.alloc(u8, total);
    defer alloc.free(wrapped);

    @memcpy(wrapped[0..prefix.len], prefix);
    @memcpy(wrapped[prefix.len..][0..script_code.len], script_code);
    @memcpy(wrapped[prefix.len + script_code.len ..][0..suffix_str.len], suffix_str);

    const src = v8.StringApi.fromUtf8(isolate, wrapped) orelse return error.StringFailed;
    const name = v8.StringApi.fromUtf8(isolate, "app.js") orelse return error.StringFailed;
    var origin = v8.ScriptOrigin.init(@ptrCast(name));

    var cs: v8.CompilerSource = .{};
    cs.initInPlace(src, &origin, null);
    defer cs.deinit();

    const script = v8.ScriptCompilerApi.compile(ctx, &cs, v8.ScriptCompilerApi.kNoCompileOptions) orelse
        return error.CompileFailed;

    const unbound = v8.ScriptExtApi.getUnboundScript(script);
    const cache = v8.UnboundScriptApi.createCodeCache(unbound);
    const info = v8.ScriptCompilerApi.getCachedDataBytes(cache);

    if (info.length <= 0) return error.NoCacheProduced;

    const result = try alloc.alloc(u8, @intCast(info.length));
    @memcpy(result, info.data[0..@intCast(info.length)]);
    return result;
}

/// Report a JavaScript exception to stderr.
fn reportException(try_catch: *const v8.TryCatch, isolate: *v8.Isolate, context: *const v8.Context) void {
    const stderr = std.fs.File.stderr();
    if (try_catch.stackTrace(context)) |stack_val| {
        if (v8.ValueApi.toString(stack_val, context)) |stack_str| {
            var stack_buf: [8192]u8 = undefined;
            const stack_len = v8.StringApi.writeUtf8(stack_str, isolate, &stack_buf);
            stderr.writeAll(stack_buf[0..stack_len]) catch {};
            stderr.writeAll("\n") catch {};
        }
    } else if (try_catch.exception()) |exc_val| {
        if (v8.ValueApi.toString(exc_val, context)) |exc_str| {
            var exc_buf: [4096]u8 = undefined;
            const exc_len = v8.StringApi.writeUtf8(exc_str, isolate, &exc_buf);
            stderr.writeAll(exc_buf[0..exc_len]) catch {};
            stderr.writeAll("\n") catch {};
        }
    }
}

/// Compute cache path based on script content hash.
fn getCachePath(allocator: std.mem.Allocator, content: []const u8) ![]const u8 {
    var hash: u64 = 14695981039346656037;
    for (content) |byte| {
        hash ^= byte;
        hash *%= 1099511628211;
    }

    const home = std.process.getEnvVarOwned(allocator, "HOME") catch return error.NoHome;
    defer allocator.free(home);

    return try std.fmt.allocPrint(allocator, "{s}/.cache/edgebox/v8-cache/{x}.codecache", .{ home, hash });
}

/// JS to run after loading a snapshot — refreshes runtime-specific data.
const snapshot_init_js =
    \\(function() {
    \\  try {
    \\    var r = JSON.parse(__edgebox_io_sync(JSON.stringify({op:'argv'})));
    \\    if (r.ok) process.argv = r.data;
    \\  } catch(e) {}
    \\  try {
    \\    var r = JSON.parse(__edgebox_io_sync(JSON.stringify({op:'env'})));
    \\    if (r.ok) process.env = r.data;
    \\  } catch(e) {}
    \\})();
;
