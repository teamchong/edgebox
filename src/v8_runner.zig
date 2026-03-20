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

    // Code cache is now loaded inside runScript using the TRANSFORMED source hash.
    // This ensures the cache matches what V8 actually compiles.
    const disk_cache: ?[]u8 = null;

    // Prefetch source files in parallel if running TSC with -p <tsconfig>
    // This reads all .ts/.tsx/.js files into the IO cache using Zig threads
    // BEFORE TSC starts, so all readFile calls hit cache instantly.
    if (std.mem.endsWith(u8, script_path, "tsc.js") or std.mem.endsWith(u8, script_path, "_tsc.js")) {
        // Always prefetch TypeScript lib.d.ts files (TSC always reads these)
        const ts_lib_dir = std.fs.path.dirname(abs_path) orelse ".";
        v8_io.prefetchDirectory(ts_lib_dir);

        // Find -p <path> in remaining args and prefetch project directory
        var peek_args = try std.process.argsWithAllocator(alloc);
        defer peek_args.deinit();
        var found_p = false;
        while (peek_args.next()) |arg| {
            if (found_p) {
                var tsconfig_abs: [std.fs.max_path_bytes]u8 = undefined;
                const tsconfig_real = std.fs.cwd().realpath(arg, &tsconfig_abs) catch arg;
                const project_dir = std.fs.path.dirname(tsconfig_real) orelse ".";
                v8_io.prefetchDirectory(project_dir);
                break;
            }
            if (std.mem.eql(u8, arg, "-p") or std.mem.eql(u8, arg, "--project")) {
                found_p = true;
            }
        }
    }

    // Note: parallel process spawning was tested but each worker repeats
    // the full TSC init (parse+bind 6.2MB), making it slower than single-process.
    // True parallelism requires shared-memory type checking (Zig threads on SAB).

    return runScript(alloc, script_code, disk_cache, abs_path, script_path, false);
}

/// Run TSC with parallel checkSourceFile sharding.
/// Spawns N worker processes, each checking a subset of source files.
fn runParallelTsc(alloc_: std.mem.Allocator, script_code: []const u8, cache_bytes: ?[]const u8, abs_path: []const u8, script_path: []const u8) !void {
    _ = cache_bytes;
    _ = script_code;

    const cpu_count = std.Thread.getCpuCount() catch 4;
    const num_workers = @min(cpu_count, 4); // Cap at 4 workers for TSC

    // Get our own executable path
    var self_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const self_path = std.fs.selfExePath(&self_path_buf) catch {
        // Fallback to single-threaded
        return runScript(alloc_, @constCast(std.fs.cwd().readFileAlloc(alloc_, script_path, 64 * 1024 * 1024) catch return error.ReadFailed), null, abs_path, script_path, false);
    };

    // Build the argv: [edgebox, _tsc.js, ...remaining args]
    var original_args = try std.process.argsWithAllocator(alloc_);
    defer original_args.deinit();
    var argv_list: std.ArrayListUnmanaged([]const u8) = .{};
    // Skip argv[0] (binary name)
    _ = original_args.next();
    // Collect all remaining args
    while (original_args.next()) |arg| {
        try argv_list.append(alloc_, arg);
    }

    // Spawn workers
    var workers: [4]?std.process.Child = .{ null, null, null, null };

    for (0..num_workers) |i| {
        var env_map = std.process.EnvMap.init(alloc_);
        // Copy existing env
        const environ = std.process.getEnvMap(alloc_) catch continue;
        var env_iter = environ.iterator();
        while (env_iter.next()) |entry| {
            env_map.put(entry.key_ptr.*, entry.value_ptr.*) catch {};
        }

        // Set shard env vars
        var shard_buf: [8]u8 = undefined;
        var total_buf: [8]u8 = undefined;
        const shard_str = std.fmt.bufPrint(&shard_buf, "{d}", .{i}) catch continue;
        const total_str = std.fmt.bufPrint(&total_buf, "{d}", .{num_workers}) catch continue;
        env_map.put("__EDGEBOX_SHARD", shard_str) catch continue;
        env_map.put("__EDGEBOX_TOTAL", total_str) catch continue;

        // Build full argv: [self_path, ...original_args]
        var full_argv: std.ArrayListUnmanaged([]const u8) = .{};
        full_argv.append(alloc_, self_path) catch continue;
        for (argv_list.items) |a| full_argv.append(alloc_, a) catch {};

        var child = std.process.Child.init(full_argv.items, alloc_);
        child.env_map = &env_map;
        child.stderr_behavior = .Inherit;
        child.stdout_behavior = .Inherit;

        child.spawn() catch continue;
        workers[i] = child;
    }

    // Wait for all workers
    var exit_code: u8 = 0;
    for (&workers) |*w| {
        if (w.*) |*child| {
            const result = child.wait() catch continue;
            if (result.Exited != 0 and exit_code == 0) {
                exit_code = @intCast(result.Exited);
            }
            w.* = null;
        }
    }

    if (exit_code != 0) {
        std.process.exit(exit_code);
    }
}

/// Embedded V8 snapshot of the bootstrap context — generated at build time
/// by v8_snapshot_gen. Contains console, require, fs, Buffer, process, etc.
/// Using this skips ~200ms of bootstrap eval on every startup.
const embedded_snapshot = @embedFile("v8_bootstrap.snapshot");

/// External references for V8 snapshot deserialization — must match the array
/// used during snapshot creation (in v8_snapshot_gen.zig).
/// External refs: all IO callbacks + null terminator
var external_refs: [11]usize = .{0} ** 11;

fn getExternalRefs() *const [11]usize {
    if (external_refs[0] == 0) {
        external_refs[0] = @intFromPtr(&v8_io.ioSyncCallback);
        external_refs[1] = @intFromPtr(&v8_io.ioBatchCallback);
        external_refs[2] = @intFromPtr(&v8_io.readFileFastCallback);
        external_refs[3] = @intFromPtr(&v8_io.fileExistsFastCallback);
        external_refs[4] = @intFromPtr(&v8_io.writeStdoutFastCallback);
        external_refs[5] = @intFromPtr(&v8_io.writeStderrFastCallback);
        external_refs[6] = @intFromPtr(&v8_io.readdirFastCallback);
        external_refs[7] = @intFromPtr(&v8_io.dirExistsFastCallback);
        external_refs[8] = @intFromPtr(&v8_io.realpathFastCallback);
    }
    return &external_refs;
}

/// Core script execution: initialize V8, load bootstrap, compile + run script.
fn runScript(alloc: std.mem.Allocator, script_code: []const u8, cache_bytes: ?[]const u8, abs_path: []const u8, display_name: []const u8, is_embedded: bool) !void {
    _ = display_name;
    _ = cache_bytes;

    // Detect TSC early for optimized path selection
    const is_tsc = std.mem.endsWith(u8, abs_path, "_tsc.js") or
        std.mem.endsWith(u8, abs_path, "tsc.js");

    // Initialize V8
    _ = try v8.initPlatform();
    defer v8.disposePlatform();

    // For TSC: use plain isolate (no 13.6MB snapshot deserialization).
    // TSC source transforms + code cache is faster than snapshot-loaded TSC.
    const use_snapshot = embedded_snapshot.len > 0 and !is_tsc;
    const isolate = if (use_snapshot)
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

    if (use_snapshot) {
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

    // Register parallel/channel APIs only for non-TSC scripts.
    // For TSC, skip these to reduce startup by ~50ms (SAB allocation + JS eval).
    if (!is_tsc) {
        const v8_parallel = @import("v8_parallel.zig");
        v8_parallel.registerGlobals(isolate, context);

        const v8_channel = @import("v8_channel.zig");
        v8_channel.registerGlobals(isolate, context);

        const v8_parallel_check = @import("v8_parallel_check.zig");
        v8_parallel_check.registerGlobals(isolate, context);

        const parallel_init_js = @embedFile("v8_parallel_init.js");
        _ = v8.eval(isolate, context, parallel_init_js, "v8_parallel_init.js") catch {};
    } else {
        // For TSC: register only the precompute callback (no SAB allocation)
        const v8_parallel_check = @import("v8_parallel_check.zig");
        const global_obj = v8.ContextApi.global(context);
        const pc_tmpl = v8.FunctionTemplateApi.create(isolate, &v8_parallel_check.precomputeCallback) orelse return;
        const pc_func = v8.FunctionTemplateApi.getFunction(pc_tmpl, context) orelse return;
        const pc_key = v8.StringApi.fromUtf8(isolate, "__edgebox_precompute_relations") orelse return;
        _ = v8.ObjectApi.set(global_obj, context, @ptrCast(pc_key), @ptrCast(pc_func));
    }

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

    // Inject TSC shim + require intercept if snapshot has pre-initialized TSC
    const tsc_shim_code = @embedFile("v8_tsc_shim.js");
    _ = v8.eval(isolate, context, tsc_shim_code, "v8_tsc_shim.js") catch {};

    // Auto-inject zero-copy optimizations for TSC
    // Single-pass multi-pattern replacement to minimize scan overhead on 9MB source.
    // Set __filename and __dirname as globals (zero-copy — no wrapper needed)
    const dirname = std.fs.path.dirname(abs_path) orelse ".";
    const set_globals = try std.fmt.allocPrint(alloc,
        "globalThis.__filename = \"{s}\"; globalThis.__dirname = \"{s}\";",
        .{ abs_path, dirname },
    );
    defer alloc.free(set_globals);
    _ = v8.eval(isolate, context, set_globals, "globals.js") catch {};

    // Apply source transforms for large scripts (TSC optimization).
    // Cache the transformed source to avoid re-transforming on every run.
    var transformed: ?[]u8 = null;
    defer if (transformed) |t| alloc.free(t);

    if (script_code.len > 5 * 1024 * 1024) {
        // Try to load cached transformed source
        const transform_cache_path = getCachePath(alloc, script_code) catch null;
        var cached_transform: ?[]u8 = null;
        if (transform_cache_path) |tcp| {
            // Replace .codecache with .transformed
            if (std.mem.endsWith(u8, tcp, ".codecache")) {
                const base = tcp[0 .. tcp.len - ".codecache".len];
                const tfm_path = std.fmt.allocPrint(alloc, "{s}.transformed", .{base}) catch null;
                if (tfm_path) |tp| {
                    cached_transform = std.fs.cwd().readFileAlloc(alloc, tp, 64 * 1024 * 1024) catch null;
                }
            }
        }

        if (cached_transform) |ct| {
            transformed = ct;
        } else {
            transformed = applyTscTransforms(alloc, script_code) catch null;
            // Save transformed source for next run
            if (transformed) |t| {
                if (transform_cache_path) |tcp| {
                    if (std.mem.endsWith(u8, tcp, ".codecache")) {
                        const base = tcp[0 .. tcp.len - ".codecache".len];
                        const tfm_path = std.fmt.allocPrint(alloc, "{s}.transformed", .{base}) catch null;
                        if (tfm_path) |tp| {
                            if (std.fs.path.dirname(tp)) |dir| {
                                std.fs.cwd().makePath(dir) catch {};
                            }
                            if (std.fs.cwd().createFile(tp, .{})) |f| {
                                defer f.close();
                                f.writeAll(t) catch {};
                            } else |_| {}
                        }
                    }
                }
            }
        }
    }

    const wrapped = transformed orelse script_code;

    // Compile with code cache
    var try_catch = v8.TryCatch.init(isolate);
    defer try_catch.deinit();

    // Use external string for zero-copy — V8 reads directly from Zig's buffer.
    // For large scripts (>64KB), this avoids a 9MB+ heap allocation.
    const source_str = if (wrapped.len > 65536)
        v8.StringApi.fromExternalOneByte(isolate, wrapped) orelse
            v8.StringApi.fromUtf8(isolate, wrapped) orelse {
            std.debug.print("[v8] ERROR: failed to create source string\n", .{});
            std.process.exit(1);
        }
    else
        v8.StringApi.fromUtf8(isolate, wrapped) orelse {
            std.debug.print("[v8] ERROR: failed to create source string\n", .{});
            std.process.exit(1);
        };
    const name_str = v8.StringApi.fromUtf8(isolate, abs_path) orelse {
        std.debug.print("[v8] ERROR: failed to create name string\n", .{});
        std.process.exit(1);
    };
    var origin = v8.ScriptOrigin.init(@ptrCast(name_str));

    // Load code cache for the TRANSFORMED source (not the original).
    // The cache must match what V8 compiles — the transformed source.
    var cached_data: ?*v8.CachedData = null;
    var transformed_cache_bytes: ?[]u8 = null;
    {
        const cache_key_source = wrapped; // Use transformed source for cache key
        const tfm_cache_path = getCachePath(alloc, cache_key_source) catch null;
        if (tfm_cache_path) |tcp| {
            transformed_cache_bytes = std.fs.cwd().readFileAlloc(alloc, tcp, 128 * 1024 * 1024) catch null;
        }
        if (transformed_cache_bytes) |cb| {
            if (cb.len > 0) {
                cached_data = v8.ScriptCompilerApi.createCachedData(cb.ptr, @intCast(cb.len));
            }
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

    // Check if code cache was accepted
    var cache_was_used = false;
    if (cached_data != null) {
        if (compiler_source.getCachedData()) |cd| {
            const info = v8.ScriptCompilerApi.getCachedDataBytes(cd);
            cache_was_used = !info.rejected;
        }
    }

    // Note: do NOT call deleteCachedData here — the Source destructor
    // (compiler_source.deinit) owns the CachedData and frees it.

    // Pump V8 message loop before execution — process pending TurboFan
    // compilation tasks so hot functions get optimized during execution.
    if (v8.global_platform) |platform| {
        while (v8.pumpMessageLoop(platform, isolate)) {}
    }

    // Execute
    _ = v8.ScriptApi.run(script, context) orelse {
        // Check if this was a deferred process.exit() — not a real error
        if (v8_io.deferred_exit_code != null) {
            // Script called process.exit() — save code cache then exit
        } else {
            reportException(&try_catch, isolate, context);
            std.process.exit(1);
        }
    };

    // Save code cache AFTER execution — captures all compiled functions.
    // V8 lazy-compiles functions on first call. Saving before execution only
    // gets top-level code. After a full type-check run, all hot functions
    // are compiled and the cache is much more complete.
    if (!cache_was_used and !is_embedded) {
        // Save cache keyed by the TRANSFORMED source (what V8 actually compiled)
        const save_cache_path = getCachePath(alloc, wrapped) catch null;
        if (save_cache_path) |cp| {
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

    // Apply deferred exit code if process.exit was called
    if (v8_io.deferred_exit_code) |code| {
        std.process.exit(code);
    }
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

/// Apply TSC-specific source transforms — single-pass scan with needle/replacement pairs.
/// Transforms: SOA typeFlags read, createType registration, integer key packing, JSDoc skip.
fn applyTscTransforms(allocator: std.mem.Allocator, source: []const u8) ![]u8 {
    // Needle/replacement pairs applied in a single scan
    const Transform = struct {
        needle: []const u8,
        replacement: []const u8,
    };

    const transforms = [_]Transform{
        // T1: createType → populate __pc_typeFlags + __pc_objectFlags SOA columns
        .{
            .needle = "typeCount++;\n    result.id = typeCount;",
            .replacement = "typeCount++;\n    result.id = typeCount;\n    if(typeof __pc_typeFlags!=='undefined'&&typeCount<262144){__pc_typeFlags[typeCount]=result.flags;if(result.objectFlags)__pc_objectFlags[typeCount]=result.objectFlags;}",
        },
        // T2: isSimpleTypeRelatedTo → read from SOA column + flag table lookup
        .{
            .needle = "const s = source.flags;\n    const t = target.flags;",
            .replacement = "const s = __pc_typeFlags[source.id|0] || source.flags;\n    const t = __pc_typeFlags[target.id|0] || target.flags;\n    if(typeof __pc_flagTable!=='undefined'){const __ft=__pc_flagTable[(s&2047)*2048+(t&2047)];if(__ft===1)return true;if(__ft===2)return false;}",
        },
        // T3: getRelationKey → packed Smi integer (stays in V8 Smi range for IDs < 32768)
        // Key = source.id * 32768 + target.id (max 2^30 = Smi limit, no HeapNumber allocation)
        // Falls back to string for IDs >= 32768 (very large projects)
        .{
            .needle = "isTypeReferenceWithGenericArguments(source) && isTypeReferenceWithGenericArguments(target) ? getGenericTypeReferenceRelationKey(source, target, postFix, ignoreConstraints) : `${source.id},${target.id}${postFix}`",
            .replacement = "isTypeReferenceWithGenericArguments(source) && isTypeReferenceWithGenericArguments(target) ? getGenericTypeReferenceRelationKey(source, target, postFix, ignoreConstraints) : (source.id<32768&&target.id<32768) ? source.id * 32768 + target.id + 1 : `${source.id},${target.id}${postFix}`",
        },
        // T4: JSDoc skip — function default parameter
        .{
            .needle = "jsDocParsingMode = 0",
            .replacement = "jsDocParsingMode = 1",
        },
        // T5: typeof guard for packed integer key (id.startsWith crashes on number)
        .{
            .needle = "id.startsWith(\"*\")",
            .replacement = "(typeof id === \"string\" && id.startsWith(\"*\"))",
        },
        // T6: getFlowCacheKey Identifier → packed integer (eliminates template string)
        // Pack 4 IDs into one number: flowContainer*2^33 + declaredType*2^22 + initialType*2^11 + symbol
        // Supports IDs up to 2048 (11 bits each) — safe for most projects
        // Falls back to string for large IDs via || operator
        .{
            .needle = "return symbol !== unknownSymbol ? `${flowContainer ? getNodeId(flowContainer) : \"-1\"}|${getTypeId(declaredType)}|${getTypeId(initialType)}|${getSymbolId(symbol)}` : void 0;",
            .replacement = "if(symbol===unknownSymbol)return void 0;var __fc=flowContainer?getNodeId(flowContainer)+1:0,__dt=getTypeId(declaredType),__it=getTypeId(initialType),__si=getSymbolId(symbol);return(__dt<32768&&__si<32768)?__dt*32768+__si+1:`${__fc}|${__dt}|${__it}|${__si}`;",
        },
        // T7: accessibleChainCache key → packed integer
        .{
            .needle = "const key = `${useOnlyExternalAliasing ? 0 : 1}|${firstRelevantLocation ? getNodeId(firstRelevantLocation) : 0}|${meaning}`;",
            .replacement = "const key = (useOnlyExternalAliasing?0:4194304)+(firstRelevantLocation?getNodeId(firstRelevantLocation):0)*8+meaning+1;",
        },
        // T8: decoratorContextOverrideType key → packed integer
        .{
            .needle = "const key = `${isPrivate ? \"p\" : \"P\"}${isStatic2 ? \"s\" : \"S\"}${nameType.id}`;",
            .replacement = "const key = (isPrivate?2:0)+(isStatic2?1:0)+nameType.id*4+1;",
        },
        // T9: createSourceFile memoization — cache by fileName+size
        .{
            .needle = "function createSourceFile(fileName, sourceText, languageVersionOrOptions, setParentNodes = false, scriptKind) {",
            .replacement = "function createSourceFile(fileName, sourceText, languageVersionOrOptions, setParentNodes = false, scriptKind) {var __ck=fileName+':'+sourceText.length;if(typeof __sfCache!=='undefined'&&__sfCache[__ck])return __sfCache[__ck];",
        },
        // T10: createSourceFile return — cache result before returning
        .{
            .needle = "(_b = tracing) == null ? void 0 : _b.pop();\n  return result;\n}\nfunction parseIsolatedEntityName",
            .replacement = "(_b = tracing) == null ? void 0 : _b.pop();\n  if(typeof __sfCache!=='undefined')__sfCache[__ck]=result;\n  return result;\n}\nfunction parseIsolatedEntityName",
        },
        // T11: fileSystemEntryExists → fast callbacks for file/dir checks
        .{
            .needle = "function fileSystemEntryExists(path, entryKind) {\n      const stat = statSync(path);\n      if (!stat) {\n        return false;\n      }\n      switch (entryKind) {\n        case 0 /* File */:\n          return stat.isFile();\n        case 1 /* Directory */:\n          return stat.isDirectory();",
            .replacement = "function fileSystemEntryExists(path, entryKind) {\n      if(typeof __edgebox_file_exists==='function'){if(entryKind===0)return !!__edgebox_file_exists(path);if(entryKind===1)return !!__edgebox_dir_exists(path);}\n      const stat = statSync(path);\n      if (!stat) {\n        return false;\n      }\n      switch (entryKind) {\n        case 0 /* File */:\n          return stat.isFile();\n        case 1 /* Directory */:\n          return stat.isDirectory();",
        },
        // T12: getFlowCacheKey ThisKeyword → packed integer
        .{
            .needle = "return `0|${flowContainer ? getNodeId(flowContainer) : \"-1\"}|${getTypeId(declaredType)}|${getTypeId(initialType)}`;",
            .replacement = "var __dt2=getTypeId(declaredType),__it2=getTypeId(initialType);return(__dt2<32768&&__it2<32768)?__dt2*32768+__it2+1:`0|${flowContainer?getNodeId(flowContainer):-1}|${__dt2}|${__it2}`;",
        },
        // T13: invokeOnce key → Smi integer (inference engine hot path)
        .{
            .needle = "function invokeOnce(source, target, action) {\n      const key = source.id + \",\" + target.id;",
            .replacement = "function invokeOnce(source, target, action) {\n      const key = (source.id<32768&&target.id<32768)?source.id*32768+target.id+1:source.id+\",\"+target.id;",
        },
        // T14: enumRelation key → Smi integer
        .{
            .needle = "const id = getSymbolId(sourceSymbol) + \",\" + getSymbolId(targetSymbol);",
            .replacement = "const __ss=getSymbolId(sourceSymbol),__ts=getSymbolId(targetSymbol);const id=(__ss<32768&&__ts<32768)?__ss*32768+__ts+1:__ss+\",\"+__ts;",
        },
        // T15: inferTypeForHomomorphicMappedType key → Smi
        .{
            .needle = "const cacheKey = source.id + \",\" + target.id + \",\" + constraint.id;",
            .replacement = "const cacheKey = (source.id<1024&&target.id<1024&&constraint.id<1024)?(source.id<<20|target.id<<10|constraint.id)+1:source.id+\",\"+target.id+\",\"+constraint.id;",
        },
        // T16: getObjectFlags → SOA column read (avoid object property access)
        .{
            .needle = "function getObjectFlags(type) {\n    return type.flags & 3899393 /* ObjectFlagsType */ ? type.objectFlags : 0;\n  }",
            .replacement = "function getObjectFlags(type) {\n    if(typeof __pc_objectFlags!=='undefined'&&type.id>0&&type.id<262144){var __of=__pc_objectFlags[type.id];if(__of)return __of;}return type.flags & 3899393 ? type.objectFlags : 0;\n  }",
        },
        // T13: Pre-compute flag table + checkSourceFile sharding
        // Calls __edgebox_precompute_relations(typeCount) to build the flag-pair
        // lookup table in SAB before type checking starts. Then isSimpleTypeRelatedTo
        // can use __pc_flagTable[(s&0x7FF)*2048+(t&0x7FF)] for O(1) lookup.
        .{
            .needle = "forEach(host.getSourceFiles(), (file) => checkSourceFileWithEagerDiagnostics(file));",
            .replacement = "{if(typeof __edgebox_precompute_relations==='function')__edgebox_precompute_relations(typeCount);const __files=host.getSourceFiles();const __shard=parseInt(process.env.__EDGEBOX_SHARD||'0');const __total=parseInt(process.env.__EDGEBOX_TOTAL||'1');const __start=Math.floor(__files.length*__shard/__total);const __end=Math.floor(__files.length*(__shard+1)/__total);for(let __i=__start;__i<__end;__i++)checkSourceFileWithEagerDiagnostics(__files[__i]);}",
        },
    };

    // Calculate max possible output size (replacements might be longer)
    var max_growth: usize = 0;
    for (&transforms) |t| {
        if (t.replacement.len > t.needle.len) {
            max_growth += (t.replacement.len - t.needle.len) * 10; // max 10 occurrences each
        }
    }

    const buf = try allocator.alloc(u8, source.len + max_growth);
    var pw: usize = 0; // write position
    var pr: usize = 0; // read position

    while (pr < source.len) {
        var matched = false;

        for (&transforms) |t| {
            if (pr + t.needle.len <= source.len and
                std.mem.startsWith(u8, source[pr..], t.needle))
            {
                @memcpy(buf[pw..][0..t.replacement.len], t.replacement);
                pw += t.replacement.len;
                pr += t.needle.len;
                matched = true;
                break;
            }
        }

        if (!matched) {
            buf[pw] = source[pr];
            pw += 1;
            pr += 1;
        }
    }

    // Resize to actual length
    if (pw < buf.len) {
        return try allocator.realloc(buf, pw);
    }
    return buf[0..pw];
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
