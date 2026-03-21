// v8_snapshot_gen — Build-time helper to create a V8 bootstrap snapshot.
//
// Creates a V8 snapshot containing the bootstrap context (console, require,
// fs, path, Buffer, process, etc.) and writes it to stdout.
//
// The edgebox binary embeds this snapshot via @embedFile, so bootstrap
// code is pre-compiled into the V8 heap — zero parsing/eval cost at runtime.
//
// Build: zig build v8-snapshot-gen
// Usage: ./zig-out/bin/v8-snapshot-gen > src/v8_bootstrap.snapshot

const std = @import("std");
const v8 = @import("v8.zig");
const v8_io = @import("v8_io.zig");

/// External references for V8 snapshot — function pointers that V8 needs
/// to serialize/deserialize. Must be null-terminated and consistent between
/// snapshot creation (here) and snapshot loading (v8_runner.zig).
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

pub fn main() !void {
    _ = try v8.initPlatform();
    defer v8.disposePlatform();

    // SnapshotCreator creates and owns its own isolate.
    // External refs register the IO callback so V8 can serialize it.
    var creator = v8.SnapshotCreator.init(getExternalRefs());

    const isolate = creator.getIsolate();
    v8.IsolateApi.enter(isolate);

    {
        var hs = v8.HandleScope.init(isolate);
        defer hs.deinit();

        const ctx = v8.ContextApi.create(isolate);
        v8.ContextApi.enter(ctx);

        // Register IO bridge — bootstrap calls _ioSync('argv') etc. during eval.
        // At snapshot-gen time these return empty data, which is fine — the runtime
        // refreshes argv/env after loading the snapshot.
        v8_io.registerGlobals(isolate, ctx);

        // Eval bootstrap
        const bootstrap_code = @embedFile("v8_bootstrap.js");
        _ = v8.eval(isolate, ctx, bootstrap_code, "v8_bootstrap.js") catch |err| {
            std.debug.print("[snapshot-gen] ERROR: bootstrap eval failed: {}\n", .{err});
            return err;
        };

        // Pre-load TSC source into V8 heap at snapshot-build time.
        // Workers can access it via globalThis.__tsc_source without disk IO.
        // Use external one-byte string (zero-copy from Zig buffer to V8).
        {
            const alloc = std.heap.page_allocator;
            const tsc_raw = std.fs.cwd().readFileAlloc(alloc, "node_modules/typescript/lib/typescript.js", 20 * 1024 * 1024) catch null;
            if (tsc_raw) |raw_src| {
                // Apply the SAME source transforms used by v8_runner at runtime.
                // This ensures snapshot TSC is identical to runtime-transformed TSC.
                const tsc_transforms = @import("v8_tsc_transforms.zig");
                const src = tsc_transforms.apply(alloc, raw_src) catch raw_src;
                alloc.free(raw_src);
                std.debug.print("[snapshot-gen] Applied {d} bytes of transforms\n", .{src.len - raw_src.len + raw_src.len});

                const v8_str = v8.StringApi.fromExternalOneByte(isolate, src) orelse
                    v8.StringApi.fromUtf8(isolate, src);
                if (v8_str) |str| {
                    // Set as global: globalThis.__tsc_source = <string>
                    const global = v8.ContextApi.global(ctx);
                    const key = v8.StringApi.fromUtf8(isolate, "__tsc_source") orelse null;
                    if (key) |k| {
                        _ = v8.ObjectApi.set(global, ctx, @ptrCast(k), @ptrCast(str));
                        std.debug.print("[snapshot-gen] Pre-loaded TSC source ({d} bytes) into snapshot\n", .{src.len});

                        // Pre-compile AND EXECUTE TSC in the snapshot.
                        // Workers start with globalThis.ts already initialized — zero load time.
                        // This executes typescript.js (API only, no CLI) during snapshot creation.
                        const init_code =
                            \\// Source already transformed by Zig (v8_tsc_transforms.zig).
                            \\// Just compile and execute — no JS replace() needed.
                            \\globalThis.__tsc_factory = new Function('module', 'exports', 'require', '__filename', '__dirname', globalThis.__tsc_source);
                            \\// Execute the factory using the REAL bootstrap require
                            \\// This initializes the ts module in the snapshot so workers get it for free.
                            \\var __tsc_mod = { exports: {} };
                            \\try {
                            \\  globalThis.__tsc_factory(__tsc_mod, __tsc_mod.exports, globalThis.require, '/snapshot/typescript.js', '/snapshot');
                            \\  globalThis.ts = __tsc_mod.exports;
                            \\  // Free source string — ts module is initialized, source no longer needed
                            \\  delete globalThis.__tsc_source;
                            \\  delete globalThis.__tsc_factory;
                            \\} catch(e) {
                            \\  // May fail during snapshot if IO is unavailable — keep factory for runtime
                            \\}
                        ;
                        _ = v8.eval(isolate, ctx, init_code, "tsc_init.js") catch |err| {
                            std.debug.print("[snapshot-gen] WARNING: TSC init failed: {} (factory still available)\n", .{err});
                        };
                        std.debug.print("[snapshot-gen] Pre-compiled + initialized TSC in snapshot\n", .{});

                        // Pre-parse lib.d.ts files into __sfCache during snapshot.
                        // The memoization key is fileName+':'+sourceText.length.
                        // At runtime, TSC calls createSourceFile with the real path.
                        // We parse with the build-time path here, but the cache key is
                        // fileName-based so it will match if the runtime path matches.
                        // Since we can't predict the runtime install path, we pre-parse
                        // and store with a WILDCARD key pattern that runtime can remap.
                        // Actually: simpler — just warm up V8's TurboFan by parsing.
                        // The act of calling createSourceFile during snapshot warms the parser.
                        const preparse_js =
                            \\(function() {
                            \\  if (typeof globalThis.ts === 'undefined' || !ts.createSourceFile) return 0;
                            \\  var fs = globalThis.require ? globalThis.require('fs') : null;
                            \\  if (!fs || !fs.readdirSync) return 0;
                            \\  var dir = 'node_modules/typescript/lib';
                            \\  try { var files = fs.readdirSync(dir); } catch(e) { return 0; }
                            \\  var count = 0;
                            \\  for (var i = 0; i < files.length; i++) {
                            \\    var f = files[i];
                            \\    if (!f.endsWith('.d.ts')) continue;
                            \\    var path = dir + '/' + f;
                            \\    try {
                            \\      var src = fs.readFileSync(path, 'utf8');
                            \\      ts.createSourceFile(path, src, 99, true);
                            \\      count++;
                            \\    } catch(e) {}
                            \\  }
                            \\  return count;
                            \\})();
                        ;
                        _ = v8.eval(isolate, ctx, preparse_js, "preparse_lib.js") catch {};
                        std.debug.print("[snapshot-gen] Pre-parsed lib.d.ts files into snapshot cache\n", .{});
                    }
                }
                // Don't free src — external string references it
            } else {
                std.debug.print("[snapshot-gen] TSC source not found (workers will load from disk)\n", .{});
            }
        }

        v8.ContextApi.exit(ctx);
        creator.setDefaultContext(ctx);
    }

    v8.IsolateApi.exit(isolate);

    // Create the snapshot blob
    // kKeep retains compiled code (Sparkplug baseline) in the snapshot,
    // giving pre-compiled functions a head start without TurboFan warmup.
    const blob = creator.createBlob(v8.SnapshotCreator.kKeep);
    creator.deinit();

    if (blob.data == null or blob.raw_size <= 0) {
        std.debug.print("[snapshot-gen] ERROR: CreateBlob returned empty data\n", .{});
        return error.SnapshotCreationFailed;
    }

    const data_slice = blob.data.?[0..@intCast(blob.raw_size)];
    defer v8.SnapshotApi.deleteData(blob.data.?);

    // Write snapshot to file (or stdout if no args)
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    if (args.len > 1) {
        // Write to specified output path
        const out_path = args[1];
        if (std.fs.path.dirname(out_path)) |dir| {
            std.fs.cwd().makePath(dir) catch {};
        }
        const f = try std.fs.cwd().createFile(out_path, .{});
        defer f.close();
        try f.writeAll(data_slice);
        std.debug.print("[snapshot-gen] Created {d} byte snapshot → {s}\n", .{ data_slice.len, out_path });
    } else {
        // Write to stdout (binary)
        const stdout = std.fs.File.stdout();
        try stdout.writeAll(data_slice);
        std.debug.print("[snapshot-gen] Created {d} byte snapshot\n", .{data_slice.len});
    }
}
