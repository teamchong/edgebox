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
var external_refs: [2]usize = .{ 0, 0 };

fn getExternalRefs() *const [2]usize {
    if (external_refs[0] == 0) {
        external_refs[0] = @intFromPtr(&v8_io.ioSyncCallback);
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

        v8.ContextApi.exit(ctx);
        creator.setDefaultContext(ctx);
    }

    v8.IsolateApi.exit(isolate);

    // Create the snapshot blob
    const blob = creator.createBlob(v8.SnapshotCreator.kClear);
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
