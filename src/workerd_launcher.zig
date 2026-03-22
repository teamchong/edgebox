//! workerd Parallel TSC Launcher
//!
//! Zig binary that:
//! 1. Reads CPU count → calculates worker count (formula: cpu_count / 2)
//! 2. Generates workerd config with N checker sockets
//! 3. Writes project config for workers to read at module init
//! 4. Starts workerd process
//! 5. Workers pre-check at module init, submit results via Zig shared memory
//! 6. Launcher collects results, deduplicates, outputs diagnostics
//!
//! Zero HTTP in the hot path. Workers use __edgebox_io_sync for coordination.

const std = @import("std");

const alloc = std.heap.page_allocator;

pub fn main() !void {
    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();
    _ = args.next(); // skip program name

    const project_dir = args.next() orelse {
        std.debug.print("Usage: edgebox-check <project-dir>\n", .{});
        std.process.exit(1);
    };

    // Formula: workers = cpu_count / 2 (reserve half for V8 platform)
    const cpu_count = std.Thread.getCpuCount() catch 4;
    const worker_count = @max(cpu_count / 2, 2);
    const base_port: u16 = 9900;

    std.debug.print("[edgebox] {d} CPUs, {d} workers (formula: cpu/2)\n", .{ cpu_count, worker_count });

    // Write project config for workers
    const config_json = try std.fmt.allocPrint(alloc, "{{\"cwd\":\"{s}\"}}", .{project_dir});
    defer alloc.free(config_json);
    {
        const f = try std.fs.cwd().createFile("/tmp/edgebox-project-config.json", .{});
        defer f.close();
        try f.writeAll(config_json);
    }

    // Generate workerd config
    const config_path = "/tmp/edgebox-workerd.capnp";
    {
        const f = try std.fs.cwd().createFile(config_path, .{});
        defer f.close();
        var w = f.writer();

        try w.writeAll("using Workerd = import \"/workerd/workerd.capnp\";\nconst config :Workerd.Config = (\n  services = [\n");
        for (0..worker_count) |i| {
            try w.print("    (name = \"checker-{d}\", worker = .checkerWorker),\n", .{i});
        }
        try w.writeAll("  ],\n  sockets = [\n");
        for (0..worker_count) |i| {
            try w.print("    (name = \"s{d}\", address = \"127.0.0.1:{d}\", http = (), service = \"checker-{d}\"),\n", .{ i, base_port + @as(u16, @intCast(i)), i });
        }
        try w.writeAll("  ],\n);\nconst checkerWorker :Workerd.Worker = (\n  modules = [\n");
        try w.writeAll("    (name = \"entrypoint\", esModule = embed \"checker-parallel.js\"),\n");
        try w.writeAll("    (name = \"./bootstrap.js\", commonJsModule = embed \"bootstrap.js\"),\n");
        try w.writeAll("    (name = \"fs\", commonJsModule = embed \"fs.js\"),\n");
        try w.writeAll("    (name = \"path\", commonJsModule = embed \"path.js\"),\n");
        try w.writeAll("    (name = \"os\", commonJsModule = embed \"os.js\"),\n");
        try w.writeAll("    (name = \"./typescript.js\", commonJsModule = embed \"typescript.js\"),\n");
        try w.writeAll("  ],\n  compatibilityDate = \"2024-09-23\",\n);\n");
    }

    std.debug.print("[edgebox] Config: {s}\n", .{config_path});
    std.debug.print("[edgebox] Project: {s}\n", .{project_dir});
    std.debug.print("[edgebox] Workers pre-checking at startup...\n", .{});

    // TODO: Start workerd, wait for workers to finish pre-check,
    // collect results via Zig shared memory (submitResult op),
    // deduplicate, output diagnostics, exit.
    //
    // For now, print the generated config path for manual testing.
    std.debug.print("[edgebox] Run: cd src/workerd-tsc && workerd serve {s}\n", .{config_path});
}
