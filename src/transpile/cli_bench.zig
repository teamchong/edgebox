//! EdgeBox TypeScript Transpiler Benchmark CLI
//!
//! Reads TypeScript files from a directory and outputs benchmark results as JSON.
//! Usage: cli_bench <directory>
//!
//! Optimized for accurate benchmarking:
//! - Uses page_allocator (no debug overhead)
//! - Pre-loads all files before timing
//! - Stack-allocated output buffer

const std = @import("std");
const native_transpiler = @import("native_transpiler.zig");

pub fn main() !void {
    // Use page_allocator for benchmarks (no debug overhead)
    const allocator = std.heap.page_allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: cli_bench <directory>\n", .{});
        std.process.exit(1);
    }

    const dir_path = args[1];

    // Collect all .ts files
    var file_paths: std.ArrayListUnmanaged([]const u8) = .empty;
    defer {
        for (file_paths.items) |f| allocator.free(f);
        file_paths.deinit(allocator);
    }

    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
        std.debug.print("Error opening directory: {}\n", .{err});
        std.process.exit(1);
    };
    defer dir.close();

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".ts")) {
            const full_path = try std.fs.path.join(allocator, &.{ dir_path, entry.name });
            try file_paths.append(allocator, full_path);
        }
    }

    if (file_paths.items.len == 0) {
        std.debug.print("No .ts files found in {s}\n", .{dir_path});
        std.process.exit(1);
    }

    // Pre-load ALL files into memory before benchmarking
    var file_contents: std.ArrayListUnmanaged([]const u8) = .empty;
    defer {
        for (file_contents.items) |c| allocator.free(c);
        file_contents.deinit(allocator);
    }

    var total_lines: usize = 0;
    var total_bytes: usize = 0;

    for (file_paths.items) |path| {
        const content = std.fs.cwd().readFileAlloc(allocator, path, 10 * 1024 * 1024) catch continue;
        total_bytes += content.len;
        for (content) |c| {
            if (c == '\n') total_lines += 1;
        }
        total_lines += 1;
        try file_contents.append(allocator, content);
    }

    // Stack-allocated output buffer (no allocation during benchmark)
    var output_buf: [1024 * 1024]u8 = undefined;

    // Warmup - run transpiler to warm CPU caches
    for (file_contents.items[0..@min(10, file_contents.items.len)]) |content| {
        _ = native_transpiler.streamingTranspile(&output_buf, content);
    }

    // Benchmark - 5 runs, take best (pure transpilation, no I/O)
    var best_time: u64 = std.math.maxInt(u64);

    for (0..5) |_| {
        var timer = try std.time.Timer.start();

        for (file_contents.items) |content| {
            _ = native_transpiler.streamingTranspile(&output_buf, content);
        }

        const elapsed = timer.read();
        if (elapsed < best_time) best_time = elapsed;
    }

    const time_ms = @as(f64, @floatFromInt(best_time)) / 1_000_000.0;
    const lines_per_sec = if (time_ms > 0) @as(f64, @floatFromInt(total_lines)) / (time_ms / 1000.0) else 0;

    // Output JSON to stderr (debug.print goes to stderr)
    const print = std.debug.print;
    print("{{\"tool\":\"edgebox\",\"files\":{d},\"lines\":{d},\"timeMs\":{d:.2},\"linesPerSecond\":{d:.0}}}\n", .{
        file_contents.items.len,
        total_lines,
        time_ms,
        lines_per_sec,
    });
}
