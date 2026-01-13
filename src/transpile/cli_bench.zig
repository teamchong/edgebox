//! EdgeBox TypeScript Transpiler Benchmark CLI
//!
//! Reads TypeScript files from a directory and outputs benchmark results as JSON.
//! Usage: cli_bench <directory>

const std = @import("std");
const fast_io = @import("fast_io.zig");
const native_transpiler = @import("native_transpiler.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: cli_bench <directory>\n", .{});
        std.process.exit(1);
    }

    const dir_path = args[1];

    // Collect all .ts files
    var files: std.ArrayListUnmanaged([]const u8) = .empty;
    defer {
        for (files.items) |f| allocator.free(f);
        files.deinit(allocator);
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
            try files.append(allocator, full_path);
        }
    }

    if (files.items.len == 0) {
        std.debug.print("No .ts files found in {s}\n", .{dir_path});
        std.process.exit(1);
    }

    // Count lines
    var total_lines: usize = 0;
    var total_bytes: usize = 0;
    for (files.items) |path| {
        const content = std.fs.cwd().readFileAlloc(allocator, path, 10 * 1024 * 1024) catch continue;
        defer allocator.free(content);
        total_bytes += content.len;
        for (content) |c| {
            if (c == '\n') total_lines += 1;
        }
        total_lines += 1; // Last line
    }

    // Warmup
    var output_buf: [1024 * 1024]u8 = undefined;
    for (files.items[0..@min(10, files.items.len)]) |path| {
        const content = std.fs.cwd().readFileAlloc(allocator, path, 10 * 1024 * 1024) catch continue;
        defer allocator.free(content);
        _ = native_transpiler.streamingTranspile(&output_buf, content);
    }

    // Benchmark - 3 runs, take best
    var best_time: u64 = std.math.maxInt(u64);

    for (0..3) |_| {
        var timer = try std.time.Timer.start();

        for (files.items) |path| {
            const content = std.fs.cwd().readFileAlloc(allocator, path, 10 * 1024 * 1024) catch continue;
            defer allocator.free(content);
            _ = native_transpiler.streamingTranspile(&output_buf, content);
        }

        const elapsed = timer.read();
        if (elapsed < best_time) best_time = elapsed;
    }

    const time_ms = @as(f64, @floatFromInt(best_time)) / 1_000_000.0;
    const lines_per_sec = if (time_ms > 0) @as(f64, @floatFromInt(total_lines)) / (time_ms / 1000.0) else 0;

    // Output JSON to stdout
    const print = std.debug.print;
    print("{s}", .{"{"}); // Open brace
    print("\"tool\":\"edgebox\",", .{});
    print("\"files\":{d},", .{files.items.len});
    print("\"lines\":{d},", .{total_lines});
    print("\"timeMs\":{d:.2},", .{time_ms});
    print("\"linesPerSecond\":{d:.0}", .{lines_per_sec});
    print("{s}\n", .{"}"}); // Close brace
}
