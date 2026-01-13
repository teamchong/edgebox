//! EdgeBox TypeScript Transpiler CLI
//!
//! Usage: edgebox-tsc [options] <files...>
//!   --outdir <dir>   Output directory
//!   --bench          Print timing info
//!
//! Example:
//!   edgebox-tsc --outdir dist src/*.ts

const std = @import("std");
const fast_io = @import("../src/transpile/fast_io.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Parse arguments
    var out_dir: ?[]const u8 = null;
    var bench_mode = false;
    var files = std.ArrayList([]const u8).init(allocator);
    defer files.deinit();

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--outdir") or std.mem.eql(u8, arg, "-o")) {
            i += 1;
            if (i < args.len) out_dir = args[i];
        } else if (std.mem.eql(u8, arg, "--bench") or std.mem.eql(u8, arg, "-b")) {
            bench_mode = true;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printUsage();
            return;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            try files.append(arg);
        }
    }

    if (files.items.len == 0) {
        printUsage();
        std.process.exit(1);
    }

    // Create output directory
    if (out_dir) |dir| {
        std.fs.cwd().makePath(dir) catch {};
    }

    // Transpile files
    var total_lines: usize = 0;
    var total_bytes: usize = 0;
    var success_count: usize = 0;

    var timer = try std.time.Timer.start();

    for (files.items) |file_path| {
        const result = fast_io.transpile(allocator, file_path);

        if (result.success) {
            success_count += 1;
            total_bytes += result.output.len;

            // Count lines
            var lines: usize = 1;
            for (result.output) |c| {
                if (c == '\n') lines += 1;
            }
            total_lines += lines;

            // Write output
            if (out_dir) |dir| {
                const basename = std.fs.path.basename(file_path);
                // Change .ts/.tsx to .js
                const js_name = blk: {
                    if (std.mem.endsWith(u8, basename, ".tsx")) {
                        break :blk try std.fmt.allocPrint(allocator, "{s}.js", .{basename[0 .. basename.len - 4]});
                    } else if (std.mem.endsWith(u8, basename, ".ts")) {
                        break :blk try std.fmt.allocPrint(allocator, "{s}.js", .{basename[0 .. basename.len - 3]});
                    } else {
                        break :blk try allocator.dupe(u8, basename);
                    }
                };
                defer allocator.free(js_name);

                const out_path = try std.fs.path.join(allocator, &.{ dir, js_name });
                defer allocator.free(out_path);

                var file = try std.fs.cwd().createFile(out_path, .{});
                defer file.close();
                try file.writeAll(result.output);
            }

            allocator.free(result.output);
        } else {
            std.debug.print("Error: Failed to transpile {s}\n", .{file_path});
        }
    }

    const elapsed_ns = timer.read();
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;

    if (bench_mode) {
        const lines_per_sec = if (elapsed_ms > 0)
            @as(f64, @floatFromInt(total_lines)) / (elapsed_ms / 1000.0)
        else
            0;

        std.debug.print("\n=== EdgeBox TSC Benchmark ===\n", .{});
        std.debug.print("Files:      {d}\n", .{files.items.len});
        std.debug.print("Success:    {d}\n", .{success_count});
        std.debug.print("Lines:      {d}\n", .{total_lines});
        std.debug.print("Time:       {d:.2}ms\n", .{elapsed_ms});
        std.debug.print("Speed:      {d:.0} lines/sec\n", .{lines_per_sec});
    }
}

fn printUsage() void {
    std.debug.print(
        \\EdgeBox TypeScript Transpiler
        \\
        \\Usage: edgebox-tsc [options] <files...>
        \\
        \\Options:
        \\  --outdir, -o <dir>   Output directory for transpiled files
        \\  --bench, -b          Print timing and performance info
        \\  --help, -h           Show this help
        \\
        \\Examples:
        \\  edgebox-tsc src/app.ts
        \\  edgebox-tsc --outdir dist src/*.ts
        \\  edgebox-tsc --bench --outdir dist src/**/*.ts
        \\
    , .{});
}
