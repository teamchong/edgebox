const std = @import("std");
const config = @import("config.zig");
const test_parser = @import("test_parser.zig");
const harness = @import("harness.zig");
const runner = @import("runner.zig");
const results = @import("results.zig");

pub const Engine = enum {
    edgebox,
    qjs,
    node,
    bun,

    pub fn getCommand(self: Engine) []const u8 {
        return switch (self) {
            .edgebox => "edgebox",
            .qjs => "qjs",
            .node => "node",
            .bun => "bun",
        };
    }

    pub fn fromString(s: []const u8) ?Engine {
        if (std.mem.eql(u8, s, "edgebox")) return .edgebox;
        if (std.mem.eql(u8, s, "qjs")) return .qjs;
        if (std.mem.eql(u8, s, "node")) return .node;
        if (std.mem.eql(u8, s, "bun")) return .bun;
        return null;
    }
};

const Options = struct {
    engine: Engine = .qjs,
    config_file: ?[]const u8 = null,
    test_dir: ?[]const u8 = null,
    harness_dir: []const u8 = "test262/harness",
    threads: u32 = 1,
    verbose: bool = false,
    json_output: bool = false,
    timeout_ms: u32 = 10000, // 10 second timeout per test
    bun_compile: bool = false, // Use bun build --compile for fair comparison
};

fn printUsage() void {
    const usage =
        \\Usage: edgebox-test262 [options] [test-dir]
        \\
        \\Options:
        \\  --engine=ENGINE    JS engine to use: edgebox, qjs, node, bun (default: qjs)
        \\  --bun-compile      Use bun build --compile for fair comparison with compiled runtimes
        \\  -c, --config=FILE  Config file (test262.conf format)
        \\  --harness=DIR      Harness directory (default: test262/harness)
        \\  -t, --threads=N    Number of parallel threads (default: 1)
        \\  -v, --verbose      Verbose output
        \\  --json             Output results as JSON
        \\  --timeout=MS       Timeout per test in ms (default: 10000)
        \\  -h, --help         Show this help
        \\
        \\Examples:
        \\  edgebox-test262 --engine=node test262/test/language/
        \\  edgebox-test262 --engine=edgebox -c vendor/quickjs-ng/test262.conf
        \\  edgebox-test262 --engine=bun --bun-compile --threads=4 test262/test/built-ins/Array/
        \\
    ;
    std.debug.print("{s}", .{usage});
}

fn parseArgs(allocator: std.mem.Allocator) !?Options {
    var opts = Options{};
    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    // Skip program name
    _ = args.skip();

    while (args.next()) |arg| {
        if (std.mem.startsWith(u8, arg, "--engine=")) {
            const engine_str = arg["--engine=".len..];
            opts.engine = Engine.fromString(engine_str) orelse {
                std.debug.print("Unknown engine: {s}\n", .{engine_str});
                return null;
            };
        } else if (std.mem.startsWith(u8, arg, "-c") or std.mem.startsWith(u8, arg, "--config=")) {
            if (std.mem.startsWith(u8, arg, "--config=")) {
                opts.config_file = arg["--config=".len..];
            } else if (std.mem.eql(u8, arg, "-c")) {
                opts.config_file = args.next();
            }
        } else if (std.mem.startsWith(u8, arg, "--harness=")) {
            opts.harness_dir = arg["--harness=".len..];
        } else if (std.mem.startsWith(u8, arg, "-t") or std.mem.startsWith(u8, arg, "--threads=")) {
            const num_str = if (std.mem.startsWith(u8, arg, "--threads="))
                arg["--threads=".len..]
            else
                args.next() orelse "1";
            opts.threads = std.fmt.parseInt(u32, num_str, 10) catch 1;
        } else if (std.mem.startsWith(u8, arg, "--timeout=")) {
            const num_str = arg["--timeout=".len..];
            opts.timeout_ms = std.fmt.parseInt(u32, num_str, 10) catch 10000;
        } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--verbose")) {
            opts.verbose = true;
        } else if (std.mem.eql(u8, arg, "--json")) {
            opts.json_output = true;
        } else if (std.mem.eql(u8, arg, "--bun-compile")) {
            opts.bun_compile = true;
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            printUsage();
            return null;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            opts.test_dir = arg;
        } else {
            std.debug.print("Unknown option: {s}\n", .{arg});
            return null;
        }
    }

    return opts;
}

const WorkerContext = struct {
    runner: *runner.Runner,
    test_files: []const []const u8,
    results: *results.ResultCollector,
    next_idx: *std.atomic.Value(usize),
    mutex: *std.Thread.Mutex,
    verbose: bool,
};

fn workerThread(ctx: *WorkerContext) void {
    while (true) {
        const idx = ctx.next_idx.fetchAdd(1, .monotonic);
        if (idx >= ctx.test_files.len) break;

        const test_file = ctx.test_files[idx];
        const result = ctx.runner.runTest(test_file) catch |err| {
            ctx.mutex.lock();
            defer ctx.mutex.unlock();
            ctx.results.addError(test_file, err);
            continue;
        };

        ctx.mutex.lock();
        defer ctx.mutex.unlock();
        ctx.results.addResult(test_file, result);

        if (ctx.verbose) {
            const status_char: u8 = switch (result.status) {
                .pass => '.',
                .fail => 'F',
                .skip => 'S',
                .timeout => 'T',
                .error_ => 'E',
            };
            std.debug.print("{c}", .{status_char});
        }
    }
}

fn runTestsParallel(
    allocator: std.mem.Allocator,
    test_runner: *runner.Runner,
    test_files: []const []const u8,
    result_collector: *results.ResultCollector,
    num_threads: u32,
    verbose: bool,
) !void {
    var next_idx = std.atomic.Value(usize).init(0);
    var mutex = std.Thread.Mutex{};

    var ctx = WorkerContext{
        .runner = test_runner,
        .test_files = test_files,
        .results = result_collector,
        .next_idx = &next_idx,
        .mutex = &mutex,
        .verbose = verbose,
    };

    // Create worker threads
    const threads = try allocator.alloc(std.Thread, num_threads);
    defer allocator.free(threads);

    for (threads) |*thread| {
        thread.* = try std.Thread.spawn(.{}, workerThread, .{&ctx});
    }

    // Wait for all threads to complete
    for (threads) |thread| {
        thread.join();
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const opts = try parseArgs(allocator) orelse {
        std.process.exit(1);
    };

    // Load config if provided
    var cfg: ?config.Config = null;
    if (opts.config_file) |cfg_file| {
        cfg = config.Config.load(allocator, cfg_file) catch |err| {
            std.debug.print("Failed to load config {s}: {}\n", .{ cfg_file, err });
            std.process.exit(1);
        };
    }
    defer if (cfg) |*c| c.deinit();

    // Determine test directory
    const test_dir = opts.test_dir orelse
        (if (cfg) |c| c.test_dir else null) orelse {
        std.debug.print("No test directory specified. Use --help for usage.\n", .{});
        std.process.exit(1);
    };

    // Determine harness directory
    const harness_dir = if (cfg) |c| c.harness_dir orelse opts.harness_dir else opts.harness_dir;

    // Load harness files
    var harness_loader = harness.HarnessLoader.init(allocator, harness_dir, opts.engine);
    defer harness_loader.deinit();

    // Initialize runner
    var test_runner = runner.Runner.init(allocator, opts.engine, &harness_loader, opts.timeout_ms, opts.bun_compile);
    defer test_runner.deinit();

    // Initialize results collector
    var result_collector = results.ResultCollector.init(allocator);
    defer result_collector.deinit();

    // Collect test files
    var test_files: std.ArrayListUnmanaged([]const u8) = .{};
    defer {
        for (test_files.items) |f| allocator.free(f);
        test_files.deinit(allocator);
    }

    try collectTestFiles(allocator, test_dir, &test_files, if (cfg) |c| &c else null);

    if (test_files.items.len == 0) {
        std.debug.print("No test files found in {s}\n", .{test_dir});
        std.process.exit(1);
    }

    std.debug.print("Running {d} tests with {s}...\n", .{ test_files.items.len, opts.engine.getCommand() });

    const start_time = std.time.milliTimestamp();

    // Run tests (with parallelism if threads > 1)
    if (opts.threads > 1) {
        try runTestsParallel(allocator, &test_runner, test_files.items, &result_collector, opts.threads, opts.verbose);
    } else {
        for (test_files.items) |test_file| {
            const result = test_runner.runTest(test_file) catch |err| {
                result_collector.addError(test_file, err);
                continue;
            };
            result_collector.addResult(test_file, result);

            if (opts.verbose) {
                const status_char: u8 = switch (result.status) {
                    .pass => '.',
                    .fail => 'F',
                    .skip => 'S',
                    .timeout => 'T',
                    .error_ => 'E',
                };
                std.debug.print("{c}", .{status_char});
            }
        }
    }

    const end_time = std.time.milliTimestamp();
    const total_time_ms = end_time - start_time;

    if (opts.verbose) {
        std.debug.print("\n", .{});
    }

    // Print results
    if (opts.json_output) {
        result_collector.printJson();
    } else {
        result_collector.printSummary(opts.engine, total_time_ms);
    }

    // Exit with error if any tests failed
    if (result_collector.failed > 0 or result_collector.errors > 0) {
        std.process.exit(1);
    }
}

fn collectTestFiles(
    allocator: std.mem.Allocator,
    dir_path: []const u8,
    test_files: *std.ArrayListUnmanaged([]const u8),
    cfg: ?*const config.Config,
) !void {
    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
        // If it's a file, add it directly
        if (err == error.NotDir) {
            if (std.mem.endsWith(u8, dir_path, ".js")) {
                // Check exclusions
                if (cfg) |c| {
                    if (c.isExcluded(dir_path)) return;
                }
                const path_copy = try allocator.dupe(u8, dir_path);
                try test_files.append(allocator, path_copy);
            }
            return;
        }
        return err;
    };
    defer dir.close();

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        const full_path = try std.fs.path.join(allocator, &.{ dir_path, entry.name });
        defer allocator.free(full_path);

        switch (entry.kind) {
            .directory => {
                // Check directory exclusions
                if (cfg) |c| {
                    if (c.isExcluded(full_path)) continue;
                }
                try collectTestFiles(allocator, full_path, test_files, cfg);
            },
            .file => {
                if (std.mem.endsWith(u8, entry.name, ".js")) {
                    // Check file exclusions
                    if (cfg) |c| {
                        if (c.isExcluded(full_path)) continue;
                    }
                    const path_copy = try allocator.dupe(u8, full_path);
                    try test_files.append(allocator, path_copy);
                }
            },
            else => {},
        }
    }
}
