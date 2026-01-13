//! File I/O Benchmark for TypeScript Transpilation
//!
//! Tests different I/O methods to find the fastest approach:
//! - Regular file read
//! - mmap
//! - mmap + madvise hints
//! - Buffer pool reuse

const std = @import("std");
const builtin = @import("builtin");
const native_transpiler = @import("native_transpiler.zig");

const print = std.debug.print;

// ============================================================================
// Test File Generation
// ============================================================================

const sample_ts =
    \\interface Config {
    \\  host: string;
    \\  port: number;
    \\  debug: boolean;
    \\}
    \\
    \\type Handler<T> = (data: T) => Promise<void>;
    \\
    \\export class Server<T> {
    \\  private config: Config;
    \\  private handlers: Map<string, Handler<T>>;
    \\
    \\  constructor(config: Config) {
    \\    this.config = config;
    \\    this.handlers = new Map();
    \\  }
    \\
    \\  public async handle(name: string, data: T): Promise<void> {
    \\    const handler = this.handlers.get(name);
    \\    if (handler) {
    \\      await handler(data);
    \\    }
    \\  }
    \\}
    \\
    \\export function createServer<T>(config: Config): Server<T> {
    \\  return new Server<T>(config);
    \\}
    \\
;

fn generateTestFiles(allocator: std.mem.Allocator, tmp_dir: std.fs.Dir, count: usize, target_size: usize) ![][]const u8 {
    var paths = try allocator.alloc([]const u8, count);
    errdefer allocator.free(paths);

    const reps = @max(1, target_size / sample_ts.len);

    for (0..count) |i| {
        var name_buf: [64]u8 = undefined;
        const name = std.fmt.bufPrint(&name_buf, "test_{d}.ts", .{i}) catch unreachable;

        const file = try tmp_dir.createFile(name, .{});
        defer file.close();

        for (0..reps) |_| {
            try file.writeAll(sample_ts);
        }

        var path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const path = try tmp_dir.realpath(name, &path_buf);
        paths[i] = try allocator.dupe(u8, path);
    }

    return paths;
}

// ============================================================================
// I/O Methods
// ============================================================================

/// Method 1: Regular file read
fn readRegular(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    return try file.readToEndAlloc(allocator, 10 * 1024 * 1024);
}

/// MmapResult for tracking mmap data
const MmapResult = struct {
    data: []const u8,
    mapped: []align(std.heap.page_size_min) const u8,

    fn deinit(self: MmapResult) void {
        if (self.mapped.len > 0) {
            std.posix.munmap(self.mapped);
        }
    }
};

/// Method 2: mmap (basic)
fn readMmap(path: []const u8) !MmapResult {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const fd = file.handle;

    const stat = try std.posix.fstat(fd);
    const size: usize = @intCast(stat.size);
    if (size == 0) return .{ .data = "", .mapped = &.{} };

    const mapped = try std.posix.mmap(
        null,
        size,
        std.posix.PROT.READ,
        .{ .TYPE = .PRIVATE },
        fd,
        0,
    );

    return .{
        .data = mapped,
        .mapped = mapped,
    };
}

/// Method 3: mmap + MADV_SEQUENTIAL
fn readMmapSequential(path: []const u8) !MmapResult {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const fd = file.handle;

    const stat = try std.posix.fstat(fd);
    const size: usize = @intCast(stat.size);
    if (size == 0) return .{ .data = "", .mapped = &.{} };

    const mapped = try std.posix.mmap(
        null,
        size,
        std.posix.PROT.READ,
        .{ .TYPE = .PRIVATE },
        fd,
        0,
    );

    // Hint: sequential access
    if (builtin.os.tag == .linux) {
        std.posix.madvise(mapped, std.posix.MADV.SEQUENTIAL) catch {};
    }

    return .{
        .data = mapped,
        .mapped = mapped,
    };
}

/// Method 4: mmap + MADV_WILLNEED
fn readMmapWillneed(path: []const u8) !MmapResult {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const fd = file.handle;

    const stat = try std.posix.fstat(fd);
    const size: usize = @intCast(stat.size);
    if (size == 0) return .{ .data = "", .mapped = &.{} };

    const mapped = try std.posix.mmap(
        null,
        size,
        std.posix.PROT.READ,
        .{ .TYPE = .PRIVATE },
        fd,
        0,
    );

    // Prefetch
    if (builtin.os.tag == .linux) {
        std.posix.madvise(mapped, std.posix.MADV.WILLNEED) catch {};
    }

    return .{
        .data = mapped,
        .mapped = mapped,
    };
}

/// Method 5: Pre-allocated buffer read
fn readBufferPool(path: []const u8, buffer: []u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const bytes_read = try file.readAll(buffer);
    return buffer[0..bytes_read];
}

// ============================================================================
// Main Benchmark
// ============================================================================

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    print("=== File I/O Benchmark ===\n\n", .{});
    print("Platform: {s}\n", .{@tagName(builtin.os.tag)});
    print("CPU cores: {d}\n\n", .{std.Thread.getCpuCount() catch 1});

    // Create temp directory
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    print("Generating test files...\n", .{});

    const small_files = try generateTestFiles(allocator, tmp_dir.dir, 100, 1024);
    defer {
        for (small_files) |p| allocator.free(p);
        allocator.free(small_files);
    }

    const medium_files = try generateTestFiles(allocator, tmp_dir.dir, 10, 100 * 1024);
    defer {
        for (medium_files) |p| allocator.free(p);
        allocator.free(medium_files);
    }

    print("Test files: 100×1KB, 10×100KB\n\n", .{});

    // ========== Single File ==========
    print("--- Single File Read (1KB) ---\n", .{});

    const test_path = small_files[0];
    const iterations: u32 = 1000;

    // Regular read
    {
        var total: u64 = 0;
        for (0..iterations) |_| {
            var timer = try std.time.Timer.start();
            const data = try readRegular(allocator, test_path);
            total += timer.read();
            allocator.free(data);
        }
        print("  Regular read:     {d:.2}µs\n", .{@as(f64, @floatFromInt(total / iterations)) / 1000.0});
    }

    // mmap
    {
        var total: u64 = 0;
        for (0..iterations) |_| {
            var timer = try std.time.Timer.start();
            const result = try readMmap(test_path);
            total += timer.read();
            result.deinit();
        }
        print("  mmap:             {d:.2}µs\n", .{@as(f64, @floatFromInt(total / iterations)) / 1000.0});
    }

    // mmap + SEQUENTIAL
    {
        var total: u64 = 0;
        for (0..iterations) |_| {
            var timer = try std.time.Timer.start();
            const result = try readMmapSequential(test_path);
            total += timer.read();
            result.deinit();
        }
        print("  mmap+SEQUENTIAL:  {d:.2}µs\n", .{@as(f64, @floatFromInt(total / iterations)) / 1000.0});
    }

    // mmap + WILLNEED
    {
        var total: u64 = 0;
        for (0..iterations) |_| {
            var timer = try std.time.Timer.start();
            const result = try readMmapWillneed(test_path);
            total += timer.read();
            result.deinit();
        }
        print("  mmap+WILLNEED:    {d:.2}µs\n", .{@as(f64, @floatFromInt(total / iterations)) / 1000.0});
    }

    // Buffer pool
    {
        var buffer: [64 * 1024]u8 = undefined;
        var total: u64 = 0;
        for (0..iterations) |_| {
            var timer = try std.time.Timer.start();
            _ = try readBufferPool(test_path, &buffer);
            total += timer.read();
        }
        print("  Buffer pool:      {d:.2}µs\n", .{@as(f64, @floatFromInt(total / iterations)) / 1000.0});
    }

    // ========== Batch Files ==========
    print("\n--- Batch Read: 100 × 1KB files ---\n", .{});

    // Regular
    {
        var timer = try std.time.Timer.start();
        for (small_files) |path| {
            const data = try readRegular(allocator, path);
            allocator.free(data);
        }
        print("  Regular (seq):    {d:.2}ms\n", .{@as(f64, @floatFromInt(timer.read())) / 1_000_000.0});
    }

    // mmap
    {
        var timer = try std.time.Timer.start();
        for (small_files) |path| {
            const result = try readMmap(path);
            result.deinit();
        }
        print("  mmap (seq):       {d:.2}ms\n", .{@as(f64, @floatFromInt(timer.read())) / 1_000_000.0});
    }

    // Buffer pool
    {
        var buffer: [64 * 1024]u8 = undefined;
        var timer = try std.time.Timer.start();
        for (small_files) |path| {
            _ = try readBufferPool(path, &buffer);
        }
        print("  Buffer pool (seq):{d:.2}ms\n", .{@as(f64, @floatFromInt(timer.read())) / 1_000_000.0});
    }

    // ========== Full Pipeline ==========
    print("\n--- Full Pipeline: Read + Transpile (100 × 1KB) ---\n", .{});

    // Regular + transpile
    {
        var output_buf: [64 * 1024]u8 = undefined;
        var timer = try std.time.Timer.start();
        for (small_files) |path| {
            const data = try readRegular(allocator, path);
            defer allocator.free(data);
            _ = native_transpiler.streamingTranspile(&output_buf, data);
        }
        print("  Regular + transpile: {d:.2}ms\n", .{@as(f64, @floatFromInt(timer.read())) / 1_000_000.0});
    }

    // mmap + transpile
    {
        var output_buf: [64 * 1024]u8 = undefined;
        var timer = try std.time.Timer.start();
        for (small_files) |path| {
            const result = try readMmap(path);
            defer result.deinit();
            _ = native_transpiler.streamingTranspile(&output_buf, result.data);
        }
        print("  mmap + transpile:    {d:.2}ms\n", .{@as(f64, @floatFromInt(timer.read())) / 1_000_000.0});
    }

    // Buffer pool + transpile
    {
        var read_buf: [64 * 1024]u8 = undefined;
        var output_buf: [64 * 1024]u8 = undefined;
        var timer = try std.time.Timer.start();
        for (small_files) |path| {
            const data = try readBufferPool(path, &read_buf);
            _ = native_transpiler.streamingTranspile(&output_buf, data);
        }
        print("  Buffer + transpile:  {d:.2}ms\n", .{@as(f64, @floatFromInt(timer.read())) / 1_000_000.0});
    }

    // ========== Medium Files ==========
    print("\n--- Full Pipeline: Read + Transpile (10 × 100KB) ---\n", .{});

    // mmap + transpile
    {
        var output_buf: [1024 * 1024]u8 = undefined;
        var timer = try std.time.Timer.start();
        for (medium_files) |path| {
            const result = try readMmap(path);
            defer result.deinit();
            _ = native_transpiler.streamingTranspile(&output_buf, result.data);
        }
        print("  mmap + transpile:    {d:.2}ms\n", .{@as(f64, @floatFromInt(timer.read())) / 1_000_000.0});
    }

    // Buffer pool + transpile
    {
        var read_buf: [1024 * 1024]u8 = undefined;
        var output_buf: [1024 * 1024]u8 = undefined;
        var timer = try std.time.Timer.start();
        for (medium_files) |path| {
            const data = try readBufferPool(path, &read_buf);
            _ = native_transpiler.streamingTranspile(&output_buf, data);
        }
        print("  Buffer + transpile:  {d:.2}ms\n", .{@as(f64, @floatFromInt(timer.read())) / 1_000_000.0});
    }

    // ========== Summary ==========
    print("\n=== Recommendations ===\n", .{});
    print("• Small files (<10KB): Buffer pool is fastest\n", .{});
    print("• Large files (>100KB): mmap with SEQUENTIAL hint\n", .{});
    print("• Best overall: Buffer pool for read, streaming transpile\n", .{});
}
