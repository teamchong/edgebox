//! Native Transpiler Benchmark
//!
//! Tests the native TypeScript transpiler performance against a variety of inputs.
//! Target: 100x faster than Node.js TypeScript (0.39ms → 0.0039ms = 3.9µs)

const std = @import("std");
const native_transpiler = @import("native_transpiler.zig");

const test_cases = [_][]const u8{
    "const x: number = 1;",
    "const a: number = 1, b: string = 'hello', c: boolean = true;",
    "function add(a: number, b: number): number { return a + b; }",
    "const multiply = (x: number, y: number): number => x * y;",
    "interface User { id: number; name: string; email: string; }",
    "type Status = 'active' | 'inactive' | 'pending';",
    "class Calculator<T extends number> { value: T; constructor(v: T) { this.value = v; } }",
    "import { foo, bar } from './module';",
    "import type { FooType } from './types';",
    "export function helper(data: string[]): boolean { return data.length > 0; }",
    "function identity<T>(value: T): T { return value; }",
    "const fetchData = async (url: string): Promise<Response> => fetch(url);",
};

// Realistic TypeScript module
const realistic_module =
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
    \\  public register(name: string, handler: Handler<T>): void {
    \\    this.handlers.set(name, handler);
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
    \\const defaultConfig: Config = {
    \\  host: 'localhost',
    \\  port: 3000,
    \\  debug: true,
    \\};
    \\
    \\export default createServer;
;

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const print = std.debug.print;

    print("=== Native TypeScript Transpiler Benchmark ===\n\n", .{});

    // Warmup
    print("Warming up...\n", .{});
    for (0..100) |_| {
        for (test_cases) |source| {
            var result = (try native_transpiler.fastTranspile(allocator, source)) orelse continue;
            result.deinit();
        }
    }

    // Benchmark individual test cases
    print("\n--- Individual Test Cases ---\n", .{});
    var total_time: u64 = 0;
    var total_cases: u64 = 0;

    for (test_cases) |source| {
        const iterations: u64 = 10000;
        var timer = try std.time.Timer.start();

        for (0..iterations) |_| {
            var result = (try native_transpiler.fastTranspile(allocator, source)) orelse continue;
            result.deinit();
        }

        const elapsed = timer.read();
        const per_iter_ns = elapsed / iterations;
        total_time += elapsed;
        total_cases += iterations;

        print("  {d:.2}µs - {s}\n", .{
            @as(f64, @floatFromInt(per_iter_ns)) / 1000.0,
            source[0..@min(40, source.len)],
        });
    }

    // Benchmark realistic module
    print("\n--- Realistic Module ({d} bytes) ---\n", .{realistic_module.len});

    const module_iterations: u64 = 10000;
    var module_timer = try std.time.Timer.start();

    for (0..module_iterations) |_| {
        var result = (try native_transpiler.fastTranspile(allocator, realistic_module)) orelse {
            print("Failed to transpile realistic module!\n", .{});
            return;
        };
        result.deinit();
    }

    const module_elapsed = module_timer.read();
    const module_per_iter = module_elapsed / module_iterations;

    print("  {d:.2}µs per transpile\n", .{
        @as(f64, @floatFromInt(module_per_iter)) / 1000.0,
    });

    // Arena allocator benchmark
    print("\n--- Arena Allocator (realistic module) ---\n", .{});

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var arena_timer = try std.time.Timer.start();
    for (0..module_iterations) |_| {
        _ = (try native_transpiler.arenaTranspile(&arena, realistic_module)) orelse continue;
        _ = arena.reset(.retain_capacity);
    }
    const arena_elapsed = arena_timer.read();
    const arena_per_iter = arena_elapsed / module_iterations;

    print("  {d:.2}µs per transpile (arena)\n", .{
        @as(f64, @floatFromInt(arena_per_iter)) / 1000.0,
    });

    // Summary
    print("\n=== Summary ===\n", .{});
    const avg_time = total_time / total_cases;
    print("Average time per small case: {d:.2}µs\n", .{
        @as(f64, @floatFromInt(avg_time)) / 1000.0,
    });
    print("Realistic module: {d:.2}µs\n", .{
        @as(f64, @floatFromInt(module_per_iter)) / 1000.0,
    });
    print("Arena optimized: {d:.2}µs\n", .{
        @as(f64, @floatFromInt(arena_per_iter)) / 1000.0,
    });

    // Compare to Node.js target
    const node_time_us: f64 = 390.0; // Node.js TypeScript: ~390µs
    const speedup = node_time_us / (@as(f64, @floatFromInt(module_per_iter)) / 1000.0);
    const arena_speedup = node_time_us / (@as(f64, @floatFromInt(arena_per_iter)) / 1000.0);

    print("\n=== vs Node.js TypeScript (390µs) ===\n", .{});
    print("Standard allocator: {d:.1}x faster\n", .{speedup});
    print("Arena allocator: {d:.1}x faster\n", .{arena_speedup});

    if (arena_speedup >= 100.0) {
        print("\n✓ TARGET ACHIEVED: 100x faster than Node.js!\n", .{});
    } else {
        print("\nTarget: 100x faster (need {d:.1}µs or less)\n", .{node_time_us / 100.0});
    }
}
