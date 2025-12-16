// Pure Zig wrapper for Binaryen wasm-opt
// Removes dependency on wasm-opt CLI binary
//
// Usage:
//   edgeboxc optimize input.wasm output.wasm [-Oz|-Os|-O1|-O2|-O3|-O4]

const std = @import("std");

// Binaryen C API bindings
const c = @cImport({
    @cInclude("binaryen-c.h");
});

pub const OptLevel = enum {
    O0, // No optimization
    O1, // Basic optimization
    O2, // More optimization
    O3, // Full optimization
    O4, // Aggressive optimization
    Os, // Optimize for size
    Oz, // Optimize aggressively for size
};

pub const OptResult = struct {
    binary: []u8,
    original_size: usize,
    optimized_size: usize,
};

/// Optimize a WASM binary using Binaryen
pub fn optimize(allocator: std.mem.Allocator, input: []const u8, level: OptLevel) !OptResult {
    const original_size = input.len;

    // Read WASM module
    const module = c.BinaryenModuleRead(@constCast(@ptrCast(input.ptr)), input.len);
    if (module == null) {
        return error.InvalidWasm;
    }
    defer c.BinaryenModuleDispose(module);

    // Enable required WASM features to match our WASM build
    const current_features = c.BinaryenModuleGetFeatures(module);
    const required_features = c.BinaryenFeatureBulkMemory() |
        c.BinaryenFeatureBulkMemoryOpt() |
        c.BinaryenFeatureSIMD128() |
        c.BinaryenFeatureSignExt() |
        c.BinaryenFeatureReferenceTypes() |
        c.BinaryenFeatureMutableGlobals();
    c.BinaryenModuleSetFeatures(module, current_features | required_features);

    // Set optimization level
    switch (level) {
        .O0 => {
            c.BinaryenSetOptimizeLevel(0);
            c.BinaryenSetShrinkLevel(0);
        },
        .O1 => {
            c.BinaryenSetOptimizeLevel(1);
            c.BinaryenSetShrinkLevel(0);
        },
        .O2 => {
            c.BinaryenSetOptimizeLevel(2);
            c.BinaryenSetShrinkLevel(0);
        },
        .O3 => {
            c.BinaryenSetOptimizeLevel(3);
            c.BinaryenSetShrinkLevel(0);
        },
        .O4 => {
            c.BinaryenSetOptimizeLevel(4);
            c.BinaryenSetShrinkLevel(0);
        },
        .Os => {
            c.BinaryenSetOptimizeLevel(2);
            c.BinaryenSetShrinkLevel(1);
        },
        .Oz => {
            c.BinaryenSetOptimizeLevel(2);
            c.BinaryenSetShrinkLevel(2);
        },
    }

    // Run optimization passes
    c.BinaryenModuleOptimize(module);

    // Write optimized module
    const result = c.BinaryenModuleAllocateAndWrite(module, null);
    if (result.binary == null) {
        return error.OptimizationFailed;
    }
    defer std.c.free(result.binary);

    // Copy to Zig-managed memory
    const binary = try allocator.alloc(u8, result.binaryBytes);
    @memcpy(binary, @as([*]const u8, @ptrCast(result.binary))[0..result.binaryBytes]);

    return OptResult{
        .binary = binary,
        .original_size = original_size,
        .optimized_size = result.binaryBytes,
    };
}

/// Run wasm-opt from CLI (edgeboxc optimize)
pub fn runOptimize(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len < 2) {
        std.debug.print("Usage: edgeboxc optimize <input.wasm> <output.wasm> [-Oz|-Os|-O1|-O2|-O3|-O4]\n", .{});
        std.process.exit(1);
    }

    const input_path = args[0];
    const output_path = args[1];

    // Parse optimization level
    var level: OptLevel = .Oz; // Default to size optimization
    if (args.len >= 3) {
        const opt = args[2];
        if (std.mem.eql(u8, opt, "-O0")) level = .O0
        else if (std.mem.eql(u8, opt, "-O1")) level = .O1
        else if (std.mem.eql(u8, opt, "-O2")) level = .O2
        else if (std.mem.eql(u8, opt, "-O3")) level = .O3
        else if (std.mem.eql(u8, opt, "-O4")) level = .O4
        else if (std.mem.eql(u8, opt, "-Os")) level = .Os
        else if (std.mem.eql(u8, opt, "-Oz")) level = .Oz;
    }

    // Read input file
    const input_file = try std.fs.cwd().openFile(input_path, .{});
    defer input_file.close();
    const input = try input_file.readToEndAlloc(allocator, 100 * 1024 * 1024); // 100MB max
    defer allocator.free(input);

    // Optimize
    const result = try optimize(allocator, input, level);
    defer allocator.free(result.binary);

    // Write output file
    const output_file = try std.fs.cwd().createFile(output_path, .{});
    defer output_file.close();
    try output_file.writeAll(result.binary);

    const reduction = @as(f64, @floatFromInt(result.original_size - result.optimized_size)) / @as(f64, @floatFromInt(result.original_size)) * 100;
    std.debug.print("Optimized: {s} -> {s}\n", .{ input_path, output_path });
    std.debug.print("  Original: {} bytes\n", .{result.original_size});
    std.debug.print("  Optimized: {} bytes ({d:.1}% reduction)\n", .{ result.optimized_size, reduction });
}

/// Main entry point for standalone wasm-opt binary
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: edgebox-wasm-opt <input.wasm> <output.wasm> [-Oz|-Os|-O1|-O2|-O3|-O4]\n", .{});
        std.debug.print("\nOptimization levels:\n", .{});
        std.debug.print("  -O0  No optimization\n", .{});
        std.debug.print("  -O1  Basic optimization\n", .{});
        std.debug.print("  -O2  More optimization\n", .{});
        std.debug.print("  -O3  Full optimization\n", .{});
        std.debug.print("  -O4  Aggressive optimization\n", .{});
        std.debug.print("  -Os  Optimize for size\n", .{});
        std.debug.print("  -Oz  Optimize aggressively for size (default)\n", .{});
        std.process.exit(1);
    }

    try runOptimize(allocator, args[1..]);
}
