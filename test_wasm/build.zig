const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .wasi,
    });

    const optimize = b.standardOptimizeOption(.{});

    // Build math.wasm
    const math_wasm = b.addExecutable(.{
        .name = "math",
        .root_module = b.createModule(.{
            .root_source_file = b.path("math.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    // Set WASM-specific options
    math_wasm.entry = .disabled; // No _start function needed
    math_wasm.rdynamic = true; // Export all symbols

    // Install to zig-out/bin/
    b.installArtifact(math_wasm);
}
