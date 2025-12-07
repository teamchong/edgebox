const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // QuickJS source directory
    const quickjs_dir = "vendor/quickjs-ng";

    // WasmEdge directory (optional, for AOT)
    const wasmedge_dir = b.option(
        []const u8,
        "wasmedge-dir",
        "Path to WasmEdge installation",
    );

    // CLI executable
    const exe = b.addExecutable(.{
        .name = "edgebox",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/cli.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    exe.root_module.addIncludePath(b.path(quickjs_dir));
    exe.root_module.addCSourceFiles(.{
        .root = b.path(quickjs_dir),
        .files = &.{
            "quickjs.c",
            "libregexp.c",
            "libunicode.c",
            "cutils.c",
            "quickjs-libc.c",
            "dtoa.c",
        },
        .flags = &.{
            "-DCONFIG_VERSION=\"2024-02-14\"",
            "-DCONFIG_BIGNUM",
            "-D_GNU_SOURCE",
            "-fno-sanitize=undefined",
        },
    });
    exe.linkLibC();

    // Optional: Link WasmEdge for AOT
    if (wasmedge_dir) |we_dir| {
        exe.root_module.addIncludePath(.{ .cwd_relative = b.fmt("{s}/include", .{we_dir}) });
        exe.root_module.addLibraryPath(.{ .cwd_relative = b.fmt("{s}/lib", .{we_dir}) });
        exe.linkSystemLibrary("wasmedge");
    }

    b.installArtifact(exe);

    // Run command
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the EdgeBox CLI");
    run_step.dependOn(&run_cmd.step);

    // Tests
    const unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    unit_tests.root_module.addIncludePath(b.path(quickjs_dir));
    unit_tests.root_module.addCSourceFiles(.{
        .root = b.path(quickjs_dir),
        .files = &.{
            "quickjs.c",
            "libregexp.c",
            "libunicode.c",
            "cutils.c",
            "quickjs-libc.c",
            "dtoa.c",
        },
        .flags = &.{
            "-DCONFIG_VERSION=\"2024-02-14\"",
            "-DCONFIG_BIGNUM",
            "-D_GNU_SOURCE",
            "-fno-sanitize=undefined",
        },
    });
    unit_tests.linkLibC();

    const run_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_tests.step);
}
