const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // QuickJS source directory
    const quickjs_dir = "vendor/quickjs-ng";

    const quickjs_c_files = &[_][]const u8{
        "quickjs.c",
        "libregexp.c",
        "libunicode.c",
        "cutils.c",
        "quickjs-libc.c",
        "dtoa.c",
    };

    // QuickJS files for WASM (quickjs-libc has __wasi__ support built-in)
    const quickjs_wasm_files = &[_][]const u8{
        "quickjs.c",
        "libregexp.c",
        "libunicode.c",
        "cutils.c",
        "quickjs-libc.c",
        "dtoa.c",
    };

    const quickjs_c_flags = &[_][]const u8{
        "-DCONFIG_VERSION=\"2024-02-14\"",
        "-DCONFIG_BIGNUM",
        "-D_GNU_SOURCE",
        "-fno-sanitize=undefined",
    };

    // WASM-specific flags (disable OS features not available in WASI)
    const quickjs_wasm_flags = &[_][]const u8{
        "-DCONFIG_VERSION=\"2024-02-14\"",
        "-DCONFIG_BIGNUM",
        "-D_GNU_SOURCE",
        "-fno-sanitize=undefined",
        "-D_WASI_EMULATED_SIGNAL",
        "-DNO_OS_SUPPORT",
    };

    // WasmEdge directory (optional, for AOT)
    const wasmedge_dir = b.option(
        []const u8,
        "wasmedge-dir",
        "Path to WasmEdge installation",
    );

    // ===================
    // WASM target (wasm32-wasi)
    // ===================
    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .wasi,
    });

    const wasm_exe = b.addExecutable(.{
        .name = "edgebox-base",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/wasm_main.zig"),
            .target = wasm_target,
            .optimize = if (optimize == .Debug) .ReleaseSmall else optimize,
        }),
    });

    wasm_exe.root_module.addIncludePath(b.path(quickjs_dir));
    wasm_exe.root_module.addCSourceFiles(.{
        .root = b.path(quickjs_dir),
        .files = quickjs_wasm_files,
        .flags = quickjs_wasm_flags,
    });
    wasm_exe.linkLibC();

    const wasm_install = b.addInstallArtifact(wasm_exe, .{});

    const wasm_step = b.step("wasm", "Build QuickJS WASM module");
    wasm_step.dependOn(&wasm_install.step);

    // ===================
    // Native CLI executable
    // ===================
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
        .files = quickjs_c_files,
        .flags = quickjs_c_flags,
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
