const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // QuickJS source directory
    const quickjs_dir = "vendor/quickjs-ng";

    // Apply patches to QuickJS before building
    const apply_patches = b.addSystemCommand(&.{
        "sh", "-c",
        \\cd vendor/quickjs-ng && \
        \\for p in ../../patches/*.patch; do \
        \\  if [ -f "$p" ]; then \
        \\    patch -p1 -N --silent < "$p" 2>/dev/null || true; \
        \\  fi; \
        \\done
    });
    apply_patches.setName("apply-quickjs-patches");

    // QuickJS files - dtoa.c is for the December 2025 version
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
        "-D_GNU_SOURCE",
        "-fno-sanitize=undefined",
    };

    // WASM-specific flags (disable OS features not available in WASI)
    const quickjs_wasm_flags = &[_][]const u8{
        "-D_GNU_SOURCE",
        "-fno-sanitize=undefined",
        "-D_WASI_EMULATED_SIGNAL",
        // Disable unused features to reduce WASM size
        "-DCONFIG_BIGNUM=0", // Disable BigInt/BigFloat/BigDecimal
    };

    // WasmEdge directory (optional, for AOT)
    const wasmedge_dir = b.option(
        []const u8,
        "wasmedge-dir",
        "Path to WasmEdge installation",
    );

    // WASI-NN support (requires WasmEdge with GGML plugin)
    const enable_wasi_nn = b.option(
        bool,
        "enable-wasi-nn",
        "Enable WASI-NN support for LLM inference (requires wasmedge --nn-preload)",
    ) orelse false;

    // ===================
    // WASM target (wasm32-wasi) with SIMD128 enabled
    // ===================
    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .wasi,
        .cpu_features_add = std.Target.wasm.featureSet(&.{
            .simd128,
            .bulk_memory,
            .sign_ext,
        }),
    });

    const wasm_exe = b.addExecutable(.{
        .name = "edgebox-base",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/wasm_main.zig"),
            .target = wasm_target,
            .optimize = if (optimize == .Debug) .ReleaseFast else optimize,
        }),
    });

    // Increase initial memory for large JS files (2GB initial, 4GB max)
    // Each page is 64KB, so 32768 pages = 2GB, 65536 pages = 4GB
    // Claude Code bundle needs ~1.3GB for parsing
    wasm_exe.initial_memory = 2 * 1024 * 1024 * 1024; // 2GB initial
    wasm_exe.max_memory = 4 * 1024 * 1024 * 1024; // 4GB max
    wasm_exe.stack_size = 16 * 1024 * 1024; // 16MB stack for deep recursion

    // Pass WASI-NN option to the source
    const build_options = b.addOptions();
    build_options.addOption(bool, "enable_wasi_nn", enable_wasi_nn);
    wasm_exe.root_module.addOptions("build_options", build_options);

    // Export wizer_init function for Wizer pre-initialization
    // Note: rdynamic exports ALL symbols which bloats the binary
    // TODO: Find a way to export only wizer_init selectively
    wasm_exe.rdynamic = true;

    wasm_exe.root_module.addIncludePath(b.path(quickjs_dir));
    wasm_exe.root_module.addCSourceFiles(.{
        .root = b.path(quickjs_dir),
        .files = quickjs_wasm_files,
        .flags = quickjs_wasm_flags,
    });
    wasm_exe.linkLibC();
    wasm_exe.step.dependOn(&apply_patches.step); // Apply patches before compiling

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
    exe.step.dependOn(&apply_patches.step); // Apply patches before compiling

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
            "-D_GNU_SOURCE",
            "-fno-sanitize=undefined",
        },
    });
    unit_tests.linkLibC();
    unit_tests.step.dependOn(&apply_patches.step); // Apply patches before compiling

    const run_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_tests.step);

    // ===================
    // qjsc compiler (native tool for compiling JS to C)
    // ===================
    const qjsc_exe = b.addExecutable(.{
        .name = "qjsc",
        .root_module = b.createModule(.{
            .root_source_file = null,
            .target = target,
            .optimize = .ReleaseFast,
        }),
    });

    qjsc_exe.root_module.addIncludePath(b.path(quickjs_dir));
    qjsc_exe.root_module.addCSourceFiles(.{
        .root = b.path(quickjs_dir),
        .files = &.{
            "qjsc.c",
            "quickjs.c",
            "libregexp.c",
            "libunicode.c",
            "cutils.c",
            "quickjs-libc.c",
            "dtoa.c",
        },
        .flags = quickjs_c_flags,
    });
    qjsc_exe.linkLibC();
    qjsc_exe.step.dependOn(&apply_patches.step); // Apply patches before compiling

    const qjsc_install = b.addInstallArtifact(qjsc_exe, .{});
    const qjsc_step = b.step("qjsc", "Build QuickJS compiler (qjsc)");
    qjsc_step.dependOn(&qjsc_install.step);

    // ===================
    // WASM static target (for pre-compiled bytecode)
    // ===================
    const wasm_static_exe = b.addExecutable(.{
        .name = "edgebox-static",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/wasm_main_static.zig"),
            .target = wasm_target,
            .optimize = if (optimize == .Debug) .ReleaseFast else optimize,
        }),
    });

    // Larger memory for Claude CLI (42MB bytecode + runtime needs ~500MB)
    wasm_static_exe.initial_memory = 512 * 1024 * 1024; // 512MB initial
    wasm_static_exe.max_memory = 4 * 1024 * 1024 * 1024; // 4GB max
    wasm_static_exe.stack_size = 128 * 1024 * 1024; // 128MB stack (very deep promise chains in reactive SDK)

    wasm_static_exe.rdynamic = true;
    wasm_static_exe.root_module.addIncludePath(b.path(quickjs_dir));

    // Add the generated bundle_compiled.c (bytecode only, main() stripped)
    // NOTE: qjsc's main() must be stripped so Zig's main() runs with full native bindings
    wasm_static_exe.root_module.addCSourceFile(.{
        .file = .{ .cwd_relative = "bundle_compiled.c" },
        .flags = quickjs_wasm_flags,
    });

    // NOTE: native_bindings.c is NOT included - Zig's wasm_main_static.zig has complete
    // native bindings (fs, fetch, spawn, crypto) using WasmEdge socket extensions

    wasm_static_exe.root_module.addCSourceFiles(.{
        .root = b.path(quickjs_dir),
        .files = quickjs_wasm_files,
        .flags = quickjs_wasm_flags,
    });
    wasm_static_exe.linkLibC();
    wasm_static_exe.step.dependOn(&apply_patches.step); // Apply patches before compiling

    const wasm_static_install = b.addInstallArtifact(wasm_static_exe, .{});
    const wasm_static_step = b.step("wasm-static", "Build WASM with pre-compiled bytecode");
    wasm_static_step.dependOn(&wasm_static_install.step);

    // WasmEdge paths - use local lib/ or fall back to ~/.wasmedge
    const home = std.process.getEnvVarOwned(b.allocator, "HOME") catch "/tmp";
    defer b.allocator.free(home);

    // Local minimal WasmEdge (built from submodule)
    const local_wasmedge_include = "vendor/wasmedge/include/api";

    // System WasmEdge (full, with AOT compiler)
    const system_wasmedge_include = b.fmt("{s}/.wasmedge/include", .{home});

    // ===================
    // edgebox - minimal fast runner using WAMR (for <10ms cold start)
    // Statically links libiwasm.a (~1MB) for fast startup
    // ===================
    const wamr_dir = "vendor/wamr";

    const run_exe = b.addExecutable(.{
        .name = "edgebox",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/edgebox_wamr.zig"),
            .target = target,
            .optimize = .ReleaseFast,
        }),
    });

    // Add WAMR include path
    run_exe.root_module.addIncludePath(b.path(wamr_dir ++ "/core/iwasm/include"));
    // Static link WAMR for fast startup
    run_exe.addObjectFile(b.path(wamr_dir ++ "/product-mini/platforms/darwin/build/libiwasm.a"));
    run_exe.linkLibC();
    run_exe.linkSystemLibrary("pthread");

    b.installArtifact(run_exe);

    const runner_step = b.step("runner", "Build edgebox runner (fast, minimal)");
    runner_step.dependOn(&b.addInstallArtifact(run_exe, .{}).step);

    // ===================
    // edgeboxc - full CLI for building (needs wasmedge compile)
    // Uses system WasmEdge with full LLVM AOT compiler
    // ===================
    const build_exe = b.addExecutable(.{
        .name = "edgeboxc",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/runtime.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    build_exe.root_module.addIncludePath(.{ .cwd_relative = system_wasmedge_include });
    // Link directly to dylib to avoid TBD parsing issues on macOS
    build_exe.addObjectFile(.{ .cwd_relative = b.fmt("{s}/.wasmedge/lib/libwasmedge.0.1.0.dylib", .{home}) });
    build_exe.linkLibC();

    b.installArtifact(build_exe);

    const build_step = b.step("build-cli", "Build edgeboxc CLI (full features)");
    build_step.dependOn(&b.addInstallArtifact(build_exe, .{}).step);

    // ===================
    // edgeboxd - HTTP daemon server using WAMR
    // Fork-based isolation with copy-on-write memory
    // ===================
    const daemon_exe = b.addExecutable(.{
        .name = "edgeboxd",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/edgeboxd_wamr.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    // Link WAMR for daemon
    daemon_exe.root_module.addIncludePath(b.path(wamr_dir ++ "/core/iwasm/include"));
    daemon_exe.addObjectFile(b.path(wamr_dir ++ "/product-mini/platforms/darwin/build/libiwasm.a"));
    daemon_exe.linkLibC();
    daemon_exe.linkSystemLibrary("pthread");

    b.installArtifact(daemon_exe);

    const daemon_step = b.step("daemon", "Build edgeboxd HTTP daemon");
    daemon_step.dependOn(&b.addInstallArtifact(daemon_exe, .{}).step);

    // ===================
    // edgebox-sandbox - OS-level sandbox wrapper for child_process
    // Enforces .edgebox.json dirs at kernel level
    // ===================
    const sandbox_exe = b.addExecutable(.{
        .name = "edgebox-sandbox",
        .root_module = b.createModule(.{
            .root_source_file = null,
            .target = target,
            .optimize = .ReleaseFast,
        }),
    });

    sandbox_exe.root_module.addCSourceFile(.{
        .file = b.path("src/edgebox_sandbox.c"),
        .flags = &.{},
    });
    sandbox_exe.linkLibC();

    b.installArtifact(sandbox_exe);

    const sandbox_step = b.step("sandbox", "Build edgebox-sandbox (OS-level process sandbox)");
    sandbox_step.dependOn(&b.addInstallArtifact(sandbox_exe, .{}).step);

    // ===================
    // edgebox-wizer - Pure Zig Wizer (WASM pre-initializer) using WAMR
    // Replaces the Rust Wizer dependency
    // ===================
    const wizer_exe = b.addExecutable(.{
        .name = "edgebox-wizer",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/wizer_wamr.zig"),
            .target = target,
            .optimize = .ReleaseFast,
        }),
    });

    // Link WAMR for wizer
    wizer_exe.root_module.addIncludePath(b.path(wamr_dir ++ "/core/iwasm/include"));
    wizer_exe.addObjectFile(b.path(wamr_dir ++ "/product-mini/platforms/darwin/build/libiwasm.a"));
    wizer_exe.linkLibC();
    wizer_exe.linkSystemLibrary("pthread");

    b.installArtifact(wizer_exe);

    const wizer_step = b.step("wizer", "Build edgebox-wizer (pure Zig WASM pre-initializer)");
    wizer_step.dependOn(&b.addInstallArtifact(wizer_exe, .{}).step);

    // ===================
    // edgebox-wasm-opt - Pure Zig wasm-opt (WASM optimizer)
    // Uses Binaryen C API, replaces wasm-opt CLI dependency
    // ===================
    const wasm_opt_exe = b.addExecutable(.{
        .name = "edgebox-wasm-opt",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/wasm_opt.zig"),
            .target = target,
            .optimize = .ReleaseFast,
        }),
    });

    // Binaryen C API (brew install binaryen)
    wasm_opt_exe.root_module.addIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });
    wasm_opt_exe.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/lib" });
    wasm_opt_exe.linkSystemLibrary("binaryen");
    wasm_opt_exe.linkLibCpp();
    wasm_opt_exe.linkLibC();

    b.installArtifact(wasm_opt_exe);

    const wasm_opt_step = b.step("wasm-opt", "Build edgebox-wasm-opt (pure Zig WASM optimizer)");
    wasm_opt_step.dependOn(&b.addInstallArtifact(wasm_opt_exe, .{}).step);

    // ===================
    // edgebox-embedded - Single file deployment (WASM embedded in binary)
    // No file I/O for loading - instant startup!
    // Usage: 1) Copy your wizered WASM to src/embedded_wasm.bin
    //        2) zig build embedded -Doptimize=ReleaseFast
    // ===================
    const embedded_exe = b.addExecutable(.{
        .name = "edgebox-embedded",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/edgebox_embedded.zig"),
            .target = target,
            .optimize = .ReleaseFast,
        }),
    });

    embedded_exe.root_module.addIncludePath(b.path(local_wasmedge_include));
    embedded_exe.addObjectFile(b.path("lib/libwasmedge-minimal.a"));
    embedded_exe.linkLibCpp();
    embedded_exe.linkLibC();
    embedded_exe.linkSystemLibrary("z");

    b.installArtifact(embedded_exe);

    const embedded_step = b.step("embedded", "Build edgebox-embedded (single file, WASM embedded in binary)");
    embedded_step.dependOn(&b.addInstallArtifact(embedded_exe, .{}).step);

    // ===================
    // cli - builds all CLI tools
    // ===================
    const cli_step = b.step("cli", "Build all CLI tools (edgebox, edgeboxc, edgeboxd, edgebox-sandbox, edgebox-wizer, edgebox-wasm-opt)");
    cli_step.dependOn(&b.addInstallArtifact(run_exe, .{}).step);
    cli_step.dependOn(&b.addInstallArtifact(build_exe, .{}).step);
    cli_step.dependOn(&b.addInstallArtifact(daemon_exe, .{}).step);
    cli_step.dependOn(&b.addInstallArtifact(sandbox_exe, .{}).step);
    cli_step.dependOn(&b.addInstallArtifact(wizer_exe, .{}).step);
    cli_step.dependOn(&b.addInstallArtifact(wasm_opt_exe, .{}).step);

}
