const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // QuickJS source directory
    const quickjs_dir = "vendor/quickjs-ng";

    // Apply patches to QuickJS before building (auto-inits submodules if needed)
    const apply_patches = b.addSystemCommand(&.{
        "sh", "-c",
        "test -f vendor/quickjs-ng/quickjs.c || git submodule update --init --recursive; " ++
            "cd vendor/quickjs-ng && for p in ../../patches/*.patch; do test -f \"$p\" && patch -p1 -N --silent < \"$p\" 2>/dev/null || true; done",
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
    // WASM target (wasm32-wasi)
    // Note: SIMD128 disabled for LLVM JIT compatibility on ARM64
    // ===================
    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .wasi,
        .cpu_features_add = std.Target.wasm.featureSet(&.{
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

    // Add frozen functions (pre-compiled hot paths for 18x speedup)
    // Use -Dhost-frozen=true to use host functions for frozen code (AOT mode optimization)
    const use_host_frozen = b.option(bool, "host-frozen", "Use host functions for frozen code") orelse false;
    wasm_exe.root_module.addIncludePath(b.path(quickjs_dir));
    const frozen_file = if (use_host_frozen) "frozen_fib_hybrid.c" else "frozen_fib.c";
    const frozen_flags = if (use_host_frozen)
        quickjs_wasm_flags ++ &[_][]const u8{"-DEDGEBOX_USE_HOST_FROZEN"}
    else
        quickjs_wasm_flags;
    wasm_exe.root_module.addCSourceFiles(.{
        .root = b.path("src/freeze"),
        .files = &.{frozen_file},
        .flags = frozen_flags,
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

    // Add frozen_functions.c (optimized C for self-recursive functions)
    // Generated by edgebox-freeze, provides frozen_init() to hot-swap slow JS functions
    // Use -Dhost-frozen=true to use host functions for frozen code (AOT mode optimization)
    const frozen_static_flags = if (use_host_frozen)
        quickjs_wasm_flags ++ &[_][]const u8{"-DEDGEBOX_USE_HOST_FROZEN"}
    else
        quickjs_wasm_flags;
    wasm_static_exe.root_module.addCSourceFile(.{
        .file = .{ .cwd_relative = "frozen_functions.c" },
        .flags = frozen_static_flags,
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

    // Platform-specific WAMR library path
    const wamr_platform = if (target.result.os.tag == .macos)
        "darwin"
    else if (target.result.os.tag == .linux)
        "linux"
    else
        "linux"; // fallback

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
    // Static link WAMR for fast startup (platform-specific)
    run_exe.addObjectFile(b.path(b.fmt("{s}/product-mini/platforms/{s}/build/libiwasm.a", .{ wamr_dir, wamr_platform })));
    run_exe.linkLibC();
    run_exe.linkSystemLibrary("pthread");
    // WAMR Fast JIT uses asmjit (C++) - need libstdc++ on Linux
    if (target.result.os.tag == .linux) {
        run_exe.linkSystemLibrary("stdc++");
    }


    b.installArtifact(run_exe);

    const runner_step = b.step("runner", "Build edgebox runner (fast, minimal)");
    runner_step.dependOn(&b.addInstallArtifact(run_exe, .{}).step);

    // ===================
    // metal0 shared modules (for HTTP/2 + TLS 1.3)
    // Only included on macOS - Linux builds skip this to avoid libdeflate AVX-512 issues
    // Uses metal0 submodule directly at vendor/metal0
    // Used by native CLI only (not WASM)
    // ===================
    const metal0_dir = "vendor/metal0";

    // Utility modules from metal0
    const hashmap_helper = b.addModule("utils.hashmap_helper", .{
        .root_source_file = b.path(metal0_dir ++ "/src/utils/hashmap_helper.zig"),
    });
    const allocator_helper = b.addModule("utils.allocator_helper", .{
        .root_source_file = b.path(metal0_dir ++ "/src/utils/allocator_helper.zig"),
    });

    // gzip module with libdeflate (use edgebox's copy since metal0's submodule may not be init'd)
    const gzip_module = b.addModule("gzip", .{
        .root_source_file = b.path(metal0_dir ++ "/packages/runtime/src/Modules/gzip/gzip.zig"),
    });
    gzip_module.addIncludePath(b.path("vendor/libdeflate"));

    // green_thread module (goroutine-style concurrency)
    const green_thread_mod = b.addModule("green_thread", .{
        .root_source_file = b.path(metal0_dir ++ "/packages/runtime/src/runtime/green_thread.zig"),
    });
    green_thread_mod.addImport("utils.allocator_helper", allocator_helper);

    // netpoller module (epoll/kqueue async I/O)
    const netpoller_mod = b.addModule("netpoller", .{
        .root_source_file = b.path(metal0_dir ++ "/packages/runtime/src/runtime/netpoller.zig"),
    });
    netpoller_mod.addImport("utils.allocator_helper", allocator_helper);
    netpoller_mod.addImport("green_thread", green_thread_mod);

    // HTTP/2 + TLS 1.3 client from metal0
    const h2_mod = b.addModule("h2", .{
        .root_source_file = b.path(metal0_dir ++ "/packages/shared/http/h2/Client.zig"),
    });
    h2_mod.addImport("gzip", gzip_module);
    h2_mod.addImport("utils.hashmap_helper", hashmap_helper);
    h2_mod.addImport("netpoller", netpoller_mod);
    h2_mod.addImport("green_thread", green_thread_mod);

    // Add h2 to run_exe (edgebox CLI) for HTTP/2 fetch support
    run_exe.root_module.addImport("h2", h2_mod);
    run_exe.root_module.addIncludePath(b.path("vendor/libdeflate"));
    // libdeflate C sources - disable advanced x86 features that Zig's backend can't handle
    // This is needed for Docker/QEMU where the emulated CPU reports features Zig doesn't support
    const libdeflate_flags: []const []const u8 = if (target.result.cpu.arch == .x86_64)
        &.{
            "-O3",
            "-DLIBDEFLATE_ASSEMBLER_DOES_NOT_SUPPORT_AVX512VNNI",
            "-DLIBDEFLATE_ASSEMBLER_DOES_NOT_SUPPORT_AVX_VNNI",
            "-DLIBDEFLATE_ASSEMBLER_DOES_NOT_SUPPORT_VPCLMULQDQ",
        }
    else
        &.{"-O3"};
    run_exe.root_module.addCSourceFiles(.{
        .root = b.path("vendor/libdeflate/lib"),
        .files = &.{
            "deflate_compress.c",
            "deflate_decompress.c",
            "gzip_compress.c",
            "gzip_decompress.c",
            "zlib_compress.c",
            "zlib_decompress.c",
            "adler32.c",
            "crc32.c",
            "utils.c",
            "arm/cpu_features.c",
        },
        .flags = libdeflate_flags,
    });

    // ===================
    // edgebox-rosetta - x86_64 runner for Rosetta 2 (Fast JIT on ARM64 Mac)
    // On Apple Silicon Macs, this binary runs under Rosetta 2 with ~95% native speed
    // and enables WAMR Fast JIT which isn't available on ARM64
    // Only needed on ARM64 Mac - on x86_64 platforms, regular edgebox has Fast JIT
    // ===================
    const x64_target = b.resolveTargetQuery(.{
        .cpu_arch = .x86_64,
        .os_tag = .macos,
    });

    const run_x64_exe = b.addExecutable(.{
        .name = "edgebox-rosetta",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/edgebox_wamr.zig"),
            .target = x64_target,
            .optimize = .ReleaseFast,
        }),
    });

    // Add WAMR include path
    run_x64_exe.root_module.addIncludePath(b.path(wamr_dir ++ "/core/iwasm/include"));
    // Static link x86_64 WAMR with Fast JIT
    run_x64_exe.addObjectFile(b.path(wamr_dir ++ "/product-mini/platforms/darwin/build-x64/libiwasm.a"));
    run_x64_exe.linkLibC();
    run_x64_exe.linkSystemLibrary("pthread");
    // WAMR Fast JIT uses asmjit (C++) - need libc++ on macOS x86_64
    run_x64_exe.linkLibCpp();

    // Add h2 module for HTTP/2 support
    run_x64_exe.root_module.addImport("h2", h2_mod);
    run_x64_exe.root_module.addIncludePath(b.path("vendor/libdeflate"));
    // libdeflate for x86_64 - disable AVX-512 features
    run_x64_exe.root_module.addCSourceFiles(.{
        .root = b.path("vendor/libdeflate/lib"),
        .files = &.{
            "deflate_compress.c",
            "deflate_decompress.c",
            "gzip_compress.c",
            "gzip_decompress.c",
            "zlib_compress.c",
            "zlib_decompress.c",
            "adler32.c",
            "crc32.c",
            "utils.c",
            "x86/cpu_features.c", // x86 CPU features detection
        },
        .flags = &.{
            "-O3",
            "-DLIBDEFLATE_ASSEMBLER_DOES_NOT_SUPPORT_AVX512VNNI",
            "-DLIBDEFLATE_ASSEMBLER_DOES_NOT_SUPPORT_AVX_VNNI",
            "-DLIBDEFLATE_ASSEMBLER_DOES_NOT_SUPPORT_VPCLMULQDQ",
        },
    });

    b.installArtifact(run_x64_exe);

    const runner_rosetta_step = b.step("runner-rosetta", "Build edgebox-rosetta for Rosetta 2 (Fast JIT on ARM64 Mac)");
    runner_rosetta_step.dependOn(&b.addInstallArtifact(run_x64_exe, .{}).step);

    // ===================
    // edgeboxc - full CLI for building (needs wasmedge compile)
    // Uses system WasmEdge with full LLVM AOT compiler
    // Now with HTTP/2 support from metal0!
    // ===================
    const build_exe = b.addExecutable(.{
        .name = "edgeboxc",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/runtime.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    // Add metal0 h2 module for HTTP/2 support
    build_exe.root_module.addImport("h2", h2_mod);
    build_exe.root_module.addImport("utils.hashmap_helper", hashmap_helper);

    // Add libdeflate C sources for gzip
    build_exe.root_module.addIncludePath(b.path("vendor/libdeflate"));
    build_exe.root_module.addCSourceFiles(.{
        .root = b.path("vendor/libdeflate/lib"),
        .files = &.{
            "deflate_compress.c",
            "deflate_decompress.c",
            "gzip_compress.c",
            "gzip_decompress.c",
            "zlib_compress.c",
            "zlib_decompress.c",
            "adler32.c",
            "crc32.c",
            "utils.c",
            "arm/cpu_features.c", // ARM NEON detection
        },
        .flags = libdeflate_flags,
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

    // Link WAMR for daemon (platform-specific)
    daemon_exe.root_module.addIncludePath(b.path(wamr_dir ++ "/core/iwasm/include"));
    daemon_exe.addObjectFile(b.path(b.fmt("{s}/product-mini/platforms/{s}/build/libiwasm.a", .{ wamr_dir, wamr_platform })));
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

    // Link WAMR for wizer (platform-specific)
    wizer_exe.root_module.addIncludePath(b.path(wamr_dir ++ "/core/iwasm/include"));
    wizer_exe.addObjectFile(b.path(b.fmt("{s}/product-mini/platforms/{s}/build/libiwasm.a", .{ wamr_dir, wamr_platform })));
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
    // edgebox-freeze - Bytecode to C transpiler
    // ===================
    const freeze_exe = b.addExecutable(.{
        .name = "edgebox-freeze",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/freeze/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    b.installArtifact(freeze_exe);

    const freeze_step = b.step("freeze", "Build edgebox-freeze (bytecode to C transpiler)");
    freeze_step.dependOn(&b.addInstallArtifact(freeze_exe, .{}).step);

    // ===================
    // gen-opcodes - Generate opcodes.zig from QuickJS headers
    // ===================
    const gen_opcodes_exe = b.addExecutable(.{
        .name = "gen-opcodes",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/freeze/gen_opcodes.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const gen_opcodes_run = b.addRunArtifact(gen_opcodes_exe);
    gen_opcodes_run.addFileArg(b.path("vendor/quickjs-ng/quickjs-opcode.h"));
    gen_opcodes_run.addFileArg(b.path("src/freeze/opcodes_gen.zig"));

    const gen_opcodes_step = b.step("gen-opcodes", "Generate opcodes_gen.zig from QuickJS headers");
    gen_opcodes_step.dependOn(&gen_opcodes_run.step);

    // ===================
    // verify-opcodes - Check opcode compatibility after QuickJS update
    // ===================
    const verify_opcodes_exe = b.addExecutable(.{
        .name = "verify-opcodes",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/freeze/verify_opcodes.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const verify_opcodes_run = b.addRunArtifact(verify_opcodes_exe);
    verify_opcodes_run.addFileArg(b.path("vendor/quickjs-ng/quickjs-opcode.h"));

    const verify_opcodes_step = b.step("verify-opcodes", "Verify opcode compatibility with QuickJS");
    verify_opcodes_step.dependOn(&verify_opcodes_run.step);

    // ===================
    // wamrc - WAMR AOT compiler (requires LLVM)
    // Build from vendor/wamr/wamr-compiler using cmake + make
    // ===================
    const wamrc_build = b.addSystemCommand(&.{
        "sh", "-c",
        \\cd vendor/wamr/wamr-compiler && \
        \\if [ ! -f build/wamrc ]; then \
        \\  mkdir -p build && cd build && \
        \\  cmake .. -DCMAKE_BUILD_TYPE=Release && \
        \\  make -j$(sysctl -n hw.ncpu 2>/dev/null || nproc) wamrc; \
        \\fi
    });
    wamrc_build.setName("build-wamrc");

    const wamrc_copy = b.addSystemCommand(&.{
        "sh", "-c",
        \\mkdir -p zig-out/bin && \
        \\cp vendor/wamr/wamr-compiler/build/wamrc zig-out/bin/wamrc 2>/dev/null || true
    });
    wamrc_copy.step.dependOn(&wamrc_build.step);
    wamrc_copy.setName("copy-wamrc");

    const wamrc_step = b.step("wamrc", "Build wamrc AOT compiler (requires LLVM)");
    wamrc_step.dependOn(&wamrc_copy.step);

    // ===================
    // cli - builds all CLI tools
    // ===================
    const cli_step = b.step("cli", "Build all CLI tools (edgebox, edgeboxc, edgeboxd, edgebox-sandbox, edgebox-wizer, edgebox-wasm-opt, edgebox-freeze, wamrc)");
    cli_step.dependOn(&b.addInstallArtifact(run_exe, .{}).step);
    cli_step.dependOn(&b.addInstallArtifact(build_exe, .{}).step);
    cli_step.dependOn(&b.addInstallArtifact(daemon_exe, .{}).step);
    cli_step.dependOn(&b.addInstallArtifact(sandbox_exe, .{}).step);
    cli_step.dependOn(&b.addInstallArtifact(wizer_exe, .{}).step);
    cli_step.dependOn(&b.addInstallArtifact(wasm_opt_exe, .{}).step);
    cli_step.dependOn(&b.addInstallArtifact(freeze_exe, .{}).step);
    cli_step.dependOn(&wamrc_copy.step);

}
