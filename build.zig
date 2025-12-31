const std = @import("std");
const build_cache = @import("build_cache.zig");

/// Custom step to apply QuickJS patches (pure Zig, no shell)
/// Checks if submodule initialized, applies patches if needed, creates marker file
const ApplyPatchesStep = struct {
    step: std.Build.Step,
    builder: *std.Build,

    pub fn create(b: *std.Build) *ApplyPatchesStep {
        const self = b.allocator.create(ApplyPatchesStep) catch @panic("OOM");
        self.* = .{
            .step = std.Build.Step.init(.{
                .id = .custom,
                .name = "apply-quickjs-patches",
                .owner = b,
                .makeFn = make,
            }),
            .builder = b,
        };
        return self;
    }

    fn make(step: *std.Build.Step, _: std.Build.Step.MakeOptions) anyerror!void {
        const self: *ApplyPatchesStep = @fieldParentPtr("step", step);
        const b = self.builder;
        const cwd = std.fs.cwd();

        // Check if submodule is initialized (quickjs.c exists)
        const quickjs_c_exists = cwd.access("vendor/quickjs-ng/quickjs.c", .{}) != error.FileNotFound;
        if (!quickjs_c_exists) {
            std.debug.print("[build] Initializing quickjs-ng submodule...\n", .{});
            var git_proc = std.process.Child.init(&.{ "git", "submodule", "update", "--init", "--recursive" }, b.allocator);
            _ = try git_proc.spawnAndWait();
        }

        // Check if patches already applied (marker file exists)
        const marker_exists = cwd.access("vendor/quickjs-ng/.patches-applied", .{}) != error.FileNotFound;
        if (marker_exists) {
            return; // Already patched
        }

        std.debug.print("[build] Applying QuickJS patches...\n", .{});

        // Reset quickjs-ng to clean state before patching
        var git_checkout = std.process.Child.init(&.{ "git", "-C", "vendor/quickjs-ng", "checkout", "." }, b.allocator);
        _ = git_checkout.spawnAndWait() catch {}; // Ignore errors

        // Find and apply patch files
        var patches_dir = cwd.openDir("patches", .{ .iterate = true }) catch |err| {
            std.debug.print("[build] Warning: Could not open patches directory: {}\n", .{err});
            return;
        };
        defer patches_dir.close();

        var iter = patches_dir.iterate();
        while (try iter.next()) |entry| {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".patch")) {
                std.debug.print("[build] Applying patch: {s}\n", .{entry.name});

                // Construct absolute path for patch file
                const abs_patch_path = b.fmt("{s}/patches/{s}", .{ b.build_root.path orelse ".", entry.name });

                var patch_proc = std.process.Child.init(&.{ "patch", "-d", "vendor/quickjs-ng", "-p1", "--silent", "-i", abs_patch_path }, b.allocator);
                const result = patch_proc.spawnAndWait() catch |err| {
                    std.debug.print("[build] Warning: Failed to apply {s}: {}\n", .{ entry.name, err });
                    continue;
                };
                if (result.Exited != 0) {
                    std.debug.print("[build] Warning: Patch {s} returned non-zero: {}\n", .{ entry.name, result.Exited });
                }
            }
        }

        // Create marker file
        const marker = try cwd.createFile("vendor/quickjs-ng/.patches-applied", .{});
        marker.close();
        std.debug.print("[build] Patches applied successfully\n", .{});
    }
};

/// Custom step to save prebuilt libraries (pure Zig, no shell)
/// Copies built WAMR and Binaryen libraries to prebuilt directory
const SavePrebuiltStep = struct {
    step: std.Build.Step,
    builder: *std.Build,
    prebuilt_dir: []const u8,
    wamr_platform: []const u8,
    binaryen_lib: []const u8,

    pub fn create(b: *std.Build, prebuilt_dir: []const u8, wamr_platform: []const u8, binaryen_lib: []const u8) *SavePrebuiltStep {
        const self = b.allocator.create(SavePrebuiltStep) catch @panic("OOM");
        self.* = .{
            .step = std.Build.Step.init(.{
                .id = .custom,
                .name = "save-prebuilt-libs",
                .owner = b,
                .makeFn = make,
            }),
            .builder = b,
            .prebuilt_dir = prebuilt_dir,
            .wamr_platform = wamr_platform,
            .binaryen_lib = binaryen_lib,
        };
        return self;
    }

    fn make(step: *std.Build.Step, _: std.Build.Step.MakeOptions) anyerror!void {
        const self: *SavePrebuiltStep = @fieldParentPtr("step", step);
        const cwd = std.fs.cwd();

        // Create directories
        const wamr_out = std.fmt.allocPrint(self.builder.allocator, "{s}/wamr", .{self.prebuilt_dir}) catch @panic("OOM");
        const binaryen_out = std.fmt.allocPrint(self.builder.allocator, "{s}/binaryen", .{self.prebuilt_dir}) catch @panic("OOM");

        cwd.makePath(wamr_out) catch |err| {
            std.debug.print("[build] Warning: Could not create {s}: {}\n", .{ wamr_out, err });
            return;
        };
        cwd.makePath(binaryen_out) catch |err| {
            std.debug.print("[build] Warning: Could not create {s}: {}\n", .{ binaryen_out, err });
            return;
        };

        // Copy WAMR libraries
        const iwasm_src = std.fmt.allocPrint(self.builder.allocator, "vendor/wamr/product-mini/platforms/{s}/build/libiwasm.a", .{self.wamr_platform}) catch @panic("OOM");
        const iwasm_dst = std.fmt.allocPrint(self.builder.allocator, "{s}/wamr/libiwasm.a", .{self.prebuilt_dir}) catch @panic("OOM");
        cwd.copyFile(iwasm_src, cwd, iwasm_dst, .{}) catch |err| {
            std.debug.print("[build] Warning: Could not copy libiwasm.a: {}\n", .{err});
        };

        // Copy AOT compiler libraries (libaotclib.a, libvmlib.a)
        const aotclib_src = "vendor/wamr/wamr-compiler/build/libaotclib.a";
        const aotclib_dst = std.fmt.allocPrint(self.builder.allocator, "{s}/wamr/libaotclib.a", .{self.prebuilt_dir}) catch @panic("OOM");
        cwd.copyFile(aotclib_src, cwd, aotclib_dst, .{}) catch |err| {
            std.debug.print("[build] Warning: Could not copy libaotclib.a: {}\n", .{err});
        };

        const vmlib_src = "vendor/wamr/wamr-compiler/build/libvmlib.a";
        const vmlib_dst = std.fmt.allocPrint(self.builder.allocator, "{s}/wamr/libvmlib.a", .{self.prebuilt_dir}) catch @panic("OOM");
        cwd.copyFile(vmlib_src, cwd, vmlib_dst, .{}) catch |err| {
            std.debug.print("[build] Warning: Could not copy libvmlib.a: {}\n", .{err});
        };

        // Copy Binaryen library
        const binaryen_src = std.fmt.allocPrint(self.builder.allocator, "vendor/binaryen/build/lib/{s}", .{self.binaryen_lib}) catch @panic("OOM");
        const binaryen_dst = std.fmt.allocPrint(self.builder.allocator, "{s}/binaryen/{s}", .{ self.prebuilt_dir, self.binaryen_lib }) catch @panic("OOM");
        cwd.copyFile(binaryen_src, cwd, binaryen_dst, .{}) catch |err| {
            std.debug.print("[build] Warning: Could not copy {s}: {}\n", .{ self.binaryen_lib, err });
        };

        std.debug.print("[build] Saved prebuilt libraries to {s}\n", .{self.prebuilt_dir});
    }
};

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Option to skip frozen functions test in cli build (for faster iteration)
    const skip_frozen_test = b.option(bool, "skip-frozen-test", "Skip frozen functions test in cli build") orelse false;

    // Auto-detect if we should use prebuilt libraries (hash-based caching)
    // Speeds up build 10x - prebuilts committed to git for CI
    const prebuilt_dir = blk: {
        const os_tag = target.result.os.tag;
        const cpu_arch = target.result.cpu.arch;

        if (os_tag == .macos and cpu_arch == .aarch64) {
            break :blk "vendor/prebuilt/darwin-arm64";
        } else if (os_tag == .macos and cpu_arch == .x86_64) {
            break :blk "vendor/prebuilt/darwin-x64";
        } else if (os_tag == .linux and cpu_arch == .aarch64) {
            break :blk "vendor/prebuilt/linux-arm64";
        } else if (os_tag == .linux and cpu_arch == .x86_64) {
            break :blk "vendor/prebuilt/linux-x64";
        } else {
            break :blk null;
        }
    };

    const use_prebuilt = if (prebuilt_dir) |dir| blk: {
        const source_dirs = [_][]const u8{
            "vendor/wamr/core",
            "vendor/wamr/wamr-compiler",
            "vendor/binaryen/src",
        };
        const should_use = build_cache.shouldUsePrebuilt(b.allocator, dir, &source_dirs) catch false;
        if (should_use) {
            std.debug.print("[build] Using prebuilt libraries (sources unchanged)\n", .{});
        } else {
            std.debug.print("[build] Building from source (sources changed or prebuilt missing)\n", .{});
        }
        break :blk should_use;
    } else false;

    // QuickJS source directory
    const quickjs_dir = "vendor/quickjs-ng";

    // Apply patches to QuickJS before building (pure Zig, no shell)
    // Uses marker file to prevent duplicate patch application
    const apply_patches_step = ApplyPatchesStep.create(b);
    const apply_patches = &apply_patches_step.step;

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

    // WASI-NN support
    const enable_wasi_nn = b.option(
        bool,
        "enable-wasi-nn",
        "Enable WASI-NN support for LLM inference",
    ) orelse false;

    // GPU support (WebGPU via wgpu-native)
    const enable_gpu = b.option(
        bool,
        "enable-gpu",
        "Enable GPU compute support (WebGPU via wgpu-native)",
    ) orelse false;

    // Auto-download wgpu-native if GPU enabled and not present
    const download_wgpu = b.addSystemCommand(&.{
        "sh", "-c",
        \\if [ ! -f vendor/wgpu-native/lib/libwgpu_native.a ]; then
        \\  echo "[build] Downloading wgpu-native..."
        \\  ./scripts/download-wgpu.sh
        \\fi
    });
    download_wgpu.setName("download-wgpu-native");

    // ===================
    // WASM target (wasm32-wasi) - SIMD ENABLED (NEVER disable!)
    // SIMD + AOT/JIT + Wizer + wasm-opt must ALL be enabled
    // ===================
    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .wasi,
        .cpu_features_add = std.Target.wasm.featureSet(&.{
            .bulk_memory,
            .sign_ext,
            .simd128, // SIMD ENABLED - required for performance
        }),
    });

    const wasm_exe = b.addExecutable(.{
        .name = "edgebox-base",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/wasm_main.zig"),
            .target = wasm_target,
            .optimize = if (optimize == .Debug) .ReleaseFast else optimize,
            .strip = true, // Remove debug symbols (saves ~4MB)
        }),
    });

    // WASM memory: start small, grow as needed (up to 4GB)
    // Initial 64MB is enough for most JS files. Memory grows dynamically via memory.grow.
    // Large bundles like Claude Code (~1.3GB) will trigger growth automatically.
    wasm_exe.initial_memory = 64 * 1024 * 1024; // 64MB initial (grows as needed)
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
    // All frozen functions stay in WASM/AOT (sandboxed) - no host exports
    wasm_exe.root_module.addIncludePath(b.path(quickjs_dir));
    wasm_exe.root_module.addCSourceFiles(.{
        .root = b.path("src/freeze"),
        .files = &.{"frozen_fib.c"},
        .flags = quickjs_wasm_flags,
    });

    wasm_exe.linkLibC();
    wasm_exe.step.dependOn(apply_patches); // Apply patches before compiling

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
    exe.step.dependOn(apply_patches); // Apply patches before compiling


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
    unit_tests.step.dependOn(apply_patches); // Apply patches before compiling

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
    qjsc_exe.step.dependOn(apply_patches); // Apply patches before compiling

    const qjsc_install = b.addInstallArtifact(qjsc_exe, .{});
    const qjsc_step = b.step("qjsc", "Build QuickJS compiler (qjsc)");
    qjsc_step.dependOn(&qjsc_install.step);

    // ===================
    // WASM static target (for pre-compiled bytecode)
    // ===================

    // Source directory option for isolated builds (e.g., bench/hello.js -> bench/zig-out/)
    const source_dir_raw = b.option([]const u8, "source-dir", "Source directory for build artifacts") orelse "";

    // Validate source_dir to prevent path traversal attacks
    // Reject paths containing ".." or absolute paths starting with "/"
    const source_dir = blk: {
        if (source_dir_raw.len == 0) break :blk source_dir_raw;
        // Check for path traversal sequences
        if (std.mem.indexOf(u8, source_dir_raw, "..") != null) {
            std.debug.print("Error: source-dir cannot contain '..'\n", .{});
            break :blk "";
        }
        // Reject absolute paths
        if (source_dir_raw[0] == '/' or source_dir_raw[0] == '\\') {
            std.debug.print("Error: source-dir cannot be an absolute path\n", .{});
            break :blk "";
        }
        break :blk source_dir_raw;
    };

    const wasm_static_exe = b.addExecutable(.{
        .name = "edgebox-static",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/wasm_main_static.zig"),
            .target = wasm_target,
            .optimize = if (optimize == .Debug) .ReleaseFast else optimize,
            .strip = true, // Remove debug symbols (saves ~4MB)
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
    // edgeboxc build generates zig-out/cache/<source-dir>/bundle_compiled.c for the target JS file
    const bundle_compiled_path = if (source_dir.len > 0)
        b.fmt("zig-out/cache/{s}/bundle_compiled.c", .{source_dir})
    else
        "zig-out/cache/bundle_compiled.c";
    wasm_static_exe.root_module.addCSourceFile(.{
        .file = .{ .cwd_relative = bundle_compiled_path },
        .flags = quickjs_wasm_flags,
    });

    // Add frozen runtime header include path
    wasm_static_exe.root_module.addIncludePath(b.path("src/freeze"));

    // Add frozen_runtime.c (shared helpers - pre-compiled once)
    // Contains SMI arithmetic, comparison, bitwise helpers used by all frozen functions
    wasm_static_exe.root_module.addCSourceFile(.{
        .file = b.path("src/freeze/frozen_runtime.c"),
        .flags = quickjs_wasm_flags,
    });

    // Add frozen_functions.c (per-project user functions)
    // Generated by edgebox-freeze, provides frozen_init() to hot-swap slow JS functions
    // All frozen functions stay in WASM/AOT (sandboxed) - no host exports
    // Per-project location - each project has its own frozen functions
    const frozen_c_path = if (source_dir.len > 0)
        b.fmt("zig-out/cache/{s}/frozen_functions.c", .{source_dir})
    else
        "zig-out/cache/frozen_functions.c";

    wasm_static_exe.root_module.addCSourceFile(.{
        .file = .{ .cwd_relative = frozen_c_path },
        .flags = quickjs_wasm_flags,
    });

    // NOTE: native_bindings.c is NOT included - Zig's wasm_main_static.zig has complete
    // native bindings (fs, fetch, spawn, crypto)

    wasm_static_exe.root_module.addCSourceFiles(.{
        .root = b.path(quickjs_dir),
        .files = quickjs_wasm_files,
        .flags = quickjs_wasm_flags,
    });
    wasm_static_exe.linkLibC();
    wasm_static_exe.step.dependOn(apply_patches); // Apply patches before compiling

    const wasm_static_install = b.addInstallArtifact(wasm_static_exe, .{});
    const wasm_static_step = b.step("wasm-static", "Build WASM with pre-compiled bytecode");
    wasm_static_step.dependOn(&wasm_static_install.step);

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

    // Use prebuilt or source build paths
    const wamr_lib_path = if (use_prebuilt and prebuilt_dir != null)
        b.fmt("{s}/wamr/libiwasm.a", .{prebuilt_dir.?})
    else
        b.fmt("{s}/product-mini/platforms/{s}/build/libiwasm.a", .{ wamr_dir, wamr_platform });
    const wamr_aot_lib_path = if (use_prebuilt and prebuilt_dir != null)
        b.fmt("{s}/wamr/libaotclib.a", .{prebuilt_dir.?})
    else
        b.fmt("{s}/wamr-compiler/build/libaotclib.a", .{wamr_dir});
    const wamr_vm_lib_path = if (use_prebuilt and prebuilt_dir != null)
        b.fmt("{s}/wamr/libvmlib.a", .{prebuilt_dir.?})
    else
        b.fmt("{s}/wamr-compiler/build/libvmlib.a", .{wamr_dir});
    const binaryen_lib_path = if (use_prebuilt and prebuilt_dir != null)
        b.fmt("{s}/binaryen", .{prebuilt_dir.?})
    else
        "vendor/binaryen/build/lib";

    const run_exe = b.addExecutable(.{
        .name = "edgebox",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/edgebox_wamr.zig"),
            .target = target,
            .optimize = .ReleaseFast,
        }),
    });
    run_exe.stack_size = 8 * 1024 * 1024; // 8MB native stack for AOT execution

    // Add WAMR include path
    run_exe.root_module.addIncludePath(b.path(wamr_dir ++ "/core/iwasm/include"));
    // Static link WAMR for fast startup (platform-specific)
    run_exe.addObjectFile(b.path(wamr_lib_path));
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
    const is_x86 = target.result.cpu.arch == .x86_64 or target.result.cpu.arch == .x86;
    const libdeflate_flags: []const []const u8 = if (is_x86)
        &.{
            "-O3",
            "-DLIBDEFLATE_ASSEMBLER_DOES_NOT_SUPPORT_AVX512VNNI",
            "-DLIBDEFLATE_ASSEMBLER_DOES_NOT_SUPPORT_AVX_VNNI",
            "-DLIBDEFLATE_ASSEMBLER_DOES_NOT_SUPPORT_VPCLMULQDQ",
        }
    else
        &.{"-O3"};
    const cpu_features_file: []const u8 = if (is_x86) "x86/cpu_features.c" else "arm/cpu_features.c";
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
            cpu_features_file,
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

    const runner_rosetta_step = b.step("runner-rosetta", "Build edgebox-rosetta for Rosetta 2 (Fast JIT on ARM64 Mac)");
    runner_rosetta_step.dependOn(&b.addInstallArtifact(run_x64_exe, .{}).step);

    // ===================
    // edgebox-arm64 - Native ARM64 runner with Fast Interpreter
    // This is the RECOMMENDED runner for Apple Silicon Macs
    // Fast Interpreter uses computed gotos - very efficient on ARM64
    // No Rosetta overhead, instant startup, works great with host functions
    // ===================
    const arm64_target = b.resolveTargetQuery(.{
        .cpu_arch = .aarch64,
        .os_tag = .macos,
    });

    const run_arm64_exe = b.addExecutable(.{
        .name = "edgebox-arm64",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/edgebox_wamr.zig"),
            .target = arm64_target,
            .optimize = .ReleaseFast,
        }),
    });

    // Add WAMR include path
    run_arm64_exe.root_module.addIncludePath(b.path(wamr_dir ++ "/core/iwasm/include"));
    // Static link ARM64 WAMR with Fast Interpreter
    run_arm64_exe.addObjectFile(b.path(wamr_dir ++ "/product-mini/platforms/darwin/build-arm64/libiwasm.a"));
    run_arm64_exe.linkLibC();
    run_arm64_exe.linkSystemLibrary("pthread");
    // No libstdc++ needed - Fast Interpreter doesn't use asmjit

    // Add h2 module for HTTP/2 support
    run_arm64_exe.root_module.addImport("h2", h2_mod);
    run_arm64_exe.root_module.addIncludePath(b.path("vendor/libdeflate"));
    // libdeflate for ARM64 - use ARM NEON features
    run_arm64_exe.root_module.addCSourceFiles(.{
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

    const runner_arm64_step = b.step("runner-arm64", "Build edgebox-arm64 for native ARM64 Mac (Fast Interpreter)");
    runner_arm64_step.dependOn(&b.addInstallArtifact(run_arm64_exe, .{}).step);

    // ===================
    // WAMR AOT compiler libraries (requires LLVM)
    // Build libaotclib.a and libvmlib.a for integrated AOT compilation
    // Uses cmake/make (WAMR's build system) but with Zig-based wrapper for cleaner platform detection
    // ===================
    const cpu_count = std.Thread.getCpuCount() catch 4;
    const is_darwin = target.result.os.tag == .macos;
    const cmake_llvm_flag = if (is_darwin)
        "-DCMAKE_PREFIX_PATH=/opt/homebrew/opt/llvm@18"
    else
        "-DLLVM_DIR=/usr/lib/llvm-18/lib/cmake/llvm";

    const aot_lib_build = b.addSystemCommand(&.{
        "sh", "-c",
        b.fmt(
            \\test -f build/libaotclib.a || (mkdir -p build && cd build && \
            \\cmake .. -DCMAKE_BUILD_TYPE=Release -DWAMR_BUILD_SIMD=1 \
            \\  -DWAMR_BUILD_WITH_CUSTOM_LLVM=1 {s} && \
            \\make -j{d})
        , .{ cmake_llvm_flag, cpu_count }),
    });
    aot_lib_build.setCwd(b.path("vendor/wamr/wamr-compiler"));
    aot_lib_build.setName("build-aot-libs");

    // Copy built libraries to prebuilt dir and save source hash (for future builds)
    // Pure Zig implementation - no shell required
    if (!use_prebuilt and prebuilt_dir != null) {
        const binaryen_lib = if (is_darwin) "libbinaryen.dylib" else "libbinaryen.so";
        const wamr_platform_dir = if (is_darwin) "darwin" else "linux";

        const save_prebuilt_step = SavePrebuiltStep.create(b, prebuilt_dir.?, wamr_platform_dir, binaryen_lib);
        save_prebuilt_step.step.dependOn(&aot_lib_build.step);

        // Save source hash
        const source_dirs = [_][]const u8{
            "vendor/wamr/core",
            "vendor/wamr/wamr-compiler",
            "vendor/binaryen/src",
        };
        build_cache.saveSourceHash(b.allocator, prebuilt_dir.?, &source_dirs) catch |err| {
            std.debug.print("[build] Warning: Could not save source hash: {}\n", .{err});
        };
    }

    // ===================
    // edgeboxc - full CLI for building with integrated AOT compiler
    // Uses WAMR's AOT compiler library (with SIMD support)
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
    build_exe.stack_size = 64 * 1024 * 1024; // 64MB native stack for AOT compilation

    // AOT compiler library dependency (skip if using prebuilt)
    if (!use_prebuilt) {
        build_exe.step.dependOn(&aot_lib_build.step);
    }

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

    // Add QuickJS sources (needed for embedded qjsc)
    build_exe.root_module.addIncludePath(b.path(quickjs_dir));
    build_exe.root_module.addCSourceFiles(.{
        .root = b.path(quickjs_dir),
        .files = &.{
            "quickjs.c",
            "libregexp.c",
            "libunicode.c",
            "cutils.c",
            "quickjs-libc.c",
            "dtoa.c", // Number-to-string conversions
        },
        .flags = quickjs_c_flags,
    });

    // Add qjsc.c with renamed main() to avoid symbol conflict
    const qjsc_flags = &[_][]const u8{
        "-D_GNU_SOURCE",
        "-fno-sanitize=undefined",
        "-Dmain=qjsc_main", // Rename main to qjsc_main
    };
    build_exe.root_module.addCSourceFile(.{
        .file = b.path(quickjs_dir ++ "/qjsc.c"),
        .flags = qjsc_flags,
    });

    // Link WAMR AOT compiler libraries (embedded - no wamrc CLI needed)
    // Note: We use libaotclib.a + libvmlib.a from wamr-compiler, NOT libiwasm.a
    // libvmlib.a includes the runtime needed for module loading
    build_exe.root_module.addIncludePath(b.path(wamr_dir ++ "/core/iwasm/include"));
    build_exe.root_module.addIncludePath(b.path(wamr_dir ++ "/core/shared/utils"));
    build_exe.addObjectFile(b.path(wamr_aot_lib_path));
    build_exe.addObjectFile(b.path(wamr_vm_lib_path));
    build_exe.linkLibC();
    build_exe.linkSystemLibrary("pthread");

    // Link Binaryen for wasm-opt integration (vendored or prebuilt)
    build_exe.root_module.addIncludePath(b.path("vendor/binaryen/src"));
    build_exe.addLibraryPath(b.path(binaryen_lib_path));
    build_exe.addRPath(b.path(binaryen_lib_path));
    build_exe.linkSystemLibrary("binaryen");

    // Link LLVM for AOT compilation
    build_exe.linkLibC();

    if (target.result.os.tag == .macos) {
        build_exe.linkSystemLibrary("c++");
        // Link LLVM from Homebrew
        build_exe.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/opt/llvm@18/lib" });
        build_exe.addRPath(.{ .cwd_relative = "/opt/homebrew/opt/llvm@18/lib" });
        build_exe.linkSystemLibrary("LLVM");
    }

    b.installArtifact(build_exe);

    // ===================
    // NOTE: edgeboxd removed - daemon mode is now built into edgebox
    // Use: edgebox <file.wasm> (auto-starts daemon)
    //      edgebox --binary <file.wasm> (direct execution, no daemon)
    //      edgebox --serve <file.wasm> (run as daemon server)
    // ===================

    // ===================
    // edgebox-gpu-worker - GPU worker process for WebGPU sandboxing
    // Runs wgpu-native in isolated process, communicates via IPC
    // Only built when -Denable-gpu=true
    // ===================
    const gpu_worker_step = b.step("gpu-worker", "Build edgebox-gpu-worker (WebGPU sandbox process)");

    if (enable_gpu) {
        const gpu_worker_exe = b.addExecutable(.{
            .name = "edgebox-gpu-worker",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/gpu_worker.zig"),
                .target = target,
                .optimize = .ReleaseFast,
            }),
        });
        gpu_worker_exe.linkLibC();

        // Depend on wgpu-native download
        gpu_worker_exe.step.dependOn(&download_wgpu.step);

        // Link wgpu-native for WebGPU support
        gpu_worker_exe.root_module.addIncludePath(b.path("vendor/wgpu-native/include/webgpu"));
        gpu_worker_exe.addObjectFile(b.path("vendor/wgpu-native/lib/libwgpu_native.a"));

        // wgpu-native requires system frameworks on macOS
        if (target.result.os.tag == .macos) {
            gpu_worker_exe.linkFramework("Metal");
            gpu_worker_exe.linkFramework("QuartzCore");
            gpu_worker_exe.linkFramework("CoreFoundation");
            gpu_worker_exe.linkFramework("Foundation");
            gpu_worker_exe.linkFramework("IOKit");
            gpu_worker_exe.linkFramework("IOSurface");
            // Objective-C runtime for wgpu-native Metal backend
            gpu_worker_exe.linkSystemLibrary("objc");
        } else if (target.result.os.tag == .linux) {
            // Linux requires Vulkan
            gpu_worker_exe.linkSystemLibrary("vulkan");
        }

        const gpu_install = b.addInstallArtifact(gpu_worker_exe, .{});
        gpu_worker_step.dependOn(&gpu_install.step);
    }

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


    const sandbox_step = b.step("sandbox", "Build edgebox-sandbox (OS-level process sandbox)");
    sandbox_step.dependOn(&b.addInstallArtifact(sandbox_exe, .{}).step);

    // ===================
    // edgebox-wizer - Pure Zig Wizer (WASM pre-initializer) using WAMR
    // Uses WAMR fast-interpreter with SIMD support via SIMDe
    // ===================
    const wizer_exe = b.addExecutable(.{
        .name = "edgebox-wizer",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/wizer_wamr.zig"),
            .target = target,
            .optimize = .ReleaseFast,
        }),
    });

    // Link WAMR for wizer (same build as runtime - fast-interpreter with SIMDe)
    wizer_exe.root_module.addIncludePath(b.path(wamr_dir ++ "/core/iwasm/include"));
    wizer_exe.addObjectFile(b.path(b.fmt("{s}/product-mini/platforms/{s}/build/libiwasm.a", .{ wamr_dir, wamr_platform })));
    wizer_exe.linkLibC();
    wizer_exe.linkSystemLibrary("pthread");


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

    // Binaryen C API (brew install binaryen on macOS, apt install binaryen on Linux)
    // Link vendored Binaryen
    wasm_opt_exe.root_module.addIncludePath(b.path("vendor/binaryen/src"));
    wasm_opt_exe.addLibraryPath(b.path("vendor/binaryen/build/lib"));
    wasm_opt_exe.addRPath(b.path("vendor/binaryen/build/lib"));
    wasm_opt_exe.linkSystemLibrary("binaryen");
    wasm_opt_exe.linkLibCpp();
    wasm_opt_exe.linkLibC();


    const wasm_opt_step = b.step("wasm-opt", "Build edgebox-wasm-opt (pure Zig WASM optimizer)");
    wasm_opt_step.dependOn(&b.addInstallArtifact(wasm_opt_exe, .{}).step);

    // ===================
    // edgebox-aot - AOT compiler tool (WASM to native)
    // ===================
    const aot_tool_exe = b.addExecutable(.{
        .name = "edgebox-aot",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/aot_tool.zig"),
            .target = target,
            .optimize = .ReleaseFast,
        }),
    });

    // Link WAMR AOT compiler
    aot_tool_exe.root_module.addIncludePath(b.path(wamr_dir ++ "/core/iwasm/include"));
    aot_tool_exe.addObjectFile(b.path(b.fmt("{s}/wamr-compiler/build/libaotclib.a", .{wamr_dir})));
    aot_tool_exe.addObjectFile(b.path(b.fmt("{s}/wamr-compiler/build/libvmlib.a", .{wamr_dir})));

    // Link LLVM (for AOT compilation)
    aot_tool_exe.linkLibC();
    if (target.result.os.tag == .macos) {
        aot_tool_exe.linkSystemLibrary("c++");
        aot_tool_exe.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/opt/llvm@18/lib" });
        aot_tool_exe.addRPath(.{ .cwd_relative = "/opt/homebrew/opt/llvm@18/lib" });
        aot_tool_exe.linkSystemLibrary("LLVM");
        aot_tool_exe.linkFramework("Security");
        aot_tool_exe.linkFramework("CoreFoundation");
    } else {
        // Linux
        aot_tool_exe.linkSystemLibrary("LLVM-18");
        aot_tool_exe.linkSystemLibrary("stdc++");
    }

    const aot_tool_step = b.step("aot-tool", "Build edgebox-aot (AOT compiler tool)");
    aot_tool_step.dependOn(&b.addInstallArtifact(aot_tool_exe, .{}).step);

    // ===================
    // Freeze is built-in to edgeboxc (no separate CLI needed)
    // The freeze module (src/freeze/) is used internally by edgeboxc build
    // ===================

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
    // cli - builds all CLI tools
    // ===================
    const cli_step = b.step("cli", "Build all CLI tools (edgebox with daemon mode, edgeboxc with integrated AOT, edgebox-sandbox, edgebox-wasm-opt)");
    cli_step.dependOn(&b.addInstallArtifact(run_exe, .{}).step);
    cli_step.dependOn(&b.addInstallArtifact(build_exe, .{}).step);
    cli_step.dependOn(&b.addInstallArtifact(sandbox_exe, .{}).step);
    // NOTE: wizer not currently used, but kept available via `zig build wizer`
    // Wizer uses prebuilt WAMR (libiwasm.a) so it benefits from caching
    // cli_step.dependOn(&b.addInstallArtifact(wizer_exe, .{}).step);
    cli_step.dependOn(&b.addInstallArtifact(wasm_opt_exe, .{}).step);
    // Include GPU worker when -Denable-gpu=true
    if (enable_gpu) {
        cli_step.dependOn(gpu_worker_step);
    }

    // ===================
    // edgebox-test262 - test262 runner for comparing JS engines
    // ===================
    const test262_exe = b.addExecutable(.{
        .name = "edgebox-test262",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/test262/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const test262_step = b.step("test262", "Build edgebox-test262 runner for comparing JS engines");
    test262_step.dependOn(&b.addInstallArtifact(test262_exe, .{}).step);

    // ===================
    // edgebox-embedded - Single binary with embedded WASM (no file loading)
    // For instant cold start like Bun/Go
    // Usage: zig build embedded -Daot-path=path/to/app.wasm [-Dname=myapp]
    // ===================
    const aot_path_str = b.option([]const u8, "aot-path", "Path to WASM file to embed");
    const embedded_name = b.option([]const u8, "name", "Output binary name (default: derived from aot-path)");

    if (aot_path_str) |aot_path| {
        // Derive name from aot-path if not specified (e.g., "myapp.wasm" -> "myapp")
        const output_name = embedded_name orelse blk: {
            const basename = std.fs.path.basename(aot_path);
            const name_end = std.mem.lastIndexOfScalar(u8, basename, '.') orelse basename.len;
            break :blk basename[0..name_end];
        };

        // Use WriteFile to copy the WASM and generate a module that embeds it
        const write_files = b.addWriteFiles();

        // Copy WASM file to build cache with known name
        const aot_copy = write_files.addCopyFile(.{ .cwd_relative = aot_path }, "aot_module.bin");

        // Generate a Zig module that @embedFile's the copied WASM
        const aot_data_zig = write_files.add("aot_data.zig",
            \\pub const data = @embedFile("aot_module.bin");
            \\
        );

        // Create the aot_data module from generated source
        const aot_data_module = b.createModule(.{
            .root_source_file = aot_data_zig,
        });

        // The generated module needs to resolve the embedFile relative to itself
        // So we need to add the directory containing aot_module.bin
        _ = aot_copy; // Used implicitly by the generated Zig file

        const embedded_exe = b.addExecutable(.{
            .name = output_name,
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/edgebox_embedded.zig"),
                .target = target,
                .optimize = .ReleaseFast,
                .strip = true,
                .imports = &.{
                    .{ .name = "aot_data", .module = aot_data_module },
                },
            }),
        });

        // Link WAMR
        embedded_exe.root_module.addIncludePath(b.path(wamr_dir ++ "/core/iwasm/include"));
        embedded_exe.addObjectFile(b.path(b.fmt("{s}/product-mini/platforms/{s}/build/libiwasm.a", .{ wamr_dir, wamr_platform })));
        embedded_exe.linkLibC();
        embedded_exe.linkSystemLibrary("pthread");
        if (target.result.os.tag == .macos) {
            embedded_exe.linkFramework("Security");
            embedded_exe.linkFramework("CoreFoundation");
        }

        const embedded_step = b.step("embedded", "Build single binary with embedded AOT");
        embedded_step.dependOn(&b.addInstallArtifact(embedded_exe, .{}).step);

        // ===================
        // embedded-daemon - Daemon with embedded WASM (no file loading)
        // ===================
        const daemon_name = if (embedded_name) |name|
            b.fmt("{s}-daemon", .{name})
        else blk: {
            const basename = std.fs.path.basename(aot_path);
            const name_end = std.mem.lastIndexOfScalar(u8, basename, '.') orelse basename.len;
            break :blk b.fmt("{s}-daemon", .{basename[0..name_end]});
        };

        // Build options for daemon - enable embedded mode
        const daemon_build_options = b.addOptions();
        daemon_build_options.addOption(bool, "embedded_mode", true);

        const embedded_daemon_exe = b.addExecutable(.{
            .name = daemon_name,
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/edgeboxd_wamr.zig"),
                .target = target,
                .optimize = .ReleaseFast,
                .strip = true,
                .imports = &.{
                    .{ .name = "aot_data", .module = aot_data_module },
                },
            }),
        });
        embedded_daemon_exe.root_module.addOptions("build_options", daemon_build_options);

        // Link WAMR
        embedded_daemon_exe.root_module.addIncludePath(b.path(wamr_dir ++ "/core/iwasm/include"));
        embedded_daemon_exe.addObjectFile(b.path(b.fmt("{s}/product-mini/platforms/{s}/build/libiwasm.a", .{ wamr_dir, wamr_platform })));
        embedded_daemon_exe.linkLibC();
        embedded_daemon_exe.linkSystemLibrary("pthread");
        if (target.result.os.tag == .macos) {
            embedded_daemon_exe.linkFramework("Security");
            embedded_daemon_exe.linkFramework("CoreFoundation");
        }

        const embedded_daemon_step = b.step("embedded-daemon", "Build daemon with embedded AOT");
        embedded_daemon_step.dependOn(&b.addInstallArtifact(embedded_daemon_exe, .{}).step);
    } else {
        // Create a dummy step that errors if no AOT path provided
        const embedded_step = b.step("embedded", "Build single binary with embedded AOT (requires -Daot-path=...)");
        const fail_step = b.addFail("embedded target requires -Daot-path=<path/to/app.aot>");
        embedded_step.dependOn(&fail_step.step);

        const embedded_daemon_step = b.step("embedded-daemon", "Build daemon with embedded AOT (requires -Daot-path=...)");
        const fail_daemon_step = b.addFail("embedded-daemon target requires -Daot-path=<path/to/app.aot>");
        embedded_daemon_step.dependOn(&fail_daemon_step.step);
    }

    // Make cli step also verify frozen functions codegen (catches errors early)
    // Build a test frozen function if bench/ exists - this is what CI runs
    // Use -Dskip-frozen-test=true for faster iteration during development
    if (!skip_frozen_test and source_dir.len > 0) {
        cli_step.dependOn(wasm_static_step);
    }
}
