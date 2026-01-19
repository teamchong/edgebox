const std = @import("std");
const build_cache = @import("build_cache.zig");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // ===================
    // Build options
    // ===================
    const frozen_optimize = b.option(
        std.builtin.OptimizeMode,
        "frozen-optimize",
        "Optimization for frozen modules: ReleaseSafe (default) or ReleaseFast",
    ) orelse .ReleaseSafe;

    const AllocatorType = enum { c, arena, gpa };
    const allocator_type = b.option(AllocatorType, "allocator", "Allocator type: c (default), arena, gpa") orelse .c;

    const source_dir_raw = b.option([]const u8, "source-dir", "Source directory for build artifacts") orelse "";
    const bytecode_path = b.option([]const u8, "bytecode", "Path to bytecode file to embed");
    const runtime_output = b.option([]const u8, "output", "Output binary name");

    // Validate source_dir
    const source_dir = blk: {
        if (source_dir_raw.len == 0) break :blk source_dir_raw;
        if (std.mem.indexOf(u8, source_dir_raw, "..") != null) break :blk "";
        if (source_dir_raw[0] == '/' or source_dir_raw[0] == '\\') break :blk "";
        break :blk source_dir_raw;
    };

    // Prebuilt library detection
    const prebuilt_dir: ?[]const u8 = blk: {
        const os_tag = target.result.os.tag;
        const cpu_arch = target.result.cpu.arch;
        if (os_tag == .macos and cpu_arch == .aarch64) break :blk "vendor/prebuilt/darwin-arm64";
        if (os_tag == .macos and cpu_arch == .x86_64) break :blk "vendor/prebuilt/darwin-x64";
        if (os_tag == .linux and cpu_arch == .aarch64) break :blk "vendor/prebuilt/linux-arm64";
        if (os_tag == .linux and cpu_arch == .x86_64) break :blk "vendor/prebuilt/linux-x64";
        break :blk null;
    };

    const use_prebuilt = if (prebuilt_dir) |dir| blk: {
        const source_dirs = [_][]const u8{ "vendor/wamr/core", "vendor/wamr/wamr-compiler" };
        const should_use = build_cache.shouldUsePrebuilt(b.allocator, dir, &source_dirs) catch false;
        if (should_use) std.debug.print("[build] Using prebuilt libraries\n", .{});
        break :blk should_use;
    } else false;

    // ===================
    // QuickJS setup (shared)
    // ===================
    const quickjs_dir = "vendor/quickjs-ng";

    const apply_patches = b.addSystemCommand(&.{
        "sh", "-c",
        "test -f vendor/quickjs-ng/quickjs.c || git submodule update --init --recursive; " ++
            "if [ ! -f vendor/quickjs-ng/.patches-applied ]; then " ++
            "cd vendor/quickjs-ng && git checkout . 2>/dev/null; " ++
            "for p in ../../patches/*.patch; do test -f \"$p\" && patch -p1 --silent < \"$p\"; done && " ++
            "touch .patches-applied; fi",
    });
    apply_patches.setName("apply-quickjs-patches");

    const quickjs_c_files = &[_][]const u8{
        "quickjs.c", "libregexp.c", "libunicode.c", "cutils.c", "quickjs-libc.c", "dtoa.c",
    };
    const quickjs_c_flags = &[_][]const u8{
        "-D_GNU_SOURCE", "-fno-sanitize=undefined", "-fno-vectorize", "-fno-slp-vectorize", "-fno-unroll-loops",
    };
    const quickjs_wasm_flags = &[_][]const u8{
        "-D_GNU_SOURCE", "-fno-sanitize=undefined", "-fno-vectorize", "-fno-slp-vectorize", "-fno-unroll-loops",
    };

    // Shared paths
    const frozen_zig_path = if (source_dir.len > 0)
        b.fmt("zig-out/cache/{s}/frozen_module.zig", .{source_dir})
    else
        "zig-out/cache/frozen_module.zig";
    const zig_hotpaths_path = if (source_dir.len > 0)
        b.fmt("zig-out/cache/{s}/zig_hotpaths.zig", .{source_dir})
    else
        "zig-out/cache/zig_hotpaths.zig";

    // Allocator config content (shared)
    const allocator_config_content = switch (allocator_type) {
        .c => "pub const allocator_type: enum { c, arena, gpa } = .c;\npub const qjs_arena: bool = false;\n",
        .arena => "pub const allocator_type: enum { c, arena, gpa } = .arena;\npub const qjs_arena: bool = true;\n",
        .gpa => "pub const allocator_type: enum { c, arena, gpa } = .gpa;\npub const qjs_arena: bool = false;\n",
    };

    // ===================
    // native - Single code path for native binary AND WASM
    // Usage: zig build native -Dbytecode=path/to/bundle.bin
    // Produces: edgebox-native (binary) + edgebox-native.wasm
    // ===================
    if (bytecode_path) |bc_path| {
        // Shared write files for bytecode embedding
        const write_files = b.addWriteFiles();
        const bc_copy = write_files.addCopyFile(.{ .cwd_relative = bc_path }, "embedded_bytecode.bin");
        _ = bc_copy;

        const bytecode_zig = write_files.add("bytecode_embed.zig",
            \\pub const data = @embedFile("embedded_bytecode.bin");
            \\
        );
        const bytecode_mod = b.createModule(.{ .root_source_file = bytecode_zig });

        const allocator_zig = write_files.add("allocator_config.zig", allocator_config_content);
        const allocator_mod = b.createModule(.{ .root_source_file = allocator_zig });

        // --- Native Binary ---
        const native_exe = b.addExecutable(.{
            .name = runtime_output orelse "edgebox-native",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/native_main_embed.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        native_exe.stack_size = 64 * 1024 * 1024;
        if (optimize != .Debug) {
            native_exe.use_llvm = true;
            native_exe.want_lto = false;
        }

        // Frozen modules for native
        const native_zig_runtime = b.createModule(.{
            .root_source_file = b.path("src/freeze/zig_runtime.zig"),
            .target = target,
            .optimize = frozen_optimize,
        });
        native_zig_runtime.addIncludePath(b.path(quickjs_dir));

        const native_math_polyfill = b.createModule(.{
            .root_source_file = b.path("src/polyfills/math.zig"),
            .target = target,
            .optimize = frozen_optimize,
        });
        native_math_polyfill.addIncludePath(b.path(quickjs_dir));

        const native_dispatch = b.createModule(.{
            .root_source_file = b.path("src/freeze/native_dispatch.zig"),
            .target = target,
            .optimize = frozen_optimize,
        });
        native_dispatch.addImport("zig_runtime", native_zig_runtime);

        const native_frozen = b.createModule(.{
            .root_source_file = .{ .cwd_relative = frozen_zig_path },
            .target = target,
            .optimize = frozen_optimize,
        });
        native_frozen.addImport("zig_runtime", native_zig_runtime);
        native_frozen.addImport("math_polyfill", native_math_polyfill);
        native_frozen.addImport("native_dispatch", native_dispatch);

        native_exe.root_module.addImport("bytecode", bytecode_mod);
        native_exe.root_module.addImport("allocator_config", allocator_mod);
        native_exe.root_module.addImport("frozen_module", native_frozen);
        native_exe.root_module.addImport("zig_runtime", native_zig_runtime);
        native_exe.root_module.addImport("math_polyfill", native_math_polyfill);
        native_exe.root_module.addImport("native_dispatch", native_dispatch);
        native_exe.root_module.addIncludePath(b.path(quickjs_dir));

        // Zig hotpaths
        if (std.fs.cwd().access(zig_hotpaths_path, .{})) |_| {
            native_exe.root_module.addAnonymousImport("zig_hotpaths", .{
                .root_source_file = .{ .cwd_relative = zig_hotpaths_path },
            });
        } else |_| {
            native_exe.root_module.addAnonymousImport("zig_hotpaths", .{
                .root_source_file = b.path("src/freeze/zig_hotpaths_stub.zig"),
            });
        }

        // QuickJS + libdeflate for native
        native_exe.root_module.addCSourceFiles(.{
            .root = b.path(quickjs_dir),
            .files = quickjs_c_files,
            .flags = quickjs_c_flags,
        });

        native_exe.root_module.addIncludePath(b.path("vendor/libdeflate"));
        const is_x86 = target.result.cpu.arch == .x86_64 or target.result.cpu.arch == .x86;
        const libdeflate_flags: []const []const u8 = if (is_x86)
            &.{ "-O3", "-DLIBDEFLATE_ASSEMBLER_DOES_NOT_SUPPORT_AVX512VNNI", "-DLIBDEFLATE_ASSEMBLER_DOES_NOT_SUPPORT_AVX_VNNI", "-DLIBDEFLATE_ASSEMBLER_DOES_NOT_SUPPORT_VPCLMULQDQ" }
        else
            &.{"-O3"};
        native_exe.root_module.addCSourceFiles(.{
            .root = b.path("vendor/libdeflate/lib"),
            .files = &.{
                "deflate_compress.c",  "deflate_decompress.c", "gzip_compress.c",
                "gzip_decompress.c",   "zlib_compress.c",      "zlib_decompress.c",
                "adler32.c",           "crc32.c",              "utils.c",
                if (is_x86) "x86/cpu_features.c" else "arm/cpu_features.c",
            },
            .flags = libdeflate_flags,
        });

        native_exe.linkLibC();
        native_exe.step.dependOn(&apply_patches.step);

        // --- WASM (same code, WASI target for full Node.js support) ---
        const wasm_target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .wasi,
            .cpu_features_add = std.Target.wasm.featureSet(&.{ .bulk_memory, .sign_ext, .simd128 }),
        });

        const wasm_name = if (runtime_output) |name| b.fmt("{s}.wasm", .{name}) else "edgebox-native.wasm";
        const wasm_exe = b.addExecutable(.{
            .name = wasm_name,
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/native_main_embed.zig"),
                .target = wasm_target,
                .optimize = if (optimize == .Debug) .ReleaseFast else optimize,
                .strip = true,
            }),
        });
        wasm_exe.initial_memory = 64 * 1024 * 1024;
        wasm_exe.max_memory = 4 * 1024 * 1024 * 1024;
        wasm_exe.stack_size = 16 * 1024 * 1024;
        wasm_exe.rdynamic = true;

        // Frozen modules for WASM
        const wasm_zig_runtime = b.createModule(.{
            .root_source_file = b.path("src/freeze/zig_runtime.zig"),
            .target = wasm_target,
            .optimize = frozen_optimize,
        });
        wasm_zig_runtime.addIncludePath(b.path(quickjs_dir));

        const wasm_math_polyfill = b.createModule(.{
            .root_source_file = b.path("src/polyfills/math.zig"),
            .target = wasm_target,
            .optimize = frozen_optimize,
        });
        wasm_math_polyfill.addIncludePath(b.path(quickjs_dir));

        const wasm_dispatch = b.createModule(.{
            .root_source_file = b.path("src/freeze/native_dispatch.zig"),
            .target = wasm_target,
            .optimize = frozen_optimize,
        });
        wasm_dispatch.addImport("zig_runtime", wasm_zig_runtime);

        const wasm_frozen = b.createModule(.{
            .root_source_file = .{ .cwd_relative = frozen_zig_path },
            .target = wasm_target,
            .optimize = frozen_optimize,
        });
        wasm_frozen.addImport("zig_runtime", wasm_zig_runtime);
        wasm_frozen.addImport("math_polyfill", wasm_math_polyfill);
        wasm_frozen.addImport("native_dispatch", wasm_dispatch);

        wasm_exe.root_module.addImport("bytecode", bytecode_mod);
        wasm_exe.root_module.addImport("allocator_config", allocator_mod);
        wasm_exe.root_module.addImport("frozen_module", wasm_frozen);
        wasm_exe.root_module.addImport("zig_runtime", wasm_zig_runtime);
        wasm_exe.root_module.addImport("math_polyfill", wasm_math_polyfill);
        wasm_exe.root_module.addImport("native_dispatch", wasm_dispatch);
        wasm_exe.root_module.addIncludePath(b.path(quickjs_dir));

        // Zig hotpaths for WASM
        if (std.fs.cwd().access(zig_hotpaths_path, .{})) |_| {
            wasm_exe.root_module.addAnonymousImport("zig_hotpaths", .{
                .root_source_file = .{ .cwd_relative = zig_hotpaths_path },
            });
        } else |_| {
            wasm_exe.root_module.addAnonymousImport("zig_hotpaths", .{
                .root_source_file = b.path("src/freeze/zig_hotpaths_stub.zig"),
            });
        }

        // QuickJS for WASM (no libdeflate - compression via host)
        wasm_exe.root_module.addCSourceFiles(.{
            .root = b.path(quickjs_dir),
            .files = quickjs_c_files,
            .flags = quickjs_wasm_flags,
        });
        wasm_exe.root_module.addCSourceFiles(.{
            .root = b.path("src/freeze"),
            .files = &.{"frozen_dispatch_cli.c"},
            .flags = quickjs_wasm_flags,
        });

        wasm_exe.linkLibC();
        wasm_exe.step.dependOn(&apply_patches.step);

        // Install both
        const native_install = b.addInstallArtifact(native_exe, .{});
        const wasm_install = b.addInstallArtifact(wasm_exe, .{});

        const native_step = b.step("native", "Build native binary + WASM (same code path)");
        native_step.dependOn(&native_install.step);
        native_step.dependOn(&wasm_install.step);

        // native-embed: Only builds the native binary (no WASM)
        const native_embed_step = b.step("native-embed", "Build native binary only (no WASM)");
        native_embed_step.dependOn(&native_install.step);
    } else {
        const native_step = b.step("native", "Build native binary + WASM (requires -Dbytecode=...)");
        const fail = b.addFail("native requires -Dbytecode=<path/to/bytecode.bin>");
        native_step.dependOn(&fail.step);

        const native_embed_step = b.step("native-embed", "Build native binary only (requires -Dbytecode=...)");
        const fail2 = b.addFail("native-embed requires -Dbytecode=<path/to/bytecode.bin>");
        native_embed_step.dependOn(&fail2.step);
    }

    // ===================
    // test - Unit tests
    // ===================
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
        .files = quickjs_c_files,
        .flags = &.{ "-D_GNU_SOURCE", "-fno-sanitize=undefined" },
    });
    unit_tests.root_module.addCSourceFile(.{
        .file = b.path("src/freeze/frozen_dispatch_cli.c"),
        .flags = &.{"-D_GNU_SOURCE"},
    });
    unit_tests.linkLibC();
    unit_tests.step.dependOn(&apply_patches.step);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&b.addRunArtifact(unit_tests).step);

    // ===================
    // Default
    // ===================
    b.default_step = b.step("help", "Show available targets");
}
