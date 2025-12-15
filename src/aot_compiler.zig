const std = @import("std");
const c = @cImport({
    @cInclude("wasm_export.h");
    @cInclude("aot_export.h");
    @cInclude("aot_comp_option.h");
});

// Note: std.debug.print writes to stderr unbuffered, so we just use it directly
// The detailed logging helps debug crashes in AOT compilation

/// Compile a WASM file to AOT format using WAMR's built-in AOT compiler
pub fn compileWasmToAot(
    allocator: std.mem.Allocator,
    wasm_path: []const u8,
    aot_path: []const u8,
    enable_simd: bool,
) !void {
    std.debug.print("[aot] Starting AOT compilation: {s} -> {s}\n", .{ wasm_path, aot_path });
    std.debug.print("[aot] SIMD enabled: {}\n", .{enable_simd});

    // Read WASM file
    std.debug.print("[aot] Reading WASM file...\n", .{});
    const wasm_file = std.fs.cwd().readFileAlloc(allocator, wasm_path, 100 * 1024 * 1024) catch |err| {
        std.debug.print("[aot] Failed to read WASM file: {}\n", .{err});
        return err;
    };
    defer allocator.free(wasm_file);
    std.debug.print("[aot] Read {d} bytes from WASM file\n", .{wasm_file.len});

    // Initialize WAMR runtime
    std.debug.print("[aot] Initializing WAMR runtime...\n", .{});
    if (!c.wasm_runtime_init()) {
        std.debug.print("[aot] WAMR runtime init failed!\n", .{});
        return error.WasmRuntimeInitFailed;
    }
    defer c.wasm_runtime_destroy();
    std.debug.print("[aot] WAMR runtime initialized\n", .{});

    // Initialize AOT compiler
    std.debug.print("[aot] Initializing AOT compiler (LLVM backend)...\n", .{});
    if (!c.aot_compiler_init()) {
        std.debug.print("[aot] AOT compiler init failed! Check LLVM installation.\n", .{});
        return error.AotCompilerInitFailed;
    }
    defer c.aot_compiler_destroy();
    std.debug.print("[aot] AOT compiler initialized\n", .{});

    // Load WASM module
    std.debug.print("[aot] Loading WASM module...\n", .{});
    var error_buf: [256]u8 = undefined;
    const wasm_module = c.wasm_runtime_load(
        wasm_file.ptr,
        @intCast(wasm_file.len),
        &error_buf,
        error_buf.len,
    ) orelse {
        std.debug.print("[aot] Failed to load WASM: {s}\n", .{&error_buf});
        return error.WasmLoadFailed;
    };
    defer c.wasm_runtime_unload(wasm_module);
    std.debug.print("[aot] WASM module loaded\n", .{});

    // Get target arch (auto-detect)
    const target_arch: [*c]const u8 = switch (@import("builtin").cpu.arch) {
        .x86_64 => "x86_64",
        .aarch64 => "aarch64",
        else => null,
    };
    if (target_arch == null) {
        std.debug.print("[aot] Unsupported architecture!\n", .{});
        return error.UnsupportedArchitecture;
    }
    std.debug.print("[aot] Target architecture: {s}\n", .{target_arch});

    // Create compilation data
    std.debug.print("[aot] Creating compilation data...\n", .{});
    const comp_data = c.aot_create_comp_data(wasm_module, target_arch, false) orelse {
        const err = c.aot_get_last_error();
        std.debug.print("[aot] Failed to create comp data: {s}\n", .{err});
        return error.CompDataCreationFailed;
    };
    defer c.aot_destroy_comp_data(comp_data);
    std.debug.print("[aot] Compilation data created\n", .{});

    // Set up compilation options
    // Use O2 instead of O3 to avoid potential LLVM crashes with aggressive optimization
    std.debug.print("[aot] Setting up compilation options (opt_level=2, simd={})...\n", .{enable_simd});
    var option: c.AOTCompOption = std.mem.zeroes(c.AOTCompOption);
    option.opt_level = 2; // O2 optimization (O3 can crash on some WASM patterns)
    option.size_level = 0; // Prioritize speed
    option.output_format = c.AOT_FORMAT_FILE;
    option.enable_simd = enable_simd;
    option.enable_bulk_memory = true;
    option.enable_bulk_memory_opt = true;
    option.enable_ref_types = true;
    option.enable_aux_stack_check = true;
    option.target_arch = @constCast(target_arch);
    option.target_abi = null;
    option.target_cpu = null;
    option.cpu_features = null;
    option.bounds_checks = 1;

    // Create compilation context
    std.debug.print("[aot] Creating compilation context (LLVM)...\n", .{});
    const comp_ctx = c.aot_create_comp_context(comp_data, &option) orelse {
        const err = c.aot_get_last_error();
        std.debug.print("[aot] Failed to create comp context: {s}\n", .{err});
        return error.CompContextCreationFailed;
    };
    defer c.aot_destroy_comp_context(comp_ctx);
    std.debug.print("[aot] Compilation context created\n", .{});

    // Compile WASM to AOT
    std.debug.print("[aot] Compiling WASM to native code (this may take a while)...\n", .{});
    if (!c.aot_compile_wasm(comp_ctx)) {
        const err = c.aot_get_last_error();
        std.debug.print("[aot] Compilation failed: {s}\n", .{err});
        return error.CompilationFailed;
    }
    std.debug.print("[aot] Compilation successful\n", .{});

    // Emit AOT file
    std.debug.print("[aot] Emitting AOT file: {s}\n", .{aot_path});
    const aot_path_z = try allocator.dupeZ(u8, aot_path);
    defer allocator.free(aot_path_z);

    if (!c.aot_emit_aot_file(comp_ctx, comp_data, aot_path_z.ptr)) {
        const err = c.aot_get_last_error();
        std.debug.print("[aot] Failed to emit AOT file: {s}\n", .{err});
        return error.EmitAotFileFailed;
    }
    std.debug.print("[aot] AOT file emitted successfully\n", .{});
}
