const std = @import("std");
const c = @cImport({
    @cInclude("wasm_export.h");
    @cInclude("aot_export.h");
    @cInclude("aot_comp_option.h");
});

/// Compile a WASM file to AOT format using WAMR's built-in AOT compiler
pub fn compileWasmToAot(
    allocator: std.mem.Allocator,
    wasm_path: []const u8,
    aot_path: []const u8,
    enable_simd: bool,
) !void {
    // Read WASM file
    const wasm_file = try std.fs.cwd().readFileAlloc(allocator, wasm_path, 100 * 1024 * 1024); // 100MB max
    defer allocator.free(wasm_file);

    // Initialize WAMR runtime
    if (!c.wasm_runtime_init()) {
        return error.WasmRuntimeInitFailed;
    }
    defer c.wasm_runtime_destroy();

    // Initialize AOT compiler
    if (!c.aot_compiler_init()) {
        return error.AotCompilerInitFailed;
    }
    defer c.aot_compiler_destroy();

    // Load WASM module
    var error_buf: [256]u8 = undefined;
    const wasm_module = c.wasm_runtime_load(
        wasm_file.ptr,
        @intCast(wasm_file.len),
        &error_buf,
        error_buf.len,
    ) orelse {
        std.debug.print("[aot] Failed to load WASM: {s}\n", .{error_buf});
        return error.WasmLoadFailed;
    };
    defer c.wasm_runtime_unload(wasm_module);

    // Get target arch (auto-detect)
    const target_arch: [*c]const u8 = switch (@import("builtin").cpu.arch) {
        .x86_64 => "x86_64",
        .aarch64 => "aarch64",
        else => null,
    };
    if (target_arch == null) {
        return error.UnsupportedArchitecture;
    }

    // Create compilation data
    const comp_data = c.aot_create_comp_data(wasm_module, target_arch, false) orelse {
        const err = c.aot_get_last_error();
        std.debug.print("[aot] Failed to create comp data: {s}\n", .{err});
        return error.CompDataCreationFailed;
    };
    defer c.aot_destroy_comp_data(comp_data);

    // Set up compilation options
    var option: c.AOTCompOption = std.mem.zeroes(c.AOTCompOption);
    option.opt_level = 3; // O3 optimization
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
    const comp_ctx = c.aot_create_comp_context(comp_data, &option) orelse {
        const err = c.aot_get_last_error();
        std.debug.print("[aot] Failed to create comp context: {s}\n", .{err});
        return error.CompContextCreationFailed;
    };
    defer c.aot_destroy_comp_context(comp_ctx);

    // Compile WASM to AOT
    if (!c.aot_compile_wasm(comp_ctx)) {
        const err = c.aot_get_last_error();
        std.debug.print("[aot] Compilation failed: {s}\n", .{err});
        return error.CompilationFailed;
    }

    // Emit AOT file
    const aot_path_z = try allocator.dupeZ(u8, aot_path);
    defer allocator.free(aot_path_z);

    if (!c.aot_emit_aot_file(comp_ctx, comp_data, aot_path_z.ptr)) {
        const err = c.aot_get_last_error();
        std.debug.print("[aot] Failed to emit AOT file: {s}\n", .{err});
        return error.EmitAotFileFailed;
    }
}
