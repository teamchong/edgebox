const std = @import("std");

// WAMR AOT compiler C API bindings
const c = @cImport({
    @cInclude("wasm_export.h");
    @cInclude("aot_export.h");
});

/// Compile a WASM file to AOT format using WAMR's embedded AOT compiler
/// No external CLI needed - LLVM is linked into edgeboxc
pub fn compileWasmToAot(
    allocator: std.mem.Allocator,
    wasm_path: []const u8,
    aot_path: []const u8,
    enable_simd: bool,
) !void {
    std.debug.print("[aot] Starting AOT compilation: {s} -> {s}\n", .{ wasm_path, aot_path });
    std.debug.print("[aot] SIMD enabled: {}\n", .{enable_simd});

    // Read WASM file
    const wasm_file = std.fs.cwd().openFile(wasm_path, .{}) catch |err| {
        std.debug.print("[aot] Failed to open WASM file: {}\n", .{err});
        return error.FileOpenFailed;
    };
    defer wasm_file.close();

    const wasm_data = wasm_file.readToEndAlloc(allocator, 100 * 1024 * 1024) catch |err| {
        std.debug.print("[aot] Failed to read WASM file: {}\n", .{err});
        return error.FileReadFailed;
    };
    defer allocator.free(wasm_data);

    std.debug.print("[aot] WASM size: {} bytes\n", .{wasm_data.len});

    // Initialize WAMR runtime (needed for module loading)
    if (!c.wasm_runtime_init()) {
        std.debug.print("[aot] Failed to init WAMR runtime\n", .{});
        return error.RuntimeInitFailed;
    }
    defer c.wasm_runtime_destroy();

    // Initialize AOT compiler (LLVM)
    std.debug.print("[aot] Initializing AOT compiler...\n", .{});
    if (!c.aot_compiler_init()) {
        const err_msg = c.aot_get_last_error();
        if (err_msg != null) {
            std.debug.print("[aot] Failed to init AOT compiler: {s}\n", .{err_msg});
        } else {
            std.debug.print("[aot] Failed to init AOT compiler\n", .{});
        }
        return error.CompilerInitFailed;
    }
    defer c.aot_compiler_destroy();

    // Load WASM module
    var error_buf: [256]u8 = undefined;
    const wasm_module = c.wasm_runtime_load(wasm_data.ptr, @intCast(wasm_data.len), &error_buf, error_buf.len);
    if (wasm_module == null) {
        std.debug.print("[aot] Failed to load WASM module: {s}\n", .{&error_buf});
        return error.ModuleLoadFailed;
    }
    defer c.wasm_runtime_unload(wasm_module);

    // Create compilation data
    const comp_data = c.aot_create_comp_data(wasm_module, null, false);
    if (comp_data == null) {
        const err_msg = c.aot_get_last_error();
        if (err_msg != null) {
            std.debug.print("[aot] Failed to create comp data: {s}\n", .{err_msg});
        }
        return error.CompDataFailed;
    }
    defer c.aot_destroy_comp_data(comp_data);

    // Set compilation options
    var option = std.mem.zeroes(c.AOTCompOption);
    option.opt_level = 3;
    option.size_level = 3; // Small code model (required for AArch64)
    option.output_format = c.AOT_FORMAT_FILE;
    option.enable_simd = enable_simd;
    option.enable_bulk_memory = true;
    option.enable_ref_types = true;
    option.bounds_checks = 0; // Disable for performance (sandbox provides safety)
    option.stack_bounds_checks = 0;

    // Create compilation context
    const comp_ctx = c.aot_create_comp_context(comp_data, &option);
    if (comp_ctx == null) {
        const err_msg = c.aot_get_last_error();
        if (err_msg != null) {
            std.debug.print("[aot] Failed to create comp context: {s}\n", .{err_msg});
        }
        return error.CompContextFailed;
    }
    defer c.aot_destroy_comp_context(comp_ctx);

    // Compile WASM to native code
    std.debug.print("[aot] Compiling WASM to native code...\n", .{});
    if (!c.aot_compile_wasm(comp_ctx)) {
        const err_msg = c.aot_get_last_error();
        if (err_msg != null) {
            std.debug.print("[aot] Compilation failed: {s}\n", .{err_msg});
        }
        return error.CompilationFailed;
    }

    // Emit AOT file
    // Need null-terminated path for C API
    var path_buf: [4096]u8 = undefined;
    const aot_path_z = std.fmt.bufPrintZ(&path_buf, "{s}", .{aot_path}) catch {
        return error.PathTooLong;
    };

    std.debug.print("[aot] Emitting AOT file...\n", .{});
    if (!c.aot_emit_aot_file(comp_ctx, comp_data, aot_path_z.ptr)) {
        const err_msg = c.aot_get_last_error();
        if (err_msg != null) {
            std.debug.print("[aot] Failed to emit AOT file: {s}\n", .{err_msg});
        }
        return error.EmitFailed;
    }

    std.debug.print("[aot] AOT compilation successful\n", .{});
}
