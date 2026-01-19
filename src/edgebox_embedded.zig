/// EdgeBox Embedded Runtime
/// Single binary with AOT code embedded - no file loading needed
/// Build: zig build wasi -Daot-path=path/to/app.aot
///
/// The WASM module uses WASI syscalls for I/O, which WAMR provides built-in.
/// No custom native functions needed - same polyfills work via WASI.
const std = @import("std");
const c = @cImport({
    @cInclude("wasm_export.h");
});

// AOT module data embedded at compile time via generated module
const aot_data = @import("aot_data").data;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Initialize WAMR with system allocator
    var init_args: c.RuntimeInitArgs = std.mem.zeroes(c.RuntimeInitArgs);
    init_args.mem_alloc_type = c.Alloc_With_System_Allocator;

    if (!c.wasm_runtime_full_init(&init_args)) {
        std.debug.print("Failed to initialize WAMR\n", .{});
        return;
    }
    defer c.wasm_runtime_destroy();

    // Copy embedded data to heap-allocated buffer (WAMR may modify it during parsing)
    const wasm_buf = std.heap.page_allocator.alloc(u8, aot_data.len) catch {
        std.debug.print("Failed to allocate buffer for module\n", .{});
        return;
    };
    defer std.heap.page_allocator.free(wasm_buf);
    @memcpy(wasm_buf, aot_data);

    // Load module
    var error_buf: [256]u8 = std.mem.zeroes([256]u8);
    const module = c.wasm_runtime_load(
        wasm_buf.ptr,
        @intCast(wasm_buf.len),
        &error_buf,
        error_buf.len,
    );
    if (module == null) {
        const err_str = std.mem.sliceTo(&error_buf, 0);
        std.debug.print("Failed to load module: {s}\n", .{err_str});
        return;
    }
    defer c.wasm_runtime_unload(module);

    // Set WASI args before instantiation
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var c_args = try allocator.alloc([*:0]const u8, args.len);
    defer allocator.free(c_args);
    for (args, 0..) |arg, i| {
        c_args[i] = @ptrCast(arg.ptr);
    }

    c.wasm_runtime_set_wasi_args(
        module,
        null, 0, // No preopened dirs
        null, 0, // No mapped dirs
        null, 0, // No env vars (inherit from process)
        @ptrCast(c_args.ptr), @intCast(c_args.len),
    );

    // Instantiate
    const stack_size: u32 = 2 * 1024 * 1024; // 2MB
    const heap_size: u32 = 16 * 1024 * 1024; // 16MB

    const module_inst = c.wasm_runtime_instantiate(
        module,
        stack_size,
        heap_size,
        &error_buf,
        error_buf.len,
    );
    if (module_inst == null) {
        std.debug.print("Failed to instantiate: {s}\n", .{&error_buf});
        return;
    }
    defer c.wasm_runtime_deinstantiate(module_inst);

    // Create execution environment
    const exec_env = c.wasm_runtime_create_exec_env(module_inst, stack_size);
    if (exec_env == null) {
        std.debug.print("Failed to create exec env\n", .{});
        return;
    }
    defer c.wasm_runtime_destroy_exec_env(exec_env);

    // Run _start (WASI entry point)
    const start_func = c.wasm_runtime_lookup_function(module_inst, "_start");
    if (start_func != null) {
        _ = c.wasm_runtime_call_wasm(exec_env, start_func, 0, null);
    }
}
