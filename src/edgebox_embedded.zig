/// EdgeBox Embedded Runtime
/// Single binary with AOT code embedded - no file loading needed
/// Build: zig build embedded -Daot-path=path/to/app.aot
const std = @import("std");
const c = @cImport({
    @cInclude("wasm_export.h");
});

// AOT module data embedded at compile time via generated module
const aot_data = @import("aot_data").data;

// Stub implementations for edgebox_* imports (return "not implemented")
// WASM type definitions:
// type 39 (http): (i32 x9) -> i32
// type 36 (spawn/file/stdlib): (i32 x5) -> i32
// type 0 (zlib): (i32 x3) -> i32
// type 40 (crypto): (i32 x7) -> i32
// type 26 (socket): (i32 x4) -> i32

fn stubHttp(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32) i32 {
    return -1;
}

fn stubType36(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32, _: i32, _: i32) i32 {
    return -1;
}

fn stubZlib(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32) i32 {
    return -1;
}

fn stubCrypto7(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32) i32 {
    return -1;
}

fn stubCrypto8(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32, _: i32) i32 {
    return -1;
}

fn stubSocket(_: c.wasm_exec_env_t, _: i32, _: i32, _: i32, _: i32) i32 {
    return -1;
}

// Use the C NativeSymbol type directly
const NativeSymbol = c.NativeSymbol;

// Stub symbols for each edgebox module
var g_http_stub = [_]NativeSymbol{
    .{ .symbol = "http_dispatch", .func_ptr = @ptrCast(@constCast(&stubHttp)), .signature = "(iiiiiiiii)i", .attachment = null },
};
var g_spawn_stub = [_]NativeSymbol{
    .{ .symbol = "spawn_dispatch", .func_ptr = @ptrCast(@constCast(&stubType36)), .signature = "(iiiii)i", .attachment = null },
};
var g_file_stub = [_]NativeSymbol{
    .{ .symbol = "file_dispatch", .func_ptr = @ptrCast(@constCast(&stubType36)), .signature = "(iiiii)i", .attachment = null },
};
var g_zlib_stub = [_]NativeSymbol{
    .{ .symbol = "zlib_dispatch", .func_ptr = @ptrCast(@constCast(&stubZlib)), .signature = "(iii)i", .attachment = null },
};
var g_crypto_stub = [_]NativeSymbol{
    .{ .symbol = "crypto_dispatch", .func_ptr = @ptrCast(@constCast(&stubCrypto7)), .signature = "(iiiiiii)i", .attachment = null },
};
var g_socket_stub = [_]NativeSymbol{
    .{ .symbol = "socket_dispatch", .func_ptr = @ptrCast(@constCast(&stubSocket)), .signature = "(iiii)i", .attachment = null },
};
var g_process_cm_stub = [_]NativeSymbol{
    .{ .symbol = "process_cm_dispatch", .func_ptr = @ptrCast(@constCast(&stubCrypto8)), .signature = "(iiiiiiii)i", .attachment = null },
};
var g_stdlib_stub = [_]NativeSymbol{
    .{ .symbol = "stdlib_dispatch", .func_ptr = @ptrCast(@constCast(&stubType36)), .signature = "(iiiii)i", .attachment = null },
};

fn registerStubNatives() void {
    _ = c.wasm_runtime_register_natives("edgebox_http", &g_http_stub, g_http_stub.len);
    _ = c.wasm_runtime_register_natives("edgebox_spawn", &g_spawn_stub, g_spawn_stub.len);
    _ = c.wasm_runtime_register_natives("edgebox_file", &g_file_stub, g_file_stub.len);
    _ = c.wasm_runtime_register_natives("edgebox_zlib", &g_zlib_stub, g_zlib_stub.len);
    _ = c.wasm_runtime_register_natives("edgebox_crypto", &g_crypto_stub, g_crypto_stub.len);
    _ = c.wasm_runtime_register_natives("edgebox_socket", &g_socket_stub, g_socket_stub.len);
    _ = c.wasm_runtime_register_natives("edgebox_process_cm", &g_process_cm_stub, g_process_cm_stub.len);
    _ = c.wasm_runtime_register_natives("edgebox_stdlib", &g_stdlib_stub, g_stdlib_stub.len);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Initialize WAMR - use system allocator like edgebox_wamr
    var init_args: c.RuntimeInitArgs = std.mem.zeroes(c.RuntimeInitArgs);
    init_args.mem_alloc_type = c.Alloc_With_System_Allocator;

    if (!c.wasm_runtime_full_init(&init_args)) {
        std.debug.print("Failed to initialize WAMR\n", .{});
        return;
    }
    defer c.wasm_runtime_destroy();

    // Register stub native functions (basic apps won't use them)
    registerStubNatives();

    // Copy embedded data to heap-allocated buffer (WAMR may modify it during parsing)
    // Use page_allocator for proper alignment
    const wasm_buf = std.heap.page_allocator.alloc(u8, aot_data.len) catch {
        std.debug.print("Failed to allocate buffer for module\n", .{});
        return;
    };
    defer std.heap.page_allocator.free(wasm_buf);
    @memcpy(wasm_buf, aot_data);

    // Load module from heap buffer
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

    // Set WASI args BEFORE instantiation
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Convert to C strings
    var c_args = try allocator.alloc([*:0]const u8, args.len);
    defer allocator.free(c_args);
    for (args, 0..) |arg, i| {
        c_args[i] = @ptrCast(arg.ptr);
    }

    c.wasm_runtime_set_wasi_args(
        module,
        null, 0, // No preopened dirs for embedded
        null, 0, // No mapped dirs
        null, 0, // No env vars (inherit from process)
        @ptrCast(c_args.ptr), @intCast(c_args.len),
    );

    // Instantiate with minimal memory (WASM linear memory handles the rest)
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

    // Run _start
    const start_func = c.wasm_runtime_lookup_function(module_inst, "_start");
    if (start_func != null) {
        _ = c.wasm_runtime_call_wasm(exec_env, start_func, 0, null);
    }
}
