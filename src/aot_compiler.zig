const std = @import("std");

// Note: std.debug.print writes to stderr unbuffered, so we just use it directly
// The detailed logging helps debug crashes in AOT compilation

/// Compile a WASM file to AOT format using WAMR's wamrc tool
/// We use the wamrc binary instead of the library because the library
/// has linking issues with Zig's build system on Linux
pub fn compileWasmToAot(
    allocator: std.mem.Allocator,
    wasm_path: []const u8,
    aot_path: []const u8,
    enable_simd: bool,
) !void {
    std.debug.print("[aot] Starting AOT compilation: {s} -> {s}\n", .{ wasm_path, aot_path });
    std.debug.print("[aot] SIMD enabled: {}\n", .{enable_simd});

    // Build wamrc command
    var args = std.ArrayList([]const u8).init(allocator);
    defer args.deinit();

    // Find wamrc in the wamr-compiler build directory
    const wamrc_path = "vendor/wamr/wamr-compiler/build/wamrc";

    try args.append(wamrc_path);
    try args.append("--opt-level=3");
    try args.append("--size-level=0");
    if (enable_simd) {
        try args.append("--enable-simd");
    }
    try args.append("--enable-bulk-memory");
    try args.append("--enable-ref-types");
    try args.append("--bounds-checks=1");
    try args.append("-o");
    try args.append(aot_path);
    try args.append(wasm_path);

    std.debug.print("[aot] Running wamrc...\n", .{});

    var child = std.process.Child.init(args.items, allocator);
    child.stderr_behavior = .Inherit;
    child.stdout_behavior = .Inherit;

    try child.spawn();
    const term = try child.wait();

    switch (term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("[aot] wamrc failed with exit code: {}\n", .{code});
                return error.CompilationFailed;
            }
        },
        else => {
            std.debug.print("[aot] wamrc terminated abnormally\n", .{});
            return error.CompilationFailed;
        },
    }

    std.debug.print("[aot] AOT compilation successful\n", .{});
}
