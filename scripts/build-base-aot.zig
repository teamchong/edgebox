const std = @import("std");
const aot_compiler = @import("../src/aot_compiler.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const wasm_path = "zig-out/bin/edgebox-base.wasm";
    const aot_path = "zig-out/bin/edgebox-base.aot";

    std.debug.print("Compiling {s} to {s}...\n", .{ wasm_path, aot_path });
    try aot_compiler.compileWasmToAot(allocator, wasm_path, aot_path, true);
    std.debug.print("AOT compilation complete!\n", .{});
}
