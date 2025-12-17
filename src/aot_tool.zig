const std = @import("std");
const aot_compiler = @import("aot_compiler.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len != 3) {
        std.debug.print("Usage: {s} <input.wasm> <output.aot>\n", .{args[0]});
        return error.InvalidArgs;
    }

    const wasm_path = args[1];
    const aot_path = args[2];

    std.debug.print("Compiling {s} to {s}...\n", .{ wasm_path, aot_path });
    try aot_compiler.compileWasmToAot(allocator, wasm_path, aot_path, true);
    std.debug.print("AOT compilation complete!\n", .{});
}
