// AOT compiler stub — compileWasmToAot
// Original implementation was removed. This stub allows edgebox-compile to build.

const std = @import("std");

pub fn compileWasmToAot(
    _: std.mem.Allocator,
    _: []const u8,
    _: []const u8,
    _: bool,
) !void {
    return error.WasmTooLarge; // Skip AOT — use WASM-only path
}
