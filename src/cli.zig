/// EdgeBox CLI Entry Point
const std = @import("std");
const main_mod = @import("main.zig");

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Convert to sentinel-terminated strings
    var sentinel_args = try allocator.alloc([:0]const u8, args.len);
    defer allocator.free(sentinel_args);

    for (args, 0..) |arg, i| {
        sentinel_args[i] = try allocator.dupeZ(u8, arg);
    }
    defer {
        for (sentinel_args) |arg| {
            allocator.free(arg);
        }
    }

    return main_mod.runCli(allocator, sentinel_args);
}
