const std = @import("std");

// Declare qjsc's main() from qjsc.c (renamed to qjsc_main to avoid conflict)
// In C, main() is just a regular function we can call
extern "c" fn qjsc_main(argc: c_int, argv: [*c][*c]u8) c_int;

/// Call qjsc's main() with given arguments
/// Returns exit code (0 = success)
pub fn compileJsToBytecode(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) !u8 {
    // Convert Zig strings to C argv format
    var c_argv = try allocator.alloc([*c]u8, args.len);
    defer allocator.free(c_argv);

    for (args, 0..) |arg, i| {
        // Allocate null-terminated C string
        const c_str = try allocator.dupeZ(u8, arg);
        c_argv[i] = @ptrCast(c_str);
    }
    defer {
        for (c_argv) |arg| {
            allocator.free(std.mem.span(arg));
        }
    }

    // Call qjsc_main() (renamed to avoid conflict with Zig's main)
    const exit_code = qjsc_main(@intCast(args.len), @ptrCast(c_argv.ptr));
    return @intCast(exit_code);
}
