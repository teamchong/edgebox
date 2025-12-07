/// EdgeBox WASM Entry Point
/// Minimal QuickJS runtime for WASI
const std = @import("std");
const quickjs = @import("quickjs_core.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print(
            \\EdgeBox - QuickJS WASM Runtime
            \\
            \\Usage:
            \\  edgebox <script.js>     Run JavaScript file
            \\  edgebox -e "<code>"     Evaluate JavaScript code
            \\
        , .{});
        return;
    }

    // Create QuickJS runtime with std module (print, console, etc.)
    var runtime = try quickjs.Runtime.init(allocator);
    defer runtime.deinit();

    var context = try runtime.newStdContext();
    defer context.deinit();

    const cmd = args[1];

    if (std.mem.eql(u8, cmd, "-e") and args.len > 2) {
        // Eval mode
        const result = context.eval(args[2]) catch |err| {
            std.debug.print("Error: {}\n", .{err});
            std.process.exit(1);
        };
        defer result.free();

        if (!result.isUndefined()) {
            if (result.toStringSlice()) |str| {
                std.debug.print("{s}\n", .{str});
            }
        }
    } else {
        // Run file
        const code = std.fs.cwd().readFileAlloc(allocator, cmd, 50 * 1024 * 1024) catch |err| {
            std.debug.print("Error reading {s}: {}\n", .{ cmd, err });
            std.process.exit(1);
        };
        defer allocator.free(code);

        const result = context.eval(code) catch |err| {
            std.debug.print("Error: {}\n", .{err});
            std.process.exit(1);
        };
        defer result.free();
    }
}
