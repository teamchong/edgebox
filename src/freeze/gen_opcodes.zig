//! Build-time opcode generator
//! Parses quickjs-opcode.h and generates opcodes.zig
//!
//! This ensures our opcode definitions stay in sync with QuickJS.

const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: gen_opcodes <quickjs-opcode.h> <output.zig>\n", .{});
        return;
    }

    const input_path = args[1];
    const output_path = args[2];

    // Read input file
    const input_file = try std.fs.cwd().openFile(input_path, .{});
    defer input_file.close();
    const content = try input_file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Parse and generate
    const output_file = try std.fs.cwd().createFile(output_path, .{});
    defer output_file.close();
    const writer = output_file.writer();

    try writer.writeAll(
        \\//! QuickJS Opcode Definitions
        \\//! AUTO-GENERATED from quickjs-opcode.h - DO NOT EDIT
        \\//!
        \\//! Regenerate with: zig build gen-opcodes
        \\
        \\const std = @import("std");
        \\
        \\pub const Opcode = enum(u8) {
        \\
    );

    // Parse DEF() lines
    var lines = std.mem.splitScalar(u8, content, '\n');
    var opcode_num: u8 = 0;
    while (lines.next()) |line| {
        // Look for DEF(name, size, n_pop, n_push, format)
        if (std.mem.indexOf(u8, line, "DEF(")) |start| {
            const def_start = start + 4;
            if (std.mem.indexOf(u8, line[def_start..], ",")) |comma| {
                var name = std.mem.trim(u8, line[def_start .. def_start + comma], " \t");

                // Convert to valid Zig identifier
                // Handle keywords
                if (std.mem.eql(u8, name, "return")) {
                    try writer.print("    @\"return\" = {d},\n", .{opcode_num});
                } else if (std.mem.eql(u8, name, "and")) {
                    try writer.print("    @\"and\" = {d},\n", .{opcode_num});
                } else if (std.mem.eql(u8, name, "or")) {
                    try writer.print("    @\"or\" = {d},\n", .{opcode_num});
                } else {
                    try writer.print("    {s} = {d},\n", .{ name, opcode_num });
                }
                opcode_num += 1;
            }
        }
    }

    try writer.writeAll(
        \\};
        \\
        \\pub const OPCODE_COUNT: u16 =
    );
    try writer.print("{d};\n", .{opcode_num});
}
