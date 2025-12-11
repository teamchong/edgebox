//! Build-time opcode generator
//! Parses quickjs-opcode.h and generates opcodes_gen.zig
//!
//! This ensures our opcode definitions stay in sync with QuickJS.
//! Run: zig build gen-opcodes

const std = @import("std");

const OpcodeInfo = struct {
    name: []const u8,
    size: u8,
    n_pop: u8,
    n_push: u8,
    format: []const u8,
    is_temp: bool, // def() vs DEF()
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: gen_opcodes <quickjs-opcode.h> <output.zig>\n", .{});
        std.process.exit(1);
    }

    const input_path = args[1];
    const output_path = args[2];

    // Read input file
    const input_file = std.fs.cwd().openFile(input_path, .{}) catch |err| {
        std.debug.print("Error opening {s}: {}\n", .{ input_path, err });
        std.process.exit(1);
    };
    defer input_file.close();
    const content = try input_file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Parse opcodes
    var opcodes = std.ArrayListUnmanaged(OpcodeInfo){};
    defer opcodes.deinit(allocator);

    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        // Look for DEF(...) or def(...)
        const is_temp = std.mem.indexOf(u8, line, "def(") != null and std.mem.indexOf(u8, line, "DEF(") == null;
        const def_pos = std.mem.indexOf(u8, line, "DEF(") orelse std.mem.indexOf(u8, line, "def(") orelse continue;

        // Skip the #define line
        if (std.mem.indexOf(u8, line, "#define") != null) continue;

        const start = def_pos + 4;
        const end = std.mem.indexOf(u8, line[start..], ")") orelse continue;
        const args_str = line[start .. start + end];

        // Parse: name, size, n_pop, n_push, format
        var parts = std.mem.splitScalar(u8, args_str, ',');
        const name = std.mem.trim(u8, parts.next() orelse continue, " \t");
        const size_str = std.mem.trim(u8, parts.next() orelse continue, " \t");
        const n_pop_str = std.mem.trim(u8, parts.next() orelse continue, " \t");
        const n_push_str = std.mem.trim(u8, parts.next() orelse continue, " \t");
        const format = std.mem.trim(u8, parts.next() orelse continue, " \t");

        const size = std.fmt.parseInt(u8, size_str, 10) catch continue;
        const n_pop = std.fmt.parseInt(u8, n_pop_str, 10) catch continue;
        const n_push = std.fmt.parseInt(u8, n_push_str, 10) catch continue;

        try opcodes.append(allocator, .{
            .name = name,
            .size = size,
            .n_pop = n_pop,
            .n_push = n_push,
            .format = format,
            .is_temp = is_temp,
        });
    }

    // Generate output to buffer, then write
    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(allocator);

    // Helper to append string
    const append = struct {
        fn f(list: *std.ArrayListUnmanaged(u8), alloc: std.mem.Allocator, str: []const u8) !void {
            try list.appendSlice(alloc, str);
        }
    }.f;

    const appendFmt = struct {
        fn f(list: *std.ArrayListUnmanaged(u8), alloc: std.mem.Allocator, comptime fmt: []const u8, args_tuple: anytype) !void {
            const len = std.fmt.count(fmt, args_tuple);
            try list.ensureUnusedCapacity(alloc, len);
            _ = std.fmt.bufPrint(list.unusedCapacitySlice(), fmt, args_tuple) catch unreachable;
            list.items.len += len;
        }
    }.f;

    try append(&output, allocator,
        \\//! QuickJS Opcode Definitions
        \\//! AUTO-GENERATED from quickjs-opcode.h - DO NOT EDIT MANUALLY
        \\//!
        \\//! Regenerate with: zig build gen-opcodes
        \\//! Source: vendor/quickjs-ng/quickjs-opcode.h
        \\
        \\const std = @import("std");
        \\
        \\/// Instruction format types
        \\pub const Format = enum {
        \\    none,
        \\    none_int,
        \\    none_loc,
        \\    none_arg,
        \\    none_var_ref,
        \\    u8,
        \\    i8,
        \\    loc8,
        \\    const8,
        \\    label8,
        \\    u16,
        \\    i16,
        \\    label16,
        \\    npop,
        \\    npopx,
        \\    npop_u16,
        \\    loc,
        \\    arg,
        \\    var_ref,
        \\    u32,
        \\    u32x2,
        \\    i32,
        \\    @"const",
        \\    label,
        \\    atom,
        \\    atom_u8,
        \\    atom_u16,
        \\    atom_label_u8,
        \\    atom_label_u16,
        \\    label_u16,
        \\};
        \\
        \\pub const Opcode = enum(u8) {
        \\
    );

    // Write enum values
    for (opcodes.items, 0..) |op, i| {
        // Handle Zig keywords
        if (std.mem.eql(u8, op.name, "return") or
            std.mem.eql(u8, op.name, "and") or
            std.mem.eql(u8, op.name, "or") or
            std.mem.eql(u8, op.name, "const"))
        {
            try appendFmt(&output, allocator, "    @\"{s}\" = {d},\n", .{ op.name, i });
        } else {
            try appendFmt(&output, allocator, "    {s} = {d},\n", .{ op.name, i });
        }
    }

    try append(&output, allocator,
        \\
        \\    pub fn name(self: Opcode) []const u8 {
        \\        return @tagName(self);
        \\    }
        \\};
        \\
        \\pub const OPCODE_COUNT: u16 =
    );
    try appendFmt(&output, allocator, "{d};\n\n", .{opcodes.items.len});

    // Write metadata table
    try append(&output, allocator,
        \\/// Opcode metadata: size, stack effects, format
        \\pub const OpcodeInfo = struct {
        \\    size: u8,
        \\    n_pop: u8,
        \\    n_push: u8,
        \\    format: Format,
        \\    is_temp: bool,
        \\};
        \\
        \\pub const opcode_info = [_]OpcodeInfo{
        \\
    );

    for (opcodes.items) |op| {
        // Map format string to enum
        var fmt_enum: []const u8 = "none";
        if (std.mem.eql(u8, op.format, "const")) {
            fmt_enum = "@\"const\"";
        } else {
            fmt_enum = op.format;
        }

        try appendFmt(&output, allocator, "    .{{ .size = {d}, .n_pop = {d}, .n_push = {d}, .format = .{s}, .is_temp = {s} }},\n", .{
            op.size,
            op.n_pop,
            op.n_push,
            fmt_enum,
            if (op.is_temp) "true" else "false",
        });
    }

    try append(&output, allocator,
        \\};
        \\
        \\/// Get metadata for an opcode
        \\pub fn getInfo(op: Opcode) OpcodeInfo {
        \\    return opcode_info[@intFromEnum(op)];
        \\}
        \\
        \\/// Get instruction size for an opcode
        \\pub fn getSize(op: Opcode) u8 {
        \\    return opcode_info[@intFromEnum(op)].size;
        \\}
        \\
    );

    // Write output file
    const output_file = std.fs.cwd().createFile(output_path, .{}) catch |err| {
        std.debug.print("Error creating {s}: {}\n", .{ output_path, err });
        std.process.exit(1);
    };
    defer output_file.close();
    output_file.writeAll(output.items) catch |err| {
        std.debug.print("Error writing {s}: {}\n", .{ output_path, err });
        std.process.exit(1);
    };

    std.debug.print("Generated {d} opcodes to {s}\n", .{ opcodes.items.len, output_path });
}
