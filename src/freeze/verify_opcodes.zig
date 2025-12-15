//! Verify opcode compatibility with QuickJS
//!
//! Run after `zig build gen-opcodes` when QuickJS is updated.
//! Usage: zig build verify-opcodes
//!
//! This checks that all opcodes we handle in codegen_ssa.zig still exist
//! in QuickJS with the same semantics (size, stack effects).

const std = @import("std");

/// Opcodes we handle in codegen_ssa.zig
/// Only need name + expected semantics, NOT the opcode value
/// (value comes from gen-opcodes which parses quickjs-opcode.h)
const HandledOpcode = struct {
    name: []const u8,
    expected_size: u8,
    expected_pop: u8,
    expected_push: u8,
    category: []const u8,
};

/// ~50 core computation opcodes we handle
const handled_opcodes = [_]HandledOpcode{
    // === ARITHMETIC (14) ===
    .{ .name = "add", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "arithmetic" },
    .{ .name = "sub", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "arithmetic" },
    .{ .name = "mul", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "arithmetic" },
    .{ .name = "div", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "arithmetic" },
    .{ .name = "mod", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "arithmetic" },
    .{ .name = "pow", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "arithmetic" },
    .{ .name = "neg", .expected_size = 1, .expected_pop = 1, .expected_push = 1, .category = "arithmetic" },
    .{ .name = "plus", .expected_size = 1, .expected_pop = 1, .expected_push = 1, .category = "arithmetic" },
    .{ .name = "inc", .expected_size = 1, .expected_pop = 1, .expected_push = 1, .category = "arithmetic" },
    .{ .name = "dec", .expected_size = 1, .expected_pop = 1, .expected_push = 1, .category = "arithmetic" },
    .{ .name = "inc_loc", .expected_size = 2, .expected_pop = 0, .expected_push = 0, .category = "arithmetic" },
    .{ .name = "dec_loc", .expected_size = 2, .expected_pop = 0, .expected_push = 0, .category = "arithmetic" },
    .{ .name = "add_loc", .expected_size = 2, .expected_pop = 1, .expected_push = 0, .category = "arithmetic" },

    // === COMPARISON (8) ===
    .{ .name = "lt", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "comparison" },
    .{ .name = "lte", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "comparison" },
    .{ .name = "gt", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "comparison" },
    .{ .name = "gte", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "comparison" },
    .{ .name = "eq", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "comparison" },
    .{ .name = "neq", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "comparison" },
    .{ .name = "strict_eq", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "comparison" },
    .{ .name = "strict_neq", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "comparison" },

    // === BITWISE (7) ===
    .{ .name = "shl", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "bitwise" },
    .{ .name = "sar", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "bitwise" },
    .{ .name = "shr", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "bitwise" },
    .{ .name = "and", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "bitwise" },
    .{ .name = "or", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "bitwise" },
    .{ .name = "xor", .expected_size = 1, .expected_pop = 2, .expected_push = 1, .category = "bitwise" },
    .{ .name = "not", .expected_size = 1, .expected_pop = 1, .expected_push = 1, .category = "bitwise" },

    // === CONTROL FLOW (12) ===
    .{ .name = "return", .expected_size = 1, .expected_pop = 1, .expected_push = 0, .category = "control" },
    .{ .name = "return_undef", .expected_size = 1, .expected_pop = 0, .expected_push = 0, .category = "control" },
    .{ .name = "if_false", .expected_size = 5, .expected_pop = 1, .expected_push = 0, .category = "control" },
    .{ .name = "if_true", .expected_size = 5, .expected_pop = 1, .expected_push = 0, .category = "control" },
    .{ .name = "if_false8", .expected_size = 2, .expected_pop = 1, .expected_push = 0, .category = "control" },
    .{ .name = "if_true8", .expected_size = 2, .expected_pop = 1, .expected_push = 0, .category = "control" },
    .{ .name = "goto", .expected_size = 5, .expected_pop = 0, .expected_push = 0, .category = "control" },
    .{ .name = "goto8", .expected_size = 2, .expected_pop = 0, .expected_push = 0, .category = "control" },
    .{ .name = "goto16", .expected_size = 3, .expected_pop = 0, .expected_push = 0, .category = "control" },
    .{ .name = "call", .expected_size = 3, .expected_pop = 1, .expected_push = 1, .category = "control" },
    .{ .name = "call0", .expected_size = 1, .expected_pop = 1, .expected_push = 1, .category = "control" },
    .{ .name = "call1", .expected_size = 1, .expected_pop = 1, .expected_push = 1, .category = "control" },
    .{ .name = "call2", .expected_size = 1, .expected_pop = 1, .expected_push = 1, .category = "control" },
    .{ .name = "call3", .expected_size = 1, .expected_pop = 1, .expected_push = 1, .category = "control" },

    // === STACK/LOCALS (~20) ===
    .{ .name = "drop", .expected_size = 1, .expected_pop = 1, .expected_push = 0, .category = "stack" },
    .{ .name = "dup", .expected_size = 1, .expected_pop = 1, .expected_push = 2, .category = "stack" },
    .{ .name = "dup2", .expected_size = 1, .expected_pop = 2, .expected_push = 4, .category = "stack" },
    .{ .name = "push_i32", .expected_size = 5, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "push_i8", .expected_size = 2, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "push_i16", .expected_size = 3, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "push_minus1", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "push_0", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "push_1", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "push_2", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "push_3", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "push_4", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "push_5", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "push_6", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "push_7", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "push_false", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "push_true", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "undefined", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "null", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "get_arg", .expected_size = 3, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "get_arg0", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "get_arg1", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "get_arg2", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "get_arg3", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "put_arg", .expected_size = 3, .expected_pop = 1, .expected_push = 0, .category = "stack" },
    .{ .name = "put_arg0", .expected_size = 1, .expected_pop = 1, .expected_push = 0, .category = "stack" },
    .{ .name = "put_arg1", .expected_size = 1, .expected_pop = 1, .expected_push = 0, .category = "stack" },
    .{ .name = "set_arg", .expected_size = 3, .expected_pop = 1, .expected_push = 1, .category = "stack" },
    .{ .name = "get_loc", .expected_size = 3, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "put_loc", .expected_size = 3, .expected_pop = 1, .expected_push = 0, .category = "stack" },
    .{ .name = "get_loc8", .expected_size = 2, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "put_loc8", .expected_size = 2, .expected_pop = 1, .expected_push = 0, .category = "stack" },
    .{ .name = "set_loc", .expected_size = 3, .expected_pop = 1, .expected_push = 1, .category = "stack" },
    .{ .name = "set_loc8", .expected_size = 2, .expected_pop = 1, .expected_push = 1, .category = "stack" },
    .{ .name = "get_loc0", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "get_loc1", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "get_loc2", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "get_loc3", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "put_loc0", .expected_size = 1, .expected_pop = 1, .expected_push = 0, .category = "stack" },
    .{ .name = "put_loc1", .expected_size = 1, .expected_pop = 1, .expected_push = 0, .category = "stack" },
    .{ .name = "put_loc2", .expected_size = 1, .expected_pop = 1, .expected_push = 0, .category = "stack" },
    .{ .name = "put_loc3", .expected_size = 1, .expected_pop = 1, .expected_push = 0, .category = "stack" },
    .{ .name = "get_var_ref0", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "get_var_ref1", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "get_var_ref2", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "get_var_ref3", .expected_size = 1, .expected_pop = 0, .expected_push = 1, .category = "stack" },
    .{ .name = "put_var_ref0", .expected_size = 1, .expected_pop = 1, .expected_push = 0, .category = "stack" },
    .{ .name = "put_var_ref1", .expected_size = 1, .expected_pop = 1, .expected_push = 0, .category = "stack" },
    .{ .name = "put_var_ref2", .expected_size = 1, .expected_pop = 1, .expected_push = 0, .category = "stack" },
    .{ .name = "put_var_ref3", .expected_size = 1, .expected_pop = 1, .expected_push = 0, .category = "stack" },

    // === GLOBAL VARIABLES ===
    .{ .name = "get_var", .expected_size = 5, .expected_pop = 0, .expected_push = 1, .category = "global" },
    .{ .name = "get_var_undef", .expected_size = 5, .expected_pop = 0, .expected_push = 1, .category = "global" },
    .{ .name = "put_var", .expected_size = 5, .expected_pop = 1, .expected_push = 0, .category = "global" },
    .{ .name = "put_var_init", .expected_size = 5, .expected_pop = 1, .expected_push = 0, .category = "global" },
    .{ .name = "put_var_strict", .expected_size = 5, .expected_pop = 2, .expected_push = 0, .category = "global" },
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const opcode_header = if (args.len > 1) args[1] else "vendor/quickjs-ng/quickjs-opcode.h";

    // Read and parse quickjs-opcode.h
    const file = std.fs.cwd().openFile(opcode_header, .{}) catch |err| {
        std.debug.print("Error opening {s}: {}\n", .{ opcode_header, err });
        std.debug.print("Run from project root or specify path as argument\n", .{});
        std.process.exit(1);
    };
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Parse opcodes from header
    const OpcodeInfo = struct { value: u16, size: u8, pop: u8, push: u8 };
    var current_opcodes = std.StringHashMap(OpcodeInfo).init(allocator);
    defer current_opcodes.deinit();

    var opcode_value: u16 = 0;
    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        const def_pos = std.mem.indexOf(u8, line, "DEF(") orelse
            std.mem.indexOf(u8, line, "def(") orelse continue;
        if (std.mem.indexOf(u8, line, "#define") != null) continue;

        const start = def_pos + 4;
        const end = std.mem.indexOf(u8, line[start..], ")") orelse continue;
        const args_str = line[start .. start + end];

        var parts = std.mem.splitScalar(u8, args_str, ',');
        const name = std.mem.trim(u8, parts.next() orelse continue, " \t");
        const size_str = std.mem.trim(u8, parts.next() orelse continue, " \t");
        const pop_str = std.mem.trim(u8, parts.next() orelse continue, " \t");
        const push_str = std.mem.trim(u8, parts.next() orelse continue, " \t");

        const size = std.fmt.parseInt(u8, size_str, 10) catch continue;
        const pop = std.fmt.parseInt(u8, pop_str, 10) catch continue;
        const push = std.fmt.parseInt(u8, push_str, 10) catch continue;

        try current_opcodes.put(name, .{ .value = opcode_value, .size = size, .pop = pop, .push = push });
        opcode_value += 1;
    }

    std.debug.print("Parsed {d} opcodes from {s}\n", .{ current_opcodes.count(), opcode_header });
    std.debug.print("Verifying {d} handled opcodes...\n\n", .{handled_opcodes.len});

    // Count by category
    var by_category = std.StringHashMap(u32).init(allocator);
    defer by_category.deinit();

    // Verify each handled opcode
    var errors: u32 = 0;
    var ok_count: u32 = 0;

    for (handled_opcodes) |handled| {
        if (current_opcodes.get(handled.name)) |current| {
            var has_error = false;

            if (current.size != handled.expected_size) {
                std.debug.print("ERROR: {s} size changed: {d} -> {d}\n", .{ handled.name, handled.expected_size, current.size });
                errors += 1;
                has_error = true;
            }
            if (current.pop != handled.expected_pop) {
                std.debug.print("ERROR: {s} n_pop changed: {d} -> {d}\n", .{ handled.name, handled.expected_pop, current.pop });
                errors += 1;
                has_error = true;
            }
            if (current.push != handled.expected_push) {
                std.debug.print("ERROR: {s} n_push changed: {d} -> {d}\n", .{ handled.name, handled.expected_push, current.push });
                errors += 1;
                has_error = true;
            }

            if (!has_error) {
                ok_count += 1;
                const count = by_category.get(handled.category) orelse 0;
                try by_category.put(handled.category, count + 1);
            }
        } else {
            std.debug.print("ERROR: {s} not found in QuickJS (removed?)\n", .{handled.name});
            errors += 1;
        }
    }

    std.debug.print("\n=== Summary ===\n", .{});
    std.debug.print("Total handled opcodes: {d}\n", .{handled_opcodes.len});
    std.debug.print("Verified OK: {d}\n", .{ok_count});
    std.debug.print("Errors: {d}\n\n", .{errors});

    std.debug.print("By category:\n", .{});
    var it = by_category.iterator();
    while (it.next()) |entry| {
        std.debug.print("  {s}: {d}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    }

    if (errors > 0) {
        std.debug.print("\nFAILED: QuickJS opcode semantics changed!\n", .{});
        std.debug.print("Update verify_opcodes.zig with new semantics.\n", .{});
        std.process.exit(1);
    } else {
        std.debug.print("\nPASSED: All {d} handled opcodes are compatible.\n", .{ok_count});
    }
}
