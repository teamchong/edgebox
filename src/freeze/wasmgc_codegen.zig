//! WasmGC WAT Codegen
//!
//! Translates freeze-analyzed QuickJS bytecodes directly to WasmGC WAT.
//! Unlike the LLVM codegen (standard WASM + linear memory), this outputs
//! WasmGC modules that work in V8 snapshots and get TurboFan-inlined.
//!
//! Only supports pure integer functions (i32 tier) — no memory, no GC.
//! The output is WAT text that wasm-tools compiles to WasmGC binary.

const std = @import("std");
const numeric_handlers = @import("numeric_handlers.zig");
const opcodes = @import("opcodes.zig");

const ShardFunction = @import("frozen_registry.zig").ShardFunction;

/// Generate a WasmGC WAT module from freeze-analyzed functions.
/// Returns WAT text that can be compiled with wasm-tools.
pub fn generateWasmGCWat(
    allocator: std.mem.Allocator,
    functions: []const ShardFunction,
) ![]const u8 {
    var wat = std.ArrayList(u8).init(allocator);
    const w = wat.writer();

    try w.writeAll("(module\n");
    try w.writeAll("  (type $flags (array (mut i32)))\n\n");

    for (functions) |sf| {
        if (sf.value_kind != .i32) continue;

        // Function signature
        try w.print("  (func ${s} (export \"{s}\")\n", .{ sf.func.name, sf.func.name });

        // Parameters
        var pi: u32 = 0;
        while (pi < sf.func.arg_count) : (pi += 1) {
            try w.print("    (param $p{d} i32)\n", .{pi});
        }
        try w.writeAll("    (result i32)\n");

        // Local variables
        var vi: u32 = 0;
        while (vi < sf.func.var_count) : (vi += 1) {
            try w.print("    (local $l{d} i32)\n", .{vi});
        }

        // Function body: translate QuickJS bytecodes to WAT
        var stack_depth: i32 = 0;
        for (sf.func.instructions) |instr| {
            const handler = numeric_handlers.getHandler(instr.opcode);
            switch (handler.pattern) {
                .push_const => {
                    const val: i32 = switch (instr.operand) {
                        .i32 => |v| v,
                        .u16 => |v| @as(i32, @intCast(v)),
                        .atom => |v| @as(i32, @intCast(v)),
                        else => 0,
                    };
                    try w.print("    (i32.const {d})\n", .{val});
                    stack_depth += 1;
                },
                .get_arg => {
                    const idx: u32 = switch (instr.operand) {
                        .u16 => |v| v,
                        else => 0,
                    };
                    try w.print("    (local.get $p{d})\n", .{idx});
                    stack_depth += 1;
                },
                .put_arg => {
                    const idx: u32 = switch (instr.operand) {
                        .u16 => |v| v,
                        else => 0,
                    };
                    try w.print("    (local.set $p{d})\n", .{idx});
                    stack_depth -= 1;
                },
                .get_loc => {
                    const idx: u32 = switch (instr.operand) {
                        .u16 => |v| v,
                        else => 0,
                    };
                    try w.print("    (local.get $l{d})\n", .{idx});
                    stack_depth += 1;
                },
                .put_loc, .set_loc => {
                    const idx: u32 = switch (instr.operand) {
                        .u16 => |v| v,
                        else => 0,
                    };
                    if (handler.pattern == .set_loc) {
                        try w.print("    (local.tee $l{d})\n", .{idx});
                    } else {
                        try w.print("    (local.set $l{d})\n", .{idx});
                        stack_depth -= 1;
                    }
                },
                .binary_arith => {
                    const op_name = switch (instr.opcode) {
                        .add => "i32.add",
                        .sub => "i32.sub",
                        .mul => "i32.mul",
                        .div => "i32.div_s",
                        .mod => "i32.rem_s",
                        else => "i32.add",
                    };
                    try w.print("    ({s})\n", .{op_name});
                    stack_depth -= 1;
                },
                .binary_cmp => {
                    const op_name = switch (instr.opcode) {
                        .eq => "i32.eq",
                        .neq, .strict_neq => "i32.ne",
                        .lt => "i32.lt_s",
                        .lte => "i32.le_s",
                        .gt => "i32.gt_s",
                        .gte => "i32.ge_s",
                        .strict_eq => "i32.eq",
                        else => "i32.eq",
                    };
                    try w.print("    ({s})\n", .{op_name});
                    stack_depth -= 1;
                },
                .bitwise_binary => {
                    const op_name = switch (instr.opcode) {
                        .and => "i32.and",
                        .or => "i32.or",
                        .xor => "i32.xor",
                        .shl => "i32.shl",
                        .sar => "i32.shr_s",
                        .shr => "i32.shr_u",
                        else => "i32.and",
                    };
                    try w.print("    ({s})\n", .{op_name});
                    stack_depth -= 1;
                },
                .logical_not => {
                    try w.writeAll("    (i32.eqz)\n");
                },
                .unary => {
                    if (instr.opcode == .neg) {
                        try w.writeAll("    (i32.const 0)\n");
                        try w.writeAll("    (i32.sub)\n"); // 0 - val
                    } else if (instr.opcode == .not) {
                        try w.writeAll("    (i32.const -1)\n");
                        try w.writeAll("    (i32.xor)\n"); // ~val
                    }
                },
                .return_val => {
                    try w.writeAll("    (return)\n");
                    stack_depth -= 1;
                },
                .drop => {
                    try w.writeAll("    (drop)\n");
                    stack_depth -= 1;
                },
                else => {
                    // Unsupported opcode — emit unreachable
                    try w.print("    ;; unsupported: {s}\n", .{@tagName(handler.pattern)});
                    try w.writeAll("    (unreachable)\n");
                },
            }
        }

        // Default return
        try w.writeAll("    (i32.const -1)\n");
        try w.writeAll("  )\n\n");
    }

    try w.writeAll(")\n");
    return wat.toOwnedSlice();
}
