const std = @import("std");

extern fn edgebox_v8_init() void;
extern fn edgebox_v8_create_isolate() ?*anyopaque;
extern fn edgebox_v8_setup_context(?*anyopaque) ?*anyopaque;
extern fn edgebox_v8_eval_in_context(?*anyopaque, ?*anyopaque, [*]const u8, c_int, *c_int) ?[*]const u8;
extern fn edgebox_v8_free(?[*]const u8) void;

pub fn main() !void {
    edgebox_v8_init();
    const iso = edgebox_v8_create_isolate() orelse return;
    const ctx = edgebox_v8_setup_context(iso) orelse return;

    // Test 1: Simple string with fromCharCode(10)
    const code1 = "'A' + String.fromCharCode(10) + 'B'";
    var len1: c_int = 0;
    const r1 = edgebox_v8_eval_in_context(iso, ctx, code1, @intCast(code1.len), &len1);
    if (r1) |data| {
        const bytes = data[0..@intCast(len1)];
        _ = std.posix.write(1, "Test 1: ") catch {};
        for (bytes) |b| {
            const hex = "0123456789abcdef";
            const h = [2]u8{ hex[b >> 4], hex[b & 0xf] };
            _ = std.posix.write(1, &h) catch {};
            _ = std.posix.write(1, " ") catch {};
        }
        _ = std.posix.write(1, "\n") catch {};
        _ = std.posix.write(1, "Expected: 41 0a 42\n") catch {};
        edgebox_v8_free(r1);
    }

    // Test 2: Array join with fromCharCode(10)
    const code2 = "['hello','world'].join(String.fromCharCode(10))";
    var len2: c_int = 0;
    const r2 = edgebox_v8_eval_in_context(iso, ctx, code2, @intCast(code2.len), &len2);
    if (r2) |data| {
        const bytes = data[0..@intCast(len2)];
        _ = std.posix.write(1, "Test 2: ") catch {};
        for (bytes) |b| {
            const hex = "0123456789abcdef";
            const h = [2]u8{ hex[b >> 4], hex[b & 0xf] };
            _ = std.posix.write(1, &h) catch {};
            _ = std.posix.write(1, " ") catch {};
        }
        _ = std.posix.write(1, "\n") catch {};
        _ = std.posix.write(1, "Expected: 68 65 6c 6c 6f 0a 77 6f 72 6c 64\n") catch {};
        edgebox_v8_free(r2);
    }
}
