/// Zig wrapper for native_bindings.c
/// Bridges QuickJS C bindings to Zig callers in main.zig
const std = @import("std");
const quickjs = @import("quickjs_core.zig");
const qjs = quickjs.c;

var global_allocator: ?std.mem.Allocator = null;
var global_cwd: ?[]const u8 = null;

extern fn register_native_bindings(ctx: ?*qjs.JSContext) void;

pub fn init(allocator: std.mem.Allocator) void {
    global_allocator = allocator;
}

pub fn setCwd(cwd: []const u8) void {
    global_cwd = cwd;
}

pub fn registerAll(ctx: *qjs.JSContext) void {
    register_native_bindings(ctx);
}
