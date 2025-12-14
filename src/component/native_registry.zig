/// Native Implementation Registry for Component Model
/// Maps WIT function calls to Zig implementations

const std = @import("std");

/// Value types that can be passed to/from native implementations
pub const Value = union(enum) {
    u8: u8,
    u16: u16,
    u32: u32,
    u64: u64,
    s8: i8,
    s16: i16,
    s32: i32,
    s64: i64,
    f32: f32,
    f64: f64,
    bool: bool,
    string: []const u8,
    list_u8: []const u8,
    list_u32: []const u32,
    resource_handle: u32,
    void: void,

    /// Helper to extract u32 value
    pub fn asU32(self: Value) !u32 {
        return switch (self) {
            .u32 => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract u64 value
    pub fn asU64(self: Value) !u64 {
        return switch (self) {
            .u64 => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract string value
    pub fn asString(self: Value) ![]const u8 {
        return switch (self) {
            .string => |v| v,
            else => error.TypeMismatch,
        };
    }
};

/// Native implementation function type
pub const ImplFn = *const fn ([]const Value) anyerror!Value;

/// Implementation entry
const Implementation = struct {
    interface: []const u8,
    function: []const u8,
    callback: ImplFn,
};

/// Native implementation registry
pub const NativeRegistry = struct {
    allocator: std.mem.Allocator,
    implementations: std.StringHashMap(Implementation),

    pub fn init(allocator: std.mem.Allocator) NativeRegistry {
        return .{
            .allocator = allocator,
            .implementations = std.StringHashMap(Implementation).init(allocator),
        };
    }

    pub fn deinit(self: *NativeRegistry) void {
        var it = self.implementations.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.implementations.deinit();
    }

    /// Register a native implementation
    pub fn register(
        self: *NativeRegistry,
        interface: []const u8,
        function: []const u8,
        impl: ImplFn,
    ) !void {
        const key = try std.fmt.allocPrint(
            self.allocator,
            "{s}::{s}",
            .{ interface, function },
        );
        errdefer self.allocator.free(key);

        try self.implementations.put(key, Implementation{
            .interface = interface,
            .function = function,
            .callback = impl,
        });
    }

    /// Call a native implementation
    pub fn call(
        self: *NativeRegistry,
        interface: []const u8,
        function: []const u8,
        args: []const Value,
    ) !Value {
        const key = try std.fmt.allocPrint(
            self.allocator,
            "{s}::{s}",
            .{ interface, function },
        );
        defer self.allocator.free(key);

        const impl = self.implementations.get(key) orelse
            return error.FunctionNotFound;

        return try impl.callback(args);
    }

    /// Check if a function is registered
    pub fn has(
        self: *NativeRegistry,
        interface: []const u8,
        function: []const u8,
    ) bool {
        const key = std.fmt.allocPrint(
            self.allocator,
            "{s}::{s}",
            .{ interface, function },
        ) catch return false;
        defer self.allocator.free(key);

        return self.implementations.contains(key);
    }
};

// Tests
test "NativeRegistry - register and call" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    // Register a test function
    const testImpl = struct {
        fn impl(args: []const Value) !Value {
            const a = try args[0].asU32();
            const b = try args[1].asU32();
            return Value{ .u32 = a + b };
        }
    }.impl;

    try registry.register("test", "add", testImpl);

    // Call the function
    const args = [_]Value{
        Value{ .u32 = 10 },
        Value{ .u32 = 20 },
    };
    const result = try registry.call("test", "add", &args);

    try std.testing.expectEqual(@as(u32, 30), try result.asU32());
}

test "NativeRegistry - function not found" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    const result = registry.call("test", "missing", &.{});
    try std.testing.expectError(error.FunctionNotFound, result);
}

test "NativeRegistry - has function" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    const testImpl = struct {
        fn impl(_: []const Value) !Value {
            return Value{ .void = {} };
        }
    }.impl;

    try registry.register("test", "func", testImpl);

    try std.testing.expect(registry.has("test", "func"));
    try std.testing.expect(!registry.has("test", "missing"));
}
