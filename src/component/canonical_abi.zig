/// Canonical ABI for WASI Component Model
/// Based on: https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md
///
/// This implements the type lowering/lifting between host and WASM memory
/// for Component Model interfaces.

const std = @import("std");

/// Canonical ABI context
pub const CanonicalABI = struct {
    allocator: std.mem.Allocator,
    memory: []u8,
    realloc_fn: ?*const fn (u32, u32, u32, u32) u32,

    pub fn init(allocator: std.mem.Allocator, memory: []u8, realloc_fn: ?*const fn (u32, u32, u32, u32) u32) CanonicalABI {
        return .{
            .allocator = allocator,
            .memory = memory,
            .realloc_fn = realloc_fn,
        };
    }

    /// Lower a string from host to WASM memory
    /// Returns pointer and length in WASM memory
    pub fn lowerString(self: *CanonicalABI, str: []const u8) !StringPtr {
        const realloc = self.realloc_fn orelse return error.NoReallocFunction;

        // Allocate space in WASM memory
        // realloc(old_ptr, old_size, alignment, new_size)
        const ptr = realloc(0, 0, 1, @intCast(str.len));

        if (ptr + str.len > self.memory.len) {
            return error.OutOfMemory;
        }

        // Copy string to WASM memory
        @memcpy(self.memory[ptr .. ptr + str.len], str);

        return StringPtr{
            .ptr = ptr,
            .len = @intCast(str.len),
        };
    }

    /// Lift a string from WASM memory to host
    /// Validates UTF-8 and copies to host memory
    pub fn liftString(self: *CanonicalABI, ptr: u32, len: u32) ![]const u8 {
        if (ptr + len > self.memory.len) return error.OutOfBounds;

        // Get slice from WASM memory
        const slice = self.memory[ptr .. ptr + len];

        // Validate UTF-8
        if (!std.unicode.utf8ValidateSlice(slice)) return error.InvalidUtf8;

        // Copy to host memory (caller must free)
        const result = try self.allocator.alloc(u8, len);
        @memcpy(result, slice);

        return result;
    }

    /// Lower a list to WASM memory
    pub fn lowerList(self: *CanonicalABI, comptime T: type, items: []const T) !ListPtr {
        const realloc = self.realloc_fn orelse return error.NoReallocFunction;

        const elem_size = @sizeOf(T);
        const total_size = items.len * elem_size;

        // Allocate space
        const ptr = realloc(0, 0, @alignOf(T), @intCast(total_size));

        if (ptr + total_size > self.memory.len) {
            return error.OutOfMemory;
        }

        // Copy elements
        for (items, 0..) |item, i| {
            const offset = ptr + @as(u32, @intCast(i * elem_size));
            switch (T) {
                u8 => self.memory[offset] = item,
                u16, u32, u64 => {
                    std.mem.writeInt(T, self.memory[offset..][0..elem_size], item, .little);
                },
                else => @compileError("Unsupported list element type"),
            }
        }

        return ListPtr{
            .ptr = ptr,
            .len = @intCast(items.len),
        };
    }

    /// Lift a list from WASM memory
    pub fn liftList(self: *CanonicalABI, comptime T: type, ptr: u32, len: u32) ![]T {
        const elem_size = @sizeOf(T);
        const total_size = len * elem_size;

        if (ptr + total_size > self.memory.len) return error.OutOfBounds;

        const result = try self.allocator.alloc(T, len);

        for (0..len) |i| {
            const offset = ptr + @as(u32, @intCast(i * elem_size));
            result[i] = switch (T) {
                u8 => self.memory[offset],
                u16, u32, u64 => std.mem.readInt(T, self.memory[offset..][0..elem_size], .little),
                else => @compileError("Unsupported list element type"),
            };
        }

        return result;
    }

    /// Lower a u32 value (passthrough)
    pub fn lowerU32(_: *CanonicalABI, value: u32) u32 {
        return value;
    }

    /// Lift a u32 value (passthrough)
    pub fn liftU32(_: *CanonicalABI, value: u32) u32 {
        return value;
    }

    /// Lower a u64 value (passthrough)
    pub fn lowerU64(_: *CanonicalABI, value: u64) u64 {
        return value;
    }

    /// Lift a u64 value (passthrough)
    pub fn liftU64(_: *CanonicalABI, value: u64) u64 {
        return value;
    }
};

/// String representation in WASM memory
pub const StringPtr = struct {
    ptr: u32,
    len: u32,
};

/// List representation in WASM memory
pub const ListPtr = struct {
    ptr: u32,
    len: u32,
};

// Tests
test "CanonicalABI - lower/lift string" {
    var memory: [1024]u8 = undefined;
    @memset(&memory, 0);

    // Mock realloc that just returns increasing addresses
    const MockRealloc = struct {
        var next_ptr: u32 = 0;
        fn realloc(_: u32, _: u32, _: u32, size: u32) u32 {
            const ptr = next_ptr;
            next_ptr += size;
            return ptr;
        }
    };

    var abi = CanonicalABI.init(std.testing.allocator, &memory, MockRealloc.realloc);

    // Lower string
    const test_str = "Hello, Component Model!";
    const str_ptr = try abi.lowerString(test_str);

    try std.testing.expectEqual(@as(u32, 0), str_ptr.ptr);
    try std.testing.expectEqual(@as(u32, test_str.len), str_ptr.len);

    // Verify it's in memory
    try std.testing.expectEqualSlices(u8, test_str, memory[str_ptr.ptr .. str_ptr.ptr + str_ptr.len]);

    // Lift string back
    const lifted = try abi.liftString(str_ptr.ptr, str_ptr.len);
    defer std.testing.allocator.free(lifted);

    try std.testing.expectEqualSlices(u8, test_str, lifted);
}

test "CanonicalABI - lower/lift list of u32" {
    var memory: [1024]u8 = undefined;
    @memset(&memory, 0);

    const MockRealloc = struct {
        var next_ptr: u32 = 0;
        fn realloc(_: u32, _: u32, _: u32, size: u32) u32 {
            const ptr = next_ptr;
            next_ptr += size;
            return ptr;
        }
    };

    var abi = CanonicalABI.init(std.testing.allocator, &memory, MockRealloc.realloc);

    // Lower list
    const test_list = [_]u32{ 1, 2, 3, 4, 5 };
    const list_ptr = try abi.lowerList(u32, &test_list);

    try std.testing.expectEqual(@as(u32, 0), list_ptr.ptr);
    try std.testing.expectEqual(@as(u32, 5), list_ptr.len);

    // Lift list back
    const lifted = try abi.liftList(u32, list_ptr.ptr, list_ptr.len);
    defer std.testing.allocator.free(lifted);

    try std.testing.expectEqualSlices(u32, &test_list, lifted);
}

test "CanonicalABI - invalid UTF-8" {
    var memory: [1024]u8 = undefined;
    @memset(&memory, 0);

    // Put invalid UTF-8 in memory
    memory[0] = 0xFF; // Invalid UTF-8 byte

    var abi = CanonicalABI.init(std.testing.allocator, &memory, null);

    // Should fail to lift
    const result = abi.liftString(0, 1);
    try std.testing.expectError(error.InvalidUtf8, result);
}

test "CanonicalABI - out of bounds" {
    var memory: [10]u8 = undefined;

    var abi = CanonicalABI.init(std.testing.allocator, &memory, null);

    // Try to access beyond memory bounds
    const result = abi.liftString(0, 100);
    try std.testing.expectError(error.OutOfBounds, result);
}
