/// Component Adapter - Orchestrates Component Model layers
/// Connects WIT parser → canonical ABI → native registry → implementations

const std = @import("std");
const wit_parser = @import("wit_parser.zig");
const native_registry = @import("native_registry.zig");
const canonical_abi = @import("canonical_abi.zig");
const timer = @import("timer.zig");

const Value = native_registry.Value;
const NativeRegistry = native_registry.NativeRegistry;

/// Function type for registering interface implementations
pub const RegisterFn = *const fn (*NativeRegistry) anyerror!void;

/// Component Adapter - Main orchestrator struct
pub const ComponentAdapter = struct {
    allocator: std.mem.Allocator,
    package: wit_parser.Package,
    registry: NativeRegistry,
    abi: canonical_abi.CanonicalABI,

    /// Initialize adapter with WIT source
    pub fn init(allocator: std.mem.Allocator, wit_source: []const u8) !ComponentAdapter {
        // Parse WIT file
        const package = try wit_parser.parse(allocator, wit_source);
        errdefer package.deinit(allocator);

        // Initialize native registry
        const registry = NativeRegistry.init(allocator);

        // Initialize canonical ABI with mock memory (for PoC testing)
        var mock_memory: [4096]u8 = undefined;
        const abi = canonical_abi.CanonicalABI.init(allocator, &mock_memory, null);

        return ComponentAdapter{
            .allocator = allocator,
            .package = package,
            .registry = registry,
            .abi = abi,
        };
    }

    /// Clean up resources
    pub fn deinit(self: *ComponentAdapter) void {
        self.package.deinit(self.allocator);
        self.registry.deinit();
        // ABI doesn't own memory in PoC, no deinit needed
    }

    /// Register an interface implementation (e.g., timer)
    pub fn registerInterface(self: *ComponentAdapter, register_fn: RegisterFn) !void {
        try register_fn(&self.registry);
    }

    /// Call a function through the adapter
    pub fn callFunction(
        self: *ComponentAdapter,
        interface_name: []const u8,
        function_name: []const u8,
        args: []const Value,
    ) !Value {
        // 1. Find interface
        const iface = self.findInterface(interface_name) orelse
            return error.InterfaceNotFound;

        // 2. Find function
        const func = self.findFunction(iface, function_name) orelse
            return error.FunctionNotFound;

        // 3. Validate argument count
        if (args.len != func.params.len) return error.WrongArgCount;

        // 4. Lift arguments (mostly passthrough for basic types)
        var lifted_args = try self.allocator.alloc(Value, args.len);
        defer self.allocator.free(lifted_args);

        for (args, 0..) |arg, i| {
            lifted_args[i] = try self.liftValue(func.params[i].type, arg);
        }

        // 5. Call native implementation
        const result = try self.registry.call(
            interface_name,
            function_name,
            lifted_args,
        );

        // 6. Lower result
        if (func.result) |result_type| {
            return try self.lowerValue(result_type, result);
        }

        return Value{ .void = {} };
    }

    // --- Helper Functions ---

    /// Find interface by name
    fn findInterface(self: *ComponentAdapter, name: []const u8) ?*const wit_parser.Interface {
        for (self.package.interfaces) |*iface| {
            if (std.mem.eql(u8, iface.name, name)) return iface;
        }
        return null;
    }

    /// Find function within interface
    fn findFunction(
        _: *ComponentAdapter,
        iface: *const wit_parser.Interface,
        name: []const u8,
    ) ?*const wit_parser.Function {
        for (iface.functions) |*func| {
            if (std.mem.eql(u8, func.name, name)) return func;
        }
        return null;
    }

    /// Lift value from WIT type to Value (passthrough for basic types in PoC)
    /// Note: Result types (result<T, E>) are handled transparently - implementations
    /// return ok_* or err variants directly, and we pass them through unchanged.
    fn liftValue(self: *ComponentAdapter, wit_type: wit_parser.Type, value: Value) !Value {
        return switch (wit_type) {
            .u32 => value, // Passthrough
            .u64 => value, // Passthrough
            .bool => value, // Passthrough
            .string => {
                // For real WASM: use self.abi.liftString()
                // For PoC: passthrough
                return value;
            },
            .named => |name| {
                // Resolve type alias (e.g., timer-id -> u32, path -> string)
                const resolved = self.resolveType(name);
                return try self.liftValue(resolved, value);
            },
            else => value, // Passthrough for all other types in PoC (result, list, record)
        };
    }

    /// Lower value from Value to WIT type (symmetric to liftValue)
    /// Note: Result types (result<T, E>) are handled transparently - implementations
    /// return ok_* or err variants directly, and we pass them through unchanged.
    fn lowerValue(self: *ComponentAdapter, wit_type: wit_parser.Type, value: Value) !Value {
        return switch (wit_type) {
            .u32 => value,
            .u64 => value,
            .bool => value,
            .string => value,
            .named => |name| {
                const resolved = self.resolveType(name);
                return try self.lowerValue(resolved, value);
            },
            else => value, // Passthrough for all other types in PoC (result, list, record)
        };
    }

    /// Resolve named type aliases (e.g., timer-id -> u32, path -> string)
    fn resolveType(self: *ComponentAdapter, name: []const u8) wit_parser.Type {
        // Look up in package.types
        for (self.package.types) |typedef| {
            if (std.mem.eql(u8, typedef.name, name)) {
                return typedef.type;
            }
        }

        // Fallback heuristics for common patterns
        if (std.mem.endsWith(u8, name, "-id")) {
            return wit_parser.Type.u32; // ID types are typically u32
        }
        if (std.mem.eql(u8, name, "path")) {
            return wit_parser.Type.string; // path is typically string
        }

        // Default: treat as u32 (safe default for most cases)
        return wit_parser.Type.u32;
    }
};

// --- Tests ---

test "ComponentAdapter - initialization" {
    // Read the WIT file instead of embedding it
    const file = try std.fs.cwd().openFile("wit/edgebox-timer.wit", .{});
    defer file.close();

    const wit_source = try file.readToEndAlloc(std.testing.allocator, 10 * 1024);
    defer std.testing.allocator.free(wit_source);

    var adapter = try ComponentAdapter.init(
        std.testing.allocator,
        wit_source,
    );
    defer adapter.deinit();

    // Verify package was parsed
    try std.testing.expect(adapter.package.interfaces.len > 0);
    try std.testing.expectEqualStrings("timer", adapter.package.interfaces[0].name);
}

test "ComponentAdapter - function lookup" {
    const file = try std.fs.cwd().openFile("wit/edgebox-timer.wit", .{});
    defer file.close();

    const wit_source = try file.readToEndAlloc(std.testing.allocator, 10 * 1024);
    defer std.testing.allocator.free(wit_source);

    var adapter = try ComponentAdapter.init(
        std.testing.allocator,
        wit_source,
    );
    defer adapter.deinit();

    // Find interface
    const iface = adapter.findInterface("timer");
    try std.testing.expect(iface != null);

    // Find function
    const func = adapter.findFunction(iface.?, "set-timeout");
    try std.testing.expect(func != null);
    try std.testing.expectEqual(@as(usize, 2), func.?.params.len);
}

test "ComponentAdapter - type resolution" {
    const file = try std.fs.cwd().openFile("wit/edgebox-timer.wit", .{});
    defer file.close();

    const wit_source = try file.readToEndAlloc(std.testing.allocator, 10 * 1024);
    defer std.testing.allocator.free(wit_source);

    var adapter = try ComponentAdapter.init(
        std.testing.allocator,
        wit_source,
    );
    defer adapter.deinit();

    const resolved = adapter.resolveType("timer-id");
    try std.testing.expect(resolved == .u32);
}

test "ComponentAdapter - end to end timer" {
    const file = try std.fs.cwd().openFile("wit/edgebox-timer.wit", .{});
    defer file.close();

    const wit_source = try file.readToEndAlloc(std.testing.allocator, 10 * 1024);
    defer std.testing.allocator.free(wit_source);

    // Initialize adapter
    var adapter = try ComponentAdapter.init(
        std.testing.allocator,
        wit_source,
    );
    defer adapter.deinit();

    // Initialize timer implementation
    timer.init(std.testing.allocator);
    defer timer.deinit();

    // Register timer implementation
    try adapter.registerInterface(timer.registerTimerImpl);

    // Call set-timeout through adapter
    const set_result = try adapter.callFunction(
        "timer",
        "set-timeout",
        &[_]Value{
            Value{ .u32 = 100 }, // callback_id
            Value{ .u64 = 5000 }, // delay_ms
        },
    );

    // Verify timer ID returned
    try std.testing.expect(set_result == .u32);
    const timer_id = try set_result.asU32();
    try std.testing.expect(timer_id > 0);

    // Verify timer was created
    const info = timer.getTimerInfo(timer_id);
    try std.testing.expect(info != null);
    try std.testing.expectEqual(@as(u32, 100), info.?.callback_id);
    try std.testing.expectEqual(@as(u64, 5000), info.?.delay_or_interval);
    try std.testing.expect(!info.?.is_interval);

    // Call clear-timeout
    _ = try adapter.callFunction(
        "timer",
        "clear-timeout",
        &[_]Value{Value{ .u32 = timer_id }},
    );

    // Verify timer removed
    try std.testing.expect(timer.getTimerInfo(timer_id) == null);
}

test "ComponentAdapter - error handling" {
    const file = try std.fs.cwd().openFile("wit/edgebox-timer.wit", .{});
    defer file.close();

    const wit_source = try file.readToEndAlloc(std.testing.allocator, 10 * 1024);
    defer std.testing.allocator.free(wit_source);

    var adapter = try ComponentAdapter.init(
        std.testing.allocator,
        wit_source,
    );
    defer adapter.deinit();

    // Test interface not found
    const result1 = adapter.callFunction("invalid", "func", &[_]Value{});
    try std.testing.expectError(error.InterfaceNotFound, result1);

    // Test function not found
    const result2 = adapter.callFunction("timer", "invalid", &[_]Value{});
    try std.testing.expectError(error.FunctionNotFound, result2);

    // Test wrong arg count
    const result3 = adapter.callFunction("timer", "set-timeout", &[_]Value{});
    try std.testing.expectError(error.WrongArgCount, result3);
}
