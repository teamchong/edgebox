/// Native Implementation Registry for Component Model
/// Maps WIT function calls to Zig implementations

const std = @import("std");
const hashmap_helper = @import("../utils/hashmap_helper.zig");

/// File statistics record (matches WIT filesystem::file-stat)
pub const FileStat = struct {
    size: u64,
    mode: u32,
    is_file: bool,
    is_directory: bool,
    modified_time: i64,
    created_time: i64,
    accessed_time: i64,
};

/// Directory entry record (matches WIT filesystem::dir-entry)
pub const DirEntry = struct {
    name: []const u8,
    is_file: bool,
    is_directory: bool,
};

/// Process output record (matches WIT process::process-output)
pub const ProcessOutput = struct {
    exit_code: i32,
    stdout: []const u8,
    stderr: []const u8,
};

/// Spawn options record (matches WIT process::spawn-options)
pub const SpawnOptions = struct {
    timeout_seconds: u32,
    stdin_data: []const u8,
    capture_output: bool,
};

/// HTTP header record (matches WIT http::http-header)
pub const HttpHeader = struct {
    name: []const u8,
    value: []const u8,
};

/// HTTP request record (matches WIT http::http-request)
pub const HttpRequest = struct {
    url: []const u8,
    method: u32, // http-method enum as u32
    headers: []HttpHeader,
    body: ?[]const u8, // optional body
    timeout_ms: u32,
};

/// HTTP response record (matches WIT http::http-response)
pub const HttpResponse = struct {
    status: u16,
    ok: bool,
    body: []const u8,
    headers: []HttpHeader,
};

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
    list_string: [][]const u8,
    resource_handle: u32,
    void: void,

    // Result type support for Component Model result<T, E>
    // For result<_, E> where success type is void
    ok_void: void,
    // For result<string, E>
    ok_string: []const u8,
    // For result<list<u8>, E>
    ok_list_u8: []const u8,
    // For result<file-stat, E>
    ok_file_stat: FileStat,
    // For result<list<dir-entry>, E>
    ok_dir_entries: []DirEntry,
    // For result<process-output, E>
    ok_process_output: ProcessOutput,
    // For result<u32, E> (spawn ID)
    ok_spawn_id: u32,
    // For result<http-response, E>
    ok_http_response: HttpResponse,
    // For result<u32, E> (request ID)
    ok_request_id: u32,
    // For result<T, E> error case - holds error enum discriminant
    err: u32,

    // Record types (for non-result usage)
    file_stat: FileStat,
    dir_entry: DirEntry,
    process_output: ProcessOutput,
    spawn_options: SpawnOptions,
    http_header: HttpHeader,
    http_request: HttpRequest,
    http_response: HttpResponse,
    list_http_header: []HttpHeader,

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

    /// Helper to extract bool value
    pub fn asBool(self: Value) !bool {
        return switch (self) {
            .bool => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract ok_string from result
    pub fn asOkString(self: Value) ![]const u8 {
        return switch (self) {
            .ok_string => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract ok_file_stat from result
    pub fn asOkFileStat(self: Value) !FileStat {
        return switch (self) {
            .ok_file_stat => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract ok_dir_entries from result
    pub fn asOkDirEntries(self: Value) ![]DirEntry {
        return switch (self) {
            .ok_dir_entries => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract ok_list_u8 from result
    pub fn asOkListU8(self: Value) ![]const u8 {
        return switch (self) {
            .ok_list_u8 => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract list_string
    pub fn asListString(self: Value) ![][]const u8 {
        return switch (self) {
            .list_string => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract process_output value
    pub fn asProcessOutput(self: Value) !ProcessOutput {
        return switch (self) {
            .process_output => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract spawn_options value
    pub fn asSpawnOptions(self: Value) !SpawnOptions {
        return switch (self) {
            .spawn_options => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract ok_process_output from result
    pub fn asOkProcessOutput(self: Value) !ProcessOutput {
        return switch (self) {
            .ok_process_output => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract ok_spawn_id from result
    pub fn asOkSpawnId(self: Value) !u32 {
        return switch (self) {
            .ok_spawn_id => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract http_header value
    pub fn asHttpHeader(self: Value) !HttpHeader {
        return switch (self) {
            .http_header => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract http_request value
    pub fn asHttpRequest(self: Value) !HttpRequest {
        return switch (self) {
            .http_request => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract http_response value
    pub fn asHttpResponse(self: Value) !HttpResponse {
        return switch (self) {
            .http_response => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract list_http_header
    pub fn asListHttpHeader(self: Value) ![]HttpHeader {
        return switch (self) {
            .list_http_header => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract ok_http_response from result
    pub fn asOkHttpResponse(self: Value) !HttpResponse {
        return switch (self) {
            .ok_http_response => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract ok_request_id from result
    pub fn asOkRequestId(self: Value) !u32 {
        return switch (self) {
            .ok_request_id => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Helper to extract error discriminant
    pub fn asErr(self: Value) !u32 {
        return switch (self) {
            .err => |v| v,
            else => error.TypeMismatch,
        };
    }

    /// Check if value is a success result
    pub fn isOk(self: Value) bool {
        return switch (self) {
            .ok_void, .ok_string, .ok_list_u8, .ok_file_stat, .ok_dir_entries, .ok_process_output, .ok_spawn_id, .ok_http_response, .ok_request_id => true,
            else => false,
        };
    }

    /// Check if value is an error result
    pub fn isErr(self: Value) bool {
        return switch (self) {
            .err => true,
            else => false,
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

/// Native implementation registry (wyhash for fast lookups)
pub const NativeRegistry = struct {
    allocator: std.mem.Allocator,
    implementations: hashmap_helper.StringHashMap(Implementation),

    pub fn init(allocator: std.mem.Allocator) NativeRegistry {
        return .{
            .allocator = allocator,
            .implementations = hashmap_helper.StringHashMap(Implementation).init(allocator),
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
