/// HTTP Component Implementation
/// Uses h2.Client directly for HTTP/2 + TLS 1.3 support
///
/// Architecture:
/// This implementation directly uses metal0's h2 package (HTTP/2 + TLS 1.3)
/// with security enforcement via safe_fetch.zig (URL whitelisting, rate limiting).
/// No dispatch layer - Component Model impl calls h2.Client directly.

const std = @import("std");
const h2 = @import("h2");
const safe_fetch = @import("../../safe_fetch.zig");
const async_runtime = @import("../async_runtime.zig");
const NativeRegistry = @import("../native_registry.zig").NativeRegistry;
const Value = @import("../native_registry.zig").Value;
const HttpHeader = @import("../native_registry.zig").HttpHeader;
const HttpRequest = @import("../native_registry.zig").HttpRequest;
const HttpResponse = @import("../native_registry.zig").HttpResponse;

// HTTP error codes (matching WIT http-error enum)
const HttpError = enum(u32) {
    invalid_url = 0,
    connection_failed = 1,
    timeout = 2,
    permission_denied = 3,
    invalid_method = 4,
    invalid_response = 5,
    rate_limit_exceeded = 6,
};

var http_allocator: ?std.mem.Allocator = null;
var g_security_policy: safe_fetch.SecurityPolicy = safe_fetch.SecurityPolicy.permissive;

// Global response storage for sync requests (protected by mutex)
var g_http_mutex: std.Thread.Mutex = .{};
var g_http_response: ?[]u8 = null;
var g_http_status: i32 = 0;

// Async request state (u8 backing for atomic compatibility)
const AsyncState = enum(u8) {
    pending = 0, // Created, not started
    running = 1, // Executing in background
    complete = 2, // Finished successfully
    failed = 3, // Finished with error
};

// Async request storage
const AsyncRequest = struct {
    url: []const u8,
    method: []const u8,
    headers: []const h2.ExtraHeader,
    body: ?[]const u8,
    response: ?[]u8 = null,
    status: i32 = 0,
    state: std.atomic.Value(AsyncState) = std.atomic.Value(AsyncState).init(.pending),
    thread_id: ?u32 = null, // Async runtime task ID
    allocator: std.mem.Allocator,

    fn deinit(self: *AsyncRequest) void {
        self.allocator.free(self.url);
        self.allocator.free(self.method);
        for (self.headers) |h| {
            self.allocator.free(h.name);
            self.allocator.free(h.value);
        }
        self.allocator.free(self.headers);
        if (self.body) |b| self.allocator.free(b);
        if (self.response) |r| self.allocator.free(r);
    }

    fn isComplete(self: *const AsyncRequest) bool {
        const s = self.state.load(.acquire);
        return s == .complete or s == .failed;
    }
};

var g_async_requests: [64]?*AsyncRequest = [_]?*AsyncRequest{null} ** 64;
var g_next_request_id: u32 = 0;
var g_async_mutex: std.Thread.Mutex = .{};

pub fn init(allocator: std.mem.Allocator) void {
    http_allocator = allocator;
}

pub fn deinit() void {
    // Free any pending async requests
    for (&g_async_requests) |*maybe_req| {
        if (maybe_req.*) |req| {
            req.deinit();
            http_allocator.?.destroy(req);
            maybe_req.* = null;
        }
    }

    // Free sync response
    g_http_mutex.lock();
    if (g_http_response) |resp| {
        http_allocator.?.free(resp);
        g_http_response = null;
    }
    g_http_mutex.unlock();

    http_allocator = null;
}

/// Set security policy (called from runtime config)
pub fn setSecurityPolicy(policy: safe_fetch.SecurityPolicy) void {
    g_security_policy = policy;
}

/// Convert http-method enum (u32) to string
fn methodToString(method: u32) []const u8 {
    return switch (method) {
        0 => "GET",
        1 => "POST",
        2 => "PUT",
        3 => "DELETE",
        4 => "PATCH",
        5 => "HEAD",
        6 => "OPTIONS",
        else => "GET", // default
    };
}

/// Synchronous HTTP request using h2.Client directly
/// WIT: fetch: func(request: http-request) -> result<http-response, http-error>
fn fetchImpl(args: []const Value) anyerror!Value {
    const allocator = http_allocator orelse return error.NotInitialized;
    const request = try args[0].asHttpRequest();

    // Security check: URL allowlist
    if (!safe_fetch.isUrlAllowed(request.url, g_security_policy)) {
        return Value{ .err = @intFromEnum(HttpError.permission_denied) };
    }

    // Build extra headers (h2.ExtraHeader format)
    var extra_headers = std.ArrayListUnmanaged(h2.ExtraHeader){};
    defer extra_headers.deinit(allocator);

    for (request.headers) |header| {
        extra_headers.append(allocator, .{
            .name = header.name,
            .value = header.value,
        }) catch return Value{ .err = @intFromEnum(HttpError.connection_failed) };
    }

    // Get method string
    const method_str = methodToString(request.method);

    // Create h2 client and make request
    var client = h2.Client.init(allocator);
    defer client.deinit();

    var response = client.request(method_str, request.url, extra_headers.items, request.body) catch {
        return Value{ .err = @intFromEnum(HttpError.connection_failed) };
    };
    defer response.deinit();

    // Copy response body (h2.Response owns memory)
    const body_data = allocator.dupe(u8, response.body) catch {
        return Value{ .err = @intFromEnum(HttpError.connection_failed) };
    };

    // Store in global for potential GET_RESPONSE calls (backward compatibility)
    g_http_mutex.lock();
    if (g_http_response) |old| allocator.free(old);
    g_http_response = body_data;
    g_http_status = @intCast(response.status);
    g_http_mutex.unlock();

    // Return result<http-response, http-error>
    return Value{ .ok_http_response = .{
        .status = @intCast(response.status),
        .ok = response.status >= 200 and response.status < 300,
        .body = body_data,
        .headers = &[_]HttpHeader{}, // TODO: parse response headers if needed
    } };
}

/// Worker function for background HTTP execution
fn httpWorker(ctx: ?*anyopaque) void {
    const async_req: *AsyncRequest = @ptrCast(@alignCast(ctx.?));
    async_req.state.store(.running, .release);

    const allocator = async_req.allocator;

    // Execute HTTP request
    var client = h2.Client.init(allocator);
    defer client.deinit();

    if (client.request(async_req.method, async_req.url, async_req.headers, async_req.body)) |resp_val| {
        var resp = resp_val;
        async_req.response = allocator.dupe(u8, resp.body) catch null;
        async_req.status = @intCast(resp.status);
        async_req.state.store(.complete, .release);
        resp.deinit();
    } else |_| {
        async_req.status = -1;
        async_req.state.store(.failed, .release);
    }
}

/// Start async HTTP request
/// WIT: fetch-start: func(request: http-request) -> result<u32, http-error>
fn fetchStartImpl(args: []const Value) anyerror!Value {
    const allocator = http_allocator orelse return error.NotInitialized;
    const request = try args[0].asHttpRequest();

    // Security check: URL allowlist
    if (!safe_fetch.isUrlAllowed(request.url, g_security_policy)) {
        return Value{ .err = @intFromEnum(HttpError.permission_denied) };
    }

    // Allocate async request
    const async_req = allocator.create(AsyncRequest) catch {
        return Value{ .err = @intFromEnum(HttpError.connection_failed) };
    };

    // Copy request data
    async_req.* = .{
        .url = allocator.dupe(u8, request.url) catch {
            allocator.destroy(async_req);
            return Value{ .err = @intFromEnum(HttpError.connection_failed) };
        },
        .method = allocator.dupe(u8, methodToString(request.method)) catch {
            allocator.free(async_req.url);
            allocator.destroy(async_req);
            return Value{ .err = @intFromEnum(HttpError.connection_failed) };
        },
        .headers = blk: {
            var headers = allocator.alloc(h2.ExtraHeader, request.headers.len) catch {
                allocator.free(async_req.url);
                allocator.free(async_req.method);
                allocator.destroy(async_req);
                return Value{ .err = @intFromEnum(HttpError.connection_failed) };
            };
            for (request.headers, 0..) |h, i| {
                headers[i] = .{
                    .name = allocator.dupe(u8, h.name) catch "",
                    .value = allocator.dupe(u8, h.value) catch "",
                };
            }
            break :blk headers;
        },
        .body = if (request.body) |b| allocator.dupe(u8, b) catch null else null,
        .allocator = allocator,
    };

    // Store in global array (protected by mutex for thread safety)
    g_async_mutex.lock();
    const request_id = g_next_request_id;
    g_next_request_id = (g_next_request_id + 1) % 64;

    if (g_async_requests[request_id]) |old| {
        old.deinit();
        allocator.destroy(old);
    }
    g_async_requests[request_id] = async_req;
    g_async_mutex.unlock();

    // Spawn background thread for TRUE ASYNC execution
    if (async_runtime.getRuntime()) |rt| {
        async_req.thread_id = rt.spawn(httpWorker, async_req) catch null;
        if (async_req.thread_id == null) {
            // Fallback: execute synchronously if spawn fails
            httpWorker(async_req);
        }
    } else {
        // Fallback: execute synchronously if runtime not available
        httpWorker(async_req);
    }

    return Value{ .ok_request_id = request_id };
}

/// Poll async request status
/// WIT: fetch-poll: func(request-id: u32) -> result<u32, http-error>
/// Returns: 0=pending, 1=complete
fn fetchPollImpl(args: []const Value) anyerror!Value {
    const request_id = try args[0].asU32();

    if (request_id >= 64) {
        return Value{ .err = @intFromEnum(HttpError.invalid_url) };
    }

    const async_req = g_async_requests[request_id] orelse {
        return Value{ .err = @intFromEnum(HttpError.invalid_url) };
    };

    // Check state atomically (non-blocking)
    return Value{ .ok_request_id = if (async_req.isComplete()) @as(u32, 1) else @as(u32, 0) };
}

/// Get async response
/// WIT: fetch-response: func(request-id: u32) -> result<http-response, http-error>
fn fetchResponseImpl(args: []const Value) anyerror!Value {
    const allocator = http_allocator orelse return error.NotInitialized;
    const request_id = try args[0].asU32();

    if (request_id >= 64) {
        return Value{ .err = @intFromEnum(HttpError.invalid_url) };
    }

    const async_req = g_async_requests[request_id] orelse {
        return Value{ .err = @intFromEnum(HttpError.invalid_url) };
    };

    // Check state
    const state = async_req.state.load(.acquire);
    if (state == .pending or state == .running) {
        return Value{ .err = @intFromEnum(HttpError.timeout) };
    }

    if (state == .failed or async_req.status < 0) {
        return Value{ .err = @intFromEnum(HttpError.connection_failed) };
    }

    // Copy response body (async_req owns memory until fetchFree)
    const body = if (async_req.response) |r|
        allocator.dupe(u8, r) catch &[_]u8{}
    else
        &[_]u8{};

    return Value{ .ok_http_response = .{
        .status = @intCast(async_req.status),
        .ok = async_req.status >= 200 and async_req.status < 300,
        .body = body,
        .headers = &[_]HttpHeader{},
    } };
}

/// Free async request resources
/// WIT: fetch-free: func(request-id: u32) -> result<_, http-error>
fn fetchFreeImpl(args: []const Value) anyerror!Value {
    const allocator = http_allocator orelse return error.NotInitialized;
    const request_id = try args[0].asU32();

    if (request_id >= 64) {
        return Value{ .err = @intFromEnum(HttpError.invalid_url) };
    }

    if (g_async_requests[request_id]) |async_req| {
        async_req.deinit();
        allocator.destroy(async_req);
        g_async_requests[request_id] = null;
    }

    return Value{ .ok_void = {} };
}

/// Register HTTP implementations with the native registry
pub fn registerHttpImpl(registry: *NativeRegistry) !void {
    try registry.register("http", "fetch", fetchImpl);
    try registry.register("http", "fetch-start", fetchStartImpl);
    try registry.register("http", "fetch-poll", fetchPollImpl);
    try registry.register("http", "fetch-response", fetchResponseImpl);
    try registry.register("http", "fetch-free", fetchFreeImpl);
}

// Tests
test "HTTP - methodToString" {
    try std.testing.expectEqualStrings("GET", methodToString(0));
    try std.testing.expectEqualStrings("POST", methodToString(1));
    try std.testing.expectEqualStrings("PUT", methodToString(2));
    try std.testing.expectEqualStrings("DELETE", methodToString(3));
    try std.testing.expectEqualStrings("PATCH", methodToString(4));
    try std.testing.expectEqualStrings("HEAD", methodToString(5));
    try std.testing.expectEqualStrings("OPTIONS", methodToString(6));
    try std.testing.expectEqualStrings("GET", methodToString(999));
}

test "HTTP - HttpHeader construction" {
    const header = HttpHeader{
        .name = "Content-Type",
        .value = "application/json",
    };

    try std.testing.expectEqualStrings("Content-Type", header.name);
    try std.testing.expectEqualStrings("application/json", header.value);
}

test "HTTP - HttpRequest construction" {
    var headers = [_]HttpHeader{
        .{ .name = "Content-Type", .value = "text/plain" },
    };

    const request = HttpRequest{
        .url = "https://api.example.com/data",
        .method = 1, // POST
        .headers = headers[0..],
        .body = "test data",
        .timeout_ms = 30000,
    };

    try std.testing.expectEqualStrings("https://api.example.com/data", request.url);
    try std.testing.expectEqual(@as(u32, 1), request.method);
    try std.testing.expectEqual(@as(usize, 1), request.headers.len);
    try std.testing.expectEqualStrings("test data", request.body.?);
}

test "HTTP - HttpResponse construction" {
    const response = HttpResponse{
        .status = 200,
        .ok = true,
        .body = "response body",
        .headers = &[_]HttpHeader{},
    };

    try std.testing.expectEqual(@as(u16, 200), response.status);
    try std.testing.expect(response.ok);
    try std.testing.expectEqualStrings("response body", response.body);
}
