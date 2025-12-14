/// HTTP Component Implementation
/// Wraps native HTTP operations for Component Model
///
/// Architecture:
/// This implementation wraps the native HTTP dispatch functions (httpDispatch opcodes)
/// which provide both synchronous and asynchronous HTTP request capabilities.
/// The native layer uses h2.Client (HTTP/2 + TLS 1.3) with security enforcement
/// via safe_fetch.zig (URL whitelisting, rate limiting, connection limits).

const std = @import("std");
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

// HTTP dispatch opcodes (from edgebox_wamr.zig)
const HTTP_OP_REQUEST: i32 = 0; // Sync request, returns status code
const HTTP_OP_GET_RESPONSE_LEN: i32 = 1; // Get response body length
const HTTP_OP_GET_RESPONSE: i32 = 2; // Retrieve response body
const HTTP_OP_START_ASYNC: i32 = 3; // Start async request (curl)
const HTTP_OP_POLL: i32 = 4; // Poll async status
const HTTP_OP_RESPONSE_LEN: i32 = 5; // Get async response length
const HTTP_OP_RESPONSE: i32 = 6; // Get async response
const HTTP_OP_FREE: i32 = 7; // Free async request

// External HTTP dispatch function
extern "c" fn __edgebox_http_dispatch(
    op: i32,
    arg1: u32,
    arg2: u32,
    arg3: u32,
    arg4: u32,
    arg5: u32,
    arg6: u32,
    arg7: u32,
    arg8: u32,
) i32;

var http_allocator: ?std.mem.Allocator = null;

pub fn init(allocator: std.mem.Allocator) void {
    http_allocator = allocator;
}

pub fn deinit() void {
    http_allocator = null;
}

/// Map native HTTP error codes to http-error enum discriminants
fn mapHttpError(code: i32) u32 {
    return switch (code) {
        -1 => @intFromEnum(HttpError.invalid_url),
        -2 => @intFromEnum(HttpError.connection_failed),
        -3 => @intFromEnum(HttpError.timeout),
        -403 => @intFromEnum(HttpError.permission_denied),
        -400 => @intFromEnum(HttpError.invalid_method),
        -500 => @intFromEnum(HttpError.invalid_response),
        -429 => @intFromEnum(HttpError.rate_limit_exceeded),
        else => @intFromEnum(HttpError.connection_failed), // default
    };
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

/// Parse JSON response to HttpResponse
/// JSON format: {"status": 200, "ok": true, "body": "...", "headers": {}}
fn parseJSONResponse(allocator: std.mem.Allocator, json_str: []const u8) !HttpResponse {
    // Simple JSON parser for our specific format
    var status: u16 = 0;
    var ok: bool = false;
    var body = std.ArrayListUnmanaged(u8){};
    errdefer body.deinit(allocator);

    // Parse status
    if (std.mem.indexOf(u8, json_str, "\"status\":")) |status_idx| {
        const status_start = status_idx + 9; // Skip "status":
        var i = status_start;
        while (i < json_str.len and (json_str[i] == ' ' or json_str[i] == '\t')) : (i += 1) {}
        var status_end = i;
        while (status_end < json_str.len and json_str[status_end] >= '0' and json_str[status_end] <= '9') : (status_end += 1) {}
        if (status_end > i) {
            status = std.fmt.parseInt(u16, json_str[i..status_end], 10) catch 0;
        }
    }

    // Parse ok
    if (std.mem.indexOf(u8, json_str, "\"ok\":")) |ok_idx| {
        const ok_start = ok_idx + 5; // Skip "ok":
        var i = ok_start;
        while (i < json_str.len and (json_str[i] == ' ' or json_str[i] == '\t')) : (i += 1) {}
        if (i + 4 <= json_str.len and std.mem.eql(u8, json_str[i .. i + 4], "true")) {
            ok = true;
        }
    }

    // Parse body
    if (std.mem.indexOf(u8, json_str, "\"body\":")) |body_idx| {
        const body_start = body_idx + 7; // Skip "body":
        var i = body_start;
        while (i < json_str.len and (json_str[i] == ' ' or json_str[i] == '\t')) : (i += 1) {}
        if (i < json_str.len and json_str[i] == '"') {
            i += 1; // Skip opening quote
            var in_escape = false;
            while (i < json_str.len) : (i += 1) {
                if (in_escape) {
                    // Handle escape sequences
                    const c = json_str[i];
                    switch (c) {
                        'n' => try body.append(allocator, '\n'),
                        't' => try body.append(allocator, '\t'),
                        'r' => try body.append(allocator, '\r'),
                        '"' => try body.append(allocator, '"'),
                        '\\' => try body.append(allocator, '\\'),
                        else => {
                            try body.append(allocator, '\\');
                            try body.append(allocator, c);
                        },
                    }
                    in_escape = false;
                } else if (json_str[i] == '\\') {
                    in_escape = true;
                } else if (json_str[i] == '"') {
                    break; // End of string
                } else {
                    try body.append(allocator, json_str[i]);
                }
            }
        }
    }

    return HttpResponse{
        .status = status,
        .ok = ok,
        .body = try body.toOwnedSlice(allocator),
        .headers = &[_]HttpHeader{}, // Empty for now
    };
}

/// Synchronous HTTP request
/// WIT: fetch: func(request: http-request) -> result<http-response, http-error>
fn fetchImpl(args: []const Value) anyerror!Value {
    const allocator = http_allocator orelse return error.NotInitialized;
    const request = try args[0].asHttpRequest();

    // 1. Build headers string (\r\n-delimited)
    var headers_buf = std.ArrayListUnmanaged(u8){};
    defer headers_buf.deinit(allocator);

    for (request.headers) |header| {
        try headers_buf.appendSlice(allocator, header.name);
        try headers_buf.appendSlice(allocator, ": ");
        try headers_buf.appendSlice(allocator, header.value);
        try headers_buf.appendSlice(allocator, "\r\n");
    }

    // 2. Convert method enum to string
    const method_str = methodToString(request.method);

    // 3. Call httpDispatch with HTTP_OP_REQUEST
    const status_code = __edgebox_http_dispatch(
        HTTP_OP_REQUEST,
        @intCast(@intFromPtr(request.url.ptr)),
        @intCast(request.url.len),
        @intCast(@intFromPtr(method_str.ptr)),
        @intCast(method_str.len),
        @intCast(@intFromPtr(headers_buf.items.ptr)),
        @intCast(headers_buf.items.len),
        if (request.body) |body| @intCast(@intFromPtr(body.ptr)) else 0,
        if (request.body) |body| @intCast(body.len) else 0,
    );

    if (status_code < 0) {
        return Value{ .err = mapHttpError(status_code) };
    }

    // 4. Get response body length
    const response_len = __edgebox_http_dispatch(
        HTTP_OP_GET_RESPONSE_LEN,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
    );

    if (response_len < 0) {
        return Value{ .err = @intFromEnum(HttpError.invalid_response) };
    }

    // 5. Get response body
    const response_body = try allocator.alloc(u8, @intCast(response_len));
    errdefer allocator.free(response_body);

    const read_result = __edgebox_http_dispatch(
        HTTP_OP_GET_RESPONSE,
        @intCast(@intFromPtr(response_body.ptr)),
        @intCast(response_body.len),
        0,
        0,
        0,
        0,
        0,
        0,
    );

    if (read_result < 0) {
        allocator.free(response_body);
        return Value{ .err = @intFromEnum(HttpError.invalid_response) };
    }

    // 6. Return result<http-response, http-error>
    return Value{ .ok_http_response = .{
        .status = @intCast(status_code),
        .ok = status_code >= 200 and status_code < 300,
        .body = response_body,
        .headers = &[_]HttpHeader{}, // Empty for now
    } };
}

/// Start async HTTP request
/// WIT: fetch-start: func(request: http-request) -> result<u32, http-error>
fn fetchStartImpl(args: []const Value) anyerror!Value {
    const allocator = http_allocator orelse return error.NotInitialized;
    const request = try args[0].asHttpRequest();

    // Build headers string
    var headers_buf = std.ArrayListUnmanaged(u8){};
    defer headers_buf.deinit(allocator);

    for (request.headers) |header| {
        try headers_buf.appendSlice(allocator, header.name);
        try headers_buf.appendSlice(allocator, ": ");
        try headers_buf.appendSlice(allocator, header.value);
        try headers_buf.appendSlice(allocator, "\r\n");
    }

    const method_str = methodToString(request.method);

    // Call HTTP_OP_START_ASYNC
    const request_id = __edgebox_http_dispatch(
        HTTP_OP_START_ASYNC,
        @intCast(@intFromPtr(request.url.ptr)),
        @intCast(request.url.len),
        @intCast(@intFromPtr(method_str.ptr)),
        @intCast(method_str.len),
        @intCast(@intFromPtr(headers_buf.items.ptr)),
        @intCast(headers_buf.items.len),
        if (request.body) |body| @intCast(@intFromPtr(body.ptr)) else 0,
        if (request.body) |body| @intCast(body.len) else 0,
    );

    if (request_id < 0) {
        return Value{ .err = mapHttpError(request_id) };
    }

    return Value{ .ok_request_id = @intCast(request_id) };
}

/// Poll async request status
/// WIT: fetch-poll: func(request-id: u32) -> result<u32, http-error>
/// Returns: 0=pending, 1=complete
fn fetchPollImpl(args: []const Value) anyerror!Value {
    const request_id = try args[0].asU32();

    const status = __edgebox_http_dispatch(
        HTTP_OP_POLL,
        request_id,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
    );

    if (status < 0) {
        return Value{ .err = mapHttpError(status) };
    }

    return Value{ .ok_request_id = @intCast(status) };
}

/// Get async response
/// WIT: fetch-response: func(request-id: u32) -> result<http-response, http-error>
fn fetchResponseImpl(args: []const Value) anyerror!Value {
    const allocator = http_allocator orelse return error.NotInitialized;
    const request_id = try args[0].asU32();

    // Get response length
    const response_len = __edgebox_http_dispatch(
        HTTP_OP_RESPONSE_LEN,
        request_id,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
    );

    if (response_len < 0) {
        return Value{ .err = @intFromEnum(HttpError.invalid_response) };
    }

    // Get response (JSON format)
    const json_response = try allocator.alloc(u8, @intCast(response_len));
    defer allocator.free(json_response);

    const read_result = __edgebox_http_dispatch(
        HTTP_OP_RESPONSE,
        request_id,
        @intCast(@intFromPtr(json_response.ptr)),
        @intCast(json_response.len),
        0,
        0,
        0,
        0,
        0,
    );

    if (read_result < 0) {
        return Value{ .err = @intFromEnum(HttpError.invalid_response) };
    }

    // Parse JSON response
    const response = try parseJSONResponse(allocator, json_response);
    return Value{ .ok_http_response = response };
}

/// Free async request resources
/// WIT: fetch-free: func(request-id: u32) -> result<_, http-error>
fn fetchFreeImpl(args: []const Value) anyerror!Value {
    const request_id = try args[0].asU32();

    const result = __edgebox_http_dispatch(
        HTTP_OP_FREE,
        request_id,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
    );

    if (result < 0) {
        return Value{ .err = mapHttpError(result) };
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

// Tests for internal functions
test "HTTP - mapHttpError" {
    const HttpError_test = enum(u32) {
        invalid_url = 0,
        connection_failed = 1,
        timeout = 2,
        permission_denied = 3,
        invalid_method = 4,
        invalid_response = 5,
        rate_limit_exceeded = 6,
    };

    try std.testing.expectEqual(@as(u32, @intFromEnum(HttpError_test.invalid_url)), mapHttpError(-1));
    try std.testing.expectEqual(@as(u32, @intFromEnum(HttpError_test.connection_failed)), mapHttpError(-2));
    try std.testing.expectEqual(@as(u32, @intFromEnum(HttpError_test.timeout)), mapHttpError(-3));
    try std.testing.expectEqual(@as(u32, @intFromEnum(HttpError_test.permission_denied)), mapHttpError(-403));
    try std.testing.expectEqual(@as(u32, @intFromEnum(HttpError_test.invalid_method)), mapHttpError(-400));
    try std.testing.expectEqual(@as(u32, @intFromEnum(HttpError_test.invalid_response)), mapHttpError(-500));
    try std.testing.expectEqual(@as(u32, @intFromEnum(HttpError_test.rate_limit_exceeded)), mapHttpError(-429));
    try std.testing.expectEqual(@as(u32, @intFromEnum(HttpError_test.connection_failed)), mapHttpError(-999));
}

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

test "HTTP - parseJSONResponse simple" {
    const json = "{\"status\": 200, \"ok\": true, \"body\": \"hello\", \"headers\": {}}";
    const response = try parseJSONResponse(std.testing.allocator, json);
    defer std.testing.allocator.free(response.body);

    try std.testing.expectEqual(@as(u16, 200), response.status);
    try std.testing.expect(response.ok);
    try std.testing.expectEqualStrings("hello", response.body);
    try std.testing.expectEqual(@as(usize, 0), response.headers.len);
}

test "HTTP - parseJSONResponse with escape sequences" {
    const json = "{\"status\": 201, \"ok\": true, \"body\": \"line1\\nline2\\ttab\", \"headers\": {}}";
    const response = try parseJSONResponse(std.testing.allocator, json);
    defer std.testing.allocator.free(response.body);

    try std.testing.expectEqual(@as(u16, 201), response.status);
    try std.testing.expect(response.ok);
    try std.testing.expectEqualStrings("line1\nline2\ttab", response.body);
}

test "HTTP - parseJSONResponse error status" {
    const json = "{\"status\": 404, \"ok\": false, \"body\": \"not found\", \"headers\": {}}";
    const response = try parseJSONResponse(std.testing.allocator, json);
    defer std.testing.allocator.free(response.body);

    try std.testing.expectEqual(@as(u16, 404), response.status);
    try std.testing.expect(!response.ok);
    try std.testing.expectEqualStrings("not found", response.body);
}

test "HTTP - parseJSONResponse empty body" {
    const json = "{\"status\": 204, \"ok\": true, \"body\": \"\", \"headers\": {}}";
    const response = try parseJSONResponse(std.testing.allocator, json);
    defer std.testing.allocator.free(response.body);

    try std.testing.expectEqual(@as(u16, 204), response.status);
    try std.testing.expect(response.ok);
    try std.testing.expectEqual(@as(usize, 0), response.body.len);
}

test "HTTP - parseJSONResponse with spaces" {
    const json = "{  \"status\" :  200 ,  \"ok\" : true ,  \"body\" :  \"data\" ,  \"headers\" : {}  }";
    const response = try parseJSONResponse(std.testing.allocator, json);
    defer std.testing.allocator.free(response.body);

    try std.testing.expectEqual(@as(u16, 200), response.status);
    try std.testing.expect(response.ok);
    try std.testing.expectEqualStrings("data", response.body);
}

test "HTTP - HttpHeader construction" {
    const header = HttpHeader{
        .name = "Content-Type",
        .value = "application/json",
    };

    try std.testing.expectEqualStrings("Content-Type", header.name);
    try std.testing.expectEqualStrings("application/json", header.value);
}

test "HTTP - HttpRequest construction with body" {
    var headers = [_]HttpHeader{
        .{ .name = "Content-Type", .value = "text/plain" },
        .{ .name = "Authorization", .value = "Bearer token123" },
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
    try std.testing.expectEqual(@as(usize, 2), request.headers.len);
    try std.testing.expect(request.body != null);
    try std.testing.expectEqualStrings("test data", request.body.?);
    try std.testing.expectEqual(@as(u32, 30000), request.timeout_ms);
}

test "HTTP - HttpRequest construction without body" {
    const request = HttpRequest{
        .url = "https://api.example.com/data",
        .method = 0, // GET
        .headers = &[_]HttpHeader{},
        .body = null,
        .timeout_ms = 0,
    };

    try std.testing.expectEqualStrings("https://api.example.com/data", request.url);
    try std.testing.expectEqual(@as(u32, 0), request.method);
    try std.testing.expectEqual(@as(usize, 0), request.headers.len);
    try std.testing.expect(request.body == null);
    try std.testing.expectEqual(@as(u32, 0), request.timeout_ms);
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
    try std.testing.expectEqual(@as(usize, 0), response.headers.len);
}

test "HTTP - status code validation" {
    // 2xx success
    const response_200 = HttpResponse{
        .status = 200,
        .ok = true,
        .body = "",
        .headers = &[_]HttpHeader{},
    };
    try std.testing.expect(response_200.ok);
    try std.testing.expect(response_200.status >= 200 and response_200.status < 300);

    // 4xx client error
    const response_404 = HttpResponse{
        .status = 404,
        .ok = false,
        .body = "not found",
        .headers = &[_]HttpHeader{},
    };
    try std.testing.expect(!response_404.ok);
    try std.testing.expect(response_404.status >= 400 and response_404.status < 500);

    // 5xx server error
    const response_500 = HttpResponse{
        .status = 500,
        .ok = false,
        .body = "server error",
        .headers = &[_]HttpHeader{},
    };
    try std.testing.expect(!response_500.ok);
    try std.testing.expect(response_500.status >= 500);
}
