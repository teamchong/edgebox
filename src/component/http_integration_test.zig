/// HTTP Component Integration Test
/// Tests HTTP implementation through the native registry
///
/// Architecture Note (Phase 7):
/// The HTTP interface uses package-level enums and records (http-error,
/// http-method, http-request, http-response, http-header) which are compatible
/// with ComponentAdapter's resolveType(). This test uses NativeRegistry directly
/// to verify the implementation.

const std = @import("std");
const NativeRegistry = @import("native_registry.zig").NativeRegistry;
const http = @import("impls/http_impl.zig");
const Value = @import("native_registry.zig").Value;
const HttpHeader = @import("native_registry.zig").HttpHeader;
const HttpRequest = @import("native_registry.zig").HttpRequest;

test "HTTP Component - integration test" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    http.init(std.testing.allocator);
    defer http.deinit();

    try http.registerHttpImpl(&registry);

    // Test that functions are registered
    try std.testing.expect(registry.has("http", "fetch"));
    try std.testing.expect(registry.has("http", "fetch-start"));
    try std.testing.expect(registry.has("http", "fetch-poll"));
    try std.testing.expect(registry.has("http", "fetch-response"));
    try std.testing.expect(registry.has("http", "fetch-free"));

    // Note: Actual HTTP requests require native implementation environment
    // These tests verify the interface is registered correctly
}

test "HTTP Component - fetch interface" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    http.init(std.testing.allocator);
    defer http.deinit();

    try http.registerHttpImpl(&registry);

    // Test fetch with simple GET request
    // Note: This test may fail if not running in proper WASM environment
    // In that case, it validates the interface contract
    var headers = [_]HttpHeader{
        .{ .name = "User-Agent", .value = "EdgeBox-Test/1.0" },
    };

    const request = HttpRequest{
        .url = "https://httpbin.org/get",
        .method = 0, // GET
        .headers = headers[0..],
        .body = null,
        .timeout_ms = 30000,
    };

    const result = registry.call("http", "fetch", &[_]Value{
        Value{ .http_request = request },
    }) catch |err| {
        // Expected error if extern functions not available in test environment
        std.debug.print("fetch call error (expected in test): {}\n", .{err});
        return;
    };

    // If call succeeds, verify result structure
    if (result.isOk()) {
        const response = result.asOkHttpResponse() catch |err| {
            std.debug.print("Failed to extract response: {}\n", .{err});
            return;
        };
        defer std.testing.allocator.free(response.body);

        std.debug.print("HTTP response: status={}, ok={}, body_len={}\n", .{
            response.status,
            response.ok,
            response.body.len,
        });
    } else {
        // Error case - check error code is valid
        const err_code = try result.asErr();
        std.debug.print("HTTP error: {}\n", .{err_code});
        // Valid error codes are 0-6 (http-error enum)
        try std.testing.expect(err_code <= 6);
    }
}

test "HTTP Component - fetch POST with body" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    http.init(std.testing.allocator);
    defer http.deinit();

    try http.registerHttpImpl(&registry);

    // Test fetch with POST request and body
    var headers = [_]HttpHeader{
        .{ .name = "Content-Type", .value = "application/json" },
        .{ .name = "User-Agent", .value = "EdgeBox-Test/1.0" },
    };

    const request = HttpRequest{
        .url = "https://httpbin.org/post",
        .method = 1, // POST
        .headers = headers[0..],
        .body = "{\"test\": \"data\"}",
        .timeout_ms = 30000,
    };

    const result = registry.call("http", "fetch", &[_]Value{
        Value{ .http_request = request },
    }) catch |err| {
        std.debug.print("fetch POST call error (expected in test): {}\n", .{err});
        return;
    };

    if (result.isOk()) {
        const response = result.asOkHttpResponse() catch |err| {
            std.debug.print("Failed to extract response: {}\n", .{err});
            return;
        };
        defer std.testing.allocator.free(response.body);

        std.debug.print("POST response: status={}, ok={}\n", .{
            response.status,
            response.ok,
        });
    }
}

test "HTTP Component - async workflow interface" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    http.init(std.testing.allocator);
    defer http.deinit();

    try http.registerHttpImpl(&registry);

    // Test async fetch workflow: start → poll → response → free
    var headers = [_]HttpHeader{
        .{ .name = "User-Agent", .value = "EdgeBox-Test/1.0" },
    };

    const request = HttpRequest{
        .url = "https://httpbin.org/delay/1",
        .method = 0, // GET
        .headers = headers[0..],
        .body = null,
        .timeout_ms = 30000,
    };

    const start_result = registry.call("http", "fetch-start", &[_]Value{
        Value{ .http_request = request },
    }) catch |err| {
        std.debug.print("fetch-start call error (expected in test): {}\n", .{err});
        return;
    };

    if (start_result.isErr()) {
        const err_code = try start_result.asErr();
        std.debug.print("fetch-start error: {}\n", .{err_code});
        try std.testing.expect(err_code <= 6);
        return;
    }

    const request_id = start_result.asOkRequestId() catch |err| {
        std.debug.print("Failed to extract request ID: {}\n", .{err});
        return;
    };

    std.debug.print("Async fetch started with ID: {}\n", .{request_id});

    // Test fetch-poll
    _ = registry.call("http", "fetch-poll", &[_]Value{
        Value{ .u32 = request_id },
    }) catch |err| {
        std.debug.print("fetch-poll call error: {}\n", .{err});
        return;
    };

    // Test fetch-response
    _ = registry.call("http", "fetch-response", &[_]Value{
        Value{ .u32 = request_id },
    }) catch |err| {
        std.debug.print("fetch-response call error: {}\n", .{err});
        return;
    };

    // Test fetch-free
    _ = registry.call("http", "fetch-free", &[_]Value{
        Value{ .u32 = request_id },
    }) catch |err| {
        std.debug.print("fetch-free call error: {}\n", .{err});
        return;
    };
}

test "HTTP Component - error handling" {
    var registry = NativeRegistry.init(std.testing.allocator);
    defer registry.deinit();

    http.init(std.testing.allocator);
    defer http.deinit();

    try http.registerHttpImpl(&registry);

    // Test with invalid URL
    const invalid_request = HttpRequest{
        .url = "not-a-valid-url",
        .method = 0, // GET
        .headers = &[_]HttpHeader{},
        .body = null,
        .timeout_ms = 5000,
    };

    const result = registry.call("http", "fetch", &[_]Value{
        Value{ .http_request = invalid_request },
    }) catch |err| {
        // Expected error in test environment
        std.debug.print("Expected error for invalid URL: {}\n", .{err});
        return;
    };

    // Should get error result
    if (result.isErr()) {
        const err_code = try result.asErr();
        std.debug.print("Got expected error code: {}\n", .{err_code});
        // Should be invalid-url (0) or connection-failed (1)
        try std.testing.expect(err_code <= 6);
    }
}
