/// WASM Fetch Implementation using Host HTTP Dispatch
/// For use in QuickJS WASM runtime with WAMR
const std = @import("std");

// Import host HTTP dispatch function
extern "edgebox_http" fn http_dispatch(opcode: u32, a1: u32, a2: u32, a3: u32, a4: u32, a5: u32, a6: u32, a7: u32, a8: u32) i32;

// HTTP Dispatch opcodes (must match edgebox_wamr.zig)
const HTTP_OP_REQUEST: u32 = 0;
const HTTP_OP_GET_RESPONSE_LEN: u32 = 1;
const HTTP_OP_GET_RESPONSE: u32 = 2;

pub const FetchError = error{
    InvalidUrl,
    ConnectionFailed,
    HostNotFound,
    Timeout,
    InvalidResponse,
    OutOfMemory,
    TlsNotSupported,
};

pub const HttpMethod = enum {
    GET,
    POST,
    PUT,
    DELETE,
    PATCH,
    HEAD,
    OPTIONS,

    pub fn toString(self: HttpMethod) []const u8 {
        return switch (self) {
            .GET => "GET",
            .POST => "POST",
            .PUT => "PUT",
            .DELETE => "DELETE",
            .PATCH => "PATCH",
            .HEAD => "HEAD",
            .OPTIONS => "OPTIONS",
        };
    }
};

pub const FetchOptions = struct {
    method: HttpMethod = .GET,
    headers: ?[]const Header = null,
    body: ?[]const u8 = null,
    timeout_ms: u32 = 30000,
};

pub const Header = struct {
    name: []const u8,
    value: []const u8,
};

pub const FetchResponse = struct {
    status: u16,
    headers: std.ArrayListUnmanaged(Header),
    body: []u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *FetchResponse) void {
        self.headers.deinit(self.allocator);
        if (self.body.len > 0) {
            self.allocator.free(self.body);
        }
    }

    pub fn text(self: *const FetchResponse) []const u8 {
        return self.body;
    }
};

/// Perform HTTP fetch using host dispatch (WAMR host functions)
pub fn fetch(allocator: std.mem.Allocator, url: []const u8, options: FetchOptions) FetchError!FetchResponse {
    const method_str = options.method.toString();

    // Build headers string "Key: Value\r\nKey2: Value2"
    var headers_buf = std.ArrayListUnmanaged(u8){};
    defer headers_buf.deinit(allocator);

    if (options.headers) |hdrs| {
        for (hdrs) |h| {
            headers_buf.appendSlice(allocator, h.name) catch return FetchError.OutOfMemory;
            headers_buf.appendSlice(allocator, ": ") catch return FetchError.OutOfMemory;
            headers_buf.appendSlice(allocator, h.value) catch return FetchError.OutOfMemory;
            headers_buf.appendSlice(allocator, "\r\n") catch return FetchError.OutOfMemory;
        }
    }

    // Call host HTTP dispatch
    const status = http_dispatch(
        HTTP_OP_REQUEST,
        @intFromPtr(url.ptr),
        @intCast(url.len),
        @intFromPtr(method_str.ptr),
        @intCast(method_str.len),
        @intFromPtr(headers_buf.items.ptr),
        @intCast(headers_buf.items.len),
        if (options.body) |b| @intFromPtr(b.ptr) else 0,
        if (options.body) |b| @intCast(b.len) else 0,
    );

    if (status < 0) {
        return FetchError.ConnectionFailed;
    }

    // Get response length
    const response_len = http_dispatch(HTTP_OP_GET_RESPONSE_LEN, 0, 0, 0, 0, 0, 0, 0, 0);
    if (response_len < 0) {
        return FetchError.InvalidResponse;
    }

    // Allocate buffer and get response body
    const body = allocator.alloc(u8, @intCast(response_len)) catch return FetchError.OutOfMemory;
    errdefer allocator.free(body);

    const result = http_dispatch(HTTP_OP_GET_RESPONSE, @intFromPtr(body.ptr), 0, 0, 0, 0, 0, 0, 0);
    if (result < 0) {
        return FetchError.InvalidResponse;
    }

    return FetchResponse{
        .status = @intCast(status),
        .headers = std.ArrayListUnmanaged(Header){},
        .body = body,
        .allocator = allocator,
    };
}

/// Simple fetch for QuickJS binding
pub fn jsFetch(allocator: std.mem.Allocator, url: []const u8, method: []const u8, headers_json: ?[]const u8, body: ?[]const u8) FetchError!FetchResponse {
    var options = FetchOptions{};

    // Parse method
    if (std.mem.eql(u8, method, "POST")) {
        options.method = .POST;
    } else if (std.mem.eql(u8, method, "PUT")) {
        options.method = .PUT;
    } else if (std.mem.eql(u8, method, "DELETE")) {
        options.method = .DELETE;
    } else if (std.mem.eql(u8, method, "PATCH")) {
        options.method = .PATCH;
    }

    options.body = body;
    _ = headers_json; // TODO: Parse headers from JSON

    return fetch(allocator, url, options);
}
