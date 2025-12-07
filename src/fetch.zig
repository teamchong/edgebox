/// fetch() Implementation for QuickJS
/// Provides browser-compatible fetch API using Zig networking
const std = @import("std");
const net = std.net;
const tls = @import("tls.zig");
const wasi_socket = @import("wasi_socket.zig");

/// Extract raw string from Uri.Component (Zig 0.15 API)
fn getComponentString(component: std.Uri.Component) []const u8 {
    return switch (component) {
        .raw => |raw| raw,
        .percent_encoded => |enc| enc,
    };
}

fn getHostString(host: ?std.Uri.Component) ?[]const u8 {
    if (host) |h| {
        return getComponentString(h);
    }
    return null;
}

fn getPathString(path: std.Uri.Component) []const u8 {
    const p = getComponentString(path);
    return if (p.len > 0) p else "/";
}

fn getQueryString(query: ?std.Uri.Component) ?[]const u8 {
    if (query) |q| {
        return getComponentString(q);
    }
    return null;
}

pub const FetchError = error{
    InvalidUrl,
    ConnectionFailed,
    TlsError,
    Timeout,
    InvalidResponse,
    OutOfMemory,
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
    headers: ?std.StringHashMap([]const u8) = null,
    body: ?[]const u8 = null,
    timeout_ms: u32 = 30000,
};

pub const FetchResponse = struct {
    status: u16,
    headers: std.StringHashMap([]const u8),
    body: []u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *FetchResponse) void {
        var iter = self.headers.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.headers.deinit();
        self.allocator.free(self.body);
    }

    pub fn text(self: *const FetchResponse) []const u8 {
        return self.body;
    }

    pub fn json(self: *const FetchResponse, comptime T: type) !T {
        return std.json.parseFromSlice(T, self.allocator, self.body, .{});
    }
};

/// Perform HTTP/HTTPS fetch
pub fn fetch(allocator: std.mem.Allocator, url: []const u8, options: FetchOptions) !FetchResponse {
    // Parse URL
    const uri = std.Uri.parse(url) catch return FetchError.InvalidUrl;

    const scheme = uri.scheme;
    const host = getHostString(uri.host) orelse return FetchError.InvalidUrl;
    const port: u16 = uri.port orelse if (std.mem.eql(u8, scheme, "https")) @as(u16, 443) else @as(u16, 80);
    const path = getPathString(uri.path);

    const is_https = std.mem.eql(u8, scheme, "https");

    // Build HTTP request
    var request = std.ArrayList(u8){};
    defer request.deinit(allocator);

    const writer = request.writer(allocator);
    try writer.print("{s} {s}", .{ options.method.toString(), path });
    if (getQueryString(uri.query)) |q| {
        try writer.print("?{s}", .{q});
    }
    try writer.writeAll(" HTTP/1.1\r\n");
    try writer.print("Host: {s}\r\n", .{host});
    try writer.writeAll("Connection: close\r\n");
    try writer.writeAll("User-Agent: EdgeBox/1.0\r\n");

    // Custom headers
    if (options.headers) |headers| {
        var iter = headers.iterator();
        while (iter.next()) |entry| {
            try writer.print("{s}: {s}\r\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        }
    }

    // Body
    if (options.body) |body| {
        try writer.print("Content-Length: {d}\r\n", .{body.len});
        try writer.writeAll("\r\n");
        try writer.writeAll(body);
    } else {
        try writer.writeAll("\r\n");
    }

    // Send request and get response
    var response_data: []u8 = undefined;

    if (is_https) {
        // HTTPS
        var client = tls.TlsClient.connect(allocator, host, port) catch return FetchError.ConnectionFailed;
        defer client.close();

        client.writeAll(request.items) catch return FetchError.ConnectionFailed;

        var response_buf = std.ArrayList(u8){};
        errdefer response_buf.deinit(allocator);

        var buf: [8192]u8 = undefined;
        while (true) {
            const n = client.read(&buf) catch |err| switch (err) {
                error.EndOfStream => break,
                else => return FetchError.ConnectionFailed,
            };
            if (n == 0) break;
            try response_buf.appendSlice(allocator, buf[0..n]);
        }
        response_data = try response_buf.toOwnedSlice(allocator);
    } else {
        // HTTP
        const addresses = net.getAddressList(allocator, host, port) catch return FetchError.ConnectionFailed;
        defer addresses.deinit();

        if (addresses.addrs.len == 0) {
            return FetchError.ConnectionFailed;
        }

        var stream = net.tcpConnectToAddress(addresses.addrs[0]) catch return FetchError.ConnectionFailed;
        defer stream.close();

        stream.writeAll(request.items) catch return FetchError.ConnectionFailed;

        var response_buf = std.ArrayList(u8).init(allocator);
        errdefer response_buf.deinit();

        var buf: [8192]u8 = undefined;
        while (true) {
            const n = stream.read(&buf) catch break;
            if (n == 0) break;
            try response_buf.appendSlice(buf[0..n]);
        }
        response_data = try response_buf.toOwnedSlice();
    }

    // Parse response
    const parsed = tls.parseHttpResponse(response_data) catch {
        allocator.free(response_data);
        return FetchError.InvalidResponse;
    };

    // Parse headers
    var headers = std.StringHashMap([]const u8).init(allocator);
    var header_lines = std.mem.splitSequence(u8, parsed.headers, "\r\n");
    _ = header_lines.next(); // Skip status line
    while (header_lines.next()) |line| {
        if (std.mem.indexOf(u8, line, ": ")) |sep| {
            const key = try allocator.dupe(u8, line[0..sep]);
            const value = try allocator.dupe(u8, line[sep + 2 ..]);
            try headers.put(key, value);
        }
    }

    // Extract body (need to copy since we'll free response_data structure)
    const body_start = @intFromPtr(parsed.body.ptr) - @intFromPtr(response_data.ptr);
    const body = try allocator.dupe(u8, response_data[body_start..]);
    allocator.free(response_data);

    return FetchResponse{
        .status = parsed.status,
        .headers = headers,
        .body = body,
        .allocator = allocator,
    };
}

// QuickJS binding (to be called from JS)
pub fn jsFetch(allocator: std.mem.Allocator, url: []const u8, method: []const u8, headers_json: ?[]const u8, body: ?[]const u8) !FetchResponse {
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

    // TODO: Parse headers from JSON

    _ = headers_json;

    return fetch(allocator, url, options);
}

test "fetch http" {
    const allocator = std.testing.allocator;

    // This test requires network, skip in CI
    if (true) return error.SkipZigTest;

    var response = try fetch(allocator, "http://httpbin.org/get", .{});
    defer response.deinit();

    try std.testing.expect(response.status == 200);
}
