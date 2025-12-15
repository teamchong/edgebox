//! HTTP Implementation for EdgeBox ABI
//!
//! All HTTP logic runs in WASM sandbox:
//! - URL parsing
//! - HTTP/1.1 request/response formatting
//! - Header parsing and validation
//! - Chunked encoding handling
//!
//! Host provides ONLY raw socket operations:
//! - host_net_connect(host, port) -> fd
//! - host_net_send(fd, bytes) -> count
//! - host_net_recv(fd, bytes) -> count
//! - host_net_close(fd)
//!
//! KEY SECURITY FEATURE:
//! - Malformed HTTP responses crash WASM, not host
//! - No header injection attacks can escape sandbox
//! - Same protection as Cloudflare should have had

const std = @import("std");
const host = @import("host.zig");

const wasm_allocator = std.heap.wasm_allocator;

// ============================================================================
// HTTP Method Enum
// ============================================================================

pub const Method = enum(u8) {
    GET = 0,
    POST = 1,
    PUT = 2,
    DELETE = 3,
    PATCH = 4,
    HEAD = 5,
    OPTIONS = 6,

    pub fn toString(self: Method) []const u8 {
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

// ============================================================================
// URL Parsing (runs in WASM)
// ============================================================================

pub const ParsedUrl = struct {
    scheme: []const u8,
    host: []const u8,
    port: u16,
    path: []const u8,
    query: ?[]const u8,

    pub fn isHttps(self: ParsedUrl) bool {
        return std.mem.eql(u8, self.scheme, "https");
    }
};

/// Parse URL into components. All parsing in WASM.
/// Supports http:// and https:// schemes.
pub fn parseUrl(url: []const u8) ?ParsedUrl {
    // Find scheme
    const scheme_end = std.mem.indexOf(u8, url, "://") orelse return null;
    const scheme = url[0..scheme_end];

    if (!std.mem.eql(u8, scheme, "http") and !std.mem.eql(u8, scheme, "https")) {
        return null; // Unsupported scheme
    }

    const after_scheme = url[scheme_end + 3 ..];

    // Find path start
    const path_start = std.mem.indexOf(u8, after_scheme, "/") orelse after_scheme.len;
    const host_port = after_scheme[0..path_start];

    // Parse host:port
    var parsed_host: []const u8 = undefined;
    var parsed_port: u16 = undefined;

    if (std.mem.indexOf(u8, host_port, ":")) |colon| {
        parsed_host = host_port[0..colon];
        parsed_port = std.fmt.parseInt(u16, host_port[colon + 1 ..], 10) catch {
            return null; // Invalid port
        };
    } else {
        parsed_host = host_port;
        parsed_port = if (std.mem.eql(u8, scheme, "https")) 443 else 80;
    }

    if (parsed_host.len == 0) return null;

    // Parse path and query
    const path_query = if (path_start < after_scheme.len)
        after_scheme[path_start..]
    else
        "/";

    var path: []const u8 = undefined;
    var query: ?[]const u8 = null;

    if (std.mem.indexOf(u8, path_query, "?")) |q| {
        path = path_query[0..q];
        query = path_query[q + 1 ..];
    } else {
        path = path_query;
    }

    if (path.len == 0) path = "/";

    return ParsedUrl{
        .scheme = scheme,
        .host = parsed_host,
        .port = parsed_port,
        .path = path,
        .query = query,
    };
}

// ============================================================================
// HTTP Request Building (runs in WASM)
// ============================================================================

/// Build HTTP/1.1 request. All formatting in WASM.
pub fn buildRequest(
    method: Method,
    parsed_url: ParsedUrl,
    headers_json: ?[]const u8,
    body: ?[]const u8,
) ![]u8 {
    var request = std.ArrayListUnmanaged(u8){};
    errdefer request.deinit(wasm_allocator);

    // Request line: "GET /path HTTP/1.1\r\n"
    try request.appendSlice(wasm_allocator, method.toString());
    try request.append(wasm_allocator, ' ');
    try request.appendSlice(wasm_allocator, parsed_url.path);
    if (parsed_url.query) |q| {
        try request.append(wasm_allocator, '?');
        try request.appendSlice(wasm_allocator, q);
    }
    try request.appendSlice(wasm_allocator, " HTTP/1.1\r\n");

    // Host header (required)
    try request.appendSlice(wasm_allocator, "Host: ");
    try request.appendSlice(wasm_allocator, parsed_url.host);
    if ((parsed_url.isHttps() and parsed_url.port != 443) or
        (!parsed_url.isHttps() and parsed_url.port != 80))
    {
        try request.append(wasm_allocator, ':');
        var port_buf: [8]u8 = undefined;
        const port_str = std.fmt.bufPrint(&port_buf, "{d}", .{parsed_url.port}) catch unreachable;
        try request.appendSlice(wasm_allocator, port_str);
    }
    try request.appendSlice(wasm_allocator, "\r\n");

    // Parse and add custom headers from JSON
    if (headers_json) |json| {
        try appendHeadersFromJson(&request, json);
    }

    // Content-Length if body present
    if (body) |b| {
        try request.appendSlice(wasm_allocator, "Content-Length: ");
        var len_buf: [16]u8 = undefined;
        const len_str = std.fmt.bufPrint(&len_buf, "{d}", .{b.len}) catch unreachable;
        try request.appendSlice(wasm_allocator, len_str);
        try request.appendSlice(wasm_allocator, "\r\n");
    }

    // Connection header
    try request.appendSlice(wasm_allocator, "Connection: close\r\n");

    // End of headers
    try request.appendSlice(wasm_allocator, "\r\n");

    // Body
    if (body) |b| {
        try request.appendSlice(wasm_allocator, b);
    }

    return request.toOwnedSlice(wasm_allocator);
}

/// Parse JSON headers and append to request. Simple JSON parsing in WASM.
fn appendHeadersFromJson(request: *std.ArrayListUnmanaged(u8), json: []const u8) !void {
    // Simple JSON object parser: {"key": "value", ...}
    // Security: All validation happens here in WASM

    var i: usize = 0;

    // Skip whitespace and opening brace
    while (i < json.len and (json[i] == ' ' or json[i] == '{')) : (i += 1) {}

    while (i < json.len) {
        // Skip whitespace
        while (i < json.len and (json[i] == ' ' or json[i] == ',' or json[i] == '\n')) : (i += 1) {}

        if (i >= json.len or json[i] == '}') break;

        // Parse key
        if (json[i] != '"') break;
        i += 1;
        const key_start = i;
        while (i < json.len and json[i] != '"') : (i += 1) {}
        const key = json[key_start..i];
        i += 1; // Skip closing quote

        // Skip colon
        while (i < json.len and (json[i] == ' ' or json[i] == ':')) : (i += 1) {}

        // Parse value
        if (i >= json.len or json[i] != '"') break;
        i += 1;
        const value_start = i;
        while (i < json.len and json[i] != '"') : (i += 1) {}
        const value = json[value_start..i];
        i += 1; // Skip closing quote

        // Validate header name (no CRLF injection)
        for (key) |c| {
            if (c == '\r' or c == '\n' or c == ':') return error.InvalidHeaderName;
        }

        // Validate header value (no CRLF injection)
        for (value) |c| {
            if (c == '\r' or c == '\n') return error.InvalidHeaderValue;
        }

        // Append header
        try request.appendSlice(wasm_allocator, key);
        try request.appendSlice(wasm_allocator, ": ");
        try request.appendSlice(wasm_allocator, value);
        try request.appendSlice(wasm_allocator, "\r\n");
    }
}

// ============================================================================
// HTTP Response Parsing (runs in WASM)
// ============================================================================

pub const Response = struct {
    status: u16,
    headers: std.StringHashMapUnmanaged([]const u8),
    body: []const u8,
    raw: []u8, // Owns the memory

    pub fn deinit(self: *Response) void {
        self.headers.deinit(wasm_allocator);
        wasm_allocator.free(self.raw);
    }
};

/// Parse HTTP/1.1 response. All parsing in WASM.
/// Security: Malformed responses cause error, not crash.
pub fn parseResponse(data: []const u8) !Response {
    // Find header/body separator
    const header_end = std.mem.indexOf(u8, data, "\r\n\r\n") orelse {
        return error.IncompleteResponse;
    };

    const headers_section = data[0..header_end];
    const body_start = header_end + 4;

    // Parse status line: "HTTP/1.1 200 OK\r\n"
    const status_line_end = std.mem.indexOf(u8, headers_section, "\r\n") orelse {
        return error.InvalidStatusLine;
    };
    const status_line = headers_section[0..status_line_end];

    // Validate HTTP version
    if (!std.mem.startsWith(u8, status_line, "HTTP/1.")) {
        return error.UnsupportedHttpVersion;
    }

    // Parse status code
    const status_start = std.mem.indexOf(u8, status_line, " ") orelse {
        return error.InvalidStatusLine;
    };
    const status_end = std.mem.indexOfPos(u8, status_line, status_start + 1, " ") orelse status_line.len;
    const status_str = status_line[status_start + 1 .. status_end];
    const status = std.fmt.parseInt(u16, status_str, 10) catch {
        return error.InvalidStatusCode;
    };

    // Parse headers
    var headers = std.StringHashMapUnmanaged([]const u8){};
    errdefer headers.deinit(wasm_allocator);

    var line_start = status_line_end + 2;
    while (line_start < header_end) {
        const line_end = std.mem.indexOfPos(u8, headers_section, line_start, "\r\n") orelse header_end;
        const line = headers_section[line_start..line_end];

        if (std.mem.indexOf(u8, line, ": ")) |colon| {
            const name = line[0..colon];
            const value = line[colon + 2 ..];
            try headers.put(wasm_allocator, name, value);
        }

        line_start = line_end + 2;
    }

    // Handle body (may need chunked decoding)
    var body = data[body_start..];

    if (headers.get("Transfer-Encoding")) |te| {
        if (std.mem.indexOf(u8, te, "chunked") != null) {
            body = try decodeChunked(body);
        }
    }

    // Copy raw data so Response owns it
    const raw = try wasm_allocator.dupe(u8, data);

    return Response{
        .status = status,
        .headers = headers,
        .body = body,
        .raw = raw,
    };
}

/// Decode chunked transfer encoding. All in WASM.
fn decodeChunked(data: []const u8) ![]const u8 {
    var result = std.ArrayListUnmanaged(u8){};
    errdefer result.deinit(wasm_allocator);

    var pos: usize = 0;
    while (pos < data.len) {
        // Parse chunk size (hex)
        const size_end = std.mem.indexOfPos(u8, data, pos, "\r\n") orelse break;
        const size_str = data[pos..size_end];
        const chunk_size = std.fmt.parseInt(usize, size_str, 16) catch break;

        if (chunk_size == 0) break; // Last chunk

        pos = size_end + 2;
        if (pos + chunk_size > data.len) break;

        try result.appendSlice(wasm_allocator, data[pos .. pos + chunk_size]);
        pos += chunk_size + 2; // Skip chunk data + CRLF
    }

    return result.toOwnedSlice(wasm_allocator);
}

// ============================================================================
// High-Level Fetch (orchestrates host calls)
// ============================================================================

/// Perform HTTP fetch. Complex logic in WASM, raw I/O via host.
pub fn fetch(
    url: []const u8,
    method_int: u32,
    headers_json: ?[]const u8,
    body: ?[]const u8,
) !*Response {
    // Parse URL (in WASM)
    const parsed = parseUrl(url) orelse return error.InvalidUrl;

    // Convert method
    const method = std.meta.intToEnum(Method, @as(u8, @intCast(method_int))) catch {
        return error.InvalidMethod;
    };

    // Check for HTTPS (not supported yet - would need TLS in WASM)
    if (parsed.isHttps()) {
        return error.HttpsNotSupported;
    }

    // Build request (in WASM)
    const request = try buildRequest(method, parsed, headers_json, body);
    defer wasm_allocator.free(request);

    // Connect via host (raw socket)
    const fd = host.netConnect(parsed.host, parsed.port);
    if (fd < 0) return error.ConnectionFailed;
    defer host.netClose(fd);

    // Send request via host (raw bytes)
    var sent: usize = 0;
    while (sent < request.len) {
        const n = host.netSend(fd, request[sent..]);
        if (n <= 0) return error.SendFailed;
        sent += @intCast(n);
    }

    // Receive response via host (raw bytes)
    var response_buf = std.ArrayListUnmanaged(u8){};
    defer response_buf.deinit(wasm_allocator);

    var recv_buf: [8192]u8 = undefined;
    while (true) {
        const n = host.netRecv(fd, &recv_buf);
        if (n <= 0) break;
        try response_buf.appendSlice(wasm_allocator, recv_buf[0..@intCast(n)]);
    }

    // Parse response (in WASM - security critical!)
    const response = try wasm_allocator.create(Response);
    errdefer wasm_allocator.destroy(response);

    response.* = try parseResponse(response_buf.items);

    return response;
}

// ============================================================================
// Tests
// ============================================================================

test "parseUrl basic" {
    const url = parseUrl("http://example.com/path?query=1");
    try std.testing.expect(url != null);
    try std.testing.expectEqualStrings("http", url.?.scheme);
    try std.testing.expectEqualStrings("example.com", url.?.host);
    try std.testing.expectEqual(@as(u16, 80), url.?.port);
    try std.testing.expectEqualStrings("/path", url.?.path);
    try std.testing.expectEqualStrings("query=1", url.?.query.?);
}

test "parseUrl https with port" {
    const url = parseUrl("https://api.example.com:8443/v1/users");
    try std.testing.expect(url != null);
    try std.testing.expectEqualStrings("https", url.?.scheme);
    try std.testing.expectEqualStrings("api.example.com", url.?.host);
    try std.testing.expectEqual(@as(u16, 8443), url.?.port);
    try std.testing.expectEqualStrings("/v1/users", url.?.path);
}

test "parseUrl invalid" {
    try std.testing.expect(parseUrl("ftp://example.com") == null);
    try std.testing.expect(parseUrl("not-a-url") == null);
    try std.testing.expect(parseUrl("http://") == null);
}
