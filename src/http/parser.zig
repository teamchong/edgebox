//! Native HTTP/1.1 Parser
//! Zero-copy, high-performance parsing in native Zig
//! Designed for maximum throughput with minimal allocations

const std = @import("std");

pub const MAX_HEADERS = 64;
pub const MAX_HEADER_SIZE = 8192;

pub const Method = enum {
    GET,
    POST,
    PUT,
    DELETE,
    PATCH,
    HEAD,
    OPTIONS,
    CONNECT,
    TRACE,

    pub fn fromString(s: []const u8) ?Method {
        const methods = [_]struct { name: []const u8, method: Method }{
            .{ .name = "GET", .method = .GET },
            .{ .name = "POST", .method = .POST },
            .{ .name = "PUT", .method = .PUT },
            .{ .name = "DELETE", .method = .DELETE },
            .{ .name = "PATCH", .method = .PATCH },
            .{ .name = "HEAD", .method = .HEAD },
            .{ .name = "OPTIONS", .method = .OPTIONS },
            .{ .name = "CONNECT", .method = .CONNECT },
            .{ .name = "TRACE", .method = .TRACE },
        };
        for (methods) |m| {
            if (std.mem.eql(u8, s, m.name)) return m.method;
        }
        return null;
    }

    pub fn toString(self: Method) []const u8 {
        return switch (self) {
            .GET => "GET",
            .POST => "POST",
            .PUT => "PUT",
            .DELETE => "DELETE",
            .PATCH => "PATCH",
            .HEAD => "HEAD",
            .OPTIONS => "OPTIONS",
            .CONNECT => "CONNECT",
            .TRACE => "TRACE",
        };
    }
};

pub const Header = struct {
    name: []const u8,
    value: []const u8,
};

pub const ParsedRequest = struct {
    method: Method,
    url: []const u8,
    version: []const u8,
    headers: []const Header,
    header_count: usize,
    body: ?[]const u8,
    keep_alive: bool,
    content_length: ?usize,

    // Pre-extracted common headers (O(1) lookup for frequent headers)
    content_type: ?[]const u8 = null,
    host: ?[]const u8 = null,
    accept: ?[]const u8 = null,
    user_agent: ?[]const u8 = null,
    accept_encoding: ?[]const u8 = null,

    // Static header storage to avoid allocations
    header_storage: [MAX_HEADERS]Header = undefined,

    pub fn getHeader(self: *const ParsedRequest, name: []const u8) ?[]const u8 {
        // Fast path: check pre-extracted common headers first
        if (std.ascii.eqlIgnoreCase(name, "content-type")) return self.content_type;
        if (std.ascii.eqlIgnoreCase(name, "host")) return self.host;
        if (std.ascii.eqlIgnoreCase(name, "accept")) return self.accept;
        if (std.ascii.eqlIgnoreCase(name, "user-agent")) return self.user_agent;
        if (std.ascii.eqlIgnoreCase(name, "accept-encoding")) return self.accept_encoding;

        // Slow path: linear search for rare headers
        for (self.headers[0..self.header_count]) |h| {
            if (std.ascii.eqlIgnoreCase(h.name, name)) {
                return h.value;
            }
        }
        return null;
    }
};

pub const ParseError = error{
    InvalidMethod,
    InvalidRequestLine,
    InvalidHeader,
    HeaderTooLarge,
    TooManyHeaders,
    IncompleteRequest,
    InvalidContentLength,
};

/// Parse HTTP/1.1 request from buffer
/// Returns parsed request with slices pointing into the input buffer (zero-copy)
pub fn parse(buf: []const u8) ParseError!ParsedRequest {
    var result = ParsedRequest{
        .method = .GET,
        .url = "",
        .version = "",
        .headers = &[_]Header{},
        .header_count = 0,
        .body = null,
        .keep_alive = true,
        .content_length = null,
    };

    // Find end of headers
    const header_end = std.mem.indexOf(u8, buf, "\r\n\r\n") orelse {
        return ParseError.IncompleteRequest;
    };

    if (header_end > MAX_HEADER_SIZE) {
        return ParseError.HeaderTooLarge;
    }

    const header_section = buf[0..header_end];

    // Parse request line
    const request_line_end = std.mem.indexOf(u8, header_section, "\r\n") orelse {
        return ParseError.InvalidRequestLine;
    };
    const request_line = header_section[0..request_line_end];

    // Parse: METHOD /path HTTP/1.1
    var parts = std.mem.splitScalar(u8, request_line, ' ');

    const method_str = parts.next() orelse return ParseError.InvalidRequestLine;
    result.method = Method.fromString(method_str) orelse return ParseError.InvalidMethod;

    result.url = parts.next() orelse return ParseError.InvalidRequestLine;
    result.version = parts.next() orelse return ParseError.InvalidRequestLine;

    // Parse headers
    var header_idx: usize = 0;
    var header_lines = std.mem.splitSequence(u8, header_section[request_line_end + 2 ..], "\r\n");

    while (header_lines.next()) |line| {
        if (line.len == 0) continue;

        const colon_idx = std.mem.indexOf(u8, line, ":") orelse {
            return ParseError.InvalidHeader;
        };

        if (header_idx >= MAX_HEADERS) {
            return ParseError.TooManyHeaders;
        }

        const name = std.mem.trim(u8, line[0..colon_idx], " \t");
        const value = std.mem.trim(u8, line[colon_idx + 1 ..], " \t");

        result.header_storage[header_idx] = Header{
            .name = name,
            .value = value,
        };

        // Check for special headers and pre-extract common headers
        if (std.ascii.eqlIgnoreCase(name, "content-length")) {
            result.content_length = std.fmt.parseInt(usize, value, 10) catch {
                return ParseError.InvalidContentLength;
            };
        } else if (std.ascii.eqlIgnoreCase(name, "connection")) {
            result.keep_alive = !std.ascii.eqlIgnoreCase(value, "close");
        } else if (std.ascii.eqlIgnoreCase(name, "content-type")) {
            result.content_type = value;
        } else if (std.ascii.eqlIgnoreCase(name, "host")) {
            result.host = value;
        } else if (std.ascii.eqlIgnoreCase(name, "accept")) {
            result.accept = value;
        } else if (std.ascii.eqlIgnoreCase(name, "user-agent")) {
            result.user_agent = value;
        } else if (std.ascii.eqlIgnoreCase(name, "accept-encoding")) {
            result.accept_encoding = value;
        }

        header_idx += 1;
    }

    result.header_count = header_idx;
    result.headers = result.header_storage[0..header_idx];

    // Extract body if present
    const body_start = header_end + 4;
    if (result.content_length) |len| {
        if (buf.len >= body_start + len) {
            result.body = buf[body_start .. body_start + len];
        }
    } else if (buf.len > body_start) {
        // No content-length, but there's data after headers
        result.body = buf[body_start..];
    }

    return result;
}

/// Check if buffer contains a complete HTTP request
pub fn isComplete(buf: []const u8) bool {
    const header_end = std.mem.indexOf(u8, buf, "\r\n\r\n") orelse return false;

    // Check for Content-Length header
    const header_section = buf[0..header_end];
    var lines = std.mem.splitSequence(u8, header_section, "\r\n");
    _ = lines.next(); // Skip request line

    while (lines.next()) |line| {
        if (line.len == 0) continue;
        const colon_idx = std.mem.indexOf(u8, line, ":") orelse continue;
        const name = std.mem.trim(u8, line[0..colon_idx], " \t");

        if (std.ascii.eqlIgnoreCase(name, "content-length")) {
            const value = std.mem.trim(u8, line[colon_idx + 1 ..], " \t");
            const content_length = std.fmt.parseInt(usize, value, 10) catch return true;
            const body_start = header_end + 4;
            return buf.len >= body_start + content_length;
        }
    }

    // No Content-Length, request is complete after headers
    return true;
}

/// Fast-path result for cached response serving
/// Only extracts what's needed: completeness + keep_alive + URL
pub const FastPathResult = struct {
    complete: bool,
    keep_alive: bool,
    header_end: usize,
    url: []const u8, // URL extracted for per-URL caching
};

/// Fast-path parse: Only check completeness and keep_alive status
/// Skips method, version, most headers - extracts URL for per-URL caching
/// Returns null if request is incomplete
pub fn parseFastPath(buf: []const u8) ?FastPathResult {
    // Find header end marker (CRLF CRLF)
    const header_end = std.mem.indexOf(u8, buf, "\r\n\r\n") orelse return null;

    // Default keep_alive = true for HTTP/1.1
    var keep_alive: bool = true;
    var content_length: ?usize = null;
    var url: []const u8 = "/";

    // Scan headers - only look for Connection and Content-Length
    const header_section = buf[0..header_end];
    var pos: usize = 0;

    // Extract URL from request line: "GET /path HTTP/1.1\r\n"
    // Skip method (find first space)
    while (pos < header_section.len and header_section[pos] != ' ') : (pos += 1) {}
    if (pos >= header_section.len) return null;
    pos += 1; // Skip space

    // URL starts here
    const url_start = pos;
    while (pos < header_section.len and header_section[pos] != ' ') : (pos += 1) {}
    if (pos >= header_section.len) return null;
    url = header_section[url_start..pos];

    // Skip to end of request line (find first CRLF)
    while (pos < header_section.len - 1) {
        if (header_section[pos] == '\r' and header_section[pos + 1] == '\n') {
            pos += 2;
            break;
        }
        pos += 1;
    }

    // Scan headers with minimal parsing
    while (pos < header_section.len) {
        // Find colon
        var colon: usize = pos;
        while (colon < header_section.len and header_section[colon] != ':') : (colon += 1) {}
        if (colon >= header_section.len) break;

        const name_len = colon - pos;

        // Check for "Connection" (10 chars) or "Content-Length" (14 chars)
        if (name_len == 10) {
            // Fast check: 'C' and 'o' match "Connection"
            if ((header_section[pos] == 'C' or header_section[pos] == 'c') and
                (header_section[pos + 1] == 'o' or header_section[pos + 1] == 'O'))
            {
                if (std.ascii.eqlIgnoreCase(header_section[pos .. pos + 10], "connection")) {
                    // Find value
                    var val_start = colon + 1;
                    while (val_start < header_section.len and (header_section[val_start] == ' ' or header_section[val_start] == '\t')) : (val_start += 1) {}
                    var val_end = val_start;
                    while (val_end < header_section.len and header_section[val_end] != '\r') : (val_end += 1) {}

                    // Check for "close"
                    const val = header_section[val_start..val_end];
                    keep_alive = !std.ascii.eqlIgnoreCase(val, "close");
                }
            }
        } else if (name_len == 14) {
            // Fast check for "Content-Length"
            if ((header_section[pos] == 'C' or header_section[pos] == 'c') and
                (header_section[pos + 1] == 'o' or header_section[pos + 1] == 'O'))
            {
                if (std.ascii.eqlIgnoreCase(header_section[pos .. pos + 14], "content-length")) {
                    var val_start = colon + 1;
                    while (val_start < header_section.len and (header_section[val_start] == ' ' or header_section[val_start] == '\t')) : (val_start += 1) {}
                    var val_end = val_start;
                    while (val_end < header_section.len and header_section[val_end] >= '0' and header_section[val_end] <= '9') : (val_end += 1) {}

                    content_length = std.fmt.parseInt(usize, header_section[val_start..val_end], 10) catch null;
                }
            }
        }

        // Skip to next line
        while (pos < header_section.len - 1) {
            if (header_section[pos] == '\r' and header_section[pos + 1] == '\n') {
                pos += 2;
                break;
            }
            pos += 1;
        }
        if (pos >= header_section.len - 1) break;
    }

    // Check if body is complete (if Content-Length present)
    if (content_length) |len| {
        const body_start = header_end + 4;
        if (buf.len < body_start + len) {
            return null; // Body incomplete
        }
    }

    return FastPathResult{
        .complete = true,
        .keep_alive = keep_alive,
        .header_end = header_end,
        .url = url,
    };
}

/// Format HTTP response into output buffer
/// Returns number of bytes written
pub fn formatResponse(
    status: u16,
    status_text: []const u8,
    headers: []const Header,
    body: []const u8,
    out: []u8,
) usize {
    var pos: usize = 0;

    // Status line
    const status_line = std.fmt.bufPrint(out[pos..], "HTTP/1.1 {d} {s}\r\n", .{ status, status_text }) catch return 0;
    pos += status_line.len;

    // Headers
    for (headers) |h| {
        const header_line = std.fmt.bufPrint(out[pos..], "{s}: {s}\r\n", .{ h.name, h.value }) catch return 0;
        pos += header_line.len;
    }

    // Content-Length if body present
    if (body.len > 0) {
        const cl = std.fmt.bufPrint(out[pos..], "Content-Length: {d}\r\n", .{body.len}) catch return 0;
        pos += cl.len;
    }

    // End of headers
    if (pos + 2 > out.len) return 0;
    out[pos] = '\r';
    out[pos + 1] = '\n';
    pos += 2;

    // Body
    if (body.len > 0) {
        if (pos + body.len > out.len) return 0;
        @memcpy(out[pos..][0..body.len], body);
        pos += body.len;
    }

    return pos;
}

/// Format a simple response with common headers
// Pre-computed common response templates (comptime)
const RESP_200_PLAIN_KA = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nConnection: keep-alive\r\nContent-Length: ";
const RESP_200_PLAIN_CL = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nConnection: close\r\nContent-Length: ";
const RESP_200_JSON_KA = "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nConnection: keep-alive\r\nContent-Length: ";
const RESP_200_JSON_CL = "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nConnection: close\r\nContent-Length: ";
const RESP_500_PLAIN = "HTTP/1.1 500 Internal Server Error\r\nContent-Type: text/plain\r\nConnection: close\r\nContent-Length: 21\r\n\r\nInternal Server Error";

/// Fast-path response formatter for common cases (zero-allocation)
pub fn formatSimpleResponse(
    status: u16,
    content_type: []const u8,
    body: []const u8,
    keep_alive: bool,
    out: []u8,
) usize {
    // Fast path for 200 OK with text/plain or application/json
    if (status == 200) {
        const is_plain = std.mem.eql(u8, content_type, "text/plain");
        const is_json = std.mem.eql(u8, content_type, "application/json");

        if (is_plain or is_json) {
            const template = if (is_plain)
                (if (keep_alive) RESP_200_PLAIN_KA else RESP_200_PLAIN_CL)
            else
                (if (keep_alive) RESP_200_JSON_KA else RESP_200_JSON_CL);

            // Copy template
            @memcpy(out[0..template.len], template);
            var pos = template.len;

            // Format content length (manual for speed)
            var len_buf: [16]u8 = undefined;
            const len_str = formatUint(body.len, &len_buf);
            @memcpy(out[pos..][0..len_str.len], len_str);
            pos += len_str.len;

            // CRLF CRLF
            out[pos] = '\r';
            out[pos + 1] = '\n';
            out[pos + 2] = '\r';
            out[pos + 3] = '\n';
            pos += 4;

            // Body
            @memcpy(out[pos..][0..body.len], body);
            return pos + body.len;
        }
    }

    // Fallback to general formatter
    const status_text = switch (status) {
        200 => "OK",
        201 => "Created",
        204 => "No Content",
        301 => "Moved Permanently",
        302 => "Found",
        304 => "Not Modified",
        400 => "Bad Request",
        401 => "Unauthorized",
        403 => "Forbidden",
        404 => "Not Found",
        405 => "Method Not Allowed",
        500 => "Internal Server Error",
        502 => "Bad Gateway",
        503 => "Service Unavailable",
        else => "Unknown",
    };

    var headers_buf: [4]Header = undefined;
    var header_count: usize = 0;

    headers_buf[header_count] = .{ .name = "Content-Type", .value = content_type };
    header_count += 1;

    if (keep_alive) {
        headers_buf[header_count] = .{ .name = "Connection", .value = "keep-alive" };
    } else {
        headers_buf[header_count] = .{ .name = "Connection", .value = "close" };
    }
    header_count += 1;

    return formatResponse(status, status_text, headers_buf[0..header_count], body, out);
}

/// Fast integer to string (no allocation)
fn formatUint(value: usize, buf: []u8) []const u8 {
    if (value == 0) {
        buf[0] = '0';
        return buf[0..1];
    }
    var v = value;
    var i: usize = buf.len;
    while (v > 0) {
        i -= 1;
        buf[i] = @intCast('0' + (v % 10));
        v /= 10;
    }
    return buf[i..];
}

// Tests
test "parse simple GET request" {
    const request = "GET /path HTTP/1.1\r\nHost: localhost\r\n\r\n";
    const parsed = try parse(request);

    try std.testing.expectEqual(Method.GET, parsed.method);
    try std.testing.expectEqualStrings("/path", parsed.url);
    try std.testing.expectEqualStrings("HTTP/1.1", parsed.version);
    try std.testing.expectEqual(@as(usize, 1), parsed.header_count);
    try std.testing.expectEqualStrings("Host", parsed.headers[0].name);
    try std.testing.expectEqualStrings("localhost", parsed.headers[0].value);
}

test "parse POST request with body" {
    const request = "POST /api HTTP/1.1\r\nHost: localhost\r\nContent-Length: 13\r\n\r\nHello, World!";
    const parsed = try parse(request);

    try std.testing.expectEqual(Method.POST, parsed.method);
    try std.testing.expectEqualStrings("/api", parsed.url);
    try std.testing.expectEqual(@as(?usize, 13), parsed.content_length);
    try std.testing.expectEqualStrings("Hello, World!", parsed.body.?);
}

test "format response" {
    var buf: [1024]u8 = undefined;
    const len = formatSimpleResponse(200, "text/plain", "Hello", true, &buf);
    const response = buf[0..len];

    try std.testing.expect(std.mem.startsWith(u8, response, "HTTP/1.1 200 OK\r\n"));
    try std.testing.expect(std.mem.indexOf(u8, response, "Content-Type: text/plain") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "Content-Length: 5") != null);
    try std.testing.expect(std.mem.endsWith(u8, response, "Hello"));
}

test "isComplete" {
    try std.testing.expect(!isComplete("GET /path HTTP/1.1\r\n"));
    try std.testing.expect(isComplete("GET /path HTTP/1.1\r\nHost: localhost\r\n\r\n"));
    try std.testing.expect(!isComplete("POST /api HTTP/1.1\r\nContent-Length: 10\r\n\r\nHello"));
    try std.testing.expect(isComplete("POST /api HTTP/1.1\r\nContent-Length: 5\r\n\r\nHello"));
}

test "parseFastPath incomplete" {
    try std.testing.expect(parseFastPath("GET /path HTTP/1.1\r\n") == null);
    try std.testing.expect(parseFastPath("POST /api HTTP/1.1\r\nContent-Length: 10\r\n\r\nHello") == null);
}

test "parseFastPath complete with keep_alive" {
    const result = parseFastPath("GET /path HTTP/1.1\r\nHost: localhost\r\n\r\n").?;
    try std.testing.expect(result.complete);
    try std.testing.expect(result.keep_alive);
    try std.testing.expectEqualStrings("/path", result.url);
}

test "parseFastPath connection close" {
    const result = parseFastPath("GET /path HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n").?;
    try std.testing.expect(result.complete);
    try std.testing.expect(!result.keep_alive);
    try std.testing.expectEqualStrings("/path", result.url);
}

test "parseFastPath with body" {
    const result = parseFastPath("POST /api HTTP/1.1\r\nContent-Length: 5\r\n\r\nHello").?;
    try std.testing.expect(result.complete);
    try std.testing.expect(result.keep_alive);
    try std.testing.expectEqualStrings("/api", result.url);
}

// ============================================================================
// Additional Tests
// ============================================================================

test "Method.fromString parses all methods" {
    try std.testing.expectEqual(Method.GET, Method.fromString("GET").?);
    try std.testing.expectEqual(Method.POST, Method.fromString("POST").?);
    try std.testing.expectEqual(Method.PUT, Method.fromString("PUT").?);
    try std.testing.expectEqual(Method.DELETE, Method.fromString("DELETE").?);
    try std.testing.expectEqual(Method.PATCH, Method.fromString("PATCH").?);
    try std.testing.expectEqual(Method.HEAD, Method.fromString("HEAD").?);
    try std.testing.expectEqual(Method.OPTIONS, Method.fromString("OPTIONS").?);
    try std.testing.expectEqual(Method.CONNECT, Method.fromString("CONNECT").?);
    try std.testing.expectEqual(Method.TRACE, Method.fromString("TRACE").?);
}

test "Method.fromString returns null for invalid methods" {
    try std.testing.expect(Method.fromString("INVALID") == null);
    try std.testing.expect(Method.fromString("get") == null); // Case sensitive
    try std.testing.expect(Method.fromString("") == null);
    try std.testing.expect(Method.fromString("GETS") == null);
}

test "Method.toString returns correct strings" {
    try std.testing.expectEqualStrings("GET", Method.GET.toString());
    try std.testing.expectEqualStrings("POST", Method.POST.toString());
    try std.testing.expectEqualStrings("DELETE", Method.DELETE.toString());
}

test "parse returns InvalidMethod for unknown method" {
    const result = parse("INVALID /path HTTP/1.1\r\nHost: localhost\r\n\r\n");
    try std.testing.expectError(ParseError.InvalidMethod, result);
}

test "parse returns IncompleteRequest for incomplete headers" {
    const result = parse("GET /path HTTP/1.1\r\nHost: localhost");
    try std.testing.expectError(ParseError.IncompleteRequest, result);
}

test "parse returns InvalidHeader for malformed headers" {
    const result = parse("GET /path HTTP/1.1\r\nBadHeader\r\n\r\n");
    try std.testing.expectError(ParseError.InvalidHeader, result);
}

test "parse returns InvalidContentLength for non-numeric content-length" {
    const result = parse("POST /api HTTP/1.1\r\nContent-Length: abc\r\n\r\n");
    try std.testing.expectError(ParseError.InvalidContentLength, result);
}

test "parse extracts common headers" {
    const request = "GET /api HTTP/1.1\r\n" ++
        "Host: example.com\r\n" ++
        "Content-Type: application/json\r\n" ++
        "Accept: text/html\r\n" ++
        "User-Agent: TestClient/1.0\r\n" ++
        "Accept-Encoding: gzip\r\n\r\n";
    const parsed = try parse(request);

    try std.testing.expectEqualStrings("example.com", parsed.host.?);
    try std.testing.expectEqualStrings("application/json", parsed.content_type.?);
    try std.testing.expectEqualStrings("text/html", parsed.accept.?);
    try std.testing.expectEqualStrings("TestClient/1.0", parsed.user_agent.?);
    try std.testing.expectEqualStrings("gzip", parsed.accept_encoding.?);
}

test "ParsedRequest.getHeader finds pre-extracted headers" {
    const request = "GET /api HTTP/1.1\r\nHost: example.com\r\nContent-Type: text/plain\r\n\r\n";
    const parsed = try parse(request);

    try std.testing.expectEqualStrings("example.com", parsed.getHeader("Host").?);
    try std.testing.expectEqualStrings("example.com", parsed.getHeader("host").?); // Case insensitive
    try std.testing.expectEqualStrings("example.com", parsed.getHeader("HOST").?);
    try std.testing.expectEqualStrings("text/plain", parsed.getHeader("Content-Type").?);
}

test "ParsedRequest.getHeader finds custom headers" {
    const request = "GET /api HTTP/1.1\r\nHost: example.com\r\nX-Custom: custom-value\r\n\r\n";
    const parsed = try parse(request);

    try std.testing.expectEqualStrings("custom-value", parsed.getHeader("X-Custom").?);
    try std.testing.expect(parsed.getHeader("X-Missing") == null);
}

test "parse handles Connection close header" {
    const request = "GET /api HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n";
    const parsed = try parse(request);

    try std.testing.expect(!parsed.keep_alive);
}

test "parse handles Connection keep-alive header" {
    const request = "GET /api HTTP/1.1\r\nHost: example.com\r\nConnection: keep-alive\r\n\r\n";
    const parsed = try parse(request);

    try std.testing.expect(parsed.keep_alive);
}

test "formatResponse produces valid HTTP response" {
    var buf: [1024]u8 = undefined;
    const headers = [_]Header{
        .{ .name = "X-Custom", .value = "test" },
    };
    const len = formatResponse(404, "Not Found", &headers, "Page not found", &buf);
    const response = buf[0..len];

    try std.testing.expect(std.mem.startsWith(u8, response, "HTTP/1.1 404 Not Found\r\n"));
    try std.testing.expect(std.mem.indexOf(u8, response, "X-Custom: test") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "Content-Length: 14") != null);
    try std.testing.expect(std.mem.endsWith(u8, response, "Page not found"));
}

test "formatSimpleResponse handles JSON content type" {
    var buf: [1024]u8 = undefined;
    const len = formatSimpleResponse(200, "application/json", "{\"ok\":true}", true, &buf);
    const response = buf[0..len];

    try std.testing.expect(std.mem.indexOf(u8, response, "Content-Type: application/json") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "Connection: keep-alive") != null);
}

test "formatSimpleResponse handles non-200 status" {
    var buf: [1024]u8 = undefined;
    const len = formatSimpleResponse(500, "text/plain", "Error", false, &buf);
    const response = buf[0..len];

    try std.testing.expect(std.mem.startsWith(u8, response, "HTTP/1.1 500 Internal Server Error\r\n"));
    try std.testing.expect(std.mem.indexOf(u8, response, "Connection: close") != null);
}

test "formatUint converts integers correctly" {
    var buf: [16]u8 = undefined;

    try std.testing.expectEqualStrings("0", formatUint(0, &buf));
    try std.testing.expectEqualStrings("1", formatUint(1, &buf));
    try std.testing.expectEqualStrings("42", formatUint(42, &buf));
    try std.testing.expectEqualStrings("12345", formatUint(12345, &buf));
}
