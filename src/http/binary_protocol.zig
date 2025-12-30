/// Binary Protocol for Zero-Copy HTTP Dispatch
///
/// Eliminates JSON serialization/parsing overhead by using fixed-layout binary format.
/// Request and response are laid out directly in WASM shared memory.
///
/// Request Binary Layout (little-endian):
/// ┌─────────────────────────────────────────────┐
/// │ method (u8)      │ 0=GET, 1=POST, 2=PUT...  │
/// │ url_len (u16)    │ Length of URL            │
/// │ headers_count    │ Number of headers (u8)   │
/// │ body_len (u32)   │ Length of body           │
/// │ url_data         │ URL bytes (url_len)      │
/// │ headers[]        │ {name_len(u8), val_len(u16), name, value} │
/// │ body_data        │ Body bytes (body_len)    │
/// └─────────────────────────────────────────────┘
///
/// Response Binary Layout (little-endian):
/// ┌─────────────────────────────────────────────┐
/// │ status (u16)     │ HTTP status code         │
/// │ content_type_len │ Length of content-type (u8) │
/// │ body_len (u32)   │ Length of body           │
/// │ content_type     │ Content-type string      │
/// │ body_data        │ Body bytes               │
/// └─────────────────────────────────────────────┘

const std = @import("std");
const parser = @import("parser.zig");

/// Request header fixed size: method(1) + url_len(2) + headers_count(1) + body_len(4) = 8 bytes
pub const REQUEST_HEADER_SIZE: usize = 8;

/// Response header fixed size: status(2) + content_type_len(1) + body_len(4) = 7 bytes
pub const RESPONSE_HEADER_SIZE: usize = 7;

/// Maximum headers per request
pub const MAX_HEADERS: usize = 32;

/// Method encoding (matches parser.Method order)
pub const MethodCode = enum(u8) {
    GET = 0,
    POST = 1,
    PUT = 2,
    DELETE = 3,
    PATCH = 4,
    HEAD = 5,
    OPTIONS = 6,
    CONNECT = 7,
    TRACE = 8,

    pub fn fromMethod(m: parser.Method) MethodCode {
        return switch (m) {
            .GET => .GET,
            .POST => .POST,
            .PUT => .PUT,
            .DELETE => .DELETE,
            .PATCH => .PATCH,
            .HEAD => .HEAD,
            .OPTIONS => .OPTIONS,
            .CONNECT => .CONNECT,
            .TRACE => .TRACE,
        };
    }

    pub fn toMethod(self: MethodCode) parser.Method {
        return switch (self) {
            .GET => .GET,
            .POST => .POST,
            .PUT => .PUT,
            .DELETE => .DELETE,
            .PATCH => .PATCH,
            .HEAD => .HEAD,
            .OPTIONS => .OPTIONS,
            .CONNECT => .CONNECT,
            .TRACE => .TRACE,
        };
    }

    pub fn toString(self: MethodCode) []const u8 {
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

/// Serialize a ParsedRequest to binary format
/// Returns: number of bytes written
pub fn serializeRequestBinary(req: *const parser.ParsedRequest, buf: []u8) usize {
    var pos: usize = 0;

    // Method (1 byte)
    buf[pos] = @intFromEnum(MethodCode.fromMethod(req.method));
    pos += 1;

    // URL length (2 bytes, little-endian)
    const url_len: u16 = @intCast(req.url.len);
    std.mem.writeInt(u16, buf[pos..][0..2], url_len, .little);
    pos += 2;

    // Headers count (1 byte)
    buf[pos] = @intCast(req.header_count);
    pos += 1;

    // Body length (4 bytes, little-endian)
    const body_len: u32 = if (req.body) |b| @intCast(b.len) else 0;
    std.mem.writeInt(u32, buf[pos..][0..4], body_len, .little);
    pos += 4;

    // URL data
    @memcpy(buf[pos..][0..req.url.len], req.url);
    pos += req.url.len;

    // Headers: [{name_len(1), value_len(2), name, value}, ...]
    for (req.headers[0..req.header_count]) |h| {
        // Name length (1 byte)
        buf[pos] = @intCast(h.name.len);
        pos += 1;

        // Value length (2 bytes, little-endian)
        const val_len: u16 = @intCast(h.value.len);
        std.mem.writeInt(u16, buf[pos..][0..2], val_len, .little);
        pos += 2;

        // Name (lowercase)
        for (h.name, 0..) |ch, i| {
            buf[pos + i] = std.ascii.toLower(ch);
        }
        pos += h.name.len;

        // Value
        @memcpy(buf[pos..][0..h.value.len], h.value);
        pos += h.value.len;
    }

    // Body data
    if (req.body) |body| {
        @memcpy(buf[pos..][0..body.len], body);
        pos += body.len;
    }

    return pos;
}

/// Parsed binary response (returned by parseResponseBinary)
pub const BinaryResponse = struct {
    status: u16,
    content_type: []const u8,
    body: []const u8,
};

/// Parse binary response format
/// Returns null on error
pub fn parseResponseBinary(buf: []const u8) ?BinaryResponse {
    if (buf.len < RESPONSE_HEADER_SIZE) return null;

    // Status (2 bytes, little-endian)
    const status = std.mem.readInt(u16, buf[0..2], .little);

    // Content-type length (1 byte)
    const ct_len: usize = buf[2];

    // Body length (4 bytes, little-endian)
    const body_len = std.mem.readInt(u32, buf[3..7], .little);

    // Validate lengths
    const total_len = RESPONSE_HEADER_SIZE + ct_len + body_len;
    if (buf.len < total_len) return null;

    // Extract content-type
    const content_type = buf[RESPONSE_HEADER_SIZE..][0..ct_len];

    // Extract body
    const body_start = RESPONSE_HEADER_SIZE + ct_len;
    const body = buf[body_start..][0..body_len];

    return BinaryResponse{
        .status = status,
        .content_type = content_type,
        .body = body,
    };
}

/// Serialize a response to binary format (for testing from Zig side)
pub fn serializeResponseBinary(status: u16, content_type: []const u8, body: []const u8, buf: []u8) usize {
    var pos: usize = 0;

    // Status (2 bytes)
    std.mem.writeInt(u16, buf[pos..][0..2], status, .little);
    pos += 2;

    // Content-type length (1 byte)
    buf[pos] = @intCast(content_type.len);
    pos += 1;

    // Body length (4 bytes)
    std.mem.writeInt(u32, buf[pos..][0..4], @intCast(body.len), .little);
    pos += 4;

    // Content-type
    @memcpy(buf[pos..][0..content_type.len], content_type);
    pos += content_type.len;

    // Body
    @memcpy(buf[pos..][0..body.len], body);
    pos += body.len;

    return pos;
}

// Tests
test "serialize and parse request" {
    var headers: [parser.MAX_HEADERS]parser.Header = undefined;
    headers[0] = .{ .name = "Host", .value = "localhost" };
    headers[1] = .{ .name = "Content-Type", .value = "text/plain" };

    const req = parser.ParsedRequest{
        .method = .POST,
        .url = "/api/test",
        .version = "HTTP/1.1",
        .headers = &headers,
        .header_count = 2,
        .body = "Hello",
        .keep_alive = true,
        .content_length = 5,
    };

    var buf: [1024]u8 = undefined;
    const len = serializeRequestBinary(&req, &buf);

    // Verify header
    try std.testing.expectEqual(@as(u8, 1), buf[0]); // POST = 1
    try std.testing.expectEqual(@as(u16, 9), std.mem.readInt(u16, buf[1..3], .little)); // url_len
    try std.testing.expectEqual(@as(u8, 2), buf[3]); // headers_count
    try std.testing.expectEqual(@as(u32, 5), std.mem.readInt(u32, buf[4..8], .little)); // body_len

    // Verify URL
    try std.testing.expectEqualStrings("/api/test", buf[8..17]);

    _ = len;
}

test "serialize and parse response" {
    var buf: [1024]u8 = undefined;
    const len = serializeResponseBinary(200, "application/json", "{\"ok\":true}", &buf);

    const resp = parseResponseBinary(buf[0..len]).?;
    try std.testing.expectEqual(@as(u16, 200), resp.status);
    try std.testing.expectEqualStrings("application/json", resp.content_type);
    try std.testing.expectEqualStrings("{\"ok\":true}", resp.body);
}
