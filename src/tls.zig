/// TLS/HTTPS Support for EdgeBox
/// Using Zig's stdlib TLS implementation
const std = @import("std");
const net = std.net;
const tls = std.crypto.tls;

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

pub const TlsError = error{
    HandshakeFailed,
    CertificateVerificationFailed,
    ConnectionClosed,
    Unexpected,
    OutOfMemory,
};

/// TLS Client connection
pub const TlsClient = struct {
    stream: net.Stream,
    tls_client: tls.Client(net.Stream),

    const Self = @This();

    /// Connect to a TLS server
    pub fn connect(allocator: std.mem.Allocator, host: []const u8, port: u16) !Self {
        // Resolve hostname
        const addresses = try net.getAddressList(allocator, host, port);
        defer addresses.deinit();

        if (addresses.addrs.len == 0) {
            return error.UnknownHostName;
        }

        // Connect TCP
        const stream = try net.tcpConnectToAddress(addresses.addrs[0]);
        errdefer stream.close();

        // TLS handshake
        const tls_client = try tls.Client(net.Stream).init(stream, .{
            .host = host,
        });

        return .{
            .stream = stream,
            .tls_client = tls_client,
        };
    }

    /// Read data
    pub fn read(self: *Self, buffer: []u8) !usize {
        return self.tls_client.read(buffer);
    }

    /// Write data
    pub fn write(self: *Self, data: []const u8) !usize {
        return self.tls_client.write(data);
    }

    /// Write all data
    pub fn writeAll(self: *Self, data: []const u8) !void {
        var index: usize = 0;
        while (index < data.len) {
            index += try self.write(data[index..]);
        }
    }

    /// Close connection
    pub fn close(self: *Self) void {
        self.tls_client.close();
        self.stream.close();
    }
};

/// Simple HTTPS request
pub fn httpsGet(allocator: std.mem.Allocator, url: []const u8) ![]u8 {
    // Parse URL
    const uri = try std.Uri.parse(url);

    const host = getHostString(uri.host) orelse return error.InvalidUrl;
    const port: u16 = uri.port orelse 443;
    const path = getPathString(uri.path);

    // Connect
    var client = try TlsClient.connect(allocator, host, port);
    defer client.close();

    // Build request
    var request_buf: [4096]u8 = undefined;
    const request = try std.fmt.bufPrint(&request_buf, "GET {s} HTTP/1.1\r\nHost: {s}\r\nConnection: close\r\nUser-Agent: EdgeBox/1.0\r\n\r\n", .{ path, host });

    // Send request
    try client.writeAll(request);

    // Read response
    var response = std.ArrayList(u8).init(allocator);
    errdefer response.deinit();

    var buf: [8192]u8 = undefined;
    while (true) {
        const n = client.read(&buf) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        if (n == 0) break;
        try response.appendSlice(buf[0..n]);
    }

    return response.toOwnedSlice();
}

/// Parse HTTP response, return body
pub fn parseHttpResponse(response: []const u8) !struct { status: u16, headers: []const u8, body: []const u8 } {
    // Find end of headers
    const header_end = std.mem.indexOf(u8, response, "\r\n\r\n") orelse return error.InvalidResponse;

    const headers = response[0..header_end];
    const body = response[header_end + 4 ..];

    // Parse status line
    const status_line_end = std.mem.indexOf(u8, headers, "\r\n") orelse return error.InvalidResponse;
    const status_line = headers[0..status_line_end];

    // "HTTP/1.1 200 OK" -> extract 200
    var parts = std.mem.splitScalar(u8, status_line, ' ');
    _ = parts.next(); // HTTP/1.1
    const status_str = parts.next() orelse return error.InvalidResponse;
    const status = try std.fmt.parseInt(u16, status_str, 10);

    return .{
        .status = status,
        .headers = headers,
        .body = body,
    };
}

// Test
test "parse http response" {
    const response = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nHello World";
    const parsed = try parseHttpResponse(response);
    try std.testing.expectEqual(@as(u16, 200), parsed.status);
    try std.testing.expectEqualStrings("Hello World", parsed.body);
}
