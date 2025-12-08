/// WASM Fetch Implementation using WASI Sockets
/// For use in QuickJS WASM runtime with WasmEdge
const std = @import("std");
const wasi_sock = @import("wasi_sock.zig");
const wasi_tls = @import("wasi_tls.zig");

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
    headers: std.ArrayList(Header),
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

/// Perform HTTP fetch using WASI sockets (WasmEdge)
pub fn fetch(allocator: std.mem.Allocator, url: []const u8, options: FetchOptions) FetchError!FetchResponse {

    // Parse URL
    const uri = std.Uri.parse(url) catch {
        return FetchError.InvalidUrl;
    };

    const scheme = uri.scheme;
    const host = getHostString(uri.host) orelse {
        return FetchError.InvalidUrl;
    };
    const port: u16 = uri.port orelse if (std.mem.eql(u8, scheme, "https")) @as(u16, 443) else @as(u16, 80);
    const path = getPathString(uri.path);


    // Resolve hostname
    const ip = wasi_sock.resolveHost(allocator, host, port) catch {
        return FetchError.HostNotFound;
    };
    defer allocator.free(ip);

    // HTTPS or HTTP?
    const is_https = std.mem.eql(u8, scheme, "https");

    // TLS socket for HTTPS
    var tls_sock: ?*wasi_tls.TlsSocket = null;
    defer if (tls_sock) |s| s.close();

    // Plain socket for HTTP
    var tcp_sock: ?wasi_sock.TcpSocket = null;
    defer if (tcp_sock) |*s| s.close();

    if (is_https) {
        tls_sock = wasi_tls.TlsSocket.connect(allocator, ip, port, host) catch return FetchError.ConnectionFailed;
    } else {
        tcp_sock = wasi_sock.TcpSocket.open(false) catch return FetchError.ConnectionFailed;
        tcp_sock.?.connect(ip, port) catch return FetchError.ConnectionFailed;
    }

    // Build HTTP request
    var request = std.ArrayList(u8){};
    defer request.deinit(allocator);

    const writer = request.writer(allocator);
    writer.print("{s} {s}", .{ options.method.toString(), path }) catch return FetchError.OutOfMemory;
    if (getQueryString(uri.query)) |q| {
        writer.print("?{s}", .{q}) catch return FetchError.OutOfMemory;
    }
    writer.writeAll(" HTTP/1.1\r\n") catch return FetchError.OutOfMemory;
    writer.print("Host: {s}\r\n", .{host}) catch return FetchError.OutOfMemory;
    writer.writeAll("Connection: close\r\n") catch return FetchError.OutOfMemory;
    writer.writeAll("User-Agent: EdgeBox/1.0\r\n") catch return FetchError.OutOfMemory;

    // Custom headers
    if (options.headers) |headers| {
        for (headers) |h| {
            writer.print("{s}: {s}\r\n", .{ h.name, h.value }) catch return FetchError.OutOfMemory;
        }
    }

    // Body
    if (options.body) |body| {
        writer.print("Content-Length: {d}\r\n", .{body.len}) catch return FetchError.OutOfMemory;
        writer.writeAll("\r\n") catch return FetchError.OutOfMemory;
        writer.writeAll(body) catch return FetchError.OutOfMemory;
    } else {
        writer.writeAll("\r\n") catch return FetchError.OutOfMemory;
    }

    // Send request
    if (tls_sock) |tls| {
        tls.sendAll(request.items) catch return FetchError.ConnectionFailed;
    } else if (tcp_sock) |*tcp| {
        tcp.sendAll(request.items) catch return FetchError.ConnectionFailed;
    }

    // Read response
    var response_buf = std.ArrayList(u8){};
    errdefer response_buf.deinit(allocator);

    var buf: [8192]u8 = undefined;
    while (true) {
        const n = if (tls_sock) |tls|
            tls.recv(&buf) catch {
                break;
            }
        else if (tcp_sock) |*tcp|
            tcp.recv(&buf) catch break
        else
            break;
        if (n == 0) break;
        response_buf.appendSlice(allocator, buf[0..n]) catch return FetchError.OutOfMemory;
    }

    const response_data = response_buf.toOwnedSlice(allocator) catch return FetchError.OutOfMemory;
    errdefer allocator.free(response_data);

    // Parse response
    const header_end = std.mem.indexOf(u8, response_data, "\r\n\r\n") orelse return FetchError.InvalidResponse;
    const header_section = response_data[0..header_end];
    const body_start = header_end + 4;

    // Parse status line
    const status_end = std.mem.indexOf(u8, header_section, "\r\n") orelse return FetchError.InvalidResponse;
    const status_line = header_section[0..status_end];

    // "HTTP/1.1 200 OK" -> extract 200
    var parts = std.mem.splitScalar(u8, status_line, ' ');
    _ = parts.next(); // HTTP/1.1
    const status_str = parts.next() orelse return FetchError.InvalidResponse;
    const status = std.fmt.parseInt(u16, status_str, 10) catch return FetchError.InvalidResponse;

    // Parse headers
    var headers = std.ArrayList(Header){};
    var header_lines = std.mem.splitSequence(u8, header_section[status_end + 2 ..], "\r\n");
    while (header_lines.next()) |line| {
        if (line.len == 0) continue;
        if (std.mem.indexOf(u8, line, ": ")) |sep| {
            headers.append(allocator, .{
                .name = line[0..sep],
                .value = line[sep + 2 ..],
            }) catch return FetchError.OutOfMemory;
        }
    }

    // Extract body
    const body = if (body_start < response_data.len)
        allocator.dupe(u8, response_data[body_start..]) catch return FetchError.OutOfMemory
    else
        allocator.alloc(u8, 0) catch return FetchError.OutOfMemory;

    allocator.free(response_data);

    return FetchResponse{
        .status = status,
        .headers = headers,
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
