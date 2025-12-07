/// WASI Socket Extensions for WasmEdge
/// Provides network socket support for WASM modules running in WasmEdge
///
/// These are WasmEdge-specific extensions to WASI (not yet standardized)
/// Based on: https://github.com/second-state/wasmedge_wasi_socket
const std = @import("std");

// Address family constants
pub const AF_UNSPEC: u8 = 0;
pub const AF_INET: u8 = 1; // IPv4
pub const AF_INET6: u8 = 2; // IPv6

// Socket type constants
pub const SOCK_ANY: u8 = 0;
pub const SOCK_DGRAM: u8 = 1; // UDP
pub const SOCK_STREAM: u8 = 2; // TCP

// Shutdown flags
pub const SHUT_RD: u8 = 1;
pub const SHUT_WR: u8 = 2;
pub const SHUT_RDWR: u8 = 3;

/// WASI Address structure (for IPv4/IPv6)
pub const WasiAddress = extern struct {
    buf: [*]u8,
    buf_len: usize,
};

/// Iovec for reading
pub const IovecRead = extern struct {
    buf: [*]u8,
    buf_len: usize,
};

/// Iovec for writing
pub const IovecWrite = extern struct {
    buf: [*]const u8,
    buf_len: usize,
};

// WasmEdge WASI socket extensions (imported from host)
extern "wasi_snapshot_preview1" fn sock_open(addr_family: u8, sock_type: u8, fd: *u32) u32;
extern "wasi_snapshot_preview1" fn sock_bind(fd: u32, addr: *WasiAddress, port: u32) u32;
extern "wasi_snapshot_preview1" fn sock_listen(fd: u32, backlog: u32) u32;
extern "wasi_snapshot_preview1" fn sock_accept(fd: u32, new_fd: *u32) u32;
extern "wasi_snapshot_preview1" fn sock_connect(fd: u32, addr: *WasiAddress, port: u32) u32;
extern "wasi_snapshot_preview1" fn sock_recv(fd: u32, buf: *IovecRead, buf_len: usize, flags: u16, recv_len: *usize, oflags: *usize) u32;
extern "wasi_snapshot_preview1" fn sock_send(fd: u32, buf: *const IovecWrite, buf_len: u32, flags: u16, send_len: *u32) u32;
extern "wasi_snapshot_preview1" fn sock_shutdown(fd: u32, flags: u8) u32;
extern "wasi_snapshot_preview1" fn sock_getaddrinfo(node: [*]const u8, node_len: u32, server: [*]const u8, server_len: u32, hint: ?*const anyopaque, res: *u32, max_len: u32, res_len: *u32) u32;

pub const SocketError = error{
    InvalidArgument,
    AddressInUse,
    AddressNotAvailable,
    ConnectionRefused,
    ConnectionReset,
    NetworkUnreachable,
    HostUnreachable,
    TimedOut,
    WouldBlock,
    NotConnected,
    PermissionDenied,
    Unknown,
};

/// Convert WASI error code to Zig error
fn wasiError(code: u32) SocketError {
    return switch (code) {
        0 => unreachable, // Success
        28 => SocketError.InvalidArgument, // EINVAL
        48 => SocketError.AddressInUse, // EADDRINUSE
        49 => SocketError.AddressNotAvailable, // EADDRNOTAVAIL
        61 => SocketError.ConnectionRefused, // ECONNREFUSED
        54 => SocketError.ConnectionReset, // ECONNRESET
        51 => SocketError.NetworkUnreachable, // ENETUNREACH
        65 => SocketError.HostUnreachable, // EHOSTUNREACH
        60 => SocketError.TimedOut, // ETIMEDOUT
        35 => SocketError.WouldBlock, // EAGAIN
        57 => SocketError.NotConnected, // ENOTCONN
        1 => SocketError.PermissionDenied, // EPERM
        else => SocketError.Unknown,
    };
}

/// TCP Socket wrapper
pub const TcpSocket = struct {
    fd: u32,

    const Self = @This();

    /// Create a new TCP socket
    pub fn open(ipv6: bool) SocketError!Self {
        var fd: u32 = undefined;
        const family: u8 = if (ipv6) AF_INET6 else AF_INET;
        const ret = sock_open(family, SOCK_STREAM, &fd);
        if (ret != 0) return wasiError(ret);
        return .{ .fd = fd };
    }

    /// Connect to a remote address
    pub fn connect(self: *Self, ip: []const u8, port: u16) SocketError!void {
        var addr = WasiAddress{
            .buf = @constCast(ip.ptr),
            .buf_len = ip.len,
        };
        const ret = sock_connect(self.fd, &addr, port);
        if (ret != 0) return wasiError(ret);
    }

    /// Send data
    pub fn send(self: *Self, data: []const u8) SocketError!usize {
        var iov = IovecWrite{
            .buf = data.ptr,
            .buf_len = data.len,
        };
        var sent: u32 = 0;
        const ret = sock_send(self.fd, &iov, 1, 0, &sent);
        if (ret != 0) return wasiError(ret);
        return sent;
    }

    /// Send all data
    pub fn sendAll(self: *Self, data: []const u8) SocketError!void {
        var remaining = data;
        while (remaining.len > 0) {
            const sent = try self.send(remaining);
            remaining = remaining[sent..];
        }
    }

    /// Receive data
    pub fn recv(self: *Self, buffer: []u8) SocketError!usize {
        var iov = IovecRead{
            .buf = buffer.ptr,
            .buf_len = buffer.len,
        };
        var received: usize = 0;
        var oflags: usize = 0;
        const ret = sock_recv(self.fd, &iov, 1, 0, &received, &oflags);
        if (ret != 0) return wasiError(ret);
        return received;
    }

    /// Shutdown socket
    pub fn shutdown(self: *Self, how: enum { read, write, both }) SocketError!void {
        const flags: u8 = switch (how) {
            .read => SHUT_RD,
            .write => SHUT_WR,
            .both => SHUT_RDWR,
        };
        const ret = sock_shutdown(self.fd, flags);
        if (ret != 0) return wasiError(ret);
    }

    /// Close socket (use WASI fd_close)
    pub fn close(self: *Self) void {
        // fd_close is standard WASI (cast to i32 for fd_t)
        _ = std.os.wasi.fd_close(@intCast(self.fd));
    }
};

/// Resolve hostname to IP address
pub fn resolveHost(allocator: std.mem.Allocator, host: []const u8, port: u16) ![]u8 {
    var port_str_buf: [8]u8 = undefined;
    const port_str = std.fmt.bufPrint(&port_str_buf, "{d}", .{port}) catch unreachable;

    var res: u32 = 0;
    var res_len: u32 = 0;

    const ret = sock_getaddrinfo(
        host.ptr,
        @intCast(host.len),
        port_str.ptr,
        @intCast(port_str.len),
        null,
        &res,
        16, // max results
        &res_len,
    );

    if (ret != 0) return error.HostNotFound;
    if (res_len == 0) return error.HostNotFound;

    // For now, return IPv4 localhost as fallback
    // TODO: Parse actual getaddrinfo result
    const ip = try allocator.alloc(u8, 4);
    ip[0] = 127;
    ip[1] = 0;
    ip[2] = 0;
    ip[3] = 1;
    return ip;
}

/// Simple HTTP GET request over WASI sockets
pub fn httpGet(allocator: std.mem.Allocator, host: []const u8, port: u16, path: []const u8) ![]u8 {
    // Open socket
    var sock = try TcpSocket.open(false);
    defer sock.close();

    // Resolve and connect
    const ip = try resolveHost(allocator, host, port);
    defer allocator.free(ip);

    try sock.connect(ip, port);

    // Build HTTP request
    var request = std.ArrayList(u8){};
    defer request.deinit(allocator);

    const writer = request.writer(allocator);
    try writer.print("GET {s} HTTP/1.1\r\n", .{path});
    try writer.print("Host: {s}\r\n", .{host});
    try writer.writeAll("Connection: close\r\n");
    try writer.writeAll("User-Agent: EdgeBox/1.0\r\n");
    try writer.writeAll("\r\n");

    // Send request
    try sock.sendAll(request.items);

    // Read response
    var response = std.ArrayList(u8){};
    defer response.deinit(allocator);

    var buf: [4096]u8 = undefined;
    while (true) {
        const n = sock.recv(&buf) catch break;
        if (n == 0) break;
        try response.appendSlice(allocator, buf[0..n]);
    }

    return try response.toOwnedSlice(allocator);
}
