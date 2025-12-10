/// WASI Socket Extensions - WAMR Compatible
/// Provides network socket support using WAMR's WASI socket API
///
/// WAMR signature conventions:
/// - `i` = i32
/// - `*` = pointer (i32 in WASM)
/// - `$` = string (null-terminated, WAMR auto-extracts ptr+len)
/// - `I` = i64
const std = @import("std");

// Address family constants (WAMR uses these values)
pub const AF_INET4: u8 = 0; // IPv4 (INET4 in WAMR)
pub const AF_INET6: u8 = 1; // IPv6 (INET6 in WAMR)
pub const AF_UNSPEC: u8 = 2; // Unspecified (INET_UNSPEC in WAMR)

// Socket type constants
pub const SOCK_DGRAM: u8 = 0; // UDP
pub const SOCK_STREAM: u8 = 1; // TCP

// Shutdown flags
pub const SHUT_RD: u8 = 1;
pub const SHUT_WR: u8 = 2;
pub const SHUT_RDWR: u8 = 3;

// Address type for __wasi_addr_t
pub const AddrType = enum(u32) {
    IPv4 = 0,
    IPv6 = 1,
};

/// IPv4 address (4 bytes)
pub const AddrIP4 = extern struct {
    n0: u8,
    n1: u8,
    n2: u8,
    n3: u8,
};

/// IPv4 address with port
pub const AddrIP4Port = extern struct {
    addr: AddrIP4,
    port: u16, // host byte order
};

/// IPv6 address (16 bytes as 8 x u16)
pub const AddrIP6 = extern struct {
    n0: u16,
    n1: u16,
    n2: u16,
    n3: u16,
    h0: u16,
    h1: u16,
    h2: u16,
    h3: u16,
};

/// IPv6 address with port
pub const AddrIP6Port = extern struct {
    addr: AddrIP6,
    port: u16,
};

/// WASI address structure - matches WAMR's __wasi_addr_t
pub const WasiAddr = extern struct {
    kind: AddrType,
    addr: extern union {
        ip4: AddrIP4Port,
        ip6: AddrIP6Port,
    },
};

/// Iovec for WAMR - uses WASM app addresses
pub const IovecApp = extern struct {
    buf: u32, // WASM linear memory offset
    buf_len: u32,
};

/// Address info hints
pub const AddrInfoHints = extern struct {
    sock_type: u8,
    address_family: u8,
    hints_enabled: u8,
};

/// Address info result
pub const WasiAddrInfo = extern struct {
    addr: WasiAddr,
    sock_type: u8,
};

// =============================================================================
// WAMR WASI Socket Functions
// Signatures MUST match WAMR's libc_wasi_wrapper.c exactly
// =============================================================================

/// sock_open: (iii*)i - (poolfd, af, socktype, *sockfd) -> errno
extern "wasi_snapshot_preview1" fn sock_open(poolfd: i32, af: i32, socktype: i32, sockfd: *i32) u16;

/// sock_connect: (i*)i - (fd, *addr) -> errno
extern "wasi_snapshot_preview1" fn sock_connect(fd: i32, addr: *const WasiAddr) u16;

/// sock_send: (i*ii*)i - (fd, *iov, iov_len, flags, *sent) -> errno
extern "wasi_snapshot_preview1" fn sock_send(fd: i32, si_data: *const IovecApp, si_data_len: u32, si_flags: u16, so_data_len: *u32) u16;

/// sock_recv: (i*ii**)i - (fd, *iov, iov_len, flags, *recvd, *oflags) -> errno
extern "wasi_snapshot_preview1" fn sock_recv(fd: i32, ri_data: *IovecApp, ri_data_len: u32, ri_flags: u16, ro_data_len: *u32, ro_flags: *u16) u16;

/// sock_shutdown: (ii)i - (fd, how) -> errno
extern "wasi_snapshot_preview1" fn sock_shutdown(fd: i32, how: i32) u16;

/// sock_listen: (ii)i - (fd, backlog) -> errno
extern "wasi_snapshot_preview1" fn sock_listen(fd: i32, backlog: i32) u16;

/// sock_bind: (i*)i - (fd, *addr) -> errno
extern "wasi_snapshot_preview1" fn sock_bind(fd: i32, addr: *const WasiAddr) u16;

/// sock_accept: (ii*)i - (fd, flags, *new_fd) -> errno
extern "wasi_snapshot_preview1" fn sock_accept(fd: i32, flags: u16, new_fd: *i32) u16;

// NOTE: sock_addr_resolve uses WAMR's special '$' string type
// We can't directly call it from Zig because Zig generates (i32,i32,i32,i32,i32,i32)->i32
// but WAMR expects ($$**i*)i with auto string handling
// For now, we'll resolve DNS differently or use IP addresses directly

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
    NotSupported,
    Unknown,
};

/// Convert WASI error code to Zig error
fn wasiError(code: u16) SocketError {
    return switch (code) {
        0 => unreachable, // Success
        28 => SocketError.InvalidArgument, // EINVAL
        48 => SocketError.AddressInUse, // EADDRINUSE
        49 => SocketError.AddressNotAvailable, // EADDRNOTAVAIL
        58 => SocketError.NotSupported, // ENOTSUP
        61 => SocketError.ConnectionRefused, // ECONNREFUSED
        54 => SocketError.ConnectionReset, // ECONNRESET
        51 => SocketError.NetworkUnreachable, // ENETUNREACH
        65 => SocketError.HostUnreachable, // EHOSTUNREACH
        60 => SocketError.TimedOut, // ETIMEDOUT
        35 => SocketError.WouldBlock, // EAGAIN
        57 => SocketError.NotConnected, // ENOTCONN
        1, 63 => SocketError.PermissionDenied, // EPERM, EACCES
        else => SocketError.Unknown,
    };
}

/// TCP Socket wrapper
pub const TcpSocket = struct {
    fd: i32,

    const Self = @This();

    /// Create a new TCP socket
    pub fn open(ipv6: bool) SocketError!Self {
        var fd: i32 = undefined;
        const family: i32 = if (ipv6) AF_INET6 else AF_INET4;
        // poolfd=3 is conventional for preopened socket pool
        const ret = sock_open(3, family, SOCK_STREAM, &fd);
        if (ret != 0) return wasiError(ret);
        return .{ .fd = fd };
    }

    /// Connect to a remote IPv4 address
    pub fn connect(self: *Self, ip: []const u8, port: u16) SocketError!void {
        if (ip.len != 4) return SocketError.InvalidArgument;

        var addr = WasiAddr{
            .kind = .IPv4,
            .addr = .{
                .ip4 = .{
                    .addr = .{
                        .n0 = ip[0],
                        .n1 = ip[1],
                        .n2 = ip[2],
                        .n3 = ip[3],
                    },
                    .port = port,
                },
            },
        };
        const ret = sock_connect(self.fd, &addr);
        if (ret != 0) return wasiError(ret);
    }

    /// Bind to a local address
    pub fn bindAddr(self: *Self, ip: []const u8, port: u16) SocketError!void {
        if (ip.len != 4) return SocketError.InvalidArgument;

        var addr = WasiAddr{
            .kind = .IPv4,
            .addr = .{
                .ip4 = .{
                    .addr = .{
                        .n0 = ip[0],
                        .n1 = ip[1],
                        .n2 = ip[2],
                        .n3 = ip[3],
                    },
                    .port = port,
                },
            },
        };
        const ret = sock_bind(self.fd, &addr);
        if (ret != 0) return wasiError(ret);
    }

    /// Listen for connections
    pub fn listen(self: *Self, backlog: i32) SocketError!void {
        const ret = sock_listen(self.fd, backlog);
        if (ret != 0) return wasiError(ret);
    }

    /// Accept a connection
    pub fn accept(self: *Self) SocketError!Self {
        var new_fd: i32 = undefined;
        const ret = sock_accept(self.fd, 0, &new_fd);
        if (ret != 0) return wasiError(ret);
        return .{ .fd = new_fd };
    }

    /// Send data
    pub fn send(self: *Self, data: []const u8) SocketError!usize {
        var iov = IovecApp{
            .buf = @intFromPtr(data.ptr),
            .buf_len = @intCast(data.len),
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
        var iov = IovecApp{
            .buf = @intFromPtr(buffer.ptr),
            .buf_len = @intCast(buffer.len),
        };
        var received: u32 = 0;
        var oflags: u16 = 0;
        const ret = sock_recv(self.fd, &iov, 1, 0, &received, &oflags);
        if (ret != 0) return wasiError(ret);
        return received;
    }

    /// Shutdown socket
    pub fn shutdown(self: *Self, how: enum { read, write, both }) SocketError!void {
        const flags: i32 = switch (how) {
            .read => SHUT_RD,
            .write => SHUT_WR,
            .both => SHUT_RDWR,
        };
        const ret = sock_shutdown(self.fd, flags);
        if (ret != 0) return wasiError(ret);
    }

    /// Close socket
    pub fn close(self: *Self) void {
        _ = std.os.wasi.fd_close(@intCast(self.fd));
    }
};

/// Parse IPv4 address string "1.2.3.4" to bytes
pub fn parseIPv4(str: []const u8) ?[4]u8 {
    var result: [4]u8 = undefined;
    var idx: usize = 0;
    var octet: u8 = 0;
    var digits: u8 = 0;

    for (str) |c| {
        if (c == '.') {
            if (digits == 0) return null;
            result[idx] = octet;
            idx += 1;
            if (idx > 3) return null;
            octet = 0;
            digits = 0;
        } else if (c >= '0' and c <= '9') {
            octet = octet * 10 + (c - '0');
            digits += 1;
            if (digits > 3) return null;
        } else {
            return null;
        }
    }

    if (digits == 0 or idx != 3) return null;
    result[3] = octet;
    return result;
}

/// Resolve hostname - for now just parse IP or return localhost
/// TODO: Implement proper DNS via alternative method (not sock_addr_resolve)
pub fn resolveHost(allocator: std.mem.Allocator, host: []const u8, port: u16) ![]u8 {
    _ = port;

    // Try to parse as IP address first
    if (parseIPv4(host)) |ip| {
        const result = try allocator.alloc(u8, 4);
        @memcpy(result, &ip);
        return result;
    }

    // Known hosts (hardcoded for common services)
    if (std.mem.eql(u8, host, "localhost") or std.mem.eql(u8, host, "127.0.0.1")) {
        const result = try allocator.alloc(u8, 4);
        result[0] = 127;
        result[1] = 0;
        result[2] = 0;
        result[3] = 1;
        return result;
    }

    // For other hosts, return localhost as fallback
    // Real DNS resolution needs WAMR's special string handling
    const result = try allocator.alloc(u8, 4);
    result[0] = 127;
    result[1] = 0;
    result[2] = 0;
    result[3] = 1;
    return result;
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
    var request = std.ArrayListUnmanaged(u8){};
    defer request.deinit(allocator);

    try request.appendSlice(allocator, "GET ");
    try request.appendSlice(allocator, path);
    try request.appendSlice(allocator, " HTTP/1.1\r\nHost: ");
    try request.appendSlice(allocator, host);
    try request.appendSlice(allocator, "\r\nConnection: close\r\nUser-Agent: EdgeBox/1.0\r\n\r\n");

    // Send request
    try sock.sendAll(request.items);

    // Read response
    var response = std.ArrayListUnmanaged(u8){};
    defer response.deinit(allocator);

    var buf: [4096]u8 = undefined;
    while (true) {
        const n = sock.recv(&buf) catch break;
        if (n == 0) break;
        try response.appendSlice(allocator, buf[0..n]);
    }

    return try response.toOwnedSlice(allocator);
}
