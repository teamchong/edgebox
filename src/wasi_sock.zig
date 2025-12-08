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

/// WasmEdge addrinfo structure layout (28 bytes)
/// Matches __wasi_addrinfo_t from WasmEdge
const WasiAddrinfo = extern struct {
    ai_flags: u16, // offset 0
    ai_family: u8, // offset 2 (1 = IPv4, 2 = IPv6)
    ai_socktype: u8, // offset 3
    ai_protocol: u8, // offset 4 (not u32 - see api.hpp)
    _pad: [3]u8, // padding to align ai_addrlen
    ai_addrlen: u32, // offset 8
    ai_addr: u32, // offset 12 (pointer to sockaddr)
    ai_canonname: u32, // offset 16
    ai_canonname_len: u32, // offset 20
    ai_next: u32, // offset 24
};

/// WasmEdge sockaddr structure layout (12 bytes)
/// Matches __wasi_sockaddr_t from WasmEdge
const WasiSockaddr = extern struct {
    sa_family: u8, // offset 0 (address family)
    _pad: [3]u8, // padding to align sa_data_len
    sa_data_len: u32, // offset 4
    sa_data: u32, // offset 8 (pointer to address data)
};

/// Maximum sa_data length (same as WasmEdge kMaxSaDataLen = 26)
const kMaxSaDataLen: u32 = 26;

/// Pre-allocated buffer structure for sock_getaddrinfo
/// Layout: result_ptr(4) + res_len(4) + hints(28) + [entries]
/// Entry layout: addrinfo(28) + sockaddr(12) + sa_data(26) + canonname(64) = 130 bytes
const AddrinfoEntry = extern struct {
    addrinfo: WasiAddrinfo,
    sockaddr: WasiSockaddr,
    sa_data: [kMaxSaDataLen]u8,
    canonname: [64]u8,
};

/// Complete buffer including control fields at the start
const DnsBuffer = extern struct {
    result_ptr: u32, // Pointer to first addrinfo (filled by us, read by WasmEdge)
    res_len: u32, // Result count (filled by WasmEdge)
    hints: WasiAddrinfo, // Hints structure
    entries: [4]AddrinfoEntry, // Pre-allocated result entries
};

/// Resolve hostname to IP address using WasmEdge sock_getaddrinfo
pub fn resolveHost(allocator: std.mem.Allocator, host: []const u8, port: u16) ![]u8 {
    var port_str_buf: [8]u8 = undefined;
    const port_str = std.fmt.bufPrint(&port_str_buf, "{d}", .{port}) catch unreachable;

    // Pre-allocate buffer with all structures
    var buffer: DnsBuffer align(4) = undefined;
    const buffer_base = @intFromPtr(&buffer);
    const max_results: u32 = 4;

    // Set up result_ptr to point to the first entry
    const entries_offset = @offsetOf(DnsBuffer, "entries");
    buffer.result_ptr = @intCast(buffer_base + entries_offset);
    buffer.res_len = 0;

    // Initialize hints
    buffer.hints = .{
        .ai_flags = 0,
        .ai_family = AF_UNSPEC, // Accept both IPv4 and IPv6
        .ai_socktype = SOCK_STREAM, // TCP
        .ai_protocol = 0, // Any protocol
        ._pad = .{ 0, 0, 0 },
        .ai_addrlen = 0,
        .ai_addr = 0,
        .ai_canonname = 0,
        .ai_canonname_len = 0,
        .ai_next = 0,
    };

    // Initialize each entry with proper pointers
    for (0..max_results) |i| {
        const entry_base = buffer_base + entries_offset + i * @sizeOf(AddrinfoEntry);
        buffer.entries[i].addrinfo = .{
            .ai_flags = 0,
            .ai_family = AF_UNSPEC,
            .ai_socktype = SOCK_ANY,
            .ai_protocol = 0,
            ._pad = .{ 0, 0, 0 },
            .ai_addrlen = @sizeOf(WasiSockaddr),
            .ai_addr = @intCast(entry_base + @offsetOf(AddrinfoEntry, "sockaddr")),
            .ai_canonname = @intCast(entry_base + @offsetOf(AddrinfoEntry, "canonname")),
            .ai_canonname_len = 64,
            .ai_next = if (i < max_results - 1) @intCast(entry_base + @sizeOf(AddrinfoEntry)) else 0,
        };
        buffer.entries[i].sockaddr = .{
            .sa_family = 0,
            ._pad = .{ 0, 0, 0 },
            .sa_data_len = kMaxSaDataLen,
            .sa_data = @intCast(entry_base + @offsetOf(AddrinfoEntry, "sa_data")),
        };
        @memset(&buffer.entries[i].sa_data, 0);
        @memset(&buffer.entries[i].canonname, 0);
    }

    const ret = sock_getaddrinfo(
        host.ptr,
        @intCast(host.len),
        port_str.ptr,
        @intCast(port_str.len),
        @ptrCast(&buffer.hints),
        &buffer.result_ptr,
        max_results,
        &buffer.res_len,
    );


    if (ret != 0) return error.HostNotFound;
    if (buffer.res_len == 0) return error.HostNotFound;

    // Parse the first addrinfo result
    const addrinfo: *const WasiAddrinfo = @ptrFromInt(buffer.result_ptr);

    // Check for IPv4 (family = 1)
    if (addrinfo.ai_family == AF_INET and addrinfo.ai_addr != 0) {
        const sockaddr: *const WasiSockaddr = @ptrFromInt(addrinfo.ai_addr);

        // sa_data contains: port(2 bytes) + IP(4 bytes) for IPv4
        if (sockaddr.sa_data != 0 and sockaddr.sa_data_len >= 6) {
            const sa_data: [*]const u8 = @ptrFromInt(sockaddr.sa_data);
            const ip = try allocator.alloc(u8, 4);
            // Skip port (2 bytes), get IP (4 bytes)
            ip[0] = sa_data[2];
            ip[1] = sa_data[3];
            ip[2] = sa_data[4];
            ip[3] = sa_data[5];
            return ip;
        }
    }

    // Check for IPv6 (family = 2)
    if (addrinfo.ai_family == AF_INET6 and addrinfo.ai_addr != 0) {
    }

    // Fallback: return localhost if parsing fails
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
