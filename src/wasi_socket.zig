/// WASI Socket Implementation in Zig
/// Implements WASI socket extensions for network operations
///
/// Implements: sock_open, sock_bind, sock_listen, sock_accept,
///             sock_connect, sock_recv, sock_send, sock_shutdown
const std = @import("std");
const posix = std.posix;
const net = std.net;

// WASI Socket Types
pub const AddressFamily = enum(u8) {
    inet4 = 1,
    inet6 = 2,
    unix = 3,
};

pub const SockType = enum(u8) {
    stream = 1, // TCP
    dgram = 2, // UDP
};

pub const ShutdownHow = enum(u8) {
    rd = 1,
    wr = 2,
    rdwr = 3,
};

pub const RecvFlags = packed struct(u16) {
    peek: bool = false,
    waitall: bool = false,
    _padding: u14 = 0,
};

pub const SendFlags = packed struct(u16) {
    _padding: u16 = 0,
};

pub const SocketError = error{
    InvalidArgument,
    AddressInUse,
    AddressNotAvailable,
    ConnectionRefused,
    ConnectionReset,
    ConnectionAborted,
    NetworkUnreachable,
    HostUnreachable,
    TimedOut,
    WouldBlock,
    NotConnected,
    AlreadyConnected,
    PermissionDenied,
    NotSocket,
    BadFileDescriptor,
    OutOfMemory,
    Unknown,
};

/// Socket wrapper
pub const Socket = struct {
    fd: posix.fd_t,

    const Self = @This();

    /// Create a new socket
    pub fn open(family: AddressFamily, sock_type: SockType) SocketError!Self {
        const af: u32 = switch (family) {
            .inet4 => posix.AF.INET,
            .inet6 => posix.AF.INET6,
            .unix => posix.AF.UNIX,
        };

        const st: u32 = switch (sock_type) {
            .stream => posix.SOCK.STREAM,
            .dgram => posix.SOCK.DGRAM,
        };

        const fd = posix.socket(af, st, 0) catch |err| {
            return mapError(err);
        };

        return .{ .fd = fd };
    }

    /// Bind socket to address
    pub fn bind(self: *Self, address: []const u8, port: u16) SocketError!void {
        var addr: posix.sockaddr = undefined;
        var addr_len: posix.socklen_t = 0;

        if (address.len == 4) {
            // IPv4
            const addr4: posix.sockaddr.in = .{
                .family = posix.AF.INET,
                .port = std.mem.nativeToBig(u16, port),
                .addr = std.mem.bytesToValue([4]u8, address[0..4]).*,
                .zero = [_]u8{0} ** 8,
            };
            addr = @bitCast(addr4);
            addr_len = @sizeOf(posix.sockaddr.in);
        } else if (address.len == 16) {
            // IPv6
            const addr6: posix.sockaddr.in6 = .{
                .family = posix.AF.INET6,
                .port = std.mem.nativeToBig(u16, port),
                .flowinfo = 0,
                .addr = address[0..16].*,
                .scope_id = 0,
            };
            addr = @bitCast(addr6);
            addr_len = @sizeOf(posix.sockaddr.in6);
        } else {
            return SocketError.InvalidArgument;
        }

        posix.bind(self.fd, &addr, addr_len) catch |err| {
            return mapError(err);
        };
    }

    /// Listen for connections
    pub fn listen(self: *Self, backlog: u31) SocketError!void {
        posix.listen(self.fd, backlog) catch |err| {
            return mapError(err);
        };
    }

    /// Accept a connection
    pub fn accept(self: *Self) SocketError!Self {
        const result = posix.accept(self.fd, null, null, 0) catch |err| {
            return mapError(err);
        };
        return .{ .fd = result };
    }

    /// Connect to address
    pub fn connect(self: *Self, address: []const u8, port: u16) SocketError!void {
        var addr: posix.sockaddr = undefined;
        var addr_len: posix.socklen_t = 0;

        if (address.len == 4) {
            // IPv4
            const addr4: posix.sockaddr.in = .{
                .family = posix.AF.INET,
                .port = std.mem.nativeToBig(u16, port),
                .addr = std.mem.bytesToValue([4]u8, address[0..4]).*,
                .zero = [_]u8{0} ** 8,
            };
            addr = @bitCast(addr4);
            addr_len = @sizeOf(posix.sockaddr.in);
        } else if (address.len == 16) {
            // IPv6
            const addr6: posix.sockaddr.in6 = .{
                .family = posix.AF.INET6,
                .port = std.mem.nativeToBig(u16, port),
                .flowinfo = 0,
                .addr = address[0..16].*,
                .scope_id = 0,
            };
            addr = @bitCast(addr6);
            addr_len = @sizeOf(posix.sockaddr.in6);
        } else {
            return SocketError.InvalidArgument;
        }

        posix.connect(self.fd, &addr, addr_len) catch |err| {
            return mapError(err);
        };
    }

    /// Receive data
    pub fn recv(self: *Self, buffer: []u8, flags: RecvFlags) SocketError!usize {
        var sys_flags: u32 = 0;
        if (flags.peek) sys_flags |= posix.MSG.PEEK;
        if (flags.waitall) sys_flags |= posix.MSG.WAITALL;

        const n = posix.recv(self.fd, buffer, sys_flags) catch |err| {
            return mapError(err);
        };
        return n;
    }

    /// Send data
    pub fn send(self: *Self, data: []const u8, flags: SendFlags) SocketError!usize {
        _ = flags;
        const n = posix.send(self.fd, data, 0) catch |err| {
            return mapError(err);
        };
        return n;
    }

    /// Shutdown socket
    pub fn shutdown(self: *Self, how: ShutdownHow) SocketError!void {
        const sys_how: posix.ShutdownHow = switch (how) {
            .rd => .recv,
            .wr => .send,
            .rdwr => .both,
        };
        posix.shutdown(self.fd, sys_how) catch |err| {
            return mapError(err);
        };
    }

    /// Close socket
    pub fn close(self: *Self) void {
        posix.close(self.fd);
    }

    /// Set socket to non-blocking mode
    pub fn setNonBlocking(self: *Self, non_blocking: bool) SocketError!void {
        const flags = posix.fcntl(self.fd, posix.F.GETFL, 0) catch |err| {
            return mapError(err);
        };

        const new_flags = if (non_blocking)
            flags | posix.O.NONBLOCK
        else
            flags & ~@as(u32, posix.O.NONBLOCK);

        _ = posix.fcntl(self.fd, posix.F.SETFL, new_flags) catch |err| {
            return mapError(err);
        };
    }
};

/// DNS resolution
pub fn getAddrInfo(allocator: std.mem.Allocator, host: []const u8, port: u16) ![]net.Address {
    var list = std.ArrayList(net.Address).init(allocator);
    errdefer list.deinit();

    const addresses = try net.getAddressList(allocator, host, port);
    defer addresses.deinit();

    for (addresses.addrs) |addr| {
        try list.append(addr);
    }

    return list.toOwnedSlice();
}

/// Map Zig errors to SocketError
fn mapError(err: anyerror) SocketError {
    return switch (err) {
        error.AddressInUse => SocketError.AddressInUse,
        error.AddressNotAvailable => SocketError.AddressNotAvailable,
        error.ConnectionRefused => SocketError.ConnectionRefused,
        error.ConnectionResetByPeer => SocketError.ConnectionReset,
        error.ConnectionAborted => SocketError.ConnectionAborted,
        error.NetworkUnreachable => SocketError.NetworkUnreachable,
        error.HostUnreachable => SocketError.HostUnreachable,
        error.ConnectionTimedOut => SocketError.TimedOut,
        error.WouldBlock => SocketError.WouldBlock,
        error.NotConnected => SocketError.NotConnected,
        error.IsConnected => SocketError.AlreadyConnected,
        error.AccessDenied => SocketError.PermissionDenied,
        error.NotOpenForReading, error.NotOpenForWriting => SocketError.BadFileDescriptor,
        error.OutOfMemory, error.SystemResources => SocketError.OutOfMemory,
        else => SocketError.Unknown,
    };
}

// Tests
test "socket open and close" {
    var sock = try Socket.open(.inet4, .stream);
    sock.close();
}

test "socket connect" {
    var sock = try Socket.open(.inet4, .stream);
    defer sock.close();

    // Try to connect to localhost:80 (will likely fail, but tests the code path)
    const addr = [4]u8{ 127, 0, 0, 1 };
    sock.connect(&addr, 80) catch |err| {
        // Connection refused is expected if nothing is listening
        if (err != SocketError.ConnectionRefused) return err;
    };
}
