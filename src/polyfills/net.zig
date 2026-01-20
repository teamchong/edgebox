/// Native net module - QuickJS C functions for TCP sockets
/// Provides low-level socket operations for the net.js polyfill
const std = @import("std");
const builtin = @import("builtin");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// Socket states matching net.js
const SOCKET_STATE = struct {
    const CREATED: i32 = 0;
    const BOUND: i32 = 1;
    const LISTENING: i32 = 2;
    const CONNECTED: i32 = 3;
    const CLOSED: i32 = 4;
};

// Platform-specific socket imports
const c = @cImport({
    @cInclude("sys/socket.h");
    @cInclude("netinet/in.h");
    @cInclude("netinet/tcp.h");
    @cInclude("arpa/inet.h");
    @cInclude("unistd.h");
    @cInclude("fcntl.h");
    @cInclude("poll.h");
    @cInclude("errno.h");
    @cInclude("netdb.h"); // For getaddrinfo/DNS resolution
    @cInclude("sys/un.h"); // For Unix domain sockets
});

// Socket tracking (simple fixed-size array for sandbox environment)
const MAX_SOCKETS = 256;
var sockets: [MAX_SOCKETS]SocketEntry = [_]SocketEntry{.{}} ** MAX_SOCKETS;

// Socket types for different connection modes
const SOCKET_TYPE = struct {
    const TCP_IPV4: i32 = 0;
    const TCP_IPV6: i32 = 1;
    const UNIX: i32 = 2;
    const UDP_IPV4: i32 = 3;
    const UDP_IPV6: i32 = 4;
};

const SocketEntry = struct {
    fd: i32 = -1,
    state: i32 = SOCKET_STATE.CLOSED,
    non_blocking: bool = false,
    socket_type: i32 = SOCKET_TYPE.TCP_IPV4,
    pending_write_bytes: usize = 0, // Track write buffer size for backpressure
    timeout_ms: u32 = 0, // Socket timeout in milliseconds (0 = no timeout)
    last_activity: i64 = 0, // Timestamp of last activity (milliseconds since epoch)
};

fn allocateSocket() ?usize {
    for (&sockets, 0..) |*entry, i| {
        if (entry.fd == -1) {
            return i;
        }
    }
    return null;
}

fn getSocket(id: i32) ?*SocketEntry {
    if (id < 0 or id >= MAX_SOCKETS) return null;
    const idx: usize = @intCast(id);
    if (sockets[idx].fd == -1) return null;
    return &sockets[idx];
}

/// __edgebox_socket_create() - Create a new TCP socket
/// Returns: socket ID (>=0) or error code (<0)
fn socketCreate(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, -1); // Not supported in WASI
    }

    const idx = allocateSocket() orelse {
        return qjs.JS_NewInt32(ctx, -1); // No slots available
    };

    const fd = c.socket(c.AF_INET, c.SOCK_STREAM, 0);
    if (fd < 0) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    // Enable SO_REUSEADDR
    var optval: c_int = 1;
    _ = c.setsockopt(fd, c.SOL_SOCKET, c.SO_REUSEADDR, &optval, @sizeOf(c_int));

    sockets[idx] = .{
        .fd = fd,
        .state = SOCKET_STATE.CREATED,
        .non_blocking = false,
    };

    return qjs.JS_NewInt32(ctx, @intCast(idx));
}

/// __edgebox_socket_create_unix() - Create a new Unix domain socket
/// Returns: socket ID (>=0) or error code (<0)
fn socketCreateUnix(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, -1); // Not supported in WASI
    }

    const idx = allocateSocket() orelse {
        return qjs.JS_NewInt32(ctx, -1); // No slots available
    };

    const fd = c.socket(c.AF_UNIX, c.SOCK_STREAM, 0);
    if (fd < 0) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    sockets[idx] = .{
        .fd = fd,
        .state = SOCKET_STATE.CREATED,
        .non_blocking = false,
        .socket_type = SOCKET_TYPE.UNIX,
    };

    return qjs.JS_NewInt32(ctx, @intCast(idx));
}

/// __edgebox_socket_connect_unix(socketId, path) - Connect to Unix domain socket
/// Returns: 0 on success, <0 on error
fn socketConnectUnix(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    // Get path
    const path_str = qjs.JS_ToCString(ctx, argv[1]);
    if (path_str == null) {
        return qjs.JS_NewInt32(ctx, -1);
    }
    defer qjs.JS_FreeCString(ctx, path_str);
    const path = std.mem.span(path_str);

    // Setup Unix socket address
    var addr: c.sockaddr_un = std.mem.zeroes(c.sockaddr_un);
    addr.sun_family = c.AF_UNIX;

    // Copy path to sun_path (ensure null-termination and bounds)
    const max_path_len = @sizeOf(@TypeOf(addr.sun_path)) - 1;
    const copy_len = @min(path.len, max_path_len);
    @memcpy(addr.sun_path[0..copy_len], path[0..copy_len]);
    addr.sun_path[copy_len] = 0;

    // Connect
    const connect_result = c.connect(entry.fd, @ptrCast(&addr), @sizeOf(c.sockaddr_un));
    if (connect_result < 0) {
        return qjs.JS_NewInt32(ctx, -3); // Connect failed
    }

    entry.state = SOCKET_STATE.CONNECTED;
    entry.socket_type = SOCKET_TYPE.UNIX;
    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_socket_bind_unix(socketId, path) - Bind Unix socket to path
/// Returns: 0 on success, <0 on error
fn socketBindUnix(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    // Get path
    const path_str = qjs.JS_ToCString(ctx, argv[1]);
    if (path_str == null) {
        return qjs.JS_NewInt32(ctx, -1);
    }
    defer qjs.JS_FreeCString(ctx, path_str);
    const path = std.mem.span(path_str);

    // Remove existing socket file if it exists (common practice for Unix sockets)
    var path_z: [108]u8 = undefined;
    const path_copy_len = @min(path.len, path_z.len - 1);
    @memcpy(path_z[0..path_copy_len], path[0..path_copy_len]);
    path_z[path_copy_len] = 0;
    _ = c.unlink(&path_z);

    // Setup Unix socket address
    var addr: c.sockaddr_un = std.mem.zeroes(c.sockaddr_un);
    addr.sun_family = c.AF_UNIX;

    const max_path_len = @sizeOf(@TypeOf(addr.sun_path)) - 1;
    const copy_len = @min(path.len, max_path_len);
    @memcpy(addr.sun_path[0..copy_len], path[0..copy_len]);
    addr.sun_path[copy_len] = 0;

    const result = c.bind(entry.fd, @ptrCast(&addr), @sizeOf(c.sockaddr_un));
    if (result < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }

    entry.state = SOCKET_STATE.BOUND;
    entry.socket_type = SOCKET_TYPE.UNIX;
    return qjs.JS_NewInt32(ctx, 0);
}

/// Helper function to check if a string looks like an IPv6 address
fn isIPv6Address(host: []const u8) bool {
    // IPv6 addresses contain colons
    for (host) |ch| {
        if (ch == ':') return true;
    }
    return false;
}

/// Helper function to check if a string is a raw IP address (no DNS needed)
fn isRawIPAddress(host: []const u8) bool {
    // Try IPv4 first
    var addr4: c.in_addr = undefined;
    var host_buf: [256]u8 = undefined;
    const host_z = std.fmt.bufPrintZ(&host_buf, "{s}", .{host}) catch return false;
    if (c.inet_pton(c.AF_INET, host_z.ptr, &addr4) == 1) return true;

    // Try IPv6
    var addr6: c.in6_addr = undefined;
    if (c.inet_pton(c.AF_INET6, host_z.ptr, &addr6) == 1) return true;

    return false;
}

/// __edgebox_socket_connect(socketId, port, [host]) - Connect to server with DNS resolution
/// Returns: 0 on success, <0 on error
/// Error codes: -1 = invalid args, -2 = DNS resolution failed, -3 = connect failed, -4 = socket error
fn socketConnect(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    var port: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &port, argv[1]);

    // Get host if provided, default to localhost
    var host_buf: [256]u8 = undefined;
    var host: []const u8 = "127.0.0.1";
    if (argc >= 3 and qjs.JS_IsString(argv[2])) {
        const host_str = qjs.JS_ToCString(ctx, argv[2]);
        if (host_str != null) {
            host = std.mem.span(host_str);
            // Copy host to buffer since we need it after freeing
            const copy_len = @min(host.len, host_buf.len - 1);
            @memcpy(host_buf[0..copy_len], host[0..copy_len]);
            host_buf[copy_len] = 0;
            host = host_buf[0..copy_len];
            qjs.JS_FreeCString(ctx, host_str);
        }
    }

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    // First, try to parse as raw IP address (fast path)
    var addr4: c.sockaddr_in = std.mem.zeroes(c.sockaddr_in);
    var addr6: c.sockaddr_in6 = std.mem.zeroes(c.sockaddr_in6);
    var use_ipv6 = false;

    // Create null-terminated string for C functions
    var host_z: [257]u8 = undefined;
    _ = std.fmt.bufPrintZ(&host_z, "{s}", .{host}) catch {
        return qjs.JS_NewInt32(ctx, -1);
    };

    // Try IPv4 first
    if (c.inet_pton(c.AF_INET, &host_z, &addr4.sin_addr) == 1) {
        addr4.sin_family = c.AF_INET;
        addr4.sin_port = c.htons(@as(u16, @intCast(port)));
        use_ipv6 = false;
    } else if (c.inet_pton(c.AF_INET6, &host_z, &addr6.sin6_addr) == 1) {
        // Try IPv6
        addr6.sin6_family = c.AF_INET6;
        addr6.sin6_port = c.htons(@as(u16, @intCast(port)));
        use_ipv6 = true;
    } else {
        // Not a raw IP address, need DNS resolution
        var hints: c.addrinfo = std.mem.zeroes(c.addrinfo);
        hints.ai_family = c.AF_UNSPEC; // Allow both IPv4 and IPv6
        hints.ai_socktype = c.SOCK_STREAM;

        var result: ?*c.addrinfo = null;
        const status = c.getaddrinfo(&host_z, null, &hints, &result);
        if (status != 0 or result == null) {
            return qjs.JS_NewInt32(ctx, -2); // DNS resolution failed
        }
        defer c.freeaddrinfo(result);

        // Use the first resolved address
        if (result.?.ai_family == c.AF_INET) {
            const sin: *c.sockaddr_in = @ptrCast(@alignCast(result.?.ai_addr));
            addr4.sin_family = c.AF_INET;
            addr4.sin_port = c.htons(@as(u16, @intCast(port)));
            addr4.sin_addr = sin.sin_addr;
            use_ipv6 = false;
        } else if (result.?.ai_family == c.AF_INET6) {
            const sin6: *c.sockaddr_in6 = @ptrCast(@alignCast(result.?.ai_addr));
            addr6.sin6_family = c.AF_INET6;
            addr6.sin6_port = c.htons(@as(u16, @intCast(port)));
            addr6.sin6_addr = sin6.sin6_addr;
            use_ipv6 = true;
        } else {
            return qjs.JS_NewInt32(ctx, -2); // Unknown address family
        }
    }

    // If socket was created for wrong address family, we need to recreate it
    const expected_type = if (use_ipv6) SOCKET_TYPE.TCP_IPV6 else SOCKET_TYPE.TCP_IPV4;
    if (entry.socket_type != expected_type) {
        // Close old socket and create new one with correct family
        _ = c.close(entry.fd);
        const new_fd = c.socket(if (use_ipv6) c.AF_INET6 else c.AF_INET, c.SOCK_STREAM, 0);
        if (new_fd < 0) {
            return qjs.JS_NewInt32(ctx, -4);
        }
        entry.fd = new_fd;
        entry.socket_type = expected_type;

        // Enable SO_REUSEADDR
        var optval: c_int = 1;
        _ = c.setsockopt(new_fd, c.SOL_SOCKET, c.SO_REUSEADDR, &optval, @sizeOf(c_int));
    }

    // Connect
    const connect_result = if (use_ipv6)
        c.connect(entry.fd, @ptrCast(&addr6), @sizeOf(c.sockaddr_in6))
    else
        c.connect(entry.fd, @ptrCast(&addr4), @sizeOf(c.sockaddr_in));

    if (connect_result < 0) {
        return qjs.JS_NewInt32(ctx, -3); // Connect failed
    }

    entry.state = SOCKET_STATE.CONNECTED;
    entry.socket_type = expected_type;
    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_socket_bind(socketId, port) - Bind socket to port
/// Returns: 0 on success, <0 on error
fn socketBind(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    var port: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &port, argv[1]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    var addr: c.sockaddr_in = std.mem.zeroes(c.sockaddr_in);
    addr.sin_family = c.AF_INET;
    addr.sin_port = c.htons(@as(u16, @intCast(port)));
    addr.sin_addr.s_addr = c.INADDR_ANY;

    const result = c.bind(entry.fd, @ptrCast(&addr), @sizeOf(c.sockaddr_in));
    if (result < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }

    entry.state = SOCKET_STATE.BOUND;
    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_socket_listen(socketId, backlog) - Start listening
/// Returns: 0 on success, <0 on error
fn socketListen(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    var backlog: i32 = 511;
    _ = qjs.JS_ToInt32(ctx, &backlog, argv[1]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    const result = c.listen(entry.fd, backlog);
    if (result < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }

    entry.state = SOCKET_STATE.LISTENING;
    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_socket_accept(socketId) - Accept incoming connection
/// Returns: new socket ID (>=0) or error code (<0)
fn socketAccept(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    if (argc < 1) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    // Set non-blocking for poll
    if (!entry.non_blocking) {
        _ = c.fcntl(entry.fd, c.F_SETFL, c.O_NONBLOCK);
        entry.non_blocking = true;
    }

    // Poll for incoming connection (non-blocking)
    var pfd = c.pollfd{
        .fd = entry.fd,
        .events = c.POLLIN,
        .revents = 0,
    };

    const poll_result = c.poll(&pfd, 1, 0); // 0 timeout = immediate return
    if (poll_result <= 0 or (pfd.revents & c.POLLIN) == 0) {
        return qjs.JS_NewInt32(ctx, -2); // No connection pending
    }

    var client_addr: c.sockaddr_in = std.mem.zeroes(c.sockaddr_in);
    var addr_len: c.socklen_t = @sizeOf(c.sockaddr_in);

    const client_fd = c.accept(entry.fd, @ptrCast(&client_addr), &addr_len);
    if (client_fd < 0) {
        return qjs.JS_NewInt32(ctx, -3);
    }

    // Allocate slot for new socket
    const client_idx = allocateSocket() orelse {
        _ = c.close(client_fd);
        return qjs.JS_NewInt32(ctx, -4);
    };

    sockets[client_idx] = .{
        .fd = client_fd,
        .state = SOCKET_STATE.CONNECTED,
        .non_blocking = false,
    };

    return qjs.JS_NewInt32(ctx, @intCast(client_idx));
}

/// __edgebox_socket_read(socketId, maxBytes) - Read from socket
/// Returns: string data, null on EOF, or undefined on EAGAIN
fn socketRead(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return quickjs.jsNull();
    }

    if (argc < 1) {
        return quickjs.jsNull();
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    var max_bytes: i32 = 65536;
    if (argc >= 2) {
        _ = qjs.JS_ToInt32(ctx, &max_bytes, argv[1]);
    }

    const entry = getSocket(socket_id) orelse {
        return quickjs.jsNull();
    };

    // Set non-blocking for poll
    if (!entry.non_blocking) {
        _ = c.fcntl(entry.fd, c.F_SETFL, c.O_NONBLOCK);
        entry.non_blocking = true;
    }

    // Poll for data (non-blocking)
    var pfd = c.pollfd{
        .fd = entry.fd,
        .events = c.POLLIN,
        .revents = 0,
    };

    const poll_result = c.poll(&pfd, 1, 0);
    if (poll_result <= 0 or (pfd.revents & c.POLLIN) == 0) {
        return quickjs.jsUndefined(); // No data available
    }

    // Read data
    var buf: [65536]u8 = undefined;
    const read_len: usize = @min(@as(usize, @intCast(max_bytes)), buf.len);

    const result = c.read(entry.fd, &buf, read_len);
    if (result == 0) {
        return quickjs.jsNull(); // EOF
    }
    if (result < 0) {
        return quickjs.jsUndefined(); // Error or EAGAIN
    }

    return qjs.JS_NewStringLen(ctx, &buf, @intCast(result));
}

/// __edgebox_socket_write(socketId, data) - Write to socket
/// Returns: bytes written (>=0) or error code (<0)
/// Also updates pending_write_bytes for backpressure tracking
fn socketWrite(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    const data_str = qjs.JS_ToCString(ctx, argv[1]);
    if (data_str == null) {
        return qjs.JS_NewInt32(ctx, -2);
    }
    defer qjs.JS_FreeCString(ctx, data_str);

    const data_slice = std.mem.span(data_str);

    // Set non-blocking for non-blocking write
    if (!entry.non_blocking) {
        _ = c.fcntl(entry.fd, c.F_SETFL, c.O_NONBLOCK);
        entry.non_blocking = true;
    }

    const result = c.write(entry.fd, data_slice.ptr, data_slice.len);
    if (result < 0) {
        const err = std.c._errno().*;
        if (err == c.EAGAIN or err == c.EWOULDBLOCK) {
            // Socket buffer is full, track as pending
            entry.pending_write_bytes += data_slice.len;
            return qjs.JS_NewInt32(ctx, 0); // Return 0 bytes written (would block)
        }
        return qjs.JS_NewInt32(ctx, -3);
    }

    const bytes_written: usize = @intCast(result);

    // Track pending bytes for backpressure
    if (bytes_written < data_slice.len) {
        entry.pending_write_bytes += data_slice.len - bytes_written;
    } else if (entry.pending_write_bytes > 0) {
        // Some data was flushed, decrease pending count
        entry.pending_write_bytes = 0;
    }

    return qjs.JS_NewInt32(ctx, @intCast(result));
}

/// __edgebox_socket_pending_bytes(socketId) - Get pending write bytes for backpressure
/// Returns: number of pending bytes in write buffer
fn socketPendingBytes(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_NewInt32(ctx, 0);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, 0);
    };

    return qjs.JS_NewInt32(ctx, @intCast(entry.pending_write_bytes));
}

/// __edgebox_socket_close(socketId) - Close socket
/// Returns: 0 on success, <0 on error
fn socketClose(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 0);
    }

    if (argc < 1) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    if (socket_id < 0 or socket_id >= MAX_SOCKETS) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    const idx: usize = @intCast(socket_id);
    if (sockets[idx].fd != -1) {
        _ = c.close(sockets[idx].fd);
        sockets[idx] = .{};
    }

    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_socket_state(socketId) - Get socket state
/// Returns: state code (0-4)
fn socketState(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_NewInt32(ctx, SOCKET_STATE.CLOSED);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, SOCKET_STATE.CLOSED);
    };

    return qjs.JS_NewInt32(ctx, entry.state);
}

/// __edgebox_socket_set_nodelay(socketId, enable) - Enable/disable TCP_NODELAY (Nagle's algorithm)
/// Returns: 0 on success, <0 on error
fn socketSetNoDelay(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 0); // No-op in WASI
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    const enable = qjs.JS_ToBool(ctx, argv[1]);
    var optval: c_int = if (enable != 0) 1 else 0;

    const result = c.setsockopt(entry.fd, c.IPPROTO_TCP, c.TCP_NODELAY, &optval, @sizeOf(c_int));
    if (result < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }

    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_socket_set_keepalive(socketId, enable, delay) - Enable/disable SO_KEEPALIVE
/// delay is in milliseconds
/// Returns: 0 on success, <0 on error
fn socketSetKeepAlive(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 0); // No-op in WASI
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    const enable = qjs.JS_ToBool(ctx, argv[1]);
    var optval: c_int = if (enable != 0) 1 else 0;

    // Set SO_KEEPALIVE
    var result = c.setsockopt(entry.fd, c.SOL_SOCKET, c.SO_KEEPALIVE, &optval, @sizeOf(c_int));
    if (result < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }

    // If enabled and delay provided, set TCP_KEEPIDLE (seconds until first keepalive probe)
    if (enable != 0 and argc >= 3) {
        var delay_ms: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &delay_ms, argv[2]);
        if (delay_ms > 0) {
            // Convert ms to seconds, minimum 1 second
            var delay_sec: c_int = @intCast(@max(1, @divTrunc(delay_ms, 1000)));
            // TCP_KEEPIDLE on Linux, TCP_KEEPALIVE on macOS
            const keepidle_opt = if (builtin.os.tag == .macos) c.TCP_KEEPALIVE else c.TCP_KEEPIDLE;
            result = c.setsockopt(entry.fd, c.IPPROTO_TCP, keepidle_opt, &delay_sec, @sizeOf(c_int));
            // Ignore error - not all systems support this
        }
    }

    return qjs.JS_NewInt32(ctx, 0);
}

/// Get current time in milliseconds (for timeout tracking)
fn getTimeMs() i64 {
    const ts = std.time.milliTimestamp();
    return ts;
}

/// __edgebox_socket_set_timeout(socketId, timeoutMs) - Set socket read timeout
/// Returns: 0 on success, <0 on error
fn socketSetTimeout(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    var timeout_ms: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &timeout_ms, argv[1]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    entry.timeout_ms = if (timeout_ms > 0) @intCast(timeout_ms) else 0;
    entry.last_activity = getTimeMs();

    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_socket_read_with_timeout(socketId, maxBytes) - Read from socket with timeout check
/// Returns: string data, null on EOF, undefined on EAGAIN, or -2 on timeout
fn socketReadWithTimeout(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return quickjs.jsNull();
    }

    if (argc < 1) {
        return quickjs.jsNull();
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    var max_bytes: i32 = 65536;
    if (argc >= 2) {
        _ = qjs.JS_ToInt32(ctx, &max_bytes, argv[1]);
    }

    const entry = getSocket(socket_id) orelse {
        return quickjs.jsNull();
    };

    // Set non-blocking for poll
    if (!entry.non_blocking) {
        _ = c.fcntl(entry.fd, c.F_SETFL, c.O_NONBLOCK);
        entry.non_blocking = true;
    }

    // Check for timeout if configured
    if (entry.timeout_ms > 0) {
        const current_time = getTimeMs();
        const elapsed = current_time - entry.last_activity;
        if (elapsed >= entry.timeout_ms) {
            // Timeout occurred - return special value
            return qjs.JS_NewInt32(ctx, -2); // Timeout indicator
        }
    }

    // Poll for data with timeout consideration
    var poll_timeout: c_int = 0; // Default: immediate (non-blocking)
    if (entry.timeout_ms > 0) {
        const remaining = @as(i64, entry.timeout_ms) - (getTimeMs() - entry.last_activity);
        if (remaining > 0) {
            poll_timeout = @min(@as(c_int, @intCast(remaining)), 100); // Max 100ms poll to allow JS event loop
        }
    }

    var pfd = c.pollfd{
        .fd = entry.fd,
        .events = c.POLLIN,
        .revents = 0,
    };

    const poll_result = c.poll(&pfd, 1, poll_timeout);
    if (poll_result == 0) {
        // No data available
        if (entry.timeout_ms > 0) {
            const current_time = getTimeMs();
            const elapsed = current_time - entry.last_activity;
            if (elapsed >= entry.timeout_ms) {
                return qjs.JS_NewInt32(ctx, -2); // Timeout
            }
        }
        return quickjs.jsUndefined(); // No data yet
    }
    if (poll_result < 0 or (pfd.revents & c.POLLIN) == 0) {
        return quickjs.jsUndefined(); // No data available
    }

    // Read data
    var buf: [65536]u8 = undefined;
    const read_len: usize = @min(@as(usize, @intCast(max_bytes)), buf.len);

    const result = c.read(entry.fd, &buf, read_len);
    if (result == 0) {
        return quickjs.jsNull(); // EOF
    }
    if (result < 0) {
        return quickjs.jsUndefined(); // Error or EAGAIN
    }

    // Update last activity timestamp
    entry.last_activity = getTimeMs();

    return qjs.JS_NewStringLen(ctx, &buf, @intCast(result));
}

/// __edgebox_socket_poll_writable(socketId, timeoutMs) - Check if socket is writable
/// Returns: 1 if writable, 0 if not yet, -1 on error
fn socketPollWritable(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 1); // Assume writable in WASI
    }

    if (argc < 1) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    var timeout_ms: i32 = 0;
    if (argc >= 2) {
        _ = qjs.JS_ToInt32(ctx, &timeout_ms, argv[1]);
    }

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    var pfd = c.pollfd{
        .fd = entry.fd,
        .events = c.POLLOUT,
        .revents = 0,
    };

    const poll_result = c.poll(&pfd, 1, timeout_ms);
    if (poll_result < 0) {
        return qjs.JS_NewInt32(ctx, -1); // Error
    }
    if (poll_result == 0 or (pfd.revents & c.POLLOUT) == 0) {
        return qjs.JS_NewInt32(ctx, 0); // Not writable yet
    }

    // Socket is writable, clear pending bytes
    entry.pending_write_bytes = 0;
    return qjs.JS_NewInt32(ctx, 1); // Writable
}

// ============ UDP Socket Functions ============

/// __edgebox_udp_socket_create(type) - Create a new UDP socket
/// type: 'udp4' or 'udp6'
/// Returns: socket ID (>=0) or error code (<0)
fn udpSocketCreate(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, -1); // Not supported in WASI
    }

    const idx = allocateSocket() orelse {
        return qjs.JS_NewInt32(ctx, -1); // No slots available
    };

    // Check if IPv6 requested
    var use_ipv6 = false;
    if (argc >= 1 and qjs.JS_IsString(argv[0])) {
        const type_str = qjs.JS_ToCString(ctx, argv[0]);
        if (type_str != null) {
            const type_slice = std.mem.span(type_str);
            use_ipv6 = std.mem.eql(u8, type_slice, "udp6");
            qjs.JS_FreeCString(ctx, type_str);
        }
    }

    const family = if (use_ipv6) c.AF_INET6 else c.AF_INET;
    const fd = c.socket(family, c.SOCK_DGRAM, 0);
    if (fd < 0) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    // Enable SO_REUSEADDR
    var optval: c_int = 1;
    _ = c.setsockopt(fd, c.SOL_SOCKET, c.SO_REUSEADDR, &optval, @sizeOf(c_int));

    sockets[idx] = .{
        .fd = fd,
        .state = SOCKET_STATE.CREATED,
        .non_blocking = false,
        .socket_type = if (use_ipv6) SOCKET_TYPE.UDP_IPV6 else SOCKET_TYPE.UDP_IPV4,
    };

    return qjs.JS_NewInt32(ctx, @intCast(idx));
}

/// __edgebox_udp_socket_bind(socketId, port, address) - Bind UDP socket to port
/// Returns: 0 on success, <0 on error
fn udpSocketBind(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    var port: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &port, argv[1]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    // Get address if provided
    var address_buf: [256]u8 = undefined;
    var address: []const u8 = "0.0.0.0";
    if (argc >= 3 and qjs.JS_IsString(argv[2])) {
        const addr_str = qjs.JS_ToCString(ctx, argv[2]);
        if (addr_str != null) {
            const addr_slice = std.mem.span(addr_str);
            const copy_len = @min(addr_slice.len, address_buf.len - 1);
            @memcpy(address_buf[0..copy_len], addr_slice[0..copy_len]);
            address_buf[copy_len] = 0;
            address = address_buf[0..copy_len];
            qjs.JS_FreeCString(ctx, addr_str);
        }
    }

    const is_ipv6 = entry.socket_type == SOCKET_TYPE.UDP_IPV6;

    if (is_ipv6) {
        var addr6: c.sockaddr_in6 = std.mem.zeroes(c.sockaddr_in6);
        addr6.sin6_family = c.AF_INET6;
        addr6.sin6_port = c.htons(@as(u16, @intCast(port)));
        if (std.mem.eql(u8, address, "0.0.0.0") or std.mem.eql(u8, address, "::")) {
            addr6.sin6_addr = std.mem.zeroes(c.in6_addr); // IN6ADDR_ANY
        } else {
            var addr_z: [257]u8 = undefined;
            _ = std.fmt.bufPrintZ(&addr_z, "{s}", .{address}) catch {
                return qjs.JS_NewInt32(ctx, -2);
            };
            if (c.inet_pton(c.AF_INET6, &addr_z, &addr6.sin6_addr) != 1) {
                return qjs.JS_NewInt32(ctx, -2);
            }
        }
        const result = c.bind(entry.fd, @ptrCast(&addr6), @sizeOf(c.sockaddr_in6));
        if (result < 0) {
            return qjs.JS_NewInt32(ctx, -3);
        }
    } else {
        var addr4: c.sockaddr_in = std.mem.zeroes(c.sockaddr_in);
        addr4.sin_family = c.AF_INET;
        addr4.sin_port = c.htons(@as(u16, @intCast(port)));
        if (std.mem.eql(u8, address, "0.0.0.0")) {
            addr4.sin_addr.s_addr = c.INADDR_ANY;
        } else {
            var addr_z: [257]u8 = undefined;
            _ = std.fmt.bufPrintZ(&addr_z, "{s}", .{address}) catch {
                return qjs.JS_NewInt32(ctx, -2);
            };
            if (c.inet_pton(c.AF_INET, &addr_z, &addr4.sin_addr) != 1) {
                return qjs.JS_NewInt32(ctx, -2);
            }
        }
        const result = c.bind(entry.fd, @ptrCast(&addr4), @sizeOf(c.sockaddr_in));
        if (result < 0) {
            return qjs.JS_NewInt32(ctx, -3);
        }
    }

    entry.state = SOCKET_STATE.BOUND;
    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_udp_socket_send(socketId, data, port, address) - Send UDP datagram
/// Returns: bytes sent (>=0) or error code (<0)
fn udpSocketSend(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    if (argc < 4) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    // Get data (can be string or ArrayBuffer)
    var data_ptr: [*]const u8 = undefined;
    var data_len: usize = 0;

    if (qjs.JS_IsString(argv[1])) {
        const data_str = qjs.JS_ToCString(ctx, argv[1]);
        if (data_str == null) {
            return qjs.JS_NewInt32(ctx, -2);
        }
        data_ptr = @ptrCast(data_str);
        data_len = std.mem.len(data_str);
        defer qjs.JS_FreeCString(ctx, data_str);

        var port: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &port, argv[2]);

        // Get address
        const addr_str = qjs.JS_ToCString(ctx, argv[3]);
        if (addr_str == null) {
            return qjs.JS_NewInt32(ctx, -3);
        }
        defer qjs.JS_FreeCString(ctx, addr_str);

        const is_ipv6 = entry.socket_type == SOCKET_TYPE.UDP_IPV6;

        var result: isize = 0;
        if (is_ipv6) {
            var addr6: c.sockaddr_in6 = std.mem.zeroes(c.sockaddr_in6);
            addr6.sin6_family = c.AF_INET6;
            addr6.sin6_port = c.htons(@as(u16, @intCast(port)));
            if (c.inet_pton(c.AF_INET6, addr_str, &addr6.sin6_addr) != 1) {
                return qjs.JS_NewInt32(ctx, -4);
            }
            result = c.sendto(entry.fd, data_ptr, data_len, 0, @ptrCast(&addr6), @sizeOf(c.sockaddr_in6));
        } else {
            var addr4: c.sockaddr_in = std.mem.zeroes(c.sockaddr_in);
            addr4.sin_family = c.AF_INET;
            addr4.sin_port = c.htons(@as(u16, @intCast(port)));
            if (c.inet_pton(c.AF_INET, addr_str, &addr4.sin_addr) != 1) {
                return qjs.JS_NewInt32(ctx, -4);
            }
            result = c.sendto(entry.fd, data_ptr, data_len, 0, @ptrCast(&addr4), @sizeOf(c.sockaddr_in));
        }

        if (result < 0) {
            return qjs.JS_NewInt32(ctx, -5);
        }
        return qjs.JS_NewInt32(ctx, @intCast(result));
    } else {
        // Assume ArrayBuffer
        var size: usize = 0;
        const buf_ptr = qjs.JS_GetArrayBuffer(ctx, &size, argv[1]);
        if (buf_ptr == null) {
            return qjs.JS_NewInt32(ctx, -2);
        }
        data_ptr = buf_ptr;
        data_len = size;

        var port: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &port, argv[2]);

        const addr_str = qjs.JS_ToCString(ctx, argv[3]);
        if (addr_str == null) {
            return qjs.JS_NewInt32(ctx, -3);
        }
        defer qjs.JS_FreeCString(ctx, addr_str);

        const is_ipv6 = entry.socket_type == SOCKET_TYPE.UDP_IPV6;

        var result: isize = 0;
        if (is_ipv6) {
            var addr6: c.sockaddr_in6 = std.mem.zeroes(c.sockaddr_in6);
            addr6.sin6_family = c.AF_INET6;
            addr6.sin6_port = c.htons(@as(u16, @intCast(port)));
            if (c.inet_pton(c.AF_INET6, addr_str, &addr6.sin6_addr) != 1) {
                return qjs.JS_NewInt32(ctx, -4);
            }
            result = c.sendto(entry.fd, data_ptr, data_len, 0, @ptrCast(&addr6), @sizeOf(c.sockaddr_in6));
        } else {
            var addr4: c.sockaddr_in = std.mem.zeroes(c.sockaddr_in);
            addr4.sin_family = c.AF_INET;
            addr4.sin_port = c.htons(@as(u16, @intCast(port)));
            if (c.inet_pton(c.AF_INET, addr_str, &addr4.sin_addr) != 1) {
                return qjs.JS_NewInt32(ctx, -4);
            }
            result = c.sendto(entry.fd, data_ptr, data_len, 0, @ptrCast(&addr4), @sizeOf(c.sockaddr_in));
        }

        if (result < 0) {
            return qjs.JS_NewInt32(ctx, -5);
        }
        return qjs.JS_NewInt32(ctx, @intCast(result));
    }
}

/// __edgebox_udp_socket_recv(socketId, maxSize) - Receive UDP datagram
/// Returns: { data: string, address: string, port: number, family: string } or null
fn udpSocketRecv(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return quickjs.jsNull();
    }

    if (argc < 1) {
        return quickjs.jsNull();
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    var max_size: i32 = 65536;
    if (argc >= 2) {
        _ = qjs.JS_ToInt32(ctx, &max_size, argv[1]);
    }

    const entry = getSocket(socket_id) orelse {
        return quickjs.jsNull();
    };

    // Set non-blocking
    if (!entry.non_blocking) {
        _ = c.fcntl(entry.fd, c.F_SETFL, c.O_NONBLOCK);
        entry.non_blocking = true;
    }

    // Poll for data
    var pfd = c.pollfd{
        .fd = entry.fd,
        .events = c.POLLIN,
        .revents = 0,
    };

    const poll_result = c.poll(&pfd, 1, 0);
    if (poll_result <= 0 or (pfd.revents & c.POLLIN) == 0) {
        return quickjs.jsUndefined(); // No data available
    }

    var buf: [65536]u8 = undefined;
    const read_len: usize = @min(@as(usize, @intCast(max_size)), buf.len);

    const is_ipv6 = entry.socket_type == SOCKET_TYPE.UDP_IPV6;

    var result: isize = 0;
    var remote_port: u16 = 0;
    var remote_addr_buf: [64]u8 = undefined;
    var remote_addr: []const u8 = "";

    if (is_ipv6) {
        var addr6: c.sockaddr_in6 = std.mem.zeroes(c.sockaddr_in6);
        var addr_len: c.socklen_t = @sizeOf(c.sockaddr_in6);
        result = c.recvfrom(entry.fd, &buf, read_len, 0, @ptrCast(&addr6), &addr_len);
        if (result > 0) {
            remote_port = c.ntohs(addr6.sin6_port);
            if (c.inet_ntop(c.AF_INET6, &addr6.sin6_addr, &remote_addr_buf, remote_addr_buf.len) != null) {
                remote_addr = std.mem.sliceTo(&remote_addr_buf, 0);
            }
        }
    } else {
        var addr4: c.sockaddr_in = std.mem.zeroes(c.sockaddr_in);
        var addr_len: c.socklen_t = @sizeOf(c.sockaddr_in);
        result = c.recvfrom(entry.fd, &buf, read_len, 0, @ptrCast(&addr4), &addr_len);
        if (result > 0) {
            remote_port = c.ntohs(addr4.sin_port);
            if (c.inet_ntop(c.AF_INET, &addr4.sin_addr, &remote_addr_buf, remote_addr_buf.len) != null) {
                remote_addr = std.mem.sliceTo(&remote_addr_buf, 0);
            }
        }
    }

    if (result <= 0) {
        return quickjs.jsUndefined();
    }

    // Create result object { data, address, port, family }
    const obj = qjs.JS_NewObject(ctx);

    // Add data
    const data_val = qjs.JS_NewStringLen(ctx, &buf, @intCast(result));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "data", data_val);

    // Add address
    const addr_val = qjs.JS_NewStringLen(ctx, remote_addr.ptr, remote_addr.len);
    _ = qjs.JS_SetPropertyStr(ctx, obj, "address", addr_val);

    // Add port
    _ = qjs.JS_SetPropertyStr(ctx, obj, "port", qjs.JS_NewInt32(ctx, @intCast(remote_port)));

    // Add family
    const family_str = if (is_ipv6) "IPv6" else "IPv4";
    _ = qjs.JS_SetPropertyStr(ctx, obj, "family", qjs.JS_NewString(ctx, family_str));

    // Add size
    _ = qjs.JS_SetPropertyStr(ctx, obj, "size", qjs.JS_NewInt32(ctx, @intCast(result)));

    return obj;
}

/// __edgebox_udp_set_broadcast(socketId, enable) - Enable/disable broadcast
/// Returns: 0 on success, <0 on error
fn udpSetBroadcast(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 0);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    const enable = qjs.JS_ToBool(ctx, argv[1]);
    var optval: c_int = if (enable != 0) 1 else 0;

    const result = c.setsockopt(entry.fd, c.SOL_SOCKET, c.SO_BROADCAST, &optval, @sizeOf(c_int));
    if (result < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }

    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_udp_set_ttl(socketId, ttl) - Set IP TTL
/// Returns: 0 on success, <0 on error
fn udpSetTTL(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 0);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    var ttl: i32 = 64;
    _ = qjs.JS_ToInt32(ctx, &ttl, argv[1]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    var optval: c_int = @intCast(ttl);
    const result = c.setsockopt(entry.fd, c.IPPROTO_IP, c.IP_TTL, &optval, @sizeOf(c_int));
    if (result < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }

    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_udp_set_multicast_ttl(socketId, ttl) - Set multicast TTL
/// Returns: 0 on success, <0 on error
fn udpSetMulticastTTL(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 0);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    var ttl: i32 = 1;
    _ = qjs.JS_ToInt32(ctx, &ttl, argv[1]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    var optval: u8 = @intCast(@min(255, @max(0, ttl)));
    const result = c.setsockopt(entry.fd, c.IPPROTO_IP, c.IP_MULTICAST_TTL, &optval, @sizeOf(u8));
    if (result < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }

    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_udp_set_multicast_loopback(socketId, enable) - Set multicast loopback
/// Returns: 0 on success, <0 on error
fn udpSetMulticastLoopback(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 0);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    const enable = qjs.JS_ToBool(ctx, argv[1]);
    var optval: u8 = if (enable != 0) 1 else 0;

    const result = c.setsockopt(entry.fd, c.IPPROTO_IP, c.IP_MULTICAST_LOOP, &optval, @sizeOf(u8));
    if (result < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }

    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_udp_add_membership(socketId, multicastAddr, interfaceAddr) - Join multicast group
/// Returns: 0 on success, <0 on error
fn udpAddMembership(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 0);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    // Get multicast address
    const mcast_str = qjs.JS_ToCString(ctx, argv[1]);
    if (mcast_str == null) {
        return qjs.JS_NewInt32(ctx, -2);
    }
    defer qjs.JS_FreeCString(ctx, mcast_str);

    // Get interface address (optional)
    var iface_addr: c.in_addr = std.mem.zeroes(c.in_addr);
    iface_addr.s_addr = c.INADDR_ANY;
    if (argc >= 3 and qjs.JS_IsString(argv[2])) {
        const iface_str = qjs.JS_ToCString(ctx, argv[2]);
        if (iface_str != null) {
            _ = c.inet_pton(c.AF_INET, iface_str, &iface_addr);
            qjs.JS_FreeCString(ctx, iface_str);
        }
    }

    // Setup multicast request
    var mreq: c.ip_mreq = std.mem.zeroes(c.ip_mreq);
    if (c.inet_pton(c.AF_INET, mcast_str, &mreq.imr_multiaddr) != 1) {
        return qjs.JS_NewInt32(ctx, -3);
    }
    mreq.imr_interface = iface_addr;

    const result = c.setsockopt(entry.fd, c.IPPROTO_IP, c.IP_ADD_MEMBERSHIP, &mreq, @sizeOf(c.ip_mreq));
    if (result < 0) {
        return qjs.JS_NewInt32(ctx, -4);
    }

    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_udp_drop_membership(socketId, multicastAddr, interfaceAddr) - Leave multicast group
/// Returns: 0 on success, <0 on error
fn udpDropMembership(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 0);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    // Get multicast address
    const mcast_str = qjs.JS_ToCString(ctx, argv[1]);
    if (mcast_str == null) {
        return qjs.JS_NewInt32(ctx, -2);
    }
    defer qjs.JS_FreeCString(ctx, mcast_str);

    // Get interface address (optional)
    var iface_addr: c.in_addr = std.mem.zeroes(c.in_addr);
    iface_addr.s_addr = c.INADDR_ANY;
    if (argc >= 3 and qjs.JS_IsString(argv[2])) {
        const iface_str = qjs.JS_ToCString(ctx, argv[2]);
        if (iface_str != null) {
            _ = c.inet_pton(c.AF_INET, iface_str, &iface_addr);
            qjs.JS_FreeCString(ctx, iface_str);
        }
    }

    // Setup multicast request
    var mreq: c.ip_mreq = std.mem.zeroes(c.ip_mreq);
    if (c.inet_pton(c.AF_INET, mcast_str, &mreq.imr_multiaddr) != 1) {
        return qjs.JS_NewInt32(ctx, -3);
    }
    mreq.imr_interface = iface_addr;

    const result = c.setsockopt(entry.fd, c.IPPROTO_IP, c.IP_DROP_MEMBERSHIP, &mreq, @sizeOf(c.ip_mreq));
    if (result < 0) {
        return qjs.JS_NewInt32(ctx, -4);
    }

    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_udp_set_multicast_interface(socketId, interfaceAddr) - Set multicast interface
/// Returns: 0 on success, <0 on error
fn udpSetMulticastInterface(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 0);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    // Get interface address
    const iface_str = qjs.JS_ToCString(ctx, argv[1]);
    if (iface_str == null) {
        return qjs.JS_NewInt32(ctx, -2);
    }
    defer qjs.JS_FreeCString(ctx, iface_str);

    var iface_addr: c.in_addr = std.mem.zeroes(c.in_addr);
    if (c.inet_pton(c.AF_INET, iface_str, &iface_addr) != 1) {
        return qjs.JS_NewInt32(ctx, -3);
    }

    const result = c.setsockopt(entry.fd, c.IPPROTO_IP, c.IP_MULTICAST_IF, &iface_addr, @sizeOf(c.in_addr));
    if (result < 0) {
        return qjs.JS_NewInt32(ctx, -4);
    }

    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_udp_socket_close(socketId) - Close UDP socket
/// Returns: 0 on success, <0 on error
fn udpSocketClose(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    // Just reuse the regular socket close
    return socketClose(ctx, quickjs.jsUndefined(), argc, argv);
}

/// __edgebox_udp_get_recv_buffer_size(socketId) - Get receive buffer size
/// Returns: buffer size or -1 on error
fn udpGetRecvBufferSize(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 65536);
    }

    if (argc < 1) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    var buf_size: c_int = 0;
    var len: c.socklen_t = @sizeOf(c_int);
    if (c.getsockopt(entry.fd, c.SOL_SOCKET, c.SO_RCVBUF, &buf_size, &len) < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }

    return qjs.JS_NewInt32(ctx, buf_size);
}

/// __edgebox_udp_get_send_buffer_size(socketId) - Get send buffer size
/// Returns: buffer size or -1 on error
fn udpGetSendBufferSize(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 65536);
    }

    if (argc < 1) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    var buf_size: c_int = 0;
    var len: c.socklen_t = @sizeOf(c_int);
    if (c.getsockopt(entry.fd, c.SOL_SOCKET, c.SO_SNDBUF, &buf_size, &len) < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }

    return qjs.JS_NewInt32(ctx, buf_size);
}

/// __edgebox_udp_set_recv_buffer_size(socketId, size) - Set receive buffer size
/// Returns: 0 on success, <0 on error
fn udpSetRecvBufferSize(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 0);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    var size: i32 = 65536;
    _ = qjs.JS_ToInt32(ctx, &size, argv[1]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    var buf_size: c_int = @intCast(size);
    if (c.setsockopt(entry.fd, c.SOL_SOCKET, c.SO_RCVBUF, &buf_size, @sizeOf(c_int)) < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }

    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_udp_set_send_buffer_size(socketId, size) - Set send buffer size
/// Returns: 0 on success, <0 on error
fn udpSetSendBufferSize(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 0);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    var size: i32 = 65536;
    _ = qjs.JS_ToInt32(ctx, &size, argv[1]);

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    var buf_size: c_int = @intCast(size);
    if (c.setsockopt(entry.fd, c.SOL_SOCKET, c.SO_SNDBUF, &buf_size, @sizeOf(c_int)) < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }

    return qjs.JS_NewInt32(ctx, 0);
}

// ============ TCP Socket Buffer/Linger Options ============

/// __edgebox_socket_get_recv_buffer_size(socketId) - Get TCP receive buffer size
fn socketGetRecvBufferSize(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 65536);
    }

    if (argc < 1) return qjs.JS_NewInt32(ctx, -1);

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse return qjs.JS_NewInt32(ctx, -1);

    var buf_size: c_int = 0;
    var len: c.socklen_t = @sizeOf(c_int);
    if (c.getsockopt(entry.fd, c.SOL_SOCKET, c.SO_RCVBUF, &buf_size, &len) < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }
    return qjs.JS_NewInt32(ctx, buf_size);
}

/// __edgebox_socket_get_send_buffer_size(socketId) - Get TCP send buffer size
fn socketGetSendBufferSize(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, 65536);
    }

    if (argc < 1) return qjs.JS_NewInt32(ctx, -1);

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse return qjs.JS_NewInt32(ctx, -1);

    var buf_size: c_int = 0;
    var len: c.socklen_t = @sizeOf(c_int);
    if (c.getsockopt(entry.fd, c.SOL_SOCKET, c.SO_SNDBUF, &buf_size, &len) < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }
    return qjs.JS_NewInt32(ctx, buf_size);
}

/// __edgebox_socket_set_recv_buffer_size(socketId, size) - Set TCP receive buffer size
fn socketSetRecvBufferSize(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) return qjs.JS_NewInt32(ctx, 0);
    if (argc < 2) return qjs.JS_NewInt32(ctx, -1);

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);
    var size: i32 = 65536;
    _ = qjs.JS_ToInt32(ctx, &size, argv[1]);

    const entry = getSocket(socket_id) orelse return qjs.JS_NewInt32(ctx, -1);

    var buf_size: c_int = @intCast(size);
    if (c.setsockopt(entry.fd, c.SOL_SOCKET, c.SO_RCVBUF, &buf_size, @sizeOf(c_int)) < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }
    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_socket_set_send_buffer_size(socketId, size) - Set TCP send buffer size
fn socketSetSendBufferSize(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) return qjs.JS_NewInt32(ctx, 0);
    if (argc < 2) return qjs.JS_NewInt32(ctx, -1);

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);
    var size: i32 = 65536;
    _ = qjs.JS_ToInt32(ctx, &size, argv[1]);

    const entry = getSocket(socket_id) orelse return qjs.JS_NewInt32(ctx, -1);

    var buf_size: c_int = @intCast(size);
    if (c.setsockopt(entry.fd, c.SOL_SOCKET, c.SO_SNDBUF, &buf_size, @sizeOf(c_int)) < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }
    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_socket_set_linger(socketId, enable, timeout) - Set SO_LINGER option
fn socketSetLinger(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) return qjs.JS_NewInt32(ctx, 0);
    if (argc < 3) return qjs.JS_NewInt32(ctx, -1);

    var socket_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &socket_id, argv[0]);

    const entry = getSocket(socket_id) orelse return qjs.JS_NewInt32(ctx, -1);

    const enable = qjs.JS_ToBool(ctx, argv[1]);
    var timeout: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &timeout, argv[2]);

    var linger = c.linger{
        .l_onoff = if (enable != 0) 1 else 0,
        .l_linger = @intCast(@max(0, timeout)),
    };

    if (c.setsockopt(entry.fd, c.SOL_SOCKET, c.SO_LINGER, &linger, @sizeOf(c.linger)) < 0) {
        return qjs.JS_NewInt32(ctx, -2);
    }
    return qjs.JS_NewInt32(ctx, 0);
}

/// Register net module native functions
pub fn register(ctx: ?*qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Register socket functions on global object (called from net.js)
    inline for (.{
        .{ "__edgebox_socket_create", socketCreate, 0 },
        .{ "__edgebox_socket_create_unix", socketCreateUnix, 0 },
        .{ "__edgebox_socket_connect", socketConnect, 3 },
        .{ "__edgebox_socket_connect_unix", socketConnectUnix, 2 },
        .{ "__edgebox_socket_bind", socketBind, 2 },
        .{ "__edgebox_socket_bind_unix", socketBindUnix, 2 },
        .{ "__edgebox_socket_listen", socketListen, 2 },
        .{ "__edgebox_socket_accept", socketAccept, 1 },
        .{ "__edgebox_socket_read", socketRead, 2 },
        .{ "__edgebox_socket_write", socketWrite, 2 },
        .{ "__edgebox_socket_close", socketClose, 1 },
        .{ "__edgebox_socket_state", socketState, 1 },
        .{ "__edgebox_socket_set_nodelay", socketSetNoDelay, 2 },
        .{ "__edgebox_socket_set_keepalive", socketSetKeepAlive, 3 },
        .{ "__edgebox_socket_pending_bytes", socketPendingBytes, 1 },
        .{ "__edgebox_socket_set_timeout", socketSetTimeout, 2 },
        .{ "__edgebox_socket_read_with_timeout", socketReadWithTimeout, 2 },
        .{ "__edgebox_socket_poll_writable", socketPollWritable, 2 },
        // TCP socket buffer options
        .{ "__edgebox_socket_get_recv_buffer_size", socketGetRecvBufferSize, 1 },
        .{ "__edgebox_socket_get_send_buffer_size", socketGetSendBufferSize, 1 },
        .{ "__edgebox_socket_set_recv_buffer_size", socketSetRecvBufferSize, 2 },
        .{ "__edgebox_socket_set_send_buffer_size", socketSetSendBufferSize, 2 },
        .{ "__edgebox_socket_set_linger", socketSetLinger, 3 },
        // UDP socket functions
        .{ "__edgebox_udp_socket_create", udpSocketCreate, 1 },
        .{ "__edgebox_udp_socket_bind", udpSocketBind, 3 },
        .{ "__edgebox_udp_socket_send", udpSocketSend, 4 },
        .{ "__edgebox_udp_socket_recv", udpSocketRecv, 2 },
        .{ "__edgebox_udp_socket_close", udpSocketClose, 1 },
        .{ "__edgebox_udp_set_broadcast", udpSetBroadcast, 2 },
        .{ "__edgebox_udp_set_ttl", udpSetTTL, 2 },
        .{ "__edgebox_udp_set_multicast_ttl", udpSetMulticastTTL, 2 },
        .{ "__edgebox_udp_set_multicast_loopback", udpSetMulticastLoopback, 2 },
        .{ "__edgebox_udp_add_membership", udpAddMembership, 3 },
        .{ "__edgebox_udp_drop_membership", udpDropMembership, 3 },
        .{ "__edgebox_udp_set_multicast_interface", udpSetMulticastInterface, 2 },
        .{ "__edgebox_udp_get_recv_buffer_size", udpGetRecvBufferSize, 1 },
        .{ "__edgebox_udp_get_send_buffer_size", udpGetSendBufferSize, 1 },
        .{ "__edgebox_udp_set_recv_buffer_size", udpSetRecvBufferSize, 2 },
        .{ "__edgebox_udp_set_send_buffer_size", udpSetSendBufferSize, 2 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, global, binding[0], func);
    }

    // Register in _modules for require('net') if not already set by JS polyfill
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        defer qjs.JS_FreeValue(ctx, modules_val);
        // Check if JS polyfill already set up net module
        const existing = qjs.JS_GetPropertyStr(ctx, modules_val, "net");
        if (qjs.JS_IsUndefined(existing)) {
            // Create minimal net module object with native indicator
            const net_obj = qjs.JS_NewObject(ctx);
            _ = qjs.JS_SetPropertyStr(ctx, net_obj, "_native", quickjs.jsTrue());
            _ = qjs.JS_SetPropertyStr(ctx, modules_val, "net", net_obj);
        } else {
            qjs.JS_FreeValue(ctx, existing);
        }
    }
}
