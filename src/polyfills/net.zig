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
});

// Socket tracking (simple fixed-size array for sandbox environment)
const MAX_SOCKETS = 256;
var sockets: [MAX_SOCKETS]SocketEntry = [_]SocketEntry{.{}} ** MAX_SOCKETS;

const SocketEntry = struct {
    fd: i32 = -1,
    state: i32 = SOCKET_STATE.CLOSED,
    non_blocking: bool = false,
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

/// __edgebox_socket_connect(socketId, port, [host]) - Connect to server
/// Returns: 0 on success, <0 on error
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
    var host: []const u8 = "127.0.0.1";
    if (argc >= 3 and qjs.JS_IsString(argv[2])) {
        const host_str = qjs.JS_ToCString(ctx, argv[2]);
        if (host_str != null) {
            host = std.mem.span(host_str);
            defer qjs.JS_FreeCString(ctx, host_str);
        }
    }

    const entry = getSocket(socket_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    // Set up server address
    var addr: c.sockaddr_in = std.mem.zeroes(c.sockaddr_in);
    addr.sin_family = c.AF_INET;
    addr.sin_port = c.htons(@as(u16, @intCast(port)));

    // Parse host address
    var addr_buf: [64]u8 = undefined;
    const host_z = std.fmt.bufPrintZ(&addr_buf, "{s}", .{host}) catch {
        return qjs.JS_NewInt32(ctx, -1);
    };

    if (c.inet_pton(c.AF_INET, host_z.ptr, &addr.sin_addr) != 1) {
        return qjs.JS_NewInt32(ctx, -2); // Invalid address
    }

    // Connect
    const result = c.connect(entry.fd, @ptrCast(&addr), @sizeOf(c.sockaddr_in));
    if (result < 0) {
        return qjs.JS_NewInt32(ctx, -3); // Connect failed
    }

    entry.state = SOCKET_STATE.CONNECTED;
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
    const result = c.write(entry.fd, data_slice.ptr, data_slice.len);
    if (result < 0) {
        return qjs.JS_NewInt32(ctx, -3);
    }

    return qjs.JS_NewInt32(ctx, @intCast(result));
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

/// Register net module native functions
pub fn register(ctx: ?*qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Register socket functions on global object (called from net.js)
    inline for (.{
        .{ "__edgebox_socket_create", socketCreate, 0 },
        .{ "__edgebox_socket_connect", socketConnect, 3 },
        .{ "__edgebox_socket_bind", socketBind, 2 },
        .{ "__edgebox_socket_listen", socketListen, 2 },
        .{ "__edgebox_socket_accept", socketAccept, 1 },
        .{ "__edgebox_socket_read", socketRead, 2 },
        .{ "__edgebox_socket_write", socketWrite, 2 },
        .{ "__edgebox_socket_close", socketClose, 1 },
        .{ "__edgebox_socket_state", socketState, 1 },
        .{ "__edgebox_socket_set_nodelay", socketSetNoDelay, 2 },
        .{ "__edgebox_socket_set_keepalive", socketSetKeepAlive, 3 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, global, binding[0], func);
    }
}
