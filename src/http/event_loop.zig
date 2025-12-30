//! Native Event Loop using kqueue (macOS) / epoll (Linux)
//! Single-threaded, non-blocking I/O multiplexing

const std = @import("std");
const builtin = @import("builtin");
const posix = std.posix;

// O_NONBLOCK value for macOS/BSD
const O_NONBLOCK: u32 = 0x4;

pub const Events = packed struct {
    read: bool = false,
    write: bool = false,
};

pub const Event = struct {
    fd: posix.fd_t,
    events: Events,
    data: usize = 0,
};

// Kevent type alias for macOS
const Kevent = std.posix.system.Kevent;
const system = std.posix.system;

pub const EventLoop = struct {
    poll_fd: posix.fd_t,
    event_buffer: if (builtin.os.tag == .macos)
        [256]Kevent
    else if (builtin.os.tag == .linux)
        [256]std.os.linux.epoll_event
    else
        [256]u8,

    const Self = @This();

    pub fn init() !Self {
        const poll_fd = if (builtin.os.tag == .macos)
            try posix.kqueue()
        else if (builtin.os.tag == .linux)
            try posix.epoll_create1(0)
        else
            @compileError("Unsupported OS");

        return Self{
            .poll_fd = poll_fd,
            .event_buffer = undefined,
        };
    }

    pub fn deinit(self: *Self) void {
        posix.close(self.poll_fd);
    }

    /// Add file descriptor to event loop
    pub fn add(self: *Self, fd: posix.fd_t, events: Events, data: usize) !void {
        if (builtin.os.tag == .macos) {
            try self.kqueueAdd(fd, events, data);
        } else if (builtin.os.tag == .linux) {
            try self.epollAdd(fd, events, data);
        }
    }

    /// Modify events for existing file descriptor
    pub fn modify(self: *Self, fd: posix.fd_t, events: Events, data: usize) !void {
        if (builtin.os.tag == .macos) {
            try self.kqueueModify(fd, events, data);
        } else if (builtin.os.tag == .linux) {
            try self.epollModify(fd, events, data);
        }
    }

    /// Remove file descriptor from event loop
    pub fn remove(self: *Self, fd: posix.fd_t) !void {
        if (builtin.os.tag == .macos) {
            try self.kqueueRemove(fd);
        } else if (builtin.os.tag == .linux) {
            try self.epollRemove(fd);
        }
    }

    /// Wait for events with timeout (milliseconds, -1 for infinite)
    pub fn wait(self: *Self, timeout_ms: i32) ![]Event {
        if (builtin.os.tag == .macos) {
            return self.kqueueWait(timeout_ms);
        } else if (builtin.os.tag == .linux) {
            return self.epollWait(timeout_ms);
        } else {
            return &[_]Event{};
        }
    }

    // ========== kqueue (macOS) ==========

    fn kqueueAdd(self: *Self, fd: posix.fd_t, events: Events, data: usize) !void {
        var changes: [2]Kevent = undefined;
        var change_count: usize = 0;

        if (events.read) {
            changes[change_count] = Kevent{
                .ident = @intCast(fd),
                .filter = system.EVFILT.READ,
                .flags = system.EV.ADD | system.EV.ENABLE,
                .fflags = 0,
                .data = 0,
                .udata = data,
            };
            change_count += 1;
        }

        if (events.write) {
            changes[change_count] = Kevent{
                .ident = @intCast(fd),
                .filter = system.EVFILT.WRITE,
                .flags = system.EV.ADD | system.EV.ENABLE,
                .fflags = 0,
                .data = 0,
                .udata = data,
            };
            change_count += 1;
        }

        _ = try posix.kevent(
            self.poll_fd,
            changes[0..change_count],
            &[_]Kevent{},
            null,
        );
    }

    fn kqueueModify(self: *Self, fd: posix.fd_t, events: Events, data: usize) !void {
        // kqueue: delete old, add new
        try self.kqueueRemove(fd);
        try self.kqueueAdd(fd, events, data);
    }

    fn kqueueRemove(self: *Self, fd: posix.fd_t) !void {
        var changes: [2]Kevent = undefined;

        changes[0] = Kevent{
            .ident = @intCast(fd),
            .filter = system.EVFILT.READ,
            .flags = system.EV.DELETE,
            .fflags = 0,
            .data = 0,
            .udata = 0,
        };

        changes[1] = Kevent{
            .ident = @intCast(fd),
            .filter = system.EVFILT.WRITE,
            .flags = system.EV.DELETE,
            .fflags = 0,
            .data = 0,
            .udata = 0,
        };

        // Ignore errors (fd might not be registered for both)
        _ = posix.kevent(self.poll_fd, &changes, &[_]Kevent{}, null) catch {};
    }

    fn kqueueWait(self: *Self, timeout_ms: i32) ![]Event {
        var timeout_val = posix.timespec{
            .sec = @divTrunc(timeout_ms, 1000),
            .nsec = @rem(timeout_ms, 1000) * 1_000_000,
        };
        const timeout: ?*const posix.timespec = if (timeout_ms < 0) null else &timeout_val;

        const n = try posix.kevent(
            self.poll_fd,
            &[_]Kevent{},
            &self.event_buffer,
            timeout,
        );

        // Convert to Event array (reuse buffer space after events)
        const events_ptr: [*]Event = @ptrCast(@alignCast(&self.event_buffer));
        var out_events = events_ptr[0..n];

        for (self.event_buffer[0..n], 0..) |kev, i| {
            out_events[i] = Event{
                .fd = @intCast(kev.ident),
                .events = Events{
                    .read = kev.filter == system.EVFILT.READ,
                    .write = kev.filter == system.EVFILT.WRITE,
                },
                .data = kev.udata,
            };
        }

        return out_events;
    }

    // ========== epoll (Linux) ==========

    fn epollAdd(self: *Self, fd: posix.fd_t, events: Events, data: usize) !void {
        var ev = std.os.linux.epoll_event{
            .events = 0,
            .data = .{ .ptr = data },
        };

        if (events.read) ev.events |= std.os.linux.EPOLL.IN;
        if (events.write) ev.events |= std.os.linux.EPOLL.OUT;

        const result = std.os.linux.epoll_ctl(
            self.poll_fd,
            std.os.linux.EPOLL.CTL_ADD,
            fd,
            &ev,
        );

        if (result < 0) {
            return error.EpollAddFailed;
        }
    }

    fn epollModify(self: *Self, fd: posix.fd_t, events: Events, data: usize) !void {
        var ev = std.os.linux.epoll_event{
            .events = 0,
            .data = .{ .ptr = data },
        };

        if (events.read) ev.events |= std.os.linux.EPOLL.IN;
        if (events.write) ev.events |= std.os.linux.EPOLL.OUT;

        const result = std.os.linux.epoll_ctl(
            self.poll_fd,
            std.os.linux.EPOLL.CTL_MOD,
            fd,
            &ev,
        );

        if (result < 0) {
            return error.EpollModifyFailed;
        }
    }

    fn epollRemove(self: *Self, fd: posix.fd_t) !void {
        const result = std.os.linux.epoll_ctl(
            self.poll_fd,
            std.os.linux.EPOLL.CTL_DEL,
            fd,
            null,
        );

        if (result < 0) {
            return error.EpollRemoveFailed;
        }
    }

    fn epollWait(self: *Self, timeout_ms: i32) ![]Event {
        const result = std.os.linux.epoll_wait(
            self.poll_fd,
            &self.event_buffer,
            self.event_buffer.len,
            timeout_ms,
        );

        if (result < 0) {
            const err = posix.errno(result);
            if (err == .INTR) return &[_]Event{};
            return error.EpollWaitFailed;
        }

        const count: usize = @intCast(result);

        // Convert to Event array
        const events_ptr: [*]Event = @ptrCast(@alignCast(&self.event_buffer));
        var out_events = events_ptr[0..count];

        for (self.event_buffer[0..count], 0..) |epev, i| {
            out_events[i] = Event{
                .fd = epev.data.fd,
                .events = Events{
                    .read = (epev.events & std.os.linux.EPOLL.IN) != 0,
                    .write = (epev.events & std.os.linux.EPOLL.OUT) != 0,
                },
                .data = epev.data.ptr,
            };
        }

        return out_events;
    }
};

/// Set socket to non-blocking mode
pub fn setNonBlocking(fd: posix.fd_t) !void {
    const flags = try posix.fcntl(fd, posix.F.GETFL, 0);
    _ = try posix.fcntl(fd, posix.F.SETFL, @as(u32, @truncate(flags)) | O_NONBLOCK);
}

/// Create a TCP listener socket
pub fn createListener(port: u16, backlog: u31) !posix.fd_t {
    const sock = try posix.socket(posix.AF.INET, posix.SOCK.STREAM, 0);
    errdefer posix.close(sock);

    // SO_REUSEADDR for fast restart
    const enable: c_int = 1;
    try posix.setsockopt(sock, posix.SOL.SOCKET, posix.SO.REUSEADDR, std.mem.asBytes(&enable));

    // Set non-blocking
    try setNonBlocking(sock);

    // Bind
    var addr = posix.sockaddr.in{
        .family = posix.AF.INET,
        .port = std.mem.nativeToBig(u16, port),
        .addr = 0, // INADDR_ANY
    };
    try posix.bind(sock, @ptrCast(&addr), @sizeOf(@TypeOf(addr)));

    // Listen
    try posix.listen(sock, backlog);

    return sock;
}

// Tests
test "event loop init/deinit" {
    var loop = try EventLoop.init();
    defer loop.deinit();
}

test "create listener" {
    const sock = try createListener(0, 128); // port 0 = random available
    defer posix.close(sock);

    // Verify it's non-blocking
    const flags = try posix.fcntl(sock, posix.F.GETFL, 0);
    try std.testing.expect((@as(u32, @truncate(flags)) & O_NONBLOCK) != 0);
}
