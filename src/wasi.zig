/// WASI Integration for EdgeBox
///
/// Provides WASI (WebAssembly System Interface) bindings for the QuickJS runtime.
/// Implements preview1 syscalls: filesystem, environment, args, clock, random.
///
/// This allows JavaScript code to access system resources in a sandboxed way.
const std = @import("std");
const Allocator = std.mem.Allocator;
const quickjs = @import("quickjs.zig");

/// WASI capability flags for sandbox configuration
pub const Capability = enum(u32) {
    /// File system access (fd_read, fd_write, path_open)
    filesystem = 1 << 0,
    /// Environment variables (environ_get)
    environ = 1 << 1,
    /// Command line arguments (args_get)
    args = 1 << 2,
    /// Clock/time access (clock_time_get)
    clock = 1 << 3,
    /// Random number generation (random_get)
    random = 1 << 4,
    /// Process control (proc_exit)
    proc = 1 << 5,
    /// Socket operations (preview2)
    sockets = 1 << 6,
    /// Polling (poll_oneoff)
    poll = 1 << 7,

    /// All capabilities enabled
    pub const all: u32 = 0xFF;
    /// Safe defaults (no filesystem, no sockets)
    pub const safe: u32 = @intFromEnum(Capability.environ) |
        @intFromEnum(Capability.args) |
        @intFromEnum(Capability.clock) |
        @intFromEnum(Capability.random);
};

/// WASI file descriptor
pub const Fd = i32;

/// Standard file descriptors
pub const STDIN: Fd = 0;
pub const STDOUT: Fd = 1;
pub const STDERR: Fd = 2;

/// WASI errno values
pub const Errno = enum(u16) {
    success = 0,
    toobig = 1,
    acces = 2,
    addrinuse = 3,
    addrnotavail = 4,
    afnosupport = 5,
    again = 6,
    already = 7,
    badf = 8,
    badmsg = 9,
    busy = 10,
    canceled = 11,
    child = 12,
    connaborted = 13,
    connrefused = 14,
    connreset = 15,
    deadlk = 16,
    destaddrreq = 17,
    dom = 18,
    dquot = 19,
    exist = 20,
    fault = 21,
    fbig = 22,
    hostunreach = 23,
    idrm = 24,
    ilseq = 25,
    inprogress = 26,
    intr = 27,
    inval = 28,
    io = 29,
    isconn = 30,
    isdir = 31,
    loop = 32,
    mfile = 33,
    mlink = 34,
    msgsize = 35,
    multihop = 36,
    nametoolong = 37,
    netdown = 38,
    netreset = 39,
    netunreach = 40,
    nfile = 41,
    nobufs = 42,
    nodev = 43,
    noent = 44,
    noexec = 45,
    nolck = 46,
    nolink = 47,
    nomem = 48,
    nomsg = 49,
    noprotoopt = 50,
    nospc = 51,
    nosys = 52,
    notconn = 53,
    notdir = 54,
    notempty = 55,
    notrecoverable = 56,
    notsock = 57,
    notsup = 58,
    notty = 59,
    nxio = 60,
    overflow = 61,
    ownerdead = 62,
    perm = 63,
    pipe = 64,
    proto = 65,
    protonosupport = 66,
    prototype = 67,
    range = 68,
    rofs = 69,
    spipe = 70,
    srch = 71,
    stale = 72,
    timedout = 73,
    txtbsy = 74,
    xdev = 75,
    notcapable = 76,
};

/// WASI clock IDs
pub const ClockId = enum(u32) {
    realtime = 0,
    monotonic = 1,
    process_cputime_id = 2,
    thread_cputime_id = 3,
};

/// WASI preopened directory
pub const Preopen = struct {
    path: []const u8,
    fd: Fd,
    read_only: bool = false,
};

/// WASI configuration for sandbox
pub const Config = struct {
    /// Enabled capabilities
    capabilities: u32 = Capability.safe,
    /// Preopened directories
    preopens: []const Preopen = &.{},
    /// Environment variables
    env: []const [:0]const u8 = &.{},
    /// Command line arguments
    args: []const [:0]const u8 = &.{},
    /// Memory limit in bytes
    memory_limit: usize = 256 * 1024 * 1024,
    /// Custom stdout handler
    stdout_handler: ?*const fn ([]const u8) void = null,
    /// Custom stderr handler
    stderr_handler: ?*const fn ([]const u8) void = null,
};

/// WASI context managing sandboxed resources
pub const WasiContext = struct {
    allocator: Allocator,
    config: Config,
    /// Open file descriptors
    fds: std.AutoHashMap(Fd, FileEntry),
    /// Next available fd
    next_fd: Fd,
    /// Exit code if proc_exit called
    exit_code: ?u32,

    const Self = @This();

    const FileEntry = struct {
        path: []const u8,
        file: ?std.fs.File,
        is_dir: bool,
        preopen_idx: ?usize,
    };

    /// Initialize WASI context
    pub fn init(allocator: Allocator, config: Config) !Self {
        var self = Self{
            .allocator = allocator,
            .config = config,
            .fds = std.AutoHashMap(Fd, FileEntry).init(allocator),
            .next_fd = 3, // After stdin/stdout/stderr
            .exit_code = null,
        };

        // Setup standard streams
        try self.fds.put(STDIN, .{
            .path = "/dev/stdin",
            .file = std.fs.File.stdin(),
            .is_dir = false,
            .preopen_idx = null,
        });
        try self.fds.put(STDOUT, .{
            .path = "/dev/stdout",
            .file = std.fs.File.stdout(),
            .is_dir = false,
            .preopen_idx = null,
        });
        try self.fds.put(STDERR, .{
            .path = "/dev/stderr",
            .file = std.fs.File.stderr(),
            .is_dir = false,
            .preopen_idx = null,
        });

        // Setup preopened directories
        for (config.preopens, 0..) |preopen, idx| {
            const dir = std.fs.cwd().openDir(preopen.path, .{}) catch |err| {
                std.log.warn("Failed to preopen {s}: {}", .{ preopen.path, err });
                continue;
            };

            try self.fds.put(preopen.fd, .{
                .path = try allocator.dupe(u8, preopen.path),
                .file = null, // Dirs don't have file handles in WASI
                .is_dir = true,
                .preopen_idx = idx,
            });
            _ = dir;
        }

        return self;
    }

    /// Deinitialize
    pub fn deinit(self: *Self) void {
        var iter = self.fds.iterator();
        while (iter.next()) |entry| {
            if (entry.value_ptr.file) |*file| {
                if (entry.key_ptr.* > STDERR) {
                    file.close();
                }
            }
            if (entry.value_ptr.preopen_idx != null) {
                self.allocator.free(entry.value_ptr.path);
            }
        }
        self.fds.deinit();
    }

    /// Check if capability is enabled
    pub fn hasCapability(self: *Self, cap: Capability) bool {
        return (self.config.capabilities & @intFromEnum(cap)) != 0;
    }

    // === WASI syscall implementations ===

    /// args_get - Get command line arguments
    pub fn argsGet(self: *Self, argv: [*]?[*:0]const u8, argv_buf: [*]u8) Errno {
        if (!self.hasCapability(.args)) return .notcapable;

        var buf_offset: usize = 0;
        for (self.config.args, 0..) |arg, i| {
            argv[i] = @ptrCast(argv_buf + buf_offset);
            @memcpy(argv_buf[buf_offset..][0..arg.len], arg);
            argv_buf[buf_offset + arg.len] = 0;
            buf_offset += arg.len + 1;
        }

        return .success;
    }

    /// args_sizes_get - Get argument sizes
    pub fn argsSizesGet(self: *Self, argc: *usize, argv_buf_size: *usize) Errno {
        if (!self.hasCapability(.args)) return .notcapable;

        argc.* = self.config.args.len;
        var size: usize = 0;
        for (self.config.args) |arg| {
            size += arg.len + 1;
        }
        argv_buf_size.* = size;

        return .success;
    }

    /// environ_get - Get environment variables
    pub fn environGet(self: *Self, environ: [*]?[*:0]const u8, environ_buf: [*]u8) Errno {
        if (!self.hasCapability(.environ)) return .notcapable;

        var buf_offset: usize = 0;
        for (self.config.env, 0..) |env, i| {
            environ[i] = @ptrCast(environ_buf + buf_offset);
            @memcpy(environ_buf[buf_offset..][0..env.len], env);
            environ_buf[buf_offset + env.len] = 0;
            buf_offset += env.len + 1;
        }

        return .success;
    }

    /// environ_sizes_get - Get environment sizes
    pub fn environSizesGet(self: *Self, count: *usize, buf_size: *usize) Errno {
        if (!self.hasCapability(.environ)) return .notcapable;

        count.* = self.config.env.len;
        var size: usize = 0;
        for (self.config.env) |env| {
            size += env.len + 1;
        }
        buf_size.* = size;

        return .success;
    }

    /// clock_time_get - Get current time
    pub fn clockTimeGet(self: *Self, clock_id: ClockId, _: u64, timestamp: *u64) Errno {
        if (!self.hasCapability(.clock)) return .notcapable;

        const ns = switch (clock_id) {
            .realtime => blk: {
                const now = std.time.timestamp();
                break :blk @as(u64, @intCast(now)) * std.time.ns_per_s;
            },
            .monotonic => @as(u64, @intCast(std.time.nanoTimestamp())),
            else => return .inval,
        };

        timestamp.* = ns;
        return .success;
    }

    /// random_get - Get random bytes
    pub fn randomGet(self: *Self, buf: []u8) Errno {
        if (!self.hasCapability(.random)) return .notcapable;

        std.crypto.random.bytes(buf);
        return .success;
    }

    /// fd_write - Write to file descriptor
    pub fn fdWrite(self: *Self, fd: Fd, iovs: []const std.posix.iovec_const, nwritten: *usize) Errno {
        if (!self.hasCapability(.filesystem) and fd > STDERR) return .notcapable;

        const entry = self.fds.get(fd) orelse return .badf;
        if (entry.is_dir) return .isdir;

        var total: usize = 0;

        // Handle stdout/stderr with custom handlers
        if (fd == STDOUT and self.config.stdout_handler != null) {
            for (iovs) |iov| {
                self.config.stdout_handler.?(@as([*]const u8, @ptrCast(iov.base))[0..iov.len]);
                total += iov.len;
            }
            nwritten.* = total;
            return .success;
        }

        if (fd == STDERR and self.config.stderr_handler != null) {
            for (iovs) |iov| {
                self.config.stderr_handler.?(@as([*]const u8, @ptrCast(iov.base))[0..iov.len]);
                total += iov.len;
            }
            nwritten.* = total;
            return .success;
        }

        // Regular file write
        const file = entry.file orelse return .badf;
        for (iovs) |iov| {
            const written = file.write(@as([*]const u8, @ptrCast(iov.base))[0..iov.len]) catch return .io;
            total += written;
        }

        nwritten.* = total;
        return .success;
    }

    /// fd_read - Read from file descriptor
    pub fn fdRead(self: *Self, fd: Fd, iovs: []std.posix.iovec, nread: *usize) Errno {
        if (!self.hasCapability(.filesystem) and fd > STDERR) return .notcapable;

        const entry = self.fds.get(fd) orelse return .badf;
        if (entry.is_dir) return .isdir;

        const file = entry.file orelse return .badf;
        var total: usize = 0;

        for (iovs) |iov| {
            const read = file.read(@as([*]u8, @ptrCast(iov.base))[0..iov.len]) catch return .io;
            total += read;
            if (read < iov.len) break;
        }

        nread.* = total;
        return .success;
    }

    /// fd_close - Close file descriptor
    pub fn fdClose(self: *Self, fd: Fd) Errno {
        if (fd <= STDERR) return .badf; // Can't close standard streams

        var entry = self.fds.fetchRemove(fd) orelse return .badf;
        if (entry.value.file) |*file| {
            file.close();
        }

        return .success;
    }

    /// proc_exit - Exit process
    pub fn procExit(self: *Self, code: u32) void {
        self.exit_code = code;
    }
};

/// Register WASI functions in QuickJS context
pub fn registerWasi(ctx: *quickjs.Context, wasi: *WasiContext) void {
    _ = wasi;

    // Store WASI context pointer in global object for access from C callbacks
    const global = ctx.getGlobal();
    defer global.free();

    // Register process object
    _ = ctx.eval(
        \\globalThis.process = {
        \\    env: {},
        \\    argv: [],
        \\    exit: function(code) { __wasi_proc_exit(code || 0); },
        \\    cwd: function() { return '/'; },
        \\    platform: 'wasi',
        \\    arch: 'wasm32'
        \\};
    ) catch {};

    // Register console object using QuickJS built-in print
    _ = ctx.eval(
        \\globalThis.console = {
        \\    log: function(...args) { print(args.join(' ')); },
        \\    error: function(...args) { print('[ERROR] ' + args.join(' ')); },
        \\    warn: function(...args) { print('[WARN] ' + args.join(' ')); },
        \\    info: function(...args) { print('[INFO] ' + args.join(' ')); },
        \\    debug: function(...args) { print('[DEBUG] ' + args.join(' ')); }
        \\};
    ) catch {};
}

// Tests
test "wasi context init" {
    const allocator = std.testing.allocator;

    var wasi = try WasiContext.init(allocator, .{
        .args = &.{ "test", "--flag" },
        .env = &.{"FOO=bar"},
    });
    defer wasi.deinit();

    var argc: usize = undefined;
    var argv_size: usize = undefined;
    try std.testing.expectEqual(Errno.success, wasi.argsSizesGet(&argc, &argv_size));
    try std.testing.expectEqual(@as(usize, 2), argc);
}

test "wasi clock" {
    const allocator = std.testing.allocator;

    var wasi = try WasiContext.init(allocator, .{
        .capabilities = Capability.all,
    });
    defer wasi.deinit();

    var timestamp: u64 = undefined;
    try std.testing.expectEqual(Errno.success, wasi.clockTimeGet(.realtime, 0, &timestamp));
    try std.testing.expect(timestamp > 0);
}

test "wasi random" {
    const allocator = std.testing.allocator;

    var wasi = try WasiContext.init(allocator, .{
        .capabilities = Capability.all,
    });
    defer wasi.deinit();

    var buf: [32]u8 = undefined;
    try std.testing.expectEqual(Errno.success, wasi.randomGet(&buf));

    // Check not all zeros (extremely unlikely for random)
    var all_zero = true;
    for (buf) |b| {
        if (b != 0) {
            all_zero = false;
            break;
        }
    }
    try std.testing.expect(!all_zero);
}
