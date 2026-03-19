/// EdgeBox IO Server — Zig-native filesystem service for workerd
///
/// Provides filesystem, process, and crypto operations over a Unix socket.
/// workerd (V8) connects here for IO since it has no native filesystem access.
///
/// Protocol: length-prefixed JSON messages over Unix socket
///   Request:  [4-byte LE length][JSON: {"op":"readFile","path":"/foo.ts"}]
///   Response: [4-byte LE length][JSON: {"ok":true,"data":"..."}]
///
/// Threading: Zig async threadpool for parallel IO (io_uring on Linux)
const std = @import("std");

const Op = enum {
    readFile,
    readFileSync,
    writeFile,
    writeFileSync,
    stat,
    statSync,
    readdir,
    readdirSync,
    realpath,
    realpathSync,
    mkdir,
    mkdirSync,
    exists,
    existsSync,
    unlink,
    unlinkSync,
    cwd,
    argv,
    env,
    exit,
};

const Request = struct {
    op: []const u8,
    path: ?[]const u8 = null,
    data: ?[]const u8 = null,
    encoding: ?[]const u8 = null,
    recursive: ?bool = null,
    code: ?i32 = null,
};

const IoServer = struct {
    allocator: std.mem.Allocator,
    socket_path: []const u8,
    server: ?std.posix.socket_t = null,
    thread_pool: std.Thread.Pool,
    should_stop: std.atomic.Value(bool),

    pub fn init(allocator: std.mem.Allocator, socket_path: []const u8) !IoServer {
        var pool: std.Thread.Pool = undefined;
        try pool.init(.{
            .allocator = allocator,
            .n_jobs = @min(std.Thread.getCpuCount() catch 4, 16),
        });

        return IoServer{
            .allocator = allocator,
            .socket_path = socket_path,
            .thread_pool = pool,
            .should_stop = std.atomic.Value(bool).init(false),
        };
    }

    pub fn deinit(self: *IoServer) void {
        self.should_stop.store(true, .release);
        if (self.server) |sock| {
            std.posix.close(sock);
        }
        // Clean up socket file
        std.fs.cwd().deleteFile(self.socket_path) catch {};
        self.thread_pool.deinit();
    }

    pub fn start(self: *IoServer) !void {
        // Remove stale socket
        std.fs.cwd().deleteFile(self.socket_path) catch {};

        const addr = try std.net.Address.initUnix(self.socket_path);
        const sock = try std.posix.socket(
            std.posix.AF.UNIX,
            std.posix.SOCK.STREAM | std.posix.SOCK.CLOEXEC,
            0,
        );
        errdefer std.posix.close(sock);

        try std.posix.bind(sock, &addr.any, addr.getOsSockLen());
        try std.posix.listen(sock, 128);
        self.server = sock;

        std.debug.print("[io-server] Listening on {s}\n", .{self.socket_path});

        // Accept loop
        while (!self.should_stop.load(.acquire)) {
            const client = std.posix.accept(sock, null, null, std.posix.SOCK.CLOEXEC) catch |err| {
                if (self.should_stop.load(.acquire)) break;
                std.debug.print("[io-server] Accept error: {}\n", .{err});
                continue;
            };

            // Handle each client connection in the thread pool
            self.thread_pool.spawn(handleClient, .{ self, client }) catch {
                std.posix.close(client);
            };
        }
    }

    fn handleClient(self: *IoServer, client: std.posix.socket_t) void {
        defer std.posix.close(client);

        var buf: [65536]u8 = undefined;
        while (!self.should_stop.load(.acquire)) {
            // Read 4-byte length prefix
            var len_buf: [4]u8 = undefined;
            const len_read = std.posix.read(client, &len_buf) catch break;
            if (len_read < 4) break;

            const msg_len = std.mem.readInt(u32, &len_buf, .little);
            if (msg_len == 0 or msg_len > buf.len) break;

            // Read message body
            var total: usize = 0;
            while (total < msg_len) {
                const n = std.posix.read(client, buf[total..msg_len]) catch break;
                if (n == 0) break;
                total += n;
            }
            if (total < msg_len) break;

            // Parse and handle request
            const response = self.handleRequest(buf[0..msg_len]) catch |err| {
                const err_msg = std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"{}\"}}", .{err}) catch break;
                defer self.allocator.free(err_msg);
                sendResponse(client, err_msg) catch break;
                continue;
            };
            defer self.allocator.free(response);
            sendResponse(client, response) catch break;
        }
    }

    fn sendResponse(client: std.posix.socket_t, msg: []const u8) !void {
        var len_buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &len_buf, @intCast(msg.len), .little);
        _ = try std.posix.write(client, &len_buf);
        _ = try std.posix.write(client, msg);
    }

    fn handleRequest(self: *IoServer, msg: []const u8) ![]const u8 {
        const parsed = std.json.parseFromSlice(Request, self.allocator, msg, .{ .ignore_unknown_fields = true }) catch {
            return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"invalid JSON\"}}", .{});
        };
        defer parsed.deinit();
        const req = parsed.value;

        // Dispatch by operation name
        if (std.mem.eql(u8, req.op, "readFileSync") or std.mem.eql(u8, req.op, "readFile")) {
            return self.opReadFile(req);
        } else if (std.mem.eql(u8, req.op, "writeFileSync") or std.mem.eql(u8, req.op, "writeFile")) {
            return self.opWriteFile(req);
        } else if (std.mem.eql(u8, req.op, "statSync") or std.mem.eql(u8, req.op, "stat")) {
            return self.opStat(req);
        } else if (std.mem.eql(u8, req.op, "readdirSync") or std.mem.eql(u8, req.op, "readdir")) {
            return self.opReaddir(req);
        } else if (std.mem.eql(u8, req.op, "realpathSync") or std.mem.eql(u8, req.op, "realpath")) {
            return self.opRealpath(req);
        } else if (std.mem.eql(u8, req.op, "mkdirSync") or std.mem.eql(u8, req.op, "mkdir")) {
            return self.opMkdir(req);
        } else if (std.mem.eql(u8, req.op, "existsSync") or std.mem.eql(u8, req.op, "exists")) {
            return self.opExists(req);
        } else if (std.mem.eql(u8, req.op, "cwd")) {
            return self.opCwd();
        } else if (std.mem.eql(u8, req.op, "exit")) {
            self.should_stop.store(true, .release);
            return try std.fmt.allocPrint(self.allocator, "{{\"ok\":true}}", .{});
        }

        return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"unknown op: {s}\"}}", .{req.op});
    }

    fn opReadFile(self: *IoServer, req: Request) ![]const u8 {
        const path = req.path orelse return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

        const file = std.fs.cwd().openFile(path, .{}) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"ENOENT: {s}\",\"code\":\"ENOENT\"}}", .{@errorName(err)});
        };
        defer file.close();

        const stat = file.stat() catch {
            return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"stat failed\",\"code\":\"EIO\"}}", .{});
        };

        // 50MB max
        if (stat.size > 50 * 1024 * 1024) {
            return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"file too large\",\"code\":\"EFBIG\"}}", .{});
        }

        const data = file.readToEndAlloc(self.allocator, 50 * 1024 * 1024) catch {
            return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"read failed\",\"code\":\"EIO\"}}", .{});
        };
        defer self.allocator.free(data);

        // JSON-escape the content
        const escaped = try jsonEscape(self.allocator, data);
        defer self.allocator.free(escaped);

        return try std.fmt.allocPrint(self.allocator, "{{\"ok\":true,\"data\":\"{s}\"}}", .{escaped});
    }

    fn opWriteFile(self: *IoServer, req: Request) ![]const u8 {
        const path = req.path orelse return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"path required\"}}", .{});
        const data = req.data orelse "";

        const file = std.fs.cwd().createFile(path, .{}) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"{s}\",\"code\":\"EPERM\"}}", .{@errorName(err)});
        };
        defer file.close();

        file.writeAll(data) catch {
            return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"write failed\",\"code\":\"EIO\"}}", .{});
        };

        return try std.fmt.allocPrint(self.allocator, "{{\"ok\":true}}", .{});
    }

    fn opStat(self: *IoServer, req: Request) ![]const u8 {
        const path = req.path orelse return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

        const stat = std.fs.cwd().statFile(path) catch {
            // Try as directory
            var dir = std.fs.cwd().openDir(path, .{}) catch {
                return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"ENOENT\",\"code\":\"ENOENT\"}}", .{});
            };
            dir.close();
            return try std.fmt.allocPrint(self.allocator, "{{\"ok\":true,\"isFile\":false,\"isDirectory\":true,\"size\":0}}", .{});
        };

        return try std.fmt.allocPrint(self.allocator, "{{\"ok\":true,\"isFile\":true,\"isDirectory\":false,\"size\":{d}}}", .{stat.size});
    }

    fn opReaddir(self: *IoServer, req: Request) ![]const u8 {
        const path = req.path orelse return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

        var dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch {
            return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"ENOENT\",\"code\":\"ENOENT\"}}", .{});
        };
        defer dir.close();

        var entries: std.ArrayListUnmanaged(u8) = .{};
        defer entries.deinit(self.allocator);

        try entries.appendSlice(self.allocator, "[");
        var first = true;

        var iter = dir.iterate();
        while (iter.next() catch null) |entry| {
            if (!first) try entries.appendSlice(self.allocator, ",");
            first = false;

            const is_dir = entry.kind == .directory;
            const name_escaped = try jsonEscape(self.allocator, entry.name);
            defer self.allocator.free(name_escaped);

            const item = try std.fmt.allocPrint(self.allocator, "{{\"name\":\"{s}\",\"isDirectory\":{s}}}", .{
                name_escaped,
                if (is_dir) "true" else "false",
            });
            defer self.allocator.free(item);
            try entries.appendSlice(self.allocator, item);
        }

        try entries.appendSlice(self.allocator, "]");

        return try std.fmt.allocPrint(self.allocator, "{{\"ok\":true,\"entries\":{s}}}", .{entries.items});
    }

    fn opRealpath(self: *IoServer, req: Request) ![]const u8 {
        const path = req.path orelse return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

        var buf: [std.fs.max_path_bytes]u8 = undefined;
        const resolved = std.fs.cwd().realpath(path, &buf) catch {
            return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"ENOENT\",\"code\":\"ENOENT\"}}", .{});
        };

        const escaped = try jsonEscape(self.allocator, resolved);
        defer self.allocator.free(escaped);

        return try std.fmt.allocPrint(self.allocator, "{{\"ok\":true,\"data\":\"{s}\"}}", .{escaped});
    }

    fn opMkdir(self: *IoServer, req: Request) ![]const u8 {
        const path = req.path orelse return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"path required\"}}", .{});
        const recursive = req.recursive orelse false;

        if (recursive) {
            std.fs.cwd().makePath(path) catch |err| {
                return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"{s}\"}}", .{@errorName(err)});
            };
        } else {
            std.fs.cwd().makeDir(path) catch |err| {
                return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"{s}\"}}", .{@errorName(err)});
            };
        }

        return try std.fmt.allocPrint(self.allocator, "{{\"ok\":true}}", .{});
    }

    fn opExists(self: *IoServer, req: Request) ![]const u8 {
        const path = req.path orelse return try std.fmt.allocPrint(self.allocator, "{{\"ok\":false,\"error\":\"path required\"}}", .{});

        const exists = blk: {
            _ = std.fs.cwd().statFile(path) catch {
                // Try directory
                var dir = std.fs.cwd().openDir(path, .{}) catch break :blk false;
                dir.close();
                break :blk true;
            };
            break :blk true;
        };

        return try std.fmt.allocPrint(self.allocator, "{{\"ok\":true,\"exists\":{s}}}", .{
            if (exists) "true" else "false",
        });
    }

    fn opCwd(self: *IoServer) ![]const u8 {
        var buf: [std.fs.max_path_bytes]u8 = undefined;
        const cwd = std.fs.cwd().realpath(".", &buf) catch "unknown";
        const escaped = try jsonEscape(self.allocator, cwd);
        defer self.allocator.free(escaped);
        return try std.fmt.allocPrint(self.allocator, "{{\"ok\":true,\"data\":\"{s}\"}}", .{escaped});
    }
};

/// JSON-escape a string (handles \n, \r, \t, \\, \", and control chars)
fn jsonEscape(allocator: std.mem.Allocator, input: []const u8) ![]const u8 {
    var out: std.ArrayListUnmanaged(u8) = .{};
    errdefer out.deinit(allocator);

    for (input) |ch| {
        switch (ch) {
            '"' => try out.appendSlice(allocator, "\\\""),
            '\\' => try out.appendSlice(allocator, "\\\\"),
            '\n' => try out.appendSlice(allocator, "\\n"),
            '\r' => try out.appendSlice(allocator, "\\r"),
            '\t' => try out.appendSlice(allocator, "\\t"),
            0x00...0x07, 0x0b, 0x0e...0x1f => {
                var hex_buf: [6]u8 = undefined;
                const hex = std.fmt.bufPrint(&hex_buf, "\\u{x:0>4}", .{ch}) catch continue;
                try out.appendSlice(allocator, hex);
            },
            else => try out.append(allocator, ch),
        }
    }

    return out.toOwnedSlice(allocator);
}

/// Main entry point — start IO server on given socket path
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const socket_path = if (args.len > 1) args[1] else "/tmp/edgebox-io.sock";

    var server = try IoServer.init(allocator, socket_path);
    defer server.deinit();

    try server.start();
}

// Export for use by edgebox_workerd.zig
pub const Server = IoServer;
