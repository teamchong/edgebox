/// EdgeBox Daemon — V8 pool with Unix socket IPC
///
/// Listens on Unix domain socket. CLI connects, sends project path.
/// Dispatches to V8 pool via condvar. Returns diagnostics.
/// NO HTTP anywhere.

const std = @import("std");
const v8_pool = @import("v8_pool.zig");
const alloc = std.heap.page_allocator;

const SOCKET_PATH = "/tmp/edgebox.sock";

pub fn start() !void {
    // Formula: cpu_count / 2
    const cpu_count = std.Thread.getCpuCount() catch 4;
    const worker_count: u32 = @intCast(@max(1, cpu_count / 2));

    std.debug.print("Starting V8 pool ({d} workers)...\n", .{worker_count});
    try v8_pool.init(worker_count);

    // Remove old socket
    std.fs.cwd().deleteFile(SOCKET_PATH) catch {};

    // Listen on Unix domain socket
    const addr = std.net.Address.initUnix(SOCKET_PATH) catch return error.SocketFailed;
    const server = try std.posix.socket(std.posix.AF.UNIX, std.posix.SOCK.STREAM, 0);
    defer std.posix.close(server);
    try std.posix.bind(server, &addr.any, addr.getOsSockLen());
    try std.posix.listen(server, 16);

    _ = std.posix.write(2, "DAEMON_BUILD_V3 listening\n") catch {};

    // Accept loop
    _ = std.posix.write(2, "[daemon] entering accept loop\n") catch {};
    while (true) {
        const client_fd = std.posix.accept(server, null, null, 0) catch continue;
        _ = std.posix.write(2, "[daemon] accepted client\n") catch {};
        handleClient(client_fd);
        _ = std.posix.write(2, "[daemon] handled client\n") catch {};
    }
}

pub fn stop() void {
    v8_pool.deinit();
    std.fs.cwd().deleteFile(SOCKET_PATH) catch {};
}

fn handleClient(fd: std.posix.fd_t) void {
    defer std.posix.close(fd);

    // Read project path (raw bytes, no HTTP, no JSON)
    var buf: [4096]u8 = undefined;
    const n = std.posix.read(fd, &buf) catch return;
    if (n == 0) return;
    const cwd = std.mem.trim(u8, buf[0..n], &[_]u8{ '\n', '\r', ' ' });

    _ = std.posix.write(2, "[daemon] handleClient cwd=") catch {};
    _ = std.posix.write(2, cwd) catch {};
    _ = std.posix.write(2, "\n") catch {};

    // Dispatch to V8 pool (condvar, zero copy)
    v8_pool.dispatch(cwd);

    // Collect results (blocks until all workers done)
    const result = v8_pool.collect() catch {
        _ = std.posix.write(fd, "error: collect failed\n") catch {};
        return;
    };
    defer alloc.free(result);

    // Send results (literal \n already replaced in collect)
    var written: usize = 0;
    while (written < result.len) {
        const w = std.posix.write(fd, result[written..]) catch break;
        if (w == 0) break;
        written += w;
    }
}
