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

    std.debug.print("Daemon listening on {s}\n", .{SOCKET_PATH});

    // Accept loop
    while (true) {
        const client_fd = std.posix.accept(server, null, null, 0) catch continue;
        handleClient(client_fd);
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

    // Dispatch to V8 pool (condvar, zero copy)
    v8_pool.dispatch(cwd);

    // Collect results (blocks until all workers done)
    const result = v8_pool.collect() catch {
        _ = std.posix.write(fd, "error: collect failed\n") catch {};
        return;
    };
    defer alloc.free(result);

    // Marker to verify this code path runs
    _ = std.posix.write(2, "[daemon] handleClient: got result, replacing \\n\n") catch {};
    _ = std.posix.write(fd, "MARKER\n") catch {};

    // Replace literal \n with actual newlines before sending
    var fixed = alloc.alloc(u8, result.len) catch {
        _ = std.posix.write(fd, result) catch {};
        return;
    };
    defer alloc.free(fixed);
    var fi: usize = 0;
    var ri: usize = 0;
    while (ri < result.len) {
        if (ri + 1 < result.len and result[ri] == '\\' and result[ri + 1] == 'n') {
            fixed[fi] = '\n';
            fi += 1;
            ri += 2;
        } else {
            fixed[fi] = result[ri];
            fi += 1;
            ri += 1;
        }
    }

    var written: usize = 0;
    while (written < fi) {
        const w = std.posix.write(fd, fixed[written..fi]) catch break;
        if (w == 0) break;
        written += w;
    }
}
