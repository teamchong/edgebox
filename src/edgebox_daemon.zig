/// EdgeBox Daemon — V8 pool with Unix socket IPC
/// Fails fast and loud — every error prints to stderr.

const std = @import("std");
const v8_pool = @import("v8_pool.zig");
const alloc = std.heap.page_allocator;

const SOCKET_PATH = "/tmp/edgebox.sock";

fn fatal(msg: []const u8) noreturn {
    _ = std.posix.write(2, "[FATAL] ") catch {};
    _ = std.posix.write(2, msg) catch {};
    _ = std.posix.write(2, "\n") catch {};
    std.process.exit(1);
}

fn log(msg: []const u8) void {
    _ = std.posix.write(2, msg) catch {};
}

pub fn start() !void {
    const cpu_count = std.Thread.getCpuCount() catch 4;
    // Formula-based: cpu_count / 6, override with EDGEBOX_WORKERS env var.
    const env_workers = std.posix.getenv("EDGEBOX_WORKERS");
    const worker_count: u32 = if (env_workers) |ew|
        std.fmt.parseInt(u32, ew, 10) catch @intCast(@min(8, @max(2, (cpu_count + 3) / 6)))
    else
        @intCast(@min(8, @max(2, (cpu_count + 3) / 6)));

    log("[daemon] starting V8 pool\n");
    v8_pool.init(worker_count) catch fatal("v8_pool.init failed");

    std.fs.cwd().deleteFile(SOCKET_PATH) catch {};

    const addr = std.net.Address.initUnix(SOCKET_PATH) catch fatal("initUnix failed");
    const server = std.posix.socket(std.posix.AF.UNIX, std.posix.SOCK.STREAM, 0) catch fatal("socket failed");
    defer std.posix.close(server);
    std.posix.bind(server, &addr.any, addr.getOsSockLen()) catch fatal("bind failed");
    std.posix.listen(server, 16) catch fatal("listen failed");

    log("[daemon] listening on /tmp/edgebox.sock\n");

    while (true) {
        const client_fd = std.posix.accept(server, null, null, 0) catch {
            log("[daemon] accept error\n");
            continue;
        };
        handleClient(client_fd);
    }
}

pub fn stop() void {
    v8_pool.deinit();
    std.fs.cwd().deleteFile(SOCKET_PATH) catch {};
}

fn handleClient(fd: std.posix.fd_t) void {
    defer std.posix.close(fd);

    var buf: [4096]u8 = undefined;
    const n = std.posix.read(fd, &buf) catch {
        log("[daemon] read error\n");
        return;
    };
    if (n == 0) {
        log("[daemon] empty request\n");
        return;
    }
    const cwd = std.mem.trim(u8, buf[0..n], &[_]u8{ '\n', '\r', ' ' });

    var timer = std.time.Timer.start() catch null;

    v8_pool.dispatch(cwd);
    const t_dispatch = if (timer) |*t| t.read() else 0;

    const result = v8_pool.collect() catch {
        log("[daemon] collect error\n");
        _ = std.posix.write(fd, "error: collect failed\n") catch {};
        return;
    };
    defer alloc.free(result);
    const t_collect = if (timer) |*t| t.read() else 0;

    var written: usize = 0;
    while (written < result.len) {
        const w = std.posix.write(fd, result[written..]) catch {
            log("[daemon] write error\n");
            break;
        };
        if (w == 0) break;
        written += w;
    }
    const t_write = if (timer) |*t| t.read() else 0;

    // Timing breakdown (nanoseconds → milliseconds)
    var tbuf: [256]u8 = undefined;
    const tmsg = std.fmt.bufPrint(&tbuf, "[daemon] dispatch:{d}ms collect:{d}ms write:{d}ms total:{d}ms bytes:{d}\n", .{
        t_dispatch / std.time.ns_per_ms,
        (t_collect - t_dispatch) / std.time.ns_per_ms,
        (t_write - t_collect) / std.time.ns_per_ms,
        t_write / std.time.ns_per_ms,
        result.len,
    }) catch "[daemon] timing error\n";
    log(tmsg);
}
