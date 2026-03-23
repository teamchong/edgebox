/// EdgeBox CLI — V8 pool daemon with Unix socket IPC
///
/// Usage:
///   edgebox daemon start     Start daemon (V8 pool, Unix socket)
///   edgebox daemon stop      Stop daemon
///   edgebox daemon status    Check if running
///   edgebox tsc [args...]    Run TSC via daemon
///
/// NO HTTP. Unix domain socket with raw bytes.
/// V8 pool pre-initialized at daemon start. Warm forever.

const std = @import("std");
const daemon = @import("edgebox_daemon.zig");
const alloc = std.heap.page_allocator;

const SOCKET_PATH = "/tmp/edgebox.sock";
const PID_FILE = "/tmp/edgebox-daemon.pid";

pub fn main() !u8 {
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len < 2) { printUsage(); return 1; }

    const cmd = args[1];
    if (std.mem.eql(u8, cmd, "daemon")) {
        if (args.len < 3) { printUsage(); return 1; }
        const sub = args[2];
        if (std.mem.eql(u8, sub, "start")) return daemonStart();
        if (std.mem.eql(u8, sub, "stop")) return daemonStop();
        if (std.mem.eql(u8, sub, "status")) return daemonStatus();
        if (std.mem.eql(u8, sub, "run")) return daemonRun(); // foreground mode
        printUsage();
        return 1;
    } else if (std.mem.eql(u8, cmd, "tsc")) {
        return runTsc();
    }
    printUsage();
    return 1;
}

fn printUsage() void {
    _ = std.posix.write(2,
        "EdgeBox — V8 pool daemon\n\n" ++
        "  edgebox daemon start    Start daemon\n" ++
        "  edgebox daemon stop     Stop daemon\n" ++
        "  edgebox daemon status   Check status\n" ++
        "  edgebox daemon run      Run in foreground\n" ++
        "  edgebox tsc             Type-check via daemon\n",
    ) catch {};
}

// ── Daemon ──

fn isDaemonRunning() bool {
    // Check if socket file exists (don't connect — that consumes the accept)
    std.fs.cwd().access(SOCKET_PATH, .{}) catch return false;
    return true;
}

fn daemonRun() u8 {
    daemon.start() catch |err| {
        std.debug.print("Daemon error: {}\n", .{err});
        return 1;
    };
    return 0;
}

fn daemonStart() u8 {
    if (isDaemonRunning()) {
        std.debug.print("Daemon already running\n", .{});
        return 0;
    }

    // Spawn self in background with "daemon run"
    var self_buf: [std.fs.max_path_bytes]u8 = undefined;
    const self_path = std.fs.selfExePath(&self_buf) catch {
        std.debug.print("Cannot find self\n", .{});
        return 1;
    };
    const argv = [_][]const u8{ self_path, "daemon", "run" };
    var child = std.process.Child.init(&argv, alloc);
    child.stdin_behavior = .Ignore;
    child.stdout_behavior = .Ignore;
    child.stderr_behavior = .Ignore;
    child.spawn() catch {
        std.debug.print("Failed to spawn daemon\n", .{});
        return 1;
    };

    const pid_str = std.fmt.allocPrint(alloc, "{d}", .{child.id}) catch return 1;
    defer alloc.free(pid_str);
    std.fs.cwd().writeFile(.{ .sub_path = PID_FILE, .data = pid_str }) catch {};

    std.debug.print("Daemon started (PID {d})\n", .{child.id});
    return 0;
}

fn daemonStop() u8 {
    const pid_data = std.fs.cwd().readFileAlloc(alloc, PID_FILE, 64) catch {
        std.debug.print("Daemon not running\n", .{});
        return 0;
    };
    defer alloc.free(pid_data);
    const pid_str = std.mem.trim(u8, pid_data, &[_]u8{ ' ', '\n', '\r' });
    const pid = std.fmt.parseInt(i32, pid_str, 10) catch return 1;
    std.posix.kill(@intCast(pid), std.posix.SIG.TERM) catch {};
    std.fs.cwd().deleteFile(PID_FILE) catch {};
    std.fs.cwd().deleteFile(SOCKET_PATH) catch {};
    std.debug.print("Daemon stopped\n", .{});
    return 0;
}

fn daemonStatus() u8 {
    if (isDaemonRunning()) {
        std.debug.print("Daemon is running\n", .{});
        return 0;
    }
    std.debug.print("Daemon is not running\n", .{});
    return 1;
}

// ── TSC ──

fn runTsc() u8 {
    // Auto-start daemon
    if (!isDaemonRunning()) {
        std.debug.print("Starting daemon...\n", .{});
        const rc = daemonStart();
        if (rc != 0) return rc;
        // Wait for workers to load TypeScript (~10s for 8 workers)
        std.Thread.sleep(12 * std.time.ns_per_s);
    }

    // Get project cwd
    var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = std.fs.cwd().realpath(".", &cwd_buf) catch "/tmp";

    // Connect to Unix socket (NOT HTTP)
    const addr = std.net.Address.initUnix(SOCKET_PATH) catch return 1;
    const fd = std.posix.socket(std.posix.AF.UNIX, std.posix.SOCK.STREAM, 0) catch {
        std.debug.print("Cannot connect to daemon\n", .{});
        return 1;
    };
    defer std.posix.close(fd);
    std.posix.connect(fd, &addr.any, addr.getOsSockLen()) catch {
        std.debug.print("Cannot connect to daemon at {s}\n", .{SOCKET_PATH});
        return 1;
    };

    // Send project path (raw bytes, no HTTP, no JSON)
    _ = std.posix.write(fd, cwd) catch return 1;
    std.posix.shutdown(fd, .send) catch {};

    // Read diagnostics (raw bytes)
    var buf: [8192]u8 = undefined;
    while (true) {
        const n = std.posix.read(fd, &buf) catch break;
        if (n == 0) break;
        _ = std.posix.write(1, buf[0..n]) catch {};
    }

    return 0;
}
