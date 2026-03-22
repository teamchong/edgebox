/// EdgeBox CLI — daemon-based JS runtime powered by workerd + Zig
///
/// Usage:
///   edgebox daemon start     Start workerd daemon in background
///   edgebox daemon stop      Stop the daemon
///   edgebox daemon status    Check if daemon is running
///   edgebox tsc [args...]    Run TypeScript compiler via daemon
///
/// The daemon starts workerd with N parallel worker services
/// (N = cpu_count / 2, formula-based). CLI sends HTTP to daemon.
/// First `edgebox tsc` auto-starts daemon if not running.

const std = @import("std");
const alloc = std.heap.page_allocator;

const DAEMON_PORT: u16 = 18787;
const PID_FILE = "/tmp/edgebox-daemon.pid";
// Config lives in project root so workerd can resolve relative embeds
var config_file_buf: [std.fs.max_path_bytes]u8 = undefined;
fn getConfigFile() []const u8 {
    var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = std.fs.cwd().realpath(".", &cwd_buf) catch return "/tmp/edgebox-daemon.capnp";
    const result = std.fmt.bufPrint(&config_file_buf, "{s}/.edgebox-daemon.capnp", .{cwd}) catch return "/tmp/edgebox-daemon.capnp";
    return result;
}
const PROJECT_CONFIG = "/tmp/edgebox-project-config.json";

pub fn main() !u8 {
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len < 2) {
        printUsage();
        return 1;
    }

    const cmd = args[1];
    if (std.mem.eql(u8, cmd, "daemon")) {
        if (args.len < 3) { printUsage(); return 1; }
        const sub = args[2];
        if (std.mem.eql(u8, sub, "start")) return daemonStart();
        if (std.mem.eql(u8, sub, "stop")) return daemonStop();
        if (std.mem.eql(u8, sub, "status")) return daemonStatus();
        printUsage();
        return 1;
    } else if (std.mem.eql(u8, cmd, "tsc")) {
        return runTsc(args[2..]);
    } else {
        printUsage();
        return 1;
    }
}

fn printUsage() void {
    _ = std.posix.write(2,
        "EdgeBox — daemon-based JS runtime (workerd + Zig)\n" ++
        "\n" ++
        "Usage:\n" ++
        "  edgebox daemon start      Start workerd daemon\n" ++
        "  edgebox daemon stop       Stop daemon\n" ++
        "  edgebox daemon status     Check daemon status\n" ++
        "  edgebox tsc [args...]     Run TypeScript compiler\n" ++
        "\n" ++
        "The daemon auto-starts on first `edgebox tsc` if not running.\n",
    ) catch {};
}

// ── Daemon management ──

fn isDaemonRunning() bool {
    const pid_data = std.fs.cwd().readFileAlloc(alloc, PID_FILE, 64) catch return false;
    defer alloc.free(pid_data);
    const pid_str = std.mem.trim(u8, pid_data, &[_]u8{ ' ', '\n', '\r' });
    const pid = std.fmt.parseInt(i32, pid_str, 10) catch return false;
    // Check if process exists (kill with signal 0 = test only)
    std.posix.kill(@intCast(pid), 0) catch return false;
    return true;
}

fn daemonStart() u8 {
    if (isDaemonRunning()) {
        std.debug.print("Daemon already running\n", .{});
        return 0;
    }

    // Find workerd binary
    const workerd_path = findWorkerd() orelse {
        std.debug.print("ERROR: workerd binary not found\n", .{});
        return 1;
    };

    // Generate capnp config with formula-based worker count
    const cpu_count = std.Thread.getCpuCount() catch 4;
    const worker_count = @max(1, cpu_count / 2);
    generateCapnpConfig(worker_count) catch {
        std.debug.print("ERROR: failed to generate config\n", .{});
        return 1;
    };

    // Spawn workerd as daemon
    const argv = [_][]const u8{ workerd_path, "serve", getConfigFile() };
    var child = std.process.Child.init(&argv, alloc);
    child.stdin_behavior = .Ignore;
    child.stdout_behavior = .Ignore;
    child.stderr_behavior = .Ignore;
    child.spawn() catch {
        std.debug.print("ERROR: failed to spawn workerd\n", .{});
        return 1;
    };

    // Write PID file
    const pid_str = std.fmt.allocPrint(alloc, "{d}", .{child.id}) catch return 1;
    defer alloc.free(pid_str);
    std.fs.cwd().writeFile(.{ .sub_path = PID_FILE, .data = pid_str }) catch {};

    std.debug.print("Daemon started (PID {d}, {d} workers, port {d})\n", .{ child.id, worker_count, DAEMON_PORT });
    return 0;
}

fn daemonStop() u8 {
    const pid_data = std.fs.cwd().readFileAlloc(alloc, PID_FILE, 64) catch {
        std.debug.print("Daemon not running\n", .{});
        return 0;
    };
    defer alloc.free(pid_data);
    const pid_str = std.mem.trim(u8, pid_data, &[_]u8{ ' ', '\n', '\r' });
    const pid = std.fmt.parseInt(i32, pid_str, 10) catch {
        std.debug.print("Invalid PID file\n", .{});
        return 1;
    };

    std.posix.kill(@intCast(pid), std.posix.SIG.TERM) catch {};
    std.fs.cwd().deleteFile(PID_FILE) catch {};
    std.debug.print("Daemon stopped (PID {d})\n", .{pid});
    return 0;
}

fn daemonStatus() u8 {
    if (isDaemonRunning()) {
        std.debug.print("Daemon is running\n", .{});
        return 0;
    } else {
        std.debug.print("Daemon is not running\n", .{});
        return 1;
    }
}

// ── TSC command ──

fn runTsc(args: []const []const u8) u8 {
    // Auto-start daemon if not running
    if (!isDaemonRunning()) {
        std.debug.print("Starting daemon...\n", .{});
        const rc = daemonStart();
        if (rc != 0) return rc;
        // Wait for daemon to be ready — only on auto-start
        std.Thread.sleep(3 * std.time.ns_per_s);
    }
    // else: daemon already warm, no wait needed

    // Determine project cwd and tsconfig
    var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = std.fs.cwd().realpath(".", &cwd_buf) catch "/tmp";

    // Write project config for the worker
    const config = std.fmt.allocPrint(alloc, "{{\"cwd\":\"{s}\"}}", .{cwd}) catch return 1;
    defer alloc.free(config);
    std.fs.cwd().writeFile(.{ .sub_path = PROJECT_CONFIG, .data = config }) catch return 1;

    // Send HTTP request to daemon — this triggers TSC
    const stream = std.net.tcpConnectToHost(alloc, "127.0.0.1", DAEMON_PORT) catch {
        std.debug.print("ERROR: cannot connect to daemon on port {d}\n", .{DAEMON_PORT});
        return 1;
    };
    defer stream.close();

    // Send POST request
    const request = std.fmt.allocPrint(alloc,
        "POST / HTTP/1.0\r\nHost: 127.0.0.1\r\nContent-Type: application/json\r\nContent-Length: {d}\r\n\r\n{s}",
        .{ config.len, config },
    ) catch return 1;
    defer alloc.free(request);
    stream.writeAll(request) catch return 1;

    // Shutdown write side — signals to workerd we're done sending
    std.posix.shutdown(stream.handle, .send) catch {};

    // Read response until connection closes (HTTP/1.0 = close after response)
    var headers_done = false;
    var buf: [8192]u8 = undefined;
    while (true) {
        const n = stream.read(&buf) catch break;
        if (n == 0) break;
        const data = buf[0..n];
        if (!headers_done) {
            if (std.mem.indexOf(u8, data, "\r\n\r\n")) |pos| {
                headers_done = true;
                if (pos + 4 < n) {
                    _ = std.posix.write(1, data[pos + 4 ..]) catch {};
                }
            }
        } else {
            _ = std.posix.write(1, data) catch {};
        }
    }

    _ = args; // TODO: pass args to TSC via project config
    return 0;
}

// ── Helpers ──

fn findWorkerd() ?[]const u8 {
    // Check relative to binary location
    const paths = [_][]const u8{
        "vendor/workerd/bazel-bin/src/workerd/server/workerd",
        "/usr/local/bin/workerd",
        "/usr/bin/workerd",
    };
    for (paths) |p| {
        std.fs.cwd().access(p, .{}) catch continue;
        return p;
    }
    return null;
}

fn generateCapnpConfig(worker_count: usize) !void {
    var config: std.ArrayListUnmanaged(u8) = .{};
    const w = config.writer(alloc);
    try w.writeAll("using Workerd = import \"/workerd/workerd.capnp\";\n");
    try w.writeAll("const config :Workerd.Config = (\n");
    try w.writeAll("  services = [\n");
    try w.writeAll("    (name = \"main\", worker = .mainWorker),\n");
    for (0..worker_count) |i| {
        try w.print("    (name = \"worker{d}\", worker = .worker{d}),\n", .{ i, i });
    }
    try w.writeAll("  ],\n");
    try w.print("  sockets = [(name = \"main\", address = \"*:{d}\", service = \"main\")],\n", .{DAEMON_PORT});
    try w.writeAll(");\n");
    try w.writeAll("const mainWorker :Workerd.Worker = (\n");
    try w.writeAll("  modules = [\n");
    try w.writeAll("    (name = \"worker\", esModule = embed \"src/workerd-tsc/main-daemon.js\"),\n");
    try w.writeAll("    (name = \"bootstrap.js\", esModule = embed \"src/workerd-tsc/bootstrap.js\"),\n");
    try w.writeAll("  ],\n");
    // Service bindings for parallel dispatch (zero-copy inside same process)
    if (worker_count > 0) {
        try w.writeAll("  bindings = [\n");
        for (0..worker_count) |i| {
            try w.print("    (name = \"WORKER_{d}\", service = \"worker{d}\"),\n", .{ i, i });
        }
        try w.writeAll("  ],\n");
    }
    try w.writeAll("  compatibilityDate = \"2024-01-01\",\n");
    try w.writeAll(");\n");

    // Generate N worker services for parallel
    for (0..worker_count) |i| {
        try w.print("const worker{d} :Workerd.Worker = (\n", .{i});
        try w.writeAll("  modules = [\n");
        try w.writeAll("    (name = \"worker\", esModule = embed \"src/workerd-tsc/checker-parallel.js\"),\n");
        try w.writeAll("    (name = \"bootstrap.js\", esModule = embed \"src/workerd-tsc/bootstrap.js\"),\n");
        try w.writeAll("    (name = \"typescript.js\", esModule = embed \"node_modules/typescript/lib/typescript.js\"),\n");
        try w.writeAll("  ],\n");
        try w.writeAll("  compatibilityDate = \"2024-01-01\",\n");
        try w.writeAll(");\n");
    }

    const content = try config.toOwnedSlice(alloc);
    defer alloc.free(content);
    try std.fs.cwd().writeFile(.{ .sub_path = getConfigFile(), .data = content });
}
