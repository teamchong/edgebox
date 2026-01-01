//! Daemon Client - Functions for communicating with EdgeBox daemon
//!
//! Provides client-side operations:
//! - warmup: Pre-load module into daemon cache
//! - down: Unregister module from daemon cache
//! - exit: Stop the daemon gracefully
//! - run: Execute WASM via daemon (auto-starts if needed)
//!
//! Uses shared memory IPC for fast path (~1-2ms), falls back to socket for
//! control operations and when shared memory is unavailable.

const std = @import("std");
const ipc = @import("../ipc/mod.zig");

/// Global shared ring for fast IPC (lazy initialized)
var g_shared_ring: ?ipc.SharedRing = null;
var g_shared_ring_init_attempted: bool = false;

/// Get the daemon socket path (XDG_RUNTIME_DIR preferred)
pub fn getSocketPath(alloc: std.mem.Allocator) ![]const u8 {
    // Prefer XDG_RUNTIME_DIR (per-user, tmpfs, auto-cleaned, mode 0700)
    if (std.posix.getenv("XDG_RUNTIME_DIR")) |xdg| {
        return std.fmt.allocPrint(alloc, "{s}/edgebox.sock", .{xdg});
    }
    // Fallback: /tmp with UID to reduce collision/attack risk
    const uid = std.c.getuid();
    return std.fmt.allocPrint(alloc, "/tmp/edgebox-{d}.sock", .{uid});
}

/// SECURITY: Sanitize paths in error messages to avoid leaking directory structure
pub fn sanitizePath(path: []const u8) []const u8 {
    if (std.mem.lastIndexOfScalar(u8, path, '/')) |idx| {
        return path[idx + 1 ..];
    }
    return path;
}

/// Connect to the daemon via Unix socket
pub fn connectToDaemon(alloc: std.mem.Allocator) !std.posix.fd_t {
    const socket_path = try getSocketPath(alloc);
    defer alloc.free(socket_path);

    const sock = try std.posix.socket(std.posix.AF.UNIX, std.posix.SOCK.STREAM, 0);
    errdefer std.posix.close(sock);

    var addr: std.posix.sockaddr.un = std.mem.zeroes(std.posix.sockaddr.un);
    addr.family = std.posix.AF.UNIX;
    @memcpy(addr.path[0..socket_path.len], socket_path);

    try std.posix.connect(sock, @ptrCast(&addr), @sizeOf(std.posix.sockaddr.un));
    return sock;
}

/// Start the daemon process via double-fork
pub fn startDaemonProcess() !void {
    // Double-fork to create a truly orphaned daemon process.
    // This prevents tools like gtimeout from killing the daemon
    // when they send SIGTERM/SIGKILL to the process group.
    const pid1 = try std.posix.fork();
    if (pid1 == 0) {
        // First child - create new session and fork again
        _ = std.posix.setsid() catch {};

        const pid2 = std.posix.fork() catch std.posix.exit(1);
        if (pid2 != 0) {
            // First child exits immediately, orphaning the grandchild
            std.posix.exit(0);
        }

        // Grandchild - the actual daemon
        // Redirect stdin/stdout to /dev/null, stderr to log file for debugging
        const dev_null = std.posix.open("/dev/null", .{ .ACCMODE = .RDWR }, 0) catch {
            std.posix.exit(1);
        };
        std.posix.dup2(dev_null, 0) catch {};
        std.posix.dup2(dev_null, 1) catch {};
        // Redirect stderr to log file for debugging
        const log_fd = std.posix.open("/tmp/edgebox-daemon.log", .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true }, 0o644) catch {
            std.posix.dup2(dev_null, 2) catch {};
            if (dev_null > 2) std.posix.close(dev_null);
            std.posix.exit(1);
        };
        std.posix.dup2(log_fd, 2) catch {};
        if (log_fd > 2) std.posix.close(log_fd);
        if (dev_null > 2) std.posix.close(dev_null);

        // Get path to self
        var self_path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const self_path = std.fs.selfExePath(&self_path_buf) catch {
            std.posix.exit(1);
        };
        self_path_buf[self_path.len] = 0;

        // Exec self as daemon server (no wasm path argument)
        const argv = [_:null]?[*:0]const u8{
            @ptrCast(&self_path_buf),
            "--daemon-server",
            null,
        };
        const envp = [_:null]?[*:0]const u8{null};
        std.posix.execveZ(@ptrCast(&self_path_buf), &argv, &envp) catch {
            std.posix.exit(1);
        };
        unreachable;
    }

    // Parent waits for first child to exit (it exits immediately after second fork)
    _ = std.posix.waitpid(pid1, 0);
}

/// Send warmup request to daemon
fn sendWarmupRequest(sock: std.posix.fd_t, wasm_path: []const u8) !void {
    // Protocol: send "WARM" prefix + path length (4 bytes) + path + newline
    _ = try std.posix.write(sock, "WARM");
    var len_buf: [4]u8 = undefined;
    std.mem.writeInt(u32, &len_buf, @intCast(wasm_path.len), .little);
    _ = try std.posix.write(sock, &len_buf);
    _ = try std.posix.write(sock, wasm_path);
    _ = try std.posix.write(sock, "\n");

    // Read response
    var buf: [1024]u8 = undefined;
    const n = std.posix.read(sock, &buf) catch 0;
    if (n > 0) {
        _ = std.posix.write(1, buf[0..n]) catch {};
    }
}

/// Send run request and print result
fn sendRequestAndPrintResult(sock: std.posix.fd_t, wasm_path: []const u8, args: []const []const u8) !void {
    // Protocol v2: path_len(4) + path + newline + args_count(4) + [arg_len(4) + arg]...
    var len_buf: [4]u8 = undefined;
    std.mem.writeInt(u32, &len_buf, @intCast(wasm_path.len), .little);
    _ = try std.posix.write(sock, &len_buf);
    _ = try std.posix.write(sock, wasm_path);
    _ = try std.posix.write(sock, "\n");

    // Send args count
    std.mem.writeInt(u32, &len_buf, @intCast(args.len), .little);
    _ = try std.posix.write(sock, &len_buf);

    // Send each arg: length + data
    for (args) |arg| {
        std.mem.writeInt(u32, &len_buf, @intCast(arg.len), .little);
        _ = try std.posix.write(sock, &len_buf);
        _ = try std.posix.write(sock, arg);
    }

    // Read and print output directly (WASM stdout goes to socket)
    var buf: [65536]u8 = undefined;
    while (true) {
        const n = std.posix.read(sock, &buf) catch break;
        if (n == 0) break;
        _ = std.posix.write(1, buf[0..n]) catch break;
    }
}

/// Pre-warm a module by asking daemon to load it
pub fn warmupModule(alloc: std.mem.Allocator, wasm_path: []const u8) !void {
    // Get absolute path
    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_path = std.fs.cwd().realpath(wasm_path, &abs_path_buf) catch {
        std.debug.print("Error: cannot resolve module: {s}\n", .{sanitizePath(wasm_path)});
        return;
    };

    // Connect to daemon (start if needed)
    const sock = connectToDaemon(alloc) catch |err| {
        if (err == error.ConnectionRefused or err == error.FileNotFound) {
            try startDaemonProcess();
            var retries: u32 = 0;
            while (retries < 50) : (retries += 1) {
                std.Thread.sleep(10 * std.time.ns_per_ms);
                if (connectToDaemon(alloc)) |s| {
                    try sendWarmupRequest(s, abs_path);
                    std.posix.close(s);
                    return;
                } else |_| {}
            }
            std.debug.print("Error: daemon failed to start\n", .{});
            return;
        }
        std.debug.print("Error connecting to daemon: {}\n", .{err});
        return;
    };
    defer std.posix.close(sock);

    try sendWarmupRequest(sock, abs_path);
}

/// Unregister a module from daemon cache
pub fn downModule(alloc: std.mem.Allocator, wasm_path: []const u8) !void {
    // Get absolute path
    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_path = std.fs.cwd().realpath(wasm_path, &abs_path_buf) catch {
        std.debug.print("Error: cannot resolve module: {s}\n", .{sanitizePath(wasm_path)});
        return;
    };

    // Connect to daemon
    const sock = connectToDaemon(alloc) catch |err| {
        if (err == error.ConnectionRefused or err == error.FileNotFound) {
            std.debug.print("Daemon not running\n", .{});
            return;
        }
        std.debug.print("Error connecting to daemon: {}\n", .{err});
        return;
    };
    defer std.posix.close(sock);

    // Protocol: send "DOWN" prefix + path length (4 bytes) + path + newline
    _ = try std.posix.write(sock, "DOWN");
    var len_buf: [4]u8 = undefined;
    std.mem.writeInt(u32, &len_buf, @intCast(abs_path.len), .little);
    _ = try std.posix.write(sock, &len_buf);
    _ = try std.posix.write(sock, abs_path);
    _ = try std.posix.write(sock, "\n");

    // Read response
    var buf: [1024]u8 = undefined;
    const n = std.posix.read(sock, &buf) catch 0;
    if (n > 0) {
        _ = std.posix.write(1, buf[0..n]) catch {};
    }
}

/// Stop the daemon gracefully
pub fn exitDaemon(alloc: std.mem.Allocator) !void {
    // Connect to daemon
    const sock = connectToDaemon(alloc) catch |err| {
        if (err == error.ConnectionRefused or err == error.FileNotFound) {
            std.debug.print("Daemon not running\n", .{});
            return;
        }
        std.debug.print("Error connecting to daemon: {}\n", .{err});
        return;
    };
    defer std.posix.close(sock);

    // Protocol: send "EXIT" command
    _ = try std.posix.write(sock, "EXIT");

    // Read response
    var buf: [1024]u8 = undefined;
    const n = std.posix.read(sock, &buf) catch 0;
    if (n > 0) {
        _ = std.posix.write(1, buf[0..n]) catch {};
    }
}

/// Try to initialize shared memory connection (lazy, once per process)
fn tryInitSharedRing() void {
    if (g_shared_ring_init_attempted) return;
    g_shared_ring_init_attempted = true;

    g_shared_ring = ipc.SharedRing.open(false) catch null;
}

/// Run via shared memory (fast path)
fn runViaSharedMemory(ring: *ipc.SharedRing, abs_path: []const u8, args: []const []const u8) !void {
    const req_id = try ring.sendRequest(abs_path, args);
    const resp = try ring.waitResponse(req_id, 30000); // 30s timeout

    if (resp) |r| {
        // Print output to stdout
        _ = std.posix.write(1, r.output) catch {};
        ring.ackResponse(&r);
    } else {
        return error.Timeout;
    }
}

/// Connect to global daemon, auto-start if needed, send wasm path for execution
pub fn runDaemon(alloc: std.mem.Allocator, wasm_path: []const u8, args: []const []const u8) !void {
    // Get absolute path for consistent identification
    var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_path = std.fs.cwd().realpath(wasm_path, &abs_path_buf) catch {
        std.debug.print("Error: cannot resolve module: {s}\n", .{sanitizePath(wasm_path)});
        return;
    };

    // Try shared memory first (fast path: ~1-2ms vs ~20ms socket)
    tryInitSharedRing();
    if (g_shared_ring) |*ring| {
        if (ring.isServerReady()) {
            if (runViaSharedMemory(ring, abs_path, args)) {
                return; // Success via shared memory
            } else |_| {
                // Fall through to socket
            }
        }
    }

    // Fallback: socket IPC (for daemon startup, errors, etc.)
    const sock = connectToDaemon(alloc) catch |err| {
        if (err == error.ConnectionRefused or err == error.FileNotFound) {
            // Daemon not running - start it
            try startDaemonProcess();
            // Wait for daemon to be ready and connect
            var retries: u32 = 0;
            while (retries < 50) : (retries += 1) {
                std.Thread.sleep(10 * std.time.ns_per_ms);
                // Try shared memory again after daemon starts
                if (!g_shared_ring_init_attempted or g_shared_ring == null) {
                    g_shared_ring_init_attempted = false;
                    tryInitSharedRing();
                }
                if (g_shared_ring) |*ring| {
                    if (ring.isServerReady()) {
                        if (runViaSharedMemory(ring, abs_path, args)) {
                            return;
                        } else |_| {}
                    }
                }
                if (connectToDaemon(alloc)) |s| {
                    try sendRequestAndPrintResult(s, abs_path, args);
                    std.posix.close(s);
                    return;
                } else |_| {}
            }
            std.debug.print("Error: daemon failed to start\n", .{});
            return;
        }
        std.debug.print("Error connecting to daemon: {}\n", .{err});
        return;
    };
    defer std.posix.close(sock);

    try sendRequestAndPrintResult(sock, abs_path, args);
}
