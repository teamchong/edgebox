//! Native HTTP Benchmark Tool
//!
//! A simple HTTP server benchmark for testing raw socket I/O performance.
//! This is a standalone tool that doesn't use WASM, useful for comparing
//! native Zig performance against WASM-based HTTP handlers.
//!
//! Usage: edgebox --native-http-bench [port]

const std = @import("std");

/// Run the native HTTP benchmark server
/// Tests raw socket I/O performance without WASM overhead
pub fn run(port: u16) !void {
    std.debug.print("[Native HTTP] Starting benchmark server on port {d}\n", .{port});
    std.debug.print("[Native HTTP] Run: wrk -t4 -c100 -d10s http://localhost:{d}/\n", .{port});

    const RESPONSE = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 13\r\nConnection: close\r\n\r\nHello, World!";

    // Create socket
    const sock = std.posix.socket(std.posix.AF.INET, std.posix.SOCK.STREAM, 0) catch |err| {
        std.debug.print("[Native HTTP] Failed to create socket: {}\n", .{err});
        return err;
    };
    defer std.posix.close(sock);

    // Set SO_REUSEADDR
    const enable: c_int = 1;
    std.posix.setsockopt(sock, std.posix.SOL.SOCKET, std.posix.SO.REUSEADDR, std.mem.asBytes(&enable)) catch {};

    // Bind
    var addr = std.posix.sockaddr.in{
        .family = std.posix.AF.INET,
        .port = std.mem.nativeToBig(u16, port),
        .addr = 0, // INADDR_ANY
    };
    std.posix.bind(sock, @ptrCast(&addr), @sizeOf(@TypeOf(addr))) catch |err| {
        std.debug.print("[Native HTTP] Failed to bind: {}\n", .{err});
        return err;
    };

    // Listen
    std.posix.listen(sock, 128) catch |err| {
        std.debug.print("[Native HTTP] Failed to listen: {}\n", .{err});
        return err;
    };

    std.debug.print("[Native HTTP] Listening on port {d}...\n", .{port});

    var req_buf: [8192]u8 = undefined;
    var request_count: u64 = 0;
    const start_time = std.time.milliTimestamp();

    while (true) {
        // Accept
        var client_addr: std.posix.sockaddr.in = undefined;
        var addr_len: std.posix.socklen_t = @sizeOf(@TypeOf(client_addr));
        const client = std.posix.accept(sock, @ptrCast(&client_addr), &addr_len, 0) catch continue;

        // Read request (just need to drain the buffer)
        _ = std.posix.read(client, &req_buf) catch {
            std.posix.close(client);
            continue;
        };

        // Send response
        _ = std.posix.write(client, RESPONSE) catch {};

        // Close
        std.posix.close(client);

        request_count += 1;
        if (request_count % 10000 == 0) {
            const elapsed = std.time.milliTimestamp() - start_time;
            if (elapsed > 0) {
                const rps = (request_count * 1000) / @as(u64, @intCast(elapsed));
                std.debug.print("[Native HTTP] {d} requests, {d} req/sec\n", .{ request_count, rps });
            }
        }
    }
}
