/// HTTP Dispatch Module
///
/// Handles all HTTP-related operations for EdgeBox WASM runtime:
/// - Synchronous HTTP requests (httpRequest, httpGetResponse)
/// - Asynchronous HTTP requests (httpStartAsync, httpPoll, etc.)
/// - HTTP server operations (httpServeOne, httpServeNative)
/// - Security: URL allowlisting, header redaction, size limits
///
/// This module is extracted from edgebox_wamr.zig for better organization.

const std = @import("std");
const h2 = @import("h2");
const safe_fetch = @import("../safe_fetch.zig");
const errors = @import("../errors.zig");
const wasm_helpers = @import("../wasm_helpers.zig");
const http = @import("../http/mod.zig");

const c = wasm_helpers.c;
const readWasmMemory = wasm_helpers.readWasmMemory;
const writeWasmMemory = wasm_helpers.writeWasmMemory;

// =============================================================================
// HTTP Operation Opcodes
// =============================================================================

pub const HTTP_OP_REQUEST: i32 = 0;
pub const HTTP_OP_GET_RESPONSE_LEN: i32 = 1;
pub const HTTP_OP_GET_RESPONSE: i32 = 2;
pub const HTTP_OP_START_ASYNC: i32 = 3;
pub const HTTP_OP_POLL: i32 = 4;
pub const HTTP_OP_RESPONSE_LEN: i32 = 5;
pub const HTTP_OP_RESPONSE: i32 = 6;
pub const HTTP_OP_FREE: i32 = 7;
pub const HTTP_OP_SERVE_ONE: i32 = 8; // High-perf single-call HTTP serve
pub const HTTP_OP_SERVE_NATIVE: i32 = 9; // Native event loop HTTP server
pub const HTTP_OP_REQUEST_GET: i32 = 10; // Batched: request + get response (saves 1-2 crossings)

// =============================================================================
// Configuration and Limits
// =============================================================================

// SECURITY: Default HTTP body size limits
pub const DEFAULT_MAX_HTTP_REQUEST_BODY: u32 = 10 * 1024 * 1024; // 10MB
pub const DEFAULT_MAX_HTTP_RESPONSE_BODY: u32 = 50 * 1024 * 1024; // 50MB

const MAX_ASYNC_OPS: usize = 64;

// SECURITY: Sensitive headers to redact in debug output
const sensitive_header_patterns = [_][]const u8{
    "authorization", "cookie", "x-api-key", "x-auth-token", "api-key", "token", "secret",
};

// =============================================================================
// State Management
// =============================================================================

// HTTP response state (sync) - protected by mutex for concurrent safety
var g_http_response: ?[]u8 = null;
var g_http_status: i32 = 0;
var g_http_mutex: std.Thread.Mutex = .{};

// Async HTTP operation state
pub const HttpOpStatus = enum { pending, complete, error_state };

pub const AsyncHttpRequest = struct {
    id: u32,
    status: HttpOpStatus,
    response: ?[]u8,
    http_status: u16,
    child: ?std.process.Child,
};

var g_http_ops: [MAX_ASYNC_OPS]?AsyncHttpRequest = [_]?AsyncHttpRequest{null} ** MAX_ASYNC_OPS;
var g_next_http_id: u32 = 1;

// Global allocator reference (must be set by init())
var allocator: std.mem.Allocator = undefined;

// Global security policy reference (must be set by init())
var security_policy: *const safe_fetch.SecurityPolicy = undefined;

// Socket state reference (for httpServeOne)
const MAX_SOCKETS = 64;
const O_NONBLOCK: usize = 0x4; // macOS/BSD O_NONBLOCK value

pub const SocketEntry = struct {
    fd: std.posix.socket_t,
    state: i32,
    read_buffer: ?[]u8, // Buffer for last read data
};

var g_sockets: *[MAX_SOCKETS]?SocketEntry = undefined;

// =============================================================================
// Initialization
// =============================================================================

/// Initialize HTTP dispatch module with allocator and security policy
pub fn init(alloc: std.mem.Allocator, policy: *const safe_fetch.SecurityPolicy, sockets: *[MAX_SOCKETS]?SocketEntry) void {
    allocator = alloc;
    security_policy = policy;
    g_sockets = sockets;
}

// =============================================================================
// Security Helpers
// =============================================================================

fn isSensitiveHeader(key: []const u8) bool {
    var lower_buf: [64]u8 = undefined;
    const lower = if (key.len <= 64) blk: {
        for (key, 0..) |ch, i| lower_buf[i] = std.ascii.toLower(ch);
        break :blk lower_buf[0..key.len];
    } else key;
    for (sensitive_header_patterns) |pattern| {
        if (std.mem.indexOf(u8, lower, pattern) != null) return true;
    }
    return false;
}

// =============================================================================
// Main HTTP Dispatch Function
// =============================================================================

/// Main HTTP dispatch entry point
/// Called from WASM runtime to handle all HTTP operations
pub export fn __edgebox_http_dispatch(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32, a4: i32, a5: i32, a6: i32, a7: i32, a8: i32) i32 {
    const debug = std.process.getEnvVarOwned(allocator, "EDGEBOX_DEBUG") catch null;
    if (debug) |d| {
        std.debug.print("[httpDispatch] opcode={d}\n", .{opcode});
        if (opcode == HTTP_OP_REQUEST or opcode == HTTP_OP_START_ASYNC) {
            std.debug.print("  url_ptr(a1)={d} url_len(a2)={d}\n", .{ a1, a2 });
            std.debug.print("  method_ptr(a3)={d} method_len(a4)={d}\n", .{ a3, a4 });
            std.debug.print("  headers_ptr(a5)={d} headers_len(a6)={d}\n", .{ a5, a6 });
            std.debug.print("  body_ptr(a7)={d} body_len(a8)={d}\n", .{ a7, a8 });
        }
        allocator.free(d);
    }
    return switch (opcode) {
        HTTP_OP_REQUEST => httpRequest(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4), @bitCast(a5), @bitCast(a6), @bitCast(a7), @bitCast(a8)),
        HTTP_OP_GET_RESPONSE_LEN => httpGetResponseLen(),
        HTTP_OP_GET_RESPONSE => httpGetResponse(exec_env, @bitCast(a1)),
        HTTP_OP_START_ASYNC => httpStartAsync(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4), @bitCast(a5), @bitCast(a6), @bitCast(a7), @bitCast(a8)),
        HTTP_OP_POLL => httpPoll(@bitCast(a1)),
        HTTP_OP_RESPONSE_LEN => httpResponseLen(@bitCast(a1)),
        HTTP_OP_RESPONSE => httpGetAsyncResponse(exec_env, @bitCast(a1), @bitCast(a2)),
        HTTP_OP_FREE => httpFree(@bitCast(a1)),
        // HTTP_OP_SERVE_ONE: a1=socket_id, a2=req_ptr, a3=req_len, a4=resp_ptr, a5=resp_len, a6=callback_idx
        HTTP_OP_SERVE_ONE => httpServeOne(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4), @bitCast(a5), @bitCast(a6)),
        // HTTP_OP_SERVE_NATIVE: a1=port, a2=handler_func_name_ptr, a3=handler_func_name_len
        HTTP_OP_SERVE_NATIVE => httpServeNative(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3)),
        // HTTP_OP_REQUEST_GET: Batched request + response fetch
        // a1=url_ptr, a2=url_len, a3=method_ptr, a4=method_len, a5=headers_ptr, a6=headers_len, a7=dest_ptr, a8=max_dest_len
        // Returns: (status << 20) | response_length, or negative on error
        HTTP_OP_REQUEST_GET => httpRequestGet(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3), @bitCast(a4), @bitCast(a5), @bitCast(a6), @bitCast(a7), @bitCast(a8)),
        else => -1,
    };
}

// =============================================================================
// Synchronous HTTP Operations
// =============================================================================

fn httpRequest(exec_env: c.wasm_exec_env_t, url_ptr: u32, url_len: u32, method_ptr: u32, method_len: u32, headers_ptr: u32, headers_len: u32, body_ptr: u32, body_len: u32) i32 {
    // SECURITY: Enforce request body size limit
    if (body_len > DEFAULT_MAX_HTTP_REQUEST_BODY) {
        std.debug.print("[HTTP] Request body too large: {d} bytes\n", .{body_len});
        return -413;
    }

    const url = readWasmMemory(exec_env, url_ptr, url_len) orelse return -1;
    const method = if (method_len > 0) readWasmMemory(exec_env, method_ptr, method_len) else null;
    const headers_str = if (headers_len > 0) readWasmMemory(exec_env, headers_ptr, headers_len) else null;
    const body = if (body_len > 0) readWasmMemory(exec_env, body_ptr, body_len) else null;

    const debug = std.process.getEnvVarOwned(allocator, "EDGEBOX_DEBUG") catch null;
    const show_debug = debug != null;
    if (debug) |d| allocator.free(d);

    // Security check: URL allowlist (uses safe_fetch policy)
    if (!safe_fetch.isUrlAllowed(url, security_policy.*)) {
        if (show_debug) std.debug.print("[HTTP] URL blocked by security policy: {s}\n", .{url});
        return -403; // Forbidden
    }

    if (show_debug) {
        std.debug.print("[HTTP] Request to: {s}\n", .{url});
        std.debug.print("[HTTP] Body ptr={d} len={d}\n", .{ body_ptr, body_len });
        if (body) |b| {
            std.debug.print("[HTTP] Body content: {s}\n", .{b[0..@min(b.len, 200)]});
        } else {
            std.debug.print("[HTTP] Body is null\n", .{});
        }
    }

    // Free previous response (protected by mutex)
    g_http_mutex.lock();
    if (g_http_response) |resp| {
        allocator.free(resp);
        g_http_response = null;
    }
    g_http_mutex.unlock();

    // Determine HTTP method string
    const method_str = method orelse "GET";

    if (show_debug) std.debug.print("[HTTP] Method: {s}, URL: {s}\n", .{ method_str, url });

    // Build extra headers if provided (using h2.ExtraHeader format)
    var extra_headers = std.ArrayListUnmanaged(h2.ExtraHeader){};
    defer extra_headers.deinit(allocator);

    if (headers_str) |hdr| {
        // SECURITY: Don't log raw headers (may contain sensitive values)
        if (show_debug) std.debug.print("[HTTP] Parsing {d} bytes of headers...\n", .{hdr.len});
        // Parse headers from "Key: Value\r\nKey2: Value2" format
        var lines = std.mem.splitSequence(u8, hdr, "\r\n");
        while (lines.next()) |line| {
            if (std.mem.indexOf(u8, line, ": ")) |colon_pos| {
                const key = line[0..colon_pos];
                const value = line[colon_pos + 2 ..];
                // SECURITY: Redact sensitive header values in debug output
                if (show_debug) {
                    if (isSensitiveHeader(key)) {
                        std.debug.print("[HTTP] Header: {s} = [REDACTED]\n", .{key});
                    } else {
                        std.debug.print("[HTTP] Header: {s} = {s}\n", .{ key, value });
                    }
                }
                extra_headers.append(allocator, .{ .name = key, .value = value }) catch {};
            }
        }
        if (show_debug) std.debug.print("[HTTP] Total headers parsed: {d}\n", .{extra_headers.items.len});
    }

    // Use metal0's h2 client for HTTP/1.1 and HTTP/2 with TLS support
    var client = h2.Client.init(allocator);
    defer client.deinit();

    if (show_debug) std.debug.print("[HTTP] Sending request via h2.Client...\n", .{});

    // Make request using h2 client (handles HTTP/1.1 for http://, HTTP/2 for https://)
    var response = client.request(method_str, url, extra_headers.items, body) catch |err| {
        if (show_debug) std.debug.print("[HTTP] Request error: {}\n", .{err});
        return -1;
    };
    defer response.deinit();

    // SECURITY: Enforce response body size limit
    if (response.body.len > DEFAULT_MAX_HTTP_RESPONSE_BODY) {
        if (show_debug) std.debug.print("[HTTP] Response too large: {d} bytes\n", .{response.body.len});
        return -413;
    }

    // Copy response body (h2.Response owns memory, we need to dupe before deinit)
    const body_data = allocator.dupe(u8, response.body) catch |err| {
        if (show_debug) std.debug.print("[HTTP] Body copy error: {}\n", .{err});
        return -1;
    };

    // Store status and response (protected by mutex)
    g_http_mutex.lock();
    g_http_status = @intCast(response.status);
    g_http_response = body_data;
    g_http_mutex.unlock();

    if (show_debug) std.debug.print("[HTTP] Status: {d}, Response body length: {d}\n", .{ g_http_status, body_data.len });

    return @intCast(response.status);
}

fn httpGetResponseLen() i32 {
    g_http_mutex.lock();
    defer g_http_mutex.unlock();
    if (g_http_response) |resp| {
        return @intCast(resp.len);
    }
    return 0;
}

fn httpGetResponse(exec_env: c.wasm_exec_env_t, dest_ptr: u32) i32 {
    g_http_mutex.lock();
    defer g_http_mutex.unlock();
    if (g_http_response) |resp| {
        if (!writeWasmMemory(exec_env, dest_ptr, resp)) {
            return -1;
        }
        return @intCast(resp.len);
    }
    return 0;
}

/// Batched HTTP request + get response in single crossing
/// For GET requests (no body), saves 2 WASM crossings vs REQUEST + GET_RESPONSE_LEN + GET_RESPONSE
/// Returns: (status << 20) | response_length, or negative error code
fn httpRequestGet(exec_env: c.wasm_exec_env_t, url_ptr: u32, url_len: u32, method_ptr: u32, method_len: u32, headers_ptr: u32, headers_len: u32, dest_ptr: u32, max_dest_len: u32) i32 {
    const url = readWasmMemory(exec_env, url_ptr, url_len) orelse return -1;
    const method = if (method_len > 0) readWasmMemory(exec_env, method_ptr, method_len) else null;
    const headers_str = if (headers_len > 0) readWasmMemory(exec_env, headers_ptr, headers_len) else null;

    // Security check: URL allowlist
    if (!safe_fetch.isUrlAllowed(url, security_policy.*)) {
        return -403;
    }

    const method_str = method orelse "GET";

    // Build extra headers
    var extra_headers = std.ArrayListUnmanaged(h2.ExtraHeader){};
    defer extra_headers.deinit(allocator);

    if (headers_str) |hdr| {
        var lines = std.mem.splitSequence(u8, hdr, "\r\n");
        while (lines.next()) |line| {
            if (std.mem.indexOf(u8, line, ": ")) |colon_pos| {
                extra_headers.append(allocator, .{
                    .name = line[0..colon_pos],
                    .value = line[colon_pos + 2 ..],
                }) catch {};
            }
        }
    }

    // Make HTTP request (no body for batched GET)
    var client = h2.Client.init(allocator);
    defer client.deinit();

    var response = client.request(method_str, url, extra_headers.items, null) catch {
        return -1;
    };
    defer response.deinit();

    // Check response size
    const resp_len = response.body.len;
    if (resp_len > max_dest_len or resp_len > DEFAULT_MAX_HTTP_RESPONSE_BODY) {
        return -413; // Response too large
    }

    // Write response directly to WASM memory (zero-copy to dest)
    if (resp_len > 0) {
        if (!writeWasmMemory(exec_env, dest_ptr, response.body)) {
            return -1;
        }
    }

    // Pack status and length: (status << 20) | length
    // Supports responses up to 1MB (2^20 = 1,048,576)
    const status_code: u32 = @intCast(response.status);
    const result: i32 = @bitCast((status_code << 20) | @as(u32, @intCast(resp_len)));
    return result;
}

// =============================================================================
// Asynchronous HTTP Operations
// =============================================================================

fn httpStartAsync(exec_env: c.wasm_exec_env_t, url_ptr: u32, url_len: u32, method_ptr: u32, method_len: u32, headers_ptr: u32, headers_len: u32, body_ptr: u32, body_len: u32) i32 {
    _ = headers_ptr;
    _ = headers_len;

    // Read URL from WASM memory
    const url = readWasmMemory(exec_env, url_ptr, url_len) orelse return -1;
    const method = readWasmMemory(exec_env, method_ptr, method_len) orelse "GET";
    const body = if (body_len > 0) readWasmMemory(exec_env, body_ptr, body_len) else null;

    // Find free slot
    var slot_idx: ?usize = null;
    for (g_http_ops, 0..) |op, i| {
        if (op == null) {
            slot_idx = i;
            break;
        }
    }
    if (slot_idx == null) return @intFromEnum(errors.ErrorCode.slot_exhausted);

    // Use internal helper for proper cleanup
    return httpStartAsyncImpl(url, method, body, slot_idx.?) catch -6;
}

/// Internal implementation with proper error handling for cleanup
fn httpStartAsyncImpl(url: []const u8, method: []const u8, body: ?[]const u8, slot_idx: usize) !i32 {
    // Allocate URL copy
    const url_copy = try allocator.dupe(u8, url);
    errdefer allocator.free(url_copy);

    // Build curl command
    var argv = std.ArrayListUnmanaged([]const u8){};
    errdefer argv.deinit(allocator);

    // Track body_copy for cleanup on error
    var body_copy: ?[]const u8 = null;
    errdefer if (body_copy) |bc| allocator.free(bc);

    try argv.append(allocator, "curl");
    try argv.append(allocator, "-s");
    try argv.append(allocator, "-S");
    try argv.append(allocator, "-w");
    try argv.append(allocator, "\n%{http_code}");
    try argv.append(allocator, "-X");
    try argv.append(allocator, method);

    if (body) |b| {
        try argv.append(allocator, "-d");
        body_copy = try allocator.dupe(u8, b);
        try argv.append(allocator, body_copy.?);
        try argv.append(allocator, "-H");
        try argv.append(allocator, "Content-Type: application/json");
    }

    try argv.append(allocator, url_copy);

    // Spawn curl
    var child = std.process.Child.init(argv.items, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();

    const request_id = g_next_http_id;
    g_next_http_id += 1;

    g_http_ops[slot_idx] = AsyncHttpRequest{
        .id = request_id,
        .status = .pending,
        .response = null,
        .http_status = 0,
        .child = child,
    };

    return @intCast(request_id);
}

fn httpPoll(request_id: u32) i32 {
    for (&g_http_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.status == .complete) return 1;
                if (req.status == .error_state) return -2;

                // Check if child process is done
                if (req.child) |*child| {
                    const result = child.wait() catch {
                        req.status = .error_state;
                        return -3;
                    };

                    // Read stdout
                    if (child.stdout) |stdout_file| {
                        const stdout = stdout_file.readToEndAlloc(allocator, 10 * 1024 * 1024) catch {
                            req.status = .error_state;
                            return -4;
                        };
                        defer allocator.free(stdout);

                        if (result.Exited != 0) {
                            req.status = .error_state;
                            return -5;
                        }

                        // Parse status code from curl output
                        var status_code: u16 = 0;
                        var response_body: []const u8 = stdout;

                        if (std.mem.lastIndexOf(u8, stdout, "\n")) |last_newline| {
                            response_body = stdout[0..last_newline];
                            const status_str = stdout[last_newline + 1 ..];
                            status_code = std.fmt.parseInt(u16, std.mem.trim(u8, status_str, " \n\r"), 10) catch 0;
                        }

                        // Build JSON response
                        var json_buf = std.ArrayListUnmanaged(u8){};
                        const writer = json_buf.writer(allocator);
                        writer.print("{{\"status\":{d},\"ok\":{s},\"body\":", .{
                            status_code,
                            if (status_code >= 200 and status_code < 300) "true" else "false",
                        }) catch {
                            req.status = .error_state;
                            return -6;
                        };

                        writer.writeByte('"') catch {};
                        for (response_body) |ch| {
                            switch (ch) {
                                '"' => writer.writeAll("\\\"") catch {},
                                '\\' => writer.writeAll("\\\\") catch {},
                                '\n' => writer.writeAll("\\n") catch {},
                                '\r' => writer.writeAll("\\r") catch {},
                                '\t' => writer.writeAll("\\t") catch {},
                                else => {
                                    if (ch < 0x20) {
                                        writer.print("\\u{x:0>4}", .{ch}) catch {};
                                    } else {
                                        writer.writeByte(ch) catch {};
                                    }
                                },
                            }
                        }
                        writer.writeByte('"') catch {};
                        writer.writeAll(",\"headers\":{}}") catch {};

                        req.response = json_buf.toOwnedSlice(allocator) catch null;
                        req.http_status = status_code;
                        req.status = .complete;
                        req.child = null;

                        return 1;
                    }
                }
                return 0; // Still pending
            }
        }
    }
    return -1; // Not found
}

fn httpResponseLen(request_id: u32) i32 {
    for (&g_http_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.response) |resp| {
                    return @intCast(resp.len);
                }
                return 0;
            }
        }
    }
    return -1;
}

fn httpGetAsyncResponse(exec_env: c.wasm_exec_env_t, request_id: u32, dest_ptr: u32) i32 {
    for (&g_http_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.response) |resp| {
                    if (writeWasmMemory(exec_env, dest_ptr, resp)) {
                        return @intCast(resp.len);
                    }
                }
                return 0;
            }
        }
    }
    return -1;
}

fn httpFree(request_id: u32) i32 {
    for (&g_http_ops) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.response) |resp| {
                    allocator.free(resp);
                }
                slot.* = null;
                return 0;
            }
        }
    }
    return -1;
}

// =============================================================================
// HTTP Server Operations
// =============================================================================

/// High-performance single-call HTTP serve
/// 1. Accept connection (blocking)
/// 2. Read request into req_buf
/// 3. Call WASM callback(request_len) -> response_len
/// 4. Write response from resp_buf
/// 5. Close connection
/// 6. Return response length
fn httpServeOne(exec_env: c.wasm_exec_env_t, socket_id: u32, req_ptr: u32, req_len: u32, resp_ptr: u32, resp_len: u32, callback_idx: u32) i32 {
    if (socket_id == 0 or socket_id > MAX_SOCKETS) return -1;
    const slot_idx = socket_id - 1;
    const entry = g_sockets[slot_idx] orelse return -1;

    // Get WASM module instance
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return -1;

    // Validate WASM memory addresses
    if (!c.wasm_runtime_validate_app_addr(module_inst, req_ptr, req_len)) return -2;
    if (!c.wasm_runtime_validate_app_addr(module_inst, resp_ptr, resp_len)) return -2;

    const req_buf: [*]u8 = @ptrCast(c.wasm_runtime_addr_app_to_native(module_inst, req_ptr) orelse return -2);
    const resp_buf: [*]u8 = @ptrCast(c.wasm_runtime_addr_app_to_native(module_inst, resp_ptr) orelse return -2);

    // Remove O_NONBLOCK for blocking accept
    const flags = std.posix.fcntl(entry.fd, std.posix.F.GETFL, 0) catch 0;
    _ = std.posix.fcntl(entry.fd, std.posix.F.SETFL, flags & ~O_NONBLOCK) catch {};

    // 1. Accept connection (blocking)
    var client_addr: std.posix.sockaddr.in = undefined;
    var addr_len: std.posix.socklen_t = @sizeOf(@TypeOf(client_addr));
    const client_fd = std.posix.accept(entry.fd, @ptrCast(&client_addr), &addr_len, 0) catch return -3;

    // 2. Read request
    const bytes_read = std.posix.read(client_fd, req_buf[0..req_len]) catch |err| {
        std.posix.close(client_fd);
        if (err == error.WouldBlock) return 0;
        return -4;
    };

    if (bytes_read == 0) {
        std.posix.close(client_fd);
        return 0;
    }

    // 3. Call WASM callback(request_len) -> response_len
    // The callback reads from req_buf and writes to resp_buf
    var argv = [1]u32{@intCast(bytes_read)};
    if (!c.wasm_runtime_call_indirect(exec_env, callback_idx, 1, &argv)) {
        std.posix.close(client_fd);
        return -5;
    }
    const response_len: u32 = argv[0];

    // 4. Write response
    if (response_len > 0 and response_len <= resp_len) {
        _ = std.posix.write(client_fd, resp_buf[0..response_len]) catch {};
    }

    // 5. Close
    std.posix.close(client_fd);

    return @intCast(response_len);
}

/// Native HTTP server with kqueue/epoll event loop
/// Args: port (only port is used, handler is set via globalThis.__http_native_handler in JS)
/// Returns: 0 on clean shutdown, or negative on error
///
/// The native server calls the WASM-exported `_http_native_dispatch` function for each request.
/// That function reads from globalThis.__http_native_handler and calls the JS handler.
fn httpServeNative(exec_env: c.wasm_exec_env_t, port: u32, _: u32, _: u32) i32 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return -1;

    const dispatch_func = c.wasm_runtime_lookup_function(module_inst, "_http_native_dispatch");
    if (dispatch_func == null) return -2;

    var server = http.NativeHttpServer.init(allocator, @intCast(port)) catch return -3;
    defer server.deinit();

    server.setWasmHandler(@ptrCast(exec_env), @ptrCast(dispatch_func));

    // Binary protocol - DISABLED (slower than JSON for small payloads)
    // Issue: QuickJS string ops (`url += String.fromCharCode(...)`) are slower than JSON.parse
    // JSON.parse is C-optimized and creates strings directly from the source buffer.
    // Future optimization: avoid JS strings entirely for fixed responses (Phase 2).
    _ = c.wasm_runtime_lookup_function(module_inst, "_http_native_dispatch_binary");

    server.run() catch return -4;

    // Print stats when server stops
    server.printStats();

    return 0;
}
