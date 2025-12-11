/// EdgeBox Minimal Runner - Low-Level API with mmap
/// Includes host-side HTTP bridge for network requests from WASM
const std = @import("std");
pub const c = @cImport({
    @cInclude("wasmedge/wasmedge.h");
    @cInclude("zlib.h");
});

// Global state for HTTP bridge
var g_memory: ?*c.WasmEdge_MemoryInstanceContext = null;
var g_http_response: ?[]u8 = null;
pub var g_http_allocator: std.mem.Allocator = undefined;

// HTTP domain permissions (loaded from .edgebox.json)
var g_http_allowed_domains: ?[][]const u8 = null;
var g_http_denied_domains: ?[][]const u8 = null;

// Filesystem permissions (loaded from .edgebox.json)
var g_allowed_dirs: ?[][]const u8 = null;

// ============================================================================
// Async Request Queues (HTTP and Spawn)
// ============================================================================

const AsyncStatus = enum { pending, complete, error_state };

const AsyncHttpRequest = struct {
    id: u32,
    child: ?std.process.Child,
    stdout_pipe: ?std.fs.File,
    status: AsyncStatus,
    response: ?[]u8,
    http_status: u16,
};

const AsyncSpawnRequest = struct {
    id: u32,
    child: ?std.process.Child,
    status: AsyncStatus,
    stdout: ?[]u8,
    stderr: ?[]u8,
    exit_code: i32,
};

const MAX_ASYNC_REQUESTS = 64;
var g_async_requests: [MAX_ASYNC_REQUESTS]?AsyncHttpRequest = [_]?AsyncHttpRequest{null} ** MAX_ASYNC_REQUESTS;
var g_async_spawns: [MAX_ASYNC_REQUESTS]?AsyncSpawnRequest = [_]?AsyncSpawnRequest{null} ** MAX_ASYNC_REQUESTS;
var g_next_request_id: u32 = 1;

/// Extract domain from URL (e.g., "https://api.anthropic.com/v1/messages" -> "api.anthropic.com")
fn extractDomain(url: []const u8) ?[]const u8 {
    // Skip scheme
    var start: usize = 0;
    if (std.mem.startsWith(u8, url, "https://")) {
        start = 8;
    } else if (std.mem.startsWith(u8, url, "http://")) {
        start = 7;
    }

    // Find end of domain (first / or : after scheme)
    var end = start;
    while (end < url.len and url[end] != '/' and url[end] != ':') {
        end += 1;
    }

    if (end > start) {
        return url[start..end];
    }
    return null;
}

/// Check if domain matches a pattern (supports wildcard like "*.sentry.io")
fn domainMatches(domain: []const u8, pattern: []const u8) bool {
    if (std.mem.startsWith(u8, pattern, "*.")) {
        // Wildcard match: "*.sentry.io" matches "ingest.sentry.io"
        const suffix = pattern[1..]; // ".sentry.io"
        return std.mem.endsWith(u8, domain, suffix);
    }
    return std.mem.eql(u8, domain, pattern);
}

/// Check if URL is allowed by HTTP permissions
fn isUrlAllowed(url: []const u8) bool {
    const domain = extractDomain(url) orelse return false;

    // Check deny list first
    if (g_http_denied_domains) |denied| {
        for (denied) |pattern| {
            if (domainMatches(domain, pattern)) {
                return false;
            }
        }
    }

    // Check allow list (if empty, allow all)
    if (g_http_allowed_domains) |allowed| {
        if (allowed.len == 0) return true; // Empty allow list = allow all
        for (allowed) |pattern| {
            if (domainMatches(domain, pattern)) {
                return true;
            }
        }
        return false; // Not in allow list
    }

    return true; // No allow list configured = allow all
}

/// Load environment variables from .env file (if exists)
fn loadDotEnv(allocator: std.mem.Allocator) void {
    const env_file = std.fs.cwd().openFile(".env", .{}) catch {
        if (std.posix.getenv("EDGEBOX_DEBUG")) |_| {
            std.debug.print("[.env] File not found (optional)\n", .{});
        }
        return;
    };
    defer env_file.close();

    const content = env_file.readToEndAlloc(allocator, 1024 * 64) catch return;
    defer allocator.free(content);

    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");

        // Skip empty lines and comments
        if (trimmed.len == 0 or trimmed[0] == '#') continue;

        // Parse KEY=VALUE
        if (std.mem.indexOfScalar(u8, trimmed, '=')) |eq_pos| {
            const key = std.mem.trim(u8, trimmed[0..eq_pos], " \t");
            var value = std.mem.trim(u8, trimmed[eq_pos + 1 ..], " \t");

            // Expand $HOME in values
            if (std.mem.indexOf(u8, value, "$HOME")) |_| {
                if (std.posix.getenv("HOME")) |home| {
                    const expanded = std.mem.replaceOwned(u8, allocator, value, "$HOME", home) catch continue;
                    defer allocator.free(expanded);
                    const key_z = allocator.dupeZ(u8, key) catch continue;
                    defer allocator.free(key_z);
                    const val_z = allocator.dupeZ(u8, expanded) catch continue;
                    defer allocator.free(val_z);
                    _ = std.c.setenv(key_z.ptr, val_z.ptr, 1);
                    continue;
                }
            }

            // Set env var without expansion
            const key_z = allocator.dupeZ(u8, key) catch continue;
            defer allocator.free(key_z);
            const val_z = allocator.dupeZ(u8, value) catch continue;
            defer allocator.free(val_z);
            _ = std.c.setenv(key_z.ptr, val_z.ptr, 1);

            if (std.posix.getenv("EDGEBOX_DEBUG")) |_| {
                // Only print key name, not value (security)
                std.debug.print("[.env] Loaded: {s}\n", .{key});
            }
        }
    }

    if (std.posix.getenv("EDGEBOX_DEBUG")) |_| {
        std.debug.print("[.env] Loaded successfully\n", .{});
    }
}

/// Load permissions from .edgebox.json (HTTP domains and filesystem dirs)
fn loadEdgeboxConfig(allocator: std.mem.Allocator) void {
    const config_file = std.fs.cwd().openFile(".edgebox.json", .{}) catch return;
    defer config_file.close();

    const content = config_file.readToEndAlloc(allocator, 1024 * 1024) catch return;
    defer allocator.free(content);

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, content, .{}) catch return;
    defer parsed.deinit();

    if (parsed.value != .object) return;

    // Parse dirs for filesystem access
    if (parsed.value.object.get("dirs")) |dirs_arr| {
        if (dirs_arr == .array) {
            var dirs = allocator.alloc([]const u8, dirs_arr.array.items.len) catch return;
            var count: usize = 0;
            for (dirs_arr.array.items) |item| {
                if (item == .string) {
                    // Expand ~ to HOME
                    var dir_path = item.string;
                    if (std.mem.startsWith(u8, dir_path, "~")) {
                        if (std.posix.getenv("HOME")) |home| {
                            const expanded = std.fmt.allocPrint(allocator, "{s}{s}", .{ home, dir_path[1..] }) catch continue;
                            dirs[count] = expanded;
                            count += 1;
                            continue;
                        }
                    }
                    dirs[count] = allocator.dupe(u8, dir_path) catch continue;
                    count += 1;
                }
            }
            g_allowed_dirs = dirs[0..count];
        }
    }

    // Parse HTTP permissions
    if (parsed.value.object.get("http")) |http_obj| {
        if (http_obj != .object) return;

        // Parse allow list
        if (http_obj.object.get("allow")) |allow_arr| {
            if (allow_arr == .array) {
                var domains = allocator.alloc([]const u8, allow_arr.array.items.len) catch return;
                var count: usize = 0;
                for (allow_arr.array.items) |item| {
                    if (item == .string) {
                        domains[count] = allocator.dupe(u8, item.string) catch continue;
                        count += 1;
                    }
                }
                g_http_allowed_domains = domains[0..count];
            }
        }

        // Parse deny list
        if (http_obj.object.get("deny")) |deny_arr| {
            if (deny_arr == .array) {
                var domains = allocator.alloc([]const u8, deny_arr.array.items.len) catch return;
                var count: usize = 0;
                for (deny_arr.array.items) |item| {
                    if (item == .string) {
                        domains[count] = allocator.dupe(u8, item.string) catch continue;
                        count += 1;
                    }
                }
                g_http_denied_domains = domains[0..count];
            }
        }
    }
}

fn stubVoid(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, _: [*c]const c.WasmEdge_Value, _: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    return c.WasmEdge_Result_Success;
}

fn stubZero(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, _: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    ret[0] = c.WasmEdge_ValueGenI32(0);
    return c.WasmEdge_Result_Success;
}

// ============================================================================
// HTTP Bridge - Host-side HTTP request handler
// ============================================================================

/// Read string from WASM memory
fn readWasmString(frame: ?*const c.WasmEdge_CallingFrameContext, ptr: u32, len: u32) ?[]const u8 {
    const mem = c.WasmEdge_CallingFrameGetMemoryInstance(frame, 0) orelse return null;
    const data = c.WasmEdge_MemoryInstanceGetPointer(mem, ptr, len);
    if (data == null) return null;
    return data[0..len];
}

/// Write data to WASM memory
fn writeWasmMemory(frame: ?*const c.WasmEdge_CallingFrameContext, ptr: u32, data: []const u8) bool {
    const mem = c.WasmEdge_CallingFrameGetMemoryInstance(frame, 0) orelse return false;
    const dest = c.WasmEdge_MemoryInstanceGetPointer(mem, ptr, @intCast(data.len));
    if (dest == null) return false;
    @memcpy(dest[0..data.len], data);
    return true;
}

/// Host function: edgebox_http_request
/// Args: url_ptr, url_len, method_ptr, method_len, headers_ptr, headers_len, body_ptr, body_len
/// Returns: status_code (negative on error), response stored in global buffer
fn hostHttpRequest(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const url_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const url_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));
    const method_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[2]));
    const method_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[3]));
    const headers_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[4]));
    const headers_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[5]));
    const body_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[6]));
    const body_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[7]));

    // Read strings from WASM memory
    const url = readWasmString(frame, url_ptr, url_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };
    const method = readWasmString(frame, method_ptr, method_len) orelse "GET";
    const headers_json = if (headers_len > 0) readWasmString(frame, headers_ptr, headers_len) else null;
    const body = if (body_len > 0) readWasmString(frame, body_ptr, body_len) else null;

    // Check if URL is allowed by .edgebox.json http permissions
    if (!isUrlAllowed(url)) {
        std.debug.print("[HTTP Bridge] Domain not allowed: {s}\n", .{extractDomain(url) orelse "unknown"});
        ret[0] = c.WasmEdge_ValueGenI32(-3); // Permission denied
        return c.WasmEdge_Result_Success;
    }

    // Free previous response
    if (g_http_response) |resp| {
        g_http_allocator.free(resp);
        g_http_response = null;
    }

    // Make HTTP request using Zig's std.http.Client
    const status = makeHttpRequest(url, method, headers_json, body) catch |err| {
        std.debug.print("[HTTP Bridge] Request failed: {}\n", .{err});
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    };

    ret[0] = c.WasmEdge_ValueGenI32(@intCast(status));
    return c.WasmEdge_Result_Success;
}

/// Host function: edgebox_http_get_response_len
/// Returns the length of the last HTTP response
fn hostHttpGetResponseLen(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, _: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const len: i32 = if (g_http_response) |resp| @intCast(resp.len) else 0;
    ret[0] = c.WasmEdge_ValueGenI32(len);
    return c.WasmEdge_Result_Success;
}

/// Host function: edgebox_http_get_response
/// Copies the HTTP response to WASM memory at the given pointer
fn hostHttpGetResponse(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const dest_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    if (g_http_response) |resp| {
        if (writeWasmMemory(frame, dest_ptr, resp)) {
            ret[0] = c.WasmEdge_ValueGenI32(@intCast(resp.len));
            return c.WasmEdge_Result_Success;
        }
    }
    ret[0] = c.WasmEdge_ValueGenI32(0);
    return c.WasmEdge_Result_Success;
}

/// Make actual HTTP request using curl subprocess
/// This is simpler and more reliable than using Zig's HTTP client directly
fn makeHttpRequest(url: []const u8, method: []const u8, headers_json: ?[]const u8, body: ?[]const u8) !u16 {
    _ = headers_json; // TODO: parse and pass headers to curl

    // Build curl command - use fixed array since we know max args
    var args_buf: [20][]const u8 = undefined;
    var args_len: usize = 0;

    args_buf[args_len] = "curl";
    args_len += 1;
    args_buf[args_len] = "-s"; // Silent
    args_len += 1;
    args_buf[args_len] = "-S"; // Show errors
    args_len += 1;
    args_buf[args_len] = "-w"; // Write out status code
    args_len += 1;
    args_buf[args_len] = "\n%{http_code}";
    args_len += 1;
    args_buf[args_len] = "-X";
    args_len += 1;
    args_buf[args_len] = method;
    args_len += 1;

    // Add body if present
    if (body) |b| {
        args_buf[args_len] = "-d";
        args_len += 1;
        args_buf[args_len] = b;
        args_len += 1;
        args_buf[args_len] = "-H";
        args_len += 1;
        args_buf[args_len] = "Content-Type: application/json";
        args_len += 1;
    }

    args_buf[args_len] = url;
    args_len += 1;

    // Run curl
    var child = std.process.Child.init(args_buf[0..args_len], g_http_allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();

    // Read output using File's readToEndAlloc
    const stdout = try child.stdout.?.readToEndAlloc(g_http_allocator, 10 * 1024 * 1024);
    defer g_http_allocator.free(stdout);

    const result = try child.wait();

    if (result.Exited != 0) {
        return error.CurlFailed;
    }

    // Parse status code from last line
    var status_code: u16 = 0;
    var response_body: []const u8 = stdout;

    if (std.mem.lastIndexOf(u8, stdout, "\n")) |last_newline| {
        response_body = stdout[0..last_newline];
        const status_str = stdout[last_newline + 1 ..];
        status_code = std.fmt.parseInt(u16, std.mem.trim(u8, status_str, " \n\r"), 10) catch 0;
    }

    // Build JSON response
    var json_buf = std.ArrayListUnmanaged(u8){};
    defer json_buf.deinit(g_http_allocator);

    const writer = json_buf.writer(g_http_allocator);
    try writer.print("{{\"status\":{d},\"ok\":{s},\"body\":", .{
        status_code,
        if (status_code >= 200 and status_code < 300) "true" else "false",
    });

    // JSON-escape the body (write as quoted string)
    try writer.writeByte('"');
    for (response_body) |ch| {
        switch (ch) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => {
                if (ch < 0x20) {
                    try writer.print("\\u{x:0>4}", .{ch});
                } else {
                    try writer.writeByte(ch);
                }
            },
        }
    }
    try writer.writeByte('"');
    try writer.writeAll(",\"headers\":{}}");

    // Store response globally
    g_http_response = try json_buf.toOwnedSlice(g_http_allocator);

    return status_code;
}

// ============================================================================
// Async HTTP Functions
// ============================================================================

/// Find a free slot in the async requests array
fn findFreeRequestSlot() ?usize {
    for (g_async_requests, 0..) |req, i| {
        if (req == null) return i;
    }
    return null;
}

/// Get request by ID
fn getRequestById(id: u32) ?*AsyncHttpRequest {
    for (&g_async_requests) |*slot| {
        if (slot.*) |*req| {
            if (req.id == id) return req;
        }
    }
    return null;
}

/// Start an async HTTP request
/// Args: url_ptr, url_len, method_ptr, method_len, headers_ptr, headers_len, body_ptr, body_len
/// Returns: request_id (positive) or error code (negative)
fn hostHttpStartAsync(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const url_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const url_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));
    const method_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[2]));
    const method_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[3]));
    const headers_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[4]));
    const headers_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[5]));
    const body_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[6]));
    const body_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[7]));

    // Read strings from WASM memory
    const url = readWasmString(frame, url_ptr, url_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };
    const method = readWasmString(frame, method_ptr, method_len) orelse "GET";
    _ = headers_ptr;
    _ = headers_len;
    const body = if (body_len > 0) readWasmString(frame, body_ptr, body_len) else null;

    // Check permissions
    if (!isUrlAllowed(url)) {
        ret[0] = c.WasmEdge_ValueGenI32(-3);
        return c.WasmEdge_Result_Success;
    }

    // Find free slot
    const slot_idx = findFreeRequestSlot() orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-4); // Too many pending requests
        return c.WasmEdge_Result_Success;
    };

    // Allocate URL copy (url slice is from WASM memory which may change)
    const url_copy = g_http_allocator.dupe(u8, url) catch {
        ret[0] = c.WasmEdge_ValueGenI32(-5);
        return c.WasmEdge_Result_Success;
    };

    // Build curl command
    var args_buf: [20][]const u8 = undefined;
    var args_len: usize = 0;

    args_buf[args_len] = "curl";
    args_len += 1;
    args_buf[args_len] = "-s";
    args_len += 1;
    args_buf[args_len] = "-S";
    args_len += 1;
    args_buf[args_len] = "-w";
    args_len += 1;
    args_buf[args_len] = "\n%{http_code}";
    args_len += 1;
    args_buf[args_len] = "-X";
    args_len += 1;
    args_buf[args_len] = method;
    args_len += 1;

    if (body != null) {
        args_buf[args_len] = "-d";
        args_len += 1;
        args_buf[args_len] = body.?;
        args_len += 1;
        args_buf[args_len] = "-H";
        args_len += 1;
        args_buf[args_len] = "Content-Type: application/json";
        args_len += 1;
    }

    args_buf[args_len] = url_copy;
    args_len += 1;

    // Spawn curl (non-blocking)
    var child = std.process.Child.init(args_buf[0..args_len], g_http_allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    child.spawn() catch {
        g_http_allocator.free(url_copy);
        ret[0] = c.WasmEdge_ValueGenI32(-6);
        return c.WasmEdge_Result_Success;
    };

    // Store request
    const request_id = g_next_request_id;
    g_next_request_id += 1;

    g_async_requests[slot_idx] = AsyncHttpRequest{
        .id = request_id,
        .child = child,
        .stdout_pipe = child.stdout,
        .status = .pending,
        .response = null,
        .http_status = 0,
    };

    ret[0] = c.WasmEdge_ValueGenI32(@intCast(request_id));
    return c.WasmEdge_Result_Success;
}

/// Poll an async request
/// Args: request_id
/// Returns: 0 = pending, 1 = complete, negative = error
fn hostHttpPoll(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const request_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    const req = getRequestById(request_id) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1); // Not found
        return c.WasmEdge_Result_Success;
    };

    if (req.status == .complete) {
        ret[0] = c.WasmEdge_ValueGenI32(1);
        return c.WasmEdge_Result_Success;
    }

    if (req.status == .error_state) {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    }

    // Check if child process is done (non-blocking)
    if (req.child) |*child| {
        // Try to wait with no timeout (non-blocking check)
        const result = child.wait() catch {
            req.status = .error_state;
            ret[0] = c.WasmEdge_ValueGenI32(-3);
            return c.WasmEdge_Result_Success;
        };

        // Process completed - read output
        if (child.stdout) |stdout_file| {
            const stdout = stdout_file.readToEndAlloc(g_http_allocator, 10 * 1024 * 1024) catch {
                req.status = .error_state;
                ret[0] = c.WasmEdge_ValueGenI32(-4);
                return c.WasmEdge_Result_Success;
            };
            defer g_http_allocator.free(stdout);

            if (result.Exited != 0) {
                req.status = .error_state;
                ret[0] = c.WasmEdge_ValueGenI32(-5);
                return c.WasmEdge_Result_Success;
            }

            // Parse status and build JSON response
            var status_code: u16 = 0;
            var response_body: []const u8 = stdout;

            if (std.mem.lastIndexOf(u8, stdout, "\n")) |last_newline| {
                response_body = stdout[0..last_newline];
                const status_str = stdout[last_newline + 1 ..];
                status_code = std.fmt.parseInt(u16, std.mem.trim(u8, status_str, " \n\r"), 10) catch 0;
            }

            // Build JSON response
            var json_buf = std.ArrayListUnmanaged(u8){};
            const writer = json_buf.writer(g_http_allocator);
            writer.print("{{\"status\":{d},\"ok\":{s},\"body\":", .{
                status_code,
                if (status_code >= 200 and status_code < 300) "true" else "false",
            }) catch {
                req.status = .error_state;
                ret[0] = c.WasmEdge_ValueGenI32(-6);
                return c.WasmEdge_Result_Success;
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

            req.response = json_buf.toOwnedSlice(g_http_allocator) catch null;
            req.http_status = status_code;
            req.status = .complete;
            req.child = null;

            ret[0] = c.WasmEdge_ValueGenI32(1);
            return c.WasmEdge_Result_Success;
        }
    }

    ret[0] = c.WasmEdge_ValueGenI32(0); // Still pending
    return c.WasmEdge_Result_Success;
}

/// Get async response length
/// Args: request_id
/// Returns: response length or negative on error
fn hostHttpAsyncResponseLen(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const request_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    const req = getRequestById(request_id) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    if (req.response) |resp| {
        ret[0] = c.WasmEdge_ValueGenI32(@intCast(resp.len));
    } else {
        ret[0] = c.WasmEdge_ValueGenI32(0);
    }
    return c.WasmEdge_Result_Success;
}

/// Get async response data
/// Args: request_id, dest_ptr
/// Returns: bytes written or negative on error
fn hostHttpAsyncResponse(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const request_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const dest_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));

    const req = getRequestById(request_id) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    if (req.response) |resp| {
        if (writeWasmMemory(frame, dest_ptr, resp)) {
            ret[0] = c.WasmEdge_ValueGenI32(@intCast(resp.len));
            return c.WasmEdge_Result_Success;
        }
    }

    ret[0] = c.WasmEdge_ValueGenI32(0);
    return c.WasmEdge_Result_Success;
}

/// Free an async request
/// Args: request_id
fn hostHttpAsyncFree(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const request_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    for (&g_async_requests) |*slot| {
        if (slot.*) |*req| {
            if (req.id == request_id) {
                if (req.response) |resp| {
                    g_http_allocator.free(resp);
                }
                slot.* = null;
                ret[0] = c.WasmEdge_ValueGenI32(0);
                return c.WasmEdge_Result_Success;
            }
        }
    }

    ret[0] = c.WasmEdge_ValueGenI32(-1);
    return c.WasmEdge_Result_Success;
}

// ============================================================================
// Async Spawn Functions
// ============================================================================

fn findFreeSpawnSlot() ?usize {
    for (g_async_spawns, 0..) |req, i| {
        if (req == null) return i;
    }
    return null;
}

fn getSpawnById(id: u32) ?*AsyncSpawnRequest {
    for (&g_async_spawns) |*slot| {
        if (slot.*) |*req| {
            if (req.id == id) return req;
        }
    }
    return null;
}

/// Start an async spawn
/// Args: command_ptr, command_len, args_ptr, args_len (JSON array), stdin_ptr, stdin_len
/// Returns: spawn_id (positive) or error code (negative)
fn hostSpawnStart(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const cmd_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const cmd_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));
    const args_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[2]));
    const args_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[3]));

    const command = readWasmString(frame, cmd_ptr, cmd_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    _ = args_ptr;
    _ = args_len;

    // Find free slot
    const slot_idx = findFreeSpawnSlot() orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-4);
        return c.WasmEdge_Result_Success;
    };

    // Parse command - split by spaces for simple case
    var argv_buf: [64][]const u8 = undefined;
    var argv_len: usize = 0;

    var iter = std.mem.splitScalar(u8, command, ' ');
    while (iter.next()) |arg| {
        if (arg.len > 0 and argv_len < 64) {
            argv_buf[argv_len] = arg;
            argv_len += 1;
        }
    }

    if (argv_len == 0) {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    }

    // Spawn process
    var child = std.process.Child.init(argv_buf[0..argv_len], g_http_allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    child.spawn() catch {
        ret[0] = c.WasmEdge_ValueGenI32(-3);
        return c.WasmEdge_Result_Success;
    };

    const spawn_id = g_next_request_id;
    g_next_request_id += 1;

    g_async_spawns[slot_idx] = AsyncSpawnRequest{
        .id = spawn_id,
        .child = child,
        .status = .pending,
        .stdout = null,
        .stderr = null,
        .exit_code = 0,
    };

    ret[0] = c.WasmEdge_ValueGenI32(@intCast(spawn_id));
    return c.WasmEdge_Result_Success;
}

/// Poll an async spawn
/// Returns: 0 = running, 1 = done, <0 = error
fn hostSpawnPoll(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const spawn_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    const req = getSpawnById(spawn_id) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    if (req.status == .complete) {
        ret[0] = c.WasmEdge_ValueGenI32(1);
        return c.WasmEdge_Result_Success;
    }

    if (req.status == .error_state) {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    }

    if (req.child) |*child| {
        const result = child.wait() catch {
            req.status = .error_state;
            ret[0] = c.WasmEdge_ValueGenI32(-3);
            return c.WasmEdge_Result_Success;
        };

        // Read stdout
        if (child.stdout) |stdout_file| {
            req.stdout = stdout_file.readToEndAlloc(g_http_allocator, 10 * 1024 * 1024) catch null;
        }

        // Read stderr
        if (child.stderr) |stderr_file| {
            req.stderr = stderr_file.readToEndAlloc(g_http_allocator, 10 * 1024 * 1024) catch null;
        }

        req.exit_code = @intCast(result.Exited);
        req.status = .complete;
        req.child = null;

        ret[0] = c.WasmEdge_ValueGenI32(1);
        return c.WasmEdge_Result_Success;
    }

    ret[0] = c.WasmEdge_ValueGenI32(0);
    return c.WasmEdge_Result_Success;
}

/// Get spawn output length (stdout + stderr as JSON)
fn hostSpawnOutputLen(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const spawn_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    const req = getSpawnById(spawn_id) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    // Build JSON: {"stdout": "...", "stderr": "...", "exitCode": N}
    var json_buf = std.ArrayListUnmanaged(u8){};
    const writer = json_buf.writer(g_http_allocator);

    writer.writeAll("{\"exitCode\":") catch {};
    writer.print("{d}", .{req.exit_code}) catch {};
    writer.writeAll(",\"stdout\":\"") catch {};

    if (req.stdout) |stdout| {
        for (stdout) |ch| {
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
    }

    writer.writeAll("\",\"stderr\":\"") catch {};

    if (req.stderr) |stderr| {
        for (stderr) |ch| {
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
    }

    writer.writeAll("\"}") catch {};

    // Store temporarily and return length
    if (g_http_response) |old| {
        g_http_allocator.free(old);
    }
    g_http_response = json_buf.toOwnedSlice(g_http_allocator) catch null;

    const len: i32 = if (g_http_response) |r| @intCast(r.len) else 0;
    ret[0] = c.WasmEdge_ValueGenI32(len);
    return c.WasmEdge_Result_Success;
}

/// Get spawn output (copies to WASM memory)
fn hostSpawnOutput(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const dest_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));

    if (g_http_response) |resp| {
        if (writeWasmMemory(frame, dest_ptr, resp)) {
            ret[0] = c.WasmEdge_ValueGenI32(@intCast(resp.len));
            return c.WasmEdge_Result_Success;
        }
    }

    ret[0] = c.WasmEdge_ValueGenI32(0);
    return c.WasmEdge_Result_Success;
}

/// Free a spawn request
fn hostSpawnFree(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const spawn_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    for (&g_async_spawns) |*slot| {
        if (slot.*) |*req| {
            if (req.id == spawn_id) {
                if (req.stdout) |s| g_http_allocator.free(s);
                if (req.stderr) |s| g_http_allocator.free(s);
                slot.* = null;
                ret[0] = c.WasmEdge_ValueGenI32(0);
                return c.WasmEdge_Result_Success;
            }
        }
    }

    ret[0] = c.WasmEdge_ValueGenI32(-1);
    return c.WasmEdge_Result_Success;
}

// Spawn dispatch opcodes
const SPAWN_OP_START: u32 = 0;
const SPAWN_OP_POLL: u32 = 1;
const SPAWN_OP_OUTPUT_LEN: u32 = 2;
const SPAWN_OP_OUTPUT: u32 = 3;
const SPAWN_OP_FREE: u32 = 4;

/// Spawn dispatch handler
fn hostSpawnDispatch(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const opcode: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    switch (opcode) {
        SPAWN_OP_START => {
            var sub_args = [_]c.WasmEdge_Value{ args[1], args[2], args[3], args[4] };
            return hostSpawnStart(null, frame, &sub_args, ret);
        },
        SPAWN_OP_POLL, SPAWN_OP_OUTPUT_LEN, SPAWN_OP_FREE => {
            var sub_args = [_]c.WasmEdge_Value{args[1]};
            if (opcode == SPAWN_OP_POLL) return hostSpawnPoll(null, frame, &sub_args, ret);
            if (opcode == SPAWN_OP_OUTPUT_LEN) return hostSpawnOutputLen(null, frame, &sub_args, ret);
            return hostSpawnFree(null, frame, &sub_args, ret);
        },
        SPAWN_OP_OUTPUT => {
            var sub_args = [_]c.WasmEdge_Value{ args[1], args[2] };
            return hostSpawnOutput(null, frame, &sub_args, ret);
        },
        else => {
            ret[0] = c.WasmEdge_ValueGenI32(-100);
            return c.WasmEdge_Result_Success;
        },
    }
}

/// Create the edgebox_spawn host module (single dispatch function)
pub fn createSpawnBridge() ?*c.WasmEdge_ModuleInstanceContext {
    initTypes();
    const mn = c.WasmEdge_StringCreateByCString("edgebox_spawn");
    defer c.WasmEdge_StringDelete(mn);
    const m = c.WasmEdge_ModuleInstanceCreate(mn) orelse return null;

    // dispatch(opcode, arg1, arg2, arg3, arg4) -> i32
    const params = [_]c.WasmEdge_ValType{ g_i32, g_i32, g_i32, g_i32, g_i32 };
    const ret_i32 = [_]c.WasmEdge_ValType{g_i32};
    addFunc(m, "spawn_dispatch", &params, &ret_i32, hostSpawnDispatch);

    return m;
}

// ============================================================================
// Async File I/O Bridge
// ============================================================================

const AsyncFileOp = enum { read, write };

const AsyncFileRequest = struct {
    id: u32,
    op: AsyncFileOp,
    status: AsyncStatus,
    data: ?[]u8,
    error_msg: ?[]const u8,
    bytes_written: usize,
};

var g_async_file_ops: [MAX_ASYNC_REQUESTS]?AsyncFileRequest = [_]?AsyncFileRequest{null} ** MAX_ASYNC_REQUESTS;

/// Start async file read operation
fn hostFileReadStart(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const path_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const path_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));

    std.debug.print("[hostFileReadStart] ptr={} len={} frame={*}\n", .{ path_ptr, path_len, frame });

    const path = readWasmString(frame, path_ptr, path_len) orelse {
        std.debug.print("[hostFileReadStart] readWasmString returned null\n", .{});
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };
    std.debug.print("[hostFileReadStart] path={s}\n", .{path});

    // Find free slot
    var slot_idx: ?usize = null;
    for (&g_async_file_ops, 0..) |*slot, i| {
        if (slot.* == null) {
            slot_idx = i;
            break;
        }
    }

    if (slot_idx == null) {
        ret[0] = c.WasmEdge_ValueGenI32(-4); // Too many pending operations
        return c.WasmEdge_Result_Success;
    }

    const request_id = g_next_request_id;
    g_next_request_id +%= 1;

    // Spawn thread to read file (for large files this prevents blocking)
    // For now, do synchronous read but structure allows async
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        g_async_file_ops[slot_idx.?] = AsyncFileRequest{
            .id = request_id,
            .op = .read,
            .status = .error_state,
            .data = null,
            .error_msg = switch (err) {
                error.FileNotFound => "ENOENT",
                error.AccessDenied => "EACCES",
                else => "EIO",
            },
            .bytes_written = 0,
        };
        ret[0] = c.WasmEdge_ValueGenI32(@intCast(request_id));
        return c.WasmEdge_Result_Success;
    };
    defer file.close();

    const content = file.readToEndAlloc(g_http_allocator, 100 * 1024 * 1024) catch {
        g_async_file_ops[slot_idx.?] = AsyncFileRequest{
            .id = request_id,
            .op = .read,
            .status = .error_state,
            .data = null,
            .error_msg = "EIO",
            .bytes_written = 0,
        };
        ret[0] = c.WasmEdge_ValueGenI32(@intCast(request_id));
        return c.WasmEdge_Result_Success;
    };

    g_async_file_ops[slot_idx.?] = AsyncFileRequest{
        .id = request_id,
        .op = .read,
        .status = .complete,
        .data = content,
        .error_msg = null,
        .bytes_written = content.len,
    };

    ret[0] = c.WasmEdge_ValueGenI32(@intCast(request_id));
    return c.WasmEdge_Result_Success;
}

/// Start async file write operation
fn hostFileWriteStart(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const path_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const path_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));
    const data_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[2]));
    const data_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[3]));

    const path = readWasmString(frame, path_ptr, path_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    const data = readWasmString(frame, data_ptr, data_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    };

    // Find free slot
    var slot_idx: ?usize = null;
    for (&g_async_file_ops, 0..) |*slot, i| {
        if (slot.* == null) {
            slot_idx = i;
            break;
        }
    }

    if (slot_idx == null) {
        ret[0] = c.WasmEdge_ValueGenI32(-4);
        return c.WasmEdge_Result_Success;
    }

    const request_id = g_next_request_id;
    g_next_request_id +%= 1;

    // Create parent directories if needed
    if (std.mem.lastIndexOf(u8, path, "/")) |last_slash| {
        if (last_slash > 0) {
            const parent = path[0..last_slash];
            if (path[0] == '/') {
                std.fs.makeDirAbsolute(parent) catch {};
            } else {
                std.fs.cwd().makePath(parent) catch {};
            }
        }
    }

    // Debug: show what path is being written
    if (std.process.getEnvVarOwned(std.heap.page_allocator, "EDGEBOX_DEBUG") catch null) |d| {
        std.heap.page_allocator.free(d);
        std.debug.print("[DEBUG] fs_write: {s}\n", .{path});
    }

    // Write file
    const file = if (path[0] == '/')
        std.fs.createFileAbsolute(path, .{}) catch |err| {
            g_async_file_ops[slot_idx.?] = AsyncFileRequest{
                .id = request_id,
                .op = .write,
                .status = .error_state,
                .data = null,
                .error_msg = switch (err) {
                    error.AccessDenied => "EACCES",
                    else => "EIO",
                },
                .bytes_written = 0,
            };
            ret[0] = c.WasmEdge_ValueGenI32(@intCast(request_id));
            return c.WasmEdge_Result_Success;
        }
    else
        std.fs.cwd().createFile(path, .{}) catch |err| {
            g_async_file_ops[slot_idx.?] = AsyncFileRequest{
                .id = request_id,
                .op = .write,
                .status = .error_state,
                .data = null,
                .error_msg = switch (err) {
                    error.AccessDenied => "EACCES",
                    else => "EIO",
                },
                .bytes_written = 0,
            };
            ret[0] = c.WasmEdge_ValueGenI32(@intCast(request_id));
            return c.WasmEdge_Result_Success;
        };
    defer file.close();

    file.writeAll(data) catch {
        g_async_file_ops[slot_idx.?] = AsyncFileRequest{
            .id = request_id,
            .op = .write,
            .status = .error_state,
            .data = null,
            .error_msg = "EIO",
            .bytes_written = 0,
        };
        ret[0] = c.WasmEdge_ValueGenI32(@intCast(request_id));
        return c.WasmEdge_Result_Success;
    };

    g_async_file_ops[slot_idx.?] = AsyncFileRequest{
        .id = request_id,
        .op = .write,
        .status = .complete,
        .data = null,
        .error_msg = null,
        .bytes_written = data.len,
    };

    ret[0] = c.WasmEdge_ValueGenI32(@intCast(request_id));
    return c.WasmEdge_Result_Success;
}

/// Poll async file operation - returns 0=pending, 1=complete, <0=error
fn hostFilePoll(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const request_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    for (&g_async_file_ops) |*slot| {
        if (slot.*) |req| {
            if (req.id == request_id) {
                ret[0] = c.WasmEdge_ValueGenI32(switch (req.status) {
                    .pending => 0,
                    .complete => 1,
                    .error_state => -1,
                });
                return c.WasmEdge_Result_Success;
            }
        }
    }

    ret[0] = c.WasmEdge_ValueGenI32(-2); // Not found
    return c.WasmEdge_Result_Success;
}

/// Get result length for completed file operation
fn hostFileResultLen(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const request_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    for (&g_async_file_ops) |*slot| {
        if (slot.*) |req| {
            if (req.id == request_id) {
                if (req.status == .error_state) {
                    // Return negative length to indicate error, with error message length
                    const err_len: i32 = if (req.error_msg) |msg| -@as(i32, @intCast(msg.len)) else -1;
                    ret[0] = c.WasmEdge_ValueGenI32(err_len);
                } else if (req.data) |data| {
                    ret[0] = c.WasmEdge_ValueGenI32(@intCast(data.len));
                } else {
                    ret[0] = c.WasmEdge_ValueGenI32(@intCast(req.bytes_written));
                }
                return c.WasmEdge_Result_Success;
            }
        }
    }

    ret[0] = c.WasmEdge_ValueGenI32(-1);
    return c.WasmEdge_Result_Success;
}

/// Copy file operation result to WASM memory
fn hostFileResult(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const request_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const dest_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));

    for (&g_async_file_ops) |*slot| {
        if (slot.*) |req| {
            if (req.id == request_id) {
                if (req.status == .error_state) {
                    // Copy error message
                    if (req.error_msg) |msg| {
                        const mem = c.WasmEdge_CallingFrameGetMemoryInstance(frame, 0) orelse {
                            ret[0] = c.WasmEdge_ValueGenI32(-1);
                            return c.WasmEdge_Result_Success;
                        };
                        const dest = c.WasmEdge_MemoryInstanceGetPointer(mem, dest_ptr, @intCast(msg.len));
                        if (dest == null) {
                            ret[0] = c.WasmEdge_ValueGenI32(-1);
                            return c.WasmEdge_Result_Success;
                        }
                        @memcpy(dest[0..msg.len], msg);
                        ret[0] = c.WasmEdge_ValueGenI32(@intCast(msg.len));
                        return c.WasmEdge_Result_Success;
                    }
                } else if (req.data) |data| {
                    // Copy read data
                    const mem = c.WasmEdge_CallingFrameGetMemoryInstance(frame, 0) orelse {
                        ret[0] = c.WasmEdge_ValueGenI32(-1);
                        return c.WasmEdge_Result_Success;
                    };
                    const dest = c.WasmEdge_MemoryInstanceGetPointer(mem, dest_ptr, @intCast(data.len));
                    if (dest == null) {
                        ret[0] = c.WasmEdge_ValueGenI32(-1);
                        return c.WasmEdge_Result_Success;
                    }
                    @memcpy(dest[0..data.len], data);
                    ret[0] = c.WasmEdge_ValueGenI32(@intCast(data.len));
                    return c.WasmEdge_Result_Success;
                } else {
                    // Write operation - return bytes written
                    ret[0] = c.WasmEdge_ValueGenI32(@intCast(req.bytes_written));
                    return c.WasmEdge_Result_Success;
                }
            }
        }
    }

    ret[0] = c.WasmEdge_ValueGenI32(-1);
    return c.WasmEdge_Result_Success;
}

/// Free async file operation
fn hostFileFree(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const request_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    for (&g_async_file_ops) |*slot| {
        if (slot.*) |req| {
            if (req.id == request_id) {
                if (req.data) |data| g_http_allocator.free(data);
                slot.* = null;
                ret[0] = c.WasmEdge_ValueGenI32(0);
                return c.WasmEdge_Result_Success;
            }
        }
    }

    ret[0] = c.WasmEdge_ValueGenI32(-1);
    return c.WasmEdge_Result_Success;
}

// File dispatch opcodes
const FILE_OP_READ_START: u32 = 0;
const FILE_OP_WRITE_START: u32 = 1;
const FILE_OP_POLL: u32 = 2;
const FILE_OP_RESULT_LEN: u32 = 3;
const FILE_OP_RESULT: u32 = 4;
const FILE_OP_FREE: u32 = 5;

/// File dispatch handler
fn hostFileDispatch(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const opcode: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    switch (opcode) {
        FILE_OP_READ_START => {
            var sub_args = [_]c.WasmEdge_Value{ args[1], args[2] };
            return hostFileReadStart(null, frame, &sub_args, ret);
        },
        FILE_OP_WRITE_START => {
            var sub_args = [_]c.WasmEdge_Value{ args[1], args[2], args[3], args[4] };
            return hostFileWriteStart(null, frame, &sub_args, ret);
        },
        FILE_OP_POLL, FILE_OP_RESULT_LEN, FILE_OP_FREE => {
            var sub_args = [_]c.WasmEdge_Value{args[1]};
            if (opcode == FILE_OP_POLL) return hostFilePoll(null, frame, &sub_args, ret);
            if (opcode == FILE_OP_RESULT_LEN) return hostFileResultLen(null, frame, &sub_args, ret);
            return hostFileFree(null, frame, &sub_args, ret);
        },
        FILE_OP_RESULT => {
            var sub_args = [_]c.WasmEdge_Value{ args[1], args[2] };
            return hostFileResult(null, frame, &sub_args, ret);
        },
        else => {
            ret[0] = c.WasmEdge_ValueGenI32(-100);
            return c.WasmEdge_Result_Success;
        },
    }
}

/// Create the edgebox_file host module (single dispatch function)
pub fn createFileBridge() ?*c.WasmEdge_ModuleInstanceContext {
    initTypes();
    const mn = c.WasmEdge_StringCreateByCString("edgebox_file");
    defer c.WasmEdge_StringDelete(mn);
    const m = c.WasmEdge_ModuleInstanceCreate(mn) orelse return null;

    // dispatch(opcode, arg1, arg2, arg3, arg4) -> i32
    const params = [_]c.WasmEdge_ValType{ g_i32, g_i32, g_i32, g_i32, g_i32 };
    const ret_i32 = [_]c.WasmEdge_ValType{g_i32};
    addFunc(m, "file_dispatch", &params, &ret_i32, hostFileDispatch);

    return m;
}

// ============================================================================
// Zlib Compression Bridge (using C zlib library)
// ============================================================================

var g_zlib_result: ?[]u8 = null;

/// Compress data using gzip format (deflate2 with gzip wrapper, wbits=31)
fn hostGzip(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const data_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const data_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));

    const data = readWasmString(frame, data_ptr, data_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    // Free previous result
    if (g_zlib_result) |prev| g_http_allocator.free(prev);
    g_zlib_result = null;

    // Use C zlib with gzip wrapper (wbits = 15 + 16 = 31)
    var stream: c.z_stream = std.mem.zeroes(c.z_stream);
    const init_rc = c.deflateInit2(&stream, c.Z_DEFAULT_COMPRESSION, c.Z_DEFLATED, 15 + 16, 8, c.Z_DEFAULT_STRATEGY);
    if (init_rc != c.Z_OK) {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    }
    defer _ = c.deflateEnd(&stream);

    const bound = c.deflateBound(&stream, @intCast(data.len));
    const compressed = g_http_allocator.alloc(u8, bound) catch {
        ret[0] = c.WasmEdge_ValueGenI32(-3);
        return c.WasmEdge_Result_Success;
    };

    stream.next_in = @constCast(data.ptr);
    stream.avail_in = @intCast(data.len);
    stream.next_out = compressed.ptr;
    stream.avail_out = @intCast(bound);

    const deflate_rc = c.deflate(&stream, c.Z_FINISH);
    if (deflate_rc != c.Z_STREAM_END) {
        g_http_allocator.free(compressed);
        ret[0] = c.WasmEdge_ValueGenI32(-4);
        return c.WasmEdge_Result_Success;
    }

    const produced = bound - stream.avail_out;
    g_zlib_result = compressed[0..produced];

    ret[0] = c.WasmEdge_ValueGenI32(@intCast(produced));
    return c.WasmEdge_Result_Success;
}

/// Decompress gzip data
fn hostGunzip(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const data_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const data_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));

    const data = readWasmString(frame, data_ptr, data_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    if (g_zlib_result) |prev| g_http_allocator.free(prev);
    g_zlib_result = null;

    // Use C zlib with gzip wrapper (wbits = 15 + 16 = 31)
    var stream: c.z_stream = std.mem.zeroes(c.z_stream);
    const init_rc = c.inflateInit2(&stream, 15 + 16);
    if (init_rc != c.Z_OK) {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    }
    defer _ = c.inflateEnd(&stream);

    // Auto-grow buffer for decompression
    var buf_size: usize = data.len * 5;
    if (buf_size < 1024) buf_size = 1024;

    while (buf_size <= 256 * 1024 * 1024) {
        const decompressed = g_http_allocator.alloc(u8, buf_size) catch {
            ret[0] = c.WasmEdge_ValueGenI32(-3);
            return c.WasmEdge_Result_Success;
        };

        // Reset stream for retry
        _ = c.inflateReset(&stream);
        stream.next_in = @constCast(data.ptr);
        stream.avail_in = @intCast(data.len);
        stream.next_out = decompressed.ptr;
        stream.avail_out = @intCast(buf_size);

        const inflate_rc = c.inflate(&stream, c.Z_FINISH);
        if (inflate_rc == c.Z_STREAM_END) {
            const produced = buf_size - stream.avail_out;
            g_zlib_result = decompressed[0..produced];
            ret[0] = c.WasmEdge_ValueGenI32(@intCast(produced));
            return c.WasmEdge_Result_Success;
        } else if (inflate_rc == c.Z_BUF_ERROR or stream.avail_out == 0) {
            g_http_allocator.free(decompressed);
            buf_size *= 2;
        } else {
            g_http_allocator.free(decompressed);
            ret[0] = c.WasmEdge_ValueGenI32(-4);
            return c.WasmEdge_Result_Success;
        }
    }

    ret[0] = c.WasmEdge_ValueGenI32(-5); // Buffer too large
    return c.WasmEdge_Result_Success;
}

/// Compress data using deflate (zlib format)
fn hostDeflate(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const data_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const data_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));

    const data = readWasmString(frame, data_ptr, data_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    if (g_zlib_result) |prev| g_http_allocator.free(prev);
    g_zlib_result = null;

    // Use C zlib with zlib wrapper (default wbits = 15)
    const bound = c.compressBound(@intCast(data.len));
    const compressed = g_http_allocator.alloc(u8, bound) catch {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    };

    var compressed_len: c.uLongf = bound;
    const rc = c.compress2(compressed.ptr, &compressed_len, data.ptr, @intCast(data.len), c.Z_DEFAULT_COMPRESSION);

    if (rc != c.Z_OK) {
        g_http_allocator.free(compressed);
        ret[0] = c.WasmEdge_ValueGenI32(-3);
        return c.WasmEdge_Result_Success;
    }

    g_zlib_result = compressed[0..compressed_len];
    ret[0] = c.WasmEdge_ValueGenI32(@intCast(compressed_len));
    return c.WasmEdge_Result_Success;
}

/// Decompress deflate/zlib data
fn hostInflate(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const data_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const data_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));

    const data = readWasmString(frame, data_ptr, data_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    if (g_zlib_result) |prev| g_http_allocator.free(prev);
    g_zlib_result = null;

    // Auto-grow buffer for decompression
    var buf_size: usize = data.len * 5;
    if (buf_size < 1024) buf_size = 1024;

    while (buf_size <= 256 * 1024 * 1024) {
        const decompressed = g_http_allocator.alloc(u8, buf_size) catch {
            ret[0] = c.WasmEdge_ValueGenI32(-2);
            return c.WasmEdge_Result_Success;
        };

        var decompressed_len: c.uLongf = @intCast(buf_size);
        const rc = c.uncompress(decompressed.ptr, &decompressed_len, data.ptr, @intCast(data.len));

        if (rc == c.Z_OK) {
            g_zlib_result = decompressed[0..decompressed_len];
            ret[0] = c.WasmEdge_ValueGenI32(@intCast(decompressed_len));
            return c.WasmEdge_Result_Success;
        } else if (rc == c.Z_BUF_ERROR) {
            g_http_allocator.free(decompressed);
            buf_size *= 2;
        } else {
            g_http_allocator.free(decompressed);
            ret[0] = c.WasmEdge_ValueGenI32(-3);
            return c.WasmEdge_Result_Success;
        }
    }

    ret[0] = c.WasmEdge_ValueGenI32(-4); // Buffer too large
    return c.WasmEdge_Result_Success;
}

/// Get zlib result into WASM memory
fn hostZlibGetResult(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const dest_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    const result = g_zlib_result orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    const mem = c.WasmEdge_CallingFrameGetMemoryInstance(frame, 0) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    };

    const dest = c.WasmEdge_MemoryInstanceGetPointer(mem, dest_ptr, @intCast(result.len));
    if (dest == null) {
        ret[0] = c.WasmEdge_ValueGenI32(-3);
        return c.WasmEdge_Result_Success;
    }

    @memcpy(dest[0..result.len], result);
    ret[0] = c.WasmEdge_ValueGenI32(@intCast(result.len));
    return c.WasmEdge_Result_Success;
}

// Zlib dispatch opcodes
const ZLIB_OP_GZIP: u32 = 0;
const ZLIB_OP_GUNZIP: u32 = 1;
const ZLIB_OP_DEFLATE: u32 = 2;
const ZLIB_OP_INFLATE: u32 = 3;
const ZLIB_OP_GET_RESULT: u32 = 4;

/// Zlib dispatch handler
fn hostZlibDispatch(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const opcode: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    switch (opcode) {
        ZLIB_OP_GZIP, ZLIB_OP_GUNZIP, ZLIB_OP_DEFLATE, ZLIB_OP_INFLATE => {
            var sub_args = [_]c.WasmEdge_Value{ args[1], args[2] };
            if (opcode == ZLIB_OP_GZIP) return hostGzip(null, frame, &sub_args, ret);
            if (opcode == ZLIB_OP_GUNZIP) return hostGunzip(null, frame, &sub_args, ret);
            if (opcode == ZLIB_OP_DEFLATE) return hostDeflate(null, frame, &sub_args, ret);
            return hostInflate(null, frame, &sub_args, ret);
        },
        ZLIB_OP_GET_RESULT => {
            var sub_args = [_]c.WasmEdge_Value{args[1]};
            return hostZlibGetResult(null, frame, &sub_args, ret);
        },
        else => {
            ret[0] = c.WasmEdge_ValueGenI32(-100);
            return c.WasmEdge_Result_Success;
        },
    }
}

/// Create the edgebox_zlib host module (single dispatch function)
pub fn createZlibBridge() ?*c.WasmEdge_ModuleInstanceContext {
    initTypes();
    const mn = c.WasmEdge_StringCreateByCString("edgebox_zlib");
    defer c.WasmEdge_StringDelete(mn);
    const m = c.WasmEdge_ModuleInstanceCreate(mn) orelse return null;

    // dispatch(opcode, arg1, arg2) -> i32
    const params = [_]c.WasmEdge_ValType{ g_i32, g_i32, g_i32 };
    const ret_i32 = [_]c.WasmEdge_ValType{g_i32};
    addFunc(m, "zlib_dispatch", &params, &ret_i32, hostZlibDispatch);

    return m;
}

// ============================================================================
// Crypto Bridge (AES encryption)
// ============================================================================

var g_crypto_result: ?[]u8 = null;

/// AES-GCM encrypt
fn hostAesGcmEncrypt(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const key_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const key_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));
    const iv_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[2]));
    const iv_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[3]));
    const data_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[4]));
    const data_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[5]));

    const key = readWasmString(frame, key_ptr, key_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };
    const iv = readWasmString(frame, iv_ptr, iv_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    };
    const data = readWasmString(frame, data_ptr, data_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-3);
        return c.WasmEdge_Result_Success;
    };

    if (g_crypto_result) |prev| g_http_allocator.free(prev);

    // AES-256-GCM encryption
    if (key.len != 32 or iv.len != 12) {
        ret[0] = c.WasmEdge_ValueGenI32(-4); // Invalid key/iv size
        return c.WasmEdge_Result_Success;
    }

    const result = g_http_allocator.alloc(u8, data.len + 16) catch {
        ret[0] = c.WasmEdge_ValueGenI32(-5);
        return c.WasmEdge_Result_Success;
    };

    var tag: [16]u8 = undefined;
    const aes = std.crypto.aead.aes_gcm.Aes256Gcm;
    aes.encrypt(result[0..data.len], &tag, data, "", @as(*const [12]u8, @ptrCast(iv.ptr)).*, @as(*const [32]u8, @ptrCast(key.ptr)).*);

    // Append tag to result
    @memcpy(result[data.len..], &tag);

    g_crypto_result = result;
    ret[0] = c.WasmEdge_ValueGenI32(@intCast(result.len));
    return c.WasmEdge_Result_Success;
}

/// AES-GCM decrypt
fn hostAesGcmDecrypt(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const key_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const key_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));
    const iv_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[2]));
    const iv_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[3]));
    const data_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[4]));
    const data_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[5]));

    const key = readWasmString(frame, key_ptr, key_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };
    const iv = readWasmString(frame, iv_ptr, iv_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    };
    const data = readWasmString(frame, data_ptr, data_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-3);
        return c.WasmEdge_Result_Success;
    };

    if (g_crypto_result) |prev| g_http_allocator.free(prev);

    if (key.len != 32 or iv.len != 12 or data.len < 16) {
        ret[0] = c.WasmEdge_ValueGenI32(-4);
        return c.WasmEdge_Result_Success;
    }

    const ciphertext_len = data.len - 16;
    const result = g_http_allocator.alloc(u8, ciphertext_len) catch {
        ret[0] = c.WasmEdge_ValueGenI32(-5);
        return c.WasmEdge_Result_Success;
    };

    const aes = std.crypto.aead.aes_gcm.Aes256Gcm;
    const tag: *const [16]u8 = @ptrCast(data[ciphertext_len..].ptr);

    aes.decrypt(result, data[0..ciphertext_len], tag.*, "", @as(*const [12]u8, @ptrCast(iv.ptr)).*, @as(*const [32]u8, @ptrCast(key.ptr)).*) catch {
        g_http_allocator.free(result);
        ret[0] = c.WasmEdge_ValueGenI32(-6); // Authentication failed
        return c.WasmEdge_Result_Success;
    };

    g_crypto_result = result;
    ret[0] = c.WasmEdge_ValueGenI32(@intCast(result.len));
    return c.WasmEdge_Result_Success;
}

/// Get crypto result into WASM memory
fn hostCryptoGetResult(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const dest_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    const result = g_crypto_result orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    const mem = c.WasmEdge_CallingFrameGetMemoryInstance(frame, 0) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    };

    const dest = c.WasmEdge_MemoryInstanceGetPointer(mem, dest_ptr, @intCast(result.len));
    if (dest == null) {
        ret[0] = c.WasmEdge_ValueGenI32(-3);
        return c.WasmEdge_Result_Success;
    }

    @memcpy(dest[0..result.len], result);
    ret[0] = c.WasmEdge_ValueGenI32(@intCast(result.len));
    return c.WasmEdge_Result_Success;
}

/// Generate random bytes
fn hostRandomBytes(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const dest_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const size: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));

    const mem = c.WasmEdge_CallingFrameGetMemoryInstance(frame, 0) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    const dest = c.WasmEdge_MemoryInstanceGetPointer(mem, dest_ptr, size);
    if (dest == null) {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    }

    std.crypto.random.bytes(dest[0..size]);
    ret[0] = c.WasmEdge_ValueGenI32(@intCast(size));
    return c.WasmEdge_Result_Success;
}

// Crypto dispatch opcodes
const CRYPTO_OP_AES_GCM_ENCRYPT: u32 = 0;
const CRYPTO_OP_AES_GCM_DECRYPT: u32 = 1;
const CRYPTO_OP_GET_RESULT: u32 = 2;
const CRYPTO_OP_RANDOM_BYTES: u32 = 3;

/// Crypto dispatch handler
fn hostCryptoDispatch(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const opcode: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    switch (opcode) {
        CRYPTO_OP_AES_GCM_ENCRYPT, CRYPTO_OP_AES_GCM_DECRYPT => {
            var sub_args = [_]c.WasmEdge_Value{ args[1], args[2], args[3], args[4], args[5], args[6] };
            if (opcode == CRYPTO_OP_AES_GCM_ENCRYPT) return hostAesGcmEncrypt(null, frame, &sub_args, ret);
            return hostAesGcmDecrypt(null, frame, &sub_args, ret);
        },
        CRYPTO_OP_GET_RESULT => {
            var sub_args = [_]c.WasmEdge_Value{args[1]};
            return hostCryptoGetResult(null, frame, &sub_args, ret);
        },
        CRYPTO_OP_RANDOM_BYTES => {
            var sub_args = [_]c.WasmEdge_Value{ args[1], args[2] };
            return hostRandomBytes(null, frame, &sub_args, ret);
        },
        else => {
            ret[0] = c.WasmEdge_ValueGenI32(-100);
            return c.WasmEdge_Result_Success;
        },
    }
}

/// Create the edgebox_crypto host module (single dispatch function)
pub fn createCryptoBridge() ?*c.WasmEdge_ModuleInstanceContext {
    initTypes();
    const mn = c.WasmEdge_StringCreateByCString("edgebox_crypto");
    defer c.WasmEdge_StringDelete(mn);
    const m = c.WasmEdge_ModuleInstanceCreate(mn) orelse return null;

    // dispatch(opcode, arg1, arg2, arg3, arg4, arg5, arg6) -> i32
    const params = [_]c.WasmEdge_ValType{ g_i32, g_i32, g_i32, g_i32, g_i32, g_i32, g_i32 };
    const ret_i32 = [_]c.WasmEdge_ValType{g_i32};
    addFunc(m, "crypto_dispatch", &params, &ret_i32, hostCryptoDispatch);

    return m;
}

// HTTP dispatch opcodes
const HTTP_OP_REQUEST: u32 = 0;
const HTTP_OP_GET_RESPONSE_LEN: u32 = 1;
const HTTP_OP_GET_RESPONSE: u32 = 2;
const HTTP_OP_START_ASYNC: u32 = 3;
const HTTP_OP_POLL: u32 = 4;
const HTTP_OP_RESPONSE_LEN: u32 = 5;
const HTTP_OP_RESPONSE: u32 = 6;
const HTTP_OP_FREE: u32 = 7;

/// HTTP dispatch handler - single entry point for all HTTP operations
fn hostHttpDispatch(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const opcode: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    switch (opcode) {
        HTTP_OP_REQUEST, HTTP_OP_START_ASYNC => {
            // REQUEST/START_ASYNC: 8 args (url_ptr, url_len, method_ptr, method_len, headers_ptr, headers_len, body_ptr, body_len)
            var sub_args: [8]c.WasmEdge_Value = undefined;
            for (0..8) |i| sub_args[i] = args[i + 1];
            if (opcode == HTTP_OP_REQUEST) {
                return hostHttpRequest(null, frame, &sub_args, ret);
            } else {
                return hostHttpStartAsync(null, frame, &sub_args, ret);
            }
        },
        HTTP_OP_GET_RESPONSE_LEN => {
            return hostHttpGetResponseLen(null, frame, null, ret);
        },
        HTTP_OP_GET_RESPONSE => {
            var sub_args = [_]c.WasmEdge_Value{args[1]};
            return hostHttpGetResponse(null, frame, &sub_args, ret);
        },
        HTTP_OP_POLL, HTTP_OP_RESPONSE_LEN, HTTP_OP_FREE => {
            var sub_args = [_]c.WasmEdge_Value{args[1]};
            if (opcode == HTTP_OP_POLL) return hostHttpPoll(null, frame, &sub_args, ret);
            if (opcode == HTTP_OP_RESPONSE_LEN) return hostHttpAsyncResponseLen(null, frame, &sub_args, ret);
            return hostHttpAsyncFree(null, frame, &sub_args, ret);
        },
        HTTP_OP_RESPONSE => {
            var sub_args = [_]c.WasmEdge_Value{ args[1], args[2] };
            return hostHttpAsyncResponse(null, frame, &sub_args, ret);
        },
        else => {
            ret[0] = c.WasmEdge_ValueGenI32(-100); // Unknown opcode
            return c.WasmEdge_Result_Success;
        },
    }
}

/// Create the edgebox_http host module (single dispatch function)
pub fn createHttpBridge() ?*c.WasmEdge_ModuleInstanceContext {
    initTypes();
    const mn = c.WasmEdge_StringCreateByCString("edgebox_http");
    defer c.WasmEdge_StringDelete(mn);
    const m = c.WasmEdge_ModuleInstanceCreate(mn) orelse return null;

    // Single dispatch function: (opcode, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) -> i32
    const params = [_]c.WasmEdge_ValType{ g_i32, g_i32, g_i32, g_i32, g_i32, g_i32, g_i32, g_i32, g_i32 };
    const ret_i32 = [_]c.WasmEdge_ValType{g_i32};
    addFunc(m, "http_dispatch", &params, &ret_i32, hostHttpDispatch);

    return m;
}

// ============================================================================
// Socket Bridge (Unix domain sockets for sandboxed networking)
// Each WASM instance gets isolated socket namespace via socket files
// ============================================================================

const SocketState = enum { created, bound, listening, connected, closed };

const SocketEntry = struct {
    id: u32,
    state: SocketState,
    socket_path: ?[]u8, // Unix socket path for this "port"
    fd: ?std.posix.socket_t, // Actual socket fd
    pending_connections: std.ArrayListUnmanaged(std.posix.socket_t), // For servers: accepted connections
    read_buffer: std.ArrayListUnmanaged(u8), // Buffered incoming data
    is_server: bool,
    virtual_port: u16, // The "port" the JS code thinks it's using
};

const MAX_SOCKETS = 256;
var g_sockets: [MAX_SOCKETS]?SocketEntry = [_]?SocketEntry{null} ** MAX_SOCKETS;
var g_next_socket_id: u32 = 1;
var g_socket_base_path: ?[]const u8 = null;

fn getSocketBasePath() []const u8 {
    if (g_socket_base_path) |p| return p;
    // Create unique socket directory for this process
    var buf: [128]u8 = undefined;
    const pid = std.c.getpid();
    const path = std.fmt.bufPrint(&buf, "/tmp/edgebox-{d}", .{pid}) catch "/tmp/edgebox-default";
    g_socket_base_path = g_http_allocator.dupe(u8, path) catch path;
    // Create directory
    std.fs.makeDirAbsolute(g_socket_base_path.?) catch {};
    return g_socket_base_path.?;
}

/// Create a new socket - returns socket ID
fn hostSocketCreate(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, _: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    // Find free slot
    var slot: ?usize = null;
    for (g_sockets, 0..) |s, i| {
        if (s == null) {
            slot = i;
            break;
        }
    }

    if (slot == null) {
        ret[0] = c.WasmEdge_ValueGenI32(-1); // No free slots
        return c.WasmEdge_Result_Success;
    }

    const id = g_next_socket_id;
    g_next_socket_id += 1;

    g_sockets[slot.?] = SocketEntry{
        .id = id,
        .state = .created,
        .socket_path = null,
        .fd = null,
        .pending_connections = .{},
        .read_buffer = .{},
        .is_server = false,
        .virtual_port = 0,
    };

    ret[0] = c.WasmEdge_ValueGenI32(@intCast(id));
    return c.WasmEdge_Result_Success;
}

fn findSocket(id: u32) ?*SocketEntry {
    for (&g_sockets) |*s| {
        if (s.*) |*entry| {
            if (entry.id == id) return entry;
        }
    }
    return null;
}

/// Bind socket to a virtual port (creates Unix socket file)
fn hostSocketBind(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const socket_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const port: u16 = @intCast(c.WasmEdge_ValueGetI32(args[1]) & 0xFFFF);

    const entry = findSocket(socket_id) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    // Create socket path
    const base = getSocketBasePath();
    var path_buf: [256]u8 = undefined;
    const path = std.fmt.bufPrint(&path_buf, "{s}/sock-{d}", .{ base, port }) catch {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    };

    // Remove existing socket file if any
    std.fs.deleteFileAbsolute(path) catch {};

    // Create Unix socket
    const fd = std.posix.socket(std.posix.AF.UNIX, std.posix.SOCK.STREAM, 0) catch {
        ret[0] = c.WasmEdge_ValueGenI32(-3);
        return c.WasmEdge_Result_Success;
    };

    // Bind to path
    var addr: std.posix.sockaddr.un = .{ .family = std.posix.AF.UNIX, .path = undefined };
    @memset(&addr.path, 0);
    @memcpy(addr.path[0..path.len], path);

    std.posix.bind(fd, @ptrCast(&addr), @sizeOf(std.posix.sockaddr.un)) catch {
        std.posix.close(fd);
        ret[0] = c.WasmEdge_ValueGenI32(-4);
        return c.WasmEdge_Result_Success;
    };

    entry.socket_path = g_http_allocator.dupe(u8, path) catch null;
    entry.fd = fd;
    entry.virtual_port = port;
    entry.state = .bound;
    entry.is_server = true;

    ret[0] = c.WasmEdge_ValueGenI32(0);
    return c.WasmEdge_Result_Success;
}

/// Listen on bound socket
fn hostSocketListen(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const socket_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const backlog: u31 = @intCast(c.WasmEdge_ValueGetI32(args[1]) & 0x7FFFFFFF);

    const entry = findSocket(socket_id) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    if (entry.fd == null or entry.state != .bound) {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    }

    std.posix.listen(entry.fd.?, backlog) catch {
        ret[0] = c.WasmEdge_ValueGenI32(-3);
        return c.WasmEdge_Result_Success;
    };

    // Set non-blocking for polling (O_NONBLOCK = 0x0004 on macOS, 0x800 on Linux)
    const O_NONBLOCK: usize = if (@import("builtin").os.tag == .macos) 0x0004 else 0x800;
    const flags = std.posix.fcntl(entry.fd.?, std.posix.F.GETFL, 0) catch 0;
    _ = std.posix.fcntl(entry.fd.?, std.posix.F.SETFL, flags | O_NONBLOCK) catch {};

    entry.state = .listening;
    ret[0] = c.WasmEdge_ValueGenI32(0);
    return c.WasmEdge_Result_Success;
}

/// Accept connection on listening socket - returns new socket ID or 0 if none pending
fn hostSocketAccept(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const socket_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    const entry = findSocket(socket_id) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    if (entry.fd == null or entry.state != .listening) {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    }

    // Try non-blocking accept
    const client_fd = std.posix.accept(entry.fd.?, null, null, std.posix.SOCK.NONBLOCK) catch |err| {
        if (err == error.WouldBlock) {
            ret[0] = c.WasmEdge_ValueGenI32(0); // No pending connection
            return c.WasmEdge_Result_Success;
        }
        ret[0] = c.WasmEdge_ValueGenI32(-3);
        return c.WasmEdge_Result_Success;
    };

    // Create new socket entry for the accepted connection
    var slot: ?usize = null;
    for (g_sockets, 0..) |s, i| {
        if (s == null) {
            slot = i;
            break;
        }
    }

    if (slot == null) {
        std.posix.close(client_fd);
        ret[0] = c.WasmEdge_ValueGenI32(-4);
        return c.WasmEdge_Result_Success;
    }

    const new_id = g_next_socket_id;
    g_next_socket_id += 1;

    g_sockets[slot.?] = SocketEntry{
        .id = new_id,
        .state = .connected,
        .socket_path = null,
        .fd = client_fd,
        .pending_connections = .{},
        .read_buffer = .{},
        .is_server = false,
        .virtual_port = 0,
    };

    ret[0] = c.WasmEdge_ValueGenI32(@intCast(new_id));
    return c.WasmEdge_Result_Success;
}

/// Connect to a virtual port (via Unix socket)
fn hostSocketConnect(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const socket_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const port: u16 = @intCast(c.WasmEdge_ValueGetI32(args[1]) & 0xFFFF);

    const entry = findSocket(socket_id) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    // Create socket path to connect to
    const base = getSocketBasePath();
    var path_buf: [256]u8 = undefined;
    const path = std.fmt.bufPrint(&path_buf, "{s}/sock-{d}", .{ base, port }) catch {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    };

    // Create client socket
    const fd = std.posix.socket(std.posix.AF.UNIX, std.posix.SOCK.STREAM | std.posix.SOCK.NONBLOCK, 0) catch {
        ret[0] = c.WasmEdge_ValueGenI32(-3);
        return c.WasmEdge_Result_Success;
    };

    // Connect to server socket
    var addr: std.posix.sockaddr.un = .{ .family = std.posix.AF.UNIX, .path = undefined };
    @memset(&addr.path, 0);
    @memcpy(addr.path[0..path.len], path);

    std.posix.connect(fd, @ptrCast(&addr), @sizeOf(std.posix.sockaddr.un)) catch |err| {
        if (err != error.WouldBlock and err != error.InProgress) {
            std.posix.close(fd);
            ret[0] = c.WasmEdge_ValueGenI32(-4);
            return c.WasmEdge_Result_Success;
        }
    };

    entry.fd = fd;
    entry.virtual_port = port;
    entry.state = .connected;

    ret[0] = c.WasmEdge_ValueGenI32(0);
    return c.WasmEdge_Result_Success;
}

/// Write data to socket - returns bytes written or -1 on error
fn hostSocketWrite(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const socket_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const data_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));
    const data_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[2]));

    const entry = findSocket(socket_id) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    if (entry.fd == null or entry.state != .connected) {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    }

    const data = readWasmString(frame, data_ptr, data_len) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-3);
        return c.WasmEdge_Result_Success;
    };

    const written = std.posix.write(entry.fd.?, data) catch |err| {
        if (err == error.WouldBlock) {
            ret[0] = c.WasmEdge_ValueGenI32(0);
            return c.WasmEdge_Result_Success;
        }
        ret[0] = c.WasmEdge_ValueGenI32(-4);
        return c.WasmEdge_Result_Success;
    };

    ret[0] = c.WasmEdge_ValueGenI32(@intCast(written));
    return c.WasmEdge_Result_Success;
}

/// Read data from socket - returns bytes available (call get_read_data to retrieve)
fn hostSocketRead(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const socket_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const max_len: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));

    const entry = findSocket(socket_id) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    if (entry.fd == null or entry.state != .connected) {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    }

    // Clear previous buffer
    entry.read_buffer.clearRetainingCapacity();

    var buf: [8192]u8 = undefined;
    const to_read = @min(max_len, buf.len);

    const n = std.posix.read(entry.fd.?, buf[0..to_read]) catch |err| {
        if (err == error.WouldBlock) {
            ret[0] = c.WasmEdge_ValueGenI32(0);
            return c.WasmEdge_Result_Success;
        }
        ret[0] = c.WasmEdge_ValueGenI32(-3);
        return c.WasmEdge_Result_Success;
    };

    if (n == 0) {
        // EOF - connection closed
        entry.state = .closed;
        ret[0] = c.WasmEdge_ValueGenI32(-4);
        return c.WasmEdge_Result_Success;
    }

    entry.read_buffer.appendSlice(g_http_allocator, buf[0..n]) catch {
        ret[0] = c.WasmEdge_ValueGenI32(-5);
        return c.WasmEdge_Result_Success;
    };

    ret[0] = c.WasmEdge_ValueGenI32(@intCast(n));
    return c.WasmEdge_Result_Success;
}

/// Get read data into WASM memory
fn hostSocketGetReadData(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const socket_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));
    const dest_ptr: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[1]));

    const entry = findSocket(socket_id) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    const data = entry.read_buffer.items;
    if (data.len == 0) {
        ret[0] = c.WasmEdge_ValueGenI32(0);
        return c.WasmEdge_Result_Success;
    }

    const mem = c.WasmEdge_CallingFrameGetMemoryInstance(frame, 0) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-2);
        return c.WasmEdge_Result_Success;
    };

    const dest = c.WasmEdge_MemoryInstanceGetPointer(mem, dest_ptr, @intCast(data.len));
    if (dest == null) {
        ret[0] = c.WasmEdge_ValueGenI32(-3);
        return c.WasmEdge_Result_Success;
    }

    @memcpy(dest[0..data.len], data);
    ret[0] = c.WasmEdge_ValueGenI32(@intCast(data.len));
    return c.WasmEdge_Result_Success;
}

/// Close socket
fn hostSocketClose(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const socket_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    for (&g_sockets, 0..) |*s, i| {
        if (s.*) |*entry| {
            if (entry.id == socket_id) {
                if (entry.fd) |fd| std.posix.close(fd);
                if (entry.socket_path) |path| {
                    std.fs.deleteFileAbsolute(path) catch {};
                    g_http_allocator.free(path);
                }
                entry.pending_connections.deinit(g_http_allocator);
                entry.read_buffer.deinit(g_http_allocator);
                g_sockets[i] = null;
                ret[0] = c.WasmEdge_ValueGenI32(0);
                return c.WasmEdge_Result_Success;
            }
        }
    }

    ret[0] = c.WasmEdge_ValueGenI32(-1);
    return c.WasmEdge_Result_Success;
}

/// Get socket state: 0=created, 1=bound, 2=listening, 3=connected, 4=closed, -1=not found
fn hostSocketState(_: ?*anyopaque, _: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const socket_id: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    const entry = findSocket(socket_id) orelse {
        ret[0] = c.WasmEdge_ValueGenI32(-1);
        return c.WasmEdge_Result_Success;
    };

    ret[0] = c.WasmEdge_ValueGenI32(@intFromEnum(entry.state));
    return c.WasmEdge_Result_Success;
}

// Socket dispatch opcodes
const SOCKET_OP_CREATE: u32 = 0;
const SOCKET_OP_BIND: u32 = 1;
const SOCKET_OP_LISTEN: u32 = 2;
const SOCKET_OP_ACCEPT: u32 = 3;
const SOCKET_OP_CONNECT: u32 = 4;
const SOCKET_OP_WRITE: u32 = 5;
const SOCKET_OP_READ: u32 = 6;
const SOCKET_OP_GET_READ_DATA: u32 = 7;
const SOCKET_OP_CLOSE: u32 = 8;
const SOCKET_OP_STATE: u32 = 9;

/// Socket dispatch handler
fn hostSocketDispatch(_: ?*anyopaque, frame: ?*const c.WasmEdge_CallingFrameContext, args: [*c]const c.WasmEdge_Value, ret: [*c]c.WasmEdge_Value) callconv(.c) c.WasmEdge_Result {
    const opcode: u32 = @bitCast(c.WasmEdge_ValueGetI32(args[0]));

    switch (opcode) {
        SOCKET_OP_CREATE => {
            return hostSocketCreate(null, frame, null, ret);
        },
        SOCKET_OP_BIND, SOCKET_OP_LISTEN, SOCKET_OP_CONNECT, SOCKET_OP_READ, SOCKET_OP_GET_READ_DATA => {
            var sub_args = [_]c.WasmEdge_Value{ args[1], args[2] };
            if (opcode == SOCKET_OP_BIND) return hostSocketBind(null, frame, &sub_args, ret);
            if (opcode == SOCKET_OP_LISTEN) return hostSocketListen(null, frame, &sub_args, ret);
            if (opcode == SOCKET_OP_CONNECT) return hostSocketConnect(null, frame, &sub_args, ret);
            if (opcode == SOCKET_OP_READ) return hostSocketRead(null, frame, &sub_args, ret);
            return hostSocketGetReadData(null, frame, &sub_args, ret);
        },
        SOCKET_OP_ACCEPT, SOCKET_OP_CLOSE, SOCKET_OP_STATE => {
            var sub_args = [_]c.WasmEdge_Value{args[1]};
            if (opcode == SOCKET_OP_ACCEPT) return hostSocketAccept(null, frame, &sub_args, ret);
            if (opcode == SOCKET_OP_CLOSE) return hostSocketClose(null, frame, &sub_args, ret);
            return hostSocketState(null, frame, &sub_args, ret);
        },
        SOCKET_OP_WRITE => {
            var sub_args = [_]c.WasmEdge_Value{ args[1], args[2], args[3] };
            return hostSocketWrite(null, frame, &sub_args, ret);
        },
        else => {
            ret[0] = c.WasmEdge_ValueGenI32(-100);
            return c.WasmEdge_Result_Success;
        },
    }
}

/// Create the edgebox_socket host module (single dispatch function)
pub fn createSocketBridge() ?*c.WasmEdge_ModuleInstanceContext {
    initTypes();
    const mn = c.WasmEdge_StringCreateByCString("edgebox_socket");
    defer c.WasmEdge_StringDelete(mn);
    const m = c.WasmEdge_ModuleInstanceCreate(mn) orelse return null;

    // dispatch(opcode, arg1, arg2, arg3) -> i32
    const params = [_]c.WasmEdge_ValType{ g_i32, g_i32, g_i32, g_i32 };
    const ret_i32 = [_]c.WasmEdge_ValType{g_i32};
    addFunc(m, "socket_dispatch", &params, &ret_i32, hostSocketDispatch);

    return m;
}

inline fn addFunc(m: ?*c.WasmEdge_ModuleInstanceContext, name: [*:0]const u8, p: []const c.WasmEdge_ValType, r: []const c.WasmEdge_ValType, f: c.WasmEdge_HostFunc_t) void {
    const ft = c.WasmEdge_FunctionTypeCreate(p.ptr, @intCast(p.len), r.ptr, @intCast(r.len));
    const fi = c.WasmEdge_FunctionInstanceCreate(ft, f, null, 0);
    const fn_name = c.WasmEdge_StringCreateByCString(name);
    c.WasmEdge_ModuleInstanceAddFunction(m, fn_name, fi);
    c.WasmEdge_StringDelete(fn_name);
    c.WasmEdge_FunctionTypeDelete(ft);
}

// Pre-create commonly used types to avoid repeated allocations
var g_i32: c.WasmEdge_ValType = undefined;
var g_types_init = false;

pub fn initTypes() void {
    if (!g_types_init) {
        g_i32 = c.WasmEdge_ValTypeGenI32();
        g_types_init = true;
    }
}

pub fn createProcessStub() ?*c.WasmEdge_ModuleInstanceContext {
    initTypes();
    const mn = c.WasmEdge_StringCreateByCString("wasmedge_process");
    defer c.WasmEdge_StringDelete(mn);
    const m = c.WasmEdge_ModuleInstanceCreate(mn) orelse return null;
    const ii = [_]c.WasmEdge_ValType{ g_i32, g_i32 };
    const iiii = [_]c.WasmEdge_ValType{ g_i32, g_i32, g_i32, g_i32 };
    const ri = [_]c.WasmEdge_ValType{g_i32};
    addFunc(m, "wasmedge_process_set_prog_name", &ii, &.{}, stubVoid);
    addFunc(m, "wasmedge_process_add_arg", &ii, &.{}, stubVoid);
    addFunc(m, "wasmedge_process_add_stdin", &ii, &.{}, stubVoid);
    addFunc(m, "wasmedge_process_add_env", &iiii, &.{}, stubVoid);
    addFunc(m, "wasmedge_process_set_timeout", &ri, &.{}, stubVoid);
    addFunc(m, "wasmedge_process_get_stdout", &ri, &.{}, stubVoid);
    addFunc(m, "wasmedge_process_get_stderr", &ri, &.{}, stubVoid);
    addFunc(m, "wasmedge_process_run", &.{}, &ri, stubZero);
    addFunc(m, "wasmedge_process_get_exit_code", &.{}, &ri, stubZero);
    addFunc(m, "wasmedge_process_get_stdout_len", &.{}, &ri, stubZero);
    addFunc(m, "wasmedge_process_get_stderr_len", &.{}, &ri, stubZero);
    return m;
}

const TIMING = true; // Set to true for timing debug

fn timer() i64 {
    return std.time.microTimestamp();
}

fn printTiming(label: []const u8, start: i64) i64 {
    if (TIMING) {
        const now = timer();
        std.debug.print("{s}: {d}us\n", .{ label, now - start });
        return now;
    }
    return start;
}

// Prefetch file into page cache using background thread
fn prefetchFileWorker(path_ptr: [*:0]const u8) void {
    const path_str = std.mem.span(path_ptr);
    const file = std.fs.cwd().openFile(path_str, .{}) catch return;
    defer file.close();

    // Get file size and mmap it
    const stat = file.stat() catch return;
    const size = stat.size;
    if (size == 0) return;

    // mmap the file
    const ptr = std.posix.mmap(
        null,
        size,
        std.posix.PROT.READ,
        .{ .TYPE = .PRIVATE },
        file.handle,
        0,
    ) catch return;
    defer std.posix.munmap(ptr);

    // Tell kernel to prefetch all pages (MADV_WILLNEED = 3)
    std.posix.madvise(ptr.ptr, size, 3) catch {};

    // Touch first and last page to ensure they're loaded
    const bytes: [*]volatile const u8 = @ptrCast(ptr.ptr);
    _ = bytes[0];
    if (size > 4096) _ = bytes[size - 1];
}

pub fn main() !void {
    const start_time = timer();

    // Initialize global allocator for HTTP bridge
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    g_http_allocator = gpa.allocator();

    _ = printTiming("init", start_time);

    var t = timer();
    var args_iter = std.process.args();
    _ = args_iter.next();
    const path = args_iter.next() orelse {
        std.debug.print("Usage: edgebox <file.wasm|dylib>\n", .{});
        return;
    };

    // Start prefetching the file immediately in background thread
    // This loads the file into page cache while we do other setup
    var prefetch_thread: ?std.Thread = null;
    const path_str = std.mem.span(path.ptr);
    const is_dylib_early = std.mem.endsWith(u8, path_str, ".dylib") or std.mem.endsWith(u8, path_str, ".so");
    if (is_dylib_early) {
        prefetch_thread = std.Thread.spawn(.{}, prefetchFileWorker, .{path.ptr}) catch null;
    }
    defer if (prefetch_thread) |pt| pt.join();

    t = printTiming("args", t);

    // Load environment variables from .env file (optional)
    loadDotEnv(g_http_allocator);

    // Load config from .edgebox.json (HTTP permissions and allowed dirs)
    loadEdgeboxConfig(g_http_allocator);

    var wasi_args: [64][*c]const u8 = undefined;
    var argc: usize = 0;
    wasi_args[argc] = path.ptr;
    argc += 1;
    while (args_iter.next()) |a| {
        if (argc < 64) {
            wasi_args[argc] = a.ptr;
            argc += 1;
        }
    }

    c.WasmEdge_LogSetErrorLevel();
    const conf = c.WasmEdge_ConfigureCreate();
    defer c.WasmEdge_ConfigureDelete(conf);

    // Remove all unused proposals for faster cold start
    inline for (.{
        c.WasmEdge_Proposal_Threads,
        c.WasmEdge_Proposal_TailCall,
        c.WasmEdge_Proposal_ExceptionHandling,
        c.WasmEdge_Proposal_Memory64,
        c.WasmEdge_Proposal_ExtendedConst,
        c.WasmEdge_Proposal_Component,
        c.WasmEdge_Proposal_FunctionReferences,
        c.WasmEdge_Proposal_GC,
        c.WasmEdge_Proposal_MultiMemories,
        c.WasmEdge_Proposal_RelaxSIMD,
        c.WasmEdge_Proposal_Annotations,
    }) |p| c.WasmEdge_ConfigureRemoveProposal(conf, p);
    t = printTiming("config", t);

    const loader = c.WasmEdge_LoaderCreate(conf) orelse return error.LoaderFailed;
    defer c.WasmEdge_LoaderDelete(loader);
    t = printTiming("loader", t);

    var ast: ?*c.WasmEdge_ASTModuleContext = null;
    var res: c.WasmEdge_Result = undefined;
    var mapped: ?[]align(std.heap.page_size_min) u8 = null;

    // path_str already defined above for prefetch
    const is_dylib = std.mem.endsWith(u8, path_str, ".dylib") or std.mem.endsWith(u8, path_str, ".so");
    const is_js = std.mem.endsWith(u8, path_str, ".js") or std.mem.endsWith(u8, path_str, ".cjs") or std.mem.endsWith(u8, path_str, ".mjs");

    // For .js files, use the edgebox-base.wasm module and pass JS file as argument
    var wasm_path_buf: [4096]u8 = undefined;
    var actual_path: [*c]const u8 = path.ptr;
    if (is_js) {
        // Find edgebox-base.wasm in the same directory as this executable
        const exe_path = std.fs.selfExePath(&wasm_path_buf) catch {
            // Fallback to looking in current directory
            @memcpy(wasm_path_buf[0..18], "edgebox-base.wasm\x00");
            actual_path = @ptrCast(&wasm_path_buf);
            return; // Will fail with proper error
        };
        // Find directory of executable
        var dir_end: usize = 0;
        for (exe_path, 0..) |byte, i| {
            if (byte == '/') dir_end = i;
        }
        if (dir_end > 0) {
            @memcpy(wasm_path_buf[0..dir_end], exe_path[0..dir_end]);
            @memcpy(wasm_path_buf[dir_end .. dir_end + 19], "/edgebox-base.wasm\x00");
            actual_path = @ptrCast(&wasm_path_buf);
        } else {
            @memcpy(wasm_path_buf[0..18], "edgebox-base.wasm\x00");
            actual_path = @ptrCast(&wasm_path_buf);
        }
        // Update wasi_args to include the JS file as first real argument
        // wasi_args[0] is the wasm module path, wasi_args[1..] are the JS args
        // We need to shift: wasm_module, js_file, original_args...
        if (argc < 63) {
            // Shift existing args right
            var i: usize = argc;
            while (i > 0) : (i -= 1) {
                wasi_args[i] = wasi_args[i - 1];
            }
            wasi_args[0] = path.ptr; // JS file as first arg
            argc += 1;
        }
    }

    // mmap only works for .wasm files, not AOT .dylib/.so
    if (!is_dylib and !is_js) {
        const file = std.fs.cwd().openFile(path_str, .{}) catch null;
        if (file) |f| {
            defer f.close();
            const size = f.getEndPos() catch 0;
            if (size > 0) {
                mapped = std.posix.mmap(null, size, std.posix.PROT.READ, .{ .TYPE = .PRIVATE }, f.handle, 0) catch null;
                if (mapped) |m| {
                    res = c.WasmEdge_LoaderParseFromBuffer(loader, &ast, m.ptr, @intCast(m.len));
                }
            }
        }
    }

    // Fallback to file-based loading (required for dylib/so or js files)
    if (ast == null) {
        if (mapped) |m| std.posix.munmap(m);
        mapped = null;
        res = c.WasmEdge_LoaderParseFromFile(loader, &ast, actual_path);
    }
    defer if (mapped) |m| std.posix.munmap(m);
    t = printTiming("parse", t);

    if (!c.WasmEdge_ResultOK(res) or ast == null) return error.ParseFailed;
    defer c.WasmEdge_ASTModuleDelete(ast);

    const validator = c.WasmEdge_ValidatorCreate(conf) orelse return error.ValidatorFailed;
    defer c.WasmEdge_ValidatorDelete(validator);
    res = c.WasmEdge_ValidatorValidate(validator, ast);
    if (!c.WasmEdge_ResultOK(res)) return error.ValidationFailed;
    t = printTiming("validate", t);

    const executor = c.WasmEdge_ExecutorCreate(conf, null) orelse return error.ExecutorFailed;
    defer c.WasmEdge_ExecutorDelete(executor);
    const store = c.WasmEdge_StoreCreate() orelse return error.StoreFailed;
    defer c.WasmEdge_StoreDelete(store);
    t = printTiming("executor", t);

    // Preopened directories for WASI - use dirs from .edgebox.json config
    // Format is "guest_path:host_path"
    var preopens: [32][*c]const u8 = undefined;
    var preopen_bufs: [32][512]u8 = undefined;
    var preopen_count: usize = 0;

    // Always preopen current directory
    preopens[preopen_count] = ".:.";
    preopen_count += 1;

    // Always preopen /tmp and HOME first (these are most commonly needed)
    preopens[preopen_count] = "/tmp:/tmp";
    preopen_count += 1;

    // Preopen home directory from environment
    if (std.posix.getenv("HOME")) |home| {
        const formatted = std.fmt.bufPrintZ(&preopen_bufs[preopen_count], "{s}:{s}", .{ home, home }) catch null;
        if (formatted) |f| {
            preopens[preopen_count] = f.ptr;
            preopen_count += 1;
        }
    }

    // If we have dirs from config, add those too
    if (g_allowed_dirs) |dirs| {
        for (dirs) |dir| {
            if (preopen_count >= 30) break;
            const formatted = std.fmt.bufPrintZ(&preopen_bufs[preopen_count], "{s}:{s}", .{ dir, dir }) catch continue;
            preopens[preopen_count] = formatted.ptr;
            preopen_count += 1;
        }
    }

    // Always try to preopen root for full access (may fail on some systems)
    preopens[preopen_count] = "/:/";
    preopen_count += 1;

    // Pass through important environment variables to WASI
    // Use static buffers to keep strings alive for the WASI call
    var env_vars: [20][*c]const u8 = undefined;
    var env_bufs: [20][1024]u8 = undefined;
    var env_count: usize = 0;
    const important_vars = [_][]const u8{ "HOME", "PWD", "USER", "PATH", "TMPDIR", "ANTHROPIC_API_KEY", "TERM", "SHELL", "HOSTNAME", "EDGEBOX_DEBUG" };
    for (important_vars) |name| {
        if (std.posix.getenv(name)) |val| {
            // Format: "NAME=VALUE"
            if (env_count < 18) {
                const formatted = std.fmt.bufPrintZ(&env_bufs[env_count], "{s}={s}", .{ name, val }) catch continue;
                env_vars[env_count] = formatted.ptr;
                env_count += 1;
            }
        }
    }

    // Add __EDGEBOX_DIRS for OS-level sandbox (bwrap/sandbox-exec/job objects)
    if (g_allowed_dirs) |dirs| {
        if (env_count < 19 and dirs.len > 0) {
            // Build JSON array: ["/tmp", "/home/user/.claude"]
            var json_buf: [4096]u8 = undefined;
            var json_offset: usize = 0;
            json_buf[json_offset] = '[';
            json_offset += 1;
            for (dirs, 0..) |dir, i| {
                if (i > 0) {
                    json_buf[json_offset] = ',';
                    json_offset += 1;
                }
                json_buf[json_offset] = '"';
                json_offset += 1;
                for (dir) |ch| {
                    if (json_offset >= json_buf.len - 10) break;
                    json_buf[json_offset] = ch;
                    json_offset += 1;
                }
                json_buf[json_offset] = '"';
                json_offset += 1;
            }
            json_buf[json_offset] = ']';
            json_offset += 1;
            json_buf[json_offset] = 0;

            const formatted = std.fmt.bufPrintZ(&env_bufs[env_count], "__EDGEBOX_DIRS={s}", .{json_buf[0..json_offset]}) catch null;
            if (formatted) |f| {
                env_vars[env_count] = f.ptr;
                env_count += 1;
            }
        }
    }

    const wasi = c.WasmEdge_ModuleInstanceCreateWASI(&wasi_args, @intCast(argc), if (env_count > 0) &env_vars else null, @intCast(env_count), &preopens, @intCast(preopen_count)) orelse return error.WasiFailed;
    defer c.WasmEdge_ModuleInstanceDelete(wasi);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, wasi);
    t = printTiming("wasi", t);

    const proc = createProcessStub() orelse return error.ProcessFailed;
    defer c.WasmEdge_ModuleInstanceDelete(proc);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, proc);
    t = printTiming("proc", t);

    // Register HTTP bridge for network requests
    const http = createHttpBridge() orelse return error.HttpBridgeFailed;
    defer c.WasmEdge_ModuleInstanceDelete(http);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, http);
    t = printTiming("http", t);

    // Register Spawn bridge for async child processes
    const spawn_bridge = createSpawnBridge() orelse return error.SpawnBridgeFailed;
    defer c.WasmEdge_ModuleInstanceDelete(spawn_bridge);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, spawn_bridge);
    t = printTiming("spawn", t);

    // Register File bridge for async file I/O
    const file_bridge = createFileBridge() orelse return error.FileBridgeFailed;
    defer c.WasmEdge_ModuleInstanceDelete(file_bridge);
    std.debug.print("[run] Registering file_bridge module\n", .{});
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, file_bridge);
    t = printTiming("file", t);

    // Register Zlib bridge for compression
    const zlib_bridge = createZlibBridge() orelse return error.ZlibBridgeFailed;
    defer c.WasmEdge_ModuleInstanceDelete(zlib_bridge);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, zlib_bridge);
    t = printTiming("zlib", t);

    // Register Crypto bridge for AES encryption
    const crypto_bridge = createCryptoBridge() orelse return error.CryptoBridgeFailed;
    defer c.WasmEdge_ModuleInstanceDelete(crypto_bridge);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, crypto_bridge);
    t = printTiming("crypto", t);

    // Register Socket bridge for sandboxed networking
    const socket_bridge = createSocketBridge() orelse return error.SocketBridgeFailed;
    defer c.WasmEdge_ModuleInstanceDelete(socket_bridge);
    _ = c.WasmEdge_ExecutorRegisterImport(executor, store, socket_bridge);
    t = printTiming("socket", t);

    var mod: ?*c.WasmEdge_ModuleInstanceContext = null;
    res = c.WasmEdge_ExecutorInstantiate(executor, &mod, store, ast);
    if (!c.WasmEdge_ResultOK(res)) return error.InstantiateFailed;
    defer c.WasmEdge_ModuleInstanceDelete(mod);
    t = printTiming("instantiate", t);

    const fn_name = c.WasmEdge_StringCreateByCString("_start");
    defer c.WasmEdge_StringDelete(fn_name);
    const func = c.WasmEdge_ModuleInstanceFindFunction(mod, fn_name) orelse return error.FuncNotFound;
    t = printTiming("findfunc", t);
    res = c.WasmEdge_ExecutorInvoke(executor, func, null, 0, null, 0);
    _ = printTiming("exec", t);
    if (!c.WasmEdge_ResultOK(res)) {
        const msg = c.WasmEdge_ResultGetMessage(res);
        if (msg != null and std.mem.indexOf(u8, std.mem.span(msg), "terminated") == null) {
            return error.ExecFailed;
        }
    }
}
