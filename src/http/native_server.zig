//! Native HTTP Server with kqueue/epoll Event Loop
//! Calls WASM handler with single crossing per request
//! Supports HTTP keep-alive connections

const std = @import("std");
const builtin = @import("builtin");
const posix = std.posix;

const parser = @import("parser.zig");
const event_loop = @import("event_loop.zig");
const EventLoop = event_loop.EventLoop;

const c = @cImport({
    @cInclude("wasm_export.h");
});

pub const MAX_CONNECTIONS = 1024;
pub const READ_BUFFER_SIZE = 8192;
pub const WRITE_BUFFER_SIZE = 65536;
pub const KEEPALIVE_TIMEOUT_MS: i64 = 5000;

/// Connection state machine
pub const ConnectionState = enum {
    reading,
    processing,
    writing,
    closing,
};

/// Individual connection state
pub const Connection = struct {
    fd: posix.fd_t,
    state: ConnectionState,
    read_buf: [READ_BUFFER_SIZE]u8,
    read_len: usize,
    write_buf: [WRITE_BUFFER_SIZE]u8,
    write_len: usize,
    write_pos: usize,
    keep_alive: bool,
    last_active: i64,

    pub fn init(fd: posix.fd_t) Connection {
        return Connection{
            .fd = fd,
            .state = .reading,
            .read_buf = undefined,
            .read_len = 0,
            .write_buf = undefined,
            .write_len = 0,
            .write_pos = 0,
            .keep_alive = true,
            .last_active = std.time.milliTimestamp(),
        };
    }

    pub fn reset(self: *Connection) void {
        self.state = .reading;
        self.read_len = 0;
        self.write_len = 0;
        self.write_pos = 0;
        self.last_active = std.time.milliTimestamp();
    }
};

/// Handler function type for processing requests
/// Takes JSON request, returns JSON response
pub const HandlerFn = *const fn (request_json: []const u8, response_buf: []u8) usize;

/// Pre-allocated buffer sizes for WASM memory
const WASM_REQ_BUF_SIZE: u32 = READ_BUFFER_SIZE * 2; // 16KB for request JSON
const WASM_RESP_BUF_SIZE: u32 = WRITE_BUFFER_SIZE; // 64KB for response JSON

/// WASM handler context with pre-allocated buffers
pub const WasmHandler = struct {
    exec_env: c.wasm_exec_env_t,
    handler_func: c.wasm_function_inst_t,
    module_inst: c.wasm_module_inst_t,

    // Pre-allocated WASM memory buffers (allocated once, reused per request)
    wasm_req_ptr: u32,
    wasm_resp_ptr: u32,
    req_native_ptr: [*]u8,
    resp_native_ptr: [*]u8,

    /// Initialize with pre-allocated buffers
    pub fn init(exec_env: c.wasm_exec_env_t, handler_func: c.wasm_function_inst_t) !WasmHandler {
        const module_inst = c.wasm_runtime_get_module_inst(exec_env);

        // Pre-allocate request buffer
        var req_native: ?*anyopaque = null;
        const wasm_req = c.wasm_runtime_module_malloc(module_inst, WASM_REQ_BUF_SIZE, &req_native);
        if (wasm_req == 0) return error.WasmMallocFailed;

        // Pre-allocate response buffer
        var resp_native: ?*anyopaque = null;
        const wasm_resp = c.wasm_runtime_module_malloc(module_inst, WASM_RESP_BUF_SIZE, &resp_native);
        if (wasm_resp == 0) {
            c.wasm_runtime_module_free(module_inst, wasm_req);
            return error.WasmMallocFailed;
        }

        return WasmHandler{
            .exec_env = exec_env,
            .handler_func = handler_func,
            .module_inst = module_inst,
            .wasm_req_ptr = @truncate(wasm_req),
            .wasm_resp_ptr = @truncate(wasm_resp),
            .req_native_ptr = @ptrCast(req_native),
            .resp_native_ptr = @ptrCast(resp_native),
        };
    }

    /// Free pre-allocated buffers
    pub fn deinit(self: *WasmHandler) void {
        c.wasm_runtime_module_free(self.module_inst, self.wasm_req_ptr);
        c.wasm_runtime_module_free(self.module_inst, self.wasm_resp_ptr);
    }

    /// Call WASM handler with request JSON, get response JSON (zero-alloc per request)
    pub fn call(self: *WasmHandler, request_json: []const u8, response_buf: []u8) !usize {
        // Check request fits in pre-allocated buffer
        if (request_json.len > WASM_REQ_BUF_SIZE) return error.RequestTooLarge;

        // Copy request to pre-allocated WASM memory (single copy)
        @memcpy(self.req_native_ptr[0..request_json.len], request_json);

        // Call handler with pre-allocated buffers
        var args = [4]u32{
            self.wasm_req_ptr,
            @intCast(request_json.len),
            self.wasm_resp_ptr,
            @intCast(@min(response_buf.len, WASM_RESP_BUF_SIZE)),
        };

        if (!c.wasm_runtime_call_wasm(self.exec_env, self.handler_func, 4, &args)) {
            return error.WasmCallFailed;
        }

        // Get response length from return value
        const resp_len_signed: i32 = @bitCast(args[0]);
        if (resp_len_signed < 0) return error.WasmCallFailed;

        const resp_len: usize = @intCast(args[0]);
        if (resp_len > response_buf.len) return error.ResponseTooLarge;

        // Copy response from pre-allocated WASM memory (single copy)
        @memcpy(response_buf[0..resp_len], self.resp_native_ptr[0..resp_len]);

        return resp_len;
    }
};

/// Native HTTP Server
pub const NativeHttpServer = struct {
    allocator: std.mem.Allocator,
    loop: EventLoop,
    listen_fd: posix.fd_t,
    running: std.atomic.Value(bool),

    // Connection management (heap-allocated to avoid stack overflow)
    connections: *[MAX_CONNECTIONS]?Connection,
    connection_count: usize,

    // Handler (either native or WASM)
    native_handler: ?HandlerFn,
    wasm_handler: ?WasmHandler,

    // Stats
    requests_handled: u64,
    start_time: i64,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, port: u16) !Self {
        var loop = try EventLoop.init();
        errdefer loop.deinit();

        const listen_fd = try event_loop.createListener(port, 1024);
        errdefer posix.close(listen_fd);

        // Register listen socket for read events
        try loop.add(listen_fd, .{ .read = true }, 0);

        // Heap-allocate connections array (too large for stack: ~16MB)
        const connections = try allocator.create([MAX_CONNECTIONS]?Connection);
        @memset(connections, null);

        return Self{
            .allocator = allocator,
            .loop = loop,
            .listen_fd = listen_fd,
            .running = std.atomic.Value(bool).init(false),
            .connections = connections,
            .connection_count = 0,
            .native_handler = null,
            .wasm_handler = null,
            .requests_handled = 0,
            .start_time = std.time.milliTimestamp(),
        };
    }

    pub fn deinit(self: *Self) void {
        // Free pre-allocated WASM buffers
        if (self.wasm_handler) |*wh| {
            wh.deinit();
        }

        // Close all connections
        for (&self.connections.*) |*conn_opt| {
            if (conn_opt.*) |*conn| {
                posix.close(conn.fd);
                conn_opt.* = null;
            }
        }

        self.allocator.destroy(self.connections);
        posix.close(self.listen_fd);
        self.loop.deinit();
    }

    pub fn setNativeHandler(self: *Self, handler: HandlerFn) void {
        self.native_handler = handler;
    }

    pub fn setWasmHandler(self: *Self, exec_env: c.wasm_exec_env_t, handler_func: c.wasm_function_inst_t) void {
        self.wasm_handler = WasmHandler.init(exec_env, handler_func) catch null;
    }

    /// Run the event loop (blocks until stopped)
    pub noinline fn run(self: *Self) !void {
        self.running.store(true, .release);
        self.start_time = std.time.milliTimestamp();

        while (self.running.load(.acquire)) {
            self.checkTimeouts();

            const events = try self.loop.wait(100);

            for (events) |event| {
                if (event.fd == self.listen_fd) {
                    self.acceptConnection() catch {};
                } else {
                    self.handleConnection(event) catch {};
                }
            }
        }
    }

    pub fn stop(self: *Self) void {
        self.running.store(false, .release);
    }

    noinline fn acceptConnection(self: *Self) !void {
        var client_addr: posix.sockaddr.in = undefined;
        var addr_len: posix.socklen_t = @sizeOf(@TypeOf(client_addr));

        const client_fd = posix.accept(self.listen_fd, @ptrCast(&client_addr), &addr_len, 0) catch |err| {
            if (err == error.WouldBlock) return;
            return err;
        };
        errdefer posix.close(client_fd);

        try event_loop.setNonBlocking(client_fd);

        const slot_idx = self.findFreeSlot() orelse {
            posix.close(client_fd);
            return;
        };

        self.connections.*[slot_idx] = Connection.init(client_fd);
        self.connection_count += 1;

        try self.loop.add(client_fd, .{ .read = true }, slot_idx + 1);
    }

    noinline fn handleConnection(self: *Self, event: event_loop.Event) !void {
        const slot_idx = event.data - 1;
        if (slot_idx >= MAX_CONNECTIONS) return;

        var conn = &(self.connections.*[slot_idx] orelse return);
        conn.last_active = std.time.milliTimestamp();

        switch (conn.state) {
            .reading => try self.handleRead(conn, slot_idx),
            .writing => try self.handleWrite(conn, slot_idx),
            .closing => self.closeConnection(conn, slot_idx),
            else => {},
        }
    }

    fn handleRead(self: *Self, conn: *Connection, slot_idx: usize) !void {
        // Read data
        const n = posix.read(conn.fd, conn.read_buf[conn.read_len..]) catch |err| {
            if (err == error.WouldBlock) return;
            self.closeConnection(conn, slot_idx);
            return;
        };

        if (n == 0) {
            self.closeConnection(conn, slot_idx);
            return;
        }

        conn.read_len += n;

        // Check if request is complete
        if (!parser.isComplete(conn.read_buf[0..conn.read_len])) {
            return; // Need more data
        }

        // Parse request
        const request = parser.parse(conn.read_buf[0..conn.read_len]) catch {
            // Send 400 Bad Request
            conn.write_len = parser.formatSimpleResponse(400, "text/plain", "Bad Request", false, &conn.write_buf);
            conn.keep_alive = false;
            conn.state = .writing;
            try self.loop.modify(conn.fd, .{ .write = true }, slot_idx + 1);
            return;
        };

        conn.keep_alive = request.keep_alive;

        // Serialize request to JSON for handler
        var json_buf: [READ_BUFFER_SIZE * 2]u8 = undefined;
        const json_len = self.serializeRequest(&request, &json_buf);

        // Call handler
        var response_json: [WRITE_BUFFER_SIZE]u8 = undefined;
        const resp_len = self.callHandler(json_buf[0..json_len], &response_json) catch {
            // Internal error
            conn.write_len = parser.formatSimpleResponse(500, "text/plain", "Internal Server Error", false, &conn.write_buf);
            conn.keep_alive = false;
            conn.state = .writing;
            try self.loop.modify(conn.fd, .{ .write = true }, slot_idx + 1);
            return;
        };

        // Parse response JSON and format HTTP response
        conn.write_len = self.formatHttpFromJson(response_json[0..resp_len], conn.keep_alive, &conn.write_buf);

        self.requests_handled += 1;
        conn.state = .writing;
        try self.loop.modify(conn.fd, .{ .write = true }, slot_idx + 1);
    }

    fn handleWrite(self: *Self, conn: *Connection, slot_idx: usize) !void {
        const remaining = conn.write_buf[conn.write_pos..conn.write_len];
        const n = posix.write(conn.fd, remaining) catch |err| {
            if (err == error.WouldBlock) return;
            self.closeConnection(conn, slot_idx);
            return;
        };

        conn.write_pos += n;

        if (conn.write_pos >= conn.write_len) {
            // Write complete
            if (conn.keep_alive) {
                // Reset for next request
                conn.reset();
                try self.loop.modify(conn.fd, .{ .read = true }, slot_idx + 1);
            } else {
                self.closeConnection(conn, slot_idx);
            }
        }
    }

    fn closeConnection(self: *Self, conn: *Connection, slot_idx: usize) void {
        self.loop.remove(conn.fd) catch {};
        posix.close(conn.fd);
        self.connections.*[slot_idx] = null;
        self.connection_count -= 1;
    }

    noinline fn checkTimeouts(self: *Self) void {
        const now = std.time.milliTimestamp();

        for (&self.connections.*, 0..) |*conn_opt, i| {
            if (conn_opt.*) |*conn| {
                if (now - conn.last_active > KEEPALIVE_TIMEOUT_MS) {
                    self.closeConnection(conn, i);
                }
            }
        }
    }

    noinline fn findFreeSlot(self: *Self) ?usize {
        for (&self.connections.*, 0..) |*conn, i| {
            if (conn.* == null) return i;
        }
        return null;
    }

    fn callHandler(self: *Self, request_json: []const u8, response_buf: []u8) !usize {
        if (self.wasm_handler) |*wh| {
            return wh.call(request_json, response_buf);
        } else if (self.native_handler) |handler| {
            return handler(request_json, response_buf);
        } else {
            return error.NoHandler;
        }
    }

    /// Serialize ParsedRequest to JSON (optimized: direct buffer writes, no stream overhead)
    fn serializeRequest(self: *Self, req: *const parser.ParsedRequest, buf: []u8) usize {
        _ = self;
        var pos: usize = 0;

        // Fast path: pre-compute method prefix
        const method_str = req.method.toString();
        const prefix = "{\"method\":\"";
        @memcpy(buf[pos..][0..prefix.len], prefix);
        pos += prefix.len;
        @memcpy(buf[pos..][0..method_str.len], method_str);
        pos += method_str.len;

        // URL with escaping
        const url_prefix = "\",\"url\":\"";
        @memcpy(buf[pos..][0..url_prefix.len], url_prefix);
        pos += url_prefix.len;
        pos = writeJsonEscaped(buf, pos, req.url);

        // Headers
        const headers_prefix = "\",\"headers\":{";
        @memcpy(buf[pos..][0..headers_prefix.len], headers_prefix);
        pos += headers_prefix.len;

        for (req.headers[0..req.header_count], 0..) |h, i| {
            if (i > 0) {
                buf[pos] = ',';
                pos += 1;
            }
            buf[pos] = '"';
            pos += 1;
            // Lowercase header name (inline)
            for (h.name) |ch| {
                buf[pos] = std.ascii.toLower(ch);
                pos += 1;
            }
            buf[pos] = '"';
            buf[pos + 1] = ':';
            buf[pos + 2] = '"';
            pos += 3;
            pos = writeJsonEscaped(buf, pos, h.value);
            buf[pos] = '"';
            pos += 1;
        }

        // Body
        const body_prefix = "},\"body\":\"";
        @memcpy(buf[pos..][0..body_prefix.len], body_prefix);
        pos += body_prefix.len;
        if (req.body) |body| {
            pos = writeJsonEscapedBody(buf, pos, body);
        }
        buf[pos] = '"';
        buf[pos + 1] = '}';
        return pos + 2;
    }

    /// Write JSON-escaped string directly to buffer (inline for speed)
    inline fn writeJsonEscaped(buf: []u8, start: usize, str: []const u8) usize {
        var pos = start;
        for (str) |ch| {
            switch (ch) {
                '"' => {
                    buf[pos] = '\\';
                    buf[pos + 1] = '"';
                    pos += 2;
                },
                '\\' => {
                    buf[pos] = '\\';
                    buf[pos + 1] = '\\';
                    pos += 2;
                },
                else => {
                    buf[pos] = ch;
                    pos += 1;
                },
            }
        }
        return pos;
    }

    /// Write JSON-escaped body with newline handling
    inline fn writeJsonEscapedBody(buf: []u8, start: usize, str: []const u8) usize {
        var pos = start;
        for (str) |ch| {
            switch (ch) {
                '"' => {
                    buf[pos] = '\\';
                    buf[pos + 1] = '"';
                    pos += 2;
                },
                '\\' => {
                    buf[pos] = '\\';
                    buf[pos + 1] = '\\';
                    pos += 2;
                },
                '\n' => {
                    buf[pos] = '\\';
                    buf[pos + 1] = 'n';
                    pos += 2;
                },
                '\r' => {
                    buf[pos] = '\\';
                    buf[pos + 1] = 'r';
                    pos += 2;
                },
                else => {
                    buf[pos] = ch;
                    pos += 1;
                },
            }
        }
        return pos;
    }

    /// Parse response JSON and format as HTTP response
    /// Expected format: {"status":200,"headers":{"Content-Type":"text/plain"},"body":"Hello"}
    fn formatHttpFromJson(self: *Self, json: []const u8, keep_alive: bool, out: []u8) usize {
        _ = self;

        // Simple JSON parsing for response
        var status: u16 = 200;
        var content_type: []const u8 = "text/plain";
        var body_start: usize = 0;
        var body_end: usize = 0;

        // Find status
        if (std.mem.indexOf(u8, json, "\"status\":")) |pos| {
            const start = pos + 9;
            var end = start;
            while (end < json.len and json[end] >= '0' and json[end] <= '9') : (end += 1) {}
            status = std.fmt.parseInt(u16, json[start..end], 10) catch 200;
        }

        // Find Content-Type in headers
        if (std.mem.indexOf(u8, json, "\"content-type\":\"")) |pos| {
            const start = pos + 16;
            if (std.mem.indexOfPos(u8, json, start, "\"")) |end| {
                content_type = json[start..end];
            }
        }

        // Find body
        if (std.mem.indexOf(u8, json, "\"body\":\"")) |pos| {
            body_start = pos + 8;
            // Find closing quote (handle escapes)
            var i = body_start;
            while (i < json.len) : (i += 1) {
                if (json[i] == '\\' and i + 1 < json.len) {
                    i += 1; // Skip escaped char
                } else if (json[i] == '"') {
                    body_end = i;
                    break;
                }
            }
        }

        // Unescape body into output buffer (after HTTP headers)
        var body_buf: [WRITE_BUFFER_SIZE / 2]u8 = undefined;
        var body_len: usize = 0;

        var i = body_start;
        while (i < body_end) : (i += 1) {
            if (json[i] == '\\' and i + 1 < body_end) {
                i += 1;
                const escaped = switch (json[i]) {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '"' => '"',
                    '\\' => '\\',
                    else => json[i],
                };
                if (body_len < body_buf.len) {
                    body_buf[body_len] = escaped;
                    body_len += 1;
                }
            } else {
                if (body_len < body_buf.len) {
                    body_buf[body_len] = json[i];
                    body_len += 1;
                }
            }
        }

        return parser.formatSimpleResponse(status, content_type, body_buf[0..body_len], keep_alive, out);
    }
};

// Simple test handler for benchmarking
pub fn testHandler(request_json: []const u8, response_buf: []u8) usize {
    _ = request_json;
    const response = "{\"status\":200,\"headers\":{\"content-type\":\"text/plain\"},\"body\":\"Hello, World!\"}";
    @memcpy(response_buf[0..response.len], response);
    return response.len;
}

// Tests
test "native server init/deinit" {
    var server = try NativeHttpServer.init(std.testing.allocator, 0);
    defer server.deinit();
}

test "serialize request" {
    var server = try NativeHttpServer.init(std.testing.allocator, 0);
    defer server.deinit();

    const req_str = "GET /test HTTP/1.1\r\nHost: localhost\r\n\r\n";
    var req = try parser.parse(req_str);

    var buf: [1024]u8 = undefined;
    const len = server.serializeRequest(&req, &buf);
    const json = buf[0..len];

    try std.testing.expect(std.mem.indexOf(u8, json, "\"method\":\"GET\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"url\":\"/test\"") != null);
}
