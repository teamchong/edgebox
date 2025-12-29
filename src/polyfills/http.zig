/// Native HTTP Server - Thin host, raw buffer passing
///
/// Security model: Host only handles raw bytes, ALL parsing in sandbox
/// - Host: accept(), read(), write(), close() - blocking I/O
/// - Sandbox: HTTP parsing, header validation, user handler, response building
///
/// This is secure because malformed HTTP can't crash the host - it's just bytes
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;
const posix = std.posix;

const Allocator = std.mem.Allocator;

// =============================================================================
// HTTP Server (blocking, thin host - raw buffer passing)
// =============================================================================

/// Connection handle for passing to sandbox
pub const ConnectionHandle = struct {
    fd: posix.socket_t,
    request_buf: []u8,
    request_len: usize,
};

pub const Server = struct {
    allocator: Allocator,
    listener: ?posix.socket_t,
    port: u16,
    running: bool,
    js_handler: qjs.JSValue, // JS callback: function(rawRequestBuffer) -> rawResponseBuffer
    js_ctx: ?*qjs.JSContext,

    // Connection pool for reuse
    connections: [64]?ConnectionHandle,
    next_conn_id: u32,

    // Shared buffer for requests (avoid allocation per request)
    request_buffer: [65536]u8,

    pub fn init(allocator: Allocator) Server {
        return .{
            .allocator = allocator,
            .listener = null,
            .port = 0,
            .running = false,
            .js_handler = qjs.JS_UNDEFINED,
            .js_ctx = null,
            .connections = [_]?ConnectionHandle{null} ** 64,
            .next_conn_id = 0,
            .request_buffer = undefined,
        };
    }

    pub fn deinit(self: *Server) void {
        self.stop();
        if (self.js_ctx) |ctx| {
            qjs.JS_FreeValue(ctx, self.js_handler);
        }
    }

    pub fn setHandler(self: *Server, ctx: *qjs.JSContext, handler: qjs.JSValue) void {
        self.js_ctx = ctx;
        self.js_handler = qjs.JS_DupValue(ctx, handler);
    }

    pub fn listen(self: *Server, port: u16) !void {
        self.port = port;

        // Create socket
        const sock = try posix.socket(posix.AF.INET, posix.SOCK.STREAM, 0);
        errdefer posix.close(sock);

        // Set SO_REUSEADDR
        const opt: i32 = 1;
        try posix.setsockopt(sock, posix.SOL.SOCKET, posix.SO.REUSEADDR, std.mem.asBytes(&opt));

        // Bind
        const addr = posix.sockaddr.in{
            .port = std.mem.nativeToBig(u16, port),
            .addr = 0, // INADDR_ANY
        };
        try posix.bind(sock, @ptrCast(&addr), @sizeOf(@TypeOf(addr)));

        // Listen
        try posix.listen(sock, 128);

        self.listener = sock;
        self.running = true;
    }

    pub fn stop(self: *Server) void {
        self.running = false;
        if (self.listener) |sock| {
            posix.close(sock);
            self.listener = null;
        }
    }

    /// Accept one connection and handle it (blocking)
    /// This is the hot path - optimized for minimal overhead
    pub fn acceptOne(self: *Server) !void {
        const sock = self.listener orelse return error.NotListening;
        const ctx = self.js_ctx orelse return error.NoJsContext;

        // Accept connection (blocking - kernel wakes us when ready)
        var client_addr: posix.sockaddr.in = undefined;
        var addr_len: posix.socklen_t = @sizeOf(@TypeOf(client_addr));
        const client = try posix.accept(sock, @ptrCast(&client_addr), &addr_len, 0);
        defer posix.close(client);

        // Read raw request bytes (no parsing in host!)
        const bytes_read = posix.read(client, &self.request_buffer) catch |err| {
            std.debug.print("[http] read error: {}\n", .{err});
            return;
        };
        if (bytes_read == 0) return;

        // Pass raw bytes to sandbox handler
        // Handler signature: function(rawBuffer: Uint8Array) -> Uint8Array
        const response_bytes = self.callSandboxHandler(ctx, self.request_buffer[0..bytes_read]) catch |err| {
            std.debug.print("[http] handler error: {}\n", .{err});
            // Send minimal error response
            const err_response = "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 5\r\nConnection: close\r\n\r\nError";
            _ = posix.write(client, err_response) catch {};
            return;
        };
        defer self.allocator.free(response_bytes);

        // Write raw response bytes (no building in host!)
        _ = posix.write(client, response_bytes) catch {};
    }

    /// Call sandbox handler with raw bytes, get raw bytes back
    fn callSandboxHandler(self: *Server, ctx: *qjs.JSContext, request_bytes: []const u8) ![]u8 {
        // Create Uint8Array from request bytes
        const global = qjs.JS_GetGlobalObject(ctx);
        defer qjs.JS_FreeValue(ctx, global);

        const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
        defer qjs.JS_FreeValue(ctx, uint8array_ctor);

        // Create ArrayBuffer with request data
        var len_arg = [1]qjs.JSValue{qjs.JS_NewInt32(ctx, @intCast(request_bytes.len))};
        const js_request = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &len_arg);
        if (qjs.JS_IsException(js_request)) return error.JsException;
        defer qjs.JS_FreeValue(ctx, js_request);

        // Copy request bytes into Uint8Array
        for (request_bytes, 0..) |byte, i| {
            const byte_val = qjs.JS_NewInt32(ctx, @intCast(byte));
            _ = qjs.JS_SetPropertyUint32(ctx, js_request, @intCast(i), byte_val);
        }

        // Call handler: handler(requestBuffer) -> responseBuffer
        var args = [1]qjs.JSValue{js_request};
        const js_response = qjs.JS_Call(ctx, self.js_handler, qjs.JS_UNDEFINED, 1, &args);
        if (qjs.JS_IsException(js_response)) {
            // Get exception for debugging
            const exception = qjs.JS_GetException(ctx);
            defer qjs.JS_FreeValue(ctx, exception);
            return error.JsException;
        }
        defer qjs.JS_FreeValue(ctx, js_response);

        // Extract bytes from response Uint8Array/String
        if (qjs.JS_IsString(js_response)) {
            // Response is a string - get raw bytes
            var len: usize = 0;
            const str = qjs.JS_ToCStringLen(ctx, &len, js_response);
            if (str == null) return error.JsException;
            defer qjs.JS_FreeCString(ctx, str);

            const result = try self.allocator.alloc(u8, len);
            @memcpy(result, str[0..len]);
            return result;
        }

        // Response should be Uint8Array or ArrayBuffer
        const len_val = qjs.JS_GetPropertyStr(ctx, js_response, "length");
        defer qjs.JS_FreeValue(ctx, len_val);

        var len: i32 = 0;
        if (qjs.JS_ToInt32(ctx, &len, len_val) != 0) return error.JsException;

        const result = try self.allocator.alloc(u8, @intCast(len));
        errdefer self.allocator.free(result);

        for (0..@intCast(len)) |i| {
            const byte_val = qjs.JS_GetPropertyUint32(ctx, js_response, @intCast(i));
            defer qjs.JS_FreeValue(ctx, byte_val);
            var byte: i32 = 0;
            _ = qjs.JS_ToInt32(ctx, &byte, byte_val);
            result[i] = @intCast(byte & 0xFF);
        }

        return result;
    }

    /// Run server loop (blocking)
    pub fn run(self: *Server) !void {
        while (self.running) {
            self.acceptOne() catch |err| {
                if (err == error.NotListening) break;
                // Continue on other errors
            };
        }
    }
};

// =============================================================================
// Global server instance (for JS integration)
// =============================================================================

var g_server: ?*Server = null;
var g_allocator: Allocator = undefined;

/// Initialize HTTP module
pub fn init(allocator: Allocator) void {
    g_allocator = allocator;
}

// =============================================================================
// QuickJS Native Functions
// =============================================================================

/// http.createServer(handler) - Create HTTP server with handler
fn httpCreateServer(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "createServer requires handler function");

    const handler = argv[0];
    if (!qjs.JS_IsFunction(ctx, handler)) {
        return qjs.JS_ThrowTypeError(ctx, "handler must be a function");
    }

    // Create server
    const server = g_allocator.create(Server) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to allocate server");
    };
    server.* = Server.init(g_allocator);
    server.setHandler(ctx.?, handler);

    g_server = server;

    // Return server object with listen method
    const js_server = qjs.JS_NewObject(ctx);

    // Add listen method
    const listen_fn = qjs.JS_NewCFunction(ctx, httpServerListen, "listen", 1);
    _ = qjs.JS_SetPropertyStr(ctx, js_server, "listen", listen_fn);

    // Add close method
    const close_fn = qjs.JS_NewCFunction(ctx, httpServerClose, "close", 0);
    _ = qjs.JS_SetPropertyStr(ctx, js_server, "close", close_fn);

    return js_server;
}

/// server.listen(port, callback)
fn httpServerListen(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const server = g_server orelse {
        return qjs.JS_ThrowInternalError(ctx, "No server instance");
    };

    var port: i32 = 8080;
    if (argc >= 1) {
        _ = qjs.JS_ToInt32(ctx, &port, argv[0]);
    }

    server.listen(@intCast(port)) catch |err| {
        return qjs.JS_ThrowInternalError(ctx, "Failed to listen: %s", @errorName(err).ptr);
    };

    // Call callback if provided
    if (argc >= 2 and qjs.JS_IsFunction(ctx, argv[1])) {
        const result = qjs.JS_Call(ctx, argv[1], qjs.JS_UNDEFINED, 0, null);
        qjs.JS_FreeValue(ctx, result);
    }

    // Return server for chaining
    return qjs.JS_UNDEFINED;
}

/// server.close()
fn httpServerClose(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (g_server) |server| {
        server.stop();
        server.deinit();
        g_allocator.destroy(server);
        g_server = null;
    }
    return qjs.JS_UNDEFINED;
}

/// Register HTTP module with QuickJS
pub fn registerModule(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Get or create _modules
    var modules = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (qjs.JS_IsUndefined(modules)) {
        modules = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, global, "_modules", modules);
    }

    // Create native http module
    const http_module = qjs.JS_NewObject(ctx);

    // Add createServer
    const create_server_fn = qjs.JS_NewCFunction(ctx, httpCreateServer, "createServer", 1);
    _ = qjs.JS_SetPropertyStr(ctx, http_module, "createServer", create_server_fn);

    // Register as _nativeHttp
    _ = qjs.JS_SetPropertyStr(ctx, modules, "_nativeHttp", http_module);
}

/// Run server event loop (call from main loop)
pub fn tick() void {
    if (g_server) |server| {
        if (server.running) {
            server.acceptOne() catch {};
        }
    }
}
