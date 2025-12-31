//! Socket Dispatch - TCP socket operations for http.createServer / net.createServer
//!
//! Provides socket operations including:
//! - Socket lifecycle: create, bind, listen, accept, connect, close
//! - I/O operations: read, write (blocking and non-blocking)
//! - Batched operations: accept+read, write+close for performance
//! - State management and validation

const std = @import("std");
const wasm_helpers = @import("../wasm_helpers.zig");
const c = wasm_helpers.c;

// Socket opcodes (must match wasm_main_static.zig)
pub const OP_CREATE: i32 = 0;
pub const OP_BIND: i32 = 1;
pub const OP_LISTEN: i32 = 2;
pub const OP_ACCEPT: i32 = 3;
pub const OP_CONNECT: i32 = 4;
pub const OP_WRITE: i32 = 5;
pub const OP_READ: i32 = 6;
pub const OP_GET_READ_DATA: i32 = 7;
pub const OP_CLOSE: i32 = 8;
pub const OP_STATE: i32 = 9;
pub const OP_SET_BLOCKING: i32 = 10; // Remove O_NONBLOCK from socket
pub const OP_ACCEPT_BLOCKING: i32 = 11; // Blocking accept (waits for connection)
pub const OP_READ_BLOCKING: i32 = 12; // Blocking read (waits for data)
pub const OP_HTTP_SERVE_ONE: i32 = 13; // High-perf: accept+read+callback+write+close in one call
pub const OP_ACCEPT_READ: i32 = 14; // Batched: accept + read into WASM buffer
pub const OP_WRITE_CLOSE: i32 = 15; // Batched: write from WASM buffer + close

// Socket states (must match JS polyfill SOCKET_STATE)
pub const STATE_CREATED: i32 = 0;
pub const STATE_BOUND: i32 = 1;
pub const STATE_LISTENING: i32 = 2;
pub const STATE_CONNECTED: i32 = 3;
pub const STATE_CLOSED: i32 = 4;

const MAX_SOCKETS = 64;
const O_NONBLOCK: usize = 0x4; // macOS/BSD O_NONBLOCK value

const SocketEntry = struct {
    fd: std.posix.socket_t,
    state: i32,
    read_buffer: ?[]u8, // Buffer for last read data
};

// Configuration interface (injected from parent module)
pub const Config = struct {
    /// Check if module can listen on a given port
    canListenPort: *const fn (port: u16) bool,
};

// Global state
var g_sockets: [MAX_SOCKETS]?SocketEntry = [_]?SocketEntry{null} ** MAX_SOCKETS;
var g_next_socket_id: u32 = 1;
var g_allocator: std.mem.Allocator = std.heap.page_allocator;
var g_config: ?Config = null;

/// Set the allocator for socket operations
pub fn setAllocator(allocator: std.mem.Allocator) void {
    g_allocator = allocator;
}

/// Set configuration for port validation
pub fn setConfig(config: Config) void {
    g_config = config;
}

/// Reset socket state for fresh child process
/// Called at start of each module execution in forked child
pub fn reset() void {
    // Close any inherited file descriptors and clear socket slots
    for (&g_sockets) |*slot| {
        if (slot.*) |entry| {
            std.posix.close(entry.fd);
            if (entry.read_buffer) |buf| {
                g_allocator.free(buf);
            }
        }
        slot.* = null;
    }
    g_next_socket_id = 1;
}

/// Main dispatch function for socket operations
pub fn dispatch(exec_env: c.wasm_exec_env_t, opcode: i32, a1: i32, a2: i32, a3: i32) i32 {
    return switch (opcode) {
        OP_CREATE => socketCreate(),
        OP_BIND => socketBind(@bitCast(a1), @bitCast(a2)),
        OP_LISTEN => socketListen(@bitCast(a1), @bitCast(a2)),
        OP_ACCEPT => socketAccept(@bitCast(a1)),
        OP_CONNECT => socketConnect(@bitCast(a1), @bitCast(a2)),
        OP_WRITE => socketWrite(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3)),
        OP_READ => socketRead(@bitCast(a1), @bitCast(a2)),
        OP_GET_READ_DATA => socketGetReadData(exec_env, @bitCast(a1), @bitCast(a2)),
        OP_CLOSE => socketClose(@bitCast(a1)),
        OP_STATE => socketState(@bitCast(a1)),
        OP_SET_BLOCKING => socketSetBlocking(@bitCast(a1)),
        OP_ACCEPT_BLOCKING => socketAcceptBlocking(@bitCast(a1)),
        OP_READ_BLOCKING => socketReadBlocking(@bitCast(a1), @bitCast(a2)),
        OP_ACCEPT_READ => socketAcceptRead(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3)),
        OP_WRITE_CLOSE => socketWriteClose(exec_env, @bitCast(a1), @bitCast(a2), @bitCast(a3)),
        else => -1,
    };
}

// ============================================================================
// Socket Operations
// ============================================================================

fn socketCreate() i32 {
    // Find free slot
    var slot_idx: ?usize = null;
    for (&g_sockets, 0..) |*slot, i| {
        if (slot.* == null) {
            slot_idx = i;
            break;
        }
    }
    if (slot_idx == null) return -1; // No free slots

    // Create TCP socket
    const fd = std.posix.socket(std.posix.AF.INET, std.posix.SOCK.STREAM, 0) catch |err| {
        std.debug.print("[Socket] Create failed: {}\n", .{err});
        return -1;
    };

    // Set socket options for server use
    std.posix.setsockopt(fd, std.posix.SOL.SOCKET, std.posix.SO.REUSEADDR, &std.mem.toBytes(@as(c_int, 1))) catch {};

    // Set non-blocking for accept polling
    const flags = std.posix.fcntl(fd, std.posix.F.GETFL, 0) catch 0;
    _ = std.posix.fcntl(fd, std.posix.F.SETFL, flags | O_NONBLOCK) catch {};

    // Socket ID = slot index + 1 (allows lookup via socket_id - 1)
    const socket_id: i32 = @intCast(slot_idx.? + 1);

    g_sockets[slot_idx.?] = SocketEntry{
        .fd = fd,
        .state = STATE_CREATED,
        .read_buffer = null,
    };

    return socket_id;
}

fn socketBind(socket_id: u32, port: u32) i32 {
    // Check port permission
    const cfg = g_config orelse return -2; // No config
    if (!cfg.canListenPort(@intCast(port))) {
        std.debug.print("[Socket] Bind denied: port {} not in allowed list\n", .{port});
        return -3; // Permission denied
    }

    // Find socket by ID (ID maps to slot index + 1)
    if (socket_id == 0 or socket_id > MAX_SOCKETS) return -1;
    const slot_idx = socket_id - 1;
    const entry = &(g_sockets[slot_idx] orelse return -1);

    var addr = std.posix.sockaddr.in{
        .family = std.posix.AF.INET,
        .port = std.mem.nativeToBig(u16, @intCast(port)),
        .addr = 0, // INADDR_ANY
    };

    std.posix.bind(entry.fd, @ptrCast(&addr), @sizeOf(@TypeOf(addr))) catch |err| {
        std.debug.print("[Socket] Bind failed: {}\n", .{err});
        return -1;
    };

    entry.state = STATE_BOUND;
    return 0;
}

fn socketListen(socket_id: u32, backlog: u32) i32 {
    if (socket_id == 0 or socket_id > MAX_SOCKETS) return -1;
    const slot_idx = socket_id - 1;
    const entry = &(g_sockets[slot_idx] orelse return -1);

    std.posix.listen(entry.fd, @intCast(@min(backlog, 128))) catch |err| {
        std.debug.print("[Socket] Listen failed: {}\n", .{err});
        return -1;
    };

    entry.state = STATE_LISTENING;
    return 0;
}

fn socketAccept(socket_id: u32) i32 {
    if (socket_id == 0 or socket_id > MAX_SOCKETS) return -1;
    const slot_idx = socket_id - 1;
    const entry = g_sockets[slot_idx] orelse return -1;

    var client_addr: std.posix.sockaddr.in = undefined;
    var addr_len: std.posix.socklen_t = @sizeOf(@TypeOf(client_addr));

    const client_fd = std.posix.accept(entry.fd, @ptrCast(&client_addr), &addr_len, 0) catch |err| {
        if (err == error.WouldBlock) return 0; // No pending connection
        std.debug.print("[Socket] Accept failed: {}\n", .{err});
        return -1;
    };

    // Set non-blocking on client socket
    const client_flags = std.posix.fcntl(client_fd, std.posix.F.GETFL, 0) catch 0;
    _ = std.posix.fcntl(client_fd, std.posix.F.SETFL, client_flags | O_NONBLOCK) catch {};

    // Find free slot for client socket
    var client_slot: ?usize = null;
    for (&g_sockets, 0..) |*slot, i| {
        if (slot.* == null) {
            client_slot = i;
            break;
        }
    }
    if (client_slot == null) {
        std.posix.close(client_fd);
        return -1;
    }

    // Socket ID = slot index + 1
    const client_socket_id: i32 = @intCast(client_slot.? + 1);

    g_sockets[client_slot.?] = SocketEntry{
        .fd = client_fd,
        .state = STATE_CONNECTED,
        .read_buffer = null,
    };

    return client_socket_id;
}

fn socketConnect(socket_id: u32, port: u32) i32 {
    if (socket_id == 0 or socket_id > MAX_SOCKETS) return -1;
    const slot_idx = socket_id - 1;
    const entry = &(g_sockets[slot_idx] orelse return -1);

    var addr = std.posix.sockaddr.in{
        .family = std.posix.AF.INET,
        .port = std.mem.nativeToBig(u16, @intCast(port)),
        .addr = std.mem.nativeToBig(u32, 0x7f000001), // 127.0.0.1
    };

    std.posix.connect(entry.fd, @ptrCast(&addr), @sizeOf(@TypeOf(addr))) catch |err| {
        std.debug.print("[Socket] Connect failed: {}\n", .{err});
        return -1;
    };

    entry.state = STATE_CONNECTED;
    return 0;
}

fn socketWrite(exec_env: c.wasm_exec_env_t, socket_id: u32, data_ptr: u32, data_len: u32) i32 {
    if (socket_id == 0 or socket_id > MAX_SOCKETS) return -1;
    const slot_idx = socket_id - 1;
    const entry = g_sockets[slot_idx] orelse return -1;

    const data = readWasmMemory(exec_env, data_ptr, data_len) orelse return -1;

    const written = std.posix.write(entry.fd, data) catch |err| {
        std.debug.print("[Socket] Write failed: {}\n", .{err});
        return -1;
    };

    return @intCast(written);
}

fn socketRead(socket_id: u32, max_len: u32) i32 {
    if (socket_id == 0 or socket_id > MAX_SOCKETS) return -1;
    const slot_idx = socket_id - 1;
    const entry = &(g_sockets[slot_idx] orelse return -1);

    // Free previous buffer
    if (entry.read_buffer) |buf| {
        g_allocator.free(buf);
        entry.read_buffer = null;
    }

    const buf = g_allocator.alloc(u8, @min(max_len, 65536)) catch return -1;

    const n = std.posix.read(entry.fd, buf) catch |err| {
        g_allocator.free(buf);
        if (err == error.WouldBlock) return 0; // No data available
        return -2; // EOF or error
    };

    if (n == 0) {
        g_allocator.free(buf);
        return -2; // EOF
    }

    entry.read_buffer = buf[0..n];
    return @intCast(n);
}

fn socketGetReadData(exec_env: c.wasm_exec_env_t, socket_id: u32, dest_ptr: u32) i32 {
    if (socket_id == 0 or socket_id > MAX_SOCKETS) return -1;
    const slot_idx = socket_id - 1;
    const entry = &(g_sockets[slot_idx] orelse return -1);

    const buf = entry.read_buffer orelse return -1;

    // Use validated WASM memory write
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return -1;
    if (!c.wasm_runtime_validate_app_addr(module_inst, dest_ptr, @intCast(buf.len))) return -1;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, dest_ptr);
    if (native_ptr == null) return -1;
    const dest: [*]u8 = @ptrCast(native_ptr);
    @memcpy(dest[0..buf.len], buf);

    return @intCast(buf.len);
}

fn socketClose(socket_id: u32) i32 {
    if (socket_id == 0 or socket_id > MAX_SOCKETS) return -1;
    const slot_idx = socket_id - 1;
    const entry = &(g_sockets[slot_idx] orelse return -1);

    std.posix.close(entry.fd);

    if (entry.read_buffer) |buf| {
        g_allocator.free(buf);
    }

    g_sockets[slot_idx] = null;
    return 0;
}

fn socketState(socket_id: u32) i32 {
    if (socket_id == 0 or socket_id > MAX_SOCKETS) return STATE_CLOSED;
    const slot_idx = socket_id - 1;
    const entry = g_sockets[slot_idx] orelse return STATE_CLOSED;
    return entry.state;
}

/// Remove O_NONBLOCK flag from socket (makes it blocking)
fn socketSetBlocking(socket_id: u32) i32 {
    if (socket_id == 0 or socket_id > MAX_SOCKETS) return -1;
    const slot_idx = socket_id - 1;
    const entry = g_sockets[slot_idx] orelse return -1;

    // Remove O_NONBLOCK flag
    const flags = std.posix.fcntl(entry.fd, std.posix.F.GETFL, 0) catch return -1;
    _ = std.posix.fcntl(entry.fd, std.posix.F.SETFL, flags & ~O_NONBLOCK) catch return -1;

    return 0;
}

/// Blocking accept - waits until a connection arrives
/// Returns client socket ID (>0) on success, -1 on error
fn socketAcceptBlocking(socket_id: u32) i32 {
    if (socket_id == 0 or socket_id > MAX_SOCKETS) return -1;
    const slot_idx = socket_id - 1;
    const entry = g_sockets[slot_idx] orelse return -1;

    // Temporarily remove O_NONBLOCK to make accept truly blocking
    const flags = std.posix.fcntl(entry.fd, std.posix.F.GETFL, 0) catch 0;
    _ = std.posix.fcntl(entry.fd, std.posix.F.SETFL, flags & ~O_NONBLOCK) catch {};

    var client_addr: std.posix.sockaddr.in = undefined;
    var addr_len: std.posix.socklen_t = @sizeOf(@TypeOf(client_addr));

    // This will block until a connection arrives
    const client_fd = std.posix.accept(entry.fd, @ptrCast(&client_addr), &addr_len, 0) catch |err| {
        std.debug.print("[Socket] Accept blocking failed: {}\n", .{err});
        return -1;
    };

    // Keep client socket blocking for read/write performance
    // (no O_NONBLOCK set)

    // Find free slot for client socket
    var client_slot: ?usize = null;
    for (&g_sockets, 0..) |*slot, i| {
        if (slot.* == null) {
            client_slot = i;
            break;
        }
    }
    if (client_slot == null) {
        std.posix.close(client_fd);
        return -1;
    }

    // Socket ID = slot index + 1
    const client_socket_id: i32 = @intCast(client_slot.? + 1);

    g_sockets[client_slot.?] = SocketEntry{
        .fd = client_fd,
        .state = STATE_CONNECTED,
        .read_buffer = null,
    };

    return client_socket_id;
}

/// Blocking read - waits until data is available
/// Returns bytes read (>0), 0 on EOF, -1 on error
fn socketReadBlocking(socket_id: u32, max_len: u32) i32 {
    if (socket_id == 0 or socket_id > MAX_SOCKETS) return -1;
    const slot_idx = socket_id - 1;
    const entry = &(g_sockets[slot_idx] orelse return -1);

    // Remove O_NONBLOCK to make read blocking
    const flags = std.posix.fcntl(entry.fd, std.posix.F.GETFL, 0) catch 0;
    _ = std.posix.fcntl(entry.fd, std.posix.F.SETFL, flags & ~O_NONBLOCK) catch {};

    // Free previous buffer
    if (entry.read_buffer) |buf| {
        g_allocator.free(buf);
        entry.read_buffer = null;
    }

    const buf = g_allocator.alloc(u8, @min(max_len, 65536)) catch return -1;

    // This will block until data is available
    const n = std.posix.read(entry.fd, buf) catch |err| {
        g_allocator.free(buf);
        std.debug.print("[Socket] Read blocking failed: {}\n", .{err});
        return -1;
    };

    if (n == 0) {
        g_allocator.free(buf);
        return 0; // EOF
    }

    entry.read_buffer = buf[0..n];
    return @intCast(n);
}

/// Batched accept + read: Accept connection AND read data in one WASM crossing
/// Args: listener_socket_id, dest_ptr (WASM addr), max_len
/// Returns: high 16 bits = client_id, low 16 bits = bytes read; or negative on error
fn socketAcceptRead(exec_env: c.wasm_exec_env_t, socket_id: u32, dest_ptr: u32, max_len: u32) i32 {
    if (socket_id == 0 or socket_id > MAX_SOCKETS) return -1;
    const slot_idx = socket_id - 1;
    const entry = g_sockets[slot_idx] orelse return -1;

    // 1. Blocking accept
    const flags = std.posix.fcntl(entry.fd, std.posix.F.GETFL, 0) catch 0;
    _ = std.posix.fcntl(entry.fd, std.posix.F.SETFL, flags & ~O_NONBLOCK) catch {};

    var client_addr: std.posix.sockaddr.in = undefined;
    var addr_len: std.posix.socklen_t = @sizeOf(@TypeOf(client_addr));
    const client_fd = std.posix.accept(entry.fd, @ptrCast(&client_addr), &addr_len, 0) catch return -2;

    // 2. Find slot for client
    var client_slot: usize = 0;
    for (g_sockets, 0..) |s, i| {
        if (s == null) {
            client_slot = i;
            break;
        }
    } else {
        std.posix.close(client_fd);
        return -3; // No available slots
    }

    g_sockets[client_slot] = SocketEntry{
        .fd = client_fd,
        .state = STATE_CONNECTED,
        .read_buffer = null,
    };
    const client_id: u32 = @intCast(client_slot + 1);

    // 3. Read directly into WASM memory
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) {
        _ = socketClose(client_id);
        return -4;
    }
    if (!c.wasm_runtime_validate_app_addr(module_inst, dest_ptr, max_len)) {
        _ = socketClose(client_id);
        return -5;
    }
    const dest: [*]u8 = @ptrCast(c.wasm_runtime_addr_app_to_native(module_inst, dest_ptr) orelse {
        _ = socketClose(client_id);
        return -5;
    });

    const bytes_read = std.posix.read(client_fd, dest[0..max_len]) catch |err| {
        if (err == error.WouldBlock) return @as(i32, @intCast(client_id)) << 16; // No data yet
        _ = socketClose(client_id);
        return -6;
    };

    // Return packed: (client_id << 16) | bytes_read
    return (@as(i32, @intCast(client_id)) << 16) | @as(i32, @intCast(bytes_read));
}

/// Batched write + close: Write data AND close connection in one WASM crossing
/// Args: client_socket_id, src_ptr (WASM addr), len
/// Returns: bytes written, or negative on error
fn socketWriteClose(exec_env: c.wasm_exec_env_t, socket_id: u32, src_ptr: u32, len: u32) i32 {
    if (socket_id == 0 or socket_id > MAX_SOCKETS) return -1;
    const slot_idx = socket_id - 1;
    const entry = g_sockets[slot_idx] orelse return -1;

    // Get data from WASM memory
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return -2;
    if (!c.wasm_runtime_validate_app_addr(module_inst, src_ptr, len)) return -3;
    const src: [*]const u8 = @ptrCast(c.wasm_runtime_addr_app_to_native(module_inst, src_ptr) orelse return -3);

    // Write data
    const bytes_written = std.posix.write(entry.fd, src[0..len]) catch 0;

    // Close
    std.posix.close(entry.fd);
    if (entry.read_buffer) |buf| g_allocator.free(buf);
    g_sockets[slot_idx] = null;

    return @intCast(bytes_written);
}

// ============================================================================
// Internal helpers
// ============================================================================

fn readWasmMemory(exec_env: c.wasm_exec_env_t, ptr: u32, len: u32) ?[]const u8 {
    const module_inst = c.wasm_runtime_get_module_inst(exec_env);
    if (module_inst == null) return null;
    if (!c.wasm_runtime_validate_app_addr(module_inst, ptr, len)) return null;
    const native_ptr = c.wasm_runtime_addr_app_to_native(module_inst, ptr);
    if (native_ptr == null) return null;
    const bytes: [*]const u8 = @ptrCast(native_ptr);
    return bytes[0..len];
}
