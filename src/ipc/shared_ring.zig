//! Shared Memory Ring Buffer for IPC
//!
//! High-performance IPC using shared memory with atomic ring buffers.
//! Reduces daemon communication overhead from ~20ms (socket) to ~1-2ms.
//!
//! Architecture:
//!   - 4MB shared memory region (header + request ring + response ring)
//!   - Lock-free request/response via atomic state transitions
//!   - Cross-platform: works on Linux and macOS via shm_open/mmap

const std = @import("std");
const posix = std.posix;
const builtin = @import("builtin");

pub const MAGIC: u32 = 0x45424F58; // "EBOX"
pub const VERSION: u32 = 1;
pub const HEADER_SIZE: usize = 64;
pub const REQUEST_RING_SIZE: usize = 1 * 1024 * 1024; // 1MB
pub const RESPONSE_RING_SIZE: usize = 2 * 1024 * 1024; // 2MB
pub const TOTAL_SIZE: usize = HEADER_SIZE + REQUEST_RING_SIZE + RESPONSE_RING_SIZE;

/// Maximum size for a single request (path + args)
pub const MAX_REQUEST_SIZE: usize = 64 * 1024; // 64KB
/// Maximum size for a single response (output data)
pub const MAX_RESPONSE_SIZE: usize = 1 * 1024 * 1024; // 1MB

/// Entry states for lock-free ring buffer
pub const EntryState = enum(u8) {
    empty = 0, // Slot available
    writing = 1, // Producer is writing
    ready = 2, // Data ready for consumer
    reading = 3, // Consumer is reading
};

/// Shared memory header (64 bytes, page-aligned access)
pub const Header = extern struct {
    magic: u32,
    version: u32,
    req_head: std.atomic.Value(u32),
    req_tail: std.atomic.Value(u32),
    resp_head: std.atomic.Value(u32),
    resp_tail: std.atomic.Value(u32),
    server_ready: std.atomic.Value(u32),
    next_req_id: std.atomic.Value(u32),
    _reserved: [32]u8,
};

/// Parsed request from shared memory
pub const Request = struct {
    id: u32,
    path: []const u8,
    args: []const []const u8,
    args_storage: [32][]const u8 = undefined, // Pre-allocated storage
    offset: usize, // Ring buffer offset for cleanup
};

/// Parsed response from shared memory
pub const Response = struct {
    id: u32,
    exit_code: i32,
    output: []const u8,
    offset: usize, // Ring buffer offset for cleanup
};

pub const SharedRing = struct {
    base: [*]align(4096) u8,
    header: *Header,
    req_ring: []u8,
    resp_ring: []u8,
    fd: posix.fd_t,
    is_server: bool,
    shm_name: [64]u8,
    shm_name_len: usize,

    const Self = @This();

    /// Get the shared memory path based on UID
    pub fn getShmName() [64]u8 {
        var name: [64]u8 = undefined;
        const uid = std.posix.system.getuid();
        const prefix = "/edgebox-";
        @memcpy(name[0..prefix.len], prefix);
        const uid_str = std.fmt.bufPrint(name[prefix.len..], "{d}", .{uid}) catch unreachable;
        const total_len = prefix.len + uid_str.len;
        @memset(name[total_len..], 0);
        return name;
    }

    /// Open shared memory (create if server, attach if client)
    pub fn open(is_server: bool) !Self {
        const shm_name = getShmName();
        const shm_name_len = std.mem.indexOfScalar(u8, &shm_name, 0) orelse shm_name.len;
        const name_slice = shm_name[0..shm_name_len :0];

        var fd: posix.fd_t = undefined;

        if (is_server) {
            // Server: create and initialize
            // First try to unlink any stale segment
            _ = std.c.shm_unlink(name_slice);

            fd = std.c.shm_open(
                name_slice,
                @bitCast(@as(u32, @bitCast(posix.O{ .CREAT = true, .EXCL = true, .ACCMODE = .RDWR }))),
                0o600,
            );
            if (fd < 0) {
                return error.ShmOpenFailed;
            }

            // Set size
            posix.ftruncate(@bitCast(fd), @intCast(TOTAL_SIZE)) catch {
                _ = posix.close(@bitCast(fd));
                _ = std.c.shm_unlink(name_slice);
                return error.FtruncateFailed;
            };
        } else {
            // Client: attach to existing
            fd = std.c.shm_open(
                name_slice,
                @bitCast(@as(u32, @bitCast(posix.O{ .ACCMODE = .RDWR }))),
                0,
            );
            if (fd < 0) {
                return error.ShmNotFound;
            }
        }

        // Map into memory
        const base_ptr = posix.mmap(
            null,
            TOTAL_SIZE,
            posix.PROT.READ | posix.PROT.WRITE,
            .{ .TYPE = .SHARED },
            @bitCast(fd),
            0,
        ) catch {
            _ = posix.close(@bitCast(fd));
            if (is_server) {
                _ = std.c.shm_unlink(name_slice);
            }
            return error.MmapFailed;
        };

        const base: [*]align(4096) u8 = @ptrCast(@alignCast(base_ptr));
        const header: *Header = @ptrCast(@alignCast(base));

        if (is_server) {
            // Initialize header
            header.magic = MAGIC;
            header.version = VERSION;
            header.req_head = std.atomic.Value(u32).init(0);
            header.req_tail = std.atomic.Value(u32).init(0);
            header.resp_head = std.atomic.Value(u32).init(0);
            header.resp_tail = std.atomic.Value(u32).init(0);
            header.server_ready = std.atomic.Value(u32).init(0);
            header.next_req_id = std.atomic.Value(u32).init(1);
            @memset(&header._reserved, 0);
        } else {
            // Validate header
            if (header.magic != MAGIC or header.version != VERSION) {
                const mapped: []align(std.heap.page_size_min) u8 = @as([*]align(std.heap.page_size_min) u8, @ptrCast(@alignCast(base)))[0..TOTAL_SIZE];
                posix.munmap(mapped);
                _ = posix.close(@bitCast(fd));
                return error.InvalidHeader;
            }
        }

        return Self{
            .base = base,
            .header = header,
            .req_ring = base[HEADER_SIZE .. HEADER_SIZE + REQUEST_RING_SIZE],
            .resp_ring = base[HEADER_SIZE + REQUEST_RING_SIZE ..][0..RESPONSE_RING_SIZE],
            .fd = @bitCast(fd),
            .is_server = is_server,
            .shm_name = shm_name,
            .shm_name_len = shm_name_len,
        };
    }

    /// Close and cleanup shared memory
    pub fn close(self: *Self) void {
        if (self.is_server) {
            self.header.server_ready.store(0, .release);
            const name_slice: [*:0]const u8 = @ptrCast(&self.shm_name);
            _ = std.c.shm_unlink(name_slice);
        }

        const mapped: []align(std.heap.page_size_min) u8 = @as([*]align(std.heap.page_size_min) u8, @ptrCast(@alignCast(self.base)))[0..TOTAL_SIZE];
        posix.munmap(mapped);
        posix.close(self.fd);
    }

    /// Mark server as ready (called by server after init)
    pub fn markServerReady(self: *Self) void {
        self.header.server_ready.store(1, .release);
    }

    /// Check if server is ready (called by client)
    pub fn isServerReady(self: *Self) bool {
        return self.header.server_ready.load(.acquire) == 1;
    }

    // ========== Client Operations ==========

    /// Send a request and return the request ID
    pub fn sendRequest(self: *Self, path: []const u8, args: []const []const u8) !u32 {
        // Calculate total size needed
        var total_size: usize = 9; // state(1) + id(4) + path_len(4)
        total_size += path.len;
        total_size += 4; // args_count
        for (args) |arg| {
            total_size += 4 + arg.len;
        }

        if (total_size > MAX_REQUEST_SIZE) {
            return error.RequestTooLarge;
        }

        // Allocate slot in ring buffer (simple bump allocator)
        const head = self.header.req_head.load(.acquire);
        if (head + total_size > REQUEST_RING_SIZE) {
            // Ring is full or wrapping - wait for tail to catch up
            // For simplicity, we reset when full (single-producer assumption)
            const tail = self.header.req_tail.load(.acquire);
            if (tail == 0) {
                return error.RingBufferFull;
            }
            // Reset head if tail has consumed
            self.header.req_head.store(0, .release);
        }

        const offset = self.header.req_head.fetchAdd(@intCast(total_size), .acq_rel);
        if (offset + total_size > REQUEST_RING_SIZE) {
            return error.RingBufferFull;
        }

        const req_id = self.header.next_req_id.fetchAdd(1, .acq_rel);

        // Write request to ring buffer
        var buf = self.req_ring[offset..][0..total_size];
        var pos: usize = 0;

        // State: writing
        buf[pos] = @intFromEnum(EntryState.writing);
        pos += 1;

        // Request ID
        std.mem.writeInt(u32, buf[pos..][0..4], req_id, .little);
        pos += 4;

        // Path length and data
        std.mem.writeInt(u32, buf[pos..][0..4], @intCast(path.len), .little);
        pos += 4;
        @memcpy(buf[pos..][0..path.len], path);
        pos += path.len;

        // Args count
        std.mem.writeInt(u32, buf[pos..][0..4], @intCast(args.len), .little);
        pos += 4;

        // Args data
        for (args) |arg| {
            std.mem.writeInt(u32, buf[pos..][0..4], @intCast(arg.len), .little);
            pos += 4;
            @memcpy(buf[pos..][0..arg.len], arg);
            pos += arg.len;
        }

        // Mark as ready (atomic store with release ordering)
        @atomicStore(u8, &buf[0], @intFromEnum(EntryState.ready), .release);

        return req_id;
    }

    /// Wait for a response by request ID
    pub fn waitResponse(self: *Self, req_id: u32, timeout_ms: u32) !?Response {
        const start = std.time.milliTimestamp();
        const deadline = start + timeout_ms;

        while (std.time.milliTimestamp() < deadline) {
            // Scan response ring for matching request ID
            var offset: usize = 0;
            while (offset < RESPONSE_RING_SIZE) {
                const state = @as(EntryState, @enumFromInt(self.resp_ring[offset]));
                if (state != .ready) {
                    offset += 1;
                    continue;
                }

                // Check request ID
                const entry_id = std.mem.readInt(u32, self.resp_ring[offset + 1 ..][0..4], .little);
                if (entry_id != req_id) {
                    // Skip to next potential entry (estimate based on header)
                    offset += 13; // Minimum entry size
                    continue;
                }

                // Found matching response - use atomic load to ensure we see the producer's writes
                _ = @atomicLoad(u8, &self.resp_ring[offset], .acquire);

                // Mark as reading
                self.resp_ring[offset] = @intFromEnum(EntryState.reading);

                const exit_code = std.mem.readInt(i32, self.resp_ring[offset + 5 ..][0..4], .little);
                const output_len = std.mem.readInt(u32, self.resp_ring[offset + 9 ..][0..4], .little);
                const output = self.resp_ring[offset + 13 ..][0..output_len];

                return Response{
                    .id = req_id,
                    .exit_code = exit_code,
                    .output = output,
                    .offset = offset,
                };
            }

            // Yield to avoid busy spinning
            std.Thread.yield() catch {};
        }

        return null; // Timeout
    }

    /// Mark a response as consumed (free the slot)
    pub fn ackResponse(self: *Self, resp: *const Response) void {
        self.resp_ring[resp.offset] = @intFromEnum(EntryState.empty);
    }

    // ========== Server Operations ==========

    /// Poll for a ready request (non-blocking)
    pub fn pollRequest(self: *Self, allocator: std.mem.Allocator) ?Request {
        var offset: usize = 0;
        while (offset < REQUEST_RING_SIZE) {
            const state = @as(EntryState, @enumFromInt(self.req_ring[offset]));
            if (state != .ready) {
                offset += 1;
                continue;
            }

            // Found a ready request - use atomic load to ensure we see the producer's writes
            _ = @atomicLoad(u8, &self.req_ring[offset], .acquire);

            // Mark as reading
            self.req_ring[offset] = @intFromEnum(EntryState.reading);

            var pos = offset + 1;

            // Request ID
            const req_id = std.mem.readInt(u32, self.req_ring[pos..][0..4], .little);
            pos += 4;

            // Path
            const path_len = std.mem.readInt(u32, self.req_ring[pos..][0..4], .little);
            pos += 4;
            const path = self.req_ring[pos..][0..path_len];
            pos += path_len;

            // Args count
            const args_count = std.mem.readInt(u32, self.req_ring[pos..][0..4], .little);
            pos += 4;

            // Parse args
            var req = Request{
                .id = req_id,
                .path = path,
                .args = &.{},
                .offset = offset,
            };

            if (args_count > 0 and args_count <= 32) {
                for (0..args_count) |i| {
                    const arg_len = std.mem.readInt(u32, self.req_ring[pos..][0..4], .little);
                    pos += 4;
                    req.args_storage[i] = self.req_ring[pos..][0..arg_len];
                    pos += arg_len;
                }
                req.args = req.args_storage[0..args_count];
            }

            _ = allocator; // Reserved for future heap allocation

            return req;
        }

        return null;
    }

    /// Send a response for a request
    pub fn sendResponse(self: *Self, req_id: u32, exit_code: i32, output: []const u8) !void {
        const output_len = @min(output.len, MAX_RESPONSE_SIZE);
        const total_size: usize = 13 + output_len; // state(1) + id(4) + exit(4) + len(4) + data

        // Find empty slot in response ring
        const head = self.header.resp_head.load(.acquire);
        var offset = head;

        // Simple allocation (TODO: proper ring management)
        if (offset + total_size > RESPONSE_RING_SIZE) {
            offset = 0;
        }

        // Write response
        var buf = self.resp_ring[offset..][0..total_size];
        var pos: usize = 0;

        // State: writing
        buf[pos] = @intFromEnum(EntryState.writing);
        pos += 1;

        // Request ID
        std.mem.writeInt(u32, buf[pos..][0..4], req_id, .little);
        pos += 4;

        // Exit code
        std.mem.writeInt(i32, buf[pos..][0..4], exit_code, .little);
        pos += 4;

        // Output length
        std.mem.writeInt(u32, buf[pos..][0..4], @intCast(output_len), .little);
        pos += 4;

        // Output data
        @memcpy(buf[pos..][0..output_len], output[0..output_len]);

        // Update head
        _ = self.header.resp_head.fetchAdd(@intCast(total_size), .acq_rel);

        // Mark as ready (atomic store with release ordering)
        @atomicStore(u8, &buf[0], @intFromEnum(EntryState.ready), .release);
    }

    /// Mark a request as consumed
    pub fn ackRequest(self: *Self, req: *const Request) void {
        self.req_ring[req.offset] = @intFromEnum(EntryState.empty);
    }
};

// ========== Tests ==========

test "SharedRing header size" {
    try std.testing.expectEqual(@as(usize, 64), @sizeOf(Header));
}

test "SharedRing getShmName" {
    const name = SharedRing.getShmName();
    try std.testing.expect(std.mem.startsWith(u8, &name, "/edgebox-"));
}
