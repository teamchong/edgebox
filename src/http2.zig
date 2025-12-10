/// HTTP/2 Protocol Implementation
/// RFC 7540 - Hypertext Transfer Protocol Version 2 (HTTP/2)
/// RFC 7541 - HPACK: Header Compression for HTTP/2
const std = @import("std");

pub const Http2Error = error{
    ConnectionError,
    StreamError,
    FrameError,
    CompressionError,
    FlowControlError,
    ProtocolError,
    OutOfMemory,
};

// =============================================================================
// HTTP/2 Frame Types (RFC 7540 Section 6)
// =============================================================================

pub const FrameType = enum(u8) {
    data = 0x0,
    headers = 0x1,
    priority = 0x2,
    rst_stream = 0x3,
    settings = 0x4,
    push_promise = 0x5,
    ping = 0x6,
    goaway = 0x7,
    window_update = 0x8,
    continuation = 0x9,
};

pub const FrameFlags = struct {
    pub const END_STREAM: u8 = 0x1;
    pub const END_HEADERS: u8 = 0x4;
    pub const PADDED: u8 = 0x8;
    pub const PRIORITY: u8 = 0x20;
    pub const ACK: u8 = 0x1; // For SETTINGS and PING
};

pub const ErrorCode = enum(u32) {
    no_error = 0x0,
    protocol_error = 0x1,
    internal_error = 0x2,
    flow_control_error = 0x3,
    settings_timeout = 0x4,
    stream_closed = 0x5,
    frame_size_error = 0x6,
    refused_stream = 0x7,
    cancel = 0x8,
    compression_error = 0x9,
    connect_error = 0xa,
    enhance_your_calm = 0xb,
    inadequate_security = 0xc,
    http_1_1_required = 0xd,
};

// =============================================================================
// HTTP/2 Settings (RFC 7540 Section 6.5.2)
// =============================================================================

pub const SettingsId = enum(u16) {
    header_table_size = 0x1,
    enable_push = 0x2,
    max_concurrent_streams = 0x3,
    initial_window_size = 0x4,
    max_frame_size = 0x5,
    max_header_list_size = 0x6,
};

pub const Settings = struct {
    header_table_size: u32 = 4096,
    enable_push: bool = true,
    max_concurrent_streams: u32 = 100,
    initial_window_size: u32 = 65535,
    max_frame_size: u32 = 16384,
    max_header_list_size: u32 = 8192,
};

// =============================================================================
// HTTP/2 Frame Header (9 bytes)
// =============================================================================

pub const FrameHeader = struct {
    length: u24,
    frame_type: FrameType,
    flags: u8,
    stream_id: u31,

    pub fn encode(self: FrameHeader, buf: *[9]u8) void {
        // Length (24 bits)
        buf[0] = @truncate(self.length >> 16);
        buf[1] = @truncate(self.length >> 8);
        buf[2] = @truncate(self.length);
        // Type
        buf[3] = @intFromEnum(self.frame_type);
        // Flags
        buf[4] = self.flags;
        // Stream ID (31 bits, R bit = 0)
        buf[5] = @truncate(self.stream_id >> 24);
        buf[6] = @truncate(self.stream_id >> 16);
        buf[7] = @truncate(self.stream_id >> 8);
        buf[8] = @truncate(self.stream_id);
    }

    pub fn decode(buf: *const [9]u8) FrameHeader {
        return .{
            .length = (@as(u24, buf[0]) << 16) | (@as(u24, buf[1]) << 8) | @as(u24, buf[2]),
            .frame_type = @enumFromInt(buf[3]),
            .flags = buf[4],
            .stream_id = @truncate((@as(u32, buf[5] & 0x7F) << 24) | (@as(u32, buf[6]) << 16) | (@as(u32, buf[7]) << 8) | @as(u32, buf[8])),
        };
    }
};

// =============================================================================
// HPACK Header Compression (RFC 7541)
// =============================================================================

pub const Hpack = struct {
    // Static table (RFC 7541 Appendix A)
    const static_table = [_]struct { name: []const u8, value: []const u8 }{
        .{ .name = ":authority", .value = "" },
        .{ .name = ":method", .value = "GET" },
        .{ .name = ":method", .value = "POST" },
        .{ .name = ":path", .value = "/" },
        .{ .name = ":path", .value = "/index.html" },
        .{ .name = ":scheme", .value = "http" },
        .{ .name = ":scheme", .value = "https" },
        .{ .name = ":status", .value = "200" },
        .{ .name = ":status", .value = "204" },
        .{ .name = ":status", .value = "206" },
        .{ .name = ":status", .value = "304" },
        .{ .name = ":status", .value = "400" },
        .{ .name = ":status", .value = "404" },
        .{ .name = ":status", .value = "500" },
        .{ .name = "accept-charset", .value = "" },
        .{ .name = "accept-encoding", .value = "gzip, deflate" },
        .{ .name = "accept-language", .value = "" },
        .{ .name = "accept-ranges", .value = "" },
        .{ .name = "accept", .value = "" },
        .{ .name = "access-control-allow-origin", .value = "" },
        .{ .name = "age", .value = "" },
        .{ .name = "allow", .value = "" },
        .{ .name = "authorization", .value = "" },
        .{ .name = "cache-control", .value = "" },
        .{ .name = "content-disposition", .value = "" },
        .{ .name = "content-encoding", .value = "" },
        .{ .name = "content-language", .value = "" },
        .{ .name = "content-length", .value = "" },
        .{ .name = "content-location", .value = "" },
        .{ .name = "content-range", .value = "" },
        .{ .name = "content-type", .value = "" },
        .{ .name = "cookie", .value = "" },
        .{ .name = "date", .value = "" },
        .{ .name = "etag", .value = "" },
        .{ .name = "expect", .value = "" },
        .{ .name = "expires", .value = "" },
        .{ .name = "from", .value = "" },
        .{ .name = "host", .value = "" },
        .{ .name = "if-match", .value = "" },
        .{ .name = "if-modified-since", .value = "" },
        .{ .name = "if-none-match", .value = "" },
        .{ .name = "if-range", .value = "" },
        .{ .name = "if-unmodified-since", .value = "" },
        .{ .name = "last-modified", .value = "" },
        .{ .name = "link", .value = "" },
        .{ .name = "location", .value = "" },
        .{ .name = "max-forwards", .value = "" },
        .{ .name = "proxy-authenticate", .value = "" },
        .{ .name = "proxy-authorization", .value = "" },
        .{ .name = "range", .value = "" },
        .{ .name = "referer", .value = "" },
        .{ .name = "refresh", .value = "" },
        .{ .name = "retry-after", .value = "" },
        .{ .name = "server", .value = "" },
        .{ .name = "set-cookie", .value = "" },
        .{ .name = "strict-transport-security", .value = "" },
        .{ .name = "transfer-encoding", .value = "" },
        .{ .name = "user-agent", .value = "" },
        .{ .name = "vary", .value = "" },
        .{ .name = "via", .value = "" },
        .{ .name = "www-authenticate", .value = "" },
    };

    // Dynamic table
    allocator: std.mem.Allocator,
    dynamic_table: std.ArrayList(struct { name: []u8, value: []u8 }),
    dynamic_table_size: usize = 0,
    max_dynamic_table_size: usize = 4096,

    pub fn init(allocator: std.mem.Allocator) Hpack {
        return .{
            .allocator = allocator,
            .dynamic_table = std.ArrayList(struct { name: []u8, value: []u8 }).init(allocator),
        };
    }

    pub fn deinit(self: *Hpack) void {
        for (self.dynamic_table.items) |entry| {
            self.allocator.free(entry.name);
            self.allocator.free(entry.value);
        }
        self.dynamic_table.deinit();
    }

    /// Encode integer with HPACK prefix (RFC 7541 Section 5.1)
    pub fn encodeInteger(value: u64, prefix_bits: u3, buf: []u8) usize {
        const max_prefix = (@as(u8, 1) << prefix_bits) - 1;
        if (value < max_prefix) {
            buf[0] = @truncate(value);
            return 1;
        }

        buf[0] = max_prefix;
        var remaining = value - max_prefix;
        var pos: usize = 1;

        while (remaining >= 128) {
            buf[pos] = @truncate((remaining & 0x7F) | 0x80);
            remaining >>= 7;
            pos += 1;
        }
        buf[pos] = @truncate(remaining);
        return pos + 1;
    }

    /// Decode integer with HPACK prefix
    pub fn decodeInteger(data: []const u8, prefix_bits: u3) struct { value: u64, bytes_read: usize } {
        const max_prefix = (@as(u8, 1) << prefix_bits) - 1;
        const first = data[0] & max_prefix;

        if (first < max_prefix) {
            return .{ .value = first, .bytes_read = 1 };
        }

        var value: u64 = max_prefix;
        var shift: u6 = 0;
        var pos: usize = 1;

        while (pos < data.len) {
            const b = data[pos];
            value += @as(u64, b & 0x7F) << shift;
            pos += 1;
            if (b & 0x80 == 0) break;
            shift += 7;
        }

        return .{ .value = value, .bytes_read = pos };
    }

    /// Encode string literal (RFC 7541 Section 5.2)
    pub fn encodeString(self: *Hpack, s: []const u8, buf: []u8, huffman: bool) usize {
        _ = self;
        if (huffman) {
            // TODO: Huffman encoding
            // For now, use literal
        }

        // Literal string (H=0)
        const len_bytes = encodeInteger(s.len, 7, buf);
        @memcpy(buf[len_bytes..][0..s.len], s);
        return len_bytes + s.len;
    }

    /// Encode headers to HPACK format
    pub fn encodeHeaders(self: *Hpack, headers: []const struct { name: []const u8, value: []const u8 }, buf: []u8) usize {
        var pos: usize = 0;

        for (headers) |h| {
            // Check static table for indexed header
            var found_index: ?usize = null;
            var found_name_only = false;

            for (static_table, 0..) |entry, i| {
                if (std.mem.eql(u8, entry.name, h.name)) {
                    if (std.mem.eql(u8, entry.value, h.value)) {
                        found_index = i + 1;
                        break;
                    }
                    if (!found_name_only) {
                        found_index = i + 1;
                        found_name_only = true;
                    }
                }
            }

            if (found_index != null and !found_name_only) {
                // Indexed header field (RFC 7541 Section 6.1)
                buf[pos] = 0x80;
                pos += encodeInteger(found_index.?, 7, buf[pos..]);
            } else if (found_index != null) {
                // Literal with indexed name (RFC 7541 Section 6.2.1)
                buf[pos] = 0x40;
                pos += encodeInteger(found_index.?, 6, buf[pos..]);
                pos += self.encodeString(h.value, buf[pos..], false);
            } else {
                // Literal with new name (RFC 7541 Section 6.2.1)
                buf[pos] = 0x40;
                pos += 1;
                pos += self.encodeString(h.name, buf[pos..], false);
                pos += self.encodeString(h.value, buf[pos..], false);
            }
        }

        return pos;
    }

    /// Decode string literal
    fn decodeString(data: []const u8, allocator: std.mem.Allocator) !struct { value: []u8, bytes_read: usize } {
        const huffman = (data[0] & 0x80) != 0;
        const len_result = decodeInteger(data, 7);
        const str_len = len_result.value;
        const str_start = len_result.bytes_read;

        if (huffman) {
            // TODO: Huffman decoding
            return error.CompressionError;
        }

        const value = try allocator.alloc(u8, str_len);
        @memcpy(value, data[str_start..][0..str_len]);

        return .{ .value = value, .bytes_read = str_start + str_len };
    }

    /// Decode headers from HPACK format
    pub fn decodeHeaders(self: *Hpack, data: []const u8, allocator: std.mem.Allocator) !std.ArrayList(struct { name: []u8, value: []u8 }) {
        var headers = std.ArrayList(struct { name: []u8, value: []u8 }).init(allocator);
        errdefer {
            for (headers.items) |h| {
                allocator.free(h.name);
                allocator.free(h.value);
            }
            headers.deinit();
        }

        var pos: usize = 0;
        while (pos < data.len) {
            const first = data[pos];

            if (first & 0x80 != 0) {
                // Indexed header field
                const idx_result = decodeInteger(data[pos..], 7);
                pos += idx_result.bytes_read;
                const idx = idx_result.value;

                if (idx > 0 and idx <= static_table.len) {
                    const entry = static_table[idx - 1];
                    const name = try allocator.dupe(u8, entry.name);
                    const value = try allocator.dupe(u8, entry.value);
                    try headers.append(.{ .name = name, .value = value });
                } else if (idx > static_table.len) {
                    const dyn_idx = idx - static_table.len - 1;
                    if (dyn_idx < self.dynamic_table.items.len) {
                        const entry = self.dynamic_table.items[dyn_idx];
                        const name = try allocator.dupe(u8, entry.name);
                        const value = try allocator.dupe(u8, entry.value);
                        try headers.append(.{ .name = name, .value = value });
                    }
                }
            } else if (first & 0x40 != 0) {
                // Literal with incremental indexing
                const idx_result = decodeInteger(data[pos..], 6);
                pos += idx_result.bytes_read;

                var name: []u8 = undefined;
                if (idx_result.value > 0) {
                    if (idx_result.value <= static_table.len) {
                        name = try allocator.dupe(u8, static_table[idx_result.value - 1].name);
                    }
                } else {
                    const name_result = try decodeString(data[pos..], allocator);
                    name = name_result.value;
                    pos += name_result.bytes_read;
                }

                const value_result = try decodeString(data[pos..], allocator);
                pos += value_result.bytes_read;

                try headers.append(.{ .name = name, .value = value_result.value });
            } else {
                // Other literal representations
                pos += 1;
            }
        }

        return headers;
    }
};

// =============================================================================
// HTTP/2 Connection
// =============================================================================

pub const Http2Connection = struct {
    allocator: std.mem.Allocator,
    hpack_encoder: Hpack,
    hpack_decoder: Hpack,
    local_settings: Settings,
    remote_settings: Settings,
    next_stream_id: u31,
    recv_window: i64,
    send_window: i64,

    // Callbacks
    write_fn: *const fn (data: []const u8) anyerror!void,
    read_fn: *const fn (buf: []u8) anyerror!usize,

    const CONNECTION_PREFACE = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n";

    pub fn init(
        allocator: std.mem.Allocator,
        write_fn: *const fn (data: []const u8) anyerror!void,
        read_fn: *const fn (buf: []u8) anyerror!usize,
        is_server: bool,
    ) Http2Connection {
        return .{
            .allocator = allocator,
            .hpack_encoder = Hpack.init(allocator),
            .hpack_decoder = Hpack.init(allocator),
            .local_settings = .{},
            .remote_settings = .{},
            .next_stream_id = if (is_server) 2 else 1,
            .recv_window = 65535,
            .send_window = 65535,
            .write_fn = write_fn,
            .read_fn = read_fn,
        };
    }

    pub fn deinit(self: *Http2Connection) void {
        self.hpack_encoder.deinit();
        self.hpack_decoder.deinit();
    }

    /// Send connection preface (client)
    pub fn sendPreface(self: *Http2Connection) !void {
        try self.write_fn(CONNECTION_PREFACE);
        try self.sendSettings();
    }

    /// Send SETTINGS frame
    pub fn sendSettings(self: *Http2Connection) !void {
        var payload: [36]u8 = undefined;
        var pos: usize = 0;

        // SETTINGS_HEADER_TABLE_SIZE
        payload[pos] = 0;
        payload[pos + 1] = @intFromEnum(SettingsId.header_table_size);
        std.mem.writeInt(u32, payload[pos + 2 ..][0..4], self.local_settings.header_table_size, .big);
        pos += 6;

        // SETTINGS_MAX_CONCURRENT_STREAMS
        payload[pos] = 0;
        payload[pos + 1] = @intFromEnum(SettingsId.max_concurrent_streams);
        std.mem.writeInt(u32, payload[pos + 2 ..][0..4], self.local_settings.max_concurrent_streams, .big);
        pos += 6;

        // SETTINGS_INITIAL_WINDOW_SIZE
        payload[pos] = 0;
        payload[pos + 1] = @intFromEnum(SettingsId.initial_window_size);
        std.mem.writeInt(u32, payload[pos + 2 ..][0..4], self.local_settings.initial_window_size, .big);
        pos += 6;

        const header = FrameHeader{
            .length = @truncate(pos),
            .frame_type = .settings,
            .flags = 0,
            .stream_id = 0,
        };

        var frame_buf: [9 + 36]u8 = undefined;
        header.encode(frame_buf[0..9]);
        @memcpy(frame_buf[9..][0..pos], payload[0..pos]);

        try self.write_fn(frame_buf[0 .. 9 + pos]);
    }

    /// Send SETTINGS ACK
    pub fn sendSettingsAck(self: *Http2Connection) !void {
        const header = FrameHeader{
            .length = 0,
            .frame_type = .settings,
            .flags = FrameFlags.ACK,
            .stream_id = 0,
        };

        var buf: [9]u8 = undefined;
        header.encode(&buf);
        try self.write_fn(&buf);
    }

    /// Send HEADERS frame
    pub fn sendHeaders(
        self: *Http2Connection,
        stream_id: u31,
        headers: []const struct { name: []const u8, value: []const u8 },
        end_stream: bool,
    ) !void {
        var header_block: [16384]u8 = undefined;
        const header_len = self.hpack_encoder.encodeHeaders(headers, &header_block);

        const header = FrameHeader{
            .length = @truncate(header_len),
            .frame_type = .headers,
            .flags = FrameFlags.END_HEADERS | (if (end_stream) FrameFlags.END_STREAM else 0),
            .stream_id = stream_id,
        };

        var frame_header: [9]u8 = undefined;
        header.encode(&frame_header);

        try self.write_fn(&frame_header);
        try self.write_fn(header_block[0..header_len]);
    }

    /// Send DATA frame
    pub fn sendData(self: *Http2Connection, stream_id: u31, data: []const u8, end_stream: bool) !void {
        const header = FrameHeader{
            .length = @truncate(data.len),
            .frame_type = .data,
            .flags = if (end_stream) FrameFlags.END_STREAM else 0,
            .stream_id = stream_id,
        };

        var frame_header: [9]u8 = undefined;
        header.encode(&frame_header);

        try self.write_fn(&frame_header);
        if (data.len > 0) {
            try self.write_fn(data);
        }
    }

    /// Send GOAWAY frame
    pub fn sendGoaway(self: *Http2Connection, last_stream_id: u31, error_code: ErrorCode) !void {
        var payload: [8]u8 = undefined;
        std.mem.writeInt(u32, payload[0..4], last_stream_id, .big);
        std.mem.writeInt(u32, payload[4..8], @intFromEnum(error_code), .big);

        const header = FrameHeader{
            .length = 8,
            .frame_type = .goaway,
            .flags = 0,
            .stream_id = 0,
        };

        var buf: [17]u8 = undefined;
        header.encode(buf[0..9]);
        @memcpy(buf[9..17], &payload);

        try self.write_fn(&buf);
    }

    /// Read and process a frame
    pub fn readFrame(self: *Http2Connection) !struct {
        header: FrameHeader,
        payload: []u8,
    } {
        var header_buf: [9]u8 = undefined;
        const header_read = try self.read_fn(&header_buf);
        if (header_read < 9) return error.ConnectionError;

        const header = FrameHeader.decode(&header_buf);

        const payload = try self.allocator.alloc(u8, header.length);
        errdefer self.allocator.free(payload);

        if (header.length > 0) {
            var total_read: usize = 0;
            while (total_read < header.length) {
                const n = try self.read_fn(payload[total_read..]);
                if (n == 0) return error.ConnectionError;
                total_read += n;
            }
        }

        return .{ .header = header, .payload = payload };
    }

    /// Allocate next stream ID
    pub fn nextStreamId(self: *Http2Connection) u31 {
        const id = self.next_stream_id;
        self.next_stream_id += 2;
        return id;
    }
};

// =============================================================================
// Tests
// =============================================================================

test "frame header encode/decode" {
    const header = FrameHeader{
        .length = 1234,
        .frame_type = .headers,
        .flags = FrameFlags.END_HEADERS | FrameFlags.END_STREAM,
        .stream_id = 5,
    };

    var buf: [9]u8 = undefined;
    header.encode(&buf);

    const decoded = FrameHeader.decode(&buf);
    try std.testing.expectEqual(header.length, decoded.length);
    try std.testing.expectEqual(header.frame_type, decoded.frame_type);
    try std.testing.expectEqual(header.flags, decoded.flags);
    try std.testing.expectEqual(header.stream_id, decoded.stream_id);
}

test "hpack integer encoding" {
    var buf: [10]u8 = undefined;

    // Small value fits in prefix
    const len1 = Hpack.encodeInteger(10, 5, &buf);
    try std.testing.expectEqual(@as(usize, 1), len1);
    try std.testing.expectEqual(@as(u8, 10), buf[0]);

    // Value requires continuation
    const len2 = Hpack.encodeInteger(1337, 5, &buf);
    try std.testing.expect(len2 > 1);

    const result = Hpack.decodeInteger(buf[0..len2], 5);
    try std.testing.expectEqual(@as(u64, 1337), result.value);
}
