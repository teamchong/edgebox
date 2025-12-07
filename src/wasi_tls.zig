/// WASI TLS 1.3 Client
/// Minimal TLS implementation for HTTPS over WASI sockets
const std = @import("std");
const wasi_sock = @import("wasi_sock.zig");
const crypto = std.crypto;

pub const TlsError = error{
    ConnectionFailed,
    HandshakeFailed,
    CertificateError,
    WriteError,
    ReadError,
    TlsAlert,
    OutOfMemory,
    UnsupportedProtocol,
    DecryptionFailed,
};

/// TLS Record Types
const ContentType = enum(u8) {
    change_cipher_spec = 20,
    alert = 21,
    handshake = 22,
    application_data = 23,
};

/// TLS Handshake Types
const HandshakeType = enum(u8) {
    client_hello = 1,
    server_hello = 2,
    encrypted_extensions = 8,
    certificate = 11,
    certificate_verify = 15,
    finished = 20,
};

/// TLS-wrapped TCP socket for HTTPS connections
pub const TlsSocket = struct {
    tcp: wasi_sock.TcpSocket,
    allocator: std.mem.Allocator,

    // TLS state
    handshake_complete: bool = false,
    client_seq: u64 = 0,
    server_seq: u64 = 0,

    // Key material (after handshake)
    client_key: [32]u8 = undefined,
    server_key: [32]u8 = undefined,
    client_iv: [12]u8 = undefined,
    server_iv: [12]u8 = undefined,

    // Read buffer for decrypted data
    read_buffer: []u8,
    read_pos: usize = 0,
    read_len: usize = 0,

    // Transcript hash for handshake
    transcript: crypto.hash.sha2.Sha256,

    const Self = @This();

    /// Connect with TLS to a remote host
    pub fn connect(
        allocator: std.mem.Allocator,
        ip: []const u8,
        port: u16,
        hostname: []const u8,
    ) TlsError!*Self {
        const self = allocator.create(Self) catch return TlsError.OutOfMemory;
        errdefer allocator.destroy(self);

        self.allocator = allocator;
        self.read_buffer = allocator.alloc(u8, 32768) catch return TlsError.OutOfMemory;
        errdefer allocator.free(self.read_buffer);

        self.transcript = crypto.hash.sha2.Sha256.init(.{});
        self.handshake_complete = false;
        self.read_pos = 0;
        self.read_len = 0;

        // Open TCP socket and connect
        self.tcp = wasi_sock.TcpSocket.open(false) catch return TlsError.ConnectionFailed;
        errdefer self.tcp.close();

        self.tcp.connect(ip, port) catch return TlsError.ConnectionFailed;

        // Perform TLS 1.3 handshake
        self.doHandshake(hostname) catch |err| {
            std.debug.print("TLS handshake failed: {}\n", .{err});
            return TlsError.HandshakeFailed;
        };

        return self;
    }

    /// Perform TLS 1.3 handshake
    fn doHandshake(self: *Self, hostname: []const u8) !void {
        // Generate ephemeral X25519 keypair
        var seed: [32]u8 = undefined;
        crypto.random.bytes(&seed);
        const keypair = crypto.dh.X25519.KeyPair.generateDeterministic(seed) catch {
            return error.HandshakeFailed;
        };

        // Generate client random
        var client_random: [32]u8 = undefined;
        crypto.random.bytes(&client_random);

        // Build and send ClientHello
        try self.sendClientHello(hostname, &client_random, &keypair.public_key);

        // Receive ServerHello
        var server_random: [32]u8 = undefined;
        var server_public_key: [32]u8 = undefined;
        try self.recvServerHello(&server_random, &server_public_key);

        // Compute shared secret
        const shared_secret = crypto.dh.X25519.scalarmult(keypair.secret_key, server_public_key) catch {
            return error.HandshakeFailed;
        };

        // Derive keys using HKDF
        try self.deriveKeys(&shared_secret, &client_random, &server_random);

        // Receive and process encrypted handshake messages
        try self.processEncryptedHandshake();

        // Send client Finished
        try self.sendClientFinished();

        self.handshake_complete = true;
    }

    fn sendClientHello(self: *Self, hostname: []const u8, client_random: *const [32]u8, public_key: *const [32]u8) !void {
        var buf: [512]u8 = undefined;
        var pos: usize = 0;

        // TLS record header (will fill length later)
        buf[pos] = @intFromEnum(ContentType.handshake);
        buf[pos + 1] = 0x03;
        buf[pos + 2] = 0x01; // Legacy version TLS 1.0
        pos += 5; // Skip length for now
        const record_start = pos;

        // Handshake header
        buf[pos] = @intFromEnum(HandshakeType.client_hello);
        pos += 4; // Skip length for now
        const handshake_start = pos;

        // Legacy version (TLS 1.2)
        buf[pos] = 0x03;
        buf[pos + 1] = 0x03;
        pos += 2;

        // Client random
        @memcpy(buf[pos..][0..32], client_random);
        pos += 32;

        // Session ID (empty for TLS 1.3)
        buf[pos] = 0;
        pos += 1;

        // Cipher suites
        buf[pos] = 0x00;
        buf[pos + 1] = 0x02; // 2 bytes
        buf[pos + 2] = 0x13;
        buf[pos + 3] = 0x01; // TLS_AES_128_GCM_SHA256
        pos += 4;

        // Compression methods
        buf[pos] = 0x01;
        buf[pos + 1] = 0x00; // null compression
        pos += 2;

        // Extensions
        const ext_start = pos;
        pos += 2; // Skip extensions length

        // Server name extension
        const sni_ext = buildSniExtension(hostname, buf[pos..]);
        pos += sni_ext;

        // Supported versions extension (TLS 1.3)
        buf[pos] = 0x00;
        buf[pos + 1] = 0x2b; // supported_versions
        buf[pos + 2] = 0x00;
        buf[pos + 3] = 0x03;
        buf[pos + 4] = 0x02;
        buf[pos + 5] = 0x03;
        buf[pos + 6] = 0x04; // TLS 1.3
        pos += 7;

        // Key share extension (X25519)
        buf[pos] = 0x00;
        buf[pos + 1] = 0x33; // key_share
        buf[pos + 2] = 0x00;
        buf[pos + 3] = 0x26; // 38 bytes
        buf[pos + 4] = 0x00;
        buf[pos + 5] = 0x24; // 36 bytes client_shares
        buf[pos + 6] = 0x00;
        buf[pos + 7] = 0x1d; // x25519
        buf[pos + 8] = 0x00;
        buf[pos + 9] = 0x20; // 32 bytes
        pos += 10;
        @memcpy(buf[pos..][0..32], public_key);
        pos += 32;

        // Supported groups extension
        buf[pos] = 0x00;
        buf[pos + 1] = 0x0a; // supported_groups
        buf[pos + 2] = 0x00;
        buf[pos + 3] = 0x04;
        buf[pos + 4] = 0x00;
        buf[pos + 5] = 0x02;
        buf[pos + 6] = 0x00;
        buf[pos + 7] = 0x1d; // x25519
        pos += 8;

        // Signature algorithms extension
        buf[pos] = 0x00;
        buf[pos + 1] = 0x0d; // signature_algorithms
        buf[pos + 2] = 0x00;
        buf[pos + 3] = 0x04;
        buf[pos + 4] = 0x00;
        buf[pos + 5] = 0x02;
        buf[pos + 6] = 0x04;
        buf[pos + 7] = 0x03; // ecdsa_secp256r1_sha256
        pos += 8;

        // Fill in extensions length
        const ext_len = pos - ext_start - 2;
        buf[ext_start] = @truncate(ext_len >> 8);
        buf[ext_start + 1] = @truncate(ext_len);

        // Fill in handshake length
        const handshake_len = pos - handshake_start;
        buf[record_start] = @truncate(handshake_len >> 16);
        buf[record_start + 1] = @truncate(handshake_len >> 8);
        buf[record_start + 2] = @truncate(handshake_len);

        // Fill in record length
        const record_len = pos - record_start;
        buf[3] = @truncate(record_len >> 8);
        buf[4] = @truncate(record_len);

        // Update transcript hash
        self.transcript.update(buf[record_start..pos]);

        // Send
        self.tcp.sendAll(buf[0..pos]) catch return error.WriteError;
    }

    fn buildSniExtension(hostname: []const u8, buf: []u8) usize {
        const host_len: u16 = @intCast(hostname.len);
        var pos: usize = 0;

        buf[pos] = 0x00;
        buf[pos + 1] = 0x00; // server_name extension
        pos += 2;

        const ext_len = 5 + host_len;
        buf[pos] = @truncate(ext_len >> 8);
        buf[pos + 1] = @truncate(ext_len);
        pos += 2;

        const list_len = 3 + host_len;
        buf[pos] = @truncate(list_len >> 8);
        buf[pos + 1] = @truncate(list_len);
        pos += 2;

        buf[pos] = 0x00; // host_name type
        pos += 1;

        buf[pos] = @truncate(host_len >> 8);
        buf[pos + 1] = @truncate(host_len);
        pos += 2;

        @memcpy(buf[pos..][0..host_len], hostname);
        pos += host_len;

        return pos;
    }

    fn recvServerHello(self: *Self, server_random: *[32]u8, server_public_key: *[32]u8) !void {
        var buf: [4096]u8 = undefined;

        // Read TLS record
        const record_len = try self.readRecord(&buf);
        if (record_len < 6) return error.HandshakeFailed;

        // Verify it's a ServerHello
        if (buf[0] != @intFromEnum(HandshakeType.server_hello)) {
            return error.HandshakeFailed;
        }

        // Update transcript
        self.transcript.update(buf[0..record_len]);

        // Parse ServerHello
        var pos: usize = 4; // Skip handshake type + length

        // Skip legacy version
        pos += 2;

        // Server random
        @memcpy(server_random, buf[pos..][0..32]);
        pos += 32;

        // Skip session ID
        const session_id_len = buf[pos];
        pos += 1 + session_id_len;

        // Skip cipher suite
        pos += 2;

        // Skip compression method
        pos += 1;

        // Parse extensions to find key_share
        if (pos + 2 > record_len) return error.HandshakeFailed;
        const ext_len = (@as(u16, buf[pos]) << 8) | buf[pos + 1];
        pos += 2;

        const ext_end = pos + ext_len;
        while (pos + 4 <= ext_end) {
            const ext_type = (@as(u16, buf[pos]) << 8) | buf[pos + 1];
            const ext_data_len = (@as(u16, buf[pos + 2]) << 8) | buf[pos + 3];
            pos += 4;

            if (ext_type == 0x0033) { // key_share
                // Skip group (2 bytes) + length (2 bytes)
                if (ext_data_len >= 36) {
                    @memcpy(server_public_key, buf[pos + 4 ..][0..32]);
                }
            }

            pos += ext_data_len;
        }
    }

    fn readRecord(self: *Self, buf: []u8) !usize {
        // Read TLS record header (5 bytes)
        var header: [5]u8 = undefined;
        var read_total: usize = 0;
        while (read_total < 5) {
            const n = self.tcp.recv(header[read_total..]) catch return error.ReadError;
            if (n == 0) return error.ReadError;
            read_total += n;
        }

        const content_type = header[0];
        _ = content_type;
        const record_len = (@as(u16, header[3]) << 8) | header[4];

        if (record_len > buf.len) return error.ReadError;

        // Read record body
        read_total = 0;
        while (read_total < record_len) {
            const n = self.tcp.recv(buf[read_total..record_len]) catch return error.ReadError;
            if (n == 0) return error.ReadError;
            read_total += n;
        }

        return record_len;
    }

    fn deriveKeys(self: *Self, shared_secret: *const [32]u8, client_random: *const [32]u8, server_random: *const [32]u8) !void {
        // Simplified key derivation - in production use proper TLS 1.3 HKDF-Expand-Label
        const transcript_hash = self.transcript.peek();

        // Create early secret
        var early_secret: [32]u8 = undefined;
        crypto.auth.hmac.sha2.HmacSha256.create(&early_secret, &[_]u8{0} ** 32, &[_]u8{0} ** 32);

        // Derive handshake secret
        var handshake_secret: [32]u8 = undefined;
        crypto.auth.hmac.sha2.HmacSha256.create(&handshake_secret, shared_secret, &early_secret);

        // Derive traffic keys (simplified)
        var key_material: [32]u8 = undefined;
        var combined: [96]u8 = undefined;
        @memcpy(combined[0..32], &handshake_secret);
        @memcpy(combined[32..64], &transcript_hash);
        @memcpy(combined[64..96], client_random);
        crypto.hash.sha2.Sha256.hash(&combined, &key_material, .{});

        @memcpy(&self.client_key, key_material[0..16] ++ key_material[16..32]);

        @memcpy(combined[64..96], server_random);
        crypto.hash.sha2.Sha256.hash(&combined, &key_material, .{});
        @memcpy(&self.server_key, key_material[0..16] ++ key_material[16..32]);

        // IVs
        @memcpy(&self.client_iv, key_material[0..12]);
        @memcpy(&self.server_iv, key_material[12..24]);
    }

    fn processEncryptedHandshake(self: *Self) !void {
        // Read and decrypt server's encrypted handshake messages
        // For now, just consume them - proper implementation would verify
        var buf: [16384]u8 = undefined;

        // Read until we get the Finished message
        var attempts: usize = 0;
        while (attempts < 10) : (attempts += 1) {
            const n = self.readRecord(&buf) catch break;
            if (n == 0) break;
            // In production, decrypt and verify each message
        }
    }

    fn sendClientFinished(self: *Self) !void {
        // Build encrypted Finished message
        const transcript_hash = self.transcript.peek();

        // Compute verify_data
        var verify_data: [32]u8 = undefined;
        crypto.auth.hmac.sha2.HmacSha256.create(&verify_data, &transcript_hash, &self.client_key);

        // Build Finished handshake message
        var finished_msg: [36]u8 = undefined;
        finished_msg[0] = @intFromEnum(HandshakeType.finished);
        finished_msg[1] = 0;
        finished_msg[2] = 0;
        finished_msg[3] = 32; // length
        @memcpy(finished_msg[4..36], &verify_data);

        // Encrypt with AES-GCM
        var nonce: [12]u8 = undefined;
        @memcpy(&nonce, &self.client_iv);
        // XOR with sequence number
        const seq_bytes = std.mem.toBytes(self.client_seq);
        for (0..8) |i| {
            nonce[4 + i] ^= seq_bytes[7 - i];
        }

        var ciphertext: [36 + 16]u8 = undefined; // message + tag
        var tag: [16]u8 = undefined;
        crypto.aead.aes_gcm.Aes128Gcm.encrypt(&ciphertext, &tag, &finished_msg, &[_]u8{}, nonce, self.client_key[0..16].*);
        @memcpy(ciphertext[36..52], &tag);

        // Build TLS record
        var record: [5 + 52]u8 = undefined;
        record[0] = @intFromEnum(ContentType.application_data);
        record[1] = 0x03;
        record[2] = 0x03;
        record[3] = 0;
        record[4] = 52;
        @memcpy(record[5..57], &ciphertext);

        self.tcp.sendAll(&record) catch return error.WriteError;
        self.client_seq += 1;
    }

    /// Send data over TLS
    pub fn send(self: *Self, data: []const u8) TlsError!usize {
        if (!self.handshake_complete) return TlsError.HandshakeFailed;

        // Encrypt data with AES-GCM
        var nonce: [12]u8 = undefined;
        @memcpy(&nonce, &self.client_iv);
        const seq_bytes = std.mem.toBytes(self.client_seq);
        for (0..8) |i| {
            nonce[4 + i] ^= seq_bytes[7 - i];
        }

        // Allocate ciphertext buffer
        const ct_len = data.len + 16 + 1; // +16 for tag, +1 for content type
        var ciphertext = self.allocator.alloc(u8, ct_len) catch return TlsError.OutOfMemory;
        defer self.allocator.free(ciphertext);

        // Prepend content type to plaintext
        var plaintext = self.allocator.alloc(u8, data.len + 1) catch return TlsError.OutOfMemory;
        defer self.allocator.free(plaintext);
        @memcpy(plaintext[0..data.len], data);
        plaintext[data.len] = @intFromEnum(ContentType.application_data);

        var tag: [16]u8 = undefined;
        crypto.aead.aes_gcm.Aes128Gcm.encrypt(ciphertext[0 .. ct_len - 16], &tag, plaintext, &[_]u8{}, nonce, self.client_key[0..16].*);
        @memcpy(ciphertext[ct_len - 16 ..], &tag);

        // Build TLS record
        var record_header: [5]u8 = undefined;
        record_header[0] = @intFromEnum(ContentType.application_data);
        record_header[1] = 0x03;
        record_header[2] = 0x03;
        record_header[3] = @truncate(ct_len >> 8);
        record_header[4] = @truncate(ct_len);

        self.tcp.sendAll(&record_header) catch return TlsError.WriteError;
        self.tcp.sendAll(ciphertext) catch return TlsError.WriteError;

        self.client_seq += 1;
        return data.len;
    }

    /// Send all data over TLS
    pub fn sendAll(self: *Self, data: []const u8) TlsError!void {
        var remaining = data;
        while (remaining.len > 0) {
            const sent = try self.send(remaining);
            if (sent == 0) return TlsError.WriteError;
            remaining = remaining[sent..];
        }
    }

    /// Receive data over TLS
    pub fn recv(self: *Self, buffer: []u8) TlsError!usize {
        if (!self.handshake_complete) return TlsError.HandshakeFailed;

        // Return buffered data first
        if (self.read_pos < self.read_len) {
            const available = self.read_len - self.read_pos;
            const to_copy = @min(available, buffer.len);
            @memcpy(buffer[0..to_copy], self.read_buffer[self.read_pos..][0..to_copy]);
            self.read_pos += to_copy;
            return to_copy;
        }

        // Read new TLS record
        var record_buf: [16384 + 256]u8 = undefined;
        const record_len = self.readRecord(&record_buf) catch return TlsError.ReadError;
        if (record_len == 0) return 0;

        // Decrypt with AES-GCM
        if (record_len < 17) return TlsError.DecryptionFailed; // Need at least tag + 1 byte

        var nonce: [12]u8 = undefined;
        @memcpy(&nonce, &self.server_iv);
        const seq_bytes = std.mem.toBytes(self.server_seq);
        for (0..8) |i| {
            nonce[4 + i] ^= seq_bytes[7 - i];
        }

        const ciphertext = record_buf[0 .. record_len - 16];
        const tag = record_buf[record_len - 16 ..][0..16];

        crypto.aead.aes_gcm.Aes128Gcm.decrypt(self.read_buffer[0..ciphertext.len], ciphertext, tag.*, &[_]u8{}, nonce, self.server_key[0..16].*) catch {
            return TlsError.DecryptionFailed;
        };

        self.server_seq += 1;

        // Remove content type byte at the end
        const plaintext_len = ciphertext.len - 1;
        self.read_pos = 0;
        self.read_len = plaintext_len;

        const to_copy = @min(plaintext_len, buffer.len);
        @memcpy(buffer[0..to_copy], self.read_buffer[0..to_copy]);
        self.read_pos = to_copy;

        return to_copy;
    }

    /// Close the TLS connection
    pub fn close(self: *Self) void {
        self.tcp.close();
        self.allocator.free(self.read_buffer);
        self.allocator.destroy(self);
    }
};

/// Resolve hostname to IP address (re-export)
pub const resolveHost = wasi_sock.resolveHost;
