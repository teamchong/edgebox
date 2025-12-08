/// WASI TLS 1.3 Client
/// Proper TLS 1.3 implementation for HTTPS over WASI sockets
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
    new_session_ticket = 4,
    encrypted_extensions = 8,
    certificate = 11,
    certificate_verify = 15,
    finished = 20,
};

const HmacSha256 = crypto.auth.hmac.sha2.HmacSha256;
const Sha256 = crypto.hash.sha2.Sha256;

/// HKDF-Extract
fn hkdfExtract(salt: []const u8, ikm: []const u8) [32]u8 {
    var out: [32]u8 = undefined;
    var salt_key: [32]u8 = undefined;
    if (salt.len == 0) {
        @memset(&salt_key, 0);
        HmacSha256.create(&out, ikm, &salt_key);
    } else {
        const len = @min(salt.len, 32);
        @memcpy(salt_key[0..len], salt[0..len]);
        if (len < 32) @memset(salt_key[len..], 0);
        HmacSha256.create(&out, ikm, &salt_key);
    }
    return out;
}

/// HKDF-Expand-Label for TLS 1.3
fn hkdfExpandLabel(secret: *const [32]u8, label: []const u8, context: []const u8, length: u16) [32]u8 {
    // Build HkdfLabel structure
    var info: [256]u8 = undefined;
    var pos: usize = 0;

    // Length (2 bytes)
    info[pos] = @truncate(length >> 8);
    info[pos + 1] = @truncate(length);
    pos += 2;

    // Label length + "tls13 " prefix
    const full_label_len = 6 + label.len;
    info[pos] = @truncate(full_label_len);
    pos += 1;

    // "tls13 " prefix
    @memcpy(info[pos..][0..6], "tls13 ");
    pos += 6;

    // Label
    @memcpy(info[pos..][0..label.len], label);
    pos += label.len;

    // Context length
    info[pos] = @truncate(context.len);
    pos += 1;

    // Context
    if (context.len > 0) {
        @memcpy(info[pos..][0..context.len], context);
        pos += context.len;
    }

    // HKDF-Expand: T(1) = HMAC(PRK, info || 0x01)
    var input: [256 + 1]u8 = undefined;
    @memcpy(input[0..pos], info[0..pos]);
    input[pos] = 0x01;

    var out: [32]u8 = undefined;
    HmacSha256.create(&out, input[0 .. pos + 1], secret);
    return out;
}

/// Derive-Secret for TLS 1.3
fn deriveSecret(secret: *const [32]u8, label: []const u8, transcript_hash: *const [32]u8) [32]u8 {
    return hkdfExpandLabel(secret, label, transcript_hash, 32);
}

/// TLS-wrapped TCP socket for HTTPS connections
pub const TlsSocket = struct {
    tcp: wasi_sock.TcpSocket,
    allocator: std.mem.Allocator,

    // TLS state
    handshake_complete: bool = false,
    client_seq: u64 = 0,
    server_seq: u64 = 0,
    hs_client_seq: u64 = 0,
    hs_server_seq: u64 = 0,

    // Key material (handshake keys)
    hs_client_key: [16]u8 = undefined,
    hs_server_key: [16]u8 = undefined,
    hs_client_iv: [12]u8 = undefined,
    hs_server_iv: [12]u8 = undefined,

    // Key material (application keys)
    app_client_key: [16]u8 = undefined,
    app_server_key: [16]u8 = undefined,
    app_client_iv: [12]u8 = undefined,
    app_server_iv: [12]u8 = undefined,

    // Secrets for key derivation
    client_handshake_secret: [32]u8 = undefined,
    server_handshake_secret: [32]u8 = undefined,
    handshake_secret: [32]u8 = undefined,

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
        self.client_seq = 0;
        self.server_seq = 0;
        self.hs_client_seq = 0;
        self.hs_server_seq = 0;

        // Open TCP socket and connect
        self.tcp = wasi_sock.TcpSocket.open(false) catch return TlsError.ConnectionFailed;
        errdefer self.tcp.close();

        self.tcp.connect(ip, port) catch return TlsError.ConnectionFailed;

        // Perform TLS 1.3 handshake
        self.doHandshake(hostname) catch {
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

        // Compute shared secret via ECDHE
        const shared_secret = crypto.dh.X25519.scalarmult(keypair.secret_key, server_public_key) catch {
            return error.HandshakeFailed;
        };

        // Derive handshake keys using proper TLS 1.3 key schedule
        try self.deriveHandshakeKeys(&shared_secret);

        // Receive and process encrypted handshake messages
        try self.processEncryptedHandshake();

        // Derive application keys
        try self.deriveApplicationKeys();

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

        // Session ID (32 bytes of random for compatibility)
        buf[pos] = 32;
        pos += 1;
        var session_id: [32]u8 = undefined;
        crypto.random.bytes(&session_id);
        @memcpy(buf[pos..][0..32], &session_id);
        pos += 32;

        // Cipher suites (offer more options for compatibility)
        buf[pos] = 0x00;
        buf[pos + 1] = 0x06; // 6 bytes = 3 cipher suites
        buf[pos + 2] = 0x13;
        buf[pos + 3] = 0x01; // TLS_AES_128_GCM_SHA256
        buf[pos + 4] = 0x13;
        buf[pos + 5] = 0x02; // TLS_AES_256_GCM_SHA384
        buf[pos + 6] = 0x13;
        buf[pos + 7] = 0x03; // TLS_CHACHA20_POLY1305_SHA256
        pos += 8;

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

        // Signature algorithms extension - offer more for compatibility
        buf[pos] = 0x00;
        buf[pos + 1] = 0x0d; // signature_algorithms
        buf[pos + 2] = 0x00;
        buf[pos + 3] = 0x0c; // 12 bytes
        buf[pos + 4] = 0x00;
        buf[pos + 5] = 0x0a; // 10 bytes of algorithms
        // RSA-PSS-RSAE-SHA256
        buf[pos + 6] = 0x08;
        buf[pos + 7] = 0x04;
        // RSA-PSS-RSAE-SHA384
        buf[pos + 8] = 0x08;
        buf[pos + 9] = 0x05;
        // RSA-PSS-RSAE-SHA512
        buf[pos + 10] = 0x08;
        buf[pos + 11] = 0x06;
        // ECDSA-SECP256R1-SHA256
        buf[pos + 12] = 0x04;
        buf[pos + 13] = 0x03;
        // RSA-PKCS1-SHA256
        buf[pos + 14] = 0x04;
        buf[pos + 15] = 0x01;
        pos += 16;

        // PSK key exchange modes extension (required for TLS 1.3)
        buf[pos] = 0x00;
        buf[pos + 1] = 0x2d; // psk_key_exchange_modes
        buf[pos + 2] = 0x00;
        buf[pos + 3] = 0x02; // 2 bytes
        buf[pos + 4] = 0x01; // 1 mode
        buf[pos + 5] = 0x01; // psk_dhe_ke
        pos += 6;

        // Fill in extensions length
        const ext_len = pos - ext_start - 2;
        buf[ext_start] = @truncate(ext_len >> 8);
        buf[ext_start + 1] = @truncate(ext_len);

        // Fill in handshake length (3 bytes after handshake type)
        // Layout: record_start = HandshakeType, record_start+1,2,3 = length
        const handshake_len = pos - handshake_start;
        buf[record_start + 1] = @truncate(handshake_len >> 16);
        buf[record_start + 2] = @truncate(handshake_len >> 8);
        buf[record_start + 3] = @truncate(handshake_len);

        // Fill in record length
        const record_len = pos - record_start;
        buf[3] = @truncate(record_len >> 8);
        buf[4] = @truncate(record_len);

        // Update transcript hash (handshake message only, not record header)
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

        // Read TLS record header first
        var header: [5]u8 = undefined;
        var read_total: usize = 0;
        while (read_total < 5) {
            const n = self.tcp.recv(header[read_total..]) catch return error.ReadError;
            if (n == 0) return error.ReadError;
            read_total += n;
        }

        const content_type = header[0];
        const record_len = (@as(u16, header[3]) << 8) | header[4];


        if (record_len > buf.len) return error.ReadError;

        // Read record body
        read_total = 0;
        while (read_total < record_len) {
            const n = self.tcp.recv(buf[read_total..record_len]) catch return error.ReadError;
            if (n == 0) return error.ReadError;
            read_total += n;
        }

        // Check if it's an alert
        if (content_type == @intFromEnum(ContentType.alert)) {
            return error.TlsAlert;
        }

        // Verify it's a ServerHello
        if (content_type != @intFromEnum(ContentType.handshake)) {
            return error.HandshakeFailed;
        }

        if (buf[0] != @intFromEnum(HandshakeType.server_hello)) {
            return error.HandshakeFailed;
        }

        // Update transcript (handshake message only)
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
        var found_key_share = false;
        while (pos + 4 <= ext_end) {
            const ext_type = (@as(u16, buf[pos]) << 8) | buf[pos + 1];
            const ext_data_len = (@as(u16, buf[pos + 2]) << 8) | buf[pos + 3];
            pos += 4;

            if (ext_type == 0x0033) { // key_share
                // Format: group(2) + key_len(2) + key(32)
                if (ext_data_len >= 36) {
                    @memcpy(server_public_key, buf[pos + 4 ..][0..32]);
                    found_key_share = true;
                }
            }

            pos += ext_data_len;
        }

        if (!found_key_share) {
            return error.HandshakeFailed;
        }
    }

    fn deriveHandshakeKeys(self: *Self, shared_secret: *const [32]u8) !void {
        // TLS 1.3 Key Schedule
        // early_secret = HKDF-Extract(salt=0, IKM=0)
        const zero_key: [32]u8 = [_]u8{0} ** 32;
        const early_secret = hkdfExtract(&[_]u8{}, &zero_key);

        // derived_secret = Derive-Secret(early_secret, "derived", "")
        var empty_hash: [32]u8 = undefined;
        Sha256.hash(&[_]u8{}, &empty_hash, .{});
        const derived_secret = deriveSecret(&early_secret, "derived", &empty_hash);

        // handshake_secret = HKDF-Extract(derived_secret, shared_secret)
        self.handshake_secret = hkdfExtract(&derived_secret, shared_secret);

        // Get transcript hash up to ServerHello
        const transcript_hash = self.transcript.peek();

        // client_handshake_traffic_secret
        self.client_handshake_secret = deriveSecret(&self.handshake_secret, "c hs traffic", &transcript_hash);

        // server_handshake_traffic_secret
        self.server_handshake_secret = deriveSecret(&self.handshake_secret, "s hs traffic", &transcript_hash);

        // Derive client handshake key and IV
        const client_key_full = hkdfExpandLabel(&self.client_handshake_secret, "key", &[_]u8{}, 16);
        @memcpy(&self.hs_client_key, client_key_full[0..16]);
        const client_iv_full = hkdfExpandLabel(&self.client_handshake_secret, "iv", &[_]u8{}, 12);
        @memcpy(&self.hs_client_iv, client_iv_full[0..12]);

        // Derive server handshake key and IV
        const server_key_full = hkdfExpandLabel(&self.server_handshake_secret, "key", &[_]u8{}, 16);
        @memcpy(&self.hs_server_key, server_key_full[0..16]);
        const server_iv_full = hkdfExpandLabel(&self.server_handshake_secret, "iv", &[_]u8{}, 12);
        @memcpy(&self.hs_server_iv, server_iv_full[0..12]);
    }

    fn processEncryptedHandshake(self: *Self) !void {
        // Read and decrypt server's encrypted handshake messages
        var buf: [16384]u8 = undefined;
        var got_finished = false;

        while (!got_finished) {
            // Read TLS record
            var header: [5]u8 = undefined;
            var read_total: usize = 0;
            while (read_total < 5) {
                const n = self.tcp.recv(header[read_total..]) catch return error.ReadError;
                if (n == 0) return error.ReadError;
                read_total += n;
            }

            const content_type = header[0];
            const record_len = (@as(u16, header[3]) << 8) | header[4];


            // Skip change_cipher_spec (backward compat) - has length 1, no need to read body
            if (content_type == @intFromEnum(ContentType.change_cipher_spec)) {
                // Read and discard the single byte
                var ccs_buf: [1]u8 = undefined;
                read_total = 0;
                while (read_total < record_len) {
                    const n = self.tcp.recv(ccs_buf[read_total..]) catch return error.ReadError;
                    if (n == 0) return error.ReadError;
                    read_total += n;
                }
                continue;
            }

            if (record_len > buf.len or record_len < 17) {
                return error.ReadError;
            }

            // Read record body
            read_total = 0;
            while (read_total < record_len) {
                const n = self.tcp.recv(buf[read_total..record_len]) catch return error.ReadError;
                if (n == 0) return error.ReadError;
                read_total += n;
            }

            // Check for alert
            if (content_type == @intFromEnum(ContentType.alert)) {
                return error.TlsAlert;
            }

            // Must be application_data (encrypted handshake)
            if (content_type != @intFromEnum(ContentType.application_data)) {
                return error.HandshakeFailed;
            }

            // Decrypt with server handshake key
            var nonce: [12]u8 = undefined;
            @memcpy(&nonce, &self.hs_server_iv);
            const seq_bytes = std.mem.toBytes(self.hs_server_seq);
            for (0..8) |i| {
                nonce[4 + i] ^= seq_bytes[7 - i];
            }

            const ciphertext_len = record_len - 16;
            const tag = buf[ciphertext_len..][0..16];

            // Build AAD: record header with encrypted length
            var aad: [5]u8 = undefined;
            aad[0] = @intFromEnum(ContentType.application_data);
            aad[1] = 0x03;
            aad[2] = 0x03;
            aad[3] = @truncate(record_len >> 8);
            aad[4] = @truncate(record_len);

            var plaintext: [16384]u8 = undefined;
            crypto.aead.aes_gcm.Aes128Gcm.decrypt(
                plaintext[0..ciphertext_len],
                buf[0..ciphertext_len],
                tag.*,
                &aad,
                nonce,
                self.hs_server_key,
            ) catch {
                return error.DecryptionFailed;
            };

            self.hs_server_seq += 1;

            // Find the inner content type (last byte)
            var inner_len = ciphertext_len;
            while (inner_len > 0 and plaintext[inner_len - 1] == 0) {
                inner_len -= 1;
            }
            if (inner_len == 0) return error.HandshakeFailed;
            inner_len -= 1; // Remove content type byte

            const inner_type = plaintext[inner_len];

            if (inner_type != @intFromEnum(ContentType.handshake)) {
                continue;
            }

            // Parse handshake messages
            var pos: usize = 0;
            while (pos < inner_len) {
                if (pos + 4 > inner_len) break;

                const hs_type = plaintext[pos];
                const hs_len = (@as(u24, plaintext[pos + 1]) << 16) | (@as(u24, plaintext[pos + 2]) << 8) | plaintext[pos + 3];


                // Update transcript with handshake message
                const msg_end = pos + 4 + hs_len;
                if (msg_end > inner_len) break;
                self.transcript.update(plaintext[pos..msg_end]);

                if (hs_type == @intFromEnum(HandshakeType.finished)) {
                    got_finished = true;
                }

                pos = msg_end;
            }
        }
    }

    fn deriveApplicationKeys(self: *Self) !void {
        // Derive application traffic secrets
        // derived_secret = Derive-Secret(handshake_secret, "derived", "")
        var empty_hash: [32]u8 = undefined;
        Sha256.hash(&[_]u8{}, &empty_hash, .{});
        const derived_secret = deriveSecret(&self.handshake_secret, "derived", &empty_hash);

        // master_secret = HKDF-Extract(derived_secret, 0)
        const zero_key: [32]u8 = [_]u8{0} ** 32;
        const master_secret = hkdfExtract(&derived_secret, &zero_key);

        // Get transcript hash (includes server Finished)
        const transcript_hash = self.transcript.peek();

        // client_application_traffic_secret_0
        const client_app_secret = deriveSecret(&master_secret, "c ap traffic", &transcript_hash);

        // server_application_traffic_secret_0
        const server_app_secret = deriveSecret(&master_secret, "s ap traffic", &transcript_hash);

        // Derive application keys
        const client_key_full = hkdfExpandLabel(&client_app_secret, "key", &[_]u8{}, 16);
        @memcpy(&self.app_client_key, client_key_full[0..16]);
        const client_iv_full = hkdfExpandLabel(&client_app_secret, "iv", &[_]u8{}, 12);
        @memcpy(&self.app_client_iv, client_iv_full[0..12]);

        const server_key_full = hkdfExpandLabel(&server_app_secret, "key", &[_]u8{}, 16);
        @memcpy(&self.app_server_key, server_key_full[0..16]);
        const server_iv_full = hkdfExpandLabel(&server_app_secret, "iv", &[_]u8{}, 12);
        @memcpy(&self.app_server_iv, server_iv_full[0..12]);
    }

    fn sendClientFinished(self: *Self) !void {
        // Compute finished_key
        const finished_key = hkdfExpandLabel(&self.client_handshake_secret, "finished", &[_]u8{}, 32);

        // Compute verify_data = HMAC(finished_key, transcript_hash)
        const transcript_hash = self.transcript.peek();
        var verify_data: [32]u8 = undefined;
        HmacSha256.create(&verify_data, &transcript_hash, &finished_key);

        // Build Finished handshake message
        var finished_msg: [36]u8 = undefined;
        finished_msg[0] = @intFromEnum(HandshakeType.finished);
        finished_msg[1] = 0;
        finished_msg[2] = 0;
        finished_msg[3] = 32; // length
        @memcpy(finished_msg[4..36], &verify_data);

        // Update transcript with our Finished
        self.transcript.update(&finished_msg);

        // Add inner content type
        var plaintext: [37]u8 = undefined;
        @memcpy(plaintext[0..36], &finished_msg);
        plaintext[36] = @intFromEnum(ContentType.handshake);

        // Encrypt with client handshake key
        var nonce: [12]u8 = undefined;
        @memcpy(&nonce, &self.hs_client_iv);
        const seq_bytes = std.mem.toBytes(self.hs_client_seq);
        for (0..8) |i| {
            nonce[4 + i] ^= seq_bytes[7 - i];
        }

        const ciphertext_len = plaintext.len + 16;

        // Build AAD
        var aad: [5]u8 = undefined;
        aad[0] = @intFromEnum(ContentType.application_data);
        aad[1] = 0x03;
        aad[2] = 0x03;
        aad[3] = @truncate(ciphertext_len >> 8);
        aad[4] = @truncate(ciphertext_len);

        var ciphertext: [37]u8 = undefined;
        var tag: [16]u8 = undefined;
        crypto.aead.aes_gcm.Aes128Gcm.encrypt(&ciphertext, &tag, &plaintext, &aad, nonce, self.hs_client_key);

        // Build TLS record
        var record: [5 + 37 + 16]u8 = undefined;
        record[0] = @intFromEnum(ContentType.application_data);
        record[1] = 0x03;
        record[2] = 0x03;
        record[3] = @truncate(ciphertext_len >> 8);
        record[4] = @truncate(ciphertext_len);
        @memcpy(record[5..42], &ciphertext);
        @memcpy(record[42..58], &tag);

        self.tcp.sendAll(&record) catch return error.WriteError;
        self.hs_client_seq += 1;
    }

    /// Send data over TLS
    pub fn send(self: *Self, data: []const u8) TlsError!usize {
        if (!self.handshake_complete) return TlsError.HandshakeFailed;

        // Build plaintext with inner content type
        var plaintext = self.allocator.alloc(u8, data.len + 1) catch return TlsError.OutOfMemory;
        defer self.allocator.free(plaintext);
        @memcpy(plaintext[0..data.len], data);
        plaintext[data.len] = @intFromEnum(ContentType.application_data);

        // Encrypt with AES-GCM
        var nonce: [12]u8 = undefined;
        @memcpy(&nonce, &self.app_client_iv);
        const seq_bytes = std.mem.toBytes(self.client_seq);
        for (0..8) |i| {
            nonce[4 + i] ^= seq_bytes[7 - i];
        }

        const ciphertext_len = plaintext.len + 16;

        // Build AAD
        var aad: [5]u8 = undefined;
        aad[0] = @intFromEnum(ContentType.application_data);
        aad[1] = 0x03;
        aad[2] = 0x03;
        aad[3] = @truncate(ciphertext_len >> 8);
        aad[4] = @truncate(ciphertext_len);

        const ciphertext = self.allocator.alloc(u8, plaintext.len) catch return TlsError.OutOfMemory;
        defer self.allocator.free(ciphertext);
        var tag: [16]u8 = undefined;

        crypto.aead.aes_gcm.Aes128Gcm.encrypt(ciphertext, &tag, plaintext, &aad, nonce, self.app_client_key);

        // Build TLS record
        var record_header: [5]u8 = undefined;
        record_header[0] = @intFromEnum(ContentType.application_data);
        record_header[1] = 0x03;
        record_header[2] = 0x03;
        record_header[3] = @truncate(ciphertext_len >> 8);
        record_header[4] = @truncate(ciphertext_len);

        self.tcp.sendAll(&record_header) catch return TlsError.WriteError;
        self.tcp.sendAll(ciphertext) catch return TlsError.WriteError;
        self.tcp.sendAll(&tag) catch return TlsError.WriteError;

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

        // Read TLS record header
        var header: [5]u8 = undefined;
        var read_total: usize = 0;
        while (read_total < 5) {
            const n = self.tcp.recv(header[read_total..]) catch return TlsError.ReadError;
            if (n == 0) return 0; // Connection closed
            read_total += n;
        }

        _ = header[0]; // content_type - always 23 (application_data) for encrypted records
        const record_len = (@as(u16, header[3]) << 8) | header[4];

        if (record_len < 17 or record_len > 16384 + 256) {
            return TlsError.ReadError;
        }

        // Read record body
        var record_buf: [16384 + 256]u8 = undefined;
        read_total = 0;
        while (read_total < record_len) {
            const n = self.tcp.recv(record_buf[read_total..record_len]) catch return TlsError.ReadError;
            if (n == 0) return TlsError.ReadError;
            read_total += n;
        }

        // Build AAD
        var aad: [5]u8 = undefined;
        aad[0] = @intFromEnum(ContentType.application_data);
        aad[1] = 0x03;
        aad[2] = 0x03;
        aad[3] = @truncate(record_len >> 8);
        aad[4] = @truncate(record_len);

        // Decrypt with AES-GCM
        var nonce: [12]u8 = undefined;
        @memcpy(&nonce, &self.app_server_iv);
        const seq_bytes = std.mem.toBytes(self.server_seq);
        for (0..8) |i| {
            nonce[4 + i] ^= seq_bytes[7 - i];
        }

        const ciphertext_len = record_len - 16;
        const tag = record_buf[ciphertext_len..][0..16];

        crypto.aead.aes_gcm.Aes128Gcm.decrypt(
            self.read_buffer[0..ciphertext_len],
            record_buf[0..ciphertext_len],
            tag.*,
            &aad,
            nonce,
            self.app_server_key,
        ) catch {
            return TlsError.DecryptionFailed;
        };

        self.server_seq += 1;

        // Find inner content type (strip padding and content type)
        var inner_len = ciphertext_len;
        while (inner_len > 0 and self.read_buffer[inner_len - 1] == 0) {
            inner_len -= 1;
        }
        if (inner_len == 0) return 0;

        const inner_content_type = self.read_buffer[inner_len - 1];

        // Check for alert
        if (inner_content_type == @intFromEnum(ContentType.alert) and inner_len >= 3) {
            // Close notify (level=1, desc=0) is normal connection close
            if (self.read_buffer[0] == 1 and self.read_buffer[1] == 0) {
                return 0; // EOF
            }
            return TlsError.TlsAlert;
        }

        inner_len -= 1; // Remove content type byte

        // Skip handshake messages (like NewSessionTicket) that come after handshake complete
        if (inner_content_type == @intFromEnum(ContentType.handshake)) {
            // Read the next record recursively
            return self.recv(buffer);
        }

        // Only return application data
        if (inner_content_type != @intFromEnum(ContentType.application_data)) {
            return TlsError.ReadError;
        }

        self.read_pos = 0;
        self.read_len = inner_len;

        const to_copy = @min(inner_len, buffer.len);
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
