/// Native TLS module - QuickJS C functions for TLS 1.3 sockets
/// Provides TLS operations for the tls.js polyfill
const std = @import("std");
const builtin = @import("builtin");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// Crypto imports
const crypto = std.crypto;

// Platform-specific socket imports
const c = @cImport({
    @cInclude("sys/socket.h");
    @cInclude("netinet/in.h");
    @cInclude("netinet/tcp.h");
    @cInclude("arpa/inet.h");
    @cInclude("unistd.h");
    @cInclude("fcntl.h");
    @cInclude("poll.h");
    @cInclude("errno.h");
    @cInclude("netdb.h");
});

// TLS Connection tracking
const MAX_TLS_CONNECTIONS = 64;
var tls_connections: [MAX_TLS_CONNECTIONS]TlsEntry = [_]TlsEntry{.{}} ** MAX_TLS_CONNECTIONS;

const TlsEntry = struct {
    fd: i32 = -1,
    handshake_complete: bool = false,
    cipher_suite: CipherSuite = .TLS_AES_128_GCM_SHA256,
    client_cipher: ?AesGcm = null,
    server_cipher: ?AesGcm = null,
    client_iv: [12]u8 = undefined,
    server_iv: [12]u8 = undefined,
    client_seq: u64 = 0,
    server_seq: u64 = 0,
    key_schedule: KeySchedule = undefined,
    read_buffer: [32768]u8 = undefined,
    read_pos: usize = 0,
    read_len: usize = 0,
    hostname: [256]u8 = undefined,
    hostname_len: usize = 0,
};

/// Cipher suites (TLS 1.3)
const CipherSuite = enum(u16) {
    TLS_AES_128_GCM_SHA256 = 0x1301,
    TLS_AES_256_GCM_SHA384 = 0x1302,
    _,

    fn keyLen(self: CipherSuite) usize {
        return switch (self) {
            .TLS_AES_128_GCM_SHA256 => 16,
            .TLS_AES_256_GCM_SHA384 => 32,
            _ => 16,
        };
    }
};

/// AES-GCM wrapper
const AesGcm = struct {
    const Aes128Gcm = crypto.aead.aes_gcm.Aes128Gcm;
    const Aes256Gcm = crypto.aead.aes_gcm.Aes256Gcm;

    key_128: ?[16]u8,
    key_256: ?[32]u8,

    fn init128(key: [16]u8) AesGcm {
        return .{ .key_128 = key, .key_256 = null };
    }

    fn init256(key: [32]u8) AesGcm {
        return .{ .key_128 = null, .key_256 = key };
    }

    fn encrypt(
        self: *const AesGcm,
        nonce: [12]u8,
        plaintext: []const u8,
        aad: []const u8,
        ciphertext: []u8,
        tag: *[16]u8,
    ) void {
        if (self.key_256) |key| {
            Aes256Gcm.encrypt(ciphertext, tag, plaintext, aad, nonce, key);
        } else if (self.key_128) |key| {
            Aes128Gcm.encrypt(ciphertext, tag, plaintext, aad, nonce, key);
        }
    }

    fn decrypt(
        self: *const AesGcm,
        nonce: [12]u8,
        ciphertext: []const u8,
        aad: []const u8,
        tag: [16]u8,
        plaintext: []u8,
    ) !void {
        if (self.key_256) |key| {
            Aes256Gcm.decrypt(plaintext, ciphertext, tag, aad, nonce, key) catch return error.AuthenticationFailed;
        } else if (self.key_128) |key| {
            Aes128Gcm.decrypt(plaintext, ciphertext, tag, aad, nonce, key) catch return error.AuthenticationFailed;
        }
    }
};

/// TLS Key Schedule (TLS 1.3 key derivation)
const KeySchedule = struct {
    handshake_secret: [32]u8,
    client_handshake_traffic_secret: [32]u8,
    server_handshake_traffic_secret: [32]u8,
    master_secret: [32]u8,
    client_application_traffic_secret: [32]u8,
    server_application_traffic_secret: [32]u8,

    fn deriveHandshakeKeys(self: *KeySchedule, shared_secret: [32]u8, transcript_hash: [32]u8) void {
        const HkdfSha256 = crypto.kdf.hkdf.HkdfSha256;

        // Early Secret
        const early_secret = HkdfSha256.extract(&[_]u8{0} ** 32, &[_]u8{0} ** 32);

        // Empty hash for "derived" label
        var empty_hash: [32]u8 = undefined;
        crypto.hash.sha2.Sha256.hash(&[_]u8{}, &empty_hash, .{});

        // Handshake Secret
        var derived: [32]u8 = undefined;
        hkdfExpandLabel(&derived, early_secret, "derived", &empty_hash);
        self.handshake_secret = HkdfSha256.extract(&derived, &shared_secret);

        // Handshake Traffic Secrets
        hkdfExpandLabel(&self.client_handshake_traffic_secret, self.handshake_secret, "c hs traffic", &transcript_hash);
        hkdfExpandLabel(&self.server_handshake_traffic_secret, self.handshake_secret, "s hs traffic", &transcript_hash);

        // Master Secret
        hkdfExpandLabel(&derived, self.handshake_secret, "derived", &empty_hash);
        self.master_secret = HkdfSha256.extract(&derived, &[_]u8{0} ** 32);
    }

    fn deriveApplicationKeys(self: *KeySchedule, transcript_hash: [32]u8) void {
        hkdfExpandLabel(&self.client_application_traffic_secret, self.master_secret, "c ap traffic", &transcript_hash);
        hkdfExpandLabel(&self.server_application_traffic_secret, self.master_secret, "s ap traffic", &transcript_hash);
    }

    fn deriveTrafficKey(secret: [32]u8, key_len: usize) struct { key: [32]u8, iv: [12]u8 } {
        var key: [32]u8 = undefined;
        var iv: [12]u8 = undefined;

        hkdfExpandLabel(key[0..key_len], secret, "key", &[_]u8{});
        hkdfExpandLabel(&iv, secret, "iv", &[_]u8{});

        return .{ .key = key, .iv = iv };
    }
};

/// HKDF-Expand-Label (RFC 8446)
fn hkdfExpandLabel(out: []u8, secret: [32]u8, label: []const u8, context: []const u8) void {
    const Hkdf = crypto.kdf.hkdf.HkdfSha256;

    var hkdf_label: [512]u8 = undefined;
    var pos: usize = 0;

    // Length (2 bytes)
    hkdf_label[pos] = @truncate(out.len >> 8);
    hkdf_label[pos + 1] = @truncate(out.len);
    pos += 2;

    // Label with "tls13 " prefix
    const full_label_len = 6 + label.len;
    hkdf_label[pos] = @truncate(full_label_len);
    pos += 1;
    @memcpy(hkdf_label[pos .. pos + 6], "tls13 ");
    pos += 6;
    @memcpy(hkdf_label[pos .. pos + label.len], label);
    pos += label.len;

    // Context
    hkdf_label[pos] = @truncate(context.len);
    pos += 1;
    if (context.len > 0) {
        @memcpy(hkdf_label[pos .. pos + context.len], context);
        pos += context.len;
    }

    Hkdf.expand(out, hkdf_label[0..pos], secret);
}

fn allocateTlsConnection() ?usize {
    for (&tls_connections, 0..) |*entry, i| {
        if (entry.fd == -1) {
            return i;
        }
    }
    return null;
}

fn getTlsConnection(id: i32) ?*TlsEntry {
    if (id < 0 or id >= MAX_TLS_CONNECTIONS) return null;
    const idx: usize = @intCast(id);
    if (tls_connections[idx].fd == -1) return null;
    return &tls_connections[idx];
}

/// Read from socket with timeout
fn socketRead(fd: i32, buf: []u8) !usize {
    var pfd = c.pollfd{
        .fd = fd,
        .events = c.POLLIN,
        .revents = 0,
    };

    // Wait up to 30 seconds for data
    const poll_result = c.poll(&pfd, 1, 30000);
    if (poll_result <= 0) return error.Timeout;

    const result = c.read(fd, buf.ptr, buf.len);
    if (result <= 0) return error.ReadFailed;
    return @intCast(result);
}

/// Write all data to socket
fn socketWriteAll(fd: i32, data: []const u8) !void {
    var written: usize = 0;
    while (written < data.len) {
        const result = c.write(fd, data.ptr + written, data.len - written);
        if (result <= 0) return error.WriteFailed;
        written += @intCast(result);
    }
}

/// __edgebox_tls_connect(host, port) - Create TLS connection
/// Returns: TLS connection ID (>=0) or error code (<0)
fn tlsConnect(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    // Get hostname
    const host_str = qjs.JS_ToCString(ctx, argv[0]);
    if (host_str == null) {
        return qjs.JS_NewInt32(ctx, -1);
    }
    defer qjs.JS_FreeCString(ctx, host_str);
    const hostname = std.mem.span(host_str);

    // Get port
    var port: i32 = 443;
    _ = qjs.JS_ToInt32(ctx, &port, argv[1]);

    // Allocate TLS connection slot
    const idx = allocateTlsConnection() orelse {
        return qjs.JS_NewInt32(ctx, -2);
    };

    // Create TCP socket
    const fd = c.socket(c.AF_INET, c.SOCK_STREAM, 0);
    if (fd < 0) {
        return qjs.JS_NewInt32(ctx, -3);
    }

    // DNS resolution
    var hints: c.addrinfo = std.mem.zeroes(c.addrinfo);
    hints.ai_family = c.AF_UNSPEC;
    hints.ai_socktype = c.SOCK_STREAM;

    var host_z: [256]u8 = undefined;
    _ = std.fmt.bufPrintZ(&host_z, "{s}", .{hostname}) catch {
        _ = c.close(fd);
        return qjs.JS_NewInt32(ctx, -4);
    };

    var result: ?*c.addrinfo = null;
    const status = c.getaddrinfo(&host_z, null, &hints, &result);
    if (status != 0 or result == null) {
        _ = c.close(fd);
        return qjs.JS_NewInt32(ctx, -5);
    }
    defer c.freeaddrinfo(result);

    // Connect
    var addr: c.sockaddr_in = std.mem.zeroes(c.sockaddr_in);
    if (result.?.ai_family == c.AF_INET) {
        const sin: *c.sockaddr_in = @ptrCast(@alignCast(result.?.ai_addr));
        addr.sin_family = c.AF_INET;
        addr.sin_port = c.htons(@as(u16, @intCast(port)));
        addr.sin_addr = sin.sin_addr;
    } else {
        _ = c.close(fd);
        return qjs.JS_NewInt32(ctx, -6);
    }

    const connect_result = c.connect(fd, @ptrCast(&addr), @sizeOf(c.sockaddr_in));
    if (connect_result < 0) {
        _ = c.close(fd);
        return qjs.JS_NewInt32(ctx, -7);
    }

    // Initialize TLS entry
    tls_connections[idx] = .{
        .fd = fd,
        .handshake_complete = false,
    };

    // Store hostname for SNI
    const copy_len = @min(hostname.len, tls_connections[idx].hostname.len - 1);
    @memcpy(tls_connections[idx].hostname[0..copy_len], hostname[0..copy_len]);
    tls_connections[idx].hostname_len = copy_len;

    // Perform TLS handshake
    const entry = &tls_connections[idx];
    performHandshake(entry) catch {
        _ = c.close(fd);
        tls_connections[idx] = .{};
        return qjs.JS_NewInt32(ctx, -8);
    };

    return qjs.JS_NewInt32(ctx, @intCast(idx));
}

/// Perform TLS 1.3 handshake
fn performHandshake(entry: *TlsEntry) !void {
    // Generate X25519 key pair
    var private_key: [32]u8 = undefined;
    crypto.random.bytes(&private_key);
    const public_key = crypto.dh.X25519.recoverPublicKey(private_key) catch return error.KeyGenFailed;

    // Initialize transcript hash
    var transcript_hash = crypto.hash.sha2.Sha256.init(.{});

    // Send ClientHello
    try sendClientHello(entry.fd, entry.hostname[0..entry.hostname_len], &public_key, &transcript_hash);

    // Receive ServerHello
    const server_public = try receiveServerHello(entry.fd, entry, &transcript_hash);

    // Compute shared secret
    const shared_secret = crypto.dh.X25519.scalarmult(private_key, server_public) catch return error.KeyExchangeFailed;

    // Get transcript hash up to ServerHello
    var transcript_copy = transcript_hash;
    var transcript: [32]u8 = undefined;
    transcript_copy.final(&transcript);

    // Derive handshake keys
    entry.key_schedule.deriveHandshakeKeys(shared_secret, transcript);

    // Setup handshake ciphers
    const client_keys = KeySchedule.deriveTrafficKey(
        entry.key_schedule.client_handshake_traffic_secret,
        entry.cipher_suite.keyLen(),
    );
    const server_keys = KeySchedule.deriveTrafficKey(
        entry.key_schedule.server_handshake_traffic_secret,
        entry.cipher_suite.keyLen(),
    );

    entry.client_iv = client_keys.iv;
    entry.server_iv = server_keys.iv;

    if (entry.cipher_suite.keyLen() == 16) {
        entry.client_cipher = AesGcm.init128(client_keys.key[0..16].*);
        entry.server_cipher = AesGcm.init128(server_keys.key[0..16].*);
    } else {
        entry.client_cipher = AesGcm.init256(client_keys.key);
        entry.server_cipher = AesGcm.init256(server_keys.key);
    }

    // Receive encrypted handshake messages
    try receiveServerHandshake(entry, &transcript_hash);

    // Derive application keys
    transcript_copy = transcript_hash;
    transcript_copy.final(&transcript);
    entry.key_schedule.deriveApplicationKeys(transcript);

    // Send client finished
    try sendClientFinished(entry, &transcript_hash);

    // Switch to application keys
    const app_client = KeySchedule.deriveTrafficKey(
        entry.key_schedule.client_application_traffic_secret,
        entry.cipher_suite.keyLen(),
    );
    const app_server = KeySchedule.deriveTrafficKey(
        entry.key_schedule.server_application_traffic_secret,
        entry.cipher_suite.keyLen(),
    );

    entry.client_iv = app_client.iv;
    entry.server_iv = app_server.iv;

    if (entry.cipher_suite.keyLen() == 16) {
        entry.client_cipher = AesGcm.init128(app_client.key[0..16].*);
        entry.server_cipher = AesGcm.init128(app_server.key[0..16].*);
    } else {
        entry.client_cipher = AesGcm.init256(app_client.key);
        entry.server_cipher = AesGcm.init256(app_server.key);
    }

    entry.client_seq = 0;
    entry.server_seq = 0;
    entry.handshake_complete = true;
}

fn sendClientHello(fd: i32, hostname: []const u8, public_key: *const [32]u8, transcript: *crypto.hash.sha2.Sha256) !void {
    var msg: [600]u8 = undefined;
    var pos: usize = 0;

    msg[pos] = 1; // client_hello
    pos += 1;

    const len_pos = pos;
    pos += 3;

    // Legacy version (TLS 1.2)
    msg[pos] = 0x03;
    msg[pos + 1] = 0x03;
    pos += 2;

    // Random (32 bytes)
    crypto.random.bytes(msg[pos .. pos + 32]);
    pos += 32;

    // Session ID (32 random bytes)
    msg[pos] = 32;
    pos += 1;
    crypto.random.bytes(msg[pos .. pos + 32]);
    pos += 32;

    // Cipher suites
    msg[pos] = 0;
    msg[pos + 1] = 4;
    pos += 2;
    msg[pos] = 0x13;
    msg[pos + 1] = 0x01; // TLS_AES_128_GCM_SHA256
    msg[pos + 2] = 0x13;
    msg[pos + 3] = 0x02; // TLS_AES_256_GCM_SHA384
    pos += 4;

    // Compression (null only)
    msg[pos] = 1;
    msg[pos + 1] = 0;
    pos += 2;

    // Extensions
    const ext_start = pos;
    pos += 2;

    // SNI extension
    msg[pos] = 0x00;
    msg[pos + 1] = 0x00;
    pos += 2;
    const sni_len: u16 = @intCast(hostname.len + 5);
    msg[pos] = @truncate(sni_len >> 8);
    msg[pos + 1] = @truncate(sni_len);
    pos += 2;
    msg[pos] = @truncate((sni_len - 2) >> 8);
    msg[pos + 1] = @truncate(sni_len - 2);
    pos += 2;
    msg[pos] = 0; // host_name type
    pos += 1;
    msg[pos] = @truncate(hostname.len >> 8);
    msg[pos + 1] = @truncate(hostname.len);
    pos += 2;
    @memcpy(msg[pos .. pos + hostname.len], hostname);
    pos += hostname.len;

    // supported_versions (TLS 1.3 only)
    msg[pos] = 0x00;
    msg[pos + 1] = 0x2b;
    pos += 2;
    msg[pos] = 0x00;
    msg[pos + 1] = 0x03;
    pos += 2;
    msg[pos] = 0x02;
    msg[pos + 1] = 0x03;
    msg[pos + 2] = 0x04; // TLS 1.3
    pos += 3;

    // signature_algorithms
    msg[pos] = 0x00;
    msg[pos + 1] = 0x0d;
    pos += 2;
    msg[pos] = 0x00;
    msg[pos + 1] = 0x08;
    pos += 2;
    msg[pos] = 0x00;
    msg[pos + 1] = 0x06;
    pos += 2;
    msg[pos] = 0x04;
    msg[pos + 1] = 0x03; // ecdsa_secp256r1_sha256
    msg[pos + 2] = 0x08;
    msg[pos + 3] = 0x04; // rsa_pss_rsae_sha256
    msg[pos + 4] = 0x04;
    msg[pos + 5] = 0x01; // rsa_pkcs1_sha256
    pos += 6;

    // supported_groups
    msg[pos] = 0x00;
    msg[pos + 1] = 0x0a;
    pos += 2;
    msg[pos] = 0x00;
    msg[pos + 1] = 0x04;
    pos += 2;
    msg[pos] = 0x00;
    msg[pos + 1] = 0x02;
    pos += 2;
    msg[pos] = 0x00;
    msg[pos + 1] = 0x1d; // x25519
    pos += 2;

    // key_share (x25519)
    msg[pos] = 0x00;
    msg[pos + 1] = 0x33;
    pos += 2;
    msg[pos] = 0x00;
    msg[pos + 1] = 0x26;
    pos += 2;
    msg[pos] = 0x00;
    msg[pos + 1] = 0x24;
    pos += 2;
    msg[pos] = 0x00;
    msg[pos + 1] = 0x1d;
    pos += 2;
    msg[pos] = 0x00;
    msg[pos + 1] = 0x20;
    pos += 2;
    @memcpy(msg[pos .. pos + 32], public_key);
    pos += 32;

    // Set extensions length
    const ext_len = pos - ext_start - 2;
    msg[ext_start] = @truncate(ext_len >> 8);
    msg[ext_start + 1] = @truncate(ext_len);

    // Set handshake length
    const hs_len = pos - len_pos - 3;
    msg[len_pos] = @truncate(hs_len >> 16);
    msg[len_pos + 1] = @truncate(hs_len >> 8);
    msg[len_pos + 2] = @truncate(hs_len);

    // Update transcript
    transcript.update(msg[0..pos]);

    // Send record
    var header: [5]u8 = undefined;
    header[0] = 22; // handshake
    header[1] = 0x03;
    header[2] = 0x01;
    header[3] = @truncate(pos >> 8);
    header[4] = @truncate(pos);

    try socketWriteAll(fd, &header);
    try socketWriteAll(fd, msg[0..pos]);
}

fn receiveServerHello(fd: i32, entry: *TlsEntry, transcript: *crypto.hash.sha2.Sha256) ![32]u8 {
    // Read record header
    var header: [5]u8 = undefined;
    _ = try socketRead(fd, &header);

    if (header[0] != 22) return error.UnexpectedRecord; // Not handshake

    const length = (@as(usize, header[3]) << 8) | header[4];
    if (length > 16384) return error.RecordTooLarge;

    // Read payload
    var payload: [16384]u8 = undefined;
    var read: usize = 0;
    while (read < length) {
        read += try socketRead(fd, payload[read..length]);
    }

    // Update transcript
    transcript.update(payload[0..length]);

    if (payload[0] != 2) return error.UnexpectedMessage; // Not server_hello

    // Parse ServerHello
    var offset: usize = 4 + 2 + 32 + 1; // type + len + version + random + session_id_len
    if (offset >= length) return error.InvalidServerHello;

    const session_id_len = payload[offset - 1];
    offset += session_id_len;

    if (offset + 3 >= length) return error.InvalidServerHello;

    // Parse cipher suite
    const cipher_suite_val = (@as(u16, payload[offset]) << 8) | payload[offset + 1];
    entry.cipher_suite = std.meta.intToEnum(CipherSuite, cipher_suite_val) catch .TLS_AES_128_GCM_SHA256;
    offset += 2;

    // Skip compression
    offset += 1;

    // Parse extensions for key_share
    if (offset + 2 > length) return error.InvalidServerHello;
    const ext_len = (@as(usize, payload[offset]) << 8) | payload[offset + 1];
    offset += 2;

    const ext_end = offset + ext_len;
    var server_public: [32]u8 = undefined;
    var found = false;

    while (offset + 4 <= ext_end) {
        const ext_type = (@as(u16, payload[offset]) << 8) | payload[offset + 1];
        const ext_size = (@as(usize, payload[offset + 2]) << 8) | payload[offset + 3];
        offset += 4;

        if (ext_type == 0x0033 and ext_size >= 36) { // key_share
            if (offset + 4 + 32 <= length) {
                @memcpy(&server_public, payload[offset + 4 .. offset + 4 + 32]);
                found = true;
            }
        }
        offset += ext_size;
    }

    if (!found) return error.MissingKeyShare;
    return server_public;
}

fn receiveServerHandshake(entry: *TlsEntry, transcript: *crypto.hash.sha2.Sha256) !void {
    while (true) {
        const record = try receiveEncryptedRecord(entry);

        // Update transcript
        transcript.update(record.payload[0..record.len]);

        if (record.len == 0) continue;
        if (record.payload[0] == 20) break; // Finished
    }
}

fn receiveEncryptedRecord(entry: *TlsEntry) !struct { content_type: u8, payload: []u8, len: usize } {
    // Read record header
    var header: [5]u8 = undefined;
    _ = try socketRead(entry.fd, &header);

    // Handle change_cipher_spec (not encrypted)
    if (header[0] == 20) {
        var ccs: [1]u8 = undefined;
        _ = try socketRead(entry.fd, &ccs);
        return .{ .content_type = 20, .payload = &entry.read_buffer, .len = 0 };
    }

    if (header[0] != 23) return error.UnexpectedRecord; // Not application_data

    const length = (@as(usize, header[3]) << 8) | header[4];
    if (length > 16640) return error.RecordTooLarge;

    // Read encrypted payload
    var read: usize = 0;
    while (read < length) {
        read += try socketRead(entry.fd, entry.read_buffer[read..length]);
    }

    // Decrypt
    const cipher = entry.server_cipher orelse return error.NotEncrypted;

    if (length < 17) return error.RecordTooShort;

    var nonce = entry.server_iv;
    const seq_bytes = std.mem.toBytes(std.mem.nativeToBig(u64, entry.server_seq));
    for (0..8) |i| {
        nonce[4 + i] ^= seq_bytes[i];
    }
    entry.server_seq += 1;

    const ciphertext = entry.read_buffer[0 .. length - 16];
    const tag = entry.read_buffer[length - 16 .. length][0..16].*;

    var aad: [5]u8 = undefined;
    aad[0] = 0x17;
    aad[1] = 0x03;
    aad[2] = 0x03;
    aad[3] = @truncate(length >> 8);
    aad[4] = @truncate(length);

    var plaintext: [16384]u8 = undefined;
    try cipher.decrypt(nonce, ciphertext, &aad, tag, plaintext[0..ciphertext.len]);

    // Find inner content type (last non-zero byte)
    var end = ciphertext.len;
    while (end > 0 and plaintext[end - 1] == 0) {
        end -= 1;
    }
    if (end == 0) return error.InvalidRecord;

    const inner_type = plaintext[end - 1];
    @memcpy(entry.read_buffer[0 .. end - 1], plaintext[0 .. end - 1]);

    return .{ .content_type = inner_type, .payload = &entry.read_buffer, .len = end - 1 };
}

fn sendClientFinished(entry: *TlsEntry, transcript: *crypto.hash.sha2.Sha256) !void {
    var finished_key: [32]u8 = undefined;
    hkdfExpandLabel(&finished_key, entry.key_schedule.client_handshake_traffic_secret, "finished", &[_]u8{});

    var transcript_copy = transcript.*;
    var transcript_data: [32]u8 = undefined;
    transcript_copy.final(&transcript_data);

    var verify_data: [32]u8 = undefined;
    crypto.auth.hmac.sha2.HmacSha256.create(&verify_data, &transcript_data, &finished_key);

    var msg: [36]u8 = undefined;
    msg[0] = 20; // finished
    msg[1] = 0;
    msg[2] = 0;
    msg[3] = 32;
    @memcpy(msg[4..36], &verify_data);

    try sendEncryptedRecord(entry, 22, &msg);
}

fn sendEncryptedRecord(entry: *TlsEntry, content_type: u8, data: []const u8) !void {
    const cipher = entry.client_cipher orelse return error.NotEncrypted;

    var nonce = entry.client_iv;
    const seq_bytes = std.mem.toBytes(std.mem.nativeToBig(u64, entry.client_seq));
    for (0..8) |i| {
        nonce[4 + i] ^= seq_bytes[i];
    }
    entry.client_seq += 1;

    // Prepare plaintext with inner content type
    var plaintext: [16384]u8 = undefined;
    @memcpy(plaintext[0..data.len], data);
    plaintext[data.len] = content_type;
    const plaintext_len = data.len + 1;

    // Encrypt
    var ciphertext: [16384]u8 = undefined;
    var tag: [16]u8 = undefined;

    const total_len = plaintext_len + 16;
    var aad: [5]u8 = undefined;
    aad[0] = 0x17;
    aad[1] = 0x03;
    aad[2] = 0x03;
    aad[3] = @truncate(total_len >> 8);
    aad[4] = @truncate(total_len);

    cipher.encrypt(nonce, plaintext[0..plaintext_len], &aad, ciphertext[0..plaintext_len], &tag);

    // Send record
    var header: [5]u8 = undefined;
    header[0] = 23; // application_data
    header[1] = 0x03;
    header[2] = 0x03;
    header[3] = @truncate(total_len >> 8);
    header[4] = @truncate(total_len);

    try socketWriteAll(entry.fd, &header);
    try socketWriteAll(entry.fd, ciphertext[0..plaintext_len]);
    try socketWriteAll(entry.fd, &tag);
}

/// __edgebox_tls_read(id, maxBytes) - Read decrypted data
fn tlsRead(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return quickjs.jsNull();
    }

    var tls_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &tls_id, argv[0]);

    const entry = getTlsConnection(tls_id) orelse {
        return quickjs.jsNull();
    };

    if (!entry.handshake_complete) {
        return quickjs.jsNull();
    }

    // Try to receive data
    const record = receiveEncryptedRecord(entry) catch {
        return quickjs.jsNull();
    };

    if (record.content_type != 23 or record.len == 0) { // application_data
        return quickjs.jsUndefined();
    }

    return qjs.JS_NewStringLen(ctx, @ptrCast(record.payload.ptr), @intCast(record.len));
}

/// __edgebox_tls_write(id, data) - Write encrypted data
fn tlsWrite(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var tls_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &tls_id, argv[0]);

    const entry = getTlsConnection(tls_id) orelse {
        return qjs.JS_NewInt32(ctx, -1);
    };

    if (!entry.handshake_complete) {
        return qjs.JS_NewInt32(ctx, -2);
    }

    const data_str = qjs.JS_ToCString(ctx, argv[1]);
    if (data_str == null) {
        return qjs.JS_NewInt32(ctx, -3);
    }
    defer qjs.JS_FreeCString(ctx, data_str);
    const data = std.mem.span(data_str);

    sendEncryptedRecord(entry, 23, data) catch {
        return qjs.JS_NewInt32(ctx, -4);
    };

    return qjs.JS_NewInt32(ctx, @intCast(data.len));
}

/// __edgebox_tls_close(id) - Close TLS connection
fn tlsClose(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var tls_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &tls_id, argv[0]);

    if (tls_id < 0 or tls_id >= MAX_TLS_CONNECTIONS) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    const idx: usize = @intCast(tls_id);
    if (tls_connections[idx].fd != -1) {
        _ = c.close(tls_connections[idx].fd);
        tls_connections[idx] = .{};
    }

    return qjs.JS_NewInt32(ctx, 0);
}

/// __edgebox_tls_state(id) - Get TLS connection state
fn tlsState(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_NewInt32(ctx, 0);
    }

    var tls_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &tls_id, argv[0]);

    const entry = getTlsConnection(tls_id) orelse {
        return qjs.JS_NewInt32(ctx, 0); // closed
    };

    // 0=closed, 1=connecting, 2=connected
    if (entry.handshake_complete) {
        return qjs.JS_NewInt32(ctx, 2);
    }
    return qjs.JS_NewInt32(ctx, 1);
}

/// Register TLS module native functions
pub fn register(ctx: ?*qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Register TLS functions on global object
    inline for (.{
        .{ "__edgebox_tls_connect", tlsConnect, 2 },
        .{ "__edgebox_tls_read", tlsRead, 2 },
        .{ "__edgebox_tls_write", tlsWrite, 2 },
        .{ "__edgebox_tls_close", tlsClose, 1 },
        .{ "__edgebox_tls_state", tlsState, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, global, binding[0], func);
    }
}
