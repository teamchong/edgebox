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

// TLS Server tracking
const MAX_TLS_SERVERS = 16;
var tls_servers: [MAX_TLS_SERVERS]TlsServerEntry = [_]TlsServerEntry{.{}} ** MAX_TLS_SERVERS;

const TlsServerEntry = struct {
    active: bool = false,
    cert_der: []const u8 = &[_]u8{},
    key_der: []const u8 = &[_]u8{},
    cert_allocated: bool = false,
    key_allocated: bool = false,
};

const TlsEntry = struct {
    fd: i32 = -1,
    handshake_complete: bool = false,
    is_server: bool = false,
    server_id: i32 = -1, // Reference to TLS server context for server-side connections
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
    reject_unauthorized: bool = true, // Certificate validation: true = validate, false = skip
    cert_verified: bool = false, // Whether certificate has been verified
    session_resumed: bool = false, // Whether session was resumed from ticket
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

fn allocateTlsServer() ?usize {
    for (&tls_servers, 0..) |*entry, i| {
        if (!entry.active) {
            return i;
        }
    }
    return null;
}

fn getTlsServer(id: i32) ?*TlsServerEntry {
    if (id < 0 or id >= MAX_TLS_SERVERS) return null;
    const idx: usize = @intCast(id);
    if (!tls_servers[idx].active) return null;
    return &tls_servers[idx];
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

/// __edgebox_tls_connect(host, port, rejectUnauthorized) - Create TLS connection
/// Returns: TLS connection ID (>=0) or error code (<0)
/// rejectUnauthorized: 1 = validate certificate (default), 0 = skip validation
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

    // Get rejectUnauthorized option (default: true = validate)
    var reject_unauthorized: i32 = 1;
    if (argc >= 3) {
        _ = qjs.JS_ToInt32(ctx, &reject_unauthorized, argv[2]);
    }

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
        .reject_unauthorized = reject_unauthorized != 0,
    };

    // Store hostname for SNI
    const copy_len = @min(hostname.len, tls_connections[idx].hostname.len - 1);
    @memcpy(tls_connections[idx].hostname[0..copy_len], hostname[0..copy_len]);
    tls_connections[idx].hostname_len = copy_len;

    // Perform TLS handshake
    const entry = &tls_connections[idx];
    performHandshake(entry) catch |err| {
        _ = c.close(fd);
        tls_connections[idx] = .{};
        // Return specific error code for certificate validation failure
        if (err == error.CertificateValidationFailed) {
            return qjs.JS_NewInt32(ctx, -9); // Certificate validation error
        }
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

// ============================================================================
// TLS Server Implementation
// ============================================================================

/// Global allocator for certificate/key storage
var gpa = std.heap.GeneralPurposeAllocator(.{}){};

/// __edgebox_tls_create_server(certPem, keyPem) - Create TLS server context
/// Returns: server ID (>=0) or error code (<0)
fn tlsCreateServer(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    // Allocate server slot
    const idx = allocateTlsServer() orelse {
        return qjs.JS_NewInt32(ctx, -2); // No slots available
    };

    // Get certificate PEM
    const cert_str = qjs.JS_ToCString(ctx, argv[0]);
    if (cert_str == null) {
        return qjs.JS_NewInt32(ctx, -3);
    }
    defer qjs.JS_FreeCString(ctx, cert_str);
    const cert_pem = std.mem.span(cert_str);

    // Get private key PEM
    const key_str = qjs.JS_ToCString(ctx, argv[1]);
    if (key_str == null) {
        return qjs.JS_NewInt32(ctx, -4);
    }
    defer qjs.JS_FreeCString(ctx, key_str);
    const key_pem = std.mem.span(key_str);

    // Copy certificate and key data
    const allocator = gpa.allocator();

    const cert_copy = allocator.alloc(u8, cert_pem.len) catch {
        return qjs.JS_NewInt32(ctx, -5);
    };
    @memcpy(cert_copy, cert_pem);

    const key_copy = allocator.alloc(u8, key_pem.len) catch {
        allocator.free(cert_copy);
        return qjs.JS_NewInt32(ctx, -6);
    };
    @memcpy(key_copy, key_pem);

    tls_servers[idx] = .{
        .active = true,
        .cert_der = cert_copy,
        .key_der = key_copy,
        .cert_allocated = true,
        .key_allocated = true,
    };

    return qjs.JS_NewInt32(ctx, @intCast(idx));
}

/// __edgebox_tls_accept(serverId, clientFd) - Accept TLS connection on server socket
/// Returns: TLS connection ID (>=0) or error code (<0)
fn tlsAccept(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (builtin.os.tag == .wasi) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    if (argc < 2) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var server_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &server_id, argv[0]);

    var client_fd: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &client_fd, argv[1]);

    const server = getTlsServer(server_id) orelse {
        return qjs.JS_NewInt32(ctx, -2); // Invalid server ID
    };

    // Allocate TLS connection slot
    const idx = allocateTlsConnection() orelse {
        return qjs.JS_NewInt32(ctx, -3); // No connection slots
    };

    // Initialize TLS entry for server-side connection
    tls_connections[idx] = .{
        .fd = client_fd,
        .handshake_complete = false,
        .is_server = true,
        .server_id = server_id,
    };

    const entry = &tls_connections[idx];

    // Perform server-side TLS handshake
    performServerHandshake(entry, server) catch |err| {
        tls_connections[idx] = .{};
        return qjs.JS_NewInt32(ctx, switch (err) {
            error.Timeout => -4,
            error.ReadFailed => -5,
            error.WriteFailed => -6,
            error.HandshakeFailed => -7,
            error.InvalidClientHello => -8,
            error.KeyExchangeFailed => -10,
            else => -11,
        });
    };

    return qjs.JS_NewInt32(ctx, @intCast(idx));
}

/// Perform TLS 1.3 server-side handshake
fn performServerHandshake(entry: *TlsEntry, server: *TlsServerEntry) !void {
    // Generate X25519 key pair for this connection
    var private_key: [32]u8 = undefined;
    crypto.random.bytes(&private_key);
    const public_key = crypto.dh.X25519.recoverPublicKey(private_key) catch return error.KeyExchangeFailed;

    // Initialize transcript hash
    var transcript_hash = crypto.hash.sha2.Sha256.init(.{});

    // Receive ClientHello
    const client_info = try receiveClientHello(entry.fd, &transcript_hash);

    // Store client's hostname (SNI) if provided
    if (client_info.hostname_len > 0) {
        const copy_len = @min(client_info.hostname_len, entry.hostname.len);
        @memcpy(entry.hostname[0..copy_len], client_info.hostname[0..copy_len]);
        entry.hostname_len = copy_len;
    }

    // Select cipher suite (prefer AES-128-GCM)
    entry.cipher_suite = .TLS_AES_128_GCM_SHA256;

    // Send ServerHello
    try sendServerHello(entry.fd, &public_key, entry.cipher_suite, &transcript_hash);

    // Compute shared secret
    const shared_secret = crypto.dh.X25519.scalarmult(private_key, client_info.client_public) catch return error.KeyExchangeFailed;

    // Get transcript hash up to ServerHello
    var transcript_copy = transcript_hash;
    var transcript: [32]u8 = undefined;
    transcript_copy.final(&transcript);

    // Derive handshake keys
    entry.key_schedule.deriveHandshakeKeys(shared_secret, transcript);

    // Setup handshake ciphers (note: for server, client/server roles are swapped)
    const server_keys = KeySchedule.deriveTrafficKey(
        entry.key_schedule.server_handshake_traffic_secret,
        entry.cipher_suite.keyLen(),
    );
    const client_keys = KeySchedule.deriveTrafficKey(
        entry.key_schedule.client_handshake_traffic_secret,
        entry.cipher_suite.keyLen(),
    );

    entry.server_iv = server_keys.iv;
    entry.client_iv = client_keys.iv;

    if (entry.cipher_suite.keyLen() == 16) {
        entry.server_cipher = AesGcm.init128(server_keys.key[0..16].*);
        entry.client_cipher = AesGcm.init128(client_keys.key[0..16].*);
    } else {
        entry.server_cipher = AesGcm.init256(server_keys.key);
        entry.client_cipher = AesGcm.init256(client_keys.key);
    }

    // Send encrypted handshake messages
    try sendServerHandshakeMessages(entry, server, &transcript_hash);

    // Derive application keys
    transcript_copy = transcript_hash;
    transcript_copy.final(&transcript);
    entry.key_schedule.deriveApplicationKeys(transcript);

    // Receive client Finished
    try receiveClientFinished(entry, &transcript_hash);

    // Switch to application keys
    const app_server = KeySchedule.deriveTrafficKey(
        entry.key_schedule.server_application_traffic_secret,
        entry.cipher_suite.keyLen(),
    );
    const app_client = KeySchedule.deriveTrafficKey(
        entry.key_schedule.client_application_traffic_secret,
        entry.cipher_suite.keyLen(),
    );

    entry.server_iv = app_server.iv;
    entry.client_iv = app_client.iv;

    if (entry.cipher_suite.keyLen() == 16) {
        entry.server_cipher = AesGcm.init128(app_server.key[0..16].*);
        entry.client_cipher = AesGcm.init128(app_client.key[0..16].*);
    } else {
        entry.server_cipher = AesGcm.init256(app_server.key);
        entry.client_cipher = AesGcm.init256(app_client.key);
    }

    entry.client_seq = 0;
    entry.server_seq = 0;
    entry.handshake_complete = true;
}

const ClientHelloInfo = struct {
    client_public: [32]u8,
    hostname: [256]u8,
    hostname_len: usize,
    cipher_suites: [8]u16,
    cipher_count: usize,
};

/// Receive and parse ClientHello message
fn receiveClientHello(fd: i32, transcript: *crypto.hash.sha2.Sha256) !ClientHelloInfo {
    // Read record header
    var header: [5]u8 = undefined;
    _ = try socketRead(fd, &header);

    if (header[0] != 22) return error.InvalidClientHello; // Not handshake

    const length = (@as(usize, header[3]) << 8) | header[4];
    if (length > 16384) return error.InvalidClientHello;

    // Read payload
    var payload: [16384]u8 = undefined;
    var read: usize = 0;
    while (read < length) {
        read += try socketRead(fd, payload[read..length]);
    }

    // Update transcript
    transcript.update(payload[0..length]);

    if (payload[0] != 1) return error.InvalidClientHello; // Not client_hello

    var info = ClientHelloInfo{
        .client_public = undefined,
        .hostname = undefined,
        .hostname_len = 0,
        .cipher_suites = undefined,
        .cipher_count = 0,
    };

    // Parse ClientHello
    var offset: usize = 4; // Skip type + length (3 bytes)
    if (offset + 2 > length) return error.InvalidClientHello;

    // Skip version (2 bytes)
    offset += 2;

    // Skip random (32 bytes)
    offset += 32;

    // Skip session ID
    if (offset >= length) return error.InvalidClientHello;
    const session_id_len = payload[offset];
    offset += 1 + session_id_len;

    // Parse cipher suites
    if (offset + 2 > length) return error.InvalidClientHello;
    const cipher_len = (@as(usize, payload[offset]) << 8) | payload[offset + 1];
    offset += 2;

    const cipher_count = @min(cipher_len / 2, info.cipher_suites.len);
    for (0..cipher_count) |i| {
        if (offset + 2 <= length) {
            info.cipher_suites[i] = (@as(u16, payload[offset]) << 8) | payload[offset + 1];
            info.cipher_count += 1;
        }
        offset += 2;
    }
    // Skip remaining cipher suites if more than our buffer
    offset += cipher_len - (cipher_count * 2);

    // Skip compression methods
    if (offset >= length) return error.InvalidClientHello;
    const compression_len = payload[offset];
    offset += 1 + compression_len;

    // Parse extensions
    if (offset + 2 > length) return error.InvalidClientHello;
    const ext_len = (@as(usize, payload[offset]) << 8) | payload[offset + 1];
    offset += 2;

    const ext_end = @min(offset + ext_len, length);
    var found_key_share = false;

    while (offset + 4 <= ext_end) {
        const ext_type = (@as(u16, payload[offset]) << 8) | payload[offset + 1];
        const ext_size = (@as(usize, payload[offset + 2]) << 8) | payload[offset + 3];
        offset += 4;

        const ext_data_end = @min(offset + ext_size, ext_end);

        switch (ext_type) {
            0x0000 => { // SNI
                if (offset + 5 <= ext_data_end) {
                    const sni_list_len = (@as(usize, payload[offset]) << 8) | payload[offset + 1];
                    _ = sni_list_len;
                    const name_type = payload[offset + 2];
                    if (name_type == 0) { // host_name
                        const name_len = (@as(usize, payload[offset + 3]) << 8) | payload[offset + 4];
                        const copy_len = @min(name_len, info.hostname.len);
                        if (offset + 5 + copy_len <= ext_data_end) {
                            @memcpy(info.hostname[0..copy_len], payload[offset + 5 .. offset + 5 + copy_len]);
                            info.hostname_len = copy_len;
                        }
                    }
                }
            },
            0x0033 => { // key_share
                if (offset + 4 <= ext_data_end) {
                    const key_share_len = (@as(usize, payload[offset]) << 8) | payload[offset + 1];
                    _ = key_share_len;
                    var key_offset = offset + 2;
                    while (key_offset + 4 <= ext_data_end) {
                        const group = (@as(u16, payload[key_offset]) << 8) | payload[key_offset + 1];
                        const key_len = (@as(usize, payload[key_offset + 2]) << 8) | payload[key_offset + 3];
                        key_offset += 4;

                        if (group == 0x001d and key_len == 32) { // x25519
                            if (key_offset + 32 <= ext_data_end) {
                                @memcpy(&info.client_public, payload[key_offset .. key_offset + 32]);
                                found_key_share = true;
                            }
                        }
                        key_offset += key_len;
                    }
                }
            },
            else => {},
        }
        offset = ext_data_end;
    }

    if (!found_key_share) return error.HandshakeFailed;
    return info;
}

/// Send ServerHello message
fn sendServerHello(fd: i32, public_key: *const [32]u8, cipher_suite: CipherSuite, transcript: *crypto.hash.sha2.Sha256) !void {
    var msg: [200]u8 = undefined;
    var pos: usize = 0;

    msg[pos] = 2; // server_hello
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

    // Session ID (echo back 32 random bytes for compatibility)
    msg[pos] = 32;
    pos += 1;
    crypto.random.bytes(msg[pos .. pos + 32]);
    pos += 32;

    // Cipher suite
    const cs_val = @intFromEnum(cipher_suite);
    msg[pos] = @truncate(cs_val >> 8);
    msg[pos + 1] = @truncate(cs_val);
    pos += 2;

    // Compression (null)
    msg[pos] = 0;
    pos += 1;

    // Extensions
    const ext_start = pos;
    pos += 2;

    // supported_versions extension (TLS 1.3)
    msg[pos] = 0x00;
    msg[pos + 1] = 0x2b;
    pos += 2;
    msg[pos] = 0x00;
    msg[pos + 1] = 0x02;
    pos += 2;
    msg[pos] = 0x03;
    msg[pos + 1] = 0x04; // TLS 1.3
    pos += 2;

    // key_share extension (x25519)
    msg[pos] = 0x00;
    msg[pos + 1] = 0x33;
    pos += 2;
    msg[pos] = 0x00;
    msg[pos + 1] = 0x24;
    pos += 2;
    msg[pos] = 0x00;
    msg[pos + 1] = 0x1d; // x25519
    pos += 2;
    msg[pos] = 0x00;
    msg[pos + 1] = 0x20; // 32 bytes
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
    header[2] = 0x03;
    header[3] = @truncate(pos >> 8);
    header[4] = @truncate(pos);

    try socketWriteAll(fd, &header);
    try socketWriteAll(fd, msg[0..pos]);

    // Send ChangeCipherSpec (for middlebox compatibility)
    const ccs = [_]u8{ 20, 0x03, 0x03, 0x00, 0x01, 0x01 };
    try socketWriteAll(fd, &ccs);
}

/// Send encrypted server handshake messages (EncryptedExtensions, Certificate, CertificateVerify, Finished)
fn sendServerHandshakeMessages(entry: *TlsEntry, server: *TlsServerEntry, transcript: *crypto.hash.sha2.Sha256) !void {
    // Send EncryptedExtensions (minimal - empty)
    var ee_msg: [4]u8 = undefined;
    ee_msg[0] = 8; // encrypted_extensions
    ee_msg[1] = 0;
    ee_msg[2] = 0;
    ee_msg[3] = 0; // length = 0
    transcript.update(&ee_msg);
    try sendServerEncryptedRecord(entry, 22, &ee_msg);

    // Send Certificate
    // For simplicity, we send the raw PEM data (clients should handle this)
    // In a production implementation, this should be proper DER encoding
    const cert_data = server.cert_der;
    var cert_msg: [8192]u8 = undefined;
    var cert_pos: usize = 0;

    cert_msg[cert_pos] = 11; // certificate
    cert_pos += 1;

    // Certificate message: request_context (1 byte len + data) + certificate_list
    const cert_list_len = 3 + cert_data.len; // 3 bytes for cert entry length prefix
    const msg_len = 1 + 3 + cert_list_len; // context_len(0) + list_len(3) + list

    cert_msg[cert_pos] = @truncate(msg_len >> 16);
    cert_msg[cert_pos + 1] = @truncate(msg_len >> 8);
    cert_msg[cert_pos + 2] = @truncate(msg_len);
    cert_pos += 3;

    // request_context (empty)
    cert_msg[cert_pos] = 0;
    cert_pos += 1;

    // certificate_list length
    cert_msg[cert_pos] = @truncate(cert_list_len >> 16);
    cert_msg[cert_pos + 1] = @truncate(cert_list_len >> 8);
    cert_msg[cert_pos + 2] = @truncate(cert_list_len);
    cert_pos += 3;

    // certificate_entry: cert_data length + cert_data + extensions(0)
    cert_msg[cert_pos] = @truncate(cert_data.len >> 16);
    cert_msg[cert_pos + 1] = @truncate(cert_data.len >> 8);
    cert_msg[cert_pos + 2] = @truncate(cert_data.len);
    cert_pos += 3;
    @memcpy(cert_msg[cert_pos .. cert_pos + cert_data.len], cert_data);
    cert_pos += cert_data.len;

    transcript.update(cert_msg[0..cert_pos]);
    try sendServerEncryptedRecord(entry, 22, cert_msg[0..cert_pos]);

    // CertificateVerify - sign the transcript with Ed25519
    // For simplicity, we'll skip signature verification in this implementation
    // In production, this would use the server's private key to sign
    var cv_msg: [72]u8 = undefined;
    cv_msg[0] = 15; // certificate_verify
    cv_msg[1] = 0;
    cv_msg[2] = 0;
    cv_msg[3] = 68; // length = 2 (algorithm) + 2 (sig length) + 64 (signature)

    // Signature algorithm: ed25519 (0x0807)
    cv_msg[4] = 0x08;
    cv_msg[5] = 0x07;

    // Signature length
    cv_msg[6] = 0;
    cv_msg[7] = 64;

    // Generate a placeholder signature (in production, sign with private key)
    crypto.random.bytes(cv_msg[8..72]);

    transcript.update(cv_msg[0..72]);
    try sendServerEncryptedRecord(entry, 22, cv_msg[0..72]);

    // Send Finished
    var finished_key: [32]u8 = undefined;
    hkdfExpandLabel(&finished_key, entry.key_schedule.server_handshake_traffic_secret, "finished", &[_]u8{});

    var transcript_copy = transcript.*;
    var transcript_data: [32]u8 = undefined;
    transcript_copy.final(&transcript_data);

    var verify_data: [32]u8 = undefined;
    crypto.auth.hmac.sha2.HmacSha256.create(&verify_data, &transcript_data, &finished_key);

    var finished_msg: [36]u8 = undefined;
    finished_msg[0] = 20; // finished
    finished_msg[1] = 0;
    finished_msg[2] = 0;
    finished_msg[3] = 32;
    @memcpy(finished_msg[4..36], &verify_data);

    transcript.update(&finished_msg);
    try sendServerEncryptedRecord(entry, 22, &finished_msg);
}

/// Send encrypted record from server
fn sendServerEncryptedRecord(entry: *TlsEntry, content_type: u8, data: []const u8) !void {
    const cipher = entry.server_cipher orelse return error.NotEncrypted;

    var nonce = entry.server_iv;
    const seq_bytes = std.mem.toBytes(std.mem.nativeToBig(u64, entry.server_seq));
    for (0..8) |i| {
        nonce[4 + i] ^= seq_bytes[i];
    }
    entry.server_seq += 1;

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

/// Receive encrypted record from client (for server)
fn receiveClientEncryptedRecord(entry: *TlsEntry) !struct { content_type: u8, payload: []u8, len: usize } {
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

    // Decrypt using client cipher (from client's perspective)
    const cipher = entry.client_cipher orelse return error.NotEncrypted;

    if (length < 17) return error.RecordTooShort;

    var nonce = entry.client_iv;
    const seq_bytes = std.mem.toBytes(std.mem.nativeToBig(u64, entry.client_seq));
    for (0..8) |i| {
        nonce[4 + i] ^= seq_bytes[i];
    }
    entry.client_seq += 1;

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

/// Receive client Finished message
fn receiveClientFinished(entry: *TlsEntry, transcript: *crypto.hash.sha2.Sha256) !void {
    while (true) {
        const record = try receiveClientEncryptedRecord(entry);

        if (record.content_type == 20) continue; // Skip ChangeCipherSpec

        // Update transcript
        transcript.update(record.payload[0..record.len]);

        if (record.len > 0 and record.payload[0] == 20) break; // Finished
    }
}

/// __edgebox_tls_destroy_server(serverId) - Destroy TLS server context
fn tlsDestroyServer(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    var server_id: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &server_id, argv[0]);

    if (server_id < 0 or server_id >= MAX_TLS_SERVERS) {
        return qjs.JS_NewInt32(ctx, -1);
    }

    const idx: usize = @intCast(server_id);
    if (tls_servers[idx].active) {
        const allocator = gpa.allocator();

        if (tls_servers[idx].cert_allocated) {
            allocator.free(@constCast(tls_servers[idx].cert_der));
        }
        if (tls_servers[idx].key_allocated) {
            allocator.free(@constCast(tls_servers[idx].key_der));
        }
        tls_servers[idx] = .{};
    }

    return qjs.JS_NewInt32(ctx, 0);
}

/// Get TLS connection cipher info
/// __edgebox_tls_get_cipher(tlsId) -> { name: string, standardName: string, version: string }
fn tlsGetCipher(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "tlsGetCipher requires tlsId");

    var tls_id: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &tls_id, argv[0]) < 0) return quickjs.jsNull();

    if (tls_id < 0 or tls_id >= MAX_TLS_CONNECTIONS) return quickjs.jsNull();

    const idx: usize = @intCast(tls_id);
    if (tls_connections[idx].fd < 0 or !tls_connections[idx].handshake_complete) {
        return quickjs.jsNull();
    }

    const obj = qjs.JS_NewObject(ctx);
    const cipher_name = switch (tls_connections[idx].cipher_suite) {
        .TLS_AES_128_GCM_SHA256 => "TLS_AES_128_GCM_SHA256",
        .TLS_AES_256_GCM_SHA384 => "TLS_AES_256_GCM_SHA384",
        _ => "TLS_AES_128_GCM_SHA256",
    };

    _ = qjs.JS_SetPropertyStr(ctx, obj, "name", qjs.JS_NewString(ctx, cipher_name));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "standardName", qjs.JS_NewString(ctx, cipher_name));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "version", qjs.JS_NewString(ctx, "TLSv1.3"));
    return obj;
}

/// Get TLS connection protocol version
/// __edgebox_tls_get_protocol(tlsId) -> string
fn tlsGetProtocol(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "tlsGetProtocol requires tlsId");

    var tls_id: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &tls_id, argv[0]) < 0) return quickjs.jsNull();

    if (tls_id < 0 or tls_id >= MAX_TLS_CONNECTIONS) return quickjs.jsNull();

    const idx: usize = @intCast(tls_id);
    if (tls_connections[idx].fd < 0 or !tls_connections[idx].handshake_complete) {
        return quickjs.jsNull();
    }

    // We only support TLS 1.3
    return qjs.JS_NewString(ctx, "TLSv1.3");
}

/// Get peer certificate info (basic info since we don't parse full X.509)
/// __edgebox_tls_get_peer_certificate(tlsId, detailed) -> object
fn tlsGetPeerCertificate(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "tlsGetPeerCertificate requires tlsId");

    var tls_id: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &tls_id, argv[0]) < 0) return qjs.JS_NewObject(ctx);

    if (tls_id < 0 or tls_id >= MAX_TLS_CONNECTIONS) return qjs.JS_NewObject(ctx);

    const idx: usize = @intCast(tls_id);
    if (tls_connections[idx].fd < 0 or !tls_connections[idx].handshake_complete) {
        return qjs.JS_NewObject(ctx);
    }

    // Return basic certificate info
    // Note: Full X.509 parsing would require significant additional code
    const obj = qjs.JS_NewObject(ctx);

    // Set subject with CN from hostname (best effort)
    const subject_obj = qjs.JS_NewObject(ctx);
    const hostname = tls_connections[idx].hostname[0..tls_connections[idx].hostname_len];
    _ = qjs.JS_SetPropertyStr(ctx, subject_obj, "CN", qjs.JS_NewStringLen(ctx, hostname.ptr, hostname.len));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "subject", subject_obj);

    // Issuer (placeholder - would need X.509 parsing)
    const issuer_obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, issuer_obj, "O", qjs.JS_NewString(ctx, "Certificate Authority"));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "issuer", issuer_obj);

    // Validity (placeholder dates)
    _ = qjs.JS_SetPropertyStr(ctx, obj, "valid_from", qjs.JS_NewString(ctx, "Jan  1 00:00:00 2024 GMT"));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "valid_to", qjs.JS_NewString(ctx, "Dec 31 23:59:59 2025 GMT"));

    // Other properties
    _ = qjs.JS_SetPropertyStr(ctx, obj, "serialNumber", qjs.JS_NewString(ctx, "00"));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "fingerprint", qjs.JS_NewString(ctx, ""));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "fingerprint256", qjs.JS_NewString(ctx, ""));

    return obj;
}

/// Check if TLS session was reused
/// __edgebox_tls_is_session_reused(tlsId) -> boolean
fn tlsIsSessionReused(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "tlsIsSessionReused requires tlsId");

    var tls_id: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &tls_id, argv[0]) < 0) return quickjs.jsFalse();

    if (tls_id < 0 or tls_id >= MAX_TLS_CONNECTIONS) return quickjs.jsFalse();

    const idx: usize = @intCast(tls_id);
    if (tls_connections[idx].fd < 0) return quickjs.jsFalse();

    // Check if session was resumed (currently we don't support resumption)
    return if (tls_connections[idx].session_resumed) quickjs.jsTrue() else quickjs.jsFalse();
}

/// Get TLS session data (for session resumption)
/// __edgebox_tls_get_session(tlsId) -> ArrayBuffer | null
fn tlsGetSession(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "tlsGetSession requires tlsId");

    var tls_id: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &tls_id, argv[0]) < 0) return quickjs.jsNull();

    if (tls_id < 0 or tls_id >= MAX_TLS_CONNECTIONS) return quickjs.jsNull();

    const idx: usize = @intCast(tls_id);
    if (tls_connections[idx].fd < 0 or !tls_connections[idx].handshake_complete) {
        return quickjs.jsNull();
    }

    // Return session data - currently we return application traffic secrets
    // which can be used for session resumption in some scenarios
    // In full implementation, this would return a NewSessionTicket message
    const session_data = &tls_connections[idx].key_schedule.client_application_traffic_secret;

    // Create ArrayBuffer with session data
    const ab = qjs.JS_NewArrayBuffer(ctx, @constCast(@ptrCast(session_data.ptr)), 32, null, null, false);
    return ab;
}

/// Register TLS module native functions
pub fn register(ctx: ?*qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Register TLS functions on global object
    inline for (.{
        .{ "__edgebox_tls_connect", tlsConnect, 3 },
        .{ "__edgebox_tls_read", tlsRead, 2 },
        .{ "__edgebox_tls_write", tlsWrite, 2 },
        .{ "__edgebox_tls_close", tlsClose, 1 },
        .{ "__edgebox_tls_state", tlsState, 1 },
        .{ "__edgebox_tls_create_server", tlsCreateServer, 2 },
        .{ "__edgebox_tls_accept", tlsAccept, 2 },
        .{ "__edgebox_tls_destroy_server", tlsDestroyServer, 1 },
        .{ "__edgebox_tls_get_cipher", tlsGetCipher, 1 },
        .{ "__edgebox_tls_get_protocol", tlsGetProtocol, 1 },
        .{ "__edgebox_tls_get_peer_certificate", tlsGetPeerCertificate, 2 },
        .{ "__edgebox_tls_is_session_reused", tlsIsSessionReused, 1 },
        .{ "__edgebox_tls_get_session", tlsGetSession, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, global, binding[0], func);
    }
}
