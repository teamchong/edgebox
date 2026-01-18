/// Native crypto module - QuickJS C functions
/// Hash, HMAC, AES-GCM encryption using Zig's std.crypto
/// Only random_bytes needs to remain as host function for CSPRNG
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// Stack buffers for crypto operations
var hash_buffer: [64]u8 = undefined; // SHA-512 is 64 bytes max
var encrypt_buffer: [65536]u8 = undefined; // 64KB for encryption output
var decrypt_buffer: [65536]u8 = undefined; // 64KB for decryption output

/// hash(algorithm, data) - Compute cryptographic hash
/// Supported: sha256, sha384, sha512, sha1, md5
fn hashFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) {
        return qjs.JS_ThrowTypeError(ctx, "hash() requires 2 arguments: algorithm, data");
    }

    // Get algorithm name
    const algo_str = qjs.JS_ToCString(ctx, argv[0]);
    if (algo_str == null) {
        return qjs.JS_ThrowTypeError(ctx, "Invalid algorithm");
    }
    defer qjs.JS_FreeCString(ctx, algo_str);
    const algorithm = std.mem.span(algo_str);

    // Get data to hash
    var data_bytes: []const u8 = undefined;
    var size: usize = 0;
    const ptr = qjs.JS_GetArrayBuffer(ctx, &size, argv[1]);

    if (ptr != null) {
        // Direct ArrayBuffer
        data_bytes = @as([*]const u8, @ptrCast(ptr))[0..size];
    } else {
        // Try as string
        const str = qjs.JS_ToCString(ctx, argv[1]);
        if (str == null) {
            return qjs.JS_ThrowTypeError(ctx, "Data must be string or ArrayBuffer");
        }
        defer qjs.JS_FreeCString(ctx, str);
        data_bytes = std.mem.span(str);
    }

    // Compute hash based on algorithm
    if (std.mem.eql(u8, algorithm, "sha256")) {
        var hash: [32]u8 = undefined;
        std.crypto.hash.sha2.Sha256.hash(data_bytes, &hash, .{});
        return hashToHex(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "sha384")) {
        var hash: [48]u8 = undefined;
        std.crypto.hash.sha2.Sha384.hash(data_bytes, &hash, .{});
        return hashToHex(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "sha512")) {
        var hash: [64]u8 = undefined;
        std.crypto.hash.sha2.Sha512.hash(data_bytes, &hash, .{});
        return hashToHex(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "sha1")) {
        var hash: [20]u8 = undefined;
        std.crypto.hash.Sha1.hash(data_bytes, &hash, .{});
        return hashToHex(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "md5")) {
        var hash: [16]u8 = undefined;
        std.crypto.hash.Md5.hash(data_bytes, &hash, .{});
        return hashToHex(ctx, &hash);
    } else {
        return qjs.JS_ThrowTypeError(ctx, "Unsupported hash algorithm (use sha256, sha384, sha512, sha1, or md5)");
    }
}

/// Helper: Convert hash bytes to hex string
fn hashToHex(ctx: ?*qjs.JSContext, hash: []const u8) qjs.JSValue {
    const hex_chars = "0123456789abcdef";
    const hex_len = hash.len * 2;

    if (hex_len > hash_buffer.len) {
        return qjs.JS_ThrowRangeError(ctx, "Hash too large");
    }

    for (hash, 0..) |byte, i| {
        hash_buffer[i * 2] = hex_chars[byte >> 4];
        hash_buffer[i * 2 + 1] = hex_chars[byte & 0x0F];
    }

    return qjs.JS_NewStringLen(ctx, &hash_buffer, @intCast(hex_len));
}

/// hmac(algorithm, key, data) - Compute HMAC
/// Supported: sha256, sha384, sha512, sha1, md5
fn hmacFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) {
        return qjs.JS_ThrowTypeError(ctx, "hmac() requires 3 arguments: algorithm, key, data");
    }

    // Get algorithm
    const algo_str = qjs.JS_ToCString(ctx, argv[0]);
    if (algo_str == null) {
        return qjs.JS_ThrowTypeError(ctx, "Invalid algorithm");
    }
    defer qjs.JS_FreeCString(ctx, algo_str);
    const algorithm = std.mem.span(algo_str);

    // Get key
    const key_str = qjs.JS_ToCString(ctx, argv[1]);
    if (key_str == null) {
        return qjs.JS_ThrowTypeError(ctx, "Invalid key");
    }
    defer qjs.JS_FreeCString(ctx, key_str);
    const key = std.mem.span(key_str);

    // Get data
    const data_str = qjs.JS_ToCString(ctx, argv[2]);
    if (data_str == null) {
        return qjs.JS_ThrowTypeError(ctx, "Invalid data");
    }
    defer qjs.JS_FreeCString(ctx, data_str);
    const data = std.mem.span(data_str);

    // Compute HMAC based on algorithm
    if (std.mem.eql(u8, algorithm, "sha256")) {
        var mac: [32]u8 = undefined;
        std.crypto.auth.hmac.sha2.HmacSha256.create(&mac, data, key);
        return hashToHex(ctx, &mac);
    } else if (std.mem.eql(u8, algorithm, "sha384")) {
        var mac: [48]u8 = undefined;
        std.crypto.auth.hmac.sha2.HmacSha384.create(&mac, data, key);
        return hashToHex(ctx, &mac);
    } else if (std.mem.eql(u8, algorithm, "sha512")) {
        var mac: [64]u8 = undefined;
        std.crypto.auth.hmac.sha2.HmacSha512.create(&mac, data, key);
        return hashToHex(ctx, &mac);
    } else if (std.mem.eql(u8, algorithm, "sha1")) {
        var mac: [20]u8 = undefined;
        std.crypto.auth.hmac.HmacSha1.create(&mac, data, key);
        return hashToHex(ctx, &mac);
    } else if (std.mem.eql(u8, algorithm, "md5")) {
        var mac: [16]u8 = undefined;
        std.crypto.auth.hmac.HmacMd5.create(&mac, data, key);
        return hashToHex(ctx, &mac);
    } else {
        return qjs.JS_ThrowTypeError(ctx, "Unsupported HMAC algorithm (use sha256, sha384, sha512, sha1, or md5)");
    }
}

/// aesGcmEncrypt(key, iv, plaintext, aad) - AES-256-GCM encryption
fn aesGcmEncrypt(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) {
        return qjs.JS_ThrowTypeError(ctx, "aesGcmEncrypt() requires at least 3 arguments: key, iv, plaintext");
    }

    // Get key (must be 32 bytes for AES-256)
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null or key_size != 32) {
        return qjs.JS_ThrowTypeError(ctx, "Key must be 32 bytes (ArrayBuffer)");
    }
    const key = @as([*]const u8, @ptrCast(key_ptr))[0..32];

    // Get IV (must be 12 bytes for GCM)
    var iv_size: usize = 0;
    const iv_ptr = qjs.JS_GetArrayBuffer(ctx, &iv_size, argv[1]);
    if (iv_ptr == null or iv_size != 12) {
        return qjs.JS_ThrowTypeError(ctx, "IV must be 12 bytes (ArrayBuffer)");
    }
    const iv = @as([*]const u8, @ptrCast(iv_ptr))[0..12];

    // Get plaintext
    var plain_size: usize = 0;
    const plain_ptr = qjs.JS_GetArrayBuffer(ctx, &plain_size, argv[2]);
    if (plain_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Plaintext must be ArrayBuffer");
    }
    const plaintext = @as([*]const u8, @ptrCast(plain_ptr))[0..plain_size];

    // Get AAD (optional)
    var aad: []const u8 = &[_]u8{};
    if (argc >= 4) {
        var aad_size: usize = 0;
        const aad_ptr = qjs.JS_GetArrayBuffer(ctx, &aad_size, argv[3]);
        if (aad_ptr != null) {
            aad = @as([*]const u8, @ptrCast(aad_ptr))[0..aad_size];
        }
    }

    // Encrypt
    const output_len = plaintext.len + 16; // ciphertext + tag
    if (output_len > encrypt_buffer.len) {
        return qjs.JS_ThrowRangeError(ctx, "Plaintext too large for encryption buffer");
    }

    const ciphertext = encrypt_buffer[0..plaintext.len];
    var tag: [16]u8 = undefined;

    std.crypto.aead.aes_gcm.Aes256Gcm.encrypt(
        ciphertext,
        &tag,
        plaintext,
        aad,
        iv.*,
        key.*,
    );

    // Copy tag after ciphertext
    @memcpy(encrypt_buffer[plaintext.len..][0..16], &tag);

    // Return as Uint8Array
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    const len_val = qjs.JS_NewInt32(ctx, @intCast(output_len));
    var ctor_args = [1]qjs.JSValue{len_val};
    const arr = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    qjs.JS_FreeValue(ctx, len_val);

    if (qjs.JS_IsException(arr)) return arr;

    // Fill array with ciphertext + tag
    for (encrypt_buffer[0..output_len], 0..) |byte, i| {
        const byte_val = qjs.JS_NewInt32(ctx, @intCast(byte));
        _ = qjs.JS_SetPropertyUint32(ctx, arr, @intCast(i), byte_val);
    }

    return arr;
}

/// aesGcmDecrypt(key, iv, ciphertext_with_tag, aad) - AES-256-GCM decryption
fn aesGcmDecrypt(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) {
        return qjs.JS_ThrowTypeError(ctx, "aesGcmDecrypt() requires at least 3 arguments: key, iv, ciphertext");
    }

    // Get key
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null or key_size != 32) {
        return qjs.JS_ThrowTypeError(ctx, "Key must be 32 bytes");
    }
    const key = @as([*]const u8, @ptrCast(key_ptr))[0..32];

    // Get IV
    var iv_size: usize = 0;
    const iv_ptr = qjs.JS_GetArrayBuffer(ctx, &iv_size, argv[1]);
    if (iv_ptr == null or iv_size != 12) {
        return qjs.JS_ThrowTypeError(ctx, "IV must be 12 bytes");
    }
    const iv = @as([*]const u8, @ptrCast(iv_ptr))[0..12];

    // Get ciphertext + tag
    var cipher_size: usize = 0;
    const cipher_ptr = qjs.JS_GetArrayBuffer(ctx, &cipher_size, argv[2]);
    if (cipher_ptr == null or cipher_size < 16) {
        return qjs.JS_ThrowTypeError(ctx, "Ciphertext must include 16-byte tag");
    }
    const ciphertext_and_tag = @as([*]const u8, @ptrCast(cipher_ptr))[0..cipher_size];

    // Split ciphertext and tag
    const plaintext_len = cipher_size - 16;
    const ciphertext = ciphertext_and_tag[0..plaintext_len];
    const tag = ciphertext_and_tag[plaintext_len..][0..16];

    // Get AAD (optional)
    var aad: []const u8 = &[_]u8{};
    if (argc >= 4) {
        var aad_size: usize = 0;
        const aad_ptr = qjs.JS_GetArrayBuffer(ctx, &aad_size, argv[3]);
        if (aad_ptr != null) {
            aad = @as([*]const u8, @ptrCast(aad_ptr))[0..aad_size];
        }
    }

    // Decrypt
    if (plaintext_len > decrypt_buffer.len) {
        return qjs.JS_ThrowRangeError(ctx, "Ciphertext too large");
    }

    const plaintext = decrypt_buffer[0..plaintext_len];

    std.crypto.aead.aes_gcm.Aes256Gcm.decrypt(
        plaintext,
        ciphertext,
        tag.*,
        aad,
        iv.*,
        key.*,
    ) catch {
        return qjs.JS_ThrowTypeError(ctx, "Decryption failed (authentication error)");
    };

    // Return as Uint8Array
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    const len_val = qjs.JS_NewInt32(ctx, @intCast(plaintext_len));
    var ctor_args = [1]qjs.JSValue{len_val};
    const arr = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    qjs.JS_FreeValue(ctx, len_val);

    if (qjs.JS_IsException(arr)) return arr;

    // Fill array
    for (plaintext, 0..) |byte, i| {
        const byte_val = qjs.JS_NewInt32(ctx, @intCast(byte));
        _ = qjs.JS_SetPropertyUint32(ctx, arr, @intCast(i), byte_val);
    }

    return arr;
}

/// crypto.randomBytes(size) - Cryptographically secure random bytes
fn randomBytesFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "randomBytes requires size argument");

    var size: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &size, argv[0]) < 0) return quickjs.jsException();
    if (size <= 0) return qjs.JS_ThrowRangeError(ctx, "size must be positive");
    if (size > 65536) return qjs.JS_ThrowRangeError(ctx, "size too large");

    // Create Uint8Array
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);
    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    const len_val = qjs.JS_NewInt32(ctx, size);
    var ctor_args = [1]qjs.JSValue{len_val};
    const arr = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    qjs.JS_FreeValue(ctx, len_val);
    if (qjs.JS_IsException(arr)) return arr;

    // Fill with cryptographically secure random bytes
    var buf: [65536]u8 = undefined;
    std.crypto.random.bytes(buf[0..@intCast(size)]);

    for (0..@intCast(size)) |i| {
        const byte_val = qjs.JS_NewInt32(ctx, @intCast(buf[i]));
        _ = qjs.JS_SetPropertyUint32(ctx, arr, @intCast(i), byte_val);
    }

    return arr;
}

/// crypto.randomUUID() - Generate UUID v4
fn randomUUIDFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    var bytes: [16]u8 = undefined;
    std.crypto.random.bytes(&bytes);

    // Set version (4) and variant (RFC 4122)
    bytes[6] = (bytes[6] & 0x0f) | 0x40; // Version 4
    bytes[8] = (bytes[8] & 0x3f) | 0x80; // Variant 1

    // Format as UUID string
    var uuid_buf: [36]u8 = undefined;
    const hex = "0123456789abcdef";
    var pos: usize = 0;
    for (0..16) |i| {
        if (i == 4 or i == 6 or i == 8 or i == 10) {
            uuid_buf[pos] = '-';
            pos += 1;
        }
        uuid_buf[pos] = hex[bytes[i] >> 4];
        uuid_buf[pos + 1] = hex[bytes[i] & 0x0f];
        pos += 2;
    }

    return qjs.JS_NewStringLen(ctx, &uuid_buf, 36);
}

/// crypto.timingSafeEqual(a, b) - Constant-time comparison
fn timingSafeEqualFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "timingSafeEqual requires 2 arguments");

    // Get first buffer
    var size_a: usize = 0;
    const ptr_a = qjs.JS_GetArrayBuffer(ctx, &size_a, argv[0]);
    if (ptr_a == null) return qjs.JS_ThrowTypeError(ctx, "First argument must be ArrayBuffer or TypedArray");
    const bytes_a = @as([*]const u8, @ptrCast(ptr_a))[0..size_a];

    // Get second buffer
    var size_b: usize = 0;
    const ptr_b = qjs.JS_GetArrayBuffer(ctx, &size_b, argv[1]);
    if (ptr_b == null) return qjs.JS_ThrowTypeError(ctx, "Second argument must be ArrayBuffer or TypedArray");
    const bytes_b = @as([*]const u8, @ptrCast(ptr_b))[0..size_b];

    // Must be same length
    if (size_a != size_b) return qjs.JS_ThrowRangeError(ctx, "Buffers must have same length");

    // Constant-time comparison using XOR accumulator
    var acc: u8 = 0;
    for (bytes_a, bytes_b) |a, b| {
        acc |= a ^ b;
    }
    return if (acc == 0) quickjs.jsTrue() else quickjs.jsFalse();
}

/// crypto.randomInt([min], max) - Cryptographically secure random integer
fn randomIntFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "randomInt requires at least 1 argument");

    var min: i64 = 0;
    var max: i64 = 0;

    if (argc == 1) {
        // randomInt(max) - range is [0, max)
        if (qjs.JS_ToInt64(ctx, &max, argv[0]) < 0) return quickjs.jsException();
    } else {
        // randomInt(min, max) - range is [min, max)
        if (qjs.JS_ToInt64(ctx, &min, argv[0]) < 0) return quickjs.jsException();
        if (qjs.JS_ToInt64(ctx, &max, argv[1]) < 0) return quickjs.jsException();
    }

    if (min >= max) return qjs.JS_ThrowRangeError(ctx, "max must be greater than min");

    // Generate random value in range [min, max)
    const range: u64 = @intCast(max - min);
    const random_val = std.crypto.random.intRangeLessThan(u64, 0, range);
    const result: i64 = min + @as(i64, @intCast(random_val));

    return qjs.JS_NewInt64(ctx, result);
}

/// crypto.pbkdf2Sync(password, salt, iterations, keylen, digest) - Key derivation
fn pbkdf2SyncFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 5) return qjs.JS_ThrowTypeError(ctx, "pbkdf2Sync requires 5 arguments: password, salt, iterations, keylen, digest");

    // Get password
    const pwd_str = qjs.JS_ToCString(ctx, argv[0]);
    if (pwd_str == null) return qjs.JS_ThrowTypeError(ctx, "Invalid password");
    defer qjs.JS_FreeCString(ctx, pwd_str);
    const password = std.mem.span(pwd_str);

    // Get salt
    const salt_str = qjs.JS_ToCString(ctx, argv[1]);
    if (salt_str == null) return qjs.JS_ThrowTypeError(ctx, "Invalid salt");
    defer qjs.JS_FreeCString(ctx, salt_str);
    const salt = std.mem.span(salt_str);

    // Get iterations
    var iterations: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &iterations, argv[2]) < 0) return quickjs.jsException();
    if (iterations <= 0) return qjs.JS_ThrowRangeError(ctx, "iterations must be positive");

    // Get keylen
    var keylen: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &keylen, argv[3]) < 0) return quickjs.jsException();
    if (keylen <= 0 or keylen > 1024) return qjs.JS_ThrowRangeError(ctx, "keylen must be 1-1024");

    // Get digest algorithm
    const digest_str = qjs.JS_ToCString(ctx, argv[4]);
    if (digest_str == null) return qjs.JS_ThrowTypeError(ctx, "Invalid digest");
    defer qjs.JS_FreeCString(ctx, digest_str);
    const digest = std.mem.span(digest_str);

    // Allocate output buffer
    var key_buffer: [1024]u8 = undefined;
    const key_out = key_buffer[0..@intCast(keylen)];

    // Derive key using PBKDF2 (RFC 2898)
    if (std.mem.eql(u8, digest, "sha256")) {
        std.crypto.pwhash.pbkdf2(key_out, password, salt, @intCast(iterations), std.crypto.auth.hmac.sha2.HmacSha256) catch {
            return qjs.JS_ThrowTypeError(ctx, "PBKDF2 failed");
        };
    } else if (std.mem.eql(u8, digest, "sha512")) {
        std.crypto.pwhash.pbkdf2(key_out, password, salt, @intCast(iterations), std.crypto.auth.hmac.sha2.HmacSha512) catch {
            return qjs.JS_ThrowTypeError(ctx, "PBKDF2 failed");
        };
    } else if (std.mem.eql(u8, digest, "sha1")) {
        std.crypto.pwhash.pbkdf2(key_out, password, salt, @intCast(iterations), std.crypto.auth.hmac.HmacSha1) catch {
            return qjs.JS_ThrowTypeError(ctx, "PBKDF2 failed");
        };
    } else {
        return qjs.JS_ThrowTypeError(ctx, "Unsupported digest for PBKDF2 (use sha256, sha512, or sha1)");
    }

    // Create Uint8Array result
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);
    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    const len_val = qjs.JS_NewInt32(ctx, keylen);
    var ctor_args = [1]qjs.JSValue{len_val};
    const arr = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    qjs.JS_FreeValue(ctx, len_val);
    if (qjs.JS_IsException(arr)) return arr;

    for (key_out, 0..) |byte, i| {
        const byte_val = qjs.JS_NewInt32(ctx, @intCast(byte));
        _ = qjs.JS_SetPropertyUint32(ctx, arr, @intCast(i), byte_val);
    }

    return arr;
}

/// crypto.hkdfSync(digest, ikm, salt, info, keylen) - HKDF key derivation (RFC 5869)
fn hkdfSyncFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 5) return qjs.JS_ThrowTypeError(ctx, "hkdfSync requires 5 arguments: digest, ikm, salt, info, keylen");

    // Get digest algorithm
    const digest_str = qjs.JS_ToCString(ctx, argv[0]);
    if (digest_str == null) return qjs.JS_ThrowTypeError(ctx, "Invalid digest");
    defer qjs.JS_FreeCString(ctx, digest_str);
    const digest = std.mem.span(digest_str);

    // Get IKM (input key material) - can be string or buffer
    var ikm: []const u8 = &[_]u8{};
    var ikm_size: usize = 0;
    const ikm_ptr = qjs.JS_GetArrayBuffer(ctx, &ikm_size, argv[1]);
    if (ikm_ptr != null) {
        ikm = @as([*]const u8, @ptrCast(ikm_ptr))[0..ikm_size];
    } else {
        const ikm_str = qjs.JS_ToCString(ctx, argv[1]);
        if (ikm_str != null) {
            defer qjs.JS_FreeCString(ctx, ikm_str);
            ikm = std.mem.span(ikm_str);
        }
    }

    // Get salt - can be string or buffer
    var salt: []const u8 = &[_]u8{};
    var salt_size: usize = 0;
    const salt_ptr = qjs.JS_GetArrayBuffer(ctx, &salt_size, argv[2]);
    if (salt_ptr != null) {
        salt = @as([*]const u8, @ptrCast(salt_ptr))[0..salt_size];
    } else {
        const salt_str = qjs.JS_ToCString(ctx, argv[2]);
        if (salt_str != null) {
            defer qjs.JS_FreeCString(ctx, salt_str);
            salt = std.mem.span(salt_str);
        }
    }

    // Get info - can be string or buffer
    var info: []const u8 = &[_]u8{};
    var info_size: usize = 0;
    const info_ptr = qjs.JS_GetArrayBuffer(ctx, &info_size, argv[3]);
    if (info_ptr != null) {
        info = @as([*]const u8, @ptrCast(info_ptr))[0..info_size];
    } else {
        const info_str = qjs.JS_ToCString(ctx, argv[3]);
        if (info_str != null) {
            defer qjs.JS_FreeCString(ctx, info_str);
            info = std.mem.span(info_str);
        }
    }

    // Get keylen
    var keylen: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &keylen, argv[4]) < 0) return quickjs.jsException();
    if (keylen <= 0 or keylen > 1024) return qjs.JS_ThrowRangeError(ctx, "keylen must be 1-1024");

    // Output buffer
    var output_buffer: [1024]u8 = undefined;
    const output = output_buffer[0..@intCast(keylen)];

    // Perform HKDF based on digest algorithm
    if (std.mem.eql(u8, digest, "sha256")) {
        const Hkdf = std.crypto.kdf.hkdf.HkdfSha256;
        const prk = Hkdf.extract(salt, ikm);
        Hkdf.expand(output, info, prk);
    } else if (std.mem.eql(u8, digest, "sha384")) {
        const HmacSha384 = std.crypto.auth.hmac.sha2.HmacSha384;
        const Hkdf = std.crypto.kdf.hkdf.Hkdf(HmacSha384);
        const prk = Hkdf.extract(salt, ikm);
        Hkdf.expand(output, info, prk);
    } else if (std.mem.eql(u8, digest, "sha512")) {
        const HmacSha512 = std.crypto.auth.hmac.sha2.HmacSha512;
        const Hkdf = std.crypto.kdf.hkdf.Hkdf(HmacSha512);
        const prk = Hkdf.extract(salt, ikm);
        Hkdf.expand(output, info, prk);
    } else if (std.mem.eql(u8, digest, "sha1")) {
        const HmacSha1 = std.crypto.auth.hmac.Hmac(std.crypto.hash.Sha1);
        const Hkdf = std.crypto.kdf.hkdf.Hkdf(HmacSha1);
        const prk = Hkdf.extract(salt, ikm);
        Hkdf.expand(output, info, prk);
    } else {
        return qjs.JS_ThrowTypeError(ctx, "Unsupported digest for HKDF (use sha256, sha384, sha512, or sha1)");
    }

    // Create Uint8Array result
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);
    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    const len_val = qjs.JS_NewInt32(ctx, keylen);
    var ctor_args = [1]qjs.JSValue{len_val};
    const arr = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    qjs.JS_FreeValue(ctx, len_val);
    if (qjs.JS_IsException(arr)) return arr;

    for (output, 0..) |byte, i| {
        const byte_val = qjs.JS_NewInt32(ctx, @intCast(byte));
        _ = qjs.JS_SetPropertyUint32(ctx, arr, @intCast(i), byte_val);
    }

    return arr;
}

/// crypto.scryptSync(password, salt, keylen, options) - scrypt key derivation
fn scryptSyncFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) return qjs.JS_ThrowTypeError(ctx, "scryptSync requires at least 3 arguments: password, salt, keylen");

    // Get password - can be string or buffer
    var password: []const u8 = &[_]u8{};
    var pwd_size: usize = 0;
    const pwd_ptr = qjs.JS_GetArrayBuffer(ctx, &pwd_size, argv[0]);
    if (pwd_ptr != null) {
        password = @as([*]const u8, @ptrCast(pwd_ptr))[0..pwd_size];
    } else {
        const pwd_str = qjs.JS_ToCString(ctx, argv[0]);
        if (pwd_str != null) {
            defer qjs.JS_FreeCString(ctx, pwd_str);
            password = std.mem.span(pwd_str);
        }
    }

    // Get salt - can be string or buffer
    var salt: []const u8 = &[_]u8{};
    var salt_size: usize = 0;
    const salt_ptr = qjs.JS_GetArrayBuffer(ctx, &salt_size, argv[1]);
    if (salt_ptr != null) {
        salt = @as([*]const u8, @ptrCast(salt_ptr))[0..salt_size];
    } else {
        const salt_str = qjs.JS_ToCString(ctx, argv[1]);
        if (salt_str != null) {
            defer qjs.JS_FreeCString(ctx, salt_str);
            salt = std.mem.span(salt_str);
        }
    }

    // Get keylen
    var keylen: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &keylen, argv[2]) < 0) return quickjs.jsException();
    if (keylen <= 0 or keylen > 1024) return qjs.JS_ThrowRangeError(ctx, "keylen must be 1-1024");

    // Get options (N, r, p) - defaults match Node.js: N=16384 (2^14), r=8, p=1
    var ln: u6 = 14; // log2(16384) = 14
    var r: u30 = 8;
    var p: u30 = 1;

    if (argc >= 4 and !qjs.JS_IsUndefined(argv[3]) and !qjs.JS_IsNull(argv[3])) {
        // Parse options object
        const n_val = qjs.JS_GetPropertyStr(ctx, argv[3], "N");
        if (!qjs.JS_IsUndefined(n_val)) {
            var n: i64 = 0;
            _ = qjs.JS_ToInt64(ctx, &n, n_val);
            qjs.JS_FreeValue(ctx, n_val);
            // Convert N to ln (log2)
            if (n > 0) {
                ln = @intCast(std.math.log2(@as(u64, @intCast(n))));
            }
        } else {
            qjs.JS_FreeValue(ctx, n_val);
        }

        const r_val = qjs.JS_GetPropertyStr(ctx, argv[3], "r");
        if (!qjs.JS_IsUndefined(r_val)) {
            var r_int: i32 = 0;
            _ = qjs.JS_ToInt32(ctx, &r_int, r_val);
            qjs.JS_FreeValue(ctx, r_val);
            if (r_int > 0) r = @intCast(r_int);
        } else {
            qjs.JS_FreeValue(ctx, r_val);
        }

        const p_val = qjs.JS_GetPropertyStr(ctx, argv[3], "p");
        if (!qjs.JS_IsUndefined(p_val)) {
            var p_int: i32 = 0;
            _ = qjs.JS_ToInt32(ctx, &p_int, p_val);
            qjs.JS_FreeValue(ctx, p_val);
            if (p_int > 0) p = @intCast(p_int);
        } else {
            qjs.JS_FreeValue(ctx, p_val);
        }
    }

    // Output buffer
    var output_buffer: [1024]u8 = undefined;
    const output = output_buffer[0..@intCast(keylen)];

    // Perform scrypt using page allocator
    const params = std.crypto.pwhash.scrypt.Params{ .ln = ln, .r = r, .p = p };
    std.crypto.pwhash.scrypt.kdf(
        std.heap.page_allocator,
        output,
        password,
        salt,
        params,
    ) catch {
        return qjs.JS_ThrowTypeError(ctx, "scrypt key derivation failed");
    };

    // Create Uint8Array result
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);
    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    const len_val = qjs.JS_NewInt32(ctx, keylen);
    var ctor_args = [1]qjs.JSValue{len_val};
    const arr = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    qjs.JS_FreeValue(ctx, len_val);
    if (qjs.JS_IsException(arr)) return arr;

    for (output, 0..) |byte, i| {
        const byte_val = qjs.JS_NewInt32(ctx, @intCast(byte));
        _ = qjs.JS_SetPropertyUint32(ctx, arr, @intCast(i), byte_val);
    }

    return arr;
}

// ============ AES-CBC Encryption ============

/// AES-CBC encrypt buffer
var cbc_encrypt_buf: [65536 + 16]u8 = undefined;
var cbc_decrypt_buf: [65536]u8 = undefined;

/// aesCbcEncrypt(key, iv, plaintext) - AES-CBC encryption with PKCS7 padding
/// key: 16 bytes (AES-128) or 32 bytes (AES-256)
/// iv: 16 bytes
/// plaintext: data to encrypt
fn aesCbcEncryptFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) return qjs.JS_ThrowTypeError(ctx, "aesCbcEncrypt requires key, iv, plaintext");

    // Get key
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null or (key_size != 16 and key_size != 32)) {
        return qjs.JS_ThrowTypeError(ctx, "Key must be 16 or 32 bytes");
    }
    const key_data = @as([*]const u8, @ptrCast(key_ptr))[0..key_size];

    // Get IV
    var iv_size: usize = 0;
    const iv_ptr = qjs.JS_GetArrayBuffer(ctx, &iv_size, argv[1]);
    if (iv_ptr == null or iv_size != 16) {
        return qjs.JS_ThrowTypeError(ctx, "IV must be 16 bytes");
    }
    const iv_data = @as([*]const u8, @ptrCast(iv_ptr))[0..16];

    // Get plaintext
    var plain_size: usize = 0;
    const plain_ptr = qjs.JS_GetArrayBuffer(ctx, &plain_size, argv[2]);
    if (plain_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Plaintext must be ArrayBuffer");
    }
    const plaintext = @as([*]const u8, @ptrCast(plain_ptr))[0..plain_size];

    // Calculate output size with PKCS7 padding
    const padding_len = 16 - (plain_size % 16);
    const output_len = plain_size + padding_len;
    if (output_len > cbc_encrypt_buf.len) {
        return qjs.JS_ThrowRangeError(ctx, "Plaintext too large");
    }

    // Copy plaintext and add PKCS7 padding
    @memcpy(cbc_encrypt_buf[0..plain_size], plaintext);
    @memset(cbc_encrypt_buf[plain_size..output_len], @truncate(padding_len));

    // Encrypt in CBC mode
    var prev_block: [16]u8 = iv_data.*;
    var i: usize = 0;
    while (i < output_len) : (i += 16) {
        // XOR plaintext block with previous ciphertext (or IV)
        var block: [16]u8 = undefined;
        for (0..16) |j| {
            block[j] = cbc_encrypt_buf[i + j] ^ prev_block[j];
        }

        // Encrypt block
        if (key_size == 32) {
            const aes = std.crypto.core.aes.Aes256.initEnc(key_data[0..32].*);
            aes.encrypt(&block, &block);
        } else {
            const aes = std.crypto.core.aes.Aes128.initEnc(key_data[0..16].*);
            aes.encrypt(&block, &block);
        }

        // Store ciphertext
        @memcpy(cbc_encrypt_buf[i..][0..16], &block);
        prev_block = block;
    }

    // Return as Uint8Array
    return createUint8Array(ctx, cbc_encrypt_buf[0..output_len]);
}

/// aesCbcDecrypt(key, iv, ciphertext) - AES-CBC decryption with PKCS7 padding removal
fn aesCbcDecryptFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) return qjs.JS_ThrowTypeError(ctx, "aesCbcDecrypt requires key, iv, ciphertext");

    // Get key
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null or (key_size != 16 and key_size != 32)) {
        return qjs.JS_ThrowTypeError(ctx, "Key must be 16 or 32 bytes");
    }
    const key_data = @as([*]const u8, @ptrCast(key_ptr))[0..key_size];

    // Get IV
    var iv_size: usize = 0;
    const iv_ptr = qjs.JS_GetArrayBuffer(ctx, &iv_size, argv[1]);
    if (iv_ptr == null or iv_size != 16) {
        return qjs.JS_ThrowTypeError(ctx, "IV must be 16 bytes");
    }
    const iv_data = @as([*]const u8, @ptrCast(iv_ptr))[0..16];

    // Get ciphertext
    var cipher_size: usize = 0;
    const cipher_ptr = qjs.JS_GetArrayBuffer(ctx, &cipher_size, argv[2]);
    if (cipher_ptr == null or cipher_size == 0 or cipher_size % 16 != 0) {
        return qjs.JS_ThrowTypeError(ctx, "Ciphertext must be ArrayBuffer with length multiple of 16");
    }
    const ciphertext = @as([*]const u8, @ptrCast(cipher_ptr))[0..cipher_size];

    if (cipher_size > cbc_decrypt_buf.len) {
        return qjs.JS_ThrowRangeError(ctx, "Ciphertext too large");
    }

    // Decrypt in CBC mode
    var prev_block: [16]u8 = iv_data.*;
    var i: usize = 0;
    while (i < cipher_size) : (i += 16) {
        var block: [16]u8 = undefined;
        @memcpy(&block, ciphertext[i..][0..16]);
        const saved_cipher = block;

        // Decrypt block
        if (key_size == 32) {
            const aes = std.crypto.core.aes.Aes256.initDec(key_data[0..32].*);
            aes.decrypt(&block, &block);
        } else {
            const aes = std.crypto.core.aes.Aes128.initDec(key_data[0..16].*);
            aes.decrypt(&block, &block);
        }

        // XOR with previous ciphertext (or IV)
        for (0..16) |j| {
            cbc_decrypt_buf[i + j] = block[j] ^ prev_block[j];
        }
        prev_block = saved_cipher;
    }

    // Remove PKCS7 padding
    const padding_len = cbc_decrypt_buf[cipher_size - 1];
    if (padding_len == 0 or padding_len > 16) {
        return qjs.JS_ThrowTypeError(ctx, "Invalid PKCS7 padding");
    }

    // Verify padding
    for (0..padding_len) |j| {
        if (cbc_decrypt_buf[cipher_size - 1 - j] != padding_len) {
            return qjs.JS_ThrowTypeError(ctx, "Invalid PKCS7 padding");
        }
    }

    const plaintext_len = cipher_size - padding_len;
    return createUint8Array(ctx, cbc_decrypt_buf[0..plaintext_len]);
}

// ============ AES-CTR Encryption ============

/// AES-CTR encrypt/decrypt buffer (same operation for both)
/// CTR mode is a stream cipher - no padding needed
var ctr_buffer: [65536]u8 = undefined;

/// aesCtrEncrypt(key, iv, data) - AES-CTR encryption/decryption
/// key: 16 bytes (AES-128) or 32 bytes (AES-256)
/// iv: 16 bytes (used as initial counter)
/// data: plaintext or ciphertext (same operation for both)
fn aesCtrEncryptFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) return qjs.JS_ThrowTypeError(ctx, "aesCtrEncrypt requires key, iv, data");

    // Get key
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null or (key_size != 16 and key_size != 32)) {
        return qjs.JS_ThrowTypeError(ctx, "Key must be 16 or 32 bytes");
    }
    const key_data = @as([*]const u8, @ptrCast(key_ptr))[0..key_size];

    // Get IV (initial counter)
    var iv_size: usize = 0;
    const iv_ptr = qjs.JS_GetArrayBuffer(ctx, &iv_size, argv[1]);
    if (iv_ptr == null or iv_size != 16) {
        return qjs.JS_ThrowTypeError(ctx, "IV must be 16 bytes");
    }
    var counter: [16]u8 = @as([*]const u8, @ptrCast(iv_ptr))[0..16].*;

    // Get data
    var data_size: usize = 0;
    const data_ptr = qjs.JS_GetArrayBuffer(ctx, &data_size, argv[2]);
    if (data_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Data must be ArrayBuffer");
    }
    const data = @as([*]const u8, @ptrCast(data_ptr))[0..data_size];

    if (data_size > ctr_buffer.len) {
        return qjs.JS_ThrowRangeError(ctx, "Data too large");
    }

    // Process in 16-byte blocks (CTR mode)
    var i: usize = 0;
    while (i < data_size) {
        // Encrypt counter to get keystream block
        var keystream: [16]u8 = undefined;
        if (key_size == 32) {
            const aes = std.crypto.core.aes.Aes256.initEnc(key_data[0..32].*);
            aes.encrypt(&keystream, &counter);
        } else {
            const aes = std.crypto.core.aes.Aes128.initEnc(key_data[0..16].*);
            aes.encrypt(&keystream, &counter);
        }

        // XOR data with keystream
        const block_size = @min(16, data_size - i);
        for (0..block_size) |j| {
            ctr_buffer[i + j] = data[i + j] ^ keystream[j];
        }

        // Increment counter (big-endian)
        var carry: u16 = 1;
        var k: usize = 15;
        while (carry > 0 and k < 16) : (k -%= 1) {
            const sum = @as(u16, counter[k]) + carry;
            counter[k] = @truncate(sum);
            carry = sum >> 8;
            if (k == 0) break;
        }

        i += 16;
    }

    return createUint8Array(ctx, ctr_buffer[0..data_size]);
}

// ============ Ed25519 Digital Signatures ============

/// ed25519Sign(privateKey, message) - Sign message with Ed25519
/// privateKey: 32 bytes (seed) or 64 bytes (keypair)
/// message: data to sign
/// Returns: 64-byte signature
fn ed25519SignFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "ed25519Sign requires privateKey, message");

    // Get private key
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null or (key_size != 32 and key_size != 64)) {
        return qjs.JS_ThrowTypeError(ctx, "Private key must be 32 bytes (seed) or 64 bytes (keypair)");
    }
    const key_data = @as([*]const u8, @ptrCast(key_ptr))[0..key_size];

    // Get message
    var msg_size: usize = 0;
    const msg_ptr = qjs.JS_GetArrayBuffer(ctx, &msg_size, argv[1]);
    if (msg_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Message must be ArrayBuffer");
    }
    const message = @as([*]const u8, @ptrCast(msg_ptr))[0..msg_size];

    // Create keypair from seed (32 bytes)
    const Ed25519 = std.crypto.sign.Ed25519;

    // Use generateDeterministic with the 32-byte seed
    const keypair = Ed25519.KeyPair.generateDeterministic(key_data[0..32].*) catch {
        return qjs.JS_ThrowTypeError(ctx, "Failed to create Ed25519 keypair from seed");
    };

    // Sign message
    const signature = keypair.sign(message, null) catch {
        return qjs.JS_ThrowTypeError(ctx, "Ed25519 signing failed");
    };

    var sig_bytes: [64]u8 = signature.toBytes();
    return createUint8Array(ctx, &sig_bytes);
}

/// ed25519Verify(publicKey, message, signature) - Verify Ed25519 signature
/// publicKey: 32 bytes
/// message: original message
/// signature: 64 bytes
/// Returns: boolean
fn ed25519VerifyFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) return qjs.JS_ThrowTypeError(ctx, "ed25519Verify requires publicKey, message, signature");

    // Get public key
    var pub_size: usize = 0;
    const pub_ptr = qjs.JS_GetArrayBuffer(ctx, &pub_size, argv[0]);
    if (pub_ptr == null or pub_size != 32) {
        return qjs.JS_ThrowTypeError(ctx, "Public key must be 32 bytes");
    }
    const pub_data = @as([*]const u8, @ptrCast(pub_ptr))[0..32];

    // Get message
    var msg_size: usize = 0;
    const msg_ptr = qjs.JS_GetArrayBuffer(ctx, &msg_size, argv[1]);
    if (msg_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Message must be ArrayBuffer");
    }
    const message = @as([*]const u8, @ptrCast(msg_ptr))[0..msg_size];

    // Get signature
    var sig_size: usize = 0;
    const sig_ptr = qjs.JS_GetArrayBuffer(ctx, &sig_size, argv[2]);
    if (sig_ptr == null or sig_size != 64) {
        return qjs.JS_ThrowTypeError(ctx, "Signature must be 64 bytes");
    }
    const sig_data = @as([*]const u8, @ptrCast(sig_ptr))[0..64];

    // Verify signature
    const Ed25519 = std.crypto.sign.Ed25519;
    const public_key = Ed25519.PublicKey.fromBytes(pub_data.*) catch {
        return quickjs.jsFalse();
    };
    const signature = Ed25519.Signature.fromBytes(sig_data.*);

    signature.verify(message, public_key) catch {
        return quickjs.jsFalse();
    };

    return quickjs.jsTrue();
}

/// ed25519GenerateKeyPair() - Generate Ed25519 key pair
/// Returns: { publicKey: 32 bytes, privateKey: 32 bytes }
fn ed25519GenerateKeyPairFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const Ed25519 = std.crypto.sign.Ed25519;

    // Generate random keypair
    const keypair = Ed25519.KeyPair.generate();

    // Extract the 32-byte seed from the secret key
    const seed = keypair.secret_key.seed();

    // Create result object
    const result = qjs.JS_NewObject(ctx);
    if (qjs.JS_IsException(result)) return result;

    // Add publicKey (32 bytes)
    const pub_arr = createUint8Array(ctx, &keypair.public_key.toBytes());
    if (qjs.JS_IsException(pub_arr)) {
        qjs.JS_FreeValue(ctx, result);
        return pub_arr;
    }
    _ = qjs.JS_SetPropertyStr(ctx, result, "publicKey", pub_arr);

    // Add privateKey (32-byte seed)
    var seed_bytes: [32]u8 = seed;
    const priv_arr = createUint8Array(ctx, &seed_bytes);
    if (qjs.JS_IsException(priv_arr)) {
        qjs.JS_FreeValue(ctx, result);
        return priv_arr;
    }
    _ = qjs.JS_SetPropertyStr(ctx, result, "privateKey", priv_arr);

    return result;
}

// ============ X25519 ECDH Key Exchange ============

/// x25519GenerateKeyPair() - Generate X25519 key pair
/// Returns: { publicKey: 32 bytes, privateKey: 32 bytes }
fn x25519GenerateKeyPairFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const X25519 = std.crypto.dh.X25519;

    // Generate random keypair
    const keypair = X25519.KeyPair.generate();

    // Create result object
    const result = qjs.JS_NewObject(ctx);
    if (qjs.JS_IsException(result)) return result;

    // Add publicKey (32 bytes)
    var pub_bytes: [32]u8 = keypair.public_key;
    const pub_arr = createUint8Array(ctx, &pub_bytes);
    if (qjs.JS_IsException(pub_arr)) {
        qjs.JS_FreeValue(ctx, result);
        return pub_arr;
    }
    _ = qjs.JS_SetPropertyStr(ctx, result, "publicKey", pub_arr);

    // Add privateKey (32 bytes) - the secret_key bytes
    var priv_bytes: [32]u8 = keypair.secret_key;
    const priv_arr = createUint8Array(ctx, &priv_bytes);
    if (qjs.JS_IsException(priv_arr)) {
        qjs.JS_FreeValue(ctx, result);
        return priv_arr;
    }
    _ = qjs.JS_SetPropertyStr(ctx, result, "privateKey", priv_arr);

    return result;
}

/// x25519ComputeSecret(privateKey, publicKey) - Compute shared secret
/// privateKey: 32 bytes (own private key)
/// publicKey: 32 bytes (other party's public key)
/// Returns: 32-byte shared secret
fn x25519ComputeSecretFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "x25519ComputeSecret requires privateKey, publicKey");

    // Get private key
    var priv_size: usize = 0;
    const priv_ptr = qjs.JS_GetArrayBuffer(ctx, &priv_size, argv[0]);
    if (priv_ptr == null or priv_size != 32) {
        return qjs.JS_ThrowTypeError(ctx, "Private key must be 32 bytes");
    }
    const priv_data = @as([*]const u8, @ptrCast(priv_ptr))[0..32];

    // Get public key
    var pub_size: usize = 0;
    const pub_ptr = qjs.JS_GetArrayBuffer(ctx, &pub_size, argv[1]);
    if (pub_ptr == null or pub_size != 32) {
        return qjs.JS_ThrowTypeError(ctx, "Public key must be 32 bytes");
    }
    const pub_data = @as([*]const u8, @ptrCast(pub_ptr))[0..32];

    // Compute shared secret using scalarmult
    const X25519 = std.crypto.dh.X25519;
    const shared_secret = X25519.scalarmult(priv_data.*, pub_data.*) catch {
        return qjs.JS_ThrowTypeError(ctx, "X25519 key exchange failed (weak public key)");
    };

    var shared_bytes: [32]u8 = shared_secret;
    return createUint8Array(ctx, &shared_bytes);
}

/// generateKeyPairSync(algorithm) - Generate key pair for algorithm
/// algorithm: "ed25519" | "x25519"
fn generateKeyPairSyncFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "generateKeyPairSync requires algorithm");

    // Get algorithm
    const algo_str = qjs.JS_ToCString(ctx, argv[0]);
    if (algo_str == null) return qjs.JS_ThrowTypeError(ctx, "Invalid algorithm");
    defer qjs.JS_FreeCString(ctx, algo_str);
    const algorithm = std.mem.span(algo_str);

    if (std.mem.eql(u8, algorithm, "ed25519")) {
        return ed25519GenerateKeyPairFunc(ctx, quickjs.jsUndefined(), 0, argv);
    } else if (std.mem.eql(u8, algorithm, "x25519")) {
        return x25519GenerateKeyPairFunc(ctx, quickjs.jsUndefined(), 0, argv);
    } else {
        return qjs.JS_ThrowTypeError(ctx, "Unsupported algorithm (use ed25519 or x25519)");
    }
}

/// Helper to create Uint8Array from bytes
fn createUint8Array(ctx: ?*qjs.JSContext, data: []const u8) qjs.JSValue {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);
    const uint8array_ctor = qjs.JS_GetPropertyStr(ctx, global, "Uint8Array");
    defer qjs.JS_FreeValue(ctx, uint8array_ctor);

    const len_val = qjs.JS_NewInt32(ctx, @intCast(data.len));
    var ctor_args = [1]qjs.JSValue{len_val};
    const arr = qjs.JS_CallConstructor(ctx, uint8array_ctor, 1, &ctor_args);
    qjs.JS_FreeValue(ctx, len_val);

    if (qjs.JS_IsException(arr)) return arr;

    for (data, 0..) |byte, i| {
        const byte_val = qjs.JS_NewInt32(ctx, @intCast(byte));
        _ = qjs.JS_SetPropertyUint32(ctx, arr, @intCast(i), byte_val);
    }

    return arr;
}

/// Register crypto module
pub fn register(ctx: *qjs.JSContext) void {
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);

    // Create crypto module
    const crypto_obj = qjs.JS_NewObject(ctx);

    // Register functions
    inline for (.{
        .{ "hash", hashFunc, 2 },
        .{ "hmac", hmacFunc, 3 },
        .{ "aesGcmEncrypt", aesGcmEncrypt, 4 },
        .{ "aesGcmDecrypt", aesGcmDecrypt, 4 },
        .{ "aesCbcEncrypt", aesCbcEncryptFunc, 3 },
        .{ "aesCbcDecrypt", aesCbcDecryptFunc, 3 },
        .{ "aesCtrEncrypt", aesCtrEncryptFunc, 3 },
        .{ "randomBytes", randomBytesFunc, 1 },
        .{ "randomUUID", randomUUIDFunc, 0 },
        .{ "timingSafeEqual", timingSafeEqualFunc, 2 },
        .{ "randomInt", randomIntFunc, 2 },
        .{ "pbkdf2Sync", pbkdf2SyncFunc, 5 },
        .{ "hkdfSync", hkdfSyncFunc, 5 },
        .{ "scryptSync", scryptSyncFunc, 4 },
        .{ "ed25519Sign", ed25519SignFunc, 2 },
        .{ "ed25519Verify", ed25519VerifyFunc, 3 },
        .{ "ed25519GenerateKeyPair", ed25519GenerateKeyPairFunc, 0 },
        .{ "x25519GenerateKeyPair", x25519GenerateKeyPairFunc, 0 },
        .{ "x25519ComputeSecret", x25519ComputeSecretFunc, 2 },
        .{ "generateKeyPairSync", generateKeyPairSyncFunc, 1 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, crypto_obj, binding[0], func);
    }

    // Set global crypto object
    _ = qjs.JS_SetPropertyStr(ctx, global, "crypto", qjs.JS_DupValue(ctx, crypto_obj));

    // Set in _modules for require('crypto')
    const modules_val = qjs.JS_GetPropertyStr(ctx, global, "_modules");
    if (!qjs.JS_IsUndefined(modules_val)) {
        _ = qjs.JS_SetPropertyStr(ctx, modules_val, "crypto", crypto_obj);
        qjs.JS_FreeValue(ctx, modules_val);
    } else {
        qjs.JS_FreeValue(ctx, crypto_obj);
    }
}
