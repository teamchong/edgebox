/// Native crypto module - QuickJS C functions
/// Hash, HMAC, AES-GCM encryption using Zig's std.crypto
/// Only random_bytes needs to remain as host function for CSPRNG
const std = @import("std");
const quickjs = @import("../quickjs_core.zig");
const qjs = quickjs.c;

// Stack buffers for crypto operations
var hash_buffer: [128]u8 = undefined; // SHA-512 hex output is 128 chars (64 bytes * 2)
var encrypt_buffer: [65536]u8 = undefined; // 64KB for encryption output
var decrypt_buffer: [65536]u8 = undefined; // 64KB for decryption output

/// hash(algorithm, data) - Compute cryptographic hash
/// Supported: sha256, sha384, sha512, sha1, md5, sha3-256, sha3-384, sha3-512, blake2b256, blake2b512, blake2s256
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
    } else if (std.mem.eql(u8, algorithm, "sha3-256") or std.mem.eql(u8, algorithm, "sha3_256")) {
        var hash: [32]u8 = undefined;
        std.crypto.hash.sha3.Sha3_256.hash(data_bytes, &hash, .{});
        return hashToHex(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "sha3-384") or std.mem.eql(u8, algorithm, "sha3_384")) {
        var hash: [48]u8 = undefined;
        std.crypto.hash.sha3.Sha3_384.hash(data_bytes, &hash, .{});
        return hashToHex(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "sha3-512") or std.mem.eql(u8, algorithm, "sha3_512")) {
        var hash: [64]u8 = undefined;
        std.crypto.hash.sha3.Sha3_512.hash(data_bytes, &hash, .{});
        return hashToHex(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "blake2b256") or std.mem.eql(u8, algorithm, "blake2b-256")) {
        var hash: [32]u8 = undefined;
        std.crypto.hash.blake2.Blake2b256.hash(data_bytes, &hash, .{});
        return hashToHex(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "blake2b512") or std.mem.eql(u8, algorithm, "blake2b-512")) {
        var hash: [64]u8 = undefined;
        std.crypto.hash.blake2.Blake2b512.hash(data_bytes, &hash, .{});
        return hashToHex(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "blake2s256") or std.mem.eql(u8, algorithm, "blake2s-256")) {
        var hash: [32]u8 = undefined;
        std.crypto.hash.blake2.Blake2s256.hash(data_bytes, &hash, .{});
        return hashToHex(ctx, &hash);
    } else {
        return qjs.JS_ThrowTypeError(ctx, "Unsupported hash algorithm (use sha256, sha384, sha512, sha1, md5, sha3-256, sha3-384, sha3-512, blake2b256, blake2b512, blake2s256)");
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

// ============ ECDSA P-256 Digital Signatures (for JWT ES256) ============

/// p256GenerateKeyPair() - Generate ECDSA P-256 key pair
/// Returns: { publicKey: 65 bytes (uncompressed), privateKey: 32 bytes }
fn p256GenerateKeyPairFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const Ecdsa = std.crypto.sign.ecdsa.EcdsaP256Sha256;

    // Generate random keypair
    const keypair = Ecdsa.KeyPair.generate();

    // Create result object
    const result = qjs.JS_NewObject(ctx);
    if (qjs.JS_IsException(result)) return result;

    // Add publicKey (65 bytes uncompressed: 0x04 + 32 bytes x + 32 bytes y)
    var pub_bytes: [65]u8 = undefined;
    pub_bytes[0] = 0x04; // Uncompressed point indicator
    const pub_point = keypair.public_key.toUncompressedSec1();
    @memcpy(pub_bytes[1..], pub_point[1..]);
    const pub_arr = createUint8Array(ctx, &pub_bytes);
    if (qjs.JS_IsException(pub_arr)) {
        qjs.JS_FreeValue(ctx, result);
        return pub_arr;
    }
    _ = qjs.JS_SetPropertyStr(ctx, result, "publicKey", pub_arr);

    // Add privateKey (32 bytes scalar)
    var priv_bytes: [32]u8 = keypair.secret_key.toBytes();
    const priv_arr = createUint8Array(ctx, &priv_bytes);
    if (qjs.JS_IsException(priv_arr)) {
        qjs.JS_FreeValue(ctx, result);
        return priv_arr;
    }
    _ = qjs.JS_SetPropertyStr(ctx, result, "privateKey", priv_arr);

    return result;
}

/// p256Sign(privateKey, message) - Sign message with ECDSA P-256
/// privateKey: 32 bytes
/// message: data to sign (should be 32-byte hash for proper usage)
/// Returns: 64-byte signature (r + s, each 32 bytes)
fn p256SignFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "p256Sign requires privateKey, message");

    const Ecdsa = std.crypto.sign.ecdsa.EcdsaP256Sha256;

    // Get private key
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null or key_size != 32) {
        return qjs.JS_ThrowTypeError(ctx, "Private key must be 32 bytes");
    }
    const key_data: *const [32]u8 = @ptrCast(@alignCast(key_ptr));

    // Get message
    var msg_size: usize = 0;
    const msg_ptr = qjs.JS_GetArrayBuffer(ctx, &msg_size, argv[1]);
    if (msg_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Message must be ArrayBuffer");
    }
    const message = @as([*]const u8, @ptrCast(msg_ptr))[0..msg_size];

    // Create key pair from secret key
    const secret_key = Ecdsa.SecretKey.fromBytes(key_data.*) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid P-256 private key");
    };
    const key_pair = Ecdsa.KeyPair.fromSecretKey(secret_key) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid P-256 private key");
    };

    // Sign the message (EcdsaP256Sha256 handles hashing internally)
    const signature = key_pair.sign(message, null) catch {
        return qjs.JS_ThrowTypeError(ctx, "P-256 signing failed");
    };

    // Return signature as 64-byte array
    var sig_bytes: [64]u8 = signature.toBytes();
    return createUint8Array(ctx, &sig_bytes);
}

/// p256Verify(publicKey, message, signature) - Verify ECDSA P-256 signature
/// publicKey: 65 bytes (uncompressed) or 33 bytes (compressed)
/// message: original message
/// signature: 64 bytes (r + s)
/// Returns: boolean
fn p256VerifyFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) return qjs.JS_ThrowTypeError(ctx, "p256Verify requires publicKey, message, signature");

    const Ecdsa = std.crypto.sign.ecdsa.EcdsaP256Sha256;

    // Get public key
    var pub_size: usize = 0;
    const pub_ptr = qjs.JS_GetArrayBuffer(ctx, &pub_size, argv[0]);
    if (pub_ptr == null or (pub_size != 65 and pub_size != 33)) {
        return qjs.JS_ThrowTypeError(ctx, "Public key must be 65 bytes (uncompressed) or 33 bytes (compressed)");
    }
    const pub_data = @as([*]const u8, @ptrCast(pub_ptr))[0..pub_size];

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
    const sig_data: *const [64]u8 = @ptrCast(@alignCast(sig_ptr));

    // Parse public key (SEC1 format)
    var pub_array: [65]u8 = undefined;
    @memcpy(pub_array[0..pub_size], pub_data[0..pub_size]);
    const public_key = Ecdsa.PublicKey.fromSec1(pub_array[0..pub_size]) catch {
        return quickjs.jsFalse();
    };

    // Parse signature
    const signature = Ecdsa.Signature.fromBytes(sig_data.*);

    // Verify (EcdsaP256Sha256 handles hashing internally)
    signature.verify(message, public_key) catch {
        return quickjs.jsFalse();
    };

    return quickjs.jsTrue();
}

/// p256ComputeSecret(privateKey, otherPublicKey) - ECDH P-256 shared secret
/// privateKey: 32 bytes (scalar)
/// otherPublicKey: 33 or 65 bytes (SEC1 format)
/// Returns: 32 bytes (x-coordinate of shared point)
fn p256ComputeSecretFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "p256ComputeSecret requires privateKey, otherPublicKey");

    const P256 = std.crypto.ecc.P256;

    // Get private key
    var priv_size: usize = 0;
    const priv_ptr = qjs.JS_GetArrayBuffer(ctx, &priv_size, argv[0]);
    if (priv_ptr == null or priv_size != 32) {
        return qjs.JS_ThrowTypeError(ctx, "Private key must be 32 bytes");
    }
    const priv_data: *const [32]u8 = @ptrCast(@alignCast(priv_ptr));

    // Get other public key
    var pub_size: usize = 0;
    const pub_ptr = qjs.JS_GetArrayBuffer(ctx, &pub_size, argv[1]);
    if (pub_ptr == null or (pub_size != 65 and pub_size != 33)) {
        return qjs.JS_ThrowTypeError(ctx, "Public key must be 33 (compressed) or 65 (uncompressed) bytes");
    }
    const pub_data = @as([*]const u8, @ptrCast(pub_ptr))[0..pub_size];

    // Parse other public key as P256 point
    const other_point = P256.fromSec1(pub_data) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid P-256 public key");
    };

    // Compute shared secret: private_key * other_public_key
    // The result is the x-coordinate of the resulting point
    const shared_point = other_point.mulPublic(priv_data.*, .big) catch {
        return qjs.JS_ThrowInternalError(ctx, "ECDH computation failed");
    };

    // Get the x-coordinate as the shared secret (standard ECDH)
    const affine = shared_point.affineCoordinates();
    var shared_secret: [32]u8 = affine.x.toBytes(.big);

    return createUint8Array(ctx, &shared_secret);
}

// ============ ECDSA P-384 Digital Signatures (for JWT ES384) ============

/// p384GenerateKeyPair() - Generate ECDSA P-384 key pair
/// Returns: { publicKey: 97 bytes (uncompressed), privateKey: 48 bytes }
fn p384GenerateKeyPairFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const Ecdsa = std.crypto.sign.ecdsa.EcdsaP384Sha384;

    // Generate random keypair
    const keypair = Ecdsa.KeyPair.generate();

    // Create result object
    const result = qjs.JS_NewObject(ctx);
    if (qjs.JS_IsException(result)) return result;

    // Add publicKey (97 bytes uncompressed: 0x04 + 48 bytes x + 48 bytes y)
    var pub_bytes: [97]u8 = undefined;
    pub_bytes[0] = 0x04; // Uncompressed point indicator
    const pub_point = keypair.public_key.toUncompressedSec1();
    @memcpy(pub_bytes[1..], pub_point[1..]);
    const pub_arr = createUint8Array(ctx, &pub_bytes);
    if (qjs.JS_IsException(pub_arr)) {
        qjs.JS_FreeValue(ctx, result);
        return pub_arr;
    }
    _ = qjs.JS_SetPropertyStr(ctx, result, "publicKey", pub_arr);

    // Add privateKey (48 bytes scalar)
    var priv_bytes: [48]u8 = keypair.secret_key.toBytes();
    const priv_arr = createUint8Array(ctx, &priv_bytes);
    if (qjs.JS_IsException(priv_arr)) {
        qjs.JS_FreeValue(ctx, result);
        return priv_arr;
    }
    _ = qjs.JS_SetPropertyStr(ctx, result, "privateKey", priv_arr);

    return result;
}

/// p384Sign(privateKey, message) - Sign message with ECDSA P-384
/// privateKey: 48 bytes
/// message: data to sign
/// Returns: 96-byte signature (r + s, each 48 bytes)
fn p384SignFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "p384Sign requires privateKey, message");

    const Ecdsa = std.crypto.sign.ecdsa.EcdsaP384Sha384;

    // Get private key
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null or key_size != 48) {
        return qjs.JS_ThrowTypeError(ctx, "Private key must be 48 bytes");
    }
    const key_data: *const [48]u8 = @ptrCast(@alignCast(key_ptr));

    // Get message
    var msg_size: usize = 0;
    const msg_ptr = qjs.JS_GetArrayBuffer(ctx, &msg_size, argv[1]);
    if (msg_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Message must be ArrayBuffer");
    }
    const message = @as([*]const u8, @ptrCast(msg_ptr))[0..msg_size];

    // Create key pair from secret key
    const secret_key = Ecdsa.SecretKey.fromBytes(key_data.*) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid P-384 private key");
    };
    const key_pair = Ecdsa.KeyPair.fromSecretKey(secret_key) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid P-384 private key");
    };

    // Sign the message
    const signature = key_pair.sign(message, null) catch {
        return qjs.JS_ThrowTypeError(ctx, "P-384 signing failed");
    };

    // Return signature as 96-byte array
    var sig_bytes: [96]u8 = signature.toBytes();
    return createUint8Array(ctx, &sig_bytes);
}

/// p384Verify(publicKey, message, signature) - Verify ECDSA P-384 signature
/// publicKey: 97 bytes (uncompressed) or 49 bytes (compressed)
/// message: original message
/// signature: 96 bytes (r + s)
/// Returns: boolean
fn p384VerifyFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) return qjs.JS_ThrowTypeError(ctx, "p384Verify requires publicKey, message, signature");

    const Ecdsa = std.crypto.sign.ecdsa.EcdsaP384Sha384;

    // Get public key
    var pub_size: usize = 0;
    const pub_ptr = qjs.JS_GetArrayBuffer(ctx, &pub_size, argv[0]);
    if (pub_ptr == null or (pub_size != 97 and pub_size != 49)) {
        return qjs.JS_ThrowTypeError(ctx, "Public key must be 97 bytes (uncompressed) or 49 bytes (compressed)");
    }
    const pub_data = @as([*]const u8, @ptrCast(pub_ptr))[0..pub_size];

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
    if (sig_ptr == null or sig_size != 96) {
        return qjs.JS_ThrowTypeError(ctx, "Signature must be 96 bytes");
    }
    const sig_data: *const [96]u8 = @ptrCast(@alignCast(sig_ptr));

    // Parse public key (SEC1 format)
    var pub_array: [97]u8 = undefined;
    @memcpy(pub_array[0..pub_size], pub_data[0..pub_size]);
    const public_key = Ecdsa.PublicKey.fromSec1(pub_array[0..pub_size]) catch {
        return quickjs.jsFalse();
    };

    // Parse signature
    const signature = Ecdsa.Signature.fromBytes(sig_data.*);

    // Verify
    signature.verify(message, public_key) catch {
        return quickjs.jsFalse();
    };

    return quickjs.jsTrue();
}

/// p384ComputeSecret(privateKey, otherPublicKey) - ECDH P-384 shared secret
/// privateKey: 48 bytes (scalar)
/// otherPublicKey: 49 or 97 bytes (SEC1 format)
/// Returns: 48 bytes (x-coordinate of shared point)
fn p384ComputeSecretFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "p384ComputeSecret requires privateKey, otherPublicKey");

    const P384 = std.crypto.ecc.P384;

    // Get private key
    var priv_size: usize = 0;
    const priv_ptr = qjs.JS_GetArrayBuffer(ctx, &priv_size, argv[0]);
    if (priv_ptr == null or priv_size != 48) {
        return qjs.JS_ThrowTypeError(ctx, "Private key must be 48 bytes");
    }
    const priv_data: *const [48]u8 = @ptrCast(@alignCast(priv_ptr));

    // Get other public key
    var pub_size: usize = 0;
    const pub_ptr = qjs.JS_GetArrayBuffer(ctx, &pub_size, argv[1]);
    if (pub_ptr == null or (pub_size != 97 and pub_size != 49)) {
        return qjs.JS_ThrowTypeError(ctx, "Public key must be 97 or 49 bytes");
    }
    const pub_data = @as([*]const u8, @ptrCast(pub_ptr))[0..pub_size];

    // Parse public key
    var pub_array: [97]u8 = undefined;
    @memcpy(pub_array[0..pub_size], pub_data[0..pub_size]);
    const other_public = P384.fromSec1(pub_array[0..pub_size]) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid P-384 public key");
    };

    // Compute shared secret: scalar * point (Zig 0.15 API takes raw bytes)
    const shared_point = other_public.mul(priv_data.*, .big) catch {
        return qjs.JS_ThrowInternalError(ctx, "ECDH computation failed");
    };

    // Get the x-coordinate as the shared secret
    const affine = shared_point.affineCoordinates();
    var shared_secret: [48]u8 = affine.x.toBytes(.big);

    return createUint8Array(ctx, &shared_secret);
}

/// ed25519DerivePublicKey(seed) - Derive public key from Ed25519 seed
/// seed: 32-byte private key seed
/// Returns: 32-byte public key
fn ed25519DerivePublicKeyFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "ed25519DerivePublicKey requires seed");

    // Get seed (32 bytes)
    var seed_size: usize = 0;
    const seed_ptr = qjs.JS_GetArrayBuffer(ctx, &seed_size, argv[0]);
    if (seed_ptr == null or seed_size != 32) {
        return qjs.JS_ThrowTypeError(ctx, "Seed must be 32 bytes");
    }
    const seed_data: *const [32]u8 = @ptrCast(@alignCast(seed_ptr));

    // Derive public key from seed
    const Ed25519 = std.crypto.sign.Ed25519;
    const keypair = Ed25519.KeyPair.generateDeterministic(seed_data.*) catch {
        return qjs.JS_ThrowTypeError(ctx, "Failed to derive Ed25519 public key");
    };

    var pub_bytes: [32]u8 = keypair.public_key.toBytes();
    return createUint8Array(ctx, &pub_bytes);
}

/// Miller-Rabin primality test helper
fn millerRabinTest(allocator: std.mem.Allocator, n: *BigInt, rounds: usize) !bool {
    // Handle small cases
    const n_const = n.toConst();
    // Compare n to 1 using orderAgainstScalar
    const cmp_to_one = n_const.orderAgainstScalar(1);
    if (cmp_to_one == .lt or cmp_to_one == .eq) {
        // n <= 1
        return false;
    }
    // Check if n == 2
    const cmp_to_two = n_const.orderAgainstScalar(2);
    if (cmp_to_two == .eq) return true; // 2 is prime

    // Check if even
    if (n_const.limbs.len > 0 and (n_const.limbs[0] & 1) == 0) {
        return false;
    }

    // Write n-1 as 2^r * d
    var n_minus_1 = try BigInt.init(allocator);
    defer n_minus_1.deinit();
    try n_minus_1.copy(n.toConst());
    try n_minus_1.addScalar(&n_minus_1, -1);

    var d = try BigInt.init(allocator);
    defer d.deinit();
    try d.copy(n_minus_1.toConst());

    var r: usize = 0;
    while (d.toConst().limbs.len > 0 and (d.toConst().limbs[0] & 1) == 0) {
        try d.shiftRight(&d, 1);
        r += 1;
    }

    // Witness loop
    var i: usize = 0;
    while (i < rounds) : (i += 1) {
        // Generate random a in [2, n-2]
        var a = try BigInt.init(allocator);
        defer a.deinit();

        // Use random bytes to generate a
        var random_bytes: [32]u8 = undefined;
        std.crypto.random.bytes(&random_bytes);
        try a.setString(16, try bytesToHex(allocator, &random_bytes));

        // a = a mod (n-3) + 2 to get a in [2, n-2]
        var n_minus_3 = try BigInt.init(allocator);
        defer n_minus_3.deinit();
        try n_minus_3.copy(n.toConst());
        try n_minus_3.addScalar(&n_minus_3, -3);

        var remainder = try BigInt.init(allocator);
        defer remainder.deinit();
        try a.divTrunc(&remainder, &a, &n_minus_3);
        try a.copy(remainder.toConst());
        try a.addScalar(&a, 2);

        // x = a^d mod n
        var x = try modPowBigInt(allocator, &a, &d, n);
        defer x.deinit();

        // Check if x == 1 or x == n-1
        var one = try BigInt.initSet(allocator, 1);
        defer one.deinit();

        if (x.toConst().eql(one.toConst()) or x.toConst().eql(n_minus_1.toConst())) {
            continue;
        }

        var composite = true;
        var j: usize = 0;
        while (j < r - 1) : (j += 1) {
            // x = x^2 mod n
            var x_squared = try BigInt.init(allocator);
            defer x_squared.deinit();
            try x_squared.mul(&x, &x);

            var new_x = try BigInt.init(allocator);
            try x.divTrunc(&new_x, &x_squared, n);
            x.deinit();
            x = new_x;

            if (x.toConst().eql(n_minus_1.toConst())) {
                composite = false;
                break;
            }
        }

        if (composite) {
            return false;
        }
    }

    return true;
}

/// Modular exponentiation for BigInt
fn modPowBigInt(allocator: std.mem.Allocator, base: *BigInt, exp: *BigInt, mod: *BigInt) !BigInt {
    var result = try BigInt.initSet(allocator, 1);
    errdefer result.deinit();

    var b = try BigInt.init(allocator);
    defer b.deinit();
    try b.copy(base.toConst());

    var e = try BigInt.init(allocator);
    defer e.deinit();
    try e.copy(exp.toConst());

    var zero = try BigInt.initSet(allocator, 0);
    defer zero.deinit();

    while (!e.toConst().eql(zero.toConst())) {
        if (e.toConst().limbs.len > 0 and (e.toConst().limbs[0] & 1) == 1) {
            var temp = try BigInt.init(allocator);
            defer temp.deinit();
            try temp.mul(&result, &b);

            var new_result = try BigInt.init(allocator);
            try result.divTrunc(&new_result, &temp, mod);
            result.deinit();
            result = new_result;
        }

        try e.shiftRight(&e, 1);

        var b_squared = try BigInt.init(allocator);
        defer b_squared.deinit();
        try b_squared.mul(&b, &b);

        var new_b = try BigInt.init(allocator);
        try b.divTrunc(&new_b, &b_squared, mod);
        b.deinit();
        try b.copy(new_b.toConst());
        new_b.deinit();
    }

    return result;
}

/// Generate a random prime of specified bit length
fn generatePrime(allocator: std.mem.Allocator, bits: usize) !BigInt {
    const byte_len = (bits + 7) / 8;
    var buffer = try allocator.alloc(u8, byte_len);
    defer allocator.free(buffer);

    while (true) {
        // Generate random bytes
        std.crypto.random.bytes(buffer);

        // Set MSB to ensure correct bit length
        buffer[0] |= 0x80;
        // Set LSB to ensure odd number
        buffer[byte_len - 1] |= 1;

        // Convert to BigInt
        var candidate = try BigInt.init(allocator);
        errdefer candidate.deinit();
        const hex_str = try bytesToHex(allocator, buffer);
        defer allocator.free(hex_str);
        try candidate.setString(16, hex_str);

        // Test primality with 20 rounds of Miller-Rabin
        if (try millerRabinTest(allocator, &candidate, 20)) {
            return candidate;
        }
        candidate.deinit();
    }
}

/// Extended Euclidean algorithm to find modular inverse
fn modInverse(allocator: std.mem.Allocator, a: *BigInt, m: *BigInt) !BigInt {
    var m0 = try BigInt.init(allocator);
    defer m0.deinit();
    try m0.copy(m.toConst());

    var x0 = try BigInt.initSet(allocator, 0);
    defer x0.deinit();
    var x1 = try BigInt.initSet(allocator, 1);
    defer x1.deinit();

    var a_copy = try BigInt.init(allocator);
    defer a_copy.deinit();
    try a_copy.copy(a.toConst());

    var m_copy = try BigInt.init(allocator);
    defer m_copy.deinit();
    try m_copy.copy(m.toConst());

    var one = try BigInt.initSet(allocator, 1);
    defer one.deinit();

    if (m_copy.toConst().eql(one.toConst())) {
        return BigInt.initSet(allocator, 0);
    }

    while (a_copy.toConst().orderAgainstScalar(0) == .gt) {
        var q = try BigInt.init(allocator);
        defer q.deinit();
        var r = try BigInt.init(allocator);
        defer r.deinit();
        try q.divTrunc(&r, &m_copy, &a_copy);

        try m_copy.copy(a_copy.toConst());
        try a_copy.copy(r.toConst());

        var temp = try BigInt.init(allocator);
        defer temp.deinit();
        try temp.mul(&q, &x1);

        var new_x0 = try BigInt.init(allocator);
        try new_x0.sub(&x0, &temp);

        try x0.copy(x1.toConst());
        try x1.copy(new_x0.toConst());
        new_x0.deinit();
    }

    // Make sure result is positive
    if (x0.isPositive() == false) {
        var result = try BigInt.init(allocator);
        try result.add(&x0, &m0);
        return result;
    }

    var result = try BigInt.init(allocator);
    try result.copy(x0.toConst());
    return result;
}

/// generatePrimeSync(size, [options]) - Generate a random prime number
/// size: bit length of the prime
/// Returns: Uint8Array containing the prime in big-endian format
fn generatePrimeSyncFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "generatePrimeSync requires size");

    var size: i32 = 0;
    _ = qjs.JS_ToInt32(ctx, &size, argv[0]);

    if (size < 2 or size > 8192) {
        return qjs.JS_ThrowRangeError(ctx, "size must be between 2 and 8192 bits");
    }

    const allocator = std.heap.page_allocator;

    var prime = generatePrime(allocator, @intCast(size)) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to generate prime");
    };
    defer prime.deinit();

    // Convert to bytes
    const prime_hex = prime.toString(allocator, 16, .lower) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to convert prime to hex");
    };
    defer allocator.free(prime_hex);

    const byte_len = (@as(usize, @intCast(size)) + 7) / 8;
    const prime_bytes = hexToBytes(allocator, prime_hex, byte_len) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to convert prime to bytes");
    };
    defer allocator.free(prime_bytes);

    return createUint8Array(ctx, prime_bytes);
}

/// checkPrimeSync(candidate, [options]) - Check if a number is prime
/// candidate: ArrayBuffer containing the number in big-endian format
/// Returns: boolean
fn checkPrimeSyncFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "checkPrimeSync requires candidate");

    var candidate_size: usize = 0;
    const candidate_ptr = qjs.JS_GetArrayBuffer(ctx, &candidate_size, argv[0]);
    if (candidate_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "candidate must be ArrayBuffer");
    }
    const candidate_bytes = @as([*]const u8, @ptrCast(candidate_ptr))[0..candidate_size];

    const allocator = std.heap.page_allocator;

    // Convert bytes to BigInt
    const hex_str = bytesToHex(allocator, candidate_bytes) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to convert candidate to hex");
    };
    defer allocator.free(hex_str);

    var candidate = BigInt.init(allocator) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to init BigInt");
    };
    defer candidate.deinit();
    candidate.setString(16, hex_str) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to parse candidate");
    };

    // Run Miller-Rabin primality test with 20 rounds
    const is_prime = millerRabinTest(allocator, &candidate, 20) catch {
        return qjs.JS_ThrowInternalError(ctx, "Primality test failed");
    };

    return if (is_prime) quickjs.jsTrue() else quickjs.jsFalse();
}

/// rsaGenerateKeyPairSync(modulusLength) - Generate RSA key pair
/// modulusLength: 2048, 3072, or 4096 bits
/// Returns: { n, e, d, modulusLength, type }
fn rsaGenerateKeyPairSyncFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "rsaGenerateKeyPairSync requires modulusLength");

    var modulus_length: i32 = 2048;
    _ = qjs.JS_ToInt32(ctx, &modulus_length, argv[0]);

    if (modulus_length != 2048 and modulus_length != 3072 and modulus_length != 4096) {
        return qjs.JS_ThrowRangeError(ctx, "modulusLength must be 2048, 3072, or 4096");
    }

    const allocator = std.heap.page_allocator;
    const prime_bits: usize = @divExact(@as(usize, @intCast(modulus_length)), 2);

    // Generate two random primes p and q
    var p = generatePrime(allocator, prime_bits) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to generate prime p");
    };
    defer p.deinit();

    var q = generatePrime(allocator, prime_bits) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to generate prime q");
    };
    defer q.deinit();

    // n = p * q
    var n = BigInt.init(allocator) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to compute n");
    };
    defer n.deinit();
    n.mul(&p, &q) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to compute n = p * q");
    };

    // phi = (p-1) * (q-1)
    var p_minus_1 = BigInt.init(allocator) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to init p_minus_1");
    };
    defer p_minus_1.deinit();
    p_minus_1.copy(p.toConst()) catch {};
    p_minus_1.addScalar(&p_minus_1, -1) catch {};

    var q_minus_1 = BigInt.init(allocator) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to init q_minus_1");
    };
    defer q_minus_1.deinit();
    q_minus_1.copy(q.toConst()) catch {};
    q_minus_1.addScalar(&q_minus_1, -1) catch {};

    var phi = BigInt.init(allocator) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to init phi");
    };
    defer phi.deinit();
    phi.mul(&p_minus_1, &q_minus_1) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to compute phi");
    };

    // e = 65537 (common public exponent)
    var e = BigInt.initSet(allocator, 65537) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to init e");
    };
    defer e.deinit();

    // d = e^(-1) mod phi
    var d = modInverse(allocator, &e, &phi) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to compute modular inverse");
    };
    defer d.deinit();

    // Convert to bytes
    const n_hex = n.toString(allocator, 16, .lower) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to convert n to hex");
    };
    defer allocator.free(n_hex);
    const n_bytes = hexToBytes(allocator, n_hex, @divExact(@as(usize, @intCast(modulus_length)), 8)) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to convert n to bytes");
    };
    defer allocator.free(n_bytes);

    const d_hex = d.toString(allocator, 16, .lower) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to convert d to hex");
    };
    defer allocator.free(d_hex);
    const d_bytes = hexToBytes(allocator, d_hex, @divExact(@as(usize, @intCast(modulus_length)), 8)) catch {
        return qjs.JS_ThrowInternalError(ctx, "Failed to convert d to bytes");
    };
    defer allocator.free(d_bytes);

    const e_bytes = [_]u8{ 0x01, 0x00, 0x01 }; // 65537 in big-endian

    // Create result object
    const result = qjs.JS_NewObject(ctx);
    if (qjs.JS_IsException(result)) return result;

    _ = qjs.JS_SetPropertyStr(ctx, result, "n", createUint8Array(ctx, n_bytes));
    _ = qjs.JS_SetPropertyStr(ctx, result, "e", createUint8Array(ctx, &e_bytes));
    _ = qjs.JS_SetPropertyStr(ctx, result, "d", createUint8Array(ctx, d_bytes));
    _ = qjs.JS_SetPropertyStr(ctx, result, "type", qjs.JS_NewString(ctx, "rsa"));
    _ = qjs.JS_SetPropertyStr(ctx, result, "modulusLength", qjs.JS_NewInt32(ctx, modulus_length));

    return result;
}

/// x25519DerivePublicKey(privateKey) - Derive public key from X25519 private key
/// privateKey: 32-byte private key
/// Returns: 32-byte public key
fn x25519DerivePublicKeyFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "x25519DerivePublicKey requires privateKey");

    // Get private key (32 bytes)
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null or key_size != 32) {
        return qjs.JS_ThrowTypeError(ctx, "Private key must be 32 bytes");
    }
    const key_data: *const [32]u8 = @ptrCast(@alignCast(key_ptr));

    // Derive public key using X25519 scalar multiplication with base point
    const X25519 = std.crypto.dh.X25519;
    const public_key = X25519.recoverPublicKey(key_data.*) catch {
        return qjs.JS_ThrowTypeError(ctx, "Failed to derive X25519 public key");
    };

    var pub_bytes: [32]u8 = public_key;
    return createUint8Array(ctx, &pub_bytes);
}

/// generateKeyPairSync(algorithm, options) - Generate key pair for algorithm
/// algorithm: "ed25519" | "x25519" | "ec"
/// options: { namedCurve: "prime256v1" | "P-256" } for EC
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
    } else if (std.mem.eql(u8, algorithm, "ec")) {
        // Parse options for namedCurve
        if (argc >= 2 and !qjs.JS_IsUndefined(argv[1]) and !qjs.JS_IsNull(argv[1])) {
            const curve_val = qjs.JS_GetPropertyStr(ctx, argv[1], "namedCurve");
            defer qjs.JS_FreeValue(ctx, curve_val);

            if (!qjs.JS_IsUndefined(curve_val)) {
                const curve_str = qjs.JS_ToCString(ctx, curve_val);
                if (curve_str != null) {
                    defer qjs.JS_FreeCString(ctx, curve_str);
                    const curve = std.mem.span(curve_str);

                    if (std.mem.eql(u8, curve, "prime256v1") or std.mem.eql(u8, curve, "P-256") or std.mem.eql(u8, curve, "p256")) {
                        return p256GenerateKeyPairFunc(ctx, quickjs.jsUndefined(), 0, argv);
                    } else if (std.mem.eql(u8, curve, "secp384r1") or std.mem.eql(u8, curve, "P-384") or std.mem.eql(u8, curve, "p384")) {
                        return p384GenerateKeyPairFunc(ctx, quickjs.jsUndefined(), 0, argv);
                    }
                }
            }
        }
        return qjs.JS_ThrowTypeError(ctx, "EC requires namedCurve option (P-256, P-384)");
    } else {
        return qjs.JS_ThrowTypeError(ctx, "Unsupported algorithm (use ed25519, x25519, or ec)");
    }
}

// ============ RSA Digital Signatures (for JWT RS256/RS384/RS512) ============

const BigInt = std.math.big.int.Managed;

/// RSA key components stored in raw bytes (big-endian)
const RsaKey = struct {
    n: []const u8, // modulus
    e: []const u8, // public exponent
    d: ?[]const u8, // private exponent (null for public key)
};

/// Check if data looks like PEM format (starts with "-----BEGIN")
fn isPemFormat(data: []const u8) bool {
    if (data.len < 11) return false;
    // Check for "-----BEGIN" at start (after skipping whitespace)
    var i: usize = 0;
    while (i < data.len and (data[i] == ' ' or data[i] == '\n' or data[i] == '\r' or data[i] == '\t')) {
        i += 1;
    }
    if (i + 10 > data.len) return false;
    return std.mem.eql(u8, data[i .. i + 10], "-----BEGIN");
}

/// Convert PEM to DER format (extracts base64 content between headers)
/// Returns owned slice that caller must free
fn pemToDer(allocator: std.mem.Allocator, pem: []const u8) ![]u8 {
    // Find start of base64 content (after first newline following -----BEGIN...-----)
    var start: usize = 0;
    var found_begin = false;
    while (start < pem.len) {
        if (start + 10 <= pem.len and std.mem.eql(u8, pem[start .. start + 10], "-----BEGIN")) {
            // Skip to end of line
            while (start < pem.len and pem[start] != '\n') {
                start += 1;
            }
            if (start < pem.len) start += 1; // Skip newline
            found_begin = true;
            break;
        }
        start += 1;
    }
    if (!found_begin) return error.InvalidPem;

    // Find end of base64 content (before -----END)
    var end = start;
    while (end < pem.len) {
        if (end + 8 <= pem.len and std.mem.eql(u8, pem[end .. end + 8], "-----END")) {
            break;
        }
        end += 1;
    }
    if (end <= start) return error.InvalidPem;

    // Decode base64 content
    return base64Decode(allocator, pem[start..end]);
}

/// Parse key - accepts either PEM or DER format
/// Returns DER bytes (may be newly allocated if PEM, or just the input if already DER)
/// Caller owns the returned slice only if input was PEM
fn parseKeyFormat(allocator: std.mem.Allocator, data: []const u8) !struct { der: []const u8, owned: bool } {
    if (isPemFormat(data)) {
        const der = try pemToDer(allocator, data);
        return .{ .der = der, .owned = true };
    }
    // Already DER format (starts with 0x30 = SEQUENCE)
    return .{ .der = data, .owned = false };
}

/// Base64 decode (standard alphabet, handles padding)
fn base64Decode(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    const base64_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    var decode_table: [256]u8 = undefined;
    @memset(&decode_table, 255);
    for (base64_chars, 0..) |c, i| {
        decode_table[c] = @intCast(i);
    }
    decode_table['='] = 0;

    // Filter out whitespace and calculate output size
    var clean_len: usize = 0;
    for (input) |c| {
        if (c != ' ' and c != '\n' and c != '\r' and c != '\t') {
            clean_len += 1;
        }
    }

    const output_len = (clean_len * 3) / 4;
    var output = try allocator.alloc(u8, output_len);
    errdefer allocator.free(output);

    var out_idx: usize = 0;
    var buffer: u32 = 0;
    var bits: u32 = 0;

    for (input) |c| {
        if (c == ' ' or c == '\n' or c == '\r' or c == '\t') continue;
        if (c == '=') break;

        const val = decode_table[c];
        if (val == 255) return error.InvalidBase64;

        buffer = (buffer << 6) | val;
        bits += 6;

        if (bits >= 8) {
            bits -= 8;
            if (out_idx < output.len) {
                output[out_idx] = @intCast((buffer >> @intCast(bits)) & 0xFF);
                out_idx += 1;
            }
        }
    }

    return output[0..out_idx];
}

/// Parse DER-encoded ASN.1 length
fn parseDerLength(data: []const u8, pos: *usize) !usize {
    if (pos.* >= data.len) return error.InvalidDer;
    const first = data[pos.*];
    pos.* += 1;

    if (first < 0x80) {
        return first;
    }

    const num_bytes = first & 0x7F;
    if (num_bytes > 4 or pos.* + num_bytes > data.len) return error.InvalidDer;

    var length: usize = 0;
    for (0..num_bytes) |_| {
        length = (length << 8) | data[pos.*];
        pos.* += 1;
    }
    return length;
}

/// Parse DER integer (returns slice to the integer bytes, big-endian)
fn parseDerInteger(data: []const u8, pos: *usize) ![]const u8 {
    if (pos.* >= data.len or data[pos.*] != 0x02) return error.InvalidDer;
    pos.* += 1;

    const len = try parseDerLength(data, pos);
    if (pos.* + len > data.len) return error.InvalidDer;

    var start = pos.*;
    // Skip leading zero if present (used for positive encoding)
    if (len > 0 and data[start] == 0x00) {
        start += 1;
    }

    const result = data[start .. pos.* + len];
    pos.* += len;
    return result;
}

/// Parse PKCS#1 RSA private key DER format
/// RSAPrivateKey ::= SEQUENCE {
///   version INTEGER, n INTEGER, e INTEGER, d INTEGER, p INTEGER, q INTEGER,
///   dP INTEGER, dQ INTEGER, qInv INTEGER
/// }
fn parseRsaPrivateKeyDer(data: []const u8) !RsaKey {
    var pos: usize = 0;

    // SEQUENCE
    if (pos >= data.len or data[pos] != 0x30) return error.InvalidDer;
    pos += 1;
    _ = try parseDerLength(data, &pos);

    // version
    _ = try parseDerInteger(data, &pos);

    // n (modulus)
    const n = try parseDerInteger(data, &pos);

    // e (public exponent)
    const e = try parseDerInteger(data, &pos);

    // d (private exponent)
    const d = try parseDerInteger(data, &pos);

    return RsaKey{ .n = n, .e = e, .d = d };
}

/// Parse PKCS#1 RSA public key DER format
/// RSAPublicKey ::= SEQUENCE { n INTEGER, e INTEGER }
fn parseRsaPublicKeyDer(data: []const u8) !RsaKey {
    var pos: usize = 0;

    // SEQUENCE
    if (pos >= data.len or data[pos] != 0x30) return error.InvalidDer;
    pos += 1;
    _ = try parseDerLength(data, &pos);

    // n (modulus)
    const n = try parseDerInteger(data, &pos);

    // e (public exponent)
    const e = try parseDerInteger(data, &pos);

    return RsaKey{ .n = n, .e = e, .d = null };
}

/// Modular exponentiation: result = base^exp mod m
/// Uses square-and-multiply algorithm with big integers
fn modPow(allocator: std.mem.Allocator, base_bytes: []const u8, exp_bytes: []const u8, mod_bytes: []const u8) ![]u8 {
    var base_int = try BigInt.init(allocator);
    defer base_int.deinit();
    var exp_int = try BigInt.init(allocator);
    defer exp_int.deinit();
    var mod_int = try BigInt.init(allocator);
    defer mod_int.deinit();
    var result = try BigInt.init(allocator);
    defer result.deinit();
    var temp = try BigInt.init(allocator);
    defer temp.deinit();

    // Set values from big-endian bytes
    try base_int.setString(16, try bytesToHex(allocator, base_bytes));
    try exp_int.setString(16, try bytesToHex(allocator, exp_bytes));
    try mod_int.setString(16, try bytesToHex(allocator, mod_bytes));

    // result = 1
    try result.set(1);

    // Square and multiply
    var exp_bits = exp_int.toConst();
    const bit_count = exp_bits.bitCountAbs();

    var i: usize = 0;
    while (i < bit_count) : (i += 1) {
        // result = result * result mod m
        try temp.mul(&result, &result);
        var r = try BigInt.init(allocator);
        defer r.deinit();
        try result.divTrunc(&r, &temp, &mod_int);
        std.mem.swap(BigInt, &result, &r);

        // Check if bit is set (from MSB)
        const bit_idx = bit_count - 1 - i;
        if (exp_bits.limbs.len > 0) {
            const limb_idx = bit_idx / @bitSizeOf(std.math.big.Limb);
            const bit_in_limb = bit_idx % @bitSizeOf(std.math.big.Limb);
            if (limb_idx < exp_bits.limbs.len) {
                const bit_set = (exp_bits.limbs[limb_idx] >> @intCast(bit_in_limb)) & 1;
                if (bit_set == 1) {
                    // result = result * base mod m
                    try temp.mul(&result, &base_int);
                    var r2 = try BigInt.init(allocator);
                    defer r2.deinit();
                    try result.divTrunc(&r2, &temp, &mod_int);
                    std.mem.swap(BigInt, &result, &r2);
                }
            }
        }
    }

    // Convert result back to bytes
    const hex_str = try result.toString(allocator, 16, .lower);
    defer allocator.free(hex_str);
    return hexToBytes(allocator, hex_str, mod_bytes.len);
}

fn bytesToHex(allocator: std.mem.Allocator, bytes: []const u8) ![]const u8 {
    var hex = try allocator.alloc(u8, bytes.len * 2);
    const hex_chars = "0123456789abcdef";
    for (bytes, 0..) |b, i| {
        hex[i * 2] = hex_chars[b >> 4];
        hex[i * 2 + 1] = hex_chars[b & 0xF];
    }
    return hex;
}

fn hexToBytes(allocator: std.mem.Allocator, hex: []const u8, target_len: usize) ![]u8 {
    const hex_len = hex.len;
    const byte_len = (hex_len + 1) / 2;
    const padding = if (target_len > byte_len) target_len - byte_len else 0;

    var bytes = try allocator.alloc(u8, target_len);
    @memset(bytes[0..padding], 0);

    var i: usize = 0;
    var j: usize = padding;
    if (hex_len % 2 == 1) {
        bytes[j] = try hexDigit(hex[0]);
        i = 1;
        j += 1;
    }
    while (i < hex_len) : ({
        i += 2;
        j += 1;
    }) {
        bytes[j] = (try hexDigit(hex[i])) << 4 | try hexDigit(hex[i + 1]);
    }
    return bytes;
}

fn hexDigit(c: u8) !u8 {
    if (c >= '0' and c <= '9') return c - '0';
    if (c >= 'a' and c <= 'f') return c - 'a' + 10;
    if (c >= 'A' and c <= 'F') return c - 'A' + 10;
    return error.InvalidHex;
}

/// PKCS#1 v1.5 signature padding for RSA
/// DigestInfo for SHA-256: 30 31 30 0d 06 09 60 86 48 01 65 03 04 02 01 05 00 04 20
const SHA256_DIGEST_INFO: []const u8 = &[_]u8{
    0x30, 0x31, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01,
    0x65, 0x03, 0x04, 0x02, 0x01, 0x05, 0x00, 0x04, 0x20,
};

fn pkcs1v15Pad(allocator: std.mem.Allocator, hash: []const u8, key_len: usize) ![]u8 {
    const digest_info = SHA256_DIGEST_INFO;
    const t_len = digest_info.len + hash.len;

    if (key_len < t_len + 11) return error.KeyTooShort;

    var em = try allocator.alloc(u8, key_len);
    em[0] = 0x00;
    em[1] = 0x01; // Block type for signature

    const ps_len = key_len - t_len - 3;
    @memset(em[2 .. 2 + ps_len], 0xFF);
    em[2 + ps_len] = 0x00;

    @memcpy(em[3 + ps_len .. 3 + ps_len + digest_info.len], digest_info);
    @memcpy(em[3 + ps_len + digest_info.len ..], hash);

    return em;
}

// ============ RSA-PSS Padding for Signatures (RFC 8017) ============

/// RSA-PSS signature padding (EMSA-PSS-ENCODE)
/// EM = maskedDB || H || 0xBC
/// where:
///   M' = (0x00 x 8) || mHash || salt
///   H = Hash(M')
///   DB = PS || 0x01 || salt
///   dbMask = MGF1(H, emLen - hLen - 1)
///   maskedDB = DB XOR dbMask
///   Set the leftmost 8*emLen - emBits bits of maskedDB to zero
fn pssPad(allocator: std.mem.Allocator, m_hash: []const u8, em_len: usize, salt_len_param: i32) ![]u8 {
    const h_len: usize = 32; // SHA-256 output

    // Determine salt length
    // RSA_PSS_SALTLEN_DIGEST (-1) = use hash length
    // RSA_PSS_SALTLEN_MAX_SIGN (-2) = maximum possible
    // Otherwise use provided value
    const max_salt_len = if (em_len > h_len + 2) em_len - h_len - 2 else 0;
    const salt_len: usize = if (salt_len_param == -1)
        h_len // SALTLEN_DIGEST
    else if (salt_len_param == -2)
        max_salt_len // SALTLEN_MAX
    else if (salt_len_param >= 0)
        @min(@as(usize, @intCast(salt_len_param)), max_salt_len)
    else
        h_len;

    // Check em_len >= h_len + salt_len + 2
    if (em_len < h_len + salt_len + 2) {
        return error.EncodingError;
    }

    const em = try allocator.alloc(u8, em_len);
    errdefer allocator.free(em);

    // Generate random salt
    var salt_buf: [64]u8 = undefined; // Max salt we support
    const salt = salt_buf[0..salt_len];
    std.crypto.random.bytes(salt);

    // M' = (0x00 x 8) || mHash || salt
    // H = Hash(M')
    var h_buf: [h_len]u8 = undefined;
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(&[_]u8{0} ** 8); // 8 zero bytes
    hasher.update(m_hash);
    hasher.update(salt);
    hasher.final(&h_buf);

    // DB = PS || 0x01 || salt
    const db_len = em_len - h_len - 1;
    var db = try allocator.alloc(u8, db_len);
    defer allocator.free(db);

    const ps_len = db_len - salt_len - 1;
    @memset(db[0..ps_len], 0); // PS padding zeros
    db[ps_len] = 0x01; // Separator
    @memcpy(db[ps_len + 1 ..], salt);

    // dbMask = MGF1(H, db_len)
    const db_mask = try mgf1Sha256(allocator, &h_buf, db_len);
    defer allocator.free(db_mask);

    // maskedDB = DB XOR dbMask
    for (db, db_mask, 0..) |d, m, i| {
        em[i] = d ^ m;
    }

    // Set the leftmost bits to zero (for RSA modulus size)
    // For standard RSA key sizes, this is typically just clearing the top bit
    em[0] &= 0x7F; // Clear top bit

    // H
    @memcpy(em[db_len..][0..h_len], &h_buf);

    // Trailer byte 0xBC
    em[em_len - 1] = 0xBC;

    return em;
}

/// RSA-PSS signature verification (EMSA-PSS-VERIFY)
fn pssVerify(allocator: std.mem.Allocator, m_hash: []const u8, em: []const u8, salt_len_param: i32) !bool {
    const h_len: usize = 32; // SHA-256 output
    const em_len = em.len;

    // Check minimum length
    if (em_len < h_len + 2) {
        return false;
    }

    // Check trailer byte
    if (em[em_len - 1] != 0xBC) {
        return false;
    }

    // Split EM into maskedDB and H
    const db_len = em_len - h_len - 1;
    const masked_db = em[0..db_len];
    const h = em[db_len .. db_len + h_len];

    // dbMask = MGF1(H, db_len)
    const db_mask = try mgf1Sha256(allocator, h, db_len);
    defer allocator.free(db_mask);

    // DB = maskedDB XOR dbMask
    var db = try allocator.alloc(u8, db_len);
    defer allocator.free(db);

    for (masked_db, db_mask, 0..) |md, dm, i| {
        db[i] = md ^ dm;
    }

    // Clear leftmost bits
    db[0] &= 0x7F;

    // Find 0x01 separator
    var sep_idx: ?usize = null;
    for (db, 0..) |byte, i| {
        if (byte == 0x01) {
            sep_idx = i;
            break;
        } else if (byte != 0x00) {
            return false;
        }
    }

    const idx = sep_idx orelse return false;
    const salt = db[idx + 1 ..];
    const actual_salt_len = salt.len;

    // Check salt length if specified
    if (salt_len_param >= 0) {
        const expected_salt_len: usize = @intCast(salt_len_param);
        if (actual_salt_len != expected_salt_len) {
            return false;
        }
    }

    // M' = (0x00 x 8) || mHash || salt
    // H' = Hash(M')
    var h_prime: [h_len]u8 = undefined;
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(&[_]u8{0} ** 8);
    hasher.update(m_hash);
    hasher.update(salt);
    hasher.final(&h_prime);

    // Constant-time comparison
    var diff: u8 = 0;
    for (h, &h_prime) |a, b| {
        diff |= a ^ b;
    }

    return diff == 0;
}

// RSA signature padding modes
const RSA_SIG_PADDING = struct {
    const PKCS1_V15: i32 = 1;
    const PSS: i32 = 6; // RSA_PKCS1_PSS_PADDING in OpenSSL
};

/// rsaSign(privateKey, message, [padding], [saltLength]) - Sign with RSA-SHA256
/// padding: 1 = PKCS#1 v1.5 (default), 6 = PSS
/// saltLength: -1 = SALTLEN_DIGEST, -2 = SALTLEN_MAX, or specific length (for PSS)
/// privateKey: ArrayBuffer (PEM or DER format)
fn rsaSignFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "rsaSign requires privateKey, message");

    // Get private key (PEM or DER)
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Private key must be ArrayBuffer");
    }
    const key_data = @as([*]const u8, @ptrCast(key_ptr))[0..key_size];

    // Convert PEM to DER if needed
    const key_parsed = parseKeyFormat(std.heap.page_allocator, key_data) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid key format (PEM or DER expected)");
    };
    defer if (key_parsed.owned) std.heap.page_allocator.free(@constCast(key_parsed.der));

    // Get message
    var msg_size: usize = 0;
    const msg_ptr = qjs.JS_GetArrayBuffer(ctx, &msg_size, argv[1]);
    if (msg_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Message must be ArrayBuffer");
    }
    const message = @as([*]const u8, @ptrCast(msg_ptr))[0..msg_size];

    // Get padding mode (optional, default to PKCS#1 v1.5)
    var padding: i32 = RSA_SIG_PADDING.PKCS1_V15;
    if (argc >= 3) {
        _ = qjs.JS_ToInt32(ctx, &padding, argv[2]);
    }

    // Get salt length (for PSS, optional)
    var salt_len: i32 = -1; // Default: SALTLEN_DIGEST
    if (argc >= 4) {
        _ = qjs.JS_ToInt32(ctx, &salt_len, argv[3]);
    }

    // Parse RSA private key
    const key = parseRsaPrivateKeyDer(key_parsed.der) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid RSA private key format");
    };

    const d = key.d orelse {
        return qjs.JS_ThrowTypeError(ctx, "Private key required for signing");
    };

    // Hash the message with SHA-256
    var hash: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(message, &hash, .{});

    const key_len = key.n.len;

    // Pad according to padding mode
    const padded = if (padding == RSA_SIG_PADDING.PSS)
        pssPad(std.heap.page_allocator, &hash, key_len, salt_len) catch {
            return qjs.JS_ThrowInternalError(ctx, "PSS padding failed");
        }
    else
        pkcs1v15Pad(std.heap.page_allocator, &hash, key_len) catch {
            return qjs.JS_ThrowInternalError(ctx, "PKCS#1 padding failed");
        };
    defer std.heap.page_allocator.free(padded);

    // Sign: signature = padded^d mod n
    const signature = modPow(std.heap.page_allocator, padded, d, key.n) catch {
        return qjs.JS_ThrowInternalError(ctx, "RSA signing failed");
    };
    defer std.heap.page_allocator.free(signature);

    return createUint8Array(ctx, signature);
}

/// rsaVerify(publicKey, message, signature, [padding], [saltLength]) - Verify RSA-SHA256 signature
/// padding: 1 = PKCS#1 v1.5 (default), 6 = PSS
/// saltLength: -1 = SALTLEN_DIGEST, -2 = SALTLEN_AUTO, or specific length (for PSS)
/// publicKey: ArrayBuffer (PEM or DER format)
fn rsaVerifyFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) return qjs.JS_ThrowTypeError(ctx, "rsaVerify requires publicKey, message, signature");

    // Get public key (PEM or DER)
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Public key must be ArrayBuffer");
    }
    const key_data = @as([*]const u8, @ptrCast(key_ptr))[0..key_size];

    // Convert PEM to DER if needed
    const key_parsed = parseKeyFormat(std.heap.page_allocator, key_data) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid key format (PEM or DER expected)");
    };
    defer if (key_parsed.owned) std.heap.page_allocator.free(@constCast(key_parsed.der));

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
    if (sig_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Signature must be ArrayBuffer");
    }
    const signature = @as([*]const u8, @ptrCast(sig_ptr))[0..sig_size];

    // Get padding mode (optional, default to PKCS#1 v1.5)
    var padding: i32 = RSA_SIG_PADDING.PKCS1_V15;
    if (argc >= 4) {
        _ = qjs.JS_ToInt32(ctx, &padding, argv[3]);
    }

    // Get salt length (for PSS, optional)
    var salt_len: i32 = -2; // Default: SALTLEN_AUTO (try to detect)
    if (argc >= 5) {
        _ = qjs.JS_ToInt32(ctx, &salt_len, argv[4]);
    }

    // Parse RSA public key
    const key = parseRsaPublicKeyDer(key_parsed.der) catch {
        // Try parsing as private key (contains public key components)
        const priv_key = parseRsaPrivateKeyDer(key_parsed.der) catch {
            return qjs.JS_ThrowTypeError(ctx, "Invalid RSA key format");
        };
        return rsaVerifyWithKey(priv_key, message, signature, padding, salt_len);
    };

    return rsaVerifyWithKey(key, message, signature, padding, salt_len);
}

fn rsaVerifyWithKey(key: RsaKey, message: []const u8, signature: []const u8, padding: i32, salt_len: i32) qjs.JSValue {
    // Hash the message with SHA-256
    var hash: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(message, &hash, .{});

    // Verify: decrypted = signature^e mod n
    const decrypted = modPow(std.heap.page_allocator, signature, key.e, key.n) catch {
        return quickjs.jsFalse();
    };
    defer std.heap.page_allocator.free(decrypted);

    if (padding == RSA_SIG_PADDING.PSS) {
        // PSS verification
        const valid = pssVerify(std.heap.page_allocator, &hash, decrypted, salt_len) catch {
            return quickjs.jsFalse();
        };
        return if (valid) quickjs.jsTrue() else quickjs.jsFalse();
    } else {
        // PKCS#1 v1.5 verification
        const key_len = key.n.len;
        const expected = pkcs1v15Pad(std.heap.page_allocator, &hash, key_len) catch {
            return quickjs.jsFalse();
        };
        defer std.heap.page_allocator.free(expected);

        // Compare (constant time)
        if (decrypted.len != expected.len) return quickjs.jsFalse();

        var diff: u8 = 0;
        for (decrypted, expected) |a, b| {
            diff |= a ^ b;
        }

        return if (diff == 0) quickjs.jsTrue() else quickjs.jsFalse();
    }
}

/// PKCS#1 v1.5 encryption padding (type 2)
/// Format: 0x00 0x02 [random non-zero bytes] 0x00 [message]
fn pkcs1v15EncryptPad(allocator: std.mem.Allocator, message: []const u8, key_len: usize) ![]u8 {
    // Need at least 11 bytes of padding: 0x00 0x02 + 8 random + 0x00
    if (message.len > key_len - 11) {
        return error.MessageTooLong;
    }

    const em = try allocator.alloc(u8, key_len);
    em[0] = 0x00;
    em[1] = 0x02;

    // Fill with random non-zero bytes
    const ps_len = key_len - message.len - 3;
    const random_bytes = em[2 .. 2 + ps_len];
    std.crypto.random.bytes(random_bytes);

    // Ensure no zero bytes in padding
    for (random_bytes) |*byte| {
        while (byte.* == 0) {
            std.crypto.random.bytes(byte[0..1]);
        }
    }

    em[2 + ps_len] = 0x00;
    @memcpy(em[3 + ps_len ..], message);

    return em;
}

/// Remove PKCS#1 v1.5 encryption padding
fn pkcs1v15EncryptUnpad(padded: []const u8) ![]const u8 {
    if (padded.len < 11) return error.InvalidPadding;
    if (padded[0] != 0x00 or padded[1] != 0x02) return error.InvalidPadding;

    // Find 0x00 separator after padding
    var sep_idx: ?usize = null;
    for (padded[2..], 2..) |byte, i| {
        if (byte == 0x00) {
            sep_idx = i;
            break;
        }
    }

    const idx = sep_idx orelse return error.InvalidPadding;
    if (idx < 10) return error.InvalidPadding; // Need at least 8 bytes of padding

    return padded[idx + 1 ..];
}

// ============ RSA-OAEP Padding (RFC 8017) ============

/// MGF1 (Mask Generation Function) using SHA-256
/// Generates a mask of the specified length from the seed
fn mgf1Sha256(allocator: std.mem.Allocator, seed: []const u8, mask_len: usize) ![]u8 {
    const hash_len: usize = 32; // SHA-256 output
    const mask = try allocator.alloc(u8, mask_len);
    errdefer allocator.free(mask);

    var counter: u32 = 0;
    var offset: usize = 0;

    while (offset < mask_len) {
        // Hash(seed || counter)
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        hasher.update(seed);

        // Append counter as big-endian 4 bytes
        const counter_bytes = [4]u8{
            @truncate(counter >> 24),
            @truncate(counter >> 16),
            @truncate(counter >> 8),
            @truncate(counter),
        };
        hasher.update(&counter_bytes);

        var hash: [hash_len]u8 = undefined;
        hasher.final(&hash);

        // Copy hash to mask
        const copy_len = @min(hash_len, mask_len - offset);
        @memcpy(mask[offset..][0..copy_len], hash[0..copy_len]);

        offset += hash_len;
        counter += 1;
    }

    return mask;
}

/// RSA-OAEP encryption padding (RFC 8017)
/// EM = 0x00 || maskedSeed || maskedDB
/// where:
///   lHash = SHA-256(label)  // Empty label by default
///   DB = lHash || PS || 0x01 || M
///   seed = random 32 bytes
///   dbMask = MGF1(seed, k - hLen - 1)
///   maskedDB = DB XOR dbMask
///   seedMask = MGF1(maskedDB, hLen)
///   maskedSeed = seed XOR seedMask
fn oaepPad(allocator: std.mem.Allocator, message: []const u8, key_len: usize) ![]u8 {
    const hash_len: usize = 32; // SHA-256 output

    // OAEP requires: mLen <= k - 2*hLen - 2
    const max_msg_len = key_len - 2 * hash_len - 2;
    if (message.len > max_msg_len) {
        return error.MessageTooLong;
    }

    const em = try allocator.alloc(u8, key_len);
    errdefer allocator.free(em);
    em[0] = 0x00;

    // lHash = SHA-256("") - empty label
    var l_hash: [hash_len]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(&[_]u8{}, &l_hash, .{});

    // Build DB: lHash || PS || 0x01 || M
    const db_len = key_len - hash_len - 1;
    var db = try allocator.alloc(u8, db_len);
    defer allocator.free(db);

    @memcpy(db[0..hash_len], &l_hash);

    // PS = padding zeros (db_len - hash_len - 1 - message.len zeros)
    const ps_len = db_len - hash_len - 1 - message.len;
    @memset(db[hash_len .. hash_len + ps_len], 0);

    // 0x01 separator
    db[hash_len + ps_len] = 0x01;

    // Message
    @memcpy(db[hash_len + ps_len + 1 ..], message);

    // Generate random seed
    var seed: [hash_len]u8 = undefined;
    std.crypto.random.bytes(&seed);

    // dbMask = MGF1(seed, db_len)
    const db_mask = try mgf1Sha256(allocator, &seed, db_len);
    defer allocator.free(db_mask);

    // maskedDB = DB XOR dbMask
    for (db, db_mask, 0..) |db_byte, mask_byte, i| {
        em[1 + hash_len + i] = db_byte ^ mask_byte;
    }

    // seedMask = MGF1(maskedDB, hash_len)
    const seed_mask = try mgf1Sha256(allocator, em[1 + hash_len ..], hash_len);
    defer allocator.free(seed_mask);

    // maskedSeed = seed XOR seedMask
    for (&seed, seed_mask, 0..) |*seed_byte, mask_byte, i| {
        em[1 + i] = seed_byte.* ^ mask_byte;
    }

    return em;
}

/// RSA-OAEP decryption unpadding (RFC 8017)
fn oaepUnpad(allocator: std.mem.Allocator, padded: []const u8) ![]const u8 {
    const hash_len: usize = 32; // SHA-256 output

    if (padded.len < 2 * hash_len + 2) {
        return error.InvalidPadding;
    }

    // First byte must be 0x00
    if (padded[0] != 0x00) {
        return error.InvalidPadding;
    }

    const masked_seed = padded[1 .. 1 + hash_len];
    const masked_db = padded[1 + hash_len ..];

    // seedMask = MGF1(maskedDB, hash_len)
    const seed_mask = try mgf1Sha256(allocator, masked_db, hash_len);
    defer allocator.free(seed_mask);

    // seed = maskedSeed XOR seedMask
    var seed: [hash_len]u8 = undefined;
    for (&seed, masked_seed, seed_mask) |*s, ms, sm| {
        s.* = ms ^ sm;
    }

    // dbMask = MGF1(seed, len(maskedDB))
    const db_mask = try mgf1Sha256(allocator, &seed, masked_db.len);
    defer allocator.free(db_mask);

    // DB = maskedDB XOR dbMask
    var db = try allocator.alloc(u8, masked_db.len);
    defer allocator.free(db);

    for (db, masked_db, db_mask) |*d, md, dm| {
        d.* = md ^ dm;
    }

    // Verify lHash (first hash_len bytes of DB)
    var expected_l_hash: [hash_len]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(&[_]u8{}, &expected_l_hash, .{});

    // Constant-time comparison of lHash
    var hash_diff: u8 = 0;
    for (db[0..hash_len], &expected_l_hash) |a, b| {
        hash_diff |= a ^ b;
    }

    if (hash_diff != 0) {
        return error.InvalidPadding;
    }

    // Find 0x01 separator after lHash and PS
    var sep_idx: ?usize = null;
    for (db[hash_len..], hash_len..) |byte, i| {
        if (byte == 0x01) {
            sep_idx = i;
            break;
        } else if (byte != 0x00) {
            // Invalid padding - only zeros allowed before 0x01
            return error.InvalidPadding;
        }
    }

    const idx = sep_idx orelse return error.InvalidPadding;

    // Message is everything after the 0x01
    const msg_start = idx + 1;
    if (msg_start >= db.len) {
        return error.InvalidPadding;
    }

    // We need to return a slice that outlives this function
    // The caller should handle this - for now, copy to the input buffer area
    // Actually, we can reuse padded since it's const and we return a slice
    // Let's just return the indices for the caller to extract

    // Store message in a static buffer (not ideal but works for sandbox)
    const msg_len = db.len - msg_start;
    if (msg_len > decrypt_buffer.len) {
        return error.MessageTooLong;
    }

    @memcpy(decrypt_buffer[0..msg_len], db[msg_start..]);
    return decrypt_buffer[0..msg_len];
}

// RSA padding modes
const RSA_PADDING = struct {
    const PKCS1_V15: i32 = 1;
    const OAEP: i32 = 4; // RSA_PKCS1_OAEP_PADDING in OpenSSL
};

/// rsaEncrypt(publicKey, message, [padding]) - Encrypt with RSA
/// padding: 1 = PKCS#1 v1.5 (default), 4 = OAEP (RSA_PKCS1_OAEP_PADDING)
/// publicKey: ArrayBuffer (PEM or DER format)
fn rsaEncryptFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "rsaEncrypt requires publicKey, message");

    // Get public key (PEM or DER)
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Public key must be ArrayBuffer");
    }
    const key_data = @as([*]const u8, @ptrCast(key_ptr))[0..key_size];

    // Convert PEM to DER if needed
    const key_parsed = parseKeyFormat(std.heap.page_allocator, key_data) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid key format (PEM or DER expected)");
    };
    defer if (key_parsed.owned) std.heap.page_allocator.free(@constCast(key_parsed.der));

    // Get message
    var msg_size: usize = 0;
    const msg_ptr = qjs.JS_GetArrayBuffer(ctx, &msg_size, argv[1]);
    if (msg_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Message must be ArrayBuffer");
    }
    const message = @as([*]const u8, @ptrCast(msg_ptr))[0..msg_size];

    // Get padding mode (optional, default to PKCS#1 v1.5)
    var padding: i32 = RSA_PADDING.PKCS1_V15;
    if (argc >= 3) {
        _ = qjs.JS_ToInt32(ctx, &padding, argv[2]);
    }

    // Parse RSA public key (or private key for its public components)
    const pub_key = parseRsaPublicKeyDer(key_parsed.der) catch blk: {
        break :blk parseRsaPrivateKeyDer(key_parsed.der) catch {
            return qjs.JS_ThrowTypeError(ctx, "Invalid RSA key format");
        };
    };

    const key_len = pub_key.n.len;

    // Pad the message according to padding mode
    const padded = if (padding == RSA_PADDING.OAEP)
        oaepPad(std.heap.page_allocator, message, key_len) catch {
            return qjs.JS_ThrowInternalError(ctx, "OAEP padding failed - message too long for RSA key");
        }
    else
        pkcs1v15EncryptPad(std.heap.page_allocator, message, key_len) catch {
            return qjs.JS_ThrowInternalError(ctx, "Message too long for RSA key");
        };
    defer std.heap.page_allocator.free(padded);

    // Encrypt: ciphertext = padded^e mod n
    const ciphertext = modPow(std.heap.page_allocator, padded, pub_key.e, pub_key.n) catch {
        return qjs.JS_ThrowInternalError(ctx, "RSA encryption failed");
    };
    defer std.heap.page_allocator.free(ciphertext);

    return createUint8Array(ctx, ciphertext);
}

/// rsaDecrypt(privateKey, ciphertext, [padding]) - Decrypt with RSA
/// padding: 1 = PKCS#1 v1.5 (default), 4 = OAEP (RSA_PKCS1_OAEP_PADDING)
/// privateKey: ArrayBuffer (PEM or DER format)
fn rsaDecryptFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "rsaDecrypt requires privateKey, ciphertext");

    // Get private key (PEM or DER)
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Private key must be ArrayBuffer");
    }
    const key_data = @as([*]const u8, @ptrCast(key_ptr))[0..key_size];

    // Convert PEM to DER if needed
    const key_parsed = parseKeyFormat(std.heap.page_allocator, key_data) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid key format (PEM or DER expected)");
    };
    defer if (key_parsed.owned) std.heap.page_allocator.free(@constCast(key_parsed.der));

    // Get ciphertext
    var ct_size: usize = 0;
    const ct_ptr = qjs.JS_GetArrayBuffer(ctx, &ct_size, argv[1]);
    if (ct_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Ciphertext must be ArrayBuffer");
    }
    const ciphertext = @as([*]const u8, @ptrCast(ct_ptr))[0..ct_size];

    // Get padding mode (optional, default to PKCS#1 v1.5)
    var padding: i32 = RSA_PADDING.PKCS1_V15;
    if (argc >= 3) {
        _ = qjs.JS_ToInt32(ctx, &padding, argv[2]);
    }

    // Parse RSA private key
    const key = parseRsaPrivateKeyDer(key_parsed.der) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid RSA private key format");
    };

    const d = key.d orelse {
        return qjs.JS_ThrowTypeError(ctx, "Private key required for decryption");
    };

    // Decrypt: padded = ciphertext^d mod n
    const padded = modPow(std.heap.page_allocator, ciphertext, d, key.n) catch {
        return qjs.JS_ThrowInternalError(ctx, "RSA decryption failed");
    };
    defer std.heap.page_allocator.free(padded);

    // Remove padding according to padding mode
    const message = if (padding == RSA_PADDING.OAEP)
        oaepUnpad(std.heap.page_allocator, padded) catch {
            return qjs.JS_ThrowInternalError(ctx, "OAEP unpadding failed - invalid ciphertext");
        }
    else
        pkcs1v15EncryptUnpad(padded) catch {
            return qjs.JS_ThrowInternalError(ctx, "Invalid ciphertext padding");
        };

    return createUint8Array(ctx, message);
}

/// rsaPrivateEncrypt(privateKey, buffer, [padding]) - Encrypt with private key (for signatures)
/// padding: 1 = PKCS#1 v1.5 (default)
/// privateKey: ArrayBuffer (PEM or DER format)
fn rsaPrivateEncryptFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "rsaPrivateEncrypt requires privateKey, buffer");

    // Get private key (PEM or DER)
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Private key must be ArrayBuffer");
    }
    const key_data = @as([*]const u8, @ptrCast(key_ptr))[0..key_size];

    // Convert PEM to DER if needed
    const key_parsed = parseKeyFormat(std.heap.page_allocator, key_data) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid key format (PEM or DER expected)");
    };
    defer if (key_parsed.owned) std.heap.page_allocator.free(@constCast(key_parsed.der));

    // Get buffer to encrypt
    var buf_size: usize = 0;
    const buf_ptr = qjs.JS_GetArrayBuffer(ctx, &buf_size, argv[1]);
    if (buf_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Buffer must be ArrayBuffer");
    }
    const buffer = @as([*]const u8, @ptrCast(buf_ptr))[0..buf_size];

    // Get padding mode (optional, default to PKCS#1 v1.5)
    var padding: i32 = RSA_PADDING.PKCS1_V15;
    if (argc >= 3) {
        _ = qjs.JS_ToInt32(ctx, &padding, argv[2]);
    }

    // Parse RSA private key
    const key = parseRsaPrivateKeyDer(key_parsed.der) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid RSA private key format");
    };

    const d = key.d orelse {
        return qjs.JS_ThrowTypeError(ctx, "Private key required for private encryption");
    };

    const key_len = key.n.len;

    // Pad the message (using type 1 padding for private key encryption - signature padding)
    const padded = pkcs1v15SignaturePad(std.heap.page_allocator, buffer, key_len) catch {
        return qjs.JS_ThrowInternalError(ctx, "Message too long for RSA key");
    };
    defer std.heap.page_allocator.free(padded);

    // Encrypt with private key: ciphertext = padded^d mod n
    const ciphertext = modPow(std.heap.page_allocator, padded, d, key.n) catch {
        return qjs.JS_ThrowInternalError(ctx, "RSA private encryption failed");
    };
    defer std.heap.page_allocator.free(ciphertext);

    return createUint8Array(ctx, ciphertext);
}

/// PKCS#1 v1.5 signature padding (block type 1)
/// Format: 0x00 0x01 [0xFF bytes] 0x00 [message]
fn pkcs1v15SignaturePad(allocator: std.mem.Allocator, message: []const u8, key_len: usize) ![]u8 {
    // Need at least 11 bytes of padding: 0x00 0x01 + 8 xFF + 0x00
    if (message.len > key_len - 11) {
        return error.MessageTooLong;
    }

    const em = try allocator.alloc(u8, key_len);
    em[0] = 0x00;
    em[1] = 0x01;

    // Fill with 0xFF bytes
    const ps_len = key_len - message.len - 3;
    @memset(em[2 .. 2 + ps_len], 0xFF);
    em[2 + ps_len] = 0x00;

    @memcpy(em[3 + ps_len ..], message);

    return em;
}

/// rsaPublicDecrypt(publicKey, buffer, [padding]) - Decrypt with public key
/// padding: 1 = PKCS#1 v1.5 (default)
/// publicKey: ArrayBuffer (PEM or DER format)
fn rsaPublicDecryptFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) return qjs.JS_ThrowTypeError(ctx, "rsaPublicDecrypt requires publicKey, buffer");

    // Get public key (PEM or DER)
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Public key must be ArrayBuffer");
    }
    const key_data = @as([*]const u8, @ptrCast(key_ptr))[0..key_size];

    // Convert PEM to DER if needed
    const key_parsed = parseKeyFormat(std.heap.page_allocator, key_data) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid key format (PEM or DER expected)");
    };
    defer if (key_parsed.owned) std.heap.page_allocator.free(@constCast(key_parsed.der));

    // Get buffer to decrypt
    var buf_size: usize = 0;
    const buf_ptr = qjs.JS_GetArrayBuffer(ctx, &buf_size, argv[1]);
    if (buf_ptr == null) {
        return qjs.JS_ThrowTypeError(ctx, "Buffer must be ArrayBuffer");
    }
    const buffer = @as([*]const u8, @ptrCast(buf_ptr))[0..buf_size];

    // Parse RSA public key (or private key for its public components)
    const pub_key = parseRsaPublicKeyDer(key_parsed.der) catch blk: {
        break :blk parseRsaPrivateKeyDer(key_parsed.der) catch {
            return qjs.JS_ThrowTypeError(ctx, "Invalid RSA key format");
        };
    };

    // Decrypt with public key: plaintext = buffer^e mod n
    const decrypted = modPow(std.heap.page_allocator, buffer, pub_key.e, pub_key.n) catch {
        return qjs.JS_ThrowInternalError(ctx, "RSA public decryption failed");
    };
    defer std.heap.page_allocator.free(decrypted);

    // Remove PKCS#1 v1.5 signature padding
    const message = pkcs1v15SignatureUnpad(decrypted) catch {
        return qjs.JS_ThrowInternalError(ctx, "Invalid padding");
    };

    return createUint8Array(ctx, message);
}

/// Remove PKCS#1 v1.5 signature padding (block type 1)
fn pkcs1v15SignatureUnpad(padded: []const u8) ![]const u8 {
    if (padded.len < 11) return error.InvalidPadding;
    if (padded[0] != 0x00 or padded[1] != 0x01) return error.InvalidPadding;

    // Find 0x00 separator after 0xFF padding
    var sep_idx: ?usize = null;
    for (padded[2..], 2..) |byte, i| {
        if (byte == 0x00) {
            sep_idx = i;
            break;
        }
        if (byte != 0xFF) return error.InvalidPadding;
    }

    const idx = sep_idx orelse return error.InvalidPadding;
    if (idx < 10) return error.InvalidPadding; // Need at least 8 bytes of 0xFF padding

    return padded[idx + 1 ..];
}

// =============================================================================
// ChaCha20-Poly1305 AEAD Encryption
// =============================================================================

/// chacha20Poly1305Encrypt(key, nonce, plaintext, aad) - ChaCha20-Poly1305 encryption
/// key: 32 bytes, nonce: 12 bytes, returns ciphertext with 16-byte tag appended
fn chacha20Poly1305Encrypt(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) {
        return qjs.JS_ThrowTypeError(ctx, "chacha20Poly1305Encrypt requires key, nonce, plaintext");
    }

    // Get key (32 bytes)
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null or key_size != 32) {
        return qjs.JS_ThrowTypeError(ctx, "Key must be 32 bytes");
    }
    const key = @as([*]const u8, @ptrCast(key_ptr))[0..32];

    // Get nonce (12 bytes)
    var nonce_size: usize = 0;
    const nonce_ptr = qjs.JS_GetArrayBuffer(ctx, &nonce_size, argv[1]);
    if (nonce_ptr == null or nonce_size != 12) {
        return qjs.JS_ThrowTypeError(ctx, "Nonce must be 12 bytes");
    }
    const nonce = @as([*]const u8, @ptrCast(nonce_ptr))[0..12];

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
        return qjs.JS_ThrowRangeError(ctx, "Plaintext too large");
    }

    const ciphertext = encrypt_buffer[0..plaintext.len];
    var tag: [16]u8 = undefined;

    std.crypto.aead.chacha_poly.ChaCha20Poly1305.encrypt(
        ciphertext,
        &tag,
        plaintext,
        aad,
        nonce.*,
        key.*,
    );

    // Append tag after ciphertext
    @memcpy(encrypt_buffer[plaintext.len..][0..16], &tag);

    return createUint8Array(ctx, encrypt_buffer[0..output_len]);
}

/// chacha20Poly1305Decrypt(key, nonce, ciphertext_with_tag, aad) - ChaCha20-Poly1305 decryption
fn chacha20Poly1305Decrypt(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) {
        return qjs.JS_ThrowTypeError(ctx, "chacha20Poly1305Decrypt requires key, nonce, ciphertext");
    }

    // Get key (32 bytes)
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null or key_size != 32) {
        return qjs.JS_ThrowTypeError(ctx, "Key must be 32 bytes");
    }
    const key = @as([*]const u8, @ptrCast(key_ptr))[0..32];

    // Get nonce (12 bytes)
    var nonce_size: usize = 0;
    const nonce_ptr = qjs.JS_GetArrayBuffer(ctx, &nonce_size, argv[1]);
    if (nonce_ptr == null or nonce_size != 12) {
        return qjs.JS_ThrowTypeError(ctx, "Nonce must be 12 bytes");
    }
    const nonce = @as([*]const u8, @ptrCast(nonce_ptr))[0..12];

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

    std.crypto.aead.chacha_poly.ChaCha20Poly1305.decrypt(
        plaintext,
        ciphertext,
        tag.*,
        aad,
        nonce.*,
        key.*,
    ) catch {
        return qjs.JS_ThrowTypeError(ctx, "Authentication failed - invalid ciphertext or tag");
    };

    return createUint8Array(ctx, plaintext);
}

// =============================================================================
// secp256k1 Elliptic Curve (Bitcoin/Ethereum)
// =============================================================================

/// secp256k1GenerateKeyPair() - Generate secp256k1 key pair
fn secp256k1GenerateKeyPairFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const Secp256k1 = std.crypto.ecc.Secp256k1;

    // Generate random private key
    var private_key: [32]u8 = undefined;
    std.crypto.random.bytes(&private_key);

    // Derive public key
    const secret = Secp256k1.scalar.Scalar.fromBytes(private_key, .big) catch {
        return qjs.JS_ThrowTypeError(ctx, "Failed to create scalar from random bytes");
    };

    const public_point = Secp256k1.basePoint.mul(secret.toBytes(.big), .big) catch {
        return qjs.JS_ThrowTypeError(ctx, "Failed to derive public key");
    };

    // Return { privateKey: Uint8Array(32), publicKey: Uint8Array(33) } (compressed)
    const result = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, result, "privateKey", createUint8Array(ctx, &private_key));

    // Compressed public key (33 bytes: prefix + x-coordinate)
    var compressed: [33]u8 = undefined;
    const affine = public_point.affineCoordinates();
    const y_bytes = affine.y.toBytes(.big);
    compressed[0] = if (y_bytes[31] & 1 == 0) 0x02 else 0x03;
    @memcpy(compressed[1..], &affine.x.toBytes(.big));

    _ = qjs.JS_SetPropertyStr(ctx, result, "publicKey", createUint8Array(ctx, &compressed));

    return result;
}

/// secp256k1Sign(privateKey, messageHash) - Sign with secp256k1 (ECDSA)
fn secp256k1SignFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) {
        return qjs.JS_ThrowTypeError(ctx, "secp256k1Sign requires privateKey and messageHash");
    }

    // Get private key (32 bytes)
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null or key_size != 32) {
        return qjs.JS_ThrowTypeError(ctx, "Private key must be 32 bytes");
    }
    const private_key = @as([*]const u8, @ptrCast(key_ptr))[0..32];

    // Get message hash (32 bytes)
    var msg_size: usize = 0;
    const msg_ptr = qjs.JS_GetArrayBuffer(ctx, &msg_size, argv[1]);
    if (msg_ptr == null or msg_size != 32) {
        return qjs.JS_ThrowTypeError(ctx, "Message hash must be 32 bytes");
    }
    const message = @as([*]const u8, @ptrCast(msg_ptr))[0..32];

    const Secp256k1 = std.crypto.ecc.Secp256k1;
    const Ecdsa = std.crypto.sign.ecdsa.Ecdsa(Secp256k1, std.crypto.hash.sha2.Sha256);

    // Create key pair from private key
    const secret_key = Ecdsa.SecretKey.fromBytes(private_key.*) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid private key");
    };
    const key_pair = Ecdsa.KeyPair.fromSecretKey(secret_key) catch {
        return qjs.JS_ThrowTypeError(ctx, "Invalid key pair");
    };

    // Sign with deterministic nonce (RFC 6979)
    const sig = key_pair.sign(message, null) catch {
        return qjs.JS_ThrowTypeError(ctx, "Signing failed");
    };

    // Return signature (64 bytes: r || s)
    return createUint8Array(ctx, &sig.toBytes());
}

/// secp256k1Verify(publicKey, messageHash, signature) - Verify secp256k1 signature
fn secp256k1VerifyFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 3) {
        return qjs.JS_ThrowTypeError(ctx, "secp256k1Verify requires publicKey, messageHash, signature");
    }

    // Get public key (33 or 65 bytes)
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null or (key_size != 33 and key_size != 65)) {
        return qjs.JS_ThrowTypeError(ctx, "Public key must be 33 (compressed) or 65 (uncompressed) bytes");
    }
    const public_key_bytes = @as([*]const u8, @ptrCast(key_ptr))[0..key_size];

    // Get message hash (32 bytes)
    var msg_size: usize = 0;
    const msg_ptr = qjs.JS_GetArrayBuffer(ctx, &msg_size, argv[1]);
    if (msg_ptr == null or msg_size != 32) {
        return qjs.JS_ThrowTypeError(ctx, "Message hash must be 32 bytes");
    }
    const message = @as([*]const u8, @ptrCast(msg_ptr))[0..32];

    // Get signature (64 bytes)
    var sig_size: usize = 0;
    const sig_ptr = qjs.JS_GetArrayBuffer(ctx, &sig_size, argv[2]);
    if (sig_ptr == null or sig_size != 64) {
        return qjs.JS_ThrowTypeError(ctx, "Signature must be 64 bytes");
    }
    const sig_bytes = @as([*]const u8, @ptrCast(sig_ptr))[0..64];

    const Secp256k1 = std.crypto.ecc.Secp256k1;
    const Ecdsa = std.crypto.sign.ecdsa.Ecdsa(Secp256k1, std.crypto.hash.sha2.Sha256);

    // Parse public key
    const public_key = if (key_size == 33)
        Ecdsa.PublicKey.fromSec1(public_key_bytes[0..33]) catch {
            return quickjs.jsFalse();
        }
    else blk: {
        // Uncompressed format - extract coordinates
        var sec1: [33]u8 = undefined;
        sec1[0] = if (public_key_bytes[64] & 1 == 0) 0x02 else 0x03;
        @memcpy(sec1[1..], public_key_bytes[1..33]);
        break :blk Ecdsa.PublicKey.fromSec1(&sec1) catch {
            return quickjs.jsFalse();
        };
    };

    // Parse signature (fromBytes doesn't return an error)
    const signature = Ecdsa.Signature.fromBytes(sig_bytes.*);

    // Verify
    signature.verify(message, public_key) catch {
        return quickjs.jsFalse();
    };

    return quickjs.jsTrue();
}

/// secp256k1ComputeSecret(privateKey, publicKey) - ECDH shared secret
fn secp256k1ComputeSecretFunc(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) {
        return qjs.JS_ThrowTypeError(ctx, "secp256k1ComputeSecret requires privateKey and publicKey");
    }

    // Get private key (32 bytes)
    var key_size: usize = 0;
    const key_ptr = qjs.JS_GetArrayBuffer(ctx, &key_size, argv[0]);
    if (key_ptr == null or key_size != 32) {
        return qjs.JS_ThrowTypeError(ctx, "Private key must be 32 bytes");
    }
    const private_key = @as([*]const u8, @ptrCast(key_ptr))[0..32];

    // Get public key (33 or 65 bytes)
    var pub_size: usize = 0;
    const pub_ptr = qjs.JS_GetArrayBuffer(ctx, &pub_size, argv[1]);
    if (pub_ptr == null or (pub_size != 33 and pub_size != 65)) {
        return qjs.JS_ThrowTypeError(ctx, "Public key must be 33 or 65 bytes");
    }
    const public_key_bytes = @as([*]const u8, @ptrCast(pub_ptr))[0..pub_size];

    const Secp256k1 = std.crypto.ecc.Secp256k1;

    // Parse public key to point
    const public_point = if (pub_size == 33)
        Secp256k1.fromSec1(public_key_bytes[0..33]) catch {
            return qjs.JS_ThrowTypeError(ctx, "Invalid public key");
        }
    else blk: {
        var sec1: [33]u8 = undefined;
        sec1[0] = if (public_key_bytes[64] & 1 == 0) 0x02 else 0x03;
        @memcpy(sec1[1..], public_key_bytes[1..33]);
        break :blk Secp256k1.fromSec1(&sec1) catch {
            return qjs.JS_ThrowTypeError(ctx, "Invalid public key");
        };
    };

    // Compute shared secret (scalar multiplication)
    const shared_point = public_point.mul(private_key.*, .big) catch {
        return qjs.JS_ThrowTypeError(ctx, "ECDH computation failed");
    };

    // Return x-coordinate as shared secret
    const affine = shared_point.affineCoordinates();
    const secret = affine.x.toBytes(.big);

    return createUint8Array(ctx, &secret);
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

// ============================================================================
// crypto.subtle Web Crypto API implementation
// ============================================================================

/// subtle.digest(algorithm, data) - returns Promise<ArrayBuffer>
/// Web Crypto uses "SHA-256", "SHA-384", "SHA-512", "SHA-1"
fn subtleDigest(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 2) {
        return qjs.JS_ThrowTypeError(ctx, "subtle.digest() requires 2 arguments: algorithm, data");
    }

    // Get algorithm - can be string or AlgorithmIdentifier object
    var algo_name: []const u8 = undefined;
    var algo_str: ?[*:0]const u8 = null;

    if (qjs.JS_IsString(argv[0])) {
        algo_str = qjs.JS_ToCString(ctx, argv[0]);
        if (algo_str == null) {
            return qjs.JS_ThrowTypeError(ctx, "Invalid algorithm");
        }
        algo_name = std.mem.span(algo_str.?);
    } else if (qjs.JS_IsObject(argv[0])) {
        // AlgorithmIdentifier object with 'name' property
        const name_prop = qjs.JS_GetPropertyStr(ctx, argv[0], "name");
        defer qjs.JS_FreeValue(ctx, name_prop);
        if (qjs.JS_IsString(name_prop)) {
            algo_str = qjs.JS_ToCString(ctx, name_prop);
            if (algo_str != null) {
                algo_name = std.mem.span(algo_str.?);
            }
        }
        if (algo_str == null) {
            return qjs.JS_ThrowTypeError(ctx, "AlgorithmIdentifier must have 'name' property");
        }
    } else {
        return qjs.JS_ThrowTypeError(ctx, "Algorithm must be string or AlgorithmIdentifier");
    }
    defer if (algo_str) |s| qjs.JS_FreeCString(ctx, s);

    // Get data - ArrayBuffer, TypedArray, or DataView
    var data_bytes: []const u8 = undefined;
    var size: usize = 0;

    // Try as ArrayBuffer first
    const ptr = qjs.JS_GetArrayBuffer(ctx, &size, argv[1]);
    if (ptr != null) {
        data_bytes = @as([*]const u8, @ptrCast(ptr))[0..size];
    } else {
        // Try as TypedArray
        const buffer = qjs.JS_GetTypedArrayBuffer(ctx, argv[1], null, &size, null);
        if (!qjs.JS_IsException(buffer)) {
            const buf_ptr = qjs.JS_GetArrayBuffer(ctx, &size, buffer);
            qjs.JS_FreeValue(ctx, buffer);
            if (buf_ptr != null) {
                data_bytes = @as([*]const u8, @ptrCast(buf_ptr))[0..size];
            } else {
                return qjs.JS_ThrowTypeError(ctx, "Data must be ArrayBuffer or TypedArray");
            }
        } else {
            qjs.JS_FreeValue(ctx, buffer);
            return qjs.JS_ThrowTypeError(ctx, "Data must be ArrayBuffer or TypedArray");
        }
    }

    // Compute hash based on algorithm (Web Crypto naming: "SHA-256", "SHA-384", etc.)
    var hash_result: []const u8 = undefined;
    var hash_256: [32]u8 = undefined;
    var hash_384: [48]u8 = undefined;
    var hash_512: [64]u8 = undefined;
    var hash_1: [20]u8 = undefined;

    // Normalize algorithm name (uppercase, handle with/without hyphen)
    if (std.ascii.eqlIgnoreCase(algo_name, "SHA-256") or std.ascii.eqlIgnoreCase(algo_name, "SHA256")) {
        std.crypto.hash.sha2.Sha256.hash(data_bytes, &hash_256, .{});
        hash_result = &hash_256;
    } else if (std.ascii.eqlIgnoreCase(algo_name, "SHA-384") or std.ascii.eqlIgnoreCase(algo_name, "SHA384")) {
        std.crypto.hash.sha2.Sha384.hash(data_bytes, &hash_384, .{});
        hash_result = &hash_384;
    } else if (std.ascii.eqlIgnoreCase(algo_name, "SHA-512") or std.ascii.eqlIgnoreCase(algo_name, "SHA512")) {
        std.crypto.hash.sha2.Sha512.hash(data_bytes, &hash_512, .{});
        hash_result = &hash_512;
    } else if (std.ascii.eqlIgnoreCase(algo_name, "SHA-1") or std.ascii.eqlIgnoreCase(algo_name, "SHA1")) {
        std.crypto.hash.Sha1.hash(data_bytes, &hash_1, .{});
        hash_result = &hash_1;
    } else {
        return qjs.JS_ThrowTypeError(ctx, "Unsupported algorithm: %s", algo_str.?);
    }

    // Create ArrayBuffer with hash result
    const array_buffer = qjs.JS_NewArrayBufferCopy(ctx, hash_result.ptr, hash_result.len);
    if (qjs.JS_IsException(array_buffer)) {
        return array_buffer;
    }

    // Wrap in resolved Promise
    const global = qjs.JS_GetGlobalObject(ctx);
    defer qjs.JS_FreeValue(ctx, global);
    const promise_ctor = qjs.JS_GetPropertyStr(ctx, global, "Promise");
    defer qjs.JS_FreeValue(ctx, promise_ctor);
    const resolve_func = qjs.JS_GetPropertyStr(ctx, promise_ctor, "resolve");
    defer qjs.JS_FreeValue(ctx, resolve_func);

    var resolve_args = [1]qjs.JSValue{array_buffer};
    const promise = qjs.JS_Call(ctx, resolve_func, promise_ctor, 1, &resolve_args);
    qjs.JS_FreeValue(ctx, array_buffer);
    return promise;
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
        .{ "p256GenerateKeyPair", p256GenerateKeyPairFunc, 0 },
        .{ "p256Sign", p256SignFunc, 2 },
        .{ "p256Verify", p256VerifyFunc, 3 },
        .{ "p256ComputeSecret", p256ComputeSecretFunc, 2 },
        .{ "p384GenerateKeyPair", p384GenerateKeyPairFunc, 0 },
        .{ "p384Sign", p384SignFunc, 2 },
        .{ "p384Verify", p384VerifyFunc, 3 },
        .{ "p384ComputeSecret", p384ComputeSecretFunc, 2 },
        .{ "rsaSign", rsaSignFunc, 4 },     // key, message, [padding], [saltLength]
        .{ "rsaVerify", rsaVerifyFunc, 5 },   // key, message, signature, [padding], [saltLength]
        .{ "rsaEncrypt", rsaEncryptFunc, 3 }, // key, message, [padding]
        .{ "rsaDecrypt", rsaDecryptFunc, 3 }, // key, ciphertext, [padding]
        .{ "rsaPrivateEncrypt", rsaPrivateEncryptFunc, 3 }, // key, buffer, [padding]
        .{ "rsaPublicDecrypt", rsaPublicDecryptFunc, 3 }, // key, buffer, [padding]
        .{ "generateKeyPairSync", generateKeyPairSyncFunc, 2 },
        .{ "rsaGenerateKeyPairSync", rsaGenerateKeyPairSyncFunc, 1 },
        .{ "generatePrimeSync", generatePrimeSyncFunc, 1 },
        .{ "checkPrimeSync", checkPrimeSyncFunc, 1 },
        .{ "ed25519DerivePublicKey", ed25519DerivePublicKeyFunc, 1 },
        .{ "x25519DerivePublicKey", x25519DerivePublicKeyFunc, 1 },
        // ChaCha20-Poly1305
        .{ "chacha20Poly1305Encrypt", chacha20Poly1305Encrypt, 4 },
        .{ "chacha20Poly1305Decrypt", chacha20Poly1305Decrypt, 4 },
        // secp256k1 (Bitcoin/Ethereum)
        .{ "secp256k1GenerateKeyPair", secp256k1GenerateKeyPairFunc, 0 },
        .{ "secp256k1Sign", secp256k1SignFunc, 2 },
        .{ "secp256k1Verify", secp256k1VerifyFunc, 3 },
        .{ "secp256k1ComputeSecret", secp256k1ComputeSecretFunc, 2 },
    }) |binding| {
        const func = qjs.JS_NewCFunction(ctx, binding[1], binding[0], binding[2]);
        _ = qjs.JS_SetPropertyStr(ctx, crypto_obj, binding[0], func);
    }

    // Create crypto.subtle Web Crypto API object
    const subtle_obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, subtle_obj, "digest", qjs.JS_NewCFunction(ctx, subtleDigest, "digest", 2));
    // Note: Additional subtle methods (encrypt, decrypt, sign, verify, etc.) can be added here
    _ = qjs.JS_SetPropertyStr(ctx, crypto_obj, "subtle", subtle_obj);

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
