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
/// Supported: sha256, sha512, sha1 (deprecated but supported)
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

        // Convert to hex string
        return hashToHex(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "sha512")) {
        var hash: [64]u8 = undefined;
        std.crypto.hash.sha2.Sha512.hash(data_bytes, &hash, .{});

        return hashToHex(ctx, &hash);
    } else if (std.mem.eql(u8, algorithm, "sha1")) {
        var hash: [20]u8 = undefined;
        std.crypto.hash.Sha1.hash(data_bytes, &hash, .{});

        return hashToHex(ctx, &hash);
    } else {
        return qjs.JS_ThrowTypeError(ctx, "Unsupported hash algorithm (use sha256, sha512, or sha1)");
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
    } else if (std.mem.eql(u8, algorithm, "sha512")) {
        var mac: [64]u8 = undefined;
        std.crypto.auth.hmac.sha2.HmacSha512.create(&mac, data, key);

        return hashToHex(ctx, &mac);
    } else {
        return qjs.JS_ThrowTypeError(ctx, "Unsupported HMAC algorithm (use sha256 or sha512)");
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
